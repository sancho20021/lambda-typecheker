module Checker where
import Base (Context (Context), Deduce ((:|-)), Expr (V, (:.), (:@)), MyError (..),
             Type (Var, (:->)), TypeScheme (Full, Partial), TypedExpr ((:::)), TypedVar ((:^)),
             Variable)
import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map as Map

type TErr a = Either MyError a

typeCheck :: Deduce -> TErr ()
typeCheck = void . typeCheckTraced

-- | Typecheck and save context in case of incorrect type
typeCheckTraced :: Deduce -> TErr Type
typeCheckTraced (Context ctx :|- te) = case typeCheck' ctx te of
  Left pe  -> Left $ MyError (Context ctx :|- te) pe
  Right ty -> pure ty

-- | Typecheck partially typed expression given the context
typeCheck'
  :: Map Variable Type  -- ^ Context
  -> TypedExpr          -- ^ Partially typed expression
  -> TErr Type
-- If expression is variable, axiom is used
typeCheck' g (V x ::: tau) =
  case Map.lookup x g of
    Nothing   -> Left $ UndefinedVar x
    Just tau' -> (V x ::: tau) `hasType` tau'
-- If expression is lambda abstraction, "-> Introduction" rule is used.
-- Type of lambda abstraction has "? -> ? -> ... -> type" shape,
-- so we try to refine first ? with the type of binding variable 'x'
typeCheck' g (x :^ sigma :. m ::: lamType) = case lamType of
  Full (sigma' :-> tau) -> sigma' === sigma >> introduce g (x :^ sigma) (m ::: Full tau)
  Partial ts            -> introduce g (x :^ sigma) (m ::: ts)
  Full _                -> Left $ TypeError (show sigma ++ " -> ?") (m ::: lamType)
-- If expression is application, "-> Elimination" rule is used.
-- m : ? -> tau, n : ?
-- Identify ? and then typecheck
typeCheck' g (m :@ n ::: tau) = do
  mType <- typeCheckTraced (Context g :|- m ::: Partial tau)
  (fullTau, nType) <- case mType of
    Var _          -> error "expected function type, got atomic type"
    sigma :-> tau' -> return (tau', sigma)
  void $ typeCheckTraced (Context g :|- n ::: Full nType)
  return fullTau

-- | "-> Introduction" rule
-- Returns type of lambda abstraction
introduce
  :: Map Variable Type  -- ^ Context
  -> TypedVar           -- ^ Binding variable
  -> TypedExpr          -- ^ return expression
  -> TErr Type
introduce g (x :^ sigma) m = do
  case Map.lookup x g of
    Nothing -> return ()
    Just ty -> Left $ DuplicateError x sigma ty
  retType <- typeCheckTraced $ g `withFreshVar` (x :^ sigma) :|- m
  return $ sigma :-> retType
  where
    withFreshVar :: Map Variable Type -> TypedVar -> Context
    withFreshVar vars (var :^ t) = Context $ Map.insert var t vars

-- | Try to refine type scheme with specified type
hasType :: TypedExpr -> Type -> TErr Type
hasType (expr ::: actual) expected =
  if substitute expected actual
  then pure expected
  else Left $ TypeError (show expected) (expr ::: actual)
  where
    substitute :: Type -> TypeScheme -> Bool
    substitute x (Full x')            = x == x'
    substitute (_ :-> b) (Partial b') = substitute b b'
    substitute _ _                    = False

-- | Analogue of Assert equals
(===) :: (Eq a, Show a) => a -> a -> TErr ()
(===) a b = if a == b
            then pure ()
            else Left $ EqFail $ show a ++ " != " ++ show b
infix 4 ===
