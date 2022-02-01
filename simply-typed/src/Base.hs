module Base
  ( Context (..)
  , Deduce (..)
  , Expr (..)
  , MyError (..)
  , Type (..)
  , TypeScheme (..)
  , TypedExpr (..)
  , TypedVar (..)
  , Variable (..)
  ) where
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

-- | Variable
-- Can be used in types and expressions
newtype Variable = Variable String
  deriving (Eq, Ord)

instance Show Variable where
  show (Variable x) = x

-- | Expression type
data Type
  = Var Variable
  | Type :-> Type
  deriving (Eq)
instance Show Type where
  show (Var s)       = show s
  show (Var s :-> b) = show s ++ " -> " ++ show b
  show (s :-> b)     = "(" ++ show s ++ ") -> " ++ show b

-- | Lambda expression
data Expr
  = V Variable
  | Expr :@ Expr
  | TypedVar :. Expr
  deriving (Eq)
instance Show Expr where
  show (V v) = show v
  show (x :. t) = "\\" ++ show x ++ ". " ++ show t
  show (a :@ b) = inParen a ++ " " ++ inParen b
    where
      inParen x = case x of
        V var -> show var
        y     -> "(" ++ show y ++ ")"

-- | Typed variable
data TypedVar = Variable :^ Type
  deriving (Eq)
instance Show TypedVar where
  show (var :^ t) = show var ++ " : " ++ show t

-- | Context that contains free variables and their types
newtype Context = Context (Map Variable Type)
  deriving (Eq)
instance Show Context where
  show (Context ctx) =
    let pairs = show . uncurry (:^) <$> Map.toList ctx
    in  intercalate ", " pairs

-- | Fully or partially specified type
data TypeScheme
  = Full Type
  | Partial TypeScheme
  deriving (Eq)
instance Show TypeScheme where
  show (Full typ)    = show typ
  show (Partial typ) = "? -> " ++ show typ

-- | Typed expression
data TypedExpr = Expr ::: TypeScheme
  deriving (Eq)
instance Show TypedExpr where
  show (expr ::: t) = show expr ++ " : " ++ show t

-- | Statement that expression
-- has its type in specified context
data Deduce = Context :|- TypedExpr
  deriving (Eq)
instance Show Deduce where
  show (Context ctx :|- te) =
    if null ctx
    then "|- " ++ show te
    else show (Context ctx) ++ " |- " ++ show te

infix 3 :|-
infix 4 :::
infixr 5 :.
infixr 6 :->
infixr 6 :^
infix 7 :@

-- | Errors that occur while parsing or type-checking
data MyError
  = MyError Deduce MyError
  | UndefinedVar Variable
  | EqFail String
  | TypeError String TypedExpr
  | DuplicateError Variable Type Type
  | ParseError String
  deriving (Eq)

instance Show MyError where
  show (MyError context s)               = show s ++ "\n  in: " ++ show context
  show (UndefinedVar s)                 = "UndefinedVar: " ++ show s
  show (EqFail s)                       = "EqFail: " ++ s
  show (TypeError typ (expr ::: eType)) =
    "TypeError: different types of " ++ show expr ++ " are expected: 1) "
      ++ typ ++ " 2) " ++ show eType
  show (DuplicateError var t1 t2)       = "DuplicateError: conflicting bindings: "
    ++ show (var :^ t1) ++ ", " ++ show (var :^ t2)
  show (ParseError s)                   = s
