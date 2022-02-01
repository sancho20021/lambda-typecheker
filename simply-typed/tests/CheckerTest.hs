module CheckerTest
  ( checkerTests
  ) where
import Base (Context (Context), Deduce ((:|-)), Expr (V, (:.), (:@)), Type (Var, (:->)),
             TypeScheme (Full), TypedExpr ((:::)), TypedVar ((:^)), Variable (Variable))
import Checker (typeCheck)
import Data.Either (isLeft)
import Data.Foldable (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace)
import ParserTest (sizedVar)
import Solution (check)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@=?))
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), Gen, frequency, testProperty, withMaxSuccess)

checkerTests :: TestTree
checkerTests = testGroup "Checker Tests" [ manualTests, qcTests ]

manualTests :: TestTree
manualTests = testGroup "Manual Tests"
  [ checkCorrect "" "\\f : (a -> a -> g). \\x : a. \\y : b. f x x : (a -> a -> g) -> a -> b -> g"
  , checkIncorrect
    "a -> a -> g != ? -> ? -> b"
    "\\f : (a -> a -> g). \\x : a. \\y : b. f x x : (a -> a -> g) -> a -> b -> b"
  , checkCorrect
    ""
    "\\f : (a -> g) -> a. \\x : a -> g. \\y : b. x (f x) : ((a -> g) -> a) -> (a -> g) -> b -> g"
  , checkIncorrect
    "b != g"
    "\\f : (a -> g) -> a. \\x : a -> g. \\y : b. x (f x) : ((a -> g) -> a) -> (a -> g) -> g -> g"
  , checkIncorrect
    "incorrect lambda type"
    "\f : ((a -> a) -> a). \\g : a -> a -> b. f (\\a : a. g a a) : ((a -> b) -> a) -> (a -> a -> b) -> a"
  , checkCorrect
    ""
    "\\f : ((a -> b) -> a). \\g : a -> a -> b. f (\\a : a. g a a) : ((a -> b) -> a) -> (a -> a -> b) -> a"
  , checkCorrect
    ""
    "\\f : (a -> b)->a. \\g : a -> a -> b. (\\a : a. g a a) (f (\\a : a. g a a)) : ((a -> b) -> a) -> (a -> a -> b) -> b"
  , checkCorrect
    "S combinator"
    "x : g -> a -> b, y : g -> a, z : g |- x z (y z) : b"
  , checkIncorrect
    ""
    "x : g -> a -> b, y : g -> a, z : g |- x z (y z) : a"
  , checkCorrect
    ""
    "y : g -> a -> b, z : g |- \\x : (a -> b) -> b. x (y z) : ((a -> b) -> b) -> b"
  , checkIncorrect
    ""
    "y : g -> a -> b, z : z |- \\x : (a -> b) -> b. x (y z) : ((a -> b) -> b) -> b"
  , checkCorrect
    ""
    "x : a -> a -> b |- \\y : a. \\z : b -> g. z (x y y) : a -> (b -> g) -> g"
  , checkIncorrect
    ""
    "x : a -> c -> b |- \\y : a. \\z : b -> g. z (x y y) : a -> (b -> g) -> g"
  , checkIncorrect
    ""
    "x : b -> a -> b |- \\y : a. \\z : b -> g. z (x y y) : a -> (b -> g) -> g"
  , checkIncorrect
    "Conflicting bindings"
    "\\x : a. \\x : b. x : a -> b -> b"
  ]

checkCorrect :: String -> String -> TestTree
checkCorrect description input =
  testCase description $ Right () @=? check input

checkIncorrect :: String -> String -> TestTree
checkIncorrect description input =
  testCase description $ assertBool "should be Left" $ isLeft $ check input

-- QuickCheck

qcTests :: TestTree
qcTests = testGroup "(checked by QuickCheck)"
  [ testProperty "Correct -> check = true" $
    withMaxSuccess 1000 $ \(CorrectExpr deduce) ->  typeCheck deduce == Right ()
  ]

type Ctx = Map Variable Type

newtype CorrectExpr = CorrectExpr { unCE :: Deduce }

instance Show CorrectExpr where
  show = show . unCE

instance Arbitrary CorrectExpr where
  arbitrary = do
    typ <- arbitrary
    (ctx, expr) <- genCorrectExpr Map.empty typ
    return $ CorrectExpr $ Context ctx :|- expr ::: Full typ

genCorrectExpr :: Ctx -> Type -> Gen (Ctx, Expr)
genCorrectExpr ctx (Var typ) = frequency
  [ (2, genVar ctx (Var typ))
  , (1, genAppl ctx (Var typ))
  ]
genCorrectExpr ctx (left :-> right) = frequency
  [ (3, genLam ctx left right)
  , (2, genVar ctx (left :-> right))
  , (1, genAppl ctx (left :-> right))
  ]

freshVar :: Ctx -> Gen Variable
freshVar ctx = do
  var <- sizedVar 5
  if Map.member var ctx
  then freshVar ctx
  else return var

genVar :: Ctx -> Type -> Gen (Ctx, Expr)
genVar ctx typ = do
  let entries = Map.toList ctx
  case find ((== typ) . snd) entries of
    Just (e, t) -> return (ctx, V e)
    Nothing -> do
      fv <- freshVar ctx
      return (Map.insert fv typ ctx, V fv)

genLam :: Ctx -> Type -> Type -> Gen (Ctx, Expr)
genLam ctx left right = do
  fv <- freshVar ctx
  (ctx', m) <- genCorrectExpr (Map.insert fv left ctx) right
  return (Map.delete fv ctx', fv :^ left :. m)

genAppl :: Ctx -> Type -> Gen (Ctx, Expr)
genAppl ctx typ = do
  argType <- arbitrary
  (ctx', p) <- genCorrectExpr ctx (argType :-> typ)
  (ctx'', q) <- genCorrectExpr ctx' argType
  return (ctx'', p :@ q)

