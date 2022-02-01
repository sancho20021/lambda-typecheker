module ParserTest
  ( parserTests
  , sizedVar
  ) where
import Base (Context (Context), Deduce ((:|-)), Expr (V, (:.), (:@)), Type (Var, (:->)),
             TypeScheme (Full), TypedExpr ((:::)), TypedVar ((:^)), Variable (Variable))
import Control.Monad (liftM2)
import qualified Data.Bifunctor as BF
import Debug.Trace (trace, traceShow)
import Parser (parse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), Gen, Positive (Positive), chooseInt, elements,
                              frequency, oneof, sized)
import qualified Test.Tasty.QuickCheck as QC

parserTests :: TestTree
parserTests = testGroup "ParserTests" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "parse . show == id" $
    \deduce -> parse (show deduce) == Right deduce
  ]

instance Arbitrary Deduce where
  arbitrary = liftM2 (:|-) arbitrary arbitrary

instance Arbitrary Context where
  arbitrary = Context <$> arbitrary

instance Arbitrary TypedExpr where
  arbitrary = liftM2 (:::) arbitrary arbitrary

instance Arbitrary Expr where
  arbitrary = sized sizedExpr

sizedExpr :: Int -> Gen Expr
sizedExpr n
  | n <= 1 = V <$> arbitrary
  | otherwise  = do
  isLam <- arbitrary
  if isLam
  then do
    x <- arbitrary
    body <- sizedExpr (n - 1)
    return $ x :. body
  else do
    lSize <- chooseInt (1, n - 1)
    left <- sizedExpr lSize
    right <- sizedExpr (n - lSize)
    return $ left :@ right

instance Arbitrary TypedVar where
  arbitrary = liftM2 (:^) arbitrary arbitrary

instance Arbitrary TypeScheme where
  arbitrary = Full <$> arbitrary

instance Arbitrary Type where
  arbitrary = sized sizedType

sizedType :: Int -> Gen Type
sizedType n
  | n <= 1 = Var <$> arbitrary
  | otherwise = do
  leftSize <- chooseInt (1, n - 1)
  left <- sizedType leftSize
  right <- sizedType (n - leftSize)
  return $ left :-> right

instance Arbitrary Variable where
  arbitrary = do
    n <- frequency $ BF.second pure <$> [(10, 1), (3, 2), (1, 3)]
    sizedVar n

sizedVar :: Int -> Gen Variable
sizedVar n = Variable <$> ((:) <$> randomLatin <*> randomStr (n - 1))

randomLatin :: Gen Char
randomLatin = elements ['a'..'z']

randomChar :: Gen Char
randomChar = elements $
  ['_', '\''] ++ ['0'..'9'] ++ ['a'..'z']

randomStr :: Int -> Gen String
randomStr 0 = return ""
randomStr n = liftM2 (:) randomChar (randomStr (n - 1))
