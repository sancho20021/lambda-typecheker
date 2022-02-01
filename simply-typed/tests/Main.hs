module Main where
import qualified CheckerTest
import qualified ParserTest
import System.Environment (setEnv)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain allTests


allTests :: TestTree
allTests = testGroup "allTests"
  [ ParserTest.parserTests
  , CheckerTest.checkerTests ]
