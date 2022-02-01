-----------------------------------------------------------------------------
-- Church style simply-typed lambda expression type-checker
-----------------------------------------------------------------------------
module Main where
import Base (MyError (ParseError))
import Parser (pInput, test)
import Solution (check)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if args == ["show"]
  then debugSolve
  else solve

testLine :: IO ()
testLine = do
  putStrLn "type line"
  line <- getLine
  test pInput line
  testLine

solve :: IO ()
solve = do
  res <- getResult
  putStrLn $ case res of
    Left (ParseError _) -> "No parse"
    Left _              -> "Incorrect"
    Right _             -> "Correct"

debugSolve :: IO ()
debugSolve = do
  res <- getResult
  case res of
    Left s  -> print s
    Right _ -> putStrLn "Correct"

getResult :: IO (Either MyError ())
getResult = check <$> getLine
