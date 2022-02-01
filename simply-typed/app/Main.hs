-----------------------------------------------------------------------------
-- Church style simply-typed lambda expression type-checker
-----------------------------------------------------------------------------
module Main where
import Parser (pInput, test)
import Solution (check)

main :: IO ()
main = debugSolve

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
    Left _  -> "Incorrect"
    Right _ -> "Correct"

debugSolve :: IO ()
debugSolve = do
  res <- getResult
  case res of
    Left s  -> putStrLn s
    Right _ -> putStrLn "Correct"

getResult :: IO (Either String ())
getResult = check <$> getLine
