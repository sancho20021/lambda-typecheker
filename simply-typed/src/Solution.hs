module Solution
  ( check
  ) where
import Checker (typeCheck)
import Parser (parse)

check :: String -> Either String ()
check input = case parse input of
  Nothing -> Left "No parse"
  Just de -> case typeCheck de of
      Left err -> Left $ show err
      Right _  -> Right ()
