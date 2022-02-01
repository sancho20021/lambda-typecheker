module Solution
  ( check
  ) where
import Base (MyError (ParseError))
import Checker (typeCheck)
import Control.Monad (void)
import Parser (parse)
import Text.Megaparsec (errorBundlePretty)

check :: String -> Either MyError ()
check input = case parse input of
  Left err -> Left $ ParseError $ errorBundlePretty err
  Right de -> void $ typeCheck de
