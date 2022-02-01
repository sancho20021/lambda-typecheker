module Parser
  ( parse
  ) where
import Base (Context (Context), Deduce ((:|-)), Expr (V, (:.), (:@)), Type (Var, (:->)),
             TypeScheme (Full), TypedExpr ((:::)), TypedVar (..), Variable (Variable))
import Control.Monad (void)
import Data.Char (isAlphaNum)
import qualified Data.Map as Map
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof, label, try), ParseErrorBundle, Parsec, between, many,
                        optional, runParser, satisfy, sepBy, some, (<|>))
import Text.Megaparsec.Char (char, letterChar, space)
import qualified Text.Megaparsec.Char.Lexer as L

-- Grammar:
-- input       ::= [context '|-'] typed_expr

-- context     ::= eps | variable ':' type [',' context]

-- typed_expr  ::= expr ':' type

-- type        ::= variable
--               | (type)
--               | (type) '->' type
--               | variable '->' type

-- expr        ::= [application] '\' variable '.' expr
--               | application

-- application ::= atom
--               | application atom

-- atom        ::= (expr)
--               | variable

-- variable    ::= [a-z] [a-z0-9'_]*
--

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) Deduce
parse = runParser pInput "Input"

pInput :: Parser Deduce
pInput = between space eof pInput'
  where
    pInput' :: Parser Deduce
    pInput' = try pExprDeduce <|> ((Context Map.empty :|-) <$> pTypedExpr)

pExprDeduce :: Parser Deduce
pExprDeduce = do
  context <- pContext
  void $ symbol "|-"
  typedExpr <- pTypedExpr
  return $ context :|- typedExpr

pContext :: Parser Context
pContext = Context . Map.fromList <$> pTypedVar `sepBy` symbol ","
  where
    pTypedVar = do
      var <- pVar
      void $ symbol ":"
      typ <- pType
      return (var, typ)

pTypedExpr :: Parser TypedExpr
pTypedExpr = do
  expr <- pExpr
  void $ symbol ":"
  typ <- pType
  return (expr ::: Full typ)

pExpr :: Parser Expr
pExpr = pApply'

pVar :: Parser Variable
pVar = label "variable" $ do
  Variable <$> lexeme ((:) <$> letterChar <*> pIdTail)
  where
    pIdTail :: Parser String
    pIdTail = many (satisfy isAlphaNum <|> char '\'' <|> char '_')

pAtom :: Parser Expr
pAtom = inParen pExpr <|> (V <$> pVar)

pApply :: Parser Expr
pApply = foldl1 (:@) <$> some pAtom

pApply' :: Parser Expr
pApply' = pLam <|> do
  h <- pApply
  lam <- optional pLam
  return $ case lam of
    Nothing -> h
    Just l  -> h :@ l

pLam :: Parser Expr
pLam = label "lambda abstraction" $ do
  void $ symbol "\\"
  x <- pVar
  void $ symbol ":"
  typ <- pType
  void $ symbol "."
  (x :^ typ :.) <$> pExpr

-- Types -----------------------------------------------
pType :: Parser Type
pType = do
  lp <- inParen pType <|> (Var <$> pVar)
  rp <- optional $ symbol "->"  >> pType
  return $ case rp of
    Nothing  -> lp
    Just rp' -> lp :-> rp'

-- Help functions

inParen :: Parser a -> Parser a
inParen = symbol "(" `between` symbol ")"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space
