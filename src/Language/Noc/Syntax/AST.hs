module Language.Noc.Syntax.AST where

----------------------- Modules ------------------------

import Language.Noc.Syntax.Lexer
-----
import Text.Parsec
import Text.Parsec.String (Parser)

----------------------- Atoms --------------------------
data Atom = QuoteAtom Expr | WordAtom String | IntAtom Integer | FloatAtom Double | StringAtom String | BoolAtom Bool deriving (Show, Eq)

type Expr = [Atom]

sign :: Num a => Parser (a -> a)
sign = (char '-' >> return negate) <|> return id

operators :: Parser String
operators = string "+" <|> string "-" <|> string "/" <|> string "*" <|> string "%"

word :: Parser Atom
word = WordAtom <$> (identifier <|> operators)

strLiteral :: Parser Atom
strLiteral = StringAtom <$> stringLiteral

int :: Parser Atom
int = do
  f <- sign
  n <- natural
  pure $ IntAtom $ f n

number :: Parser Atom
number = do
  f <- sign
  n <- float
  pure $ FloatAtom $ f n

bool :: Parser Atom
bool = do
  v <- (string "True" <|> string "False")
  case v of
    "True" -> pure $ BoolAtom True
    "False" -> pure $ BoolAtom False

quote :: Parser Atom
quote = QuoteAtom <$> (brackets stack)

stack :: Parser Expr
stack = many $ lexeme (quote <|> bool <|> (try number) <|> (try int) <|> strLiteral <|> word)

-------------------- Function declaration -----------------
data Declaration = Declaration {declName :: String, declVal :: Expr} deriving (Show, Eq)

type Program = [Declaration]

manyTill1 :: Parser Char -> Parser String -> Parser String
manyTill1 p pend = (:) <$> p <*> (manyTill p pend)

function :: Parser Declaration
function = do
  lexeme $ reserved "def"
  name <- lexeme $ (:) <$> (letter <|> char '_') <*> (manyTill1 (alphaNum <|> char '\'' <|> char '_') (whiteSpace >> symbol "="))
  content <- (lexeme $ braces $ (whiteSpace *> stack))
  pure $ Declaration name content

program :: Parser Program
program = whiteSpace *> (many function) <* eof
