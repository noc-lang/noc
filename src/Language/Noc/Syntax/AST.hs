module Language.Noc.Syntax.AST where

----------------------- Modules ------------------------

import Language.Noc.Syntax.Lexer
-----
import Text.Parsec
import Text.Parsec.String (Parser)

----------------------- Atoms --------------------------
data Atom = QuoteAtom Expr | WordAtom String | FloatAtom Double | StringAtom String deriving (Show, Eq)

type Expr = [Atom]

sign :: Parser (Double -> Double)
sign = (char '-' >> return negate) <|> return id

word :: Parser Atom
word = WordAtom <$> identifier

strLiteral :: Parser Atom
strLiteral = StringAtom <$> stringLiteral

number :: Parser Atom
number = do
  f <- sign
  n <- naturalOrFloat
  pure $
    FloatAtom $
      f $ case n of
        Left (int) -> fromInteger int
        Right (fl) -> fl

quote :: Parser Atom
quote = QuoteAtom <$> (brackets stack)

stack :: Parser Expr
stack = many $ lexeme (quote <|> (try number) <|> strLiteral <|> word)

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
