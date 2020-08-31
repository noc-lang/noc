module Language.Noc.Parser
  ( parseNoc,
    parseNocFile,
    ParseError,
    Stack,
    Atom (QuoteAtom, WordAtom, FloatAtom),
  )
where

-- Noc Grammar
-- <word> ::= [A-Za-z-_]+
-- <numbers> ::= [0-9]+
-- <quote> ::= "[" (<number> | <word> | <quote> |)+ "]"
-- <stack> ::= (<word> | <quote>)*

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

-- Noc AST
data Atom = QuoteAtom Stack | WordAtom String | FloatAtom Float deriving (Show, Eq)

type Stack = [Atom]

-----------------------
opSpaces :: Parser ()
opSpaces = (optional $ spaces)

spaces' :: Parser Atom -> Parser Atom
spaces' p = between opSpaces opSpaces p

operator :: Parser Char
operator = oneOf "+-/*"

readDigit :: Parser String -> Parser Float
readDigit p = (\x -> read x :: Float) <$> p

-----------------------
word :: Parser Atom
word = spaces' $ (many1 (alphaNum <|> operator) <|> string ".") >>= (pure . WordAtom)

number :: Parser Atom
number = spaces' $ (readDigit $ many1 digit) >>= (pure . FloatAtom)

quote :: Parser Atom
quote = do
  char '['
  content <- stack
  char ']'
  opSpaces
  pure $ QuoteAtom content

-----------------------------------------------------
stack :: Parser Stack
stack = opSpaces >> sepBy (try number <|> word <|> quote) spaces

parseNoc :: String -> Either ParseError Stack
parseNoc expr = parse stack "" expr

parseNocFile :: String -> IO (Either ParseError Stack)
parseNocFile path = parseFromFile stack path
