{-# LANGUAGE FlexibleContexts #-}

module Language.Noc.Syntax.AST where

----------------------- Modules ------------------------

import Data.Map (Map, fromList, keys)
import Data.Text (Text, empty, pack, strip, unpack)
import Language.Noc.Syntax.Lexer
import Text.Parsec
import Text.Parsec.String (Parser)

----------------------- Atoms --------------------------
data Atom = QuoteAtom Expr | WordAtom String | IntAtom Integer | FloatAtom Double | StringAtom String | CharAtom Char | BoolAtom Bool deriving (Show, Eq)

type Expr = [Atom]

--------------------- Utils ----------------------------

sign :: Num a => Parser (a -> a)
sign = (char '-' >> return negate) <|> return id

---- Escape char for single quote strings
doubleQuoteLiteral :: Parser String
doubleQuoteLiteral = between (char '"') (char '"') (many $ doubleQuoteLetter <|> doubleQuoteEscape)

doubleQuoteLetter :: Parser Char
doubleQuoteLetter = satisfy (\c -> (c /= '"') && (c /= '\\'))

doubleQuoteEscape :: Parser Char
doubleQuoteEscape = do
  char '\\'
  let codes = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
  esc <- choice $ map (\(c, code) -> char c >> return code) codes
  return esc

--------------------------------------------------------

word :: Parser Atom
word = WordAtom <$> (identifier <|> operator)

strLiteral :: Parser Atom
strLiteral = StringAtom <$> doubleQuoteLiteral

chrLiteral :: Parser Atom
chrLiteral = CharAtom <$> charLiteral

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
stack = many $ lexeme (quote <|> bool <|> (try number) <|> (try int) <|> strLiteral <|> chrLiteral <|> word)

-------------------- Module -----------------

data Module = Module {imports :: [FilePath], decls :: [Map Text (Maybe DocString, Expr)]}

type DocString = String

parseNameWithParens :: Parser String
parseNameWithParens = lexeme $ (:) <$> (symbol "(" >> (noneOf "'\\")) <*> (manyTill (noneOf "\\") (whiteSpace >> (symbol ")") >> whiteSpace >> (symbol "=")))

parseName :: Parser String
parseName = lexeme $ (:) <$> (noneOf "'()\\") <*> (manyTill (noneOf "()\\") (whiteSpace >> (symbol "=")))

parseContent :: Parser (Maybe DocString, Expr)
parseContent = do
  doc <- optionMaybe $ (try $ symbol "---") >> (manyTill anyChar (try $ symbol "---"))
  e <- whiteSpace *> stack
  case doc of
    (Just a) -> return (Just $ unpack $ strip $ pack a, e)
    Nothing -> return (Nothing, e)

function :: Parser (Map Text (Maybe DocString, Expr))
function = do
  lexeme $ reserved "def"
  name <- parseNameWithParens <|> parseName

  contentFunction <- (lexeme $ braces $ parseContent)
  let (doc, content) = contentFunction
  pure $ fromList [(pack name, (doc, content))]

load :: Parser String
load = do
  lexeme $ reserved $ "load"
  path <- lexeme $ (stringLiteral <|> (many $ alphaNum <|> char '/' <|> char '\\' <|> char '.'))
  pure path

program :: Parser Module
program = do
  whiteSpace
  loads <- many load
  funcs <- many function
  eof
  case funcs of
    [] -> pure $ Module loads [fromList []]
    v -> pure $ Module loads v
