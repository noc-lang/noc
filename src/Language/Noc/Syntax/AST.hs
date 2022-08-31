{-# LANGUAGE FlexibleContexts #-}

module Language.Noc.Syntax.AST where

import Data.Char (digitToInt)
import Data.Map (Map, fromList, keys)
import Data.Text (Text, empty, pack, strip, unpack)
import Language.Noc.Syntax.Lexer
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

data Constant = IntConst Integer | FloatConst Double | StringConst String | CharConst Char | BoolConst Bool deriving (Show,Eq)

data Atom = QuoteAtom Expr | WordAtom String | ConstAtom Constant deriving (Show, Eq)

type Expr = [Atom]

--------------------- Utils ----------------------------

sign :: Num a => Parser (a -> a)
sign = (char '-' >> return negate) <|> return id

number' :: Integer -> Parser Char -> Parser Integer
number' base baseDigit = do
  digits <- many1 baseDigit
  let n = foldl (\x d -> base * x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

---- Escape char for double quote chars
doubleQuoteLiteral :: Parser String
doubleQuoteLiteral = between (char '"') (char '"') (many $ doubleQuoteLetter <|> doubleQuoteEscape)

doubleQuoteLetter :: Parser Char
doubleQuoteLetter = satisfy (\c -> (c /= '"') && (c /= '\\'))

doubleQuoteEscape :: Parser Char
doubleQuoteEscape = do
  char '\\'
  esc <- escapeCode
  return esc

escapeCode :: Parser Char
escapeCode = charEsc <|> charNum <|> charAscii <|> charControl

charEsc :: Parser Char
charEsc = do
  let codes = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
  esc <- choice $ map (\(c, code) -> char c >> return code) codes
  return esc

charNum :: Parser Char
charNum = do
  code <- decimal <|> (char 'o' >> number' 8 octDigit) <|> (char 'x' >> number' 16 hexDigit)
  if code > 0x10FFFF
    then fail "invalid escape sequence"
    else return $ toEnum $ fromInteger code

charAscii :: Parser Char
charAscii = choice (map parseAscii asciiMap)
  where
    parseAscii (asc, code) = try (do _ <- string asc; return code)
    -- escape code tables
    asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)
    ascii3codes = ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "SUB", "ESC", "DEL"]
    ascii3 = ['\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK', '\BEL', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB', '\CAN', '\SUB', '\ESC', '\DEL']
    ascii2codes = ["BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", "EM", "FS", "GS", "RS", "US", "SP"]
    ascii2 = ['\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\EM', '\FS', '\GS', '\RS', '\US', '\SP']

charControl :: Parser Char
charControl = do
  _ <- char '^'
  code <- upper
  return $ toEnum $ fromEnum code - fromEnum 'A' + 1

--------------------------------------------------------

word :: Parser Atom
word = WordAtom <$> (identifier <|> operator)

strLiteral :: Parser Atom
strLiteral = ConstAtom <$> StringConst <$> doubleQuoteLiteral

chrLiteral :: Parser Atom
chrLiteral = ConstAtom <$> CharConst <$> charLiteral

int :: Parser Atom
int = do
  f <- sign
  n <- natural
  pure $ ConstAtom <$> IntConst $ f n

number :: Parser Atom
number = do
  f <- sign
  n <- float
  pure $ ConstAtom <$> FloatConst $ f n

bool :: Parser Atom
bool = do
  v <- (string "True" <|> string "False")
  case v of
    "True" -> pure $ ConstAtom $ BoolConst True
    "False" -> pure $ ConstAtom $ BoolConst False

quote :: Parser Atom
quote = QuoteAtom <$> (brackets stack)

stack :: Parser Expr
stack = many $ lexeme (quote <|> bool <|> (try number) <|> (try int) <|> strLiteral <|> chrLiteral <|> word)

-------------------- Module -----------------

type Functions = Map Text (Maybe DocString, Expr)

data Module = Module {main :: FilePath, imports :: [FilePath], decls :: Functions} deriving Show

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

function :: Parser (Text, (Maybe DocString, Expr))
function = do
  lexeme $ reserved "def"
  name <- parseNameWithParens <|> parseName
  contentFunction <- (lexeme $ braces $ parseContent)
  let (doc, content) = contentFunction
  pure (pack name, (doc, content))

load :: Parser String
load = do
  lexeme $ reserved $ "load"
  path <- lexeme $ (stringLiteral <|> (many $ noneOf "\n\r"))
  pure path

program :: FilePath -> Parser Module
program path = do
  whiteSpace
  loads <- many load
  funcs <- many function
  eof
  pure $ Module path loads (fromList funcs)

parseNocFile :: FilePath -> IO (Either ParseError Module)
parseNocFile path = parseFromFile (program path) path