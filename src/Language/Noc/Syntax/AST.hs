module Language.Noc.Syntax.AST where

----------------------- Modules ------------------------

import Data.Map (Map, fromList, keys)
import Data.Text (Text, empty, pack)
import Language.Noc.Syntax.Lexer
import Text.Parsec
import Text.Parsec.String (Parser)

----------------------- Atoms --------------------------
data Atom = QuoteAtom Expr | WordAtom String | IntAtom Integer | FloatAtom Double | StringAtom String | BoolAtom Bool deriving (Show, Eq)

type Expr = [Atom]

--------------------- Utils ----------------------------

sign :: Num a => Parser (a -> a)
sign = (char '-' >> return negate) <|> return id

operators :: Parser String
operators = string "+" <|> string "-" <|> string "/" <|> string "*"

--------------------------------------------------------

word :: Parser Atom
word = WordAtom <$> (identifier <|> operators)

strLiteral :: Parser Atom
strLiteral = StringAtom <$> (stringLiteral <|> (between (string "'") (string "'") (many $ noneOf "'")))

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

-------------------- Module -----------------

data Module = Module {imports :: [FilePath], decls :: [Map Text (DocString, Expr)]}

type DocString = String

parseContent :: Parser (DocString, Expr)
parseContent = do
  doc <- optionMaybe $ between (symbol "---") (symbol "---") (many $ noneOf "-")
  e <- whiteSpace *> stack 
  case doc of
    (Just x) -> return (x,e)
    Nothing -> return ([],e)

function :: Parser (Map Text (DocString, Expr))
function = do
  lexeme $ reserved "def"
  name <- lexeme $ (:) <$> (letter <|> char '_') <*> (manyTill (alphaNum <|> char '\'' <|> char '_') (whiteSpace >> symbol "="))
  contentFunction <- (lexeme $ braces $ parseContent)
  let (doc,content) = contentFunction
  pure $ fromList [(pack name, (doc,content))]

load :: Parser String
load = do
  lexeme $ reserved $ "load"
  path <- lexeme $ stringLiteral
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