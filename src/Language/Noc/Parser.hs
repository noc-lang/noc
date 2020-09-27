module Language.Noc.Parser
  ( parseNoc,
    parseNocFile,
    ParseError,
    Stack,
    Atom (QuoteAtom, WordAtom, FloatAtom),
    Program,
    Declaration(..)
  )
where

----------------------- Modules --------------------------------------------------
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Language.Noc.Lexer

----------------------- Atoms Parser ----------------------------------------------
data Atom = QuoteAtom Stack | WordAtom String | FloatAtom Double deriving (Show,Eq)
type Stack = [Atom]

word :: Parser Atom
word = WordAtom <$> identifier

number :: Parser Atom
number = FloatAtom . either fromIntegral id <$> naturalOrFloat

quote :: Parser Atom
quote = QuoteAtom <$> (brackets stack)

stack :: Parser Stack
stack = many $ lexeme (quote <|> number <|> word)

----------------------- Declaration Parser ------------------------------------------
data Declaration = Declaration {declName :: String, declVal :: [Stack]} deriving (Show,Eq)
type Program = [Declaration]

function :: Parser Declaration
function = do
            lexeme $ reserved "def"
            name <- lexeme $ manyTill alphaNum (whiteSpace >> symbol "=")
            content <- (lexeme $ braces $ (whiteSpace *> stack))
            pure $ Declaration name [content]

program :: Parser Program
program = whiteSpace *> (many function) <* eof

----------------------- REPL Parser -------------------------------------------------
data REPLInput = Decl Declaration | Expression Stack deriving Show

replFunction :: Parser REPLInput
replFunction = function >>= (pure . Decl)

replExpression :: Parser REPLInput
replExpression = (whiteSpace *> stack) >>= (pure . Expression)
  
----------------------- Parse Functions ----------------------------------------------
parseNoc :: String -> Either ParseError REPLInput
parseNoc expr = parse (try replFunction <|> replExpression) "" expr

parseNocFile :: String -> IO (Either ParseError Program)
parseNocFile path = parseFromFile program path