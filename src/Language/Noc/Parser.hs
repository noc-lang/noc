module Language.Noc.Parser
  ( parseNoc,
    parseNocFile,
    ParseError,
    Stack,
    Atom (QuoteAtom, WordAtom, FloatAtom),
  )
where

-- Modules
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Language.Noc.Lexer

-- AST
data Atom = QuoteAtom Stack | WordAtom String | FloatAtom Double deriving (Show, Eq)
type Stack = [Atom]

-------- Parser combinators ------------
word :: Parser Atom
word = WordAtom <$> identifier

number :: Parser Atom
number = do
          content <- naturalOrFloat
          pure $ case content of
            Left i -> FloatAtom $ (read (show i) :: Double)
            Right f -> FloatAtom f

quote :: Parser Atom
quote = QuoteAtom <$> between (symbol "[") (symbol "]") stack
----------------------------------------
stack :: Parser Stack
stack = many $ lexeme (try number <|> word <|> quote)

parseNoc :: String -> Either ParseError Stack
parseNoc expr = parse (whiteSpace *> stack <* eof) "" expr

parseNocFile :: String -> IO (Either ParseError Stack)
parseNocFile path = parseFromFile (whiteSpace *> stack <* eof) path
