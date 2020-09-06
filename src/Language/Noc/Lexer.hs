{-# LANGUAGE FlexibleContexts #-}

module Language.Noc.Lexer where

-- Modules
import Text.Parsec (noneOf,oneOf,alphaNum)
import Control.Applicative ((<|>))
import Text.Parsec.Prim (Stream, ParsecT)
import qualified Text.Parsec.Token as P

-- Lexer definition
lexer :: Stream s m Char => P.GenTokenParser s u m
lexer = P.makeTokenParser $ P.LanguageDef {
  P.commentStart = "/*",
  P.commentEnd = "*/",
  P.commentLine = "#",
  P.nestedComments = False,
  P.identStart = alphaNum <|> oneOf "+-/*",
  P.identLetter = alphaNum <|> oneOf "+-/*",
  P.opStart = (noneOf ""),
  P.opLetter = (noneOf ""),
  P.reservedNames = [],
  P.reservedOpNames = [],
  P.caseSensitive = False
}

-- Lexer functions
naturalOrFloat :: Stream s m Char => ParsecT s u m (Either Integer Double)
naturalOrFloat = P.naturalOrFloat lexer

identifier :: Stream s m Char => ParsecT s u m String
identifier = P.identifier lexer

whiteSpace :: Stream s m Char => ParsecT s u m ()
whiteSpace = P.whiteSpace lexer

lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme = P.lexeme lexer

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol = P.symbol lexer
