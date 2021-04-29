{-# LANGUAGE FlexibleContexts #-}

module Language.Noc.Syntax.Lexer where

----------------------- Modules --------------------------------------------------

import Text.Parsec (alphaNum, noneOf, oneOf, (<|>))
import Text.Parsec.Prim (ParsecT, Stream)
import qualified Text.Parsec.Token as P

----------------- Lexer definition ------------------------------------------------

lexer :: Stream s m Char => P.GenTokenParser s u m
lexer =
  P.makeTokenParser $
    P.LanguageDef
      { P.commentStart = "/*",
        P.commentEnd = "*/",
        P.commentLine = "#",
        P.nestedComments = False,
        P.identStart = alphaNum <|> oneOf "+-/*_",
        P.identLetter = alphaNum <|> oneOf "+-/*'_",
        P.opStart = (noneOf ""),
        P.opLetter = (noneOf ""),
        P.reservedNames = ["def"],
        P.reservedOpNames = [],
        P.caseSensitive = False
      }

----------------- Lexer functions --------------------------------------------------
naturalOrFloat :: Stream s m Char => ParsecT s u m (Either Integer Double)
naturalOrFloat = P.naturalOrFloat lexer

brackets :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
brackets = P.brackets lexer

braces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
braces = P.braces lexer

identifier :: Stream s m Char => ParsecT s u m String
identifier = P.identifier lexer

whiteSpace :: Stream s m Char => ParsecT s u m ()
whiteSpace = P.whiteSpace lexer

lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme = P.lexeme lexer

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol = P.symbol lexer

reserved :: Stream s m Char => String -> ParsecT s u m ()
reserved = P.reserved lexer

stringLiteral :: Stream s m Char => ParsecT s u m String
stringLiteral = P.stringLiteral lexer
