module Language.Noc.Eval (eval) where

import Language.Noc.Parser (Atom (FloatAtom, QuoteAtom, WordAtom), ParseError, Stack,Program)

data Value = QuoteValue Stack | FloatValue Float deriving (Show)
type Values = [Value]
-------------------

eval :: Show a => Either ParseError a -> IO ()
eval res = case res of
  Right a -> print a
  Left err -> print err

-- ASTs
-- [Declaration]      -> File 
-- Expression [Atom]  -> REPL      -> Stack globale
-- Decl (Declaration) -> REPL      -> Pas dans la stack globale