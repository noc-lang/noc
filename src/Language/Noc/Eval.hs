module Language.Noc.Eval (eval) where

import Language.Noc.Internal
import Language.Noc.Parser (Atom (FloatAtom, QuoteAtom, WordAtom), ParseError, Stack)

eval :: Either ParseError Stack -> IO ()
eval res = case res of
  Right a -> print a
  Left err -> print err
