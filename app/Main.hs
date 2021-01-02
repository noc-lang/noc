module Main where

import Interpreter.CLI (opts, run)
import Options.Applicative (execParser)

import Language.Noc.Syntax.AST (parseNoc)
import Language.Noc.Runtime.Eval (eval)

main :: IO ()
main = run =<< (execParser opts)