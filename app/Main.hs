module Main where

import Interpreter.CLI (opts, run)
import Options.Applicative (execParser)

main :: IO ()
main = run =<< (execParser opts)
