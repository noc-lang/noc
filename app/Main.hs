module Main where

import Command.CLI (opts, run)
import Options.Applicative (execParser)

main :: IO ()
main = run =<< (execParser opts)
