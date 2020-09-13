module Main where

import Options.Applicative (execParser)
import CLI.CLI (run,opts)

main :: IO ()
main = run =<< (execParser opts)