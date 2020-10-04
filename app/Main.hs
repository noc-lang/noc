module Main where

import CLI.CLI (opts, run)
import Options.Applicative (execParser)

main :: IO ()
main = run =<< (execParser opts)
