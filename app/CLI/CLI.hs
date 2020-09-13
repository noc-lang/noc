module CLI.CLI where

import REPL.REPL (nocREPL)
import Language.Noc.Eval (eval)
import Language.Noc.Parser (parseNocFile, parseNoc)
import CLI.Commands
import Data.List (foldl')
import Options.Applicative 

opts = info (cmd <**> helper) (fullDesc <> header "noc - User-friendly stack-based concatenative language.")
---------------------------------------------------
run :: Command -> IO ()
run Version = putStrLn "Noc v0.1"
run (Exec path) = (parseNocFile path) >>= eval
run Repl = nocREPL

cmd :: Parser Command
cmd = foldl' (<|>) empty cmdFuncs
