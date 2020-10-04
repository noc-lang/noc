module CLI.CLI where

import CLI.Commands
import Data.List (foldl')
import Language.Noc.Runtime.Eval
import Language.Noc.Syntax.AST (parseNoc, parseNocFile)
import Options.Applicative
import REPL.REPL (nocREPL)

opts = info (cmd <**> helper) (fullDesc <> header "noc - User-friendly stack-based concatenative language.")

---------------------------------------------------
run :: Command -> IO ()
run Version = putStrLn "Noc v0.1 https://github.com/noc-lang/noc"
run (Exec path) = (parseNocFile path) >>= print
run Repl = nocREPL []

cmd :: Parser Command
cmd = foldl' (<|>) empty cmdFuncs
