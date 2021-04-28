module Interpreter.CLI where

----------------------- Modules --------------------------------------------------

import Data.List (foldl')
import Interactive.REPL (nocREPL)
import Interpreter.Commands
import Language.Noc.Runtime.Eval
import Language.Noc.Syntax.AST (parseNoc, parseNocFile)
import Options.Applicative

-----------------------------------------------------------------------------------
opts = info (helper <*> cmd) (fullDesc <> header "noc - User-friendly stack-based concatenative language.")

---------------------------------------------------
run :: Command -> IO ()
run Version = putStrLn "Noc version 1.0"
run Repl = nocREPL
run (Exec path) = (parseNocFile path) >>= (either print (print . eval))

cmd :: Parser Command
cmd = foldl' (<|>) empty cmdFuncs
