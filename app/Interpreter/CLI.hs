module Interpreter.CLI where

----------------------- Modules ------------------------

import Interpreter.Commands
import Language.Noc.Syntax.AST
import Language.Noc.Syntax.Lexer
import Interactive.REPL (nocREPL)
-----
import Data.List (foldl')
import Text.Parsec (ParseError,eof,many)
import qualified Text.Parsec.String as P
import Options.Applicative (info,helper,fullDesc,header,(<|>),empty,Parser)

---------------- Interpreter's parser -------------------

program :: P.Parser Program
program = whiteSpace *> (many function) <* eof

parseNocFile :: String -> IO (Either ParseError Program)
parseNocFile path = P.parseFromFile program path

----------------- CLI Parser ----------------------------

opts = info (helper <*> cmd) (fullDesc <> header "noc - User-friendly stack-based concatenative language.")

run :: Command -> IO ()
run Version = putStrLn "Noc version 1.0"
run Repl = nocREPL
run (Exec path) = (parseNocFile path) >>= (either print print)

cmd :: Parser Command
cmd = foldl' (<|>) empty cmdFuncs