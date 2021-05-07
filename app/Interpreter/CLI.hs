module Interpreter.CLI where

----------------------- Modules ------------------------

import Data.List (foldl')
import qualified Data.Map as M (empty)
import Interactive.REPL (nocREPL)
import Interpreter.Commands
import Language.Noc.Syntax.AST
import Language.Noc.Syntax.Lexer
import Options.Applicative (Parser, empty, fullDesc, header, helper, info, (<|>))
import Text.Parsec (ParseError, eof, many)
import qualified Text.Parsec.String as P

---------------- Interpreter's parser -------------------

program :: P.Parser Program
program = whiteSpace *> (many function) <* eof

parseNocFile :: String -> IO (Either ParseError Program)
parseNocFile path = P.parseFromFile program path

----------------- CLI Parser ----------------------------

opts = info (helper <*> cmd) (fullDesc <> header "noc - User-friendly stack-based concatenative language.")

run :: Command -> IO ()
run Version = putStrLn "Noc version 1.0"
run Repl = nocREPL [] (M.empty)
run (Exec path) = (parseNocFile path) >>= (either print print)

cmd :: Parser Command
cmd = foldl' (<|>) empty cmdFuncs
