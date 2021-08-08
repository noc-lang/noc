module Interpreter.CLI where

----------------------- Modules ------------------------

import Data.List (foldl')
import qualified Data.Map as M (empty)
import Data.Version (showVersion)
import Interactive.REPL (nocREPL)
import Interpreter.Commands
import Interpreter.Utils
import Language.Noc.PrettyPrinter (displayStack)
import Options.Applicative (Parser, ParserInfo, empty, fullDesc, header, helper, info, (<|>))
import qualified Paths_noc as PN (version)

----------------- CLI Parser ----------------------------

opts :: ParserInfo Command
opts = info (helper <*> cmd) (fullDesc <> header "noc - A user-friendly concatenative language.")

run :: Command -> IO ()
run Version = putStrLn $ "Noc version " <> (showVersion PN.version)
run Repl = nocREPL [] M.empty
run (WriteStack (path : _)) = do
  v <- runModule path
  case v of
    Nothing -> return ()
    (Just s) -> putStrLn $ "---\nStack: \n" <> displayStack s
run (Exec (path : _)) = do
  v <- runModule path
  case v of
    _ -> return ()

cmd :: Parser Command
cmd = foldl' (<|>) empty cmdFuncs
