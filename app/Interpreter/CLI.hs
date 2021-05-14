module Interpreter.CLI where

----------------------- Modules ------------------------

import Data.List (foldl')
import qualified Data.Map as M (empty,fromList)
import Interactive.REPL (nocREPL)
import Interpreter.Commands
import Control.Monad.RWS
import Control.Monad.Except
import Language.Noc.Runtime.Prelude (prelude)
import Language.Noc.Runtime.Eval
import Language.Noc.Runtime.Internal
import Language.Noc.Syntax.AST
import Language.Noc.Syntax.Lexer
import Language.Noc.Runtime.Eval (evalFile)
import Options.Applicative (Parser, empty, fullDesc, header, helper, info, (<|>))
import Language.Noc.PrettyPrinter (displayStack)
import Text.Parsec (ParseError, eof, many)
import Data.Text (Text, pack)
import Data.Map (Map,toList)
import qualified Text.Parsec.String as P

---------------- Interpreter's parser -------------------

parseNocFile :: String -> IO (Either ParseError Module)
parseNocFile path = P.parseFromFile program path

----------------- CLI Parser ----------------------------

opts = info (helper <*> cmd) (fullDesc <> header "noc - User-friendly stack-based concatenative language.")

run :: Command -> IO ()
run Version = putStrLn "Noc version 1.0"
run Repl = nocREPL [] (M.empty)
run (Exec path) = do
    parseFile <- parseNocFile path
    case parseFile of
        (Left err) -> print err
        (Right succ) -> do
            let (Module imports decls) = succ
            let succ' = toList decls
            ---
            let otherFuncs = filterProg (/=) succ'
            let mainFunction = filterProg (==) succ'
            ---
            let otherFuncsMap = M.fromList $ map (\(k,v) -> (k, Function v)) otherFuncs
            ---
            evalFile' <- runExceptT $ runRWST (evalFile mainFunction) (prelude <> otherFuncsMap) []
            ---
            case evalFile' of
                (Left err') -> print err'
                (Right _) -> return ()

cmd :: Parser Command
cmd = foldl' (<|>) empty cmdFuncs
