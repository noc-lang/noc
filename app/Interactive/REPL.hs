module Interactive.REPL where

import System.Console.Haskeline
import System.Directory (getXdgDirectory, XdgDirectory (..))
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (MonadIO)
import Language.Noc.Runtime.Eval 
import Language.Noc.Runtime.Internal
import Language.Noc.Syntax.AST (parseNoc,REPLProgram,REPLInput (..))
import Interactive.Commands
import System.IO (hFlush, stdout)

type Stack = [String]
type Env = [String]

--------------- Utils ----------------------------------
nocREPL :: Stack -> Env -> IO ()
nocREPL stack env = prompt >>= (repl stack env)

defaultSettings' :: MonadIO m => FilePath -> Settings m 
defaultSettings' path = Settings {
    complete = completeFilename,
    historyFile = Just path,
    autoAddHistory = True
}

prompt :: IO [String]
prompt = do
            path <- getXdgDirectory XdgCache ".noc_history"
            input <- runInputT (defaultSettings' path) (getInputLine "noc> ")
            return $ words $ fromMaybe "" input

---------------------------------------------------------
cmd :: String -> [(String, REPLCommands)] -> IO ()
cmd name funcs = action
                where (Just f) = lookup name funcs
                      (REPLCommands _ _ action) = f

run :: String -> [(String, REPLCommands)] -> Stack -> Env -> IO ()
run name funcs stack env = case name `elem` (map fst funcs) of
                            True  -> cmd name funcs
                            False -> putStrLn ("Unknown command '" ++ name ++ "' or lack of arguments")

----------------- REPL function ---------------------------
repl :: Stack -> Env -> [String] -> IO ()
repl stack env [] = nocREPL stack env
repl stack env  [(':' : cmd)] = (run cmd singleCommands stack env) >> nocREPL stack env
repl stack env ((':':cmd):args) = (run cmd (commandsArgs args) stack env) >> nocREPL stack env
repl stack env code = do
                    let expression = unwords $ code
                    let parsed = parseNoc $ expression
                    case parsed of
                        (Left err) -> (print err) >> nocREPL stack env
                        (Right succ) -> case succ of
                                            (ExprInput []) -> (putStrLn ("Noc doesn't recognize '" ++ expression ++ "' expression.")) >> nocREPL stack env
                                            _ -> (print succ) >> nocREPL stack env