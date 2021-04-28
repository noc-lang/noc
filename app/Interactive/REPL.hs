module Interactive.REPL where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromMaybe)
import Interactive.Commands
import Language.Noc.Runtime.Eval
import Language.Noc.Runtime.Internal
import Language.Noc.Syntax.AST (REPLInput (..), parseNoc)
import System.Console.Haskeline
import System.Directory (XdgDirectory (..), getXdgDirectory)
import System.IO (hFlush, stdout)

--------------- Utils ----------------------------------
nocREPL :: IO ()
nocREPL = prompt >>= (repl)

defaultSettings' :: MonadIO m => FilePath -> Settings m
defaultSettings' path =
  Settings
    { complete = completeFilename,
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
  where
    (Just f) = lookup name funcs
    (REPLCommands _ _ action) = f

run :: String -> [(String, REPLCommands)] -> IO ()
run name funcs = case name `elem` (map fst funcs) of
  True -> cmd name funcs
  False -> putStrLn ("Unknown command '" ++ name ++ "' or lack of arguments")

----------------- REPL function ---------------------------
repl :: [String] -> IO ()
repl [] = nocREPL
repl [(':' : cmd)] = (run cmd singleCommands) >> nocREPL
repl ((':' : cmd) : args) = (run cmd (commandsArgs args)) >> nocREPL
repl code = do
  let expression = unwords $ code
  let parsed = parseNoc $ expression
  case parsed of
    (Left err) -> (print err) >> nocREPL
    (Right succ) -> case succ of
      (ExprInput []) -> (putStrLn ("Noc doesn't recognize '" ++ expression ++ "' expression.")) >> nocREPL
      _ -> (print succ) >> nocREPL
