module Interactive.Commands where

----------------Modules -----------------------------

import System.Console.Haskeline.History (emptyHistory, writeHistory)
import System.Directory (XdgDirectory (..), getXdgDirectory)
import Language.Noc.Runtime.Internal
import Language.Noc.Syntax.AST (program, Declaration (..), Program)
import Control.Monad.RWS
import Control.Monad.Except
import qualified Data.Map as M (empty, toList)
import Language.Noc.PrettyPrinter (displayEnv)
import Text.Parsec (ParseError)
import Text.Parsec.String (parseFromFile)
import Data.Text (pack)
import qualified Data.Map as M (fromList)
import Control.Exception (try,SomeException)

----------------------------------------------------
data REPLCommands = REPLCommands {name :: String, args :: [String], action :: IO ()}

commands :: [String] -> Stack -> Env -> (Stack -> Env -> IO ()) -> [(String, REPLCommands)]
commands args stack env repl = [
  ("load", load args stack env repl), 
  ("quit", quit args stack env repl),
  ("help", help args stack env repl),
  ("env", env' args stack env repl),
  ("reset", reset args stack env repl)]

----------------------------------------------------

quit :: [String] -> Stack -> Env -> (Stack -> Env -> IO ()) -> REPLCommands
quit arg _ _ _ =
  REPLCommands
    { name = "quit",
      args = arg,
      action = do
        putStrLn "Leaving Noc REPL."
        path <- getXdgDirectory XdgCache ".noc_history"
        writeHistory path emptyHistory
    }

----------------------------------------------------

help :: [String] -> Stack -> Env -> (Stack -> Env -> IO ()) -> REPLCommands
help arg stack env repl =
  REPLCommands
    { name = "help",
      args = arg,
      action = do
        putStrLn $ unlines
          [ "Commands available from the prompt:\n",
                ":quit | Exit REPL.",
                ":load [filepath] | Load Noc file.",
                ":reset | Resetting global stack.",
                ":env | Show environment."
          ]
        repl stack env
    }

----------------------------------------------------

load :: [String] -> Stack -> Env -> (Stack -> Env -> IO ()) -> REPLCommands
load arg stack env repl =
  REPLCommands
    { name = "load",
      args = arg,
      action = do
        parsed <- try $ parseFromFile program (unwords arg) :: IO (Either SomeException (Either ParseError Program))
        case parsed of
          (Left err) -> (print err) >> (repl stack env)
          (Right succ) -> case succ of
            (Left err') -> do
              putStrLn ("'" ++ (unwords arg) ++ "' is not loaded.")
              print err'
              repl stack env
            (Right succ') -> do
              let succMap = M.fromList $ map (\(Declaration name expr) -> (pack name, Function expr)) succ'
              putStrLn ("'" ++ (unwords arg) ++ "' is loaded.")
              repl stack (env <> succMap)
    }

----------------------------------------------------

env' :: [String] -> Stack -> Env -> (Stack -> Env -> IO ()) -> REPLCommands
env' arg stack env repl = 
  REPLCommands
    { name = "env",
      args = arg,
      action = do
        let environment = foldl (\acc (name, Function expr) -> (displayEnv name expr) <> acc) "" (M.toList env)
        putStrLn environment
        repl stack env
    }

----------------------------------------------------

reset :: [String] -> Stack -> Env -> (Stack -> Env -> IO ()) -> REPLCommands
reset arg _ _ repl = 
  REPLCommands
    { name = "reset",
      args = arg,
      action = do
        (putStrLn "Resetting stack and env...")
        repl [] (M.empty)
    }