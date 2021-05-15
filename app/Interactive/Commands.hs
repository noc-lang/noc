module Interactive.Commands where

----------------Modules -----------------------------

import Control.Exception (SomeException, try)
import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.Map as M (Map, empty, fromList, keys, toList)
import Data.Text (Text, pack, unpack)
import Language.Noc.PrettyPrinter (displayEnv)
import Language.Noc.Runtime.Internal
import Language.Noc.Runtime.Prelude (prelude)
import Language.Noc.Syntax.AST (Expr, Module (..), program)
import System.Console.Haskeline.History (emptyHistory, writeHistory)
import System.Directory (XdgDirectory (..), getXdgDirectory)
import Text.Parsec (ParseError)
import Text.Parsec.String (parseFromFile)

----------------------------------------------------
data REPLCommands = REPLCommands {name :: String, args :: [String], action :: IO ()}

commands :: [String] -> Stack -> Env -> (Stack -> Env -> IO ()) -> [(String, REPLCommands)]
commands args stack env repl =
  [ ("load", load args stack env repl),
    ("quit", quit args stack env repl),
    ("help", help args stack env repl),
    ("env", env' args stack env repl),
    ("reset", reset args stack env repl)
  ]

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
        putStrLn $
          unlines
            [ "Commands available from the prompt:\n",
              ":quit | Exit REPL and clear the .noc_history file",
              ":load [filepath] | Load Noc file.",
              ":reset | Reset stack and env.",
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
        parsed <- try $ parseFromFile program (unwords arg) :: IO (Either SomeException (Either ParseError Module))
        case parsed of
          ---
          (Left err) -> (print err) >> (repl stack env)
          ---
          (Right succ) -> case succ of
            ---
            (Left err') -> (putStrLn ("'" ++ (unwords arg) ++ "' is not loaded.")) >> (print err') >> (repl stack env)
            ---
            (Right succ') -> do
              ---
              let (Module imports decls) = succ'
              let succ'' = M.toList decls
              ---
              let succMap l = M.fromList $ map (\(k, v) -> (k, Function v)) l
              ---
              let name = map (\(k, v) -> k) succ''
              ---
              let envFiltered = filter (\(k, v) -> k `elem` M.keys env) succ''
              let preludeFiltered = filter (\(k, v) -> k `elem` M.keys prelude) succ''
              ---
              case (length preludeFiltered >= 1, length envFiltered >= 1) of
                (True, _) -> (print $ "cannot declare the function with " <> (unpack $ fst $ head preludeFiltered) <> " name. (reserved to prelude)") >> repl stack env
                ---
                (False, True) -> do
                  let otherFuncs = filter (\(k, v) -> not $ k `elem` M.keys env) succ''
                  putStrLn ("'" ++ (unwords arg) ++ "' is reloaded.")
                  repl stack (succMap otherFuncs <> succMap envFiltered)
                ---
                (False, False) -> (putStrLn ("'" ++ (unwords arg) ++ "' is loaded.")) >> (repl stack (env <> succMap succ''))
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
