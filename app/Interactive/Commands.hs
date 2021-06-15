module Interactive.Commands where

----------------Modules -----------------------------

import Control.Exception (SomeException, try)
import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.Map as M (Map, empty, fromList, keys, toList, union)
import qualified Data.Text as T (unpack, unwords)
import Interpreter.Utils
import Language.Noc.PrettyPrinter (displayEnv)
import Language.Noc.Runtime.Eval
import Language.Noc.Runtime.Internal
import Language.Noc.Runtime.Prelude (prelude)
import Language.Noc.Syntax.AST (Atom (..), Expr, Module (..), program)
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
        parse <- try $ parseFromFile program (unwords arg) :: IO (Either SomeException (Either ParseError Module))
        case parse of
          (Left errPath) -> (print errPath) >> (repl stack env)
          ---
          (Right succ) -> case succ of
            (Left errParse) -> (print errParse) >> (repl stack env)
            ---
            (Right (Module imports decls)) -> case isMultipleDecls $ map (T.unwords . M.keys) decls of
              (Just k) -> (print $ NameError $ "Cannot load '" <> (unwords arg) <> "' file, multiple function declarations for '" <> (T.unpack k) <> "' function.") >> repl stack env
              Nothing -> do
                let declList = M.toList $ foldr M.union M.empty decls
                ---
                let declMap l = M.fromList $ map (\(k, d) -> (k, Function d)) l
                let filter' e l = filter (\(k, _) -> k `elem` M.keys e) l
                ---
                let fnames = map (\(k, v) -> k) declList
                ---
                let envFilter = filter' env declList
                let preludeFilter = filter' prelude declList
                ---
                case (length preludeFilter > 0, length envFilter > 0) of
                  (True, _) -> do
                    let ((n, _) : xs) = preludeFilter
                    (print $ NameError $ "cannot declare the function with '" <> (T.unpack n) <> "' name. (reserved to prelude)") >> repl stack env
                  ---
                  (_, True) -> do
                    let otherFuncs = filter (\(k, v) -> not $ k `elem` M.keys env) declList
                    putStrLn ("'" ++ (unwords arg) ++ "' is reloaded.")
                    repl stack (declMap otherFuncs <> declMap envFilter)
                  ---
                  _ -> (putStrLn ("'" ++ (unwords arg) ++ "' is loaded.")) >> (repl stack (env <> declMap declList))
    }

----------------------------------------------------

env' :: [String] -> Stack -> Env -> (Stack -> Env -> IO ()) -> REPLCommands
env' arg stack env repl =
  REPLCommands
    { name = "env",
      args = arg,
      action = do
        let environment = foldl (\acc (name, Function (_,expr)) -> (displayEnv name expr) <> acc) "" (M.toList env)
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
