module Interactive.Commands where

----------------Modules -----------------------------

import Control.Exception (SomeException, try)
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.State (runStateT)
import qualified Data.Map as M (Map, empty, fromList, keys, lookup, toList, union)
import qualified Data.Text as T (Text, pack, unpack, unwords)
import Interpreter.Utils
import Language.Noc.PrettyPrinter (displayEnv, displayStack)
import Language.Noc.Runtime.Eval
import Language.Noc.Runtime.Internal
import Language.Noc.Runtime.Prelude (prelude)
import qualified Language.Noc.Syntax.AST as A
import Language.Noc.Syntax.Lexer
import System.Console.Haskeline.History (emptyHistory, writeHistory)
import System.Directory (XdgDirectory (..), doesFileExist, getXdgDirectory, listDirectory)
import Text.Parsec (ParseError, eof, parse, (<|>))
import Text.Parsec.String (Parser, parseFromFile)

----------------------- REPL Parser -------------------------------

data REPLInput = DeclInput (M.Map T.Text (Maybe A.DocString, A.Expr)) | ExprInput A.Expr deriving (Show, Eq)

reservedWords :: [String]
reservedWords = ["load"]

replFunction :: Parser REPLInput
replFunction = (A.function <* eof) >>= (pure . DeclInput)

replExpression :: Parser REPLInput
replExpression = (whiteSpace *> A.stack) >>= (pure . ExprInput)

parseREPL :: String -> Either ParseError REPLInput
parseREPL expr = parse (replFunction <|> replExpression) "" expr

----------------------- REPL Eval -------------------------------

funcREPL :: (M.Map T.Text (Maybe A.DocString, A.Expr)) -> Env -> Either EvalError ((), Env)
funcREPL func env = runExcept $ runStateT (evalFunc $ M.toList func) env

exprREPL :: A.Expr -> Stack -> Env -> IO (Either EvalError ((), Stack, ()))
exprREPL expr stack env = runExceptT $ runRWST (eval expr) (prelude <> env) stack

----------------------------------------------------
data REPLCommands = REPLCommands {name :: String, args :: [String], action :: IO ()}

commands :: [String] -> Stack -> Env -> (Stack -> Env -> IO ()) -> [(String, REPLCommands)]
commands args stack env repl =
  [ ("load", load args stack env repl),
    ("quit", quit args stack env repl),
    ("help", help args stack env repl),
    ("env", env' args stack env repl),
    ("reset", reset args stack env repl),
    ("debug", debug args stack env repl)
  ]

-------- Utils -------------------------------------

hasDoubleQuoteChars :: [String] -> Bool
hasDoubleQuoteChars [] = False
hasDoubleQuoteChars arg = (head $ unwords arg) == '"' && (last $ unwords arg) == '"'

filename :: [String] -> String
filename [] = []
filename arg = case hasDoubleQuoteChars arg of
  True -> tail $ init $ unwords arg
  False -> unwords arg

isFunction :: (T.Text, EnvEntry) -> Bool
isFunction (_, x) = case x of
  (Function (_, expr)) -> True
  _ -> False

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
              ":env | Show environment.",
              ":debug | Debug a Noc expression."
            ]
        repl stack env
    }

----------------------------------------------------

loadFile :: FilePath -> FilePath -> Stack -> Env -> (Stack -> Env -> IO ()) -> IO ()
loadFile moduleName path stack env repl = do
  parsed <- parseImports [path] []
  case parsed of
    (Left err) -> putStrLn err >> repl stack env
    (Right succ) -> (putStrLn $ "The '" <> moduleName <> "' module is loaded.") >> repl stack (foldr M.union env succ)

load :: [String] -> Stack -> Env -> (Stack -> Env -> IO ()) -> REPLCommands
load arg stack env repl =
  REPLCommands
    { name = "load",
      args = arg,
      action = do
        let path = if (last $ reverse $ unwords arg) == '"' && (last $ unwords arg) == '"' then tail $ init $ unwords arg else unwords arg
        isSTDModule' <- isSTDModule path
        case ((snd $ isInternalModule path), isSTDModule') of
          (True, _) -> loadFile path path stack env repl
          (_, Just p) -> loadFile path p stack env repl
          _ -> do
            doesFileExist' <- doesFileExist path
            case doesFileExist' of
              True -> loadFile path path stack env repl
              False -> (putStrLn $ "noc: '" <> path <> "' openFile: does not exist (No such file or directory)") >> repl stack env
    }

----------------------------------------------------

env' :: [String] -> Stack -> Env -> (Stack -> Env -> IO ()) -> REPLCommands
env' arg stack env repl =
  REPLCommands
    { name = "env",
      args = arg,
      action = do
        let environment = foldl (\acc (name, Function (_, expr)) -> (displayEnv name expr) <> acc) "" (filter isFunction $ M.toList env)
        case environment == [] of
          True -> putStrLn "[]" >> repl stack env
          False -> putStrLn environment >> repl stack env
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

----------------------------------------------------

runDebug :: Stack -> Env -> A.Expr -> IO ()
runDebug _ _ [] = return ()
runDebug s e (x : xs) = case x of
  (A.WordAtom w) -> case M.lookup (T.pack w) e of
    (Just (Function (_, expr))) -> (putStrLn $ "(" <> w <> ") ") >> runDebug s e (expr <> xs)
    (Just (Constant (_, expr))) -> do
      eval' <- exprREPL [A.WordAtom w] s e
      putStrLn $ displayStack s <> " (" <> w <> ")"
      case eval' of
        (Left err) -> print err
        (Right ((), s', ())) -> (putStrLn $ displayStack s') >> runDebug s' e xs
    Nothing -> print (NameError (w <> " function doesn't declared or not in prelude."))
  _ -> do
    eval' <- exprREPL [x] s e
    case eval' of
      (Left err) -> print err
      (Right ((), s', ())) -> (putStrLn $ displayStack s') >> runDebug s' e xs

debug :: [String] -> Stack -> Env -> (Stack -> Env -> IO ()) -> REPLCommands
debug arg stack env repl =
  REPLCommands
    { name = "debug",
      args = arg,
      action = do
        let parsed = parse replExpression "" (unwords arg)
        case parsed of
          (Left err) -> (print err) >> repl stack env
          (Right succ) -> case succ of
            (ExprInput expr) -> (runDebug [] (prelude <> env) expr) >> repl stack env
    }
