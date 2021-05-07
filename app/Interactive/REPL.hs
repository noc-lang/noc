module Interactive.REPL where

----------------Modules ------------------------------------------ù

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS
import Control.Monad.State (runStateT)
import qualified Data.Map as M (empty, toList)
import Data.Maybe (fromMaybe)
import Interactive.Commands
import Language.Noc.PrettyPrinter (displayEnv, displayStack)
import Language.Noc.Runtime.Eval
import Language.Noc.Runtime.Internal
import Language.Noc.Syntax.AST
import Language.Noc.Syntax.Lexer
import System.Console.Haskeline
import System.Directory (XdgDirectory (..), getXdgDirectory)
import System.IO (hFlush, stdout)
import Text.Parsec (ParseError, eof, parse, (<|>))
import Text.Parsec.String (Parser)

----------------------- REPL Parser -------------------------------

data REPLInput = DeclInput Declaration | ExprInput Expr deriving (Show, Eq)

replFunction :: Parser REPLInput
replFunction = (function <* eof) >>= (pure . DeclInput)

replExpression :: Parser REPLInput
replExpression = (whiteSpace *> stack) >>= (pure . ExprInput)

parseREPL :: String -> Either ParseError REPLInput
parseREPL expr = parse (replFunction <|> replExpression) "" expr

----------------------- REPL Eval -------------------------------

funcREPL :: Declaration -> Env -> Either EvalError ((), Env)
funcREPL decl env = runExcept $ runStateT (evalFunc decl) env

exprREPL :: Expr -> Stack -> Either EvalError ((), Stack, ())
exprREPL expr stack = runExcept $ runRWST (eval expr) prelude stack

----------------------- Utils -----------------------------------
nocREPL :: Stack -> Env -> IO ()
nocREPL stack env = prompt >>= repl stack env

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

cmd :: String -> [(String, REPLCommands)] -> IO ()
cmd name funcs = action
  where
    (Just f) = lookup name funcs
    (REPLCommands _ _ action) = f

run :: String -> [(String, REPLCommands)] -> IO ()
run name funcs = case name `elem` (map fst funcs) of
  True -> cmd name funcs
  False -> putStrLn ("Unknown command '" ++ name ++ "' or lack of arguments")

----------------- REPL function -------------------------------------

repl :: Stack -> Env -> [String] -> IO ()
repl stack env [] = nocREPL stack env
repl stack env [":env"] = (putStrLn $ foldl (\acc (name, values) -> (displayEnv name values) <> acc) "" (M.toList env)) >> nocREPL stack env
repl stack env [":reset"] = (putStrLn "Resetting stack and env...") >> nocREPL [] (M.empty)
repl stack env [(':' : cmd)] = (run cmd singleCommands) >> nocREPL stack env
repl stack env ((':' : cmd) : args) = (run cmd (commandsArgs args)) >> nocREPL stack env
repl stack env code = do
  let expression = unwords code
  let parsed = parseREPL expression
  case parsed of
    (Left err) -> (print err) >> nocREPL stack env
    (Right succ) -> case succ of
      ---
      (ExprInput []) -> (putStrLn ("Noc doesn't recognize '" ++ expression ++ "' expression.")) >> nocREPL stack env
      (DeclInput func) -> case funcREPL func env of
        ---
        (Left err') -> print err' >> nocREPL stack env
        (Right ((), newenv)) -> nocREPL stack newenv
      (ExprInput expr) -> case exprREPL expr stack of
        ---
        (Left err'') -> print err'' >> nocREPL stack env
        (Right ((), s, ())) -> (putStrLn $ displayStack s) >> (nocREPL s env)