module Interactive.REPL where

----------------Modules ------------------------------------------ù

import Control.Monad.IO.Class (MonadIO)
import Data.Map (Map, toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Interactive.Commands
import Language.Noc.PrettyPrinter (displayStack)
import Language.Noc.Runtime.Eval
import Language.Noc.Runtime.Internal
import Language.Noc.Runtime.Prelude (prelude)
import Language.Noc.Syntax.AST
import Language.Noc.Syntax.Lexer
import System.Console.Haskeline
import System.Directory (XdgDirectory (..), getXdgDirectory)
import Text.Parsec
import Text.Parsec.String (Parser)

----------------------- REPL Session -----------------------------
defaultSettings' :: MonadIO m => FilePath -> Settings m
defaultSettings' path =
  Settings
    { complete = completeFilename,
      historyFile = Just path,
      autoAddHistory = True
    }

tryAction :: String -> InputT IO (Maybe String)
tryAction name = withInterrupt loop
  where
    loop =
      handle
        (\Interrupt -> outputStrLn "KeyboardInterrupt" >> loop)
        (getInputLine name)

prompt :: String -> IO [String]
prompt name = do
  path <- getXdgDirectory XdgCache ".noc_history"
  input <- runInputT (defaultSettings' path) (tryAction $ name <> " ")
  return $ words $ fromMaybe "" input

readMultiline :: [String] -> IO [String]
readMultiline l = do
  inp <- prompt "noc|"
  case inp of
    [":}"] -> return l
    content -> readMultiline (l <> content)

nocREPL :: Stack -> Env -> IO ()
nocREPL stack env = (prompt "noc>") >>= repl stack env

----------------- REPL Commands  -----------------------------------

run :: String -> [(String, REPLCommands)] -> Stack -> Env -> IO ()
run name funcs stack env = case name `elem` (map fst funcs) of
  True -> do
    let (Just f) = lookup name funcs
    let (REPLCommands _ _ action) = f
    action
  False -> (putStrLn ("Unknown command '" ++ name ++ "' or lack of arguments")) >> (nocREPL stack env)

repl :: Stack -> Env -> [String] -> IO ()
repl stack env [] = nocREPL stack env
repl stack env [":{"] = readMultiline [] >>= (repl stack env)
repl stack env ((s : cmd) : args) = case s of
  ':' -> run cmd (commands args stack env nocREPL) stack env
  _ -> case (s : cmd) `elem` reservedWords of
    True -> run (s : cmd) (commands (if args == [] then ["\"\""] else args) stack env nocREPL) stack env
    False -> do
      let code = ((s : cmd) : args)
      let expression = unwords code
      let parsed = parseREPL expression
      case parsed of
        (Left err) -> (print err) >> nocREPL stack env
        (Right succ) -> case succ of
          ---
          (ExprInput []) -> (putStrLn ("Noc doesn't recognize '" ++ expression ++ "' expression.")) >> nocREPL stack env
          (DeclInput decl) -> case funcREPL decl env of
            ---
            (Left err') -> print err' >> nocREPL stack env
            (Right ((), newenv)) -> nocREPL stack newenv
          ---
          (ExprInput expr) -> do
            eval' <- exprREPL expr stack env
            case eval' of
              ---
              (Left err'') -> print err'' >> nocREPL stack env
              (Right ((), s, ())) -> (putStrLn $ displayStack s) >> (nocREPL s env)
