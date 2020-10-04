module REPL.REPL where

import Language.Noc.Eval 
import Language.Noc.Internal (Values)
import Language.Noc.Parser (parseNoc)
import REPL.Commands
import System.IO (hFlush, stdout)

--------------- Utils ----------------------------------
nocREPL :: Values -> IO ()
nocREPL stack = prompt >>= (repl stack)

-- flushing stdout buffer before calling getLine
prompt :: IO [String]
prompt = putStr "noc> " >> hFlush stdout >> (getLine >>= (return . words))

isCommand :: [String] -> Bool
isCommand cmd = case (head cmd) of
  (':' : xs) -> True
  _ -> False

---------------------------------------------------------
commandOne :: String -> IO ()
commandOne cmd = getAction f
  where
    (Just f) = lookup cmd singleCommands

commandsWithArgs :: String -> [String] -> IO ()
commandsWithArgs cmd args = getAction f
  where
    (Just f) = lookup cmd (commandsArgs args)

----------------- Getter ---------------------------------
getAction :: REPLCommands -> IO ()
getAction (REPLCommands _ _ action) = action

----------------- REPL function ---------------------------
repl :: Values -> [String] -> IO ()
repl stack [] = nocREPL stack
repl stack [(':' : command)] = case command `elem` map fst singleCommands of
  True -> do
    case command of
      "reset" -> (commandOne command) >> nocREPL []
      _ -> (commandOne command) >> nocREPL stack
  False -> (putStrLn ("Unknown command '" ++ command ++ "' or lack of arguments")) >> nocREPL stack
repl stack commands = case (isCommand commands) of
  True -> case cmd' `elem` map fst (commandsArgs xs) of
    True -> (commandsWithArgs cmd' xs) >> nocREPL stack
    False -> (putStrLn ("Unknown command '" ++ x ++ "' or lack of arguments")) >> nocREPL stack
    where
      (x : xs) = commands
      (y : cmd') = x
  False -> do
    let parsed = parseNoc $ unwords $ commands
    case parsed of
      (Left err) -> (print err) >> nocREPL stack
      (Right succ) -> (print succ) >> nocREPL stack
