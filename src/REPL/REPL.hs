module REPL.REPL where

import System.IO (hFlush, stdout)
import Language.Noc.Eval (eval)
import Language.Noc.Parser (parseNoc)
import REPL.Commands

--------------- Utils ----------------------------------
nocREPL :: IO ()
nocREPL = prompt >>= repl

-- flushing stdout buffer before calling getLine
prompt :: IO [String]
prompt = putStr "noc> " >> hFlush stdout >> (getLine >>= (return . words))

isCommand :: [String] -> Bool
isCommand cmd = case (head cmd) of
                        (':':xs) -> True
                        _ -> False

---------------------------------------------------------
commandOne :: String -> IO ()
commandOne cmd = getAction f
                    where (Just f) = lookup cmd singleCommands

commandsWithArgs :: String -> [String] -> IO ()
commandsWithArgs cmd args = getAction f
                        where (Just f) = lookup cmd (commandsArgs args)

----------------- Getter ---------------------------------
getAction :: REPLCommands -> IO ()
getAction (REPLCommands _ _ action) = action

----------------- REPL function ---------------------------
repl :: [String] -> IO ()
repl [] = nocREPL

repl [(':':command)] = case command `elem` map fst singleCommands of
                        True  -> (commandOne command) >> nocREPL
                        False -> (putStrLn ("Unknown command '"++command++"' or lack of arguments")) >> nocREPL

repl commands = case (isCommand commands) of
                    True -> case cmd' `elem` map fst (commandsArgs xs) of
                                True -> (commandsWithArgs cmd' xs) >> nocREPL
                                False -> (putStrLn ("Unknown command '"++x++"' or lack of arguments")) >> nocREPL
                            where (x:xs) = commands
                                  (y:cmd') = x
                    False -> (eval $ parseNoc $ unwords $ commands) >> nocREPL
                    