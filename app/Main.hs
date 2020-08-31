module Main where

import Language.Noc.Eval (eval)
import Language.Noc.Parser
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

newinput = prompt >>= repl

-- flushing stdout buffer before calling getLine
prompt :: IO String
prompt = putStr "noc> " >> hFlush stdout >> getLine

----------------------------------------------------------------
repl :: String -> IO ()
repl "quit" = exitSuccess
repl c = (eval $ parseNoc c) >> newinput

nocfile :: String -> IO ()
nocfile p = (parseNocFile p) >>= eval

badArguments :: IO ()
badArguments = putStrLn "noc-language: Too many arguments."

----------------------------------------------------------------
command :: [String] -> IO ()
command [] = newinput -- no arguments
command [path] = nocfile path -- one argument
command _ = badArguments -- too many arguments

main :: IO ()
main = getArgs >>= command
