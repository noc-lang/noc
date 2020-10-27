module Interactive.REPL where

import Language.Noc.Runtime.Eval 
import Language.Noc.Runtime.Internal (Values)
import Language.Noc.Syntax.AST (parseNoc,REPLProgram)
import Interactive.Commands
import System.IO (hFlush, stdout)

type Env = [String]

--------------- Utils ----------------------------------
nocREPL :: Values -> Env -> IO ()
nocREPL stack env = prompt >>= (repl stack env)

-- flushing stdout buffer before calling getLine
prompt :: IO [String]
prompt = putStr "noc> " >> hFlush stdout >> (getLine >>= (return . words))

---------------------------------------------------------
cmd :: String -> [(String, REPLCommands)] -> IO ()
cmd name funcs = action
                where (Just f) = lookup name funcs
                      (REPLCommands _ _ action) = f

run :: String -> [(String, REPLCommands)] -> Values -> Env -> IO ()
run name funcs stack env = case name `elem` (map fst funcs) of
                            True  -> cmd name funcs
                            False -> putStrLn ("Unknown command '" ++ name ++ "' or lack of arguments")

----------------- REPL function ---------------------------
repl :: Values -> Env -> [String] -> IO ()
repl stack env [] = nocREPL stack env
repl stack env  [(':' : cmd)] = (run cmd singleCommands stack env) >> nocREPL stack env
repl stack env ((':':cmd):args) = (run cmd (commandsArgs args) stack env) >> nocREPL stack env
repl stack env code = do
                    let parsed = parseNoc $ unwords $ code
                    case parsed of
                        (Left err) -> (print err) >> nocREPL stack env
                        (Right succ) -> (print succ) >> nocREPL stack env