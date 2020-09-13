module REPL.Commands where

import System.Exit (exitSuccess)
import System.Console.ANSI (clearScreen)

----------------------------------------------------
data REPLCommands = REPLCommands {name :: String, args :: [String], action :: IO ()}

singleCommands :: [(String, REPLCommands)]
singleCommands = [("quit", quit), ("help", help), ("clear",clear)]

commandsArgs :: [String] -> [(String,REPLCommands)]
commandsArgs args = [("load",load args), ("reload", reload args)]

----------------------------------------------------
quit :: REPLCommands
quit = REPLCommands {
    name = "quit",
    args = [],
    action = putStrLn "Leaving Noc REPL." >> exitSuccess
}

----------------------------------------------------

help :: REPLCommands
help = REPLCommands {
    name = "help",
    args = [],
    action = putStrLn "Commands available from the prompt:\n\n :quit                       Exit REPL.\n :load [filepath]            Load Noc file.\n :reload [filepath]          Reload Noc file\n :clear                      Clear terminal."
}

----------------------------------------------------

load :: [String] -> REPLCommands
load arg = REPLCommands {
    name = "load",
    args = arg,
    action = putStrLn ("'"++(unwords arg)++"' loaded.")
}

----------------------------------------------------

reload :: [String] -> REPLCommands
reload arg = REPLCommands {
    name = "reload",
    args = arg,
    action = putStrLn ("'"++(unwords arg)++"' reloaded.")
}
----------------------------------------------------

clear :: REPLCommands
clear = REPLCommands {
    name = "clear",
    args = [],
    action = clearScreen
}
----------------------------------------------------