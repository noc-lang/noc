module Interactive.Commands where

----------------Modules -----------------------------

import System.Console.Haskeline.History (emptyHistory, writeHistory)
import System.Directory (XdgDirectory (..), getXdgDirectory)
import System.Exit (exitSuccess)

----------------------------------------------------

data REPLCommands = REPLCommands {name :: String, args :: [String], action :: IO ()}

singleCommands :: [(String, REPLCommands)]
singleCommands = [("quit", quit), ("help", help)]

commandsArgs :: [String] -> [(String, REPLCommands)]
commandsArgs args = [("load", load args), ("reload", reload args)]

----------------------------------------------------

quit :: REPLCommands
quit =
  REPLCommands
    { name = "quit",
      args = [],
      action = do
        putStrLn "Leaving Noc REPL."
        path <- getXdgDirectory XdgCache ".noc_history"
        writeHistory path emptyHistory
        exitSuccess
    }

----------------------------------------------------

help :: REPLCommands
help =
  REPLCommands
    { name = "help",
      args = [],
      action =
        putStrLn $
          unlines
            [ "Commands available from the prompt:\n",
              ":quit | Exit REPL.",
              ":load [filepath] | Load Noc file.",
              ":reload [filepath] | Reload Noc file.",
              ":reset | Resetting global stack.",
              ":env | Show environment."
            ]
    }

----------------------------------------------------

load :: [String] -> REPLCommands
load arg =
  REPLCommands
    { name = "load",
      args = arg,
      action = putStrLn ("'" ++ (unwords arg) ++ "' loaded.")
    }

----------------------------------------------------

reload :: [String] -> REPLCommands
reload arg =
  REPLCommands
    { name = "reload",
      args = arg,
      action = putStrLn ("'" ++ (unwords arg) ++ "' reloaded.")
    }
