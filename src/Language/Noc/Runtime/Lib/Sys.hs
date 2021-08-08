module Language.Noc.Runtime.Lib.Sys where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M (fromList)
import qualified Data.Text as T (pack)
import Language.Noc.Runtime.Internal
import Language.Noc.Runtime.PreludeDoc
import Language.Noc.Syntax.AST
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)

----------------------------------------------------

sys :: Env
sys =
  M.fromList
    [ (T.pack "exit", Constant $ (docExit, PrimVal builtinExit)),
      (T.pack "args", Constant $ (docArgs, PrimVal builtinArgs))
    ]

----------------------------------------------------

builtinExit :: Eval ()
builtinExit = do
  returncode <- pop
  case returncode of
    (IntVal x) -> case x of
      0 -> liftIO exitSuccess
      n -> liftIO $ (putStrLn $ "*** Exception: ExitFailure " <> show n) >> (exitWith $ ExitFailure $ fromIntegral n)
    _ -> throwError $ TypeError "the exit parameter must be an integer parameter."

----------------------------------------------------

builtinArgs :: Eval ()
builtinArgs = do
  args <- liftIO getArgs
  case args of
    [('-' : '-' : _), filename] -> push $ QuoteVal []
    (('-' : '-' : _) : _ : y : args) -> case y of
      "--" -> push $ QuoteVal (map StringAtom args)
      _ -> push $ QuoteVal (map StringAtom (y : args))
    -------------------------------
    [filename] -> push $ QuoteVal []
    (_ : y : args) -> case y of
      "--" -> push $ QuoteVal (map StringAtom args)
      _ -> push $ QuoteVal (map StringAtom (y : args))
    _ -> push $ QuoteVal (map StringAtom args)
