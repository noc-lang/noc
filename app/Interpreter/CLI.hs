module Interpreter.CLI where

----------------------- Modules ------------------------

import Control.Monad.Except
import Control.Monad.RWS
import Data.List (foldl')
import qualified Data.Map as M (Map, empty, fromList, keys, toList, union)
import qualified Data.Text as T (Text, unpack, unwords)
import Interactive.REPL (nocREPL)
import Interpreter.Commands
import Interpreter.Utils
import Language.Noc.Runtime.Eval (evalFile)
import Language.Noc.Runtime.Internal
import Language.Noc.Runtime.Prelude (prelude)
import Language.Noc.Syntax.AST
import Options.Applicative (Parser, ParserInfo, empty, fullDesc, header, helper, info, (<|>))
import qualified Text.Parsec.String as P (parseFromFile)
import Data.Version (showVersion)
import qualified Paths_noc as PN (version)

----------------- CLI Parser ----------------------------

opts :: ParserInfo Command
opts = info (helper <*> cmd) (fullDesc <> header "noc - User-friendly stack-based concatenative language.")

run :: Command -> IO ()
run Version = putStrLn $ "Noc version " <> (showVersion PN.version)
run Repl = nocREPL [] M.empty
run (Exec path) = do
  parse <- P.parseFromFile program path
  ---
  case parse of
    (Left err) -> print err
    ---
    (Right (Module imports decls)) -> case isMultipleDecls $ concatMap M.keys decls of
      Just k -> print $ NameError $ "Multiple function declarations for '" <> (T.unpack k) <> "' function."
      ---
      Nothing -> case filter (\k -> k `elem` (M.keys prelude)) (M.keys $ unionMap decls) of
        [] -> do
          let (main', other') = filterProg (unionMap decls)
          ---
          let otherFuncsMap = M.fromList $ map (\(k, v) -> (k, Function v)) other'
          evalFile' <- runExceptT $ runRWST (evalFile main') (prelude <> otherFuncsMap) []
          ---
          case evalFile' of
            (Left err') -> print err'
            (Right _) -> return ()
        (x : _) -> print $ NameError $ "cannot declare the function with '" <> (T.unpack x) <> "' name. (reserved to prelude)"

cmd :: Parser Command
cmd = foldl' (<|>) empty cmdFuncs
