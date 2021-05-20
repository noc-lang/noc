module Language.Noc.Runtime.Eval where

-------------- Modules -----------------------

import Control.Monad.Except (throwError)
import Control.Monad.RWS
import Control.Monad.Reader (ask)
import Data.Foldable (traverse_)
import qualified Data.Map as M (Map, delete, empty, fromList, keys, lookup, toList, union)
import qualified Data.Text as T (Text, pack, unpack)
import Language.Noc.Runtime.Internal
import Language.Noc.Runtime.Prelude
import Language.Noc.Syntax.AST

--------------- Utils -----------------------

isUnknown :: [T.Text] -> [T.Text] -> Atom -> Bool
isUnknown p e x = case x of
  (WordAtom y) -> (not $ T.pack y `elem` p) && (not $ T.pack y `elem` e) -- e -> env or declaration function of a file.
  _ -> False

-------- Evaluate expressions ----------------

eval :: Expr -> Eval ()
eval expr = do
  stack <- get
  env <- ask

  case (length expr, expr) of
    (1, [WordAtom w]) -> evalWord w env
    (1, [expr']) -> push (readValue expr')
    _ -> evalExpr expr

---------- Evaluate Noc file -----------------

evalFile :: [(T.Text, Expr)] -> Eval ()
evalFile mainfunc = do
  env <- ask

  case (length mainfunc > 1, mainfunc, env) of
    (_, [], _) -> liftIO $ print "the main function not found."
    (_, [(_, expr)], _) -> evalExpr expr

------- Evaluate function declaration ---------

evalFunc :: [(T.Text, Expr)] -> DeclEval ()
evalFunc func = do
  env <- get
  let [(k, v)] = func
  let funcName = k
  let function = M.fromList [(funcName, Function v)]
  ---
  let isUnknown' = any (isUnknown (M.keys prelude) (M.keys env)) v
  let unknownFunctions = filter (isUnknown (M.keys prelude) (M.keys env)) v
  ---
  case (funcName `elem` (M.keys prelude), funcName `elem` (M.keys env), (isUnknown', unknownFunctions)) of
    (True, _, _) -> throwError $ NameError $ "cannot declare the function with " <> (T.unpack funcName) <> " name. (reserved to prelude)"
    (_, _, (True, u)) -> do
      let (WordAtom w) = head u
      throwError $ NameError $ "cannot declare '" <> (T.unpack funcName) <> "' function, the function '" <> w <> "' is not declared"
    (False, True, _) -> (put $ (M.delete funcName env) <> function) >> return ()
    (False, False, _) -> (put $ env <> function) >> return ()
