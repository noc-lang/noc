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
    (_, [], _) -> throwError $ MainError $ "the main function not found."
    (_, [(_, expr)], _) -> evalExpr expr

------- Evaluate function declaration ---------

evalFunc :: [(T.Text, Expr)] -> DeclEval ()
evalFunc func = do
  env <- get
  let [(k, v)] = func
  let funcName = k
  let function = M.fromList [(funcName, Function v)]

  case (funcName `elem` (M.keys prelude), funcName `elem` (M.keys env)) of
    (True, _) -> throwError $ NameError $ "cannot declare the function with " <> (T.unpack funcName) <> " name. (reserved to prelude)"
    (False, True) -> (put $ (M.delete funcName env) <> function) >> return ()
    (False, False) -> (put $ env <> function) >> return ()
