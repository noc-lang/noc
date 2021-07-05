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
    (1, [QuoteAtom y]) -> case all isChar y && y /= [] of
      True -> (push $ QuoteVal y) >> builtinSugar
      False -> push $ QuoteVal y
    (1, [expr']) -> push (readValue expr')
    _ -> evalExpr expr

---------- Evaluate Noc file -----------------

evalFile :: [(T.Text, (Maybe DocString, Expr))] -> Eval ()
evalFile [] = return ()
evalFile [(_, (_, v))] = evalExpr v

------- Evaluate function declaration ---------

evalFunc :: [(T.Text, (Maybe DocString, Expr))] -> DeclEval ()
evalFunc [(k, decl)] = do
  env <- get
  let function = M.fromList [(k, Function decl)]
  ---
  case (k `elem` (M.keys prelude), k `elem` (M.keys env)) of
    (True, _) -> throwError $ NameError $ "cannot declare the function with '" <> (T.unpack k) <> "' name. (reserved to prelude)"
    (_, True) -> (put $ (M.delete k env) <> function) >> return ()
    _ -> (put $ env <> function) >> return ()
