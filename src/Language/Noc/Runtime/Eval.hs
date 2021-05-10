module Language.Noc.Runtime.Eval where

-------------- Modules -----------------------

import Control.Monad.RWS
import Control.Monad.Reader (ask)
import qualified Data.Map as M (empty,fromList,union,lookup,delete,keys)
import qualified Data.Text as T (pack,unpack)
import Control.Monad.Except (throwError)
import Data.Foldable (traverse_)
import Language.Noc.Runtime.Internal
import Language.Noc.Runtime.Prelude
import Language.Noc.Syntax.AST

-------------------------------

eval :: Expr -> Eval ()
eval expr = do
  stack <- get
  env <- ask

  case (length expr, expr) of
    (1,[WordAtom w]) -> evalWord w env
    (1,[expr']) -> push (readValue expr')
    _ -> evalExpr expr 

-------------------------------

evalFile :: [Declaration] -> Eval ()
evalFile mainfunc = do
  env <- ask
  case (length mainfunc > 1, mainfunc, env) of
    (_, [],_) -> throwError $ MainError $ "the main function not found."
    (True, _, _) -> throwError $ MainError $ "there are multiple main functions."
    (False,[Declaration name expr],otherf) -> evalExpr expr

-------------------------------

evalFunc :: Declaration -> DeclEval ()
evalFunc func = do
  env <- get
  let funcName = T.pack $ declName func
  let function = (M.fromList [(funcName, Function $ declVal func)])

  case (funcName `elem` (M.keys prelude), funcName `elem` (M.keys env)) of
    (True,_) -> throwError $ NameError $ "cannot declare the function with " <> (T.unpack funcName) <> " name. (reserved to prelude)"
    (False,True) -> (put $ (M.delete funcName env) <> function) >> return ()
    (False,False) -> (put $ env <> function) >> return ()