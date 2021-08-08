module Language.Noc.Runtime.Lib.Seq where

import Control.Monad.Except (throwError)
import Control.Monad.RWS
import qualified Data.Map as M (fromList)
import qualified Data.Text as T (pack)
import Language.Noc.Runtime.Internal
import Language.Noc.Runtime.PreludeDoc
import Language.Noc.Syntax.AST

----------------------------------------------------

seq' :: Env
seq' =
  M.fromList
    [ (T.pack "unquote", Constant $ (docUnquote, PrimVal builtinUnquote)),
      (T.pack "pushr", Constant $ (docPushr, PrimVal builtinPushr)),
      (T.pack "popr", Constant $ (docPopr, PrimVal builtinPopr)),
      (T.pack "step", Constant $ (docStep, PrimVal builtinStep)),
      (T.pack "fold", Constant $ (docFold, PrimVal builtinFold))
    ]

----------------------------------------------------

builtinUnquote :: Eval ()
builtinUnquote = do
  v1 <- pop
  case v1 of
    ((QuoteVal x)) -> evalExpr x
    _ -> throwError $ TypeError "can only unquote with a quotation."

----------------------------------------------------

builtinPushr :: Eval ()
builtinPushr = do
  v <- pop
  l <- pop
  case l of
    (QuoteVal l') -> push $ QuoteVal (l' <> [readAtom v])
    _ -> throwError $ TypeError "can only pushr with a quotation."

----------------------------------------------------

builtinPopr :: Eval ()
builtinPopr = do
  env <- ask
  v1 <- pop
  case v1 of
    ((QuoteVal x)) -> case reverse x of
      [] -> return ()
      ((WordAtom y) : ys) -> (push $ QuoteVal $ reverse ys) >> (evalWord y env)
      (y : ys) -> (push $ QuoteVal $ reverse ys) >> (push $ readValue y)
    _ -> throwError $ TypeError "can only popr with a quotation."

runStep :: Expr -> Int -> Expr -> Eval ()
runStep _ len [] = do
  stack <- get
  let origin = take (length stack - len) stack
  let new = QuoteVal $ map readAtom (reverse $ take len $ reverse stack)
  put $ origin <> [new]
runStep f len (x : xs) = (evalExpr $ [x] <> f) >> runStep f len xs

builtinStep :: Eval ()
builtinStep = do
  f <- pop
  l <- pop
  case f of
    (QuoteVal f') -> case l of
      (QuoteVal l') -> runStep f' (length l') l'
      _ -> throwError $ TypeError "step: the first parameter must be a quote."
    _ -> throwError $ TypeError "step: the second parameter must be a quote."

----------------------------------------------------

runFold :: Expr -> Expr -> Expr -> Eval ()
runFold _ acc [] = evalExpr acc
runFold f acc (x : xs) = do
  let app = acc <> [x] <> f
  evalExpr app
  v <- pop
  runFold f [readAtom v] xs

builtinFold :: Eval ()
builtinFold = do
  f <- pop
  acc <- pop
  l <- pop
  let acc' = [readAtom acc]
  case f of
    (QuoteVal f') -> case l of
      (QuoteVal l') -> runFold f' acc' l'
      _ -> throwError $ TypeError "fold: the first parameter must be a quote."
    _ -> throwError $ TypeError "fold: the third parameter must be a quote."
