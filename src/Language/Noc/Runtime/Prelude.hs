{-# LANGUAGE RankNTypes #-}

module Language.Noc.Runtime.Prelude where

import Control.Monad.RWS
import Control.Monad.State
import Language.Noc.Runtime.Internal
import Language.Noc.Syntax.AST
import Control.Monad.Except (throwError)
import qualified Data.Text as T (pack,unpack,Text)
import qualified Data.Map as M (fromList)
import Control.Exception (try, SomeException)
import qualified Data.Text.IO as TIO (readFile)

----------------------------------------------------

prelude :: Env 
prelude = M.fromList [
  (T.pack "dup", Constant $ PrimVal builtinDup),
  (T.pack "pop", Constant $ PrimVal builtinPop),
  (T.pack "popr", Constant $ PrimVal builtinPopr),
  (T.pack "zap", Constant $ PrimVal builtinZap),
  (T.pack "cat", Constant $ PrimVal builtinCat),
  (T.pack "+", Constant $ PrimVal $ builtinOp (+)),
  (T.pack "-", Constant $ PrimVal $ builtinOp (-)),
  (T.pack "*", Constant $ PrimVal $ builtinOp (*)),
  (T.pack "/", Constant $ PrimVal $ builtinDiv),
  (T.pack "rotN", Constant $ PrimVal builtinRotN),
  (T.pack "unquote", Constant $ PrimVal builtinUnquote),
  (T.pack "print", Constant $ PrimVal builtinPrint),
  (T.pack "read", Constant $ PrimVal builtinReadFile)]

----------------------------------------------------

builtinOp :: (forall a. Num a => a -> a -> a) -> Eval ()
builtinOp operator = do
    v1 <- pop
    v2 <- pop
    case (v1,v2) of
        ((FloatVal v1'),(FloatVal v2')) -> push $ FloatVal $ operator v2' v1'
        ((IntVal v1'),(IntVal v2')) -> push $ IntVal $ operator v2' v1'
        ((FloatVal v1'),(IntVal v2')) -> push $ FloatVal $ operator (fromIntegral v2') v1'
        ((IntVal v1'),(FloatVal v2')) -> push $ FloatVal $ operator v2' (fromIntegral v1')
        _ -> throwError $ TypeError "cannot operate with different types."

----------------------------------------------------

operateDiv v1 v2 = case v1 of
    0 -> throwError $ ZeroDivisionError $ "cannot divide by 0."
    _ -> push $ FloatVal $ (/) v2 v1

builtinDiv :: Eval ()
builtinDiv = do
    v1 <- pop
    v2 <- pop
    case (v1,v2) of
        ((FloatVal v1'),(FloatVal v2')) -> operateDiv v1' v2' 
        ((IntVal v1'),(IntVal v2')) -> operateDiv (fromIntegral v1') (fromIntegral v2')
        ((FloatVal v1'),(IntVal v2')) -> operateDiv v1' (fromIntegral v2') 
        ((IntVal v1'),(FloatVal v2')) -> operateDiv (fromIntegral v1') v2'

        _ -> throwError $ TypeError "cannot operate with different types."
    
----------------------------------------------------

builtinDup :: Eval ()
builtinDup = do
    v1 <- pop
    push v1 >> push v1
    
----------------------------------------------------

builtinPop :: Eval ()
builtinPop = do
    v1 <- pop
    return ()

----------------------------------------------------

builtinZap :: Eval ()
builtinZap = put [] >> return ()

----------------------------------------------------

builtinCat :: Eval ()
builtinCat = do
    v1 <- pop
    v2 <- pop
    case (v1,v2) of
        ((QuoteVal a),(QuoteVal b)) -> push $ (QuoteVal $ b ++ a)
        ((StringVal a),(StringVal b)) -> push $ (StringVal $ b <> a)
        _ -> throwError $ TypeError "cannot cat with different types or concat functions,floats."

----------------------------------------------------

builtinRotN :: Eval ()
builtinRotN = do
    n <- pop
    case n of
        (FloatVal x) -> do
            stack <- get
            let rot = take (truncate x) $ reverse stack
            put $ (initN (truncate x) stack) <> rot
        _ -> throwError $ TypeError "the parameter isn't a float."

----------------------------------------------------

builtinUnquote :: Eval ()
builtinUnquote = do
    v1 <- pop
    case v1 of
        ((QuoteVal x)) -> evalExpr x
        _ -> throwError $ TypeError "can only unquote with a quotation."
            
----------------------------------------------------

builtinPopr :: Eval ()
builtinPopr = do
    env <- ask
    v1 <- pop
    case v1 of
        ((QuoteVal x)) -> case reverse x of
            ((WordAtom y):ys) -> (push $ QuoteVal $ reverse ys) >> (evalWord y env)
            (y:ys) -> (push $ QuoteVal $ reverse ys) >> (push $ readValue y)
        _ -> throwError $ TypeError "can only popr with a quotation."

----------------------------------------------------

builtinPrint :: Eval ()
builtinPrint = do
    v <- pop
    case v of
        (StringVal x) -> (liftIO $ print x) >> return ()
        (FloatVal x) -> (liftIO $ print x) >> return ()
        (IntVal x) -> (liftIO $ print x) >> return ()
        _ -> throwError $ TypeError "can only print with strings,floats."

----------------------------------------------------

builtinReadFile :: Eval ()
builtinReadFile = do
    path <- pop
    case path of
        (StringVal x) -> do
                            content <- liftIO (try $ TIO.readFile (T.unpack x) :: IO (Either SomeException T.Text))
                            case content of
                                (Left err) -> throwError $ FileNotFoundError "the file does not exist (no such file or directory)"
                                (Right succ) -> (push $ StringVal succ) >> return () 
        _ -> throwError $ TypeError "the parameter is not string."
    