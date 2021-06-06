{-# LANGUAGE RankNTypes #-}

module Language.Noc.Runtime.Prelude where

import Control.Exception (SomeException, try)
import Control.Monad.Except (throwError)
import Control.Monad.RWS
import Control.Monad.State
import qualified Data.Map as M (fromList, keys)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.IO as TIO (getLine, readFile)
import Language.Noc.Runtime.Internal
import Language.Noc.PrettyPrinter
import Language.Noc.Syntax.AST
import System.IO
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import Text.Read (readMaybe)

----------------------------------------------------

prelude :: Env
prelude =
  M.fromList
    [ -- Stack-shuffler
      (T.pack "dup", Constant $ PrimVal builtinDup),
      (T.pack "pop", Constant $ PrimVal builtinPop),
      (T.pack "zap", Constant $ PrimVal builtinZap),
      (T.pack "cat", Constant $ PrimVal builtinCat),
      (T.pack "rotN", Constant $ PrimVal builtinRotN),
      -- Arithmetic operators
      (T.pack "+", Constant $ PrimVal $ builtinOp (+)),
      (T.pack "-", Constant $ PrimVal $ builtinOp (-)),
      (T.pack "*", Constant $ PrimVal $ builtinOp (*)),
      (T.pack "/", Constant $ PrimVal $ builtinDiv),
      -- I/O
      (T.pack "print", Constant $ PrimVal builtinPrint),
      (T.pack "putstr", Constant $ PrimVal builtinPutStr),
      (T.pack "ask", Constant $ PrimVal builtinAsk),
      (T.pack "args", Constant $ PrimVal builtinArgs),
      -- Fs
      (T.pack "read", Constant $ PrimVal builtinReadFile),
      (T.pack "write", Constant $ PrimVal builtinWrite),
      -- Quote
      (T.pack "unquote", Constant $ PrimVal builtinUnquote),
      (T.pack "pushr", Constant $ PrimVal builtinPushr),
      (T.pack "popr", Constant $ PrimVal builtinPopr),
      -- Misc
      (T.pack "id", Constant $ PrimVal builtinId),
      (T.pack "str", Constant $ PrimVal builtinStr),
      (T.pack "int", Constant $ PrimVal builtinInt),
      (T.pack "float", Constant $ PrimVal builtinFloat),
      (T.pack "exit", Constant $ PrimVal builtinExit)
    ]

----------------------------------------------------

builtinOp :: (forall a. Num a => a -> a -> a) -> Eval ()
builtinOp operator = do
  v1 <- pop
  v2 <- pop
  case (v1, v2) of
    ((FloatVal v1'), (FloatVal v2')) -> push $ FloatVal $ operator v2' v1'
    ((IntVal v1'), (IntVal v2')) -> push $ IntVal $ operator v2' v1'
    ((FloatVal v1'), (IntVal v2')) -> push $ FloatVal $ operator (fromIntegral v2') v1'
    ((IntVal v1'), (FloatVal v2')) -> push $ FloatVal $ operator v2' (fromIntegral v1')
    _ -> throwError $ TypeError "cannot operate with different types."

----------------------------------------------------

builtinDiv :: Eval ()
builtinDiv = do
  v1 <- pop
  v2 <- pop
  case (v1, v2) of
    ((FloatVal v1'), (FloatVal v2')) -> operateDiv v1' v2'
    ((IntVal v1'), (IntVal v2')) -> operateDiv (fromIntegral v1') (fromIntegral v2')
    ((FloatVal v1'), (IntVal v2')) -> operateDiv v1' (fromIntegral v2')
    ((IntVal v1'), (FloatVal v2')) -> operateDiv (fromIntegral v1') v2'
    _ -> throwError $ TypeError "cannot operate with different types."
  where
    operateDiv v1 v2 = case v1 of
      0 -> throwError $ ZeroDivisionError $ "cannot divide by 0."
      _ -> push $ FloatVal $ (/) v2 v1

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
  case (v1, v2) of
    ((QuoteVal a), (QuoteVal b)) -> push $ (QuoteVal $ b ++ a)
    ((StringVal a), (StringVal b)) -> push $ (StringVal $ b <> a)
    _ -> throwError $ TypeError "cannot cat with different types or concat functions,floats."

----------------------------------------------------

builtinRotN :: Eval ()
builtinRotN = do
  n <- pop
  case n of
    (IntVal x) -> do
      stack <- get
      let n = fromIntegral x
      let rot = take n $ reverse stack
      put $ (initN n stack) <> rot
    _ -> throwError $ TypeError "the parameter isn't an int."

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
      ((WordAtom y) : ys) -> (push $ QuoteVal $ reverse ys) >> (evalWord y env)
      (y : ys) -> (push $ QuoteVal $ reverse ys) >> (push $ readValue y)
    _ -> throwError $ TypeError "can only popr with a quotation."

----------------------------------------------------

builtinPrint :: Eval ()
builtinPrint = do
  v <- pop
  case v of
    (StringVal x) -> (liftIO $ print x) >> return ()
    (FloatVal x) -> (liftIO $ print x) >> return ()
    (IntVal x) -> (liftIO $ print x) >> return ()
    (BoolVal x) -> (liftIO $ print x) >> return ()
    (QuoteVal x) -> (liftIO $ putStrLn $ displayQuote x)
    _ -> throwError $ TypeError "can only print with strings,floats,integers,bool."

----------------------------------------------------

builtinPutStr :: Eval ()
builtinPutStr = do
  v <- pop
  case v of
    (StringVal x) -> (liftIO $ putStr $ T.unpack $ x) >> return ()
    _ -> throwError $ TypeError "can only putstr with strings."

----------------------------------------------------

builtinReadFile :: Eval ()
builtinReadFile = do
  path <- pop
  case path of
    (StringVal x) -> do
      content <- liftIO (try $ TIO.readFile (T.unpack x) :: IO (Either SomeException T.Text))
      case content of
        (Left err) -> liftIO $ print "the file does not exist (no such file or directory)"
        (Right succ) -> push $ StringVal succ
    _ -> throwError $ TypeError "the parameter is not string."

----------------------------------------------------

builtinAsk :: Eval ()
builtinAsk = do
  msg <- pop
  case msg of
    (StringVal x) -> do
      liftIO $ putStr $ T.unpack x
      liftIO $ hFlush stdout
      inp <- liftIO $ TIO.getLine
      push $ StringVal inp
    _ -> throwError $ TypeError "the parameter is not string."

----------------------------------------------------

builtinArgs :: Eval ()
builtinArgs = do
  args <- liftIO getArgs
  case args of
    [] -> push $ QuoteVal []
    (_:y:args)  -> case y of
      "--" -> push $ QuoteVal (map StringAtom args)
      _ -> push $ QuoteVal (map StringAtom (y:args))
    _ -> push $ QuoteVal (map StringAtom args)

----------------------------------------------------

builtinPushr :: Eval ()
builtinPushr = do
  v <- pop
  l <- pop
  case l of
    (QuoteVal l') -> push $ QuoteVal (l' <> [readAtom v])
    _ -> throwError $ TypeError "can only pushr with a quotation."

----------------------------------------------------

builtinWrite :: Eval ()
builtinWrite = do
  content <- pop
  path <- pop
  case path of
    (StringVal p) -> case content of
      (StringVal c) -> liftIO $ writeFile (T.unpack p) (T.unpack c)
      _ -> throwError $ TypeError "the first parameter is not string."
    _ -> throwError $ TypeError "the second parameter is not string."

----------------------------------------------------

builtinId :: Eval ()
builtinId = do
  v <- pop
  push v

----------------------------------------------------

builtinStr :: Eval ()
builtinStr = do
  v <- pop
  case v of
    (StringVal x) -> push $ StringVal x
    (FloatVal x) -> push $ StringVal $ T.pack $ show x
    (IntVal x) -> push $ StringVal $ T.pack $ show x
    (BoolVal x) -> push $ StringVal $ T.pack $ show x
    (QuoteVal x) -> push $ StringVal $ T.pack $ displayQuote x
    _ -> throwError $ TypeError "can only str with str,float,int,bool"

----------------------------------------------------

builtinInt :: Eval ()
builtinInt = do
  v <- pop
  case v of
    (IntVal x) -> push $ IntVal x
    (StringVal x) -> case readMaybe (T.unpack x) :: Maybe Integer of
      (Just v) -> push $ IntVal v
      Nothing -> throwError $ ValueError "the value is not a integer."
    _ -> throwError $ TypeError "can only int with int,str"

----------------------------------------------------

builtinFloat :: Eval ()
builtinFloat = do
  v <- pop
  case v of
    (IntVal x) -> push $ FloatVal $ fromIntegral x
    (FloatVal x) -> push $ FloatVal x
    (StringVal x) -> case readMaybe (T.unpack x) :: Maybe Double of
      (Just v) -> push $ FloatVal v
      Nothing -> throwError $ ValueError "the value is not a integer."
    _ -> throwError $ TypeError "can only float with int,float,str"

----------------------------------------------------

builtinExit :: Eval ()
builtinExit = do
  typeExit <- pop
  case typeExit of
    (StringVal x) -> case (T.unpack x) of 
      "success" -> liftIO exitSuccess
      "failure" -> liftIO $ (putStrLn "*** Exception: ExitFailure") >> exitFailure
      _ -> throwError $ TypeError "the exit parameter is wrong."
    _ -> throwError $ TypeError "the exit parameter is not string." 

    