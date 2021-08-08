{-# LANGUAGE RankNTypes #-}

module Language.Noc.Runtime.Prelude where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS
import Control.Monad.State
import qualified Data.Map as M (Map, fromList, lookup)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.IO as TIO (getLine)
import Language.Noc.PrettyPrinter
import Language.Noc.Runtime.Internal
import Language.Noc.Runtime.Lib
import Language.Noc.Runtime.PreludeDoc
import Language.Noc.Syntax.AST
import System.IO
import Text.Read (readMaybe)

----------------------------------------------------

otherModules :: M.Map T.Text Env
otherModules =
  M.fromList
    [ (T.pack "fs", fs),
      (T.pack "char", char),
      (T.pack "str", str),
      (T.pack "sys", sys),
      (T.pack "seq", seq')
    ]

prelude :: Env
prelude =
  M.fromList
    [ -- Stack-shuffler
      (T.pack "dup", Constant $ (docDup, PrimVal builtinDup)),
      (T.pack "pop", Constant $ (docPop, PrimVal builtinPop)),
      (T.pack "zap", Constant $ (docZap, PrimVal builtinZap)),
      (T.pack "cat", Constant $ (docCat, PrimVal builtinCat)),
      (T.pack "rotNM", Constant $ (docRotNM, PrimVal builtinRotNM)),
      -- Arithmetic operators
      (T.pack "+", Constant $ (docOp "+", PrimVal $ builtinOp (+))),
      (T.pack "-", Constant $ (docOp "-", PrimVal $ builtinOp (-))),
      (T.pack "*", Constant $ (docOp "*", PrimVal $ builtinOp (*))),
      (T.pack "/", Constant $ (docDiv, PrimVal $ builtinDiv)),
      (T.pack "^", Constant $ (docPow, PrimVal $ builtinPow)),
      -- I/O
      (T.pack "print", Constant $ (docPrint, PrimVal builtinPrint)),
      (T.pack "putstr", Constant $ (docPutStr, PrimVal builtinPutStr)),
      (T.pack "putchar", Constant $ (docPutChar, PrimVal builtinPutChar)),
      (T.pack "ask", Constant $ (docAsk, PrimVal builtinAsk)),
      -- Boolean
      (T.pack ">", Constant $ (docBoolOp ">", PrimVal $ builtinBoolOp ">" (>))),
      (T.pack "<", Constant $ (docBoolOp "<", PrimVal $ builtinBoolOp "<" (<))),
      (T.pack ">=", Constant $ (docBoolOp ">=", PrimVal $ builtinBoolOp ">=" (>=))),
      (T.pack "<=", Constant $ (docBoolOp "<=", PrimVal $ builtinBoolOp "<=" (<=))),
      (T.pack "and", Constant $ (docBoolOp "and", PrimVal $ builtinCondBool "and" (&&))),
      (T.pack "or", Constant $ (docBoolOp "or", PrimVal $ builtinCondBool "or" (||))),
      -- Misc
      (T.pack "id", Constant $ (docId, PrimVal builtinId)),
      (T.pack "str", Constant $ (docStr, PrimVal builtinStr)),
      (T.pack "int", Constant $ (docInt, PrimVal builtinInt)),
      (T.pack "float", Constant $ (docFloat, PrimVal builtinFloat)),
      (T.pack "bool", Constant $ (docBool, PrimVal builtinBool)),
      (T.pack "help", Constant $ (docHelp, PrimVal builtinHelp)),
      (T.pack "case", Constant $ (docCase, PrimVal builtinCase))
    ]

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
    _ -> throwError $ TypeError "cannot cat with different types or concat functions,int,floats."

----------------------------------------------------

builtinRotNM :: Eval ()
builtinRotNM = do
  m <- pop
  n <- pop
  stack <- get
  case stack of
    [] -> throwError $ EmptyStackError "rotNM: no or few elements in the stack."
    _ -> case m of
      (IntVal m') -> case n of
        (IntVal n') -> case n' <= 0 of
          True -> case n' == 0 of
            True -> return ()
            False -> throwError $ ValueError "rotNM: cannot rotate with a negative number in first parameter."
          False -> do
            let original = take (length stack - (fromIntegral n')) stack
            let rot = drop (length stack - (fromIntegral n')) stack
            let result f x = last $ take (fromIntegral $ x + 1) (iterate f rot)
            case m' < 0 of
              True -> do
                let rotate l = tail l ++ [head l]
                put $ original <> (result rotate (abs m'))
              False -> do
                let rotate l = [last l] ++ init l
                put $ original <> (result rotate m')
        _ -> throwError $ TypeError "rotNM: the first parameter must be an integer."
      _ -> throwError $ TypeError "rotNM: the second parameter must be an integer."

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

builtinPow :: Eval ()
builtinPow = do
  exp' <- pop
  n <- pop
  case n of
    (FloatVal n') -> case exp' of
      (FloatVal exp'') -> push $ FloatVal $ n' ** exp''
      (IntVal exp'') -> push $ FloatVal $ n' ** (fromIntegral exp'')
      _ -> throwError $ TypeError "^: the second parameter has a wrong type."
    (IntVal n') -> case exp' of
      (FloatVal exp'') -> push $ FloatVal $ (fromIntegral n') ** exp''
      (IntVal exp'') -> push $ IntVal $ n' ^ exp''
      _ -> throwError $ TypeError "^: the second parameter has a wrong type."
    _ -> throwError $ TypeError "^: the first parameter has a wrong type."

----------------------------------------------------

builtinPrint :: Eval ()
builtinPrint = do
  v <- pop
  case v of
    (StringVal x) -> (liftIO $ print x) >> return ()
    (CharVal x) -> (liftIO $ print x) >> return ()
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

builtinPutChar :: Eval ()
builtinPutChar = do
  v <- pop
  case v of
    (CharVal x) -> (liftIO $ putChar x) >> return ()
    _ -> throwError $ TypeError "can only putchar with char."

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
    _ -> throwError $ TypeError "ask: the parameter is not string."

----------------------------------------------------

builtinBoolOp :: String -> (forall a. Ord a => a -> a -> Bool) -> Eval ()
builtinBoolOp fname op = do
  v1 <- pop
  v2 <- pop
  case v2 of
    (QuoteVal x) -> case v1 of
      (QuoteVal y) -> push $ BoolVal $ length x `op` length y
      _ -> throwError $ TypeError $ fname <> ": the second argument has a wrong type."
    ---
    (FloatVal x) -> case v1 of
      (FloatVal y) -> push $ BoolVal $ x `op` y
      (IntVal y) -> push $ BoolVal $ x `op` (fromIntegral y)
      (BoolVal y) -> do
        push v1
        builtinInt
        v <- pop
        let (IntVal z) = v
        push $ BoolVal $ x `op` (fromIntegral z)
      _ -> throwError $ TypeError $ fname <> ": the second argument has a wrong type."
    ---
    (IntVal x) -> case v1 of
      (FloatVal y) -> push $ BoolVal $ (fromIntegral x) `op` y
      (IntVal y) -> push $ BoolVal $ x `op` y
      (BoolVal y) -> do
        push v1
        builtinInt
        v <- pop
        let (IntVal z) = v
        push $ BoolVal $ x `op` z
      _ -> throwError $ TypeError $ fname <> ": the second argument has a wrong type."
    ---
    (StringVal x) -> case v1 of
      (StringVal y) -> push $ BoolVal $ (length $ T.unpack x) `op` (length $ T.unpack y)
      (CharVal y) -> push $ BoolVal $ (length $ T.unpack x) `op` length [y]
      _ -> throwError $ TypeError $ fname <> ": the second argument has a wrong type."
    ---
    (CharVal x) -> case v1 of
      (StringVal y) -> push $ BoolVal $ length [x] `op` (length $ T.unpack y)
      (CharVal y) -> push $ BoolVal $ length [x] `op` length [y]
      _ -> throwError $ TypeError $ fname <> ": the second argument has a wrong type."
    ---
    (BoolVal x) -> case v1 of
      (FloatVal y) -> do
        push v2
        builtinInt
        v <- pop
        let (IntVal z) = v
        push $ BoolVal $ (fromIntegral z) `op` y
      (IntVal y) -> do
        push v2
        builtinInt
        v <- pop
        let (IntVal z) = v
        push $ BoolVal $ z `op` y
      (BoolVal y) -> do
        push v1
        builtinInt
        v <- pop
        let (IntVal z) = v
        ---
        push v2
        builtinInt
        v' <- pop
        let (IntVal z') = v'
        push $ BoolVal $ z' `op` z
      _ -> throwError $ TypeError $ fname <> ": the second parameter has a wrong type."
    _ -> throwError $ TypeError $ fname <> ": the first parameter has a wrong type."

----------------------------------------------------

builtinCondBool :: String -> (Bool -> Bool -> Bool) -> Eval ()
builtinCondBool fname op = do
  v1 <- pop
  v2 <- pop
  case v2 of
    (BoolVal y) -> case v1 of
      (IntVal x) -> do
        push v1
        builtinBool
        v <- pop
        let (BoolVal z) = v
        push $ BoolVal $ y `op` z
      (BoolVal x) -> push $ BoolVal $ y `op` x
      _ -> throwError $ TypeError $ fname <> ": the second parameter has a wrong type."
    (IntVal y) -> case v1 of
      (IntVal x) -> do
        push v1
        builtinBool
        v <- pop
        let (BoolVal z) = v
        ---
        push v2
        builtinBool
        v' <- pop
        let (BoolVal z') = v'
        push $ BoolVal $ z' `op` z
      (BoolVal x) -> do
        push v2
        builtinBool
        v <- pop
        let (BoolVal z) = v
        push $ BoolVal $ z `op` x
      _ -> throwError $ TypeError $ fname <> ": the second parameter has a wrong type."
    _ -> throwError $ TypeError $ fname <> ": the first parameter has a wrong type."

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
    (CharVal x) -> push $ StringVal $ T.pack [x]
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
    (FloatVal x) -> push $ IntVal $ floor x
    (IntVal x) -> push $ IntVal x
    (StringVal x) -> case readMaybe (T.unpack x) :: Maybe Integer of
      (Just v) -> push $ IntVal v
      Nothing -> throwError $ ValueError "int: the value is not a integer."
    (BoolVal x) -> case x of
      True -> push $ IntVal 1
      False -> push $ IntVal 0
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
      Nothing -> throwError $ ValueError "float: the value is not a integer."
    _ -> throwError $ TypeError "can only float with int,float,str"

----------------------------------------------------

builtinBool :: Eval ()
builtinBool = do
  v <- pop
  case v of
    (IntVal 0) -> push $ BoolVal False
    _ -> push $ BoolVal True

----------------------------------------------------

builtinHelp :: Eval ()
builtinHelp = do
  env <- ask
  quote <- pop
  case quote of
    (QuoteVal n) -> case n of
      [WordAtom n'] -> case M.lookup (T.pack n') env of
        (Just (Constant (d, _))) -> push $ StringVal $ T.pack $ "docstring for '" <> n' <> "' function:\n------------\n" <> d
        (Just (Function (d, _))) -> case d of
          (Just d') -> push $ StringVal $ T.pack $ "docstring for '" <> n' <> "' function:\n------------\n" <> d'
          Nothing -> push $ StringVal $ T.pack $ "help: no documentation entry for '" <> n' <> "' function."
        Nothing -> throwError $ NameError $ "help: the '" <> n' <> "' function does not exists."
      _ -> throwError $ ValueError $ "help: bad expression in the quote (required: function)."
    _ -> throwError $ TypeError "help: cannot help without a quote parameter."

----------------------------------------------------

runCase :: Value -> Expr -> Eval ()
runCase _ [] = throwError $ EmptyStackError "case: no pattern matches."
runCase c (p : ps) = do
  case p of
    (QuoteAtom [QuoteAtom [WordAtom "_"], QuoteAtom expr]) -> evalExpr expr
    (QuoteAtom [QuoteAtom p', QuoteAtom expr]) -> do
      evalExpr p'
      p'' <- pop
      case eqValue c p'' of
        True -> evalExpr expr
        False -> runCase c ps
    _ -> throwError $ ValueError "cannot case with bad pattern(s)."

builtinCase' :: Eval ()
builtinCase' = do
  patterns <- pop
  tocase <- pop
  case tocase of
    (QuoteVal c) -> case patterns of
      (QuoteVal pat) -> do
        evalExpr c
        case' <- pop
        case all isPattern pat of
          True -> runCase case' pat
          False -> throwError $ ValueError "cannot case with bad pattern(s)."
      _ -> throwError $ TypeError "cannot case with a wrong type. (the second parameter must be a quote)."
    _ -> throwError $ TypeError "cannot case with a wrong type. (the first parameter must be a quote)."

builtinCase :: Eval ()
builtinCase = do
  patterns <- pop
  tocase <- pop
  let c = QuoteVal [readAtom tocase] in (push c >> push patterns >> builtinCase')
