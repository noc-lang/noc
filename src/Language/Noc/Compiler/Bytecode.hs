module Language.Noc.Compiler.Bytecode where

import Control.Exception (Exception, SomeException, throwIO)
import Data.Char (ord)
import Data.List (findIndex)
import Data.Map (Map, toList)
import Data.Text (Text, empty, pack, unpack)
import Language.Noc.Resolution.Name
import Language.Noc.Syntax.AST

data NocError = UTF8Error String deriving (Show)

instance Exception NocError

type Position = Int

type Size = Int

data Bytecode = Bytecode {sym :: [SymbolDef], constant :: [Constant], doc :: [(String, DocString)], opcodes :: OpCodes} deriving (Show)

data SymbolDef = FuncSym String Position | FuncPrim String Position | OpcodeSym OpCode deriving (Show,Eq)

type OpCodes = [OpCode]

data OpCode
  = CALL_SYMBOL Position
  | PUSH_CONST Position
  | -- Function
    RETURN
  | -- Quote
    CREATE_QUOTE Size
  | POPR_QUOTE
  | PUSHR_QUOTE
  | UNQUOTE_QUOTE
  | PUSH_SYM Position
  | -- Stack-shuffler
    DUP
  | POP
  | CLEAR_STACK
  | CONCAT
  | ROT
  | -- Arithmetic
    ADD_OP
  | MINUS_OP
  | MUL_OP
  | DIV_OP
  | EXP_OP
  | -- Boolean
    GREATER_CMP
  | LESS_CMP
  | GREATER_OR_EQ_CMP
  | LESS_OR_EQ_CMP
  | EQUAL
  | AND_BOOL
  | OR_BOOL
  deriving (Show, Eq)

toOpcode :: String -> OpCode
toOpcode "dup" = DUP
toOpcode "pop" = POP
toOpcode "zap" = CLEAR_STACK
toOpcode "cat" = CONCAT
toOpcode "rotNM" = ROT
toOpcode "+" = ADD_OP
toOpcode "-" = MINUS_OP
toOpcode "*" = MUL_OP
toOpcode "/" = DIV_OP
toOpcode "^" = EXP_OP
toOpcode ">" = GREATER_CMP
toOpcode "<" = LESS_CMP
toOpcode ">=" = GREATER_OR_EQ_CMP
toOpcode "<=" = LESS_OR_EQ_CMP
toOpcode "==" = EQUAL
toOpcode "and" = AND_BOOL
toOpcode "or" = OR_BOOL
toOpcode "pushr" = PUSHR_QUOTE
toOpcode "popr" = POPR_QUOTE
toOpcode "unquote" = UNQUOTE_QUOTE

isPrim :: String -> Bool
isPrim w = any (== w) internalFuncs

isOpcode :: String -> Bool
isOpcode w = w `elem` opcodes'

pushWord :: Bool -> OpCode -> OpCode
pushWord True (CALL_SYMBOL i) = PUSH_SYM i
pushWord False opcode = opcode

index :: a -> (a -> b -> Bool) -> [b] -> Position
index v f l = maybe (-1) id $ findIndex (f v) l

set :: Eq a => a -> [a] -> [a]
set v l = if v `elem` l then l else l <> [v]

updatePos :: String -> Int -> SymbolDef -> SymbolDef
updatePos decl_name new_pos (FuncSym name pos) = case name of
  "main" -> FuncSym name 0
  _ -> if decl_name == name then FuncSym name new_pos else FuncSym name pos
updatePos decl_name new_pos s = s 

isSameSymbolDecl :: String -> SymbolDef -> Bool
isSameSymbolDecl name (FuncSym w _) = name == w
isSameSymbolDecl name _ = False

update :: Expr -> Bytecode -> Bool -> IO Bytecode
update [] (Bytecode s c d o) _ = return $ Bytecode s c d o
update (WordAtom x : xs) (Bytecode s c d o) isQuote = case isPrim x of
  True -> update xs (Bytecode new c d (o <> [pushWord isQuote $ CALL_SYMBOL $ index elem (==) new])) isQuote
    where
      elem = FuncPrim x $ index x (==) internalFuncs
      new = elem `set` s
  False -> case isOpcode x of
    True -> case isQuote of
      True -> update xs (Bytecode new c d (o <> [PUSH_SYM $ index elem (==) new])) isQuote
        where
          elem = OpcodeSym $ toOpcode x 
          new = elem `set` s
      False -> update xs (Bytecode new c d (o <> [toOpcode x])) isQuote
        where
          new = (OpcodeSym $ toOpcode x) `set` s
    False -> update xs (Bytecode new c d (o <> [pushWord isQuote $ CALL_SYMBOL $ index x isSameSymbolDecl new])) isQuote
      where
        elem = FuncSym x (-1)
        ---
        setSymDecl (FuncSym n p) l = if any (isSameSymbolDecl n) l then l else l <> [FuncSym n p] 
        setSymDecl _ l = l
        ---
        new = elem `setSymDecl` s
update (ConstAtom x : xs) (Bytecode s c d o) isQuote = do
  let new = x `set` c
  case x of
    (CharConst y) -> case ord y > 127 of
      True -> throwIO $ UTF8Error "cannot encode utf8 in a char type"
      False -> update xs (Bytecode s new d (o <> [PUSH_CONST $ index x (==) new])) isQuote
    _ -> update xs (Bytecode s new d (o <> [PUSH_CONST $ index x (==) new])) isQuote
update (QuoteAtom x : xs) (Bytecode s c d o) isQuote = do
  (Bytecode s' c' d' o') <- update x (Bytecode s c d o) True
  update xs (Bytecode s' c' d' (o' <> [CREATE_QUOTE $ length x])) isQuote

genBytecode :: [(Text, (Maybe DocString, Expr))] -> Bytecode -> IO Bytecode
genBytecode [] bytecode = return bytecode
genBytecode ((name, (docstring, expr)) : xs) (Bytecode s c d o) = do
  (Bytecode s' c' d' o') <- update expr (Bytecode ((FuncSym (unpack name) (-1)) `set` s) c d o) False
  let new_s = map (updatePos (unpack name) (length o)) s'
  case docstring of
    (Just doc') -> genBytecode xs $ Bytecode new_s c' (d' <> [(unpack name, doc')]) (o' <> [RETURN])
    Nothing -> genBytecode xs $ Bytecode new_s c' d' (o' <> [RETURN])