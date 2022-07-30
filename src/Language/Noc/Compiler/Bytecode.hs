module Language.Noc.Compiler.Bytecode where

import Data.List (findIndex)
import Data.Map (Map, toList)
import Data.Text (Text, empty, pack, unpack)
import Language.Noc.Resolution.Name
import Language.Noc.Syntax.AST

type Position = Int

type Size = Int

data Bytecode = Bytecode {sym :: [SymbolDef], constant :: [Constant], doc :: [(DocString, Position)], opcodes :: OpCodes} deriving (Show)

data SymbolDef = FuncSym String Position | FuncPrim Position | OpcodeSym OpCode deriving (Show,Eq)

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

index :: Eq a => a -> [a] -> Position
index v l = maybe (-1) id $ findIndex (== v) l

set :: Eq a => a -> [a] -> [a]
set v l = if v `elem` l then l else l <> [v]

updatePos :: String -> Int -> SymbolDef -> SymbolDef
updatePos decl_name new_pos (FuncSym name pos) = case name of
  "main" -> FuncSym name 0
  _ -> if decl_name == name then FuncSym name new_pos else FuncSym name pos
updatePos decl_name new_pos s = s 

isSymbolDecl :: String -> SymbolDef -> Bool
isSymbolDecl name (FuncSym w _) = name == w
isSymbolDecl name _ = False

update :: Expr -> Bytecode -> Bool -> Bytecode
update [] (Bytecode s c d o) _ = Bytecode s c d o
update (WordAtom x : xs) (Bytecode s c d o) isQuote = case isPrim x of
  True -> update xs (Bytecode new c d (o <> [pushWord isQuote $ CALL_SYMBOL $ index elem new])) isQuote
    where
      elem = FuncPrim $ index x internalFuncs
      new = elem `set` s
  False -> case isOpcode x of
    True -> case isQuote of
      True -> update xs (Bytecode new c d (o <> [PUSH_SYM $ index elem new])) isQuote
        where
          elem = OpcodeSym $ toOpcode x 
          new = elem `set` s
      False -> update xs (Bytecode new c d (o <> [toOpcode x])) isQuote
        where
          new = (OpcodeSym $ toOpcode x) `set` s
    False -> update xs (Bytecode new c d (o <> [pushWord isQuote $ CALL_SYMBOL $ index elem new])) isQuote
      where
        elem = FuncSym x (-1)
        new = elem `set` s
update (ConstAtom x : xs) (Bytecode s c d o) isQuote = update xs (Bytecode s new d (o <> [PUSH_CONST $ index x new])) isQuote
  where
    new = x `set` c
update (QuoteAtom x : xs) (Bytecode s c d o) isQuote = update xs (Bytecode s' c' d' (o' <> [CREATE_QUOTE $ length x])) isQuote
  where
    (Bytecode s' c' d' o') = update x (Bytecode s c d o) True

genBytecode :: [(Text, (Maybe DocString, Expr))] -> Bytecode -> IO Bytecode
genBytecode [] bytecode = return bytecode
genBytecode ((name, (docstring, expr)) : xs) (Bytecode s c d o) = do
  -- (unpack name) `inTable` s => check if function declaration's name exists in the table
  let (Bytecode s' c' d' o') = update expr (Bytecode ((FuncSym (unpack name) (-1)) `set` s) c d o) False
  let new_s = map (updatePos (unpack name) (length o)) s'
  let symbol_decl = head $ filter (isSymbolDecl (unpack name)) new_s
  case docstring of
    (Just doc') -> genBytecode xs $ Bytecode new_s c' (d' <> [(doc', index symbol_decl new_s)]) (o' <> [RETURN])
    Nothing -> genBytecode xs $ Bytecode new_s c' d' (o' <> [RETURN])