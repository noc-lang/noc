module Language.Noc.Compiler.Bytecode where

import Data.List (findIndex)
import Data.Map (Map, toList)
import Data.Text (Text, empty, pack, unpack)
import Language.Noc.Resolution.Name
import Language.Noc.Syntax.AST

type Position = Int

type Size = Int

data Bytecode = Bytecode {sym :: [SymbolDef], prim :: [String], constant :: [Constant], doc :: [(DocString, Position)], opcodes :: OpCodes, nb_functions :: Int} deriving (Show)

data SymbolDef = FuncSym String Position | OpcodeSym OpCode deriving (Show)

type OpCodes = [OpCode]

data OpCode
  = CALL_SYMBOL Position
  | CALL_PRIM Position
  | PUSH_CONST Position
  | -- Function
    RETURN
  | -- Quote
    CREATE_QUOTE Size
  | POPR_QUOTE
  | PUSHR_QUOTE
  | UNQUOTE_QUOTE
  | PUSH_SYM Position
  | PUSH_PRIM Position
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
isPrim w = w `elem` prelude || (any (\(_, funcs) -> any (== w) funcs) internal)

isOpcode :: String -> Bool
isOpcode w = w `elem` opcodes'


pushWord :: Bool -> OpCode -> OpCode
pushWord True (CALL_PRIM i) = PUSH_PRIM i
pushWord True (CALL_SYMBOL i) = PUSH_SYM i
pushWord False opcode = opcode

inTable :: Eq a => a -> [a] -> [a]
inTable v l = case v `elem` l of
  True -> l
  False -> l <> [v]

index :: Eq a => a -> [a] -> Position
index v l = maybe (-1) id $ findIndex (== v) l

check :: String -> SymbolDef -> Bool
check v (FuncSym w _) = if v == w then True else False
check v (OpcodeSym w) =  if (toOpcode v) == w then True else False

inTableSym :: String -> Bool -> [SymbolDef] -> [SymbolDef]
inTableSym v isOpcode l = case any (check v) l of
  True -> l
  False -> case isOpcode of
    True -> l <> [OpcodeSym $ toOpcode v]
    False -> l <> [FuncSym v (-1)]

indexSym :: String -> [SymbolDef] -> Position
indexSym w s = maybe (-1) id $ findIndex check s
  where check (FuncSym x _) = w == x
        check (OpcodeSym x) = (toOpcode w) == x

setPos :: String -> Int -> SymbolDef -> SymbolDef
setPos decl_name new_pos (FuncSym name pos) = case decl_name of
  "main" -> FuncSym name 0
  _ -> case decl_name == name of
    True -> FuncSym name new_pos
    False -> FuncSym name pos
setPos decl_name new_pos s = s 

update :: Expr -> Bytecode -> Bool -> Bytecode
update [] (Bytecode s p c d o n) _ = Bytecode s p c d o n
update (WordAtom x : xs) (Bytecode s p c d o n) isQuote = case isPrim x of
  True -> update xs (Bytecode s new c d (o <> [pushWord isQuote $ CALL_PRIM $ index x new]) n) isQuote
    where
      new = x `inTable` p
  False -> case isOpcode x of
    True -> case isQuote of
      True -> update xs (Bytecode new p c d (o <> [PUSH_SYM $ indexSym x new]) n) isQuote
        where new = inTableSym x True s
      False -> update xs (Bytecode s p c d (o <> [toOpcode x]) n) isQuote
    False -> update xs (Bytecode new p c d (o <> [pushWord isQuote $ CALL_SYMBOL $ indexSym x new]) n) isQuote
      where
        new = inTableSym x False s
update (ConstAtom x : xs) (Bytecode s p c d o n) isQuote = update xs (Bytecode s p new d (o <> [PUSH_CONST $ index x new]) n) isQuote
  where
    new = x `inTable` c
update (QuoteAtom x : xs) (Bytecode s p c d o n) isQuote = update xs (Bytecode s' p' c' d' (o' <> [CREATE_QUOTE $ length x]) n') isQuote
  where
    (Bytecode s' p' c' d' o' n') = update x (Bytecode s p c d o n) True

genBytecode :: [(Text, (Maybe DocString, Expr))] -> Bytecode -> IO Bytecode
genBytecode [] bytecode = return bytecode
genBytecode ((name, (docstring, expr)) : xs) (Bytecode s p c d o n) = do
  -- (unpack name) `inTable` s => check if function declaration's name exists in the table
  let (Bytecode s' p' c' d' o' n') = update expr (Bytecode (inTableSym (unpack name) False s) p c d o n) False
  let new_s = map (setPos (unpack name) (length o)) s'
  case docstring of
    (Just doc') -> genBytecode xs $ Bytecode new_s p' c' (d' <> [(doc', indexSym (unpack name) s')]) (o' <> [RETURN]) (n+1)
    Nothing -> genBytecode xs $ Bytecode new_s p' c' d' (o' <> [RETURN]) (n+1) 