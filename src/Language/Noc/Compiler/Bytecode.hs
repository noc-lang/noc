module Language.Noc.Compiler.Bytecode where

import Data.List (findIndex)
import Data.Map (Map, toList)
import Data.Text (Text, empty, pack, unpack)
import Language.Noc.Resolution.Name
import Language.Noc.Syntax.AST

type Position = Int

type Size = Int

data Bytecode = Bytecode {sym :: [(String, Position)], prim :: [String], constant :: [Constant], doc :: [(Position, DocString)], opcodes :: OpCodes} deriving (Show)

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
  | PUSH_OPCODE OpCode
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

inTable :: Eq a => a -> [a] -> [a]
inTable v l = case v `elem` l of
  True -> l
  False -> l <> [v]

inTableSym :: String -> [(String, Position)] -> [(String, Position)]
inTableSym v l = case v `elem` (map fst l) of
  True -> l
  False -> case v of
    "main" -> l <> [(v, 0)]
    _ -> l <> [(v, (-1))]

pushWord :: Bool -> OpCode -> OpCode
pushWord True (CALL_PRIM i) = PUSH_PRIM i
pushWord True (CALL_SYMBOL i) = PUSH_SYM i
pushWord False opcode = opcode

index :: Eq a => a -> [a] -> Int
index v l = maybe (-1) id $ findIndex (== v) l

setPos :: String -> Int -> (String, Position) -> (String, Position)
setPos decl_name new_pos (name, pos) = case (decl_name == name) && (name /= "main") of
  True -> (name, new_pos)
  False -> (name, pos)

update :: Expr -> Bytecode -> Bool -> Bytecode
update [] (Bytecode s p c d o) _ = Bytecode s p c d o
update (WordAtom x : xs) (Bytecode s p c d o) isQuote = case isPrim x of
  True -> update xs (Bytecode s new c d (o <> [pushWord isQuote $ CALL_PRIM $ index x new])) isQuote
    where
      new = x `inTable` p
  False -> case isOpcode x of
    True -> case isQuote of
      True -> update xs (Bytecode s p c d (o <> [PUSH_OPCODE $ toOpcode x])) isQuote
      False -> update xs (Bytecode s p c d (o <> [toOpcode x])) isQuote
    False -> update xs (Bytecode new p c d (o <> [pushWord isQuote $ CALL_SYMBOL $ index x (map fst new)])) isQuote
      where
        new = x `inTableSym` s
update (ConstAtom x : xs) (Bytecode s p c d o) isQuote = update xs (Bytecode s p new d (o <> [PUSH_CONST $ index x new])) isQuote
  where
    new = x `inTable` c
update (QuoteAtom x : xs) (Bytecode s p c d o) isQuote = update xs (Bytecode s' p' c' d' (o' <> [CREATE_QUOTE $ length x])) isQuote
  where
    (Bytecode s' p' c' d' o') = update x (Bytecode s p c d o) True

genBytecode :: [(Text, (Maybe DocString, Expr))] -> Bytecode -> IO Bytecode
genBytecode [] bytecode = return bytecode
genBytecode ((name, (docstring, expr)) : xs) (Bytecode s p c d o) = do
  -- (unpack name) `inTable` s => check if function declaration's name exists in the table
  let (Bytecode s' p' c' d' o') = update expr (Bytecode ((unpack name) `inTableSym` s) p c d o) False
  let new_s = map (setPos (unpack name) (length o)) s'
  case docstring of
    (Just doc') -> genBytecode xs $ Bytecode new_s p' c' (d' <> [(index (unpack name) (map fst s), doc')]) (o' <> [RETURN])
    Nothing -> genBytecode xs (Bytecode new_s p' c' d' (o' <> [RETURN]))