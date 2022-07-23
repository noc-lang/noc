module Language.Noc.Compiler.Serialize where

import Data.Binary
import Data.Binary.Put
import Language.Noc.Compiler.Bytecode
import Language.Noc.Syntax.AST (Atom(WordAtom), Constant(..))
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as BS (writeFile)

encodeInteger :: Integral a => a -> Put
encodeInteger i = putInt64le (fromIntegral i :: Int64)

encodeOpCode :: OpCode -> Put
encodeOpCode (CALL_SYMBOL i) = putWord8 0x01 >> encodeInteger i
encodeOpCode (PUSH_CONST i) = putWord8 0x02 >> encodeInteger i
encodeOpCode RETURN = putWord8 0x03
encodeOpCode (CREATE_QUOTE s) = putWord8 0x04 >> encodeInteger s
encodeOpCode POPR_QUOTE = putWord8 0x05
encodeOpCode PUSHR_QUOTE = putWord8 0x6
encodeOpCode UNQUOTE_QUOTE = putWord8 0x7
encodeOpCode (PUSH_SYM i) = putWord8 0x8 >> encodeInteger i
encodeOpCode DUP = putWord8 0x9
encodeOpCode POP = putWord8 0x10
encodeOpCode CLEAR_STACK = putWord8 0x11
encodeOpCode CONCAT = putWord8 0x12
encodeOpCode ROT = putWord8 0x13
encodeOpCode ADD_OP = putWord8 0x14
encodeOpCode MINUS_OP = putWord8 0x15
encodeOpCode MUL_OP = putWord8 0x16
encodeOpCode DIV_OP = putWord8 0x17
encodeOpCode EXP_OP = putWord8 0x18
encodeOpCode GREATER_CMP = putWord8 0x19
encodeOpCode LESS_CMP = putWord8 0x20
encodeOpCode GREATER_OR_EQ_CMP = putWord8 0x21
encodeOpCode LESS_OR_EQ_CMP = putWord8 0x22
encodeOpCode EQUAL = putWord8 0x23
encodeOpCode AND_BOOL = putWord8 0x24
encodeOpCode OR_BOOL = putWord8 0x25

encodeConst :: Constant -> Put
encodeConst (IntConst n) = putWord8 0x04 >> (encodeInteger n)
encodeConst (FloatConst n) = putWord8 0x05 >> putDoublele n
encodeConst (StringConst s) = putWord8 0x06 >> (encodeInteger $ length s) >> putStringUtf8 s
encodeConst (CharConst c) = putWord8 0x07 >> putCharUtf8 c
encodeConst (BoolConst True) = putWord8 0x08 >> putWord8 0x01
encodeConst (BoolConst False) = putWord8 0x08 >> putWord8 0x00

encodeString :: String -> Put
encodeString w = (encodeInteger $ length w) >> putStringUtf8 w

encodeSym :: SymbolDef -> Put
encodeSym (FuncSym w p) = putWord8 0x01 >> encodeString w >> encodeInteger p
encodeSym (FuncPrim w) = putWord8 0x02 >> encodeString w
encodeSym (OpcodeSym w) = putWord8 0x03 >> encodeOpCode w

encode' :: Bytecode -> Put
encode' (Bytecode sym constant doc opcodes nb_functions) = do
    (encodeInteger $ length sym) >> mapM_ encodeSym sym
    (encodeInteger $ length constant) >> mapM_ encodeConst constant
    (encodeInteger $ length doc) >> mapM_ (\(d, pos) -> encodeString d >> (encodeInteger pos)) doc
    (encodeInteger nb_functions) >> (encodeInteger $ length opcodes) >> mapM_ encodeOpCode opcodes

serializeBytecode :: FilePath -> Bytecode -> IO ()
serializeBytecode filepath bytecode = do
    let res = runPut $ encode' bytecode
    BS.writeFile filepath res