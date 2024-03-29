module Language.Noc.Compiler.Serialize where

import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString as BS (length)
import qualified Data.ByteString.Lazy as BSL (writeFile)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Language.Noc.Compiler.Bytecode
import Language.Noc.Syntax.AST (Atom(WordAtom), Constant(..))
import Data.Int (Int64)

encodeInteger :: Integral a => a -> Put
encodeInteger i = putInt64le (fromIntegral i :: Int64)

encodeOpCode :: OpCode -> Put
encodeOpCode (CALL_SYMBOL i) = putWord8 0 >> encodeInteger i
encodeOpCode (PUSH_CONST i) = putWord8 1 >> encodeInteger i
encodeOpCode RETURN = putWord8 2
encodeOpCode (CREATE_QUOTE s) = putWord8 3 >> encodeInteger s
encodeOpCode POPR_QUOTE = putWord8 4
encodeOpCode PUSHR_QUOTE = putWord8 5
encodeOpCode UNQUOTE_QUOTE = putWord8 6
encodeOpCode (PUSH_SYM i) = putWord8 7 >> encodeInteger i
encodeOpCode DUP = putWord8 8
encodeOpCode POP = putWord8 9
encodeOpCode CLEAR_STACK = putWord8 10
encodeOpCode CONCAT = putWord8 11
encodeOpCode ROT = putWord8 12
encodeOpCode ADD_OP = putWord8 13
encodeOpCode MINUS_OP = putWord8 14
encodeOpCode MUL_OP = putWord8 15
encodeOpCode DIV_OP = putWord8 16
encodeOpCode EXP_OP = putWord8 17
encodeOpCode GREATER_CMP = putWord8 18
encodeOpCode LESS_CMP = putWord8 19
encodeOpCode GREATER_OR_EQ_CMP = putWord8 20
encodeOpCode LESS_OR_EQ_CMP = putWord8 21
encodeOpCode EQUAL = putWord8 22
encodeOpCode AND_BOOL = putWord8 23
encodeOpCode OR_BOOL = putWord8 24

encodeString :: String -> Put
encodeString w = (encodeInteger $ BS.length encoded) >> (putByteString encoded)
    where encoded = encodeUtf8 $ pack w

encodeConst :: Constant -> Put
encodeConst (IntConst n) = putWord8 4 >> (encodeInteger n)
encodeConst (FloatConst n) = putWord8 5 >> putDoublele n
encodeConst (StringConst s) = putWord8 6 >> (encodeString s)
encodeConst (CharConst c) = putWord8 7 >> putCharUtf8 c
encodeConst (BoolConst True) = putWord8 8 >> putWord8 1
encodeConst (BoolConst False) = putWord8 8 >> putWord8 0

encodeSym :: SymbolDef -> Put
encodeSym (FuncSym n p) = putWord8 0 >> (encodeString n) >> encodeInteger p
encodeSym (FuncPrim n p) = putWord8 1 >> (encodeString n) >> encodeInteger p
encodeSym (OpcodeSym w) = putWord8 2 >> encodeOpCode w

encode' :: Bytecode -> Put
encode' (Bytecode sym constant doc opcodes) = do
    (encodeInteger $ length sym) >> mapM_ encodeSym sym
    (encodeInteger $ length constant) >> mapM_ encodeConst constant
    (encodeInteger $ length doc) >> mapM_ (\(name, d) -> encodeString name >> encodeString d) doc
    (encodeInteger $ length opcodes) >> mapM_ encodeOpCode opcodes

serializeBytecode :: FilePath -> Bytecode -> IO ()
serializeBytecode filepath bytecode = do
    let res = runPut $ encode' bytecode
    BSL.writeFile filepath res