module Language.Noc.Runtime.Lib.Char where

import Control.Monad.Except (throwError)
import Data.Char (chr, ord)
import qualified Data.Map as M (fromList)
import qualified Data.Text as T (pack)
import Language.Noc.Runtime.Internal
import Language.Noc.Runtime.PreludeDoc

----------------------------------------------------

char :: Env
char =
  M.fromList
    [ (T.pack "chr", Constant $ (docChr, PrimVal builtinChr)),
      (T.pack "ord", Constant $ (docOrd, PrimVal builtinOrd))
    ]

----------------------------------------------------

builtinChr :: Eval ()
builtinChr = do
  n <- pop
  case n of
    (IntVal x) -> push $ CharVal $ chr $ fromIntegral x
    _ -> throwError $ TypeError "can only chr with int."

----------------------------------------------------

builtinOrd :: Eval ()
builtinOrd = do
  c <- pop
  case c of
    (CharVal x) -> push $ IntVal $ fromIntegral $ ord x
    _ -> throwError $ TypeError "can only ord with char."
