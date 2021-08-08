module Language.Noc.Runtime.Lib.FS where

import Control.Exception (SomeException, try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M (fromList)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.IO as TIO (readFile)
import Language.Noc.Runtime.Internal
import Language.Noc.Runtime.PreludeDoc
import System.Directory (doesPathExist)

----------------------------------------------------

fs :: Env
fs =
  M.fromList
    [ (T.pack "open", Constant $ (docOpen, PrimVal builtinOpen))
    ]

----------------------------------------------------

read' :: T.Text -> Eval ()
read' path = do
  isExist <- liftIO $ doesPathExist $ T.unpack path
  case isExist of
    True -> do
      content' <- liftIO (try $ TIO.readFile $ T.unpack path :: IO (Either SomeException T.Text))
      case content' of
        (Left err) -> throwError $ FileNotFoundError $ "open: the file does not exist (no such file)"
        (Right succ) -> push $ StringVal succ
    False -> throwError $ FileNotFoundError $ "open: the file does not exist (no such file or directory)"

write' :: T.Text -> T.Text -> Eval ()
write' path content = liftIO $ writeFile (T.unpack path) (T.unpack content)

append :: T.Text -> T.Text -> Eval ()
append path content = liftIO $ appendFile (T.unpack path) (T.unpack content)

builtinOpen :: Eval ()
builtinOpen = do
  mode <- pop
  content <- pop
  filename <- pop
  case mode of
    (StringVal m) -> case content of
      (StringVal c) -> case filename of
        (StringVal f) -> case (T.unpack m) of
          "r" -> read' f
          "w" -> write' f c
          "a" -> append f c
          "rw" -> (read' f) >> (write' f c)
          "ra" -> (read' f) >> (append f c)
          _ -> throwError $ ValueError "open: incorrect mode."
        _ -> throwError $ TypeError "open: the first parameter must be a string."
      _ -> throwError $ TypeError "oepn: the second parameter must be a string."
    _ -> throwError $ TypeError "open: the third parameter must be a string."
