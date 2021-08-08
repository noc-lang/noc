module Language.Noc.Runtime.Lib.Str where

import Control.Monad.Except (throwError)
import Control.Monad.RWS
import qualified Data.Map as M (fromList)
import qualified Data.Text as T (Text, pack, replace, splitOn, unpack)
import Language.Noc.PrettyPrinter
import Language.Noc.Runtime.Internal
import Language.Noc.Runtime.PreludeDoc
import Language.Noc.Syntax.AST

----------------------------------------------------

str :: Env
str =
  M.fromList
    [ (T.pack "format", Constant $ (docFormat, PrimVal builtinFormat)),
      (T.pack "tostr", Constant $ (docToStr, PrimVal builtinToStr)),
      (T.pack "chars", Constant $ (docChars, PrimVal builtinChars))
    ]

----------------------------------------------------

format' :: [T.Text] -> Expr -> [T.Text] -> Either EvalError [T.Text]
format' [] [] res = Right res
format' [] _ res = Right res
format' (x : xs) (y : ys) res = case isBrace x of
  True -> case y of
    (StringAtom a) -> format' xs ys (T.replace (T.pack "{}") (T.pack a) x : res)
    (CharAtom a) -> format' xs ys (T.replace (T.pack "{}") (T.pack $ show a) x : res)
    (IntAtom a) -> format' xs ys (T.replace (T.pack "{}") (T.pack $ show a) x : res)
    (FloatAtom a) -> format' xs ys (T.replace (T.pack "{}") (T.pack $ show a) x : res)
    (BoolAtom a) -> format' xs ys (T.replace (T.pack "{}") (T.pack $ show a) x : res)
    (QuoteAtom a) -> format' xs ys (T.replace (T.pack "{}") (T.pack $ displayQuote a) x : res)
    _ -> Left $ TypeError "format: cannot format with this type."
  False -> format' xs (y : ys) (x : res)
format' (x : xs) [] res = case isBrace x of
  True -> Left $ ValueError "format: too many braces."
  False -> format' xs [] (x : res)

builtinFormat :: Eval ()
builtinFormat = do
  quote <- pop
  str <- pop
  case str of
    (StringVal s) -> case quote of
      (QuoteVal l) -> do
        stack <- get
        put []
        evalExpr l
        putAllInQuote []
        newstack <- get
        put $ stack <> newstack
        v <- pop
        let (QuoteVal val) = v
        let toFormat = format' (T.splitOn (T.pack " ") s) val []
        case toFormat of
          (Left err) -> throwError $ err
          (Right succ) -> push $ StringVal $ T.pack $ initSafe $ T.unpack $ foldl (\acc x -> (x <> T.pack " ") <> acc) (T.pack "") succ
      _ -> throwError $ TypeError "cannot format with a wrong parameter (the second parameter is a quote)."
    _ -> throwError $ TypeError "cannot format with a wrong parameter (the first parameter is a string)."

----------------------------------------------------

builtinToStr :: Eval ()
builtinToStr = do
  v <- pop
  case v of
    (QuoteVal l) -> case all isChar l of
      True -> push $ StringVal $ T.pack $ foldr (\(CharAtom x) acc -> x : acc) [] l
      False -> throwError $ TypeError "can only sugar on a quote based on char atoms."
    _ -> throwError $ TypeError "can only sugar with a quote."

----------------------------------------------------

builtinChars :: Eval ()
builtinChars = do
  v <- pop
  case v of
    (StringVal s) -> push $ QuoteVal $ map (\x -> CharAtom x) (T.unpack s)
    _ -> throwError $ TypeError "can only desugar with a string."
