module Interpreter.Utils where

import qualified Data.Map as M (Map, toList)
import qualified Data.Text as T (Text, empty, pack, unwords)
import Language.Noc.Syntax.AST (Expr)

isMainError :: (M.Map T.Text Expr) -> Bool
isMainError decl = not $ any (\(k, v) -> k == (T.pack "main")) (M.toList decl)

isMultipleDecls :: [T.Text] -> Maybe T.Text
isMultipleDecls [] = Nothing
isMultipleDecls (x : xs) = case x `elem` xs of
  True -> Just x
  False -> isMultipleDecls xs

filterProg :: (M.Map T.Text Expr) -> ([(T.Text, Expr)], [(T.Text, Expr)])
filterProg prog = (main', other')
  where
    filter' pred ast = filter (\(k, v) -> pred k (T.pack "main")) (M.toList ast)
    main' = filter' (==) prog
    other' = filter' (/=) prog
