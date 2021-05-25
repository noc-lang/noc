module Interpreter.Utils where

import qualified Data.Map as M (Map, toList, union, empty)
import qualified Data.Text as T (Text, pack, unwords)
import Language.Noc.Syntax.AST (Expr)

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

unionMap :: [M.Map T.Text Expr] -> (M.Map T.Text Expr)
unionMap l = foldr M.union M.empty l