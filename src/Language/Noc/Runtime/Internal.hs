module Language.Noc.Runtime.Internal where

import Language.Noc.Syntax.AST (Stack)

data Value = QuoteValue Stack | FloatValue Double | WordValue String | StringValue String deriving (Show,Eq)

type Values = [Value]

