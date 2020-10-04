module Language.Noc.Internal where

import Language.Noc.Parser (Stack)

data Value = QuoteValue Stack | FloatValue Double | WordValue String | StringValue String deriving (Show,Eq)

type Values = [Value]

