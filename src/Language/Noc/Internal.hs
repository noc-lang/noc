module Language.Noc.Internal where

import Language.Noc.Parser

-- DataType for evaluator
data Value = QuoteValue Stack | FloatValue Float deriving (Show)
type Values = [Value]
-------------------




