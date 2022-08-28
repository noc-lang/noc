module Language.Noc.Resolution.Name where

opcodes' :: [String]
opcodes' =
  [ "dup",
    "pop",
    "zap",
    "cat",
    "rotNM",
    "+",
    "-",
    "*",
    "/",
    "^",
    ">",
    "<",
    ">=",
    "<=",
    "==",
    "and",
    "or",
    "pushr",
    "popr",
    "unquote"
  ]

internalFuncs :: [String]
internalFuncs = prelude <> foldl (\acc (_,funcs) -> acc <> funcs) [] internal

prelude :: [String]
prelude =
  [ "id",
    "str",
    "int",
    "float",
    "bool",
    "help",
    "case",
    "trace",
    "chr",
    "ord",
    "print",
    "ask",
    "putstr"
  ]

internal :: [(String, [String])]
internal =
  [ ("fs", fs),
    ("str", str),
    ("sys", sys),
    ("seq", seq')
  ]

fs :: [String]
fs =
  [ "open"
  ]

str :: [String]
str =
  [ "format",
    "tostr",
    "chars"
  ]

sys :: [String]
sys =
  [ "exit",
    "args",
    "catch"
  ]

seq' :: [String]
seq' =
  [ "step",
    "fold"
  ]