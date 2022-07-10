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