module Language.Noc.Runtime.PreludeDoc where

import Language.Noc.Syntax.AST (DocString)

docDup :: DocString
docDup =
  "Duplicate the top-stack element\n\n\
  \(example)\n\
  \  stack: [1 2]\n\
  \  dup => [1 2 2]"

docPop :: DocString
docPop =
  "Remove the top-stack element\n\n\
  \(example)\n\
  \  stack: [1 2]\n\
  \  pop => [1]"

docZap :: DocString
docZap =
  "Clear all the stack\n\n\
  \(example)\n\
  \  stack: [1 2 3]\n\
  \  zap: []"

docCat :: DocString
docCat =
  "Concatenate 2 values (string and quotes)\n\n\
  \(example)\n\
  \  [1] [2] cat => [1 2]\n\
  \  \"Hello, \" \"World!\" cat => \"Hello, World!\""

docRotNM :: DocString
docRotNM =
  "Rotate the stack N elements M times\n\n\
  \(example)\n\
  \  stack: [1 2 3]\n\
  \  2 1 rotNM => [1 3 2]\n\
  \  3 -1 rotNM => [2 3 1]"

docOp :: String -> DocString
docOp "+" =
  "Sum of the 2 top-stack elements\n\n\
  \(example)\n\
  \  stack: [5 6]\n\
  \  + => [11]"
docOp "-" =
  "Difference of the 2 top-stack elements\n\n\
  \(example)\n\
  \  stack: [10 9]\n\
  \  - => [1]"
docOp "*" =
  "Product of the 2 top-stack elements\n\n\
  \(example)\n\
  \  stack: [20 0.5]\n\
  \  * => [10.0]"

docDiv :: DocString
docDiv =
  "Quotient of the 2 top-stack elements\n\n\
  \(example)\n\
  \  stack: [10 2]\n\
  \  / => [5.0]"

docPow :: DocString
docPow =
  "The exponent of a number\n\n\
  \(example)\n\
  \  stack: [10 2]\n\
  \  ^ => [100]"

docPrint :: DocString
docPrint =
  "Output a value (Standard output)\n\n\
  \(example)\n\
  \  \"Hello, World!\" print => \"Hello, World!\"\n\
  \  => []"

docPutStr :: DocString
docPutStr =
  "Output a string value (Standard output)\n\n\
  \(example)\n\
  \  \"Hello!\" putstr => Hello!\n\
  \  => []"

docPutChar :: DocString
docPutChar =
  "Output a char value (Standard output)\n\n\
  \(example)\n\
  \  'a' putchar => a=> []"

docAsk :: DocString
docAsk =
  "Read line from the standard input\n\n\
  \(example)\n\
  \  \"Your name: \" ask\n\
  \  Your name: john\n\
  \  => [\"john\"]"

docArgs :: DocString
docArgs =
  "Get the program's command line arguments\n\n\
  \(example)\n\
  \  def main = {\n\
  \    args print\n\
  \  }\n\
  \ /*\n\
  \ noc file.noc a b\n\
  \ Output:\n\
  \ [\"a\", \"b\"]\n\
  \ -----\n\
  \ noc file.noc -- a b --arg c\n\
  \ [\"a\" \"b\" \"--arg\" \"c\"]\n\
  \ */"

docOpen :: DocString
docOpen =
  "Proceed to 'read', 'write', 'append' actions to files\n\n\
  \(example)\n\
  \ \"filename\" \"\" \"r\" open => [\"This is a content.\\n\"]\n\
  \ pop \"filename\" \"content\" \"w\" open => []\n\
  \ \"filename\" \"\\ncontent2\" \"a\" open => []\n\n\
  \ Modes combined: \n\
  \ \"filename\" \"This is a new content\" \"rw\" open => [\"content\\ncontent2\"]\n\
  \ pop \"filename\" \"This is a new content\" \"ra\" open => [\"This is a new content\"]"

docUnquote :: DocString
docUnquote =
  "Evaluate instruction into a quote\n\n\
  \(example)\n\
  \  [[5 5 +] 5 5 +] unquote => [[5 5 +] 10]"

docPushr :: DocString
docPushr =
  "Push a value into a quote\n\n\
  \(example)\n\
  \  [5] 5 pushr => [[5 5]]"

docPopr :: DocString
docPopr =
  "Get out the top-element in the quote\n\
  \n\n\
  \(example)\n\
  \  [1 2] popr => [[1] 2]"

docId :: DocString
docId =
  "Get the top-stack element (does nothing)\n\n\
  \(example)\n\
  \  stack: [5]\n\
  \  id => [5]"

docStr :: DocString
docStr =
  "Type conversion\n\n\
  \(example)\n\
  \  5 str => [\"5\"]"

docInt :: DocString
docInt =
  "Type conversion\n\n\
  \(example)\n\
  \  \"10\" int => [10]"

docFloat :: DocString
docFloat =
  "Type conversion\n\n\
  \(example)\n\
  \  \"10.5\" float => [10.5]"

docExit :: DocString
docExit =
  "Exit program after the current instruction\n\n\
  \(example)\n\
  \ def main = {\n\
  \ \"ERROR! ...\" print\n\
  \ 1 exit\n\
  \ \"other instructions...\" print \n\
  \ }\n\
  \ /*\n\
  \ Output:\n\
  \ \"ERROR! ...\"\n\
  \ *** Exception: ExitFailure 1\n\
  \ */\n\
  \ ---------\n\
  \ def main = {\n\
  \ \"other instructions...\" print\n\
  \ 0 exit\n\
  \ }\n\
  \ /*\n\
  \ Output: \n\
  \ \"other instructions...\"\n\
  \ */"

docFormat :: DocString
docFormat =
  "Format string, replace braces by corresponding value\n\n\
  \(example)\n\
  \ \"Hello, {}!\" [\"John\"] format => [\"Hello, John!\"]\n\
  \ \"Numbers: {} {}\" [20 3.14] format => [\"Numbers: 20 3.14\"]\n\
  \ \"Boolean: {} {}\" [True False] format => [\"Boolean: True False\"]\n\
  \ \"Quotes: {} {}\" [[1 2 3] [[\"a\" 1] [\"b\" 2]]] format => [\"Quotes: [1 2 3] [[\\\"a\\\" 1] [\\\"b\\\" 2]]\"]\n\
  \ \"Expression: {}\" [[5 5 +] unquote 2 *] format => [\"Expression: 20\"]"

docHelp :: DocString
docHelp =
  "Get a function's doc-string\n\n\
  \(example)\n\
  \ [pop] help \n\
  \ Output: \n\
  \ docstring for 'pop' function: \n\
  \ ------------ \n\
  \ Remove the top-stack element\n\
  \ (example)\n\
  \   stack: [1 2]\n\
  \   pop => [1]"

docBool :: DocString
docBool =
  "Convert a value to boolean value\n\n\
  \(example)\n\
  \ 1 Bool => [True]"

docCase :: DocString
docCase =
  "Pattern matching combinator\n\n\
  \(example)\n\
  \ def fact = { \n\
  \   [\n\
  \     [[0] [pop 1]]\n\
  \     [[_] [dup 1 - quote [dup] cat fact *]] # wildcard (match any pattern)\n\
  \   ] case\n\
  \ }\n\
  \ \n\
  \ def main = { \n\
  \   [6 dup] fact \n\
  \ }"

docToStr :: DocString
docToStr =
  "Convert a quote of chars to string\n\n\
  \(example)\n\
  \ ['a' 'b' 'c'] tostr => [\"abc\"]"

docChars :: DocString
docChars =
  "Convert a string to a quote of chars\n\n\
  \(example)\n\
  \ \"abc\" chars => ['a' 'b' 'c']"

docChr :: DocString
docChr =
  "Get the character Unicode representation of the decimal number\n\n\
  \(example)\n\
  \ 97 chr => ['a']"

docOrd :: DocString
docOrd =
  "Get the decimal representation of a Unicode character\n\n\
  \(example)\n\
  \ 'A' ord => [65]"

docStep :: DocString
docStep =
  "Execute a specific function for each element of a quote\n\n\
  \quote [function] step \n\n\
  \(example)\n\
  \ [\"noc\" \"hello\" \"world!\"] [$ len] step => [[3 5 6]]"

docFold :: DocString
docFold =
  "Reduce a quote of elements to one value in accumulating each elements with a specific function\n\n\
  \quote (initial_value -> accumulator) [function] fold \n\n\
  \(example)\n\
  \ [1 2 3] 0 [+] fold => [6]"

docBoolOp :: String -> DocString
docBoolOp ">" =
  "Compare if the first value is greater than the second value\n\n\
  \(example)\n\
  \  stack: [5 6]\n\
  \  > => [False]"
docBoolOp "<" =
  "Compare if the first value is less than the second value\n\n\
  \(example)\n\
  \  stack: [5 6]\n\
  \  < => [True]"
docBoolOp ">=" =
  "Compare if the first value is greater or equal than the second value\n\n\
  \(example)\n\
  \  stack: [5 5]\n\
  \  >= => [True]"
docBoolOp "<=" =
  "Compare if the first value is less or equal than the second value\n\n\
  \(example)\n\
  \  stack: [4 5]\n\
  \  <= => [True]"
docBoolOp "and" =
  "The 'AND' logic operator\n\n\
  \(example)\n\
  \  True True and => [True]"
docBoolOp "or" =
  "The 'OR' logic operator\n\n\
  \(example)\n\
  \  False False or => [False]"
