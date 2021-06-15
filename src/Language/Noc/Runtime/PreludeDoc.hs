module Language.Noc.Runtime.PreludeDoc where

import Language.Noc.Syntax.AST (DocString)

docDup :: DocString
docDup = "Duplicate the top-stack element\n\n\
\(example)\n\
\  stack: [1 2]\n\
\  dup => [1 2 2]"  

docPop :: DocString
docPop = "Remove the top-stack element\n\n\
\(example)\n\
\  stack: [1 2]\n\
\  pop => [1]"

docZap :: DocString
docZap = "Clear all the stack\n\n\
\(example)\n\
\  stack: [1 2 3]\n\
\  zap: []"

docCat :: DocString
docCat = "Concatenate 2 values (string and quotes)\n\n\
\(example)\n\
\  [1] [2] cat => [1 2]\n\
\  \"Hello, \" \"World!\" cat => \"Hello, World!\""

docRotN :: DocString
docRotN = "Rotate the stack N elements\n\n\
\(example)\n\
\  stack: [1 2 3]\n\
\  2 rotN => [1,3,2]"

docOp :: String -> DocString
docOp "+" = "Sum of the 2 top-stack elements\n\n\
\(example)\n\
\  stack: [5 6]\n\
\  + => [11]"
docOp "-" = "Difference of the 2 top-stack elements\n\n\
\(example)\n\
\  stack: [10 9]\n\
\  - => [1]"
docOp "*" = "Product of the 2 top-stack elements\n\n\
\(example)\n\
\  stack: [20 0.5]\n\
\  * => [10.0]"

docDiv :: DocString
docDiv = "Quotient of the 2 top-stack elements\n\n\
\(example)\n\
\  stack: [10 2]\n\
\  / => [5.0]"

docPrint :: DocString
docPrint = "Output a value (Standard output)\n\n\
\(example)\n\
\  \"Hello, World!\" print => \"Hello, World!\"\n\
\  => []"


docPutStr :: DocString
docPutStr = "Output a string value (Standard output)\n\n\
\(example)\n\
\  \"Hello!\" putstr => Hello!\n\
\  => []"

docAsk :: DocString
docAsk = "Read line from the standard input\n\n\
\(example)\n\
\  \"Your name: \" ask\n\
\  Your name: john\n\
\  => [\"john\"]"

docArgs :: DocString
docArgs = "Get the program's command line arguments\n\n\
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


docReadFile :: DocString
docReadFile = "Open a file and read the content\n\n\
\(example)\n\
\  \"file.txt\" read => [\"A file containing text!\"]"

docWrite :: DocString
docWrite = "Write text into a file\n\n\
\(example)\n\
\  \"file.txt\" \"Hello!\" write\n\
\ => []"

docUnquote :: DocString
docUnquote = "Evaluate instruction into a quote\n\n\
\(example)\n\
\  [[5 5 +] 5 5 +] unquote => [[5 5 +] 10]"


docPushr :: DocString
docPushr = "Push a value into a quote\n\n\
\(example)\n\
\  [5] 5 pushr => [[5 5]]"

docPopr :: DocString
docPopr = "Get out the top-element in the quote\n\
\n\n\
\(example)\n\
\  [1 2] popr => [[1] 2]"

docId :: DocString
docId = "Get the top-stack element (does nothing)\n\n\
\(example)\n\
\  stack: [5]\n\
\  id => [5]"

docStr :: DocString
docStr = "Type conversion\n\n\
\(example)\n\
\  5 str => [\"5\"]"

docInt :: DocString
docInt = "Type conversion\n\n\
\(example)\n\
\  \"10\" int => [10]"

docFloat :: DocString
docFloat = "Type conversion\n\n\
\(example)\n\
\  \"10.5\" float => [10.5]"

docExit :: DocString
docExit = "Exit program after the current instruction\n\n\
\(example)\n\
\ def main = {\n\
\ \"ERROR! ...\" print\n\
\ \"failure\" exit\n\
\ \"other instructions...\" print \n\
\ }\n\
\ /*\n\
\ Output:\n\
\ \"ERROR! ...\"\n\
\ *** Exception: ExitFailure\n\
\ */\n\
\ ---------\n\
\ def main = {\n\
\ \"other instructions...\" print\n\
\ \"success\" exit\n\
\ }\n\
\ /*\n\
\ Output: \n\
\ \"other instructions...\"\n\
\ */"

docFormat :: DocString
docFormat = "Format string, replace braces by corresponding value\n\n\
\(example)\n\
\ \"Hello, {}!\" [\"John\"] format => [\"Hello, John!\"]\n\
\ \"Numbers: {} {}\" [20 3.14] format => [\"Numbers: 20 3.14\"]\n\
\ \"Boolean: {} {}\" [True False] format => [\"Boolean: True False\"]\n\
\ \"Quotes: {} {}\" [[1 2 3] [[\"a\" 1] [\"b\" 2]]] format => [\"Quotes: [1 2 3] [[\\\"a\\\" 1] [\\\"b\\\" 2]]\"]\n\
\ \"Expression: {}\" [[5 5 +] unquote 2 *] format => [\"Expression: 20\"]"

docHelp :: DocString
docHelp = "Get a function's doc-string\n\n\
\(example)\n\
\ [pop] help \n\
\ Output: \n\
\ Remove the top-stack element\n\
\ (example)\n\
\   stack: [1 2]\n\
\   pop => [1]"