#include <string.h>
#include <stdio.h>
#include "../core/types.h"

char* render_op_doc(NocOpLabel label) {
    switch(label) {
        case DUP:
            return "Duplicate the top-stack element\n\
(example)\n\
    stack: [1 2]\n\
    dup => [1 2 2]"; 

        case POP:
            return "Remove the top-stack element\n\
(example)\n\
    stack: [1 2]\n\
    pop => [1]";

        case CLEAR_STACK:
            return "Clear all the stack\n\
(example)\n\
    stack: [1 2 3]\n\
    zap: []";

        case CONCAT:
            return "Concatenate 2 values (string and quotes)\n\
(example)\n\
  [1] [2] cat => [1 2]\n\
  \"Hello, \" \"World!\" cat => \"Hello, World!\"";

        case ROT:
            return "Rotate the stack N elements M times\n\
(example)\n\
    stack: [1 2 3]\n\
    2 1 rotNM => [1 3 2]\n\
    3 -1 rotNM => [2 3 1]";

        case ADD_OP:
            return "Sum of the 2 top-stack elements\n\
(example)\n\
    stack: [5 6]\n\
    + => [11]";

        case MINUS_OP:
            return "Difference of the 2 top-stack elements\n\
(example)\n\
    stack: [10 9]\n\
    - => [1]";

        case DIV_OP:
            return "Quotient of the 2 top-stack elements\n\
(example)\n\
    stack: [10 2]\n\
    / => [5.0]";

        case EXP_OP:
            return "The exponent of a number\n\
(example)\n\
    stack: [10 2]\n\
    ^ => [100]";

        case GREATER_CMP:
            return "Compare if the first value is greater than the second value\n\
(example)\n\
    5 6 > => [False]";

        case LESS_CMP:
            return "Compare if the first value is less than the second value\n\
(example)\n\
    stack: [5 6]\n\
    < => [True]";

        case GREATER_OR_EQ_CMP:
            return "Compare if the first value is greater or equal than the second value\n\
(example)\n\
    stack: [5 5]\n\
    >= => [True]";

        case LESS_OR_EQ_CMP:
            return "Compare if the first value is less or equal than the second value\n\
(example)\n\
    stack: [4 5]\n\
    <= => [True]";

        case EQUAL:
            return "Check if 2 values are equal\n\
(example)\n\
    5 6 == => [False]\n\
    10.2 10 0.2 + == => [True]";

        case AND_BOOL:
            return "The 'AND' logic operator\n\
(example)\n\
    True True and => [True]";

        case OR_BOOL:
            return "The 'OR' logic operator\n\
(example)\n\
    False False or => [False]";

        case PUSHR_QUOTE:
            return "Push a value into a quote\n\
(example)\n\
    [5] 5 pushr => [[5 5]]";

        case POPR_QUOTE:
            return "Get out the top-element in the quote\n\
(example)\n\
    [1 2] popr => [[1] 2]";

        case UNQUOTE_QUOTE:
            return "Evaluate instruction into a quote\n\
(example) [[5 5 +] 5 5 +] unquote => [[5 5 +] 10]";
    }
}

char* render_prim_doc(char* funcname) {
    if(strcmp(funcname, "id") == 0) {

    } else if(strcmp(funcname, "str") == 0) {
        return "Type conversion\n\
(example)\n\
    5 str => [\"5\"]";
    } else if(strcmp(funcname, "int") == 0) {
        return "Type conversion\n\
(example)\n\
    \"10\" int => [10]";
    } else if(strcmp(funcname, "float") == 0) {
        return "Type conversion\n\
(example)\n\
    \"10.5\" float => [10.5]";
    } else if(strcmp(funcname, "bool") == 0) {
        return "Convert a value to boolean value\n\
(example)\n\
    1 Bool => [True]";
    } else if(strcmp(funcname, "help") == 0) {
        return "helpception ^^";
    } else if(strcmp(funcname, "case") == 0) {
        return "Pattern matching combinator\n\
(example)\n\
 def fact = { \n\
   [\n\
     [[1] [1]]\n\
     [[_] [dup 1 - dup fact *]] # wildcard (match any pattern)\n\
   ] case\n\
 }\n\
 \n\
 def main = { \n\
   6 dup fact print \n\
 }";
    } else if(strcmp(funcname, "trace") == 0) {
        return "Trace the Noc stack\n\
(example)\n\
 1 2 3 trace + => stack: [1 5]\n\
 output: [1 2 3]";
    } else if(strcmp(funcname, "chr") == 0) {
        return "Get the character Unicode representation of the decimal number\n\
(example)\n\
 97 chr => ['a']";
    } else if(strcmp(funcname, "ord") == 0) {
        return "Get the decimal representation of a Unicode character\n\
(example)\n\
 'A' ord => [65]";
    } else if(strcmp(funcname, "print") == 0) {
        return "Output a value (Standard output)\n\
(example)\n\
  \"Hello, World!\" print => \"Hello, World!\"\n\
  => []";
    } else if(strcmp(funcname, "ask") == 0) {
        return "Read line from the standard input\n\
(example)\n\
  \"Your name: \" ask\n\
  Your name: john\n\
  => [\"john\"]";
    } else if(strcmp(funcname, "putstr") == 0) {
        return "Output a string value (Standard output)\n\
(example)\n\
  \"Hello!\" putstr => Hello!\n\
  => []";
    } else if(strcmp(funcname, "open") == 0) {
        return "Proceed to 'read', 'write', 'append' actions to files\n\
(example)\n\
    \"filename\" \"\" \"r\" open => [\"This is a content.\\n\"]\n\
    pop \"filename\" \"content\" \"w\" open => []\n\
    \"filename\" \"\\ncontent2\" \"a\" open => []\n\n\
    Modes combined: \n\
    pop \"filename\" \"This is a new content\" \"r+\" open => [\"This is a new content\"]";
    } else if(strcmp(funcname, "format") == 0) {
        return "Format string, replace braces by corresponding value\n\
(example)\n\
    \"Hello, {}!\" [\"John\"] format => [\"Hello, John!\"]\n\
    \"Numbers: {} {}\" [20 3.14] format => [\"Numbers: 20 3.14\"]\n\
    \"Boolean: {} {}\" [True False] format => [\"Boolean: True False\"]\n\
    \"Quotes: {} {}\" [[1 2 3] [[\"a\" 1] [\"b\" 2]]] format => [\"Quotes: [1 2 3] [[\\\"a\\\" 1] [\\\"b\\\" 2]]\"]\n\
    \"Expression: {}\" [[5 5 +] unquote 2 *] format => [\"Expression: 20\"]";
    } else if(strcmp(funcname, "tostr") == 0) {
        return "Convert a quote of chars to string\n\n\
(example)\n\
    ['a' 'b' 'c'] tostr => [\"abc\"]";
    } else if(strcmp(funcname, "chars") == 0) {
        return "Convert a string to a quote of chars\n\n\
(example)\n\
    \"abc\" chars => ['a' 'b' 'c']";
    } else if(strcmp(funcname, "exit") == 0) {
        return "Exit program after the current instruction\n\
(example)\n\
    def main = {\n\
    \"ERROR! ...\" print\n\
    1 exit\n\
    \"other instructions...\" print \n\
    }\n\
    /*\n\
    Output:\n\
    \"ERROR! ...\"\n\
    *** Exception: ExitFailure 1\n\
    */\n\
    ---------\n\
    def main = {\n\
    \"other instructions...\" print\n\
    0 exit\n\
    }\n\
    /*\n\
    Output: \n\
    \"other instructions...\"\n\
    */";
    } else if(strcmp(funcname, "step") == 0) {
        return "Execute a specific function for each element of a quote\n\
quote [function] step \n\n\
(example)\n\
 [\"noc\" \"hello\" \"world!\"] [$ len] step => [[3 5 6]]";
    } else if(strcmp(funcname, "fold") == 0) {
        return "Reduce a quote of elements to one value in accumulating each elements with a specific function\n\
quote (initial_value -> accumulator) [function] fold \n\
(example)\n\
 [1 2 3] 0 [+] fold => [6]";
    } else
        return "(help) No entry for this function.";
}