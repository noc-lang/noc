#ifndef TYPES_H
#define TYPES_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

// NocValue type
typedef struct NocValue NocValue;
typedef struct Sym Sym;
typedef struct NocBytecode NocBytecode;

typedef struct NocStack {
    NocValue *array;
    size_t capacity;
    size_t cursor;
} NocStack;

typedef enum NocValueLabel { FLOAT_VAL, INT_VAL, STRING_VAL, CHAR_VAL, BOOL_VAL, SYMBOL_VAL, QUOTE_VAL } NocValueLabel;
struct NocValue {
    NocValueLabel label;
    union {
        double f;
        long i;
        char *s;
        char c;
        bool b;
        Sym* symbol;
        NocStack quote;
    };
};

// NocOp type
typedef enum NocOpLabel { 
    CALL_SYMBOL,
    PUSH_CONST,
    RETURN,
    CREATE_QUOTE,
    POPR_QUOTE,
    PUSHR_QUOTE,
    UNQUOTE_QUOTE,
    PUSH_SYM,
    DUP,
    POP,
    CLEAR_STACK,
    CONCAT,
    ROT,
    ADD_OP,
    MINUS_OP,
    MUL_OP,
    DIV_OP,
    EXP_OP,
    GREATER_CMP,
    LESS_CMP,
    GREATER_OR_EQ_CMP,
    LESS_OR_EQ_CMP,
    EQUAL,
    AND_BOOL,
    OR_BOOL
} NocOpLabel;

typedef struct NocOp {
    NocOpLabel label;
    int64_t operand;
} NocOp;

// Noc tables type
typedef enum SymLabel { NOC_FUNC, PRIM, OP } SymLabel;
struct Sym {
    SymLabel label;
    char* name;
    union {
        int p; // noc_func
        void (*func)(NocBytecode); // for prim
        NocOp opcode; // for opcode
    };
};

typedef struct Doc {
    char* name;
    char* docstring;
} Doc;

typedef struct Docs {
    size_t size_doc;
    Doc* doc;
} Docs;

typedef struct OpCodes {
    size_t size;
    NocOp* elems;
} OpCodes;

typedef enum NocTableLabel { SYMBOL, CONSTANT, DOC, OPCODE } NocTableLabel;
typedef struct Table {
    NocTableLabel label;
    union {
        Sym* sym;
        NocValue* constant;
        Docs doc;
        OpCodes opcodes;
    };
} Table;

struct NocBytecode {
    Table sym;
    Table consts;
    Table doc;
    Table opcodes;
};

#endif
