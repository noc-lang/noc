#ifndef TYPES_H
#define TYPES_H

#include <stdbool.h>
#include <stdint.h>

// NocValue type
typedef enum NocValueLabel { FLOAT_VAL, INT_VAL, STRING_VAL, CHAR_VAL, BOOL_VAL } NocValueLabel;
typedef struct NocValue {
    NocValueLabel label;
    union {
        double f;
        long i;
        char *s;
        char c;
        bool b;
    };
} NocValue;

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
typedef enum SymTableLabel { NOC_FUNC, PRIM, OP } SymTableLabel;
typedef struct SymTable {
    SymTableLabel label;
    union {
        int p; // noc_func
        void (*func)(); // for prim
        NocOp opcode; // for opcode
    };
} SymTable;

typedef struct DocTable {
    char* docstring;
    int pos;
} DocTable;

typedef enum NocTableLabel { SYMBOL, CONSTANT, DOC, OPCODE } NocTableLabel;
typedef struct Table {
    NocTableLabel label;
    union {
        SymTable* sym;
        NocValue* constant;
        DocTable* doc;
        NocOp* opcode;
    };
} Table;

// ----------------
typedef struct OpCodes {
    int64_t size;
    Table elems;
} OpCodes;

typedef struct NocBytecode {
    Table sym;
    Table consts;
    Table doc;
    OpCodes opcodes;
} NocBytecode;
// ----------------

typedef struct NocStack {
    NocValue *array;
    int capacity;
    int cursor;
    int size;
} NocStack;

bool isFull(NocStack* stack);
void create_stack(NocStack* s);
void push_stack(NocStack* stack, NocValue val);
NocValue pop_stack(NocStack* stack);
NocValue peek_stack(NocStack* stack);
void list_array(NocStack *stack);

typedef struct NocVM {
    NocStack stack;
    NocStack callstack;
} NocVM;

#endif
