#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "types.h"
#include "errors.h"
#include "stack.h"
#include "runtime.h"

void noc_push_const(NocBytecode b, NocOp opcode) {    
    push_stack(&vm.stack, b.consts.constant[opcode.operand]);
}

void noc_opcode_operator(NocBytecode b, NocOp opcode) {
    NocValue v1 = pop_stack(&vm.stack);
    NocValue v2 = pop_stack(&vm.stack);
    NocValue result;
    if((v1.label == INT_VAL || v1.label == FLOAT_VAL) && (v2.label == INT_VAL || v2.label == FLOAT_VAL)) { 
        if(v1.label == INT_VAL && v2.label == INT_VAL) {
            result.label = INT_VAL;

            switch(opcode.label) {
                case ADD_OP:
                    result.i = v2.i + v1.i;
                    break;
                case MINUS_OP:
                    result.i = v2.i - v1.i;
                    break;
                case MUL_OP:
                    result.i = v2.i * v1.i;
                    break;
                case DIV_OP:
                    result.label = FLOAT_VAL;
                    result.f = (double)v2.i / (double)v1.i;
                    break;
                case EXP_OP:
                    result.i = pow(v2.i,v1.i);
                    break;
                case EQUAL:
                    result.label = BOOL_VAL;
                    result.b = v2.i == v1.i;
                    break;
                case GREATER_CMP:
                    result.label = BOOL_VAL;
                    result.b = v2.i > v1.i;
                    break;
                case LESS_CMP:
                    result.label = BOOL_VAL;
                    result.b = v2.i < v1.i;
                    break;
                case GREATER_OR_EQ_CMP:
                    result.label = BOOL_VAL;
                    result.b = v2.i >= v1.i;
                    break;
                case LESS_OR_EQ_CMP:
                    result.label = BOOL_VAL;
                    result.b = v2.i <= v1.i;
                    break;
            }
            
        }
        if(v1.label == INT_VAL && v2.label == FLOAT_VAL) {
            result.label = FLOAT_VAL;

            switch(opcode.label) {
                case ADD_OP:
                    result.f = v2.f + v1.i;
                    break;
                case MINUS_OP:
                    result.f = v2.f - v1.i;
                    break;
                case MUL_OP:
                    result.f = v2.f * v1.i;
                    break;
                case DIV_OP:
                    result.f = v2.f / v1.i;
                    break;
                case EXP_OP:
                    result.f = pow(v2.f, v1.i);
                    break;
                case EQUAL:
                    result.label = BOOL_VAL;
                    result.b = v2.f == v1.i;
                    break;
                case GREATER_CMP:
                    result.label = BOOL_VAL;
                    result.b = v2.f > v1.i;
                    break;
                case LESS_CMP:
                    result.label = BOOL_VAL;
                    result.b = v2.f < v1.i;
                    break;
                case GREATER_OR_EQ_CMP:
                    result.label = BOOL_VAL;
                    result.b = v2.f >= v1.i;
                    break;
                case LESS_OR_EQ_CMP:
                    result.label = BOOL_VAL;
                    result.b = v2.f <= v1.i;
                    break;
            }

        }
        if(v1.label == FLOAT_VAL && v2.label == FLOAT_VAL) {
            result.label = FLOAT_VAL;
            
            switch(opcode.label) {
                case ADD_OP:
                    result.f = v2.f + v1.f;
                    break;
                case MINUS_OP:
                    result.f = v2.f - v1.f;
                    break;
                case MUL_OP:
                    result.f = v2.f * v1.f;
                    break;
                case DIV_OP:
                    result.f = v2.f / v1.f;
                    break;
                case EXP_OP:
                    result.f = pow(v2.f, v1.f);
                    break;
                case EQUAL:
                    result.label = BOOL_VAL;
                    result.b = v2.f == v1.f;
                    break;
                case GREATER_CMP:
                    result.label = BOOL_VAL;
                    result.b = v2.f > v1.f;
                    break;
                case LESS_CMP:
                    result.label = BOOL_VAL;
                    result.b = v2.f < v1.f;
                    break;
                case GREATER_OR_EQ_CMP:
                    result.label = BOOL_VAL;
                    result.b = v2.f >= v1.f;
                    break;
                case LESS_OR_EQ_CMP:
                    result.label = BOOL_VAL;
                    result.b = v2.f <= v1.f;
                    break;
            }
        }
        if(v1.label == FLOAT_VAL && v2.label == INT_VAL) {
            result.label = FLOAT_VAL;

            switch(opcode.label) {
                case ADD_OP:
                    result.f = v2.i + v1.f;
                    break;
                case MINUS_OP:
                    result.f = v2.i - v1.f;
                    break;
                case MUL_OP:
                    result.f = v2.i * v1.f;
                    break;
                case DIV_OP:
                    result.f = v2.i / v1.f;
                    break;
                case EXP_OP:
                    result.f = pow(v2.i, v1.f);
                    break;
                case EQUAL:
                    result.label = BOOL_VAL;
                    result.b = v2.i == v1.f;
                    break;
                case GREATER_CMP:
                    result.label = BOOL_VAL;
                    result.b = v2.i > v1.f;
                    break;
                case LESS_CMP:
                    result.label = BOOL_VAL;
                    result.b = v2.i < v1.f;
                    break;
                case GREATER_OR_EQ_CMP:
                    result.label = BOOL_VAL;
                    result.b = v2.i >= v1.f;
                    break;
                case LESS_OR_EQ_CMP:
                    result.label = BOOL_VAL;
                    result.b = v2.i <= v1.f;
                    break;
            }
        }
        push_stack(&vm.stack, result);
    } else {
        throw_noc_error(TYPE_ERROR, "cannot call '%s' operator with a %s value and %s value", 3, noc_opcode_operator_to_str(opcode.label), noc_value_to_str(v2.label), noc_value_to_str(v1.label));
    }
}

void noc_dup(NocBytecode b, NocOp opcode) {
    push_stack(&vm.stack, peek_stack(&vm.stack));
}

void noc_pop(NocBytecode b, NocOp opcode) {
    pop_stack(&vm.stack);
}

void noc_zap(NocBytecode b, NocOp opcode) {
    vm.stack.cursor = 0;
}

void noc_cat(NocBytecode b, NocOp opcode) {
    NocValue v1 = pop_stack(&vm.stack);
    NocValue v2 = pop_stack(&vm.stack);
    if(v1.label == STRING_VAL && v2.label == STRING_VAL) {
        char* result = calloc((strlen(v1.s) + strlen(v2.s)) + 1, sizeof(char));
        if(result == NULL)
            throw_noc_error(OUT_OF_MEMORY_ERROR, "malloc cannot allocate more memory. (source: VM/core/opcodes.c => noc_cat)", 0);
        strcat(result, v2.s);
        strcat(result, v1.s);
        NocValue res = {.label = STRING_VAL, .s = result};
        push_stack(&vm.stack, res);
    } else if(v1.label == QUOTE_VAL && v2.label == QUOTE_VAL) {
        NocValue new_quote;
        new_quote.label = QUOTE_VAL;
        create_stack(&new_quote.quote, v1.quote.capacity  + v2.quote.capacity - 2);
        
        v2.quote.cursor = 0;
        for(int i = 0; i < v2.quote.capacity-1; i++) {
            v2.quote.cursor++;
            push_stack(&new_quote.quote, peek_stack(&v2.quote));
        }

        v1.quote.cursor = 0;
        for(int i = 0; i < v1.quote.capacity-1; i++) {
            v1.quote.cursor++;
            push_stack(&new_quote.quote, peek_stack(&v1.quote));
        }
        
        push_stack(&vm.stack, new_quote);
    } else {
        throw_noc_error(TYPE_ERROR, "cannot concatenate %s value with %s value", 2, noc_value_to_str(v2.label), noc_value_to_str(v1.label));
    }
}

void noc_rotnm(NocBytecode b, NocOp opcode) {
    NocValue v1 = pop_stack(&vm.stack);
    NocValue v2 = pop_stack(&vm.stack);
    if(v1.label == INT_VAL && v2.label == INT_VAL) {
        int tmp_cursor;

        // example for "4 1 rotNM": (stack) 0 7 5 2 3 | cursor = 4

        if(v1.i > 0) {
            tmp_cursor = vm.stack.cursor;
        } else { // cursor = 1
            vm.stack.cursor -= (v2.i - 1);
            tmp_cursor = vm.stack.cursor;
        }
     
        int i = abs(v1.i);
        int j = v2.i;

        while(i > 0) {
            while(j > 0) {
                NocValue elem = peek_stack(&vm.stack);

                if(v1.i > 0) {
                    vm.stack.cursor -= (j-1); // cursor = 4 | 0 (7) 5 2 [3]
                } else {
                    vm.stack.cursor += (j-1); // cursor = 1 | 0 [7] 5 2 (3)
                }
            
                NocValue elem2 = peek_stack(&vm.stack);
                // 0 [7] 5 2 [3] (elem = 3 | elem2 = 7) first iteration
                // 0 3 [5] 2 [7] (elem = 7 | elem2 = 5) second iteration
                vm.stack.array[vm.stack.cursor] = elem;
                vm.stack.cursor = tmp_cursor;
                vm.stack.array[vm.stack.cursor] = elem2;
                // 0 [3] 5 2 [7] first iteration
                // 0 [7] 2 [5] second iteration
                j -= 1;
            }
            j = v2.i;
            i -= 1;
        }
        // final result: 0 7 3 5 2

        if(v1.i < 0) // reset the stack cursor if the second argument is negative
            vm.stack.cursor += (v2.i-1);
    } else {
        throw_noc_error(TYPE_ERROR, "cannot rotNM with the type %s and %s", 2, noc_value_to_str(v2.label), noc_value_to_str(v1.label));
    }
}

void noc_opcode_cmp(NocBytecode b, NocOp opcode) {
    NocValue v1 = pop_stack(&vm.stack);
    NocValue v2 = pop_stack(&vm.stack);
    NocValue result;
    result.label = BOOL_VAL;

    if(v1.label == STRING_VAL && v2.label == STRING_VAL)
        result.b = v2.s == v1.s;
    else if(v1.label == CHAR_VAL && v2.label == CHAR_VAL)
        result.b = v2.c == v1.c;
    else if(v1.label == BOOL_VAL && v2.label == BOOL_VAL)
        result.b = v2.b == v1.b;
    else
        result.b = false;

    if((v1.label == INT_VAL || v1.label == FLOAT_VAL) && (v2.label == INT_VAL || v2.label == FLOAT_VAL)) {
        push_stack(&vm.stack, v2);
        push_stack(&vm.stack, v1);
        noc_opcode_operator(b, opcode);
    } else {
        push_stack(&vm.stack, result);
    }
}

void noc_opcode_bool(NocBytecode b, NocOp opcode) {
    NocValue v1 = pop_stack(&vm.stack);
    NocValue v2 = pop_stack(&vm.stack);
    if(v1.label == BOOL_VAL && v2.label == BOOL_VAL) {
        NocValue result;
        result.label = BOOL_VAL;
        switch(opcode.label) {
            case AND_BOOL:
                result.b = v2.b && v1.b;
                break;
            case OR_BOOL:
                result.b = v2.b || v1.b;
                break;
        }
        push_stack(&vm.stack, result);
    } else {
        throw_noc_error(TYPE_ERROR, "cannot call '%s' function with the %s value and %s value", 3, noc_opcode_operator_to_str(opcode.label), noc_value_to_str(v2.label), noc_value_to_str(v1.label));
    }
}

void noc_create_quote(NocBytecode b, NocOp opcode) {
    NocValue v;
    v.label = QUOTE_VAL;
   
    create_stack(&v.quote, opcode.operand+1);

    vm.stack.cursor -= opcode.operand;

    for(int i = 0; i < opcode.operand; i++) {
        vm.stack.cursor++;
        push_stack(&v.quote, peek_stack(&vm.stack));
    }

    vm.stack.cursor -= opcode.operand;
    push_stack(&vm.stack, v);
}

void noc_unquote(NocBytecode b, NocOp opcode) {
    NocValue v = pop_stack(&vm.stack);
    if(v.label == QUOTE_VAL) {
        int tmp = v.quote.cursor;
        v.quote.cursor = 1;
        for(int i = 0; i < tmp; i++) {
            NocValue elem = peek_stack(&v.quote);
            if(elem.label == SYMBOL_VAL) {
                if(elem.symbol->label == OP)
                    call_opcode(b, elem.symbol->opcode);
                else if(elem.symbol->label == NOC_FUNC) {
                    run(b, elem.symbol->p);
                } else if(elem.symbol->label == PRIM)
                    call_prim(elem.symbol);
            } else
                push_stack(&vm.stack, elem);
            v.quote.cursor++;
        }
    } else
        throw_noc_error(TYPE_ERROR, "cannot unquote a %s value", 1, noc_value_to_str(v.label));
}

void noc_push_sym(NocBytecode b, NocOp opcode) {
    Sym *s = malloc(sizeof(Sym));
    s = &b.sym.sym[opcode.operand];
    NocValue v = {.label = SYMBOL_VAL, .symbol = s};
    push_stack(&vm.stack, v);
}

void noc_pushr(NocBytecode b, NocOp opcode) {
    NocValue v = pop_stack(&vm.stack);
    NocValue v2 = pop_stack(&vm.stack);
    if(v2.label == QUOTE_VAL) {
        push_stack(&v2.quote, v);
        push_stack(&vm.stack, v2);
    } else
        throw_noc_error(TYPE_ERROR, "cannot pushr a %s value", 1, noc_value_to_str(v2.label));
}

void noc_popr(NocBytecode b, NocOp opcode) {
    NocValue v = pop_stack(&vm.stack);
    if(v.label == QUOTE_VAL) {
        NocValue elem = pop_stack(&v.quote);
        push_stack(&vm.stack, v);
        push_stack(&vm.stack, elem);
    } else
        throw_noc_error(TYPE_ERROR, "cannot popr a %s value", 1, noc_value_to_str(v.label));
}

// Memory adresses of opcodes
void* OPCODES_FUNCS[] = {
    NULL, // call_symbol
    &noc_push_const, // push_const
    NULL, // return
    &noc_create_quote, // create_quote
    &noc_popr, // popr
    &noc_pushr, // pushr
    &noc_unquote, // unquote
    &noc_push_sym, // push_sym
    &noc_dup, // dup
    &noc_pop, // pop
    &noc_zap, // zap
    &noc_cat, // cat
    &noc_rotnm, // rotNM
    &noc_opcode_operator, // +
    &noc_opcode_operator, // -
    &noc_opcode_operator, // *
    &noc_opcode_operator, // /
    &noc_opcode_operator, // ^
    &noc_opcode_operator, // >
    &noc_opcode_operator, // <
    &noc_opcode_operator, // >=
    &noc_opcode_operator, // <=
    &noc_opcode_cmp, // ==
    &noc_opcode_bool, // and
    &noc_opcode_bool, // or
};