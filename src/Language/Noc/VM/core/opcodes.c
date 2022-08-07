#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "types.h"
#include "errors.h"
#include "stack.h"

extern NocVM vm;

char* noc_opcode_operator_to_str(NocOpLabel label) {
    switch(label) {
        case ADD_OP:
            return "+";
        case MINUS_OP:
            return "-";
        case MUL_OP:
            return "*";
        case DIV_OP:    
            return "/";
        case EXP_OP:
            return "^";
        case GREATER_CMP:
            return ">";
        case LESS_CMP:
            return "<";
        case GREATER_OR_EQ_CMP:
            return ">=";
        case LESS_OR_EQ_CMP:
            return "<=";
        case EQUAL:
            return "==";
        case AND_BOOL:
            return "and";
        case OR_BOOL:
            return "or";
    }
}

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
                    result.i = v2.i / v1.i;
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
                    result.label == BOOL_VAL;
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
                    result.label == BOOL_VAL;
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
                    result.label == BOOL_VAL;
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
                    result.label == BOOL_VAL;
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

void noc_return(NocBytecode b, NocOp opcode) {
    if(vm.callstack.size > 0) {
        pop_stack(&vm.callstack);
    }
}

void noc_call_symbol(NocBytecode b, NocOp opcode) {
    if(b.sym.sym[opcode.operand].label == PRIM) {
        void (*f)() = b.sym.sym[opcode.operand].func;
        f();
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
        char* result = malloc(sizeof(char) * (strlen(v1.s) + strlen(v2.s)));
        if(result == NULL)
            throw_noc_error(OUT_OF_MEMORY_ERROR, "malloc cannot allocate more memory. (source: VM/core/opcodes.c => noc_cat)", 0);
        strcat(result, v2.s);
        strcat(result, v1.s);
        NocValue res = {STRING_VAL, .s = result};
        push_stack(&vm.stack, res);
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