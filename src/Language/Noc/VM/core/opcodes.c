#include "types.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

extern NocVM vm;

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
            }
        }
        push_stack(&vm.stack, result);
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