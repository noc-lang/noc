#include <stdlib.h>
#include "deserializer.h"
#include "opcodes.h"
#include "types.h"
#include "stack.h"

void call_prim(Sym *s, NocBytecode b) {
    void (*f)(NocBytecode) = s->func;
    f(b);
}

void call_noc_func(int pos) {
    NocValue index = {.label = INT_VAL, .i = pos+1};
    push_stack(&vm.callstack, index);  
}

void call_opcode(NocBytecode b, NocOp op) {
    void (*f)(NocBytecode, NocOp) = OPCODES_FUNCS[op.label];
    f(b, op);
}

void run(NocBytecode b, int pos) {
    while(pos < b.opcodes.opcodes.size) {
        if(b.opcodes.opcodes.elems[pos].label == RETURN) {
            if(vm.callstack.cursor > 0)
                pos = pop_stack(&vm.callstack).i;
            else
                return;
        } else if(b.opcodes.opcodes.elems[pos].label == CALL_SYMBOL) {
            Sym sym_elem = b.sym.sym[b.opcodes.opcodes.elems[pos].operand];
            if(sym_elem.label == PRIM) {
                call_prim(&sym_elem, b);
                pos++;
            } else {
                call_noc_func(pos);
                pos = sym_elem.p;
            }
        } else {
            call_opcode(b, b.opcodes.opcodes.elems[pos]);
            pos++;
        }
    }
}