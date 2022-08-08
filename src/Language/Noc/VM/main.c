#include <stdio.h>
#include <stdlib.h>
#include "core/opcodes.h"
#include "core/deserializer.h"
#include "core/stack.h"

NocVM vm;

// void listArray(NocStack s) {
//     printf("cursor = %d\n", s.cursor);
//     for(int i = 0; i < 5; i++) {
//         printf("%d - ", s.array[i].b);
//     }
//     printf("\n");
// }

void run(NocBytecode b) {
    int i = 0;
    while(i < b.opcodes.size) {
        if(vm.callstack.cursor < 10) {
            listArray(vm.callstack);
        }
        if(b.opcodes.elems.opcode[i].label == RETURN) {
            if(vm.callstack.cursor > 0) {
                NocValue index = pop_stack(&vm.callstack);
                i = index.i;
            } else { exit(EXIT_SUCCESS); }
        } else if(b.opcodes.elems.opcode[i].label == CALL_SYMBOL) {
            SymTable sym_elem = b.sym.sym[b.opcodes.elems.opcode[i].operand];
            if(sym_elem.label == PRIM) {
                void (*f)() = sym_elem.func;
                f();
                i++;
            } else {
                NocValue index = {.label = INT_VAL, .i = i+1};
                push_stack(&vm.callstack, index);
                i = sym_elem.p;
            }
        } else {
            void (*f)(NocBytecode, NocOp) = OPCODES_FUNCS[b.opcodes.elems.opcode[i].label];
            f(b, b.opcodes.elems.opcode[i]);
            i++;
        }
    }
}

int main(char* arg, char* argv[]) {
    NocBytecode bytecode;

    // Initializing stacks
    create_stack(&vm.stack);
    create_stack(&vm.callstack);
    
    deserialize(&bytecode, argv[1]); // argv[1] => bytecode path
    run(bytecode);

    free_stack(&vm);
    free_bytecode(&bytecode);

    return EXIT_SUCCESS;
}