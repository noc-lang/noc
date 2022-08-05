#include <stdio.h>
#include <stdlib.h>
#include "core/opcodes.h"
#include "core/deserializer.h"
#include "core/stack.h"

NocVM vm;

void run(NocBytecode b) {
    for(int i = 0; i < b.opcodes.size; i++) {
        void (*f)(NocBytecode, NocOp) = OPCODES_FUNCS[b.opcodes.elems.opcode[i].label];
        f(b, b.opcodes.elems.opcode[i]);
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