#include <stdio.h>
#include <stdlib.h>
#include "core/opcodes.h"
#include "core/deserializer.h"
#include <math.h>

NocVM vm;

void run(NocBytecode b) {
    for(int i = 0; i < b.opcodes.size; i++) {
        void (*f)(NocBytecode, NocOp) = OPCODES_FUNCS[b.opcodes.elems.opcode[i].label];
        f(b, b.opcodes.elems.opcode[i]);
    }
}

int main(char* arg, char* argv[]) {
    create_stack(&vm.stack);
    create_stack(&vm.callstack);

    // argv[1] => bytecode path
    NocBytecode bytecode = deserialize(argv[1]);
    run(bytecode);
    
    //list_array(&vm.stack);

    return 0;
}