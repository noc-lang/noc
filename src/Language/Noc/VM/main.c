#include <stdlib.h>
#include "core/deserializer.h"
#include "core/stack.h"
#include "core/runtime.h"

int main(char* arg, char* argv[]) {
    NocBytecode bytecode;

    // Initializing stacks
    create_stack(&vm.stack);
    create_stack(&vm.callstack);
    
    deserialize(&bytecode, argv[1]); // argv[1] => bytecode path
    run(bytecode, 0);

    free_stack(&vm);
    free_bytecode(&bytecode);

    return EXIT_SUCCESS;
}