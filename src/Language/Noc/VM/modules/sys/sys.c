#include <stdlib.h>
#include "../../core/types.h"
#include "../../core/errors.h"
#include "../../core/stack.h"

void noc_exit(NocBytecode b) {
    NocValue v = pop_stack(&vm.stack);
    if(v.label == INT_VAL) {
        exit(v.i);
    } else
        throw_noc_error(TYPE_ERROR, "cannot error with %s value", 1, noc_value_to_str(v.label));

}
