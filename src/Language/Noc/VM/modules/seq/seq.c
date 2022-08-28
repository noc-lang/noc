#include <stdlib.h>
#include "../../core/types.h"
#include "../../core/errors.h"
#include "../../core/stack.h"
#include "../../core/opcodes.h"

void noc_step(NocBytecode b) {
    NocValue v = pop_stack(&vm.stack);
    NocValue v2 = pop_stack(&vm.stack);
    if(v.label == QUOTE_VAL) {
        if(v2.label == QUOTE_VAL) {
            NocValue q;
            q.label = QUOTE_VAL;
            NocStack s;
            create_stack(&s, v2.quote.capacity);
            q.quote = s;

            NocOp op = {.label = UNQUOTE_QUOTE, .operand = -1};
            void (*f)(NocBytecode, NocOp) = OPCODES_FUNCS[op.label];

            for(int i = 1; i < v2.quote.cursor+1; i++) {
                push_stack(&vm.stack, v2.quote.array[i]);
                push_stack(&vm.stack, v);
                f(b, op);
                push_stack(&q.quote, pop_stack(&vm.stack));
            }
            push_stack(&vm.stack, q);
        } else
            throw_noc_error(TYPE_ERROR, "the first argument has %s value", 1, noc_value_to_str(v2.label));
    } else
        throw_noc_error(TYPE_ERROR, "the second argument has %s value", 1, noc_value_to_str(v.label));
}

void noc_fold(NocBytecode b) {
    NocValue v = pop_stack(&vm.stack);
    NocValue v2 = pop_stack(&vm.stack);
    NocValue v3 = pop_stack(&vm.stack);
    push_stack(&vm.stack, v2);
    if(v3.label == QUOTE_VAL) {
        if(v.label == QUOTE_VAL) {
            NocOp op = {.label = UNQUOTE_QUOTE, .operand = -1};
            void (*f)(NocBytecode, NocOp) = OPCODES_FUNCS[op.label];
            for(int i = 1; i < v3.quote.cursor+1; i++) {
                push_stack(&vm.stack, v3.quote.array[i]);
                push_stack(&vm.stack, v);
                f(b, op);
            }
            push_stack(&vm.stack, pop_stack(&vm.stack));
        } else
            throw_noc_error(TYPE_ERROR, "the third argument has %s value", 1, noc_value_to_str(v.label));
    } else
        throw_noc_error(TYPE_ERROR, "the first argument has %s value", 1, noc_value_to_str(v3.label));
}