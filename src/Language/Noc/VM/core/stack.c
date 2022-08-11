#include <stdlib.h>
#include "errors.h"
#include "types.h"
#include "stack.h"

NocVM vm;

void free_stack(NocVM *vm) {
    free(vm->stack.array);
    free(vm->callstack.array);
}

bool isFull(NocStack* stack) {
    return stack->cursor >= (stack->capacity - 1);
}

void create_stack(NocStack* stack) {
    stack->capacity = 10000;
    stack->array = malloc(sizeof(NocValue) * stack->capacity);
    if(stack->array == NULL)
        throw_noc_error(OUT_OF_MEMORY_ERROR, "malloc cannot allocate more memory. (source: VM/core/stack.c => create_stack)", 0);
    stack->cursor = 0;
}

void push_stack(NocStack* stack, NocValue val) {
    if(isFull(stack)) {
        stack->capacity *= 2;
        stack->array = realloc(stack->array, sizeof(NocValue) * stack->capacity);
        if(stack->array == NULL)
            throw_noc_error(OUT_OF_MEMORY_ERROR, "malloc cannot allocate more memory. (source: VM/core/stack.c => push_stack)", 0);
    }
    stack->cursor++;
    stack->array[stack->cursor] = val;
}

NocValue pop_stack(NocStack* stack) {
    if(stack->cursor == 0) {
        throw_noc_error(EMPTY_STACK_ERROR, "cannot pop", 0);
    }
    return stack->array[stack->cursor--];
}

NocValue peek_stack(NocStack* stack) {
    if(stack->cursor == 0) {
        throw_noc_error(EMPTY_STACK_ERROR, "cannot dup", 0);
    }
    return stack->array[stack->cursor];
}