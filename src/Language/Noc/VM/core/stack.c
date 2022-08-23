#include <stdio.h>
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

void print_stack(NocStack stack) {
    printf("[");
    for(int i = 1; i < stack.cursor+1; i++) {
        if(stack.array[i].label == INT_VAL) {
            printf("%ld", stack.array[i].i);
        } else if(stack.array[i].label == FLOAT_VAL) {
            printf("%f", stack.array[i].f);
        } else if(stack.array[i].label == STRING_VAL) {
            printf("\"%s\"", stack.array[i].s);
        } else if(stack.array[i].label == CHAR_VAL) {
            printf("'%c'", stack.array[i].c);
        } else if(stack.array[i].label == BOOL_VAL) {
            printf("%s", stack.array[i].b ? "True" : "False");
        } else if(stack.array[i].label == SYMBOL_VAL) {
            if(stack.array[i].symbol->label == OP) {
                printf("sym{%s}", noc_opcode_operator_to_str(stack.array[i].symbol->opcode.label));
            } else {
                printf("sym{%s}", stack.array[i].symbol->name);
            }
        } else if(stack.array[i].label == QUOTE_VAL) {
            print_stack(stack.array[i].quote);
        }
        if(i < stack.cursor) { printf(" "); }
    }
    printf("]");
}

void create_stack(NocStack* stack, int capacity) {
    stack->capacity = capacity;
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