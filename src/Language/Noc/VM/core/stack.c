#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include "types.h"

bool isFull(NocStack* stack) {
    return stack->cursor >= (stack->capacity - 1);
}

void create_stack(NocStack* stack) {
    stack->capacity = 5;
    stack->array = malloc(sizeof(NocValue) * stack->capacity);
    assert(stack->array != NULL);
    stack->cursor = 0;
}

void push_stack(NocStack* stack, NocValue val) {
    if(isFull(stack)) {
        stack->capacity *= 2;
        stack->array = realloc(stack->array, sizeof(NocValue) * stack->capacity);
        assert(stack->array != NULL);
    }
    stack->cursor++;
    stack->array[stack->cursor] = val;
}

NocValue pop_stack(NocStack* stack) {
    if(stack->cursor == 0) {
        exit(1);
    }
    return stack->array[stack->cursor--];
}

NocValue peek_stack(NocStack* stack) {
    return stack->array[stack->cursor];
}

void list_array(NocStack *stack) {
    printf("cursor: %d\n", stack->cursor);
    printf("[ ");
    for(int i = 0; i < stack->capacity; i++) {
        printf("%ld, ", stack->array[i].i);
    }
    printf("]\n\n");
}