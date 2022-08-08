#ifndef STACK_H
#define STACK_H

#include "types.h"

typedef struct NocStack {
    NocValue *array;
    int capacity;
    int cursor;
} NocStack;

typedef struct NocVM {
    NocStack stack;
    NocStack callstack;
} NocVM;

void free_stack(NocVM *vm);
bool isFull(NocStack* stack);
void create_stack(NocStack* s);
void push_stack(NocStack* stack, NocValue val);
NocValue pop_stack(NocStack* stack);
NocValue peek_stack(NocStack* stack);

#endif