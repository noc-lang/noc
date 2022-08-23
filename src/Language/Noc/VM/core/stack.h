#ifndef STACK_H
#define STACK_H

#include "types.h"

typedef struct NocVM {
    NocStack stack;
    NocStack callstack;
} NocVM;

extern NocVM vm;

void free_stack(NocVM *vm);
void print_stack(NocStack stack);
void create_stack(NocStack* s, int capacity);
void push_stack(NocStack* stack, NocValue val);
NocValue pop_stack(NocStack* stack);
NocValue peek_stack(NocStack* stack);

#endif