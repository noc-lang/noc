#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../core/types.h"
#include "../../core/errors.h"
#include "../../core/stack.h"

void noc_tostr(NocBytecode b) {
    NocValue v = pop_stack(&vm.stack);
    if(v.label == QUOTE_VAL) {
        char* res = malloc(sizeof(char) * v.quote.cursor+1);
        for(int i = 1; i < v.quote.cursor+1; i++) {
            if(v.quote.array[i].label != CHAR_VAL)
                throw_noc_error(TYPE_ERROR, "the quote must contains only chars", 0);
            res[i-1] = v.quote.array[i].c;
        }
        res[v.quote.cursor] = '\0';
        NocValue r = {.label = STRING_VAL, .s = res};
        push_stack(&vm.stack, r);
    } else
        throw_noc_error(TYPE_ERROR, "cannot tostr with %s value", 1, noc_value_to_str(v.label));
}

void noc_chars(NocBytecode b) {
    NocValue v = pop_stack(&vm.stack);
    if(v.label == STRING_VAL) {
        NocValue q;
        NocStack s;
        create_stack(&s, strlen(v.s)+1);
        q.label = QUOTE_VAL;
        q.quote = s;
        NocValue v2;
        v2.label = CHAR_VAL;
        for(int i = 0; i < strlen(v.s); i++) {
            v2.c = v.s[i];
            push_stack(&q.quote, v2);
        }
        push_stack(&vm.stack, q);
    } else
        throw_noc_error(TYPE_ERROR, "cannot chars with %s value", 1, noc_value_to_str(v.label));
}