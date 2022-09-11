#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../core/types.h"
#include "../../core/errors.h"
#include "../../core/stack.h"
#include "../prelude.h"

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
            int num = (int)v.s[i];
            if(num < 0 || num > 127) {
                throw_noc_error(UTF8_ERROR, "cannot encode utf8 in a char type", 0);
            }
            v2.c = v.s[i];
            push_stack(&q.quote, v2);
        }
        push_stack(&vm.stack, q);
    } else
        throw_noc_error(TYPE_ERROR, "cannot chars with %s value", 1, noc_value_to_str(v.label));
}

void noc_format(NocBytecode b) {
    NocValue v = pop_stack(&vm.stack);
    NocValue v2 = pop_stack(&vm.stack);
    if(v2.label == STRING_VAL) {
        if(v.label == QUOTE_VAL) {
            if(strlen(v2.s) > 1024) {
                throw_noc_error(BAD_ARGUMENT, "the format string exceeds 1024 characters", 0);
            }

            if(v.quote.cursor == 0)
                throw_noc_error(BAD_ARGUMENT, "cannot format with an empty quote", 0);
            
            NocValue res;
            res.label = STRING_VAL;

            char buffer[1024] = {0};
            int j = 0;
            int i = 0;
            v.quote.cursor = 1;

            while(i < strlen(v2.s)) {
                if((v2.s[i] == '{') && (v2.s[i+1] == '}')) {
                    push_stack(&vm.stack, peek_stack(&v.quote));
                    if(peek_stack(&vm.stack).label != STRING_VAL)
                        noc_str(b);
                    NocValue v3 = pop_stack(&vm.stack);
                    strcat(buffer, v3.s);
                    j += strlen(v3.s);
                   v.quote.cursor++;
                   i+=2;
                } else {
                    buffer[j] = v2.s[i];
                    j++;
                    i++;
                }
            }
            res.s = buffer;
            push_stack(&vm.stack, res);
        } else
           throw_noc_error(TYPE_ERROR, "the first argument has %s value", 1, noc_value_to_str(v.label)); 
    } else
        throw_noc_error(TYPE_ERROR, "the second argument has %s value", 1, noc_value_to_str(v2.label));
}