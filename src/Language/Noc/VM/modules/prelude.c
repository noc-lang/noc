#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../core/types.h"
#include "../core/stack.h"
#include "../core/errors.h"
#include "../core/runtime.h"
#include "docstring.h"

void noc_id(NocBytecode b) {
    push_stack(&vm.stack, pop_stack(&vm.stack));
}

// ---
int count_digits(int number) {
    if(number == 0)
        return 1;
    else
        return ceil(log((number < 0) ? abs(number) : number)/log(10));
}

void noc_str(NocBytecode b) {
    NocValue v = pop_stack(&vm.stack);
    NocValue v_res;
    v_res.label = STRING_VAL;

    if(v.label == STRING_VAL) {
        char* res = malloc(sizeof(char) * strlen(v.s + 2));
        sprintf(res, "\"%s\"", v.s);
        v_res.s = res;
    } else if(v.label == INT_VAL) {
        char* res = malloc(sizeof(char) * count_digits(v.i));
        sprintf(res, "%ld", v.i);
        v_res.s = res;
    } else if(v.label == CHAR_VAL) {
        char* res = malloc(sizeof(char));
        sprintf(res, "%c", v.c);
        v_res.s = res;
    } else if(v.label == FLOAT_VAL) {
        double integral, decimal, tmp;
        int nb_digits = 1;
        decimal = modf(v.f, &integral);
        while(decimal >= 0.1) {
            nb_digits++;
            decimal = modf(decimal*10, &tmp);
        }
        nb_digits += count_digits((long)integral);
        char* res = malloc(sizeof(char) * nb_digits);
        sprintf(res, "%f", v.f);
        v_res.s = res;
    } else if(v.label == BOOL_VAL) {
        v_res.s = v.b ? "True" : "False";
    } else {
        throw_noc_error(TYPE_ERROR, "cannot str with a %s value", 1, noc_value_to_str(v.label));
    }
    push_stack(&vm.stack, v_res);
}

void noc_int(NocBytecode b) {
    NocValue v = pop_stack(&vm.stack);
    if(v.label == STRING_VAL) {
        NocValue v2 = {.label = INT_VAL, .i = atoi(v.s)};
        push_stack(&vm.stack, v2); 
    } else
        throw_noc_error(TYPE_ERROR, "cannot int with %s value", 1, noc_value_to_str(v.label));
}

void noc_float(NocBytecode b) {
    NocValue v = pop_stack(&vm.stack);
    if(v.label == STRING_VAL) {
        NocValue v2 = {.label = FLOAT_VAL, .f = atof(v.s)};
        push_stack(&vm.stack, v2); 
    } else
        throw_noc_error(TYPE_ERROR, "cannot float with %s value", 1, noc_value_to_str(v.label));
}

void noc_bool(NocBytecode b) {
    NocValue v = pop_stack(&vm.stack);
    if(v.label == INT_VAL) {
        NocValue v2 = {.label = BOOL_VAL, .b = (bool)v.i};
        push_stack(&vm.stack, v2);
    } else
        throw_noc_error(TYPE_ERROR, "cannot bool with %s value", 1, noc_value_to_str(v.label));
}

void noc_help(NocBytecode b) {
    NocValue v = pop_stack(&vm.stack);
    if(v.label == QUOTE_VAL) {
        if(v.quote.cursor > 0) {
            if(v.quote.array[1].label == SYMBOL_VAL) {
                NocValue v2;
                v2.label = STRING_VAL;
                if(v.quote.array[1].symbol->label == NOC_FUNC) {
                    if(v.quote.array[1].symbol->p == (-1)) {
                        v2.s = "(help) No doc entry for this function.";
                    } else {
                        for(int i = 0; i < b.doc.doc.size_doc; i++) {
                            if(strcmp(b.doc.doc.doc[i].name, v.quote.array[1].symbol->name) == 0) {
                                v2.s = b.doc.doc.doc[i].docstring;
                                break;
                            }
                        }
                    }
                } else if(v.quote.array[1].symbol->label == OP) {
                    v2.s = render_op_doc(v.quote.array[1].symbol->opcode.label);
                } else {
                    v2.s = render_prim_doc(v.quote.array[1].symbol->name);
                }
                push_stack(&vm.stack, v2);
            }
        }
    } else
        throw_noc_error(TYPE_ERROR, "cannot help with %s value", 1, noc_value_to_str(v.label));
}

void noc_trace(NocBytecode b) {
    print_stack(vm.stack);
    printf("\n");
}

void noc_chr(NocBytecode b) {
    NocValue v = pop_stack(&vm.stack);
    if(v.label == INT_VAL) {
        NocValue v2 = {.label = CHAR_VAL, .c = (char)v.i};
        push_stack(&vm.stack, v2);
    } else
        throw_noc_error(TYPE_ERROR, "cannot chr with %s value", 1, noc_value_to_str(v.label));
}

void noc_ord(NocBytecode b) {
    NocValue v = pop_stack(&vm.stack);
    if(v.label == CHAR_VAL) {
        NocValue v2 = {.label = INT_VAL, .i = (int)v.c};
        push_stack(&vm.stack, v2);
    } else
        throw_noc_error(TYPE_ERROR, "cannot ord with %s value", 1, noc_value_to_str(v.label));
}

void noc_putstr(NocBytecode b) {
    NocValue v = pop_stack(&vm.stack);
    if(v.label == STRING_VAL) {
        printf("%s", v.s);
    } else
        throw_noc_error(TYPE_ERROR, "cannot putstr %s value", 1, noc_value_to_str(v.label));
}

void noc_print(NocBytecode b) {
   NocValue v = pop_stack(&vm.stack);
   switch(v.label) {
      case FLOAT_VAL:
         printf("%.10f\n", v.f);
         break;
      case INT_VAL:
         printf("%ld\n", v.i);
         break;
      case STRING_VAL:
         printf("\"%s\"\n", v.s);
         break;
      case CHAR_VAL:
         printf("'%c'\n", v.c);
         break;
      case BOOL_VAL:
         printf("%s\n", v.b ? "True" : "False");
         break;
      case QUOTE_VAL:
         print_stack(v.quote);
         printf("\n");
   }  
}

void noc_ask(NocBytecode b) {
    NocValue v = pop_stack(&vm.stack);
    if(v.label == STRING_VAL) {
        printf("%s", v.s);
        char *line = NULL;
        size_t size = 0;
        ssize_t line_size = getline(&line, &size, stdin);
        if (line[line_size - 1] == '\n')  { line[line_size - 1] = '\0';}
        v.s = line;
        push_stack(&vm.stack, v);
    } else
        throw_noc_error(TYPE_ERROR, "cannot ask with %s value", 1, noc_value_to_str(v.label));
}

void noc_case(NocBytecode b) {
    NocValue v = pop_stack(&vm.stack);
    NocValue v2 = pop_stack(&vm.stack);
    if(v.label == QUOTE_VAL) {
        NocOp op;
        op.operand = -1;
        for(int i = 1; i < v.quote.cursor+1; i++) {
            if(v.quote.array[i].quote.cursor != 2)
                throw_noc_error(BAD_ARGUMENT, "bad quote structure in the pattern matching", 0);

            push_stack(&vm.stack, v.quote.array[i].quote.array[1].quote.array[1]);
            push_stack(&vm.stack, v2);
            op.label = EQUAL;
            call_opcode(b, op);

            if(pop_stack(&vm.stack).b) {
                push_stack(&vm.stack, v.quote.array[i].quote.array[2]);
                op.label = UNQUOTE_QUOTE;
                call_opcode(b, op);
                return;
            };
        }

        if(strcmp(v.quote.array[v.quote.cursor].quote.array[1].quote.array[1].symbol->name, "_") != 0)
            throw_noc_error(BAD_ARGUMENT, "not having otherwise case [_]", 0);

        push_stack(&vm.stack, v.quote.array[v.quote.cursor].quote.array[2]);
        op.label = UNQUOTE_QUOTE;
        call_opcode(b, op);
    } else
        throw_noc_error(TYPE_ERROR, "cannot case with %s value", 1, noc_value_to_str(v.label));
}