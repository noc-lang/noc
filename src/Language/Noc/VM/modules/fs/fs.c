#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "../../core/types.h"
#include "../../core/errors.h"
#include "../../core/stack.h"

void read_file(FILE* file) {
    long n = 0;
    char* output = NULL;

    fseek(file, 0, 2);
    n = ftell(file);
    fseek(file, 0, 0);

    output = malloc(sizeof(char) * n);
    if(output == NULL)
        throw_noc_error(OUT_OF_MEMORY_ERROR, "malloc cannot allocate more memory. (source: VM/modules/fs/fs.c => read_file)", 0);

    fread(output, sizeof(output), n, file);
    NocValue res = {.label = STRING_VAL, .s = output};
    push_stack(&vm.stack, res);
}

void write_file(FILE* file, NocValue v) {
    fputs(v.s, file);
}

void noc_open(NocBytecode b) {
    NocValue v = pop_stack(&vm.stack);
    NocValue v2 = pop_stack(&vm.stack);
    NocValue v3 = pop_stack(&vm.stack);
    if(v.label == STRING_VAL) {
        if(v2.label == STRING_VAL) {
            if(v3.label == STRING_VAL) {
                FILE* file = NULL;
                file = fopen(v3.s, v.s);

                if(file == NULL)
                    throw_noc_error(FILE_NOT_FOUND_ERROR, "No such file '%s'", 1, v3.s);
                
                if(strcmp(v.s, "r") == 0)
                    read_file(file);
                else if(strcmp(v.s, "w") == 0) 
                    write_file(file, v2);
                else if(strcmp(v.s, "a") == 0)
                    write_file(file, v2);
                else if(strcmp(v.s, "r+") == 0) {
                    read_file(file);
                    write_file(file, v2);
                } else
                    throw_noc_error(BAD_ARGUMENT, "(open) unknown mode", 0);

                fclose(file);
            } else
                throw_noc_error(TYPE_ERROR, "The first argument has a %s value", 1, noc_value_to_str(v3.label));
        } else
            throw_noc_error(TYPE_ERROR, "The second argument has a %s value", 1, noc_value_to_str(v2.label));
    } else
        throw_noc_error(TYPE_ERROR, "The third argument has a %s value", 1, noc_value_to_str(v.label));
}   