#include <stdio.h>
#include <stdlib.h>
#include <error.h>
#include <string.h>
#include <stdarg.h>
#include "errors.h"
#include "types.h"

char* noc_value_to_str(NocValueLabel label) {
    switch(label) {
        case FLOAT_VAL:
            return "FLOAT_VAL";
        case INT_VAL:
            return "INT_VAL";
        case STRING_VAL:
            return "STRING_VAL";
        case CHAR_VAL:
            return "CHAR_VAL";
        case BOOL_VAL:
            return "BOOL_VAL";
    }
}

char* noc_err_to_str(NocError err) {
    switch(err) {
        case ZERO_DIVISION_ERROR:
            return "ZERO_DIVISION_ERROR";
        case EMPTY_STACK_ERROR:
            return "EMPTY_STACK_ERROR";
        case TYPE_ERROR:
            return "TYPE_ERROR";
        case NAME_ERROR:
            return "NAME_ERROR";
        case BAD_ARGUMENT:
            return "BAD_ARGUMENT";
        case FILE_NOT_FOUND_ERROR:
            return "FILE_NOT_FOUND_ERROR";
        case OUT_OF_MEMORY_ERROR:
            return "OUT_OF_MEMORY_ERROR";
    }
}

void throw_noc_error(NocError err, char* fmt, int num, ...) {
    va_list args;

    va_start(args, num);
    // Size of the formatted string
    int size = strlen(fmt);
    for(int i = 0; i < num; i++)
        size += strlen(va_arg(args, char*));
 
    va_start(args, num);
    char* msg_formatted = malloc(sizeof(char) * size);
    
    if(msg_formatted == NULL) {
        fprintf(stderr, "[%s] malloc cannot allocate more memory. (source: VM/core/errors.c => throw_noc_error)\n", noc_err_to_str(OUT_OF_MEMORY_ERROR));
        exit(EXIT_FAILURE);
    }

    vsprintf(msg_formatted, fmt, args);

    va_end(args);

    fprintf(stderr, "[%s] %s.\n", noc_err_to_str(err), msg_formatted);

    free(msg_formatted);

    exit(EXIT_FAILURE);
}