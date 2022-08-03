#ifndef ERRORS_H
#define ERRORS_H

#include "types.h"

typedef enum NocError {
    ZERO_DIVISION_ERROR,
    EMPTY_STACK_ERROR,
    TYPE_ERROR,
    NAME_ERROR,
    BAD_ARGUMENT,
    FILE_NOT_FOUND_ERROR,
    OUT_OF_MEMORY_ERROR
} NocError;

char* noc_value_to_str(NocValueLabel label);
char* noc_err_to_str(NocError err);
void throw_noc_error(NocError err, char* fmt, int num, ...);

#endif