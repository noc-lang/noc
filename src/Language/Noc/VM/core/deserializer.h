#ifndef DESERIALIZER_H
#define DESERIALIZER_H

#include <stdio.h>
#include <stdint.h>
#include "types.h"

void free_bytecode(NocBytecode *b);

Table decode_sym_table(FILE *file);

Table decode_constant_table(FILE *file);

Table decode_doc_table(FILE *file);

Table decode_opcode_table(FILE *file);

void deserialize(NocBytecode *b, char* filename);

#endif