#ifndef DESERIALIZER_H
#define DESERIALIZER_H

#include <stdint.h>
#include "types.h"

Table decode_sym_table(FILE *file);

Table decode_constant_table(FILE *file);

Table decode_doc_table(FILE *file);

Table decode_opcode_table(FILE *file, int64_t* size_opcodes);

NocBytecode deserialize(char* filename);

#endif