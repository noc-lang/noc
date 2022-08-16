#ifndef OPCODES_H
#define OPCODES_H

#include "types.h"

extern void* OPCODES_FUNCS[];

void noc_push_const(NocBytecode b, NocOp opcode);
void noc_opcode_operator(NocBytecode b, NocOp opcode);
void noc_dup(NocBytecode b, NocOp opcode);
void noc_pop(NocBytecode b, NocOp opcode);
void noc_zap(NocBytecode b, NocOp opcode);
void noc_cat(NocBytecode b, NocOp opcode);
void noc_rotnm(NocBytecode b, NocOp opcode);
void noc_opcode_cmp(NocBytecode b, NocOp opcode);
void noc_opcode_bool(NocBytecode b, NocOp opcode);
void noc_create_quote(NocBytecode b, NocOp opcode);
void noc_unquote(NocBytecode b, NocOp opcode);
void noc_push_sym(NocBytecode b, NocOp opcode);

#endif