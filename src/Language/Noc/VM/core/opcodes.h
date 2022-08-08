#ifndef OPCODES_H
#define OPCODES_H

#include "types.h"

char* noc_opcode_operator_to_str(NocOpLabel label);
void noc_push_const(NocBytecode b, NocOp opcode);
void noc_opcode_operator(NocBytecode b, NocOp opcode);
void noc_dup(NocBytecode b, NocOp opcode);
void noc_pop(NocBytecode b, NocOp opcode);
void noc_zap(NocBytecode b, NocOp opcode);
void noc_cat(NocBytecode b, NocOp opcode);
void noc_rotnm(NocBytecode b, NocOp opcode);
void noc_opcode_cmp(NocBytecode b, NocOp opcode);
void noc_opcode_bool(NocBytecode b, NocOp opcode);

void* OPCODES_FUNCS[] = {
    NULL, // call_symbol
    &noc_push_const, // push_const
    NULL, // return
    NULL, // create_quote
    NULL, // popr
    NULL, // pushr
    NULL, // unquote
    NULL, // push_sym
    &noc_dup, // dup
    &noc_pop, // pop
    &noc_zap, // zap
    &noc_cat, // cat
    &noc_rotnm, // rotNM
    &noc_opcode_operator, // +
    &noc_opcode_operator, // -
    &noc_opcode_operator, // *
    &noc_opcode_operator, // /
    &noc_opcode_operator, // ^
    &noc_opcode_operator, // >
    &noc_opcode_operator, // <
    &noc_opcode_operator, // >=
    &noc_opcode_operator, // <=
    &noc_opcode_cmp, // ==
    &noc_opcode_bool, // and
    &noc_opcode_bool, // or
};

#endif