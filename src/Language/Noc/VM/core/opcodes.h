#ifndef OPCODES_H
#define OPCODES_H

#include "types.h"

void noc_push_const(NocBytecode b, NocOp opcode);
void noc_opcode_operator(NocBytecode b, NocOp opcode);
void noc_return(NocBytecode b, NocOp opcode);
void noc_call_symbol(NocBytecode b, NocOp opcode);
void noc_dup(NocBytecode b, NocOp opcode);
void noc_pop(NocBytecode b, NocOp opcode);
void noc_zap(NocBytecode b, NocOp opcode);

void* OPCODES_FUNCS[] = {
    &noc_call_symbol, // call_symbol
    &noc_push_const, // push_const
    &noc_return, // return
    NULL, // create_quote
    NULL, // popr
    NULL, // pushr
    NULL, // unquote
    NULL, // push_sym
    &noc_dup, // dup
    &noc_pop, // pop
    &noc_zap, // zap
    NULL, // cat
    NULL, // rotNM
    &noc_opcode_operator, // +
    &noc_opcode_operator, // -
    &noc_opcode_operator, // *
    &noc_opcode_operator, // /
    &noc_opcode_operator, // ^
    NULL, // >
    NULL, // <
    NULL, // <=
    NULL, // <=
    NULL, // ==
    NULL, // and
    NULL, // or
};

#endif