#ifndef RUNTIME_H
#define RUNTIME_H

#include "types.h"

void call_prim(Sym *s);
void call_noc_func(int pos);
void call_opcode(NocBytecode b, NocOp op);
void run(NocBytecode b, int CALL_POS);

#endif