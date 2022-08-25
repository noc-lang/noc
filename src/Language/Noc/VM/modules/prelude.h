#ifndef PRELUDE_H
#define PRELUDE_H

#include "../core/types.h"

void noc_id(NocBytecode b);
void noc_str(NocBytecode b);
void noc_int(NocBytecode b);
void noc_float(NocBytecode b);
void noc_bool(NocBytecode b);
void noc_help(NocBytecode b);
void noc_trace(NocBytecode b);
void noc_chr(NocBytecode b);
void noc_ord(NocBytecode b);
void noc_print(NocBytecode b);
void noc_putstr(NocBytecode b);

#endif