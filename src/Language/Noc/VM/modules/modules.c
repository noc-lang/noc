#include <stdlib.h>
#include "modules.h"

void* PRIM_FUNCS[] = {
    &noc_id,
    &noc_str,
    &noc_int,
    &noc_float,
    &noc_bool,
    &noc_help,
    &noc_case,
    &noc_trace,
    &noc_chr,
    &noc_ord,
    &noc_print,
    &noc_ask,
    &noc_putstr,
    &noc_open,
    NULL,
    &noc_tostr,
    &noc_chars,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
};