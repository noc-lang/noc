#include <stdio.h>
#include "../../core/types.h"
#include "io.h"

extern NocVM vm;

void noc_print() {
   NocValue v = pop_stack(&vm.stack);
   switch(v.label) {
      case FLOAT_VAL:
         printf("%f\n", v.f);
         break;
      case INT_VAL:
         printf("%ld\n", v.i);
         break;
      case STRING_VAL:
         printf("\"%s\"\n", v.s);
         break;
      case CHAR_VAL:
         printf("'%c'\n", v.c);
         break;
      case BOOL_VAL:
         printf("%s\n", v.b ? "True" : "False");
         break;
   }  
}
