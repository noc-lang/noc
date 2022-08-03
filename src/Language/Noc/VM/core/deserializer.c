#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include "types.h"
#include "../modules/modules.h"
#include "errors.h"

// Utils
int64_t decode_integer(FILE *file, int64_t* size) {
    fread(size, sizeof(int64_t), 1, file);
    return (*size);
}

char* decode_string(FILE *file) {
    // size of the functions's name
    int64_t size_elem;
    fread(&size_elem, sizeof(int64_t), 1, file);

    char *elem_name = malloc(sizeof(char) * size_elem);

    if(elem_name == NULL)
        throw_noc_error(OUT_OF_MEMORY_ERROR, "malloc cannot allocate more memory. (source: VM/core/deserializer.c => decode_string)", 0);
 
    char c;
    for(int k = 0; k < size_elem; k++) {
        fread(&c, sizeof(char), 1, file);
        elem_name[k] = c;
    }
    return elem_name;
}

NocOp decode_opcode(FILE *file) {
    uint8_t op_i;
    fread(&op_i, sizeof(uint8_t), 1, file);
    NocOp opcode;
    opcode.label = (NocOpLabel)op_i;
    if(op_i == 0 || op_i == 1 || op_i == 3 || op_i == 7) {
        int64_t operand;
        fread(&operand, sizeof(int64_t), 1, file);
        opcode.operand = operand;
    } else {
        opcode.operand = -1;
    }
    return opcode;
}

// --------------------------------------

Table decode_sym_table(FILE *file) {
    // size of the table
    int64_t size;
    decode_integer(file, &size);

    Table result;
    result.sym = malloc(sizeof(SymTable) * size);

    if(result.sym == NULL)
        throw_noc_error(OUT_OF_MEMORY_ERROR, "malloc cannot allocate more memory. (source: VM/core/deserializer.c => decode_sym_table)", 0);

    int64_t pos;

    for(int i = 0; i < size; i++) {
        uint8_t constructor;
        fread(&constructor, sizeof(uint8_t), 1, file);
        result.sym[i].label = (SymTableLabel)constructor;

        // noc_func constructor
        if(constructor == 0) {
            decode_integer(file, &pos);
            result.sym[i].p = pos;
        // prim constructor
        } else if(constructor == 1) {
            decode_integer(file, &pos);
            result.sym[i].func = PRIM_FUNCS[pos];
        // opcode constructor
        } else {
            result.sym[i].label = OP;
            result.sym[i].opcode = decode_opcode(file);
        }
    }
    return result;
}

Table decode_constant_table(FILE *file) {
    int64_t size;
    decode_integer(file, &size);

    Table result;
    result.constant = malloc(sizeof(NocValue) * size);

    if(result.constant == NULL)
        throw_noc_error(OUT_OF_MEMORY_ERROR, "malloc cannot allocate more memory. (source: VM/core/deserializer.c => decode_constant_table)", 0);

    uint8_t const_type;

    for(int i = 0; i < size; i++) {
        fread(&const_type, sizeof(uint8_t), 1, file);
        NocValue v;

        // integer
        if(const_type == 4) {
            int64_t val;
            // ---
            v.label = INT_VAL;
            v.i = decode_integer(file, &val);
        }

        // float
        if(const_type == 5) {
            double val;
            fread(&val, sizeof(double), 1, file);
            // ---
            v.label = FLOAT_VAL;
            v.f = val;
        }

        // string
        if(const_type == 6) {
            v.label = STRING_VAL;
            v.s = decode_string(file);
        }

        // char
        if(const_type == 7) {
            char val;
            fread(&val, sizeof(char), 1, file);
            // ---
            v.label = CHAR_VAL;
            v.c = val;
        }

        // bool
        if(const_type == 8) {
            uint8_t val;
            fread(&val, sizeof(uint8_t), 1, file);
            v.label = BOOL_VAL;
            v.b = (bool)(val);
        }
        result.constant[i] = v;
    }
    return result;
}

Table decode_doc_table(FILE *file) {
    int64_t size;
    decode_integer(file, &size);
    Table result;
    result.label = DOC;
    result.doc = malloc(sizeof(DocTable) * size);

    if(result.doc == NULL)
        throw_noc_error(OUT_OF_MEMORY_ERROR, "malloc cannot allocate more memory. (source: VM/core/deserializer.c => decode_doc_table)", 0);
    
    for(int i = 0; i < size; i++) {
        result.doc[i].docstring = decode_string(file);
        int64_t pos;
        result.doc[i].pos = decode_integer(file, &pos);
    }
    return result;
}

Table decode_opcode_table(FILE *file, int64_t* size_opcodes) {
    fread(size_opcodes, sizeof(int64_t), 1, file);
    Table result;
    result.label = OPCODE;
    result.opcode = malloc(sizeof(NocOp) * (*size_opcodes));
    
    if(result.opcode == NULL)
        throw_noc_error(OUT_OF_MEMORY_ERROR, "malloc cannot allocate more memory. (source: VM/core/deserializer.c => decode_opcode_table)", 0);

    for(int i = 0; i < (*size_opcodes); i++) {
        result.opcode[i] = decode_opcode(file);
    }
    return result;
}

NocBytecode deserialize(char* filename) {
    FILE *bytecode_file = fopen(filename, "rb");

    if(bytecode_file == NULL)
        throw_noc_error(FILE_NOT_FOUND_ERROR, "no such '%s' file", 1, filename);

    // Decode sym table
    Table sym = decode_sym_table(bytecode_file);

    // Decode consts table
    Table consts = decode_constant_table(bytecode_file);

    // Decode doc table
    Table doc = decode_doc_table(bytecode_file);
    
    // Decode opcode table
    int64_t size_opcodes;
    Table opcode = decode_opcode_table(bytecode_file, &size_opcodes);

    fclose(bytecode_file);

    OpCodes opcodes = {size_opcodes, opcode};
    NocBytecode program = {sym, consts, doc, opcodes};
    
    return program;
}
