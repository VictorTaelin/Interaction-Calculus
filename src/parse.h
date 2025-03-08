#ifndef PARSE_H
#define PARSE_H

#include "ic.h"
#include <stdbool.h>
#include <stdint.h>

#define MAX_NAME_LEN 64
#define MAX_GLOBAL_VARS 1024
#define MAX_LEXICAL_VARS 1024

typedef struct {
  char name[MAX_NAME_LEN];
  Term var;
  uint32_t loc;
} Binder;

typedef struct {
  IC* ic;
  const char* input;
  size_t pos;
  size_t line;
  size_t col;

  Binder global_vars[MAX_GLOBAL_VARS];
  size_t global_vars_count;

  Binder lexical_vars[MAX_LEXICAL_VARS];
  size_t lexical_vars_count;
} Parser;

void init_parser(Parser* parser, IC* ic, const char* input);
Term parse_string(IC* ic, const char* input);
Term parse_file(IC* ic, const char* filename);

#endif // PARSE_H
