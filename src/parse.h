#ifndef PARSE_H
#define PARSE_H

#include "ic.h"
#include <stdbool.h>
#include <stdint.h>

#define MAX_NAME_LEN 64
#define MAX_GLOBAL_OCCURS 1024
#define MAX_GLOBAL_BINDERS 1024
#define MAX_LEXICAL_BINDERS 1024

typedef struct {
  char name[MAX_NAME_LEN];
  Term original_term;
  uint32_t last_occurrence_loc;
} Binder;

typedef struct {
  IC* ic;
  const char* input;
  size_t pos;
  size_t line;
  size_t col;

  struct {
    char name[MAX_NAME_LEN];
    uint32_t loc;
  } global_occurs[MAX_GLOBAL_OCCURS];
  size_t global_occurs_count;

  Binder global_binders[MAX_GLOBAL_BINDERS];
  size_t global_binders_count;

  Binder lexical_binders[MAX_LEXICAL_BINDERS];
  size_t lexical_binders_count;
} Parser;

void init_parser(Parser* parser, IC* ic, const char* input);
Term parse_string(IC* ic, const char* input);
Term parse_file(IC* ic, const char* filename);

#endif // PARSE_H

