#ifndef PARSE_H
#define PARSE_H

#include "ic.h"

#define MAX_NAME_LEN 32
#define MAX_GLOBAL_OCCURS 1024
#define MAX_GLOBAL_BINDERS 1024
#define MAX_LEXICAL_BINDERS 1024
#define MAX_SCOPE_DEPTH 64

typedef struct {
  char name[MAX_NAME_LEN];
  Term term;
} Binding;

typedef struct {
  char name[MAX_NAME_LEN];
  uint32_t loc;
} Use;

typedef struct {
  IC* ic;
  const char* input;
  size_t pos;
  size_t line;
  size_t col;
  Use global_occurs[MAX_GLOBAL_OCCURS];
  size_t global_occurs_count;
  Binding global_binders[MAX_GLOBAL_BINDERS];
  size_t global_binders_count;
  Binding lexical_binders[MAX_LEXICAL_BINDERS];
  size_t lexical_binders_count;
} Parser;

void init_parser(Parser* parser, IC* ic, const char* input);
uint32_t parse_term_alloc(Parser* parser);
void parse_term(Parser* parser, uint32_t loc);
Term parse_string(IC* ic, const char* input);
Term parse_file(IC* ic, const char* filename);

#endif // PARSE_H
