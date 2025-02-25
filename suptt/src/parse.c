#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "parse.h"
#include "memory.h"

// Initialize a parser with the given input string
void init_parser(Parser* parser, const char* input) {
  parser->input = input;
  parser->pos = 0;
  parser->line = 1;
  parser->col = 1;
}

// Error reporting
void parse_error(Parser* parser, const char* message) {
  fprintf(stderr, "Parse error at line %zu, column %zu: %s\n",
          parser->line, parser->col, message);
  exit(1);
}

// The following functions are stubs for now
// They will be implemented in a future update

void parse_term(Parser* parser, uint32_t loc) {
  // Stub - will be implemented later
}

uint32_t parse_uint(Parser* parser) {
  // Stub - will be implemented later
  return 0;
}

char* parse_name(Parser* parser) {
  // Stub - will be implemented later
  return NULL;
}