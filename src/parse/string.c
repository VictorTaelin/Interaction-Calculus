#include <stdio.h>
#include <stdlib.h>
#include "../parse.h"
#include "../memory.h"

// Parse a string into a term
Term parse_string(const char* input) {
  init_memory();
  
  Parser parser;
  init_parser(&parser, input);
  
  uint32_t loc = parse_term_alloc(&parser);
  resolve_var_uses(&parser);
  
  return heap[loc];
}