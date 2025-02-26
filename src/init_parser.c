#include <stddef.h>
#include "parse.h"

// Initialize a parser with the given input string
void init_parser(Parser* parser, const char* input) {
  parser->input = input;
  parser->pos = 0;
  parser->line = 1;
  parser->col = 1;
  parser->lcs_count = 0;  // Local captures (unresolved variable uses)
  parser->vrs_count = 0;  // Variable references (bindings)
}