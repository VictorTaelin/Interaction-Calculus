#include <stddef.h>
#include "../../parse.h"

// Parse a variable
void parse_term_var(Parser* parser, uint64_t loc) {
  char* name = parse_name(parser);
  add_var_use(parser, name, loc);
}
