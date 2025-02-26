#include <stdio.h>
#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Resolve all variable uses after parsing
void resolve_var_uses(Parser* parser) {
  for (size_t i = 0; i < parser->lcs_count; i++) {
    const char* name = parser->lcs[i].name;
    uint32_t loc = parser->lcs[i].loc;
    
    Term* binding = lookup_var_binding(parser, name);
    if (binding) {
      heap[loc] = *binding;
    } else {
      char error_msg[100];
      snprintf(error_msg, sizeof(error_msg), "Undefined variable: %s", name);
      parse_error(parser, error_msg);
    }
  }
}