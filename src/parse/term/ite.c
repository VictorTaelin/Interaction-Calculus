#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a bool elimination
void parse_ite(Parser* parser, uint32_t loc) {
  expect(parser, "?", "for bool elimination");
  
  uint32_t ite_node = alloc_term(3);
  uint32_t cnd_loc = ite_node;
  uint32_t thn_loc = ite_node + 1;
  uint32_t els_loc = ite_node + 2;
  
  parse_term(parser, cnd_loc);
  expect(parser, "{", "after condition in bool elimination");
  
  parse_term(parser, thn_loc);
  expect(parser, "};{", "between branches in bool elimination");
  
  parse_term(parser, els_loc);
  expect(parser, "}", "after else branch in bool elimination");
  
  store_term(loc, ITE, 0, ite_node);
}