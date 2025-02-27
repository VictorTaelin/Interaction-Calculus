#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a unit elimination
void parse_term_use(Parser* parser, uint32_t loc) {
  expect(parser, "-", "for unit elimination");

  uint32_t use_node = alloc_term(2);
  uint32_t val_loc = use_node;
  uint32_t bod_loc = use_node + 1;

  parse_term(parser, val_loc);
  expect(parser, ";", "after unit value");

  parse_term(parser, bod_loc);
  store_term(loc, USE, 0, use_node);
}
