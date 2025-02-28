#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a unit elimination
void parse_term_use(Parser* parser, uint64_t loc) {
  expect(parser, "-", "for unit elimination");

  uint64_t use_node = alloc(2);
  uint64_t val_loc = use_node;
  uint64_t bod_loc = use_node + 1;

  parse_term(parser, val_loc);
  expect(parser, ";", "after unit value");

  parse_term(parser, bod_loc);
  store_term(loc, USE, 0, use_node);
}
