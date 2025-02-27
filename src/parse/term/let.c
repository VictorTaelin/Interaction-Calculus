#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a let binding
void parse_term_let(Parser* parser, uint32_t loc) {
  expect(parser, "!", "for let binding");

  char* name = parse_name(parser);
  expect(parser, "=", "after name in let binding");

  uint32_t let_node = alloc_term(2);
  uint32_t val_loc = let_node;
  uint32_t bod_loc = let_node + 1;

  bind_var(parser, name, make_term(VAR, 0, val_loc));

  parse_term(parser, val_loc);
  expect(parser, ";", "after value in let binding");

  parse_term(parser, bod_loc);
  store_term(loc, LET, 0, let_node);
}
