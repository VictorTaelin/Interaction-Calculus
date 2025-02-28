#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a sigma type elimination
void parse_term_get(Parser* parser, uint64_t loc) {
  expect(parser, "![", "for sigma elimination");

  uint64_t get_node = alloc(2);
  uint64_t val_loc = get_node;
  uint64_t bod_loc = get_node + 1;

  char* name1 = parse_name(parser);
  Term fst_var = make_term(VAR, 0, val_loc);  // First variable points to value
  bind_var(parser, name1, fst_var);

  expect(parser, ",", "between names in sigma elimination");

  char* name2 = parse_name(parser);
  Term snd_var = make_term(VAR, 0, bod_loc);  // Second variable points to body
  bind_var(parser, name2, snd_var);

  expect(parser, "]", "after names in sigma elimination");
  expect(parser, "=", "after names in sigma elimination");

  parse_term(parser, val_loc);
  expect(parser, ";", "after pair in sigma elimination");

  parse_term(parser, bod_loc);
  store_term(loc, GET, 0, get_node);
}
