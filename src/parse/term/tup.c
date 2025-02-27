#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a tuple
void parse_term_tup(Parser* parser, uint64_t loc) {
  expect(parser, "[", "for tuple");

  uint64_t tup_node = alloc_term(2);
  uint64_t fst_loc = tup_node;
  uint64_t snd_loc = tup_node + 1;

  parse_term(parser, fst_loc);
  expect(parser, ",", "between elements in tuple");

  parse_term(parser, snd_loc);
  expect(parser, "]", "after elements in tuple");

  store_term(loc, TUP, 0, tup_node);
}
