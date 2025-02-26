#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a sigma type elimination
void parse_get(Parser* parser, uint32_t loc) {
  expect(parser, "![", "for sigma elimination");
  
  char* name1 = parse_name(parser);
  expect(parser, ",", "between names in sigma elimination");
  
  char* name2 = parse_name(parser);
  expect(parser, "]", "after names in sigma elimination");
  expect(parser, "=", "after names in sigma elimination");
  
  uint32_t get_node = alloc_term(3);
  uint32_t val_loc = get_node;
  uint32_t bod_loc = get_node + 1;
  
  parse_term(parser, val_loc);
  expect(parser, ";", "after pair in sigma elimination");
  
  Term fst_var = make_term(VAR, 0, get_node + 2);
  Term snd_var = make_term(VAR, 0, get_node + 2);
  add_var_binding(parser, name1, fst_var);
  add_var_binding(parser, name2, snd_var);
  
  parse_term(parser, bod_loc);
  store_term(loc, GET, 0, get_node);
}