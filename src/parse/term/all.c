#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a pi type
void parse_term_all(Parser* parser, uint32_t loc) {
  if (check_utf8(parser, 0xCE, 0xA0)) {
    consume_utf8(parser, 2);
  } else if (!consume(parser, "Π")) {
    parse_error(parser, "Expected 'Π' for pi type");
  }
  
  char* name = parse_name(parser);
  expect(parser, ":", "after name in pi type");
  
  uint32_t all_node = alloc_term(2);
  uint32_t inp_loc = all_node;
  uint32_t out_loc = all_node + 1;
  
  parse_term(parser, inp_loc);
  expect(parser, ".", "after input type in pi type");
  
  Term var_term = make_term(VAR, 0, inp_loc);
  add_var_binding(parser, name, var_term);
  
  parse_term(parser, out_loc);
  store_term(loc, ALL, 0, all_node);
}