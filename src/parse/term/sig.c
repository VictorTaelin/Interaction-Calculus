#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a sigma type
void parse_term_sig(Parser* parser, uint32_t loc) {
  if (check_utf8(parser, 0xCE, 0xA3)) {
    consume_utf8(parser, 2);
  } else if (!consume(parser, "Σ")) {
    parse_error(parser, "Expected 'Σ' for sigma type");
  }
  
  char* name = parse_name(parser);
  expect(parser, ":", "after name in sigma type");
  
  uint32_t sig_node = alloc_term(2);
  uint32_t fst_loc = sig_node;
  uint32_t snd_loc = sig_node + 1;
  
  parse_term(parser, fst_loc);
  expect(parser, ".", "after first type in sigma type");
  
  Term var_term = make_term(VAR, 0, fst_loc);
  add_var_binding(parser, name, var_term);
  
  parse_term(parser, snd_loc);
  store_term(loc, SIG, 0, sig_node);
}