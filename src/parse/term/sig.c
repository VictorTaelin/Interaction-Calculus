#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a sigma type
void parse_term_sig(Parser* parser, uint64_t loc) {
  if (check_utf8(parser, 0xCE, 0xA3)) {
    consume_utf8(parser, 2);
  } else if (!consume(parser, "Σ")) {
    parse_error(parser, "Expected 'Σ' for sigma type");
  }

  char* name = parse_name(parser);
  expect(parser, ":", "after name in sigma type");

  uint64_t sig_node = alloc(2);
  uint64_t fst_loc = sig_node;
  uint64_t snd_loc = sig_node + 1;

  parse_term(parser, fst_loc);
  expect(parser, ".", "after first type in sigma type");

  Term var_term = make_term(VAR, 0, fst_loc);
  bind_var(parser, name, var_term);

  parse_term(parser, snd_loc);
  store_term(loc, SIG, 0, sig_node);
}
