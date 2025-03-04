#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a lambda
void parse_term_lam(Parser* parser, uint32_t loc) {
  if (check_utf8(parser, 0xCE, 0xBB)) {
    consume_utf8(parser, 2);
  } else if (!consume(parser, "λ")) {
    parse_error(parser, "Expected 'λ' for lambda");
  }

  char* name = parse_name(parser);
  expect(parser, ".", "after name in lambda");

  uint32_t lam_node = alloc(1);

  Term var_term = make_term(VAR, 0, lam_node);
  bind_var(parser, name, var_term);

  parse_term(parser, lam_node);
  store_term(loc, LAM, 0, lam_node);
}
