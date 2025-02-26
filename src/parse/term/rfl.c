#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a reflexivity proof
void parse_rfl(Parser* parser, uint32_t loc) {
  if (check_utf8(parser, 0xCE, 0xB8)) {
    consume_utf8(parser, 2);
  } else if (!consume(parser, "θ")) {
    parse_error(parser, "Expected 'θ' for reflexivity");
  }
  
  store_term(loc, RFL, 0, 0);
}