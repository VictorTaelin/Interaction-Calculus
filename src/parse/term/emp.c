#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse an empty type
void parse_term_emp(Parser* parser, uint32_t loc) {
  if (check_utf8_3bytes(parser, 0xE2, 0x8A, 0xA5)) {
    consume_utf8(parser, 3);
    store_term(loc, EMP, 0, 0);
  } else {
    parse_error(parser, "Expected '⊥' for empty type");
  }
}
