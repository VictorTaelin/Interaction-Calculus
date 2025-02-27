#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a unit type
void parse_term_uni(Parser* parser, uint32_t loc) {
  if (check_utf8_3bytes(parser, 0xE2, 0x8A, 0xA4)) {
    consume_utf8(parser, 3);
    store_term(loc, UNI, 0, 0);
  } else {
    parse_error(parser, "Expected '⊤' for unit type");
  }
}
