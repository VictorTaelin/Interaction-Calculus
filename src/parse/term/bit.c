#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a bool type
void parse_term_bit(Parser* parser, uint32_t loc) {
  if (check_utf8_4bytes(parser, 0xF0, 0x9D, 0x94, 0xB9)) {
    consume_utf8(parser, 4);
  } else if (!consume(parser, "𝔹")) {
    parse_error(parser, "Expected '𝔹' for bool type");
  }
  
  store_term(loc, BIT, 0, 0);
}