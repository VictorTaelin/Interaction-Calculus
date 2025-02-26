#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a unit value
void parse_term_nil(Parser* parser, uint32_t loc) {
  expect(parser, "()", "for unit value");
  store_term(loc, NIL, 0, 0);
}