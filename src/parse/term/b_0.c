#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a false value
void parse_b_0(Parser* parser, uint32_t loc) {
  expect(parser, "0", "for false value");
  store_term(loc, B_0, 0, 0);
}