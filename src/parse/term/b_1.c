#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a true value
void parse_b_1(Parser* parser, uint32_t loc) {
  expect(parser, "1", "for true value");
  store_term(loc, B_1, 0, 0);
}