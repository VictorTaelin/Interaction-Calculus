#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a universe type
void parse_set(Parser* parser, uint32_t loc) {
  expect(parser, "*", "for universe type");
  store_term(loc, SET, 0, 0);
}