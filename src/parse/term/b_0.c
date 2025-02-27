#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a false value
void parse_term_bt0(Parser* parser, uint32_t loc) {
  expect(parser, "0", "for false value");
  store_term(loc, BT0, 0, 0);
}
