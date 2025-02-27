#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a true value
void parse_term_bt1(Parser* parser, uint64_t loc) {
  expect(parser, "1", "for true value");
  store_term(loc, BT1, 0, 0);
}
