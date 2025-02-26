#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse an equality type
void parse_eql(Parser* parser, uint32_t loc) {
  expect(parser, "<", "for equality type");
  
  uint32_t eql_node = alloc_term(2);
  uint32_t lft_loc = eql_node;
  uint32_t rgt_loc = eql_node + 1;
  
  parse_term(parser, lft_loc);
  expect(parser, "=", "between terms in equality type");
  
  parse_term(parser, rgt_loc);
  expect(parser, ">", "after terms in equality type");
  
  store_term(loc, EQL, 0, eql_node);
}