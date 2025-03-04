#include <string.h>
#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a collapser
void parse_term_col(Parser* parser, uint32_t loc) {
  expect(parser, "!&", "for collapser");

  uint8_t label = parse_uint(parser) & 3;
  expect(parser, "{", "after label in collapser");

  static char var1[MAX_NAME_LEN], var2[MAX_NAME_LEN];

  char* temp_name = parse_name(parser);
  strncpy(var1, temp_name, MAX_NAME_LEN - 1);
  var1[MAX_NAME_LEN - 1] = '\0';

  expect(parser, ",", "between names in collapser");

  temp_name = parse_name(parser);
  strncpy(var2, temp_name, MAX_NAME_LEN - 1);
  var2[MAX_NAME_LEN - 1] = '\0';

  expect(parser, "}", "after names in collapser");
  expect(parser, "=", "after names in collapser");

  // Allocate a node specifically for the collapse value
  uint32_t col_node = alloc(1);

  // Create collapse variable terms that point to the col_node, NOT to the loc
  Term co0_term = make_term(CO0, label, col_node);
  Term co1_term = make_term(CO1, label, col_node);
  bind_var(parser, var1, co0_term);
  bind_var(parser, var2, co1_term);

  // Parse the collapse value into col_node
  parse_term(parser, col_node);
  expect(parser, ";", "after value in collapser");

  // Parse the body of the collapse into loc
  parse_term(parser, loc);
}
