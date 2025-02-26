#include <string.h>
#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a collapser
void parse_col(Parser* parser, uint32_t loc) {
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
  
  uint32_t col_node = alloc_term(1);
  uint32_t val_loc = col_node;
  
  Term co0_term = make_term(CO0, label, loc);
  Term co1_term = make_term(CO1, label, loc);
  add_var_binding(parser, var1, co0_term);
  add_var_binding(parser, var2, co1_term);
  
  parse_term(parser, val_loc);
  expect(parser, ";", "after value in collapser");
  
  parse_term(parser, loc);
}