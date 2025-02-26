#include <ctype.h>
#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse an application
void parse_term_app(Parser* parser, uint32_t loc) {
  expect(parser, "(", "for application");
  
  uint32_t app_node = alloc_term(2);
  uint32_t fun_loc = app_node;
  uint32_t arg_loc = app_node + 1;
  
  parse_term(parser, fun_loc);
  
  if (!isspace(parser->input[parser->pos])) {
    parse_error(parser, "Expected whitespace between function and argument in application");
  }
  skip(parser);
  
  parse_term(parser, arg_loc);
  expect(parser, ")", "after application");
  
  store_term(loc, APP, 0, app_node);
}