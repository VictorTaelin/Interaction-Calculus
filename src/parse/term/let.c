#include <string.h>
#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a let expression (syntax sugar for application of lambda)
void parse_term_let(Parser* parser, uint64_t loc) {
  expect(parser, "!", "for let expression");
  
  char* name = parse_name(parser);
  expect(parser, "=", "after name in let expression");
  
  // Allocate nodes for the application and lambda
  uint64_t app_node = alloc(2);
  uint64_t lam_node = alloc(1);
  
  // Set up the application structure
  uint64_t fun_loc = app_node;     // lambda function
  uint64_t arg_loc = app_node + 1; // value

  // Create variable term for the lambda parameter
  bind_var(parser, name, make_term(VAR, 0, lam_node));
  
  // Parse the value into arg_loc
  parse_term(parser, arg_loc);
  
  expect(parser, ";", "after value in let expression");
  
  // Parse the body into lam_node
  parse_term(parser, lam_node);
  
  // Store the lambda at fun_loc
  store_term(fun_loc, LAM, 0, lam_node);
  
  // Store the application at loc
  store_term(loc, APP, 0, app_node);
}
