#include <ctype.h>
#include <stddef.h>
#include "../../parse.h"

// Parse an unsigned integer
uint32_t parse_uint(Parser* parser) {
  parse_whitespace(parser);
  
  if (!isdigit(parser->input[parser->pos])) {
    parse_error(parser, "Expected a digit");
  }
  
  uint32_t value = 0;
  while (isdigit(parser->input[parser->pos])) {
    value = value * 10 + (parser->input[parser->pos] - '0');
    parser->pos++;
    parser->col++;
  }
  
  return value;
}