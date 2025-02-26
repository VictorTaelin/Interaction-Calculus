#include <stddef.h>
#include "../../parse.h"

// Look at the next character without consuming it
char peek_char(Parser* parser) {
  parse_whitespace(parser);
  return parser->input[parser->pos];
}