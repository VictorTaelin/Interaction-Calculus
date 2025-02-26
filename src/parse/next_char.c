#include <stddef.h>
#include "../parse.h"

// Consume the next character
char next_char(Parser* parser) {
  parse_whitespace(parser);
  char c = parser->input[parser->pos];
  if (c != '\0') {
    parser->pos++;
    parser->col++;
  }
  return c;
}