#include <ctype.h>
#include <stddef.h>
#include "../../parse.h"

// Parse a name (identifier)
// Returns a pointer to a static buffer - results must be copied if needed
char* parse_name(Parser* parser) {
  parse_whitespace(parser);
  
  if (!isalpha(parser->input[parser->pos]) && parser->input[parser->pos] != '_') {
    parse_error(parser, "Expected an identifier");
  }
  
  static char name[MAX_NAME_LEN];
  size_t i = 0;
  
  while (isalnum(parser->input[parser->pos]) || parser->input[parser->pos] == '_') {
    if (i < MAX_NAME_LEN - 1) {
      name[i++] = parser->input[parser->pos];
    }
    parser->pos++;
    parser->col++;
  }
  
  name[i] = '\0';
  return name;
}