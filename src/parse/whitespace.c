#include <ctype.h>
#include "../parse.h"

// Skip whitespace and comments
void parse_whitespace(Parser* parser) {
  while (parser->input[parser->pos] != '\0') {
    char c = parser->input[parser->pos];
    
    // Skip whitespace
    if (isspace(c)) {
      if (c == '\n') {
        parser->line++;
        parser->col = 1;
      } else {
        parser->col++;
      }
      parser->pos++;
      continue;
    }
    
    // Skip comments (starting with #)
    if (c == '#') {
      while (parser->input[parser->pos] != '\0' && parser->input[parser->pos] != '\n') {
        parser->pos++;
      }
      continue;
    }
    
    // Not whitespace, exit
    break;
  }
}