#include <string.h>
#include <ctype.h>
#include <stddef.h>
#include "../parse.h"

// Try to consume a specific string
bool consume(Parser* parser, const char* str) {
  parse_whitespace(parser);
  
  size_t len = strlen(str);
  size_t i = 0;
  size_t pos = parser->pos;
  
  // Try to match each character, skipping whitespace
  while (i < len && parser->input[pos] != '\0') {
    if (isspace(parser->input[pos])) {
      pos++;
      continue;
    }
    
    if (isspace(str[i])) {
      i++;
      continue;
    }
    
    if (parser->input[pos] != str[i]) {
      return false;
    }
    
    pos++;
    i++;
  }
  
  // If we matched the full string
  if (i == len) {
    size_t old_pos = parser->pos;
    parser->pos = pos;
    parser->col += (pos - old_pos);
    return true;
  }
  
  return false;
}