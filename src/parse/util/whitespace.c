#include <stddef.h>
#include "../../parse.h"

// Skip whitespace characters and comments
void parse_whitespace(Parser* parser) {
  while (parser->input[parser->pos] != '\0') {
    char c = parser->input[parser->pos];
    
    if (c == ' ' || c == '\t') {
      parser->pos++;
      parser->col++;
    } else if (c == '\n') {
      parser->pos++;
      parser->line++;
      parser->col = 1;
    } else if (c == '\r') {
      parser->pos++;
      if (parser->input[parser->pos] == '\n') {
        parser->pos++;
      }
      parser->line++;
      parser->col = 1;
    } else if (c == '/' && parser->input[parser->pos + 1] == '/') {
      parser->pos += 2;
      parser->col += 2;
      while (parser->input[parser->pos] != '\0' && 
             parser->input[parser->pos] != '\n') {
        parser->pos++;
        parser->col++;
      }
    } else {
      break;
    }
  }
}