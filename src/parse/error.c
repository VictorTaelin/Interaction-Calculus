#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include "../parse.h"

// Error reporting
void parse_error(Parser* parser, const char* message) {
  fprintf(stderr, "Parse error at line %zu, column %zu: %s\n",
          parser->line, parser->col, message);
  
  // Print the line with the error
  const char* line_start = parser->input + parser->pos;
  while (line_start > parser->input && *(line_start - 1) != '\n') {
    line_start--;
  }
  
  const char* line_end = parser->input + parser->pos;
  while (*line_end != '\0' && *line_end != '\n') {
    line_end++;
  }
  
  fprintf(stderr, "%.*s\n", (int)(line_end - line_start), line_start);
  fprintf(stderr, "%*s^\n", (int)parser->col - 1, "");
  
  exit(1);
}