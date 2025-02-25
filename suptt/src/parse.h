#ifndef PARSE_H
#define PARSE_H

#include "types.h"
#include <stdint.h>

// Parser state structure
typedef struct {
  const char* input;  // Input string
  size_t pos;         // Current position
  size_t line;        // Current line number
  size_t col;         // Current column number
} Parser;

// Initialize a parser with the given input string
void init_parser(Parser* parser, const char* input);

// Parse a term from the input and store it at the given location
void parse_term(Parser* parser, uint32_t loc);

// Parse a uint from the input
uint32_t parse_uint(Parser* parser);

// Parse a name from the input
char* parse_name(Parser* parser);

// Error reporting
void parse_error(Parser* parser, const char* message);

// TODO: Individual term parsers will be implemented later

#endif // PARSE_H