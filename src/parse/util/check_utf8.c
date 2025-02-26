#include <stddef.h>
#include "../../parse.h"

// Helper function to check if the current position has a specific UTF-8 character
bool check_utf8(Parser* parser, uint8_t b1, uint8_t b2) {
  return (unsigned char)parser->input[parser->pos] == b1 && 
         (unsigned char)parser->input[parser->pos + 1] == b2;
}