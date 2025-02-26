#include <stddef.h>
#include "../parse.h"

// Helper function to check if the current position has a specific 2-byte UTF-8 character
bool check_utf8(Parser* parser, uint8_t b1, uint8_t b2) {
  return (unsigned char)parser->input[parser->pos] == b1 && 
         (unsigned char)parser->input[parser->pos + 1] == b2;
}

// Helper function to check if the current position has a specific 3-byte UTF-8 character
bool check_utf8_3bytes(Parser* parser, uint8_t b1, uint8_t b2, uint8_t b3) {
  return (unsigned char)parser->input[parser->pos] == b1 && 
         (unsigned char)parser->input[parser->pos + 1] == b2 &&
         (unsigned char)parser->input[parser->pos + 2] == b3;
}

// Helper function to check if the current position has a specific 4-byte UTF-8 character
bool check_utf8_4bytes(Parser* parser, uint8_t b1, uint8_t b2, uint8_t b3, uint8_t b4) {
  return (unsigned char)parser->input[parser->pos] == b1 && 
         (unsigned char)parser->input[parser->pos + 1] == b2 &&
         (unsigned char)parser->input[parser->pos + 2] == b3 &&
         (unsigned char)parser->input[parser->pos + 3] == b4;
}

// Parse and advance for multi-byte UTF-8 characters
void consume_utf8(Parser* parser, int bytes) {
  parser->pos += bytes;
  parser->col++;
}