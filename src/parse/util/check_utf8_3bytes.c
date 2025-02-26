#include <stddef.h>
#include "../../parse.h"

bool check_utf8_3bytes(Parser* parser, uint8_t b1, uint8_t b2, uint8_t b3) {
  return (unsigned char)parser->input[parser->pos] == b1 && 
         (unsigned char)parser->input[parser->pos + 1] == b2 &&
         (unsigned char)parser->input[parser->pos + 2] == b3;
}