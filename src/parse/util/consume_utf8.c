#include <stddef.h>
#include "../../parse.h"

// Parse and advance for multi-byte UTF-8 characters
void consume_utf8(Parser* parser, int bytes) {
  parser->pos += bytes;
  parser->col++;
}