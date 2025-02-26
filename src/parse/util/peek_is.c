#include <stddef.h>
#include "../../parse.h"

// Check if the next character is a specific character
bool peek_is(Parser* parser, char c) {
  return peek_char(parser) == c;
}