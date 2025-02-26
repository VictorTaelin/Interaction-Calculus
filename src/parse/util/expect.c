#include <stdio.h>
#include <stddef.h>
#include "../../parse.h"

// Consume specified token or report specific error
bool expect(Parser* parser, const char* token, const char* error_context) {
  if (!consume(parser, token)) {
    char msg[128];
    snprintf(msg, sizeof(msg), "Expected '%s' %s", token, error_context);
    parse_error(parser, msg);
    return false;
  }
  return true;
}