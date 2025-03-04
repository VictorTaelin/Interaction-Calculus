//./../../IC.md//
//./../types.h//
//./../memory.h//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "../parse.h"
#include "../memory.h"

// Main term parser - dispatcher for specific term types
void parse_term(Parser* parser, uint64_t loc) {
  skip(parser);

  if (parser->input[parser->pos] == '\0') {
    parse_error(parser, "Unexpected end of input");
  }

  unsigned char c = (unsigned char)parser->input[parser->pos];

  if (isalpha(c) || c == '_') {
    parse_term_var(parser, loc);
  } else if (c == '!') {
    parser->pos++; // Peek ahead
    char next = peek_char(parser);
    parser->pos--; // Restore position

    if (next == '&') {
      parse_term_col(parser, loc);
    } else if (isalpha(next) || next == '_') {
      parse_term_let(parser, loc);
    } else {
      parse_error(parser, "Expected '&' or name after '!' for collapser or let");
    }
  } else if (c == '&') {
    parse_term_sup(parser, loc);
  } else if (c == 0xCE) {
    unsigned char next_byte = (unsigned char)parser->input[parser->pos + 1];
    if (next_byte == 0xBB) {
      parse_term_lam(parser, loc);
    } else {
      parse_error(parser, "Unknown Unicode symbol starting with CE");
    }
  } else if (c == '(') {
    parse_term_app(parser, loc);
  } else {
    char error_msg[100];
    snprintf(error_msg, sizeof(error_msg), "Unexpected character: %c (code: %d)", c, (int)c);
    parse_error(parser, error_msg);
  }
}
