#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "../parse.h"
#include "../memory.h"

// Main term parser - dispatcher for specific term types
void parse_term(Parser* parser, uint32_t loc) {
  parse_whitespace(parser);
  
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
    } else if (next == '[') {
      parse_term_get(parser, loc);
    } else {
      parse_term_let(parser, loc);
    }
  } else if (c == '&') {
    parse_term_sup(parser, loc);
  } else if (c == '*') {
    parse_term_set(parser, loc);
  } else if (c == 0xE2) {
    if ((unsigned char)parser->input[parser->pos + 1] == 0x8A) {
      if ((unsigned char)parser->input[parser->pos + 2] == 0xA5) {
        parse_term_emp(parser, loc);
      } else if ((unsigned char)parser->input[parser->pos + 2] == 0xA4) {
        parse_term_uni(parser, loc);
      } else {
        parse_error(parser, "Unknown Unicode symbol starting with E2 8A");
      }
    } else {
      parse_error(parser, "Unknown Unicode symbol starting with E2");
    }
  } else if (c == 0xC2 && (unsigned char)parser->input[parser->pos + 1] == 0xAC) {
    parse_term_efq(parser, loc);
  } else if (c == '(' && parser->input[parser->pos + 1] == ')') {
    parse_term_nil(parser, loc);
  } else if (c == '-') {
    parse_term_use(parser, loc);
  } else if (c == 0xF0 && 
            (unsigned char)parser->input[parser->pos + 1] == 0x9D && 
            (unsigned char)parser->input[parser->pos + 2] == 0x94 && 
            (unsigned char)parser->input[parser->pos + 3] == 0xB9) {
    parse_term_bit(parser, loc);
  } else if (c == '0') {
    parse_term_bt0(parser, loc);
  } else if (c == '1') {
    parse_term_bt1(parser, loc);
  } else if (c == '?') {
    parse_term_ite(parser, loc);
  } else if (c == 0xCE) {
    unsigned char next_byte = (unsigned char)parser->input[parser->pos + 1];
    
    if (next_byte == 0xA3) {
      parse_term_sig(parser, loc);
    } else if (next_byte == 0xA0) {
      parse_term_all(parser, loc);
    } else if (next_byte == 0xBB) {
      parse_term_lam(parser, loc);
    } else if (next_byte == 0xB8) {
      parse_term_rfl(parser, loc);
    } else {
      parse_error(parser, "Unknown Unicode symbol starting with CE");
    }
  } else if (c == '[') {
    parse_term_tup(parser, loc);
  } else if (c == '(') {
    parse_term_app(parser, loc);
  } else if (c == '<') {
    parse_term_eql(parser, loc);
  } else if (c == '%') {
    parse_term_rwt(parser, loc);
  } else {
    char error_msg[100];
    snprintf(error_msg, sizeof(error_msg), "Unexpected character: %c (code: %d)", c, (int)c);
    parse_error(parser, error_msg);
  }
}