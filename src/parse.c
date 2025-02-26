#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "parse.h"
#include "memory.h"
#include "show.h"

// Parse a string into a term
Term parse_string(const char* input) {
  init_memory();
  
  Parser parser;
  init_parser(&parser, input);
  
  uint32_t loc = parse_term_alloc(&parser);
  resolve_var_uses(&parser);
  
  return heap[loc];
}

// Main term parser - dispatcher for specific term types
void parse_term(Parser* parser, uint32_t loc) {
  parse_whitespace(parser);
  
  if (parser->input[parser->pos] == '\0') {
    parse_error(parser, "Unexpected end of input");
  }
  
  unsigned char c = (unsigned char)parser->input[parser->pos];
  
  if (isalpha(c) || c == '_') {
    parse_var(parser, loc);
  } else if (c == '!') {
    parser->pos++; // Peek ahead
    char next = peek_char(parser);
    parser->pos--; // Restore position
    
    if (next == '&') {
      parse_col(parser, loc);
    } else if (next == '[') {
      parse_get(parser, loc);
    } else {
      parse_let(parser, loc);
    }
  } else if (c == '&') {
    parse_sup(parser, loc);
  } else if (c == '*') {
    parse_set(parser, loc);
  } else if (c == 0xE2) {
    if ((unsigned char)parser->input[parser->pos + 1] == 0x8A) {
      if ((unsigned char)parser->input[parser->pos + 2] == 0xA5) {
        parse_emp(parser, loc);
      } else if ((unsigned char)parser->input[parser->pos + 2] == 0xA4) {
        parse_uni(parser, loc);
      } else {
        parse_error(parser, "Unknown Unicode symbol starting with E2 8A");
      }
    } else {
      parse_error(parser, "Unknown Unicode symbol starting with E2");
    }
  } else if (c == 0xC2 && (unsigned char)parser->input[parser->pos + 1] == 0xAC) {
    parse_efq(parser, loc);
  } else if (c == '(' && parser->input[parser->pos + 1] == ')') {
    parse_nil(parser, loc);
  } else if (c == '-') {
    parse_use(parser, loc);
  } else if (c == 0xF0 && 
            (unsigned char)parser->input[parser->pos + 1] == 0x9D && 
            (unsigned char)parser->input[parser->pos + 2] == 0x94 && 
            (unsigned char)parser->input[parser->pos + 3] == 0xB9) {
    parse_bit(parser, loc);
  } else if (c == '0') {
    parse_b_0(parser, loc);
  } else if (c == '1') {
    parse_b_1(parser, loc);
  } else if (c == '?') {
    parse_ite(parser, loc);
  } else if (c == 0xCE) {
    unsigned char next_byte = (unsigned char)parser->input[parser->pos + 1];
    
    if (next_byte == 0xA3) {
      parse_sig(parser, loc);
    } else if (next_byte == 0xA0) {
      parse_all(parser, loc);
    } else if (next_byte == 0xBB) {
      parse_lam(parser, loc);
    } else if (next_byte == 0xB8) {
      parse_rfl(parser, loc);
    } else {
      parse_error(parser, "Unknown Unicode symbol starting with CE");
    }
  } else if (c == '[') {
    parse_tup(parser, loc);
  } else if (c == '(') {
    parse_app(parser, loc);
  } else if (c == '<') {
    parse_eql(parser, loc);
  } else if (c == '%') {
    parse_rwt(parser, loc);
  } else {
    char error_msg[100];
    snprintf(error_msg, sizeof(error_msg), "Unexpected character: %c (code: %d)", c, (int)c);
    parse_error(parser, error_msg);
  }
}