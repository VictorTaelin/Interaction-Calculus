#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "parse.h"
#include "memory.h"

// Forward declarations for internal functions
uint64_t parse_term_alloc(Parser* parser);
void parse_term(Parser* parser, uint64_t loc);
void skip(Parser* parser);
char peek_char(Parser* parser);
char next_char(Parser* parser);
bool peek_is(Parser* parser, char c);
void parse_error(Parser* parser, const char* message);
Term* lookup_var_binding(Parser* parser, const char* name);

// Add a variable binding
void bind_var(Parser* parser, const char* name, Term term) {
  if (parser->vrs_count >= MAX_VARS) {
    parse_error(parser, "Too many variable bindings");
  }

  strncpy(parser->vrs[parser->vrs_count].name, name, MAX_NAME_LEN - 1);
  parser->vrs[parser->vrs_count].name[MAX_NAME_LEN - 1] = '\0';
  parser->vrs[parser->vrs_count].term = term;
  parser->vrs_count++;
}

// Track a variable use to be resolved later
void add_var_use(Parser* parser, const char* name, uint64_t loc) {
  if (parser->lcs_count >= MAX_USES) {
    parse_error(parser, "Too many variable uses");
  }

  strncpy(parser->lcs[parser->lcs_count].name, name, MAX_NAME_LEN - 1);
  parser->lcs[parser->lcs_count].name[MAX_NAME_LEN - 1] = '\0';
  parser->lcs[parser->lcs_count].loc = loc;
  parser->lcs_count++;
}


// Check if the next bytes match the specified UTF-8 bytes
bool check_utf8(Parser* parser, uint8_t b1, uint8_t b2) {
  return (unsigned char)parser->input[parser->pos] == b1 &&
         (unsigned char)parser->input[parser->pos + 1] == b2;
}

// Check if the next bytes match the specified 3-byte UTF-8 sequence
bool check_utf8_3bytes(Parser* parser, uint8_t b1, uint8_t b2, uint8_t b3) {
  return (unsigned char)parser->input[parser->pos] == b1 &&
         (unsigned char)parser->input[parser->pos + 1] == b2 &&
         (unsigned char)parser->input[parser->pos + 2] == b3;
}

// Check if the next bytes match the specified 4-byte UTF-8 sequence
bool check_utf8_4bytes(Parser* parser, uint8_t b1, uint8_t b2, uint8_t b3, uint8_t b4) {
  return (unsigned char)parser->input[parser->pos] == b1 &&
         (unsigned char)parser->input[parser->pos + 1] == b2 &&
         (unsigned char)parser->input[parser->pos + 2] == b3 &&
         (unsigned char)parser->input[parser->pos + 3] == b4;
}

// Consume the next n bytes of UTF-8 character
void consume_utf8(Parser* parser, int bytes) {
  for (int i = 0; i < bytes; i++) {
    next_char(parser);
  }
}

// Try to consume a string, returning whether it matched
bool consume(Parser* parser, const char* str) {
  size_t len = strlen(str);

  skip(parser);
  if (strncmp(parser->input + parser->pos, str, len) == 0) {
    for (size_t i = 0; i < len; i++) {
      next_char(parser);
    }
    return true;
  }

  return false;
}

// Report a parsing error
void parse_error(Parser* parser, const char* message) {
  fprintf(stderr, "Parse error at line %zu, column %zu: %s\n", 
          parser->line, parser->col, message);
  fprintf(stderr, "Input:\n%s\n", parser->input);

  // Create a pointer to the error location
  fprintf(stderr, "        ");
  for (size_t i = 0; i < parser->pos && i < 40; i++) {
    fprintf(stderr, " ");
  }
  fprintf(stderr, "^\n");

  exit(1);
}

// Expect a specific token, with an error if not found
bool expect(Parser* parser, const char* token, const char* error_context) {
  if (!consume(parser, token)) {
    char error[256];
    snprintf(error, sizeof(error), "Expected '%s' %s", token, error_context);
    parse_error(parser, error);
    return false;
  }
  return true;
}

// Initialize a parser with the given input string
void init_parser(Parser* parser, const char* input) {
  parser->input = input;
  parser->pos = 0;
  parser->line = 1;
  parser->col = 1;
  parser->lcs_count = 0;
  parser->vrs_count = 0;
}

// Look up a variable binding by name
Term* lookup_var_binding(Parser* parser, const char* name) {
  for (size_t i = 0; i < parser->vrs_count; i++) {
    if (strcmp(parser->vrs[i].name, name) == 0) {
      return &parser->vrs[i].term;
    }
  }
  return NULL;
}

// Parse an identifier
char* parse_name(Parser* parser) {
  static char name[MAX_NAME_LEN];
  size_t i = 0;

  if (!isalpha(peek_char(parser)) && peek_char(parser) != '_') {
    parse_error(parser, "Expected name starting with letter or underscore");
  }

  while (isalnum(peek_char(parser)) || peek_char(parser) == '_') {
    if (i < MAX_NAME_LEN - 1) {
      name[i++] = next_char(parser);
    } else {
      parse_error(parser, "Name too long");
    }
  }

  name[i] = '\0';
  return name;
}

// Get the next character and advance position
char next_char(Parser* parser) {
  char c = parser->input[parser->pos++];

  if (c == '\n') {
    parser->line++;
    parser->col = 1;
  } else {
    parser->col++;
  }

  return c;
}

// Peek at the next character without advancing
char peek_char(Parser* parser) {
  return parser->input[parser->pos];
}

// Check if the next character matches the specified one
bool peek_is(Parser* parser, char c) {
  return peek_char(parser) == c;
}

// Resolve all variable uses to their bindings
void resolve_var_uses(Parser* parser) {
  for (size_t i = 0; i < parser->lcs_count; i++) {
    Term* binding = lookup_var_binding(parser, parser->lcs[i].name);

    if (binding == NULL) {
      char error[MAX_NAME_LEN + 50];
      snprintf(error, sizeof(error), "Undefined variable: %s", parser->lcs[i].name);
      parse_error(parser, error);
    }

    heap[parser->lcs[i].loc] = *binding;
  }
}

// Store a term at the given location
void store_term(uint64_t loc, TermTag tag, uint16_t label, uint64_t value) {
  heap[loc] = make_term(tag, label, value);
}

// Parse a string into a term
Term parse_string(const char* input) {
  Parser parser;
  init_parser(&parser, input);

  uint64_t term_loc = parse_term_alloc(&parser);

  // Resolve variable references
  resolve_var_uses(&parser);

  return heap[term_loc];
}

// Allocate space for a term and parse into it
uint64_t parse_term_alloc(Parser* parser) {
  uint64_t loc = alloc(1);
  parse_term(parser, loc);
  return loc;
}

// Parse an unsigned integer
uint16_t parse_uint(Parser* parser) {
  uint16_t value = 0;
  bool has_digit = false;

  while (isdigit(peek_char(parser))) {
    value = value * 10 + (next_char(parser) - '0');
    has_digit = true;
  }

  if (!has_digit) {
    parse_error(parser, "Expected digit");
  }

  return value;
}

// Skip over whitespace and comments
void skip(Parser* parser) {
  while (1) {
    char c = peek_char(parser);

    if (isspace(c)) {
      next_char(parser);
    } else if (c == '/' && parser->input[parser->pos + 1] == '/') {
      // Skip line comment
      next_char(parser); // Skip '/'
      next_char(parser); // Skip '/'

      while (peek_char(parser) != '\0' && peek_char(parser) != '\n') {
        next_char(parser);
      }

      if (peek_char(parser) == '\n') {
        next_char(parser);
      }
    } else {
      break;
    }
  }
}
