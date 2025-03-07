//./../HVM-Nano.md//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "parse.h"

//-----------------------------------------------------------------------------
// Forward declarations for internal functions
//-----------------------------------------------------------------------------
uint32_t parse_term_alloc(Parser* parser);
void parse_term(Parser* parser, uint32_t loc);
void skip(Parser* parser);
char peek_char(Parser* parser);
char next_char(Parser* parser);
bool peek_is(Parser* parser, char c);
void parse_error(Parser* parser, const char* message);
Term* lookup_var_binding(Parser* parser, const char* name);

//-----------------------------------------------------------------------------
// Variable management functions
//-----------------------------------------------------------------------------

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
void add_var_use(Parser* parser, const char* name, uint32_t loc) {
  if (parser->lcs_count >= MAX_USES) {
    parse_error(parser, "Too many variable uses");
  }

  strncpy(parser->lcs[parser->lcs_count].name, name, MAX_NAME_LEN - 1);
  parser->lcs[parser->lcs_count].name[MAX_NAME_LEN - 1] = '\0';
  parser->lcs[parser->lcs_count].loc = loc;
  parser->lcs_count++;
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

// Resolve all variable uses to their bindings
void resolve_var_uses(Parser* parser) {
  for (size_t i = 0; i < parser->lcs_count; i++) {
    Term* binding = lookup_var_binding(parser, parser->lcs[i].name);
    if (binding == NULL) {
      char error[MAX_NAME_LEN + 50];
      snprintf(error, sizeof(error), "Undefined variable: %s", parser->lcs[i].name);
      parse_error(parser, error);
    }
    parser->ic->heap[parser->lcs[i].loc] = *binding;
  }
}

//-----------------------------------------------------------------------------
// UTF-8 helper functions
//-----------------------------------------------------------------------------

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

//-----------------------------------------------------------------------------
// Parse helper functions
//-----------------------------------------------------------------------------

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
void init_parser(Parser* parser, IC* ic, const char* input) {
  parser->ic = ic;
  parser->input = input;
  parser->pos = 0;
  parser->line = 1;
  parser->col = 1;
  parser->lcs_count = 0;
  parser->vrs_count = 0;
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

// Store a term at the given location
void store_term(Parser* parser, uint32_t loc, TermTag tag, uint32_t value) {
  parser->ic->heap[loc] = ic_make_term(tag, value);
}

// Parse an unsigned integer
uint8_t parse_uint(Parser* parser) {
  uint8_t value = 0;
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

//-----------------------------------------------------------------------------
// Term parsing functions (moved from parse/term/*)
//-----------------------------------------------------------------------------

// Parse a variable
void parse_term_var(Parser* parser, uint32_t loc) {
  char* name = parse_name(parser);
  add_var_use(parser, name, loc);
}

// Parse a superposition
void parse_term_sup(Parser* parser, uint32_t loc) {
  expect(parser, "&", "for superposition");

  uint8_t label = parse_uint(parser) & 0x3; // Only first 2 bits (0-3)
  expect(parser, "{", "after label in superposition");

  uint32_t sup_node = ic_alloc(parser->ic, 2);
  uint32_t lft_loc = sup_node;
  uint32_t rgt_loc = sup_node + 1;

  parse_term(parser, lft_loc);
  expect(parser, ",", "between terms in superposition");

  parse_term(parser, rgt_loc);
  expect(parser, "}", "after terms in superposition");

  // Use the appropriate superposition tag based on label
  TermTag sup_tag = SUP_TAG(label);
  store_term(parser, loc, sup_tag, sup_node);
}

// Parse a lambda
void parse_term_lam(Parser* parser, uint32_t loc) {
  if (check_utf8(parser, 0xCE, 0xBB)) {
    consume_utf8(parser, 2);
  } else if (!consume(parser, "λ")) {
    parse_error(parser, "Expected 'λ' for lambda");
  }

  char* name = parse_name(parser);
  expect(parser, ".", "after name in lambda");

  uint32_t lam_node = ic_alloc(parser->ic, 1);

  Term var_term = ic_make_term(VAR, lam_node);
  bind_var(parser, name, var_term);

  parse_term(parser, lam_node);
  store_term(parser, loc, LAM, lam_node);
}

// Parse a collapser
void parse_term_col(Parser* parser, uint32_t loc) {
  expect(parser, "!&", "for collapser");

  uint8_t label = parse_uint(parser) & 0x3; // Only first 2 bits (0-3)
  expect(parser, "{", "after label in collapser");

  static char var1[MAX_NAME_LEN], var2[MAX_NAME_LEN];

  char* temp_name = parse_name(parser);
  strncpy(var1, temp_name, MAX_NAME_LEN - 1);
  var1[MAX_NAME_LEN - 1] = '\0';

  expect(parser, ",", "between names in collapser");

  temp_name = parse_name(parser);
  strncpy(var2, temp_name, MAX_NAME_LEN - 1);
  var2[MAX_NAME_LEN - 1] = '\0';

  expect(parser, "}", "after names in collapser");
  expect(parser, "=", "after names in collapser");

  // Allocate a node specifically for the collapse value
  uint32_t col_node = ic_alloc(parser->ic, 1);

  // Create collapse variable terms that point to the col_node, NOT to the loc
  Term co0_term = ic_make_co0(label, col_node);
  Term co1_term = ic_make_co1(label, col_node);
  bind_var(parser, var1, co0_term);
  bind_var(parser, var2, co1_term);

  // Parse the collapse value into col_node
  parse_term(parser, col_node);
  expect(parser, ";", "after value in collapser");

  // Parse the body of the collapse into loc
  parse_term(parser, loc);
}

// Parse an application
void parse_term_app(Parser* parser, uint32_t loc) {
  expect(parser, "(", "for application");

  uint32_t app_node = ic_alloc(parser->ic, 2);
  uint32_t fun_loc = app_node;
  uint32_t arg_loc = app_node + 1;

  parse_term(parser, fun_loc);

  if (!isspace(parser->input[parser->pos])) {
    parse_error(parser, "Expected whitespace between function and argument in application");
  }
  skip(parser);

  parse_term(parser, arg_loc);
  expect(parser, ")", "after application");

  store_term(parser, loc, APP, app_node);
}

// Parse a let expression (syntax sugar for application of lambda)
void parse_term_let(Parser* parser, uint32_t loc) {
  expect(parser, "!", "for let expression");

  char* name = parse_name(parser);
  expect(parser, "=", "after name in let expression");

  // Allocate nodes for the application and lambda
  uint32_t app_node = ic_alloc(parser->ic, 2);
  uint32_t lam_node = ic_alloc(parser->ic, 1);

  // Set up the application structure
  uint32_t fun_loc = app_node;     // lambda function
  uint32_t arg_loc = app_node + 1; // value

  // Create variable term for the lambda parameter
  bind_var(parser, name, ic_make_term(VAR, lam_node));

  // Parse the value into arg_loc
  parse_term(parser, arg_loc);

  expect(parser, ";", "after value in let expression");

  // Parse the body into lam_node
  parse_term(parser, lam_node);

  // Store the lambda at fun_loc
  store_term(parser, fun_loc, LAM, lam_node);

  // Store the application at loc
  store_term(parser, loc, APP, app_node);
}

// Main term parser - dispatcher for specific term types (moved from parse/term.c)
void parse_term(Parser* parser, uint32_t loc) {
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

//-----------------------------------------------------------------------------
// Public parsing functions
//-----------------------------------------------------------------------------

// Allocate space for a term and parse into it
uint32_t parse_term_alloc(Parser* parser) {
  uint32_t loc = ic_alloc(parser->ic, 1);
  parse_term(parser, loc);
  return loc;
}

// Parse a string into a term
Term parse_string(IC* ic, const char* input) {
  Parser parser;
  init_parser(&parser, ic, input);

  skip(&parser);

  // Parse the main term
  uint32_t term_loc = parse_term_alloc(&parser);

  // Resolve variable uses for the main term
  resolve_var_uses(&parser);

  return parser.ic->heap[term_loc];
}

// Parse a file into a term
Term parse_file(IC* ic, const char* filename) {
  FILE* file = fopen(filename, "r");
  if (!file) {
    fprintf(stderr, "Error: Could not open file '%s'\n", filename);
    exit(1);
  }

  // Get file size
  fseek(file, 0, SEEK_END);
  long size = ftell(file);
  fseek(file, 0, SEEK_SET);

  // Allocate buffer
  char* buffer = (char*)malloc(size + 1);
  if (!buffer) {
    fprintf(stderr, "Error: Memory allocation failed\n");
    fclose(file);
    exit(1);
  }

  // Read file contents
  size_t read_size = fread(buffer, 1, size, file);
  fclose(file);

  buffer[read_size] = '\0';

  // Parse the string
  Term term = parse_string(ic, buffer);

  // Free the buffer
  free(buffer);

  return term;
}
