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
    parser->hvmn->heap[parser->lcs[i].loc] = *binding;
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
void init_parser(Parser* parser, HVMN* hvmn, const char* input) {
  parser->hvmn = hvmn;
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
void store_term(Parser* parser, uint32_t loc, TermTag tag, uint8_t label, uint32_t value) {
  parser->hvmn->heap[loc] = hvmn_make_term(tag, label, value);
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

// Map a function name (A-P) to its function ID (0-15)
uint8_t get_function_id(const char* name) {
  // For simplicity, we only support uppercase letters A-P as function names
  if (strlen(name) != 1 || name[0] < 'A' || name[0] > 'P') {
    fprintf(stderr, "Error: Function names must be uppercase letters A-P\n");
    exit(1);
  }
  return (uint8_t)(name[0] - 'A'); // A=0, B=1, ..., P=15
}

// Parse a variable
void parse_term_var(Parser* parser, uint32_t loc) {
  char* name = parse_name(parser);
  add_var_use(parser, name, loc);
}

// Parse a superposition
void parse_term_sup(Parser* parser, uint32_t loc) {
  expect(parser, "&", "for superposition");

  uint8_t label = parse_uint(parser) & 0xF; // Ensure it fits in 4 bits
  expect(parser, "{", "after label in superposition");

  uint32_t sup_node = hvmn_alloc(parser->hvmn, 2);
  uint32_t lft_loc = sup_node;
  uint32_t rgt_loc = sup_node + 1;

  parse_term(parser, lft_loc);
  expect(parser, ",", "between terms in superposition");

  parse_term(parser, rgt_loc);
  expect(parser, "}", "after terms in superposition");

  store_term(parser, loc, SUP, label, sup_node);
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

  uint32_t lam_node = hvmn_alloc(parser->hvmn, 1);

  Term var_term = hvmn_make_term(VAR, 0, lam_node);
  bind_var(parser, name, var_term);

  parse_term(parser, lam_node);
  store_term(parser, loc, LAM, 0, lam_node);
}

// Parse a collapser
void parse_term_col(Parser* parser, uint32_t loc) {
  expect(parser, "!&", "for collapser");

  uint8_t label = parse_uint(parser) & 0xF; // Ensure it fits in 4 bits
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
  uint32_t col_node = hvmn_alloc(parser->hvmn, 1);

  // Create collapse variable terms that point to the col_node, NOT to the loc
  Term co0_term = hvmn_make_term(CO0, label, col_node);
  Term co1_term = hvmn_make_term(CO1, label, col_node);
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

  uint32_t app_node = hvmn_alloc(parser->hvmn, 2);
  uint32_t fun_loc = app_node;
  uint32_t arg_loc = app_node + 1;

  parse_term(parser, fun_loc);

  if (!isspace(parser->input[parser->pos])) {
    parse_error(parser, "Expected whitespace between function and argument in application");
  }
  skip(parser);

  parse_term(parser, arg_loc);
  expect(parser, ")", "after application");

  store_term(parser, loc, APP, 0, app_node);
}

// Parse a number literal (NAT)
void parse_term_num(Parser* parser, uint32_t loc) {
  uint32_t value = 0;
  bool has_digit = false;

  while (isdigit(peek_char(parser))) {
    value = value * 10 + (next_char(parser) - '0');
    has_digit = true;
  }

  if (!has_digit) {
    parse_error(parser, "Expected number");
  }

  // For NAT terms, store the value directly in the term with label=0
  store_term(parser, loc, NAT, 0, value);
}

// Parse a successor term (SUC) - now implemented as a special CAL
void parse_term_suc(Parser* parser, uint32_t loc) {
  expect(parser, "+", "for successor");

  // Allocate space for the predecessor term
  uint32_t cal_node = hvmn_alloc(parser->hvmn, 1);

  // Parse the predecessor term
  parse_term(parser, cal_node);

  // Store the term as a CAL with special label SUC (the built-in increment function)
  store_term(parser, loc, CAL, SUC, cal_node);
}

// Parse a function call (CAL)
void parse_term_cal(Parser* parser, uint32_t loc) {
  expect(parser, "@", "for function call");

  // Parse function name
  char* name = parse_name(parser);

  // Look up function ID
  uint8_t func_id = get_function_id(name);

  expect(parser, "(", "after function name in call");

  // Allocate space for the argument
  uint32_t cal_node = hvmn_alloc(parser->hvmn, 1);

  // Parse the argument
  parse_term(parser, cal_node);

  expect(parser, ")", "after argument in function call");

  // Store the CAL term with the function ID as the label
  store_term(parser, loc, CAL, func_id, cal_node);
}

// Parse a let expression (syntax sugar for application of lambda)
void parse_term_let(Parser* parser, uint32_t loc) {
  expect(parser, "!", "for let expression");

  char* name = parse_name(parser);
  expect(parser, "=", "after name in let expression");

  // Allocate nodes for the application and lambda
  uint32_t app_node = hvmn_alloc(parser->hvmn, 2);
  uint32_t lam_node = hvmn_alloc(parser->hvmn, 1);

  // Set up the application structure
  uint32_t fun_loc = app_node;     // lambda function
  uint32_t arg_loc = app_node + 1; // value

  // Create variable term for the lambda parameter
  bind_var(parser, name, hvmn_make_term(VAR, 0, lam_node));

  // Parse the value into arg_loc
  parse_term(parser, arg_loc);

  expect(parser, ";", "after value in let expression");

  // Parse the body into lam_node
  parse_term(parser, lam_node);

  // Store the lambda at fun_loc
  store_term(parser, fun_loc, LAM, 0, lam_node);

  // Store the application at loc
  store_term(parser, loc, APP, 0, app_node);
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
  } else if (c == '@') {
    parse_term_cal(parser, loc);
  } else if (c == '+') {
    parse_term_suc(parser, loc);
  } else if (isdigit(c)) {
    parse_term_num(parser, loc);
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
// Function book parsing
//-----------------------------------------------------------------------------

// Parse a function definition (@A(0) = ... @A(1+n) = ...)
void parse_function(Parser* parser) {
  expect(parser, "@", "for function definition");

  // Parse function name
  char* name = parse_name(parser);
  uint8_t func_id = get_function_id(name);

  // Check if function already exists
  if (func_id >= parser->hvmn->book->function_count) {
    // New function
    parser->hvmn->book->function_count = func_id + 1;
    parser->hvmn->book->functions[func_id].clause_count = 0;
  }

  // Parse and open parenthesis
  if (!expect(parser, "(", "after function name")) {
    return;
  }

  // Parse pattern number
  if (!isdigit(peek_char(parser))) {
    parse_error(parser, "Expected number in function pattern");
    return;
  }

  uint32_t pattern_num = parse_uint(parser);

  // Check for pattern variable
  bool has_variable = false;
  char var_name[MAX_NAME_LEN] = "";

  if (consume(parser, "+")) {
    has_variable = true;
    char* name = parse_name(parser);
    strncpy(var_name, name, MAX_NAME_LEN - 1);
    var_name[MAX_NAME_LEN - 1] = '\0';
  }

  // Close parenthesis
  if (!expect(parser, ")", "after function pattern")) {
    return;
  }

  // Require equals sign
  if (!expect(parser, "=", "after function pattern")) {
    return;
  }

  // Get the current clause
  uint8_t clause_idx = parser->hvmn->book->functions[func_id].clause_count;

  // Patterns should be consecutive
  if (pattern_num != clause_idx) {
    parse_error(parser, "Function patterns must be consecutive starting from 0");
    return;
  }

  if (clause_idx >= MAX_CLAUSES) {
    parse_error(parser, "Too many clauses for a single function");
    return;
  }

  // If we have a pattern variable, bind it as a variable with the special value
  if (has_variable) {
    // Create a VAR term with the special PATTERN_VAR_MASK value
    Term pattern_var = MAKE_TERM(false, VAR, 0, PATTERN_VAR_MASK);
    bind_var(parser, var_name, pattern_var);
  }

  // Record the starting position in the heap before parsing
  uint32_t heap_start = parser->hvmn->heap_pos;

  // Parse the clause body directly into the heap
  uint32_t clause_loc = hvmn_alloc(parser->hvmn, 1);
  parse_term(parser, clause_loc);

  // Resolve variable uses for this clause
  resolve_var_uses(parser);

  // Calculate the number of terms used by this clause
  uint32_t term_count = parser->hvmn->heap_pos - heap_start;

  // Clear variable bindings and uses for the next clause
  parser->vrs_count = 0;
  parser->lcs_count = 0;

  // Allocate memory for all terms in the clause
  Clause* clause = &parser->hvmn->book->functions[func_id].clauses[clause_idx];
  clause->terms = (Term*)malloc(term_count * sizeof(Term));
  if (!clause->terms) {
    parse_error(parser, "Memory allocation failed for function clause");
    return;
  }
  
  clause->term_count = term_count;
  
  // Copy all terms from the heap to the clause, adjusting pointers to be relative
  for (uint32_t i = 0; i < term_count; i++) {
    Term term = parser->hvmn->heap[heap_start + i];
    TermTag tag = TERM_TAG(term);
    uint32_t val = TERM_VAL(term);
    
    // If this is a pointer to another term in the same clause, make it relative
    if ((tag == VAR && val != PATTERN_VAR_MASK) || 
        tag == LAM || tag == APP || tag == SUP || tag == CAL || 
        (tag == NAT && TERM_LAB(term) == 1)) {
      
      if (val >= heap_start && val < heap_start + term_count) {
        // Convert to a relative offset from the start of the clause
        val -= heap_start;
        term = MAKE_TERM(TERM_SUB(term), tag, TERM_LAB(term), val);
      }
    }
    
    clause->terms[i] = term;
  }

  // Increment clause count
  parser->hvmn->book->functions[func_id].clause_count++;
}

// Parse the book of functions
void parse_book(Parser* parser) {
  skip(parser);

  // Parse functions as long as we see @ character
  while (peek_char(parser) == '@') {
    parse_function(parser);
    skip(parser);
  }
}

//-----------------------------------------------------------------------------
// Public parsing functions
//-----------------------------------------------------------------------------

// Allocate space for a term and parse into it
uint32_t parse_term_alloc(Parser* parser) {
  uint32_t loc = hvmn_alloc(parser->hvmn, 1);
  parse_term(parser, loc);
  return loc;
}

// Parse a string into a term
Term parse_string(HVMN* hvmn, const char* input) {
  Parser parser;
  init_parser(&parser, hvmn, input);

  // First parse the book of functions
  parse_book(&parser);

  skip(&parser);
  
  // Check for main term marker
  if (peek_char(&parser) == '>') {
    next_char(&parser); // Skip the '>' character
    skip(&parser);
  }

  // Parse the main term
  uint32_t term_loc = parse_term_alloc(&parser);

  // Resolve variable uses for the main term
  resolve_var_uses(&parser);

  return parser.hvmn->heap[term_loc];
}

// Parse a file into a term
Term parse_file(HVMN* hvmn, const char* filename) {
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
  Term term = parse_string(hvmn, buffer);

  // Free the buffer
  free(buffer);

  return term;
}
