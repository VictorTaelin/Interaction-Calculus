#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "parse.h"
#include "memory.h"
#include "show.h"

// Initialize a parser with the given input string
void init_parser(Parser* parser, const char* input) {
  parser->input = input;
  parser->pos = 0;
  parser->line = 1;
  parser->col = 1;
  parser->lcs_count = 0;
  parser->vrs_count = 0;
}

// Parse a string into a term
Term parse_string(const char* input) {
  // Initialize memory if not already initialized
  init_memory();
  
  // Initialize parser
  Parser parser;
  init_parser(&parser, input);
  
  // Parse the term
  uint32_t loc = parse_term_alloc(&parser);
  
  // Resolve all variable uses
  resolve_var_uses(&parser);
  
  // Return the parsed term
  return heap[loc];
}

// Helper function to allocate memory and parse a term
uint32_t parse_term_alloc(Parser* parser) {
  uint32_t loc = alloc(1);
  parse_term(parser, loc);
  return loc;
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

// Add a variable binding
void add_var_binding(Parser* parser, const char* name, Term term) {
  if (parser->vrs_count >= MAX_VARS) {
    parse_error(parser, "Too many variable bindings");
  }
  
  strncpy(parser->vrs[parser->vrs_count].name, name, MAX_NAME_LEN - 1);
  parser->vrs[parser->vrs_count].name[MAX_NAME_LEN - 1] = '\0';
  parser->vrs[parser->vrs_count].term = term;
  parser->vrs_count++;
}

// Look up a variable binding by name
Term* lookup_var_binding(Parser* parser, const char* name) {
  for (int i = 0; i < parser->vrs_count; i++) {
    if (strcmp(parser->vrs[i].name, name) == 0) {
      return &parser->vrs[i].term;
    }
  }
  return NULL;
}

// Resolve all variable uses after parsing
void resolve_var_uses(Parser* parser) {
  // Now resolve each variable use
  for (size_t i = 0; i < parser->lcs_count; i++) {
    const char* name = parser->lcs[i].name;
    uint32_t loc = parser->lcs[i].loc;
    
    Term* binding = lookup_var_binding(parser, name);
    if (binding) {
      // Found binding, update the heap
      heap[loc] = *binding;
    } else {
      // No binding found, report error
      char error_msg[100];
      snprintf(error_msg, sizeof(error_msg), "Undefined variable: %s", name);
      parse_error(parser, error_msg);
    }
  }
}

// Skip whitespace characters
void parse_whitespace(Parser* parser) {
  while (parser->input[parser->pos] != '\0') {
    char c = parser->input[parser->pos];
    
    // Skip spaces, tabs, and newlines
    if (c == ' ' || c == '\t') {
      parser->pos++;
      parser->col++;
    } else if (c == '\n') {
      parser->pos++;
      parser->line++;
      parser->col = 1;
    } else if (c == '\r') {
      parser->pos++;
      // Check for \r\n sequence
      if (parser->input[parser->pos] == '\n') {
        parser->pos++;
      }
      parser->line++;
      parser->col = 1;
    } else if (c == '/' && parser->input[parser->pos + 1] == '/') {
      // Skip single-line comments
      parser->pos += 2;
      parser->col += 2;
      while (parser->input[parser->pos] != '\0' && 
             parser->input[parser->pos] != '\n') {
        parser->pos++;
        parser->col++;
      }
    } else {
      // Not whitespace, stop skipping
      break;
    }
  }
}

// Look at the next character without consuming it
char peek_char(Parser* parser) {
  parse_whitespace(parser);
  return parser->input[parser->pos];
}

// Check if the next character is a specific character
bool peek_is(Parser* parser, char c) {
  return peek_char(parser) == c;
}

// Consume the next character
char next_char(Parser* parser) {
  parse_whitespace(parser);
  char c = parser->input[parser->pos];
  if (c != '\0') {
    parser->pos++;
    parser->col++;
  }
  return c;
}

// Try to consume a specific string
bool consume(Parser* parser, const char* str) {
  parse_whitespace(parser);
  
  size_t len = strlen(str);
  size_t i = 0;
  size_t pos = parser->pos;
  
  // Try to match each character
  while (i < len && parser->input[pos] != '\0') {
    // Skip whitespace within the expected string
    if (isspace(parser->input[pos])) {
      pos++;
      continue;
    }
    
    // Skip whitespace within the pattern string
    if (isspace(str[i])) {
      i++;
      continue;
    }
    
    // Check if characters match
    if (parser->input[pos] != str[i]) {
      return false;
    }
    
    pos++;
    i++;
  }
  
  // If we matched the full string
  if (i == len) {
    // Update parser position
    size_t old_pos = parser->pos;
    parser->pos = pos;
    parser->col += (pos - old_pos);
    return true;
  }
  
  return false;
}

// Parse an unsigned integer
uint32_t parse_uint(Parser* parser) {
  parse_whitespace(parser);
  
  // Check if we have a digit
  if (!isdigit(parser->input[parser->pos])) {
    parse_error(parser, "Expected a digit");
  }
  
  // Parse the number
  uint32_t value = 0;
  while (isdigit(parser->input[parser->pos])) {
    value = value * 10 + (parser->input[parser->pos] - '0');
    parser->pos++;
    parser->col++;
  }
  
  return value;
}

// The old functions register_name and lookup_name are no longer used.
// They have been replaced by add_var_use, add_var_binding, lookup_var_binding, and resolve_var_uses.
// The implementation of these new functions is provided above.

// Parse a name (identifier)
char* parse_name(Parser* parser) {
  parse_whitespace(parser);
  
  // Check if we have a valid identifier start
  if (!isalpha(parser->input[parser->pos]) && parser->input[parser->pos] != '_') {
    parse_error(parser, "Expected an identifier");
  }
  
  // Allocate memory for the name
  static char name[MAX_NAME_LEN];
  size_t i = 0;
  
  // Parse the identifier
  while (isalnum(parser->input[parser->pos]) || parser->input[parser->pos] == '_') {
    if (i < MAX_NAME_LEN - 1) {
      name[i++] = parser->input[parser->pos];
    }
    parser->pos++;
    parser->col++;
  }
  
  // Null terminate the name
  name[i] = '\0';
  
  return name;
}

// Error reporting
void parse_error(Parser* parser, const char* message) {
  fprintf(stderr, "Parse error at line %zu, column %zu: %s\n",
          parser->line, parser->col, message);
  
  // Print the line with the error
  const char* line_start = parser->input + parser->pos;
  while (line_start > parser->input && *(line_start - 1) != '\n') {
    line_start--;
  }
  
  const char* line_end = parser->input + parser->pos;
  while (*line_end != '\0' && *line_end != '\n') {
    line_end++;
  }
  
  // Print the line
  fprintf(stderr, "%.*s\n", (int)(line_end - line_start), line_start);
  
  // Print the error pointer
  size_t col = parser->col;
  fprintf(stderr, "%*s^\n", (int)col - 1, "");
  
  exit(1);
}

// Helper function to check if the current position has a specific UTF-8 character
bool check_utf8(Parser* parser, uint8_t b1, uint8_t b2) {
  return (unsigned char)parser->input[parser->pos] == b1 && 
         (unsigned char)parser->input[parser->pos + 1] == b2;
}

bool check_utf8_3bytes(Parser* parser, uint8_t b1, uint8_t b2, uint8_t b3) {
  return (unsigned char)parser->input[parser->pos] == b1 && 
         (unsigned char)parser->input[parser->pos + 1] == b2 &&
         (unsigned char)parser->input[parser->pos + 2] == b3;
}

bool check_utf8_4bytes(Parser* parser, uint8_t b1, uint8_t b2, uint8_t b3, uint8_t b4) {
  return (unsigned char)parser->input[parser->pos] == b1 && 
         (unsigned char)parser->input[parser->pos + 1] == b2 &&
         (unsigned char)parser->input[parser->pos + 2] == b3 &&
         (unsigned char)parser->input[parser->pos + 3] == b4;
}

// Main term parser
void parse_term(Parser* parser, uint32_t loc) {
  parse_whitespace(parser);
  
  // Check for end of input
  if (parser->input[parser->pos] == '\0') {
    parse_error(parser, "Unexpected end of input");
  }
  
  // Get the next character and its code
  unsigned char c = (unsigned char)parser->input[parser->pos];
  
  if (isalpha(c) || c == '_') {
    // Variable
    parse_var(parser, loc);
  } else if (c == '!') {
    // Check the next character to determine if it's a let binding or a collapser
    parser->pos++; // Consume the '!'
    char next = peek_char(parser);
    parser->pos--; // Move back to before the '!'
    
    if (next == '&') {
      // Collapser
      parse_col(parser, loc);
    } else if (next == '[') {
      // Sigma type elimination (get)
      parse_get(parser, loc);
    } else {
      // Let binding
      parse_let(parser, loc);
    }
  } else if (c == '&') {
    // Superposition
    parse_sup(parser, loc);
  } else if (c == '*') {
    // Universe type
    parse_set(parser, loc);
  } else if (c == 0xE2) {
    // Unicode symbols starting with E2
    // Could be ⊥ (E2 8A A5) or ⊤ (E2 8A A4)
    if ((unsigned char)parser->input[parser->pos + 1] == 0x8A) {
      if ((unsigned char)parser->input[parser->pos + 2] == 0xA5) {
        // Empty type (⊥)
        parse_emp(parser, loc);
      } else if ((unsigned char)parser->input[parser->pos + 2] == 0xA4) {
        // Unit type (⊤)
        parse_uni(parser, loc);
      } else {
        parse_error(parser, "Unknown Unicode symbol starting with E2 8A");
      }
    } else {
      parse_error(parser, "Unknown Unicode symbol starting with E2");
    }
  } else if (c == 0xC2 && (unsigned char)parser->input[parser->pos + 1] == 0xAC) {
    // Empty type elimination (¬)
    parse_efq(parser, loc);
  } else if (c == '(' && parser->input[parser->pos + 1] == ')') {
    // Unit value ()
    parse_nil(parser, loc);
  } else if (c == '-') {
    // Unit elimination
    parse_use(parser, loc);
  } else if (c == 0xF0 && 
            (unsigned char)parser->input[parser->pos + 1] == 0x9D && 
            (unsigned char)parser->input[parser->pos + 2] == 0x94 && 
            (unsigned char)parser->input[parser->pos + 3] == 0xB9) {
    // Bool type (𝔹)
    parse_bit(parser, loc);
  } else if (c == '0') {
    // False value
    parse_b_0(parser, loc);
  } else if (c == '1') {
    // True value
    parse_b_1(parser, loc);
  } else if (c == '?') {
    // Bool elimination
    parse_ite(parser, loc);
  } else if (c == 0xCE) {
    // Unicode symbols starting with CE
    unsigned char next_byte = (unsigned char)parser->input[parser->pos + 1];
    
    if (next_byte == 0xA3) {
      // Sigma type (Σ)
      parse_sig(parser, loc);
    } else if (next_byte == 0xA0) {
      // Pi type (Π)
      parse_all(parser, loc);
    } else if (next_byte == 0xBB) {
      // Lambda (λ)
      parse_lam(parser, loc);
    } else if (next_byte == 0xB8) {
      // Reflexivity (θ)
      parse_rfl(parser, loc);
    } else {
      parse_error(parser, "Unknown Unicode symbol starting with CE");
    }
  } else if (c == '[') {
    // Tuple
    parse_tup(parser, loc);
  } else if (c == '(') {
    // Application or grouping
    parse_app(parser, loc);
  } else if (c == '<') {
    // Equality type
    parse_eql(parser, loc);
  } else if (c == '%') {
    // Equality elimination
    parse_rwt(parser, loc);
  } else {
    // Unknown term type
    char error_msg[100];
    snprintf(error_msg, sizeof(error_msg), "Unexpected character: %c (code: %d)", c, (int)c);
    parse_error(parser, error_msg);
  }
}

// Parse a variable
void parse_var(Parser* parser, uint32_t loc) {
  char* name = parse_name(parser);
  
  // Add this variable use to the lcs array to be resolved later
  add_var_use(parser, name, loc);
}

// Parse a let binding
void parse_let(Parser* parser, uint32_t loc) {
  if (!consume(parser, "!")) {
    parse_error(parser, "Expected '!' for let binding");
  }
  
  char* name = parse_name(parser);
  
  if (!consume(parser, "=")) {
    parse_error(parser, "Expected '=' after name in let binding");
  }
  
  uint32_t let_node = alloc(2);
  uint32_t val_loc = let_node;
  uint32_t bod_loc = let_node + 1;
  
  // Parse the value
  parse_term(parser, val_loc);
  
  if (!consume(parser, ";")) {
    parse_error(parser, "Expected ';' after value in let binding");
  }
  
  // Add variable binding pointing to the value location
  Term var_term = make_term(VAR, 0, val_loc);
  add_var_binding(parser, name, var_term);
  
  // Parse the body
  parse_term(parser, bod_loc);
  
  // Create the let term
  heap[loc] = make_term(LET, 0, let_node);
}

// Parse a superposition
void parse_sup(Parser* parser, uint32_t loc) {
  if (!consume(parser, "&")) {
    parse_error(parser, "Expected '&' for superposition");
  }
  
  // Parse the label
  uint8_t label = parse_uint(parser) & 3; // Ensure it fits in 2 bits
  
  if (!consume(parser, "{")) {
    parse_error(parser, "Expected '{' after label in superposition");
  }
  
  uint32_t sup_node = alloc(2);
  uint32_t lft_loc = sup_node;
  uint32_t rgt_loc = sup_node + 1;
  
  // Parse the left term
  parse_term(parser, lft_loc);
  
  if (!consume(parser, ",")) {
    parse_error(parser, "Expected ',' between terms in superposition");
  }
  
  // Parse the right term
  parse_term(parser, rgt_loc);
  
  if (!consume(parser, "}")) {
    parse_error(parser, "Expected '}' after terms in superposition");
  }
  
  // Create the superposition term
  heap[loc] = make_term(SUP, label, sup_node);
}

// Parse a collapser
void parse_col(Parser* parser, uint32_t loc) {
  if (!consume(parser, "!&")) {
    parse_error(parser, "Expected '!&' for collapser");
  }
  
  // Parse the label
  uint8_t label = parse_uint(parser) & 3; // Ensure it fits in 2 bits
  
  if (!consume(parser, "{")) {
    parse_error(parser, "Expected '{' after label in collapser");
  }
  
  // IMPORTANT: We need to make local copies of these immediately
  static char var1[MAX_NAME_LEN];
  static char var2[MAX_NAME_LEN];
  
  // Parse the first variable name and immediately make a copy
  char* temp_name = parse_name(parser);
  strncpy(var1, temp_name, MAX_NAME_LEN - 1);
  var1[MAX_NAME_LEN - 1] = '\0';
  
  if (!consume(parser, ",")) {
    parse_error(parser, "Expected ',' between names in collapser");
  }
  
  // Parse the second variable name and immediately make a copy
  temp_name = parse_name(parser);
  strncpy(var2, temp_name, MAX_NAME_LEN - 1);
  var2[MAX_NAME_LEN - 1] = '\0';
  
  if (!consume(parser, "}")) {
    parse_error(parser, "Expected '}' after names in collapser");
  }
  
  if (!consume(parser, "=")) {
    parse_error(parser, "Expected '=' after names in collapser");
  }
  
  uint32_t col_node = alloc(1); // Only need space for the value
  uint32_t val_loc = col_node;
  
  // Add variable bindings for the CO0 and CO1 terms
  Term co0_term = make_term(CO0, label, loc); // Point to the location where body will be stored
  Term co1_term = make_term(CO1, label, loc);
  add_var_binding(parser, var1, co0_term);
  add_var_binding(parser, var2, co1_term);
  
  // Parse the value being collapsed
  parse_term(parser, val_loc);
  
  if (!consume(parser, ";")) {
    parse_error(parser, "Expected ';' after value in collapser");
  }
  
  // Parse the body directly into the location provided
  parse_term(parser, loc);
  
  // The body is directly stored at loc, so we don't need to create a new term
}

// Parse a universe type
void parse_set(Parser* parser, uint32_t loc) {
  if (!consume(parser, "*")) {
    parse_error(parser, "Expected '*' for universe type");
  }
  
  heap[loc] = make_term(SET, 0, 0);
}

// Parse an empty type
void parse_emp(Parser* parser, uint32_t loc) {
  // Check for UTF-8 encoding of ⊥ (E2 8A A5)
  if (check_utf8_3bytes(parser, 0xE2, 0x8A, 0xA5)) {
    parser->pos += 3;
    parser->col++;
    heap[loc] = make_term(EMP, 0, 0);
  } else {
    parse_error(parser, "Expected '⊥' for empty type");
  }
}

// Parse an empty type elimination
void parse_efq(Parser* parser, uint32_t loc) {
  // Check for UTF-8 encoding of ¬ (C2 AC)
  if (check_utf8(parser, 0xC2, 0xAC)) {
    parser->pos += 2;
    parser->col++;
    
    uint32_t efq_node = alloc(1);
    
    // Parse the term being eliminated
    parse_term(parser, efq_node);
    
    heap[loc] = make_term(EFQ, 0, efq_node);
  } else {
    parse_error(parser, "Expected '¬' for empty type elimination");
  }
}

// Parse a unit type
void parse_uni(Parser* parser, uint32_t loc) {
  // Check for UTF-8 encoding of ⊤ (E2 8A A4)
  if (check_utf8_3bytes(parser, 0xE2, 0x8A, 0xA4)) {
    parser->pos += 3;
    parser->col++;
    heap[loc] = make_term(UNI, 0, 0);
  } else {
    parse_error(parser, "Expected '⊤' for unit type");
  }
}

// Parse a unit value
void parse_nil(Parser* parser, uint32_t loc) {
  if (!consume(parser, "()")) {
    parse_error(parser, "Expected '()' for unit value");
  }
  
  heap[loc] = make_term(NIL, 0, 0);
}

// Parse a unit elimination
void parse_use(Parser* parser, uint32_t loc) {
  if (!consume(parser, "-")) {
    parse_error(parser, "Expected '-' for unit elimination");
  }
  
  uint32_t use_node = alloc(2);
  uint32_t val_loc = use_node;
  uint32_t bod_loc = use_node + 1;
  
  // Parse the unit value
  parse_term(parser, val_loc);
  
  if (!consume(parser, ";")) {
    parse_error(parser, "Expected ';' after unit value");
  }
  
  // Parse the body
  parse_term(parser, bod_loc);
  
  heap[loc] = make_term(USE, 0, use_node);
}

// Parse a bool type
void parse_bit(Parser* parser, uint32_t loc) {
  // Check for UTF-8 encoding of 𝔹 (F0 9D 94 B9)
  if (check_utf8_4bytes(parser, 0xF0, 0x9D, 0x94, 0xB9)) {
    parser->pos += 4;
    parser->col++;
  } else if (!consume(parser, "𝔹")) {
    parse_error(parser, "Expected '𝔹' for bool type");
  }
  
  heap[loc] = make_term(BIT, 0, 0);
}

// Parse a false value
void parse_b_0(Parser* parser, uint32_t loc) {
  if (!consume(parser, "0")) {
    parse_error(parser, "Expected '0' for false value");
  }
  
  heap[loc] = make_term(B_0, 0, 0);
}

// Parse a true value
void parse_b_1(Parser* parser, uint32_t loc) {
  if (!consume(parser, "1")) {
    parse_error(parser, "Expected '1' for true value");
  }
  
  heap[loc] = make_term(B_1, 0, 0);
}

// Parse a bool elimination
void parse_ite(Parser* parser, uint32_t loc) {
  if (!consume(parser, "?")) {
    parse_error(parser, "Expected '?' for bool elimination");
  }
  
  uint32_t ite_node = alloc(3);
  uint32_t cnd_loc = ite_node;
  uint32_t thn_loc = ite_node + 1;
  uint32_t els_loc = ite_node + 2;
  
  // Parse the condition
  parse_term(parser, cnd_loc);
  
  if (!consume(parser, "{")) {
    parse_error(parser, "Expected '{' after condition in bool elimination");
  }
  
  // Parse the then branch
  parse_term(parser, thn_loc);
  
  if (!consume(parser, "};{")) {
    parse_error(parser, "Expected '};{' between branches in bool elimination");
  }
  
  // Parse the else branch
  parse_term(parser, els_loc);
  
  if (!consume(parser, "}")) {
    parse_error(parser, "Expected '}' after else branch in bool elimination");
  }
  
  heap[loc] = make_term(ITE, 0, ite_node);
}

// Parse a sigma type
void parse_sig(Parser* parser, uint32_t loc) {
  // Check for UTF-8 encoding of Σ (CE A3)
  if (check_utf8(parser, 0xCE, 0xA3)) {
    parser->pos += 2;
    parser->col++;
  } else if (!consume(parser, "Σ")) {
    parse_error(parser, "Expected 'Σ' for sigma type");
  }
  
  // Parse the variable name
  char* name = parse_name(parser);
  
  if (!consume(parser, ":")) {
    parse_error(parser, "Expected ':' after name in sigma type");
  }
  
  uint32_t sig_node = alloc(2);
  uint32_t fst_loc = sig_node;
  uint32_t snd_loc = sig_node + 1;
  
  // Parse the first type
  parse_term(parser, fst_loc);
  
  if (!consume(parser, ".")) {
    parse_error(parser, "Expected '.' after first type in sigma type");
  }
  
  // Add variable binding for the second type
  Term var_term = make_term(VAR, 0, fst_loc);
  add_var_binding(parser, name, var_term);
  
  // Parse the second type
  parse_term(parser, snd_loc);
  
  heap[loc] = make_term(SIG, 0, sig_node);
}

// Parse a tuple
void parse_tup(Parser* parser, uint32_t loc) {
  if (!consume(parser, "[")) {
    parse_error(parser, "Expected '[' for tuple");
  }
  
  uint32_t tup_node = alloc(2);
  uint32_t fst_loc = tup_node;
  uint32_t snd_loc = tup_node + 1;
  
  // Parse the first element
  parse_term(parser, fst_loc);
  
  if (!consume(parser, ",")) {
    parse_error(parser, "Expected ',' between elements in tuple");
  }
  
  // Parse the second element
  parse_term(parser, snd_loc);
  
  if (!consume(parser, "]")) {
    parse_error(parser, "Expected ']' after elements in tuple");
  }
  
  heap[loc] = make_term(TUP, 0, tup_node);
}

// Parse a sigma type elimination
void parse_get(Parser* parser, uint32_t loc) {
  if (!consume(parser, "![")) {
    parse_error(parser, "Expected '![' for sigma elimination");
  }
  
  // Parse the variable names
  char* name1 = parse_name(parser);
  
  if (!consume(parser, ",")) {
    parse_error(parser, "Expected ',' between names in sigma elimination");
  }
  
  char* name2 = parse_name(parser);
  
  if (!consume(parser, "]")) {
    parse_error(parser, "Expected ']' after names in sigma elimination");
  }
  
  if (!consume(parser, "=")) {
    parse_error(parser, "Expected '=' after names in sigma elimination");
  }
  
  uint32_t get_node = alloc(3);
  uint32_t val_loc = get_node; // Just store the value and body
  uint32_t bod_loc = get_node + 1;
  
  // Parse the pair being eliminated
  parse_term(parser, val_loc);
  
  if (!consume(parser, ";")) {
    parse_error(parser, "Expected ';' after pair in sigma elimination");
  }
  
  // Add variable bindings for the projections
  Term fst_var = make_term(VAR, 0, get_node + 2); // Store placeholder fst at get_node + 2
  Term snd_var = make_term(VAR, 0, get_node + 2); // Store placeholder snd at same location temporarily
  add_var_binding(parser, name1, fst_var);
  add_var_binding(parser, name2, snd_var);
  
  // Parse the body
  parse_term(parser, bod_loc);
  
  heap[loc] = make_term(GET, 0, get_node);
}

// Parse a pi type
void parse_all(Parser* parser, uint32_t loc) {
  // Check for UTF-8 encoding of Π (CE A0)
  if (check_utf8(parser, 0xCE, 0xA0)) {
    parser->pos += 2;
    parser->col++;
  } else if (!consume(parser, "Π")) {
    parse_error(parser, "Expected 'Π' for pi type");
  }
  
  // Parse the variable name
  char* name = parse_name(parser);
  
  if (!consume(parser, ":")) {
    parse_error(parser, "Expected ':' after name in pi type");
  }
  
  uint32_t all_node = alloc(2);
  uint32_t inp_loc = all_node;
  uint32_t out_loc = all_node + 1;
  
  // Parse the input type
  parse_term(parser, inp_loc);
  
  if (!consume(parser, ".")) {
    parse_error(parser, "Expected '.' after input type in pi type");
  }
  
  // Add variable binding for the output type
  Term var_term = make_term(VAR, 0, inp_loc);
  add_var_binding(parser, name, var_term);
  
  // Parse the output type
  parse_term(parser, out_loc);
  
  heap[loc] = make_term(ALL, 0, all_node);
}

// Parse a lambda
void parse_lam(Parser* parser, uint32_t loc) {
  // Check for UTF-8 encoding of λ (CE BB)
  if (check_utf8(parser, 0xCE, 0xBB)) {
    parser->pos += 2;
    parser->col++;
  } else if (!consume(parser, "λ")) {
    parse_error(parser, "Expected 'λ' for lambda");
  }
  
  // Parse the variable name
  char* name = parse_name(parser);
  
  if (!consume(parser, ".")) {
    parse_error(parser, "Expected '.' after name in lambda");
  }
  
  uint32_t lam_node = alloc(1);
  
  // Add variable binding pointing to the lambda's location
  Term var_term = make_term(VAR, 0, lam_node);
  add_var_binding(parser, name, var_term);
  
  // Parse the body
  parse_term(parser, lam_node);
  
  heap[loc] = make_term(LAM, 0, lam_node);
}

// Parse an application
void parse_app(Parser* parser, uint32_t loc) {  
  if (!consume(parser, "(")) {
    parse_error(parser, "Expected '(' for application");
  }
  
  uint32_t app_node = alloc(2);
  uint32_t fun_loc = app_node;
  uint32_t arg_loc = app_node + 1;
  
  // Parse the function
  parse_term(parser, fun_loc);
  
  // Check for whitespace between function and argument
  if (!isspace(parser->input[parser->pos])) {
    parse_error(parser, "Expected whitespace between function and argument in application");
  }
  parse_whitespace(parser);
  
  // Parse the argument
  parse_term(parser, arg_loc);
  
  if (!consume(parser, ")")) {
    parse_error(parser, "Expected ')' after application");
  }
  
  heap[loc] = make_term(APP, 0, app_node);
}

// Parse an equality type
void parse_eql(Parser* parser, uint32_t loc) {
  if (!consume(parser, "<")) {
    parse_error(parser, "Expected '<' for equality type");
  }
  
  uint32_t eql_node = alloc(2);
  uint32_t lft_loc = eql_node;
  uint32_t rgt_loc = eql_node + 1;
  
  // Parse the left term
  parse_term(parser, lft_loc);
  
  if (!consume(parser, "=")) {
    parse_error(parser, "Expected '=' between terms in equality type");
  }
  
  // Parse the right term
  parse_term(parser, rgt_loc);
  
  if (!consume(parser, ">")) {
    parse_error(parser, "Expected '>' after terms in equality type");
  }
  
  heap[loc] = make_term(EQL, 0, eql_node);
}

// Parse a reflexivity proof
void parse_rfl(Parser* parser, uint32_t loc) {
  // Check for UTF-8 encoding of θ (CE B8)
  if (check_utf8(parser, 0xCE, 0xB8)) {
    parser->pos += 2;
    parser->col++;
  } else if (!consume(parser, "θ")) {
    parse_error(parser, "Expected 'θ' for reflexivity");
  }
  
  heap[loc] = make_term(RFL, 0, 0);
}

// Parse an equality elimination
void parse_rwt(Parser* parser, uint32_t loc) {
  if (!consume(parser, "%")) {
    parse_error(parser, "Expected '%' for equality elimination");
  }
  
  uint32_t rwt_node = alloc(2);
  uint32_t eq_loc = rwt_node;
  uint32_t bod_loc = rwt_node + 1;
  
  // Parse the equality proof
  parse_term(parser, eq_loc);
  
  if (!consume(parser, ";")) {
    parse_error(parser, "Expected ';' after equality proof");
  }
  
  // Parse the body
  parse_term(parser, bod_loc);
  
  heap[loc] = make_term(RWT, 0, rwt_node);
}
