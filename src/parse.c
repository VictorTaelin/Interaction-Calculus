#include "parse.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Forward declarations for internal functions
uint32_t parse_term_alloc(Parser* parser);
void parse_term(Parser* parser, uint32_t loc);
void skip(Parser* parser);
char peek_char(Parser* parser);
char next_char(Parser* parser);
bool peek_is(Parser* parser, char c);
void parse_error(Parser* parser, const char* message);

// Helper functions for scoping
static bool starts_with_dollar(const char* name) {
  return name[0] == '$';
}

static void add_global_occur(Parser* parser, const char* name, uint32_t loc) {
  if (parser->global_occurs_count >= MAX_GLOBAL_OCCURS) {
    parse_error(parser, "Too many global variable occurrences");
  }
  strncpy(parser->global_occurs[parser->global_occurs_count].name, name, MAX_NAME_LEN - 1);
  parser->global_occurs[parser->global_occurs_count].name[MAX_NAME_LEN - 1] = '\0';
  parser->global_occurs[parser->global_occurs_count].loc = loc;
  parser->global_occurs_count++;
}

static void add_global_binder(Parser* parser, const char* name, Term term) {
  for (size_t i = 0; i < parser->global_binders_count; i++) {
    if (strcmp(parser->global_binders[i].name, name) == 0) {
      char error[256];
      snprintf(error, sizeof(error), "Duplicate global variable binder: %s", name);
      parse_error(parser, error);
    }
  }
  if (parser->global_binders_count >= MAX_GLOBAL_BINDERS) {
    parse_error(parser, "Too many global variable binders");
  }
  strncpy(parser->global_binders[parser->global_binders_count].name, name, MAX_NAME_LEN - 1);
  parser->global_binders[parser->global_binders_count].name[MAX_NAME_LEN - 1] = '\0';
  parser->global_binders[parser->global_binders_count].term = term;
  parser->global_binders_count++;
}

static void push_lexical_binder(Parser* parser, const char* name, Term term) {
  if (parser->lexical_binders_count >= MAX_LEXICAL_BINDERS) {
    parse_error(parser, "Too many lexical binders");
  }
  strncpy(parser->lexical_binders[parser->lexical_binders_count].name, name, MAX_NAME_LEN - 1);
  parser->lexical_binders[parser->lexical_binders_count].name[MAX_NAME_LEN - 1] = '\0';
  parser->lexical_binders[parser->lexical_binders_count].term = term;
  parser->lexical_binders_count++;
}

static void pop_lexical_binder(Parser* parser) {
  if (parser->lexical_binders_count > 0) {
    parser->lexical_binders_count--;
  }
}

static Term* find_lexical_binding(Parser* parser, const char* name) {
  for (int i = parser->lexical_binders_count - 1; i >= 0; i--) {
    if (strcmp(parser->lexical_binders[i].name, name) == 0) {
      return &parser->lexical_binders[i].term;
    }
  }
  return NULL;
}

static void resolve_global_occurs(Parser* parser) {
  for (size_t i = 0; i < parser->global_occurs_count; i++) {
    const char* name = parser->global_occurs[i].name;
    Term* binding = NULL;
    for (size_t j = 0; j < parser->global_binders_count; j++) {
      if (strcmp(parser->global_binders[j].name, name) == 0) {
        binding = &parser->global_binders[j].term;
        break;
      }
    }
    if (binding == NULL) {
      char error[256];
      snprintf(error, sizeof(error), "Undefined global variable: %s", name);
      parse_error(parser, error);
    }
    parser->ic->heap[parser->global_occurs[i].loc] = *binding;
  }
}

// Parse helper functions
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

void parse_error(Parser* parser, const char* message) {
  fprintf(stderr, "Parse error at line %zu, column %zu: %s\n", 
          parser->line, parser->col, message);
  fprintf(stderr, "Input:\n%s\n", parser->input);
  fprintf(stderr, "        ");
  for (size_t i = 0; i < parser->pos && i < 40; i++) {
    fprintf(stderr, " ");
  }
  fprintf(stderr, "^\n");
  exit(1);
}

bool expect(Parser* parser, const char* token, const char* error_context) {
  if (!consume(parser, token)) {
    char error[256];
    snprintf(error, sizeof(error), "Expected '%s' %s", token, error_context);
    parse_error(parser, error);
    return false;
  }
  return true;
}

void init_parser(Parser* parser, IC* ic, const char* input) {
  parser->ic = ic;
  parser->input = input;
  parser->pos = 0;
  parser->line = 1;
  parser->col = 1;
  parser->global_occurs_count = 0;
  parser->global_binders_count = 0;
  parser->lexical_binders_count = 0;
}

static void parse_name(Parser* parser, char* name) {
  size_t i = 0;
  char c = peek_char(parser);
  if (!isalpha(c) && c != '_' && c != '$') {
    parse_error(parser, "Expected name starting with letter, underscore, or '$'");
  }
  while (isalnum(peek_char(parser)) || peek_char(parser) == '_' || peek_char(parser) == '$') {
    if (i < MAX_NAME_LEN - 1) {
      name[i++] = next_char(parser);
    } else {
      parse_error(parser, "Name too long");
    }
  }
  name[i] = '\0';
}

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

char peek_char(Parser* parser) {
  return parser->input[parser->pos];
}

bool peek_is(Parser* parser, char c) {
  return peek_char(parser) == c;
}

void store_term(Parser* parser, uint32_t loc, TermTag tag, uint32_t value) {
  parser->ic->heap[loc] = ic_make_term(tag, value);
}

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

void skip(Parser* parser) {
  while (1) {
    char c = peek_char(parser);
    if (isspace(c)) {
      next_char(parser);
    } else if (c == '/' && parser->input[parser->pos + 1] == '/') {
      next_char(parser);
      next_char(parser);
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

bool check_utf8(Parser* parser, uint8_t b1, uint8_t b2) {
  return (unsigned char)parser->input[parser->pos] == b1 &&
         (unsigned char)parser->input[parser->pos + 1] == b2;
}

void consume_utf8(Parser* parser, int bytes) {
  for (int i = 0; i < bytes; i++) {
    next_char(parser);
  }
}

// Term parsing functions
static void parse_term_var(Parser* parser, uint32_t loc) {
  char name[MAX_NAME_LEN];
  parse_name(parser, name);
  if (starts_with_dollar(name)) {
    add_global_occur(parser, name, loc);
  } else {
    Term* binding = find_lexical_binding(parser, name);
    if (binding == NULL) {
      char error[256];
      snprintf(error, sizeof(error), "Undefined lexical variable: %s", name);
      parse_error(parser, error);
    }
    parser->ic->heap[loc] = *binding;
  }
}

static void parse_term_lam(Parser* parser, uint32_t loc) {
  if (check_utf8(parser, 0xCE, 0xBB)) {
    consume_utf8(parser, 2);
  } else if (!consume(parser, "λ")) {
    parse_error(parser, "Expected 'λ' for lambda");
  }
  char name[MAX_NAME_LEN];
  parse_name(parser, name);
  expect(parser, ".", "after name in lambda");
  uint32_t lam_node = ic_alloc(parser->ic, 1);
  Term var_term = ic_make_term(VAR, lam_node);
  bool is_lexical = !starts_with_dollar(name);
  if (!is_lexical) {
    add_global_binder(parser, name, var_term);
  } else {
    push_lexical_binder(parser, name, var_term);
  }
  parse_term(parser, lam_node);
  if (is_lexical) {
    pop_lexical_binder(parser);
  }
  store_term(parser, loc, LAM, lam_node);
}

static void parse_term_app(Parser* parser, uint32_t loc) {
  expect(parser, "(", "for application");
  uint32_t current_loc = ic_alloc(parser->ic, 1);
  parse_term(parser, current_loc);
  skip(parser);
  while (peek_char(parser) != ')') {
    uint32_t arg_loc = ic_alloc(parser->ic, 1);
    parse_term(parser, arg_loc);
    uint32_t app_node = ic_alloc(parser->ic, 2);
    parser->ic->heap[app_node] = parser->ic->heap[current_loc];
    parser->ic->heap[app_node + 1] = parser->ic->heap[arg_loc];
    uint32_t new_loc = ic_alloc(parser->ic, 1);
    store_term(parser, new_loc, APP, app_node);
    current_loc = new_loc;
    skip(parser);
  }
  parser->ic->heap[loc] = parser->ic->heap[current_loc];
  expect(parser, ")", "after terms in application");
}

static void parse_term_sup(Parser* parser, uint32_t loc) {
  expect(parser, "&", "for superposition");
  uint8_t label = parse_uint(parser) & 0x3;
  expect(parser, "{", "after label in superposition");
  uint32_t sup_node = ic_alloc(parser->ic, 2);
  uint32_t lft_loc = sup_node;
  uint32_t rgt_loc = sup_node + 1;
  parse_term(parser, lft_loc);
  expect(parser, ",", "between terms in superposition");
  parse_term(parser, rgt_loc);
  expect(parser, "}", "after terms in superposition");
  store_term(parser, loc, SUP_TAG(label), sup_node);
}

static void parse_term_dup(Parser* parser, uint32_t loc) {
  expect(parser, "!&", "for duplication");
  uint8_t label = parse_uint(parser) & 0x3;
  expect(parser, "{", "after label in duplication");
  char x0[MAX_NAME_LEN];
  char x1[MAX_NAME_LEN];
  parse_name(parser, x0);
  expect(parser, ",", "between names in duplication");
  parse_name(parser, x1);
  expect(parser, "}", "after names in duplication");
  expect(parser, "=", "after names in duplication");
  uint32_t dup_node = ic_alloc(parser->ic, 1);
  parse_term(parser, dup_node);
  expect(parser, ";", "after value in duplication");
  Term co0_term = ic_make_co0(label, dup_node);
  Term co1_term = ic_make_co1(label, dup_node);
  bool x0_lexical = !starts_with_dollar(x0);
  bool x1_lexical = !starts_with_dollar(x1);
  if (x0_lexical) {
    push_lexical_binder(parser, x0, co0_term);
  } else {
    add_global_binder(parser, x0, co0_term);
  }
  if (x1_lexical) {
    push_lexical_binder(parser, x1, co1_term);
  } else {
    add_global_binder(parser, x1, co1_term);
  }
  parse_term(parser, loc);
  if (x1_lexical) {
    pop_lexical_binder(parser);
  }
  if (x0_lexical) {
    pop_lexical_binder(parser);
  }
}

static void parse_term_era(Parser* parser, uint32_t loc) {
  expect(parser, "*", "for erasure");
  store_term(parser, loc, ERA, 0);
}

static void parse_term_let(Parser* parser, uint32_t loc) {
  expect(parser, "!", "for let expression");
  char name[MAX_NAME_LEN];
  parse_name(parser, name);
  expect(parser, "=", "after name in let expression");
  uint32_t app_node = ic_alloc(parser->ic, 2);
  uint32_t lam_node = ic_alloc(parser->ic, 1);
  uint32_t fun_loc = app_node;
  uint32_t arg_loc = app_node + 1;
  parse_term(parser, arg_loc);
  expect(parser, ";", "after value in let expression");
  Term var_term = ic_make_term(VAR, lam_node);
  bool is_lexical = !starts_with_dollar(name);
  if (!is_lexical) {
    add_global_binder(parser, name, var_term);
  } else {
    push_lexical_binder(parser, name, var_term);
  }
  parse_term(parser, lam_node);
  if (is_lexical) {
    pop_lexical_binder(parser);
  }
  store_term(parser, fun_loc, LAM, lam_node);
  store_term(parser, loc, APP, app_node);
}

void parse_term(Parser* parser, uint32_t loc) {
  skip(parser);
  if (parser->input[parser->pos] == '\0') {
    parse_error(parser, "Unexpected end of input");
  }
  unsigned char c = (unsigned char)parser->input[parser->pos];
  if (isalpha(c) || c == '_' || c == '$') {
    parse_term_var(parser, loc);
  } else if (c == '!') {
    parser->pos++;
    char next = peek_char(parser);
    parser->pos--;
    if (next == '&') {
      parse_term_dup(parser, loc);
    } else if (isalpha(next) || next == '_' || next == '$') {
      parse_term_let(parser, loc);
    } else {
      parse_error(parser, "Expected '&' or name after '!' for duplication or let");
    }
  } else if (c == '&') {
    parse_term_sup(parser, loc);
  } else if (c == 0xCE && (unsigned char)parser->input[parser->pos + 1] == 0xBB) {
    parse_term_lam(parser, loc);
  } else if (c == '(') {
    parse_term_app(parser, loc);
  } else if (c == '*') {
    parse_term_era(parser, loc);
  } else {
    char error_msg[100];
    snprintf(error_msg, sizeof(error_msg), "Unexpected character: %c (code: %d)", c, (int)c);
    parse_error(parser, error_msg);
  }
}

// Public parsing functions
uint32_t parse_term_alloc(Parser* parser) {
  uint32_t loc = ic_alloc(parser->ic, 1);
  parse_term(parser, loc);
  return loc;
}

Term parse_string(IC* ic, const char* input) {
  Parser parser;
  init_parser(&parser, ic, input);
  skip(&parser);
  uint32_t term_loc = parse_term_alloc(&parser);
  resolve_global_occurs(&parser);
  return parser.ic->heap[term_loc];
}

Term parse_file(IC* ic, const char* filename) {
  FILE* file = fopen(filename, "r");
  if (!file) {
    fprintf(stderr, "Error: Could not open file '%s'\n", filename);
    exit(1);
  }
  fseek(file, 0, SEEK_END);
  long size = ftell(file);
  fseek(file, 0, SEEK_SET);
  char* buffer = (char*)malloc(size + 1);
  if (!buffer) {
    fprintf(stderr, "Error: Memory allocation failed\n");
    fclose(file);
    exit(1);
  }
  size_t read_size = fread(buffer, 1, size, file);
  fclose(file);
  buffer[read_size] = '\0';
  Term term = parse_string(ic, buffer);
  free(buffer);
  return term;
}
