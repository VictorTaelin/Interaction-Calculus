// parse.c
#include "parse.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Forward declarations
Val parse_term_alloc(Parser* parser);
void parse_term(Parser* parser, Val loc);
void skip(Parser* parser);
char peek_char(Parser* parser);
char next_char(Parser* parser);
bool peek_is(Parser* parser, char c);
void parse_error(Parser* parser, const char* message);

// Helper functions
static bool starts_with_dollar(const char* name) {
  return name[0] == '$';
}

static size_t find_or_add_global_var(Parser* parser, const char* name) {
  for (size_t i = 0; i < parser->global_vars_count; i++) {
    if (strcmp(parser->global_vars[i].name, name) == 0) {
      return i;
    }
  }
  if (parser->global_vars_count >= MAX_GLOBAL_VARS) {
    parse_error(parser, "Too many global variables");
  }
  size_t idx = parser->global_vars_count++;
  Binder* binder = &parser->global_vars[idx];
  strncpy(binder->name, name, MAX_NAME_LEN - 1);
  binder->name[MAX_NAME_LEN - 1] = '\0';
  binder->var = NONE;
  binder->loc = NONE;
  return idx;
}

static void push_lexical_binder(Parser* parser, const char* name, Term term) {
  if (parser->lexical_vars_count >= MAX_LEXICAL_VARS) {
    parse_error(parser, "Too many lexical binders");
  }
  Binder* binder = &parser->lexical_vars[parser->lexical_vars_count];
  strncpy(binder->name, name, MAX_NAME_LEN - 1);
  binder->name[MAX_NAME_LEN - 1] = '\0';
  binder->var = term;
  binder->loc = NONE;
  parser->lexical_vars_count++;
}

static void pop_lexical_binder(Parser* parser) {
  if (parser->lexical_vars_count > 0) {
    parser->lexical_vars_count--;
  }
}

static Binder* find_lexical_binder(Parser* parser, const char* name) {
  for (int i = parser->lexical_vars_count - 1; i >= 0; i--) {
    if (strcmp(parser->lexical_vars[i].name, name) == 0) {
      return &parser->lexical_vars[i];
    }
  }
  return NULL;
}

static void resolve_global_vars(Parser* parser) {
  for (size_t i = 0; i < parser->global_vars_count; i++) {
    Binder* binder = &parser->global_vars[i];
    if (binder->var == NONE) {
      char error[256];
      snprintf(error, sizeof(error), "Undefined global variable: %s", binder->name);
      parse_error(parser, error);
    }
    if (binder->loc != NONE) {
      parser->ic->heap[binder->loc] = binder->var;
    }
  }
}

static void move_term(Parser* parser, Val from_loc, Val to_loc) {
  for (size_t i = 0; i < parser->global_vars_count; i++) {
    if (parser->global_vars[i].loc == from_loc) {
      parser->global_vars[i].loc = to_loc;
    }
  }
  for (size_t i = 0; i < parser->lexical_vars_count; i++) {
    if (parser->lexical_vars[i].loc == from_loc) {
      parser->lexical_vars[i].loc = to_loc;
    }
  }
  parser->ic->heap[to_loc] = parser->ic->heap[from_loc];
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
  fprintf(stderr, "    ");
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
  parser->global_vars_count = 0;
  parser->lexical_vars_count = 0;
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

void store_term(Parser* parser, Val loc, TermTag tag, Val value) {
  parser->ic->heap[loc] = ic_make_term(tag, value);
}

Val parse_uint(Parser* parser) {
  Val value = 0;
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
static void parse_term_var(Parser* parser, Val loc) {
  char name[MAX_NAME_LEN];
  parse_name(parser, name);
  if (starts_with_dollar(name)) {
    size_t idx = find_or_add_global_var(parser, name);
    if (parser->global_vars[idx].var == NONE) {
      parser->global_vars[idx].loc = loc;
    } else {
      parser->ic->heap[loc] = parser->global_vars[idx].var;
    }
  } else {
    Binder* binder = find_lexical_binder(parser, name);
    if (binder == NULL) {
      char error[256];
      snprintf(error, sizeof(error), "Undefined lexical variable: %s", name);
      parse_error(parser, error);
    }
    if (binder->loc == NONE) {
      parser->ic->heap[loc] = binder->var;
      binder->loc = loc;
    } else {
      Val dup_loc = ic_alloc(parser->ic, 1);
      parser->ic->heap[dup_loc] = parser->ic->heap[binder->loc];
      Term dp0 = ic_make_co0(0, dup_loc);
      Term dp1 = ic_make_co1(0, dup_loc);
      parser->ic->heap[binder->loc] = dp0;
      parser->ic->heap[loc] = dp1;
      binder->loc = loc;
    }
  }
}

static void parse_term_lam(Parser* parser, Val loc) {
  if (check_utf8(parser, 0xCE, 0xBB)) {
    consume_utf8(parser, 2);
  } else if (!consume(parser, "λ")) {
    parse_error(parser, "Expected 'λ' for lambda");
  }
  char name[MAX_NAME_LEN];
  parse_name(parser, name);
  expect(parser, ".", "after name in lambda");
  Val lam_node = ic_alloc(parser->ic, 1);
  Term var_term = ic_make_term(VAR, lam_node);
  if (starts_with_dollar(name)) {
    size_t idx = find_or_add_global_var(parser, name);
    if (parser->global_vars[idx].var != NONE) {
      char error[256];
      snprintf(error, sizeof(error), "Duplicate global variable binder: %s", name);
      parse_error(parser, error);
    }
    parser->global_vars[idx].var = var_term;
  } else {
    push_lexical_binder(parser, name, var_term);
  }
  parse_term(parser, lam_node);
  if (!starts_with_dollar(name)) {
    pop_lexical_binder(parser);
  }
  store_term(parser, loc, LAM, lam_node);
}

static void parse_term_app(Parser* parser, Val loc) {
  expect(parser, "(", "for application");
  parse_term(parser, loc);
  skip(parser);
  while (peek_char(parser) != ')') {
    Val app_node = ic_alloc(parser->ic, 2);
    move_term(parser, loc, app_node + 0);
    parse_term(parser, app_node + 1);
    store_term(parser, loc, APP, app_node);
    skip(parser);
  }
  expect(parser, ")", "after terms in application");
}

static void parse_term_sup(Parser* parser, Val loc) {
  expect(parser, "&", "for superposition");
  Lab label = parse_uint(parser) & 0x7;
  expect(parser, "{", "after label in superposition");
  Val sup_node = ic_alloc(parser->ic, 2);
  parse_term(parser, sup_node + 0);
  expect(parser, ",", "between terms in superposition");
  parse_term(parser, sup_node + 1);
  expect(parser, "}", "after terms in superposition");
  store_term(parser, loc, SUP_TAG(label), sup_node);
}

static void parse_term_dup(Parser* parser, Val loc) {
  expect(parser, "!&", "for duplication");
  Lab label = parse_uint(parser) & 0x7;
  expect(parser, "{", "after label in duplication");
  char x0[MAX_NAME_LEN];
  char x1[MAX_NAME_LEN];
  parse_name(parser, x0);
  expect(parser, ",", "between names in duplication");
  parse_name(parser, x1);
  expect(parser, "}", "after names in duplication");
  expect(parser, "=", "after names in duplication");
  Val dup_node = ic_alloc(parser->ic, 1);
  parse_term(parser, dup_node);
  expect(parser, ";", "after value in duplication");
  Term co0_term = ic_make_co0(label, dup_node);
  Term co1_term = ic_make_co1(label, dup_node);
  if (starts_with_dollar(x0)) {
    size_t idx = find_or_add_global_var(parser, x0);
    if (parser->global_vars[idx].var != NONE) {
      char error[256];
      snprintf(error, sizeof(error), "Duplicate global variable binder: %s", x0);
      parse_error(parser, error);
    }
    parser->global_vars[idx].var = co0_term;
  } else {
    push_lexical_binder(parser, x0, co0_term);
  }
  if (starts_with_dollar(x1)) {
    size_t idx = find_or_add_global_var(parser, x1);
    if (parser->global_vars[idx].var != NONE) {
      char error[256];
      snprintf(error, sizeof(error), "Duplicate global variable binder: %s", x1);
      parse_error(parser, error);
    }
    parser->global_vars[idx].var = co1_term;
  } else {
    push_lexical_binder(parser, x1, co1_term);
  }
  parse_term(parser, loc);
  if (!starts_with_dollar(x1)) {
    pop_lexical_binder(parser);
  }
  if (!starts_with_dollar(x0)) {
    pop_lexical_binder(parser);
  }
}

static void parse_term_era(Parser* parser, Val loc) {
  expect(parser, "*", "for erasure");
  store_term(parser, loc, ERA, 0);
}

static void parse_term_num(Parser* parser, Val loc) {
  Val value = parse_uint(parser);
  store_term(parser, loc, NUM, value);
}

static void parse_term_suc(Parser* parser, Val loc) {
  expect(parser, "+", "for successor");
  Val suc_node = ic_alloc(parser->ic, 1);
  parse_term(parser, suc_node);
  store_term(parser, loc, SUC, suc_node);
}

static void parse_term_swi(Parser* parser, Val loc) {
  expect(parser, "?", "for switch");
  Val swi_node = ic_alloc(parser->ic, 3);
  parse_term(parser, swi_node);
  expect(parser, "{", "after condition in switch");
  expect(parser, "0", "for zero case");
  expect(parser, ":", "after '0'");
  parse_term(parser, swi_node + 1);
  expect(parser, ";", "after zero case");
  expect(parser, "+", "for successor case");
  expect(parser, ":", "after '+'");
  parse_term(parser, swi_node + 2);
  expect(parser, ";", "after successor case");
  expect(parser, "}", "to close switch");
  store_term(parser, loc, SWI, swi_node);
}

static void parse_term_let(Parser* parser, Val loc) {
  expect(parser, "!", "for let expression");
  char name[MAX_NAME_LEN];
  parse_name(parser, name);
  expect(parser, "=", "after name in let expression");
  Val app_node = ic_alloc(parser->ic, 2);
  Val lam_node = ic_alloc(parser->ic, 1);
  parse_term(parser, app_node + 1);
  expect(parser, ";", "after value in let expression");
  Term var_term = ic_make_term(VAR, lam_node);
  if (starts_with_dollar(name)) {
    size_t idx = find_or_add_global_var(parser, name);
    if (parser->global_vars[idx].var != NONE) {
      char error[256];
      snprintf(error, sizeof(error), "Duplicate global variable binder: %s", name);
      parse_error(parser, error);
    }
    parser->global_vars[idx].var = var_term;
  } else {
    push_lexical_binder(parser, name, var_term);
  }
  parse_term(parser, lam_node);
  if (!starts_with_dollar(name)) {
    pop_lexical_binder(parser);
  }
  store_term(parser, app_node + 0, LAM, lam_node);
  store_term(parser, loc, APP, app_node);
}

void parse_term(Parser* parser, Val loc) {
  skip(parser);
  if (parser->input[parser->pos] == '\0') {
    parse_error(parser, "Unexpected end of input");
  }
  unsigned char c = (unsigned char)parser->input[parser->pos];
  if (isalpha(c) || c == '_' || c == '$') {
    parse_term_var(parser, loc);
  } else if (isdigit(c)) {
    parse_term_num(parser, loc);
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
  } else if (c == '+') {
    parse_term_suc(parser, loc);
  } else if (c == '?') {
    parse_term_swi(parser, loc);
  } else {
    char error_msg[100];
    snprintf(error_msg, sizeof(error_msg), "Unexpected character: %c (code: %d)", c, (int)c);
    parse_error(parser, error_msg);
  }
}

Val parse_term_alloc(Parser* parser) {
  Val loc = ic_alloc(parser->ic, 1);
  parse_term(parser, loc);
  return loc;
}

Term parse_string(IC* ic, const char* input) {
  Parser parser;
  init_parser(&parser, ic, input);
  skip(&parser);
  Val term_loc = parse_term_alloc(&parser);
  resolve_global_vars(&parser);
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
