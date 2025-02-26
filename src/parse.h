#ifndef PARSE_H
#define PARSE_H

#include "types.h"
#include <stdint.h>
#include <stdbool.h>

// Maximum name length for identifiers
#define MAX_NAME_LEN 64

// Maximum number of variable uses and bindings to track
#define MAX_USES 1024
#define MAX_VARS 1024

// Variable use structure to track unresolved variable uses
typedef struct {
  char name[MAX_NAME_LEN];  // Variable name
  uint32_t loc;             // Location in heap to update
} VarUse;

// Variable binding structure 
typedef struct {
  char name[MAX_NAME_LEN];  // Variable name
  Term term;                // Term representing the variable (VAR, CO0, CO1)
} VarBinding;

// Parser state structure
typedef struct {
  const char* input;  // Input string
  size_t pos;         // Current position
  size_t line;        // Current line number
  size_t col;         // Current column number
  
  // Uses and bindings for variable resolution
  VarUse lcs[MAX_USES];            // Array of unresolved variable uses
  size_t lcs_count;                // Number of uses
  
  VarBinding vrs[MAX_VARS];        // Map from names to variable terms
  size_t vrs_count;                // Number of bindings
} Parser;

// Initialize a parser with the given input string
void init_parser(Parser* parser, const char* input);

// Main parsing functions
Term parse_string(const char* input);
Term parse_file(const char* filename);
uint32_t parse_term_alloc(Parser* parser);
void parse_term(Parser* parser, uint32_t loc);

// Variable management
void add_var_use(Parser* parser, const char* name, uint32_t loc);
void bind_var(Parser* parser, const char* name, Term term);
Term* lookup_var_binding(Parser* parser, const char* name);
void resolve_var_uses(Parser* parser);

// Helper parsing functions
void skip(Parser* parser);
char peek_char(Parser* parser);
char next_char(Parser* parser);
bool peek_is(Parser* parser, char c);
bool consume(Parser* parser, const char* str);
bool expect(Parser* parser, const char* token, const char* error_context);
uint32_t parse_uint(Parser* parser);
char* parse_name(Parser* parser);
void parse_error(Parser* parser, const char* message);

// UTF-8 helpers
bool check_utf8(Parser* parser, uint8_t b1, uint8_t b2);
bool check_utf8_3bytes(Parser* parser, uint8_t b1, uint8_t b2, uint8_t b3);
bool check_utf8_4bytes(Parser* parser, uint8_t b1, uint8_t b2, uint8_t b3, uint8_t b4);
void consume_utf8(Parser* parser, int bytes);

// Term creation helpers
uint32_t alloc_term(uint32_t n);
void store_term(uint32_t loc, TermTag tag, uint8_t label, uint32_t value);

// Individual term parsers
void parse_term_var(Parser* parser, uint32_t loc);
void parse_term_let(Parser* parser, uint32_t loc);
void parse_term_sup(Parser* parser, uint32_t loc);
void parse_term_col(Parser* parser, uint32_t loc);
void parse_term_set(Parser* parser, uint32_t loc);
void parse_term_emp(Parser* parser, uint32_t loc);
void parse_term_efq(Parser* parser, uint32_t loc);
void parse_term_uni(Parser* parser, uint32_t loc);
void parse_term_nil(Parser* parser, uint32_t loc);
void parse_term_use(Parser* parser, uint32_t loc);
void parse_term_bit(Parser* parser, uint32_t loc);
void parse_term_bt0(Parser* parser, uint32_t loc);
void parse_term_bt1(Parser* parser, uint32_t loc);
void parse_term_ite(Parser* parser, uint32_t loc);
void parse_term_sig(Parser* parser, uint32_t loc);
void parse_term_tup(Parser* parser, uint32_t loc);
void parse_term_get(Parser* parser, uint32_t loc);
void parse_term_all(Parser* parser, uint32_t loc);
void parse_term_lam(Parser* parser, uint32_t loc);
void parse_term_app(Parser* parser, uint32_t loc);
void parse_term_eql(Parser* parser, uint32_t loc);
void parse_term_rfl(Parser* parser, uint32_t loc);
void parse_term_rwt(Parser* parser, uint32_t loc);

#endif // PARSE_H