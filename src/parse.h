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
  
  // Uses and bindings as described in suptt.md
  VarUse lcs[MAX_USES];            // Array of unresolved variable uses
  size_t lcs_count;                // Number of uses
  
  VarBinding vrs[MAX_VARS];        // Map from names to variable terms
  size_t vrs_count;                // Number of bindings
} Parser;

// Initialize a parser with the given input string
void init_parser(Parser* parser, const char* input);

// Main parsing functions
Term parse_string(const char* input);
uint32_t parse_term_alloc(Parser* parser);
void parse_term(Parser* parser, uint32_t loc);

// Helper functions for parsing
void parse_whitespace(Parser* parser);
char peek_char(Parser* parser);
char next_char(Parser* parser);
bool consume(Parser* parser, const char* str);

// Function to check if the next character is a specific character
bool peek_is(Parser* parser, char c);

// Parse a uint from the input
uint32_t parse_uint(Parser* parser);

// Track a variable use to be resolved later
void add_var_use(Parser* parser, const char* name, uint32_t loc);

// Add a variable binding
void add_var_binding(Parser* parser, const char* name, Term term);

// Look up a variable binding by name
Term* lookup_var_binding(Parser* parser, const char* name);

// Resolve all variable uses after parsing
void resolve_var_uses(Parser* parser);

// Parse a name from the input
char* parse_name(Parser* parser);

// Error reporting
void parse_error(Parser* parser, const char* message);

// Individual term parsers
void parse_var(Parser* parser, uint32_t loc);
void parse_let(Parser* parser, uint32_t loc);
void parse_sup(Parser* parser, uint32_t loc);
void parse_col(Parser* parser, uint32_t loc);
void parse_set(Parser* parser, uint32_t loc);
void parse_emp(Parser* parser, uint32_t loc);
void parse_efq(Parser* parser, uint32_t loc);
void parse_uni(Parser* parser, uint32_t loc);
void parse_nil(Parser* parser, uint32_t loc);
void parse_use(Parser* parser, uint32_t loc);
void parse_bit(Parser* parser, uint32_t loc);
void parse_b_0(Parser* parser, uint32_t loc);
void parse_b_1(Parser* parser, uint32_t loc);
void parse_ite(Parser* parser, uint32_t loc);
void parse_sig(Parser* parser, uint32_t loc);
void parse_tup(Parser* parser, uint32_t loc);
void parse_get(Parser* parser, uint32_t loc);
void parse_all(Parser* parser, uint32_t loc);
void parse_lam(Parser* parser, uint32_t loc);
void parse_app(Parser* parser, uint32_t loc);
void parse_eql(Parser* parser, uint32_t loc);
void parse_rfl(Parser* parser, uint32_t loc);
void parse_rwt(Parser* parser, uint32_t loc);

#endif // PARSE_H