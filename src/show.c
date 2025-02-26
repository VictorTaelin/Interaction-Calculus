//./../suptt.md//
//./memory.h//
//./types.h//
//./parse.c//
//./parse/term.c//
//./parse/term/col.c//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "types.h"
#include "memory.h"
#include "show.h"

// Maximum string length for term representation
#define MAX_STR_LEN 65536

// Structure to track variable names
typedef struct {
  uint32_t count;        // Number of variables encountered
  uint32_t* locations;   // Array of variable locations
  TermTag* types;        // Array of variable types (VAR, CO0, CO1)
  char** names;          // Array of variable names
  uint32_t capacity;     // Capacity of the arrays
} VarNameTable;

// Structure to track collapser nodes
typedef struct {
  uint32_t* locations;   // Array of collapser locations
  uint32_t count;        // Number of collapsers
  uint32_t capacity;     // Capacity of the array
} ColTable;

// Initialize variable name table
void init_var_table(VarNameTable* table) {
  table->count = 0;
  table->capacity = 64;
  table->locations = (uint32_t*)malloc(table->capacity * sizeof(uint32_t));
  table->types = (TermTag*)malloc(table->capacity * sizeof(TermTag));
  table->names = (char**)malloc(table->capacity * sizeof(char*));
}

// Free variable name table
void free_var_table(VarNameTable* table) {
  for (uint32_t i = 0; i < table->count; i++) {
    free(table->names[i]);
  }
  free(table->locations);
  free(table->types);
  free(table->names);
}

// Initialize collapser table
void init_col_table(ColTable* table) {
  table->count = 0;
  table->capacity = 64;
  table->locations = (uint32_t*)malloc(table->capacity * sizeof(uint32_t));
}

// Free collapser table
void free_col_table(ColTable* table) {
  free(table->locations);
}

// Add a variable to the table and return its name
char* add_variable(VarNameTable* table, uint32_t location, TermTag type) {
  // Check if we need to expand the table
  if (table->count >= table->capacity) {
    table->capacity *= 2;
    table->locations = (uint32_t*)realloc(table->locations, table->capacity * sizeof(uint32_t));
    table->types = (TermTag*)realloc(table->types, table->capacity * sizeof(TermTag));
    table->names = (char**)realloc(table->names, table->capacity * sizeof(char*));
  }
  
  // Check if the variable is already in the table
  for (uint32_t i = 0; i < table->count; i++) {
    if (table->locations[i] == location && table->types[i] == type) {
      return table->names[i];
    }
  }
  
  // Add the new variable
  table->locations[table->count] = location;
  table->types[table->count] = type;
  
  // Generate a name for the variable based on its type
  char* name = (char*)malloc(16);
  if (type == CO0) {
    sprintf(name, "a%u", table->count);
  } else if (type == CO1) {
    sprintf(name, "b%u", table->count);
  } else {
    sprintf(name, "x%u", table->count);
  }
  
  table->names[table->count] = name;
  table->count++;
  return name;
}

// Get a variable name from the table
char* get_var_name(VarNameTable* table, uint32_t location, TermTag type) {
  for (uint32_t i = 0; i < table->count; i++) {
    if (table->locations[i] == location && table->types[i] == type) {
      return table->names[i];
    }
  }
  return "?"; // Unknown variable
}

// Forward declarations
void assign_var_ids(Term term, VarNameTable* var_table, ColTable* col_table);
void stringify_term(Term term, VarNameTable* var_table, char* buffer, int* pos, int max_len);
void stringify_collapsers(ColTable* col_table, VarNameTable* var_table, char* buffer, int* pos, int max_len);

// Register a collapser in the table
bool register_collapser(ColTable* table, uint32_t location) {
  // Check if the collapser is already in the table
  for (uint32_t i = 0; i < table->count; i++) {
    if (table->locations[i] == location) {
      return false; // Already registered
    }
  }
  
  // Check if we need to expand the table
  if (table->count >= table->capacity) {
    table->capacity *= 2;
    table->locations = (uint32_t*)realloc(table->locations, table->capacity * sizeof(uint32_t));
  }
  
  // Add the new collapser
  table->locations[table->count++] = location;
  return true; // Newly registered
}

// Assign IDs to variables and register collapsers
void assign_var_ids(Term term, VarNameTable* var_table, ColTable* col_table) {
  // Follow substitutions
  if (TERM_SUB(term)) {
    return; // No need to process substitutions
  }
  
  TermTag tag = TERM_TAG(term);
  uint32_t val = TERM_VAL(term);
  
  switch (tag) {
    case VAR:
      // Nothing specific to do for regular variables
      break;
    
    case CO0:
    case CO1: {
      // Register the collapser and process its value if newly added
      if (register_collapser(col_table, val)) {
        Term val_term = heap[val];
        assign_var_ids(val_term, var_table, col_table);
      }
      break;
    }
      
    case LAM: {
      // Lambda has a body
      uint32_t lam_loc = val;
      add_variable(var_table, lam_loc, VAR);
      assign_var_ids(heap[lam_loc], var_table, col_table);
      break;
    }
    
    case LET: {
      // Let has a value and a body
      uint32_t let_loc = val;
      add_variable(var_table, let_loc, VAR);
      assign_var_ids(heap[let_loc], var_table, col_table);
      assign_var_ids(heap[let_loc + 1], var_table, col_table);
      break;
    }
    
    case APP: {
      // Application has function and argument
      uint32_t app_loc = val;
      assign_var_ids(heap[app_loc], var_table, col_table);
      assign_var_ids(heap[app_loc + 1], var_table, col_table);
      break;
    }
    
    case SUP: {
      // Superposition has left and right
      uint32_t sup_loc = val;
      assign_var_ids(heap[sup_loc], var_table, col_table);
      assign_var_ids(heap[sup_loc + 1], var_table, col_table);
      break;
    }
    
    case EFQ: {
      // EFQ has a value
      uint32_t efq_loc = val;
      assign_var_ids(heap[efq_loc], var_table, col_table);
      break;
    }
    
    case USE: {
      // USE has a value and a body
      uint32_t use_loc = val;
      assign_var_ids(heap[use_loc], var_table, col_table);
      assign_var_ids(heap[use_loc + 1], var_table, col_table);
      break;
    }
    
    case ITE: {
      // ITE has condition, then, else
      uint32_t ite_loc = val;
      assign_var_ids(heap[ite_loc], var_table, col_table);
      assign_var_ids(heap[ite_loc + 1], var_table, col_table);
      assign_var_ids(heap[ite_loc + 2], var_table, col_table);
      break;
    }
    
    case SIG:
    case TUP: {
      // Sig and Tup have first and second
      uint32_t pair_loc = val;
      assign_var_ids(heap[pair_loc], var_table, col_table);
      assign_var_ids(heap[pair_loc + 1], var_table, col_table);
      break;
    }
    
    case GET: {
      // GET has value and body
      uint32_t get_loc = val;
      add_variable(var_table, get_loc, VAR);    // First var
      add_variable(var_table, get_loc + 1, VAR); // Second var
      assign_var_ids(heap[get_loc + 2], var_table, col_table);
      assign_var_ids(heap[get_loc + 3], var_table, col_table);
      break;
    }
    
    case ALL: {
      // ALL has input and output
      uint32_t all_loc = val;
      add_variable(var_table, all_loc, VAR);
      assign_var_ids(heap[all_loc], var_table, col_table);
      assign_var_ids(heap[all_loc + 1], var_table, col_table);
      break;
    }
    
    case EQL: {
      // EQL has left and right
      uint32_t eql_loc = val;
      assign_var_ids(heap[eql_loc], var_table, col_table);
      assign_var_ids(heap[eql_loc + 1], var_table, col_table);
      break;
    }
    
    case RWT: {
      // RWT has value and body
      uint32_t rwt_loc = val;
      assign_var_ids(heap[rwt_loc], var_table, col_table);
      assign_var_ids(heap[rwt_loc + 1], var_table, col_table);
      break;
    }
    
    // No need to process leaf nodes (SET, EMP, UNI, NIL, BIT, BT0, BT1, RFL)
    default:
      break;
  }
}

// Stringify collapsers
void stringify_collapsers(ColTable* col_table, VarNameTable* var_table, char* buffer, int* pos, int max_len) {
  for (uint32_t i = 0; i < col_table->count; i++) {
    uint32_t col_loc = col_table->locations[i];
    Term val_term = heap[col_loc];
    uint8_t lab = TERM_LAB(val_term);
    
    // Generate variable names for the collapser
    char* var0 = add_variable(var_table, col_loc, CO0);
    char* var1 = add_variable(var_table, col_loc, CO1);
    
    // Add collapser header
    *pos += snprintf(buffer + *pos, max_len - *pos, "! &%u{%s,%s} = ", lab, var0, var1);
    
    // Add the value
    stringify_term(val_term, var_table, buffer, pos, max_len);
    
    // Add separator
    *pos += snprintf(buffer + *pos, max_len - *pos, ";\n");
  }
}

// Stringify a term
void stringify_term(Term term, VarNameTable* var_table, char* buffer, int* pos, int max_len) {
  // Check for substitution
  if (TERM_SUB(term)) {
    term = heap[TERM_VAL(term)]; // Follow the substitution
  }
  
  TermTag tag = TERM_TAG(term);
  uint32_t val = TERM_VAL(term);
  uint8_t lab = TERM_LAB(term);
  
  switch (tag) {
    case VAR:
      *pos += snprintf(buffer + *pos, max_len - *pos, "%s", get_var_name(var_table, val, VAR));
      break;
      
    case CO0:
      *pos += snprintf(buffer + *pos, max_len - *pos, "%s", get_var_name(var_table, val, CO0));
      break;
      
    case CO1:
      *pos += snprintf(buffer + *pos, max_len - *pos, "%s", get_var_name(var_table, val, CO1));
      break;
      
    case LAM:
      *pos += snprintf(buffer + *pos, max_len - *pos, "λ%s.", get_var_name(var_table, val, VAR));
      stringify_term(heap[val], var_table, buffer, pos, max_len);
      break;
      
    case LET:
      *pos += snprintf(buffer + *pos, max_len - *pos, "!%s = ", get_var_name(var_table, val, VAR));
      stringify_term(heap[val], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, "; ");
      stringify_term(heap[val + 1], var_table, buffer, pos, max_len);
      break;
      
    case APP:
      *pos += snprintf(buffer + *pos, max_len - *pos, "(");
      stringify_term(heap[val], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, " ");
      stringify_term(heap[val + 1], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, ")");
      break;
      
    case SUP:
      *pos += snprintf(buffer + *pos, max_len - *pos, "&%u{", lab);
      stringify_term(heap[val], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, ",");
      stringify_term(heap[val + 1], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, "}");
      break;
      
    case SET:
      *pos += snprintf(buffer + *pos, max_len - *pos, "*");
      break;
      
    case EMP:
      *pos += snprintf(buffer + *pos, max_len - *pos, "⊥");
      break;
      
    case EFQ:
      *pos += snprintf(buffer + *pos, max_len - *pos, "¬");
      stringify_term(heap[val], var_table, buffer, pos, max_len);
      break;
      
    case UNI:
      *pos += snprintf(buffer + *pos, max_len - *pos, "⊤");
      break;
      
    case NIL:
      *pos += snprintf(buffer + *pos, max_len - *pos, "()");
      break;
      
    case USE:
      *pos += snprintf(buffer + *pos, max_len - *pos, "-");
      stringify_term(heap[val], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, "; ");
      stringify_term(heap[val + 1], var_table, buffer, pos, max_len);
      break;
      
    case BIT:
      *pos += snprintf(buffer + *pos, max_len - *pos, "𝔹");
      break;
      
    case BT0:
      *pos += snprintf(buffer + *pos, max_len - *pos, "0");
      break;
      
    case BT1:
      *pos += snprintf(buffer + *pos, max_len - *pos, "1");
      break;
      
    case ITE:
      *pos += snprintf(buffer + *pos, max_len - *pos, "?");
      stringify_term(heap[val], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, "{");
      stringify_term(heap[val + 1], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, "}; {");
      stringify_term(heap[val + 2], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, "}");
      break;
      
    case SIG:
      *pos += snprintf(buffer + *pos, max_len - *pos, "Σ%s:", get_var_name(var_table, val, VAR));
      stringify_term(heap[val], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, ".");
      stringify_term(heap[val + 1], var_table, buffer, pos, max_len);
      break;
      
    case TUP:
      *pos += snprintf(buffer + *pos, max_len - *pos, "[");
      stringify_term(heap[val], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, ",");
      stringify_term(heap[val + 1], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, "]");
      break;
      
    case GET:
      *pos += snprintf(buffer + *pos, max_len - *pos, "![%s,%s] = ", 
              get_var_name(var_table, val, VAR), 
              get_var_name(var_table, val + 1, VAR));
      stringify_term(heap[val + 2], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, "; ");
      stringify_term(heap[val + 3], var_table, buffer, pos, max_len);
      break;
      
    case ALL:
      *pos += snprintf(buffer + *pos, max_len - *pos, "Π%s:", get_var_name(var_table, val, VAR));
      stringify_term(heap[val], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, ".");
      stringify_term(heap[val + 1], var_table, buffer, pos, max_len);
      break;
      
    case EQL:
      *pos += snprintf(buffer + *pos, max_len - *pos, "<");
      stringify_term(heap[val], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, "=");
      stringify_term(heap[val + 1], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, ">");
      break;
      
    case RFL:
      *pos += snprintf(buffer + *pos, max_len - *pos, "θ");
      break;
      
    case RWT:
      *pos += snprintf(buffer + *pos, max_len - *pos, "%%");
      stringify_term(heap[val], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, "; ");
      stringify_term(heap[val + 1], var_table, buffer, pos, max_len);
      break;
      
    default:
      *pos += snprintf(buffer + *pos, max_len - *pos, "<?unknown term>");
      break;
  }
}

// Main function to convert a term to string
char* term_to_string(Term term) {
  // Initialize tables
  VarNameTable var_table;
  ColTable col_table;
  init_var_table(&var_table);
  init_col_table(&col_table);
  
  // Assign IDs to variables and register collapsers
  assign_var_ids(term, &var_table, &col_table);
  
  // Allocate buffer for the string representation
  char* buffer = (char*)malloc(MAX_STR_LEN);
  int pos = 0;
  
  // First stringify all collapsers
  stringify_collapsers(&col_table, &var_table, buffer, &pos, MAX_STR_LEN);
  
  // Then stringify the main term
  stringify_term(term, &var_table, buffer, &pos, MAX_STR_LEN);
  
  // Free tables
  free_var_table(&var_table);
  free_col_table(&col_table);
  
  return buffer;
}

// Display a term to the specified output stream
void show_term(FILE* stream, Term term) {
  char* str = term_to_string(term);
  fprintf(stream, "%s", str);
  free(str);
}


for some reason, the file above isn't working properly. consider:

λf.
  !&0{f00x,f00y} = f;
  !&0{f01x,f01y} = λk01.(f00x (f00y k01));
  λk.(f01x (f01y k));

when parsing and stringifying it, we get:

! &0{a3,b4} = λx2.(? (? x2));
! &0{a5,b6} = x0;
λx0.λx1.(a3 (b4 x1))














