#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "ic.h"
#include "show.h"

// For backward compatibility with the showing code
#define DP0 100  // Just a value not used for any other tag
#define DP1 101  // Just a value not used for any other tag

// Helper functions for numeric operations
static uint32_t get_num_val(Term term) {
  if (TERM_TAG(term) == NUM) {
    return TERM_VAL(term);
  } else {
    return 0; // Default to 0 if not a number
  }
}

// Maximum string length for term representation
#define MAX_STR_LEN 65536

// Structure to track variable names
typedef struct {
  uint32_t count;        // Number of variables encountered
  uint32_t* locations;   // Array of variable locations
  TermTag* types;        // Array of variable types (VAR, DP0, DP1)
  char** names;          // Array of variable names
  uint32_t capacity;     // Capacity of the arrays
} VarNameTable;

// Structure to track duplication nodes
typedef struct {
  uint32_t* locations;   // Array of duplication locations
  uint8_t* labels;       // Array of duplication labels
  uint32_t count;        // Number of duplications
  uint32_t capacity;     // Capacity of the array
} DupTable;

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

// Initialize duplication table
void init_dup_table(DupTable* table) {
  table->count = 0;
  table->capacity = 64;
  table->locations = (uint32_t*)malloc(table->capacity * sizeof(uint32_t));
  table->labels = (uint8_t*)malloc(table->capacity * sizeof(uint8_t));
}

// Free duplication table
void free_dup_table(DupTable* table) {
  free(table->locations);
  free(table->labels);
}

// Convert an index to an alphabetic variable name (a, b, c, ..., z, aa, ab, ...)
char* index_to_var_name(uint32_t index) {
  char* name = (char*)malloc(16);
  if (index < 26) {
    // a-z
    sprintf(name, "%c", 'a' + index);
  } else {
    // aa, ab, ac, ...
    uint32_t first = (index - 26) / 26;
    uint32_t second = (index - 26) % 26;
    sprintf(name, "%c%c", 'a' + first, 'a' + second);
  }
  return name;
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

  // For compatibility, we only store the basic types (VAR, DP0, DP1) in the table
  TermTag basicType = type;
  if (IS_DP0(type)) {
    basicType = DP0;
  } else if (IS_DP1(type)) {
    basicType = DP1;
  }

  // Check if the variable is already in the table
  for (uint32_t i = 0; i < table->count; i++) {
    if (table->locations[i] == location && table->types[i] == basicType) {
      return table->names[i];
    }
  }

  // Add the new variable
  table->locations[table->count] = location;
  table->types[table->count] = basicType;

  // Generate a name for the variable based on its type
  char* name;
  if (basicType == VAR) {
    name = index_to_var_name(table->count);
  } else if (basicType == DP0) {
    name = (char*)malloc(16);
    sprintf(name, "a%u", table->count);
  } else if (basicType == DP1) {
    name = (char*)malloc(16);
    sprintf(name, "b%u", table->count);
  }

  table->names[table->count] = name;
  table->count++;
  return name;
}

// Get a variable name from the table
char* get_var_name(VarNameTable* table, uint32_t location, TermTag type) {
  // Convert to basic type for lookup
  TermTag basicType = type;
  if (IS_DP0(type)) {
    basicType = DP0;
  } else if (IS_DP1(type)) {
    basicType = DP1;
  }

  for (uint32_t i = 0; i < table->count; i++) {
    if (table->locations[i] == location && table->types[i] == basicType) {
      return table->names[i];
    }
  }
  return "?"; // Unknown variable
}

// Forward declarations
void assign_var_ids(IC* ic, Term term, VarNameTable* var_table, DupTable* dup_table);
void stringify_term(IC* ic, Term term, VarNameTable* var_table, char* buffer, int* pos, int max_len, const char* prefix);
void stringify_duplications(IC* ic, DupTable* dup_table, VarNameTable* var_table, char* buffer, int* pos, int max_len, const char* prefix);

// Register a duplication in the table
bool register_duplication(DupTable* table, uint32_t location, uint8_t label) {
  for (uint32_t i = 0; i < table->count; i++) {
    if (table->locations[i] == location) {
      if (table->labels[i] != label) {
        fprintf(stderr, "Label mismatch for duplication\n");
        exit(1);
      }
      return false;
    }
  }
  if (table->count >= table->capacity) {
    table->capacity *= 2;
    table->locations = (uint32_t*)realloc(table->locations, table->capacity * sizeof(uint32_t));
    table->labels = (uint8_t*)realloc(table->labels, table->capacity * sizeof(uint8_t));
  }
  table->locations[table->count] = location;
  table->labels[table->count] = label;
  table->count++;
  return true;
}

// Assign IDs to variables and register duplications
void assign_var_ids(IC* ic, Term term, VarNameTable* var_table, DupTable* dup_table) {
  TermTag tag = TERM_TAG(term);
  uint32_t val = TERM_VAL(term);
  uint8_t lab = TERM_LAB(term);

  switch (tag) {
    case VAR: {
      uint32_t loc = val;
      Term subst = ic->heap[loc];
      if (TERM_SUB(subst)) {
        assign_var_ids(ic, ic_clear_sub(subst), var_table, dup_table);
      }
      // For VAR, nothing else to do
      break;
    }

    // Handle all DUP X variants (DX0-DX3)
    case DX0:
    case DX1:
    case DX2:
    case DX3:
    // Handle all DUP Y variants (DY0-DY3)
    case DY0:
    case DY1:
    case DY2:
    case DY3: {
      uint32_t loc = val;
      Term subst = ic->heap[loc];
      if (TERM_SUB(subst)) {
        assign_var_ids(ic, ic_clear_sub(subst), var_table, dup_table);
      } else {
        if (register_duplication(dup_table, loc, lab)) {
          assign_var_ids(ic, subst, var_table, dup_table);
        }
      }
      break;
    }

    case LAM: {
      uint32_t lam_loc = val;
      add_variable(var_table, lam_loc, VAR);
      assign_var_ids(ic, ic->heap[lam_loc], var_table, dup_table);
      break;
    }

    case APP: {
      uint32_t app_loc = val;
      assign_var_ids(ic, ic->heap[app_loc], var_table, dup_table);
      assign_var_ids(ic, ic->heap[app_loc + 1], var_table, dup_table);
      break;
    }

    case ERA: {
      // ERA terms don't have children, so nothing to do
      break;
    }

    // Handle all superposition variants (SP0-SP7)
    case SP0:
    case SP1:
    case SP2:
    case SP3:
    case SP4:
    case SP5:
    case SP6:
    case SP7: {
      uint32_t sup_loc = val;
      assign_var_ids(ic, ic->heap[sup_loc], var_table, dup_table);
      assign_var_ids(ic, ic->heap[sup_loc + 1], var_table, dup_table);
      break;
    }
    
    case NUM: {
      // NUM has no variables to assign
      break;
    }
    
    case SUC: {
      uint32_t suc_loc = val;
      assign_var_ids(ic, ic->heap[suc_loc], var_table, dup_table);
      break;
    }
    
    case SWI: {
      uint32_t swi_loc = val;
      assign_var_ids(ic, ic->heap[swi_loc], var_table, dup_table);     // Number
      assign_var_ids(ic, ic->heap[swi_loc + 1], var_table, dup_table); // Zero branch
      assign_var_ids(ic, ic->heap[swi_loc + 2], var_table, dup_table); // Successor branch
      break;
    }

    default:
      break;
  }
}

// Stringify duplications
void stringify_duplications(IC* ic, DupTable* dup_table, VarNameTable* var_table, char* buffer, int* pos, int max_len, const char* prefix) {
  // First, add all duplication variables
  for (uint32_t i = 0; i < dup_table->count; i++) {
    uint32_t dup_loc = dup_table->locations[i];
    add_variable(var_table, dup_loc, DP0);
    add_variable(var_table, dup_loc, DP1);
  }

  // Then, stringify each duplication
  for (uint32_t i = 0; i < dup_table->count; i++) {
    uint32_t dup_loc = dup_table->locations[i];
    uint8_t lab = dup_table->labels[i];
    Term val_term = ic->heap[dup_loc];

    // Get variable names
    char* var0 = get_var_name(var_table, dup_loc, DP0);
    char* var1 = get_var_name(var_table, dup_loc, DP1);

    // Add duplication header with optional prefix
    if (prefix) {
      *pos += snprintf(buffer + *pos, max_len - *pos, "! &%u{%s%s,%s%s} = ", lab, prefix, var0, prefix, var1);
    } else {
      *pos += snprintf(buffer + *pos, max_len - *pos, "! &%u{%s,%s} = ", lab, var0, var1);
    }

    // Add the value
    stringify_term(ic, val_term, var_table, buffer, pos, max_len, prefix);

    // Add separator
    *pos += snprintf(buffer + *pos, max_len - *pos, ";\n");
  }
}

// Stringify a term
void stringify_term(IC* ic, Term term, VarNameTable* var_table, char* buffer, int* pos, int max_len, const char* prefix) {
  TermTag tag = TERM_TAG(term);
  uint32_t val = TERM_VAL(term);
  uint8_t lab = TERM_LAB(term);

  switch (tag) {
    case VAR: {
      uint32_t loc = val;
      Term subst = ic->heap[loc];
      if (TERM_SUB(subst)) {
        stringify_term(ic, ic_clear_sub(subst), var_table, buffer, pos, max_len, prefix);
      } else {
        char* name = get_var_name(var_table, loc, VAR);
        if (prefix) {
          *pos += snprintf(buffer + *pos, max_len - *pos, "%s%s", prefix, name);
        } else {
          *pos += snprintf(buffer + *pos, max_len - *pos, "%s", name);
        }
      }
      break;
    }

    // Handle all DUP X variants (DX0-DX3)
    case DX0:
    case DX1:
    case DX2:
    case DX3:
    // Handle all DUP Y variants (DY0-DY3)
    case DY0:
    case DY1:
    case DY2:
    case DY3: {
      TermTag co_type = (tag >= DX0 && tag <= DX3) ? DP0 : DP1;
      uint32_t loc = val;
      Term subst = ic->heap[loc];
      if (TERM_SUB(subst)) {
        stringify_term(ic, ic_clear_sub(subst), var_table, buffer, pos, max_len, prefix);
      } else {
        char* name = get_var_name(var_table, loc, co_type);
        if (prefix) {
          *pos += snprintf(buffer + *pos, max_len - *pos, "%s%s", prefix, name);
        } else {
          *pos += snprintf(buffer + *pos, max_len - *pos, "%s", name);
        }
      }
      break;
    }

    case LAM: {
      uint32_t lam_loc = val;
      char* var_name = get_var_name(var_table, lam_loc, VAR);
      if (prefix) {
        *pos += snprintf(buffer + *pos, max_len - *pos, "λ%s%s.", prefix, var_name);
      } else {
        *pos += snprintf(buffer + *pos, max_len - *pos, "λ%s.", var_name);
      }
      stringify_term(ic, ic->heap[lam_loc], var_table, buffer, pos, max_len, prefix);
      break;
    }

    case APP: {
      *pos += snprintf(buffer + *pos, max_len - *pos, "(");
      stringify_term(ic, ic->heap[val], var_table, buffer, pos, max_len, prefix);
      *pos += snprintf(buffer + *pos, max_len - *pos, " ");
      stringify_term(ic, ic->heap[val + 1], var_table, buffer, pos, max_len, prefix);
      *pos += snprintf(buffer + *pos, max_len - *pos, ")");
      break;
    }

    case ERA: {
      *pos += snprintf(buffer + *pos, max_len - *pos, "*");
      break;
    }

    // Handle all superposition variants (SP0-SP7)
    case SP0:
    case SP1:
    case SP2:
    case SP3:
    case SP4:
    case SP5:
    case SP6:
    case SP7: {
      *pos += snprintf(buffer + *pos, max_len - *pos, "&%u{", lab);
      stringify_term(ic, ic->heap[val], var_table, buffer, pos, max_len, prefix);
      *pos += snprintf(buffer + *pos, max_len - *pos, ",");
      stringify_term(ic, ic->heap[val + 1], var_table, buffer, pos, max_len, prefix);
      *pos += snprintf(buffer + *pos, max_len - *pos, "}");
      break;
    }
    
    case NUM: {
      *pos += snprintf(buffer + *pos, max_len - *pos, "%u", val);
      break;
    }
    
    case SUC: {
      *pos += snprintf(buffer + *pos, max_len - *pos, "+");
      stringify_term(ic, ic->heap[val], var_table, buffer, pos, max_len, prefix);
      break;
    }
    
    case SWI: {
      *pos += snprintf(buffer + *pos, max_len - *pos, "?");
      stringify_term(ic, ic->heap[val], var_table, buffer, pos, max_len, prefix);
      *pos += snprintf(buffer + *pos, max_len - *pos, "{0:");
      stringify_term(ic, ic->heap[val + 1], var_table, buffer, pos, max_len, prefix);
      *pos += snprintf(buffer + *pos, max_len - *pos, ";+:");
      stringify_term(ic, ic->heap[val + 2], var_table, buffer, pos, max_len, prefix);
      *pos += snprintf(buffer + *pos, max_len - *pos, ";}");
      break;
    }

    default:
      *pos += snprintf(buffer + *pos, max_len - *pos, "<?unknown term>");
      break;
  }
}

// Convert a term to its string representation with optional namespace prefix
static char* term_to_string_internal(IC* ic, Term term, const char* prefix) {
  // Initialize tables
  VarNameTable var_table;
  DupTable dup_table;
  init_var_table(&var_table);
  init_dup_table(&dup_table);

  // Assign IDs to variables and register duplications
  assign_var_ids(ic, term, &var_table, &dup_table);

  // Allocate buffer for the string representation
  char* buffer = (char*)malloc(MAX_STR_LEN);
  int pos = 0;

  // First stringify all duplications
  stringify_duplications(ic, &dup_table, &var_table, buffer, &pos, MAX_STR_LEN, prefix);

  // Then stringify the main term
  stringify_term(ic, term, &var_table, buffer, &pos, MAX_STR_LEN, prefix);

  // Free tables
  free_var_table(&var_table);
  free_dup_table(&dup_table);

  return buffer;
}

// Convert a term to its string representation
char* term_to_string(IC* ic, Term term) {
  return term_to_string_internal(ic, term, NULL);
}

// Convert a term to its string representation with a prefix for variable names
char* term_to_string_namespaced(IC* ic, Term term, const char* prefix) {
  return term_to_string_internal(ic, term, prefix);
}

// Display a term to the specified output stream
void show_term(FILE* stream, IC* ic, Term term) {
  char* str = term_to_string(ic, term);
  fprintf(stream, "%s", str);
  free(str);
}

// Display a term to the specified output stream with a prefix for variable names
void show_term_namespaced(FILE* stream, IC* ic, Term term, const char* prefix) {
  char* str = term_to_string_namespaced(ic, term, prefix);
  fprintf(stream, "%s", str);
  free(str);
}
