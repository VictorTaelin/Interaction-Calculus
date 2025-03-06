#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "ic.h"
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
  uint8_t* labels;       // Array of collapser labels
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
  table->labels = (uint8_t*)malloc(table->capacity * sizeof(uint8_t));
}

// Free collapser table
void free_col_table(ColTable* table) {
  free(table->locations);
  free(table->labels);
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
void assign_var_ids(IC* ic, Term term, VarNameTable* var_table, ColTable* col_table);
void stringify_term(IC* ic, Term term, VarNameTable* var_table, char* buffer, int* pos, int max_len);
void stringify_collapsers(IC* ic, ColTable* col_table, VarNameTable* var_table, char* buffer, int* pos, int max_len);

// Register a collapser in the table
bool register_collapser(ColTable* table, uint32_t location, uint8_t label) {
  for (uint32_t i = 0; i < table->count; i++) {
    if (table->locations[i] == location) {
      if (table->labels[i] != label) {
        fprintf(stderr, "Label mismatch for collapser\n");
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

// Assign IDs to variables and register collapsers
void assign_var_ids(IC* ic, Term term, VarNameTable* var_table, ColTable* col_table) {
  TermTag tag = TERM_TAG(term);
  uint32_t val = TERM_VAL(term);
  uint8_t lab = TERM_LAB(term);

  switch (tag) {
    case VAR:
    case CO0:
    case CO1: {
      uint32_t loc = val;
      Term subst = ic->heap[loc];
      if (TERM_SUB(subst)) {
        assign_var_ids(ic, ic_clear_sub(subst), var_table, col_table);
      } else {
        if (tag == CO0 || tag == CO1) {
          uint8_t lab = TERM_LAB(term);
          if (register_collapser(col_table, loc, lab)) {
            assign_var_ids(ic, subst, var_table, col_table);
          }
        }
        // For VAR, do nothing
      }
      break;
    }

    case LAM: {
      uint32_t lam_loc = val;
      add_variable(var_table, lam_loc, VAR);
      assign_var_ids(ic, ic->heap[lam_loc], var_table, col_table);
      break;
    }

    case APP: {
      uint32_t app_loc = val;
      assign_var_ids(ic, ic->heap[app_loc], var_table, col_table);
      assign_var_ids(ic, ic->heap[app_loc + 1], var_table, col_table);
      break;
    }

    case SUP: {
      uint32_t sup_loc = val;
      assign_var_ids(ic, ic->heap[sup_loc], var_table, col_table);
      assign_var_ids(ic, ic->heap[sup_loc + 1], var_table, col_table);
      break;
    }

    case NAT: {
      // For NAT, there are no child terms to process
      break;
    }

    case CAL: {
      uint32_t cal_loc = val;
      assign_var_ids(ic, ic->heap[cal_loc], var_table, col_table);
      break;
    }

    default:
      break;
  }
}

// Stringify collapsers
void stringify_collapsers(IC* ic, ColTable* col_table, VarNameTable* var_table, char* buffer, int* pos, int max_len) {
  // First, add all collapser variables
  for (uint32_t i = 0; i < col_table->count; i++) {
    uint32_t col_loc = col_table->locations[i];
    add_variable(var_table, col_loc, CO0);
    add_variable(var_table, col_loc, CO1);
  }

  // Then, stringify each collapser
  for (uint32_t i = 0; i < col_table->count; i++) {
    uint32_t col_loc = col_table->locations[i];
    uint8_t lab = col_table->labels[i];
    Term val_term = ic->heap[col_loc];

    // Get variable names
    char* var0 = get_var_name(var_table, col_loc, CO0);
    char* var1 = get_var_name(var_table, col_loc, CO1);

    // Add collapser header
    *pos += snprintf(buffer + *pos, max_len - *pos, "! &%u{%s,%s} = ", lab, var0, var1);

    // Add the value
    stringify_term(ic, val_term, var_table, buffer, pos, max_len);

    // Add separator
    *pos += snprintf(buffer + *pos, max_len - *pos, ";\n");
  }
}

// Stringify a term
void stringify_term(IC* ic, Term term, VarNameTable* var_table, char* buffer, int* pos, int max_len) {
  TermTag tag = TERM_TAG(term);
  uint32_t val = TERM_VAL(term);
  uint8_t lab = TERM_LAB(term);

  switch (tag) {
    case VAR:
    case CO0:
    case CO1: {
      uint32_t loc = val;
      Term subst = ic->heap[loc];
      if (TERM_SUB(subst)) {
        stringify_term(ic, ic_clear_sub(subst), var_table, buffer, pos, max_len);
      } else {
        char* name = get_var_name(var_table, loc, tag);
        *pos += snprintf(buffer + *pos, max_len - *pos, "%s", name);
      }
      break;
    }

    case LAM: {
      uint32_t lam_loc = val;
      char* var_name = get_var_name(var_table, lam_loc, VAR);
      *pos += snprintf(buffer + *pos, max_len - *pos, "λ%s.", var_name);
      stringify_term(ic, ic->heap[lam_loc], var_table, buffer, pos, max_len);
      break;
    }

    case APP: {
      *pos += snprintf(buffer + *pos, max_len - *pos, "(");
      stringify_term(ic, ic->heap[val], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, " ");
      stringify_term(ic, ic->heap[val + 1], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, ")");
      break;
    }

    case SUP: {
      *pos += snprintf(buffer + *pos, max_len - *pos, "&%u{", lab);
      stringify_term(ic, ic->heap[val], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, ",");
      stringify_term(ic, ic->heap[val + 1], var_table, buffer, pos, max_len);
      *pos += snprintf(buffer + *pos, max_len - *pos, "}");
      break;
    }

    case NAT: {
      // For NAT, the value is stored directly in the term
      *pos += snprintf(buffer + *pos, max_len - *pos, "%u", val);
      break;
    }

    case CAL: {
      if (lab == 0xFF) { // Special case for increment (SUC)
        *pos += snprintf(buffer + *pos, max_len - *pos, "+");
        stringify_term(ic, ic->heap[val], var_table, buffer, pos, max_len);
      } else {
        char func_name = 'A' + lab; // Convert function ID to name (A-P)
        *pos += snprintf(buffer + *pos, max_len - *pos, "@%c(", func_name);
        stringify_term(ic, ic->heap[val], var_table, buffer, pos, max_len);
        *pos += snprintf(buffer + *pos, max_len - *pos, ")");
      }
      break;
    }

    default:
      *pos += snprintf(buffer + *pos, max_len - *pos, "<?unknown term>");
      break;
  }
}

// Main function to convert a term to string
char* term_to_string(IC* ic, Term term) {
  // Initialize tables
  VarNameTable var_table;
  ColTable col_table;
  init_var_table(&var_table);
  init_col_table(&col_table);

  // Assign IDs to variables and register collapsers
  assign_var_ids(ic, term, &var_table, &col_table);

  // Allocate buffer for the string representation
  char* buffer = (char*)malloc(MAX_STR_LEN);
  int pos = 0;

  // First stringify all collapsers
  stringify_collapsers(ic, &col_table, &var_table, buffer, &pos, MAX_STR_LEN);

  // Then stringify the main term
  stringify_term(ic, term, &var_table, buffer, &pos, MAX_STR_LEN);

  // Free tables
  free_var_table(&var_table);
  free_col_table(&col_table);

  return buffer;
}

// Display a term to the specified output stream
void show_term(FILE* stream, IC* ic, Term term) {
  char* str = term_to_string(ic, term);
  fprintf(stream, "%s", str);
  free(str);
}
