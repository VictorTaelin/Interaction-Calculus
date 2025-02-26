#include <string.h>
#include <stddef.h>
#include "../../parse.h"

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