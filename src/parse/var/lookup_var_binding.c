#include <string.h>
#include <stddef.h>
#include "../../parse.h"

// Look up a variable binding by name
Term* lookup_var_binding(Parser* parser, const char* name) {
  for (int i = 0; i < parser->vrs_count; i++) {
    if (strcmp(parser->vrs[i].name, name) == 0) {
      return &parser->vrs[i].term;
    }
  }
  return NULL;
}