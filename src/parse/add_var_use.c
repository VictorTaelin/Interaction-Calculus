#include <string.h>
#include <stddef.h>
#include "../parse.h"

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