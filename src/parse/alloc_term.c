#include <stddef.h>
#include "../parse.h"
#include "../memory.h"

// Allocate memory for a term with n fields
uint32_t alloc_term(uint32_t n) {
  return alloc(n);
}