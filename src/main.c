#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "types.h"
#include "memory.h"
#include "parse.h"
#include "whnf.h"
#include "normal.h"
#include "show.h"

// Default test term string
const char* DEFAULT_TEST_TERM = "((λf.λx.!&0{f0,f1}=f;(f0 (f1 x)) λB.λT.λF.((B F) T)) λa.λb.a)";

// Test the parser by parsing and showing the default test term
void test_parser() {
  printf("Parsing test term: %s\n", DEFAULT_TEST_TERM);
  
  // Parse the term
  Term term = parse_string(DEFAULT_TEST_TERM);
  
  // Show the parsed term
  printf("Parsed term: ");
  show_term(stdout, term);
  printf("\n\n");
}

int main(int argc, char* argv[]) {
  // Initialize memory
  init_memory();
  
  // Parse command line arguments
  const char* input = DEFAULT_TEST_TERM;
  if (argc > 1) {
    // If a command line argument is provided, use it as the input
    input = argv[1];
  }
  
  // Parse the term
  Term term = parse_string(input);
  
  printf("Original term:\n");
  show_term(stdout, term);
  printf("\n\n");
  
  // Normalize the term
  term = normal(term);
  
  printf("Normal form:\n");
  show_term(stdout, term);
  printf("\n\n");
  
  printf("Total interactions: %llu\n", interaction_count);
  
  return 0;
}
