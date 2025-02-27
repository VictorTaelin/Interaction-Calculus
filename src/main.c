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

// Run a term through normalization and print results
void process_term(Term term) {
  printf("Original term:\n");
  show_term(stdout, term);
  printf("\n\n");

  // Reset interaction counter
  interaction_count = 0;

  // Normalize the term
  term = normal(term);

  printf("Total interactions: %llu\n", interaction_count);

  printf("Normal form:\n");
  show_term(stdout, term);
  printf("\n\n");

}

// Test function with the default term
void test() {
  printf("Running with default test term: %s\n", DEFAULT_TEST_TERM);

  // Parse the term
  Term term = parse_string(DEFAULT_TEST_TERM);

  // Process the term
  process_term(term);
}

// Parse a file and return the term
Term parse_file(const char* filename) {
  FILE* file = fopen(filename, "r");
  if (!file) {
    fprintf(stderr, "Error: Could not open file '%s'\n", filename);
    exit(1);
  }

  // Get file size
  fseek(file, 0, SEEK_END);
  long size = ftell(file);
  fseek(file, 0, SEEK_SET);

  // Allocate buffer
  char* buffer = (char*)malloc(size + 1);
  if (!buffer) {
    fprintf(stderr, "Error: Memory allocation failed\n");
    fclose(file);
    exit(1);
  }

  // Read file contents
  size_t read_size = fread(buffer, 1, size, file);
  fclose(file);

  buffer[read_size] = '\0';

  // Parse the string
  Term term = parse_string(buffer);

  // Free the buffer
  free(buffer);

  return term;
}

void print_usage() {
  printf("Usage: suptt <command> [arguments]\n\n");
  printf("Commands:\n");
  printf("  run <file>     - Parse and normalize a SupTT file\n");
  printf("  eval <expr>    - Parse and normalize a SupTT expression\n");
  printf("\n");
}

int main(int argc, char* argv[]) {
  // Initialize memory
  init_memory();

  // Check if no arguments provided
  if (argc < 2) {
    print_usage();
    return 1;
  }

  // Get command
  const char* command = argv[1];

  // Handle commands
  if (strcmp(command, "run") == 0) {
    // Check if filename is provided
    if (argc < 3) {
      fprintf(stderr, "Error: No file specified\n");
      print_usage();
      return 1;
    }

    // Parse and process the file
    const char* filename = argv[2];
    Term term = parse_file(filename);
    process_term(term);

  } else if (strcmp(command, "eval") == 0) {
    // Check if expression is provided
    if (argc < 3) {
      fprintf(stderr, "Error: No expression specified\n");
      print_usage();
      return 1;
    }

    // Parse and process the expression
    const char* expression = argv[2];
    Term term = parse_string(expression);
    process_term(term);

  } else {
    fprintf(stderr, "Error: Unknown command '%s'\n", command);
    print_usage();
    return 1;
  }

  return 0;
}
