#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>
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
  // Reset interaction counter
  interaction_count = 0;

  // Record start time
  clock_t start = clock();

  // Normalize the term
  term = normal(term);

  // Record end time
  clock_t end = clock();

  // Calculate time in seconds
  double time_seconds = (double)(end - start) / CLOCKS_PER_SEC;

  // Get heap size (the number of allocated nodes)
  size_t size = heap_ptr;

  // Calculate PERF, avoiding division by zero
  double perf = time_seconds > 0 ? (interaction_count / time_seconds) / 1000000.0 : 0.0;

  show_term(stdout, term);
  printf("\n\n");

  // Print statistics
  printf("WORK: %llu interactions\n", interaction_count);
  printf("TIME: %.7f seconds\n", time_seconds);
  printf("SIZE: %zu nodes\n", size);
  printf("PERF: %.3f MIPS\n", perf);
  printf("\n");
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

// Benchmark function to run normalization repeatedly for 1 second
void benchmark_term(Term term) {
  printf("Original term:\n");
  show_term(stdout, term);
  printf("\n\n");

  // Create a snapshot of the initial state
  uint32_t original_heap_ptr = heap_ptr;
  Term* original_heap_state = (Term*)malloc(original_heap_ptr * sizeof(Term));
  if (!original_heap_state) {
    fprintf(stderr, "Error: Memory allocation failed for heap snapshot\n");
    return;
  }

  // Copy the initial heap state
  memcpy(original_heap_state, heap, original_heap_ptr * sizeof(Term));

  // Get a snapshot of the term as it might get modified during normalization
  Term original_term = term;

  // Normalize once and show the result
  Term result = normal(term);
  printf("Normal form:\n");
  show_term(stdout, result);
  printf("\n\n");

  // Reset for benchmarking
  uint64_t total_interactions = 0;
  uint32_t iterations = 0;

  // Start timing
  struct timeval start_time, current_time;
  gettimeofday(&start_time, NULL);
  double elapsed_seconds = 0;

  // Run normalization in a loop until 1 second has passed
  while (elapsed_seconds < 1.0) {
    // Reset heap state to original
    heap_ptr = original_heap_ptr;
    memcpy(heap, original_heap_state, original_heap_ptr * sizeof(Term));

    // Reset interaction counter
    interaction_count = 0;

    // Normalize the term again
    normal(original_term);

    // Accumulate interactions
    total_interactions += interaction_count;
    iterations++;

    // Check elapsed time
    gettimeofday(&current_time, NULL);
    elapsed_seconds = (current_time.tv_sec - start_time.tv_sec) + 
                      (current_time.tv_usec - start_time.tv_usec) / 1000000.0;
  }

  // Calculate MIPS (Million Interactions Per Second)
  double mips = (total_interactions / elapsed_seconds) / 1000000.0;

  // Print benchmark results
  printf("BENCHMARK:\n");
  printf("- LOOP: %u\n", iterations);
  printf("- WORK: %llu\n", total_interactions);
  printf("- TIME: %.3f seconds\n", elapsed_seconds);
  printf("- PERF: %.3f MIPS\n", mips);

  // Clean up
  free(original_heap_state);
}

void print_usage() {
  printf("Usage: ic <command> [arguments]\n\n");
  printf("Commands:\n");
  printf("  run <file>     - Parse and normalize a IC file\n");
  printf("  eval <expr>    - Parse and normalize a IC expression\n");
  printf("  bench <file>   - Benchmark normalization of a IC file\n");
  printf("\n");
}

int main(int argc, char* argv[]) {
  // Initialize memory
  init_memory();

  int result = 0;

  // Check if no arguments provided
  if (argc < 2) {
    test(); // Run with default test term
  } else {
    // Get command
    const char* command = argv[1];

    // Handle commands
    if (strcmp(command, "run") == 0) {
      // Check if filename is provided
      if (argc < 3) {
        fprintf(stderr, "Error: No file specified\n");
        print_usage();
        result = 1;
      } else {
        // Parse and process the file
        const char* filename = argv[2];
        Term term = parse_file(filename);
        process_term(term);
      }
    } else if (strcmp(command, "eval") == 0) {
      // Check if expression is provided
      if (argc < 3) {
        fprintf(stderr, "Error: No expression specified\n");
        print_usage();
        result = 1;
      } else {
        // Parse and process the expression
        const char* expression = argv[2];
        Term term = parse_string(expression);
        process_term(term);
      }
    } else if (strcmp(command, "bench") == 0) {
      // Check if filename is provided
      if (argc < 3) {
        fprintf(stderr, "Error: No file specified for benchmark\n");
        print_usage();
        result = 1;
      } else {
        // Parse and benchmark the file
        const char* filename = argv[2];
        Term term = parse_file(filename);
        benchmark_term(term);
      }
    } else {
      fprintf(stderr, "Error: Unknown command '%s'\n", command);
      print_usage();
      result = 1;
    }
  }

  // Clean up memory before exiting
  cleanup_memory();

  return result;
}
