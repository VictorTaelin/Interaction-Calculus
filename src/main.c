#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>
#include "ic.h"
#include "parse.h"
#include "show.h"

// Forward declarations for GPU functions
#ifdef HAVE_CUDA
extern Term ic_normal_cuda(IC* ic, Term term, int thread_count);
extern int ic_cuda_available();
#endif

#ifdef HAVE_METAL
extern Term ic_normal_metal(IC* ic, Term term);
extern int ic_metal_available();
#endif

// Stub functions when CUDA is not available
#ifndef HAVE_CUDA
static inline int ic_cuda_available() {
  return 0; // CUDA not available
}

static inline Term ic_normal_cuda(IC* ic, Term term, int thread_count) {
  fprintf(stderr, "Warning: CUDA GPU support not compiled. Falling back to other methods.\n");
  #ifdef HAVE_METAL
  return ic_normal_metal(ic, term);
  #else
  return ic_normal(ic, term);
  #endif
}
#endif

// Stub functions when Metal is not available
#ifndef HAVE_METAL
static inline int ic_metal_available() {
  return 0; // Metal not available
}

static inline Term ic_normal_metal(IC* ic, Term term) {
  fprintf(stderr, "Warning: Metal GPU support not compiled. Running on CPU instead.\n");
  return ic_normal(ic, term);
}
#endif

// Default test term string
const char* DEFAULT_TEST_TERM = "((λf.λx.!&0{f0,f1}=f;(f0 (f1 x)) λB.λT.λF.((B F) T)) λa.λb.a)";

// Run a term through normalization and print results
void process_term(IC* ic, Term term, int use_gpu, int thread_count) {
  // Reset interaction counter
  ic->interactions = 0;

  // Record start time
  struct timeval start_time, current_time;
  gettimeofday(&start_time, NULL);
  double elapsed_seconds = 0;

  // Normalize the term
  if (use_gpu) {
    if (ic_cuda_available()) {
      term = ic_normal_cuda(ic, term, thread_count);
    } else if (ic_metal_available()) {
      printf("CUDA not available, using Metal GPU acceleration\n");
      term = ic_normal_metal(ic, term);
    } else {
      printf("Warning: No GPU acceleration available, falling back to CPU execution\n");
      term = ic_normal(ic, term);
    }
  } else {
    term = ic_normal(ic, term);
  }

  // Record end time
  gettimeofday(&current_time, NULL);
  elapsed_seconds = (current_time.tv_sec - start_time.tv_sec) + 
                    (current_time.tv_usec - start_time.tv_usec) / 1000000.0;

  // Get heap size (the number of allocated nodes)
  size_t size = ic->heap_pos;

  // Calculate PERF, avoiding division by zero
  double perf = elapsed_seconds > 0 ? (ic->interactions / elapsed_seconds) / 1000000.0 : 0.0;

  show_term(stdout, ic, term);
  printf("\n\n");

  // Print statistics
  printf("WORK: %llu interactions\n", ic->interactions);
  printf("TIME: %.7f seconds\n", elapsed_seconds);
  printf("SIZE: %zu nodes\n", size);
  printf("PERF: %.3f MIPS\n", perf);

  // Determine which mode was actually used
  const char* mode_str = "CPU";
  if (use_gpu) {
    if (ic_cuda_available()) {
      mode_str = "CUDA GPU";
      printf("THREADS: %d\n", thread_count);
    } else if (ic_metal_available()) {
      mode_str = "Metal GPU";
    }
  }

  printf("MODE: %s\n", mode_str);
  printf("\n");
}

// Test function with the default term
void test(IC* ic, int use_gpu, int thread_count) {
  printf("Running with default test term: %s\n", DEFAULT_TEST_TERM);

  // Parse the term
  Term term = parse_string(ic, DEFAULT_TEST_TERM);

  // Process the term
  process_term(ic, term, use_gpu, thread_count);
}

// Now implemented in parse.c

// Benchmark function to run normalization repeatedly for 1 second
void benchmark_term(IC* ic, Term term, int use_gpu, int thread_count) {
  // Create a snapshot of the initial state
  uint32_t original_heap_pos = ic->heap_pos;
  Term* original_heap_state = (Term*)malloc(original_heap_pos * sizeof(Term));
  if (!original_heap_state) {
    fprintf(stderr, "Error: Memory allocation failed for heap snapshot\n");
    return;
  }

  // Copy the initial heap state
  memcpy(original_heap_state, ic->heap, original_heap_pos * sizeof(Term));

  // Get a snapshot of the term as it might get modified during normalization
  Term original_term = term;

  // Normalize once and show the result
  Term result;
  if (use_gpu) {
    if (ic_cuda_available()) {
      result = ic_normal_cuda(ic, term, thread_count);
    } else if (ic_metal_available()) {
      printf("CUDA not available, using Metal GPU acceleration\n");
      result = ic_normal_metal(ic, term);
    } else {
      printf("Warning: No GPU acceleration available, falling back to CPU execution\n");
      result = ic_normal(ic, term);
    }
  } else {
    result = ic_normal(ic, term);
  }

  show_term(stdout, ic, result);
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
    ic->heap_pos = original_heap_pos;
    memcpy(ic->heap, original_heap_state, original_heap_pos * sizeof(Term));

    // Reset interaction counter
    ic->interactions = 0;

    // Normalize the term again
    if (use_gpu) {
      if (ic_cuda_available()) {
        ic_normal_cuda(ic, original_term, thread_count);
      } else if (ic_metal_available()) {
        ic_normal_metal(ic, original_term);
      } else {
        ic_normal(ic, original_term);
      }
    } else {
      ic_normal(ic, original_term);
    }

    // Accumulate interactions
    total_interactions += ic->interactions;
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

  // Determine which mode was actually used for benchmark
  const char* mode_str = "CPU";
  if (use_gpu) {
    if (ic_cuda_available()) {
      mode_str = "CUDA GPU";
      printf("- THREADS: %d\n", thread_count);
    } else if (ic_metal_available()) {
      mode_str = "Metal GPU";
    }
  }

  printf("- MODE: %s\n", mode_str);

  // Clean up
  free(original_heap_state);
}

void print_usage() {
  printf("Usage: ic <command> [arguments] [options]\n\n");
  printf("Commands:\n");
  printf("  run <file>     - Parse and normalize a IC file on CPU\n");
  printf("  run-gpu <file> - Parse and normalize a IC file on GPU (CUDA or Metal)\n");
  printf("  eval <expr>    - Parse and normalize a IC expression on CPU\n");
  printf("  eval-gpu <expr> - Parse and normalize a IC expression on GPU (CUDA or Metal)\n");
  printf("  bench <file>   - Benchmark normalization of a IC file on CPU\n");
  printf("  bench-gpu <file> - Benchmark normalization of a IC file on GPU (CUDA or Metal)\n");
  printf("\n");
  printf("Options for GPU commands:\n");
  printf("  -t <num>       - Number of CUDA threads to use (default: 1)\n");
  printf("\n");
}

int main(int argc, char* argv[]) {
  // Initialize IC context
  IC* ic = ic_default_new();
  if (!ic) {
    fprintf(stderr, "Error: Failed to initialize IC context\n");
    return 1;
  }

  int result = 0;
  int thread_count = 1; // Default thread count

  // Parse thread count from command line arguments
  for (int i = 1; i < argc - 1; i++) {
    if (strcmp(argv[i], "-t") == 0 && i + 1 < argc) {
      thread_count = atoi(argv[i + 1]);
      if (thread_count <= 0) {
        fprintf(stderr, "Warning: Invalid thread count '%s', using default (1)\n", argv[i + 1]);
        thread_count = 1;
      }
    }
  }

  // Check if no arguments provided
  if (argc < 2) {
    test(ic, 0, thread_count); // Run with default test term on CPU
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
        // Parse and process the file on CPU
        const char* filename = argv[2];
        Term term = parse_file(ic, filename);
        process_term(ic, term, 0, thread_count);
      }
    } else if (strcmp(command, "run-gpu") == 0) {
      // Check if filename is provided
      if (argc < 3) {
        fprintf(stderr, "Error: No file specified\n");
        print_usage();
        result = 1;
      } else {
        // Parse and process the file on GPU
        const char* filename = argv[2];
        Term term = parse_file(ic, filename);
        process_term(ic, term, 1, thread_count);
      }
    } else if (strcmp(command, "eval") == 0) {
      // Check if expression is provided
      if (argc < 3) {
        fprintf(stderr, "Error: No expression specified\n");
        print_usage();
        result = 1;
      } else {
        // Parse and process the expression on CPU
        const char* expression = argv[2];
        Term term = parse_string(ic, expression);
        process_term(ic, term, 0, thread_count);
      }
    } else if (strcmp(command, "eval-gpu") == 0) {
      // Check if expression is provided
      if (argc < 3) {
        fprintf(stderr, "Error: No expression specified\n");
        print_usage();
        result = 1;
      } else {
        // Parse and process the expression on GPU
        const char* expression = argv[2];
        Term term = parse_string(ic, expression);
        process_term(ic, term, 1, thread_count);
      }
    } else if (strcmp(command, "bench") == 0) {
      // Check if filename is provided
      if (argc < 3) {
        fprintf(stderr, "Error: No file specified for benchmark\n");
        print_usage();
        result = 1;
      } else {
        // Parse and benchmark the file on CPU
        const char* filename = argv[2];
        Term term = parse_file(ic, filename);
        benchmark_term(ic, term, 0, thread_count);
      }
    } else if (strcmp(command, "bench-gpu") == 0) {
      // Check if filename is provided
      if (argc < 3) {
        fprintf(stderr, "Error: No file specified for benchmark\n");
        print_usage();
        result = 1;
      } else {
        // Parse and benchmark the file on GPU
        const char* filename = argv[2];
        Term term = parse_file(ic, filename);
        benchmark_term(ic, term, 1, thread_count);
      }
    } else {
      fprintf(stderr, "Error: Unknown command '%s'\n", command);
      print_usage();
      result = 1;
    }
  }

  // Clean up IC context before exiting
  ic_free(ic);

  return result;
}
