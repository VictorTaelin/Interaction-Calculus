#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>
#include "hvmn.h"
#include "parse.h"
#include "show.h"

// Forward declarations for GPU functions
#ifdef HAVE_CUDA
extern Term hvmn_normal_cuda(HVMN* hvmn, Term term, int thread_count);
extern int hvmn_cuda_available();
#endif

#ifdef HAVE_METAL
extern Term hvmn_normal_metal(HVMN* hvmn, Term term);
extern int hvmn_metal_available();
#endif

// Stub functions when CUDA is not available
#ifndef HAVE_CUDA
static inline int hvmn_cuda_available() {
  return 0; // CUDA not available
}

static inline Term hvmn_normal_cuda(HVMN* hvmn, Term term, int thread_count) {
  fprintf(stderr, "Warning: CUDA GPU support not compiled. Falling back to other methods.\n");
  #ifdef HAVE_METAL
  return hvmn_normal_metal(hvmn, term);
  #else
  return hvmn_normal(hvmn, term);
  #endif
}
#endif

// Stub functions when Metal is not available
#ifndef HAVE_METAL
static inline int hvmn_metal_available() {
  return 0; // Metal not available
}

static inline Term hvmn_normal_metal(HVMN* hvmn, Term term) {
  fprintf(stderr, "Warning: Metal GPU support not compiled. Running on CPU instead.\n");
  return hvmn_normal(hvmn, term);
}
#endif

// Default test term string
const char* DEFAULT_TEST_TERM = "((λf.λx.!&0{f0,f1}=f;(f0 (f1 x)) λB.λT.λF.((B F) T)) λa.λb.a)";

// Run a term through normalization and print results
void process_term(HVMN* hvmn, Term term, int use_gpu, int thread_count) {
  // Reset interaction counter
  hvmn->interactions = 0;

  // Record start time
  struct timeval start_time, current_time;
  gettimeofday(&start_time, NULL);
  double elapsed_seconds = 0;

  // Normalize the term
  if (use_gpu) {
    if (hvmn_cuda_available()) {
      term = hvmn_normal_cuda(hvmn, term, thread_count);
    } else if (hvmn_metal_available()) {
      printf("CUDA not available, using Metal GPU acceleration\n");
      term = hvmn_normal_metal(hvmn, term);
    } else {
      printf("Warning: No GPU acceleration available, falling back to CPU execution\n");
      term = hvmn_normal(hvmn, term);
    }
  } else {
    term = hvmn_normal(hvmn, term);
  }

  // Record end time
  gettimeofday(&current_time, NULL);
  elapsed_seconds = (current_time.tv_sec - start_time.tv_sec) + 
                    (current_time.tv_usec - start_time.tv_usec) / 1000000.0;

  // Get heap size (the number of allocated nodes)
  size_t size = hvmn->heap_pos;

  // Calculate PERF, avoiding division by zero
  double perf = elapsed_seconds > 0 ? (hvmn->interactions / elapsed_seconds) / 1000000.0 : 0.0;

  show_term(stdout, hvmn, term);
  printf("\n\n");

  // Print statistics
  printf("WORK: %llu interactions\n", hvmn->interactions);
  printf("TIME: %.7f seconds\n", elapsed_seconds);
  printf("SIZE: %zu nodes\n", size);
  printf("PERF: %.3f MIPS\n", perf);

  // Determine which mode was actually used
  const char* mode_str = "CPU";
  if (use_gpu) {
    if (hvmn_cuda_available()) {
      mode_str = "CUDA GPU";
      printf("THREADS: %d\n", thread_count);
    } else if (hvmn_metal_available()) {
      mode_str = "Metal GPU";
    }
  }

  printf("MODE: %s\n", mode_str);
  printf("\n");
}

// Test function with the default term
void test(HVMN* hvmn, int use_gpu, int thread_count) {
  printf("Running with default test term: %s\n", DEFAULT_TEST_TERM);

  // Parse the term
  Term term = parse_string(hvmn, DEFAULT_TEST_TERM);

  // Process the term
  process_term(hvmn, term, use_gpu, thread_count);
}

// Now implemented in parse.c

// Benchmark function to run normalization repeatedly for 1 second
void benchmark_term(HVMN* hvmn, Term term, int use_gpu, int thread_count) {
  // Create a snapshot of the initial state
  uint32_t original_heap_pos = hvmn->heap_pos;
  Term* original_heap_state = (Term*)malloc(original_heap_pos * sizeof(Term));
  if (!original_heap_state) {
    fprintf(stderr, "Error: Memory allocation failed for heap snapshot\n");
    return;
  }

  // Copy the initial heap state
  memcpy(original_heap_state, hvmn->heap, original_heap_pos * sizeof(Term));

  // Get a snapshot of the term as it might get modified during normalization
  Term original_term = term;

  // Normalize once and show the result
  Term result;
  if (use_gpu) {
    if (hvmn_cuda_available()) {
      result = hvmn_normal_cuda(hvmn, term, thread_count);
    } else if (hvmn_metal_available()) {
      printf("CUDA not available, using Metal GPU acceleration\n");
      result = hvmn_normal_metal(hvmn, term);
    } else {
      printf("Warning: No GPU acceleration available, falling back to CPU execution\n");
      result = hvmn_normal(hvmn, term);
    }
  } else {
    result = hvmn_normal(hvmn, term);
  }

  show_term(stdout, hvmn, result);
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
    hvmn->heap_pos = original_heap_pos;
    memcpy(hvmn->heap, original_heap_state, original_heap_pos * sizeof(Term));

    // Reset interaction counter
    hvmn->interactions = 0;

    // Normalize the term again
    if (use_gpu) {
      if (hvmn_cuda_available()) {
        hvmn_normal_cuda(hvmn, original_term, thread_count);
      } else if (hvmn_metal_available()) {
        hvmn_normal_metal(hvmn, original_term);
      } else {
        hvmn_normal(hvmn, original_term);
      }
    } else {
      hvmn_normal(hvmn, original_term);
    }

    // Accumulate interactions
    total_interactions += hvmn->interactions;
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
    if (hvmn_cuda_available()) {
      mode_str = "CUDA GPU";
      printf("- THREADS: %d\n", thread_count);
    } else if (hvmn_metal_available()) {
      mode_str = "Metal GPU";
    }
  }

  printf("- MODE: %s\n", mode_str);

  // Clean up
  free(original_heap_state);
}

void print_usage() {
  printf("Usage: hvmn <command> [arguments] [options]\n\n");
  printf("Commands:\n");
  printf("  run <file>       - Parse and normalize a HVMN file on CPU\n");
  printf("  run-gpu <file>   - Parse and normalize a HVMN file on GPU (CUDA or Metal)\n");
  printf("  eval <expr>      - Parse and normalize a HVMN expression on CPU\n");
  printf("  eval-gpu <expr>  - Parse and normalize a HVMN expression on GPU (CUDA or Metal)\n");
  printf("  bench <file>     - Benchmark normalization of a HVMN file on CPU\n");
  printf("  bench-gpu <file> - Benchmark normalization of a HVMN file on GPU (CUDA or Metal)\n");
  printf("\n");
  printf("Options for GPU commands:\n");
  printf("  -t <num>       - Number of CUDA threads to use (default: 1)\n");
  printf("\n");
}

int main(int argc, char* argv[]) {
  // Initialize HVMN context
  HVMN* hvmn = hvmn_default_new();
  if (!hvmn) {
    fprintf(stderr, "Error: Failed to initialize HVMN context\n");
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
    test(hvmn, 0, thread_count); // Run with default test term on CPU
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
        Term term = parse_file(hvmn, filename);
        process_term(hvmn, term, 0, thread_count);
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
        Term term = parse_file(hvmn, filename);
        process_term(hvmn, term, 1, thread_count);
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
        Term term = parse_string(hvmn, expression);
        process_term(hvmn, term, 0, thread_count);
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
        Term term = parse_string(hvmn, expression);
        process_term(hvmn, term, 1, thread_count);
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
        Term term = parse_file(hvmn, filename);
        benchmark_term(hvmn, term, 0, thread_count);
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
        Term term = parse_file(hvmn, filename);
        benchmark_term(hvmn, term, 1, thread_count);
      }
    } else {
      fprintf(stderr, "Error: Unknown command '%s'\n", command);
      print_usage();
      result = 1;
    }
  }

  // Clean up HVMN context before exiting
  hvmn_free(hvmn);

  return result;
}
