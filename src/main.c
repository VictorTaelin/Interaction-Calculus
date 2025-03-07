#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>
#include "ic.h"
#include "collapse.h"
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

// Function declarations
static Term normalize_term(IC* ic, Term term, int use_gpu, int use_collapse, int thread_count);
static void process_term(IC* ic, Term term, int use_gpu, int use_collapse, int thread_count);
static void benchmark_term(IC* ic, Term term, int use_gpu, int use_collapse, int thread_count);
static void test(IC* ic, int use_gpu, int use_collapse, int thread_count);
static void print_usage(void);

// Normalize a term based on mode flags
static Term normalize_term(IC* ic, Term term, int use_gpu, int use_collapse, int thread_count) {
  if (use_collapse) {
    if (use_gpu) {
      fprintf(stderr, "Warning: Collapse mode is not available for GPU. Using normal GPU normalization.\n");
      if (ic_cuda_available()) {
        return ic_normal_cuda(ic, term, thread_count);
      } else if (ic_metal_available()) {
        return ic_normal_metal(ic, term);
      } else {
        fprintf(stderr, "Warning: No GPU acceleration available. Falling back to CPU normalization.\n");
        return ic_normal(ic, term);
      }
    } else {
      term = ic_collapse_sups(ic, term);
      term = ic_collapse_cols(ic, term);
      return term;
    }
  } else {
    if (use_gpu) {
      if (ic_cuda_available()) {
        return ic_normal_cuda(ic, term, thread_count);
      } else if (ic_metal_available()) {
        return ic_normal_metal(ic, term);
      } else {
        fprintf(stderr, "Warning: No GPU acceleration available. Falling back to CPU normalization.\n");
        return ic_normal(ic, term);
      }
    } else {
      return ic_normal(ic, term);
    }
  }
}

// Process and print results of term normalization
static void process_term(IC* ic, Term term, int use_gpu, int use_collapse, int thread_count) {
  ic->interactions = 0; // Reset interaction counter

  struct timeval start_time, current_time;
  gettimeofday(&start_time, NULL);

  term = normalize_term(ic, term, use_gpu, use_collapse, thread_count);

  gettimeofday(&current_time, NULL);
  double elapsed_seconds = (current_time.tv_sec - start_time.tv_sec) +
                           (current_time.tv_usec - start_time.tv_usec) / 1000000.0;

  size_t size = ic->heap_pos; // Heap size in nodes
  double perf = elapsed_seconds > 0 ? (ic->interactions / elapsed_seconds) / 1000000.0 : 0.0;

  show_term(stdout, ic, term);
  printf("\n\n");
  printf("WORK: %llu interactions\n", ic->interactions);
  printf("TIME: %.7f seconds\n", elapsed_seconds);
  printf("SIZE: %zu nodes\n", size);
  printf("PERF: %.3f MIPS\n", perf);

  const char* mode_str;
  if (use_collapse && !use_gpu) {
    mode_str = "CPU (collapse)";
  } else if (use_gpu) {
    if (ic_cuda_available()) {
      mode_str = "CUDA GPU";
      printf("THREADS: %d\n", thread_count);
    } else if (ic_metal_available()) {
      mode_str = "Metal GPU";
    } else {
      mode_str = "CPU";
    }
  } else {
    mode_str = "CPU";
  }
  printf("MODE: %s\n", mode_str);
  if (use_gpu && use_collapse) {
    printf("Note: Collapse mode is not available for GPU. Used normal GPU normalization.\n");
  }
  printf("\n");
}

// Benchmark normalization performance over 1 second
static void benchmark_term(IC* ic, Term term, int use_gpu, int use_collapse, int thread_count) {
  // Snapshot initial heap state
  uint32_t original_heap_pos = ic->heap_pos;
  Term* original_heap_state = (Term*)malloc(original_heap_pos * sizeof(Term));
  if (!original_heap_state) {
    fprintf(stderr, "Error: Memory allocation failed for heap snapshot\n");
    return;
  }
  memcpy(original_heap_state, ic->heap, original_heap_pos * sizeof(Term));
  Term original_term = term;

  // Normalize once to show result
  Term result = normalize_term(ic, term, use_gpu, use_collapse, thread_count);
  show_term(stdout, ic, result);
  printf("\n\n");

  // Benchmark loop
  uint64_t total_interactions = 0;
  uint32_t iterations = 0;
  struct timeval start_time, current_time;
  gettimeofday(&start_time, NULL);
  double elapsed_seconds = 0;

  while (elapsed_seconds < 1.0) {
    ic->heap_pos = original_heap_pos;
    memcpy(ic->heap, original_heap_state, original_heap_pos * sizeof(Term));
    ic->interactions = 0;

    normalize_term(ic, original_term, use_gpu, use_collapse, thread_count);

    total_interactions += ic->interactions;
    iterations++;

    gettimeofday(&current_time, NULL);
    elapsed_seconds = (current_time.tv_sec - start_time.tv_sec) +
                      (current_time.tv_usec - start_time.tv_usec) / 1000000.0;
  }

  double mips = (total_interactions / elapsed_seconds) / 1000000.0;

  printf("BENCHMARK:\n");
  printf("- LOOP: %u\n", iterations);
  printf("- WORK: %llu\n", total_interactions);
  printf("- TIME: %.3f seconds\n", elapsed_seconds);
  printf("- PERF: %.3f MIPS\n", mips);

  const char* mode_str;
  if (use_collapse && !use_gpu) {
    mode_str = "CPU (collapse)";
  } else if (use_gpu) {
    if (ic_cuda_available()) {
      mode_str = "CUDA GPU";
      printf("- THREADS: %d\n", thread_count);
    } else if (ic_metal_available()) {
      mode_str = "Metal GPU";
    } else {
      mode_str = "CPU";
    }
  } else {
    mode_str = "CPU";
  }
  printf("- MODE: %s\n", mode_str);
  if (use_gpu && use_collapse) {
    printf("- Note: Collapse mode is not available for GPU. Used normal GPU normalization.\n");
  }

  free(original_heap_state);
}

// Run default test term
static void test(IC* ic, int use_gpu, int use_collapse, int thread_count) {
  printf("Running with default test term: %s\n", DEFAULT_TEST_TERM);
  Term term = parse_string(ic, DEFAULT_TEST_TERM);
  process_term(ic, term, use_gpu, use_collapse, thread_count);
}

// Print command-line usage
static void print_usage(void) {
  printf("Usage: ic <command> [arguments] [options]\n\n");
  printf("Commands:\n");
  printf("  run <file>       - Parse and normalize a IC file on CPU\n");
  printf("  run-gpu <file>   - Parse and normalize a IC file on GPU (CUDA or Metal)\n");
  printf("  eval <expr>      - Parse and normalize a IC expression on CPU\n");
  printf("  eval-gpu <expr>  - Parse and normalize a IC expression on GPU (CUDA or Metal)\n");
  printf("  bench <file>     - Benchmark normalization of a IC file on CPU\n");
  printf("  bench-gpu <file> - Benchmark normalization of a IC file on GPU (CUDA or Metal)\n");
  printf("\n");
  printf("Options:\n");
  printf("  -C             - Use collapse mode (CPU only)\n");
  printf("  -t <num>       - Number of CUDA threads to use (default: 1)\n");
  printf("\n");
}

int main(int argc, char* argv[]) {
  IC* ic = ic_default_new();
  if (!ic) {
    fprintf(stderr, "Error: Failed to initialize IC context\n");
    return 1;
  }

  int result = 0;
  int use_gpu = 0;
  int use_collapse = 0;
  int thread_count = 1;

  if (argc < 2) {
    test(ic, 0, 0, thread_count);
    goto cleanup;
  }

  const char* command = argv[1];
  if (strcmp(command, "run-gpu") == 0 || strcmp(command, "eval-gpu") == 0 || strcmp(command, "bench-gpu") == 0) {
    use_gpu = 1;
  } else if (strcmp(command, "run") != 0 && strcmp(command, "eval") != 0 && strcmp(command, "bench") != 0) {
    fprintf(stderr, "Error: Unknown command '%s'\n", command);
    print_usage();
    result = 1;
    goto cleanup;
  }

  if (argc < 3) {
    fprintf(stderr, "Error: No term source specified\n");
    print_usage();
    result = 1;
    goto cleanup;
  }

  // Parse flags
  for (int i = 3; i < argc; i++) {
    if (strcmp(argv[i], "-C") == 0) {
      use_collapse = 1;
    } else if (strcmp(argv[i], "-t") == 0) {
      if (i + 1 < argc) {
        thread_count = atoi(argv[++i]);
        if (thread_count <= 0) {
          fprintf(stderr, "Warning: Invalid thread count '%s', using default (1)\n", argv[i]);
          thread_count = 1;
        }
      } else {
        fprintf(stderr, "Error: -t flag requires a number\n");
        print_usage();
        result = 1;
        goto cleanup;
      }
    } else {
      fprintf(stderr, "Error: Unknown flag '%s'\n", argv[i]);
      print_usage();
      result = 1;
      goto cleanup;
    }
  }

  // Parse term based on command
  Term term;
  if (strcmp(command, "eval") == 0 || strcmp(command, "eval-gpu") == 0) {
    term = parse_string(ic, argv[2]);
  } else { // run, run-gpu, bench, bench-gpu
    term = parse_file(ic, argv[2]);
  }

  // Execute command
  if (strcmp(command, "bench") == 0 || strcmp(command, "bench-gpu") == 0) {
    benchmark_term(ic, term, use_gpu, use_collapse, thread_count);
  } else { // run, run-gpu, eval, eval-gpu
    process_term(ic, term, use_gpu, use_collapse, thread_count);
  }

cleanup:
  ic_free(ic);
  return result;
}
