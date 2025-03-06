#include <stdio.h>
#include <cuda_runtime.h>
#include "hvmn.h"

// Maximum threads per block (hardware constraint)
#define HVMN_MAX_THREADS_PER_BLOCK 512

// Check if CUDA is available
extern "C" int hvmn_cuda_available() {
  int deviceCount = 0;
  cudaError_t error = cudaGetDeviceCount(&deviceCount);
  if (error != cudaSuccess) {
    return 0; // CUDA error occurred
  }
  return deviceCount > 0;
}

// Device memory pointers and sizes
__device__ Term* d_heap;
__device__ Term* d_stack;
__device__ uint32_t d_heap_size_per_thread;
__device__ uint32_t d_stack_size_per_thread;
__device__ uint32_t* d_heap_pos_array;          // Per-thread heap positions
__device__ unsigned long long* d_interactions_array; // Per-thread interaction counters

// Device utility functions

// Create a term with substitution bit
__device__ inline Term d_hvmn_make_sub(Term term) {
  return term | TERM_SUB_MASK;
}

// Remove substitution bit from a term
__device__ inline Term d_hvmn_clear_sub(Term term) {
  return term & ~TERM_SUB_MASK;
}

// Create a term with specified tag, label, and value
__device__ inline Term d_hvmn_make_term(bool sub, TermTag tag, uint8_t lab, uint32_t val) {
  return MAKE_TERM(sub, tag, lab, val);
}

// Allocate n consecutive terms in the thread's local heap
__device__ inline uint32_t d_hvmn_alloc(uint32_t n, uint32_t* heap_pos) {
  if (*heap_pos + n > d_heap_size_per_thread) {
    // Handle overflow - return current position without updating
    // This prevents illegal memory access but may cause computation errors
    return *heap_pos;
  }
  
  uint32_t ptr = *heap_pos;
  *heap_pos += n;
  return ptr;
}

// Apply a lambda to an argument
__device__ inline Term d_hvmn_app_lam(Term app, Term lam, Term* heap, unsigned long long* interactions) {
  atomicAdd(interactions, 1ULL);
  uint32_t app_loc = TERM_VAL(app);
  uint32_t lam_loc = TERM_VAL(lam);
  Term arg = heap[app_loc + 1];
  Term bod = heap[lam_loc + 0];
  heap[lam_loc] = d_hvmn_make_sub(arg);
  return bod;
}

// Apply a superposition
__device__ inline Term d_hvmn_app_sup(Term app, Term sup, Term* heap, uint32_t* heap_pos, unsigned long long* interactions) {
  atomicAdd(interactions, 1ULL);
  uint32_t app_loc = TERM_VAL(app);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);
  Term arg = heap[app_loc + 1];
  Term lft = heap[sup_loc + 0];
  Term rgt = heap[sup_loc + 1];

  uint32_t col_loc = d_hvmn_alloc(1, heap_pos);
  uint32_t app1_loc = d_hvmn_alloc(2, heap_pos);
  heap[col_loc] = arg;

  Term x0 = d_hvmn_make_term(false, CO0, sup_lab, col_loc);
  Term x1 = d_hvmn_make_term(false, CO1, sup_lab, col_loc);

  heap[sup_loc + 1] = x0; // Reuse sup_loc as app0_loc
  heap[app1_loc + 0] = rgt;
  heap[app1_loc + 1] = x1;

  heap[app_loc + 0] = d_hvmn_make_term(false, APP, 0, sup_loc);
  heap[app_loc + 1] = d_hvmn_make_term(false, APP, 0, app1_loc);
  return d_hvmn_make_term(false, SUP, sup_lab, app_loc);
}

// Collapse a lambda
__device__ inline Term d_hvmn_col_lam(Term col, Term lam, Term* heap, uint32_t* heap_pos, unsigned long long* interactions) {
  atomicAdd(interactions, 1ULL);
  uint32_t col_loc = TERM_VAL(col);
  uint32_t lam_loc = TERM_VAL(lam);
  uint8_t col_lab = TERM_LAB(col);
  bool is_co0 = (TERM_TAG(col) == CO0);
  Term bod = heap[lam_loc + 0];

  uint32_t alloc_start = d_hvmn_alloc(5, heap_pos);
  uint32_t lam0_loc = alloc_start;
  uint32_t lam1_loc = alloc_start + 1;
  uint32_t sup_loc = alloc_start + 2;
  uint32_t col_new_loc = alloc_start + 4;

  heap[sup_loc + 0] = d_hvmn_make_term(false, VAR, 0, lam0_loc);
  heap[sup_loc + 1] = d_hvmn_make_term(false, VAR, 0, lam1_loc);
  heap[lam_loc] = d_hvmn_make_sub(d_hvmn_make_term(false, SUP, col_lab, sup_loc));
  heap[col_new_loc] = bod;
  heap[lam0_loc] = d_hvmn_make_term(false, CO0, col_lab, col_new_loc);
  heap[lam1_loc] = d_hvmn_make_term(false, CO1, col_lab, col_new_loc);

  if (is_co0) {
    heap[col_loc] = d_hvmn_make_sub(d_hvmn_make_term(false, LAM, 0, lam1_loc));
    return d_hvmn_make_term(false, LAM, 0, lam0_loc);
  } else {
    heap[col_loc] = d_hvmn_make_sub(d_hvmn_make_term(false, LAM, 0, lam0_loc));
    return d_hvmn_make_term(false, LAM, 0, lam1_loc);
  }
}

// Collapse a superposition
__device__ inline Term d_hvmn_col_sup(Term col, Term sup, Term* heap, uint32_t* heap_pos, unsigned long long* interactions) {
  atomicAdd(interactions, 1ULL);
  uint32_t col_loc = TERM_VAL(col);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t col_lab = TERM_LAB(col);
  uint8_t sup_lab = TERM_LAB(sup);
  bool is_co0 = (TERM_TAG(col) == CO0);
  Term lft = heap[sup_loc + 0];
  Term rgt = heap[sup_loc + 1];

  if (col_lab == sup_lab) {
    if (is_co0) {
      heap[col_loc] = d_hvmn_make_sub(rgt);
      return lft;
    } else {
      heap[col_loc] = d_hvmn_make_sub(lft);
      return rgt;
    }
  } else {
    uint32_t sup_start = d_hvmn_alloc(4, heap_pos);
    uint32_t sup0_loc = sup_start;
    uint32_t sup1_loc = sup_start + 2;

    heap[sup0_loc + 0] = d_hvmn_make_term(false, CO0, col_lab, sup_loc + 0);
    heap[sup0_loc + 1] = d_hvmn_make_term(false, CO0, col_lab, sup_loc + 1);
    heap[sup1_loc + 0] = d_hvmn_make_term(false, CO1, col_lab, sup_loc + 0);
    heap[sup1_loc + 1] = d_hvmn_make_term(false, CO1, col_lab, sup_loc + 1);
    heap[sup_loc + 0] = lft;
    heap[sup_loc + 1] = rgt;

    if (is_co0) {
      heap[col_loc] = d_hvmn_make_sub(d_hvmn_make_term(false, SUP, sup_lab, sup1_loc));
      return d_hvmn_make_term(false, SUP, sup_lab, sup0_loc);
    } else {
      heap[col_loc] = d_hvmn_make_sub(d_hvmn_make_term(false, SUP, sup_lab, sup0_loc));
      return d_hvmn_make_term(false, SUP, sup_lab, sup1_loc);
    }
  }
}

// Reduce a term to WHNF (Weak Head Normal Form)
__device__ inline Term d_hvmn_whnf(Term term, Term* heap, Term* stack, uint32_t* heap_pos, uint32_t* stack_pos, unsigned long long* interactions) {
  uint32_t stop = *stack_pos;
  Term next = term;
  uint32_t local_stack_pos = stop;

  while (true) {
    TermTag tag = TERM_TAG(next);
    switch (tag) {
      case VAR: {
        uint32_t var_loc = TERM_VAL(next);
        Term subst = heap[var_loc];
        if (TERM_SUB(subst)) {
          next = d_hvmn_clear_sub(subst);
          continue;
        }
        break;
      }
      case CO0:
      case CO1: {
        uint32_t col_loc = TERM_VAL(next);
        Term val = heap[col_loc];
        if (TERM_SUB(val)) {
          next = d_hvmn_clear_sub(val);
          continue;
        } else {
          stack[local_stack_pos++] = next;
          next = val;
          continue;
        }
      }
      case APP: {
        uint32_t app_loc = TERM_VAL(next);
        stack[local_stack_pos++] = next;
        next = heap[app_loc];
        continue;
      }
      default: {
        if (local_stack_pos == stop) {
          *stack_pos = local_stack_pos;
          return next;
        }
        Term prev = stack[--local_stack_pos];
        TermTag ptag = TERM_TAG(prev);
        if (ptag == APP && tag == LAM) {
          next = d_hvmn_app_lam(prev, next, heap, interactions);
          continue;
        } else if (ptag == APP && tag == SUP) {
          next = d_hvmn_app_sup(prev, next, heap, heap_pos, interactions);
          continue;
        } else if ((ptag == CO0 || ptag == CO1) && tag == LAM) {
          next = d_hvmn_col_lam(prev, next, heap, heap_pos, interactions);
          continue;
        } else if ((ptag == CO0 || ptag == CO1) && tag == SUP) {
          next = d_hvmn_col_sup(prev, next, heap, heap_pos, interactions);
          continue;
        }
        stack[local_stack_pos++] = prev;
        break;
      }
    }
    if (local_stack_pos == stop) {
      *stack_pos = local_stack_pos;
      return next;
    }
    while (local_stack_pos > stop) {
      Term host = stack[--local_stack_pos];
      TermTag htag = TERM_TAG(host);
      uint32_t hloc = TERM_VAL(host);
      if (htag == APP || htag == CO0 || htag == CO1) {
        heap[hloc] = next;
      }
      next = host;
    }
    *stack_pos = local_stack_pos;
    return next;
  }
}

// Reduce a term to normal form
__device__ inline Term d_hvmn_normal(Term term, Term* heap, Term* stack, uint32_t* heap_pos, uint32_t* stack_pos, unsigned long long* interactions) {
  *stack_pos = 0;
  uint32_t local_stack_pos = 0;
  uint32_t root_loc = d_hvmn_alloc(1, heap_pos);
  heap[root_loc] = term;
  stack[local_stack_pos++] = d_hvmn_make_term(false, (TermTag)0, 0, root_loc);

  while (local_stack_pos > 0) {
    uint32_t loc = TERM_VAL(stack[--local_stack_pos]);
    Term current = heap[loc];
    current = d_hvmn_whnf(current, heap, stack, heap_pos, &local_stack_pos, interactions);
    heap[loc] = current;
    TermTag tag = TERM_TAG(current);
    uint32_t val = TERM_VAL(current);
    if (tag == LAM) {
      stack[local_stack_pos++] = d_hvmn_make_term(false, (TermTag)0, 0, val);
    } else if (tag == APP || tag == SUP) {
      stack[local_stack_pos++] = d_hvmn_make_term(false, (TermTag)0, 0, val);
      stack[local_stack_pos++] = d_hvmn_make_term(false, (TermTag)0, 0, val + 1);
    }
  }
  *stack_pos = local_stack_pos;
  return heap[root_loc];
}

// CUDA kernel for normalization
__global__ void normalizeKernel(int N, uint32_t initial_size) {
  int tid = threadIdx.x + blockIdx.x * blockDim.x;
  if (tid >= N) return;

  // Local heap and stack for this thread
  Term* local_heap = d_heap + (tid * d_heap_size_per_thread);
  Term* local_stack = d_stack + (tid * d_stack_size_per_thread);
  uint32_t* local_heap_pos = &d_heap_pos_array[tid];
  uint32_t local_stack_pos = 0;
  unsigned long long* local_interactions = &d_interactions_array[tid];

  // Copy initial term to local heap
  for (uint32_t i = 0; i < initial_size; i++) {
    Term original = d_heap[i];
    bool sub = TERM_SUB(original);
    TermTag tag = TERM_TAG(original);
    uint8_t lab = TERM_LAB(original);
    uint32_t val = TERM_VAL(original);
    local_heap[i] = d_hvmn_make_term(sub, tag, lab, val);
  }
  *local_heap_pos = initial_size;

  // Normalize the term
  Term term = local_heap[0];
  term = d_hvmn_normal(term, local_heap, local_stack, local_heap_pos, &local_stack_pos, local_interactions);
  local_heap[0] = term;
}

// Host function to normalize a term on the GPU
extern "C" Term hvmn_normal_cuda(HVMN* hvmn, Term term, int thread_count) {
  const size_t TOTAL_BUFFER_SIZE = 22ULL * 1024ULL * 1024ULL * 1024ULL; // 22 GB
  const size_t HEAP_PORTION = 18ULL * 1024ULL * 1024ULL * 1024ULL; // 18 GB for heaps
  const size_t STACK_PORTION = 4ULL * 1024ULL * 1024ULL * 1024ULL; // 4 GB for stacks
  
  int N = (thread_count > 0) ? thread_count : 1;
  
  const size_t HEAP_SIZE_PER_THREAD_BYTES = HEAP_PORTION / N;
  const size_t STACK_SIZE_PER_THREAD_BYTES = STACK_PORTION / N;
  
  const uint32_t heap_size_per_thread = HEAP_SIZE_PER_THREAD_BYTES / sizeof(Term);
  const uint32_t stack_size_per_thread = STACK_SIZE_PER_THREAD_BYTES / sizeof(Term);

  // Display memory allocation info for debug purposes
  printf("Memory Config: Total: %.2f GB (Heap: %.2f GB, Stack: %.2f GB)\n", 
         TOTAL_BUFFER_SIZE / (1024.0 * 1024.0 * 1024.0),
         HEAP_PORTION / (1024.0 * 1024.0 * 1024.0),
         STACK_PORTION / (1024.0 * 1024.0 * 1024.0));
  printf("Per Thread: Heap: %.2f MB, Stack: %.2f MB\n",
         HEAP_SIZE_PER_THREAD_BYTES / (1024.0 * 1024.0),
         STACK_SIZE_PER_THREAD_BYTES / (1024.0 * 1024.0));

  // Device memory allocation
  Term *d_heap_ptr, *d_stack_ptr;
  uint32_t* d_heap_pos_array_ptr;
  unsigned long long* d_interactions_array_ptr;
  cudaError_t err;

  err = cudaMalloc(&d_heap_ptr, HEAP_PORTION);
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (heap): %s\n", cudaGetErrorString(err));
    return term;
  }

  err = cudaMalloc(&d_stack_ptr, STACK_PORTION);
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (stack): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    return term;
  }

  err = cudaMalloc(&d_heap_pos_array_ptr, N * sizeof(uint32_t));
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (heap_pos): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    return term;
  }

  err = cudaMalloc(&d_interactions_array_ptr, N * sizeof(unsigned long long));
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (interactions): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    cudaFree(d_heap_pos_array_ptr);
    return term;
  }

  // Copy initial heap to device (temporary buffer for kernel to distribute)
  err = cudaMemcpy(d_heap_ptr, hvmn->heap, hvmn->heap_pos * sizeof(Term), cudaMemcpyHostToDevice);
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (copy to device): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    cudaFree(d_heap_pos_array_ptr);
    cudaFree(d_interactions_array_ptr);
    return term;
  }

  // Initialize arrays
  uint32_t* h_heap_pos_array = (uint32_t*)malloc(N * sizeof(uint32_t));
  unsigned long long* h_interactions_array = (unsigned long long*)malloc(N * sizeof(unsigned long long));
  for (int i = 0; i < N; i++) {
    h_heap_pos_array[i] = 0;
    h_interactions_array[i] = 0;
  }
  cudaMemcpy(d_heap_pos_array_ptr, h_heap_pos_array, N * sizeof(uint32_t), cudaMemcpyHostToDevice);
  cudaMemcpy(d_interactions_array_ptr, h_interactions_array, N * sizeof(unsigned long long), cudaMemcpyHostToDevice);

  // Set device symbols
  cudaMemcpyToSymbol(d_heap, &d_heap_ptr, sizeof(Term*));
  cudaMemcpyToSymbol(d_stack, &d_stack_ptr, sizeof(Term*));
  cudaMemcpyToSymbol(d_heap_size_per_thread, &heap_size_per_thread, sizeof(uint32_t));
  cudaMemcpyToSymbol(d_stack_size_per_thread, &stack_size_per_thread, sizeof(uint32_t));
  cudaMemcpyToSymbol(d_heap_pos_array, &d_heap_pos_array_ptr, sizeof(uint32_t*));
  cudaMemcpyToSymbol(d_interactions_array, &d_interactions_array_ptr, sizeof(unsigned long long*));

  // Launch kernel
  int blocks = (N + HVMN_MAX_THREADS_PER_BLOCK - 1) / HVMN_MAX_THREADS_PER_BLOCK;
  int threads_per_block = (N < HVMN_MAX_THREADS_PER_BLOCK) ? N : HVMN_MAX_THREADS_PER_BLOCK;
  printf("CUDA Configuration: %d thread(s) total, %d block(s), %d thread(s) per block\n", N, blocks, threads_per_block);
  normalizeKernel<<<blocks, threads_per_block>>>(N, hvmn->heap_pos);
  cudaDeviceSynchronize();

  err = cudaGetLastError();
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Kernel Error: %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    cudaFree(d_heap_pos_array_ptr);
    cudaFree(d_interactions_array_ptr);
    free(h_heap_pos_array);
    free(h_interactions_array);
    return term;
  }

  // Copy results back
  cudaMemcpy(h_heap_pos_array, d_heap_pos_array_ptr, N * sizeof(uint32_t), cudaMemcpyDeviceToHost);
  cudaMemcpy(h_interactions_array, d_interactions_array_ptr, N * sizeof(unsigned long long), cudaMemcpyDeviceToHost);

  uint64_t total_interactions = 0;
  for (int i = 0; i < N; i++) {
    total_interactions += h_interactions_array[i];
  }
  uint32_t heap_pos_thread0 = h_heap_pos_array[0];

  // Only copy back the result from the first thread
  err = cudaMemcpy(hvmn->heap, d_heap_ptr, heap_pos_thread0 * sizeof(Term), cudaMemcpyDeviceToHost);
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (copy from device): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    cudaFree(d_heap_pos_array_ptr);
    cudaFree(d_interactions_array_ptr);
    free(h_heap_pos_array);
    free(h_interactions_array);
    return term;
  }

  hvmn->heap_pos = heap_pos_thread0;
  hvmn->interactions = total_interactions;

  cudaFree(d_heap_ptr);
  cudaFree(d_stack_ptr);
  cudaFree(d_heap_pos_array_ptr);
  cudaFree(d_interactions_array_ptr);
  free(h_heap_pos_array);
  free(h_interactions_array);

  return hvmn->heap[0];
}
