#include <stdio.h>
#include <cuda_runtime.h>
#include "ic.h"

// Maximum threads per block (hardware constraint)
#define IC_MAX_THREADS_PER_BLOCK 512

// Check if CUDA is available
extern "C" int ic_cuda_available() {
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
__device__ uint8_t* d_subst;                   // Substitution bitmap
__device__ uint32_t d_heap_size_per_thread;
__device__ uint32_t d_stack_size_per_thread;
__device__ uint32_t d_subst_size_per_thread;   // Size of bitmap in bytes
__device__ uint32_t* d_heap_pos_array;          // Per-thread heap positions
__device__ unsigned long long* d_interactions_array; // Per-thread interaction counters

// Device utility functions

// Set the substitution bit for a location
__device__ inline void d_ic_set_subst(uint32_t loc, uint8_t* subst) {
  uint32_t byte_idx = loc / 8;
  uint8_t bit_idx = loc % 8;
  // Use atomicOr for thread safety
  atomicOr(&subst[byte_idx], 1 << bit_idx);
}

// Check if a location is marked as a substitution
__device__ inline bool d_ic_is_subst(uint32_t loc, uint8_t* subst) {
  uint32_t byte_idx = loc / 8;
  uint8_t bit_idx = loc % 8;
  return (subst[byte_idx] & (1 << bit_idx)) != 0;
}

// Clear the substitution bit for a location
__device__ inline void d_ic_clear_subst(uint32_t loc, uint8_t* subst) {
  uint32_t byte_idx = loc / 8;
  uint8_t bit_idx = loc % 8;
  // Use atomicAnd for thread safety
  atomicAnd(&subst[byte_idx], ~(1 << bit_idx));
}

// Create a term with specified tag and value
__device__ inline Term d_ic_make_term(TermTag tag, uint32_t val) {
  return MAKE_TERM(tag, val);
}

// Create a superposition term with the given label
__device__ inline Term d_ic_make_sup(uint8_t label, uint32_t val) {
  return d_ic_make_term((TermTag)(SP0 + (label & 0x3)), val);
}

// Create a collapser X term with the given label
__device__ inline Term d_ic_make_col_x(uint8_t label, uint32_t val) {
  return d_ic_make_term((TermTag)(CX0 + (label & 0x3)), val);
}

// Create a collapser Y term with the given label
__device__ inline Term d_ic_make_col_y(uint8_t label, uint32_t val) {
  return d_ic_make_term((TermTag)(CY0 + (label & 0x3)), val);
}

// Get the label from a term tag
__device__ inline uint8_t d_ic_get_label(TermTag tag) {
  if (tag >= SP0 && tag <= SP3) {
    return tag - SP0;
  } else if (tag >= CX0 && tag <= CY3) {
    return (tag - CX0) & 0x3;
  }
  return 0;
}

// Check if a term tag is a collapser X
__device__ inline bool d_ic_is_col_x(TermTag tag) {
  return (tag >= CX0 && tag <= CX3);
}

// Check if a term tag is a collapser Y
__device__ inline bool d_ic_is_col_y(TermTag tag) {
  return (tag >= CY0 && tag <= CY3);
}

// Check if a term tag is a superposition
__device__ inline bool d_ic_is_sup(TermTag tag) {
  return (tag >= SP0 && tag <= SP3);
}

// Allocate n consecutive terms in the thread's local heap
__device__ inline uint32_t d_ic_alloc(uint32_t n, uint32_t* heap_pos) {
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
__device__ inline Term d_ic_app_lam(Term app, Term lam, Term* heap, uint8_t* subst, unsigned long long* interactions) {
  atomicAdd(interactions, 1ULL);
  uint32_t app_loc = TERM_VAL(app);
  uint32_t lam_loc = TERM_VAL(lam);
  Term arg = heap[app_loc + 1];
  Term bod = heap[lam_loc + 0];
  heap[lam_loc] = arg;
  d_ic_set_subst(lam_loc, subst);
  return bod;
}

// Apply a superposition
__device__ inline Term d_ic_app_sup(Term app, Term sup, Term* heap, uint8_t* subst, uint32_t* heap_pos, unsigned long long* interactions) {
  atomicAdd(interactions, 1ULL);
  uint32_t app_loc = TERM_VAL(app);
  uint32_t sup_loc = TERM_VAL(sup);
  TermTag sup_tag = TERM_TAG(sup);
  uint8_t sup_lab = d_ic_get_label(sup_tag);
  Term arg = heap[app_loc + 1];
  Term lft = heap[sup_loc + 0];
  Term rgt = heap[sup_loc + 1];

  uint32_t col_loc = d_ic_alloc(1, heap_pos);
  uint32_t app1_loc = d_ic_alloc(2, heap_pos);
  heap[col_loc] = arg;

  Term x0 = d_ic_make_col_x(sup_lab, col_loc);
  Term x1 = d_ic_make_col_y(sup_lab, col_loc);

  heap[sup_loc + 1] = x0; // Reuse sup_loc as app0_loc
  heap[app1_loc + 0] = rgt;
  heap[app1_loc + 1] = x1;

  heap[app_loc + 0] = d_ic_make_term(APP, sup_loc);
  heap[app_loc + 1] = d_ic_make_term(APP, app1_loc);
  return d_ic_make_sup(sup_lab, app_loc);
}

// Collapse a lambda
__device__ inline Term d_ic_col_lam(Term col, Term lam, Term* heap, uint8_t* subst, uint32_t* heap_pos, unsigned long long* interactions) {
  atomicAdd(interactions, 1ULL);
  uint32_t col_loc = TERM_VAL(col);
  uint32_t lam_loc = TERM_VAL(lam);
  TermTag col_tag = TERM_TAG(col);
  uint8_t col_lab = d_ic_get_label(col_tag);
  bool is_col_x = d_ic_is_col_x(col_tag);
  Term bod = heap[lam_loc + 0];

  uint32_t alloc_start = d_ic_alloc(5, heap_pos);
  uint32_t lam0_loc = alloc_start;
  uint32_t lam1_loc = alloc_start + 1;
  uint32_t sup_loc = alloc_start + 2;
  uint32_t col_new_loc = alloc_start + 4;

  heap[sup_loc + 0] = d_ic_make_term(VAR, lam0_loc);
  heap[sup_loc + 1] = d_ic_make_term(VAR, lam1_loc);
  heap[lam_loc] = d_ic_make_sup(col_lab, sup_loc);
  d_ic_set_subst(lam_loc, subst);
  heap[col_new_loc] = bod;
  heap[lam0_loc] = d_ic_make_col_x(col_lab, col_new_loc);
  heap[lam1_loc] = d_ic_make_col_y(col_lab, col_new_loc);

  if (is_col_x) {
    heap[col_loc] = d_ic_make_term(LAM, lam1_loc);
    d_ic_set_subst(col_loc, subst);
    return d_ic_make_term(LAM, lam0_loc);
  } else {
    heap[col_loc] = d_ic_make_term(LAM, lam0_loc);
    d_ic_set_subst(col_loc, subst);
    return d_ic_make_term(LAM, lam1_loc);
  }
}

// Collapse a superposition
__device__ inline Term d_ic_col_sup(Term col, Term sup, Term* heap, uint8_t* subst, uint32_t* heap_pos, unsigned long long* interactions) {
  atomicAdd(interactions, 1ULL);
  uint32_t col_loc = TERM_VAL(col);
  uint32_t sup_loc = TERM_VAL(sup);
  TermTag col_tag = TERM_TAG(col);
  TermTag sup_tag = TERM_TAG(sup);
  uint8_t col_lab = d_ic_get_label(col_tag);
  uint8_t sup_lab = d_ic_get_label(sup_tag);
  bool is_col_x = d_ic_is_col_x(col_tag);
  Term lft = heap[sup_loc + 0];
  Term rgt = heap[sup_loc + 1];

  if (col_lab == sup_lab) {
    if (is_col_x) {
      heap[col_loc] = rgt;
      d_ic_set_subst(col_loc, subst);
      return lft;
    } else {
      heap[col_loc] = lft;
      d_ic_set_subst(col_loc, subst);
      return rgt;
    }
  } else {
    uint32_t sup_start = d_ic_alloc(4, heap_pos);
    uint32_t sup0_loc = sup_start;
    uint32_t sup1_loc = sup_start + 2;

    heap[sup0_loc + 0] = d_ic_make_col_x(col_lab, sup_loc + 0);
    heap[sup0_loc + 1] = d_ic_make_col_x(col_lab, sup_loc + 1);
    heap[sup1_loc + 0] = d_ic_make_col_y(col_lab, sup_loc + 0);
    heap[sup1_loc + 1] = d_ic_make_col_y(col_lab, sup_loc + 1);
    heap[sup_loc + 0] = lft;
    heap[sup_loc + 1] = rgt;

    if (is_col_x) {
      heap[col_loc] = d_ic_make_sup(sup_lab, sup1_loc);
      d_ic_set_subst(col_loc, subst);
      return d_ic_make_sup(sup_lab, sup0_loc);
    } else {
      heap[col_loc] = d_ic_make_sup(sup_lab, sup0_loc);
      d_ic_set_subst(col_loc, subst);
      return d_ic_make_sup(sup_lab, sup1_loc);
    }
  }
}

// Reduce a term to WHNF (Weak Head Normal Form)
__device__ inline Term d_ic_whnf(Term term, Term* heap, uint8_t* subst, Term* stack, uint32_t* heap_pos, uint32_t* stack_pos, unsigned long long* interactions) {
  uint32_t stop = *stack_pos;
  Term next = term;
  uint32_t local_stack_pos = stop;

  while (true) {
    TermTag tag = TERM_TAG(next);
    switch (tag) {
      case VAR: {
        uint32_t var_loc = TERM_VAL(next);
        if (d_ic_is_subst(var_loc, subst)) {
          next = heap[var_loc];
          continue;
        }
        break;
      }
      case CX0:
      case CY0:
      case CX1:
      case CY1:
      case CX2:
      case CY2:
      case CX3:
      case CY3: {
        uint32_t col_loc = TERM_VAL(next);
        if (d_ic_is_subst(col_loc, subst)) {
          next = heap[col_loc];
          continue;
        } else {
          stack[local_stack_pos++] = next;
          next = heap[col_loc];
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
          next = d_ic_app_lam(prev, next, heap, subst, interactions);
          continue;
        } else if (ptag == APP && (tag >= SP0 && tag <= SP3)) {
          next = d_ic_app_sup(prev, next, heap, subst, heap_pos, interactions);
          continue;
        } else if ((ptag >= CX0 && ptag <= CY3) && tag == LAM) {
          next = d_ic_col_lam(prev, next, heap, subst, heap_pos, interactions);
          continue;
        } else if ((ptag >= CX0 && ptag <= CY3) && (tag >= SP0 && tag <= SP3)) {
          next = d_ic_col_sup(prev, next, heap, subst, heap_pos, interactions);
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
      if (htag == APP || (htag >= CX0 && htag <= CY3)) {
        heap[hloc] = next;
      }
      next = host;
    }
    *stack_pos = local_stack_pos;
    return next;
  }
}

// Reduce a term to normal form
__device__ inline Term d_ic_normal(Term term, Term* heap, uint8_t* subst, Term* stack, uint32_t* heap_pos, uint32_t* stack_pos, unsigned long long* interactions) {
  *stack_pos = 0;
  uint32_t local_stack_pos = 0;
  uint32_t root_loc = d_ic_alloc(1, heap_pos);
  heap[root_loc] = term;
  stack[local_stack_pos++] = d_ic_make_term(VAR, root_loc);

  while (local_stack_pos > 0) {
    uint32_t loc = TERM_VAL(stack[--local_stack_pos]);
    Term current = heap[loc];
    current = d_ic_whnf(current, heap, subst, stack, heap_pos, &local_stack_pos, interactions);
    heap[loc] = current;
    TermTag tag = TERM_TAG(current);
    uint32_t val = TERM_VAL(current);
    if (tag == LAM) {
      stack[local_stack_pos++] = d_ic_make_term(VAR, val);
    } else if (tag == APP || (tag >= SP0 && tag <= SP3)) {
      stack[local_stack_pos++] = d_ic_make_term(VAR, val);
      stack[local_stack_pos++] = d_ic_make_term(VAR, val + 1);
    }
  }
  *stack_pos = local_stack_pos;
  return heap[root_loc];
}

// CUDA kernel for normalization
__global__ void normalizeKernel(int N, uint32_t initial_size) {
  int tid = threadIdx.x + blockIdx.x * blockDim.x;
  if (tid >= N) return;

  // Local heap, stack, and substitution bitmap for this thread
  Term* local_heap = d_heap + (tid * d_heap_size_per_thread);
  Term* local_stack = d_stack + (tid * d_stack_size_per_thread);
  uint8_t* local_subst = d_subst + (tid * d_subst_size_per_thread);
  uint32_t* local_heap_pos = &d_heap_pos_array[tid];
  uint32_t local_stack_pos = 0;
  unsigned long long* local_interactions = &d_interactions_array[tid];

  // Initialize substitution bitmap to zeros
  for (uint32_t i = 0; i < d_subst_size_per_thread; i++) {
    local_subst[i] = 0;
  }

  // Copy initial term to local heap
  for (uint32_t i = 0; i < initial_size; i++) {
    Term original = d_heap[i];
    TermTag tag = TERM_TAG(original);
    uint32_t val = TERM_VAL(original);
    local_heap[i] = d_ic_make_term(tag, val);
  }
  *local_heap_pos = initial_size;

  // Normalize the term
  Term term = local_heap[0];
  term = d_ic_normal(term, local_heap, local_subst, local_stack, local_heap_pos, &local_stack_pos, local_interactions);
  local_heap[0] = term;
}

// Host function to normalize a term on the GPU
extern "C" Term ic_normal_cuda(IC* ic, Term term, int thread_count) {
  const size_t TOTAL_BUFFER_SIZE = 22ULL * 1024ULL * 1024ULL * 1024ULL; // 22 GB
  const size_t HEAP_PORTION = 16ULL * 1024ULL * 1024ULL * 1024ULL; // 16 GB for heaps
  const size_t STACK_PORTION = 4ULL * 1024ULL * 1024ULL * 1024ULL; // 4 GB for stacks
  const size_t SUBST_PORTION = 2ULL * 1024ULL * 1024ULL * 1024ULL; // 2 GB for substitution bitmaps
  
  int N = (thread_count > 0) ? thread_count : 1;
  
  const size_t HEAP_SIZE_PER_THREAD_BYTES = HEAP_PORTION / N;
  const size_t STACK_SIZE_PER_THREAD_BYTES = STACK_PORTION / N;
  const size_t SUBST_SIZE_PER_THREAD_BYTES = SUBST_PORTION / N;
  
  const uint32_t heap_size_per_thread = HEAP_SIZE_PER_THREAD_BYTES / sizeof(Term);
  const uint32_t stack_size_per_thread = STACK_SIZE_PER_THREAD_BYTES / sizeof(Term);
  const uint32_t subst_size_per_thread = SUBST_SIZE_PER_THREAD_BYTES / sizeof(uint8_t);

  // Display memory allocation info for debug purposes
  printf("Memory Config: Total: %.2f GB (Heap: %.2f GB, Stack: %.2f GB, Subst: %.2f GB)\n", 
         TOTAL_BUFFER_SIZE / (1024.0 * 1024.0 * 1024.0),
         HEAP_PORTION / (1024.0 * 1024.0 * 1024.0),
         STACK_PORTION / (1024.0 * 1024.0 * 1024.0),
         SUBST_PORTION / (1024.0 * 1024.0 * 1024.0));
  printf("Per Thread: Heap: %.2f MB, Stack: %.2f MB, Subst: %.2f MB\n",
         HEAP_SIZE_PER_THREAD_BYTES / (1024.0 * 1024.0),
         STACK_SIZE_PER_THREAD_BYTES / (1024.0 * 1024.0),
         SUBST_SIZE_PER_THREAD_BYTES / (1024.0 * 1024.0));

  // Device memory allocation
  Term *d_heap_ptr, *d_stack_ptr;
  uint8_t *d_subst_ptr;
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

  err = cudaMalloc(&d_subst_ptr, SUBST_PORTION);
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (subst): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    return term;
  }

  err = cudaMalloc(&d_heap_pos_array_ptr, N * sizeof(uint32_t));
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (heap_pos): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    cudaFree(d_subst_ptr);
    return term;
  }

  err = cudaMalloc(&d_interactions_array_ptr, N * sizeof(unsigned long long));
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (interactions): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    cudaFree(d_subst_ptr);
    cudaFree(d_heap_pos_array_ptr);
    return term;
  }

  // Copy initial heap to device (temporary buffer for kernel to distribute)
  err = cudaMemcpy(d_heap_ptr, ic->heap, ic->heap_pos * sizeof(Term), cudaMemcpyHostToDevice);
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (copy to device): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    cudaFree(d_subst_ptr);
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
  cudaMemcpyToSymbol(d_subst, &d_subst_ptr, sizeof(uint8_t*));
  cudaMemcpyToSymbol(d_heap_size_per_thread, &heap_size_per_thread, sizeof(uint32_t));
  cudaMemcpyToSymbol(d_stack_size_per_thread, &stack_size_per_thread, sizeof(uint32_t));
  cudaMemcpyToSymbol(d_subst_size_per_thread, &subst_size_per_thread, sizeof(uint32_t));
  cudaMemcpyToSymbol(d_heap_pos_array, &d_heap_pos_array_ptr, sizeof(uint32_t*));
  cudaMemcpyToSymbol(d_interactions_array, &d_interactions_array_ptr, sizeof(unsigned long long*));

  // Launch kernel
  int blocks = (N + IC_MAX_THREADS_PER_BLOCK - 1) / IC_MAX_THREADS_PER_BLOCK;
  int threads_per_block = (N < IC_MAX_THREADS_PER_BLOCK) ? N : IC_MAX_THREADS_PER_BLOCK;
  printf("CUDA Configuration: %d thread(s) total, %d block(s), %d thread(s) per block\n", N, blocks, threads_per_block);
  normalizeKernel<<<blocks, threads_per_block>>>(N, ic->heap_pos);
  cudaDeviceSynchronize();

  err = cudaGetLastError();
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Kernel Error: %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    cudaFree(d_subst_ptr);
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
  err = cudaMemcpy(ic->heap, d_heap_ptr, heap_pos_thread0 * sizeof(Term), cudaMemcpyDeviceToHost);
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (copy from device): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    cudaFree(d_subst_ptr);
    cudaFree(d_heap_pos_array_ptr);
    cudaFree(d_interactions_array_ptr);
    free(h_heap_pos_array);
    free(h_interactions_array);
    return term;
  }

  ic->heap_pos = heap_pos_thread0;
  ic->interactions = total_interactions;

  cudaFree(d_heap_ptr);
  cudaFree(d_stack_ptr);
  cudaFree(d_subst_ptr);
  cudaFree(d_heap_pos_array_ptr);
  cudaFree(d_interactions_array_ptr);
  free(h_heap_pos_array);
  free(h_interactions_array);

  return ic->heap[0];
}
