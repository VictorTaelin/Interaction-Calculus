/*

(old implementation omitted)

PROBLEM: the file above is working perfectly, but, it is only running the
runtime in a single-threaded fashion. Our goal is to update this implementation
in order to run it with N threads instead. To do so, we must cudaMalloc a buffer
of 16 GB exactly (matching the target GPU memory), and, then, split it into N
buffers of 16/N GB, one for each thread. We must also copy the initial term
(i.e., the slice of the original IC object, from 0 to size), to each thread IC,
making sure each receives the same identical term. This can be done with a
kernel, where each thread copies the initial term into its own IC object. Note
that each thread will also keep its own interaction counter, which will be added
at the end to compute the total interaction count. Also, make sure to split the
stack among threads too. Ideally, the stack and the heap size should be the same,
i.e., for N=16 threads, we'd have 1 GB per thread, with a 512 MB heap and a 512
MB stack.

Refactor this file whole to make sure it uses N threads now.
Keep everything else the same.
Pay extra attention to avoid errors.
Do it now.
*/

#include <stdio.h>
#include <cuda_runtime.h>
#include "ic.h"

// Check if CUDA is available
extern "C" int ic_cuda_available() {
  int deviceCount = 0;
  cudaError_t error = cudaGetDeviceCount(&deviceCount);
  
  if (error != cudaSuccess) {
    return 0; // CUDA error occurred
  }
  
  return deviceCount > 0;
}

// Device memory for heap, stack, and counters
__device__ Term* d_heap;
__device__ Term* d_stack;
__device__ uint32_t d_heap_size_per_thread;
__device__ uint32_t d_stack_size_per_thread;
__device__ uint32_t* d_heap_pos_array;  // Per-thread heap positions
__device__ unsigned long long* d_interactions_array;  // Per-thread interaction counters

// Device implementations of IC functions

// Create a term with substitution bit
__device__ inline Term d_ic_make_sub(Term term) {
  return term | TERM_SUB_MASK;
}

// Remove substitution bit from a term
__device__ inline Term d_ic_clear_sub(Term term) {
  return term & ~TERM_SUB_MASK;
}

// Create a term with specified tag, label, and value
__device__ inline Term d_ic_make_term(TermTag tag, uint8_t lab, uint32_t val) {
  return MAKE_TERM(false, tag, lab, val);
}

// Allocate n consecutive terms in memory (thread-safe per thread)
__device__ inline uint32_t d_ic_alloc(uint32_t n, int tid) {
  uint32_t ptr = d_heap_pos_array[tid];
  d_heap_pos_array[tid] += n;
  
  // Check if we've run out of memory for this thread
  if (d_heap_pos_array[tid] >= (tid + 1) * d_heap_size_per_thread) {
    // Wrap around within thread's heap segment (simplified error handling)
    d_heap_pos_array[tid] = (tid + 1) * d_heap_size_per_thread - 1;
  }
  
  return ptr;
}

// Apply a lambda to an argument
__device__ inline Term d_ic_app_lam(Term app, Term lam, int tid) {
  atomicAdd(&d_interactions_array[tid], 1ULL);
  
  uint32_t app_loc = TERM_VAL(app);
  uint32_t lam_loc = TERM_VAL(lam);
  
  Term arg = d_heap[app_loc + 1];
  Term bod = d_heap[lam_loc + 0];

  // Create substitution for the lambda variable
  d_heap[lam_loc] = d_ic_make_sub(arg);

  return bod;
}

// Apply a superposition
__device__ inline Term d_ic_app_sup(Term app, Term sup, int tid) {
  atomicAdd(&d_interactions_array[tid], 1ULL);
  
  uint32_t app_loc = TERM_VAL(app);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);

  Term arg = d_heap[app_loc + 1];
  Term lft = d_heap[sup_loc + 0];
  Term rgt = d_heap[sup_loc + 1];

  // Allocate within thread's heap segment
  uint32_t col_loc = d_ic_alloc(1, tid);
  uint32_t app1_loc = d_ic_alloc(2, tid);
  
  // Adjust locations to thread's heap segment
  col_loc += tid * d_heap_size_per_thread;
  app1_loc += tid * d_heap_size_per_thread;
  
  // Store the arg in the collapser location
  d_heap[col_loc] = arg;

  // Create CO0 and CO1 terms
  Term x0 = d_ic_make_term(CO0, sup_lab, col_loc);
  Term x1 = d_ic_make_term(CO1, sup_lab, col_loc);

  // Reuse sup_loc for app0 (adjusted for thread)
  uint32_t app0_loc = sup_loc;
  d_heap[app0_loc + 1] = x0; // lft is already in heap[app0_loc + 0]

  // Set up app1
  d_heap[app1_loc + 0] = rgt;
  d_heap[app1_loc + 1] = x1;

  // Reuse app_loc for the result superposition
  d_heap[app_loc + 0] = d_ic_make_term(APP, 0, app0_loc);
  d_heap[app_loc + 1] = d_ic_make_term(APP, 0, app1_loc);

  return d_ic_make_term(SUP, sup_lab, app_loc);
}

// Collapse a lambda
__device__ inline Term d_ic_col_lam(Term col, Term lam, int tid) {
  atomicAdd(&d_interactions_array[tid], 1ULL);
  
  uint32_t col_loc = TERM_VAL(col);
  uint32_t lam_loc = TERM_VAL(lam);
  uint8_t col_lab = TERM_LAB(col);
  uint8_t is_co0 = (TERM_TAG(col) == CO0);

  Term bod = d_heap[lam_loc + 0];

  // Batch allocate memory for efficiency within thread's segment
  uint32_t alloc_start = d_ic_alloc(5, tid);
  alloc_start += tid * d_heap_size_per_thread;
  uint32_t lam0_loc = alloc_start;
  uint32_t lam1_loc = alloc_start + 1;
  uint32_t sup_loc = alloc_start + 2; // 2 locations
  uint32_t col_new_loc = alloc_start + 4;

  // Set up the superposition
  d_heap[sup_loc + 0] = d_ic_make_term(VAR, 0, lam0_loc);
  d_heap[sup_loc + 1] = d_ic_make_term(VAR, 0, lam1_loc);

  // Replace lambda's variable with the superposition
  d_heap[lam_loc] = d_ic_make_sub(d_ic_make_term(SUP, col_lab, sup_loc));

  // Set up the new collapser
  d_heap[col_new_loc] = bod;

  // Set up new lambda bodies
  d_heap[lam0_loc] = d_ic_make_term(CO0, col_lab, col_new_loc);
  d_heap[lam1_loc] = d_ic_make_term(CO1, col_lab, col_new_loc);

  // Create and return the appropriate lambda
  if (is_co0) {
    d_heap[col_loc] = d_ic_make_sub(d_ic_make_term(LAM, 0, lam1_loc));
    return d_ic_make_term(LAM, 0, lam0_loc);
  } else {
    d_heap[col_loc] = d_ic_make_sub(d_ic_make_term(LAM, 0, lam0_loc));
    return d_ic_make_term(LAM, 0, lam1_loc);
  }
}

// Collapse a superposition
__device__ inline Term d_ic_col_sup(Term col, Term sup, int tid) {
  atomicAdd(&d_interactions_array[tid], 1ULL);
  
  uint32_t col_loc = TERM_VAL(col);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t col_lab = TERM_LAB(col);
  uint8_t sup_lab = TERM_LAB(sup);
  uint8_t is_co0 = (TERM_TAG(col) == CO0);

  Term lft = d_heap[sup_loc + 0];
  Term rgt = d_heap[sup_loc + 1];

  if (col_lab == sup_lab) {
    if (is_co0) {
      d_heap[col_loc] = d_ic_make_sub(rgt);
      return lft;
    } else {
      d_heap[col_loc] = d_ic_make_sub(lft);
      return rgt;
    }
  } else {
    // Allocate within thread's segment
    uint32_t sup_start = d_ic_alloc(4, tid);
    sup_start += tid * d_heap_size_per_thread;
    uint32_t sup0_loc = sup_start;
    uint32_t sup1_loc = sup_start + 2;

    // Use existing locations as collapser locations
    uint32_t col_lft_loc = sup_loc + 0;
    uint32_t col_rgt_loc = sup_loc + 1;
    
    // Set up the first superposition (for CO0)
    d_heap[sup0_loc + 0] = d_ic_make_term(CO0, col_lab, col_lft_loc);
    d_heap[sup0_loc + 1] = d_ic_make_term(CO0, col_lab, col_rgt_loc);
    
    // Set up the second superposition (for CO1)
    d_heap[sup1_loc + 0] = d_ic_make_term(CO1, col_lab, col_lft_loc);
    d_heap[sup1_loc + 1] = d_ic_make_term(CO1, col_lab, col_rgt_loc);
    
    // Set up original collapsers to point to lft and rgt
    d_heap[col_lft_loc] = lft;
    d_heap[col_rgt_loc] = rgt;

    if (is_co0) {
      d_heap[col_loc] = d_ic_make_sub(d_ic_make_term(SUP, sup_lab, sup1_loc));
      return d_ic_make_term(SUP, sup_lab, sup0_loc);
    } else {
      d_heap[col_loc] = d_ic_make_sub(d_ic_make_term(SUP, sup_lab, sup0_loc));
      return d_ic_make_term(SUP, sup_lab, sup1_loc);
    }
  }
}

// Reduce a term to WHNF (Weak Head Normal Form) with per-thread stack
__device__ inline Term d_ic_whnf(Term term, Term* stack, uint32_t* stack_pos, int tid) {
  uint32_t stop = *stack_pos;
  Term next = term;
  uint32_t local_stack_pos = stop;

  while (1) {
    TermTag tag = TERM_TAG(next);

    switch (tag) {
      case VAR: {
        uint32_t var_loc = TERM_VAL(next);
        Term subst = d_heap[var_loc];
        if (TERM_SUB(subst)) {
          next = d_ic_clear_sub(subst);
          continue;
        }
        break;
      }

      case CO0:
      case CO1: {
        uint32_t col_loc = TERM_VAL(next);
        Term val = d_heap[col_loc];
        if (TERM_SUB(val)) {
          next = d_ic_clear_sub(val);
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
        next = d_heap[app_loc];
        continue;
      }

      default: {
        if (local_stack_pos == stop) {
          *stack_pos = local_stack_pos;
          return next;
        } else {
          Term prev = stack[--local_stack_pos];
          TermTag ptag = TERM_TAG(prev);
          
          if (ptag == APP && tag == LAM) {
            next = d_ic_app_lam(prev, next, tid);
            continue;
          } 
          else if (ptag == APP && tag == SUP) {
            next = d_ic_app_sup(prev, next, tid);
            continue;
          }
          else if ((ptag == CO0 || ptag == CO1) && tag == LAM) {
            next = d_ic_col_lam(prev, next, tid);
            continue;
          }
          else if ((ptag == CO0 || ptag == CO1) && tag == SUP) {
            next = d_ic_col_sup(prev, next, tid);
            continue;
          }
          
          stack[local_stack_pos++] = prev;
          break;
        }
      }
    }

    if (local_stack_pos == stop) {
      *stack_pos = local_stack_pos;
      return next;
    } else {
      while (local_stack_pos > stop) {
        Term host = stack[--local_stack_pos];
        TermTag htag = TERM_TAG(host);
        uint32_t hloc = TERM_VAL(host);
        
        if (htag == APP || htag == CO0 || htag == CO1) {
          d_heap[hloc] = next;
        }
        next = host;
      }
      *stack_pos = local_stack_pos;
      return next;
    }
  }
}

// Reduce a term to normal form with per-thread stack
__device__ inline Term d_ic_normal(Term term, Term* stack, uint32_t* stack_pos, int tid) {
  // Reset stack
  *stack_pos = 0;
  uint32_t local_stack_pos = 0;

  // Allocate a new node for the initial term within thread's heap
  uint32_t root_loc = d_ic_alloc(1, tid) + tid * d_heap_size_per_thread;
  d_heap[root_loc] = term;

  // Push initial location to stack as a "location"
  stack[local_stack_pos++] = MAKE_TERM(false, 0, 0, root_loc);

  while (local_stack_pos > 0) {
    // Pop current location from stack
    uint32_t loc = TERM_VAL(stack[--local_stack_pos]);

    // Get term at this location
    Term current = d_heap[loc];

    // Reduce to WHNF
    current = d_ic_whnf(current, stack, &local_stack_pos, tid);

    // Store the WHNF term back to the heap
    d_heap[loc] = current;

    // Get term details
    TermTag tag = TERM_TAG(current);
    uint32_t val = TERM_VAL(current);

    // Push subterm locations based on term type
    if (tag == LAM) {
      stack[local_stack_pos++] = MAKE_TERM(false, 0, 0, val);
    }
    else if (tag == APP || tag == SUP) {
      stack[local_stack_pos++] = MAKE_TERM(false, 0, 0, val);
      stack[local_stack_pos++] = MAKE_TERM(false, 0, 0, val + 1);
    }
  }

  *stack_pos = local_stack_pos;
  return d_heap[root_loc];
}

// CUDA kernel to copy initial term and normalize
__global__ void normalizeKernel(int N, uint32_t initial_size) {
  int tid = threadIdx.x;
  if (tid >= N) return;

  // Define thread-specific heap and stack offsets
  uint32_t heap_offset = tid * d_heap_size_per_thread;
  uint32_t stack_offset = tid * d_stack_size_per_thread;
  Term* thread_stack = d_stack + stack_offset;
  uint32_t thread_stack_pos = 0;

  // Copy initial term to thread's heap segment
  for (uint32_t i = 0; i < initial_size; i++) {
    d_heap[heap_offset + i] = d_heap[i];
  }
  d_heap_pos_array[tid] = initial_size;

  // Normalize the term in thread's heap segment
  Term term = d_heap[heap_offset];
  term = d_ic_normal(term, thread_stack, &thread_stack_pos, tid);
  d_heap[heap_offset] = term;
}

// Host function to normalize a term on the GPU with N threads
extern "C" Term ic_normal_cuda(IC* ic, Term term) {
  // Total buffer size: 16 GB
  const size_t TOTAL_BUFFER_SIZE = 16ULL * 1024ULL * 1024ULL * 1024ULL; // 16 GB in bytes
  const int N = 16; // Number of threads (e.g., 16 as suggested)
  const size_t BUFFER_SIZE_PER_THREAD = TOTAL_BUFFER_SIZE / N; // 1 GB per thread
  const size_t HEAP_SIZE_PER_THREAD_BYTES = BUFFER_SIZE_PER_THREAD / 2; // 512 MB heap
  const size_t STACK_SIZE_PER_THREAD_BYTES = BUFFER_SIZE_PER_THREAD / 2; // 512 MB stack
  const uint32_t heap_size_per_thread = HEAP_SIZE_PER_THREAD_BYTES / sizeof(Term);
  const uint32_t stack_size_per_thread = STACK_SIZE_PER_THREAD_BYTES / sizeof(Term);

  // Allocate device memory
  Term* d_heap_ptr;
  Term* d_stack_ptr;
  uint32_t* d_heap_pos_array_ptr;
  unsigned long long* d_interactions_array_ptr;
  
  cudaError_t err;
  err = cudaMalloc((void**)&d_heap_ptr, N * heap_size_per_thread * sizeof(Term));
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (heap allocation): %s\n", cudaGetErrorString(err));
    return term;
  }
  
  err = cudaMalloc((void**)&d_stack_ptr, N * stack_size_per_thread * sizeof(Term));
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (stack allocation): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    return term;
  }
  
  err = cudaMalloc((void**)&d_heap_pos_array_ptr, N * sizeof(uint32_t));
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (heap pos array allocation): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    return term;
  }
  
  err = cudaMalloc((void**)&d_interactions_array_ptr, N * sizeof(unsigned long long));
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (interactions array allocation): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    cudaFree(d_heap_pos_array_ptr);
    return term;
  }

  // Copy initial heap to device (to first thread's segment initially)
  err = cudaMemcpy(d_heap_ptr, ic->heap, ic->heap_pos * sizeof(Term), cudaMemcpyHostToDevice);
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (heap copy to device): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    cudaFree(d_heap_pos_array_ptr);
    cudaFree(d_interactions_array_ptr);
    return term;
  }

  // Initialize heap positions and interaction counters
  uint32_t* h_heap_pos_array = (uint32_t*)malloc(N * sizeof(uint32_t));
  unsigned long long* h_interactions_array = (unsigned long long*)malloc(N * sizeof(unsigned long long));
  for (int i = 0; i < N; i++) {
    h_heap_pos_array[i] = ic->heap_pos;
    h_interactions_array[i] = 0;
  }
  cudaMemcpy(d_heap_pos_array_ptr, h_heap_pos_array, N * sizeof(uint32_t), cudaMemcpyHostToDevice);
  cudaMemcpy(d_interactions_array_ptr, h_interactions_array, N * sizeof(unsigned long long), cudaMemcpyHostToDevice);

  // Set up device constants
  cudaMemcpyToSymbol(d_heap, &d_heap_ptr, sizeof(Term*));
  cudaMemcpyToSymbol(d_stack, &d_stack_ptr, sizeof(Term*));
  cudaMemcpyToSymbol(d_heap_size_per_thread, &heap_size_per_thread, sizeof(uint32_t));
  cudaMemcpyToSymbol(d_stack_size_per_thread, &stack_size_per_thread, sizeof(uint32_t));
  cudaMemcpyToSymbol(d_heap_pos_array, &d_heap_pos_array_ptr, sizeof(uint32_t*));
  cudaMemcpyToSymbol(d_interactions_array, &d_interactions_array_ptr, sizeof(unsigned long long*));

  // Launch kernel with N threads
  normalizeKernel<<<1, N>>>(N, ic->heap_pos);

  // Wait for kernel to complete
  cudaDeviceSynchronize();

  // Check for kernel errors
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

  // Copy back heap positions and interaction counters
  cudaMemcpy(h_heap_pos_array, d_heap_pos_array_ptr, N * sizeof(uint32_t), cudaMemcpyDeviceToHost);
  cudaMemcpy(h_interactions_array, d_interactions_array_ptr, N * sizeof(unsigned long long), cudaMemcpyDeviceToHost);

  // Sum interactions and find max heap position
  uint64_t total_interactions = 0;
  uint32_t max_heap_pos = 0;
  for (int i = 0; i < N; i++) {
    total_interactions += h_interactions_array[i];
    if (h_heap_pos_array[i] > max_heap_pos) {
      max_heap_pos = h_heap_pos_array[i];
    }
  }

  // Copy normalized heap back from thread 0's segment
  err = cudaMemcpy(ic->heap, d_heap_ptr, max_heap_pos * sizeof(Term), cudaMemcpyDeviceToHost);
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (heap copy from device): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    cudaFree(d_heap_pos_array_ptr);
    cudaFree(d_interactions_array_ptr);
    free(h_heap_pos_array);
    free(h_interactions_array);
    return term;
  }

  // Update host context
  ic->heap_pos = max_heap_pos;
  ic->interactions = total_interactions;

  // Free device memory
  cudaFree(d_heap_ptr);
  cudaFree(d_stack_ptr);
  cudaFree(d_heap_pos_array_ptr);
  cudaFree(d_interactions_array_ptr);
  free(h_heap_pos_array);
  free(h_interactions_array);

  // Return the normalized term from thread 0
  return ic->heap[0];
}
