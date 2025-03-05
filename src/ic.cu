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

// Device memory for heap and stack
__device__ Term* d_heap;
__device__ Term* d_stack;
__device__ uint32_t d_heap_size;
__device__ uint32_t d_stack_size;
__device__ uint32_t d_heap_pos;
__device__ uint32_t d_stack_pos;
__device__ uint64_t d_interactions;

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

// Allocate n consecutive terms in memory
__device__ inline uint32_t d_ic_alloc(uint32_t n) {
  uint32_t ptr = d_heap_pos;
  d_heap_pos += n;
  
  // Check if we've run out of memory
  if (d_heap_pos >= d_heap_size) {
    // In a real implementation, we'd need error handling here
    // Since we can't easily abort a kernel, we'll just wrap around
    // This is just a safeguard; the host should ensure enough memory
    d_heap_pos = d_heap_size - 1;
  }
  
  return ptr;
}

// Apply a lambda to an argument
__device__ inline Term d_ic_app_lam(Term app, Term lam) {
  d_interactions++;
  
  uint32_t app_loc = TERM_VAL(app);
  uint32_t lam_loc = TERM_VAL(lam);
  
  Term arg = d_heap[app_loc + 1];
  Term bod = d_heap[lam_loc + 0];

  // Create substitution for the lambda variable
  d_heap[lam_loc] = d_ic_make_sub(arg);

  return bod;
}

// Apply a superposition
__device__ inline Term d_ic_app_sup(Term app, Term sup) {
  d_interactions++;
  
  uint32_t app_loc = TERM_VAL(app);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);

  Term arg = d_heap[app_loc + 1];
  Term lft = d_heap[sup_loc + 0];
  Term rgt = d_heap[sup_loc + 1];

  // Allocate only what's necessary
  uint32_t col_loc = d_ic_alloc(1);
  uint32_t app1_loc = d_ic_alloc(2);
  
  // Store the arg in the collapser location
  d_heap[col_loc] = arg;

  // Create CO0 and CO1 terms
  Term x0 = d_ic_make_term(CO0, sup_lab, col_loc);
  Term x1 = d_ic_make_term(CO1, sup_lab, col_loc);

  // Reuse sup_loc for app0
  d_heap[sup_loc + 1] = x0; // lft is already in heap[sup_loc + 0]

  // Set up app1
  d_heap[app1_loc + 0] = rgt;
  d_heap[app1_loc + 1] = x1;

  // Reuse app_loc for the result superposition
  d_heap[app_loc + 0] = d_ic_make_term(APP, 0, sup_loc);
  d_heap[app_loc + 1] = d_ic_make_term(APP, 0, app1_loc);

  return d_ic_make_term(SUP, sup_lab, app_loc);
}

// Collapse a lambda
__device__ inline Term d_ic_col_lam(Term col, Term lam) {
  d_interactions++;
  
  uint32_t col_loc = TERM_VAL(col);
  uint32_t lam_loc = TERM_VAL(lam);
  uint8_t col_lab = TERM_LAB(col);
  uint8_t is_co0 = (TERM_TAG(col) == CO0);

  Term bod = d_heap[lam_loc + 0];

  // Batch allocate memory for efficiency
  uint32_t alloc_start = d_ic_alloc(5);
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
__device__ inline Term d_ic_col_sup(Term col, Term sup) {
  d_interactions++;
  
  uint32_t col_loc = TERM_VAL(col);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t col_lab = TERM_LAB(col);
  uint8_t sup_lab = TERM_LAB(sup);
  uint8_t is_co0 = (TERM_TAG(col) == CO0);

  Term lft = d_heap[sup_loc + 0];
  Term rgt = d_heap[sup_loc + 1];

  // Fast path for matching labels (common case)
  if (col_lab == sup_lab) {
    // Labels match: simple substitution
    if (is_co0) {
      d_heap[col_loc] = d_ic_make_sub(rgt);
      return lft;
    } else {
      d_heap[col_loc] = d_ic_make_sub(lft);
      return rgt;
    }
  } else {
    // Labels don't match: create nested collapsers
    uint32_t sup_start = d_ic_alloc(4); // 2 sups with 2 terms each
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

// Reduce a term to WHNF (Weak Head Normal Form)
__device__ inline Term d_ic_whnf(Term term) {
  uint32_t stop = d_stack_pos;
  Term next = term;
  uint32_t stack_pos = stop;

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
        break; // No substitution, so it's in WHNF
      }

      case CO0:
      case CO1: {
        uint32_t col_loc = TERM_VAL(next);
        Term val = d_heap[col_loc];
        if (TERM_SUB(val)) {
          next = d_ic_clear_sub(val);
          continue;
        } else {
          d_stack[stack_pos++] = next;
          next = val;
          continue;
        }
      }

      case APP: {
        uint32_t app_loc = TERM_VAL(next);
        d_stack[stack_pos++] = next;
        next = d_heap[app_loc]; // Reduce the function part
        continue;
      }

      default: { // SUP, LAM
        if (stack_pos == stop) {
          d_stack_pos = stack_pos; // Update stack position before return
          return next; // Stack empty, term is in WHNF
        } else {
          Term prev = d_stack[--stack_pos];
          TermTag ptag = TERM_TAG(prev);
          
          // Handle interactions based on term types
          if (ptag == APP && tag == LAM) {
            next = d_ic_app_lam(prev, next);
            continue;
          } 
          else if (ptag == APP && tag == SUP) {
            next = d_ic_app_sup(prev, next); 
            continue;
          }
          else if ((ptag == CO0 || ptag == CO1) && tag == LAM) {
            next = d_ic_col_lam(prev, next);
            continue;
          }
          else if ((ptag == CO0 || ptag == CO1) && tag == SUP) {
            next = d_ic_col_sup(prev, next);
            continue;
          }
          
          // No interaction found, proceed to stack traversal
          d_stack[stack_pos++] = prev;
          break;
        }
      }
    }

    // After processing, check stack and update heap if needed
    if (stack_pos == stop) {
      d_stack_pos = stack_pos;
      return next; // Stack empty, return WHNF
    } else {
      while (stack_pos > stop) {
        Term host = d_stack[--stack_pos];
        TermTag htag = TERM_TAG(host);
        uint32_t hloc = TERM_VAL(host);
        
        // Update the heap with the reduced term
        if (htag == APP || htag == CO0 || htag == CO1) {
          d_heap[hloc] = next;
        }
        next = host;
      }
      d_stack_pos = stack_pos;
      return next; // Return updated original term
    }
  }
}

// Reduce a term to normal form
__device__ inline Term d_ic_normal(Term term) {
  // Reset stack
  d_stack_pos = 0;
  uint32_t stack_pos = 0;

  // Allocate a new node for the initial term
  uint32_t root_loc = d_ic_alloc(1);
  d_heap[root_loc] = term;

  // Push initial location to stack as a "location"
  d_stack[stack_pos++] = MAKE_TERM(false, 0, 0, root_loc);

  while (stack_pos > 0) {
    // Pop current location from stack
    uint32_t loc = TERM_VAL(d_stack[--stack_pos]);

    // Get term at this location
    Term current = d_heap[loc];

    // Reduce to WHNF
    d_stack_pos = stack_pos;
    current = d_ic_whnf(current);
    stack_pos = d_stack_pos;

    // Store the WHNF term back to the heap
    d_heap[loc] = current;

    // Get term details
    TermTag tag = TERM_TAG(current);
    uint32_t val = TERM_VAL(current);

    // Push subterm locations based on term type
    if (tag == LAM) {
      d_stack[stack_pos++] = MAKE_TERM(false, 0, 0, val);
    }
    else if (tag == APP || tag == SUP) {
      // Both APP and SUP need to push two locations
      d_stack[stack_pos++] = MAKE_TERM(false, 0, 0, val);
      d_stack[stack_pos++] = MAKE_TERM(false, 0, 0, val + 1);
    }
    // Other tags have no subterms to process
  }

  // Update stack position and return the fully normalized term
  d_stack_pos = stack_pos;
  return d_heap[root_loc];
}

// CUDA kernel to normalize a term
__global__ void normalizeKernel() {
  // Single-threaded implementation (block 0, thread 0)
  if (blockIdx.x == 0 && threadIdx.x == 0) {
    // Get the term from the heap's entry point
    Term term = d_heap[0];
    
    // Perform normalization
    term = d_ic_normal(term);
    
    // Store the result back to the heap's entry point
    d_heap[0] = term;
  }
}

// Host function to normalize a term on the GPU
extern "C" Term ic_normal_cuda(IC* ic, Term term) {
  // Allocate GPU memory for heap
  Term* d_heap_ptr;
  Term* d_stack_ptr;
  uint32_t heap_size = ic->heap_size;
  uint32_t stack_size = ic->stack_size;
  
  // Allocate device memory for heap and stack
  cudaError_t err;
  err = cudaMalloc((void**)&d_heap_ptr, heap_size * sizeof(Term));
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (heap allocation): %s\n", cudaGetErrorString(err));
    return term; // Return original term on error
  }
  
  err = cudaMalloc((void**)&d_stack_ptr, stack_size * sizeof(Term));
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (stack allocation): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    return term; // Return original term on error
  }
  
  // Copy heap from host to device
  err = cudaMemcpy(d_heap_ptr, ic->heap, ic->heap_pos * sizeof(Term), cudaMemcpyHostToDevice);
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (heap copy to device): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    return term; // Return original term on error
  }
  
  // Set up constants on device
  uint32_t heap_pos = ic->heap_pos;
  uint64_t interactions = 0;
  uint32_t stack_pos = 0;
  
  cudaMemcpyToSymbol(d_heap, &d_heap_ptr, sizeof(Term*));
  cudaMemcpyToSymbol(d_stack, &d_stack_ptr, sizeof(Term*));
  cudaMemcpyToSymbol(d_heap_size, &heap_size, sizeof(uint32_t));
  cudaMemcpyToSymbol(d_stack_size, &stack_size, sizeof(uint32_t));
  cudaMemcpyToSymbol(d_heap_pos, &heap_pos, sizeof(uint32_t));
  cudaMemcpyToSymbol(d_stack_pos, &stack_pos, sizeof(uint32_t));
  cudaMemcpyToSymbol(d_interactions, &interactions, sizeof(uint64_t));
  
  // Launch kernel with a single thread
  normalizeKernel<<<1, 1>>>();
  
  // Wait for kernel to complete
  cudaDeviceSynchronize();
  
  // Check for kernel errors
  err = cudaGetLastError();
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Kernel Error: %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    return term; // Return original term on error
  }
  
  // Get updated values back from device
  cudaMemcpyFromSymbol(&heap_pos, d_heap_pos, sizeof(uint32_t));
  cudaMemcpyFromSymbol(&interactions, d_interactions, sizeof(uint64_t));
  
  // Copy updated heap back to host
  err = cudaMemcpy(ic->heap, d_heap_ptr, heap_pos * sizeof(Term), cudaMemcpyDeviceToHost);
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (heap copy from device): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    return term; // Return original term on error
  }
  
  // Update the host context
  ic->heap_pos = heap_pos;
  ic->interactions = interactions;
  
  // Free device memory
  cudaFree(d_heap_ptr);
  cudaFree(d_stack_ptr);
  
  // Return the normalized term
  return ic->heap[0];
}
