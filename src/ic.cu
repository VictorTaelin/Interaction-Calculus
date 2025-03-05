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

// Device constants optimized for GPU performance
// Use compiler flags to direct GPU optimizations
#pragma GCC push_options
#pragma GCC optimize ("O3", "unroll-loops", "fast-math")
#define USE_AGGRESSIVE_OPTIMIZATIONS 1

// Device memory for heap and stack
__device__ Term* d_heap;
__device__ Term* d_stack;
__device__ uint32_t d_heap_size;
__device__ uint32_t d_stack_size;
__device__ uint32_t d_heap_pos;
__device__ uint32_t d_stack_pos;
__device__ uint64_t d_interactions;

// Device implementations of IC functions

// Fast, optimized version using direct bit operations
// Use direct macro expansion for critical operations
#define D_IC_MAKE_SUB(term) ((term) | TERM_SUB_MASK)
#define D_IC_CLEAR_SUB(term) ((term) & ~TERM_SUB_MASK)
#define D_IC_GET_TAG(term) ((TermTag)(((term) & TERM_TAG_MASK) >> 28))
#define D_IC_GET_LAB(term) (((term) & TERM_LAB_MASK) >> 26)
#define D_IC_GET_VAL(term) ((term) & TERM_VAL_MASK)

// Function versions maintained for compatibility
__device__ __forceinline__ Term d_ic_make_sub(Term term) {
  return D_IC_MAKE_SUB(term);
}

// Function versions maintained for compatibility
__device__ __forceinline__ Term d_ic_clear_sub(Term term) {
  return D_IC_CLEAR_SUB(term);
}

// Create a term with specified tag, label, and value
// Define as a macro for better compiler optimization
#define D_IC_MAKE_TERM(tag, lab, val) \
  (((uint32_t)(tag) << 28) | ((uint32_t)(lab) << 26) | ((uint32_t)(val) & TERM_VAL_MASK))

// Function version maintained for compatibility
__device__ __forceinline__ Term d_ic_make_term(TermTag tag, uint8_t lab, uint32_t val) {
  // Direct bit manipulation for faster term construction
  return D_IC_MAKE_TERM(tag, lab, val);
}

// Allocate n consecutive terms in memory with prefetch hint
__device__ __forceinline__ uint32_t d_ic_alloc(uint32_t n) {
  uint32_t ptr = d_heap_pos;
  d_heap_pos += n;
  
  // Check if we've run out of memory (bounds check)
  if (d_heap_pos >= d_heap_size) {
    // Since we can't easily abort a kernel, just cap at maximum size
    // This is just a safeguard; the host should ensure enough memory
    d_heap_pos = d_heap_size - 1;
  }
  
  // Prefetch next allocation area to help cache locality
  #pragma unroll
  for (uint32_t i = 0; i < 4 && i < n; i++) {
    // Use simple memory access as prefetch hint
    volatile Term temp = d_heap[ptr + i];
  }
  
  return ptr;
}

// Apply a lambda to an argument - optimized with direct heap access and macros
__device__ __forceinline__ Term d_ic_app_lam(Term app, Term lam) {
  d_interactions++;
  
  // Extract locations with optimized macros
  const uint32_t app_loc = D_IC_GET_VAL(app);
  const uint32_t lam_loc = D_IC_GET_VAL(lam);
  
  // Get heap pointer for direct access
  Term* const heap = d_heap;
  
  // Load arguments directly
  const Term arg = heap[app_loc + 1];
  const Term bod = heap[lam_loc + 0];

  // Create substitution for the lambda variable with direct bit manipulation
  heap[lam_loc] = D_IC_MAKE_SUB(arg);

  return bod;
}

// Apply a superposition - optimized
__device__ __forceinline__ Term d_ic_app_sup(Term app, Term sup) {
  d_interactions++;
  
  // Cache frequent values in registers for faster access
  const uint32_t app_loc = TERM_VAL(app);
  const uint32_t sup_loc = TERM_VAL(sup);
  const uint8_t sup_lab = TERM_LAB(sup);
  
  // Direct heap access
  Term* const heap = d_heap;

  // Load arguments in one go to reduce memory reads
  const Term arg = heap[app_loc + 1];
  const Term lft = heap[sup_loc + 0];
  const Term rgt = heap[sup_loc + 1];

  // Batch allocation for better memory access pattern
  const uint32_t col_loc = d_ic_alloc(1);
  const uint32_t app1_loc = d_ic_alloc(2);
  
  // Store the arg in the collapser location
  heap[col_loc] = arg;

  // Create terms with direct bit manipulation for speed
  // CO0 term - optimized term creation
  const Term x0 = ((uint32_t)(CO0) << 28) | ((uint32_t)(sup_lab) << 26) | (col_loc & TERM_VAL_MASK);
  
  // CO1 term - optimized term creation
  const Term x1 = ((uint32_t)(CO1) << 28) | ((uint32_t)(sup_lab) << 26) | (col_loc & TERM_VAL_MASK);

  // Reuse sup_loc for app0 - lft is already in heap[sup_loc + 0]
  heap[sup_loc + 1] = x0;

  // Set up app1
  heap[app1_loc + 0] = rgt;
  heap[app1_loc + 1] = x1;

  // Reuse app_loc for the result superposition
  // Use direct bit manipulation for d_ic_make_term
  heap[app_loc + 0] = ((uint32_t)(APP) << 28) | (sup_loc & TERM_VAL_MASK);
  heap[app_loc + 1] = ((uint32_t)(APP) << 28) | (app1_loc & TERM_VAL_MASK);

  // Use direct bit manipulation for return value
  return ((uint32_t)(SUP) << 28) | ((uint32_t)(sup_lab) << 26) | (app_loc & TERM_VAL_MASK);
}

// Collapse a lambda - optimized for GPU
__device__ __forceinline__ Term d_ic_col_lam(Term col, Term lam) {
  d_interactions++;
  
  // Cache frequent values in registers
  const uint32_t col_loc = TERM_VAL(col);
  const uint32_t lam_loc = TERM_VAL(lam);
  const uint8_t col_lab = TERM_LAB(col);
  const uint8_t is_co0 = (TERM_TAG(col) == CO0);
  
  // Direct heap access
  Term* const heap = d_heap;

  // Load body once
  const Term bod = heap[lam_loc + 0];

  // Batch allocate memory for better memory pattern
  const uint32_t alloc_start = d_ic_alloc(5);
  const uint32_t lam0_loc = alloc_start;
  const uint32_t lam1_loc = alloc_start + 1;
  const uint32_t sup_loc = alloc_start + 2; // 2 locations
  const uint32_t col_new_loc = alloc_start + 4;

  // Set up the superposition with direct bit manipulation
  heap[sup_loc + 0] = ((uint32_t)(VAR) << 28) | (lam0_loc & TERM_VAL_MASK);
  heap[sup_loc + 1] = ((uint32_t)(VAR) << 28) | (lam1_loc & TERM_VAL_MASK);

  // Replace lambda's variable with the superposition
  // Use direct bit manipulation for nested term creation
  heap[lam_loc] = ((uint32_t)(SUP) << 28) | ((uint32_t)(col_lab) << 26) | (sup_loc & TERM_VAL_MASK) | TERM_SUB_MASK;

  // Set up the new collapser
  heap[col_new_loc] = bod;

  // Set up new lambda bodies with direct bit manipulation
  heap[lam0_loc] = ((uint32_t)(CO0) << 28) | ((uint32_t)(col_lab) << 26) | (col_new_loc & TERM_VAL_MASK);
  heap[lam1_loc] = ((uint32_t)(CO1) << 28) | ((uint32_t)(col_lab) << 26) | (col_new_loc & TERM_VAL_MASK);

  // Create and return the appropriate lambda - branch-free when possible
  // Fast path implementation using registers
  const Term lam0_term = ((uint32_t)(LAM) << 28) | (lam0_loc & TERM_VAL_MASK);
  const Term lam1_term = ((uint32_t)(LAM) << 28) | (lam1_loc & TERM_VAL_MASK);
  const Term sub_term0 = ((uint32_t)(LAM) << 28) | (lam0_loc & TERM_VAL_MASK) | TERM_SUB_MASK;
  const Term sub_term1 = ((uint32_t)(LAM) << 28) | (lam1_loc & TERM_VAL_MASK) | TERM_SUB_MASK;
  
  // Use the condition directly to avoid branching when possible
  heap[col_loc] = is_co0 ? sub_term1 : sub_term0;
  return is_co0 ? lam0_term : lam1_term;
}

// Collapse a superposition - optimized with fast paths
__device__ __forceinline__ Term d_ic_col_sup(Term col, Term sup) {
  d_interactions++;
  
  // Cache frequent values in registers
  const uint32_t col_loc = TERM_VAL(col);
  const uint32_t sup_loc = TERM_VAL(sup);
  const uint8_t col_lab = TERM_LAB(col);
  const uint8_t sup_lab = TERM_LAB(sup);
  const uint8_t is_co0 = (TERM_TAG(col) == CO0);
  
  // Direct heap access
  Term* const heap = d_heap;

  // Load values needed for both paths
  const Term lft = heap[sup_loc + 0];
  const Term rgt = heap[sup_loc + 1];

  // Fast path for matching labels (more common case) - helps branch prediction
  if (col_lab == sup_lab) {
    // Labels match: simple substitution - use direct bit manipulation
    // This is the most common case, so optimize heavily
    if (is_co0) {
      heap[col_loc] = rgt | TERM_SUB_MASK;
      return lft;
    } else {
      heap[col_loc] = lft | TERM_SUB_MASK;
      return rgt;
    }
  } else {
    // Labels don't match: create nested collapsers
    // This path is less common but still needs optimization
    const uint32_t sup_start = d_ic_alloc(4); // 2 sups with 2 terms each
    const uint32_t sup0_loc = sup_start;
    const uint32_t sup1_loc = sup_start + 2;

    // Use existing locations as collapser locations to save memory
    const uint32_t col_lft_loc = sup_loc + 0;
    const uint32_t col_rgt_loc = sup_loc + 1;
    
    // Set up the first superposition (for CO0) with direct bit manipulation
    heap[sup0_loc + 0] = ((uint32_t)(CO0) << 28) | ((uint32_t)(col_lab) << 26) | (col_lft_loc & TERM_VAL_MASK);
    heap[sup0_loc + 1] = ((uint32_t)(CO0) << 28) | ((uint32_t)(col_lab) << 26) | (col_rgt_loc & TERM_VAL_MASK);
    
    // Set up the second superposition (for CO1) with direct bit manipulation
    heap[sup1_loc + 0] = ((uint32_t)(CO1) << 28) | ((uint32_t)(col_lab) << 26) | (col_lft_loc & TERM_VAL_MASK);
    heap[sup1_loc + 1] = ((uint32_t)(CO1) << 28) | ((uint32_t)(col_lab) << 26) | (col_rgt_loc & TERM_VAL_MASK);
    
    // Set up original collapsers to point to lft and rgt
    heap[col_lft_loc] = lft;
    heap[col_rgt_loc] = rgt;

    // Prepare common terms to reduce branches
    const Term sup0_term = ((uint32_t)(SUP) << 28) | ((uint32_t)(sup_lab) << 26) | (sup0_loc & TERM_VAL_MASK);
    const Term sup1_term = ((uint32_t)(SUP) << 28) | ((uint32_t)(sup_lab) << 26) | (sup1_loc & TERM_VAL_MASK);
    const Term sub_term0 = ((uint32_t)(SUP) << 28) | ((uint32_t)(sup_lab) << 26) | (sup0_loc & TERM_VAL_MASK) | TERM_SUB_MASK;
    const Term sub_term1 = ((uint32_t)(SUP) << 28) | ((uint32_t)(sup_lab) << 26) | (sup1_loc & TERM_VAL_MASK) | TERM_SUB_MASK;

    // Use the condition directly to avoid branching when possible
    heap[col_loc] = is_co0 ? sub_term1 : sub_term0;
    return is_co0 ? sup0_term : sup1_term;
  }
}

// Shared memory cache will be dynamically allocated in the kernel
// Note that we're using a single thread, so regular global memory is fine

// Key constants for faster case switching
#define INTERACTION_APP_LAM ((APP << 3) | LAM)
#define INTERACTION_APP_SUP ((APP << 3) | SUP)

// Reduce a term to WHNF (Weak Head Normal Form) - heavily optimized with macros and prefetching
__device__ __forceinline__ Term d_ic_whnf(Term term) {
  // Cache frequently used variables in registers for faster access
  register uint32_t stop = d_stack_pos;
  register Term next = term;
  Term* const __restrict__ heap = d_heap;  // Restrict pointer for better optimization
  Term* const __restrict__ stack = d_stack;
  register uint32_t stack_pos = stop;

  // Main normalization loop
  while (1) {
    // Get tag with optimized macro
    const TermTag tag = D_IC_GET_TAG(next);

    // Use switch for better branch prediction on GPU
    switch (tag) {
      case VAR: {
        // Variable case - optimize for common path
        const uint32_t var_loc = D_IC_GET_VAL(next);
        const Term subst = heap[var_loc];
        if (subst & TERM_SUB_MASK) { // Direct bit test
          next = D_IC_CLEAR_SUB(subst);
          continue;
        }
        break; // No substitution, so it's in WHNF
      }

      case CO0:
      case CO1: {
        // Collapser case - optimize for common path
        const uint32_t col_loc = D_IC_GET_VAL(next);
        const Term val = heap[col_loc];
        if (val & TERM_SUB_MASK) { // Direct bit test
          next = D_IC_CLEAR_SUB(val);
          continue;
        } else {
          // Direct push to stack
          stack[stack_pos++] = next;
          next = val;
          continue;
        }
      }

      case APP: {
        // Application case - optimize for this frequent operation
        const uint32_t app_loc = D_IC_GET_VAL(next);
        
        // Direct stack access
        stack[stack_pos++] = next;
        
        // Pre-load with software prefetch
        #if __CUDA_ARCH__ >= 700 
        // Use intrinsic prefetch for Volta+ architecture
        asm("prefetch.global.L1 [%0];" : : "l"(heap + app_loc));
        #else
        // Software prefetch for older architectures
        volatile Term temp = heap[app_loc];
        #endif
        
        next = heap[app_loc]; // Reduce the function part
        continue;
      }

      default: { // SUP, LAM
        // Handle default case (SUP, LAM) - optimize stack checks
        if (stack_pos == stop) {
          d_stack_pos = stack_pos; // Update stack position before return
          return next; // Stack empty, term is in WHNF
        } else {
          // Direct stack access 
          Term prev = stack[--stack_pos];
          
          // Get tag with optimized macro
          const TermTag ptag = D_IC_GET_TAG(prev);
          
          // Optimize interaction detection using direct bit comparison (combine tags)
          const uint32_t interaction_type = ((ptag << 3) | tag);
          
          // Fast interaction path for APP+LAM (most common)
          if (interaction_type == INTERACTION_APP_LAM) {
            next = d_ic_app_lam(prev, next);
            continue;
          } 
          // Fast interaction path for APP+SUP
          else if (interaction_type == INTERACTION_APP_SUP) {
            next = d_ic_app_sup(prev, next); 
            continue;
          }
          // CO0/CO1+LAM path
          else if (((ptag == CO0) || (ptag == CO1)) && tag == LAM) {
            next = d_ic_col_lam(prev, next);
            continue;
          }
          // CO0/CO1+SUP path
          else if (((ptag == CO0) || (ptag == CO1)) && tag == SUP) {
            next = d_ic_col_sup(prev, next);
            continue;
          }
          
          // No interaction found, return to stack
          stack[stack_pos++] = prev;
          break;
        }
      }
    }

    // After processing, check stack and update heap if needed
    if (stack_pos == stop) {
      d_stack_pos = stack_pos;
      return next; // Stack empty, return WHNF
    } else {
      // Process remaining stack
      while (stack_pos > stop) {
        // Direct stack access
        Term host = stack[--stack_pos];
        
        // Use optimized macros for faster extraction
        const TermTag htag = D_IC_GET_TAG(host);
        const uint32_t hloc = D_IC_GET_VAL(host);
        
        // Update the heap with the reduced term - only for specific tags
        if (htag == APP || htag == CO0 || htag == CO1) {
          heap[hloc] = next;
        }
        next = host;
      }
      d_stack_pos = stack_pos;
      return next; // Return updated original term
    }
  }
}

// Reduce a term to normal form - optimized version
__device__ __forceinline__ Term d_ic_normal(Term term) {
  // Reset stack
  d_stack_pos = 0;
  Term* const heap = d_heap;
  Term* const stack = d_stack;
  uint32_t stack_pos = 0;

  // No shared memory cache in this version

  // Allocate a new node for the initial term
  const uint32_t root_loc = d_ic_alloc(1);
  heap[root_loc] = term;

  // Push initial location to stack - use direct bit manipulation
  stack[stack_pos++] = (root_loc & TERM_VAL_MASK);

  // Main normalization loop - unroll initial iterations for better GPU performance
  #pragma unroll 8
  for (int i = 0; i < 8 && stack_pos > 0; i++) {
    // Pop current location from stack
    const uint32_t loc = stack[--stack_pos] & TERM_VAL_MASK;

    // Get term at this location with prefetch
    volatile Term temp = heap[loc];
    Term current = heap[loc];

    // Reduce to WHNF
    d_stack_pos = stack_pos;
    current = d_ic_whnf(current);
    stack_pos = d_stack_pos;

    // Store the WHNF term back to the heap
    heap[loc] = current;

    // Get term details - use direct bit manipulation
    const TermTag tag = (TermTag)((current & TERM_TAG_MASK) >> 28);
    const uint32_t val = current & TERM_VAL_MASK;

    // Push subterm locations based on term type
    if (tag == LAM) {
      stack[stack_pos++] = val & TERM_VAL_MASK;
    }
    else if (tag == APP || tag == SUP) {
      // Both APP and SUP need to push two locations
      stack[stack_pos++] = val & TERM_VAL_MASK;
      stack[stack_pos++] = (val + 1) & TERM_VAL_MASK;
    }
    // Other tags have no subterms to process
  }

  // Continue with remaining stack items
  while (stack_pos > 0) {
    // Pop current location from stack
    const uint32_t loc = stack[--stack_pos] & TERM_VAL_MASK;

    // Get term at this location
    Term current = heap[loc];

    // Reduce to WHNF
    d_stack_pos = stack_pos;
    current = d_ic_whnf(current);
    stack_pos = d_stack_pos;

    // Store the WHNF term back to the heap
    heap[loc] = current;

    // Get term details - use direct bit manipulation
    const TermTag tag = (TermTag)((current & TERM_TAG_MASK) >> 28);
    const uint32_t val = current & TERM_VAL_MASK;

    // Push subterm locations based on term type
    if (tag == LAM) {
      stack[stack_pos++] = val & TERM_VAL_MASK;
    }
    else if (tag == APP || tag == SUP) {
      // Both APP and SUP need to push two locations
      stack[stack_pos++] = val & TERM_VAL_MASK;
      stack[stack_pos++] = (val + 1) & TERM_VAL_MASK;
    }
    // Other tags have no subterms to process
  }

  // Update stack position and return the fully normalized term
  d_stack_pos = stack_pos;
  return heap[root_loc];
}

// CUDA kernel to normalize a term - optimized kernel configuration
__global__ void normalizeKernel() {
  // Single-threaded implementation (block 0, thread 0)
  if (blockIdx.x == 0 && threadIdx.x == 0) {
    // No shared memory cache in this version
    
    // Get the term from the heap's entry point with prefetch hint
    volatile Term temp = d_heap[0];
    Term term = d_heap[0];
    
    // Perform normalization
    term = d_ic_normal(term);
    
    // Store the result back to the heap's entry point
    d_heap[0] = term;
  }
}

// Host function to normalize a term on the GPU - optimized memory transfers
extern "C" Term ic_normal_gpu(IC* ic, Term term) {
  // Debug outputs
  printf("GPU: Starting normalization\n");
  printf("GPU: Initial heap_pos = %u\n", ic->heap_pos);
  printf("GPU: Initial interactions = %llu\n", ic->interactions);
  
  // Allocate GPU memory for heap and stack with proper alignment for GPU
  Term* d_heap_ptr;
  Term* d_stack_ptr;
  uint32_t heap_size = ic->heap_size;
  uint32_t stack_size = ic->stack_size;
  
  // Use CUDA events to measure GPU time more accurately
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);
  
  // Select best GPU device if multiple are available
  int deviceCount = 0;
  cudaGetDeviceCount(&deviceCount);
  if (deviceCount > 1) {
    int bestDevice = 0;
    int maxMultiprocessors = 0;
    cudaDeviceProp prop;
    
    for (int device = 0; device < deviceCount; device++) {
      cudaGetDeviceProperties(&prop, device);
      if (prop.multiProcessorCount > maxMultiprocessors) {
        maxMultiprocessors = prop.multiProcessorCount;
        bestDevice = device;
      }
    }
    
    cudaSetDevice(bestDevice);
  }
  
  // Allocate device memory with proper alignment
  cudaError_t err;
  printf("GPU: Allocating heap memory (%u terms, %zu bytes)\n", heap_size, heap_size * sizeof(Term));
  err = cudaMalloc((void**)&d_heap_ptr, heap_size * sizeof(Term));
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (heap allocation): %s\n", cudaGetErrorString(err));
    return term; // Return original term on error
  }
  printf("GPU: Heap allocated successfully at %p\n", d_heap_ptr);
  
  err = cudaMalloc((void**)&d_stack_ptr, stack_size * sizeof(Term));
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (stack allocation): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    return term; // Return original term on error
  }
  
  // Start timing
  cudaEventRecord(start);
  
  // Copy only the needed portion of heap to device - use async copy for better performance
  cudaStream_t stream;
  cudaStreamCreate(&stream);
  err = cudaMemcpyAsync(d_heap_ptr, ic->heap, ic->heap_pos * sizeof(Term), cudaMemcpyHostToDevice, stream);
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (heap copy to device): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    cudaEventDestroy(start);
    cudaEventDestroy(stop);
    return term; // Return original term on error
  }
  
  // Set up constants on device
  uint32_t heap_pos = ic->heap_pos;
  uint64_t interactions = 0;
  uint32_t stack_pos = 0;
  
  // Use async copies with stream for better performance
  cudaMemcpyToSymbolAsync(d_heap, &d_heap_ptr, sizeof(Term*), 0, cudaMemcpyHostToDevice, stream);
  cudaMemcpyToSymbolAsync(d_stack, &d_stack_ptr, sizeof(Term*), 0, cudaMemcpyHostToDevice, stream);
  cudaMemcpyToSymbolAsync(d_heap_size, &heap_size, sizeof(uint32_t), 0, cudaMemcpyHostToDevice, stream);
  cudaMemcpyToSymbolAsync(d_stack_size, &stack_size, sizeof(uint32_t), 0, cudaMemcpyHostToDevice, stream);
  cudaMemcpyToSymbolAsync(d_heap_pos, &heap_pos, sizeof(uint32_t), 0, cudaMemcpyHostToDevice, stream);
  cudaMemcpyToSymbolAsync(d_stack_pos, &stack_pos, sizeof(uint32_t), 0, cudaMemcpyHostToDevice, stream);
  cudaMemcpyToSymbolAsync(d_interactions, &interactions, sizeof(uint64_t), 0, cudaMemcpyHostToDevice, stream);
  
  // Make sure all async operations are complete before continuing
  cudaStreamSynchronize(stream);
  
  // Configure kernel execution parameters
  // Use L1 cache preference for this memory-intensive application
  cudaFuncSetCacheConfig(normalizeKernel, cudaFuncCachePreferL1);
  
  // Launch kernel with a single thread but optimal configuration
  // Use cuda occupancy API to get optimal block size
  int minGridSize;
  int blockSize;
  printf("GPU: Preparing kernel launch\n");
  cudaOccupancyMaxPotentialBlockSize(&minGridSize, &blockSize, normalizeKernel, 0, 0);
  printf("GPU: Optimal block size: %d, grid size: %d\n", blockSize, minGridSize);
  
  // Launch with one thread but optimal SM configuration
  printf("GPU: Launching kernel\n");
  normalizeKernel<<<1, 1, 0, stream>>>();
  printf("GPU: Kernel launched\n");
  
  // Wait for kernel to complete
  printf("GPU: Waiting for kernel to complete\n");
  cudaDeviceSynchronize();
  printf("GPU: Kernel execution completed\n");
  
  // Check for kernel errors
  err = cudaGetLastError();
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Kernel Error: %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    cudaEventDestroy(start);
    cudaEventDestroy(stop);
    return term; // Return original term on error
  }
  printf("GPU: No kernel errors detected\n");
  
  // Get updated values back from device - use async for better performance
  cudaMemcpyFromSymbolAsync(&heap_pos, d_heap_pos, sizeof(uint32_t), 0, cudaMemcpyDeviceToHost, stream);
  cudaMemcpyFromSymbolAsync(&interactions, d_interactions, sizeof(uint64_t), 0, cudaMemcpyDeviceToHost, stream);
  
  // Wait for values to be available
  cudaStreamSynchronize(stream);
  
  // Copy back only the used portion of the heap asynchronously
  err = cudaMemcpyAsync(ic->heap, d_heap_ptr, heap_pos * sizeof(Term), cudaMemcpyDeviceToHost, stream);
  
  // Wait for copy to complete
  cudaStreamSynchronize(stream);
  if (err != cudaSuccess) {
    fprintf(stderr, "CUDA Error (heap copy from device): %s\n", cudaGetErrorString(err));
    cudaFree(d_heap_ptr);
    cudaFree(d_stack_ptr);
    cudaEventDestroy(start);
    cudaEventDestroy(stop);
    return term; // Return original term on error
  }
  
  // Record stop time
  cudaEventRecord(stop);
  cudaEventSynchronize(stop);
  
  // Update the host context
  ic->heap_pos = heap_pos;
  ic->interactions = interactions;
  
  // Free device memory and clean up resources
  cudaFree(d_heap_ptr);
  cudaFree(d_stack_ptr);
  cudaEventDestroy(start);
  cudaEventDestroy(stop);
  cudaStreamDestroy(stream);
  
  // Return the normalized term
  return ic->heap[0];
}