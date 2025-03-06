#import <Foundation/Foundation.h>
#import <Metal/Metal.h>
#include "hvmn.h"

/**
 * HVM-Nano (HVMN) - Metal Objective-C++ bridge file
 * 
 * This file provides the bridge between the main C implementation
 * and the Metal GPU implementation for accelerated normalization:
 * - Metal context creation and management
 * - Metal device, command queue, and pipeline setup
 * - Metal buffer management and synchronization
 */

// Structure to hold Metal-specific resources
typedef struct {
  id<MTLDevice> device;
  id<MTLCommandQueue> commandQueue;
  id<MTLLibrary> library;
  id<MTLFunction> normalizeFunction;
  id<MTLComputePipelineState> normalizePipeline;
  id<MTLBuffer> heapBuffer;
  id<MTLBuffer> stackBuffer;
  id<MTLBuffer> heapPosBuffer;
  id<MTLBuffer> stackPosBuffer;
  id<MTLBuffer> interactionsBuffer;
  id<MTLBuffer> heapSizeBuffer;
  id<MTLBuffer> stackSizeBuffer;
  bool initialized;
} MetalContext;

// Global Metal context
static MetalContext metalContext = {0};

/**
 * Initialize the Metal environment.
 * @return true if initialization was successful, false otherwise
 */
static bool initMetal() {
  @autoreleasepool {
    // Get the default Metal device
    metalContext.device = MTLCreateSystemDefaultDevice();
    if (!metalContext.device) {
      fprintf(stderr, "Metal: Error creating system default device\n");
      return false;
    }
    
    // Create command queue
    metalContext.commandQueue = [metalContext.device newCommandQueue];
    if (!metalContext.commandQueue) {
      fprintf(stderr, "Metal: Error creating command queue\n");
      return false;
    }
    
    // Load Metal library from the default bundle
    NSError* error = nil;
    NSString* metalLibraryPath = [[NSBundle mainBundle] pathForResource:@"hvmn" ofType:@"metallib"];
    
    if (metalLibraryPath) {
      // Load pre-compiled library if available
      NSURL* metalLibraryURL = [NSURL fileURLWithPath:metalLibraryPath];
      metalContext.library = [metalContext.device newLibraryWithURL:metalLibraryURL error:&error];
    } else {
      // If no pre-compiled library, load the source code and compile it
      NSString* shaderSource = [[NSBundle mainBundle] pathForResource:@"hvmn" ofType:@"metal"];
      
      if (shaderSource) {
        metalContext.library = [metalContext.device newLibraryWithSource:shaderSource
                                                                options:nil
                                                                  error:&error];
      } else {
        // As a last resort, use the shader source from the implementation file
        NSString* shaderPath = [[NSBundle mainBundle] pathForResource:@"hvmn" ofType:@"metal"];
        NSString* shaderSource = [NSString stringWithContentsOfFile:shaderPath
                                                         encoding:NSUTF8StringEncoding
                                                            error:&error];
        
        if (shaderSource) {
          metalContext.library = [metalContext.device newLibraryWithSource:shaderSource
                                                                   options:nil
                                                                     error:&error];
        }
      }
    }
    
    if (!metalContext.library) {
      fprintf(stderr, "Metal: Error creating library: %s\n", 
             error ? [[error localizedDescription] UTF8String] : "unknown error");
      return false;
    }
    
    // Get the normalize function from the library
    metalContext.normalizeFunction = [metalContext.library newFunctionWithName:@"normalizeKernel"];
    if (!metalContext.normalizeFunction) {
      fprintf(stderr, "Metal: Failed to find the normalizeKernel function\n");
      return false;
    }
    
    // Create compute pipeline
    metalContext.normalizePipeline = [metalContext.device newComputePipelineStateWithFunction:metalContext.normalizeFunction
                                                                                error:&error];
    if (!metalContext.normalizePipeline) {
      fprintf(stderr, "Metal: Error creating compute pipeline: %s\n", 
             [[error localizedDescription] UTF8String]);
      return false;
    }
    
    metalContext.initialized = true;
    return true;
  }
}

/**
 * Check if Metal is available on this system.
 * @return 1 if Metal is available, 0 otherwise
 */
extern "C" int hvmn_metal_available() {
  @autoreleasepool {
    id<MTLDevice> device = MTLCreateSystemDefaultDevice();
    return device != nil;
  }
}

/**
 * Normalize a term using Metal.
 * @param hvmn The HVMN context
 * @param term The term to normalize
 * @return The normalized term
 */
extern "C" Term hvmn_normal_metal(HVMN* hvmn, Term term) {
  @autoreleasepool {
    // Initialize Metal if not already done
    if (!metalContext.initialized) {
      if (!initMetal()) {
        fprintf(stderr, "Metal: Failed to initialize Metal. Falling back to CPU.\n");
        return term;
      }
    }
    
    // Get heap and stack parameters
    uint32_t heap_size = hvmn->heap_size;
    uint32_t stack_size = hvmn->stack_size;
    uint32_t heap_pos = hvmn->heap_pos;
    uint32_t stack_pos = 0;
    uint32_t interactions = 0; // Use uint32_t for Metal compatibility
    
    // Create Metal buffers
    metalContext.heapBuffer = [metalContext.device newBufferWithLength:heap_size * sizeof(Term)
                                                             options:MTLResourceStorageModeShared];
    
    metalContext.stackBuffer = [metalContext.device newBufferWithLength:stack_size * sizeof(Term)
                                                              options:MTLResourceStorageModeShared];
    
    metalContext.heapPosBuffer = [metalContext.device newBufferWithBytes:&heap_pos
                                                               length:sizeof(uint32_t)
                                                              options:MTLResourceStorageModeShared];
    
    metalContext.stackPosBuffer = [metalContext.device newBufferWithBytes:&stack_pos
                                                                length:sizeof(uint32_t)
                                                               options:MTLResourceStorageModeShared];
    
    metalContext.interactionsBuffer = [metalContext.device newBufferWithBytes:&interactions
                                                                    length:sizeof(uint32_t)
                                                                   options:MTLResourceStorageModeShared];
    
    metalContext.heapSizeBuffer = [metalContext.device newBufferWithBytes:&heap_size
                                                                length:sizeof(uint32_t)
                                                               options:MTLResourceStorageModeShared];
    
    metalContext.stackSizeBuffer = [metalContext.device newBufferWithBytes:&stack_size
                                                                 length:sizeof(uint32_t)
                                                                options:MTLResourceStorageModeShared];
    
    // Verify buffer allocation
    if (!metalContext.heapBuffer || !metalContext.stackBuffer || 
        !metalContext.heapPosBuffer || !metalContext.stackPosBuffer ||
        !metalContext.interactionsBuffer || !metalContext.heapSizeBuffer ||
        !metalContext.stackSizeBuffer) {
      fprintf(stderr, "Metal: Failed to create buffers\n");
      return term;
    }
    
    // Copy heap data to the Metal buffer
    Term* heapData = (Term*)metalContext.heapBuffer.contents;
    memcpy(heapData, hvmn->heap, hvmn->heap_pos * sizeof(Term));
    
    // Set up Metal command execution
    id<MTLCommandBuffer> commandBuffer = [metalContext.commandQueue commandBuffer];
    id<MTLComputeCommandEncoder> computeEncoder = [commandBuffer computeCommandEncoder];
    
    // Configure the compute command encoder
    [computeEncoder setComputePipelineState:metalContext.normalizePipeline];
    
    // Set buffer arguments for the kernel
    [computeEncoder setBuffer:metalContext.heapBuffer offset:0 atIndex:0];
    [computeEncoder setBuffer:metalContext.stackBuffer offset:0 atIndex:1];
    [computeEncoder setBuffer:metalContext.heapPosBuffer offset:0 atIndex:2];
    [computeEncoder setBuffer:metalContext.stackPosBuffer offset:0 atIndex:3];
    [computeEncoder setBuffer:metalContext.interactionsBuffer offset:0 atIndex:4];
    [computeEncoder setBuffer:metalContext.heapSizeBuffer offset:0 atIndex:5];
    [computeEncoder setBuffer:metalContext.stackSizeBuffer offset:0 atIndex:6];
    
    // Configure grid and threadgroup sizes
    MTLSize gridSize = MTLSizeMake(1, 1, 1);
    MTLSize threadGroupSize = MTLSizeMake(1, 1, 1);
    
    // Dispatch the kernel
    [computeEncoder dispatchThreadgroups:gridSize threadsPerThreadgroup:threadGroupSize];
    [computeEncoder endEncoding];
    
    // Add completion handler
    [commandBuffer addCompletedHandler:^(id<MTLCommandBuffer> buffer) {
      if (buffer.error) {
        NSLog(@"Metal: Command buffer execution failed: %@", buffer.error);
      }
    }];
    
    // Execute the command buffer
    [commandBuffer commit];
    [commandBuffer waitUntilCompleted];
    
    // Read back results
    heap_pos = *(uint32_t*)metalContext.heapPosBuffer.contents;
    stack_pos = *(uint32_t*)metalContext.stackPosBuffer.contents;
    interactions = *(uint32_t*)metalContext.interactionsBuffer.contents;
    
    // Copy data back from Metal buffer to HVMN heap
    memcpy(hvmn->heap, heapData, heap_pos * sizeof(Term));
    
    // Update HVMN state
    hvmn->heap_pos = heap_pos;
    hvmn->interactions += interactions;
    
    // Return the normalized term
    return hvmn->heap[0];
  }
}

/**
 * Compile the Metal shader file.
 * @param metal_file_path Path to the Metal shader file
 * @return true if compilation was successful, false otherwise
 */
extern "C" bool hvmn_metal_compile_shader(const char* metal_file_path) {
  @autoreleasepool {
    // Initialize Metal if not already done
    if (!metalContext.initialized) {
      if (!initMetal()) {
        return false;
      }
    }
    
    NSError* error = nil;
    NSString* sourcePath = [NSString stringWithUTF8String:metal_file_path];
    NSString* shaderSource = [NSString stringWithContentsOfFile:sourcePath
                                                    encoding:NSUTF8StringEncoding
                                                       error:&error];
    
    if (!shaderSource) {
      fprintf(stderr, "Metal: Error reading shader source: %s\n",
             error ? [[error localizedDescription] UTF8String] : "unknown error");
      return false;
    }
    
    // Compile the shader
    id<MTLLibrary> library = [metalContext.device newLibraryWithSource:shaderSource
                                                            options:nil
                                                              error:&error];
    
    if (!library) {
      fprintf(stderr, "Metal: Error compiling shader: %s\n",
             error ? [[error localizedDescription] UTF8String] : "unknown error");
      return false;
    }
    
    // Check that our function exists
    id<MTLFunction> normalizeFunction = [library newFunctionWithName:@"normalizeKernel"];
    if (!normalizeFunction) {
      fprintf(stderr, "Metal: Failed to find the normalizeKernel function\n");
      return false;
    }
    
    printf("Metal: Shader compiled successfully\n");
    return true;
  }
}