#import <Foundation/Foundation.h>
#import <Metal/Metal.h>
#include "ic.h"

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

// Initialize the Metal environment
static bool initMetal() {
    @autoreleasepool {
        // Get the default Metal device
        metalContext.device = MTLCreateSystemDefaultDevice();
        if (!metalContext.device) {
            fprintf(stderr, "Metal: Error creating system default device\n");
            return false;
        }
        
        NSLog(@"Metal: Using device %@", [metalContext.device name]);
        
        // Create command queue
        metalContext.commandQueue = [metalContext.device newCommandQueue];
        if (!metalContext.commandQueue) {
            fprintf(stderr, "Metal: Error creating command queue\n");
            return false;
        }
        
        // Load Metal library from the default bundle
        NSError* error = nil;
        NSString* metalLibraryPath = [[NSBundle mainBundle] pathForResource:@"ic" ofType:@"metallib"];
        
        if (metalLibraryPath) {
            // Load pre-compiled library if available
            metalContext.library = [metalContext.device newLibraryWithFile:metalLibraryPath error:&error];
        } else {
            // If no pre-compiled library, load the source code and compile it
            NSString* shaderSource = [[NSBundle mainBundle] pathForResource:@"ic" ofType:@"metal"];
            
            if (shaderSource) {
                metalContext.library = [metalContext.device newLibraryWithSource:shaderSource
                                                                         options:nil
                                                                           error:&error];
            } else {
                // As a last resort, use the shader source from the implementation file
                NSString* shaderPath = [[NSBundle mainBundle] pathForResource:@"ic" ofType:@"metal"];
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

// Check if Metal is available on this system
extern "C" int ic_metal_available() {
    @autoreleasepool {
        id<MTLDevice> device = MTLCreateSystemDefaultDevice();
        return device != nil;
    }
}

// Normalize a term using Metal
extern "C" Term ic_normal_metal(IC* ic, Term term) {
    // Debug outputs
    printf("Metal: Starting normalization\n");
    printf("Metal: Initial heap_pos = %u\n", ic->heap_pos);
    printf("Metal: Initial interactions = %llu\n", ic->interactions);
    
    @autoreleasepool {
        // Initialize Metal if not already done
        if (!metalContext.initialized) {
            if (!initMetal()) {
                fprintf(stderr, "Metal: Failed to initialize Metal. Falling back to CPU.\n");
                return term;
            }
        }
        
        // Ensure we have valid heap and stack sizes
        uint32_t heap_size = ic->heap_size;
        uint32_t stack_size = ic->stack_size;
        uint32_t heap_pos = ic->heap_pos;
        uint32_t stack_pos = 0;
        uint32_t interactions = 0; // Use uint32_t for Metal compatibility
        
        // Create metal buffers
        printf("Metal: Allocating heap buffer (%u terms, %zu bytes)\n", 
               heap_size, heap_size * sizeof(Term));
        
        // Create or recreate buffers if needed
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
        
        // Check if buffers were created successfully
        if (!metalContext.heapBuffer || !metalContext.stackBuffer || 
            !metalContext.heapPosBuffer || !metalContext.stackPosBuffer ||
            !metalContext.interactionsBuffer || !metalContext.heapSizeBuffer ||
            !metalContext.stackSizeBuffer) {
            fprintf(stderr, "Metal: Failed to create buffers\n");
            return term;
        }
        
        // Copy heap data to the Metal buffer
        printf("Metal: Copying heap data to Metal buffer (%u terms)\n", ic->heap_pos);
        Term* heapData = (Term*)metalContext.heapBuffer.contents;
        memcpy(heapData, ic->heap, ic->heap_pos * sizeof(Term));
        
        // Create command buffer and compute command encoder
        id<MTLCommandBuffer> commandBuffer = [metalContext.commandQueue commandBuffer];
        id<MTLComputeCommandEncoder> computeEncoder = [commandBuffer computeCommandEncoder];
        
        // Set compute pipeline
        [computeEncoder setComputePipelineState:metalContext.normalizePipeline];
        
        // Set buffers
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
        printf("Metal: Dispatching kernel\n");
        [computeEncoder dispatchThreadgroups:gridSize threadsPerThreadgroup:threadGroupSize];
        [computeEncoder endEncoding];
        
        // Add completion handler
        [commandBuffer addCompletedHandler:^(id<MTLCommandBuffer> buffer) {
            if (buffer.error) {
                NSLog(@"Metal: Command buffer execution failed: %@", buffer.error);
            } else {
                NSLog(@"Metal: Command buffer execution completed successfully");
            }
        }];
        
        // Commit and wait for completion
        printf("Metal: Committing command buffer\n");
        [commandBuffer commit];
        [commandBuffer waitUntilCompleted];
        
        // Read back results
        printf("Metal: Reading results\n");
        heap_pos = *(uint32_t*)metalContext.heapPosBuffer.contents;
        stack_pos = *(uint32_t*)metalContext.stackPosBuffer.contents;
        interactions = *(uint32_t*)metalContext.interactionsBuffer.contents;
        
        // Copy data back from Metal buffer to IC heap
        printf("Metal: Copying results back to heap (%u terms)\n", heap_pos);
        memcpy(ic->heap, heapData, heap_pos * sizeof(Term));
        
        // Update IC state
        ic->heap_pos = heap_pos;
        ic->interactions += interactions;
        
        printf("Metal: Final heap_pos = %u\n", ic->heap_pos);
        printf("Metal: Interactions performed = %u\n", interactions);
        
        // Return the normalized term
        return ic->heap[0];
    }
}

// Add a convenience function to compile the Metal shader file
extern "C" bool ic_metal_compile_shader(const char* metal_file_path) {
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