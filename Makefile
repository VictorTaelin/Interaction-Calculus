CC = gcc
CFLAGS = -w -std=c99 -O3 -march=native -mtune=native -flto -ffast-math -funroll-loops -finline-functions -fpeel-loops
SRC_DIR = src
OBJ_DIR = obj
BIN_DIR = bin

# Check if NVCC exists and is usable
NVCC_CHECK := $(shell which nvcc 2>/dev/null || echo "")

ifneq ($(NVCC_CHECK),)
  # NVCC is available
  NVCC := $(NVCC_CHECK)
  NVCCFLAGS = -O3 --std=c++11 -w
  CUDA_CFLAGS = -DHAVE_CUDA
  CUDA_SRCS = $(SRC_DIR)/ic.cu
  CUDA_OBJS = $(OBJ_DIR)/ic.o
  CUDA_LDFLAGS = -lcudart
  HAS_CUDA = 1
else
  # No NVCC available
  CUDA_SRCS =
  CUDA_OBJS =
  CUDA_CFLAGS =
  CUDA_LDFLAGS =
  HAS_CUDA = 0
endif

# Check if we're on macOS for Metal support
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
  # Check if xcrun exists (required for Metal)
  METAL_CHECK := $(shell which xcrun 2>/dev/null || echo "")
  ifneq ($(METAL_CHECK),)
    # Compile Metal on macOS
    METAL_CFLAGS = -DHAVE_METAL
    METAL_LDFLAGS = -framework Metal -framework Foundation -lc++
    METAL_SRCS = $(SRC_DIR)/ic_metal.mm $(SRC_DIR)/ic.metal
    METAL_OBJS = $(OBJ_DIR)/ic_metal.o
    HAS_METAL = 1

    # Use clang for Objective-C++ compilation
    CXX = clang++
    OBJCXX = clang++
    OBJCXXFLAGS = -fobjc-arc -O3 -std=c++14

    # Metal compiler
    METAL_COMPILER = xcrun -sdk macosx metal
    METAL_COMPILER_FLAGS = -O
    METAL_OUTPUT = $(BIN_DIR)/ic.metallib
  else
    # No Metal available
    METAL_SRCS =
    METAL_OBJS =
    METAL_CFLAGS =
    METAL_LDFLAGS =
    HAS_METAL = 0
  endif
else
  # Not macOS, no Metal
  METAL_SRCS =
  METAL_OBJS =
  METAL_CFLAGS =
  METAL_LDFLAGS =
  HAS_METAL = 0
endif

# Main source files
SRCS = $(SRC_DIR)/main.c \
       $(SRC_DIR)/ic.c \
       $(SRC_DIR)/show.c \
       $(SRC_DIR)/parse.c

# All parser source files - updated for new structure
PARSE_SRCS = $(wildcard $(SRC_DIR)/parse/*.c) \
             $(wildcard $(SRC_DIR)/parse/term/*.c)

# Objects
OBJS = $(SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)
PARSE_OBJS = $(PARSE_SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)

# Executable
TARGET = $(BIN_DIR)/main
TARGET_LN = $(BIN_DIR)/ic

# Directories
DIRS = $(OBJ_DIR) $(BIN_DIR) $(OBJ_DIR)/parse \
       $(OBJ_DIR)/parse/term

.PHONY: all clean check-cuda remote-build remote-run

all: $(DIRS) $(TARGET) $(TARGET_LN)

$(DIRS):
	mkdir -p $@

# Build target with CUDA, Metal, or neither
ifeq ($(HAS_CUDA),1)
ifeq ($(HAS_METAL),1)
$(TARGET): $(OBJS) $(PARSE_OBJS) $(CUDA_OBJS) $(METAL_OBJS) $(METAL_OUTPUT)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(PARSE_OBJS) $(CUDA_OBJS) $(METAL_OBJS) $(CUDA_LDFLAGS) $(METAL_LDFLAGS)
else
$(TARGET): $(OBJS) $(PARSE_OBJS) $(CUDA_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(CUDA_LDFLAGS)
endif
else
ifeq ($(HAS_METAL),1)
$(TARGET): $(OBJS) $(PARSE_OBJS) $(METAL_OBJS) $(METAL_OUTPUT)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(PARSE_OBJS) $(METAL_OBJS) $(METAL_LDFLAGS)
else
$(TARGET): $(OBJS) $(PARSE_OBJS)
	$(CC) $(CFLAGS) -o $@ $^
endif
endif

$(TARGET_LN): $(TARGET)
	ln -sf main $(TARGET_LN)

# Compile Metal shader library
$(METAL_OUTPUT): $(SRC_DIR)/ic.metal | $(BIN_DIR)
	$(METAL_COMPILER) $(METAL_COMPILER_FLAGS) -o $@ $<

# Compile C files
ifeq ($(HAS_CUDA),1)
ifeq ($(HAS_METAL),1)
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) $(CUDA_CFLAGS) $(METAL_CFLAGS) -c -o $@ $<
else
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) $(CUDA_CFLAGS) -c -o $@ $<
endif
else
ifeq ($(HAS_METAL),1)
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) $(METAL_CFLAGS) -c -o $@ $<
else
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) -c -o $@ $<
endif
endif

# Compile CUDA
ifeq ($(HAS_CUDA),1)
$(OBJ_DIR)/ic.o: $(SRC_DIR)/ic.cu
	$(NVCC) $(NVCCFLAGS) -c -o $@ $<
endif

# Compile Metal Objective-C++
ifeq ($(HAS_METAL),1)
$(OBJ_DIR)/ic_metal.o: $(SRC_DIR)/ic_metal.mm
	$(OBJCXX) $(OBJCXXFLAGS) $(METAL_CFLAGS) -c -o $@ $<
endif

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

# Show GPU/Metal compilation status
status:
ifeq ($(HAS_CUDA),1)
	@echo "CUDA compiler found ($(NVCC)). Building with CUDA GPU support."
else
	@echo "CUDA compiler not found. Building without CUDA GPU support."
endif

ifeq ($(HAS_METAL),1)
	@echo "Metal supported on this system. Building with Metal GPU support."
else
	@echo "Metal not supported on this system. Building without Metal GPU support."
endif

metal-status: $(TARGET)
	@echo "Testing Metal availability..."
	@./$(TARGET) eval-gpu "λx.x" 2>&1 | grep -i "Metal" || true
