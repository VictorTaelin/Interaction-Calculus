CC = gcc
CFLAGS = -w -std=c99 -O3 -march=native -mtune=native -flto
SRC_DIR = src
OBJ_DIR = obj
BIN_DIR = bin

# Metal GPU acceleration is the only supported GPU backend

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
       $(SRC_DIR)/collapse.c \
       $(SRC_DIR)/show.c \
       $(SRC_DIR)/parse.c

# Parser is now included in the main source files
# Objects
OBJS = $(SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)

# Executable
TARGET = $(BIN_DIR)/main
TARGET_LN = $(BIN_DIR)/ic

# Directories
DIRS = $(OBJ_DIR) $(BIN_DIR)

.PHONY: all clean status metal-status

all: $(DIRS) $(TARGET) $(TARGET_LN)

$(DIRS):
	mkdir -p $@

# Build target with Metal or CPU-only
ifeq ($(HAS_METAL),1)
$(TARGET): $(OBJS) $(METAL_OBJS) $(METAL_OUTPUT)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(METAL_OBJS) $(METAL_LDFLAGS)
else
$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^
endif

$(TARGET_LN): $(TARGET)
	ln -sf main $(TARGET_LN)

# Compile Metal shader library
$(METAL_OUTPUT): $(SRC_DIR)/ic.metal | $(BIN_DIR)
	$(METAL_COMPILER) $(METAL_COMPILER_FLAGS) -o $@ $<

# Compile C files
ifeq ($(HAS_METAL),1)
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) $(METAL_CFLAGS) -c -o $@ $<
else
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) -c -o $@ $<
endif

# Compile Metal Objective-C++
ifeq ($(HAS_METAL),1)
$(OBJ_DIR)/ic_metal.o: $(SRC_DIR)/ic_metal.mm
	$(OBJCXX) $(OBJCXXFLAGS) $(METAL_CFLAGS) -c -o $@ $<
endif

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

# Show GPU acceleration status
status:
ifeq ($(HAS_METAL),1)
	@echo "Metal supported on this system. Building with Metal GPU support."
else
	@echo "No GPU acceleration available. Building CPU-only version."
endif

metal-status: $(TARGET)
	@echo "Testing Metal availability..."
	@./$(TARGET) eval-gpu "λx.x" 2>&1 | grep -i "Metal" || true
