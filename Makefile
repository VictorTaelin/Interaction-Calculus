CC = gcc
CFLAGS = -w -std=c99 -O3 -march=native -mtune=native -flto
SRC_DIR = src
OBJ_DIR = obj
BIN_DIR = bin

# Check if NVCC exists and is usable
NVCC_CHECK := $(shell which nvcc 2>/dev/null || echo "")

ifneq ($(NVCC_CHECK),)
  # NVCC is available
  NVCC := $(NVCC_CHECK)
  NVCCFLAGS = -O3 --std=c++11 -arch=sm_75 --use_fast_math -Xptxas -O3,-v \
    -prec-div=false -prec-sqrt=false -fmad=true -res-usage \
    --compiler-options -ffast-math,-march=native,-mtune=native,-O3,-funroll-loops
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

# Build target with or without CUDA
ifeq ($(HAS_CUDA),1)
$(TARGET): $(OBJS) $(PARSE_OBJS) $(CUDA_OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(CUDA_LDFLAGS)
else
$(TARGET): $(OBJS) $(PARSE_OBJS)
	$(CC) $(CFLAGS) -o $@ $^
endif

$(TARGET_LN): $(TARGET)
	ln -sf main $(TARGET_LN)

ifeq ($(HAS_CUDA),1)
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) $(CUDA_CFLAGS) -c -o $@ $<
else
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) -c -o $@ $<
endif

ifeq ($(HAS_CUDA),1)
$(OBJ_DIR)/ic.o: $(SRC_DIR)/ic.cu
	$(NVCC) $(NVCCFLAGS) -c -o $@ $<
endif

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

# Show GPU compilation status
status:
ifeq ($(HAS_CUDA),1)
	@echo "CUDA compiler found ($(NVCC)). Building with GPU support."
else
	@echo "CUDA compiler not found. Building without GPU support."
endif
