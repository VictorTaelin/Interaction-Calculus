CC = gcc
CFLAGS = -w -std=c99 -O2
SRC_DIR = src
OBJ_DIR = obj
BIN_DIR = bin

# Main source files
SRCS = $(SRC_DIR)/main.c \
       $(SRC_DIR)/memory.c \
       $(SRC_DIR)/parse.c \
       $(SRC_DIR)/show.c \
       $(SRC_DIR)/whnf.c

# Interaction functions (will be implemented later)
# INT_SRCS = $(wildcard $(SRC_DIR)/interactions/*.c)

# Objects
OBJS = $(SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)
# INT_OBJS = $(INT_SRCS:$(SRC_DIR)/interactions/%.c=$(OBJ_DIR)/interactions/%.o)

# Executable
TARGET = $(BIN_DIR)/main

# Directories
DIRS = $(OBJ_DIR) $(BIN_DIR) # $(OBJ_DIR)/interactions

.PHONY: all clean

all: $(DIRS) $(TARGET)

$(DIRS):
	mkdir -p $@

$(TARGET): $(OBJS) # $(INT_OBJS)
	$(CC) $(CFLAGS) -o $@ $^

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) -c -o $@ $<

# $(OBJ_DIR)/interactions/%.o: $(SRC_DIR)/interactions/%.c
# 	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)
