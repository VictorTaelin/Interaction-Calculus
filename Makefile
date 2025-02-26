CC = gcc
CFLAGS = -w -std=c99 -O2
SRC_DIR = src
OBJ_DIR = obj
BIN_DIR = bin

# Main source files
SRCS = $(SRC_DIR)/main.c \
       $(SRC_DIR)/memory.c \
       $(SRC_DIR)/show.c \
       $(SRC_DIR)/whnf.c

# All parser source files - updated for new structure
PARSE_SRCS = $(wildcard $(SRC_DIR)/parse/*.c) \
             $(wildcard $(SRC_DIR)/parse/term/*.c)

# Interaction functions
INT_SRCS = $(SRC_DIR)/interactions/app_lam.c \
           $(SRC_DIR)/interactions/app_sup.c \
           $(SRC_DIR)/interactions/col_sup.c \
           $(SRC_DIR)/interactions/col_lam.c \
           $(SRC_DIR)/interactions/other_rules.c

# Objects
OBJS = $(SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)
PARSE_OBJS = $(PARSE_SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)
INT_OBJS = $(INT_SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)

# Executable
TARGET = $(BIN_DIR)/main
TARGET_LN = $(BIN_DIR)/suptt

# Directories
DIRS = $(OBJ_DIR) $(BIN_DIR) $(OBJ_DIR)/interactions $(OBJ_DIR)/parse \
       $(OBJ_DIR)/parse/term

.PHONY: all clean

all: $(DIRS) $(TARGET) $(TARGET_LN)

$(DIRS):
	mkdir -p $@

$(TARGET): $(OBJS) $(PARSE_OBJS) $(INT_OBJS)
	$(CC) $(CFLAGS) -o $@ $^

$(TARGET_LN): $(TARGET)
	ln -sf main $(TARGET_LN)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)