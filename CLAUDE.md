# IC Project Guide

## Important Files
- `InteractionCalculus.md` - PRIMARY SPECIFICATION for the entire project
- `src/main.c` - Program entry point and test runner
- `src/types.h` - Core data structures and term representation 
- `src/memory.c/h` - Heap management and allocation
- `src/parse.c/h` - Term parsing implementation
- `src/whnf.c/h` - Term normalization
- `src/show.c/h` - Term to string conversions
- `src/interactions/` - Future directory for interaction implementations

## Build Commands
- Build the project: `make`
- Clean build artifacts: `make clean`
- Run with default test term: `./bin/main`
- Run with custom term: `./bin/main "((λf.λx.!&0{f0,f1}=f;(f0 (f1 x)) λB.λT.λF.((B F) T)) λa.λb.a)"`

## Code Style
- Use C99 standard with portable implementation 
- Implement 32-bit term representation as specified in InteractionCalculus.md
- Follow the memory model from InteractionCalculus.md - terms as 32-bit pointers with sub/tag/lab/val fields
- Use `snake_case` for functions and variables
- Constants should be in `UPPER_CASE`
- 2 space indentation
- Include error handling for all parser functions
- Use descriptive variable names that reflect domain terminology
- Add comments for complex interactions and term operations
- Keep header files minimal with only necessary declarations
- Implement core interactions as specified in the InteractionCalculus.md spec
