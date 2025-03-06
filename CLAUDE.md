# HVMN Project Guide

## Important Files
- `HVM-Nano.md` - Project Spec (ALWAYS READ IT)
- `src/main.c` - Program entry point (CLI)
- `src/hvmn.h` - The complete HVM-Nano runtime
- `src/parse.[c|h]` - Term parsing
- `src/show.[c|h]` - Term stringification

## Build Commands
- Build the project: `make`
- Clean build artifacts: `make clean`
- Run with default test term: `./bin/hvmn`
- Run with custom test term: `./bin/hvmn "((λf.λx.\!&0{f0,f1}=f;(f0 (f1 x)) λB.λT.λF.((B F) T)) λa.λb.a)"`

## Code Style
- Use C99 standard with portable implementation 
- Implement 32-bit term representation as specified in HVM-Nano.md
- Use `snake_case` for functions and variables
- Constants should be in `UPPER_CASE`
- Use 2 space indentation
- Include error handling for all parser functions
