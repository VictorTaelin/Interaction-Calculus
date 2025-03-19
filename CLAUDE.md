# IC Project Guide

## Important Files
- `README.md` - Project Spec (ALWAYS READ IT)
- `src/main.c` - Program entry point (CLI)
- `src/ic.h` - The complete IC runtime
- `src/parse.[c|h]` - Term parsing
- `src/show.[c|h]` - Term stringification

## Build Commands
- Build the project: `make`
- Clean build artifacts: `make clean`
- Run with custom test term: `./bin/main "(λf.λx.(f (f (f x))) λb.(b λt.λf.f λt.λf.t) λt.λf.t)"`

## Code Style
- Use C99 standard with portable implementation 
- Functions and variables should use `snake_case`
- Constants should be in `UPPER_CASE`
- Use 2 space indentation
