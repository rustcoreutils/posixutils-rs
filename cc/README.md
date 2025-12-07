# Guide to working on pcc, our C99 compiler

## Overview

pcc is a C99 compiler targeting POSIX.2024 compliance. It currently supports x86-64 and AArch64 (ARM64) on Linux and macOS.

Reference: [ISO/IEC 9899:1999 (C99)](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf)

## Architecture

The compiler pipeline:

```
Source → Lexer → Preprocessor → Parser → Type Check → Linearize → SSA → Lower → Codegen → Assembly
```

Key source files:

| File | Purpose |
|------|---------|
| `token/lexer.rs` | Tokenization |
| `token/preprocess.rs` | C preprocessor (#include, #define, #ifdef, etc.) |
| `parse/parser.rs` | Recursive descent parser producing AST |
| `parse/ast.rs` | AST node definitions |
| `types.rs` | C type system |
| `symbol.rs` | Symbol table with scope management |
| `linearize.rs` | AST → IR conversion, SSA construction |
| `ir.rs` | Intermediate representation definitions |
| `ssa.rs` | SSA phi node insertion |
| `lower.rs` | IR lowering passes (phi elimination) |
| `dominate.rs` | Dominator tree and dominance frontiers |
| `codegen/x86_64/` | x86-64 code generator |
| `codegen/aarch64/` | AArch64 code generator |

## Building

```bash
cargo build && cargo build --release
```

This builds the whole tree, which is usually the best solution.

## Testing

For frequent compiler development, this runs the compiler subset of tests:

```bash
cargo test --release -p posixutils-cc
```

## Debugging

The compiler supports C input via `-` for stdin, and can output intermediate representations:

- `--dump-asm` - Output assembly to stdout (instead of invoking assembler/linker)
- `--dump-ir` - Output IR before code generation

Examples:

```bash
# Compile from stdin, view generated assembly
echo 'int main() { return 42; }' | ./target/release/pcc - --dump-asm

# View IR for a source file
./target/release/pcc myfile.c --dump-ir

# Using heredoc for multi-line test cases
./target/release/pcc - --dump-asm <<'EOF'
int add(int a, int b) {
    return a + b;
}
int main() {
    return add(1, 2);
}
EOF
```

## Current Limitations

Supported:
- Basic types: void, char, short, int, long, long long (signed/unsigned), float, double, \_Bool
- Pointers (including void\*), arrays, structs, unions
- Control flow: if/else, while, do-while, for, switch, goto
- Functions with parameters and return values
- Function pointer declarations
- Enums
- Variadic functions (va\_list, va\_start, va\_arg, va\_copy, va\_end)
- Storage class specifiers: static, extern
  - Static local variables (block scope with static duration)
  - Static functions (internal linkage)
  - Static global variables (internal linkage)
  - Extern declarations
- C preprocessor basics (#include, #define, #ifdef, #ifndef, #if, #elif, #else, #endif)
- Bitfields (named, unnamed, zero-width for alignment)

Not yet implemented:
- goto, longjmp, setjmp
- `inline` and inlining support
- multi-register returns (for structs larger than 8 bytes)
- -fverbose-asm
- Complex initializers
- VLAs (variable-length arrays)
- _Complex and _Atomic types
- Thread-local storage, alignas, etc.
- top builtins to implement:
  __builtin_expect
  __builtin_clz / clzl / clzll
  __builtin_ctz / ctzl / ctzll
  __sync_synchronize
  __sync_fetch_and_add (and maybe a couple of its siblings)
  __builtin_unreachable (helps optimizations + silences some warnings)
- string interning
- DCE and other opt passes
- assembly peephole optimizations

## Known Issues

### Preprocessor

- **Chained macro expansion after token paste**: When a token paste (`##`) creates an identifier that is itself a function-like macro, the subsequent macro expansion may not work correctly. For example:
  ```c
  #define BAR_test(y) y
  #define FOO(x) BAR_ ## x(1)
  FOO(test)  // Should expand to 1, but may produce incorrect output
  ```

## Code Quality

Please run `cargo fmt` before committing code, and `cargo clippy` regularly while working. Code should build without warnings.

```bash
cargo fmt && cargo clippy -p posixutils-cc
```

DO NOT `allow(dead_code)` to fix warnings. Instead, remove dead code; do
not leave it around as a maintenance burden (and LLM token
tax).

Read CONTRIBUTING.md in the root of the repository for more details.
