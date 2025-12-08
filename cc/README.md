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

- `-S -o -` - Output assembly to stdout (standard clang/gcc option)
- `--dump-ir` - Output IR before code generation

Examples:

```bash
# Compile from stdin, view generated assembly
echo 'int main() { return 42; }' | ./target/release/pcc - -S -o -

# View IR for a source file
./target/release/pcc myfile.c --dump-ir

# Using heredoc for multi-line test cases
./target/release/pcc - -S -o - <<'EOF'
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
- C99 standard

Not yet implemented (exceptions to C99, or features we want to add):
- Actual inlining optimization (the `inline` keyword is supported but functions are not inlined)
- multi-register returns (for structs larger than 8 bytes)
- -fverbose-asm
- VLAs (variable-length arrays)
- top builtins to implement:
  __builtin_expect
  __sync_synchronize
  __sync_fetch_and_add (and maybe a couple of its siblings)
- assembly peephole optimizations
- _Complex
- C11 Alignment Specifiers (_Alignas, _Alignof)
- C11 Thread-Local Storage (_Thread_local) and atomics (_Atomic)
- Other C11 features: _Static_assert, _Generic, anonymous structs

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
