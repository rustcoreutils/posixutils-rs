# Guide to working on pcc, our C99 compiler

## Overview

pcc is a C99 compiler targeting POSIX.2024 compliance. It currently supports x86-64 and AArch64 (ARM64) on Linux and macOS.

Reference: [ISO/IEC 9899:1999 (C99)](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf)

## Quick start

Build: `cargo build && cargo build --release`

Testing (compiler subset): `cargo test --release -p posixutils-cc`

Debugging, via stdio:
```
echo 'int main() { return 42; }' | ./target/release/pcc - -S -o -
```

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
| `ir/linearize.rs` | AST → IR conversion, SSA construction |
| `ir/mod.rs` | Intermediate representation definitions |
| `ir/ssa.rs` | SSA phi node insertion |
| `ir/lower.rs` | IR lowering passes (phi elimination) |
| `ir/dominate.rs` | Dominator tree and dominance frontiers |
| `ir/dce.rs` | Dead code elimination |
| `ir/instcombine.rs` | Instruction combining/simplification |
| `arch/x86_64/` | x86-64 code generator |
| `arch/aarch64/` | AArch64 code generator |
| `arch/lir.rs` | Low-level IR (LIR) definitions |
| `arch/regalloc.rs` | Register allocation |

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
- GCC-compatible inline assembly (extended asm with constraints, clobbers, asm goto)

Not yet implemented (features we want to add):
- -fverbose-asm
- top builtins to implement:
  __builtin_expect
  __sync_synchronize
  __sync_fetch_and_add (and maybe a couple of its siblings)
- assembly peephole optimizations
- C11 atomics (_Atomic operations)
- Other C11 features: _Generic, anonymous structs

## Code Quality

Please run `cargo fmt` before committing code, and `cargo clippy` regularly while working. Code should build without warnings.

```bash
cargo fmt && cargo clippy -p posixutils-cc
```

DO NOT `allow(dead_code)` to fix warnings. Instead, remove dead code; do
not leave it around as a maintenance burden (and LLM token
tax).

Read CONTRIBUTING.md in the root of the repository for more details.
