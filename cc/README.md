# Guide to working on c17, our C99 compiler

## Overview

c17 is a C17 compiler (ISO/IEC 9899:2018) targeting POSIX.2024 compliance. C17 is a defect-report revision of C11 and adds no features over it, so the language delta tracked below is C11's. It supports x86-64 and AArch64 (ARM64) on Linux and macOS.

References:
- [ISO/IEC 9899:1999 (C99)](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf) — baseline
- [ISO/IEC 9899:2011 (C11)](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf) — delta tracked in [doc/c11-checklist.md](doc/c11-checklist.md)

## Quick start

Build: `cargo build && cargo build --release`

Testing (compiler subset): `cargo test --release -p posixutils-cc`

Debugging, via stdio:
```
echo 'int main() { return 42; }' | ./target/release/c17 - -S -o -
```

## Architecture

The compiler pipeline:

```
Source → Lexer → Preprocessor → Parser → Type Check → Linearize
       → Mapping (target lowering) → Optimize → Lower (φ → copies)
       → Codegen → Assembly
```

Key source files:

| File / Dir | Purpose |
|------------|---------|
| `main.rs` | Driver CLI (c17 binary): arg parsing, pipeline orchestration, dump-ir stages |
| `lib.rs` | Library entry point (re-exports for tests) |
| `token/lexer.rs` | Tokenization |
| `token/preprocess.rs` | C preprocessor (`#include`, `#define`, `#ifdef`, …) |
| `parse/parser.rs` | Recursive descent parser producing AST |
| `parse/expression.rs` | Expression parsing and constant-expression evaluation |
| `parse/ast.rs` | AST node definitions |
| `types.rs` | C type system |
| `strings.rs` | String interning (`StringId`); pre-interns keywords at startup |
| `kw.rs` | Pre-interned keyword constants and tag-based classification |
| `symbol.rs` | Symbol table with scope management |
| `target.rs` | Target triple, pointer/long widths, predefined macros |
| `builtins.rs`, `builtin_headers.rs` | `__builtin_*` recognition and bundled headers |
| `include/` | Bundled freestanding headers (`stdarg.h`, `stdatomic.h`, `float.h`, …) |
| `diag.rs` | Diagnostics, source-stream tracking, error/warning counts |
| `rtlib.rs` | Runtime library helpers (libgcc / compiler-rt selection) |
| `os/` | OS-specific knobs (linux, macos, freebsd) |
| `abi/` | Per-ABI classification: `sysv_amd64.rs`, `aapcs64.rs` |
| `ir/mod.rs` | IR definitions (opcodes, pseudos, instructions, functions). See `ir/README.md`. |
| `ir/linearize.rs` (+ `_init.rs`, `_stmt.rs`, `_emit.rs`) | AST → IR conversion, SSA construction |
| `ir/ssa.rs` | φ-node insertion |
| `ir/dominate.rs` | Dominator tree and dominance frontiers (Cooper) |
| `ir/dce.rs` | Dead code elimination |
| `ir/instcombine.rs` | Constant folding and algebraic simplification |
| `ir/inline.rs` | Function inlining |
| `ir/lower.rs` | IR lowering (φ elimination to copies) |
| `opt.rs` | Optimization pass driver (`InstCombine` + `DCE` to fixed point, after inlining) |
| `arch/mapping.rs` | Target-neutral hardware mapping (int128 expansion, ABI shaping) |
| `arch/regalloc.rs` | Register allocation framework (shared) |
| `arch/lir.rs` | Low-level IR (LIR) definitions |
| `arch/codegen.rs`, `arch/dwarf.rs` | Common codegen helpers, DWARF emission |
| `arch/x86_64/` | x86-64 code generator (incl. x87 long-double in `x87.rs`) |
| `arch/aarch64/` | AArch64 code generator |
| `cflow.rs`, `ctags.rs`, `cxref.rs` | POSIX `cflow` / `ctags` / `cxref` tools sharing the parser |

## Debugging

The compiler supports C input via `-` for stdin, and can output intermediate representations:

- `-S -o -` — Output assembly to stdout (standard clang/gcc option)
- `--dump-ir [<stage>]` — Dump IR at a pipeline stage. Stages: `post-linearize`, `post-mapping`, `post-opt`, `post-lower`, `all`. Bare `--dump-ir` defaults to `post-opt`.
- `--dump-ir-func <name>` — Limit IR dump to one function (use with `--dump-ir`)
- `--dump-ast` — Parse and dump AST to stdout
- `--dump-tokens` — Dump preprocessed token stream
- `-E` — Run the preprocessor only

Examples:

```bash
# Compile from stdin, view generated assembly
echo 'int main() { return 42; }' | ./target/release/c17 - -S -o -

# View IR for a source file
./target/release/c17 myfile.c --dump-ir

# Using heredoc for multi-line test cases
./target/release/c17 - -S -o - <<'EOF'
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
- C99 standard (see [doc/c99-checklist.md](doc/c99-checklist.md))
- C11 deltas: `_Generic`, `_Atomic` / `<stdatomic.h>` (including access through ordinary operators), `<tgmath.h>`, `_Noreturn`, `_Static_assert`, `_Alignas` / `_Alignof`, `_Thread_local` (Local-Exec and Initial-Exec models), anonymous struct/union members, Unicode literals. See [doc/c11-checklist.md](doc/c11-checklist.md) for full delta.
- GCC-compatible inline assembly: extended asm with constraints, clobbers, named operands, matching constraints, `asm goto` with labels

Not yet implemented (features we want to add):
- TLS General-Dynamic model (for `_Thread_local` in shared libraries — `__tls_get_addr` path)
- `-fverbose-asm`
- assembly peephole optimizations

Will not implement:
- C99 `_Imaginary` type (removed in C11; no mainstream compiler implements it)

Off by default:
- Trigraphs. They were deprecated in C99 but **not removed until C23**, so C17
  still mandates them and POSIX's RATIONALE notes that supporting them is the
  conforming behavior. `--trigraphs` enables translation phase 1. The default
  is off because the replacement applies everywhere, including inside string
  literals — `"What??!"` becomes `"What|"` — and `??` is far likelier to appear
  by accident than by intent. GCC and Clang default them off for the same reason.

## Runtime dependencies

`c17` does not implement translation phase 8 itself. It shells out to `as` to
assemble and to `cc` to link, so **both must be on `$PATH`**. There is no crt
object selection, no dynamic-linker path resolution, and no explicit `-lc` or
`-lgcc` anywhere in the crate.

This is a deliberate choice and it satisfies the implicit `-l c` and the
executable-permission mandates transitively, since every conforming host `cc`
does those things. The consequence worth knowing is that `c17` inherits the
host driver's crt and runtime-library decisions rather than making its own.

## Code Quality

Please run `cargo fmt` before committing code, and `cargo clippy` regularly while working. Code should build without warnings.

```bash
cargo fmt && cargo clippy -p posixutils-cc
```

DO NOT `allow(dead_code)` to fix warnings. Instead, remove dead code; do
not leave it around as a maintenance burden (and LLM token
tax).

Read CONTRIBUTING.md in the root of the repository for more details.
