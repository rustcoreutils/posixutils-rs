# Guide to working on c17, our C17 compiler

## Overview

c17 implements **C17 (ISO/IEC 9899:2018) only**, plus selected GNU extensions, targeting POSIX.2024 compliance. C17 is a defect-report revision of C11 and adds no features over it. There is one language mode: no `-std=` switching between revisions, and no strict-versus-GNU axis. `-std=` is still accepted, because build systems pass it unconditionally, but a request for an older revision is reported rather than honoured (silence it with `-Wno-c17-dialect`). It supports x86-64 and AArch64 (ARM64) on Linux and macOS.

References:
- [ISO/IEC 9899:2011 (C11)](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf) — C17 is this plus defect reports; the published C17 text is not free
- Conformance findings and known divergences: [audit.md](audit.md)

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
| `constexpr.rs` | Integer constant-expression folding, shared by the parser and the linearizer |
| `float.rs` | Wide floating-point literals and target-width conversion |
| `strings.rs` | String interning (`StringId`); pre-interns keywords at startup |
| `kw.rs` | Pre-interned keyword constants and tag-based classification |
| `symbol.rs` | Symbol table with scope management |
| `target.rs` | Target triple, pointer/long widths, predefined macros |
| `builtins.rs`, `builtin_headers.rs` | `__builtin_*` recognition and bundled headers |
| `include/` | Bundled freestanding headers (`stdarg.h`, `stdatomic.h`, `float.h`, …) |
| `diag.rs` | Diagnostics, source-stream tracking, error/warning counts |
| `rtlib.rs` | Runtime library helpers (libgcc / compiler-rt selection) |
| `linkargs.rs` | Link-line construction, preserving the order `-L`/`-l`/operands were given in |
| `ppargs.rs` | `-D`/`-U`/`-I` ordering for the tools that need it |
| `tools.rs` | Shared exit-status handling for `cflow`/`ctags`/`cxref` |
| `os/` | OS-specific knobs (linux, macos, freebsd) |
| `abi/` | Per-ABI classification: `sysv_amd64.rs`, `aapcs64.rs` |
| `ir/mod.rs` | IR definitions (opcodes, pseudos, instructions, functions). See `ir/README.md`. |
| `ir/linearize.rs` (+ `_init.rs`, `_stmt.rs`, `_emit.rs`, `_atomic.rs`) | AST → IR conversion, SSA construction |
| `ir/mem2reg.rs` | Promotion of address-free locals to registers |
| `ir/tls.rs` | Thread-local access expansion (dynamic model) |
| `ir/validate.rs` | IR invariant checks |
| `ir/mach_o_dtors.rs` | Mach-O destructor registration |
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
- The C17 language, C99 baseline and all C11 additions alike
- C11 additions: `_Generic`, `_Atomic` / `<stdatomic.h>` (including access through ordinary operators), `<tgmath.h>`, `_Noreturn`, `_Static_assert`, `_Alignas` / `_Alignof`, `_Thread_local` (Local-Exec and Initial-Exec models), anonymous struct/union members, Unicode literals
- GCC-compatible inline assembly: extended asm with constraints, clobbers,
  named operands, matching constraints, `asm goto` with labels, SSE (`x`) and
  x87 (`t`/`u`) operand classes on x86-64, and vector (`w`) operands with the
  `b`/`h`/`s`/`d`/`q` width modifiers on AArch64
- GNU extensions real code depends on: case ranges (`case 1 ... 9:`),
  designated-initializer ranges (`[0 ... 3] = v`), computed goto (`&&label`
  and `goto *p`), statement expressions, `typeof`, `__attribute__` including
  `mode` and `vector_size`, `__builtin_*`, and case-range-style `...` spacing
  matching gcc's (`case 1...9:` is one pp-number and is rejected there too)
- Variably modified types everywhere C17 admits them, including a `typedef` of
  one (6.7.7), whose extents are evaluated at the typedef rather than at each use
- `-fverbose-asm`, annotating each instruction with the source names it came from
- Cross-compilation as far as `-S`: `--sysroot`, `-isystem` and `-idirafter`
  give `--target` the target's headers. `as` and `cc` are still the host's, so
  assembling and linking for another target is not supported

Not yet implemented:
- assembly peephole optimizations
- the GCC atomic builtins `__sync_*` and `__atomic_*`. C11 `<stdatomic.h>` is
  complete; these are the older spellings of machinery c17 already has, which
  is why they are on this list rather than the one below. c17 no longer
  predefines `__GCC_HAVE_SYNC_COMPARE_AND_SWAP_*`, so code guarded on it takes
  its portable branch instead of failing on an undeclared identifier

Will not implement:
- vector *arithmetic*. `vector_size` gives a type a vector's storage — the size
  and alignment GCC gives it, which is what glibc's `<link.h>` needs — and that
  is deliberately where it stops. Element-wise `+`, `*` and the rest need a
  vector type in the IR and in both backends, so they are diagnosed rather than
  silently computed on one element
- SIMD intrinsic headers (`immintrin.h` and friends). Code guarded on
  `#ifdef __SSE2__` reaches for one and fails. The macro is not the fault:
  SSE2 is architectural baseline for x86-64 and gcc defines it unconditionally,
  as c17 does — it describes the target, not the header set. See `doc/TODO.md`
- `__auto_type`; nested functions and `__label__`. Clang refuses nested
  functions too, and they need executable-stack trampolines
- `_Imaginary` types. Optional in C99, C11 and C17 alike -- never removed,
  as this line used to claim. Neither gcc nor clang implements them: gcc
  rejects `_Imaginary double x;` outright and leaves `_Imaginary_I` undefined,
  while still defining `__STDC_IEC_559_COMPLEX__`.

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
