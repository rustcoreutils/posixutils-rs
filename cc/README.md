# Guide to working on c17, our C17 compiler

## Overview

c17 implements **C17 (ISO/IEC 9899:2018) only**, plus selected GNU extensions, targeting POSIX.2024 compliance. C17 is a defect-report revision of C11 and adds no features over it. There is one language mode: no `-std=` switching between revisions, and no strict-versus-GNU axis. `-std=` is still accepted, because build systems pass it unconditionally, but a request for an older revision is reported rather than honoured (silence it with `-Wno-c17-dialect`). It supports x86-64 and AArch64 (ARM64) on Linux and macOS.

References:
- [ISO/IEC 9899:2011 (C11)](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf) — C17 is this plus defect reports; the published C17 text is not free

## Documents

| Document | Description |
|----------|-------------|
| [ATTR.md](ATTR.md) | Function attributes (`__attribute__`, `_Noreturn`, `__has_attribute`) |
| [BUILTIN.md](BUILTIN.md) | Compiler builtins (`__builtin_*`), and what is deliberately absent |
| [TODO.md](TODO.md) | Outstanding work only -- technical debt, optimization passes, torture-suite status |
| [DECISIONS.md](DECISIONS.md) | Settled choices and deliberate divergences from gcc. Not a backlog |
| [ir/README.md](ir/README.md) | IR structure and the pass pipeline |

## Quick start

Build: `cargo build && cargo build --release`

Testing (compiler subset): `cargo test --release -p posixutils-cc`.  Run it
alone: CI uses `--test-threads=1`, and two full runs at once starve the heavy
tests into failures that are not real.

Debugging, via stdio:
```
echo 'int main() { return 42; }' | ./target/release/c17 - -S -o -
```

## Architecture

The compiler pipeline:

```
Source → Lexer → Preprocessor → Parser → Type Check → Linearize
       → Mapping (target lowering) → Optimize
       → Library calls (libm fallbacks, binary128 and x86-64 _Float16 soft-float)
       → Lower (φ → copies) → Codegen → Assembly
```

Key source files, in pipeline order -- where the central data structures,
algorithms and contracts live:

| File / Dir | Purpose |
|------------|---------|
| `main.rs` | Driver: the pipeline above, stage by stage, with the IR verified between stages |
| `token/preprocess.rs` | The preprocessor: macro expansion with hide sets, `#include`, conditionals |
| `parse/parser.rs`, `parse/ast.rs` | Recursive-descent parser and the AST it builds |
| `types.rs` | The C type system: interned `TypeId`s, layout, bit-field placement, qualifiers |
| `ir/mod.rs` | IR data structures -- opcodes, pseudos, instructions, blocks, functions. See [ir/README.md](ir/README.md) |
| `ir/linearize.rs` | AST to IR: control flow, lvalues, initializers, lifetime markers |
| `ir/ssa.rs`, `ir/dominate.rs` | SSA construction: phis at the iterated dominance frontier (Sreedhar-Gao), renaming |
| `ir/cfg.rs`, `ir/validate.rs` | Every CFG edit, critical-edge splitting, CFG simplification; the IR invariants every stage is checked against |
| `opt.rs` | The optimizer: the pass order, and why each pass sits where it does |
| `ir/dataflow.rs` | The sparse conditional solver (Wegman-Zadeck) behind `sccp` and `vrp` |
| `ir/memloc.rs`, `ir/loadfwd.rs` | Memory analysis: what an access addresses, what may alias, and what a location holds at a point |
| `abi/` | Calling-convention classification (System V AMD64, Microsoft x64, AAPCS64) |
| `arch/regalloc.rs` | Shared register allocation: liveness, local lifetimes and slot sharing, spill slots |
| `arch/x86_64/`, `arch/aarch64/` | Code generators: instruction selection, frames, calls, inline asm |

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
  and `goto *p`, with `&&a - &&b` as a constant for jump tables of offsets),
  the omitted middle operand (`a ?: b`, with `a` evaluated
  once), statement expressions, `typeof`, `__attribute__` including `mode` and
  `vector_size`, `__builtin_*` (see [BUILTIN.md](BUILTIN.md)), and case-range-style
  `...` spacing matching gcc's (`case 1...9:` is one pp-number and is rejected
  there too)
- Variably modified types everywhere C17 admits them, including a `typedef` of
  one (6.7.7), whose extents are evaluated at the typedef rather than at each use
- `-fverbose-asm`, annotating each instruction with the source names it came from
- `-fdebug-prefix-map=OLD=NEW`, `-fmacro-prefix-map=OLD=NEW` and
  `-ffile-prefix-map=OLD=NEW`, with gcc's rules: the last matching option
  wins, `OLD` is a plain string prefix, and the argument splits at its last `=`
- `-fpermissive`, downgrading to warnings the handful of C17 constraints GCC
  13 warns about and GCC 14 refuses: pre-C99 implicit `int`, an implicit
  function declaration, and a `return` whose value-ness does not match the
  function's type.  A named list, not a dialect: everything else C17 requires
  is still checked
- gcc's diagnostic severities: what gcc refuses is an error, what it warns
  about by default is a warning, and what it reports only under `-pedantic`
  is silent by default. `-pedantic-errors` makes errors of both kinds of
  warning, as in gcc
- Cross-compilation as far as `-S`: `--sysroot`, `-isystem` and `-idirafter`
  give `--target` the target's headers. `as` and `cc` are still the host's, so
  assembling and linking for another target is not supported

Not yet implemented:
- assembly peephole optimizations
- the AVX families of intrinsics (`avxintrin.h` and later): `<immintrin.h>`
  stops at SSE4.2, and `-march=x86-64-v3` claims no more than v2. The SSE
  through SSE4.2 headers and a core `<arm_neon.h>` are bundled, written in C
  over `vector_size` values

Will not implement:
- `__auto_type`; nested functions and `__label__`. Clang refuses nested
  functions too, and they need executable-stack trampolines
- `_Imaginary` types, beyond what C17 requires of an implementation without
  them. They belong to Annex G, which binds only an implementation defining
  `__STDC_IEC_559_COMPLEX__`; c17 does not define it, so what remains is that
  `_Imaginary` is a keyword, that using it is diagnosed (it is no permitted
  type specifier, C17 6.7.2p2), and that `imaginary` and `_Imaginary_I` are
  left undefined (POSIX `<complex.h>`). Few compilers support the types --
  gcc rejects them, though glibc's `<stdc-predef.h>` defines
  `__STDC_IEC_559_COMPLEX__` for it -- and real code that needs them would
  reopen this. The GNU imaginary constant suffix (`2i`) is supported; it gives
  a `_Complex` value with a zero real part.

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

## Testing Requirements

1. Every fix and feature comes with tests that guard against regressions.
2. Changes to `cc/ir/`, `cc/token/` and `cc/parse/` include unit tests.
3. Every change also gets an end-to-end integration test in `cc/tests/`.

## Code Quality

Please run `cargo fmt` before committing code, and `cargo clippy` regularly while working. Code should build without warnings.

```bash
cargo fmt && cargo clippy -p posixutils-cc --all-targets
```

DO NOT `allow(dead_code)` to fix warnings. Instead, remove dead code; do
not leave it around as a maintenance burden (and LLM token
tax).

Read CONTRIBUTING.md in the root of the repository for more details.
