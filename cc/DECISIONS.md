# c17 decisions

Choices c17 has made, and the places where it deliberately differs from gcc.
**None of this is outstanding work** -- that lives in [TODO.md](TODO.md),
which holds only what is still to be done.

Each entry carries its reason, so a future reader can weigh the reason rather
than rediscover the choice.

## Table of Contents

- [Settled -- do not re-open](#settled--do-not-re-open)
- [Known Divergences](#known-divergences)
- [GNU extensions: what c17 will and will not grow](#gnu-extensions-what-c17-will-and-will-not-grow)
- [Torture tests skipped by decision](#torture-tests-skipped-by-decision)

## Settled — do not re-open

### `_FORTIFY_SOURCE` compiles but checks nothing

**Deferred indefinitely by maintainer decision** -- a decision, not a backlog
item, and this is its only record. `_FORTIFY_SOURCE`,
`__builtin_object_size` and the `_chk` family appear nowhere in POSIX.1-2024
or C17, so this is not a conformance gap.

What the build needs already works: c17 accepts `-D_FORTIFY_SOURCE=2` (and
`=3`, whose `__builtin_dynamic_object_size` falls back to
`__builtin_object_size`), compiles glibc's fortified headers, and links.
Distro builds pass the flag by default -- Debian's `dpkg-buildflags` does --
so this part is load-bearing. With `-O`, `__OPTIMIZE__` is predefined, glibc
compiles its wrappers, and c17 emits the `__*_chk` calls.

What it does not do is *check*. `__builtin_object_size` folds at parse time
from what the expression shows (an array, a member, `&lvalue`, constant
pointer arithmetic). Inside a glibc wrapper its argument is the wrapper's own
parameter, which at parse time is unknown, so it folds to `(size_t)-1` -- the
encoding for "do not check". The program pays for the wrappers and checks
nothing, and anyone who sets the flag expecting hardening gets no diagnostic
saying so.

Doing it properly means folding `__builtin_object_size` after inlining. There
is no IR representation for an unresolved builtin query -- no opcode, no
expression node that survives linearization, and no post-inline
pointer-provenance analysis to build one on. `instcombine` refuses to touch
`Call` and every memory-touching opcode, and its `Simplification` enum can
only copy or fold to a constant. That is the cost the deferral weighs.

### Trigraphs are off by default — decided, not deferred

**Settled. Not a to-do, not an open conformance item, not awaiting a
decision.** It is recorded here only so it stops being rediscovered.

POSIX APPLICATION USAGE 88224 says outright that a compiler doing this is "not
conforming to POSIX.1-2024", which is why it keeps reading like an open item to
anyone coming to the spec fresh. `--trigraphs` implements translation phase 1
exactly. The default is off because replacement reaches inside string
literals — `"What??!"` becomes `"What|"` — and `??` is far likelier to appear
by accident than by intent; gcc and clang default them off for the same reason.
`git log --grep '#C55'` has the record.

### Unreachable code is not emitted, at any level

**Settled.**

At every level, `-O0` included, c17 emits nothing for code that no path
reaches. That covers the arm of an `if`, `?:`, `while`, `for`, `do` or
`switch` whose controlling expression is a constant expression, the right
operand of a `&&` or `||` whose left operand decides it, and whatever follows
a `return`, `break`, `continue` or `goto` up to the next label. A block that a
label, `case` or `default` reaches is kept, and so is one whose address is
taken. In `if (0) { f(); case 1: g(); }` only `f()` goes. At `-O0`,
`__builtin_constant_p` of anything not already a constant is 0 straight away,
so a branch on it folds too.

gcc does the same. Its front end folds the condition and its CFG cleanup,
which runs at every level, deletes what the fold cut off. Programs rely on
this: gcc.c-torture's `link_error` tests (`medce-1`, `20030330-1`) call a
function that exists nowhere from such an arm. What is lost is a breakpoint
on a dead line, which gcc loses as well.

The condition has to be a constant expression, asked through the shared walk
(`constexpr::eval_truth`). A `const` object does not qualify, so
`const int k = 0; if (k)` keeps its arm, as in gcc. Unlike gcc, c17 does not
fold `&x == 0`, `(g(), 0)` or `g() && 0` at `-O0`: none of them is a constant
expression.

One more condition decides its branch, and only under `-fno-trapping-math`:
a floating comparison of an unknown value with a constant, when no value,
NaN included, can change the answer. `x > +Inf` is the example (`fp-cmp-7`).
gcc folds exactly those forms at `-O0`. With trapping math on, the default,
the comparison stays, because a NaN operand would raise `FE_INVALID`. The
optimizer decides which comparisons qualify by the same rule
(`constfold::fcmp_against_constant`).

### Float constant folding stops where an operation raises

Arithmetic, negation, comparison and conversion fold over float constants, at
the format the program computes in rather than at the 128 significand bits a
literal is carried in. Arithmetic and conversion leave to run time every
operation that would raise invalid, divide-by-zero or overflow: a NaN or
infinite operand, a division by zero, a result or narrowing that overflows
(`ir/constfold.rs`). C lets a program read those flags through `<fenv.h>`,
and folding the operation would take the flag with it. An inexact or
underflowing result is folded, as gcc folds it without `-frounding-math`.

Comparisons are the exception, as in gcc: from `-O1` one whose answer no
operand value can change folds -- against a NaN every ordered predicate to 0
and `!=` to 1, `x > +Inf` to 0, `(x < y) && (x > y)` to 0 -- although a
relational operator with a NaN operand raises `FE_INVALID` at run time
(C17 F.9.3: they are the signaling compares, `comis*`/`fcomip`/`fcmpe`), and
the fold takes the flag with it. gcc.c-torture's `ieee/fp-cmp-6` and friends
require the fold. What the optimizer never does is the other half: run a
relational on a path the program would not have, or turn one into a quiet
compare where its operands may be unordered.

Nor does anything else that can raise run on a path the program would not
have: `c ? a * b : 0` keeps its branch rather than becoming a select, as do
an arm's narrowing, float-to-integer or inexact integer-to-float conversion
and its `sqrt` (`ir::FpRaise`, asked through
`Instruction::may_raise_fp_exception` by `ifconv` and by the linearizer's
`is_pure_expr`). Folding a constant drops an inexact flag; speculating an
operation would *add* flags nothing caused, which no flag setting allows. The
exact operations -- negation, `fabs`, `copysign`, widening, quiet equality --
still become selects. `-fno-trapping-math` lets the linearizer speculate the
rest, as gcc does.

## Known Divergences

Behaviours where c17 deliberately differs from gcc on the same source.

### `_Generic` on a wide bit-field expression

`_Generic((x.b + 0), unsigned long long: ...)` with `unsigned long long b : 40`
matches `unsigned long long` here and matches **nothing** under gcc, which
treats the 40-bit width as part of the type for selection purposes. C17 leaves
the type of a bit-field wider than `int` to the implementation, so neither is
wrong.

Not worth closing at the price it asks. The width rides beside the type rather
than in it, precisely so `sizeof` stays 8 and the ABI, DWARF and both backends
keep seeing `unsigned long long` — putting it in the `TypeId` would make
`types_compatible`, `common_type` and `emit_convert` all disagree with
themselves. No torture test depends on it.

| Area | Divergence |
|---|---|
| `mode` with a vector mode | `mode(V4SI)` and the other vector modes warn that they are not implemented and leave the declared type unchanged, so `sizeof` is the element's (4) where gcc's is the vector's (16). Scalar modes (`QI`..`TI`, `SF`, `DF`, ...) bind as in gcc, on a declarator, a struct member or a parameter. gcc itself deprecates vector modes in favour of `vector_size` |
| `return` with the wrong value-ness | `return expr;` in a `void` function, and a bare `return;` in a non-`void` one, are errors, as they are by default from GCC 14 (`-Wreturn-mismatch`); GCC 13 and earlier warn. Both are C17 6.8.6.4p1 constraint violations. `-fpermissive` downgrades them, as it does implicit `int` |
| An octal or hex escape out of range | `'\400'`, `"\x123"`, `u"\x12345"`: an escape its element type cannot represent is an error here and a warning in gcc. C17 6.4.4.4p9 makes it a constraint violation. `-fpermissive` downgrades it, and the literal then keeps the low bits, as gcc's does |
| `_FORTIFY_SOURCE` | Compiles the wrappers and emits `__*_chk` calls, but checks nothing; see the settled entry above |
| Identifier characters U+FD3E, U+FD3F | Rejected here; GCC's binary accepts them. Ornate parentheses, which C17 Annex D excludes between its F900-FD3D and FD40-FDCF ranges -- GCC's own `ucnid.tab` does not list them and Clang's table does not either, so the table is followed rather than the binary |
| Darwin: an over-aligned variadic aggregate | clang disagrees with itself, so no compiler satisfies this in both directions. Measured on macOS CI: its caller stacks the aggregate at the next eight-byte granule and its `va_arg` rounds the cursor up to the type's own alignment, reading somewhere else. A program built entirely with clang has the same defect. c17 follows `va_arg` -- its caller realigns the outgoing area so the argument really is that aligned -- which means a c17 caller reaches a clang callee and a clang caller does not reach a c17 callee. `codegen_over_aligned_argument_area` therefore does not put this shape through its host-compiler cross-check on Apple; the pure-c17 runs still cover it at every optimization level |
| Darwin: a vector that is not one of the machine's widths | gcc and clang disagree about these on System V, and c17 follows gcc. A vector wider than sixteen bytes has no register class unless AVX is on, so gcc gives it memory and a hidden return pointer while clang legalizes it into a pair of SSE registers -- on every target it compiles for, Linux included. A one-lane vector (`double`, `long long` or `float` under `vector_size`, and a register-sized struct holding one) is memory to gcc and the bare scalar in its own register to clang. So a clang caller of a c17 callee returning `int __attribute__((vector_size(32)))` reads the wrong place, and `va_arg` disagrees likewise. Neither compiler is wrong -- the psABI classifies no such type -- so `vector_abi_interop_host` leaves these shapes out of its host-compiler cross-check on Apple and runs them with c17 on both sides, which `GCC_VECTORS` in its sources keys off `__APPLE__` and `C17_ALONE` to do |
| Darwin: `ms_abi` with `long double`, `_Float16` or an eight-byte vector | clang's Win64 lowering of these differs from gcc's: it returns `long double` in ST0 where gcc writes it through the hidden return pointer, it disagrees on `_Float16`, and it passes and returns an eight-byte vector by reference where gcc uses a general register. c17 follows gcc throughout, so on Apple -- the only host where the other half of an interop pair is clang -- `codegen_ms_abi_types_interoperate_with_gcc` and `vector_abi_interop_ms_abi` leave these shapes out of the cross-check there (`WIDE_SCALARS`, `EIGHT_BYTE_VECTOR`). What c17 compiles alone still carries them: `codegen_ms_abi_inlines_across_conventions` at every optimization level for the two scalars, and a two-unit run for the vector. `__float128` is a separate matter: Apple's targets have none at all, and c17 refuses it there exactly as clang does |
| Feature-test macros are predefined | On Linux c17 predefines `_GNU_SOURCE`, `_DEFAULT_SOURCE`, `_XOPEN_SOURCE` (800) and `_XOPEN_SOURCE_EXTENDED`, plus `_REENTRANT` (and `_DARWIN_C_SOURCE` on macOS); gcc predefines none of them. The whole GNU and XSI namespace is therefore visible to every program: a program's own `getline`, `index`, `random` or `qsort_r` can conflict with the header's, and glibc 2.38 and later binds `strtol`, `strtoll` and the `scanf` family to their C2X forms, which also accept a `0b` prefix. Kept because a great deal of code assumes the GNU declarations are visible without asking. `-U_GNU_SOURCE` (and the others) withdraws them |
| `max_align_t` | `long double` here (`<stddef.h>`: size 16, alignment 16); gcc's is a struct of `long long` and `long double` (size 32, alignment 16), on x86-64 and aarch64 alike. The alignment, which is what C17 7.19p2 specifies, agrees; `sizeof`, and the layout of any struct holding one, differ |

## GNU extensions: what c17 will and will not grow

This is a **decision table, not a backlog**. Frequency in real source is the
tie-breaker, never the justification — "the kernel uses it 116 times" argues
that the kernel is GCC-specific, not that c17 should be. Each row had to clear
the project's own filter before earning a verdict:

1. Is there a POSIX or C17 basis? (None of these have one — GNU extensions
   start at a disadvantage, they do not start neutral.)
2. Does refusing it actually block a real build, or does the guarded code
   already have a portable path c17 can take?
3. Is it an *alternate spelling* of machinery c17 already has, or genuinely
   new subsystems?
4. Is it a de-facto standard both GCC and Clang accept, or a GCC quirk?

| Extension | Verdict | Why |
|---|---|---|
| SIMD intrinsic headers | **SSE through SSE4.2, and core NEON** | Bundled and written in C over GNU vectors; see "SIMD headers" below. The AVX families are not |
| `__auto_type` | **No** | Not in glibc's headers or CPython; `__typeof__`, which c17 has, does the same job in the macros that use it |
| nested functions / `__label__` | **Never** | GCC-only, Clang refuses nested functions, so portable code already avoids them; they need executable-stack trampolines. See the c-torture section |
| VLA as a struct member | **No** | GCC-only (Clang refuses it); needs struct layout computed at run time and `offsetof` through it |
| `_Decimal32/64/128` | **No** | TR 24732, folded into C23, so newer than C17; IEEE 754 decimal arithmetic is a whole numeric tower, wanted here by a single torture test |
| C23 `[[...]]` attributes | **No** | Newer than C17, which is the language c17 implements. `__has_c_attribute` is left undefined and `__STDC_VERSION__` is `201710L`, so code that probes before using them takes its older path |

**The standing rule**: anything GNU-specific, or newer than C17, is out of
scope unless a real corpus forces the question. The c-torture harness skips
such tests with a named reason rather than counting them as failures -- what
is left failing is then a list of defects, not a list of decisions.

### SIMD headers

On x86-64 c17 predefines `__SSE__`, `__SSE2__`, `__MMX__`, `__SSE_MATH__` and
`__SSE2_MATH__`, matching GCC's x86-64 default; on aarch64 it predefines
`__ARM_NEON`. The intrinsic headers those macros lead a project to are
bundled: `<mmintrin.h>` through `<nmmintrin.h>` (SSE4.2), `<immintrin.h>`,
`<x86intrin.h>` and `<mm_malloc.h>` on x86-64, and a core `<arm_neon.h>` on
aarch64. They are written in C over GNU vectors and checked against gcc on
the hardware, so every function is available whatever the `-m` flags; only
the feature macros (`__SSE4_1__` and the rest) follow `-msse3` .. `-msse4.2`,
`-mpopcnt` and `-march=x86-64-v2`. The AVX families are not bundled, and no
flag claims them.

### Which macros may be withdrawn, and which may not

The distinction is what the macro is a statement *about*:

- **Compiler capability** — `__GCC_HAVE_SYNC_COMPARE_AND_SWAP_N` means "I
  provide the `__sync_*` builtins". c17 implements the family at 1, 2, 4 and
  8 bytes and defines the macro for exactly those sizes; a macro of this kind
  is defined if and only if c17 provides what it names.
- **Target capability** — `__SSE2__` says the code may use SSE2, which is in
  the x86-64 baseline (the psABI passes `double` in XMM registers), and gcc
  defines it by default for x86-64. `__ARM_NEON` is the same: Advanced SIMD is
  mandatory in the AArch64 base architecture. c17 defines both, and the code
  follows this section.

Code that writes `#ifdef __SSE2__` around `#include <emmintrin.h>` treats a
target fact as implying a compiler fact. That inference holds for gcc and
clang because they ship the intrinsic headers, and for c17 because it ships
them too (see "SIMD headers").

`__ARM_NEON__` is not defined on aarch64: it is the AArch32 spelling, and gcc
does not define it there.

## Torture tests

The harness (`scripts/c17_torture.sh`) attempts every test; nothing is
skipped by name. Only the suite's own directives -- a `.x` file, `dg-skip-if`,
a `dg-do` target selector -- keep a test from running.

### Deliberate divergences from gcc

Two divergences c17 keeps, which the tests below see as failures:

| Test | Why c17 does not follow |
|---|---|
| (no torture test) | `{ .t = v, .t.b = 9 }`, where `.t` is a struct or union initialized from a whole value: c17 keeps `v`'s other bytes and replaces only `t.b`; gcc discards all of `v`, so `t.a` reads 0. C17 6.7.9p19 overrides "the same subobject", which `t.b` is and `t` is not. Which member a union value holds is not known until run time, so a union is treated like a struct here: its bytes are the value's |
| `920728-1` | `return;` in a function returning non-void. C17 6.8.6.4p1 makes it a constraint violation; GCC 13 warns, GCC 14 errors by default. The test asks for `-std=gnu89`, which the harness translates to `-fpermissive`, and `-fpermissive` downgrades the error to a warning, so the test runs and passes |

Complex integer division is a third, recorded in `BUILTIN.md`: c17 uses
Smith's method because the exact formula overflows, and so answers `6 + 1i`
for `(-9 + 38i) / (5 + 6i)` exactly as gcc does.
