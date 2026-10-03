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

Comparisons of two constants are the exception, as in gcc: one against a NaN
folds -- every ordered predicate to 0, `!=` to 1 -- although an ordered
comparison with a NaN raises `FE_INVALID`. At run time both backends emit
quiet compares, which do not raise it for a quiet NaN either, so the fold
agrees with what the unfolded code does.

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

## Torture tests skipped by decision

### Out of scope, and so skipped rather than counted

Anything GNU-specific or newer than C17 is **out of scope**: the harness
(`scripts/c17_torture.sh`) skips it with a named reason instead of reporting a
failure, because counting it measures a decision rather than a defect.

The names live in the harness's lists, one shell variable per category, and
are not repeated here. Each entry there is `<sub-suite>/<name>`, because a
test name is not unique across sub-suites: `20021204-1`, `20031011-1` and
`20050119-1` each name a nested-function test in `compile/` **and** a
different test in `execute/` that passes.

| Category | Harness list | Why |
|---|---|---|
| Nested functions | `OUT_OF_SCOPE_NESTED_FN` | Needs a static chain and executable trampolines |
| VLA as a struct member | `OUT_OF_SCOPE_VLA_MEMBER` | Needs struct layout computed at run time, and `offsetof` through it |
| Post-C17 | `OUT_OF_SCOPE_POST_C17` | `_Decimal64` (TR 24732), C23 `[[...]]` attributes, C23 `enum E : bool`, C2y `uabs` |
| GNU-only attribute | `OUT_OF_SCOPE_GNU_ATTR` | `scalar_storage_order`; needs reverse-endian load/store lowering |
| gcc's own front ends | `OUT_OF_SCOPE_GCC_INTERNAL` | `-fgimple`, which parses gcc's internal representation rather than C; gcc rejects them without the flag too |
| Builtins gcc synthesizes for itself | `OUT_OF_SCOPE_GCC_INTERNAL_BUILTIN` | `__builtin_setjmp`, `__builtin_apply`, `__builtin_stack_save` and the like, which no header declares; recorded in `BUILTIN.md`'s "Not implemented" table |
| `-fgnu89-inline` semantics | `OUT_OF_SCOPE_GNU89_INLINE` | `compile/20021120-1`, `-2` redefine an `extern inline` function under `-fgnu89-inline`. c17 honours the flag and compiles both; the skip rests on c17 not rejecting the same redefinition without the flag, as gcc does, which the tests themselves do not exercise |
| Another target's backend | `OUT_OF_SCOPE_OTHER_TARGET` | `mipscop-1`..`-4` |
| `__builtin_issignaling` | `OUT_OF_SCOPE_ISSIGNALING` | No system header uses the builtin (`<math.h>`'s `issignaling` is its own macro), and seven of the nine tests need a format c17 does not have (`_Float128`, `_Float64x`, `bfloat16`) |
| Pre-C99 implicit `int` with no dialect request | `NEEDS_PRE_C99_DIALECT` | `compile/pr29201`. C17 6.7.2p2 requires a type specifier and GCC 14 made it an error too. A test that asks for `-fpermissive` passes; one that asks for `-std=gnu89` passes because the harness translates that to `-fpermissive` -- c17 itself ignores `-std=gnu89` |
| Vector values | `OUT_OF_SCOPE_VECTOR_ARITH` | A vector of floating lanes four bytes wide or less at a call boundary, which gcc passes like no type c17 has. Every other vector operation runs |
| `__label__` | `OUT_OF_SCOPE_LOCAL_LABELS` | Block-scope label declarations, ruled out with nested functions |
| Label difference as a constant | `OUT_OF_SCOPE_LABEL_DIFF` | `&&a - &&b` in a static initializer. Labels as values are supported; the difference needs a symbol-difference relocation |
| A C17 constraint gcc only warns about | `C17_CONSTRAINT_GCC_WARNS` | `compile/pr38857`: 6.7.4p3, an external inline definition referring to a static. `-fpermissive` relaxes it |
| gcc-specific *behaviour* | `OUT_OF_SCOPE_GCC_BEHAVIOUR` | See below |

The gcc-specific behaviour list holds four kinds of test:

- `execute/20031003-1`: `(int)2147483648.0f` is undefined behaviour (C17
  6.3.1.4p1); gcc's folder saturates it to `INT_MAX`, and aarch64 agrees by
  hardware accident.
- A conditional with one `void` arm, which C17 6.5.15p3 forbids and gcc
  accepts without comment: `execute/pr46309`, `compile/pr26725`,
  `compile/20000211-1`. c17 rejects it, and `-fpermissive` does not relax it.
- `compile/950919-1`: a GNU preprocessor assertion (`#cpu(m68k)`), which gcc
  itself calls deprecated.
- An empty write kept as a call: `builtins/printf`, `builtins/fprintf`,
  `builtins/fputs`, `execute/printf-chk-1`, `execute/fprintf-chk-1`,
  `execute/vprintf-chk-1`, `execute/vfprintf-chk-1` abort when `printf("")`,
  `fprintf(fp, "")` or `fputs("", fp)` reaches the library at `-O1` and up.
  C17 7.21.2p4 gives a stream its orientation from the first input or output
  function applied to it, whether or not a byte moves, so c17 keeps every
  empty write; gcc drops them and loses the orientation (`fwide(stdout, 0)`
  after `printf("")` is negative under c17 and 0 under gcc).

These are listed **by name** in the harness, never matched against the source.
A content match is wrong: `pr86659-1`, `pr86659-2` and `pr87623` all mention
`scalar_storage_order` and **pass**, so a scan for the feature would throw
away cases c17 gets right. A name list also keeps every skip auditable, and a
test added to the suite later shows up as a new failure and gets triaged then
-- which is the right moment to decide.

Nothing is matched by content: every test is attempted unless a list names it,
and a test's `.x` file is read for the few shapes the suite uses rather than
taken as "skip". Matching the source, the `dg-require-effective-target` names,
or the mere presence of a `.x` file would hide tests c17 passes and bugs it
has.

Skipping a test that is in the passing baseline is reported as a regression,
by name. A skip of a test that was never in the baseline is not caught that
way.

### Deliberate divergences from gcc

The gcc-specific behaviour above is skipped. Two more divergences c17 keeps
but does **not** skip, because they are not GNU-specific:

| Test | Why c17 does not follow |
|---|---|
| (no torture test) | `{ .t = v, .t.b = 9 }`, where `.t` is a struct or union initialized from a whole value: c17 keeps `v`'s other bytes and replaces only `t.b`; gcc discards all of `v`, so `t.a` reads 0. C17 6.7.9p19 overrides "the same subobject", which `t.b` is and `t` is not. Which member a union value holds is not known until run time, so a union is treated like a struct here: its bytes are the value's |
| `920728-1` | `return;` in a function returning non-void. C17 6.8.6.4p1 makes it a constraint violation; GCC 13 warns, GCC 14 errors by default. The test asks for `-std=gnu89`, which the harness translates to `-fpermissive`, and `-fpermissive` downgrades the error to a warning, so the test runs and passes |

Complex integer division is a third, recorded in `BUILTIN.md`: c17 uses
Smith's method because the exact formula overflows, and so answers `6 + 1i`
for `(-9 + 38i) / (5 + 6i)` exactly as gcc does.
