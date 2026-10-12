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

### Feature-test macros are not predefined

**Settled (user decision, 2026-10-10): c17 matches gcc.** It predefines none
of `_GNU_SOURCE`, `_DEFAULT_SOURCE`, `_XOPEN_SOURCE`,
`_XOPEN_SOURCE_EXTENDED`, `_POSIX_C_SOURCE`, `_POSIX_SOURCE`,
`_DARWIN_C_SOURCE` or `_REENTRANT`, on any target. They are the program's to
define (POSIX.1-2024 2.2.1). A program that names none gets the C library's
default namespace: glibc's `<features.h>` turns on `_DEFAULT_SOURCE` (POSIX
2008 plus BSD and SVID), and GNU or XSI interfaces need the program's own
`#define _GNU_SOURCE` or `_XOPEN_SOURCE`, exactly as under gcc. `-pthread`
defines `_REENTRANT`, as gcc's does.

### `-E` passes plain assembler and preprocessed input over

**Settled (user decision, 2026-10-12): c17 matches gcc.** Under `-E`, `-M`
and `-MM`, an input that is not preprocessed -- plain assembler (`.s`,
`-x assembler`) or C already preprocessed (`.i`, `-x cpp-output`) -- is
passed over: nothing is written, `-o` creates no file, and the operand is not
opened, as gcc 13 does. `.S` is preprocessed as before, and `-x c` makes any
file C source to preprocess.

### Float constant folding stops where an operation raises

Arithmetic, negation, comparison and conversion fold over float constants, at
the format the program computes in rather than at the 128 significand bits a
literal is carried in. Arithmetic and float-to-float conversion leave to run
time every operation that would raise invalid, divide-by-zero or overflow: a
NaN or infinite arithmetic operand, a signalling NaN converted, a division by
zero, a result or narrowing that overflows (`ir/constfold.rs`). C lets a
program read those flags through `<fenv.h>`, and folding the operation would
take the flag with it. An inexact or underflowing result is folded, as gcc
folds it without `-frounding-math`, and so is a conversion of an infinity or
a quiet NaN to another float format, which raises nothing.

A conversion to an integer type is the other exception, chosen to match gcc.
A value outside the type's range -- an infinity, a NaN, a huge or a negative
value to an unsigned type -- is undefined (C17 6.3.1.4p1) and raises
`FE_INVALID` at run time, where the instruction's answer differs by target:
x86-64 gives the minimum, aarch64 saturates. gcc folds a constant one in
every context, at `-O0` as well, and saturates: the maximum above the range,
the minimum (0 for unsigned) below it, and 0 for any NaN, signalling or not,
with or without `-fsignaling-nans`. c17 folds the same constants to the same
answers, dropping the flag as gcc does, by one rule
(`FloatVal::to_integer_saturating`): static initializers and integer
constant expressions (`constexpr.rs`), a constant cast or implicitly
converted in code (`fold_float_to_integer` in the linearizer, at every
level), and the optimizer's `FCvtS`/`FCvtU` (`constfold::eval_fcvt`).
gcc.c-torture's `execute/20031003-1` requires it. Only an array size refuses
the saturated value, making the array a VLA as gcc does for
`int a[(int)1e300 > 0];`. A conversion of a value not known until run time
is the hardware's. One gap remains: a 128-bit conversion becomes a libgcc
call before the optimizer runs, so a constant that reaches one through a
variable is converted at run time, where gcc folds it from `-O1`.

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
| `_FORTIFY_SOURCE` | `__builtin_object_size` is answered after inlining (`ir/objsize.rs`) and the `_chk` calls are decided as gcc decides them (`ir/libcall_fold/fortify.rs`); level 3 checks against `__builtin_dynamic_object_size`, which gives a `malloc` or other `alloc_size` result its size, as gcc does. The differences: the IR does not record subobjects, so type 1 of a pointer whose object only propagation finds is the whole object's remaining size, which never fires where gcc's would not, and type 3 is 0; the parser answers both exactly where the expression names the member. A provable overflow is left to abort at run time without gcc's compile-time `-Wstringop-overflow` warning, which c17 does not have. And gcc's strlen pass re-expresses some checks that stay -- `__strncat_chk` with a bound no shorter than its source as `__strcat_chk`, `__strcat_chk` onto a string of known length as `__strcpy_chk` or `__memcpy_chk` at its end -- where c17 keeps the call as written; the check is made either way |
| Identifier characters U+FD3E, U+FD3F | Rejected here; GCC's binary accepts them. Ornate parentheses, which C17 Annex D excludes between its F900-FD3D and FD40-FDCF ranges -- GCC's own `ucnid.tab` does not list them and Clang's table does not either, so the table is followed rather than the binary |
| Darwin: an over-aligned variadic aggregate | clang disagrees with itself, so no compiler satisfies this in both directions. Measured on macOS CI: its caller stacks the aggregate at the next eight-byte granule and its `va_arg` rounds the cursor up to the type's own alignment, reading somewhere else. A program built entirely with clang has the same defect. c17 follows `va_arg` -- its caller realigns the outgoing area so the argument really is that aligned -- which means a c17 caller reaches a clang callee and a clang caller does not reach a c17 callee. `codegen_over_aligned_argument_area` therefore does not put this shape through its host-compiler cross-check on Apple; the pure-c17 runs still cover it at every optimization level |
| Darwin: a vector that is not one of the machine's widths | gcc and clang disagree about these on System V, and c17 follows gcc. A vector wider than sixteen bytes has no register class unless AVX is on, so gcc gives it memory and a hidden return pointer while clang legalizes it into a pair of SSE registers -- on every target it compiles for, Linux included. A one-lane vector (`double`, `long long` or `float` under `vector_size`, and a register-sized struct holding one) is memory to gcc and the bare scalar in its own register to clang. So a clang caller of a c17 callee returning `int __attribute__((vector_size(32)))` reads the wrong place, and `va_arg` disagrees likewise. Neither compiler is wrong -- the psABI classifies no such type -- so `vector_abi_interop_host` leaves these shapes out of its host-compiler cross-check on Apple and runs them with c17 on both sides, which `GCC_VECTORS` in its sources keys off `__APPLE__` and `C17_ALONE` to do |
| Darwin: `ms_abi` with `long double`, `_Float16` or an eight-byte vector | clang's Win64 lowering of these differs from gcc's: it returns `long double` in ST0 where gcc writes it through the hidden return pointer, it disagrees on `_Float16`, and it passes and returns an eight-byte vector by reference where gcc uses a general register. c17 follows gcc throughout, so on Apple -- the only host where the other half of an interop pair is clang -- `codegen_ms_abi_types_interoperate_with_gcc` and `vector_abi_interop_ms_abi` leave these shapes out of the cross-check there (`WIDE_SCALARS`, `EIGHT_BYTE_VECTOR`). What c17 compiles alone still carries them: `codegen_ms_abi_inlines_across_conventions` at every optimization level for the two scalars, and a two-unit run for the vector. `__float128` is a separate matter: Apple's targets have none at all, and c17 refuses it there exactly as clang does |
| `max_align_t` | `long double` here (`<stddef.h>`: size 16, alignment 16); gcc's is a struct of `long long` and `long double` (size 32, alignment 16), on x86-64 and aarch64 alike. The alignment, which is what C17 7.19p2 specifies, agrees; `sizeof`, and the layout of any struct holding one, differ |

### Stack probes

`-fstack-clash-protection` probes as gcc 13 does (`target::StackClash`), with
three deliberate differences.

- **`-fstack-check` is an alias of `-fstack-clash-protection`.** User decision.
  gcc's `-fstack-check` is an older, different mechanism: it probes a fixed
  distance below the stack pointer for Ada's overflow checks, and gcc turns it
  off whenever the clash protection is on -- which Debian's and Ubuntu's gcc
  have by default, so there `-fstack-check` already means the clash probes,
  with a warning. c17 maps `-fstack-check`, `=specific` and `=generic` onto the
  clash probes; `-fstack-check=no` and `-fno-stack-check` withdraw that request
  and only it, the two staying separate requests as in gcc.
- **x86-64 probes a large outgoing argument area.** c17 reserves each call's
  stacked arguments with one `subq`; once that area and what the prologue left
  unprobed could reach the guard, it is probed as an `alloca` is. gcc pushes
  its arguments, each push a probe, but reserves a large by-value aggregate's
  area unprobed, and the copy into it or the call's return address can land
  beyond the guard.
- **Darwin is probed too.** clang's driver drops the option for Apple targets,
  whose own defence is `___chkstk_darwin`. The probes are plain stores
  that are right on any stack with a guard page, so c17 emits the same ones
  for Mach-O as for ELF rather than take the option and do nothing. One
  difference in order: a Darwin aarch64 frame that needs probes stores a
  frame record at its top and points x29 at it before probing, then the
  usual record at the bottom -- sixteen more bytes -- because Darwin's
  `backtrace()`, crash reports and samplers walk the frame-pointer chain,
  and gcc's order (probe, then store the record) leaves the function out of
  that chain while its probes run. clang stores the record before calling
  `___chkstk_darwin` for the same reason.

### Vector-PCS definitions save c17's scratch V registers

A function of the AAPCS64 vector procedure call standard
(`aarch64_vector_pcs`) preserves all of q8-q23 for its caller. c17 saves
whole each of them its body may change: every one the allocator gives a
value, every one an asm statement clobbers, all sixteen in a function that
makes a call (as gcc saves them) -- and always v16-v18, the code generator's
fixed floating-point scratch registers, where gcc saves only what it
writes. Nearly every floating-point or vector operation passes through
them, and which ones a function uses is known only once its code is
emitted, after its frame is laid out; saving them costs a leaf function
48 bytes of frame and three stores and loads. Calls the function makes are
the base standard's, so what is live across one still takes a register
that standard preserves; the allocator prefers v24-v31 to v19-v23, which
would need saving. `.variant_pcs` marks a definition, as gcc's does; gcc
also marks a vector-PCS function a unit only calls, which c17 does not:
c17 calls it by the base standard, which a lazily bound PLT entry keeps.

### Frames past two gigabytes

Frame quantities are `i64` in both back ends, and a frame may span up to
`TypeTable::MAX_FRAME_BYTES`, `PTRDIFF_MAX` less the prologue's headroom --
gcc's bound too, give or take its 511 bytes. A displacement or immediate
past 32 bits goes through a register, one rule in each back end's
`legalize.rs`: aarch64's X15, which every function has, and on x86-64 a
*far scratch*.

- **The far scratch is a callee-saved register held back for the whole
  function**, `R15` unless the frame base, a global register variable or the
  function's inline asm has it (then `R14`, `R13`, `R12`, `RBX`). gcc instead
  picks a free register at each use. The register is decided before
  allocation from `arch::regalloc::frame_reach`, an upper bound on every
  displacement the frame can need, so allocation runs once, and a function
  under two gigabytes -- every frame there was before -- gives up nothing and
  is compiled as it was. One held back is never in use where the legalizer
  needs it, so the rewrite is two instructions with no save, and `movabsq`
  leaves the flags alone. R10 and R11 could not serve: emitters keep values
  in them from one instruction to the next. Saving a register around each
  far access with `push`/`pop` would move `%rsp`, which the outgoing argument
  stores and `%rsp`-based probes address from.
- **x86-64 passes a by-value argument past two gigabytes.** gcc refuses one
  ("sorry, unimplemented: passing too large argument on stack"); C sets no
  such limit, and the outgoing area is reserved and copied like any other.
- **An inline-asm memory operand in a frame slot past 32 bits** -- a local
  under `m`, or a value spilled there under `g` or `rm` -- is written as an
  offset from one address the x86-64 far scratch holds while the template
  runs, loaded after the statement's inputs: no other register is free
  there, the allocator gives the far scratch to no value, and it is chosen
  from registers the statement neither clobbers nor pins. One more than two
  gigabytes from the first such operand, in a frame past four, takes R10 or
  R11 holding its address instead. On aarch64 those constraints take a
  register, loaded through X15's legalizer, and a local's address is formed
  in a scratch register, as everywhere.

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
| Basic asm at file scope | **Yes** | xz-utils and libxcrypt version their shared libraries' symbols with `__asm__(".symver ...")` between definitions, as glibc does; without it liblzma lacks its `XZ_5.2` symbols and libcrypt exports none of the crypt API. The text is written verbatim, in source order among the unit's definitions, each of which names its own section after it. Only the basic form, as in gcc: no qualifiers, and operands are an error. The `symver` attribute is not provided, so `__has_attribute(symver)` is 0 and such code takes its asm path |
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

The harness (`scripts/c17_torture.sh`) attempts every test except those in
its `SKIP_BY_NAME` table, which holds what is out of scope -- nested
functions, VLA struct members, post-C17 features, gcc's own internals,
another target's assembly -- and the odd test gcc itself rejects, each with
its reason. The suite's own directives (a `.x` file, `dg-skip-if`, a `dg-do`
target selector) skip the rest. A failure that is a c17 gap or bug is fixed,
not listed.

### Deliberate divergences from gcc

Two divergences c17 keeps, which the tests below see as failures:

| Test | Why c17 does not follow |
|---|---|
| (no torture test) | `{ .t = v, .t.b = 9 }`, where `.t` is a struct or union initialized from a whole value: c17 keeps `v`'s other bytes and replaces only `t.b`; gcc discards all of `v`, so `t.a` reads 0. C17 6.7.9p19 overrides "the same subobject", which `t.b` is and `t` is not. Which member a union value holds is not known until run time, so a union is treated like a struct here: its bytes are the value's |
| `920728-1` | `return;` in a function returning non-void. C17 6.8.6.4p1 makes it a constraint violation; GCC 13 warns, GCC 14 errors by default. The test asks for `-std=gnu89`, which the harness translates to `-fpermissive`, and `-fpermissive` downgrades the error to a warning, so the test runs and passes |

Complex integer division is a third, recorded in `BUILTIN.md`: c17 uses
Smith's method because the exact formula overflows, and so answers `6 + 1i`
for `(-9 + 38i) / (5 + 6i)` exactly as gcc does.
