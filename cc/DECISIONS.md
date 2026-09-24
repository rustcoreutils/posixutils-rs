# c17 decisions

Choices c17 has made and does not intend to revisit, and the places where it
deliberately differs from gcc. **None of this is outstanding work** -- that
lives in [TODO.md](TODO.md), which holds only what is still to be done.

Recorded so a future reader finds the reasoning rather than rediscovering it,
and so none of it is raised again as a question.

## Table of Contents

- [Settled -- do not re-open](#settled--do-not-re-open)
- [Known Divergences](#known-divergences)
- [GNU extensions: what c17 will and will not grow](#gnu-extensions-what-c17-will-and-will-not-grow)
- [Torture tests skipped by decision](#torture-tests-skipped-by-decision)

## Settled — do not re-open

Recorded so a future reader knows the reasoning rather than rediscovering it,
and so neither is raised again as a question.

### `_FORTIFY_SOURCE` compiles but checks nothing

**Not a conformance item, and the sole record of this one.**
`_FORTIFY_SOURCE`, `__builtin_object_size` and the `_chk` family appear nowhere
in POSIX.1-2024, so this is not a POSIX conformance gap. **Deferred
indefinitely by maintainer decision** — a decision, not a backlog item.

What *is* required of the compiler already works, and should not be confused
with what is missing: c17 accepts `-D_FORTIFY_SOURCE=2`, compiles glibc's
fortified headers, and links. Distro builds and `configure` scripts pass the
flag by default, so this is load-bearing. (CPython's own `configure.ac` passes
`-U_FORTIFY_SOURCE` for libmpdec, because glibc's `memmove`/`bcopy` wrappers
are wrong there — so the acceptance gate does not lean on it either.)

What is missing is the checking, and the symptom is silence rather than
breakage. `-D_FORTIFY_SOURCE=2` now compiles glibc's fortified wrappers and
emits `__*_chk` calls — before `__OPTIMIZE__` was predefined it emitted none at
all, because glibc compiled no wrapper to begin with. What it still does not do
is *check*: `__builtin_object_size` folds to `-1` at parse time, and `-1` is
the encoding for "do not check". The program now pays for the wrappers and
checks nothing.

Anyone who sets the flag expecting hardening does not get it, and gets no
diagnostic saying so.

**The remaining job is folding `__builtin_object_size` after inlining**, and it
is a real one. There is no IR
representation for an unresolved builtin query — no opcode, no expression node
that survives linearization, and no post-inline pointer-provenance analysis to
build one on. `instcombine` refuses to touch `Call` and every memory-touching
opcode, and its `Simplification` enum can only copy or fold to a constant.

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

## Known Divergences

### `_Generic` on a wide bit-field expression

`_Generic((x.b + 0), unsigned long long: ...)` with `unsigned long long b : 40`
matches `unsigned long long` here and matches **nothing** under gcc, which
treats the 40-bit width as part of the type for selection purposes.

Not worth closing at the price it asks. The width rides beside the type rather
than in it, precisely so `sizeof` stays 8 and the ABI, DWARF and both backends
keep seeing `unsigned long long` — putting it in the `TypeId` would make
`types_compatible`, `common_type` and `emit_convert` all disagree with
themselves. No torture test depends on it.


Behaviours where c17 differs from gcc on the same source. None is a
translation-limit or a diagnostic gap; each silently changes what the program
does or claims.

| Area | Divergence |
|---|---|
| `__attribute__((used))` | Honoured only because nothing is pruned: an unreferenced static survives `-O2` whether or not it is marked. Real pruning would have to start reading the attribute |
| `mode` on a vector type | `vector_size` gives a type a vector's storage and `mode` binds to a declarator, including a struct member's and a parameter's. The vector modes themselves still warn that they are ignored |
| `return` with the wrong value-ness | `return expr;` in a `void` function, and a bare `return;` in a non-`void` one, are errors here and warnings in gcc. Both are genuine C17 6.8.6.4p1 constraint violations |
| `_FORTIFY_SOURCE` | Compiles the wrappers and emits `__*_chk` calls, but still checks nothing. What remains -- folding `__builtin_object_size` after inlining -- is described above, and is an ordinary compiler feature rather than fortify-specific work |
| Identifier characters U+FD3E, U+FD3F | Rejected here; GCC's binary accepts them. Ornate parentheses, which ISO C Annex D excludes between its F900-FD3D and FD40-FDCF ranges -- GCC's own `ucnid.tab` does not list them and Clang's table does not either, so the table is followed rather than the binary. See #C158 |
| Non-NFC identifiers | GCC warns `-Wnormalized=` when an identifier is not in Normalization Form C; c17 is silent. A diagnostic-quality gap, not a conformance one -- both compile the same program |
| `#__VA_ARGS__` spacing | `V(a , b)` stringifies as `"a, b"`; gcc gives `"a , b"`. The separating comma's own spacing is discarded by the argument splitter. Pinned by `preprocessor_va_args_loses_space_before_a_separator` |
| Darwin: an over-aligned variadic aggregate | clang disagrees with itself, so no compiler satisfies this in both directions. Measured on macOS CI: its caller stacks the aggregate at the next eight-byte granule and its `va_arg` rounds the cursor up to the type's own alignment, reading somewhere else. A program built entirely with clang has the same defect. c17 follows `va_arg` -- its caller realigns the outgoing area so the argument really is that aligned -- which means a c17 caller reaches a clang callee and a clang caller does not reach a c17 callee. `codegen_over_aligned_argument_area` therefore does not put this shape through its host-compiler cross-check on Apple; the pure-c17 runs still cover it at every optimization level |
| A constant branch at `-O0` | c17 runs no optimizer at `-O0`, so `if (0) { ... }` keeps its arm; gcc folds it in a CFG cleanup it runs at every level. Deliberate: `-O0` output stays a faithful transcription of the source, so a breakpoint in a dead arm still has somewhere to land. The visible cost is that `20030330-1` and `medce-1` link at `-O1` and above and not at `-O0` |
| `max_align_t` | `long double` here (16 bytes), a struct of `long long` + `long double` under gcc (32). Both meet the alignment requirement; `sizeof` differs. Implementation-defined (C17 7.19) |

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
| SIMD intrinsic headers | **No — fix the predefines** | See below; the blocking is self-inflicted |
| `__auto_type` | **No** | 6 files across four trees; fails minimalism on its own numbers |
| nested functions / `__label__` | **Never** | GCC-only, Clang refuses it, needs executable-stack trampolines; see the c-torture section |
| VLA as a struct member | **No** | GCC-only; needs struct layout computed at run time and `offsetof` through it |
| `_Decimal32/64/128` | **No** | IEEE 754 decimal arithmetic, a whole numeric tower for a single torture test |
| C23 `[[...]]` attributes | **No** | Newer than C17, which is the language c17 implements |

**The standing rule**: anything GNU-specific, or newer than C17, is out of
scope unless a real corpus forces the question. The c-torture harness skips
such tests with a named reason rather than counting them as failures -- what
is left failing is then a list of defects, not a list of decisions.

### SIMD headers — the blocking is ours

c17 predefines `__SSE__`, `__SSE2__` and `__MMX__`, matching GCC's x86-64
baseline. But GCC defines those *and* ships `<emmintrin.h>`; c17 defines them
and does not. So a project's `#ifdef __SSE2__` guard opens the door to a header
that isn't there, when the same file's `#else` branch would have compiled:

```c
#ifdef __SSE2__
#include <emmintrin.h>      /* c17: 'emmintrin.h': file not found */
#else
... portable fallback ...   /* builds clean; -U__SSE2__ proves it */
#endif
```

The choice is to bundle the intrinsic headers — thousands of functions, plus
element-wise vector arithmetic in the IR and both backends — or to stop
claiming the capability. The second is a few lines in the predefines and costs
those projects only the speed of their own fallback path. Advertising what we
cannot deliver is the actual defect here; adding SIMD to make the advertisement
true would be the tail wagging the dog.

Vector *arithmetic* is the related non-goal. `vector_size` gives a type a
vector's storage, which is what makes glibc's `<link.h>` compile, and that is
deliberately where it stops.

### TLS descriptors rather than the older `@tlsgd` sequence

The dynamic model uses **TLS descriptors** rather than the older
`@tlsgd` + `__tls_get_addr` sequence. That is already gcc's default on
AArch64; on x86-64 it is gcc's `-mtls-dialect=gnu2`. Two measured reasons:

- The `@tlsgd` sequence is a byte-exact 16-byte blob — `data16` prefix,
  `.value 0x6666`, `rex64` — that the linker pattern-matches in order to relax
  it to a static model. Emitting it without the padding is a hard link error
  (`TLS transition from R_X86_64_TLSGD to R_X86_64_GOTTPOFF failed`), and c17's
  LIR emits structured instructions rather than byte blobs.
- A descriptor resolver preserves every register but the one it returns
  through. `__tls_get_addr` is an ordinary call and clobbers all caller-saved
  registers.

That second point is why the register allocator is not a blocker here. The
sequence is **not** call-like: it
declares a single clobber through `opcode_constraints`, the same mechanism that
already handles `DivS` clobbering `RAX`/`RDX`. Adding it to `is_call_like_*`
would be actively wrong — call positions send every live floating-point value
to a stack slot and spill argument registers, none of which a descriptor needs.

The address computation is an IR opcode (`Opcode::TlsAddr`) rather than
something a backend `emit_*` helper synthesizes, because register allocation
runs over the IR and finishes before any machine instruction exists. `ir::tls`
expands thread-local accesses into it, and only under the dynamic model, so
Local-Exec keeps its one-instruction form.

Both architectures return an *offset* from the thread pointer, which the
sequence then adds — gcc hides this on x86-64 by folding the addition into the
access as `%fs:(%rax)`.


### Which macros may be withdrawn, and which may not

The distinction is what the macro is a statement *about*, and getting it wrong
once cost a correct macro:

- **Compiler capability** — `__GCC_HAVE_SYNC_COMPARE_AND_SWAP_N` means "I
  provide the `__sync_*` builtins". While c17 did not, the macro was false and
  withdrawing it was the fix; the family is implemented now and the macro is
  back. The rule is the same in both directions.
- **Target capability** — `__SSE2__` means "this target has SSE2". That is
  architectural baseline for x86-64 (32-bit x86 does *not* define it, which is
  the proof it describes the target rather than the compiler), and gcc defines
  it unconditionally. `__ARM_NEON` is the same: Advanced SIMD is mandatory in
  the AArch64 base architecture. Both are **true**, both stay.

Code that writes `#ifdef __SSE2__` around `#include <emmintrin.h>` is treating
a target fact as though it implied a compiler fact. That inference holds for
gcc and clang because they ship the intrinsic headers; c17 does not, so such a
file still fails on the missing header. **The honest gap is the header, not the
macro** — withdrawing a true statement about the target would not make the
header appear, and would break the far more common code that tests `__SSE2__`
to pick an algorithm rather than to reach for an intrinsic.

`__ARM_NEON__` was withdrawn on aarch64 for a third reason again: it is the
AArch32 spelling, and gcc does not define it there. c17 did, which was simply
wrong.

`a ?: b` is the one that could not simply be rewritten to `a ? a : b`: the
condition must be evaluated exactly once, so it has its own AST node. The
missing bit-manipulation builtins went in alongside it — `__builtin_clrsb` and
its wider forms, and `__builtin_ffsll`, which completed a family whose first
two members were already there.

## Torture tests skipped by decision

### Out of scope, and so skipped rather than counted

Anything GNU-specific or newer than C17 is **out of scope**: the harness skips
it with a named reason instead of reporting a failure, because counting it
measures a decision rather than a defect. These are skipped on top of what the
older `UNSUPPORTED_RE` already caught (`vector_size`, `__label__`,
`__builtin_apply`, `__builtin_setjmp`, `alias`).

Each entry is `<sub-suite>/<name>`, because a test name is not unique across
them: `20021204-1`, `20031011-1` and `20050119-1` name a nested-function test
in `compile/` **and** a different test in `execute/` that passes. A bare-name
list silenced all six.

| Category | Tests |
|---|---|
| Nested functions | `execute/`: `20010209-1`, `20010605-1`, `20030501-1`, `20040520-1`, `20090219-1`, `nest-align-1`, `nestfunc-7`, `nest-stdar-1`, `pr103405`, `pr22061-3`, `pr22061-4`. `compile/`: `20010903-2`, `20011023-1`, `20020309-1`, `20021204-1`, `20030418-1`, `20030716-1`, `20031011-1`, `20040310-1`, `20040317-3`, `20050119-1`, `951116-1`, `nested-2`, `nested-3`, `pr35006`, `pr99324`. Needs a static chain and executable trampolines |
| VLA as a struct member | `execute/`: `20020412-1`, `20040308-1`, `20040423-1`, `20041218-2`, `20070919-1`, `align-nest`, `pr41935`, `pr82210`. `compile/`: `20020210-1`, `20030224-1`, `20050801-2`, `920428-4`, `920501-16`, `pr42956`, `pr77754-6`, `pr82564`. Needs struct layout computed at run time, and `offsetof` through it |
| Post-C17 | `pr80692` (`_Decimal64`, TR 24732), `pr123978`, `pr124358`, `pr125291` (C23 `[[...]]` attributes), and `compile/pr111059-7`..`-12` and `compile/pr111911-2` (C23 `enum E : bool`) |
| GNU-only attribute | `20230630-2`, `20230630-4` (`scalar_storage_order`; needs reverse-endian load/store lowering) |
| gcc's own front ends | `compile/pr115143-2`, `compile/pr115143-3` (`-fgimple`, which parses gcc's internal representation rather than C; gcc rejects them without the flag too) |
| `-fgnu89-inline` semantics | `compile/20021120-1`, `compile/20021120-2`. c17 honours the flag; these also want a redefinition *rejected* without it, which c17 does not diagnose |
| Another target's backend | `compile/mipscop-1`..`-4` |
| `__builtin_issignaling` | `ieee/builtin-issignaling-1` and its eight format-specific siblings. No system header uses the builtin, and seven of the nine need a format c17 does not have (`_Float128`, `_Float64x`, `bfloat16`) |
| Pre-C99 implicit `int` with no dialect request | `compile/pr29201`. C17 6.7.2p2 requires a type specifier and gcc made it an error too; a test that asks, with `-std=gnu89` or `-fpermissive`, is honoured and passes |
| gcc-specific *behaviour* | `20021127-1` (gcc folds `llabs()` and never calls the program's own definition of it), `20031003-1` (gcc's folder saturates undefined behaviour; aarch64 agrees by hardware accident), `pr46309` (a conditional with one `void` arm, which C17 6.5.15p3 forbids) |

These are listed **by name** in the harness, never matched against the source.
Scanning for the feature looked tidier and was wrong: `pr86659-1`, `pr86659-2`
and `pr87623` all mention `scalar_storage_order` and **pass** anyway, so a
content match threw away three cases c17 gets right. A name list also keeps
every skip auditable, and a test added to the suite later shows up as a new
failure and gets triaged then -- which is the right moment to decide.

The one thing still matched by content is the older `UNSUPPORTED_RE`, and it
now follows a relative `#include` too: `pr71626-2`, `pr109938` and `pr109986`
are thin wrappers around files elsewhere in the tree, so the `vector_size`
that blocks them is not in the file named on the command line.

Skipping a test that *passes* is reported as a regression, by name -- proved by
injecting one into a skip list and watching the gate fail.

### Deliberate divergences from gcc

`20021127-1`, `20031003-1` and `pr46309` are skipped as gcc-specific behaviour
above; the reasoning is in that table. Two more are divergences c17 keeps but
does **not** skip, because neither is GNU-specific:

| Test | Why c17 does not follow |
|---|---|
| `991014-1` | Needs `sizeof` to answer ~9.2 exabytes exactly. Sizes are carried in **bits**, so that object is 2^66 bits -- not representable in a `u64` either, and `MAX_OBJECT_BYTES` is a consequence of `size_bits`'s return type rather than an arbitrary cap. Reaching it means changing the compiler's canonical size unit from bits to bytes, through the type table, IR instruction sizes, ABI classification and both backends |
| `920728-1` | `return;` in a function returning non-void. C17 6.8.6.4p1 makes it a constraint violation; gcc issues a warning and compiles. `-fpermissive` arguably ought to downgrade it, as it does for implicit `int` |

Complex integer division is a third, recorded in `BUILTIN.md`: c17 uses
Smith's method because the exact formula overflows, and so answers `6 + 1i`
for `(-9 + 38i) / (5 + 6i)` exactly as gcc does.
