# c17 TODO

## Table of Contents

- [Settled — do not re-open](#settled--do-not-re-open)
- [Technical Debt](#technical-debt)
- [Known Divergences](#known-divergences)
- [GNU extensions not implemented](#gnu-extensions-not-implemented)
- [Future Features](#future-features)
- [Optimization Passes](#optimization-passes)
- [Assembly Peephole Optimizations](#assembly-peephole-optimizations)
- [External Test Suites](#external-test-suites)

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

## Technical Debt

### Stack frames are larger than gcc's

CPython hardcodes `C_RECURSION_LIMIT 10000` (`Include/cpython/pystate.h`),
tuned to gcc's frame sizes. Test files that deliberately recurse to that limit
blow the default 8 MB stack under c17 before the counter trips; they pass with
`ulimit -s 65536`. Not a miscompile, but a real quality gap — and the
acceptance gate has to raise the stack to measure correctness.

At `-O2` on the default 8 MB stack, `test_call`, `test_descr`, `test_io`,
`test_isinstance` and `test_userdict` still run out of stack; the gate works
around it with `ulimit -s 65536`. Two things inflate every frame and are the
place to look next:

- `arch/x86_64/frame.rs` `zero_stack_frame` emits a `rep stosq` over the whole
  frame in *every* prologue, plus six register shuffles around it — even for a
  function with no locals at all. It is a workaround for narrow values stored
  to 8-byte slots and reloaded at wider widths, so it cannot simply be deleted;
  fixing the store/reload width mismatch would retire it.
- Every local occupies at least 8 bytes and slots are never reused
  (`arch/*/regalloc.rs`, `size.max(8)` with `reusable = false`), so a 4-byte
  `int` costs 8.

**Why `reusable = false` cannot simply be flipped.**
Setting it true changes nothing, and neither does the gate its own comment
proposes — `!addr_taken_syms.contains(..)` excludes every array, since indexing
emits `SymAddr`. The blocker is upstream of the flag: **a Sym pseudo has no
defining instruction**, so backward liveness finds every use upward-exposed and
propagates it to function entry. `compute_live_intervals` then gives anything
live-in `start = block_start_pos`, so every local's interval begins at 0. Six
`int[16]` locals in six disjoint scopes come out as
`[0,12] [0,35] [0,58] [0,81] [0,104] [0,127]` — all overlapping at 0, so
`try_reuse_stack_slot` never finds a free slot and the pool stays empty for the
whole function.

A fix needs both ends of the interval: a **start** at the local's declaration
rather than 0, which is what unblocks reuse; and an **end** extended over every
pseudo derived from the Sym (`SymAddr`, and copies or arithmetic on that
pointer), with a Sym whose address escapes staying permanent — which is what
makes reuse *safe*, and whose absence was the slot-reuse corruption fixed in
2026-05.

Scale, measured on `Python/ceval.c` at -O2, per function: c17's largest frame
is **7256 bytes** against gcc's **376**, and c17 emits a frame for 134
functions where gcc emits one for 44. `_PyEval_EvalFrameDefault` is the
outlier, and it is also the function that recurses, which is why the five
CPython tests above are the ones that fail.

A second, independent contributor: c17 spills values gcc keeps in registers. A
function whose locals never have their address taken and never outlive a call
still gets a frame — 16 bytes where gcc needs none — and eight simultaneously
live `int`s cost 72 bytes where gcc uses callee-saved registers and none. Slot
reuse is the larger multiplier, but this is why even leaf functions carry a
frame.

### A by-value struct argument is still copied word by word in the backend

Fixed at the IR level: copies past 128 bytes now become a `memcpy` call, which
took a 256 KB by-value struct from a 65-second compile to 0.01 s. The backend
still unrolls the *stacked-argument* copy at instruction-selection time, so the
same case emits ~65,000 `movq` where gcc emits one `call memcpy`. Compile time
is no longer the problem; code size is. The fix belongs wherever the stacked
argument is written, and has to avoid clobbering argument registers already set
up — which is why it was not folded into the IR-level change.

### R10 reserved globally for division scratch

**Location**: `arch/x86_64/regalloc.rs` lines 187-208

**Issue**: R10 is permanently excluded from the allocatable register pool because x86-64 `div`/`idiv` instructions clobber RAX and RDX. When the divisor is in RAX or RDX, we need a scratch register.

**Cost**: All generated code loses one GP register, even functions without division.

**Better solutions**:
1. Per-function reservation (only exclude R10 in functions with div/mod)
2. Instruction-level constraints in register allocator
3. Pre-coloring (constrain divisor to never be RAX/RDX)
4. Spill to stack when needed

---

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
| x86-64: a register aggregate after a stacked FP argument | With the SSE file exhausted and the ninth `double` placed on the stack, a following two-eightbyte INTEGER aggregate -- which still belongs in general registers -- is passed wrongly. Eight leading doubles pass, nine do not, and over-alignment is not involved: a plain `struct { long long a, b; }` reproduces it. Found while testing the aarch64 argument-alignment rule, whose own test is aarch64-only because this blocks the shape it needs |
| `_FORTIFY_SOURCE` | Compiles the wrappers and emits `__*_chk` calls, but still checks nothing. What remains -- folding `__builtin_object_size` after inlining -- is described above, and is an ordinary compiler feature rather than fortify-specific work |
| `-Ofast`, `-Oz` | Refused by name with a reason, where gcc and clang accept them. `-Ofast` relaxes IEEE arithmetic and c17 has no fast-math mode to relax into; `-Oz` has no smaller-than-`-Os` tier to select. `-Os` and `-Og` are supported |
| Identifier characters U+FD3E, U+FD3F | Rejected here; GCC's binary accepts them. Ornate parentheses, which ISO C Annex D excludes between its F900-FD3D and FD40-FDCF ranges -- GCC's own `ucnid.tab` does not list them and Clang's table does not either, so the table is followed rather than the binary. See #C158 |
| Non-NFC identifiers | GCC warns `-Wnormalized=` when an identifier is not in Normalization Form C; c17 is silent. A diagnostic-quality gap, not a conformance one -- both compile the same program |
| `#__VA_ARGS__` spacing | `V(a , b)` stringifies as `"a, b"`; gcc gives `"a , b"`. The separating comma's own spacing is discarded by the argument splitter. Pinned by `preprocessor_va_args_loses_space_before_a_separator` |
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
| `__atomic_*` / `__sync_*` | **Not implemented; macro withdrawn** | Alternate spellings of complete C11 atomics — see below |
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

### `__atomic_*` / `__sync_*` — not implemented, and not claimed

`__atomic_*` and `__sync_*` were the only rows to survive the filter on merit:
c17's C11 atomics are complete — type system, parser, IR, linearizer, both
backends, `<stdatomic.h>` — so these builtins would map onto machinery that
already exists rather than adding a subsystem, and would inherit the same
lock-free width ceiling (#X1).

They are still not implemented, and c17 does not claim them: it predefines no
`__GCC_HAVE_SYNC_COMPARE_AND_SWAP_{1,2,4,8}`, so a guarded `#ifdef` does not
open a door onto a wall when the `#else` beside it would have compiled.

### Which macros may be withdrawn, and which may not

The distinction is what the macro is a statement *about*, and getting it wrong
once cost a correct macro:

- **Compiler capability** — `__GCC_HAVE_SYNC_COMPARE_AND_SWAP_N` means "I
  provide the `__sync_*` builtins". c17 does not, so the macro was false and
  withdrawing it is the fix.
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

## Future Features

### C11 Atomics — remaining semantic validation

The type system, parser, IR, linearizer, both code generators, `<stdatomic.h>`,
and access through ordinary operators are all done. `_Atomic` on an array or
function type is rejected.

**Nothing remaining.** A struct or union *at* a machine width is operated on
through an unsigned integer surrogate. Anything else -- `long double`,
`__int128`, complex, and any width that is not a machine integer size, a 3-byte
struct included -- warns and falls back to an ordinary access. That ceiling is
deliberate: gcc's `__atomic_*` calls need `-latomic` and c17 links through the
host `cc` without it (#X1).

Accessing a member of an atomic struct is a warning, not a rejection: C11
6.5.2.3p5 makes it undefined behaviour rather than a constraint violation.

Rejecting `_Atomic` on a struct or union with a VLA member is unreachable: such
a type cannot be formed at all, since a VLA member is rejected outright and a
member reaching one through a typedef is refused by 6.7.2.1p9.

### C11 Thread-Local Storage

Complete on Linux: Local-Exec, Initial-Exec and the dynamic model, on both
architectures, with `-shared` and `-fPIC` taking the dynamic model while
`-fPIE` and a plain executable keep Local-Exec.

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

**Remaining:**
- Not implemented on FreeBSD, whose rtld may lack x86-64 descriptor support;
  TLS is gated on Linux, as it already was.
- The older `gnu` dialect is not implemented. If a target needs it, it belongs
  behind `-mtls-dialect=gnu`.
- Four latent Local-Exec sites remain in the x86-64 backend
  (`loc_to_gp_operand`, the two inline-asm operand paths, `loc_to_asm_string`,
  the last of which formats a thread-local as a plain `name(%rip)`). None is
  reachable from C source — under the dynamic model the expansion pass removes
  thread-local operands before codegen sees them — and three are `&self` and
  cannot emit a sequence at all.

---

## Optimization Passes

The compiler uses SSA-form IR. Already implemented passes (see `cc/ir/`):

- `instcombine` — constant folding, algebraic simplification. Constants are
  resolved through `Copy` chains (`ConstMap`), so folding crosses a promoted
  local rather than stopping at it
- `dce` — mark-sweep DCE, fold-cbr-to-trivially-unreachable, unreachable-block removal
- `inline` — function inlining (module-level)

`cc/opt.rs` runs `inline → (instcombine + dce)*` to fixed point. Promotion of
locals out of memory is not in that pipeline: `ir/ssa.rs` + `ir/mem2reg.rs`
run once during linearization, at every `-O` level.

### Future passes (not yet implemented)

#### SCCP — Sparse Conditional Constant Propagation

Propagate constants through CFG along reachable paths only. Lattice: `{UNDEF, CONST(c), UNKNOWN}`.

#### CFG Simplification

Convert constant branches to unconditional jumps. Merge simple blocks. Remove jumps-to-jumps.

#### Copy Propagation & SSA Cleanup

`t1 = x; y = t1;` → `y = x`. Simplify φ-nodes where all incoming operands are same.

**Unblocked on x86-64.** The defect was thirty-odd emitters in
`arch/x86_64/features.rs` that hand-rolled an `%rbp` displacement from a stack
slot index, which is the caller's incoming-argument area rather than the
callee's frame — `__builtin_bswap`, `__builtin_ctz`, `va_start`, `va_arg` and
`va_copy`. They are unreachable from C today because the linearizer
materializes every operand through a `Load`, so these emitters only ever see a
register; that is exactly the arrangement copy propagation undoes. They now go
through `stack_mem`/`stack_field`, which know where slots live.

aarch64 has had the same treatment: twenty-two emitters computed a frame
address themselves, eight of them with `frame_size + off`, and all now go
through `stack_mem`/`stack_mem_plus`. The producers are typed too — `LocalSlot`
and `IncomingOff` apply their sign convention inside their constructors, so the
two spaces can no longer be assigned to one another.

What remains on that target is the `Loc::IncomingArg` variant itself, which is
what would make the distinction *exhaustively* checked rather than centralized
in three accessors — see #C34 in git log for why adding the variant
naively would be a step backwards.

#### Local CSE / Value Numbering

Inside a block, deduplicate `t1 = a + b; t2 = a + b;` → `t2 = t1`.

#### GVN — Global Value Numbering

Deduplicate computations across blocks using dominator-order value numbering.

#### LICM — Loop-Invariant Code Motion

Hoist pure, loop-invariant computations out of loop bodies.

#### Loop Canonicalization & Strength Reduction

Normalize induction variables. Replace multiplications with additions.

### Suggested pass pipeline

```
InstCombine → SCCP → DCE → CFG simplify → Copy prop → Local CSE → InstCombine
[Later] GVN → DCE → Inlining → re-run above → LICM → Loop opts → final cleanup
```

### Priority

| Priority | Pass | Complexity | Impact |
|---|---|---|---|
| 1 | CFG simplify | Low | Medium |
| 2 | Copy/φ cleanup | Low | Medium |
| 3 | Local CSE | Medium | Medium |
| 4 | SCCP | Medium | High |
| 5 | GVN | High | Medium |
| 6 | LICM | Medium | Medium |
| 7 | Loop opts | High | Low |

---

## Assembly Peephole Optimizations

Post-codegen peephole optimizations on generated assembly.

| Pattern | Optimization |
|---|---|
| `mov %rax, %rax` | Delete (no-op move) |
| `mov %rax, %rbx; mov %rbx, %rax` | Delete second (useless copy-back) |
| `add $0, %rax` | Delete (no-op add) |
| `imul $1, %rax, %rax` | Delete (multiply by 1) |
| `cmp $0, %rax; je L` | `test %rax, %rax; je L` (shorter) |
| `mov $imm, %rax; add %rax, %rbx` | `add $imm, %rbx` if imm fits |

## External Test Suites

Carried over from the retired C99 checklist, where they sat as unticked
conformance boxes. They are not conformance gaps -- they are test-coverage
work, and were never a claim about the language.

| Suite | Note |
|---|---|
| GCC torture tests | **Running.** `cc/scripts/c17_torture.sh`, baselined. The whole `execute/` directory, in C17 mode -- `dg_scan` strips `-std=` rather than selecting a dialect |
| clang test suite | Not run against c17 |

These are not only test-coverage work. A differential probe against
`gcc -std=c17` at `-O0` and `-O2` reaches silent miscompiles in mandated C99
features that the CPython acceptance gate never touches.
`gcc.c-torture/execute` is a few thousand self-checking programs needing no
reference compiler, and is the highest-yield item on this page.

`cc/scripts/c17_torture.sh` drives it against an external checkout (the suite
is GPLv3 and is not vendored) and diffs a recorded baseline, so a regression
fails by name rather than shifting a percentage.

The suite is run at `-O0` and `-O2`. `c17_torture.sh` prints the totals and
names every regression against `torture-baseline.txt`; the groups below say
what each remaining failure needs.

One conformance gap found while working through it: c17 has no C17 6.7.3p2
check, so `restrict int x;` is accepted where gcc errors that `restrict` may
only qualify a pointer to object type.

### Out of scope, and so skipped rather than counted

Anything GNU-specific or newer than C17 is **out of scope**: the harness skips
it with a named reason instead of reporting a failure, because counting it
measures a decision rather than a defect. These are skipped on top of what the
older `UNSUPPORTED_RE` already caught (`vector_size`, `__label__`,
`__builtin_apply`, `__builtin_setjmp`, `alias`).

| Category | Tests |
|---|---|
| Nested functions | `20010209-1`, `20010605-1`, `20030501-1`, `20040520-1`, `20090219-1`, `nest-align-1`, `nestfunc-7`, `nest-stdar-1`, `pr103405`, `pr22061-3`, `pr22061-4`. Needs a static chain and executable trampolines |
| VLA as a struct member | `20020412-1`, `20040308-1`, `20040423-1`, `20041218-2`, `20070919-1`, `align-nest`, `pr41935`, `pr82210`. Needs struct layout computed at run time, and `offsetof` through it |
| Post-C17 | `pr80692` (`_Decimal64`, TR 24732), `pr123978`, `pr124358`, `pr125291` (C23 `[[...]]` attributes) |
| GNU-only attribute | `20230630-2`, `20230630-4` (`scalar_storage_order`; needs reverse-endian load/store lowering) |
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

### Still open

Most of what is left is one thing.

| Group | Note |
|---|---|
| Dead-call elimination proofs | `20011115-1`, `20020720-1`, `20030216-1`, `20041114-1`, `compare-3`, `pure-1`, `shiftopt-1` at -O2; `20030330-1` and `medce-1` at **both** levels, because neither has an `#ifndef __OPTIMIZE__` fallback definition and gcc deletes an `if (0)` body in CFG cleanup, which it runs at -O0 too. Each calls an undefined `link_error` the optimizer is expected to delete, so they fail to *link*. Standard C, and optimizer strength rather than a defect -- building real dead-code and value-range analysis would improve -O2 generally, well beyond these tests |
| Missing optimizations behind `__OPTIMIZE__` | `20030125-1`, `builtin-constant`. Same class: the tests only assert them when the optimizer is on, and each fails at `-O2` only |
| Address of a string-literal element as a constant | `921019-1`: `(void *)&("X"[0])` in a static initializer |
| The two divergences above | `991014-1`, `920728-1` |

One conformance gap worth naming: `(cond) ? some_void_call() : 0` is rejected.
gcc accepts a conditional with one `void` arm as an extension; C17 6.5.15p3
requires both or neither.
