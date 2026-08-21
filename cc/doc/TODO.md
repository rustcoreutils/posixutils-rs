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

**Not a conformance item, and the sole record of this one.** It was also
tracked as **#C12** in `cc/audit.md` until 2026-08-21, when it was removed
from there: `_FORTIFY_SOURCE`, `__builtin_object_size` and the `_chk` family
appear nowhere in POSIX.1-2024, so an entry in a POSIX conformance audit's
Open list overstated what it was. The number is kept here so
`git log --grep '#C12'` still finds the history. **Deferred indefinitely by
maintainer decision (2026-08-19)** — a decision, not a backlog item.

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
diagnostic saying so. That is layer 8 below.

Peeling this apart one layer at a time has been the only way to see it: each
fix exposes the next blocker, and most of the layers are invisible until the
one before it is in place. Seven are done:

1. `__builtin_object_size` computing real sizes rather than "unknown".
2. Implicit declarations for the `__builtin___*_chk` family.
3. Asm label renaming, so glibc's `__REDIRECT` aliases resolve.
4. `always_inline`, so the fortified wrapper reaches the caller at all.
5. Inline definitions emitting no external definition.
6. `__builtin_va_arg_pack` / `_len`, so the wrappers' argument forwarding
   compiles at all — see below.
7. Predefining `__OPTIMIZE__`, which is what makes glibc compile the wrappers.

~~**Layer 5 — `__gnu_inline__` / `extern inline` must emit no out-of-line
definition.**~~ Done. It turned out not to be a fortify problem at all: `inline`
was never recorded on a function definition in the first place, so *every*
spelling emitted an external definition and an `inline` function in a shared
header failed to link.

**Layer 6 — `__builtin_va_arg_pack`.** ~~Done.~~ This, not layer 8, was the
next thing in the way, and this file did not know it. glibc's `bits/stdio2.h`
forwards `sprintf`/`printf` into the `__*_chk` family with `__va_arg_pack()`,
and those wrappers are compiled only when `__OPTIMIZE__` is defined — so
predefining `__OPTIMIZE__` without the builtin failed to compile *any* program
including `<stdio.h>` with `_FORTIFY_SOURCE`. The claim below that
`__OPTIMIZE__` and the object-size fold "must land together" was therefore
wrong twice over: the pairing it named was not the blocking one, and neither
had to wait for the other.

**Layer 7 — `__OPTIMIZE__`.** Done, with `__OPTIMIZE_SIZE__` and
`__NO_INLINE__` alongside it. Every row matches gcc; see
`c17_optimization_macros_match_gcc`.

**Layer 8 — `__builtin_object_size` has to be folded after inlining.** The
wrapper computes the size of its own `__dest` *parameter*, which is genuinely
unknown, so the front end folds it to `-1` before the inliner ever runs and the
`_chk` call is handed `-1` — a value that means "do not check". gcc defers the
fold until after inlining, when `__dest` is known to be the caller's `buf`.
Fixing this means carrying the query into the IR and folding it in a
post-inline pass that can trace a pointer back to its object.

`__OPTIMIZE__` was previously reverted three times, most recently after
measuring the duplicate-symbol failure that turned out to be layer 5. It is in
now, and it did not need layer 8 to get there: the fortified wrappers compile,
link and run, they simply do not check. That is worse than checking and better
than not compiling, and it is where GCC parity stops.

Layer 8 is the whole remaining job, and it is a real one. There is no IR
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
See #C55 in `cc/audit.md`.

## Technical Debt

### Stack frames are larger than gcc's

CPython hardcodes `C_RECURSION_LIMIT 10000` (`Include/cpython/pystate.h`),
tuned to gcc's frame sizes. Twelve of its test files deliberately recurse to
that limit and blow the default 8 MB stack under c17 before the counter trips;
they pass with `ulimit -s 65536`. Not a miscompile, but a real quality gap —
and the acceptance gate has to raise the stack to measure correctness.

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

Behaviours where c17 differs from gcc on the same source. None is a
translation-limit or a diagnostic gap; each silently changes what the program
does or claims.

_The `__int128` and `long double` argument-passing bugs, the universal-character-name
encoding, and the silently-dropped attributes used to belong here; all are closed, see
#C42, #C43, #C57, #C58 and #C59 in `cc/audit.md`. What remains of that family: `used` is
satisfied only because nothing is pruned -- re-probed 2026-08-20, an unreferenced static
survives `-O2` whether or not it is marked. `vector_size` is implemented as storage and
`mode` as of #C85, the latter also binding to a struct member's and a parameter's
declarator as of #C149; only the vector modes still warn. #C38 -- an inlined stacked
float HFA on aarch64 -- is fixed, this note having outlived it (re-probed 2026-08-18 at
-O0 and -O2 under qemu)._

_Constraint diagnostics used to belong here. As of 2026-08-15 a 35-case matrix
-- 21 constraint violations and 14 accept-side controls -- agrees with
`gcc -std=c17` on every row; see #C45-#C49 in `cc/audit.md`. Two divergences are
deliberate and remain: `return` with a value in a `void` function, and a bare
`return` in a non-`void` one, are errors here and warnings in gcc, both being
genuine 6.8.6.4 violations. `__attribute__((transparent_union))` used to sit
here too -- an argument matching any member of *any* union parameter was
accepted rather than checked -- and is closed, see #C51 in `cc/audit.md`._

_The packed-bit-field layout used to be a row in the table below:
`struct __attribute__((packed)) { unsigned a:20, b:20; }` was 8 bytes here and
5 under gcc. Closed by #C110 -- under a pack cap the unit rule is switched off
entirely, and a span that is not one addressable unit is assembled byte by
byte, as gcc does on both targets._

| Area | Divergence |
|------|-----------|
| `_FORTIFY_SOURCE` | Compiles the wrappers and emits `__*_chk` calls, but still checks nothing. Seven of eight layers are done; the one that remains -- folding `__builtin_object_size` after inlining -- is described above, and is an ordinary compiler feature rather than fortify-specific work |
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
|-----------|---------|-----|
| SIMD intrinsic headers | **No — fix the predefines** | See below; the blocking is self-inflicted |
| `__atomic_*` / `__sync_*` | **Not implemented; macro withdrawn** | Alternate spellings of complete C11 atomics — see below |
| `__auto_type` | **No** | 6 files across four trees; fails minimalism on its own numbers |
| nested functions / `__label__` | **Never** | GCC-only, Clang refuses it, needs executable-stack trampolines |

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

### Atomics — the macro went, the builtins did not come

`__atomic_*` and `__sync_*` were the only rows to survive the filter on merit:
c17's C11 atomics are complete — type system, parser, IR, linearizer, both
backends, `<stdatomic.h>` — so these builtins would map onto machinery that
already exists rather than adding a subsystem, and would inherit the same
lock-free width ceiling (#X1).

They are still not implemented. What changed is that c17 no longer *claims*
them: it predefined `__GCC_HAVE_SYNC_COMPARE_AND_SWAP_{1,2,4,8}` on both
targets while `__sync_bool_compare_and_swap` was an undeclared identifier, so a
guarded `#ifdef` opened a door onto a wall when the `#else` beside it would have
compiled. The macro is gone, which is what makes the guard tell the truth.

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

Implemented since this list was first measured: case ranges, designated
initializer ranges, computed goto, and the omitted middle operand `a ?: b`
(1373 files of the Linux kernel, and five of the six sparse files c17 could not
parse). All four passed the same filter for a reason worth recording — each is
a *syntax* over control flow, initializers or an operator c17 already had, none
added a subsystem, and all four block outright with no fallback path. CPython's
configure now detects computed gotos, so it builds its indirect-branch
interpreter rather than the switch fallback.

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

**Nothing remaining.** An `_Atomic` aggregate of lock-free size used to fall
through to a non-atomic struct copy; closed by #C116, which admits a struct or
union *at* a machine width and operates on its bits through an unsigned integer
surrogate. Anything else -- `long double`, `__int128`, complex, and any width
that is not a machine integer size, a 3-byte struct included -- still warns and
falls back to an ordinary access. That ceiling is deliberate and unchanged:
gcc's `__atomic_*` calls need `-latomic` and c17 links through the host `cc`
without it (#X1).

The deferral this section used to record was justified by the
value-versus-address convention for small aggregates needing to be settled
first. That turned out not to be a blocker, and not to be one convention --
see #C116 in `cc/audit.md` for what the three sites actually decide. The member-access
warning that stood here is #C113, closed 2026-08-18; C11 6.5.2.3p5 makes it
undefined behaviour rather than a constraint violation, so it is a warning
rather than the rejection an earlier version of this list called for.

Two entries that used to sit here were removed after being probed rather than
implemented. Rejecting `_Atomic` on a struct or union with a VLA member is
unreachable: such a type cannot be formed at all, since a VLA member is
rejected outright and a member reaching one through a typedef is refused by
6.7.2.1p9. (That second half used to read "the typedef route is rejected at
the typedef", which stopped being true when #C138 implemented 6.7.7; the
conclusion survives, the reason changed. Re-probed 2026-08-19, both spellings.)
And `int *_Atomic p;` now parses -- it was a qualifier-list bug at file scope,
not a missing feature.

### Variably modified `typedef` — done

C17 6.7.7 is implemented as of #C138. The interesting half is 6.7.7p3's "the
array size expressions are evaluated each time the declaration of the typedef
name is reached in the order of execution": at the typedef, once, however many
objects the name then declares, and again on re-entry. `ExprKind::VmTypedefExtent`
names an extent the typedef already evaluated, so a use rides the existing VLA
machinery rather than repeating the size expressions -- copying those to each
use would re-evaluate them and get all three rules wrong.

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

That second point is why the register allocator turned out not to be the
blocker this file previously described. The sequence is **not** call-like: it
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

- `instcombine` — constant folding, algebraic simplification
- `dce` — mark-sweep DCE, fold-cbr-to-trivially-unreachable, unreachable-block removal
- `inline` — function inlining (module-level)

`cc/opt.rs` runs `inline → (instcombine + dce)*` to fixed point.

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
in three accessors — see #C34 in `cc/audit.md` for why adding the variant
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
|----------|------|------------|--------|
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
|---------|--------------|
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
|-------|------|
| GCC torture tests (C99 subset) | Not run against c17 |
| clang test suite (C99 subset) | Not run against c17 |

**Reclassified 2026-08-21.** These are no longer only test-coverage work. A
hand-written differential probe of nine ordinary constructs against
`gcc -std=c17`, at `-O0` and `-O2`, found a silent miscompile in its first
hour (#C155, `&vla`), in a mandated C99 feature that CPython's 40,817 passing
tests never reach. `gcc.c-torture/execute` is a few thousand self-checking
programs needing no reference compiler, and is the highest-yield item on this
page.
