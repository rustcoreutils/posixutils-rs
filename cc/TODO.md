# c17 TODO

Outstanding work only: everything here is something c17 intends to finish.

Decisions -- what c17 will not do, and where it deliberately differs from gcc
-- are in [DECISIONS.md](DECISIONS.md). A settled choice is not a to-do, and
keeping one here reads as a backlog item nobody is picking up.

## Table of Contents

- [Technical Debt](#technical-debt)
- [Future Features](#future-features)
- [Optimization Passes](#optimization-passes)
- [Assembly Peephole Optimizations](#assembly-peephole-optimizations)
- [External Test Suites](#external-test-suites)

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

## Future Features

### C11 Thread-Local Storage

Complete on Linux, on both architectures. What is left:

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

What exists today, and why the pass order is what it is, is in
[ir/README.md](ir/README.md). This section is only what is not built yet.

### Future passes (not yet implemented)

#### CFG Simplification

Convert constant branches to unconditional jumps. Merge simple blocks. Remove jumps-to-jumps.

#### Copy Propagation & SSA Cleanup

`t1 = x; y = t1;` → `y = x`. Simplify φ-nodes where all incoming operands are same.

**Partly delivered, in read-only form.** `instcombine`'s `ConstMap` answers
what a pseudo ultimately copies from (`root`) and what constant reaches it at
a stated width and signedness (`get_at`), which is what the identity rules --
`x - x`, `x & x`, `x == x` -- actually need: promotion out of memory gives
every use of a local its own `Copy`, so the two sides of `x >> 0 != x` arrive
as distinct pseudos naming one value. That is a query, not a rewrite: the
copies are still there for `dce` to collect, and no pseudo is merged. A real
pass would rewrite the uses and delete them.

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
SCCP → InstCombine → DCE  (today)
[Next] CFG simplify → Copy prop → Local CSE
[Later] GVN → DCE → Inlining → re-run above → LICM → Loop opts → final cleanup
```

### Priority

| Priority | Pass | Complexity | Impact |
|---|---|---|---|
| 1 | CFG simplify | Low | Medium |
| 2 | Copy/φ cleanup | Low | Medium |
| 3 | Local CSE | Medium | Medium |
| 4 | GVN | High | Medium |
| 5 | LICM | Medium | Medium |
| 6 | Loop opts | High | Low |

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

### Still open

Most of what is left is one thing.

| Group | Note |
|---|---|
| Dead-call elimination proofs | `20011115-1`, `20020720-1`, `20030216-1`, `20041114-1`, `compare-3`, `pure-1`, `shiftopt-1`, at `-O1` and above. Each calls an undefined `link_error` that the optimizer is expected to delete, so they fail to *link*; all pass at `-O0`, where the test's own `#ifndef __OPTIMIZE__` supplies a definition. Standard C, and optimizer strength rather than a defect. What each one needs differs: a copy-chain root for the identity tests (`shiftopt-1`), if-conversion of a short-circuit diamond (`compare-3`), a transitive dead-static prune (`20011115-1`), `fabs` recognized in its plain spelling plus float folding (`20020720-1`), propagation of a load from a `const` global (`20030216-1`), and value-range propagation across an edge (`20041114-1`) or escape analysis with store-to-load forwarding (`pure-1`) for the two large ones |
| Missing optimizations behind `__OPTIMIZE__` | `20030125-1` needs `(float)floor((double)x)` narrowed to `floorf(x)`, which is exact only for the exactly-rounding functions -- the test's weak `sinf` aborts to catch an over-eager narrower. `builtin-constant` needs `__builtin_constant_p` answered after propagation rather than syntactically at parse time. Both abort at run time rather than failing to link |
| Inline definition with no out-of-line body | `930526-1`, at `-O1` and `-Og` only. gnu89 `inline` emits no out-of-line body, so the call must be inlined or it cannot link; the callee is over the inliner's size cap and level 1 does not inline aggressively |
| Address of a string-literal element as a constant | `921019-1`: `(void *)&("X"[0])` in a static initializer |
| The two divergences above | `991014-1`, `920728-1` |

One conformance gap worth naming: `(cond) ? some_void_call() : 0` is rejected.
gcc accepts a conditional with one `void` arm as an extension; C17 6.5.15p3
requires both or neither.
