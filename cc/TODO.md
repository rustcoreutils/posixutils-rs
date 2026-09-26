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

### Dominator construction is quadratic on a wide join

`domtree_build` is Cooper-Harvey-Kennedy, whose `intersect` walks the
dominator chain once per predecessor. A block with thousands of predecessors
under a deep chain -- the two labels every `if ... goto` in
`compile/20001226-1` jumps to -- makes that predecessors x depth. It is what
is left of that test's compile time (about a second for 8192 pairs, still
growing a little faster than linearly), and it runs once per `ssa_convert`
and again per `loadfwd`. Lengauer-Tarjan, which gcc uses, is near-linear on
any shape.

---

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

### Object size and value width are the same `u32`

`size_bits` answers a *value* width -- what an instruction operand holds,
bounded at 128 bits -- and `size_bytes` answers an *object* size, which is
not bounded. They are both plain integers, so nothing stops one being used
where the other is meant, and the unit (bits or bytes) is a naming convention
rather than a type.

That cost a silent miscompile once already. While `MAX_OBJECT_BYTES` was
`u32::MAX / 8`, `size_bits` could not saturate -- the parser refused any type
that would reach it -- so deriving a byte count as `size_bits / 8` was safe by
accident. Raising the bound made saturation reachable and every such site
began answering 536870911 for a larger type: array indexing, the stride of an
array of a large struct, and `p + 1` on a pointer to one, all while `sizeof`
stayed right.

A `grep` for `size_bits(..) / 8` finds nothing outside `size_bytes` itself, and
that is worth *less* than it appears: the class survived that grep at nine more
sites, because the bit count reaches the division through something a grep
cannot follow. Through a **local variable**, twice spelled `let
target_size_bytes = target_size / 8;` -- a name that says bytes over an
expression that computes them from bits. Through a **`u32` field** --
`Linearizer::struct_return_size`, `ArgClass::Indirect`'s payload, and
`Instruction::size`. And through an **equality** rather than a length, where two
distinct aggregates past the cap compare equal and a guard that meant "the same
type" stopped meaning it.

Those nine are fixed, and the remaining conversions are safe for reasons nothing
enforces: a `complex_base` is a scalar, a bit-field width is bounded by its
storage unit, and a threshold comparison against 64 or 128 answers correctly even
when saturated. That is three different arguments a reader has to reconstruct per
site, which is the problem.

What would settle it is making the distinction a *type* -- a `Bits` newtype
for value widths that deliberately implements no division, and a `ByteSize`
for object sizes -- so that every conversion is a compile error the compiler
finds rather than a spelling a reader has to notice. Widening `size_bits` to
`u64` is **not** that fix and was measured: of the 401 resulting type errors,
364 want a `u32` because they are value widths, and silencing them with `as
u32` reintroduces the same truncation at the seventy aggregate-fed sites.

A second unit lives in the same area and is settled: `TypeTable::MAX_OBJECT_BYTES`
bounds what a size can be *described* as, while
`TypeTable::MAX_STACK_OBJECT_BYTES` bounds what the backends can give a *slot*,
because a frame displacement is an `i32`. `crate::abi::slot_bytes` is the only
place an object size becomes that `i32`, and `arch::regalloc::grow_frame` the
only place a frame total grows. Lifting the second bound is its own feature --
see [64-bit stack frames](#64-bit-stack-frames).

---

### 64-bit stack frames

An automatic object past `MAX_STACK_OBJECT_BYTES` (just under 2 GiB) is refused
with a diagnostic, and so is a frame whose total passes it. gcc compiles both:
x86-64 reaches the frame through `movabsq`-materialised displacements, and
aarch64 through `movz`/`movk` into a scratch register. This is a gap, not a
decision -- the diagnostic exists so that c17 never emits a wrapped frame, and
it is the placeholder for this feature.

The torture harness skips the tests that need it by name,
`NEEDS_64BIT_FRAMES` in `cc/scripts/c17_torture.sh` (`compile/20031023-1..4`,
`compile/stack-check-1`), so that they are neither counted as failures nor
forgotten. Deleting that list is part of finishing this.

What it takes:

- Widen every frame quantity to `i64`: `MemAddr` displacements on both targets,
  `Loc::Stack`/`Loc::IncomingArg`, `RegAlloc::stack_offset`, the shared
  `ActiveSlot`/`FreeSlot`, `callee_saved_offset`, `stack_alloc_size`,
  `reg_save_area_offset`, the outgoing-argument layout, `IncomingOff`, and the
  CFI directive offsets. `grow_frame` and `slot_bytes` then bound at
  `MAX_OBJECT_BYTES` instead, less the prologue headroom
  (`FRAME_HEADROOM_BYTES`) and the frame's final alignment rounding, which
  `grow_frame` reserves today for the same reason.
- A displacement outside the target's encodable range goes through a scratch
  register: `movabsq` plus an indexed or `addq` form on x86-64. On aarch64
  `legalize.rs` already expands any offset through X15; what changes is only
  the width of the offsets it is given.
- The prologue's `subq $N, %rsp` becomes `movabsq $N, %r11; subq %r11, %rsp`.
- Stack probing. No target probes today; a frame larger than the guard gap
  should touch each page on the way down, as gcc and clang do.

---

### An `__atomic_*` read-modify-write ignores its memory order

`__atomic_fetch_add` and its eleven siblings lower through `emit_atomic_rmw`,
which an `_Atomic` compound assignment also uses and which is sequentially
consistent by C17 6.5.16.2p3. The `order` argument is evaluated and then
dropped, so `__atomic_fetch_add(p, v, __ATOMIC_RELAXED)` gets a seq-cst
operation -- correct, never wrong, and slower than asked for. The load, store,
exchange and compare-exchange forms *do* carry their order into the
instruction, so the family is inconsistent with itself.

What it needs is for `emit_atomic_rmw` and its CAS loop to take an order
rather than assuming one, and for the `_Atomic`-operator callers to keep
passing seq-cst. The aarch64 LL/SC loop then has to pick its acquire/release
variants from it.

---

## Optimization Passes

What exists today, and why the pass order is what it is, is in
[ir/README.md](ir/README.md). This section is only what is not built yet.

### Future passes (not yet implemented)

#### CFG Simplification

Merge simple blocks. Remove jumps-to-jumps. Converting a constant branch to an
unconditional jump is done, by `sccp`; collapsing a short-circuit diamond is
done, by `ifconv`. What is left is the block-level tidying neither of those
does, which is what keeps a collapsed diamond's now-empty predecessor around.

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

#### Const Globals: What Is Not Folded

A load of a `const` global becomes its initializer. What is declined, and
why, is in `ir/constglobal.rs`: `volatile`, a weak definition, a tentative
definition, an `extern` declaration, a type-punned read, and a load narrower
than the object.

An aggregate is foldable in principle -- `const int a[3] = {7,8,9}` knows
what `a[1]` is -- but needs the load's offset matched against the element or
member list rather than the whole-object compare the scalar case uses.

#### Float Constants: What Is Not Folded

Arithmetic, comparison, negation and both directions of conversion fold over
float constants, at the format the program computes in rather than at the 128
significand bits a literal is carried in.

What is deliberately left alone is everything that *raises*: a NaN or infinite
operand, a division by zero, a narrowing that overflows. C lets a program read
those flags through `<fenv.h>`, and folding the operation takes the flag with
it. Reinstating them would mean modelling the exception, not just the value.

`sccp` has no float lattice, so a float constant that is only constant along
one reachable path is not propagated -- only `instcombine` sees these.

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
| GCC torture tests | **Running.** `cc/scripts/c17_torture.sh`, baselined. Every sub-suite -- `execute/`, `execute/ieee/`, `execute/builtins/` and `compile/` -- in C17 mode; `dg_scan` strips `-std=` rather than selecting a dialect |
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
| Builtin folding | The whole of `execute/builtins/`. Each test defines its own `strlen`, `memcpy` or `printf` that calls `abort()` when `__OPTIMIZE__` is set, so a run-time failure there means c17 emitted a real call where gcc folded the builtin or expanded it inline. Nothing fails to *compile*, so no build is blocked; it is gcc-parity and code quality. Deferred by decision. The same group: `execute/printf-chk-1`, `fprintf-chk-1`, `vprintf-chk-1` and `vfprintf-chk-1` at `-O2`, which expect `__printf_chk` with a constant format to become `puts`/`putchar`; `builtins/abs-2`, `abs-3`, `complex-1` and `memcmp` at `-O2`, which expect a constant call folded so that a `link_error` reference disappears; and `builtins/strncmp` at `-O0`, whose own `strncmp` returns an uninitialised value for `n == 0`, so it passes only when the call is folded to 0 -- which gcc does at every level |
| Dead-call elimination proofs | `20030330-1` and `medce-1` at `-O0` (a constant branch keeps its arm there, which is recorded in DECISIONS.md), and `ieee/compare-fp-3` and `ieee/fp-cmp-6`/`-7`/`-9` at every level. Each calls an undefined `link_error` the optimizer is expected to delete, so they fail to *link*. Standard C, and optimizer strength rather than a defect: what is missing is folding a comparison whose operands are known to relate |
| `always_inline` on a library builtin | `pr46360`. `__attribute__((always_inline))` on a declaration of `strncpy` -- c17 refuses because it has no body to substitute, where gcc inlines its own expansion |
| An `extern inline` reading a file-scope static | `pr38857`. A C17 6.7.4p3 constraint gcc does not enforce. Relaxed by `-fpermissive`; the test does not pass it |
| Inline asm | `pr34966` -- an x87 output constraint on an operand with no home, at `-O2` only. `pr39394` -- an anonymous struct with a variably-modified member as an `"=m"` operand |
| An array subscript in a statement expression | `split-path-5`. `({ __typeof__(pat[i]) __x = (pat[i]); ... })` is rejected as a non-constant initializer for an object with static storage duration, which it is not |
| `__builtin_iseqsig` | `pr122588-1`. The IEEE signalling equality predicate; no system header uses it |
| The remaining divergence | `991014-1` |

One conformance gap worth naming: `(cond) ? some_void_call() : 0` is rejected.
gcc accepts a conditional with one `void` arm as an extension; C17 6.5.15p3
requires both or neither.
