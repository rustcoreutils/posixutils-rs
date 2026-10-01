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
tuned to gcc's frame sizes. At `-O2` on the default 8 MB stack, `test_call`,
`test_compile` and `test_isinstance` recurse toward that limit and run out of
stack before the counter trips; the acceptance gate raises the stack with
`ulimit -s 65536` to measure correctness. Not a miscompile, but a real quality
gap.

Locals are not what inflates a frame: each has a lifetime, locals whose
lifetimes never meet share a slot (`arch::regalloc::local_lifetimes` and
`place_locals`), and no prologue zeroes its frame. What does is spilling --
c17 keeps in the frame what gcc keeps in registers -- and the interpreter loop,
`_PyEval_EvalFrameDefault`, is both the largest frame and the function that
recurses. Two contributors, in both allocators:

- A floating-point value live across a call or a block boundary always goes to
  a stack slot: the chordal pass does not model evicting a vector register
  across a call (`run_chordal_color`).
- A large function's register pressure: there is no live-range splitting, so a
  value that is spilled anywhere is spilled everywhere.

A slot stays at least eight bytes, and that is not part of this gap: the back
ends move register-passed values and aggregate tails a whole eightbyte at a
time (see `arch::regalloc::local_slot`).

### R10 and R11 are never allocated

Every x86-64 emitter that needs a temporary takes R10 or R11, so the allocator
gives neither to a pseudo -- in every function, though most never use them.
Division needs neither: `div`/`idiv`'s RAX:RDX and a variable shift's RCX
are allocator constraints (`RegConstraints`). Freeing R10 and R11
per function would take each emitter asking the allocator for its scratch,
as an instruction-level constraint, rather than assuming it.

---

## Future Features

### C11 Thread-Local Storage

Complete on Linux and macOS, on both architectures. What is left:

- FreeBSD gets the ELF Local and Initial Exec models, but not the descriptor
  model: its rtld may lack x86-64 descriptor support, so `-fPIC`/`-shared`
  code uses Initial Exec there, which cannot be `dlopen`ed with a thread-local
  block larger than the static-TLS surplus.
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

That cost a silent miscompile once already. While `max_object_bytes` was
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

A second unit lives in the same area and is settled: `TypeTable::max_object_bytes`
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
  `max_object_bytes` instead, less the prologue headroom
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

#### Const Globals: What Is Not Folded

A load of a `const` global -- by name or through its address -- becomes its
initializer. What is declined, and why, is in `ir/constglobal.rs`: `volatile`,
a weak definition, a tentative definition, an `extern` declaration, a
type-punned read, and a load of less than the whole object.

An aggregate is foldable in principle -- `const int a[3] = {7,8,9}` knows
what `a[1]` is -- but needs the load's offset matched against the element or
member list rather than the whole-object compare the scalar case uses.

#### Float Constants: What Is Not Folded

Arithmetic, comparison, negation and both directions of conversion fold over
float constants, at the format the program computes in rather than at the 128
significand bits a literal is carried in.

Arithmetic and conversion leave alone everything that *raises*: a NaN or
infinite operand, a division by zero, a narrowing that overflows. C lets a
program read those flags through `<fenv.h>`, and folding the operation takes
the flag with it. Reinstating them would mean modelling the exception, not
just the value.

Comparisons are the exception, as in gcc: one against a NaN constant folds
(every ordered predicate to 0, `!=` to 1) although an ordered comparison with
a NaN is specified to raise `FE_INVALID`. Both backends emit quiet compares,
which do not raise it for a quiet NaN either.

`sccp` has no float lattice, so a float constant that is only constant along
one reachable path is not propagated -- only `instcombine` sees these.

#### GVN and Global Code Motion, as one project

Global value numbering finds congruent computations across blocks, and global
code motion (Click's, as QBE's `gcm.c` does it) places each where it belongs:
the two are one project, since a congruence GVN finds has no control
dependence until GCM gives it one. GCM needs only the dominator tree
(`ir/dominate.rs`) and a loop tree, and subsumes loop-invariant code motion,
code sinking and partial dead-code elimination -- with none of the loop
canonicalization (preheaders, a single back edge, LCSSA) a separate LICM would
need. Two things to get right: pin what may trap or touch memory -- loads,
`alloca`, `div`/`rem` -- so GVN does not merge what GCM will not move, and
re-verify that no use precedes its definition within a block afterward.

#### Local CSE

Inside a block, `t1 = a + b; t2 = a + b;` becomes `t2 = t1`. Measure before
building it: most of what looks like redundancy in the IR is copies, which
`copyprop` removes, and constants, which cost nothing.

---

## Assembly Peephole Optimizations

Post-codegen cleanup of the two patterns c17 emits in bulk:

| Pattern | Optimization |
|---|---|
| A register-to-register `mov` whose source dies at it | Coalesce in the allocator, or delete the move and rename |
| `jmp L` (aarch64 `b L`) immediately followed by `L:` | Delete the jump |

## External Test Suites

Carried over from the retired C99 checklist, where they sat as unticked
conformance boxes. They are not conformance gaps -- they are test-coverage
work, and were never a claim about the language.

| Suite | Note |
|---|---|
| GCC torture tests | **Running.** `cc/scripts/c17_torture.sh`, baselined. Every sub-suite -- `execute/`, `execute/ieee/`, `execute/builtins/` and `compile/` -- in C17 mode; `dg_scan` strips `-std=` rather than selecting a dialect. `-t aarch64` builds the same suite for linux-aarch64, assembles every `compile/` output with the cross assembler and runs the executables under qemu, against `torture-baseline-aarch64.txt`: the only gate aarch64 code generation has |
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
names every regression against `torture-baseline.txt`.

One conformance gap found while working through it: c17 has no C17 6.7.3p2
check, so `restrict int x;` is accepted where gcc errors that `restrict` may
only qualify a pointer to object type.

One conformance gap worth naming: `(cond) ? some_void_call() : 0` is rejected.
gcc accepts a conditional with one `void` arm as an extension; C17 6.5.15p3
requires both or neither.
