# c17 TODO

Outstanding work only: everything here is something c17 intends to finish.

Decisions -- what c17 will not do, and where it deliberately differs from gcc
-- are in [DECISIONS.md](DECISIONS.md). A settled choice is not a to-do, and
keeping one here reads as a backlog item nobody is picking up.

## Table of Contents

- [Technical Debt](#technical-debt)
- [Future Features](#future-features)
- [Conformance](#conformance)
- [Optimization Passes](#optimization-passes)
- [Assembly Peephole Optimizations](#assembly-peephole-optimizations)

## Technical Debt

### Stack frames are larger than gcc's

CPython hardcodes `C_RECURSION_LIMIT 10000` (`Include/cpython/pystate.h`),
tuned to gcc's frame sizes. At `-O2` on the default 8 MB stack, `test_call`,
`test_compile` and `test_isinstance` recurse toward that limit and run out of
stack before the counter trips, so the acceptance gate has to run under
`ulimit -s 65536`. Done is the CPython `-O2` suite passing on the default
8 MB stack.

Locals are not what inflates a frame: locals whose lifetimes never meet share
a slot (`arch::regalloc::local_lifetimes`, `place_locals`), and no prologue
zeroes its frame. Spilling is: c17 keeps in the frame what gcc keeps in
registers, and the interpreter loop, `_PyEval_EvalFrameDefault`, is both the
largest frame and the function that recurses. Two contributors, in both
allocators:

- A floating-point value live across a call or a block boundary always goes to
  a stack slot (`run_chordal_color`): the chordal pass never gives such a value
  a register, not even a callee-saved one where the ABI has them (aarch64
  `d8`-`d15`).
- There is no live-range splitting, so a value spilled anywhere is spilled
  everywhere, which is what a large function's register pressure turns into.

A slot stays at least eight bytes, and that is not part of this gap: the back
ends move register-passed values and aggregate tails a whole eightbyte at a
time (see `arch::regalloc::local_slot`).

### R10 and R11 are never allocated

`Reg::allocatable()` (`arch/x86_64/regalloc.rs`) leaves out R10 and R11 in
every function, because x86-64 emitters take them as scratch without asking.
The per-opcode half of the fix exists -- `opcode_clobbers_r10_r11` already
turns an opcode's scratch use into a constraint point -- but it cannot be
switched on while scratch use crosses IR-instruction boundaries: the variadic
register save area, the restore of spilled arguments, and several
multi-instruction floating-point and struct lowerings hold a value in R10/R11
from one instruction to the next.

Done is R10 and R11 in `Reg::allocatable()`, with each of those emitters
either confined to one instruction's lowering or declaring its scratch as a
constraint.

### Latent Local-Exec paths in the x86-64 backend

Four sites print a thread-local as a Local-Exec operand whatever the model:
`loc_to_gp_operand` (`MemAddr::TlsLocalExec`), the two inline-asm operand
paths in `arch/x86_64/inline_asm.rs` (`%fs:sym@TPOFF`), and
`loc_to_asm_string`, which formats a thread-local as a plain `name(%rip)`.
None is reachable from C source -- under the dynamic model `ir::tls` removes
thread-local operands before codegen, and the static model's accesses go
through other paths -- and three are `&self`, so they cannot emit an Initial
Exec sequence. They should report an internal error, as the backends already
do for a thread-local named anywhere but a `TlsAddr`, rather than print an
access that is wrong for an `extern` or shared-mode thread-local.

---

## Future Features

### Object size and value width are the same `u32`

`TypeTable::size_bits` answers a *value* width -- what an instruction operand
holds, bounded at 128 bits -- and `size_bytes` answers an *object* size, which
is not bounded. Both are plain integers, so nothing stops one being used where
the other is meant, and the unit is a naming convention rather than a type.
`size_bits` saturates for an object past `u32::MAX` bits, so any byte count
derived from it is wrong for a large object while `sizeof` stays right.

A `grep` for `size_bits(..) / 8` is not a check: the bit count reaches a
division through a local variable, a `u32` field (`ArgClass::Indirect`'s
payload, `Instruction::size`) or an equality comparison, none of which the
grep follows. The conversions that remain are safe for three different
reasons nothing enforces: a `complex_base` is a scalar, a bit-field width is
bounded by its storage unit, and a threshold comparison against 64 or 128
answers correctly even when saturated.

Done is the distinction as a *type*: a `Bits` newtype for value widths that
implements no division, and a `ByteSize` for object sizes, so every conversion
is a compile error. Widening `size_bits` to `u64` is not this fix -- most of
its callers want a `u32` because they are value widths, and casting them back
reintroduces the truncation at the aggregate-fed sites.

---

### 64-bit stack frames

An automatic object past `TypeTable::MAX_STACK_OBJECT_BYTES` (just under
2 GiB) is refused with a diagnostic, and so is a frame whose total passes it,
because a frame displacement is an `i32`. gcc compiles both: x86-64 reaches the
frame through `movabsq`-materialised displacements, and aarch64 through
`movz`/`movk` into a scratch register. The diagnostic exists so that c17 never
emits a wrapped frame.

The torture harness skips the tests that need it by name,
`NEEDS_64BIT_FRAMES` in `cc/scripts/c17_torture.sh` (`compile/20031023-1..4`,
`compile/stack-check-1`). Deleting that list is part of finishing this.

What it takes:

- Widen every frame quantity to `i64`: `MemAddr` displacements on both targets,
  `Loc::Stack`/`Loc::IncomingArg`, `RegAlloc::stack_offset`, the shared
  `ActiveSlot`/`FreeSlot`, `callee_saved_offset`, `stack_alloc_size`,
  `reg_save_area_offset`, the outgoing-argument layout, `IncomingOff`, and the
  CFI directive offsets. `crate::abi::slot_bytes` (the only place an object
  size becomes a slot size) and `arch::regalloc::grow_frame` (the only place a
  frame total grows) then bound at `max_object_bytes` instead, less the
  prologue headroom (`FRAME_HEADROOM_BYTES`) and the frame's final alignment
  rounding, which `grow_frame` reserves for the same reason.
- A displacement outside the target's encodable range goes through a scratch
  register: `movabsq` plus an indexed or `addq` form on x86-64. On aarch64
  `legalize.rs` already expands any offset through X15; only the width of the
  offsets it is given changes.
- The prologue's `subq $N, %rsp` becomes `movabsq $N, %r11; subq %r11, %rsp`.
- Stack probing: a frame larger than the guard gap touches each page on the
  way down, as gcc and clang do. No target probes.

---

### An `__atomic_*` read-modify-write ignores its memory order

`__atomic_fetch_add` and its eleven siblings lower through `emit_atomic_rmw`
(`ir/linearize_atomic.rs`), which an `_Atomic` compound assignment also uses
and which is sequentially consistent by C17 6.5.16.2p3. The `order` argument
is evaluated and then dropped, so `__atomic_fetch_add(p, v, __ATOMIC_RELAXED)`
gets a seq-cst operation -- never wrong, and slower than asked for. The load,
store, exchange and compare-exchange forms carry their order into the
instruction.

Done is `emit_atomic_rmw` and its CAS loop (`emit_atomic_cas_loop`) taking an
order, the `_Atomic`-operator callers passing seq-cst, and the aarch64 LL/SC
loop picking its acquire/release variants from it.

---

## Conformance

### `restrict` on a non-pointer is accepted

C17 6.7.3p2 is a constraint: `restrict` may only qualify a pointer to object
type. c17 accepts `restrict int x;` silently, at file and block scope; gcc
rejects it. Done is an error at the declaration.

---

## Optimization Passes

What exists, and why the pass order is what it is, is in
[ir/README.md](ir/README.md). This section is only what is not built.

### Fold loads from `const` aggregates

`ir/constglobal.rs` folds a load of a `const` scalar global into its
initializer, but skips every aggregate initializer, so
`static const int a[3] = {7,8,9}; ... a[1]` and a member of a `const` struct
are still loaded at `-O2`. Done is the load's offset and width matched against
the element or member list, under the same refusals the scalar case applies.

### A float lattice for `sccp`

`ir/sccp.rs` tracks only integer constants (`Val::Const(i128)`), so a float
constant that is constant only along the reachable paths is not propagated;
only `instcombine`, which sees one instruction at a time, folds float
constants. Done is float values in the lattice, folded under the same rules
`constfold` applies (nothing that raises a floating-point exception).

### GVN and Global Code Motion, as one project

Global value numbering finds congruent computations across blocks, and global
code motion (Click's, as QBE's `gcm.c` does it) places each where it belongs:
the two are one project, since a congruence GVN finds has no control
dependence until GCM gives it one. GCM needs the dominator tree
(`ir/dominate.rs`) and a loop tree, which c17 does not have, and subsumes
loop-invariant code motion, code sinking and partial dead-code elimination --
with none of the loop canonicalization (preheaders, a single back edge, LCSSA)
a separate LICM would need. Two things to get right: pin what may trap or
touch memory -- loads, `alloca`, `div`/`rem` -- so GVN does not merge what GCM
will not move, and re-verify that no use precedes its definition within a
block afterward.

---

## Assembly Peephole Optimizations

Both patterns appear in bulk in `-O2` output on both targets:

| Pattern | Done is |
|---|---|
| A register-to-register `mov` whose source dies at it | The allocator coalesces the two, or a post-codegen pass deletes the move and renames |
| `jmp L` (aarch64 `b L`) immediately followed by `L:` | The jump is not emitted |
