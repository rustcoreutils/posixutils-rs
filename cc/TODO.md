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

## Technical Debt

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

---

## Future Features

### 64-bit stack frames

An automatic object past `TypeTable::MAX_STACK_OBJECT_BYTES` (just under
2 GiB) is refused with a diagnostic, and so is a frame whose total passes it,
because a frame displacement is an `i32`. gcc compiles both: x86-64 reaches the
frame through `movabsq`-materialised displacements, and aarch64 through
`movz`/`movk` into a scratch register. The diagnostic exists so that c17 never
emits a wrapped frame.

The torture tests that need it (`compile/20031023-1..4`,
`compile/stack-check-1`) fail with that diagnostic.

What it takes:

- Widen every frame quantity to `i64`: `MemAddr` displacements on both targets,
  `Loc::Stack`/`Loc::IncomingArg`, `RegAlloc::stack_offset`,
  `callee_saved_offset`, `stack_alloc_size`, `reg_save_area_offset`, the
  outgoing-argument layout, `IncomingOff`, and the CFI directive offsets.
  `crate::abi::slot_bytes` (the only place an object
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
