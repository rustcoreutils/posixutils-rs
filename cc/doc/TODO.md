# pcc TODO

## Table of Contents

- [Technical Debt](#technical-debt)
- [Future Features](#future-features)
- [Optimization Passes](#optimization-passes)
- [Assembly Peephole Optimizations](#assembly-peephole-optimizations)

## Technical Debt

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

### C11 `_Generic` Type-Generic Selection

```c
#define cbrt(x) _Generic((x), float: cbrtf, long double: cbrtl, default: cbrt)(x)
```

**Status:** Not started
**Complexity:** Moderate

### C11 Atomics — Semantic Validation (Phase 8)

Phases 1-7 (type system, parser, IR, linearizer, x86-64/ARM64 codegen, stdatomic.h) are done.

**Remaining:** Phase 8 semantic validation:
- Reject `&(atomic_struct.member)`
- Validate `sizeof` on atomic type returns atomic size

### C11 Thread-Local Storage — Dynamic Model

Done: parser, symbol table, IR, x86-64 Local-Exec (`%fs:sym@TPOFF`) and Initial-Exec (`sym@GOTTPOFF`), AArch64 Local-Exec (`mrs tpidr_el0` + `tprel_hi12`/`tprel_lo12_nc`) and Initial-Exec (`gottpoff` / `gottpoff_lo12`).

**Remaining:**
- General-Dynamic model (`_Thread_local` in shared libraries; `__tls_get_addr` on x86-64, `tlsdesc` on AArch64).

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
