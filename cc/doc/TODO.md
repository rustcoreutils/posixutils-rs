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

### C11 Thread-Local Storage — ARM64 + Dynamic Models

Phases 1-3 (parser, symbol table, IR, x86-64 Local-Exec codegen) are done.

**Remaining:**
- ARM64 Local-Exec codegen (`mrs tpidr_el0` + `tprel` relocations)
- Initial-Exec model (for preemptible symbols)
- General-Dynamic model (for shared libraries, `__tls_get_addr`)

---

## Optimization Passes

The compiler uses SSA-form IR. Passes run iteratively until fixed point.

### Pass 1: SCCP — Sparse Conditional Constant Propagation

Propagate constants through CFG along reachable paths only. Lattice: `{UNDEF, CONST(c), UNKNOWN}`.

### Pass 2: CFG Simplification

Convert constant branches to unconditional jumps. Merge simple blocks. Remove jumps-to-jumps.

### Pass 3: Copy Propagation & SSA Cleanup

`t1 = x; y = t1;` → `y = x`. Simplify φ-nodes where all incoming operands are same.

### Pass 4: Local CSE / Value Numbering

Inside a block, deduplicate `t1 = a + b; t2 = a + b;` → `t2 = t1`.

### Pass 5: GVN — Global Value Numbering

Deduplicate computations across blocks using dominator-order value numbering.

### Pass 7: LICM — Loop-Invariant Code Motion

Hoist pure, loop-invariant computations out of loop bodies.

### Pass 8: Loop Canonicalization & Strength Reduction

Normalize induction variables. Replace multiplications with additions.

### Suggested Pass Pipeline

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
