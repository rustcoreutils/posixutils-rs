# c17 TODO

## Table of Contents

- [Technical Debt](#technical-debt)
- [Known Divergences](#known-divergences)
- [Future Features](#future-features)
- [Optimization Passes](#optimization-passes)
- [Assembly Peephole Optimizations](#assembly-peephole-optimizations)
- [External Test Suites](#external-test-suites)

## Technical Debt

### Floating literals are stored as `f64`

**Location**: `ExprKind::FloatLit(f64)` in `cc/parse/ast.rs`

**Issue**: Every floating literal is parsed into an `f64`, so any value outside
double's range is lost before the type system ever sees it. `LDBL_MAX`
evaluates to `inf` on x86-64, where `long double` is x87 80-bit and holds it
comfortably; gcc prints `1.18973e+4932`. `LDBL_MIN` underflows to `0` the same
way, and a literal needing 54-64 mantissa bits is silently rounded:
`3.14159265358979323846L` differs from gcc's bits from the 53rd onward.

**Groundwork done**: `parse_hex_float_parts` (`cc/parse/expression.rs`) already
returns the exact `(significand, exp2)` decomposition, so hex literals need no
further parsing work -- only a way to carry the result past `ExprKind::FloatLit`,
`Initializer::Float` and `Loc::FImm`, all of which hold an `f64`. Of the 47
`FImm` sites across the two backends, ~12 bind the value; the rest pass it
through. A decimal literal outside double's range additionally needs a
decimal-to-binary conversion at width, which is the larger part.

**Consequence**: `long double` constants are silently limited to 53 bits of
mantissa and double's exponent range. `c17_long_double_round_trip` passes only
because `inf` compares equal to itself.

**Cost of fixing**: touches the AST, the constant folder, and both backends'
immediate paths (`Loc::FImm` is `(f64, u32)`).

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

| Area | Divergence |
|------|-----------|
| `_FORTIFY_SOURCE` | Compiles, but `__builtin_object_size` always answers "unknown", so nothing is actually checked |
| `_Complex` with static storage | Cannot be initialized at all; gcc accepts `1.0 + 2.0*I` and `CMPLX(...)` |
| `isnan()` on a `long double` | 65535 rather than 1, because `__builtin_isnan` is absent and glibc falls back to `__isnanl` |

## Future Features

### C11 Atomics — remaining semantic validation

The type system, parser, IR, linearizer, both code generators, `<stdatomic.h>`,
and access through ordinary operators are all done. `_Atomic` on an array or
function type is rejected.

**Remaining:**
- Warn on member access of an atomic struct or union, as gcc does
  ("accessing a member of an atomic structure"). C11 6.5.2.3p5 makes it
  undefined behaviour rather than a constraint violation, so this is a
  warning, not the rejection an earlier version of this list called for.

Two entries that used to sit here were removed after being probed rather than
implemented. Rejecting `_Atomic` on a struct or union with a VLA member is
unreachable: such a type cannot be formed at all, since a VLA member is
rejected outright and the typedef route is rejected at the typedef. And
`int *_Atomic p;` now parses -- it was a qualifier-list bug at file scope, not
a missing feature.

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

**Blocked.** An attempt at this surfaced a register-allocator defect that is
still open: a non-variadic `Arg` pseudo's location is computed off the wrong
frame base (spill slot versus callee frame). Any pass that merges pseudos
trips it, so copy propagation cannot land until that is fixed.

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
