# c17 TODO

## Table of Contents

- [Technical Debt](#technical-debt)
- [Known Divergences](#known-divergences)
- [Future Features](#future-features)
- [Optimization Passes](#optimization-passes)
- [Assembly Peephole Optimizations](#assembly-peephole-optimizations)
- [External Test Suites](#external-test-suites)

## Technical Debt

### Decimal floating literals are rounded through `f64`

**Location**: `parse_number_literal` in `cc/parse/expression.rs`

**Issue**: A literal is now carried as `FloatVal` (`cc/float.rs`), which holds
the x87 80-bit encoding and is exact for `long double` on both targets, so
`LDBL_MAX`, `LDBL_MIN` and every *hex* literal survive. A **decimal** literal
still goes through `f64::from_str`, so one outside double's range or needing
more than 53 mantissa bits is rounded: `3.14159265358979323846L` differs from
gcc's bits from the 53rd onward, and `1.18973149535723176502e+4932L` written
out longhand is still `inf`.

**What it needs**: a correctly-rounded decimal-to-64-bit-significand
conversion. `10^4932` is ~16,400 bits, so it takes a big-integer scale and
round with sticky-bit tracking; Rust's std rounds only to 53 bits and the
minimal-dependency rule rules out a crate. The result feeds straight into
`FloatVal::from_parts`, which already rounds an exact `(mantissa, exp2)` pair
to the target width -- nothing else has to change.

### `_FORTIFY_SOURCE` compiles but checks nothing

Peeling this apart one layer at a time has been the only way to see it: each
fix exposes the next blocker, and three of the layers are invisible until the
one before it is in place. Five are done:

1. `__builtin_object_size` computing real sizes rather than "unknown".
2. Implicit declarations for the `__builtin___*_chk` family.
3. Asm label renaming, so glibc's `__REDIRECT` aliases resolve.
4. `always_inline`, so the fortified wrapper reaches the caller at all.
5. Inline definitions emitting no external definition.

~~**Layer 5 — `__gnu_inline__` / `extern inline` must emit no out-of-line
definition.**~~ Done. It turned out not to be a fortify problem at all: `inline`
was never recorded on a function definition in the first place, so *every*
spelling emitted an external definition and an `inline` function in a shared
header failed to link.

**Layer 6 — `__builtin_object_size` has to be folded after inlining.** The
wrapper computes the size of its own `__dest` *parameter*, which is genuinely
unknown, so the front end folds it to `-1` before the inliner ever runs and the
`_chk` call is handed `-1` — a value that means "do not check". gcc defers the
fold until after inlining, when `__dest` is known to be the caller's `buf`.
Fixing this means carrying the query into the IR and folding it in a
post-inline pass that can trace a pointer back to its object.

Until layer 6 lands, predefining `__OPTIMIZE__` (which is what makes glibc
compile the wrappers at all) is a regression rather than a fix: it has been
implemented and reverted three times, most recently after measuring the
duplicate-symbol failure that turned out to be layer 5. `__OPTIMIZE__` and
layer 6 must land together.

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
| `_FORTIFY_SOURCE` | Still checks nothing. Five of six layers are done; the one that remains is described above, and is an ordinary compiler feature rather than fortify-specific work |
| `isnan()` on a `long double` | 65535 rather than 1. The builtins now exist; glibc only uses them at `__GNUC_PREREQ (4,4)`, and claiming that also demands `__float128` (`bits/floatn.h` turns on `__HAVE_FLOAT128` at 4.3), which x86-64 c17 has no arithmetic for. Both answers conform — C99 7.12.3.4 requires only a nonzero value |

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
