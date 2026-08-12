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

### C11 Thread-Local Storage — General Dynamic

Done: parser, symbol table, IR, x86-64 Local-Exec (`%fs:sym@TPOFF`) and
Initial-Exec (`sym@GOTTPOFF`), AArch64 Local-Exec (`mrs tpidr_el0` +
`tprel_hi12`/`tprel_lo12_nc`) and Initial-Exec (`gottpoff` /
`gottpoff_lo12`), and model selection: `-shared` and `-fPIC` take
Initial-Exec, while `-fPIE` and a plain executable keep Local-Exec.

**Remaining: the General Dynamic model** — `@TLSGD` plus a call to
`__tls_get_addr` on x86-64, TLSDESC on AArch64, of which there is currently
none. gcc uses it for `-fPIC`, where c17 uses Initial-Exec. Initial-Exec is a
legitimate position-independent model and is correct for a shared object
loaded at startup, but it requires the object's thread-locals to fit in the
loader's static TLS surplus, so a large block fails at `dlopen`:

```
$ c17 -fPIC --shared -o lib.so libbig.c   # _Thread_local int big[600000];
dlopen failed: ./lib.so: cannot allocate memory in static TLS block
```

gcc loads the same source. The failure is size-dependent — a small block fits
in the surplus and works — so a test for this has to exceed it deliberately.

**The hard part is the register allocator, not the relocations.** A
synthesized call to `__tls_get_addr` must be declared call-like or live
pseudos get allocated into the registers it clobbers. `is_call_like_x86_64`
(`cc/arch/x86_64/regalloc.rs`) keys on `Opcode`, but General Dynamic would
make `Load` / `Store` / `SymAddr` call-like *only when the operand is a TLS
symbol* — an operand-granularity condition that list cannot express. Settle
that before writing any emission code.

New relocation spellings belong in the two typed places — the
`MemAddr::format` match (`cc/arch/x86_64/lir.rs`) and the AArch64 instruction
`emit` (`cc/arch/aarch64/lir.rs`) — not `Directive::Raw`. The
synthesized-call idiom to copy is `emit_memcpy`
(`cc/arch/x86_64/features.rs`).

**Four latent Local-Exec sites** remain, none reachable from C source today:
`loc_to_gp_operand`, the two inline-asm operand paths, and `loc_to_asm_string`
(which formats a global as plain `name(%rip)` with no TLS handling at all).
They emit a single memory operand, and Initial-Exec needs two instructions, so
they cannot simply call `use_tls_ie` — closing them means giving those paths a
way to emit a sequence.

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
