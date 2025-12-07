# pcc TODO

## Technical Debt

### R10 reserved globally for division scratch

**Location**: `arch/x86_64/codegen.rs` lines 160-179

**Issue**: R10 is permanently excluded from the allocatable register pool because x86-64 `div`/`idiv` instructions clobber RAX (quotient) and RDX (remainder). When the divisor operand happens to be in RAX or RDX, we need a scratch register to hold it during the division.

**Current approach**: Reserve R10 globally so it's always available as scratch for division.

**Cost**: All generated code loses one general-purpose register, even functions that never use division/modulo operations.

**Better solutions**:

1. **Per-function reservation**: Only exclude R10 from allocation in functions that contain `DivS`, `DivU`, `ModS`, or `ModU` instructions. Scan the IR before register allocation.

2. **Instruction-level constraints**: Integrate scratch register requirements into the register allocator. Mark division instructions as needing a scratch and ensure one is available only when needed.

3. **Pre-coloring**: During register allocation, constrain the divisor operand to never be assigned RAX or RDX, eliminating the need for a scratch register.

4. **Spill to stack**: When divisor is in RAX/RDX, use a temporary stack slot instead of a dedicated scratch register.

Production compilers (GCC, LLVM) use approaches 2-4. The current approach is simple and correct but sacrifices some optimization potential.

### XMM register save area for variadic functions (x86-64)

**Location**: `arch/x86_64/codegen.rs` lines 1012-1015

**Issue**: On x86-64 SysV ABI, variadic functions that receive floating-point arguments via XMM0-XMM7 should save those registers in the register save area during the prologue. This allows `va_arg(ap, double)` to retrieve FP arguments passed in registers.

**Current state**: The compiler correctly:
- Passes FP arguments to variadic functions (caller side) in XMM registers
- Sets AL to the count of XMM registers used for variadic calls
- Saves GP registers (RDI, RSI, RDX, RCX, R8, R9) in the register save area

However, XMM registers are not saved, so user-defined variadic functions cannot use `va_arg` to retrieve `double` arguments.

**Impact**: Low. Most POSIX variadic functions (printf, sprintf, scanf, etc.) are provided by libc and work correctly. This only affects user-defined variadic functions that accept floating-point variadic arguments.

**Solution**: In the function prologue for variadic functions:
1. Allocate additional space in the register save area (8 XMM regs × 16 bytes = 128 bytes)
2. Use `movaps` to save XMM0-XMM7 at their ABI-specified offsets
3. Update `va_start` to initialize the FP register save area pointer correctly
4. Update `va_arg` to read from XMM save area when extracting FP types

---

## Future Features

### C11 Atomics (`_Atomic`) Implementation

#### Overview

Add C11 `_Atomic` type qualifier/specifier support to achieve full C11 compliance.

#### Research Summary

**C11 `_Atomic` has two syntactic forms:**
1. **Type specifier**: `_Atomic(type-name)` - creates a new atomic type
2. **Type qualifier**: `_Atomic type-name` - can combine with `const`, `volatile`, `restrict`

**Key semantics:**
- Atomic types may have different size/alignment than non-atomic counterparts
- Normal assignment and compound assignment are sequentially consistent
- Increment/decrement are atomic read-modify-write operations
- Cannot take address of atomic struct member (UB)
- Arrays and functions cannot be made atomic

**Target instructions:**
- **x86-64**: `LOCK` prefix + `ADD`, `XADD`, `CMPXCHG`, etc. (`XCHG` has implicit lock)
- **ARM64**: `LDXR`/`STXR` (load/store exclusive) or LSE `LDADD`, `STADD`, etc.

#### Implementation Phases

**Phase 1: Type System (`cc/types.rs`)**

1. Add `ATOMIC` modifier to `TypeModifiers`: `const ATOMIC = 1 << 14;`
2. Add helper methods: `is_atomic()`, `atomic_of()`, `strip_atomic()`
3. Size/alignment: atomic types can have same size as non-atomic (x86-64 compatible)

**Phase 2: Parser (`cc/parse/parser.rs`)**

1. Recognize `_Atomic` keyword in tokenizer/lexer
2. Parse type specifier form: `_Atomic(type-name)`
3. Parse type qualifier form: `_Atomic` as qualifier
4. Validation: reject on array/function types, reject double-atomic

**Phase 3: IR Extensions (`cc/ir.rs`)**

Add new atomic opcodes:
- `AtomicLoad`, `AtomicStore` - with memory ordering
- `AtomicRmwAdd`, `AtomicRmwSub`, `AtomicRmwAnd`, `AtomicRmwOr`, `AtomicRmwXor`
- `AtomicRmwXchg`, `AtomicCmpXchg`, `AtomicFence`

Add `MemoryOrder` enum: `Relaxed`, `Consume`, `Acquire`, `Release`, `AcqRel`, `SeqCst`

**Phase 4: Linearizer (`cc/linearize.rs`)**

1. Detect atomic type in load/store → emit `AtomicLoad`/`AtomicStore`
2. Handle compound assignment: `atomic_var += 1` → `AtomicRmwAdd`
3. Handle increment/decrement: `++atomic_var` → `AtomicRmwAdd` with value 1

**Phase 5: Code Generation (`cc/arch/*/codegen.rs`)**

x86-64:
- `AtomicLoad`: simple MOV (x86 loads are atomic if aligned)
- `AtomicStore (SeqCst)`: `xchg [addr], reg` or `mov + mfence`
- `AtomicRmwAdd`: `lock xadd [addr], reg`
- `AtomicCmpXchg`: `lock cmpxchg [addr], reg`

ARM64:
- `AtomicLoad (Acquire)`: `ldar`
- `AtomicStore (Release)`: `stlr`
- `AtomicRmwAdd (LSE)`: `ldaddal`
- `AtomicRmwAdd (LL/SC)`: `ldaxr`/`stlxr` loop

**Phase 6: `<stdatomic.h>` Support**

- Type aliases: `atomic_bool`, `atomic_int`, `atomic_uint`, etc.
- Core functions: `atomic_load()`, `atomic_store()`, `atomic_exchange()`, etc.
- Fence operations: `atomic_thread_fence()`, `atomic_signal_fence()`

**Phase 7: Semantic Validation**

- Reject `&(atomic_struct.member)`
- `sizeof` on atomic type returns atomic size

#### Implementation Order

| Order | Phase | Complexity | Dependencies |
|-------|-------|------------|--------------|
| 1 | Type system (`ATOMIC` modifier) | Low | None |
| 2 | Parser (`_Atomic` keyword) | Medium | Phase 1 |
| 3 | IR opcodes | Medium | None |
| 4 | Linearizer (atomic load/store) | Medium | Phases 1-3 |
| 5 | x86-64 codegen | High | Phases 1-4 |
| 6 | ARM64 codegen | High | Phases 1-4 |
| 7 | stdatomic.h builtins | Medium | Phases 1-5 |
| 8 | Semantic validation | Low | Phases 1-2 |

#### Test Cases Needed

1. Parser tests: `_Atomic(int)`, `_Atomic int`, combined qualifiers
2. Basic atomic ops: load, store, assignment
3. Compound assignment: `+=`, `-=`, `&=`, `|=`, `^=`
4. Increment/decrement: `++`, `--` (pre and post)
5. Struct with atomic members
6. Pointer to atomic
7. stdatomic.h functions (when implemented)

#### References

- [Atomic types - cppreference.com](https://en.cppreference.com/w/c/language/atomic)
- [stdatomic.h - cppreference.com](https://en.cppreference.com/w/c/header/stdatomic)
- [C/C++11 mappings to processors](https://www.cl.cam.ac.uk/~pes20/cpp/cpp0xmappings.html)
- [LOCK prefix - x86 reference](https://www.felixcloutier.com/x86/lock)
- [AArch64 atomic access - Microsoft](https://devblogs.microsoft.com/oldnewthing/20220811-00/?p=106963)

---

### C99 Complex Numbers (`_Complex`)

#### Overview

Add C99 complex number support. The `<complex.h>` header and library functions (`cabs`, `csqrt`, etc.) are provided by the system - the compiler only needs to handle the type and arithmetic.

#### Types

| Type | Size | Layout |
|------|------|--------|
| `float _Complex` | 8 bytes | `{float real, float imag}` |
| `double _Complex` | 16 bytes | `{double real, double imag}` |
| `long double _Complex` | 32 bytes | `{long double real, long double imag}` |

#### Syntax

```c
#include <complex.h>

double _Complex z1 = 1.0 + 2.0*I;   // _Complex keyword
float _Complex z2 = 3.0f - 4.0f*I;  // float complex
```

**Constraints:**
- `_Complex` must combine with `float`, `double`, or `long double`
- Cannot combine with integer types (`int _Complex` is invalid)
- `complex` is a macro from `<complex.h>` expanding to `_Complex`

#### Implementation Phases

**Phase 1: Type System (`cc/types.rs`)**

1. Add `COMPLEX` modifier to `TypeModifiers`
2. Add three complex type variants or track via modifier
3. Size: 2× the base float type; Alignment: same as base type

**Phase 2: Parser (`cc/parse/parser.rs`)**

1. Add `_Complex` keyword to lexer
2. Parse as type specifier (like `long`, `unsigned`)
3. Validate: only with `float`/`double`/`long double`

**Phase 3: Linearizer - Arithmetic Expansion (`cc/linearize.rs`)**

Complex arithmetic must be expanded to real operations:

```c
// Addition: c = a + b
c.real = a.real + b.real;
c.imag = a.imag + b.imag;

// Subtraction: c = a - b
c.real = a.real - b.real;
c.imag = a.imag - b.imag;

// Multiplication: c = a * b
c.real = a.real*b.real - a.imag*b.imag;
c.imag = a.real*b.imag + a.imag*b.real;

// Division: c = a / b (simplified, production needs overflow handling)
denom = b.real*b.real + b.imag*b.imag;
c.real = (a.real*b.real + a.imag*b.imag) / denom;
c.imag = (a.imag*b.real - a.real*b.imag) / denom;
```

**Phase 4: Code Generation - ABI**

x86-64 SysV:
- `float _Complex`: returned in xmm0 (both parts packed)
- `double _Complex`: returned in xmm0 (real) + xmm1 (imag)
- Arguments: same pattern, use two SSE registers

ARM64:
- Treated as struct of two floats/doubles
- Passed/returned in two FP registers (d0+d1 or s0+s1)

**Phase 5: Member Access (Optional GCC Extensions)**

```c
double _Complex z = 1.0 + 2.0*I;
double r = __real__ z;  // 1.0
double i = __imag__ z;  // 2.0
__real__ z = 3.0;       // Can assign to parts
```

Alternative: users can call `creal(z)` and `cimag(z)` which are library functions.

#### Implementation Order

| Order | Component | Complexity |
|-------|-----------|------------|
| 1 | Type system (`COMPLEX` modifier) | Low |
| 2 | Parser (`_Complex` keyword) | Low |
| 3 | Basic load/store of complex values | Medium |
| 4 | Addition/subtraction expansion | Medium |
| 5 | Multiplication expansion | Medium |
| 6 | Division expansion | Medium-High |
| 7 | ABI for function calls | Medium |
| 8 | `__real__`/`__imag__` extensions | Low (optional) |

#### What the Compiler Does NOT Need

- Library functions (`cabs`, `csqrt`, `cexp`, etc.) - just normal calls
- The `I` macro - defined in `<complex.h>` as `_Complex_I`
- The `complex` macro - defined in `<complex.h>` as `_Complex`

#### References

- [C99 Complex arithmetic - cppreference](https://en.cppreference.com/w/c/numeric/complex)
- [complex.h - cppreference](https://en.cppreference.com/w/c/header/complex)

---

### C11 Alignment Specifiers (`_Alignas`, `_Alignof`)

#### Overview

Add C11 alignment control to specify/query memory alignment of objects.

#### Syntax

```c
_Alignas(16) float sse_data[4];     // Align to 16-byte boundary
_Alignas(int) char c;               // Align char like int
size_t align = _Alignof(double);    // Query alignment (compile-time constant)
```

**Two forms:**
1. `_Alignas(constant-expression)` - explicit byte alignment (must be power of 2)
2. `_Alignas(type-name)` - align like another type

#### Restrictions

- Cannot apply to: bit-fields, `register` variables, function parameters, typedefs
- Cannot reduce alignment below natural alignment (only increase)
- When multiple `_Alignas` specifiers appear, the strictest (largest) wins
- `_Alignof` cannot be applied to function types or incomplete types

#### Implementation Phases

**Phase 1: Type System (`cc/types.rs`)**

1. Add `alignment: Option<u32>` field to relevant type structures
2. Add helpers: `alignment_of(type)`, `set_explicit_alignment()`
3. Track whether alignment is natural vs explicitly specified

**Phase 2: Parser (`cc/parse/parser.rs`)**

1. Add `_Alignas` and `_Alignof` keywords to lexer
2. Parse `_Alignas(expr)` and `_Alignas(type-name)` as declaration specifiers
3. Parse `_Alignof(type-name)` as unary expression (returns `size_t`)
4. Semantic checks: power of 2, not less than natural alignment

**Phase 3: IR Extensions (`cc/ir.rs`)**

1. Add alignment field to `Alloca` instruction for local variables
2. Add alignment attribute to global variable definitions

**Phase 4: Code Generation**

For globals:
```asm
    .balign 16
symbol:
    .zero 64
```

For locals:
- Adjust stack pointer with additional padding
- Use aligned stack slots

**Phase 5: Struct Layout**

- When computing struct layout, member `_Alignas` affects padding
- Struct alignment = max of all member alignments

#### Implementation Order

| Order | Component | Complexity |
|-------|-----------|------------|
| 1 | `_Alignof` operator | Easy - compile-time type query |
| 2 | `_Alignas` for globals | Easy - `.balign` directives |
| 3 | `_Alignas` for struct members | Moderate - layout changes |
| 4 | `_Alignas` for locals | Moderate - stack frame adjustments |

#### References

- [_Alignas - cppreference.com](https://en.cppreference.com/w/c/language/_Alignas.html)
- [Alignment (C11) - Microsoft Learn](https://learn.microsoft.com/en-us/cpp/c-language/alignment-c)

---

### C11 Thread-Local Storage (`_Thread_local`)

#### Overview

Add thread-local storage class specifier for per-thread variable instances.

#### Syntax

```c
_Thread_local int errno;              // Each thread gets its own copy
static _Thread_local int counter;     // Thread-local + file scope
extern _Thread_local int shared_tls;  // Declaration of TLS from another TU
```

**Constraints:**
- Can combine with `static` or `extern`, but not `auto` or `register`
- Cannot be used on function parameters or local block-scope variables (unless `static`)
- GCC extension `__thread` is equivalent

#### TLS Memory Models

| Model | Use Case | Mechanism |
|-------|----------|-----------|
| Local-Exec | Non-preemptible in executables | TP offset is link-time constant |
| Initial-Exec | Preemptible at program start | GOT entry holds fixed TP offset |
| Local-Dynamic | Non-preemptible in shared libs | Module ID lookup, local offsets |
| General-Dynamic | Preemptible in shared libs | Full runtime lookup via `__tls_get_addr` |

For a simple compiler targeting executables, **Local-Exec** is sufficient and simplest.

#### Implementation Phases

**Phase 1: Lexer/Parser**

1. Add `_Thread_local` keyword (and `__thread` as extension)
2. Parse as storage class specifier
3. Validate: not with `auto`/`register`, not on block-scope non-static

**Phase 2: Symbol Table (`cc/symbol.rs`)**

1. Add `is_thread_local: bool` to `Symbol`
2. Track TLS storage class during declaration

**Phase 3: IR (`cc/ir.rs`)**

1. Mark global variables as TLS in IR representation
2. Add `is_tls` flag to global definitions

**Phase 4: Code Generation - x86-64**

Local-Exec model (simplest, for executables):
```asm
# Read TLS variable
movl %fs:symbol@TPOFF, %eax

# Write TLS variable
movl $42, %fs:symbol@TPOFF

# Get address of TLS variable
movq %fs:0, %rax
leaq symbol@TPOFF(%rax), %rax
```

Relocations needed:
- `R_X86_64_TPOFF32` - 32-bit signed TP offset

Section placement:
- Initialized: `.tdata` section
- Zero-initialized: `.tbss` section

**Phase 5: Code Generation - ARM64**

Local-Exec model:
```asm
# Read TLS variable (default -mtls-size=24)
mrs  x0, tpidr_el0
add  x0, x0, #:tprel_hi12:symbol
add  x0, x0, #:tprel_lo12_nc:symbol
ldr  w0, [x0]
```

Relocations needed:
- `R_AARCH64_TLSLE_ADD_TPREL_HI12`
- `R_AARCH64_TLSLE_ADD_TPREL_LO12_NC`

#### Implementation Order

| Order | Component | Complexity |
|-------|-----------|------------|
| 1 | Parser + symbol tracking | Low |
| 2 | IR representation | Low |
| 3 | x86-64 Local-Exec codegen | Medium |
| 4 | ARM64 Local-Exec codegen | Medium |
| 5 | Initial-Exec model (optional) | High |
| 6 | General-Dynamic (for DSOs) | High |

#### Complexity Assessment

TLS is **significantly more complex** than alignment:
- Requires special relocations the assembler/linker must handle
- Platform-specific thread pointer access (`%fs` on x86-64, `tpidr_el0` on ARM64)
- Multiple models depending on PIC/PIE/shared library context
- Runtime support for dynamic models (`__tls_get_addr`)

**Recommendation:** Start with Local-Exec model only, which works for simple executables and avoids runtime dependencies.

#### References

- [Thread-local storage - Wikipedia](https://en.wikipedia.org/wiki/Thread-local_storage)
- [All about thread-local storage - MaskRay](https://maskray.me/blog/2021-02-14-all-about-thread-local-storage)
- [GCC Thread-Local Documentation](https://gcc.gnu.org/onlinedocs/gcc/Thread-Local.html)
- [thread_local - cppreference.com](https://en.cppreference.com/w/c/thread/thread_local)

---

### Other C11 Features

| Feature | Description | Complexity |
|---------|-------------|------------|
| `_Static_assert(expr, msg)` | Compile-time assertion | Easy |
| `_Generic(expr, type: val, ...)` | Type-generic selection | Moderate |
| `_Noreturn` | Function attribute (never returns) | Easy |
| Anonymous structs/unions | `struct { struct { int x; }; };` | Moderate |
| `<stdalign.h>` | `alignas`/`alignof` macros | Preprocessor only |

#### `_Static_assert` Implementation

```c
_Static_assert(sizeof(int) == 4, "int must be 32 bits");
```

1. Parser: recognize keyword, parse `(constant-expr, string-literal)`
2. Semantic: evaluate expression at compile time
3. If false: emit error with the string message
4. No codegen needed - purely compile-time

#### `_Noreturn` Implementation

```c
_Noreturn void exit(int status);
```

1. Parser: recognize as function specifier
2. Type system: mark function type as noreturn
3. Semantic: warn if function can return
4. Codegen: can omit function epilogue, enable optimizations

---

## Optimization Passes

### Overview

The compiler uses a sparse-style SSA IR, which is well-suited for classical optimizations. Passes are run iteratively until a fixed point is reached.

### Pass 1: SCCP - Sparse Conditional Constant Propagation

**Status:** Not implemented

**What it does:**
- Propagate constants through the CFG, only along reachable paths
- If a branch's condition is constant, mark only that successor reachable
- Lattice: `{UNDEF, CONST(c), UNKNOWN}`
- Forward dataflow across the CFG updating φs

**Simpler alternative:** Global constant propagation without conditional pruning.

### Pass 2: CFG Simplification

**Status:** Not implemented

**What it does:**
- Convert constant-condition branches into unconditional jumps
- Merge simple blocks: A → B where A's only successor is B and B's only predecessor is A
- Remove jumps to jumps (fallthrough simplification)

**Implementation:**
- After DCE, walk the CFG
- If branch/switch has constant condition → replace with direct jump
- If block just jumps, no φ, no side effects → inline successor or merge

### Pass 3: Copy Propagation & SSA Cleanup

**Status:** Not implemented

**What it does:**
- `t1 = x; y = t1;` → `y = x`
- φ simplifications: `t = φ(x, x, x)` → `t = x`

**Implementation:**
- Use SSA def-use chains
- For each `t = x` where x is SSA value and t isn't address-taken, replace uses of t with x
- For φ-nodes where all incoming operands are same → replace uses and remove φ
- Run DCE afterwards

### Pass 4: Local CSE / Value Numbering

**Status:** Not implemented

**What it does:**
- Inside a block, recognize when re-computing same pure expression
- `t1 = a + b; t2 = a + b;` → `t2 = t1`

**Implementation:**
- For each block: maintain map `(opcode, operand1, operand2, type) → SSA value`
- When expression already in map, reuse prior value
- Limit to pure operations: arithmetic, logical ops, comparisons

### Pass 5: GVN - Global Value Numbering

**Status:** Not implemented (optional, bigger investment)

**What it does:**
- Deduplicate computations across blocks, not just inside one
- More global version of CSE respecting control flow and dominance

**Implementation:**
- Walk in dominator order
- Assign "value numbers" to expressions
- Equivalent expressions sharing same number are merged

### Pass 6: Conservative Function Inlining

**Status:** Not implemented

**What it does:**
- Inline small, non-recursive, non-varargs functions into callers
- Exposes new optimization opportunities

**Constraints:**
- Only inline static/internal functions
- Limit by IR size: e.g. ≤ N instructions, ≤ M basic blocks
- Reject varargs, VLAs, alloca, weird control flow

**After inlining:** Re-run InstCombine → SCCP → DCE

### Pass 7: LICM - Loop-Invariant Code Motion

**Status:** Not implemented

**What it does:**
- Hoist computations that are pure and loop-invariant

**Implementation:**
- Detect natural loops via back edges and dominators
- Only hoist arithmetic/logical ops whose operands are defined outside loop
- Don't hoist loads/stores without alias analysis

### Pass 8: Loop Canonicalization & Strength Reduction

**Status:** Not implemented (low priority)

**What it does:**
- Normalize induction variables: `for (i = 0; i < n; ++i)` style
- Replace multiplications with additions

**Implementation:**
- Identify induction variables: φ node in loop header with one incoming from preheader, one from latch
- Handle simple patterns: `i = φ(i0, i + c)`
- Turn `base + i * c` patterns into increments

### Suggested Pass Pipeline

```
IR generation + SSA construction
    ↓
Early InstCombine (constant folding + algebraic)
    ↓
SCCP (or simple global const prop)
    ↓
DCE + unreachable block removal
    ↓
CFG simplification
    ↓
Copy propagation + φ simplification → DCE
    ↓
Local CSE → InstCombine
    ↓
[Later] GVN → DCE/CFG simplify
    ↓
[Later] Conservative inlining → re-run passes 2-7
    ↓
[Later] LICM + loop opts → final InstCombine + DCE
```

### Implementation Priority

| Priority | Pass | Complexity | Impact |
|----------|------|------------|--------|
| 1 | CFG simplify | Low | Medium |
| 2 | Copy/φ cleanup | Low | Medium |
| 3 | Local CSE | Medium | Medium |
| 4 | SCCP | Medium | High |
| 5 | GVN | High | Medium |
| 6 | Inlining | High | High |
| 7 | LICM | Medium | Medium |
| 8 | Loop opts | High | Low |

---

## Assembly Peephole Optimizations

### Overview

Post-codegen peephole optimizations on the generated assembly. These are low-complexity, high-impact micro-optimizations.

### Potential Optimizations

| Pattern | Optimization |
|---------|--------------|
| `mov %rax, %rax` | Delete (no-op move) |
| `mov %rax, %rbx; mov %rbx, %rax` | Delete second (useless copy-back) |
| `add $0, %rax` | Delete (no-op add) |
| `sub $0, %rax` | Delete (no-op sub) |
| `imul $1, %rax, %rax` | Delete (multiply by 1) |
| `xor %rax, %rax; mov $0, %rax` | Keep only xor (shorter encoding) |
| `cmp $0, %rax; je L` | `test %rax, %rax; je L` (shorter) |
| `mov $imm, %rax; add %rax, %rbx` | `add $imm, %rbx` if imm fits |

### Implementation Approach

1. Parse LIR instructions before emission
2. Pattern match on instruction sequences (2-3 instruction windows)
3. Replace with optimized sequences
4. Run multiple passes until no changes
