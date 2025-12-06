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
