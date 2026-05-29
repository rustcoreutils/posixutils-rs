# IR Reference

SSA-form intermediate representation for pcc C99 compiler. Inspired by Linus Torvalds' sparse IR.

## Core Concepts

- **SSA form**: Each pseudo assigned exactly once; phi nodes at join points
- **Basic blocks**: Linear instruction sequences ending with a terminator
- **Pseudos**: Virtual registers (`%r0`), arguments (`%arg0`), phis (`%phi0`), constants (`$42`), symbols
- **Typed operations**: Instructions carry TypeId and bit size

## Opcodes

### Terminators (end basic blocks)

| Opcode | Description |
|--------|-------------|
| `ret` | Return from function |
| `br` | Unconditional branch to `bb_true` |
| `cbr` | Conditional branch: `src[0]` ? `bb_true` : `bb_false` |
| `switch` | Multi-way branch via `switch_cases` / `switch_default` |
| `unreachable` | UB if reached. Emitted by the linearizer after every `noreturn` call (e.g. `_exit`, `abort`); DCE only treats a block as trivially unreachable when `Unreachable` is its *first* non-trivial instruction (so a `call _exit; unreachable` block is still reachable). |
| `longjmp` | Non-local jump (never returns) |

### Integer Arithmetic (binary)

| Opcode | Description |
|--------|-------------|
| `add` | Addition |
| `sub` | Subtraction |
| `mul` | Multiplication |
| `divs` | Signed division |
| `divu` | Unsigned division |
| `mods` | Signed modulo |
| `modu` | Unsigned modulo |
| `shl` | Shift left |
| `lsr` | Logical shift right (unsigned) |
| `asr` | Arithmetic shift right (signed) |

### Floating-Point Arithmetic

| Opcode | Description |
|--------|-------------|
| `fadd` | Float add |
| `fsub` | Float subtract |
| `fmul` | Float multiply |
| `fdiv` | Float divide |

### Bitwise Operations

| Opcode | Description |
|--------|-------------|
| `and` | Bitwise AND |
| `or` | Bitwise OR |
| `xor` | Bitwise XOR |
| `not` | Bitwise NOT (unary) |
| `neg` | Integer negation (unary) |
| `fneg` | Float negation (unary) |

### Integer Comparisons (result: 0 or 1)

| Opcode | Description |
|--------|-------------|
| `seteq` | == |
| `setne` | != |
| `setlt` | < (signed) |
| `setle` | <= (signed) |
| `setgt` | > (signed) |
| `setge` | >= (signed) |
| `setb` | < (unsigned, "below") |
| `setbe` | <= (unsigned) |
| `seta` | > (unsigned, "above") |
| `setae` | >= (unsigned) |

### Floating-Point Comparisons (ordered)

| Opcode | Description |
|--------|-------------|
| `fcmp_oeq` | == (ordered) |
| `fcmp_one` | != (ordered) |
| `fcmp_olt` | < (ordered) |
| `fcmp_ole` | <= (ordered) |
| `fcmp_ogt` | > (ordered) |
| `fcmp_oge` | >= (ordered) |

### Type Conversions

| Opcode | Description |
|--------|-------------|
| `trunc` | Truncate to smaller integer |
| `zext` | Zero-extend to larger integer |
| `sext` | Sign-extend to larger integer |
| `fcvtu` | Float to unsigned int |
| `fcvts` | Float to signed int |
| `ucvtf` | Unsigned int to float |
| `scvtf` | Signed int to float |
| `fcvtf` | Float to float (size change) |

### Memory Operations

| Opcode | Description |
|--------|-------------|
| `load` | Load from address `src[0]` + `offset` |
| `store` | Store `src[0]` to address `src[1]` + `offset` |
| `symaddr` | Get address of symbol |

### SSA Operations

| Opcode | Description |
|--------|-------------|
| `phi` | SSA merge: `phi_list` = [(bb, pseudo), ...] |
| `phisrc` | Phi source: explicit defining instruction for a phi operand in the predecessor block; backpointer in `phi_list` (not a use). Lets DCE keep phi sources live without false uses. |
| `copy` | Value copy (phi elimination) |
| `setval` | Create pseudo for constant |
| `select` | Ternary: `src[0]` ? `src[1]` : `src[2]` (pure; enables `cmov`/`csel`) |

### Call Operations

| Opcode | Description |
|--------|-------------|
| `call` | Function call; `func_name` or `indirect_target` |

Call fields:
- `src`: arguments
- `arg_types`: parallel type info
- `variadic_arg_start`: where varargs begin
- `is_noreturn_call`: function never returns
- `abi_info`: rich ABI classification (see below)
- `returns_via_sret()`: derived from abi_info — large struct return via hidden pointer
- `returns_two_regs()`: derived from abi_info — 9-16 byte struct via RAX+RDX / X0+X1

### ABI Classification (`abi_info`)

Optional `CallAbiInfo` provides detailed ABI classification:

```
CallAbiInfo {
    params: Vec<ArgClass>,  // Per-argument classification
    ret: ArgClass,          // Return value classification
}
```

`ArgClass` variants:
- `Direct { classes, size_bits }` - pass in register(s)
- `Indirect { align, size_bits }` - pass by pointer (sret)
- `Hfa { base, count }` - homogeneous FP aggregate (AArch64)
- `Extend { signed, size_bits }` - extend small integer
- `Ignore` - zero-sized type

This provides per-eightbyte register class (INTEGER vs SSE) for struct fields.
Use `returns_via_sret()` and `returns_two_regs()` to query return strategy.

### Variadic Support

| Opcode | Description |
|--------|-------------|
| `va_start` | Initialize va_list |
| `va_arg` | Get next vararg |
| `va_end` | Clean up va_list |
| `va_copy` | Copy va_list |

### Bit Manipulation Builtins

| Opcode | Description |
|--------|-------------|
| `bswap16` | Byte-swap 16-bit |
| `bswap32` | Byte-swap 32-bit |
| `bswap64` | Byte-swap 64-bit |
| `ctz32` | Count trailing zeros (32-bit) |
| `ctz64` | Count trailing zeros (64-bit) |
| `clz32` | Count leading zeros (32-bit) |
| `clz64` | Count leading zeros (64-bit) |
| `popcount32` | Population count (32-bit) |
| `popcount64` | Population count (64-bit) |

### Floating-Point Builtins

| Opcode | Description |
|--------|-------------|
| `fabs32` | Absolute value (`float`) |
| `fabs64` | Absolute value (`double`) |
| `signbit32` | Test sign bit (`float`); returns int |
| `signbit64` | Test sign bit (`double`); returns int |

### Memory Builtins

These lower to libc calls (`memset` / `memcpy` / `memmove`) and are marked as side-effecting roots so DCE preserves them.

| Opcode | Description |
|--------|-------------|
| `memset` | `memset(dst, c, n)` |
| `memcpy` | `memcpy(dst, src, n)` |
| `memmove` | `memmove(dst, src, n)` (overlap-safe) |

### Stack & Non-local Jumps

| Opcode | Description |
|--------|-------------|
| `alloca` | Dynamic stack allocation |
| `setjmp` | Save context; returns 0 or longjmp value |
| `longjmp` | Restore context (never returns) |
| `frameaddress` | `__builtin_frame_address(level)` |
| `returnaddress` | `__builtin_return_address(level)` |

### C11 Atomics

All atomic ops carry a memory-order operand (relaxed/consume/acquire/release/acq-rel/seq-cst) and are side-effecting roots.

| Opcode | Description |
|--------|-------------|
| `atomicload` | Atomic load |
| `atomicstore` | Atomic store |
| `atomicswap` | Atomic exchange (returns old) |
| `atomiccas` | Compare-and-swap (returns success/old) |
| `atomicfetchadd` | Atomic fetch-and-add |
| `atomicfetchsub` | Atomic fetch-and-subtract |
| `atomicfetchand` | Atomic fetch-and-and |
| `atomicfetchor` | Atomic fetch-and-or |
| `atomicfetchxor` | Atomic fetch-and-xor |
| `fence` | Thread / signal memory fence |

### Int128 Decomposition

Emitted by the mapping pass when the target lacks native 128-bit ops. They model the two-limb representation explicitly so register allocation and codegen can deal with 64-bit chunks.

| Opcode | Description |
|--------|-------------|
| `lo64` | Extract low 64 bits of a 128-bit pseudo |
| `hi64` | Extract high 64 bits of a 128-bit pseudo |
| `pair64` | Combine `(lo, hi)` into a 128-bit pseudo |
| `addc` | 64-bit add producing a carry output |
| `adcc` | 64-bit add with carry in *and* out |
| `subc` | 64-bit sub producing a borrow output |
| `sbcc` | 64-bit sub with borrow in *and* out |
| `umulhi` | Upper 64 bits of an unsigned 64×64 multiply |

### Miscellaneous

| Opcode | Description |
|--------|-------------|
| `entry` | Function entry point |
| `nop` | No operation |
| `asm` | Inline assembly (see `AsmData`) |

## Data Structures

### PseudoKind

```
Void        - no value
Undef       - undefined
Reg(u32)    - virtual register %r{n}
Arg(u32)    - function argument %arg{n}
Phi(u32)    - phi result %phi{n}
Sym(String) - symbol reference
Val(i128)   - integer constant ${n} (wide enough for `__int128` constants)
FVal(f64)   - float constant ${n}
```

### Instruction Fields

```
op: Opcode              - operation
target: PseudoId        - result (optional)
src: Vec<PseudoId>      - operands
typ: TypeId             - result type
size: u32               - bit width
src_size: u32           - source width (conversions)
offset: i64             - memory offset
bb_true/bb_false        - branch targets
phi_list                - [(bb, pseudo), ...]
switch_cases/default    - switch targets
func_name               - direct call target
indirect_target         - indirect call target
arg_types               - call argument types
asm_data                - inline assembly
```

### BasicBlock

```
id: BasicBlockId        - unique ID (.L{n})
insns: Vec<Instruction> - instruction sequence
parents/children        - CFG edges
idom                    - immediate dominator
dom_level               - dominator tree depth
dom_children            - dominated blocks
dom_frontier            - dominance frontier
phi_map                 - var name -> phi index
```

### Function

```
name                    - function name
return_type             - return TypeId
params                  - [(name, TypeId), ...]
blocks                  - basic blocks
entry                   - entry block ID
pseudos                 - all pseudos
locals                  - local variable map
is_static/inline/noreturn - attributes
```

### Module

```
functions               - all functions
globals                 - [(name, TypeId, Initializer), ...]
strings/wide_strings    - string literals
extern_symbols          - symbols needing GOT
```

## IR Passes

| File | Purpose |
|------|---------|
| `linearize.rs` (+ `_init.rs`, `_stmt.rs`, `_emit.rs`) | AST to IR; builds basic blocks. Split by responsibility: top-level/expr in `linearize.rs`, designated initializers in `_init.rs`, statements in `_stmt.rs`, helper emitters in `_emit.rs`. |
| `ssa.rs` | Memory to SSA; inserts phi nodes |
| `dominate.rs` | Dominator tree (Cooper algorithm) |
| `dce.rs` | Dead code elimination (mark-sweep on SSA roots, fold-branches-to-unreachable, unreachable-block removal) |
| `instcombine.rs` | Constant folding, algebraic simplification |
| `inline.rs` | Function inlining |
| `lower.rs` | Phi elimination to copies |

The driver in `cc/opt.rs` runs `inline → (instcombine + dce)*` to fixed point (up to 10 iterations).

## Display Format

```
define type#1 main() {
.L0:
    %0 = setval.32 $10
    %1 = setval.32 $20
    %2 = add.32 %0, %1
    ret %2
}
```

Format: `target = op.size src1, src2`
