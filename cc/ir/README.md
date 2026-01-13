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
| `unreachable` | UB if reached; optimization hint |
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
| `copy` | Value copy (phi elimination) |
| `setval` | Create pseudo for constant |
| `sel` | Ternary: `src[0]` ? `src[1]` : `src[2]` |

### Call Operations

| Opcode | Description |
|--------|-------------|
| `call` | Function call; `func_name` or `indirect_target` |

Call fields:
- `src`: arguments
- `arg_types`: parallel type info
- `variadic_arg_start`: where varargs begin
- `is_sret_call`: large struct return via hidden pointer
- `is_two_reg_return`: 9-16 byte struct via RAX+RDX / X0+X1
- `is_noreturn_call`: function never returns
- `abi_info`: rich ABI classification (see below)

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

This provides more detailed ABI information than `is_sret_call`/`is_two_reg_return`,
including per-eightbyte register class (INTEGER vs SSE) for struct fields.

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

### Stack & Non-local Jumps

| Opcode | Description |
|--------|-------------|
| `alloca` | Dynamic stack allocation |
| `setjmp` | Save context; returns 0 or longjmp value |
| `longjmp` | Restore context (never returns) |

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
Val(i64)    - integer constant ${n}
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
| `linearize.rs` | AST to IR; builds basic blocks |
| `ssa.rs` | Memory to SSA; inserts phi nodes |
| `dominate.rs` | Dominator tree (Cooper algorithm) |
| `dce.rs` | Dead code elimination |
| `instcombine.rs` | Constant folding, algebraic simplification |
| `inline.rs` | Function inlining |
| `lower.rs` | Phi elimination to copies |

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
