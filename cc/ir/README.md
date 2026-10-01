# IR Reference

SSA-form intermediate representation for the c17 C17 compiler. Inspired by Linus Torvalds' sparse IR.

## Core Concepts

- **SSA form**: Each pseudo assigned exactly once; phi nodes at join points
- **Basic blocks**: Linear instruction sequences ending with a terminator
- **Pseudos**: Virtual registers (`%r0`), arguments (`%arg0`), phis (`%phi0`), constants (`$42`), symbols
- **Typed operations**: Instructions carry TypeId and bit size

## A Tour

### Two IRs

c17 has two intermediate representations, with a hard line between them.

- **HIR**, this directory: `ir::Function`, `BasicBlock`, `Instruction`. It is
  a typed SSA form over virtual registers (pseudos), modelled on sparse's
  linearized IR. It is nearly target-independent: the only target knowledge
  it carries is type layout, ABI classification on calls (`abi_info`), and
  whatever target mapping has rewritten. Every optimization runs here.
- **LIR**, in `cc/arch/`: one enum per CPU, `X86Inst` (`arch/x86_64/lir.rs`)
  and `Aarch64Inst` (`arch/aarch64/lir.rs`). Each variant is one machine
  instruction or assembler directive, with typed operands: registers, sized
  memory addressing modes, immediates, labels and condition codes. The goal is
  a low-level IR that can express everything each supported CPU does, so
  nothing reaches the output as a string the compiler cannot inspect. Shared
  operand types, the `Directive` set (sections, data, CFI, DWARF) and the
  `EmitAsm`/`LirInst` traits live in `arch/lir.rs`.

Nothing is optimized at the LIR level today. LIR exists so that what is
emitted is data, not text: instructions can be checked, rewritten and
measured before printing. That is what makes AArch64's immediate legalizer and
branch relaxation possible, and where an assembly peephole would go.

### From HIR to LIR

The HIR half is set out stage by stage under [Pipeline](#pipeline).

```
linearize ─► ssa ─► target mapping ─► optimize ─► libcall fallbacks ─► lower
   (HIR, memory form)   (HIR, SSA, target-shaped)                (HIR, out of SSA)
                                                                        │
             ┌──────────────────────────────────────────────────────────┘
             ▼
  register allocation ─► instruction selection ─► [aarch64: legalize, relax] ─► print
   (over HIR pseudos)     (HIR insn → LIR insns)        (LIR → LIR)              (EmitAsm)
```

- **Target mapping** (`arch/mapping.rs` with an `ArchMapper` per target) is
  the last HIR-to-HIR step that knows the CPU. It rewrites what the target
  cannot do directly into HIR it can: `__int128` into 64-bit limbs, and
  operations with no instruction into library calls. A libm opcode such as
  `sqrt` is kept until after the optimizer, so it can still be folded.
- **Register allocation** (`arch/{x86_64,aarch64}/regalloc.rs`, both linear
  scan, sharing liveness, local lifetimes and slot placement in
  `arch/regalloc.rs`) runs on lowered HIR, not on LIR. It assigns every pseudo
  a location (`Loc`: a register, a stack slot, an immediate or a symbol).
  Register constraints, such as division's `rax:rdx` or variable shifts' `rcx`
  on x86-64, come from per-opcode tables (`opcode_constraints`). A few
  registers are reserved as codegen scratch and never allocated: R10/R11 on
  x86-64; X9–X11, V16–V18 and the legalizer's X15 on AArch64.
- **Instruction selection** is a `match insn.op` per target
  (`emit_insn` in `arch/*/codegen.rs`), split by topic into `expression.rs`,
  `memory.rs`, `call.rs`, `float.rs`, `atomic.rs`, `inline_asm.rs`, `frame.rs`
  and, on x86-64, `x87.rs`. Each HIR instruction becomes LIR through
  `push_lir`, reading operand locations from the allocator.
- **AArch64 only:** `push_lir` passes every instruction through
  `legalize.rs`, which expands an immediate no encoding can hold through X15.
  After the function is complete, `relax.rs` rewrites any conditional branch
  whose target is out of range.
- **Printing:** `CodeGenBase::emit_all` (`arch/codegen.rs`) prints the LIR
  buffer through `EmitAsm`, writing ELF or Mach-O syntax as the `Target` says.
  `arch/dwarf.rs` produces the debug sections.

### Key data structures

- `Function`, in `mod.rs`: its blocks (`blocks`, with an `entry`), the pseudo
  table (`pseudos`, indexed by `PseudoId`), the local variables (`locals`,
  name to `LocalVar`), and the parameters.
- `Pseudo` / `PseudoKind`: what a `PseudoId` is: a register, argument, phi,
  constant (`Val`/`FVal`) or symbol (`Sym`). A constant's value lives in its
  pseudo, not in the instruction that uses it. A temporary may have no table
  entry at all, which means a plain register.
- `Instruction`: an opcode, a target, operands, result type and width, and the
  rarer fields in `InsnExtra`. See [Instruction Fields](#instruction-fields).
- `BasicBlock`: instructions ending in exactly one terminator, plus the CFG
  edge cache (`parents`/`children`). `cfg.rs` owns every edge edit.
- `TypeTable`, in `cc/types.rs`: every `TypeId` resolves here, along with
  layout and qualifiers. Passes ask it instead of guessing from `size`.
- `Module`: the functions, globals and string literals of one translation
  unit.

### How a pass is written

Most passes are a single function, `run(func: &mut Function, ...) -> bool`,
that returns whether it changed anything. The fixed-point loop in
`cc/opt.rs` relies on that answer; a pass that reports no change after
changing something leaves work undone. The rules every pass follows:

- **Edit the CFG only through `cfg.rs`** (`add_edge`, `remove_edge`,
  `remove_unreachable_blocks`, `simplify_cfg`) and `propagate.rs`
  (`retarget_terminator`). Never edit `parents`/`children` by hand; the
  validator checks them against the instructions after every stage.
- **Delete with `Instruction::kill`**, which turns the instruction into a
  `nop`. Don't remove instructions from a block while iterating over it;
  `remove_nops` compacts the block later.
- **Make new instructions with `build::Builder`**, which allocates pseudos and
  constants, carries the source position, and marks volatile accesses.
- **Never rewrite a rule that already exists.** Constant evaluation is
  `constfold`. "What constant is this?" is `facts::ConstMap`. Rewriting a
  proven constant or branch is `propagate`. A sparse conditional analysis is a
  lattice plugged into `dataflow`. Memory questions go to `memloc` (where does
  this point), `escape` (can anything else reach this local) and `effects`
  (what can this call touch).
- **Respect what is observable.** A volatile access
  (`is_volatile_access`) is never deleted, merged or moved. An opcode with
  `has_side_effects` is a root for DCE. `may_access_memory` says how far an
  instruction's effect reaches, and `is_memory_barrier` says what a moved
  memory operation may not cross. Today only `ifconv` moves code, and only
  instructions it has proved safe to speculate, which never touch memory.
- **SSA holds until `lower`.** Each pseudo has one definition, and the
  function-wide maps in `facts.rs` are sound only because of that. Never run
  them on lowered IR.

**To start a new pass:**
1. Read `opt.rs`'s `PASSES` table, including the comment beside each entry
   explaining its position, and pick yours.
2. Read a small pass in the same style: `copyprop.rs` (rewriting uses),
   `constglobal.rs` (rewriting memory to constants) or `ifconv.rs` (reshaping
   the CFG).
3. Write it as `run(func, ...) -> bool` and add it to `PASSES` with a comment
   saying why it sits there.
4. Look at the result with `--dump-ir post-opt` (or `post-linearize`,
   `post-mapping`, `post-lower`, `all`), narrowed with `--dump-ir-func <name>`.
5. Test at two levels, as `cc/CLAUDE.md` requires:
   - unit tests in the pass's own file, which build HIR by hand
     (`Function::new`, `add_pseudo`, `add_insn`) and assert on opcodes rather
     than printed text;
   - an end-to-end test under `cc/tests/` that compiles and runs C at `-O0`
     and `-O2` (`compile_and_run_optimized` is `-O1`).
   A validator failure in either is a bug in the pass.

## Opcodes

Opcodes are listed by their dump spelling (`Opcode::name`). Unless a row
says otherwise, `typ`/`size` describe the result.

### Terminators (end basic blocks)

| Opcode | Description |
|--------|-------------|
| `ret` | Return from function; `src[0]` is the value, if any |
| `br` | Unconditional branch to `bb_true` |
| `cbr` | Conditional branch: `src[0]` ? `bb_true` : `bb_false` |
| `switch` | Multi-way branch on `src[0]`: `switch_cases` holds `(low, high, block)` ranges, `switch_default` the rest |
| `indirectbr` | GNU computed goto to the address in `src[0]`. The instruction cannot name its targets (any address-taken label may be one), so the edges are recorded on the block, as for `asm goto` |
| `unreachable` | Undefined if reached; the back ends emit a trap (`ud2`, `brk #1`). Follows every `noreturn` call, so ending in one does not make a block dead: DCE treats a block as unreachable only when `unreachable` is its first instruction with an effect |
| `longjmp` | Restore a `setjmp` context; never returns |

### Integer Arithmetic and Bitwise (binary)

| Opcode | Description |
|--------|-------------|
| `add`, `sub`, `mul` | Two's-complement, wrapping at `size` |
| `divs`, `divu` | Signed / unsigned division |
| `mods`, `modu` | Signed / unsigned remainder |
| `shl` | Shift left |
| `lsr`, `asr` | Logical / arithmetic shift right |
| `and`, `or`, `xor` | Bitwise |

### Integer Unary

| Opcode | Description |
|--------|-------------|
| `not` | Bitwise complement |
| `neg` | Two's-complement negation |

### Floating-Point Arithmetic

All at the width of `typ`. The rows marked *libm* carry the library
function's name in `func_name`; after optimization, a target without the
instruction turns them into that call (`arch::mapping::call_library_fallbacks`).

| Opcode | Description |
|--------|-------------|
| `fadd`, `fsub`, `fmul`, `fdiv` | IEEE 754 arithmetic |
| `fneg` | Flip the sign bit |
| `fabs` | Clear the sign bit and nothing else: exact, raises nothing, even for a NaN |
| `copysign` | `src[0]` with the sign bit of `src[1]`; exact, a NaN keeps its payload |
| `sqrt` | Correctly rounded square root; sets no `errno` (the linearizer keeps the call for arguments that must). *libm* |
| `fmin`, `fmax` | Smaller / larger operand; a NaN operand is ignored for the other. *libm* |
| `fma` | `src[0] * src[1] + src[2]`, rounded once. *libm* |
| `ffloor`, `fceil`, `ftrunc`, `fround`, `frint`, `fnearbyint` | One opcode, `RoundToIntegral`, keyed on the rounding: the integral value of `floor`, `ceil`, `trunc`, `round`, `rint`, `nearbyint`, as a float with its sign. *libm* |

### Comparisons (result: 0 or 1)

The result is `typ`/`size`; the operands' type and width are
`src_typ`/`src_size`, read through `operand_type`/`operand_width`.
`Opcode::is_int_comparison` and `is_float_comparison` classify them.

| Opcode | Description |
|--------|-------------|
| `seteq`, `setne` | `==`, `!=` |
| `setlt`, `setle`, `setgt`, `setge` | Signed `<`, `<=`, `>`, `>=` |
| `setb`, `setbe`, `seta`, `setae` | Unsigned `<`, `<=`, `>`, `>=` ("below", "above") |
| `fcmp_oeq`, `fcmp_olt`, `fcmp_ole`, `fcmp_ogt`, `fcmp_oge` | Ordered `==`, `<`, `<=`, `>`, `>=`: false when either operand is a NaN |
| `fcmp_one` | C's `!=`, which despite the name is **unordered**: true when either operand is a NaN (`constfold::fcmp_mask`) |

### Type Conversions

Each reads `src_typ`/`src_size` and produces `typ`/`size`.

| Opcode | Description |
|--------|-------------|
| `trunc` | Integer to a narrower integer |
| `zext`, `sext` | Integer to a wider integer, zero- / sign-filled |
| `fcvts`, `fcvtu` | Float to signed / unsigned integer |
| `scvtf`, `ucvtf` | Signed / unsigned integer to float |
| `fcvtf` | Float to float of another width |
| `signbit` | Whether the floating operand's sign bit is set, as an `int` 0 or 1 (also for `-0.0` and a NaN) |

### Memory Operations

| Opcode | Description |
|--------|-------------|
| `load` | Load from address `src[0]` + `offset` into `target` |
| `store` | Store `src[1]` to address `src[0]` + `offset` |
| `symaddr` | Address of the symbol `src[0]` (a link-time constant) |
| `tlsaddr` | Address of a thread-local symbol. A separate opcode because under a call-based TLS model the address is computed by a call that clobbers registers, and the allocator must see that from the opcode alone; `tls.rs` expands it for those models |

For `load` and `store`, `offset` is a **byte** displacement and `size` is the
access width in **bits**. An atomic access has its own opcode.

Both carry a **volatile marker**, `Instruction::is_volatile`, printed as a
trailing `volatile` in a dump. Ask it through `Instruction::is_volatile_access`.
The qualifier has to live on the *access* because it is not always on any
object: for `volatile int *p`, `p` is an ordinary pointer and `*p` is the
volatile object, so `LocalVar::is_ordinary` and
`memloc::GlobalFacts::is_volatile` — which answer only for a named object —
have nothing to say about it. Those two are what a pass asks about the object
as a whole (`memloc::is_ordinary_object` asks both); the marker is what it asks
about the access.

`Linearizer::emit` sets the marker for every access the linearizer emits, from
`types.contains_volatile` of the type that access reaches, and
`ir::build::Builder` does the same for the accesses a pass synthesizes. Reading
a volatile object is observable behaviour (C17 5.1.2.3p6), so a marked access
survives every optimization level: `dce::is_root` treats it as a root,
`loadfwd` will not forward one or fold two into one, `dse` will not delete one,
`constglobal` will not answer one from an initializer, and `ssa` will not
promote the object it reaches out of memory.

### SSA Operations

| Opcode | Description |
|--------|-------------|
| `phi` | SSA merge: `phi_list` = [(bb, pseudo), ...] |
| `phisrc` | Phi source: explicit defining instruction for a phi operand in the predecessor block; backpointer in `phi_list` (not a use). Lets DCE keep phi sources live without false uses. |
| `copy` | `target = src[0]`. Made by SSA promotion, phi elimination, inlining and folding; `copyprop` forwards the no-op ones |
| `setval` | Defines `target` as a constant; the value lives in the target pseudo (`PseudoKind::Val`/`FVal`) |
| `sel` | `Select`: `src[0]` ? `src[1]` : `src[2]`, both arms already evaluated (made by `ifconv`; becomes `cmov`/`csel`) |

### Call Operations

| Opcode | Description |
|--------|-------------|
| `call` | Function call; `func_name` or `indirect_target` |

Call fields (all but `src` live in `InsnExtra`):
- `src`: arguments
- `arg_types`: parallel type info
- `variadic_arg_start`: where varargs begin
- `is_noreturn_call`: function never returns
- `abi_info`: rich ABI classification (see below)
- `returns_via_sret()`: derived from abi_info — large struct return via hidden pointer
- `returns_two_regs()`: derived from abi_info — 9-16 byte struct via RAX+RDX / X0+X1
- `known`: the library function the parser recognised, for `libcall_fold`

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
| `va_arg_pack_len` | `__builtin_va_arg_pack_len()`: the caller's variadic argument count. Replaced by a constant when its `always_inline` function is inlined; one that survives is diagnosed, never emitted |

### Deferred Builtins

| Opcode | Description |
|--------|-------------|
| `constant_p` | `__builtin_constant_p`, held back until propagation has run: `sccp` answers 1 when it proves the operand constant, and `lower` answers 0 for every one left -- all of them at `-O0` |

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

### Memory Builtins

Operands are `(dst, src-or-byte, n)`. The back ends emit the libc call; one whose length is a small constant never gets that far, because `memexpand` replaces it with loads and stores. Treated as roots by DCE; `dse`/`loadfwd` see their effect through `memloc`.

| Opcode | Description |
|--------|-------------|
| `memset` | `memset(dst, c, n)` |
| `memcpy` | `memcpy(dst, src, n)` |
| `memmove` | `memmove(dst, src, n)` (overlap-safe) |

### Stack & Non-local Jumps

| Opcode | Description |
|--------|-------------|
| `alloca` | Dynamic stack allocation |
| `stacksave`, `stackrestore` | Capture and restore the stack pointer. The inliner brackets a callee that `alloca`s with them, so its allocation is released where the call would have returned instead of accumulating in the caller (a loop would otherwise grow the stack every iteration) |
| `setjmp` | Save context; returns 0, or the value a `longjmp` passed (`longjmp` is a terminator, above) |
| `frame_address` | `__builtin_frame_address(level)`; the level is `frame_level()` |
| `return_address` | `__builtin_return_address(level)` |

### C11 Atomics

Each carries its ordering (relaxed / consume / acquire / release / acq-rel /
seq-cst) in `memory_order`, and is a side-effecting root. `src[0]` is the
address.

| Opcode | Description |
|--------|-------------|
| `atomic_load`, `atomic_store` | Atomic load / store |
| `atomic_swap` | Exchange; returns the old value |
| `atomic_cas` | Compare-and-swap: `(addr, expected_addr, desired, order)`. Returns success as a `bool`; on failure the current value is written to `*expected_addr` (gcc's `__atomic_compare_exchange` shape). `size` is the element width |
| `atomic_fetch_add`, `atomic_fetch_sub`, `atomic_fetch_and`, `atomic_fetch_or`, `atomic_fetch_xor` | Read-modify-write; returns the old value |
| `fence` | Thread or signal fence |

### Int128 Decomposition

Emitted by target mapping (`arch/mapping.rs`), which expands `__int128` operations into 64-bit limbs on both targets, so register allocation and code generation see only 64-bit values. A carry or borrow is consumed by naming its producer as `src[2]`.

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
| `entry` | Marks the start of the entry block; the linearizer stores the parameters to their locals after it. Emits nothing -- the back ends write the prologue themselves |
| `nop` | No operation. A deleted instruction becomes one (`Instruction::kill`), and `remove_nops` compacts them |
| `asm` | Inline assembly (see `AsmData`) |
| `lifetime.end` | The lifetime of the local `lifetime_of` names ends: control falls out of the block that declared it. Named out of band, never in `src`, so it is no use and no escape; kept by DCE, dropped with its local by `mem2reg`, and read by the allocators, which let locals whose lifetimes never meet share a slot |

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

Inline, on every instruction:

```
op: Opcode              - operation
target: PseudoId        - result (optional)
src: Vec<PseudoId>      - operands
typ: TypeId             - result type, for every opcode
size: u32               - result width in bits, for every opcode
src_typ/src_size        - operand type and width, for an opcode that reads
                          another type than it produces: the conversions,
                          the comparisons, the bit counts. Read them
                          through `Instruction::operand_type`/`operand_width`
offset: i64             - byte displacement of a load or store; the level of
                          frame_address/return_address (`frame_level()`)
bb_true/bb_false        - branch targets
phi_list                - [(bb, pseudo), ...]
pos                     - source position, for diagnostics and debug info
is_volatile             - the access is volatile (see Memory Operations)
extra                   - Option<Box<InsnExtra>>, below
```

`InsnExtra` holds the fields only a few opcodes use, out of line so that the
common instructions stay small. Read it through `Instruction::extra()`, which
answers an all-empty `InsnExtra` when there is none, and write through
`extra_mut()`, which allocates one:

```
func_name               - direct call target; the library function a libm
                          opcode (sqrt, fma, ...) falls back to
indirect_target         - indirect call target
arg_types               - call argument types, parallel to src
variadic_arg_start      - index of the first variadic argument
ends_with_va_arg_pack   - argument list ends with __builtin_va_arg_pack()
is_noreturn_call        - the callee never returns
callee_binding          - whether func_name may be defined in this module;
                          ask through `Instruction::local_callee`
known                   - the library function a call is, for libcall_fold
abi_info                - CallAbiInfo (see below)
switch_cases/default    - switch ranges and default target
asm_data                - inline assembly (template, operands, clobbers)
memory_order            - ordering of an atomic operation
lifetime_of             - the local a lifetime.end names
```

### BasicBlock

```
id: BasicBlockId        - unique ID (.L{n})
insns: Vec<Instruction> - instruction sequence
parents/children        - CFG edges: a cache of what the instructions name,
                          edited only through `cfg.rs`
addr_taken              - reached through `&&label`, by no recorded edge
```

Dominator information is deliberately **not** here. It is an analysis result,
not block content: `dominate::domtree_build(func)` returns a `DomTree` that
describes the CFG as it was when asked. Storing it on the block -- computed
once during linearization, never recomputed -- meant inlining and DCE left it
describing a graph that no longer existed, waiting for the first pass that
read it.

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

### Pipeline

`main.rs` drives a translation unit through:

1. **Linearize** each function, then `ssa_convert` and `mem2reg` it.
2. `validate` (SSA stage).
3. `mach_o_dtors` (Mach-O only), target mapping (`arch/mapping.rs`, which
   also expands `__int128` into limbs), then `tls`.
4. `validate`.
5. **Optimize** (`cc/opt.rs`), described below.
6. `arch::mapping::call_library_fallbacks`: an opcode the target has no
   instruction for (`sqrt`, binary128 arithmetic, ...) becomes its library
   call, after the optimizer has had its chance to fold it.
7. `validate`.
8. **Lower** (`lower.rs`), then `validate` at the lowered stage, which drops
   the single-definition and phi invariants that phi elimination breaks on
   purpose.

A validator failure is an internal compiler error, on every compile.

The optimizer, from `-O1` up, runs:
- one round of `sccp`, `instcombine`, `copyprop`, `dce` and `simplify_cfg`
  on every function, so the inliner sizes a callee by the code it will emit;
- `inline`, then `memexpand`;
- the passes in `opt::PASSES` to a fixed point: `constglobal`, `memexpand`,
  `loadfwd`, `vrp`, `ifconv`, `sccp`, `instcombine`, `libcall_fold`,
  `copyprop`, `dse`, `dce`, `simplify_cfg`. This runs for at most
  `MAX_ITERATIONS` rounds;
- then `mem2reg`.

The order inside the loop matters, and `PASSES` gives the reason for each
pass's place beside it. If a function is still changing when the rounds run
out, it is left as it is, which is correct but less optimized. It is reported,
with the passes still changing it, under `--dump-ir post-opt`.

At `-O0` only `inline` (for `always_inline` functions only) and `memexpand`
run.

CFG simplification runs only inside that loop. Critical edges are split once,
at the top of lowering, immediately before φ elimination. Nothing merges blocks
after that, because merging would undo the splitting the copies depend on. See
`cfg.rs`.

### Construction and lowering

| File | Purpose |
|------|---------|
| `linearize.rs` (+ `_init.rs`, `_stmt.rs`, `_emit.rs`, `_atomic.rs`) | AST to IR, with every local in memory. `linearize.rs` holds functions and expressions, `_init.rs` initializers and globals, `_stmt.rs` statements, `_emit.rs` shared emitters (constants, block copies, bit-fields, assignments), and `_atomic.rs` ordinary operators on `_Atomic` objects as atomic RMW loops. Marks each block-scope local's `lifetime.end` |
| `ssa.rs` | Promotes each eligible local out of memory into SSA values. A local is eligible when it is scalar, not volatile or atomic, never has its address taken, and is only accessed whole. φs go at iterated dominance frontiers, followed by renaming over the dominator tree. Runs once, during linearization |
| `mem2reg.rs` | Despite the name, it promotes nothing: it deletes the locals no instruction names any more, with their lifetime markers, so they get no stack slot. Runs after `ssa` and again after the optimizer |
| `mach_o_dtors.rs` | Mach-O does not run a `destructor` listed in `__mod_term_func` for an executable, so each one is registered with `atexit` from a synthesized constructor |
| `tls.rs` | Expands `tlsaddr` into its call for the call-based TLS models (ELF TLS descriptors, every Darwin access), so the register allocator sees the clobbers |
| `lower.rs` | Out of SSA. It answers each remaining `constant_p` with 0, splits critical edges, then eliminates φs into copies, which it sequentializes as a parallel copy |

### Optimization passes

| File | Purpose |
|------|---------|
| `inline.rs` | Inlines at the call site by callee size: always below a small threshold, larger ones with an `inline` hint, under a per-caller growth cap. `always_inline` callees are inlined at every level. A callee that `alloca`s is bracketed with `stacksave`/`stackrestore`. Afterwards it deletes `static` functions with no callers left |
| `memexpand.rs` | A `memcpy`, `memset` or `memmove` of a small constant length becomes integer loads and stores, at every level. It also owns the chunking and size limit that the linearizer's aggregate copies use |
| `constglobal.rs` | A load of a `const` global, by name or through its address, becomes its initializer. Needs no alias or escape analysis, because modifying a `const`-defined object is undefined behaviour (C17 6.7.3p6) |
| `loadfwd.rs` | Store-to-load forwarding and redundant-load elimination, across blocks, using `memloc`/`escape`/`effects`. Also `MemOracle`: the value a location holds just before an instruction, as a pseudo or, for one byte, a constant |
| `vrp.rs` | Value-range propagation over `range.rs` intervals, on the `dataflow` solver. Unlike `sccp`, it attaches facts to CFG *edges*: `var <= 0` being false gives `var >= 1` on that edge. It runs before `ifconv`, which would otherwise collapse the diamond the fact hangs on |
| `ifconv.rs` | Turns a short-circuit `&&`/`\|\|` diamond into a `sel` when the arm is safe to speculate (no memory access, call or trap). This puts the two comparisons in one block, where `instcombine` can relate them |
| `sccp.rs` | Sparse conditional constant propagation (Wegman–Zadeck) on the `dataflow` solver. It folds constant branches, removing the dead edge; `dce` then deletes the unreachable blocks |
| `instcombine.rs` | Per-instruction rewriting of pure operations: constant folding through `constfold`, algebraic identities (`x - x`, `x ^ x`, ...), and pairs of comparisons over the same operands. It never moves or reorders instructions |
| `libcall_fold/` | Folds calls the parser tagged as known library functions (`Instruction::known`). Results the arguments decide become constants: `strlen("abc")` is 3, and `strcmp(p, "")` is the first byte of `p`. An output call whose result is unused becomes a cheaper one that writes the same bytes: `printf("hi\n")` becomes `puts("hi")`. A `memmove` whose blocks cannot overlap becomes a `memcpy`. A dispatcher, with one module per family of functions |
| `copyprop.rs` | Each use of a no-op `copy` (same width, same register class) reads the copy's source instead; `dce` collects the copies |
| `dse.rs` | Dead-store elimination, for two cases: a store whose every byte is overwritten before any is read (a forward walk within a block), and a store to a non-escaping local that nothing reads before the function returns (a backward walk over the CFG) |
| `dce.rs` | Mark-sweep from the roots, which are any opcode with `has_side_effects()` plus any volatile access. It also folds a branch into a block that does nothing but `unreachable`, and removes unreachable blocks |
| `cfg.rs` | Every edit to the CFG goes through here: successors (including `asm goto` labels), edge removal, critical-edge splitting, unreachable-block removal, and `simplify_cfg` (forwarder threading and block merging) |

### Analyses and shared rules (not passes)

| File | Purpose |
|------|---------|
| `validate.rs` | The IR invariants: single definition; phi arity matching the predecessors; one terminator, at the end of the block; `parents`/`children` matching what the instructions name; branch targets that exist; operand types; paired lifetime markers; displacements in range; and every memory access or barrier counted as a side effect. Run at every stage listed above |
| `dominate.rs` | Dominator tree by Lengauer–Tarjan (simple form, O(E log V)), and iterated dominance frontiers by Sreedhar–Gao. It returns a `DomTree` snapshot rather than writing into the blocks |
| `dataflow.rs` | The sparse conditional solver that `sccp` and `vrp` share: seeding, executable-edge marking, the worklists, the step budget, and rewriting what a solution proves. A pass supplies only the lattice and the transfer functions |
| `range.rs` | A set of W-bit integers as one interval that may wrap, which answers signed and unsigned questions alike, plus its transfer functions. It has no IR types, so it is tested exhaustively at four bits |
| `constfold.rs` | Evaluates one operation over constants, at the operand's width and the signedness the opcode implies, for integer and floating types. It is the one copy of these rules, shared by `instcombine`, `sccp`, `vrp`, `copyprop` and both back ends |
| `facts.rs` | Function-wide queries a pass builds before rewriting anything: `ConstMap` (the constant a pseudo holds) and `CmpFacts` (the comparison that defined it). They are sound without dominance only because of SSA's single definition, so they must not be used after `lower` |
| `propagate.rs` | The rewrites an analysis performs once it has proved something: a value becomes a constant, a conditional terminator a `br`. Shared by `sccp` and `vrp` |
| `memloc.rs` | What an address points to (a base, a constant byte offset and a width), whether two accesses can overlap, and module-wide facts about globals |
| `escape.rs` | Which locals' addresses can reach a callee, an asm statement or another thread. A call cannot write a local that does not escape |
| `effects.rs` | What a call may do to memory, from the callee's attributes and from its body within this translation unit |
| `strdata.rs` | The bytes of objects whose contents hold for the whole run (string literals, and `const` `char` arrays under `constglobal`'s rule), and the string a pointer into one reads. For a length it also gives the one length every `sel` and φ arm agree on, which for a local array comes from what `MemOracle` says the earlier stores left in it |
| `build.rs` | `Builder`: makes the instructions that replace one instruction (new pseudos, constants, loads, stores, operations) at its source position, with volatile marking. Used by `memexpand` and `libcall_fold` |

## Display Format

```
define int main() {
.L0:
    %1 = setval.32 $10
    %3 = setval.32 $20
    %4 = copy.32 %1($10)
    %5 = copy.32 %3($20)
    %6 = add.32 %4, %5
    ret.32 %6
}
```

Format: `target = op.size src1, src2`

Printing needs the pseudo and type tables, so it goes through
`module.display(&types)` / `func.display(&types)` rather than a bare `{}`.
Nothing else can resolve what an instruction actually holds: a `setval`'s
constant lives in its *target pseudo*, types are `TypeId` indices, and symbols
are `PseudoKind::Sym`. A constant or symbol operand prints as `%id($value)` /
`%id(@name)` -- the id so the line stays linked to its definition, the payload
because that is the part an index cannot show.


## References

- **sparse** (Linus Torvalds, Chris Li, Luc Van Oostenryck, et al.,
  <https://sparse.docs.kernel.org/>): the model for the HIR. Its linearizer,
  its pseudo-based SSA, and opcodes such as `phisrc`, `setval` and `sel` (and
  the `OP_PHISOURCE` design that keeps a phi's sources live without counting
  them as uses) all come from sparse.
- R. Cytron, J. Ferrante, B. K. Rosen, M. N. Wegman, F. K. Zadeck, "Efficiently
  Computing Static Single Assignment Form and the Control Dependence Graph",
  *TOPLAS* 13(4), 1991: SSA construction by φ placement at dominance frontiers,
  then renaming over the dominator tree (`ssa.rs`).
- V. C. Sreedhar, G. R. Gao, "A Linear Time Algorithm for Placing φ-Nodes",
  *POPL* 1995: iterated dominance frontiers without computing frontiers first
  (`dominate.rs::idf_compute`).
- T. Lengauer, R. E. Tarjan, "A Fast Algorithm for Finding Dominators in a
  Flowgraph", *TOPLAS* 1(1), 1979: the dominator tree, in its simple form
  (`dominate.rs::domtree_build`).
- M. N. Wegman, F. K. Zadeck, "Constant Propagation with Conditional
  Branches", *TOPLAS* 13(2), 1991: the sparse conditional solver behind
  `sccp` and `vrp` (`dataflow.rs`).
- J. A. Navas, P. Schachte, H. Søndergaard, P. J. Stuckey,
  "Signedness-Agnostic Program Analysis: Precise Integer Bounds for Low-Level
  Code", *APLAS* 2012: wrapped intervals, the domain `range.rs` uses so that
  one interval answers both signed and unsigned questions.
- P. Briggs, K. D. Cooper, T. J. Harvey, L. T. Simpson, "Practical
  Improvements to the Construction and Destruction of Static Single
  Assignment Form", *Software: Practice and Experience* 28(8), 1998: the
  lost-copy and swap problems, which critical-edge splitting and copy
  sequentialization in `lower.rs` exist to avoid.
- V. C. Sreedhar, R. D. Ju, D. M. Gillies, V. Santhanam, "Translating Out of
  Static Single Assignment Form", *SAS* 1999: sequentializing parallel copies,
  and Method I for a φ reached along an edge that cannot be split
  (`lower.rs`).
- M. Poletto, V. Sarkar, "Linear Scan Register Allocation", *TOPLAS* 21(5),
  1999: the allocators in `arch/*/regalloc.rs`. The register-constraint
  handling follows LLVM's approach.
- R. L. Smith, "Algorithm 116: Complex Division", *CACM* 5(8), 1962: the
  linearizer's `_Complex` division, and its truncating integer form, which
  matches gcc.
- **LLVM** (<https://llvm.org/docs/LangRef.html>): the naming of the
  floating-point comparisons and of `instcombine`, the constraint-aware
  register allocation, and several Darwin ABI details whose LLVM source is
  cited in `arch/`.
- **ABIs**, which `abi_info` and the back ends implement: System V AMD64 psABI
  (<https://gitlab.com/x86-psABIs/x86-64-ABI>), AAPCS64
  (<https://github.com/ARM-software/abi-aa>), and Apple's "Writing ARM64 code
  for Apple platforms" for the Darwin variations.
- **IEEE 754-2019** and C17 Annex F: the semantics of the floating-point
  opcodes, and what `constfold` may fold.
