//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Intermediate Representation (IR) for pcc C99 compiler
// SSA-form IR with basic blocks and typed pseudo-registers
//
// The IR uses Single Static Assignment (SSA) form where each variable
// is assigned exactly once. This simplifies dataflow analysis and
// optimization passes.
//

pub mod dce;
pub mod dominate;
pub mod inline;
pub mod instcombine;
pub mod linearize;
pub mod lower;
pub mod ssa;

use crate::abi::{get_abi_for_conv, ArgClass, CallingConv};
use crate::diag::Position;
use crate::target::Target;
use crate::types::{TypeId, TypeTable};
use std::collections::{HashMap, HashSet};
use std::fmt;

const DEFAULT_INSN_CAPACITY: usize = 32;
const DEFAULT_CFG_EDGE_CAPACITY: usize = 4;
const DEFAULT_DOM_CAPACITY: usize = 4;
const DEFAULT_SRC_CAPACITY: usize = 4;
const DEFAULT_PHI_CAPACITY: usize = 4;
const DEFAULT_PARAM_CAPACITY: usize = 8;
const DEFAULT_BLOCK_CAPACITY: usize = 512;
const DEFAULT_PSEUDO_CAPACITY: usize = 2048;
const DEFAULT_LOCAL_CAPACITY: usize = 64;

// ============================================================================
// Call ABI Information
// ============================================================================

/// ABI classification information for a function call.
///
/// This carries the argument and return value classifications from the
/// frontend through the IR to the backend. It replaces the binary
/// boolean flags with richer type information.
#[derive(Debug, Clone)]
pub struct CallAbiInfo {
    /// Per-argument classification (parallel to src for Call instructions)
    pub params: Vec<ArgClass>,
    /// Return value classification
    pub ret: ArgClass,
}

impl CallAbiInfo {
    /// Create a new CallAbiInfo with the given classifications.
    pub fn new(params: Vec<ArgClass>, ret: ArgClass) -> Self {
        Self { params, ret }
    }
}

// ============================================================================
// Instruction Reference - for def-use chains
// ============================================================================

/// Reference to an instruction by (basic block id, instruction index)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InsnRef {
    pub bb: BasicBlockId,
    pub idx: usize,
}

impl InsnRef {
    pub fn new(bb: BasicBlockId, idx: usize) -> Self {
        Self { bb, idx }
    }
}

// ============================================================================
// Opcodes
// ============================================================================

/// IR opcodes for the intermediate representation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Opcode {
    // Function entry
    Entry,

    // Terminators - end a basic block
    Ret,    // Return from function
    Br,     // Unconditional branch
    Cbr,    // Conditional branch
    Switch, // Multi-way branch

    // Integer arithmetic binary ops
    Add,
    Sub,
    Mul,
    DivU, // Unsigned division
    DivS, // Signed division
    ModU, // Unsigned modulo
    ModS, // Signed modulo
    Shl,  // Shift left
    Lsr,  // Logical shift right (unsigned)
    Asr,  // Arithmetic shift right (signed)

    // Floating-point binary ops
    FAdd,
    FSub,
    FMul,
    FDiv,

    // Bitwise/logical binary ops
    And,
    Or,
    Xor,

    // Integer comparisons (result is 0 or 1)
    SetEq, // ==
    SetNe, // !=
    SetLt, // < (signed)
    SetLe, // <= (signed)
    SetGt, // > (signed)
    SetGe, // >= (signed)
    SetB,  // < (unsigned, "below")
    SetBe, // <= (unsigned)
    SetA,  // > (unsigned, "above")
    SetAe, // >= (unsigned)

    // Floating-point comparisons (ordered)
    FCmpOEq,
    FCmpONe,
    FCmpOLt,
    FCmpOLe,
    FCmpOGt,
    FCmpOGe,

    // Unary ops
    Not,  // Bitwise NOT
    Neg,  // Integer negation
    FNeg, // Float negation

    // Type conversions
    Trunc, // Truncate to smaller integer
    Zext,  // Zero-extend to larger integer
    Sext,  // Sign-extend to larger integer
    FCvtU, // Float to unsigned int
    FCvtS, // Float to signed int
    UCvtF, // Unsigned int to float
    SCvtF, // Signed int to float
    FCvtF, // Float to float (different sizes)

    // Memory ops
    Load,  // Load from memory
    Store, // Store to memory

    // SSA-specific
    Phi,       // Phi node for SSA
    PhiSource, // Phi source: explicit defining instruction for phi operand in predecessor block
    Copy,      // Copy (for out-of-SSA)

    // Other
    SymAddr, // Get address of symbol
    Call,    // Function call
    Select,  // Ternary select: cond ? a : b (pure expressions only, enables cmov/csel)
    SetVal,  // Create pseudo for constant
    Nop,     // No operation

    // Variadic function support
    VaStart, // Initialize va_list
    VaArg,   // Get next vararg
    VaEnd,   // Clean up va_list (usually no-op)
    VaCopy,  // Copy va_list

    // Byte-swapping builtins
    Bswap16, // Byte-swap 16-bit value
    Bswap32, // Byte-swap 32-bit value
    Bswap64, // Byte-swap 64-bit value

    // Count trailing zeros builtins
    Ctz32, // Count trailing zeros in 32-bit value
    Ctz64, // Count trailing zeros in 64-bit value

    // Count leading zeros builtins
    Clz32, // Count leading zeros in 32-bit value
    Clz64, // Count leading zeros in 64-bit value

    // Population count builtins
    Popcount32, // Count set bits in 32-bit value
    Popcount64, // Count set bits in 64-bit value

    // Stack allocation builtin
    Alloca, // Dynamic stack allocation

    // Memory builtins - generate calls to C library functions
    Memset,  // memset(dest, c, n) - set memory
    Memcpy,  // memcpy(dest, src, n) - copy memory
    Memmove, // memmove(dest, src, n) - copy overlapping memory

    // Floating-point builtins
    Fabs32,    // Absolute value of float
    Fabs64,    // Absolute value of double
    Signbit32, // Test sign bit of float (returns int)
    Signbit64, // Test sign bit of double (returns int)

    // Optimization hints
    Unreachable, // Code path is never reached (undefined behavior if reached)

    // Stack introspection
    FrameAddress,  // __builtin_frame_address(level) - returns frame pointer at level
    ReturnAddress, // __builtin_return_address(level) - returns return address at level

    // Non-local jumps (setjmp/longjmp)
    Setjmp,  // Save execution context, returns 0 or value from longjmp
    Longjmp, // Restore execution context (never returns)

    // Inline assembly
    Asm, // Inline assembly statement

    // Atomic memory operations (C11 _Atomic support)
    AtomicLoad,     // Atomic load with memory ordering
    AtomicStore,    // Atomic store with memory ordering
    AtomicSwap,     // Atomic exchange (returns old value)
    AtomicCas,      // Compare-and-swap (returns success/old value)
    AtomicFetchAdd, // Atomic fetch-and-add
    AtomicFetchSub, // Atomic fetch-and-subtract
    AtomicFetchAnd, // Atomic fetch-and-and
    AtomicFetchOr,  // Atomic fetch-and-or
    AtomicFetchXor, // Atomic fetch-and-xor
    Fence,          // Memory fence

    // Int128 decomposition ops (used by mapping pass expansion)
    Lo64,   // Extract low 64 bits from 128-bit pseudo
    Hi64,   // Extract high 64 bits from 128-bit pseudo
    Pair64, // Combine two 64-bit pseudos into 128-bit: target = (src[0]=lo, src[1]=hi)
    AddC,   // 64-bit add with carry output: target = src[0] + src[1], sets carry
    AdcC, // 64-bit add with carry in+out: target = src[0] + src[1] + carry; src[2] = carry producer
    SubC, // 64-bit sub with borrow output: target = src[0] - src[1], sets borrow
    SbcC, // 64-bit sub with borrow in+out: target = src[0] - src[1] - borrow; src[2] = borrow producer
    UMulHi, // Upper 64 bits of unsigned 64×64 multiply: target = (src[0] * src[1]) >> 64
}

impl Opcode {
    /// Check if this opcode is a terminator (ends a basic block)
    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            Opcode::Ret
                | Opcode::Br
                | Opcode::Cbr
                | Opcode::Switch
                | Opcode::Unreachable
                | Opcode::Longjmp
        )
    }

    /// Check if this opcode has side effects (cannot be deleted even if unused).
    /// These are "root" instructions for dead code elimination.
    pub fn has_side_effects(&self) -> bool {
        matches!(
            self,
            Opcode::Ret
                | Opcode::Br
                | Opcode::Cbr
                | Opcode::Switch
                | Opcode::Unreachable
                | Opcode::Store
                | Opcode::Call
                | Opcode::Entry
                | Opcode::VaStart
                | Opcode::VaEnd
                | Opcode::VaCopy
                | Opcode::VaArg
                | Opcode::Alloca
                | Opcode::Memset
                | Opcode::Memcpy
                | Opcode::Memmove
                | Opcode::Setjmp
                | Opcode::Longjmp
                | Opcode::Asm
                | Opcode::AtomicLoad
                | Opcode::AtomicStore
                | Opcode::AtomicSwap
                | Opcode::AtomicCas
                | Opcode::AtomicFetchAdd
                | Opcode::AtomicFetchSub
                | Opcode::AtomicFetchAnd
                | Opcode::AtomicFetchOr
                | Opcode::AtomicFetchXor
                | Opcode::Fence
        )
    }

    /// Get the opcode name for display
    pub fn name(&self) -> &'static str {
        match self {
            Opcode::Entry => "entry",
            Opcode::Ret => "ret",
            Opcode::Br => "br",
            Opcode::Cbr => "cbr",
            Opcode::Switch => "switch",
            Opcode::Add => "add",
            Opcode::Sub => "sub",
            Opcode::Mul => "mul",
            Opcode::DivU => "divu",
            Opcode::DivS => "divs",
            Opcode::ModU => "modu",
            Opcode::ModS => "mods",
            Opcode::Shl => "shl",
            Opcode::Lsr => "lsr",
            Opcode::Asr => "asr",
            Opcode::FAdd => "fadd",
            Opcode::FSub => "fsub",
            Opcode::FMul => "fmul",
            Opcode::FDiv => "fdiv",
            Opcode::And => "and",
            Opcode::Or => "or",
            Opcode::Xor => "xor",
            Opcode::SetEq => "seteq",
            Opcode::SetNe => "setne",
            Opcode::SetLt => "setlt",
            Opcode::SetLe => "setle",
            Opcode::SetGt => "setgt",
            Opcode::SetGe => "setge",
            Opcode::SetB => "setb",
            Opcode::SetBe => "setbe",
            Opcode::SetA => "seta",
            Opcode::SetAe => "setae",
            Opcode::FCmpOEq => "fcmp_oeq",
            Opcode::FCmpONe => "fcmp_one",
            Opcode::FCmpOLt => "fcmp_olt",
            Opcode::FCmpOLe => "fcmp_ole",
            Opcode::FCmpOGt => "fcmp_ogt",
            Opcode::FCmpOGe => "fcmp_oge",
            Opcode::Not => "not",
            Opcode::Neg => "neg",
            Opcode::FNeg => "fneg",
            Opcode::Trunc => "trunc",
            Opcode::Zext => "zext",
            Opcode::Sext => "sext",
            Opcode::FCvtU => "fcvtu",
            Opcode::FCvtS => "fcvts",
            Opcode::UCvtF => "ucvtf",
            Opcode::SCvtF => "scvtf",
            Opcode::FCvtF => "fcvtf",
            Opcode::Load => "load",
            Opcode::Store => "store",
            Opcode::Phi => "phi",
            Opcode::PhiSource => "phisrc",
            Opcode::Copy => "copy",
            Opcode::SymAddr => "symaddr",
            Opcode::Call => "call",
            Opcode::Select => "sel",
            Opcode::SetVal => "setval",
            Opcode::Nop => "nop",
            Opcode::VaStart => "va_start",
            Opcode::VaArg => "va_arg",
            Opcode::VaEnd => "va_end",
            Opcode::VaCopy => "va_copy",
            Opcode::Bswap16 => "bswap16",
            Opcode::Bswap32 => "bswap32",
            Opcode::Bswap64 => "bswap64",
            Opcode::Ctz32 => "ctz32",
            Opcode::Ctz64 => "ctz64",
            Opcode::Clz32 => "clz32",
            Opcode::Clz64 => "clz64",
            Opcode::Popcount32 => "popcount32",
            Opcode::Popcount64 => "popcount64",
            Opcode::Alloca => "alloca",
            Opcode::Memset => "memset",
            Opcode::Memcpy => "memcpy",
            Opcode::Memmove => "memmove",
            Opcode::Fabs32 => "fabs32",
            Opcode::Fabs64 => "fabs64",
            Opcode::Signbit32 => "signbit32",
            Opcode::Signbit64 => "signbit64",
            Opcode::Unreachable => "unreachable",
            Opcode::FrameAddress => "frame_address",
            Opcode::ReturnAddress => "return_address",
            Opcode::Setjmp => "setjmp",
            Opcode::Longjmp => "longjmp",
            Opcode::Asm => "asm",
            Opcode::AtomicLoad => "atomic_load",
            Opcode::AtomicStore => "atomic_store",
            Opcode::AtomicSwap => "atomic_swap",
            Opcode::AtomicCas => "atomic_cas",
            Opcode::AtomicFetchAdd => "atomic_fetch_add",
            Opcode::AtomicFetchSub => "atomic_fetch_sub",
            Opcode::AtomicFetchAnd => "atomic_fetch_and",
            Opcode::AtomicFetchOr => "atomic_fetch_or",
            Opcode::AtomicFetchXor => "atomic_fetch_xor",
            Opcode::Fence => "fence",
            Opcode::Lo64 => "lo64",
            Opcode::Hi64 => "hi64",
            Opcode::Pair64 => "pair64",
            Opcode::AddC => "addc",
            Opcode::AdcC => "adcc",
            Opcode::SubC => "subc",
            Opcode::SbcC => "sbcc",
            Opcode::UMulHi => "umulhi",
        }
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

// ============================================================================
// Memory Ordering - for atomic operations
// ============================================================================

/// Memory ordering for atomic operations (C11 memory model)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u8)]
pub enum MemoryOrder {
    /// No ordering constraints
    #[default]
    Relaxed = 0,
    /// Data dependency ordering (rarely used, treated as Acquire)
    Consume = 1,
    /// Acquire semantics: no reads/writes can be reordered before this
    Acquire = 2,
    /// Release semantics: no reads/writes can be reordered after this
    Release = 3,
    /// Both acquire and release semantics
    AcqRel = 4,
    /// Sequential consistency: total global ordering
    SeqCst = 5,
}

impl fmt::Display for MemoryOrder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MemoryOrder::Relaxed => write!(f, "relaxed"),
            MemoryOrder::Consume => write!(f, "consume"),
            MemoryOrder::Acquire => write!(f, "acquire"),
            MemoryOrder::Release => write!(f, "release"),
            MemoryOrder::AcqRel => write!(f, "acq_rel"),
            MemoryOrder::SeqCst => write!(f, "seq_cst"),
        }
    }
}

// ============================================================================
// Pseudo - Virtual registers / values in SSA form
// ============================================================================

/// Unique ID for a pseudo (virtual register)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PseudoId(pub u32);

impl fmt::Display for PseudoId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

/// Type of pseudo value
#[derive(Debug, Clone, PartialEq)]
pub enum PseudoKind {
    /// Void (no value)
    Void,
    /// Undefined value
    Undef,
    /// Virtual register (result of an instruction)
    Reg(u32),
    /// Function argument
    Arg(u32),
    /// Phi node result
    Phi(u32),
    /// Symbol reference (variable, function)
    Sym(String),
    /// Constant integer value
    Val(i128),
    /// Constant float value
    FVal(f64),
}

/// A pseudo (virtual register or value) in SSA form
#[derive(Debug, Clone)]
pub struct Pseudo {
    pub id: PseudoId,
    pub kind: PseudoKind,
    /// Optional name for debugging (from source variable)
    pub name: Option<String>,
}

impl PartialEq for Pseudo {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.kind == other.kind
    }
}

impl Default for Pseudo {
    fn default() -> Self {
        Self {
            id: PseudoId(0),
            kind: PseudoKind::Void,
            name: None,
        }
    }
}

impl Pseudo {
    /// Create an undefined pseudo
    pub fn undef(id: PseudoId) -> Self {
        Self {
            id,
            kind: PseudoKind::Undef,
            name: None,
        }
    }

    /// Create a register pseudo
    pub fn reg(id: PseudoId, nr: u32) -> Self {
        Self {
            id,
            kind: PseudoKind::Reg(nr),
            name: None,
        }
    }

    /// Create an argument pseudo
    pub fn arg(id: PseudoId, nr: u32) -> Self {
        Self {
            id,
            kind: PseudoKind::Arg(nr),
            name: None,
        }
    }

    /// Create a phi pseudo
    pub fn phi(id: PseudoId, nr: u32) -> Self {
        Self {
            id,
            kind: PseudoKind::Phi(nr),
            name: None,
        }
    }

    /// Create a symbol pseudo
    pub fn sym(id: PseudoId, name: String) -> Self {
        Self {
            id,
            kind: PseudoKind::Sym(name.clone()),
            name: Some(name),
        }
    }

    /// Create a constant value pseudo
    pub fn val(id: PseudoId, value: i128) -> Self {
        Self {
            id,
            kind: PseudoKind::Val(value),
            name: None,
        }
    }

    /// Create a constant float pseudo
    pub fn fval(id: PseudoId, value: f64) -> Self {
        Self {
            id,
            kind: PseudoKind::FVal(value),
            name: None,
        }
    }

    /// With a name for debugging
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }
}

impl fmt::Display for Pseudo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            PseudoKind::Void => write!(f, "VOID"),
            PseudoKind::Undef => write!(f, "UNDEF"),
            PseudoKind::Reg(nr) => {
                if let Some(name) = &self.name {
                    write!(f, "%r{}({})", nr, name)
                } else {
                    write!(f, "%r{}", nr)
                }
            }
            PseudoKind::Arg(nr) => write!(f, "%arg{}", nr),
            PseudoKind::Phi(nr) => {
                if let Some(name) = &self.name {
                    write!(f, "%phi{}({})", nr, name)
                } else {
                    write!(f, "%phi{}", nr)
                }
            }
            PseudoKind::Sym(name) => write!(f, "{}", name),
            PseudoKind::Val(v) => {
                if *v > 1000 || *v < -1000 {
                    write!(f, "${:#x}", v)
                } else {
                    write!(f, "${}", v)
                }
            }
            PseudoKind::FVal(v) => write!(f, "${}", v),
        }
    }
}

// ============================================================================
// BasicBlock ID
// ============================================================================

/// Unique ID for a basic block
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BasicBlockId(pub u32);

impl fmt::Display for BasicBlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ".L{}", self.0)
    }
}

// ============================================================================
// Inline Assembly Support
// ============================================================================

/// Constraint information for an inline asm operand
#[derive(Debug, Clone)]
pub struct AsmConstraint {
    /// The pseudo (register or value) for this operand
    pub pseudo: PseudoId,
    /// Optional symbolic name for the operand (e.g., [result])
    pub name: Option<String>,
    /// Matching output operand index (for constraints like "0")
    pub matching_output: Option<usize>,
    /// The constraint string (e.g., "r", "a", "=r", "+m")
    /// Used by codegen to determine specific register requirements
    /// Note: Early clobber (&) is parsed but not explicitly handled since our
    /// simple register allocator doesn't share registers between inputs/outputs
    pub constraint: String,
    /// Size of the operand in bits (8, 16, 32, 64), derived from the C type
    pub size: u32,
}

/// Data for an inline assembly instruction
#[derive(Debug, Clone)]
pub struct AsmData {
    /// The assembly template string
    pub template: String,
    /// Output operands
    pub outputs: Vec<AsmConstraint>,
    /// Input operands
    pub inputs: Vec<AsmConstraint>,
    /// Clobber list: registers and special values ("memory", "cc")
    pub clobbers: Vec<String>,
    /// Goto labels for asm goto: BasicBlockIds the asm can jump to
    /// Referenced in template as %l0, %l1, ... or %l[name]
    pub goto_labels: Vec<(BasicBlockId, String)>,
}

// ============================================================================
// Instruction
// ============================================================================

/// An IR instruction
#[derive(Debug, Clone)]
pub struct Instruction {
    /// Opcode
    pub op: Opcode,
    /// Result pseudo (target)
    pub target: Option<PseudoId>,
    /// Source operands
    pub src: Vec<PseudoId>,
    /// Type of the result (interned TypeId)
    pub typ: Option<TypeId>,
    /// For branches: true target
    pub bb_true: Option<BasicBlockId>,
    /// For conditional branches: false target
    pub bb_false: Option<BasicBlockId>,
    /// For memory ops: offset
    pub offset: i64,
    /// For phi nodes: list of (bb, pseudo) pairs
    pub phi_list: Vec<(BasicBlockId, PseudoId)>,
    /// For calls: function name or pseudo
    pub func_name: Option<String>,
    /// Bit size of the operation (target size for conversions)
    pub size: u32,
    /// Source size for extension/truncation operations
    pub src_size: u32,
    /// Source type for conversion operations (interned TypeId)
    pub src_typ: Option<TypeId>,
    /// For switch: case value to target block mapping
    pub switch_cases: Vec<(i64, BasicBlockId)>,
    /// For switch: default block (if no case matches)
    pub switch_default: Option<BasicBlockId>,
    /// For calls: argument types (parallel to src for Call instructions, interned TypeIds)
    pub arg_types: Vec<TypeId>,
    /// For variadic calls: index where variadic arguments start (0-based)
    /// All arguments at this index and beyond are variadic (should be passed on stack)
    pub variadic_arg_start: Option<usize>,
    /// For calls: true if the called function is noreturn (never returns).
    /// Code after a noreturn call is unreachable.
    pub is_noreturn_call: bool,
    /// For indirect calls: pseudo containing the function pointer address.
    /// When this is Some, the call is indirect (call through function pointer).
    pub indirect_target: Option<PseudoId>,
    /// Source position for debug info
    pub pos: Option<Position>,
    /// For inline assembly: the asm data (template, operands, clobbers)
    pub asm_data: Option<Box<AsmData>>,
    /// For calls: rich ABI classification for arguments and return value.
    /// Derive sret/two-reg-return status via `returns_via_sret()` / `returns_two_regs()`.
    pub abi_info: Option<Box<CallAbiInfo>>,
    /// For atomic operations: memory ordering constraint
    pub memory_order: MemoryOrder,
}

impl Default for Instruction {
    fn default() -> Self {
        Self {
            op: Opcode::Nop,
            target: None,
            src: Vec::with_capacity(DEFAULT_SRC_CAPACITY),
            typ: None,
            bb_true: None,
            bb_false: None,
            offset: 0,
            phi_list: Vec::with_capacity(DEFAULT_PHI_CAPACITY),
            func_name: None,
            size: 0,
            src_size: 0,
            src_typ: None,
            switch_cases: Vec::new(),
            switch_default: None,
            arg_types: Vec::with_capacity(DEFAULT_PARAM_CAPACITY),
            variadic_arg_start: None,
            is_noreturn_call: false,
            indirect_target: None,
            pos: None,
            asm_data: None,
            abi_info: None,
            memory_order: MemoryOrder::default(),
        }
    }
}

impl Instruction {
    /// Create a new instruction
    pub fn new(op: Opcode) -> Self {
        Self {
            op,
            ..Default::default()
        }
    }

    /// Set the target
    pub fn with_target(mut self, target: PseudoId) -> Self {
        self.target = Some(target);
        self
    }

    /// Add a source operand
    pub fn with_src(mut self, src: PseudoId) -> Self {
        self.src.push(src);
        self
    }

    /// Set src1 and src2 (for binary ops)
    pub fn with_src2(mut self, src1: PseudoId, src2: PseudoId) -> Self {
        self.src = vec![src1, src2];
        self
    }

    /// Set src1, src2, src3 (for ternary ops like select)
    pub fn with_src3(mut self, src1: PseudoId, src2: PseudoId, src3: PseudoId) -> Self {
        self.src = vec![src1, src2, src3];
        self
    }

    /// Set the type (caller should also call with_size if needed)
    pub fn with_type(mut self, typ: TypeId) -> Self {
        self.typ = Some(typ);
        self
    }

    /// Set type and size together (convenience for callers with TypeTable access)
    pub fn with_type_and_size(mut self, typ: TypeId, size: u32) -> Self {
        self.typ = Some(typ);
        self.size = size;
        self
    }

    /// Set the true branch target
    pub fn with_bb_true(mut self, bb: BasicBlockId) -> Self {
        self.bb_true = Some(bb);
        self
    }

    /// Set the false branch target
    pub fn with_bb_false(mut self, bb: BasicBlockId) -> Self {
        self.bb_false = Some(bb);
        self
    }

    /// Set memory offset
    pub fn with_offset(mut self, offset: i64) -> Self {
        self.offset = offset;
        self
    }

    /// Set function name for calls
    pub fn with_func(mut self, name: impl Into<String>) -> Self {
        self.func_name = Some(name.into());
        self
    }

    /// Set bit size
    pub fn with_size(mut self, size: u32) -> Self {
        self.size = size;
        self
    }

    /// Set source position for debug info
    pub fn with_pos(mut self, pos: Position) -> Self {
        self.pos = Some(pos);
        self
    }

    /// Set memory ordering for atomic operations
    pub fn with_memory_order(mut self, order: MemoryOrder) -> Self {
        self.memory_order = order;
        self
    }

    /// Create a return instruction
    pub fn ret(src: Option<PseudoId>) -> Self {
        let mut insn = Self::new(Opcode::Ret);
        if let Some(s) = src {
            insn.src.push(s);
        }
        insn
    }

    /// Create a return instruction with type
    pub fn ret_typed(src: Option<PseudoId>, typ: TypeId, size: u32) -> Self {
        Self::ret(src).with_type_and_size(typ, size)
    }

    /// Create an unconditional branch
    pub fn br(target: BasicBlockId) -> Self {
        Self::new(Opcode::Br).with_bb_true(target)
    }

    /// Create a conditional branch
    pub fn cbr(cond: PseudoId, bb_true: BasicBlockId, bb_false: BasicBlockId) -> Self {
        Self::new(Opcode::Cbr)
            .with_src(cond)
            .with_bb_true(bb_true)
            .with_bb_false(bb_false)
    }

    /// Create a switch instruction
    pub fn switch_insn(
        value: PseudoId,
        cases: Vec<(i64, BasicBlockId)>,
        default: Option<BasicBlockId>,
        size: u32,
    ) -> Self {
        Self {
            op: Opcode::Switch,
            src: vec![value],
            switch_cases: cases,
            switch_default: default,
            size,
            ..Default::default()
        }
    }

    /// Create a binary operation
    pub fn binop(
        op: Opcode,
        target: PseudoId,
        src1: PseudoId,
        src2: PseudoId,
        typ: TypeId,
        size: u32,
    ) -> Self {
        Self::new(op)
            .with_target(target)
            .with_src2(src1, src2)
            .with_type_and_size(typ, size)
    }

    /// Create a unary operation
    pub fn unop(op: Opcode, target: PseudoId, src: PseudoId, typ: TypeId, size: u32) -> Self {
        Self::new(op)
            .with_target(target)
            .with_src(src)
            .with_type_and_size(typ, size)
    }

    /// Create a load instruction
    pub fn load(target: PseudoId, addr: PseudoId, offset: i64, typ: TypeId, size: u32) -> Self {
        Self::new(Opcode::Load)
            .with_target(target)
            .with_src(addr)
            .with_offset(offset)
            .with_type_and_size(typ, size)
    }

    /// Create a store instruction
    pub fn store(value: PseudoId, addr: PseudoId, offset: i64, typ: TypeId, size: u32) -> Self {
        Self::new(Opcode::Store)
            .with_src(addr)
            .with_src(value)
            .with_offset(offset)
            .with_type_and_size(typ, size)
    }

    /// Create a call instruction
    pub fn call(
        target: Option<PseudoId>,
        func: &str,
        args: Vec<PseudoId>,
        arg_types: Vec<TypeId>,
        ret_type: TypeId,
        ret_size: u32,
    ) -> Self {
        let mut insn = Self::new(Opcode::Call)
            .with_func(func)
            .with_type_and_size(ret_type, ret_size);
        if let Some(t) = target {
            insn.target = Some(t);
        }
        insn.src = args;
        insn.arg_types = arg_types;
        insn
    }

    /// Create a call instruction with ABI classification.
    ///
    /// This is the canonical way for IR passes to synthesize call instructions
    /// (e.g., runtime library calls). It classifies parameters and return value
    /// using the given calling convention, attaches `CallAbiInfo`, and returns
    /// a ready-to-emit instruction.
    #[allow(clippy::too_many_arguments)]
    pub fn call_with_abi(
        target: Option<PseudoId>,
        func_name: &str,
        args: Vec<PseudoId>,
        arg_types: Vec<TypeId>,
        ret_type: TypeId,
        conv: CallingConv,
        types: &TypeTable,
        target_info: &Target,
    ) -> Self {
        let ret_size = types.size_bits(ret_type);
        let abi = get_abi_for_conv(conv, target_info);
        let param_classes: Vec<_> = arg_types
            .iter()
            .map(|&t| abi.classify_param(t, types))
            .collect();
        let ret_class = abi.classify_return(ret_type, types);
        let call_abi_info = Box::new(CallAbiInfo::new(param_classes, ret_class));

        let mut insn = Self::call(target, func_name, args, arg_types, ret_type, ret_size);
        insn.abi_info = Some(call_abi_info);
        insn
    }

    /// Create an indirect call instruction (call through function pointer)
    pub fn call_indirect(
        target: Option<PseudoId>,
        func_addr: PseudoId,
        args: Vec<PseudoId>,
        arg_types: Vec<TypeId>,
        ret_type: TypeId,
        ret_size: u32,
    ) -> Self {
        let mut insn = Self::call(target, "<indirect>", args, arg_types, ret_type, ret_size);
        insn.indirect_target = Some(func_addr);
        insn
    }

    /// Create a symbol address instruction (get address of a symbol like string literals)
    pub fn sym_addr(target: PseudoId, sym: PseudoId, typ: TypeId) -> Self {
        Self::new(Opcode::SymAddr)
            .with_target(target)
            .with_src(sym)
            .with_type_and_size(typ, 64) // Pointers are always 64-bit
    }

    /// Create a phi node
    pub fn phi(target: PseudoId, typ: TypeId, size: u32) -> Self {
        Self::new(Opcode::Phi)
            .with_target(target)
            .with_type_and_size(typ, size)
    }

    /// Create a phi source instruction (placed in predecessor block).
    /// Back-pointer to owning phi is stored in phi_list by the caller.
    pub fn phi_source(target: PseudoId, src: PseudoId, typ: TypeId, size: u32) -> Self {
        Self::new(Opcode::PhiSource)
            .with_target(target)
            .with_src(src)
            .with_type_and_size(typ, size)
    }

    /// Create a select (ternary) instruction for pure expressions
    /// Enables cmov/csel codegen instead of branches
    pub fn select(
        target: PseudoId,
        cond: PseudoId,
        if_true: PseudoId,
        if_false: PseudoId,
        typ: TypeId,
        size: u32,
    ) -> Self {
        Self::new(Opcode::Select)
            .with_target(target)
            .with_src3(cond, if_true, if_false)
            .with_type_and_size(typ, size)
    }

    /// Create an inline assembly instruction
    pub fn asm(data: AsmData) -> Self {
        Self {
            op: Opcode::Asm,
            asm_data: Some(Box::new(data)),
            ..Default::default()
        }
    }

    /// Check if this call/return uses a hidden sret pointer for the return value.
    pub fn returns_via_sret(&self) -> bool {
        self.abi_info
            .as_ref()
            .map(|ai| matches!(ai.ret, ArgClass::Indirect { .. }))
            .unwrap_or(false)
    }

    /// Check if this call/return uses two registers for the return value.
    pub fn returns_two_regs(&self) -> bool {
        self.abi_info
            .as_ref()
            .map(|ai| match &ai.ret {
                ArgClass::Direct { classes, .. } => classes.len() == 2,
                _ => false,
            })
            .unwrap_or(false)
    }

    /// Convert this instruction to a no-op, clearing all operands.
    pub fn kill(&mut self) {
        self.op = Opcode::Nop;
        self.src.clear();
        self.target = None;
        self.phi_list.clear();
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Format: target = op src1, src2
        if let Some(target) = &self.target {
            write!(f, "{} = ", target)?;
        }

        write!(f, "{}", self.op.name())?;

        // Size suffix (for conversions, show src_size→size)
        if self.src_size > 0
            && self.src_size != self.size
            && matches!(
                self.op,
                Opcode::Sext
                    | Opcode::Zext
                    | Opcode::Trunc
                    | Opcode::FCvtS
                    | Opcode::FCvtU
                    | Opcode::SCvtF
                    | Opcode::UCvtF
                    | Opcode::FCvtF
            )
        {
            write!(f, ".{}to{}", self.src_size, self.size)?;
        } else if self.size > 0 {
            write!(f, ".{}", self.size)?;
        }

        // Operands depend on opcode
        match self.op {
            Opcode::Br => {
                if let Some(bb) = &self.bb_true {
                    write!(f, " {}", bb)?;
                }
            }
            Opcode::Cbr => {
                if let Some(cond) = self.src.first() {
                    write!(f, " {}", cond)?;
                }
                if let Some(bb) = &self.bb_true {
                    write!(f, ", {}", bb)?;
                }
                if let Some(bb) = &self.bb_false {
                    write!(f, ", {}", bb)?;
                }
            }
            Opcode::Phi => {
                for (i, (bb, pseudo)) in self.phi_list.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, " {} ({})", pseudo, bb)?;
                }
            }
            Opcode::PhiSource => {
                if let Some(src) = self.src.first() {
                    write!(f, " {}", src)?;
                }
                if let Some((bb, pseudo)) = self.phi_list.first() {
                    write!(f, " (-> {}:{})", bb, pseudo)?;
                }
            }
            Opcode::Call => {
                if let Some(func) = &self.func_name {
                    write!(f, " {}", func)?;
                }
                write!(f, "(")?;
                for (i, arg) in self.src.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")?;
            }
            Opcode::Switch => {
                if let Some(val) = self.src.first() {
                    write!(f, " {}", val)?;
                }
                for (case_val, bb) in &self.switch_cases {
                    write!(f, ", {} => {}", case_val, bb)?;
                }
                if let Some(default_bb) = &self.switch_default {
                    write!(f, ", default => {}", default_bb)?;
                }
            }
            Opcode::Load | Opcode::Store => {
                for (i, src) in self.src.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    } else {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", src)?;
                }
                if self.offset != 0 {
                    write!(f, " + {}", self.offset)?;
                }
            }
            _ => {
                for (i, src) in self.src.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    } else {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", src)?;
                }
            }
        }

        Ok(())
    }
}

// ============================================================================
// BasicBlock
// ============================================================================

/// A basic block - a sequence of instructions ending with a terminator
#[derive(Debug, Clone)]
pub struct BasicBlock {
    /// Unique ID
    pub id: BasicBlockId,
    /// Instructions in this block
    pub insns: Vec<Instruction>,
    /// Predecessor blocks (CFG)
    pub parents: Vec<BasicBlockId>,
    /// Successor blocks (CFG)
    pub children: Vec<BasicBlockId>,
    /// Optional label name
    pub label: Option<String>,

    // ========================================================================
    // Dominator tree fields (computed by dominate.rs)
    // ========================================================================
    /// Immediate dominator (idom) - the closest dominator in the dominator tree
    pub idom: Option<BasicBlockId>,
    /// Depth in dominator tree (entry is level 0)
    pub dom_level: u32,
    /// Blocks immediately dominated by this one (dominator tree children)
    pub dom_children: Vec<BasicBlockId>,
    /// Dominance frontier - blocks where this block's dominance ends
    pub dom_frontier: Vec<BasicBlockId>,

    // ========================================================================
    // SSA construction fields (used during SSA conversion)
    // ========================================================================
    /// Phi nodes at the beginning of this block (variable name -> phi instruction)
    /// Used during SSA construction to track inserted phi nodes
    pub phi_map: HashMap<String, usize>,
}

impl Default for BasicBlock {
    fn default() -> Self {
        Self {
            id: BasicBlockId(0),
            insns: Vec::with_capacity(DEFAULT_INSN_CAPACITY),
            parents: Vec::with_capacity(DEFAULT_CFG_EDGE_CAPACITY),
            children: Vec::with_capacity(DEFAULT_CFG_EDGE_CAPACITY),
            label: None,
            idom: None,
            dom_level: 0,
            dom_children: Vec::with_capacity(DEFAULT_DOM_CAPACITY),
            dom_frontier: Vec::with_capacity(DEFAULT_DOM_CAPACITY),
            phi_map: HashMap::with_capacity(DEFAULT_PHI_CAPACITY),
        }
    }
}

impl BasicBlock {
    /// Create a new basic block
    pub fn new(id: BasicBlockId) -> Self {
        Self {
            id,
            ..Default::default()
        }
    }

    /// Add an instruction to this block
    pub fn add_insn(&mut self, insn: Instruction) {
        self.insns.push(insn);
    }

    /// Insert an instruction before the terminator
    pub fn insert_before_terminator(&mut self, insn: Instruction) {
        if self.is_terminated() {
            let pos = self.insns.len() - 1;
            self.insns.insert(pos, insn);
        } else {
            self.insns.push(insn);
        }
    }

    /// Check if the block is terminated
    pub fn is_terminated(&self) -> bool {
        self.insns
            .last()
            .map(|i| i.op.is_terminator())
            .unwrap_or(false)
    }

    /// Add a predecessor
    pub fn add_parent(&mut self, parent: BasicBlockId) {
        if !self.parents.contains(&parent) {
            self.parents.push(parent);
        }
    }

    /// Add a successor
    pub fn add_child(&mut self, child: BasicBlockId) {
        if !self.children.contains(&child) {
            self.children.push(child);
        }
    }

    /// Remove edges to/from blocks not in the keep set
    pub fn retain_edges(&mut self, keep: &std::collections::HashSet<BasicBlockId>) {
        self.parents.retain(|p| keep.contains(p));
        self.children.retain(|c| keep.contains(c));
    }

    /// Remove phi entries for a specific predecessor.
    /// Note: corresponding PhiSource instructions in the removed predecessor
    /// block become dead and are cleaned up by a subsequent DCE pass.
    pub fn remove_phi_predecessor(&mut self, pred: BasicBlockId) {
        for insn in &mut self.insns {
            if insn.op == Opcode::Phi {
                insn.phi_list.retain(|(p, _)| *p != pred);
            }
        }
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Label
        if let Some(label) = &self.label {
            writeln!(f, "{}:", label)?;
        } else {
            writeln!(f, "{}:", self.id)?;
        }

        // Instructions
        for insn in &self.insns {
            writeln!(f, "    {}", insn)?;
        }

        Ok(())
    }
}

// ============================================================================
// Function (Entrypoint)
// ============================================================================

/// Information about a local variable for SSA conversion
#[derive(Debug, Clone)]
pub struct LocalVar {
    /// Symbol pseudo for this variable (address)
    pub sym: PseudoId,
    /// Type of the variable (interned TypeId)
    pub typ: TypeId,
    /// Is this variable volatile?
    pub is_volatile: bool,
    /// Is this variable atomic?
    pub is_atomic: bool,
    /// Block where this variable was declared (for scope-aware phi placement)
    /// Phi nodes for this variable should only be placed at blocks dominated by this block.
    pub decl_block: Option<BasicBlockId>,
    /// Explicit alignment from _Alignas specifier (C11 6.7.5)
    /// None means use natural alignment for the type
    pub explicit_align: Option<u32>,
}

/// A parameter whose local storage is filled implicitly by the backend prologue
/// (e.g. complex / two-SSE struct params passed in XMM registers).
/// The inliner uses this to generate explicit struct copies at inline sites.
#[derive(Debug, Clone, Copy)]
pub struct ImplicitParamCopy {
    /// Index into the call instruction's source list
    pub arg_index: u32,
    /// Callee's local Sym pseudo that receives the data
    pub local_sym: PseudoId,
    /// Struct size in bytes
    pub size_bytes: usize,
    /// Type for the 8-byte load/store operations (typically `long`)
    pub qword_type: TypeId,
}

/// A function in IR form
#[derive(Debug, Clone)]
pub struct Function {
    /// Function name
    pub name: String,
    /// Return type (interned TypeId)
    pub return_type: TypeId,
    /// Parameter names and types (interned TypeIds)
    pub params: Vec<(String, TypeId)>,
    /// All basic blocks
    pub blocks: Vec<BasicBlock>,
    /// Entry block ID
    pub entry: BasicBlockId,
    /// All pseudos indexed by PseudoId
    pub pseudos: Vec<Pseudo>,
    /// Next pseudo ID to allocate (monotonically increasing)
    pub next_pseudo: u32,
    /// Local variables (name -> info), used for SSA conversion
    pub locals: HashMap<String, LocalVar>,
    /// Maximum dominator tree depth (computed by dominate.rs)
    pub max_dom_level: u32,
    /// Is this function static (internal linkage)?
    pub is_static: bool,
    /// Is this function noreturn (never returns)?
    pub is_noreturn: bool,
    /// Is this function declared with the inline keyword?
    pub is_inline: bool,
    /// Parameters whose data is supplied implicitly by the backend prologue
    /// (e.g. complex / two-SSE struct params).  When inlining the function,
    /// the inliner must generate an explicit struct copy from the caller's
    /// address argument into the local.
    pub implicit_param_copies: Vec<ImplicitParamCopy>,
    /// Block ID -> index in `blocks` vec (O(1) lookup)
    block_idx: HashMap<BasicBlockId, usize>,
    /// Pseudo ID -> index in `pseudos` vec (O(1) lookup)
    pseudo_idx: HashMap<PseudoId, usize>,
}

impl Default for Function {
    fn default() -> Self {
        Self {
            name: String::new(),
            return_type: TypeId::INVALID,
            params: Vec::with_capacity(DEFAULT_PARAM_CAPACITY),
            blocks: Vec::with_capacity(DEFAULT_BLOCK_CAPACITY),
            entry: BasicBlockId(0),
            pseudos: Vec::with_capacity(DEFAULT_PSEUDO_CAPACITY),
            next_pseudo: 0,
            locals: HashMap::with_capacity(DEFAULT_LOCAL_CAPACITY),
            max_dom_level: 0,
            is_static: false,
            is_noreturn: false,
            is_inline: false,
            implicit_param_copies: Vec::new(),
            block_idx: HashMap::with_capacity(DEFAULT_BLOCK_CAPACITY),
            pseudo_idx: HashMap::with_capacity(DEFAULT_PSEUDO_CAPACITY),
        }
    }
}

impl Function {
    /// Create a new function
    pub fn new(name: impl Into<String>, return_type: TypeId) -> Self {
        Self {
            name: name.into(),
            return_type,
            ..Default::default()
        }
    }

    /// Add a parameter
    pub fn add_param(&mut self, name: impl Into<String>, typ: TypeId) {
        self.params.push((name.into(), typ));
    }

    /// Add a basic block
    pub fn add_block(&mut self, block: BasicBlock) {
        let idx = self.blocks.len();
        self.block_idx.insert(block.id, idx);
        self.blocks.push(block);
    }

    /// Get a block by ID
    pub fn get_block(&self, id: BasicBlockId) -> Option<&BasicBlock> {
        self.block_idx
            .get(&id)
            .and_then(|&idx| self.blocks.get(idx))
    }

    /// Get a mutable block by ID
    pub fn get_block_mut(&mut self, id: BasicBlockId) -> Option<&mut BasicBlock> {
        self.block_idx
            .get(&id)
            .and_then(|&idx| self.blocks.get_mut(idx))
    }

    /// Add a pseudo for tracking
    pub fn add_pseudo(&mut self, pseudo: Pseudo) {
        let idx = self.pseudos.len();
        self.pseudo_idx.insert(pseudo.id, idx);
        self.pseudos.push(pseudo);
    }

    /// Rebuild block index after bulk mutation of `self.blocks`
    pub fn rebuild_block_idx(&mut self) {
        self.block_idx.clear();
        for (idx, block) in self.blocks.iter().enumerate() {
            self.block_idx.insert(block.id, idx);
        }
    }

    /// Add a local variable
    #[allow(clippy::too_many_arguments)]
    pub fn add_local(
        &mut self,
        name: impl Into<String>,
        sym: PseudoId,
        typ: TypeId,
        is_volatile: bool,
        is_atomic: bool,
        decl_block: Option<BasicBlockId>,
        explicit_align: Option<u32>,
    ) {
        self.locals.insert(
            name.into(),
            LocalVar {
                sym,
                typ,
                is_volatile,
                is_atomic,
                decl_block,
                explicit_align,
            },
        );
    }

    /// Get a local variable
    pub fn get_local(&self, name: &str) -> Option<&LocalVar> {
        self.locals.get(name)
    }

    /// Allocate a new pseudo ID
    /// Returns a unique ID and increments the counter
    pub fn alloc_pseudo(&mut self) -> PseudoId {
        let id = PseudoId(self.next_pseudo);
        self.next_pseudo += 1;
        id
    }

    /// Create a new register pseudo and return its ID.
    /// The pseudo is added to self.pseudos.
    pub fn create_reg_pseudo(&mut self) -> PseudoId {
        let id = self.alloc_pseudo();
        self.add_pseudo(Pseudo::reg(id, id.0));
        id
    }

    /// Create a new constant integer pseudo and return its ID.
    /// The pseudo is added to self.pseudos.
    pub fn create_const_pseudo(&mut self, value: i128) -> PseudoId {
        let id = self.alloc_pseudo();
        let pseudo = Pseudo::val(id, value);
        self.add_pseudo(pseudo);
        id
    }

    /// Get a pseudo by its ID
    pub fn get_pseudo(&self, id: PseudoId) -> Option<&Pseudo> {
        self.pseudo_idx
            .get(&id)
            .and_then(|&idx| self.pseudos.get(idx))
    }

    /// Get the constant integer value of a pseudo, if it is a Val.
    pub fn const_val(&self, id: PseudoId) -> Option<i128> {
        self.get_pseudo(id).and_then(|p| match &p.kind {
            PseudoKind::Val(v) => Some(*v),
            _ => None,
        })
    }

    /// Get the symbol name of a pseudo, if it is a Sym.
    pub fn sym_name_of(&self, id: PseudoId) -> Option<&str> {
        self.get_pseudo(id).and_then(|p| match &p.kind {
            PseudoKind::Sym(name) => Some(name.as_str()),
            _ => None,
        })
    }

    /// Check if block a dominates block b
    #[cfg(test)]
    pub fn dominates(&self, a: BasicBlockId, b: BasicBlockId) -> bool {
        if a == b {
            return true;
        }

        let mut current = b;
        while let Some(bb) = self.get_block(current) {
            if let Some(idom) = bb.idom {
                if idom == a {
                    return true;
                }
                current = idom;
            } else {
                break;
            }
        }
        false
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Function header
        write!(f, "define type#{} {}(", self.return_type.0, self.name)?;
        for (i, (name, typ)) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "type#{} %{}", typ.0, name)?;
        }
        writeln!(f, ") {{")?;

        // Basic blocks
        for block in &self.blocks {
            write!(f, "{}", block)?;
        }

        writeln!(f, "}}")
    }
}

// ============================================================================
// Global Variable Initializer
// ============================================================================

/// Initializer for global variables
#[derive(Debug, Clone, PartialEq, Default)]
pub enum Initializer {
    /// No initializer (zero-initialized)
    #[default]
    None,
    /// Integer initializer
    Int(i128),
    /// Float/double initializer
    Float(f64),
    /// String literal initializer (for char arrays)
    String(String),
    /// Wide string literal initializer (for wchar_t arrays)
    WideString(String),
    /// Array initializer: element size in bytes, list of (offset, initializer) pairs
    /// Elements not listed are zero-initialized
    Array {
        elem_size: usize,
        total_size: usize,
        elements: Vec<(usize, Initializer)>,
    },
    /// Struct initializer: list of (offset, size, initializer) tuples
    /// Fields not listed are zero-initialized
    Struct {
        total_size: usize,
        /// Each tuple is (offset, field_size, initializer)
        fields: Vec<(usize, usize, Initializer)>,
    },
    /// Address of a symbol (for pointer initializers like `int *p = &x;`)
    SymAddr(String),
    /// Address of a symbol plus offset (for pointer initializers like `int *p = &s.field;`)
    SymAddrOffset(String, i64),
}

impl fmt::Display for Initializer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Initializer::None => write!(f, "0"),
            Initializer::Int(v) => write!(f, "{}", v),
            Initializer::Float(v) => write!(f, "{}", v),
            Initializer::String(s) => write!(f, "\"{}\"", s.escape_default()),
            Initializer::WideString(s) => write!(f, "L\"{}\"", s.escape_default()),
            Initializer::Array {
                total_size,
                elements,
                ..
            } => {
                write!(f, "[{}]{{ ", total_size)?;
                for (i, (off, init)) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "+{}: {}", off, init)?;
                }
                write!(f, " }}")
            }
            Initializer::Struct { total_size, fields } => {
                write!(f, "struct({}){{ ", total_size)?;
                for (i, (off, size, init)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "+{}[{}]: {}", off, size, init)?;
                }
                write!(f, " }}")
            }
            Initializer::SymAddr(name) => write!(f, "&{}", name),
            Initializer::SymAddrOffset(name, offset) => {
                if *offset >= 0 {
                    write!(f, "&{}+{}", name, offset)
                } else {
                    write!(f, "&{}{}", name, offset)
                }
            }
        }
    }
}

// ============================================================================
// Global Variable Definition
// ============================================================================

/// A global variable definition with full metadata
#[derive(Debug, Clone)]
pub struct GlobalDef {
    /// Variable name
    pub name: String,
    /// Variable type
    pub typ: TypeId,
    /// Initializer (None for uninitialized)
    pub init: Initializer,
    /// C11 _Thread_local / GCC __thread
    pub is_thread_local: bool,
    /// Static storage class (internal linkage)
    pub is_static: bool,
    /// Explicit alignment from _Alignas specifier (None = use natural alignment)
    pub explicit_align: Option<u32>,
}

impl GlobalDef {
    /// Create a new global variable definition
    pub fn new(name: impl Into<String>, typ: TypeId, init: Initializer) -> Self {
        Self {
            name: name.into(),
            typ,
            init,
            is_thread_local: false,
            is_static: false,
            explicit_align: None,
        }
    }

    /// Set explicit alignment from _Alignas specifier
    pub fn with_align(mut self, align: Option<u32>) -> Self {
        self.explicit_align = align;
        self
    }

    /// Set static storage class (internal linkage)
    pub fn with_static(mut self, is_static: bool) -> Self {
        self.is_static = is_static;
        self
    }
}

// ============================================================================
// Module (Translation Unit)
// ============================================================================

/// A module containing multiple functions
#[derive(Debug, Clone)]
pub struct Module {
    /// Functions
    pub functions: Vec<Function>,
    /// Global variables
    pub globals: Vec<GlobalDef>,
    /// String literals (label, content)
    pub strings: Vec<(String, String)>,
    /// Wide string literals (label, content)
    pub wide_strings: Vec<(String, String)>,
    /// Generate debug info
    pub debug: bool,
    /// Source file paths (stream id -> path) for .file directives
    pub source_files: Vec<String>,
    /// External symbols (declared extern but not defined in this module)
    /// These need GOT access on macOS
    pub extern_symbols: HashSet<String>,
    /// External thread-local symbols (declared extern _Thread_local but not defined)
    /// These need TLS access pattern instead of GOT
    pub extern_tls_symbols: HashSet<String>,
    /// Compilation directory (for DW_AT_comp_dir in DWARF)
    pub comp_dir: Option<String>,
    /// Primary source filename (for DW_AT_name in DWARF)
    pub source_name: Option<String>,
}

impl Module {
    /// Create a new module
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            globals: Vec::new(),
            strings: Vec::new(),
            wide_strings: Vec::new(),
            debug: false,
            source_files: Vec::new(),
            extern_symbols: HashSet::new(),
            extern_tls_symbols: HashSet::new(),
            comp_dir: None,
            source_name: None,
        }
    }

    /// Add a function
    pub fn add_function(&mut self, func: Function) {
        self.functions.push(func);
    }

    /// Add a global variable
    pub fn add_global(&mut self, name: impl Into<String>, typ: TypeId, init: Initializer) {
        self.globals.push(GlobalDef::new(name, typ, init));
    }

    /// Add a global variable with explicit alignment (C11 _Alignas)
    /// Handles C tentative definitions: if a global with the same name exists
    /// and has Initializer::None (tentative), replace it with the new definition.
    pub fn add_global_aligned(
        &mut self,
        name: impl Into<String>,
        typ: TypeId,
        init: Initializer,
        align: Option<u32>,
        is_static: bool,
    ) {
        self.add_global_impl(name, typ, init, align, is_static, false);
    }

    /// Add a thread-local global variable with explicit alignment (C11 _Alignas)
    /// Handles C tentative definitions: if a global with the same name exists
    /// and has Initializer::None (tentative), replace it with the new definition.
    pub fn add_global_tls_aligned(
        &mut self,
        name: impl Into<String>,
        typ: TypeId,
        init: Initializer,
        align: Option<u32>,
        is_static: bool,
    ) {
        self.add_global_impl(name, typ, init, align, is_static, true);
    }

    fn add_global_impl(
        &mut self,
        name: impl Into<String>,
        typ: TypeId,
        init: Initializer,
        align: Option<u32>,
        is_static: bool,
        is_thread_local: bool,
    ) {
        let name = name.into();
        // Check for existing tentative definition
        if let Some(existing) = self.globals.iter_mut().find(|g| g.name == name) {
            // Replace tentative definition with actual definition
            if matches!(existing.init, Initializer::None) {
                debug_assert_eq!(
                    existing.typ, typ,
                    "tentative definition type mismatch for '{}'",
                    name
                );
                existing.init = init;
                existing.is_static = is_static;
                if is_thread_local {
                    existing.is_thread_local = true;
                }
                if align.is_some() {
                    existing.explicit_align = align;
                }
                return;
            }
        }
        let mut def = GlobalDef::new(name, typ, init)
            .with_align(align)
            .with_static(is_static);
        if is_thread_local {
            def.is_thread_local = true;
        }
        self.globals.push(def);
    }

    /// Add a string literal and return its label
    pub fn add_string(&mut self, content: String) -> String {
        let label = format!(".LC{}", self.strings.len());
        self.strings.push((label.clone(), content));
        label
    }

    /// Add a wide string literal and return its label
    pub fn add_wide_string(&mut self, content: String) -> String {
        let label = format!(".LWC{}", self.wide_strings.len());
        self.wide_strings.push((label.clone(), content));
        label
    }
}

impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Globals
        for global in &self.globals {
            let tls_marker = if global.is_thread_local { " [tls]" } else { "" };
            match &global.init {
                Initializer::None => {
                    writeln!(f, "@{}: type#{}{}", global.name, global.typ.0, tls_marker)?
                }
                init => writeln!(
                    f,
                    "@{}: type#{} = {}{}",
                    global.name, global.typ.0, init, tls_marker
                )?,
            }
        }

        if !self.globals.is_empty() {
            writeln!(f)?;
        }

        // Functions
        for func in &self.functions {
            writeln!(f, "{}", func)?;
        }

        Ok(())
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::abi::{ArgClass, RegClass};
    use crate::target::Target;
    use crate::types::{Type, TypeTable};

    #[test]
    fn test_opcode_is_terminator() {
        assert!(Opcode::Ret.is_terminator());
        assert!(Opcode::Br.is_terminator());
        assert!(Opcode::Cbr.is_terminator());
        assert!(!Opcode::Add.is_terminator());
        assert!(!Opcode::Load.is_terminator());
    }

    #[test]
    fn test_pseudo_display() {
        let reg = Pseudo::reg(PseudoId(1), 1);
        assert_eq!(format!("{}", reg), "%r1");

        let reg_named = Pseudo::reg(PseudoId(2), 2).with_name("x");
        assert_eq!(format!("{}", reg_named), "%r2(x)");

        let val = Pseudo::val(PseudoId(3), 42);
        assert_eq!(format!("{}", val), "$42");

        let big_val = Pseudo::val(PseudoId(4), 0x1000);
        assert_eq!(format!("{}", big_val), "$0x1000");

        let arg = Pseudo::arg(PseudoId(5), 0);
        assert_eq!(format!("{}", arg), "%arg0");
    }

    #[test]
    fn test_instruction_binop() {
        let types = TypeTable::new(&Target::host());
        let insn = Instruction::binop(
            Opcode::Add,
            PseudoId(3),
            PseudoId(1),
            PseudoId(2),
            types.int_id,
            32,
        );
        assert_eq!(insn.op, Opcode::Add);
        assert_eq!(insn.target, Some(PseudoId(3)));
        assert_eq!(insn.src.len(), 2);
    }

    #[test]
    fn test_instruction_display() {
        let types = TypeTable::new(&Target::host());
        let insn = Instruction::binop(
            Opcode::Add,
            PseudoId(3),
            PseudoId(1),
            PseudoId(2),
            types.int_id,
            32,
        );
        let s = format!("{}", insn);
        assert!(s.contains("add"));
        assert!(s.contains("%3"));
        assert!(s.contains("%1"));
        assert!(s.contains("%2"));
    }

    #[test]
    fn test_basic_block() {
        let mut bb = BasicBlock::new(BasicBlockId(0));
        assert!(!bb.is_terminated());

        bb.add_insn(Instruction::new(Opcode::Nop));
        assert!(!bb.is_terminated());

        bb.add_insn(Instruction::ret(None));
        assert!(bb.is_terminated());
    }

    #[test]
    fn test_function_display() {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("main", types.int_id);
        func.add_param("argc", types.int_id);

        let mut entry = BasicBlock::new(BasicBlockId(0));
        entry.add_insn(Instruction::ret(Some(PseudoId(0))));
        func.add_block(entry);

        let s = format!("{}", func);
        assert!(s.contains("define"));
        assert!(s.contains("main"));
        assert!(s.contains("argc"));
        assert!(s.contains("ret"));
    }

    #[test]
    fn test_branch_instruction() {
        let br = Instruction::br(BasicBlockId(1));
        assert_eq!(br.op, Opcode::Br);
        assert_eq!(br.bb_true, Some(BasicBlockId(1)));

        let cbr = Instruction::cbr(PseudoId(0), BasicBlockId(1), BasicBlockId(2));
        assert_eq!(cbr.op, Opcode::Cbr);
        assert_eq!(cbr.src.len(), 1);
        assert_eq!(cbr.bb_true, Some(BasicBlockId(1)));
        assert_eq!(cbr.bb_false, Some(BasicBlockId(2)));
    }

    #[test]
    fn test_call_instruction() {
        let mut types = TypeTable::new(&Target::host());
        let char_ptr = types.intern(Type::pointer(types.char_id));
        let arg_types = vec![char_ptr, types.int_id];
        let mut call = Instruction::call(
            Some(PseudoId(1)),
            "printf",
            vec![PseudoId(2), PseudoId(3)],
            arg_types.clone(),
            types.int_id,
            32,
        );
        // Add abi_info (required for codegen)
        call.abi_info = Some(Box::new(CallAbiInfo::new(
            vec![
                ArgClass::Direct {
                    classes: vec![RegClass::Integer],
                    size_bits: 64,
                },
                ArgClass::Direct {
                    classes: vec![RegClass::Integer],
                    size_bits: 32,
                },
            ],
            ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits: 32,
            },
        )));
        assert_eq!(call.op, Opcode::Call);
        assert_eq!(call.func_name, Some("printf".to_string()));
        assert_eq!(call.src.len(), 2);
        assert_eq!(call.arg_types.len(), 2);
        assert!(call.abi_info.is_some());
    }

    #[test]
    fn test_load_store() {
        let types = TypeTable::new(&Target::host());

        let load = Instruction::load(PseudoId(1), PseudoId(2), 8, types.int_id, 32);
        assert_eq!(load.op, Opcode::Load);
        assert_eq!(load.offset, 8);

        let store = Instruction::store(PseudoId(1), PseudoId(2), 0, types.int_id, 32);
        assert_eq!(store.op, Opcode::Store);
        assert_eq!(store.src.len(), 2);
    }

    #[test]
    fn test_module() {
        let types = TypeTable::new(&Target::host());
        let mut module = Module::new();

        module.add_global("counter", types.int_id, Initializer::Int(0));

        let func = Function::new("main", types.int_id);
        module.add_function(func);

        assert_eq!(module.globals.len(), 1);
        assert_eq!(module.functions.len(), 1);
    }

    #[test]
    fn test_module_extern_symbols() {
        let mut module = Module::new();

        // New module should have empty extern_symbols
        assert!(module.extern_symbols.is_empty());

        // Can insert extern symbols
        module.extern_symbols.insert("printf".to_string());
        module.extern_symbols.insert("malloc".to_string());

        assert_eq!(module.extern_symbols.len(), 2);
        assert!(module.extern_symbols.contains("printf"));
        assert!(module.extern_symbols.contains("malloc"));

        // Can remove symbols (simulates defining them after extern declaration)
        module.extern_symbols.remove("printf");
        assert_eq!(module.extern_symbols.len(), 1);
        assert!(!module.extern_symbols.contains("printf"));
        assert!(module.extern_symbols.contains("malloc"));
    }

    #[test]
    fn test_memory_order_display() {
        assert_eq!(format!("{}", MemoryOrder::Relaxed), "relaxed");
        assert_eq!(format!("{}", MemoryOrder::Consume), "consume");
        assert_eq!(format!("{}", MemoryOrder::Acquire), "acquire");
        assert_eq!(format!("{}", MemoryOrder::Release), "release");
        assert_eq!(format!("{}", MemoryOrder::AcqRel), "acq_rel");
        assert_eq!(format!("{}", MemoryOrder::SeqCst), "seq_cst");
    }

    #[test]
    fn test_memory_order_default() {
        let order: MemoryOrder = Default::default();
        assert_eq!(order, MemoryOrder::Relaxed);
    }

    #[test]
    fn test_atomic_opcodes_not_terminators() {
        // Atomic operations should not be terminators
        assert!(!Opcode::AtomicLoad.is_terminator());
        assert!(!Opcode::AtomicStore.is_terminator());
        assert!(!Opcode::AtomicSwap.is_terminator());
        assert!(!Opcode::AtomicCas.is_terminator());
        assert!(!Opcode::AtomicFetchAdd.is_terminator());
        assert!(!Opcode::AtomicFetchSub.is_terminator());
        assert!(!Opcode::AtomicFetchAnd.is_terminator());
        assert!(!Opcode::AtomicFetchOr.is_terminator());
        assert!(!Opcode::AtomicFetchXor.is_terminator());
        assert!(!Opcode::Fence.is_terminator());
    }

    #[test]
    fn test_atomic_opcode_names() {
        assert_eq!(Opcode::AtomicLoad.name(), "atomic_load");
        assert_eq!(Opcode::AtomicStore.name(), "atomic_store");
        assert_eq!(Opcode::AtomicSwap.name(), "atomic_swap");
        assert_eq!(Opcode::AtomicCas.name(), "atomic_cas");
        assert_eq!(Opcode::AtomicFetchAdd.name(), "atomic_fetch_add");
        assert_eq!(Opcode::AtomicFetchSub.name(), "atomic_fetch_sub");
        assert_eq!(Opcode::AtomicFetchAnd.name(), "atomic_fetch_and");
        assert_eq!(Opcode::AtomicFetchOr.name(), "atomic_fetch_or");
        assert_eq!(Opcode::AtomicFetchXor.name(), "atomic_fetch_xor");
        assert_eq!(Opcode::Fence.name(), "fence");
    }

    #[test]
    fn test_instruction_with_memory_order() {
        let mut insn = Instruction::new(Opcode::AtomicLoad);
        assert_eq!(insn.memory_order, MemoryOrder::Relaxed); // default

        insn = insn.with_memory_order(MemoryOrder::SeqCst);
        assert_eq!(insn.memory_order, MemoryOrder::SeqCst);

        insn = insn.with_memory_order(MemoryOrder::Acquire);
        assert_eq!(insn.memory_order, MemoryOrder::Acquire);
    }

    #[test]
    fn test_local_var_is_atomic() {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("test", types.void_id);

        // Add a non-atomic local
        let sym1 = PseudoId(1);
        func.add_pseudo(Pseudo::sym(sym1, "x".to_string()));
        func.add_local("x", sym1, types.int_id, false, false, None, None);

        // Add an atomic local
        let sym2 = PseudoId(2);
        func.add_pseudo(Pseudo::sym(sym2, "y".to_string()));
        func.add_local("y", sym2, types.int_id, false, true, None, None);

        // Check the is_atomic field
        assert!(!func.locals.get("x").unwrap().is_atomic);
        assert!(func.locals.get("y").unwrap().is_atomic);
    }

    #[test]
    fn test_fabs_opcodes() {
        assert_eq!(Opcode::Fabs32.name(), "fabs32");
        assert_eq!(Opcode::Fabs64.name(), "fabs64");
        assert!(!Opcode::Fabs32.is_terminator());
        assert!(!Opcode::Fabs64.is_terminator());
    }

    #[test]
    fn test_add_global_aligned_tentative_definition() {
        let types = TypeTable::new(&Target::host());
        let mut module = Module::new();

        // Add a tentative definition (no initializer)
        module.add_global_aligned("x", types.int_id, Initializer::None, None, false);
        assert_eq!(module.globals.len(), 1);
        assert!(matches!(module.globals[0].init, Initializer::None));

        // Add actual definition - should replace the tentative one
        module.add_global_aligned("x", types.int_id, Initializer::Int(42), Some(4), false);
        assert_eq!(module.globals.len(), 1); // Still only one global
        assert!(matches!(module.globals[0].init, Initializer::Int(42)));
        assert_eq!(module.globals[0].explicit_align, Some(4));
    }

    #[test]
    fn test_add_global_aligned_non_tentative_not_replaced() {
        let types = TypeTable::new(&Target::host());
        let mut module = Module::new();

        // Add a real definition (with initializer)
        module.add_global_aligned("x", types.int_id, Initializer::Int(10), None, false);
        assert_eq!(module.globals.len(), 1);

        // Add another definition with same name - should NOT replace (adds new entry)
        module.add_global_aligned("x", types.int_id, Initializer::Int(20), None, false);
        assert_eq!(module.globals.len(), 2); // Two globals now (linker will error)
    }

    #[test]
    fn test_add_global_tls_aligned_tentative_definition() {
        let types = TypeTable::new(&Target::host());
        let mut module = Module::new();

        // Add a TLS tentative definition
        module.add_global_tls_aligned("tls_var", types.int_id, Initializer::None, None, false);
        assert_eq!(module.globals.len(), 1);
        assert!(matches!(module.globals[0].init, Initializer::None));

        // Add actual TLS definition - should replace
        module.add_global_tls_aligned(
            "tls_var",
            types.int_id,
            Initializer::Int(100),
            Some(8),
            false,
        );
        assert_eq!(module.globals.len(), 1);
        assert!(matches!(module.globals[0].init, Initializer::Int(100)));
        assert!(module.globals[0].is_thread_local);
        assert_eq!(module.globals[0].explicit_align, Some(8));
    }

    #[test]
    fn test_switch_insn_uses_src() {
        let insn = Instruction::switch_insn(
            PseudoId(5),
            vec![(0, BasicBlockId(1)), (1, BasicBlockId(2))],
            Some(BasicBlockId(3)),
            32,
        );
        assert!(insn.target.is_none());
        assert_eq!(insn.src.len(), 1);
        assert_eq!(insn.src[0], PseudoId(5));
        assert_eq!(insn.switch_cases.len(), 2);
        assert_eq!(insn.switch_default, Some(BasicBlockId(3)));
        let s = format!("{}", insn);
        assert!(s.contains("switch"));
        assert!(s.contains("%5"));
        assert!(s.contains("default"));
    }

    #[test]
    fn test_instruction_kill() {
        let types = TypeTable::new(&Target::host());
        let mut insn = Instruction::binop(
            Opcode::Add,
            PseudoId(3),
            PseudoId(1),
            PseudoId(2),
            types.int_id,
            32,
        );
        insn.phi_list.push((BasicBlockId(0), PseudoId(10)));
        assert_eq!(insn.op, Opcode::Add);
        assert!(insn.target.is_some());
        assert!(!insn.src.is_empty());
        assert!(!insn.phi_list.is_empty());
        insn.kill();
        assert_eq!(insn.op, Opcode::Nop);
        assert!(insn.target.is_none());
        assert!(insn.src.is_empty());
        assert!(insn.phi_list.is_empty());
    }

    #[test]
    fn test_function_const_val() {
        let target = Target::host();
        let types = TypeTable::new(&target);
        let mut func = Function::new("test_const_val", types.int_id);
        let val_id = func.create_const_pseudo(42);
        assert_eq!(func.const_val(val_id), Some(42));
        let reg_id = func.alloc_pseudo();
        let reg = Pseudo::reg(reg_id, reg_id.0);
        func.add_pseudo(reg);
        assert_eq!(func.const_val(reg_id), None);
        assert_eq!(func.const_val(PseudoId(9999)), None);
    }

    #[test]
    fn test_function_sym_name_of() {
        let target = Target::host();
        let types = TypeTable::new(&target);
        let mut func = Function::new("test_sym_name", types.int_id);
        let sym_id = func.alloc_pseudo();
        let sym = Pseudo::sym(sym_id, "my_symbol".to_string());
        func.add_pseudo(sym);
        assert_eq!(func.sym_name_of(sym_id), Some("my_symbol"));
        let reg_id = func.alloc_pseudo();
        let reg = Pseudo::reg(reg_id, reg_id.0);
        func.add_pseudo(reg);
        assert_eq!(func.sym_name_of(reg_id), None);
        assert_eq!(func.sym_name_of(PseudoId(9999)), None);
    }

    #[test]
    fn test_returns_via_sret() {
        let mut insn = Instruction::new(Opcode::Call);
        assert!(!insn.returns_via_sret());
        insn.abi_info = Some(Box::new(CallAbiInfo::new(
            vec![],
            ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits: 64,
            },
        )));
        assert!(!insn.returns_via_sret());
        insn.abi_info = Some(Box::new(CallAbiInfo::new(
            vec![],
            ArgClass::Indirect {
                align: 8,
                size_bits: 256,
            },
        )));
        assert!(insn.returns_via_sret());
    }

    #[test]
    fn test_returns_two_regs() {
        let mut insn = Instruction::new(Opcode::Ret);
        assert!(!insn.returns_two_regs());
        insn.abi_info = Some(Box::new(CallAbiInfo::new(
            vec![],
            ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits: 64,
            },
        )));
        assert!(!insn.returns_two_regs());
        insn.abi_info = Some(Box::new(CallAbiInfo::new(
            vec![],
            ArgClass::Direct {
                classes: vec![RegClass::Integer, RegClass::Integer],
                size_bits: 128,
            },
        )));
        assert!(insn.returns_two_regs());
    }

    // ========================================================================
    // Function::create_reg_pseudo
    // ========================================================================

    #[test]
    fn test_create_reg_pseudo() {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("test", types.int_id);
        func.next_pseudo = 10;

        let id1 = func.create_reg_pseudo();
        assert_eq!(id1, PseudoId(10));
        assert_eq!(func.next_pseudo, 11);

        // Verify the pseudo was registered
        let pseudo = func.get_pseudo(id1).expect("pseudo must exist");
        assert!(matches!(pseudo.kind, PseudoKind::Reg(_)));

        let id2 = func.create_reg_pseudo();
        assert_eq!(id2, PseudoId(11));
        assert_eq!(func.next_pseudo, 12);

        // IDs must be distinct
        assert_ne!(id1, id2);
    }

    // ========================================================================
    // Instruction::call_with_abi
    // ========================================================================

    #[test]
    fn test_call_with_abi_basic() {
        let target = Target::host();
        let types = TypeTable::new(&target);

        let insn = Instruction::call_with_abi(
            Some(PseudoId(2)),
            "__divti3",
            vec![PseudoId(0), PseudoId(1)],
            vec![types.int128_id, types.int128_id],
            types.int128_id,
            CallingConv::C,
            &types,
            &target,
        );

        assert_eq!(insn.op, Opcode::Call);
        assert_eq!(insn.target, Some(PseudoId(2)));
        assert_eq!(insn.func_name.as_deref(), Some("__divti3"));
        assert_eq!(insn.src.len(), 2);
        assert_eq!(insn.arg_types.len(), 2);
        assert!(insn.abi_info.is_some());

        let abi = insn.abi_info.as_ref().unwrap();
        assert_eq!(abi.params.len(), 2);
    }

    #[test]
    fn test_call_with_abi_conversion() {
        let target = Target::host();
        let types = TypeTable::new(&target);

        // float → signed int128 (__fixsfti)
        let insn = Instruction::call_with_abi(
            Some(PseudoId(1)),
            "__fixsfti",
            vec![PseudoId(0)],
            vec![types.float_id],
            types.int128_id,
            CallingConv::C,
            &types,
            &target,
        );

        assert_eq!(insn.op, Opcode::Call);
        assert_eq!(insn.func_name.as_deref(), Some("__fixsfti"));
        assert!(insn.abi_info.is_some());
        assert_eq!(insn.abi_info.as_ref().unwrap().params.len(), 1);
    }

    #[test]
    fn test_call_with_abi_sets_size() {
        let target = Target::host();
        let types = TypeTable::new(&target);

        let insn = Instruction::call_with_abi(
            Some(PseudoId(1)),
            "__addtf3",
            vec![PseudoId(0)],
            vec![types.double_id],
            types.double_id,
            CallingConv::C,
            &types,
            &target,
        );

        // Size should be set from ret_type
        assert_eq!(insn.size, types.size_bits(types.double_id));
    }
}
