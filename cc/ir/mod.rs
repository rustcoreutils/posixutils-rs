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

use crate::abi::ArgClass;
use crate::diag::Position;
use crate::types::TypeId;
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
/// is_sret_call/is_two_reg_return flags with richer type information.
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
    Phi,  // Phi node for SSA
    Copy, // Copy (for out-of-SSA)

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

    // Floating-point builtins
    Fabs32, // Absolute value of float
    Fabs64, // Absolute value of double

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
            Opcode::Fabs32 => "fabs32",
            Opcode::Fabs64 => "fabs64",
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
    Val(i64),
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
    pub fn val(id: PseudoId, value: i64) -> Self {
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
    /// For calls: true if this call returns a large struct via sret (hidden pointer arg).
    /// The first element of `src` is the sret pointer when this is true.
    pub is_sret_call: bool,
    /// For calls: true if the called function is noreturn (never returns).
    /// Code after a noreturn call is unreachable.
    pub is_noreturn_call: bool,
    /// For calls/returns: true if this returns a 9-16 byte struct via two registers
    /// (RAX+RDX on x86-64, X0+X1 on AArch64) per ABI.
    pub is_two_reg_return: bool,
    /// For indirect calls: pseudo containing the function pointer address.
    /// When this is Some, the call is indirect (call through function pointer).
    pub indirect_target: Option<PseudoId>,
    /// Source position for debug info
    pub pos: Option<Position>,
    /// For inline assembly: the asm data (template, operands, clobbers)
    pub asm_data: Option<Box<AsmData>>,
    /// For calls: rich ABI classification for arguments and return value.
    /// When present, this provides more detailed information than is_sret_call
    /// and is_two_reg_return, including per-argument register class info.
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
            is_sret_call: false,
            is_noreturn_call: false,
            is_two_reg_return: false,
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
            target: Some(value),
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

    /// Create an indirect call instruction (call through function pointer)
    pub fn call_indirect(
        target: Option<PseudoId>,
        func_addr: PseudoId,
        args: Vec<PseudoId>,
        arg_types: Vec<TypeId>,
        ret_type: TypeId,
        ret_size: u32,
    ) -> Self {
        let mut insn = Self::new(Opcode::Call).with_type_and_size(ret_type, ret_size);
        insn.indirect_target = Some(func_addr);
        insn.func_name = Some("<indirect>".to_string());
        if let Some(t) = target {
            insn.target = Some(t);
        }
        insn.src = args;
        insn.arg_types = arg_types;
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
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Format: target = op src1, src2
        if let Some(target) = &self.target {
            write!(f, "{} = ", target)?;
        }

        write!(f, "{}", self.op.name())?;

        // Size suffix
        if self.size > 0 {
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

    /// Remove phi entries for a specific predecessor
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
        self.blocks.push(block);
    }

    /// Get a block by ID
    pub fn get_block(&self, id: BasicBlockId) -> Option<&BasicBlock> {
        self.blocks.iter().find(|b| b.id == id)
    }

    /// Get a mutable block by ID
    pub fn get_block_mut(&mut self, id: BasicBlockId) -> Option<&mut BasicBlock> {
        self.blocks.iter_mut().find(|b| b.id == id)
    }

    /// Add a pseudo for tracking
    pub fn add_pseudo(&mut self, pseudo: Pseudo) {
        self.pseudos.push(pseudo);
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

    /// Create a new constant integer pseudo and return its ID
    /// The pseudo is added to self.pseudos
    pub fn create_const_pseudo(&mut self, value: i64) -> PseudoId {
        let id = self.alloc_pseudo();
        let pseudo = Pseudo::val(id, value);
        self.add_pseudo(pseudo);
        id
    }

    /// Get a pseudo by its ID
    pub fn get_pseudo(&self, id: PseudoId) -> Option<&Pseudo> {
        self.pseudos.iter().find(|p| p.id == id)
    }

    /// Check if block a dominates block b
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
    Int(i64),
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
            explicit_align: None,
        }
    }

    /// Create a thread-local global variable definition
    pub fn thread_local(name: impl Into<String>, typ: TypeId, init: Initializer) -> Self {
        Self {
            name: name.into(),
            typ,
            init,
            is_thread_local: true,
            explicit_align: None,
        }
    }

    /// Set explicit alignment from _Alignas specifier
    pub fn with_align(mut self, align: Option<u32>) -> Self {
        self.explicit_align = align;
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
    ) {
        let name = name.into();
        // Check for existing tentative definition
        if let Some(existing) = self.globals.iter_mut().find(|g| g.name == name) {
            // Replace tentative definition with actual definition
            if matches!(existing.init, Initializer::None) {
                // C99 6.9.2: Multiple declarations must have compatible types.
                // The parser should enforce this; assert here as a safety check.
                debug_assert_eq!(
                    existing.typ, typ,
                    "tentative definition type mismatch for '{}'",
                    name
                );
                existing.init = init;
                if align.is_some() {
                    existing.explicit_align = align;
                }
                return;
            }
        }
        self.globals
            .push(GlobalDef::new(name, typ, init).with_align(align));
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
    ) {
        let name = name.into();
        // Check for existing tentative definition
        if let Some(existing) = self.globals.iter_mut().find(|g| g.name == name) {
            // Replace tentative definition with actual definition
            if matches!(existing.init, Initializer::None) {
                // C99 6.9.2: Multiple declarations must have compatible types.
                // The parser should enforce this; assert here as a safety check.
                debug_assert_eq!(
                    existing.typ, typ,
                    "tentative definition type mismatch for '{}'",
                    name
                );
                existing.init = init;
                existing.is_thread_local = true;
                if align.is_some() {
                    existing.explicit_align = align;
                }
                return;
            }
        }
        self.globals
            .push(GlobalDef::thread_local(name, typ, init).with_align(align));
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
        module.add_global_aligned("x", types.int_id, Initializer::None, None);
        assert_eq!(module.globals.len(), 1);
        assert!(matches!(module.globals[0].init, Initializer::None));

        // Add actual definition - should replace the tentative one
        module.add_global_aligned("x", types.int_id, Initializer::Int(42), Some(4));
        assert_eq!(module.globals.len(), 1); // Still only one global
        assert!(matches!(module.globals[0].init, Initializer::Int(42)));
        assert_eq!(module.globals[0].explicit_align, Some(4));
    }

    #[test]
    fn test_add_global_aligned_non_tentative_not_replaced() {
        let types = TypeTable::new(&Target::host());
        let mut module = Module::new();

        // Add a real definition (with initializer)
        module.add_global_aligned("x", types.int_id, Initializer::Int(10), None);
        assert_eq!(module.globals.len(), 1);

        // Add another definition with same name - should NOT replace (adds new entry)
        module.add_global_aligned("x", types.int_id, Initializer::Int(20), None);
        assert_eq!(module.globals.len(), 2); // Two globals now (linker will error)
    }

    #[test]
    fn test_add_global_tls_aligned_tentative_definition() {
        let types = TypeTable::new(&Target::host());
        let mut module = Module::new();

        // Add a TLS tentative definition
        module.add_global_tls_aligned("tls_var", types.int_id, Initializer::None, None);
        assert_eq!(module.globals.len(), 1);
        assert!(matches!(module.globals[0].init, Initializer::None));

        // Add actual TLS definition - should replace
        module.add_global_tls_aligned("tls_var", types.int_id, Initializer::Int(100), Some(8));
        assert_eq!(module.globals.len(), 1);
        assert!(matches!(module.globals[0].init, Initializer::Int(100)));
        assert!(module.globals[0].is_thread_local);
        assert_eq!(module.globals[0].explicit_align, Some(8));
    }
}
