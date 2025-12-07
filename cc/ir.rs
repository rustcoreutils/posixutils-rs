//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Intermediate Representation (IR) for pcc C99 compiler
// Based on sparse's linearize.c SSA-style IR
//
// The IR uses Single Static Assignment (SSA) form where each variable
// is assigned exactly once. This simplifies dataflow analysis and
// optimization passes.
//

use crate::diag::Position;
use crate::types::TypeId;
use std::collections::HashMap;
use std::fmt;

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
// Opcodes - Following sparse's opcode.def
// ============================================================================

/// IR opcodes following sparse's design
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
    Select,  // Ternary select: cond ? a : b
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

    // Stack allocation builtin
    Alloca, // Dynamic stack allocation
}

impl Opcode {
    /// Check if this opcode is a terminator (ends a basic block)
    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            Opcode::Ret | Opcode::Br | Opcode::Cbr | Opcode::Switch
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
            Opcode::Alloca => "alloca",
        }
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
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
    /// Source position for debug info
    pub pos: Option<Position>,
}

impl Default for Instruction {
    fn default() -> Self {
        Self {
            op: Opcode::Nop,
            target: None,
            src: Vec::new(),
            typ: None,
            bb_true: None,
            bb_false: None,
            offset: 0,
            phi_list: Vec::new(),
            func_name: None,
            size: 0,
            src_size: 0,
            switch_cases: Vec::new(),
            switch_default: None,
            arg_types: Vec::new(),
            variadic_arg_start: None,
            is_sret_call: false,
            pos: None,
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

    /// Create a select (ternary) instruction
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
            insns: Vec::new(),
            parents: Vec::new(),
            children: Vec::new(),
            label: None,
            idom: None,
            dom_level: 0,
            dom_children: Vec::new(),
            dom_frontier: Vec::new(),
            phi_map: HashMap::new(),
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
    /// Block where this variable was declared (for scope-aware phi placement)
    /// Phi nodes for this variable should only be placed at blocks dominated by this block.
    pub decl_block: Option<BasicBlockId>,
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
    /// Local variables (name -> info), used for SSA conversion
    pub locals: HashMap<String, LocalVar>,
    /// Maximum dominator tree depth (computed by dominate.rs)
    pub max_dom_level: u32,
    /// Is this function static (internal linkage)?
    pub is_static: bool,
}

impl Default for Function {
    fn default() -> Self {
        Self {
            name: String::new(),
            return_type: TypeId::INVALID,
            params: Vec::new(),
            blocks: Vec::new(),
            entry: BasicBlockId(0),
            pseudos: Vec::new(),
            locals: HashMap::new(),
            max_dom_level: 0,
            is_static: false,
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
    pub fn add_local(
        &mut self,
        name: impl Into<String>,
        sym: PseudoId,
        typ: TypeId,
        is_volatile: bool,
        decl_block: Option<BasicBlockId>,
    ) {
        self.locals.insert(
            name.into(),
            LocalVar {
                sym,
                typ,
                is_volatile,
                decl_block,
            },
        );
    }

    /// Get a local variable
    pub fn get_local(&self, name: &str) -> Option<&LocalVar> {
        self.locals.get(name)
    }

    /// Compute the next available pseudo ID
    /// This scans all existing pseudos to find the maximum ID, then returns max + 1
    pub fn next_pseudo_id(&self) -> PseudoId {
        let max_id = self.pseudos.iter().map(|p| p.id.0).max().unwrap_or(0);
        PseudoId(max_id + 1)
    }

    /// Create a new constant integer pseudo and return its ID
    /// The pseudo is added to self.pseudos
    pub fn create_const_pseudo(&mut self, value: i64) -> PseudoId {
        let id = self.next_pseudo_id();
        let pseudo = Pseudo::val(id, value);
        self.add_pseudo(pseudo);
        id
    }

    /// Get a pseudo by its ID
    #[cfg(test)]
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
}

impl fmt::Display for Initializer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Initializer::None => write!(f, "0"),
            Initializer::Int(v) => write!(f, "{}", v),
            Initializer::Float(v) => write!(f, "{}", v),
        }
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
    /// Global variables (name, type, initializer)
    pub globals: Vec<(String, TypeId, Initializer)>,
    /// String literals (label, content)
    pub strings: Vec<(String, String)>,
    /// Generate debug info
    pub debug: bool,
    /// Source file paths (stream id -> path) for .file directives
    pub source_files: Vec<String>,
}

impl Module {
    /// Create a new module
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            globals: Vec::new(),
            strings: Vec::new(),
            debug: false,
            source_files: Vec::new(),
        }
    }

    /// Add a function
    pub fn add_function(&mut self, func: Function) {
        self.functions.push(func);
    }

    /// Add a global variable
    pub fn add_global(&mut self, name: impl Into<String>, typ: TypeId, init: Initializer) {
        self.globals.push((name.into(), typ, init));
    }

    /// Add a string literal and return its label
    pub fn add_string(&mut self, content: String) -> String {
        let label = format!(".LC{}", self.strings.len());
        self.strings.push((label.clone(), content));
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
        for (name, typ, init) in &self.globals {
            match init {
                Initializer::None => writeln!(f, "@{}: type#{}", name, typ.0)?,
                _ => writeln!(f, "@{}: type#{} = {}", name, typ.0, init)?,
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
        let types = TypeTable::new();
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
        let types = TypeTable::new();
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
        let types = TypeTable::new();
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
        let mut types = TypeTable::new();
        let char_ptr = types.intern(Type::pointer(types.char_id));
        let arg_types = vec![char_ptr, types.int_id];
        let call = Instruction::call(
            Some(PseudoId(1)),
            "printf",
            vec![PseudoId(2), PseudoId(3)],
            arg_types.clone(),
            types.int_id,
            32,
        );
        assert_eq!(call.op, Opcode::Call);
        assert_eq!(call.func_name, Some("printf".to_string()));
        assert_eq!(call.src.len(), 2);
        assert_eq!(call.arg_types.len(), 2);
    }

    #[test]
    fn test_load_store() {
        let types = TypeTable::new();

        let load = Instruction::load(PseudoId(1), PseudoId(2), 8, types.int_id, 32);
        assert_eq!(load.op, Opcode::Load);
        assert_eq!(load.offset, 8);

        let store = Instruction::store(PseudoId(1), PseudoId(2), 0, types.int_id, 32);
        assert_eq!(store.op, Opcode::Store);
        assert_eq!(store.src.len(), 2);
    }

    #[test]
    fn test_module() {
        let types = TypeTable::new();
        let mut module = Module::new();

        module.add_global("counter", types.int_id, Initializer::Int(0));

        let func = Function::new("main", types.int_id);
        module.add_function(func);

        assert_eq!(module.globals.len(), 1);
        assert_eq!(module.functions.len(), 1);
    }
}
