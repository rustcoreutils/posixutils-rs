//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 Code Generator
// Converts IR to x86-64 assembly (AT&T syntax)
//
// Uses linear scan register allocation and System V AMD64 ABI.
//

use crate::arch::codegen::CodeGenerator;
#[allow(unused_imports)]
use crate::arch::lir::{Directive, FpSize, Label, OperandSize, Symbol};
#[allow(unused_imports)]
use crate::arch::x86_64::lir::{
    CallTarget, GpOperand, IntCC, MemAddr, ShiftCount, X86Inst, XmmOperand,
};
use crate::ir::{Function, Initializer, Instruction, Module, Opcode, Pseudo, PseudoId, PseudoKind};
use crate::target::Target;
use crate::types::{Type, TypeModifiers};
use std::collections::HashMap;

// ============================================================================
// x86-64 Register Definitions
// ============================================================================

/// x86-64 physical registers
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reg {
    // 64-bit general purpose registers
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    Rbp,
    Rsp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Reg {
    /// Get AT&T syntax name for 64-bit register
    pub fn name64(&self) -> &'static str {
        match self {
            Reg::Rax => "%rax",
            Reg::Rbx => "%rbx",
            Reg::Rcx => "%rcx",
            Reg::Rdx => "%rdx",
            Reg::Rsi => "%rsi",
            Reg::Rdi => "%rdi",
            Reg::Rbp => "%rbp",
            Reg::Rsp => "%rsp",
            Reg::R8 => "%r8",
            Reg::R9 => "%r9",
            Reg::R10 => "%r10",
            Reg::R11 => "%r11",
            Reg::R12 => "%r12",
            Reg::R13 => "%r13",
            Reg::R14 => "%r14",
            Reg::R15 => "%r15",
        }
    }

    /// Get AT&T syntax name for 32-bit register
    pub fn name32(&self) -> &'static str {
        match self {
            Reg::Rax => "%eax",
            Reg::Rbx => "%ebx",
            Reg::Rcx => "%ecx",
            Reg::Rdx => "%edx",
            Reg::Rsi => "%esi",
            Reg::Rdi => "%edi",
            Reg::Rbp => "%ebp",
            Reg::Rsp => "%esp",
            Reg::R8 => "%r8d",
            Reg::R9 => "%r9d",
            Reg::R10 => "%r10d",
            Reg::R11 => "%r11d",
            Reg::R12 => "%r12d",
            Reg::R13 => "%r13d",
            Reg::R14 => "%r14d",
            Reg::R15 => "%r15d",
        }
    }

    /// Get AT&T syntax name for 16-bit register
    pub fn name16(&self) -> &'static str {
        match self {
            Reg::Rax => "%ax",
            Reg::Rbx => "%bx",
            Reg::Rcx => "%cx",
            Reg::Rdx => "%dx",
            Reg::Rsi => "%si",
            Reg::Rdi => "%di",
            Reg::Rbp => "%bp",
            Reg::Rsp => "%sp",
            Reg::R8 => "%r8w",
            Reg::R9 => "%r9w",
            Reg::R10 => "%r10w",
            Reg::R11 => "%r11w",
            Reg::R12 => "%r12w",
            Reg::R13 => "%r13w",
            Reg::R14 => "%r14w",
            Reg::R15 => "%r15w",
        }
    }

    /// Get AT&T syntax name for 8-bit register (low byte)
    pub fn name8(&self) -> &'static str {
        match self {
            Reg::Rax => "%al",
            Reg::Rbx => "%bl",
            Reg::Rcx => "%cl",
            Reg::Rdx => "%dl",
            Reg::Rsi => "%sil",
            Reg::Rdi => "%dil",
            Reg::Rbp => "%bpl",
            Reg::Rsp => "%spl",
            Reg::R8 => "%r8b",
            Reg::R9 => "%r9b",
            Reg::R10 => "%r10b",
            Reg::R11 => "%r11b",
            Reg::R12 => "%r12b",
            Reg::R13 => "%r13b",
            Reg::R14 => "%r14b",
            Reg::R15 => "%r15b",
        }
    }

    /// Get register name for a given bit size
    pub fn name_for_size(&self, bits: u32) -> &'static str {
        match bits {
            8 => self.name8(),
            16 => self.name16(),
            32 => self.name32(),
            _ => self.name64(),
        }
    }

    /// Is this a callee-saved register?
    pub fn is_callee_saved(&self) -> bool {
        matches!(
            self,
            Reg::Rbx | Reg::Rbp | Reg::R12 | Reg::R13 | Reg::R14 | Reg::R15
        )
    }

    /// Argument registers in order (System V AMD64 ABI)
    pub fn arg_regs() -> &'static [Reg] {
        &[Reg::Rdi, Reg::Rsi, Reg::Rdx, Reg::Rcx, Reg::R8, Reg::R9]
    }

    /// All allocatable registers (excluding RSP, RBP, and R10)
    /// R10 is reserved as scratch for division instructions
    pub fn allocatable() -> &'static [Reg] {
        &[
            Reg::Rax,
            Reg::Rbx,
            Reg::Rcx,
            Reg::Rdx,
            Reg::Rsi,
            Reg::Rdi,
            Reg::R8,
            Reg::R9,
            // R10 is reserved for division scratch
            Reg::R11,
            Reg::R12,
            Reg::R13,
            Reg::R14,
            Reg::R15,
        ]
    }

    /// Stack pointer register
    pub fn sp() -> Reg {
        Reg::Rsp
    }

    /// Base/frame pointer register
    pub fn bp() -> Reg {
        Reg::Rbp
    }
}

// ============================================================================
// XMM Register Definitions (SSE/FP)
// ============================================================================

/// x86-64 XMM registers for floating-point operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum XmmReg {
    Xmm0,
    Xmm1,
    Xmm2,
    Xmm3,
    Xmm4,
    Xmm5,
    Xmm6,
    Xmm7,
    Xmm8,
    Xmm9,
    Xmm10,
    Xmm11,
    Xmm12,
    Xmm13,
    Xmm14,
    Xmm15,
}

impl XmmReg {
    /// Get AT&T syntax name for XMM register
    pub fn name(&self) -> &'static str {
        match self {
            XmmReg::Xmm0 => "%xmm0",
            XmmReg::Xmm1 => "%xmm1",
            XmmReg::Xmm2 => "%xmm2",
            XmmReg::Xmm3 => "%xmm3",
            XmmReg::Xmm4 => "%xmm4",
            XmmReg::Xmm5 => "%xmm5",
            XmmReg::Xmm6 => "%xmm6",
            XmmReg::Xmm7 => "%xmm7",
            XmmReg::Xmm8 => "%xmm8",
            XmmReg::Xmm9 => "%xmm9",
            XmmReg::Xmm10 => "%xmm10",
            XmmReg::Xmm11 => "%xmm11",
            XmmReg::Xmm12 => "%xmm12",
            XmmReg::Xmm13 => "%xmm13",
            XmmReg::Xmm14 => "%xmm14",
            XmmReg::Xmm15 => "%xmm15",
        }
    }

    /// Floating-point argument registers (System V AMD64 ABI)
    pub fn arg_regs() -> &'static [XmmReg] {
        &[
            XmmReg::Xmm0,
            XmmReg::Xmm1,
            XmmReg::Xmm2,
            XmmReg::Xmm3,
            XmmReg::Xmm4,
            XmmReg::Xmm5,
            XmmReg::Xmm6,
            XmmReg::Xmm7,
        ]
    }

    /// All allocatable XMM registers
    /// XMM0-XMM7 are caller-saved (scratch), XMM8-XMM15 are callee-saved on Windows but not on System V
    /// XMM14 and XMM15 are reserved as scratch registers for codegen operations
    pub fn allocatable() -> &'static [XmmReg] {
        &[
            XmmReg::Xmm0,
            XmmReg::Xmm1,
            XmmReg::Xmm2,
            XmmReg::Xmm3,
            XmmReg::Xmm4,
            XmmReg::Xmm5,
            XmmReg::Xmm6,
            XmmReg::Xmm7,
            XmmReg::Xmm8,
            XmmReg::Xmm9,
            XmmReg::Xmm10,
            XmmReg::Xmm11,
            XmmReg::Xmm12,
            XmmReg::Xmm13,
            // XMM14 and XMM15 are reserved for scratch use in codegen
        ]
    }
}

// ============================================================================
// Operand - Location of a value (register or memory)
// ============================================================================

/// Location of a value
#[derive(Debug, Clone)]
pub enum Loc {
    /// In a general-purpose register
    Reg(Reg),
    /// In an XMM register (floating-point)
    Xmm(XmmReg),
    /// On the stack at [rbp - offset]
    Stack(i32),
    /// Immediate integer constant
    Imm(i64),
    /// Immediate float constant (value, size in bits)
    FImm(f64, u32),
    /// Global symbol
    Global(String),
}

// ============================================================================
// Register Allocator (Linear Scan)
// ============================================================================

/// Live interval for a pseudo
#[derive(Debug, Clone)]
struct LiveInterval {
    pseudo: PseudoId,
    start: usize,
    end: usize,
}

/// Simple linear scan register allocator for x86-64
pub struct RegAlloc {
    /// Mapping from pseudo to location
    locations: HashMap<PseudoId, Loc>,
    /// Free general-purpose registers
    free_regs: Vec<Reg>,
    /// Free XMM registers (for floating-point)
    free_xmm_regs: Vec<XmmReg>,
    /// Active integer register intervals (sorted by end point)
    active: Vec<(LiveInterval, Reg)>,
    /// Active XMM register intervals (sorted by end point)
    active_xmm: Vec<(LiveInterval, XmmReg)>,
    /// Next stack slot offset
    stack_offset: i32,
    /// Callee-saved registers that were used
    used_callee_saved: Vec<Reg>,
    /// Track which pseudos need FP registers (based on type)
    fp_pseudos: std::collections::HashSet<PseudoId>,
}

impl RegAlloc {
    pub fn new() -> Self {
        Self {
            locations: HashMap::new(),
            free_regs: Reg::allocatable().to_vec(),
            free_xmm_regs: XmmReg::allocatable().to_vec(),
            active: Vec::new(),
            active_xmm: Vec::new(),
            stack_offset: 0,
            used_callee_saved: Vec::new(),
            fp_pseudos: std::collections::HashSet::new(),
        }
    }

    /// Perform register allocation for a function
    pub fn allocate(&mut self, func: &Function) -> HashMap<PseudoId, Loc> {
        // Reset state
        self.locations.clear();
        self.free_regs = Reg::allocatable().to_vec();
        self.free_xmm_regs = XmmReg::allocatable().to_vec();
        self.active.clear();
        self.active_xmm.clear();
        self.stack_offset = 0;
        self.used_callee_saved.clear();
        self.fp_pseudos.clear();

        // Scan instructions to identify which pseudos need FP registers
        self.identify_fp_pseudos(func);

        // Pre-allocate argument registers
        // System V AMD64 ABI: integer args in RDI, RSI, RDX, RCX, R8, R9
        //                     FP args in XMM0-XMM7
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = XmmReg::arg_regs();
        let mut int_arg_idx = 0;
        let mut fp_arg_idx = 0;

        // Detect if there's a hidden return pointer (for functions returning large structs)
        // The __sret pseudo has arg_idx=0 and shifts all other arg indices by 1
        let sret_pseudo = func
            .pseudos
            .iter()
            .find(|p| matches!(p.kind, PseudoKind::Arg(0)) && p.name.as_deref() == Some("__sret"));
        let arg_idx_offset: u32 = if sret_pseudo.is_some() { 1 } else { 0 };

        // If there's a hidden return pointer, allocate RDI for it
        if let Some(sret) = sret_pseudo {
            self.locations.insert(sret.id, Loc::Reg(int_arg_regs[0]));
            self.free_regs.retain(|&r| r != int_arg_regs[0]);
            int_arg_idx += 1;
        }

        for (i, (_name, typ)) in func.params.iter().enumerate() {
            // Find the pseudo for this argument
            for pseudo in &func.pseudos {
                if let PseudoKind::Arg(arg_idx) = pseudo.kind {
                    if arg_idx == (i as u32) + arg_idx_offset {
                        let is_fp = typ.is_float();
                        if is_fp {
                            // FP argument
                            if fp_arg_idx < fp_arg_regs.len() {
                                self.locations
                                    .insert(pseudo.id, Loc::Xmm(fp_arg_regs[fp_arg_idx]));
                                self.free_xmm_regs.retain(|&r| r != fp_arg_regs[fp_arg_idx]);
                                self.fp_pseudos.insert(pseudo.id);
                            } else {
                                // FP arg on stack
                                let offset =
                                    16 + (i - int_arg_regs.len() - fp_arg_regs.len()) as i32 * 8;
                                self.locations.insert(pseudo.id, Loc::Stack(-offset));
                            }
                            fp_arg_idx += 1;
                        } else {
                            // Integer argument
                            if int_arg_idx < int_arg_regs.len() {
                                self.locations
                                    .insert(pseudo.id, Loc::Reg(int_arg_regs[int_arg_idx]));
                                self.free_regs.retain(|&r| r != int_arg_regs[int_arg_idx]);
                            } else {
                                // Integer arg on stack
                                let offset = 16 + (i - int_arg_regs.len()) as i32 * 8;
                                self.locations.insert(pseudo.id, Loc::Stack(-offset));
                            }
                            int_arg_idx += 1;
                        }
                        break;
                    }
                }
            }
        }

        // Compute live intervals (simplified: just walk instructions)
        let intervals = self.compute_live_intervals(func);

        // Find all call positions - arguments in caller-saved registers that live across
        // calls need to be spilled to stack
        let mut call_positions: Vec<usize> = Vec::new();
        let mut pos = 0usize;
        for block in &func.blocks {
            for insn in &block.insns {
                if insn.op == Opcode::Call {
                    call_positions.push(pos);
                }
                pos += 1;
            }
        }

        // Check arguments in caller-saved registers - if their interval crosses a call,
        // spill them to the stack instead
        let int_arg_regs_set: Vec<Reg> = Reg::arg_regs().to_vec();
        for interval in &intervals {
            if let Some(Loc::Reg(reg)) = self.locations.get(&interval.pseudo) {
                // Check if this is an argument register (caller-saved)
                if int_arg_regs_set.contains(reg) {
                    // Check if interval crosses any call
                    let crosses_call = call_positions
                        .iter()
                        .any(|&call_pos| interval.start <= call_pos && call_pos < interval.end);
                    if crosses_call {
                        // Spill to stack - remove from register, allocate stack slot
                        let reg_to_restore = *reg;
                        self.stack_offset += 8;
                        self.locations
                            .insert(interval.pseudo, Loc::Stack(self.stack_offset));
                        // Restore the register to free list for other uses
                        self.free_regs.push(reg_to_restore);
                    }
                }
            }
        }

        // Force alloca results to the stack. Alloca results typically have long live
        // ranges (used across multiple loop iterations) and putting them in registers
        // leads to clobbering issues when the codegen needs temp registers.
        for block in &func.blocks {
            for insn in &block.insns {
                if insn.op == Opcode::Alloca {
                    if let Some(target) = insn.target {
                        self.stack_offset += 8;
                        self.locations.insert(target, Loc::Stack(self.stack_offset));
                    }
                }
            }
        }

        // Allocate registers using linear scan
        for interval in intervals {
            self.expire_old_intervals(interval.start);

            // Check if already allocated (e.g., arguments)
            if self.locations.contains_key(&interval.pseudo) {
                continue;
            }

            // Check if this is a constant - constants don't need registers
            if let Some(pseudo) = func.pseudos.iter().find(|p| p.id == interval.pseudo) {
                match &pseudo.kind {
                    PseudoKind::Val(v) => {
                        self.locations.insert(interval.pseudo, Loc::Imm(*v));
                        continue;
                    }
                    PseudoKind::FVal(v) => {
                        // Float constants are stored as FImm
                        // Find the size from the SetVal instruction that defines this constant
                        let size = func
                            .blocks
                            .iter()
                            .flat_map(|b| &b.insns)
                            .find(|insn| {
                                insn.op == crate::ir::Opcode::SetVal
                                    && insn.target == Some(interval.pseudo)
                            })
                            .map(|insn| insn.size)
                            .unwrap_or(64); // Default to double if not found
                        self.locations.insert(interval.pseudo, Loc::FImm(*v, size));
                        self.fp_pseudos.insert(interval.pseudo);
                        continue;
                    }
                    PseudoKind::Sym(name) => {
                        // Check if this is a local variable or a global symbol
                        if let Some(local_var) = func.locals.get(name) {
                            // Local variable - allocate stack space based on type size
                            let size = (local_var.typ.size_bits() / 8) as i32;
                            let size = std::cmp::max(size, 8); // Minimum 8 bytes
                                                               // Align to 8 bytes
                            let aligned_size = (size + 7) & !7;
                            self.stack_offset += aligned_size;
                            self.locations
                                .insert(interval.pseudo, Loc::Stack(self.stack_offset));
                            // Mark as FP if the type is float
                            if local_var.typ.is_float() {
                                self.fp_pseudos.insert(interval.pseudo);
                            }
                        } else {
                            // Global symbol
                            self.locations
                                .insert(interval.pseudo, Loc::Global(name.clone()));
                        }
                        continue;
                    }
                    _ => {}
                }
            }

            // Determine if this pseudo needs an FP register
            let needs_fp = self.fp_pseudos.contains(&interval.pseudo);

            if needs_fp {
                // Try to allocate an XMM register
                if let Some(xmm) = self.free_xmm_regs.pop() {
                    self.locations.insert(interval.pseudo, Loc::Xmm(xmm));
                    self.active_xmm.push((interval.clone(), xmm));
                    self.active_xmm.sort_by_key(|(i, _)| i.end);
                } else {
                    // Spill to stack
                    self.stack_offset += 8;
                    self.locations
                        .insert(interval.pseudo, Loc::Stack(self.stack_offset));
                }
            } else {
                // Try to allocate a general-purpose register
                if let Some(reg) = self.free_regs.pop() {
                    if reg.is_callee_saved() && !self.used_callee_saved.contains(&reg) {
                        self.used_callee_saved.push(reg);
                    }
                    self.locations.insert(interval.pseudo, Loc::Reg(reg));
                    self.active.push((interval.clone(), reg));
                    // Keep active sorted by end point
                    self.active.sort_by_key(|(i, _)| i.end);
                } else {
                    // Spill to stack
                    self.stack_offset += 8;
                    self.locations
                        .insert(interval.pseudo, Loc::Stack(self.stack_offset));
                }
            }
        }

        self.locations.clone()
    }

    /// Scan function to identify which pseudos need FP registers
    fn identify_fp_pseudos(&mut self, func: &Function) {
        for block in &func.blocks {
            for insn in &block.insns {
                // Check instruction type - FP instructions produce FP results
                let is_fp_op = matches!(
                    insn.op,
                    Opcode::FAdd
                        | Opcode::FSub
                        | Opcode::FMul
                        | Opcode::FDiv
                        | Opcode::FNeg
                        | Opcode::FCmpOEq
                        | Opcode::FCmpONe
                        | Opcode::FCmpOLt
                        | Opcode::FCmpOLe
                        | Opcode::FCmpOGt
                        | Opcode::FCmpOGe
                        | Opcode::UCvtF
                        | Opcode::SCvtF
                        | Opcode::FCvtF
                );

                // Mark target pseudo as FP if this is an FP operation
                // (except comparisons which produce int)
                if is_fp_op
                    && !matches!(
                        insn.op,
                        Opcode::FCmpOEq
                            | Opcode::FCmpONe
                            | Opcode::FCmpOLt
                            | Opcode::FCmpOLe
                            | Opcode::FCmpOGt
                            | Opcode::FCmpOGe
                    )
                {
                    if let Some(target) = insn.target {
                        self.fp_pseudos.insert(target);
                    }
                }

                // Also check the type if available
                // (but exclude comparisons which always produce int regardless of operand type)
                if let Some(ref typ) = insn.typ {
                    if typ.is_float()
                        && !matches!(
                            insn.op,
                            Opcode::FCmpOEq
                                | Opcode::FCmpONe
                                | Opcode::FCmpOLt
                                | Opcode::FCmpOLe
                                | Opcode::FCmpOGt
                                | Opcode::FCmpOGe
                        )
                    {
                        if let Some(target) = insn.target {
                            self.fp_pseudos.insert(target);
                        }
                    }
                }
            }
        }
    }

    fn expire_old_intervals(&mut self, point: usize) {
        // Expire integer register intervals
        let mut to_remove = Vec::new();
        for (i, (interval, reg)) in self.active.iter().enumerate() {
            if interval.end < point {
                self.free_regs.push(*reg);
                to_remove.push(i);
            }
        }
        // Remove in reverse order to preserve indices
        for i in to_remove.into_iter().rev() {
            self.active.remove(i);
        }

        // Expire XMM register intervals
        let mut to_remove_xmm = Vec::new();
        for (i, (interval, xmm)) in self.active_xmm.iter().enumerate() {
            if interval.end < point {
                self.free_xmm_regs.push(*xmm);
                to_remove_xmm.push(i);
            }
        }
        for i in to_remove_xmm.into_iter().rev() {
            self.active_xmm.remove(i);
        }
    }

    fn compute_live_intervals(&self, func: &Function) -> Vec<LiveInterval> {
        use crate::ir::BasicBlockId;

        // Track earliest definition, latest definition, and latest use for each pseudo
        struct IntervalInfo {
            pseudo: PseudoId,
            first_def: usize,
            last_def: usize,
            last_use: usize,
        }

        let mut intervals: HashMap<PseudoId, IntervalInfo> = HashMap::new();
        let mut pos = 0usize;

        // First pass: compute block end positions
        let mut block_end_pos: HashMap<BasicBlockId, usize> = HashMap::new();
        let mut temp_pos = 0usize;
        for block in &func.blocks {
            temp_pos += block.insns.len();
            block_end_pos.insert(block.id, temp_pos.saturating_sub(1));
        }

        // Collect phi sources with their source blocks for later processing
        // Also track phi targets - they need intervals extended to cover all their copy definitions
        let mut phi_sources: Vec<(BasicBlockId, PseudoId)> = Vec::new();
        let mut phi_targets: Vec<(BasicBlockId, PseudoId)> = Vec::new();

        for block in &func.blocks {
            for insn in &block.insns {
                // Definition point - track both first and last definition
                // This is important because phi elimination creates multiple definitions
                // of the same pseudo (via Copy instructions in predecessor blocks)
                if let Some(target) = insn.target {
                    intervals
                        .entry(target)
                        .and_modify(|info| {
                            info.first_def = info.first_def.min(pos);
                            info.last_def = info.last_def.max(pos);
                        })
                        .or_insert(IntervalInfo {
                            pseudo: target,
                            first_def: pos,
                            last_def: pos,
                            last_use: pos,
                        });
                }

                // Use points
                for &src in &insn.src {
                    if let Some(info) = intervals.get_mut(&src) {
                        info.last_use = info.last_use.max(pos);
                    } else {
                        intervals.insert(
                            src,
                            IntervalInfo {
                                pseudo: src,
                                first_def: pos,
                                last_def: pos,
                                last_use: pos,
                            },
                        );
                    }
                }

                // Collect phi sources - they need to be live at the END of their source block
                // Also collect phi targets - they have Copy definitions in each source block
                for (src_bb, pseudo) in &insn.phi_list {
                    phi_sources.push((*src_bb, *pseudo));
                    // The phi target (if present) is defined via Copy at the end of each source block
                    if let Some(target) = insn.target {
                        phi_targets.push((*src_bb, target));
                    }
                }

                pos += 1;
            }
        }

        // Process phi sources: extend their live interval to the end of their source block
        // This is critical for loops where phi sources come from later blocks via back edges
        // NOTE: We only extend last_use, NOT last_def, because phi sources are USED (not defined)
        // at the copy point. Setting last_def incorrectly can confuse the interval calculation.
        for (src_bb, pseudo) in phi_sources {
            if let Some(&end_pos) = block_end_pos.get(&src_bb) {
                if let Some(info) = intervals.get_mut(&pseudo) {
                    info.last_use = info.last_use.max(end_pos);
                } else {
                    intervals.insert(
                        pseudo,
                        IntervalInfo {
                            pseudo,
                            first_def: end_pos,
                            last_def: end_pos,
                            last_use: end_pos,
                        },
                    );
                }
            }
        }

        // Process phi targets: extend their live interval to cover all Copy definitions
        // The phi target is defined via Copy at the end of each source block
        // For loops, this means the target must be live until the last Copy (at the loop back edge)
        for (src_bb, target) in phi_targets {
            if let Some(&end_pos) = block_end_pos.get(&src_bb) {
                if let Some(info) = intervals.get_mut(&target) {
                    info.last_def = info.last_def.max(end_pos);
                } else {
                    intervals.insert(
                        target,
                        IntervalInfo {
                            pseudo: target,
                            first_def: end_pos,
                            last_def: end_pos,
                            last_use: end_pos,
                        },
                    );
                }
            }
        }

        // Find the maximum position in the function
        let max_pos = pos.saturating_sub(1);

        // Convert to LiveInterval
        // The interval spans from first_def to max(last_def, last_use)
        // This ensures that pseudos with multiple definitions (from phi copies)
        // stay live across all their definitions
        //
        // IMPORTANT: For loop variables, if last_def > last_use (definition comes after use
        // due to back edge), the variable must stay live until the end of the function
        // because it will be used again in the next loop iteration.
        let mut result: Vec<_> = intervals
            .into_values()
            .map(|info| {
                let end = if info.last_def > info.last_use {
                    // Loop variable: definition after use means we wrap around
                    // Extend to end of function to ensure it stays live
                    max_pos
                } else {
                    info.last_def.max(info.last_use)
                };
                LiveInterval {
                    pseudo: info.pseudo,
                    start: info.first_def,
                    end,
                }
            })
            .collect();
        result.sort_by_key(|i| i.start);
        result
    }

    /// Get stack size needed (aligned to 16 bytes)
    pub fn stack_size(&self) -> i32 {
        // Align to 16 bytes
        (self.stack_offset + 15) & !15
    }

    /// Get callee-saved registers that need to be preserved
    pub fn callee_saved_used(&self) -> &[Reg] {
        &self.used_callee_saved
    }
}

impl Default for RegAlloc {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// x86-64 Code Generator
// ============================================================================

/// x86-64 code generator
pub struct X86_64CodeGen {
    /// Target info
    target: Target,
    /// Output buffer
    output: String,
    /// LIR instruction buffer (for deferred emission and future peephole optimization)
    lir_buffer: Vec<X86Inst>,
    /// Current function's register allocation
    locations: HashMap<PseudoId, Loc>,
    /// Current function's pseudos (for looking up values)
    pseudos: Vec<Pseudo>,
    /// Current function name (for generating unique labels)
    current_fn: String,
    /// Callee-saved registers used in current function (for epilogue)
    callee_saved_regs: Vec<Reg>,
    /// Offset to add to stack locations to account for callee-saved registers
    callee_saved_offset: i32,
    /// Offset from rbp to register save area (for variadic functions)
    reg_save_area_offset: i32,
    /// Number of fixed GP parameters (for variadic functions)
    num_fixed_gp_params: usize,
    /// Whether to emit basic unwind tables (cfi_startproc/cfi_endproc)
    emit_unwind_tables: bool,
    /// Last emitted source line (for avoiding duplicate .loc directives)
    last_debug_line: u32,
    /// Last emitted source file index
    last_debug_file: u16,
    /// Whether to emit debug info (.file/.loc directives)
    emit_debug: bool,
    /// Counter for generating unique internal labels
    unique_label_counter: u32,
}

impl X86_64CodeGen {
    pub fn new(target: Target) -> Self {
        Self {
            target,
            output: String::new(),
            lir_buffer: Vec::new(),
            locations: HashMap::new(),
            pseudos: Vec::new(),
            current_fn: String::new(),
            callee_saved_regs: Vec::new(),
            callee_saved_offset: 0,
            reg_save_area_offset: 0,
            num_fixed_gp_params: 0,
            emit_unwind_tables: true, // Default to emitting basic unwind tables
            last_debug_line: 0,
            last_debug_file: 0,
            emit_debug: false,
            unique_label_counter: 0,
        }
    }

    /// Push a LIR instruction to the buffer (deferred emission)
    fn push_lir(&mut self, inst: X86Inst) {
        self.lir_buffer.push(inst);
    }

    /// Emit all buffered LIR instructions to the output string
    fn emit_all(&mut self) {
        use crate::arch::lir::EmitAsm;
        for inst in &self.lir_buffer {
            inst.emit(&self.target, &mut self.output);
        }
    }

    /// Convert a Loc to a GpOperand for LIR
    fn loc_to_gp_operand(&self, loc: &Loc) -> GpOperand {
        match loc {
            Loc::Reg(r) => GpOperand::Reg(*r),
            Loc::Stack(offset) => {
                let adjusted = *offset + self.callee_saved_offset;
                GpOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: -adjusted,
                })
            }
            Loc::Imm(v) => GpOperand::Imm(*v),
            Loc::FImm(_, _) => GpOperand::Imm(0), // FP immediates handled separately
            Loc::Xmm(_) => GpOperand::Imm(0),     // XMM handled separately
            Loc::Global(name) => GpOperand::Mem(MemAddr::RipRelative(Symbol::global(name.clone()))),
        }
    }

    /// Emit .loc directive for source line tracking (if debug is enabled and line changed)
    fn emit_loc(&mut self, insn: &Instruction) {
        if !self.emit_debug {
            return;
        }
        if let Some(pos) = &insn.pos {
            // Only emit if line changed (avoid duplicate .loc directives)
            let file = pos.stream + 1; // DWARF file indices start at 1
            let line = pos.line;
            if line != self.last_debug_line || file != self.last_debug_file {
                // LIR: .loc directive for debug info
                self.push_lir(X86Inst::Directive(Directive::loc(
                    file.into(),
                    line,
                    pos.col.into(),
                )));
                self.last_debug_line = line;
                self.last_debug_file = file;
            }
        }
    }

    fn emit_header(&mut self) {
        // Header comment and initial text section
        self.push_lir(X86Inst::Directive(Directive::Comment(
            "Generated by pcc (x86-64)".into(),
        )));
        self.push_lir(X86Inst::Directive(Directive::Text));
    }

    fn emit_global(&mut self, name: &str, typ: &Type, init: &Initializer) {
        let size = typ.size_bits() / 8;
        let size = if size == 0 { 8 } else { size }; // Default to 8 bytes

        // Check storage class - skip .globl for static
        let is_static = typ.modifiers.contains(TypeModifiers::STATIC);

        // Get alignment from type info
        let align = typ.alignment() as u32;

        // Use .comm for uninitialized external (non-static) globals
        let use_bss = matches!(init, Initializer::None) && !is_static;

        if use_bss {
            // Use .comm for uninitialized external globals
            self.push_lir(X86Inst::Directive(Directive::comm(name, size, align)));
            return;
        }

        // Data section
        self.push_lir(X86Inst::Directive(Directive::Data));

        // Global visibility (if not static)
        if !is_static {
            self.push_lir(X86Inst::Directive(Directive::global(name)));
        }

        // ELF-only type and size (handled by Directive::emit which skips on macOS)
        self.push_lir(X86Inst::Directive(Directive::type_object(name)));
        self.push_lir(X86Inst::Directive(Directive::size(name, size)));

        // Alignment
        if align > 1 {
            self.push_lir(X86Inst::Directive(Directive::Align(align.trailing_zeros())));
        }

        // Label
        self.push_lir(X86Inst::Directive(Directive::global_label(name)));

        // Emit initializer
        match init {
            Initializer::None => {
                // For static uninitialized, use .zero (not .comm)
                self.push_lir(X86Inst::Directive(Directive::Zero(size)));
            }
            Initializer::Int(val) => match size {
                1 => self.push_lir(X86Inst::Directive(Directive::Byte(*val))),
                2 => self.push_lir(X86Inst::Directive(Directive::Short(*val))),
                4 => self.push_lir(X86Inst::Directive(Directive::Long(*val))),
                _ => self.push_lir(X86Inst::Directive(Directive::Quad(*val))),
            },
            Initializer::Float(val) => {
                if size == 4 {
                    // float - emit as 32-bit IEEE 754
                    let bits = (*val as f32).to_bits();
                    self.push_lir(X86Inst::Directive(Directive::Long(bits as i64)));
                } else {
                    // double - emit as 64-bit IEEE 754
                    let bits = val.to_bits();
                    self.push_lir(X86Inst::Directive(Directive::Quad(bits as i64)));
                }
            }
        }
    }

    /// Check if a function contains va_start (indicating it's variadic)
    fn is_variadic_function(func: &Function) -> bool {
        for block in &func.blocks {
            for insn in &block.insns {
                if matches!(insn.op, crate::ir::Opcode::VaStart) {
                    return true;
                }
            }
        }
        false
    }

    fn emit_function(&mut self, func: &Function) {
        // Save current function name for unique label generation
        self.current_fn = func.name.clone();

        // Check if this function uses varargs
        let is_variadic = Self::is_variadic_function(func);

        // Register allocation
        let mut alloc = RegAlloc::new();
        self.locations = alloc.allocate(func);
        self.pseudos = func.pseudos.clone();

        let stack_size = alloc.stack_size();
        self.callee_saved_regs = alloc.callee_saved_used().to_vec();
        self.callee_saved_offset = self.callee_saved_regs.len() as i32 * 8;

        // For variadic functions, we need extra space for the register save area
        // 6 GP regs * 8 bytes = 48 bytes for GP registers
        // (We don't save XMM registers in this implementation for simplicity)
        let reg_save_area_size: i32 = if is_variadic { 48 } else { 0 };
        self.reg_save_area_offset = if is_variadic {
            // The register save area will be at the end of the stack frame
            self.callee_saved_offset + stack_size + reg_save_area_size
        } else {
            0
        };

        // Function prologue
        self.push_lir(X86Inst::Directive(Directive::Blank));
        self.push_lir(X86Inst::Directive(Directive::Text));

        // Skip .globl for static functions (internal linkage)
        if !func.is_static {
            self.push_lir(X86Inst::Directive(Directive::global(&func.name)));
        }

        // ELF-only type (handled by Directive::emit which skips on macOS)
        self.push_lir(X86Inst::Directive(Directive::type_func(&func.name)));

        // Function label
        self.push_lir(X86Inst::Directive(Directive::global_label(&func.name)));

        // CFI: Start procedure (enables stack unwinding for this function)
        if self.emit_unwind_tables {
            self.push_lir(X86Inst::Directive(Directive::CfiStartProc));
        }

        // Prologue: save frame pointer and allocate stack
        let bp = Reg::bp();
        let sp = Reg::sp();
        self.push_lir(X86Inst::Push {
            src: GpOperand::Reg(bp),
        });
        if self.emit_debug {
            // After pushq %rbp: CFA is now at %rsp+16, and %rbp is saved at CFA-16
            self.push_lir(X86Inst::Directive(Directive::CfiDefCfaOffset(16)));
            self.push_lir(X86Inst::Directive(Directive::cfi_offset("%rbp", -16)));
        }
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(sp),
            dst: GpOperand::Reg(bp),
        });
        if self.emit_debug {
            // After movq %rsp, %rbp: CFA is now tracked by %rbp+16
            self.push_lir(X86Inst::Directive(Directive::cfi_def_cfa_register("%rbp")));
        }

        // Save callee-saved registers
        let mut cfi_offset = -24i32; // First callee-saved is at -24 (after rbp at -16)
        for reg in &self.callee_saved_regs.clone() {
            self.push_lir(X86Inst::Push {
                src: GpOperand::Reg(*reg),
            });
            if self.emit_debug {
                self.push_lir(X86Inst::Directive(Directive::cfi_offset(
                    reg.name64(),
                    cfi_offset,
                )));
            }
            cfi_offset -= 8;
        }

        // Allocate stack space for locals + register save area (if variadic)
        let total_stack =
            stack_size + (self.callee_saved_regs.len() as i32 * 8) + reg_save_area_size;
        // Ensure 16-byte alignment
        let aligned_stack = (total_stack + 15) & !15;
        if aligned_stack > self.callee_saved_regs.len() as i32 * 8 {
            self.push_lir(X86Inst::Sub {
                size: OperandSize::B64,
                src: GpOperand::Imm(
                    (aligned_stack - (self.callee_saved_regs.len() as i32 * 8)) as i64,
                ),
                dst: Reg::Rsp,
            });
        }

        // For variadic functions, save argument registers to the register save area
        // This must be done before any other code uses these registers
        // AMD64 ABI: rdi at offset 0, rsi at offset 8, rdx at offset 16, etc.
        if is_variadic {
            let int_arg_regs = Reg::arg_regs();
            // Save all 6 GP argument registers at their ABI-specified offsets
            for (i, reg) in int_arg_regs.iter().enumerate() {
                // offset from rbp = reg_save_area_offset - (i * 8)
                // reg at ABI offset i*8 relative to reg_save_area base
                let offset = self.reg_save_area_offset - (i as i32 * 8);
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(*reg),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -offset,
                    }),
                });
            }
        }

        // Move arguments from registers to their allocated locations if needed
        // System V AMD64 ABI: integer args in RDI, RSI, RDX, RCX, R8, R9
        //                     FP args in XMM0-XMM7 (separate counters)
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = XmmReg::arg_regs();
        let mut int_arg_idx = 0;
        let mut fp_arg_idx = 0;

        // Detect if there's a hidden return pointer (for functions returning large structs)
        // The __sret pseudo has arg_idx=0 and shifts all other arg indices by 1
        let has_sret = func
            .pseudos
            .iter()
            .any(|p| matches!(p.kind, PseudoKind::Arg(0)) && p.name.as_deref() == Some("__sret"));
        let arg_idx_offset: u32 = if has_sret { 1 } else { 0 };

        // If there's a hidden return pointer, it takes RDI, so params start from RSI
        if has_sret {
            int_arg_idx = 1;
        }

        for (i, (_name, typ)) in func.params.iter().enumerate() {
            // Find the pseudo for this argument
            for pseudo in &func.pseudos {
                if let PseudoKind::Arg(arg_idx) = pseudo.kind {
                    if arg_idx == (i as u32) + arg_idx_offset {
                        let is_fp = typ.is_float();
                        if is_fp {
                            // FP argument
                            if fp_arg_idx < fp_arg_regs.len() {
                                if let Some(Loc::Stack(offset)) = self.locations.get(&pseudo.id) {
                                    // Move from FP arg register to stack
                                    let adjusted = offset + self.callee_saved_offset;
                                    let fp_size = if typ.size_bits() == 32 {
                                        FpSize::Single
                                    } else {
                                        FpSize::Double
                                    };
                                    self.push_lir(X86Inst::MovFp {
                                        size: fp_size,
                                        src: XmmOperand::Reg(fp_arg_regs[fp_arg_idx]),
                                        dst: XmmOperand::Mem(MemAddr::BaseOffset {
                                            base: Reg::Rbp,
                                            offset: -adjusted,
                                        }),
                                    });
                                }
                            }
                            fp_arg_idx += 1;
                        } else {
                            // Integer argument
                            if int_arg_idx < int_arg_regs.len() {
                                if let Some(Loc::Stack(offset)) = self.locations.get(&pseudo.id) {
                                    // Move from arg register to stack
                                    let adjusted = offset + self.callee_saved_offset;
                                    self.push_lir(X86Inst::Mov {
                                        size: OperandSize::B64,
                                        src: GpOperand::Reg(int_arg_regs[int_arg_idx]),
                                        dst: GpOperand::Mem(MemAddr::BaseOffset {
                                            base: Reg::Rbp,
                                            offset: -adjusted,
                                        }),
                                    });
                                }
                            }
                            int_arg_idx += 1;
                        }
                        break;
                    }
                }
            }
        }

        // Save number of fixed GP params for va_start
        // Count how many non-FP params we have
        if is_variadic {
            self.num_fixed_gp_params = func
                .params
                .iter()
                .filter(|(_, typ)| !typ.is_float())
                .count();
            if has_sret {
                self.num_fixed_gp_params += 1; // Account for hidden sret pointer
            }
        }

        // Emit basic blocks
        for block in &func.blocks {
            self.emit_block(block);
        }

        // CFI: End procedure
        if self.emit_unwind_tables {
            self.push_lir(X86Inst::Directive(Directive::CfiEndProc));
        }
    }

    fn emit_block(&mut self, block: &crate::ir::BasicBlock) {
        // Emit block label (include function name for uniqueness)
        if let Some(label) = &block.label {
            // LIR: named block label (using Raw since format differs from standard)
            self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                ".L_{}_{}:",
                self.current_fn, label
            ))));
        } else {
            // LIR: numbered block label
            self.push_lir(X86Inst::Directive(Directive::BlockLabel(Label::new(
                &self.current_fn,
                block.id.0,
            ))));
        }

        // Emit instructions
        for insn in &block.insns {
            self.emit_insn(insn);
        }
    }

    fn emit_insn(&mut self, insn: &Instruction) {
        // Emit .loc directive for debug info
        self.emit_loc(insn);

        match insn.op {
            Opcode::Entry => {
                // Already handled in function prologue
            }

            Opcode::Ret => {
                // Move return value to appropriate register if present
                // System V AMD64 ABI: integers in RAX, floats in XMM0
                if let Some(src) = insn.src.first() {
                    let src_loc = self.get_location(*src);
                    let is_fp = matches!(src_loc, Loc::Xmm(_) | Loc::FImm(..))
                        || insn.typ.as_ref().is_some_and(|t| t.is_float());
                    if is_fp {
                        // Float return value goes in XMM0
                        self.emit_fp_move(*src, XmmReg::Xmm0, insn.size);
                    } else {
                        // Integer return value goes in RAX
                        self.emit_move(*src, Reg::Rax, insn.size);
                    }
                }
                // Epilogue: restore callee-saved registers and return
                let bp = Reg::bp();
                let num_callee_saved = self.callee_saved_regs.len();
                if num_callee_saved > 0 {
                    // Set rsp to point to the first callee-saved register (pushed after rbp setup)
                    let offset = num_callee_saved * 8;
                    // LIR: lea to restore rsp
                    self.push_lir(X86Inst::Lea {
                        addr: MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -(offset as i32),
                        },
                        dst: Reg::Rsp,
                    });
                    // Pop callee-saved registers in reverse order
                    // Collect to avoid borrow conflict with push_lir
                    let callee_saved: Vec<Reg> =
                        self.callee_saved_regs.iter().rev().copied().collect();
                    for reg in callee_saved {
                        // LIR: pop callee-saved register
                        self.push_lir(X86Inst::Pop { dst: reg });
                    }
                } else {
                    // No callee-saved registers, just restore rsp from rbp
                    // LIR: mov rbp to rsp
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::Rbp),
                        dst: GpOperand::Reg(Reg::Rsp),
                    });
                }
                // LIR: pop rbp
                self.push_lir(X86Inst::Pop { dst: bp });
                // LIR: ret
                self.push_lir(X86Inst::Ret);
            }

            Opcode::Br => {
                if let Some(target) = insn.bb_true {
                    // LIR: unconditional jump
                    self.push_lir(X86Inst::Jmp {
                        target: Label::new(&self.current_fn, target.0),
                    });
                }
            }

            Opcode::Cbr => {
                if let Some(&cond) = insn.src.first() {
                    let loc = self.get_location(cond);
                    let size = insn.size.max(32);
                    let op_size = OperandSize::from_bits(size);
                    match &loc {
                        Loc::Reg(r) => {
                            // LIR: test register with itself
                            self.push_lir(X86Inst::Test {
                                size: op_size,
                                src: GpOperand::Reg(*r),
                                dst: GpOperand::Reg(*r),
                            });
                        }
                        Loc::Stack(offset) => {
                            let adjusted = offset + self.callee_saved_offset;
                            // LIR: compare stack location with 0
                            self.push_lir(X86Inst::Cmp {
                                size: op_size,
                                src: GpOperand::Imm(0),
                                dst: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rbp,
                                    offset: -adjusted,
                                }),
                            });
                        }
                        Loc::Imm(v) => {
                            if *v != 0 {
                                if let Some(target) = insn.bb_true {
                                    // LIR: unconditional jump (constant true)
                                    self.push_lir(X86Inst::Jmp {
                                        target: Label::new(&self.current_fn, target.0),
                                    });
                                }
                                return;
                            } else {
                                if let Some(target) = insn.bb_false {
                                    // LIR: unconditional jump (constant false)
                                    self.push_lir(X86Inst::Jmp {
                                        target: Label::new(&self.current_fn, target.0),
                                    });
                                }
                                return;
                            }
                        }
                        Loc::Global(_) => {
                            self.emit_move(cond, Reg::Rax, size);
                            // LIR: test rax with itself
                            self.push_lir(X86Inst::Test {
                                size: op_size,
                                src: GpOperand::Reg(Reg::Rax),
                                dst: GpOperand::Reg(Reg::Rax),
                            });
                        }
                        Loc::Xmm(x) => {
                            // Compare XMM register with zero
                            // LIR: xorps to zero xmm15, then ucomiss
                            self.push_lir(X86Inst::XorpsSelf { reg: XmmReg::Xmm15 });
                            self.push_lir(X86Inst::UComiFp {
                                size: FpSize::Single,
                                src: XmmOperand::Reg(*x),
                                dst: XmmReg::Xmm15,
                            });
                        }
                        Loc::FImm(v, _) => {
                            // Float immediate - check if non-zero
                            if *v != 0.0 {
                                if let Some(target) = insn.bb_true {
                                    // LIR: unconditional jump (constant true)
                                    self.push_lir(X86Inst::Jmp {
                                        target: Label::new(&self.current_fn, target.0),
                                    });
                                }
                                return;
                            } else {
                                if let Some(target) = insn.bb_false {
                                    // LIR: unconditional jump (constant false)
                                    self.push_lir(X86Inst::Jmp {
                                        target: Label::new(&self.current_fn, target.0),
                                    });
                                }
                                return;
                            }
                        }
                    }
                    if let Some(target) = insn.bb_true {
                        // LIR: conditional jump if not equal (non-zero)
                        self.push_lir(X86Inst::Jcc {
                            cc: IntCC::Ne,
                            target: Label::new(&self.current_fn, target.0),
                        });
                    }
                    if let Some(target) = insn.bb_false {
                        // LIR: unconditional jump to false branch
                        self.push_lir(X86Inst::Jmp {
                            target: Label::new(&self.current_fn, target.0),
                        });
                    }
                }
            }

            Opcode::Switch => {
                // Switch uses target as the value to switch on
                if let Some(val) = insn.target {
                    let size = insn.size.max(32);
                    let op_size = OperandSize::from_bits(size);
                    // Move switch value to eax
                    self.emit_move(val, Reg::Rax, size);

                    // Generate comparisons for each case
                    for (case_val, target_bb) in &insn.switch_cases {
                        // LIR: compare with case value
                        self.push_lir(X86Inst::Cmp {
                            size: op_size,
                            src: GpOperand::Imm(*case_val),
                            dst: GpOperand::Reg(Reg::Rax),
                        });
                        // LIR: conditional jump on equal
                        self.push_lir(X86Inst::Jcc {
                            cc: IntCC::E,
                            target: Label::new(&self.current_fn, target_bb.0),
                        });
                    }

                    // Jump to default (or fall through if no default)
                    if let Some(default_bb) = insn.switch_default {
                        // LIR: unconditional jump to default
                        self.push_lir(X86Inst::Jmp {
                            target: Label::new(&self.current_fn, default_bb.0),
                        });
                    }
                }
            }

            Opcode::Add
            | Opcode::Sub
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
            | Opcode::Shl
            | Opcode::Lsr
            | Opcode::Asr => {
                self.emit_binop(insn);
            }

            Opcode::Mul => {
                self.emit_mul(insn);
            }

            Opcode::DivS | Opcode::DivU | Opcode::ModS | Opcode::ModU => {
                self.emit_div(insn);
            }

            // Floating-point arithmetic operations
            Opcode::FAdd | Opcode::FSub | Opcode::FMul | Opcode::FDiv => {
                self.emit_fp_binop(insn);
            }

            Opcode::FNeg => {
                self.emit_fp_neg(insn);
            }

            // Floating-point comparisons
            Opcode::FCmpOEq
            | Opcode::FCmpONe
            | Opcode::FCmpOLt
            | Opcode::FCmpOLe
            | Opcode::FCmpOGt
            | Opcode::FCmpOGe => {
                self.emit_fp_compare(insn);
            }

            // Integer to float conversions
            Opcode::UCvtF | Opcode::SCvtF => {
                self.emit_int_to_float(insn);
            }

            // Float to integer conversions
            Opcode::FCvtU | Opcode::FCvtS => {
                self.emit_float_to_int(insn);
            }

            // Float to float conversions (e.g., float to double)
            Opcode::FCvtF => {
                self.emit_float_to_float(insn);
            }

            Opcode::SetEq
            | Opcode::SetNe
            | Opcode::SetLt
            | Opcode::SetLe
            | Opcode::SetGt
            | Opcode::SetGe
            | Opcode::SetB
            | Opcode::SetBe
            | Opcode::SetA
            | Opcode::SetAe => {
                self.emit_compare(insn);
            }

            Opcode::Neg => {
                self.emit_neg(insn);
            }

            Opcode::Not => {
                self.emit_not(insn);
            }

            Opcode::Load => {
                self.emit_load(insn);
            }

            Opcode::Store => {
                self.emit_store(insn);
            }

            Opcode::Call => {
                self.emit_call(insn);
            }

            Opcode::SetVal => {
                if let Some(target) = insn.target {
                    if let Some(pseudo) = self.pseudos.iter().find(|p| p.id == target) {
                        let target_loc = self.locations.get(&target).cloned();
                        match &pseudo.kind {
                            PseudoKind::Val(v) => {
                                if let Some(Loc::Reg(r)) = target_loc {
                                    self.push_lir(X86Inst::Mov {
                                        size: OperandSize::from_bits(insn.size),
                                        src: GpOperand::Imm(*v),
                                        dst: GpOperand::Reg(r),
                                    });
                                }
                            }
                            PseudoKind::FVal(v) => {
                                // Only emit code if the target is in an XMM register
                                // FImm locations are materialized inline at use sites
                                if let Some(Loc::Xmm(_)) = target_loc {
                                    self.emit_fp_const_load(target, *v, insn.size);
                                }
                                // For FImm locations, do nothing - the value will be
                                // loaded inline when used in operations
                            }
                            _ => {}
                        }
                    }
                }
            }

            Opcode::Copy => {
                if let (Some(target), Some(&src)) = (insn.target, insn.src.first()) {
                    // Pass the type to emit_copy for proper sign/zero extension
                    self.emit_copy_with_type(src, target, insn.size, insn.typ.as_ref());
                }
            }

            Opcode::SymAddr => {
                if let (Some(target), Some(&src)) = (insn.target, insn.src.first()) {
                    let dst_loc = self.get_location(target);
                    let dst_reg = match &dst_loc {
                        Loc::Reg(r) => *r,
                        _ => Reg::Rax,
                    };
                    let src_loc = self.get_location(src);
                    match src_loc {
                        Loc::Global(name) => {
                            // Check if it's a local label (starts with '.') or global symbol
                            let is_local_label = name.starts_with('.');
                            self.push_lir(X86Inst::Lea {
                                addr: MemAddr::RipRelative(Symbol {
                                    name: name.clone(),
                                    is_local: is_local_label,
                                }),
                                dst: dst_reg,
                            });
                        }
                        Loc::Stack(offset) => {
                            // Get address of stack location
                            let adjusted = offset + self.callee_saved_offset;
                            self.push_lir(X86Inst::Lea {
                                addr: MemAddr::BaseOffset {
                                    base: Reg::Rbp,
                                    offset: -adjusted,
                                },
                                dst: dst_reg,
                            });
                        }
                        _ => {}
                    }
                    // Move to final destination if needed
                    if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
                        self.emit_move_to_loc(dst_reg, &dst_loc, 64);
                    }
                }
            }

            Opcode::Select => {
                self.emit_select(insn);
            }

            Opcode::Zext | Opcode::Sext | Opcode::Trunc => {
                self.emit_extend(insn);
            }

            // ================================================================
            // Variadic function support (va_* builtins)
            // ================================================================
            Opcode::VaStart => {
                self.emit_va_start(insn);
            }

            Opcode::VaArg => {
                self.emit_va_arg(insn);
            }

            Opcode::VaEnd => {
                // va_end is a no-op on all platforms
            }

            Opcode::VaCopy => {
                self.emit_va_copy(insn);
            }

            // ================================================================
            // Byte-swapping builtins
            // ================================================================
            Opcode::Bswap16 => {
                self.emit_bswap16(insn);
            }

            Opcode::Bswap32 => {
                self.emit_bswap32(insn);
            }

            Opcode::Bswap64 => {
                self.emit_bswap64(insn);
            }

            Opcode::Alloca => {
                self.emit_alloca(insn);
            }

            // Skip no-ops and unimplemented
            _ => {}
        }
    }

    fn get_location(&self, pseudo: PseudoId) -> Loc {
        self.locations.get(&pseudo).cloned().unwrap_or(Loc::Imm(0))
    }

    fn emit_move(&mut self, src: PseudoId, dst: Reg, size: u32) {
        let size = size.max(32);
        let op_size = OperandSize::from_bits(size);
        let loc = self.get_location(src);
        match loc {
            Loc::Reg(r) if r == dst => {
                // No-op: same register
            }
            Loc::Reg(r) => {
                // LIR: register-to-register move
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(r),
                    dst: GpOperand::Reg(dst),
                });
            }
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                // LIR: memory-to-register move
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                    dst: GpOperand::Reg(dst),
                });
            }
            Loc::Imm(v) => {
                // x86-64: movl sign-extends to 64-bit, movq only works with 32-bit signed immediates
                // For values outside 32-bit signed range, use movabsq
                if size == 64 && (v > i32::MAX as i64 || v < i32::MIN as i64) {
                    // LIR: 64-bit immediate move
                    self.push_lir(X86Inst::MovAbs { imm: v, dst });
                } else {
                    // LIR: immediate-to-register move
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Imm(v),
                        dst: GpOperand::Reg(dst),
                    });
                }
            }
            Loc::Global(name) => {
                // LIR: RIP-relative memory-to-register move
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Mem(MemAddr::RipRelative(Symbol::global(name.clone()))),
                    dst: GpOperand::Reg(dst),
                });
            }
            Loc::Xmm(x) => {
                // Move from XMM to general-purpose register
                // LIR: XMM to GP move
                self.push_lir(X86Inst::MovXmmGp {
                    size: OperandSize::B64,
                    src: x,
                    dst,
                });
            }
            Loc::FImm(..) => {
                // Float immediate cannot be directly moved to GP register
                // This should not normally happen
            }
        }
    }

    fn emit_move_to_loc(&mut self, src: Reg, dst: &Loc, size: u32) {
        // For stack stores, use actual size to properly handle char/short
        // For register-to-register, use minimum 32-bit
        match dst {
            Loc::Reg(r) if *r == src => {
                // No-op: same register
            }
            Loc::Reg(r) => {
                let reg_size = size.max(32);
                let op_size = OperandSize::from_bits(reg_size);
                // LIR: register-to-register move
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(src),
                    dst: GpOperand::Reg(*r),
                });
            }
            Loc::Stack(offset) => {
                // Use actual size for memory stores (8, 16, 32, 64 bits)
                let adjusted = offset + self.callee_saved_offset;
                let op_size = OperandSize::from_bits(size);
                // LIR: register-to-memory move
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(src),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                });
            }
            _ => {}
        }
    }

    fn emit_binop(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(size);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        let work_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::Rax,
        };
        self.emit_move(src1, work_reg, size);
        if matches!(insn.op, Opcode::Shl | Opcode::Lsr | Opcode::Asr) {
            let src2_loc = self.get_location(src2);
            match src2_loc {
                Loc::Imm(v) => {
                    // LIR: shift by immediate
                    let shift_count = ShiftCount::Imm(v as u8);
                    match insn.op {
                        Opcode::Shl => self.push_lir(X86Inst::Shl {
                            size: op_size,
                            count: shift_count,
                            dst: work_reg,
                        }),
                        Opcode::Lsr => self.push_lir(X86Inst::Shr {
                            size: op_size,
                            count: shift_count,
                            dst: work_reg,
                        }),
                        Opcode::Asr => self.push_lir(X86Inst::Sar {
                            size: op_size,
                            count: shift_count,
                            dst: work_reg,
                        }),
                        _ => {}
                    }
                }
                _ => {
                    self.emit_move(src2, Reg::Rcx, 8);
                    // LIR: shift by %cl
                    let shift_count = ShiftCount::Cl;
                    match insn.op {
                        Opcode::Shl => self.push_lir(X86Inst::Shl {
                            size: op_size,
                            count: shift_count,
                            dst: work_reg,
                        }),
                        Opcode::Lsr => self.push_lir(X86Inst::Shr {
                            size: op_size,
                            count: shift_count,
                            dst: work_reg,
                        }),
                        Opcode::Asr => self.push_lir(X86Inst::Sar {
                            size: op_size,
                            count: shift_count,
                            dst: work_reg,
                        }),
                        _ => {}
                    }
                }
            }
        } else {
            let src2_loc = self.get_location(src2);
            let src2_gp = self.loc_to_gp_operand(&src2_loc);
            // LIR: binary operation (add, sub, and, or, xor)
            match insn.op {
                Opcode::Add => self.push_lir(X86Inst::Add {
                    size: op_size,
                    src: src2_gp,
                    dst: work_reg,
                }),
                Opcode::Sub => self.push_lir(X86Inst::Sub {
                    size: op_size,
                    src: src2_gp,
                    dst: work_reg,
                }),
                Opcode::And => self.push_lir(X86Inst::And {
                    size: op_size,
                    src: src2_gp,
                    dst: work_reg,
                }),
                Opcode::Or => self.push_lir(X86Inst::Or {
                    size: op_size,
                    src: src2_gp,
                    dst: work_reg,
                }),
                Opcode::Xor => self.push_lir(X86Inst::Xor {
                    size: op_size,
                    src: src2_gp,
                    dst: work_reg,
                }),
                _ => {}
            }
        }
        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, size);
        }
    }

    fn emit_mul(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(size);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::Rax,
        };
        self.emit_move(src1, dst_reg, size);
        let src2_loc = self.get_location(src2);
        // LIR: 2-operand imul instruction
        let src2_gp = self.loc_to_gp_operand(&src2_loc);
        self.push_lir(X86Inst::IMul2 {
            size: op_size,
            src: src2_gp,
            dst: dst_reg,
        });
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size);
        }
    }

    fn emit_div(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(size);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        self.emit_move(src1, Reg::Rax, size);
        match insn.op {
            Opcode::DivS | Opcode::ModS => {
                if size == 64 {
                    // LIR: sign-extend RAX to RDX:RAX
                    self.push_lir(X86Inst::Cqto);
                } else {
                    // LIR: sign-extend EAX to EDX:EAX
                    self.push_lir(X86Inst::Cltd);
                }
            }
            Opcode::DivU | Opcode::ModU => {
                // LIR: zero RDX for unsigned division
                self.push_lir(X86Inst::Xor {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rdx),
                    dst: Reg::Rdx,
                });
            }
            _ => {}
        }
        let src2_loc = self.get_location(src2);
        let divisor_reg = match &src2_loc {
            Loc::Reg(r) if *r != Reg::Rax && *r != Reg::Rdx => *r,
            _ => {
                self.emit_move(src2, Reg::R10, size);
                Reg::R10
            }
        };
        // LIR: division instruction
        match insn.op {
            Opcode::DivS | Opcode::ModS => {
                self.push_lir(X86Inst::IDiv {
                    size: op_size,
                    divisor: GpOperand::Reg(divisor_reg),
                });
            }
            _ => {
                self.push_lir(X86Inst::Div {
                    size: op_size,
                    divisor: GpOperand::Reg(divisor_reg),
                });
            }
        }
        let dst_loc = self.get_location(target);
        let result_reg = match insn.op {
            Opcode::DivS | Opcode::DivU => Reg::Rax,
            Opcode::ModS | Opcode::ModU => Reg::Rdx,
            _ => Reg::Rax,
        };
        self.emit_move_to_loc(result_reg, &dst_loc, size);
    }

    fn emit_compare(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(size);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        self.emit_move(src1, Reg::Rax, size);
        let src2_loc = self.get_location(src2);
        // x86-64 cmp instruction only supports 32-bit signed immediates
        // For larger values, we need to load into a register first
        let src2_gp = match &src2_loc {
            Loc::Imm(v) if *v > i32::MAX as i64 || *v < i32::MIN as i64 => {
                // Large immediate - load into rcx first
                // LIR: movabsq for large immediate
                self.push_lir(X86Inst::MovAbs {
                    imm: *v,
                    dst: Reg::Rcx,
                });
                GpOperand::Reg(Reg::Rcx)
            }
            _ => self.loc_to_gp_operand(&src2_loc),
        };
        // LIR: compare instruction
        self.push_lir(X86Inst::Cmp {
            size: op_size,
            src: src2_gp,
            dst: GpOperand::Reg(Reg::Rax),
        });
        // Map opcode to condition code
        let cc = match insn.op {
            Opcode::SetEq => IntCC::E,
            Opcode::SetNe => IntCC::Ne,
            Opcode::SetLt => IntCC::L,
            Opcode::SetLe => IntCC::Le,
            Opcode::SetGt => IntCC::G,
            Opcode::SetGe => IntCC::Ge,
            Opcode::SetB => IntCC::B,
            Opcode::SetBe => IntCC::Be,
            Opcode::SetA => IntCC::A,
            Opcode::SetAe => IntCC::Ae,
            _ => return,
        };
        // LIR: setCC instruction
        self.push_lir(X86Inst::SetCC { cc, dst: Reg::Rax });
        // LIR: zero extend AL to EAX
        self.push_lir(X86Inst::Movzx {
            src_size: OperandSize::B8,
            dst_size: OperandSize::B32,
            src: GpOperand::Reg(Reg::Rax),
            dst: Reg::Rax,
        });
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::Rax, &dst_loc, 32);
    }

    fn emit_neg(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(size);
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        let work_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::Rax,
        };
        self.emit_move(src, work_reg, size);
        // LIR: negate instruction
        self.push_lir(X86Inst::Neg {
            size: op_size,
            dst: work_reg,
        });
        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, size);
        }
    }

    fn emit_not(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(size);
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        let work_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::Rax,
        };
        self.emit_move(src, work_reg, size);
        // LIR: bitwise not instruction
        self.push_lir(X86Inst::Not {
            size: op_size,
            dst: work_reg,
        });
        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, size);
        }
    }

    // ========================================================================
    // Floating-Point Instructions (SSE)
    // ========================================================================

    /// Emit a floating-point load operation
    fn emit_fp_load(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm15,
        };
        let addr_loc = self.get_location(addr);

        let fp_size = if size <= 32 {
            FpSize::Single
        } else {
            FpSize::Double
        };

        match addr_loc {
            Loc::Reg(r) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: r,
                        offset: insn.offset as i32,
                    }),
                    dst: XmmOperand::Reg(dst_xmm),
                });
            }
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                if is_symbol {
                    // Local variable - load directly from stack slot
                    let total_offset = offset - insn.offset as i32 + self.callee_saved_offset;
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -total_offset,
                        }),
                        dst: XmmOperand::Reg(dst_xmm),
                    });
                } else {
                    // Spilled address - load address first, then load from that address
                    let adjusted = offset + self.callee_saved_offset;
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -adjusted,
                        }),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: insn.offset as i32,
                        }),
                        dst: XmmOperand::Reg(dst_xmm),
                    });
                }
            }
            Loc::Global(name) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::RipRelative(Symbol {
                        name: name.to_string(),
                        is_local: false,
                    })),
                    dst: XmmOperand::Reg(dst_xmm),
                });
            }
            _ => {
                // Load address into R11, then load from that address
                self.emit_move(addr, Reg::R11, 64);
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: insn.offset as i32,
                    }),
                    dst: XmmOperand::Reg(dst_xmm),
                });
            }
        }

        // If destination is not the XMM register we loaded to, move it
        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, size);
        }
    }

    /// Emit a floating-point store operation
    fn emit_fp_store(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let (addr, value) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&a), Some(&v)) => (a, v),
            _ => return,
        };
        let fp_size = if size <= 32 {
            FpSize::Single
        } else {
            FpSize::Double
        };

        // IMPORTANT: Check address location BEFORE emit_fp_move, because emit_fp_move
        // may clobber RAX when loading immediate values. If addr is in RAX, we need
        // to save it to R11 first.
        let addr_loc = self.get_location(addr);
        let addr_reg = match &addr_loc {
            Loc::Reg(Reg::Rax) => {
                // Address is in RAX - move it to R11 before emit_fp_move clobbers it
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Reg(Reg::R11),
                });
                Some(Reg::R11)
            }
            Loc::Reg(r) => Some(*r),
            _ => None,
        };

        // Move value to XMM15 (scratch register) - this may clobber RAX
        self.emit_fp_move(value, XmmReg::Xmm15, size);

        match addr_loc {
            Loc::Reg(_) => {
                // Use the saved register (R11 if it was RAX, otherwise original)
                let r = addr_reg.unwrap_or(Reg::Rax);
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: r,
                        offset: insn.offset as i32,
                    }),
                });
            }
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                if is_symbol {
                    // Local variable - store directly to stack slot
                    let total_offset = offset - insn.offset as i32 + self.callee_saved_offset;
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Reg(XmmReg::Xmm15),
                        dst: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -total_offset,
                        }),
                    });
                } else {
                    // Spilled address - load address first, then store through it
                    let adjusted = offset + self.callee_saved_offset;
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -adjusted,
                        }),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Reg(XmmReg::Xmm15),
                        dst: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: insn.offset as i32,
                        }),
                    });
                }
            }
            Loc::Global(name) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Mem(MemAddr::RipRelative(Symbol {
                        name: name.to_string(),
                        is_local: false,
                    })),
                });
            }
            _ => {
                // Load address into R11, then store
                self.emit_move(addr, Reg::R11, 64);
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: insn.offset as i32,
                    }),
                });
            }
        }
    }

    /// Emit floating-point binary operation (addss/addsd, subss/subsd, etc.)
    fn emit_fp_binop(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_loc = self.get_location(target);
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm0, // Use XMM0 as work register
        };
        let fp_size = FpSize::from_bits(size);

        // Helper to emit the FP binop LIR instruction
        let emit_fp_binop_lir = |cg: &mut Self, src: XmmOperand, dst: XmmReg| match insn.op {
            Opcode::FAdd => cg.push_lir(X86Inst::AddFp {
                size: fp_size,
                src,
                dst,
            }),
            Opcode::FSub => cg.push_lir(X86Inst::SubFp {
                size: fp_size,
                src,
                dst,
            }),
            Opcode::FMul => cg.push_lir(X86Inst::MulFp {
                size: fp_size,
                src,
                dst,
            }),
            Opcode::FDiv => cg.push_lir(X86Inst::DivFp {
                size: fp_size,
                src,
                dst,
            }),
            _ => {}
        };

        // Move first operand to destination XMM register
        self.emit_fp_move(src1, dst_xmm, size);

        // Apply operation with second operand
        let src2_loc = self.get_location(src2);
        match src2_loc {
            Loc::Xmm(x) => {
                emit_fp_binop_lir(self, XmmOperand::Reg(x), dst_xmm);
            }
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                emit_fp_binop_lir(
                    self,
                    XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                    dst_xmm,
                );
            }
            Loc::FImm(v, _) => {
                // Load float immediate to a scratch register, then operate
                // Use XMM14 if dst is XMM15, otherwise use XMM15
                let scratch = if dst_xmm == XmmReg::Xmm15 {
                    XmmReg::Xmm14
                } else {
                    XmmReg::Xmm15
                };
                self.emit_fp_imm_to_xmm(v, scratch, size);
                emit_fp_binop_lir(self, XmmOperand::Reg(scratch), dst_xmm);
            }
            _ => {
                // Move to a scratch register first
                // Use XMM14 if dst is XMM15, otherwise use XMM15
                let scratch = if dst_xmm == XmmReg::Xmm15 {
                    XmmReg::Xmm14
                } else {
                    XmmReg::Xmm15
                };
                self.emit_fp_move(src2, scratch, size);
                emit_fp_binop_lir(self, XmmOperand::Reg(scratch), dst_xmm);
            }
        }

        // Move result to destination if not already there
        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, size);
        }
    }

    /// Emit floating-point negation
    fn emit_fp_neg(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_loc = self.get_location(target);
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm0,
        };
        let fp_size = FpSize::from_bits(size);

        // Move source to destination
        self.emit_fp_move(src, dst_xmm, size);

        // XOR with sign bit mask to negate
        // For float: 0x80000000, for double: 0x8000000000000000
        // Use a scratch register that's not dst_xmm to hold the sign mask
        let scratch_xmm = if dst_xmm == XmmReg::Xmm15 {
            XmmReg::Xmm14
        } else {
            XmmReg::Xmm15
        };
        if size <= 32 {
            // Create sign mask in scratch register: all zeros except sign bit
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B32,
                src: GpOperand::Imm(0x80000000),
                dst: GpOperand::Reg(Reg::Rax),
            });
            self.push_lir(X86Inst::MovGpXmm {
                size: OperandSize::B32,
                src: Reg::Rax,
                dst: scratch_xmm,
            });
            self.push_lir(X86Inst::XorFp {
                size: fp_size,
                src: scratch_xmm,
                dst: dst_xmm,
            });
        } else {
            self.push_lir(X86Inst::MovAbs {
                imm: 0x8000000000000000u64 as i64,
                dst: Reg::Rax,
            });
            self.push_lir(X86Inst::MovGpXmm {
                size: OperandSize::B64,
                src: Reg::Rax,
                dst: scratch_xmm,
            });
            self.push_lir(X86Inst::XorFp {
                size: fp_size,
                src: scratch_xmm,
                dst: dst_xmm,
            });
        }

        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, size);
        }
    }

    /// Emit floating-point comparison
    fn emit_fp_compare(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let fp_size = FpSize::from_bits(size);

        // Load first operand to XMM0
        self.emit_fp_move(src1, XmmReg::Xmm0, size);

        // Compare with second operand using ucomiss/ucomisd
        let src2_loc = self.get_location(src2);
        match src2_loc {
            Loc::Xmm(x) => {
                self.push_lir(X86Inst::UComiFp {
                    size: fp_size,
                    src: XmmOperand::Reg(x),
                    dst: XmmReg::Xmm0,
                });
            }
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                self.push_lir(X86Inst::UComiFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                    dst: XmmReg::Xmm0,
                });
            }
            Loc::FImm(v, _) => {
                self.emit_fp_imm_to_xmm(v, XmmReg::Xmm15, size);
                self.push_lir(X86Inst::UComiFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmReg::Xmm0,
                });
            }
            _ => {
                self.emit_fp_move(src2, XmmReg::Xmm15, size);
                self.push_lir(X86Inst::UComiFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmReg::Xmm0,
                });
            }
        }

        // Set result based on comparison type
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::Rax,
        };

        // Use appropriate setcc instruction
        // Note: FP comparisons set flags differently - need to handle unordered (NaN) cases
        let cc = match insn.op {
            Opcode::FCmpOEq => IntCC::E,  // Equal (ZF=1, PF=0)
            Opcode::FCmpONe => IntCC::Ne, // Not equal
            Opcode::FCmpOLt => IntCC::B,  // Below (CF=1) - for ordered less than
            Opcode::FCmpOLe => IntCC::Be, // Below or equal
            Opcode::FCmpOGt => IntCC::A,  // Above (CF=0, ZF=0)
            Opcode::FCmpOGe => IntCC::Ae, // Above or equal
            _ => return,
        };

        self.push_lir(X86Inst::SetCC { cc, dst: dst_reg });
        self.push_lir(X86Inst::Movzx {
            src_size: OperandSize::B8,
            dst_size: OperandSize::B32,
            src: GpOperand::Reg(dst_reg),
            dst: dst_reg,
        });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, 32);
        }
    }

    /// Emit integer to float conversion
    fn emit_int_to_float(&mut self, insn: &Instruction) {
        let dst_size = insn.size.max(32);
        let src_size = insn.src_size.max(32);
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_loc = self.get_location(target);
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm0,
        };

        // Move integer to RAX first
        self.emit_move(src, Reg::Rax, src_size);

        // Convert using cvtsi2ss/cvtsi2sd
        let int_size = OperandSize::from_bits(src_size);
        let fp_size = FpSize::from_bits(dst_size);
        self.push_lir(X86Inst::CvtIntToFp {
            int_size,
            fp_size,
            src: GpOperand::Reg(Reg::Rax),
            dst: dst_xmm,
        });

        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, dst_size);
        }
    }

    /// Emit float to integer conversion
    fn emit_float_to_int(&mut self, insn: &Instruction) {
        let dst_size = insn.size.max(32);
        let src_size = insn.src_size.max(32);
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Move float to XMM0
        self.emit_fp_move(src, XmmReg::Xmm0, src_size);

        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::Rax,
        };

        // Convert using cvttss2si/cvttsd2si (truncate toward zero)
        let fp_size = FpSize::from_bits(src_size);
        let int_size = OperandSize::from_bits(dst_size);
        self.push_lir(X86Inst::CvtFpToInt {
            fp_size,
            int_size,
            src: XmmOperand::Reg(XmmReg::Xmm0),
            dst: dst_reg,
        });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, dst_size);
        }
    }

    /// Emit float to float conversion (e.g., float to double)
    fn emit_float_to_float(&mut self, insn: &Instruction) {
        let dst_size = insn.size.max(32);
        let src_size = insn.src_size.max(32);
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_loc = self.get_location(target);
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm0,
        };

        // Move source to XMM0
        self.emit_fp_move(src, XmmReg::Xmm0, src_size);

        // Convert
        if src_size <= 32 && dst_size > 32 {
            // float to double: cvtss2sd
            self.push_lir(X86Inst::CvtFpFp {
                src_size: FpSize::Single,
                dst_size: FpSize::Double,
                src: XmmReg::Xmm0,
                dst: dst_xmm,
            });
        } else if src_size > 32 && dst_size <= 32 {
            // double to float: cvtsd2ss
            self.push_lir(X86Inst::CvtFpFp {
                src_size: FpSize::Double,
                dst_size: FpSize::Single,
                src: XmmReg::Xmm0,
                dst: dst_xmm,
            });
        } else {
            // Same size, just move
            if dst_xmm != XmmReg::Xmm0 {
                let fp_size = FpSize::from_bits(dst_size);
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm0),
                    dst: XmmOperand::Reg(dst_xmm),
                });
            }
        }

        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, dst_size);
        }
    }

    /// Load a floating-point constant into an XMM register
    fn emit_fp_const_load(&mut self, target: PseudoId, value: f64, size: u32) {
        let dst_loc = self.get_location(target);
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm0,
        };

        self.emit_fp_imm_to_xmm(value, dst_xmm, size);

        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, size);
        }
    }

    /// Load a float immediate value into an XMM register
    fn emit_fp_imm_to_xmm(&mut self, value: f64, xmm: XmmReg, size: u32) {
        if value == 0.0 {
            // Use xorps/xorpd to zero the register (faster)
            let fp_size = FpSize::from_bits(size);
            self.push_lir(X86Inst::XorFp {
                size: fp_size,
                src: xmm,
                dst: xmm,
            });
        } else if size <= 32 {
            // Float: load via integer register
            let bits = (value as f32).to_bits();
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B32,
                src: GpOperand::Imm(bits as i64),
                dst: GpOperand::Reg(Reg::Rax),
            });
            self.push_lir(X86Inst::MovGpXmm {
                size: OperandSize::B32,
                src: Reg::Rax,
                dst: xmm,
            });
        } else {
            // Double: load via integer register
            let bits = value.to_bits();
            self.push_lir(X86Inst::MovAbs {
                imm: bits as i64,
                dst: Reg::Rax,
            });
            self.push_lir(X86Inst::MovGpXmm {
                size: OperandSize::B64,
                src: Reg::Rax,
                dst: xmm,
            });
        }
    }

    /// Move a value to an XMM register
    fn emit_fp_move(&mut self, src: PseudoId, dst: XmmReg, size: u32) {
        let src_loc = self.get_location(src);
        let fp_size = FpSize::from_bits(size);

        match src_loc {
            Loc::Xmm(x) if x == dst => {
                // Already in destination
            }
            Loc::Xmm(x) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(x),
                    dst: XmmOperand::Reg(dst),
                });
            }
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                    dst: XmmOperand::Reg(dst),
                });
            }
            Loc::FImm(v, imm_size) => {
                // Use the size from the FImm, not the passed-in size
                // This ensures float constants are loaded as float, not double
                self.emit_fp_imm_to_xmm(v, dst, imm_size);
            }
            Loc::Reg(r) => {
                // Move from GP register to XMM (unusual but possible)
                self.push_lir(X86Inst::MovGpXmm {
                    size: OperandSize::B64,
                    src: r,
                    dst,
                });
            }
            Loc::Imm(v) => {
                // Integer immediate to float
                if size <= 32 {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Imm(v),
                        dst: GpOperand::Reg(Reg::Rax),
                    });
                    self.push_lir(X86Inst::MovGpXmm {
                        size: OperandSize::B32,
                        src: Reg::Rax,
                        dst,
                    });
                } else {
                    self.push_lir(X86Inst::MovAbs {
                        imm: v,
                        dst: Reg::Rax,
                    });
                    self.push_lir(X86Inst::MovGpXmm {
                        size: OperandSize::B64,
                        src: Reg::Rax,
                        dst,
                    });
                }
            }
            Loc::Global(name) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::RipRelative(Symbol::global(name.clone()))),
                    dst: XmmOperand::Reg(dst),
                });
            }
        }
    }

    /// Move from XMM register to a location
    fn emit_fp_move_from_xmm(&mut self, src: XmmReg, dst: &Loc, size: u32) {
        let fp_size = FpSize::from_bits(size);

        match dst {
            Loc::Xmm(x) if *x == src => {
                // Already in destination
            }
            Loc::Xmm(x) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(src),
                    dst: XmmOperand::Reg(*x),
                });
            }
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(src),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                });
            }
            Loc::Reg(r) => {
                // Move from XMM to GP register
                self.push_lir(X86Inst::MovXmmGp {
                    size: OperandSize::B64,
                    src,
                    dst: *r,
                });
            }
            _ => {}
        }
    }

    fn emit_load(&mut self, insn: &Instruction) {
        let mem_size = insn.size;
        let reg_size = insn.size.max(32);
        let addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);

        // Check if this is an FP load
        let is_fp =
            insn.typ.as_ref().is_some_and(|t| t.is_float()) || matches!(dst_loc, Loc::Xmm(_));

        if is_fp {
            self.emit_fp_load(insn);
            return;
        }

        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::Rax,
        };

        // Determine if we need sign or zero extension for small types
        // is_unsigned() returns true for explicitly unsigned types
        // For plain char, use target.char_signed to determine signedness
        let is_unsigned = insn.typ.as_ref().is_some_and(|t| {
            if t.is_unsigned() {
                true
            } else if t.is_plain_char() {
                // Plain char: unsigned if target says char is not signed
                !self.target.char_signed
            } else {
                false
            }
        });

        let addr_loc = self.get_location(addr);
        match addr_loc {
            Loc::Reg(r) => {
                if mem_size <= 16 {
                    // Use sign/zero extending load
                    // LIR: use Movzx or Movsx
                    let src_size = OperandSize::from_bits(mem_size);
                    if is_unsigned {
                        self.push_lir(X86Inst::Movzx {
                            src_size,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: r,
                                offset: insn.offset as i32,
                            }),
                            dst: dst_reg,
                        });
                    } else {
                        self.push_lir(X86Inst::Movsx {
                            src_size,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: r,
                                offset: insn.offset as i32,
                            }),
                            dst: dst_reg,
                        });
                    }
                } else {
                    // 32/64-bit load
                    let op_size = OperandSize::from_bits(reg_size);
                    // LIR: simple Mov
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: r,
                            offset: insn.offset as i32,
                        }),
                        dst: GpOperand::Reg(dst_reg),
                    });
                }
            }
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                if is_symbol {
                    // Local variable - load directly from stack slot
                    let total_offset = offset - insn.offset as i32 + self.callee_saved_offset;
                    if mem_size <= 16 {
                        // LIR: sign/zero extending load from stack
                        let src_size = OperandSize::from_bits(mem_size);
                        if is_unsigned {
                            self.push_lir(X86Inst::Movzx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rbp,
                                    offset: -total_offset,
                                }),
                                dst: dst_reg,
                            });
                        } else {
                            self.push_lir(X86Inst::Movsx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rbp,
                                    offset: -total_offset,
                                }),
                                dst: dst_reg,
                            });
                        }
                    } else {
                        // LIR: regular load from stack
                        let op_size = OperandSize::from_bits(reg_size);
                        self.push_lir(X86Inst::Mov {
                            size: op_size,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::Rbp,
                                offset: -total_offset,
                            }),
                            dst: GpOperand::Reg(dst_reg),
                        });
                    }
                } else {
                    // Spilled address - load address first, then load from that address
                    let adjusted = offset + self.callee_saved_offset;
                    // LIR: load spilled address
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -adjusted,
                        }),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    if mem_size <= 16 {
                        // LIR: sign/zero extending load through R11
                        let src_size = OperandSize::from_bits(mem_size);
                        if is_unsigned {
                            self.push_lir(X86Inst::Movzx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::R11,
                                    offset: insn.offset as i32,
                                }),
                                dst: dst_reg,
                            });
                        } else {
                            self.push_lir(X86Inst::Movsx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::R11,
                                    offset: insn.offset as i32,
                                }),
                                dst: dst_reg,
                            });
                        }
                    } else {
                        // LIR: regular load through R11
                        let op_size = OperandSize::from_bits(reg_size);
                        self.push_lir(X86Inst::Mov {
                            size: op_size,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::R11,
                                offset: insn.offset as i32,
                            }),
                            dst: GpOperand::Reg(dst_reg),
                        });
                    }
                }
            }
            Loc::Global(name) => {
                if mem_size <= 16 {
                    // LIR: sign/zero extending load from global
                    let src_size = OperandSize::from_bits(mem_size);
                    if is_unsigned {
                        self.push_lir(X86Inst::Movzx {
                            src_size,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Mem(MemAddr::RipRelative(Symbol::global(name.clone()))),
                            dst: dst_reg,
                        });
                    } else {
                        self.push_lir(X86Inst::Movsx {
                            src_size,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Mem(MemAddr::RipRelative(Symbol::global(name.clone()))),
                            dst: dst_reg,
                        });
                    }
                } else {
                    // LIR: regular load from global
                    let op_size = OperandSize::from_bits(reg_size);
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Mem(MemAddr::RipRelative(Symbol::global(name.clone()))),
                        dst: GpOperand::Reg(dst_reg),
                    });
                }
            }
            _ => {
                self.emit_move(addr, Reg::R11, 64);
                if mem_size <= 16 {
                    // LIR: sign/zero extending load through R11
                    let src_size = OperandSize::from_bits(mem_size);
                    if is_unsigned {
                        self.push_lir(X86Inst::Movzx {
                            src_size,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::R11,
                                offset: insn.offset as i32,
                            }),
                            dst: dst_reg,
                        });
                    } else {
                        self.push_lir(X86Inst::Movsx {
                            src_size,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::R11,
                                offset: insn.offset as i32,
                            }),
                            dst: dst_reg,
                        });
                    }
                } else {
                    // LIR: regular load through R11
                    let op_size = OperandSize::from_bits(reg_size);
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: insn.offset as i32,
                        }),
                        dst: GpOperand::Reg(dst_reg),
                    });
                }
            }
        }
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, reg_size);
        }
    }

    fn emit_store(&mut self, insn: &Instruction) {
        // Use actual size for memory stores (8, 16, 32, 64 bits)
        // This is critical for char/short types that need byte/word stores
        let mem_size = insn.size;
        // Register operations use minimum 32-bit
        let reg_size = insn.size.max(32);

        let (addr, value) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&a), Some(&v)) => (a, v),
            _ => return,
        };

        // Check if this is an FP store
        let value_loc = self.get_location(value);
        let is_fp = insn.typ.as_ref().is_some_and(|t| t.is_float())
            || matches!(value_loc, Loc::Xmm(_) | Loc::FImm(..));

        if is_fp {
            self.emit_fp_store(insn);
            return;
        }

        // For struct stores (size > 64), we need to copy multiple words
        // The value is a symbol containing the struct data
        if mem_size > 64 {
            self.emit_struct_store(insn, addr, value);
            return;
        }

        self.emit_move(value, Reg::Rax, reg_size);
        let addr_loc = self.get_location(addr);
        match addr_loc {
            Loc::Reg(r) => {
                let op_size = OperandSize::from_bits(mem_size);
                // LIR: store through register
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: r,
                        offset: insn.offset as i32,
                    }),
                });
            }
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                let op_size = OperandSize::from_bits(mem_size);
                if is_symbol {
                    // Local variable - store directly to stack slot
                    let total_offset = offset - insn.offset as i32 + self.callee_saved_offset;
                    // LIR: store to stack slot
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Reg(Reg::Rax),
                        dst: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -total_offset,
                        }),
                    });
                } else {
                    // Spilled address - load address first, then store through it
                    let adjusted = offset + self.callee_saved_offset;
                    // LIR: load spilled address
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -adjusted,
                        }),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    // LIR: store through loaded address
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Reg(Reg::Rax),
                        dst: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: insn.offset as i32,
                        }),
                    });
                }
            }
            Loc::Global(name) => {
                let op_size = OperandSize::from_bits(mem_size);
                // LIR: store to global via RIP-relative
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::RipRelative(Symbol::global(name.clone()))),
                });
            }
            _ => {
                self.emit_move(addr, Reg::R11, 64);
                let op_size = OperandSize::from_bits(mem_size);
                // LIR: store through R11
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: insn.offset as i32,
                    }),
                });
            }
        }
    }

    /// Emit a struct copy (store of size > 64 bits)
    /// The value is a symbol containing the source struct data
    fn emit_struct_store(&mut self, insn: &Instruction, addr: PseudoId, value: PseudoId) {
        let struct_size = insn.size; // Size in bits
        let num_qwords = (struct_size + 63) / 64;

        // Get source address (where the struct data is)
        let value_loc = self.get_location(value);
        // Get destination address
        let addr_loc = self.get_location(addr);

        // Load source address into R10
        match value_loc {
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                // LIR: lea for source address
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    },
                    dst: Reg::R10,
                });
            }
            Loc::Reg(r) => {
                if r != Reg::R10 {
                    // LIR: mov for source address
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: GpOperand::Reg(Reg::R10),
                    });
                }
            }
            Loc::Global(ref name) => {
                // LIR: lea for global source address
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::RipRelative(Symbol::global(name.clone())),
                    dst: Reg::R10,
                });
            }
            _ => return,
        }

        // Load destination address into R11
        match addr_loc {
            Loc::Stack(offset) => {
                let adjusted = offset - insn.offset as i32 + self.callee_saved_offset;
                // LIR: lea for destination address
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    },
                    dst: Reg::R11,
                });
            }
            Loc::Reg(r) => {
                if insn.offset != 0 {
                    // LIR: lea with offset
                    self.push_lir(X86Inst::Lea {
                        addr: MemAddr::BaseOffset {
                            base: r,
                            offset: insn.offset as i32,
                        },
                        dst: Reg::R11,
                    });
                } else if r != Reg::R11 {
                    // LIR: mov for destination address
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                }
            }
            Loc::Global(ref name) => {
                // LIR: lea for global destination address
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::RipRelative(Symbol::global(name.clone())),
                    dst: Reg::R11,
                });
            }
            _ => return,
        }

        // Copy qword by qword
        for i in 0..num_qwords {
            let byte_offset = (i * 8) as i32;
            // LIR: load from source
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::R10,
                    offset: byte_offset,
                }),
                dst: GpOperand::Reg(Reg::Rax),
            });
            // LIR: store to destination
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::Rax),
                dst: GpOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: byte_offset,
                }),
            });
        }
    }

    fn emit_call(&mut self, insn: &Instruction) {
        let func_name = match &insn.func_name {
            Some(n) => n.clone(),
            None => return,
        };

        // System V AMD64 ABI:
        // - Integer arguments: RDI, RSI, RDX, RCX, R8, R9 (6 registers)
        // - Floating-point arguments: XMM0-XMM7 (8 registers)
        // - Each class has its own counter (int and FP are independent)
        // - For variadic functions: float args go in XMM registers (printf needs this),
        //   but variadic integer args go on stack (our va_arg impl reads from stack)
        // - AL must be set to the number of XMM registers used for variadic calls
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = XmmReg::arg_regs();
        let mut int_arg_idx = 0;
        let mut fp_arg_idx = 0;
        let mut stack_args = 0;

        let variadic_start = insn.variadic_arg_start.unwrap_or(usize::MAX);

        // First pass: determine which args go on stack
        // For variadic calls: variadic INTEGER args go on stack, variadic FLOAT args use XMM
        // For non-variadic calls: overflow args go on stack
        let mut stack_arg_indices = Vec::new();
        let mut temp_int_idx = 0;
        let mut temp_fp_idx = 0;

        for i in 0..insn.src.len() {
            let arg_type = insn.arg_types.get(i);
            let is_fp = if let Some(typ) = arg_type {
                typ.is_float()
            } else {
                let arg_loc = self.get_location(insn.src[i]);
                matches!(arg_loc, Loc::Xmm(_) | Loc::FImm(..))
            };

            // Note: We pass all args (fixed and variadic) the same way per SysV ABI
            // Variadic args start at index variadic_start, but use same register sequence
            let _is_variadic_arg = i >= variadic_start;

            if is_fp {
                // Float args go in XMM registers (all args, including variadic)
                if temp_fp_idx >= fp_arg_regs.len() {
                    stack_arg_indices.push(i);
                }
                temp_fp_idx += 1;
            } else {
                // Integer args go in GP registers (all args, including variadic)
                // per x86-64 SysV ABI
                if temp_int_idx >= int_arg_regs.len() {
                    stack_arg_indices.push(i);
                }
                temp_int_idx += 1;
            }
        }

        // Ensure 16-byte stack alignment before call
        // If we're pushing an odd number of 8-byte args, add padding
        let needs_padding = stack_arg_indices.len() % 2 == 1;
        if needs_padding {
            self.push_lir(X86Inst::Sub {
                size: OperandSize::B64,
                src: GpOperand::Imm(8),
                dst: Reg::Rsp,
            });
        }

        // Push stack arguments in reverse order
        for &i in stack_arg_indices.iter().rev() {
            let arg = insn.src[i];
            let arg_type = insn.arg_types.get(i);
            let is_fp = if let Some(typ) = arg_type {
                typ.is_float()
            } else {
                let arg_loc = self.get_location(arg);
                matches!(arg_loc, Loc::Xmm(_) | Loc::FImm(..))
            };

            if is_fp {
                let fp_size = if let Some(typ) = arg_type {
                    typ.size_bits()
                } else {
                    64
                };
                self.emit_fp_move(arg, XmmReg::Xmm15, fp_size);
                self.push_lir(X86Inst::Sub {
                    size: OperandSize::B64,
                    src: GpOperand::Imm(8),
                    dst: Reg::Rsp,
                });
                let fp_lir_size = FpSize::from_bits(fp_size);
                self.push_lir(X86Inst::MovFp {
                    size: fp_lir_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rsp,
                        offset: 0,
                    }),
                });
            } else {
                let arg_size = if let Some(typ) = arg_type {
                    typ.size_bits().max(32)
                } else {
                    64
                };
                self.emit_move(arg, Reg::Rax, arg_size);
                self.push_lir(X86Inst::Push {
                    src: GpOperand::Reg(Reg::Rax),
                });
            }
            stack_args += 1;
        }

        // Now handle register arguments
        for i in 0..insn.src.len() {
            // Skip args that went to stack
            if stack_arg_indices.contains(&i) {
                continue;
            }
            let arg = insn.src[i];
            // Get argument type if available, otherwise fall back to location-based detection
            let arg_type = insn.arg_types.get(i);
            let is_fp = if let Some(typ) = arg_type {
                typ.is_float()
            } else {
                // Fall back to location-based detection for backwards compatibility
                let arg_loc = self.get_location(arg);
                matches!(arg_loc, Loc::Xmm(_) | Loc::FImm(..))
            };

            // Get argument size from type, with minimum 32-bit for register ops
            let arg_size = if let Some(typ) = arg_type {
                typ.size_bits().max(32)
            } else {
                64 // Default for backwards compatibility
            };

            if is_fp {
                // FP size from type (32 for float, 64 for double)
                let fp_size = if let Some(typ) = arg_type {
                    typ.size_bits()
                } else {
                    64
                };
                // Float args go in XMM registers
                self.emit_fp_move(arg, fp_arg_regs[fp_arg_idx], fp_size);
                fp_arg_idx += 1;
            } else {
                // Integer args go in GP registers
                self.emit_move(arg, int_arg_regs[int_arg_idx], arg_size);
                int_arg_idx += 1;
            }
        }

        // For variadic function calls, set AL to the number of XMM registers used
        // This is required by the System V AMD64 ABI for variadic functions
        if insn.variadic_arg_start.is_some() {
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B8,
                src: GpOperand::Imm(fp_arg_idx as i64),
                dst: GpOperand::Reg(Reg::Rax),
            });
        }

        // Emit the call
        self.push_lir(X86Inst::Call {
            target: CallTarget::Direct(Symbol::global(func_name.clone())),
        });

        // Clean up stack arguments (including padding if any)
        let stack_cleanup = stack_args * 8 + if needs_padding { 8 } else { 0 };
        if stack_cleanup > 0 {
            self.push_lir(X86Inst::Add {
                size: OperandSize::B64,
                src: GpOperand::Imm(stack_cleanup as i64),
                dst: Reg::Rsp,
            });
        }

        // Handle return value
        if let Some(target) = insn.target {
            let dst_loc = self.get_location(target);
            // Check if return value is floating-point based on its location or type
            let is_fp_result = if let Some(ref typ) = insn.typ {
                typ.is_float()
            } else {
                matches!(dst_loc, Loc::Xmm(_) | Loc::FImm(..))
            };

            // Get return value size from type
            let ret_size = insn.size.max(32);

            if is_fp_result {
                // FP return value is in XMM0
                self.emit_fp_move_from_xmm(XmmReg::Xmm0, &dst_loc, ret_size);
            } else {
                // Integer return value is in RAX
                self.emit_move_to_loc(Reg::Rax, &dst_loc, ret_size);
            }
        }
    }

    fn emit_select(&mut self, insn: &Instruction) {
        let (cond, then_val, else_val) = match (insn.src.first(), insn.src.get(1), insn.src.get(2))
        {
            (Some(&c), Some(&t), Some(&e)) => (c, t, e),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(size);
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::Rax,
        };
        self.emit_move(else_val, dst_reg, size);
        let cond_loc = self.get_location(cond);
        match &cond_loc {
            Loc::Reg(r) => {
                // LIR: test register with itself
                self.push_lir(X86Inst::Test {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(*r),
                    dst: GpOperand::Reg(*r),
                });
            }
            Loc::Imm(v) => {
                if *v != 0 {
                    self.emit_move(then_val, dst_reg, size);
                    if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
                        self.emit_move_to_loc(dst_reg, &dst_loc, size);
                    }
                    return;
                }
                if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
                    self.emit_move_to_loc(dst_reg, &dst_loc, size);
                }
                return;
            }
            _ => {
                self.emit_move(cond, Reg::R11, 64);
                // LIR: test R11 with itself
                self.push_lir(X86Inst::Test {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::R11),
                    dst: GpOperand::Reg(Reg::R11),
                });
            }
        }
        self.emit_move(then_val, Reg::R10, size);
        // LIR: conditional move if not equal (non-zero)
        self.push_lir(X86Inst::CMov {
            cc: IntCC::Ne,
            size: op_size,
            src: GpOperand::Reg(Reg::R10),
            dst: dst_reg,
        });
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size);
        }
    }

    fn emit_extend(&mut self, insn: &Instruction) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::Rax,
        };
        match insn.op {
            Opcode::Zext => {
                self.emit_move(src, dst_reg, insn.size);
            }
            Opcode::Sext => {
                // Move source at its original size, then sign-extend
                self.emit_move(src, dst_reg, insn.src_size.max(32));
                match insn.src_size {
                    8 => {
                        // LIR: sign-extend byte to qword
                        self.push_lir(X86Inst::Movsx {
                            src_size: OperandSize::B8,
                            dst_size: OperandSize::B64,
                            src: GpOperand::Reg(dst_reg),
                            dst: dst_reg,
                        });
                    }
                    16 => {
                        // LIR: sign-extend word to qword
                        self.push_lir(X86Inst::Movsx {
                            src_size: OperandSize::B16,
                            dst_size: OperandSize::B64,
                            src: GpOperand::Reg(dst_reg),
                            dst: dst_reg,
                        });
                    }
                    32 => {
                        // LIR: sign-extend dword to qword
                        self.push_lir(X86Inst::Movsx {
                            src_size: OperandSize::B32,
                            dst_size: OperandSize::B64,
                            src: GpOperand::Reg(dst_reg),
                            dst: dst_reg,
                        });
                    }
                    _ => {}
                }
            }
            Opcode::Trunc => {
                // Move source value to register, then mask to target size
                self.emit_move(src, dst_reg, 64);
                // Truncate by masking to the target size
                match insn.size {
                    8 => {
                        // LIR: zero-extend byte to dword (masks to 8 bits)
                        self.push_lir(X86Inst::Movzx {
                            src_size: OperandSize::B8,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Reg(dst_reg),
                            dst: dst_reg,
                        });
                    }
                    16 => {
                        // LIR: zero-extend word to dword (masks to 16 bits)
                        self.push_lir(X86Inst::Movzx {
                            src_size: OperandSize::B16,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Reg(dst_reg),
                            dst: dst_reg,
                        });
                    }
                    32 => {
                        // LIR: mov dword to self (32-bit ops zero upper 32 bits)
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::B32,
                            src: GpOperand::Reg(dst_reg),
                            dst: GpOperand::Reg(dst_reg),
                        });
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, insn.size);
        }
    }

    fn emit_copy_with_type(&mut self, src: PseudoId, dst: PseudoId, size: u32, typ: Option<&Type>) {
        // Keep actual size for handling narrow types
        let actual_size = size;
        let reg_size = size.max(32);
        let dst_loc = self.get_location(dst);
        let src_loc = self.get_location(src);

        // Check if this is a FP copy (source or dest is in XMM or is FImm)
        let is_fp_copy =
            matches!(&src_loc, Loc::Xmm(_) | Loc::FImm(..)) || matches!(&dst_loc, Loc::Xmm(_));

        // Determine if the type is unsigned (for proper sign/zero extension)
        // For plain char, use target.char_signed to determine signedness
        let is_unsigned = typ.is_some_and(|t| {
            if t.is_unsigned() {
                true
            } else if t.is_plain_char() {
                // Plain char: unsigned if target says char is not signed
                !self.target.char_signed
            } else {
                false
            }
        });

        if is_fp_copy {
            // Handle FP copy
            let dst_xmm = match &dst_loc {
                Loc::Xmm(x) => *x,
                _ => XmmReg::Xmm0,
            };

            self.emit_fp_move(src, dst_xmm, reg_size);

            if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
                self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, reg_size);
            }
        } else {
            // Integer copy
            match &dst_loc {
                Loc::Reg(r) => {
                    self.emit_move(src, *r, reg_size);
                    // For narrow types (8 or 16 bits), truncate to correct width
                    // Unsigned: zero-extend (AND mask)
                    // Signed: sign-extend (shift left then arithmetic shift right)
                    if actual_size == 8 {
                        if is_unsigned {
                            // LIR: zero-extend with AND mask
                            self.push_lir(X86Inst::And {
                                size: OperandSize::B32,
                                src: GpOperand::Imm(0xFF),
                                dst: *r,
                            });
                        } else {
                            // Sign-extend: shift left 24 bits then arithmetic shift right 24 bits
                            // LIR: shift left
                            self.push_lir(X86Inst::Shl {
                                size: OperandSize::B32,
                                count: ShiftCount::Imm(24),
                                dst: *r,
                            });
                            // LIR: arithmetic shift right
                            self.push_lir(X86Inst::Sar {
                                size: OperandSize::B32,
                                count: ShiftCount::Imm(24),
                                dst: *r,
                            });
                        }
                    } else if actual_size == 16 {
                        if is_unsigned {
                            // LIR: zero-extend with AND mask
                            self.push_lir(X86Inst::And {
                                size: OperandSize::B32,
                                src: GpOperand::Imm(0xFFFF),
                                dst: *r,
                            });
                        } else {
                            // Sign-extend: shift left 16 bits then arithmetic shift right 16 bits
                            // LIR: shift left
                            self.push_lir(X86Inst::Shl {
                                size: OperandSize::B32,
                                count: ShiftCount::Imm(16),
                                dst: *r,
                            });
                            // LIR: arithmetic shift right
                            self.push_lir(X86Inst::Sar {
                                size: OperandSize::B32,
                                count: ShiftCount::Imm(16),
                                dst: *r,
                            });
                        }
                    }
                }
                Loc::Stack(_) => {
                    self.emit_move(src, Reg::Rax, reg_size);
                    // For narrow types stored to stack, use the actual size
                    if actual_size <= 16 {
                        self.emit_move_to_loc(Reg::Rax, &dst_loc, actual_size);
                    } else {
                        self.emit_move_to_loc(Reg::Rax, &dst_loc, reg_size);
                    }
                }
                _ => {}
            }
        }
    }

    // ========================================================================
    // Variadic function support (va_* builtins)
    // ========================================================================
    //
    // On x86-64 System V ABI, va_list is a 24-byte struct:
    //   struct {
    //       unsigned int gp_offset;     // offset to next GP reg in save area
    //       unsigned int fp_offset;     // offset to next FP reg in save area
    //       void *overflow_arg_area;    // pointer to stack arguments
    //       void *reg_save_area;        // pointer to register save area
    //   };
    //
    // This implementation provides a simplified version that works with
    // stack-based arguments. Full register save area support would require
    // function prologue changes.

    /// Emit va_start: Initialize va_list
    fn emit_va_start(&mut self, insn: &Instruction) {
        let ap_addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };

        let ap_loc = self.get_location(ap_addr);

        // For x86-64 System V ABI:
        // va_list is a 24-byte struct. We initialize:
        // - gp_offset = fixed_gp_params * 8 (offset to first variadic GP arg in save area)
        // - fp_offset = 176 (skip all 8 FP regs, not saving them in this impl)
        // - overflow_arg_area = rbp + 16 (where stack args start, for overflow)
        // - reg_save_area = pointer to where we saved the argument registers

        let gp_offset = (self.num_fixed_gp_params * 8) as i32;
        let reg_save_base = self.reg_save_area_offset;

        match ap_loc {
            Loc::Stack(offset) => {
                // gp_offset = offset to next variadic GP arg
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(gp_offset as i64),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset,
                    }),
                });
                // fp_offset = 176 (all FP args consumed, use stack for FP varargs)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(176),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: offset + 4,
                    }),
                });
                // overflow_arg_area = rbp + 16
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: 16,
                    },
                    dst: Reg::Rax,
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: offset + 8,
                    }),
                });
                // reg_save_area = pointer to saved registers
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -reg_save_base,
                    },
                    dst: Reg::Rax,
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: offset + 16,
                    }),
                });
            }
            Loc::Reg(r) => {
                // Register contains the address of the va_list struct
                // gp_offset = offset to next variadic GP arg
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(gp_offset as i64),
                    dst: GpOperand::Mem(MemAddr::BaseOffset { base: r, offset: 0 }),
                });
                // fp_offset = 176 (all FP args consumed)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(176),
                    dst: GpOperand::Mem(MemAddr::BaseOffset { base: r, offset: 4 }),
                });
                // overflow_arg_area = rbp + 16
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: 16,
                    },
                    dst: Reg::Rax,
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset { base: r, offset: 8 }),
                });
                // reg_save_area = pointer to saved registers
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -reg_save_base,
                    },
                    dst: Reg::Rax,
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: r,
                        offset: 16,
                    }),
                });
            }
            _ => {}
        }
    }

    /// Emit va_arg: Get the next variadic argument
    fn emit_va_arg(&mut self, insn: &Instruction) {
        let ap_addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let default_type = Type::basic(crate::types::TypeKind::Int);
        let arg_type = insn.typ.as_ref().unwrap_or(&default_type);
        let arg_size = arg_type.size_bits().max(32);
        let arg_bytes = (arg_size / 8).max(8) as i32; // Minimum 8 bytes per slot

        let ap_loc = self.get_location(ap_addr);
        let dst_loc = self.get_location(target);

        // Generate unique label for this va_arg call
        let label_suffix = self.unique_label_counter;
        self.unique_label_counter += 1;

        match &ap_loc {
            Loc::Stack(ap_offset) => {
                if arg_type.is_float() {
                    // For float args, use overflow_arg_area (FP register save not implemented)
                    // LIR: load overflow_arg_area pointer
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: ap_offset + 8,
                        }),
                        dst: GpOperand::Reg(Reg::Rax),
                    });

                    let fp_size = arg_type.size_bits();
                    let lir_fp_size = if fp_size <= 32 {
                        FpSize::Single
                    } else {
                        FpSize::Double
                    };
                    // LIR: load float value from overflow area
                    self.push_lir(X86Inst::MovFp {
                        size: lir_fp_size,
                        src: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rax,
                            offset: 0,
                        }),
                        dst: XmmOperand::Reg(XmmReg::Xmm15),
                    });

                    // Store to destination
                    match &dst_loc {
                        Loc::Xmm(x) => {
                            // LIR: move to destination XMM register
                            self.push_lir(X86Inst::MovFp {
                                size: lir_fp_size,
                                src: XmmOperand::Reg(XmmReg::Xmm15),
                                dst: XmmOperand::Reg(*x),
                            });
                        }
                        Loc::Stack(dst_offset) => {
                            // LIR: store to stack
                            self.push_lir(X86Inst::MovFp {
                                size: lir_fp_size,
                                src: XmmOperand::Reg(XmmReg::Xmm15),
                                dst: XmmOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rbp,
                                    offset: *dst_offset,
                                }),
                            });
                        }
                        _ => {}
                    }

                    // Advance overflow_arg_area
                    // LIR: add bytes to pointer
                    self.push_lir(X86Inst::Add {
                        size: OperandSize::B64,
                        src: GpOperand::Imm(arg_bytes as i64),
                        dst: Reg::Rax,
                    });
                    // LIR: store updated pointer
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::Rax),
                        dst: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: ap_offset + 8,
                        }),
                    });
                } else {
                    // For integer args, check gp_offset to see if we use reg_save_area or overflow
                    // gp_offset < 48 means we have saved registers to read
                    let overflow_label = Label::new("va_overflow", label_suffix);
                    let done_label = Label::new("va_done", label_suffix);

                    // LIR: load gp_offset
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: *ap_offset,
                        }),
                        dst: GpOperand::Reg(Reg::Rax),
                    });
                    // LIR: compare with 48
                    self.push_lir(X86Inst::Cmp {
                        size: OperandSize::B32,
                        src: GpOperand::Imm(48),
                        dst: GpOperand::Reg(Reg::Rax),
                    });
                    // LIR: jump if above or equal
                    self.push_lir(X86Inst::Jcc {
                        cc: IntCC::Ae,
                        target: overflow_label.clone(),
                    });

                    // Use register save area: load from reg_save_area + gp_offset
                    // LIR: load reg_save_area
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: ap_offset + 16,
                        }),
                        dst: GpOperand::Reg(Reg::R10),
                    });
                    // LIR: sign-extend gp_offset
                    self.push_lir(X86Inst::Movsx {
                        src_size: OperandSize::B32,
                        dst_size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::Rax),
                        dst: Reg::R11,
                    });
                    // LIR: calculate reg_save_area + gp_offset
                    self.push_lir(X86Inst::Add {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::R11),
                        dst: Reg::R10,
                    });

                    // Load value from save area
                    let lir_arg_size = OperandSize::from_bits(arg_size);
                    match &dst_loc {
                        Loc::Reg(r) => {
                            // LIR: load from save area to destination register
                            self.push_lir(X86Inst::Mov {
                                size: lir_arg_size,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::R10,
                                    offset: 0,
                                }),
                                dst: GpOperand::Reg(*r),
                            });
                        }
                        Loc::Stack(dst_offset) => {
                            // LIR: load to temp then store to stack
                            self.push_lir(X86Inst::Mov {
                                size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::R10,
                                    offset: 0,
                                }),
                                dst: GpOperand::Reg(Reg::R11),
                            });
                            self.push_lir(X86Inst::Mov {
                                size: OperandSize::B32,
                                src: GpOperand::Reg(Reg::R11),
                                dst: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rbp,
                                    offset: *dst_offset,
                                }),
                            });
                        }
                        _ => {}
                    }

                    // Increment gp_offset by 8
                    // LIR: add 8 to gp_offset
                    self.push_lir(X86Inst::Add {
                        size: OperandSize::B32,
                        src: GpOperand::Imm(8),
                        dst: Reg::Rax,
                    });
                    // LIR: store updated gp_offset
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Reg(Reg::Rax),
                        dst: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: *ap_offset,
                        }),
                    });
                    // LIR: jump to done
                    self.push_lir(X86Inst::Jmp {
                        target: done_label.clone(),
                    });

                    // Overflow path: use overflow_arg_area
                    // LIR: overflow label
                    self.push_lir(X86Inst::Directive(Directive::BlockLabel(overflow_label)));
                    // LIR: load overflow_arg_area
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: ap_offset + 8,
                        }),
                        dst: GpOperand::Reg(Reg::Rax),
                    });

                    match &dst_loc {
                        Loc::Reg(r) => {
                            // LIR: load from overflow area
                            self.push_lir(X86Inst::Mov {
                                size: lir_arg_size,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rax,
                                    offset: 0,
                                }),
                                dst: GpOperand::Reg(*r),
                            });
                        }
                        Loc::Stack(dst_offset) => {
                            // LIR: load to temp then store
                            self.push_lir(X86Inst::Mov {
                                size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rax,
                                    offset: 0,
                                }),
                                dst: GpOperand::Reg(Reg::R11),
                            });
                            self.push_lir(X86Inst::Mov {
                                size: OperandSize::B32,
                                src: GpOperand::Reg(Reg::R11),
                                dst: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rbp,
                                    offset: *dst_offset,
                                }),
                            });
                        }
                        _ => {}
                    }

                    // Advance overflow_arg_area
                    // LIR: add arg_bytes
                    self.push_lir(X86Inst::Add {
                        size: OperandSize::B64,
                        src: GpOperand::Imm(arg_bytes as i64),
                        dst: Reg::Rax,
                    });
                    // LIR: store updated pointer
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::Rax),
                        dst: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: ap_offset + 8,
                        }),
                    });

                    // Done label
                    self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
                }
            }
            Loc::Reg(ap_reg) => {
                // Similar logic for when va_list is in a register
                if arg_type.is_float() {
                    // LIR: load overflow_arg_area pointer (at offset 8 from va_list base)
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: *ap_reg,
                            offset: 8,
                        }),
                        dst: GpOperand::Reg(Reg::Rax),
                    });

                    let fp_size = arg_type.size_bits();
                    let lir_fp_size = if fp_size <= 32 {
                        FpSize::Single
                    } else {
                        FpSize::Double
                    };
                    // LIR: load float value
                    self.push_lir(X86Inst::MovFp {
                        size: lir_fp_size,
                        src: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rax,
                            offset: 0,
                        }),
                        dst: XmmOperand::Reg(XmmReg::Xmm15),
                    });

                    match &dst_loc {
                        Loc::Xmm(x) => {
                            // LIR: move to destination XMM
                            self.push_lir(X86Inst::MovFp {
                                size: lir_fp_size,
                                src: XmmOperand::Reg(XmmReg::Xmm15),
                                dst: XmmOperand::Reg(*x),
                            });
                        }
                        Loc::Stack(dst_offset) => {
                            // LIR: store to stack
                            self.push_lir(X86Inst::MovFp {
                                size: lir_fp_size,
                                src: XmmOperand::Reg(XmmReg::Xmm15),
                                dst: XmmOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rbp,
                                    offset: *dst_offset,
                                }),
                            });
                        }
                        _ => {}
                    }

                    // LIR: advance overflow_arg_area
                    self.push_lir(X86Inst::Add {
                        size: OperandSize::B64,
                        src: GpOperand::Imm(arg_bytes as i64),
                        dst: Reg::Rax,
                    });
                    // LIR: store updated pointer
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::Rax),
                        dst: GpOperand::Mem(MemAddr::BaseOffset {
                            base: *ap_reg,
                            offset: 8,
                        }),
                    });
                } else {
                    // Check gp_offset
                    let overflow_label = Label::new("va_overflow", label_suffix);
                    let done_label = Label::new("va_done", label_suffix);

                    // LIR: load gp_offset
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: *ap_reg,
                            offset: 0,
                        }),
                        dst: GpOperand::Reg(Reg::Rax),
                    });
                    // LIR: compare with 48
                    self.push_lir(X86Inst::Cmp {
                        size: OperandSize::B32,
                        src: GpOperand::Imm(48),
                        dst: GpOperand::Reg(Reg::Rax),
                    });
                    // LIR: conditional jump
                    self.push_lir(X86Inst::Jcc {
                        cc: IntCC::Ae,
                        target: overflow_label.clone(),
                    });

                    // Use register save area
                    // LIR: load reg_save_area (at offset 16)
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: *ap_reg,
                            offset: 16,
                        }),
                        dst: GpOperand::Reg(Reg::R10),
                    });
                    // LIR: sign-extend gp_offset
                    self.push_lir(X86Inst::Movsx {
                        src_size: OperandSize::B32,
                        dst_size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::Rax),
                        dst: Reg::R11,
                    });
                    // LIR: add offset to base
                    self.push_lir(X86Inst::Add {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::R11),
                        dst: Reg::R10,
                    });

                    let lir_arg_size = OperandSize::from_bits(arg_size);
                    match &dst_loc {
                        Loc::Reg(r) => {
                            // LIR: load from save area
                            self.push_lir(X86Inst::Mov {
                                size: lir_arg_size,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::R10,
                                    offset: 0,
                                }),
                                dst: GpOperand::Reg(*r),
                            });
                        }
                        Loc::Stack(dst_offset) => {
                            // LIR: load to temp then store
                            self.push_lir(X86Inst::Mov {
                                size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::R10,
                                    offset: 0,
                                }),
                                dst: GpOperand::Reg(Reg::R11),
                            });
                            self.push_lir(X86Inst::Mov {
                                size: OperandSize::B32,
                                src: GpOperand::Reg(Reg::R11),
                                dst: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rbp,
                                    offset: *dst_offset,
                                }),
                            });
                        }
                        _ => {}
                    }

                    // Increment gp_offset by 8
                    // LIR: add 8
                    self.push_lir(X86Inst::Add {
                        size: OperandSize::B32,
                        src: GpOperand::Imm(8),
                        dst: Reg::Rax,
                    });
                    // LIR: store updated gp_offset
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Reg(Reg::Rax),
                        dst: GpOperand::Mem(MemAddr::BaseOffset {
                            base: *ap_reg,
                            offset: 0,
                        }),
                    });
                    // LIR: jump to done
                    self.push_lir(X86Inst::Jmp {
                        target: done_label.clone(),
                    });

                    // Overflow path: use overflow_arg_area
                    // LIR: overflow label
                    self.push_lir(X86Inst::Directive(Directive::BlockLabel(overflow_label)));
                    // LIR: load overflow_arg_area
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: *ap_reg,
                            offset: 8,
                        }),
                        dst: GpOperand::Reg(Reg::Rax),
                    });

                    match &dst_loc {
                        Loc::Reg(r) => {
                            // LIR: load from overflow area
                            self.push_lir(X86Inst::Mov {
                                size: lir_arg_size,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rax,
                                    offset: 0,
                                }),
                                dst: GpOperand::Reg(*r),
                            });
                        }
                        Loc::Stack(dst_offset) => {
                            // LIR: load to temp then store
                            self.push_lir(X86Inst::Mov {
                                size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rax,
                                    offset: 0,
                                }),
                                dst: GpOperand::Reg(Reg::R11),
                            });
                            self.push_lir(X86Inst::Mov {
                                size: OperandSize::B32,
                                src: GpOperand::Reg(Reg::R11),
                                dst: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rbp,
                                    offset: *dst_offset,
                                }),
                            });
                        }
                        _ => {}
                    }

                    // Advance overflow_arg_area
                    // LIR: add arg_bytes
                    self.push_lir(X86Inst::Add {
                        size: OperandSize::B64,
                        src: GpOperand::Imm(arg_bytes as i64),
                        dst: Reg::Rax,
                    });
                    // LIR: store updated pointer
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::Rax),
                        dst: GpOperand::Mem(MemAddr::BaseOffset {
                            base: *ap_reg,
                            offset: 8,
                        }),
                    });

                    // Done label
                    self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
                }
            }
            _ => {}
        }
    }

    /// Emit va_copy: Copy a va_list (24 bytes)
    fn emit_va_copy(&mut self, insn: &Instruction) {
        let dest_addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let src_addr = match insn.src.get(1) {
            Some(&s) => s,
            None => return,
        };

        let dest_loc = self.get_location(dest_addr);
        let src_loc = self.get_location(src_addr);

        // Copy 24 bytes from src to dest
        // Both src_loc and dest_loc contain addresses of va_list structs
        match (&src_loc, &dest_loc) {
            (Loc::Stack(src_off), Loc::Stack(dst_off)) => {
                // Copy gp_offset (4 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *src_off,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *dst_off,
                    }),
                });
                // Copy fp_offset (4 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 4,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 4,
                    }),
                });
                // Copy overflow_arg_area (8 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 8,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 8,
                    }),
                });
                // Copy reg_save_area (8 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 16,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 16,
                    }),
                });
            }
            (Loc::Reg(src_reg), Loc::Reg(dst_reg)) => {
                // Both src and dest are in registers (containing addresses)
                // Copy gp_offset (4 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 0,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 0,
                    }),
                });
                // Copy fp_offset (4 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 4,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 4,
                    }),
                });
                // Copy overflow_arg_area (8 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 8,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 8,
                    }),
                });
                // Copy reg_save_area (8 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 16,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 16,
                    }),
                });
            }
            (Loc::Reg(src_reg), Loc::Stack(dst_off)) => {
                // Src in register, dest on stack
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 0,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *dst_off,
                    }),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 4,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 4,
                    }),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 8,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 8,
                    }),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 16,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 16,
                    }),
                });
            }
            (Loc::Stack(src_off), Loc::Reg(dst_reg)) => {
                // Src on stack, dest in register
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *src_off,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 0,
                    }),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 4,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 4,
                    }),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 8,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 8,
                    }),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 16,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 16,
                    }),
                });
            }
            _ => {}
        }
    }

    // =========================================================================
    // Byte-swapping builtins
    // =========================================================================

    /// Emit bswap16: Byte-swap a 16-bit value
    /// x86-64 doesn't have bswap for 16-bit, use ror $8 (rotate right 8 bits)
    fn emit_bswap16(&mut self, insn: &Instruction) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let dst = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_loc = self.get_location(src);
        let dst_loc = self.get_location(dst);

        // Load source into %ax
        match src_loc {
            Loc::Reg(r) => {
                if r != Reg::Rax {
                    // LIR: zero-extend 16-bit to 32-bit
                    self.push_lir(X86Inst::Movzx {
                        src_size: OperandSize::B16,
                        dst_size: OperandSize::B32,
                        src: GpOperand::Reg(r),
                        dst: Reg::Rax,
                    });
                }
            }
            Loc::Stack(off) => {
                // LIR: zero-extend load from stack
                self.push_lir(X86Inst::Movzx {
                    src_size: OperandSize::B16,
                    dst_size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                    dst: Reg::Rax,
                });
            }
            Loc::Imm(v) => {
                // LIR: load immediate
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(v),
                    dst: GpOperand::Reg(Reg::Rax),
                });
            }
            _ => return,
        }

        // Rotate 8 bits right to swap bytes
        // LIR: ror
        self.push_lir(X86Inst::Ror {
            size: OperandSize::B16,
            count: ShiftCount::Imm(8),
            dst: Reg::Rax,
        });

        // Store result
        match dst_loc {
            Loc::Reg(r) => {
                if r != Reg::Rax {
                    // LIR: zero-extend result
                    self.push_lir(X86Inst::Movzx {
                        src_size: OperandSize::B16,
                        dst_size: OperandSize::B32,
                        src: GpOperand::Reg(Reg::Rax),
                        dst: r,
                    });
                }
            }
            Loc::Stack(off) => {
                // LIR: store 16-bit result
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B16,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                });
            }
            _ => {}
        }
    }

    /// Emit bswap32: Byte-swap a 32-bit value
    fn emit_bswap32(&mut self, insn: &Instruction) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let dst = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_loc = self.get_location(src);
        let dst_loc = self.get_location(dst);

        // Load source into %eax
        match src_loc {
            Loc::Reg(r) => {
                if r != Reg::Rax {
                    // LIR: move 32-bit value to rax
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Reg(r),
                        dst: GpOperand::Reg(Reg::Rax),
                    });
                }
            }
            Loc::Stack(off) => {
                // LIR: load 32-bit from stack
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
            }
            Loc::Imm(v) => {
                // LIR: load immediate
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(v),
                    dst: GpOperand::Reg(Reg::Rax),
                });
            }
            _ => return,
        }

        // Byte-swap 32-bit value
        // LIR: bswap
        self.push_lir(X86Inst::Bswap {
            size: OperandSize::B32,
            reg: Reg::Rax,
        });

        // Store result
        match dst_loc {
            Loc::Reg(r) => {
                if r != Reg::Rax {
                    // LIR: move result
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Reg(Reg::Rax),
                        dst: GpOperand::Reg(r),
                    });
                }
            }
            Loc::Stack(off) => {
                // LIR: store to stack
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                });
            }
            _ => {}
        }
    }

    /// Emit bswap64: Byte-swap a 64-bit value
    fn emit_bswap64(&mut self, insn: &Instruction) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let dst = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_loc = self.get_location(src);
        let dst_loc = self.get_location(dst);

        // Load source into %rax
        match src_loc {
            Loc::Reg(r) => {
                if r != Reg::Rax {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: GpOperand::Reg(Reg::Rax),
                    });
                }
            }
            Loc::Stack(off) => {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
            }
            Loc::Imm(v) => {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Imm(v),
                    dst: GpOperand::Reg(Reg::Rax),
                });
            }
            _ => return,
        }

        // Byte-swap 64-bit value
        self.push_lir(X86Inst::Bswap {
            size: OperandSize::B64,
            reg: Reg::Rax,
        });

        // Store result
        match dst_loc {
            Loc::Reg(r) => {
                if r != Reg::Rax {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::Rax),
                        dst: GpOperand::Reg(r),
                    });
                }
            }
            Loc::Stack(off) => {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                });
            }
            _ => {}
        }
    }

    /// Emit __builtin_alloca - dynamic stack allocation
    fn emit_alloca(&mut self, insn: &Instruction) {
        let size = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load size into RAX
        self.emit_move(size, Reg::Rax, 64);

        // Round up to 16-byte alignment: (size + 15) & ~15
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Imm(15),
            dst: Reg::Rax,
        });
        self.push_lir(X86Inst::And {
            size: OperandSize::B64,
            src: GpOperand::Imm(-16),
            dst: Reg::Rax,
        });

        // Subtract from stack pointer
        self.push_lir(X86Inst::Sub {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rax),
            dst: Reg::Rsp,
        });

        // Return new stack pointer
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rsp),
            dst: GpOperand::Reg(Reg::Rax),
        });

        // Store result
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::Rax, &dst_loc, 64);
    }
}

// ============================================================================
// CodeGenerator trait implementation
// ============================================================================

impl CodeGenerator for X86_64CodeGen {
    fn generate(&mut self, module: &Module) -> String {
        self.output.clear();
        self.last_debug_line = 0;
        self.last_debug_file = 0;
        self.emit_debug = module.debug;

        // Emit file header
        self.emit_header();

        // Emit .file directives unconditionally (useful for diagnostics/profiling)
        for (i, path) in module.source_files.iter().enumerate() {
            // File indices in DWARF start at 1
            self.push_lir(X86Inst::Directive(Directive::file(
                (i + 1) as u32,
                path.clone(),
            )));
        }

        // Emit globals
        for (name, typ, init) in &module.globals {
            self.emit_global(name, typ, init);
        }

        // Emit string literals
        if !module.strings.is_empty() {
            self.emit_strings(&module.strings);
        }

        // Emit functions
        for func in &module.functions {
            self.emit_function(func);
        }

        // Emit all buffered LIR instructions to output string
        self.emit_all();

        self.output.clone()
    }

    fn set_emit_unwind_tables(&mut self, emit: bool) {
        self.emit_unwind_tables = emit;
    }
}

impl X86_64CodeGen {
    fn emit_strings(&mut self, strings: &[(String, String)]) {
        // Switch to rodata section (Directive handles OS differences)
        self.push_lir(X86Inst::Directive(Directive::Rodata));

        for (label, content) in strings {
            // String labels are local (start with .)
            self.push_lir(X86Inst::Directive(Directive::local_label(label.clone())));
            // Emit string with proper escaping
            self.push_lir(X86Inst::Directive(Directive::Asciz(Self::escape_string(
                content,
            ))));
        }

        // Switch back to text section for functions
        self.push_lir(X86Inst::Directive(Directive::Text));
    }

    fn escape_string(s: &str) -> String {
        let mut result = String::new();
        for c in s.chars() {
            match c {
                '\n' => result.push_str("\\n"),
                '\r' => result.push_str("\\r"),
                '\t' => result.push_str("\\t"),
                '\\' => result.push_str("\\\\"),
                '"' => result.push_str("\\\""),
                c if c.is_ascii_graphic() || c == ' ' => result.push(c),
                c => {
                    // Emit as octal escape
                    result.push_str(&format!("\\{:03o}", c as u32));
                }
            }
        }
        result
    }
}
