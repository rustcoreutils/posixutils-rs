//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 Register Allocator
// Linear scan register allocation for x86-64
//
// ============================================================================
// REGISTER ALLOCATION POLICY (LLVM-style constraint-aware allocation)
// ============================================================================
//
// Reserved registers (NEVER allocated to pseudos):
//   R10, R11 - Codegen scratch registers
//
// Codegen MUST use only scratch registers (R10, R11) for temporaries.
// Using allocatable registers (Rax, Rbx, etc.) risks clobbering live values
// that were assigned by the register allocator.
//
// Constrained registers (handled via LLVM-style constraint system):
//   Rax:Rdx - Clobbered by division (idiv/div uses edx:eax dividend, writes
//             quotient to eax and remainder to edx)
//   Rcx     - Shift counts (shl, shr, sar) for variable shifts
//
// The allocator uses RegConstraints to track which instructions clobber
// specific registers, and ConstraintPoint to identify positions where
// constraints apply. When allocating, pseudos that are live across a
// constraint point (but not involved in that instruction) are excluded
// from being allocated to the clobbered registers.
//
// Calling convention (System V AMD64 ABI):
//   Rdi, Rsi, Rdx, Rcx, R8, R9 - Integer arguments
//   Xmm0-Xmm7                   - FP arguments
//   Rax, Xmm0                   - Return values
//   Rbx, Rbp, R12-R15          - Callee-saved
// ============================================================================

use crate::arch::regalloc::{
    expire_intervals, find_call_positions, identify_fp_pseudos, interval_crosses_call, FpReg,
    GpReg, LiveInterval,
};
use crate::ir::{Function, Opcode, PseudoId, PseudoKind};
use crate::types::TypeTable;
use std::collections::{HashMap, HashSet};

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

    /// All allocatable registers (excluding RSP, RBP, R10, R11)
    /// R10 and R11 are reserved as scratch registers for codegen operations.
    /// Codegen MUST use only R10/R11 for temporaries to avoid clobbering
    /// live values assigned by the register allocator.
    ///
    /// Note: Rax, Rdx, and Rcx are allocatable but have constraints:
    /// - Rax/Rdx: Clobbered by division instructions
    /// - Rcx: Required for variable shift counts
    ///
    /// These constraints are handled by the LLVM-style constraint system
    /// in the register allocator, NOT by codegen save/restore.
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
            // R10 and R11 are reserved as scratch for codegen
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

    /// Callee-saved registers available for allocation
    pub fn callee_saved() -> &'static [Reg] {
        &[Reg::Rbx, Reg::R12, Reg::R13, Reg::R14, Reg::R15]
    }
}

// Implement the GpReg trait for x86-64 registers
impl GpReg for Reg {
    fn name(&self) -> &'static str {
        self.name64()
    }

    fn name_for_size(&self, bits: u32) -> &'static str {
        Reg::name_for_size(self, bits)
    }

    fn is_callee_saved(&self) -> bool {
        Reg::is_callee_saved(self)
    }

    fn arg_regs() -> &'static [Self] {
        Reg::arg_regs()
    }

    fn callee_saved_regs() -> &'static [Self] {
        Reg::callee_saved()
    }

    fn allocatable_regs() -> &'static [Self] {
        Reg::allocatable()
    }
}

// ============================================================================
// Register Constraints (LLVM-style constraint-aware allocation)
// ============================================================================

/// Register constraints for an instruction.
/// Used by the register allocator to avoid assigning pseudos to registers
/// that will be clobbered by constrained instructions like division.
#[derive(Debug, Clone)]
pub struct RegConstraints {
    /// Registers that are clobbered (written) by this instruction
    pub clobbers: &'static [Reg],
    /// Registers that must hold specific input values
    #[allow(dead_code)]
    pub inputs: &'static [Reg],
}

impl RegConstraints {
    /// No constraints - most instructions have no implicit register requirements
    pub const NONE: RegConstraints = RegConstraints {
        clobbers: &[],
        inputs: &[],
    };
}

/// Get register constraints for an opcode.
/// Division clobbers Rax (quotient) and Rdx (remainder).
/// Shifts require the count in Cl (Rcx) for variable shifts.
/// VaArg clobbers Rax (used as scratch) and Rcx (used for sign-extended offset).
pub fn opcode_constraints(op: Opcode) -> RegConstraints {
    match op {
        // Division: uses Rax:Rdx as dividend, clobbers both with quotient/remainder
        Opcode::DivS | Opcode::DivU | Opcode::ModS | Opcode::ModU => RegConstraints {
            clobbers: &[Reg::Rax, Reg::Rdx],
            inputs: &[Reg::Rax],
        },
        // Variable shifts: count must be in Cl (Rcx)
        // Note: shifts don't clobber Rcx, they just require it as input
        Opcode::Shl | Opcode::Lsr | Opcode::Asr => RegConstraints {
            clobbers: &[],
            inputs: &[Reg::Rcx],
        },
        // VaArg: emit_va_arg_int uses Rax for reg_save_area/overflow pointer,
        // and Rcx for sign-extended offset calculation. Any pseudo that is live
        // across a va_arg (but not the result or va_list operand) must not be
        // in these registers.
        Opcode::VaArg => RegConstraints {
            clobbers: &[Reg::Rax, Reg::Rcx],
            inputs: &[],
        },
        _ => RegConstraints::NONE,
    }
}

/// A point in the function where register constraints apply.
/// Used during allocation to avoid assigning pseudos to registers
/// that would be clobbered at this point.
#[derive(Debug, Clone)]
pub struct ConstraintPoint {
    /// Instruction position in the function
    pub position: usize,
    /// Registers clobbered at this point
    pub clobbers: Vec<Reg>,
    /// Pseudos that ARE the operands of the constrained instruction
    /// (these should NOT be evicted - they're the actual div/mod operands)
    pub involved_pseudos: Vec<PseudoId>,
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

    /// Callee-saved FP registers (none on System V ABI)
    pub fn callee_saved() -> &'static [XmmReg] {
        // System V AMD64 ABI: all XMM registers are caller-saved
        &[]
    }
}

// Implement the FpReg trait for x86-64 XMM registers
impl FpReg for XmmReg {
    fn name(&self) -> &'static str {
        XmmReg::name(self)
    }

    fn name_for_size(&self, _bits: u32) -> &'static str {
        // XMM registers don't have size variants - always use full name
        XmmReg::name(self)
    }

    fn is_callee_saved(&self) -> bool {
        // System V AMD64 ABI: all XMM registers are caller-saved
        false
    }

    fn arg_regs() -> &'static [Self] {
        XmmReg::arg_regs()
    }

    fn callee_saved_regs() -> &'static [Self] {
        XmmReg::callee_saved()
    }

    fn allocatable_regs() -> &'static [Self] {
        XmmReg::allocatable()
    }
}

// ============================================================================
// Operand - Location of a value (register or memory)
// ============================================================================

/// Location of a value
#[derive(Debug, Clone, PartialEq)]
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

/// Information about an argument spilled from a caller-saved register to stack
#[derive(Debug, Clone)]
pub struct SpilledArg {
    /// The pseudo that was spilled
    pub pseudo: PseudoId,
    /// The register the argument originally arrived in
    pub from_reg: Reg,
    /// The stack offset where it was spilled to
    pub to_stack_offset: i32,
}

// ============================================================================
// Register Allocator (Linear Scan)
// ============================================================================

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
    fp_pseudos: HashSet<PseudoId>,
    /// Arguments spilled from caller-saved registers to stack
    spilled_args: Vec<SpilledArg>,
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
            fp_pseudos: HashSet::new(),
            spilled_args: Vec::new(),
        }
    }

    /// Perform register allocation for a function
    pub fn allocate(&mut self, func: &Function, types: &TypeTable) -> HashMap<PseudoId, Loc> {
        self.reset_state();
        // Use shared identify_fp_pseudos with type-checker closure
        self.fp_pseudos = identify_fp_pseudos(func, |typ| types.is_float(typ));
        self.allocate_arguments(func, types);

        let (intervals, constraint_points) = self.compute_live_intervals(func);
        let call_positions = find_call_positions(func);

        self.spill_args_across_calls(func, &intervals, &call_positions);
        self.allocate_alloca_to_stack(func);
        self.run_linear_scan(func, types, intervals, &call_positions, &constraint_points);

        self.locations.clone()
    }

    /// Reset allocator state for a new function
    fn reset_state(&mut self) {
        self.locations.clear();
        self.free_regs = Reg::allocatable().to_vec();
        self.free_xmm_regs = XmmReg::allocatable().to_vec();
        self.active.clear();
        self.active_xmm.clear();
        self.stack_offset = 0;
        self.used_callee_saved.clear();
        self.fp_pseudos.clear();
        self.spilled_args.clear();
    }

    /// Pre-allocate argument registers per System V AMD64 ABI
    fn allocate_arguments(&mut self, func: &Function, types: &TypeTable) {
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = XmmReg::arg_regs();
        let mut int_arg_idx = 0;
        let mut fp_arg_idx = 0;

        // Detect hidden return pointer for large struct returns
        let sret_pseudo = func
            .pseudos
            .iter()
            .find(|p| matches!(p.kind, PseudoKind::Arg(0)) && p.name.as_deref() == Some("__sret"));
        let arg_idx_offset: u32 = if sret_pseudo.is_some() { 1 } else { 0 };

        // Allocate RDI for hidden return pointer if present
        if let Some(sret) = sret_pseudo {
            self.locations.insert(sret.id, Loc::Reg(int_arg_regs[0]));
            self.free_regs.retain(|&r| r != int_arg_regs[0]);
            int_arg_idx += 1;
        }

        for (i, (_name, typ)) in func.params.iter().enumerate() {
            for pseudo in &func.pseudos {
                if let PseudoKind::Arg(arg_idx) = pseudo.kind {
                    if arg_idx == (i as u32) + arg_idx_offset {
                        let is_fp = types.is_float(*typ);
                        if is_fp {
                            if fp_arg_idx < fp_arg_regs.len() {
                                self.locations
                                    .insert(pseudo.id, Loc::Xmm(fp_arg_regs[fp_arg_idx]));
                                self.free_xmm_regs.retain(|&r| r != fp_arg_regs[fp_arg_idx]);
                                self.fp_pseudos.insert(pseudo.id);
                            } else {
                                let offset =
                                    16 + (i - int_arg_regs.len() - fp_arg_regs.len()) as i32 * 8;
                                self.locations.insert(pseudo.id, Loc::Stack(-offset));
                            }
                            fp_arg_idx += 1;
                        } else {
                            if int_arg_idx < int_arg_regs.len() {
                                self.locations
                                    .insert(pseudo.id, Loc::Reg(int_arg_regs[int_arg_idx]));
                                self.free_regs.retain(|&r| r != int_arg_regs[int_arg_idx]);
                            } else {
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
    }

    /// Spill arguments in caller-saved registers if their interval crosses a call
    fn spill_args_across_calls(
        &mut self,
        _func: &Function,
        intervals: &[LiveInterval],
        call_positions: &[usize],
    ) {
        // Check arguments in caller-saved registers
        let int_arg_regs_set: Vec<Reg> = Reg::arg_regs().to_vec();
        for interval in intervals {
            if let Some(Loc::Reg(reg)) = self.locations.get(&interval.pseudo) {
                if int_arg_regs_set.contains(reg) && interval_crosses_call(interval, call_positions)
                {
                    let from_reg = *reg;
                    self.stack_offset += 8;
                    let to_stack_offset = self.stack_offset;

                    // Record the spill for codegen to emit stores in prologue
                    self.spilled_args.push(SpilledArg {
                        pseudo: interval.pseudo,
                        from_reg,
                        to_stack_offset,
                    });

                    self.locations
                        .insert(interval.pseudo, Loc::Stack(to_stack_offset));
                    self.free_regs.push(from_reg);
                }
            }
        }
    }

    /// Get arguments that were spilled from caller-saved registers
    pub fn spilled_args(&self) -> &[SpilledArg] {
        &self.spilled_args
    }

    /// Force alloca results to stack to avoid clobbering issues
    fn allocate_alloca_to_stack(&mut self, func: &Function) {
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
    }

    /// Find registers that would conflict with this interval due to constraints.
    /// If a pseudo is live at a constraint point (e.g., division) and is NOT an operand
    /// of that instruction, it cannot be allocated to any clobbered register (Rax/Rdx).
    fn find_conflicting_registers(
        &self,
        interval: &LiveInterval,
        constraint_points: &[ConstraintPoint],
    ) -> HashSet<Reg> {
        let mut conflicts = HashSet::new();

        for cp in constraint_points {
            // If interval is live at this constraint point...
            // Use <= for end to handle case where interval ends exactly at constraint point
            if interval.start <= cp.position && cp.position <= interval.end {
                // ...and this pseudo is NOT involved in the constrained instruction...
                if !cp.involved_pseudos.contains(&interval.pseudo) {
                    // ...then it cannot be in any clobbered register
                    for &reg in &cp.clobbers {
                        conflicts.insert(reg);
                    }
                }
            }
        }

        conflicts
    }

    /// Run the linear scan allocation algorithm
    fn run_linear_scan(
        &mut self,
        func: &Function,
        types: &TypeTable,
        intervals: Vec<LiveInterval>,
        call_positions: &[usize],
        constraint_points: &[ConstraintPoint],
    ) {
        for interval in intervals {
            self.expire_old_intervals(interval.start);

            if self.locations.contains_key(&interval.pseudo) {
                continue;
            }

            // Handle constants and symbols
            if let Some(pseudo) = func.pseudos.iter().find(|p| p.id == interval.pseudo) {
                match &pseudo.kind {
                    PseudoKind::Val(v) => {
                        self.locations.insert(interval.pseudo, Loc::Imm(*v));
                        continue;
                    }
                    PseudoKind::FVal(v) => {
                        let size = func
                            .blocks
                            .iter()
                            .flat_map(|b| &b.insns)
                            .find(|insn| {
                                insn.op == Opcode::SetVal && insn.target == Some(interval.pseudo)
                            })
                            .map(|insn| insn.size)
                            .unwrap_or(64);
                        self.locations.insert(interval.pseudo, Loc::FImm(*v, size));
                        self.fp_pseudos.insert(interval.pseudo);
                        continue;
                    }
                    PseudoKind::Sym(name) => {
                        if let Some(local_var) = func.locals.get(name) {
                            let size = (types.size_bits(local_var.typ) / 8) as i32;
                            let size = size.max(8);
                            let aligned_size = (size + 7) & !7;
                            self.stack_offset += aligned_size;
                            self.locations
                                .insert(interval.pseudo, Loc::Stack(self.stack_offset));
                            if types.is_float(local_var.typ) {
                                self.fp_pseudos.insert(interval.pseudo);
                            }
                        } else {
                            self.locations
                                .insert(interval.pseudo, Loc::Global(name.clone()));
                        }
                        continue;
                    }
                    _ => {}
                }
            }

            // Allocate register based on type
            let needs_fp = self.fp_pseudos.contains(&interval.pseudo);
            let crosses_call = interval_crosses_call(&interval, call_positions);

            // Find registers that conflict due to constraints (e.g., Rax/Rdx for division)
            let conflicting_regs = self.find_conflicting_registers(&interval, constraint_points);

            if needs_fp {
                if let Some(xmm) = self.free_xmm_regs.pop() {
                    self.locations.insert(interval.pseudo, Loc::Xmm(xmm));
                    self.active_xmm.push((interval.clone(), xmm));
                    self.active_xmm.sort_by_key(|(i, _)| i.end);
                } else {
                    self.stack_offset += 8;
                    self.locations
                        .insert(interval.pseudo, Loc::Stack(self.stack_offset));
                }
            } else if crosses_call {
                // Interval crosses a call - must use callee-saved register or spill
                // Also exclude registers that conflict with constraints
                if let Some(idx) = self
                    .free_regs
                    .iter()
                    .position(|r| r.is_callee_saved() && !conflicting_regs.contains(r))
                {
                    let reg = self.free_regs.remove(idx);
                    if !self.used_callee_saved.contains(&reg) {
                        self.used_callee_saved.push(reg);
                    }
                    self.locations.insert(interval.pseudo, Loc::Reg(reg));
                    self.active.push((interval.clone(), reg));
                    self.active.sort_by_key(|(i, _)| i.end);
                } else {
                    // No callee-saved registers available - spill to stack
                    self.stack_offset += 8;
                    self.locations
                        .insert(interval.pseudo, Loc::Stack(self.stack_offset));
                }
            } else {
                // Interval doesn't cross a call - prefer caller-saved registers
                // to minimize callee-saved register usage and stack conflicts
                // Also exclude registers that conflict with constraints
                let reg_opt = if let Some(idx) = self
                    .free_regs
                    .iter()
                    .position(|r| !r.is_callee_saved() && !conflicting_regs.contains(r))
                {
                    Some(self.free_regs.remove(idx))
                } else if let Some(idx) = self
                    .free_regs
                    .iter()
                    .position(|r| !conflicting_regs.contains(r))
                {
                    Some(self.free_regs.remove(idx))
                } else {
                    None
                };

                if let Some(reg) = reg_opt {
                    if reg.is_callee_saved() && !self.used_callee_saved.contains(&reg) {
                        self.used_callee_saved.push(reg);
                    }
                    self.locations.insert(interval.pseudo, Loc::Reg(reg));
                    self.active.push((interval.clone(), reg));
                    self.active.sort_by_key(|(i, _)| i.end);
                } else {
                    self.stack_offset += 8;
                    self.locations
                        .insert(interval.pseudo, Loc::Stack(self.stack_offset));
                }
            }
        }
    }

    fn expire_old_intervals(&mut self, point: usize) {
        // Expire integer register intervals
        expire_intervals(&mut self.active, &mut self.free_regs, point);
        // Expire XMM register intervals
        expire_intervals(&mut self.active_xmm, &mut self.free_xmm_regs, point);
    }

    /// Compute live intervals and collect constraint points.
    /// Returns (intervals, constraint_points) where:
    /// - intervals: Live ranges for each pseudo
    /// - constraint_points: Positions where register constraints apply (e.g., division clobbers)
    fn compute_live_intervals(&self, func: &Function) -> (Vec<LiveInterval>, Vec<ConstraintPoint>) {
        use crate::ir::BasicBlockId;

        struct IntervalInfo {
            pseudo: PseudoId,
            first_def: usize,
            last_def: usize,
            last_use: usize,
        }

        let mut intervals: HashMap<PseudoId, IntervalInfo> = HashMap::new();
        let mut constraint_points: Vec<ConstraintPoint> = Vec::new();
        let mut pos = 0usize;

        // First pass: compute block start and end positions
        let mut block_start_pos: HashMap<BasicBlockId, usize> = HashMap::new();
        let mut block_end_pos: HashMap<BasicBlockId, usize> = HashMap::new();
        let mut temp_pos = 0usize;
        for block in &func.blocks {
            block_start_pos.insert(block.id, temp_pos);
            temp_pos += block.insns.len();
            block_end_pos.insert(block.id, temp_pos.saturating_sub(1));
        }

        // Pre-initialize intervals for argument pseudos with first_def = 0
        // because arguments are live from function entry
        for pseudo in &func.pseudos {
            if let PseudoKind::Arg(_) = pseudo.kind {
                intervals.insert(
                    pseudo.id,
                    IntervalInfo {
                        pseudo: pseudo.id,
                        first_def: 0,
                        last_def: 0,
                        last_use: 0,
                    },
                );
            }
        }

        // Collect phi sources and targets
        let mut phi_sources: Vec<(BasicBlockId, PseudoId)> = Vec::new();
        let mut phi_targets: Vec<(BasicBlockId, PseudoId)> = Vec::new();

        for block in &func.blocks {
            for insn in &block.insns {
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

                // For indirect calls, the indirect_target pseudo is also used
                if let Some(indirect) = insn.indirect_target {
                    if let Some(info) = intervals.get_mut(&indirect) {
                        info.last_use = info.last_use.max(pos);
                    } else {
                        intervals.insert(
                            indirect,
                            IntervalInfo {
                                pseudo: indirect,
                                first_def: pos,
                                last_def: pos,
                                last_use: pos,
                            },
                        );
                    }
                }

                for (src_bb, pseudo) in &insn.phi_list {
                    phi_sources.push((*src_bb, *pseudo));
                    if let Some(target) = insn.target {
                        phi_targets.push((*src_bb, target));
                    }
                }

                // Collect constraint points for instructions that clobber registers
                let constraints = opcode_constraints(insn.op);
                if !constraints.clobbers.is_empty() {
                    // Collect pseudos involved in this instruction.
                    // "Involved" pseudos are ALLOWED to be in clobbered registers.
                    //
                    // For division: dividend must be in Rax, so target and sources are involved
                    // For VaArg: only target should be involved (result goes to Rax),
                    //            sources (va_list pointer) must NOT be in Rax/Rcx
                    let mut involved = Vec::new();
                    if let Some(t) = insn.target {
                        involved.push(t);
                    }
                    // For VaArg, sources should NOT be in clobbered registers,
                    // so we don't add them to involved_pseudos
                    if insn.op != Opcode::VaArg {
                        involved.extend(insn.src.iter().copied());
                    }

                    constraint_points.push(ConstraintPoint {
                        position: pos,
                        clobbers: constraints.clobbers.to_vec(),
                        involved_pseudos: involved,
                    });
                }

                pos += 1;
            }
        }

        // Extend phi source intervals to end of their source block
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

        // Extend phi target intervals
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

        // Handle loop back edges
        let mut loop_back_edges: Vec<(BasicBlockId, BasicBlockId, usize)> = Vec::new();
        for block in &func.blocks {
            if let Some(last_insn) = block.insns.last() {
                let mut targets = Vec::new();
                if let Some(target) = last_insn.bb_true {
                    targets.push(target);
                }
                if let Some(target) = last_insn.bb_false {
                    targets.push(target);
                }

                let from_start = block_start_pos.get(&block.id).copied().unwrap_or(0);
                for target_bb in targets {
                    let target_start = block_start_pos.get(&target_bb).copied().unwrap_or(0);
                    if target_start < from_start {
                        let from_end = block_end_pos.get(&block.id).copied().unwrap_or(0);
                        loop_back_edges.push((block.id, target_bb, from_end));
                    }
                }
            }
        }

        // Extend lifetimes for loop variables
        for (_from_bb, to_bb, back_edge_pos) in &loop_back_edges {
            let loop_start = block_start_pos.get(to_bb).copied().unwrap_or(0);

            for info in intervals.values_mut() {
                if info.first_def < loop_start
                    && info.last_use >= loop_start
                    && info.last_use <= *back_edge_pos
                {
                    info.last_use = info.last_use.max(*back_edge_pos);
                }
            }
        }

        let max_pos = pos.saturating_sub(1);

        let mut result: Vec<_> = intervals
            .into_values()
            .map(|info| {
                let end = if info.last_def > info.last_use {
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
        (result, constraint_points)
    }

    /// Get stack size needed (aligned to 16 bytes)
    pub fn stack_size(&self) -> i32 {
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
