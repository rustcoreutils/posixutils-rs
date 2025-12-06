//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 Code Generator
// Converts IR to AArch64 assembly
//
// Uses linear scan register allocation and AAPCS64 calling convention.
//

use crate::arch::aarch64::lir::{Aarch64Inst, CallTarget, Cond, GpOperand, MemAddr};
use crate::arch::codegen::CodeGenerator;
use crate::arch::lir::{Directive, FpSize, Label, OperandSize, Symbol};
use crate::ir::{Function, Initializer, Instruction, Module, Opcode, Pseudo, PseudoId, PseudoKind};
use crate::target::Target;
use crate::types::{Type, TypeModifiers};
use std::collections::HashMap;

// ============================================================================
// AArch64 Register Definitions
// ============================================================================

/// AArch64 physical registers
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reg {
    // General purpose registers x0-x28
    X0,
    X1,
    X2,
    X3,
    X4,
    X5,
    X6,
    X7,
    X8,
    X9,
    X10,
    X11,
    X12,
    X13,
    X14,
    X15,
    X16,
    X17,
    X18,
    X19,
    X20,
    X21,
    X22,
    X23,
    X24,
    X25,
    X26,
    X27,
    X28,
    // Frame pointer
    X29,
    // Link register
    X30,
    // Stack pointer (special, shares encoding with XZR in some contexts)
    SP,
}

impl Reg {
    /// Get 64-bit register name
    pub fn name64(&self) -> &'static str {
        match self {
            Reg::X0 => "x0",
            Reg::X1 => "x1",
            Reg::X2 => "x2",
            Reg::X3 => "x3",
            Reg::X4 => "x4",
            Reg::X5 => "x5",
            Reg::X6 => "x6",
            Reg::X7 => "x7",
            Reg::X8 => "x8",
            Reg::X9 => "x9",
            Reg::X10 => "x10",
            Reg::X11 => "x11",
            Reg::X12 => "x12",
            Reg::X13 => "x13",
            Reg::X14 => "x14",
            Reg::X15 => "x15",
            Reg::X16 => "x16",
            Reg::X17 => "x17",
            Reg::X18 => "x18",
            Reg::X19 => "x19",
            Reg::X20 => "x20",
            Reg::X21 => "x21",
            Reg::X22 => "x22",
            Reg::X23 => "x23",
            Reg::X24 => "x24",
            Reg::X25 => "x25",
            Reg::X26 => "x26",
            Reg::X27 => "x27",
            Reg::X28 => "x28",
            Reg::X29 => "x29",
            Reg::X30 => "x30",
            Reg::SP => "sp",
        }
    }

    /// Get 32-bit register name
    pub fn name32(&self) -> &'static str {
        match self {
            Reg::X0 => "w0",
            Reg::X1 => "w1",
            Reg::X2 => "w2",
            Reg::X3 => "w3",
            Reg::X4 => "w4",
            Reg::X5 => "w5",
            Reg::X6 => "w6",
            Reg::X7 => "w7",
            Reg::X8 => "w8",
            Reg::X9 => "w9",
            Reg::X10 => "w10",
            Reg::X11 => "w11",
            Reg::X12 => "w12",
            Reg::X13 => "w13",
            Reg::X14 => "w14",
            Reg::X15 => "w15",
            Reg::X16 => "w16",
            Reg::X17 => "w17",
            Reg::X18 => "w18",
            Reg::X19 => "w19",
            Reg::X20 => "w20",
            Reg::X21 => "w21",
            Reg::X22 => "w22",
            Reg::X23 => "w23",
            Reg::X24 => "w24",
            Reg::X25 => "w25",
            Reg::X26 => "w26",
            Reg::X27 => "w27",
            Reg::X28 => "w28",
            Reg::X29 => "w29",
            Reg::X30 => "w30",
            Reg::SP => "sp", // SP doesn't have a 32-bit form in normal use
        }
    }

    /// Get register name for a given bit size
    pub fn name_for_size(&self, bits: u32) -> &'static str {
        match bits {
            8 | 16 | 32 => self.name32(),
            _ => self.name64(),
        }
    }

    /// Is this a callee-saved register?
    pub fn is_callee_saved(&self) -> bool {
        matches!(
            self,
            Reg::X19
                | Reg::X20
                | Reg::X21
                | Reg::X22
                | Reg::X23
                | Reg::X24
                | Reg::X25
                | Reg::X26
                | Reg::X27
                | Reg::X28
        )
    }

    /// Argument registers in order (AAPCS64)
    pub fn arg_regs() -> &'static [Reg] {
        &[
            Reg::X0,
            Reg::X1,
            Reg::X2,
            Reg::X3,
            Reg::X4,
            Reg::X5,
            Reg::X6,
            Reg::X7,
        ]
    }

    /// All allocatable registers
    /// Excludes: x8 (indirect result), x16/x17 (linker scratch),
    ///           x18 (platform), x29 (fp), x30 (lr), sp
    pub fn allocatable() -> &'static [Reg] {
        &[
            Reg::X0,
            Reg::X1,
            Reg::X2,
            Reg::X3,
            Reg::X4,
            Reg::X5,
            Reg::X6,
            Reg::X7,
            // Skip x8 (indirect result register for large struct returns per AAPCS64)
            Reg::X9,
            Reg::X10,
            Reg::X11,
            Reg::X12,
            Reg::X13,
            Reg::X14,
            Reg::X15,
            // Skip x16, x17 (linker scratch)
            // Skip x18 (platform reserved)
            Reg::X19,
            Reg::X20,
            Reg::X21,
            Reg::X22,
            Reg::X23,
            Reg::X24,
            Reg::X25,
            Reg::X26,
            Reg::X27,
            Reg::X28,
            // Skip x29 (fp), x30 (lr)
        ]
    }

    /// Scratch registers for codegen (not allocatable, used for temporaries)
    /// x16 (IP0) and x17 (IP1) are linker scratch registers
    pub fn scratch_regs() -> (Reg, Reg) {
        (Reg::X16, Reg::X17)
    }

    /// Frame pointer register
    pub fn fp() -> Reg {
        Reg::X29
    }

    /// Link register
    pub fn lr() -> Reg {
        Reg::X30
    }

    /// Stack pointer register
    pub fn sp() -> Reg {
        Reg::SP
    }

    /// Platform reserved register (x18)
    /// Should not be used; this is only for documentation and completeness
    pub fn platform_reserved() -> Reg {
        Reg::X18
    }
}

// ============================================================================
// AArch64 Floating-Point Register Definitions
// ============================================================================

/// AArch64 SIMD/FP registers (V0-V31, accessed as D0-D31 for double, S0-S31 for float)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VReg {
    V0,
    V1,
    V2,
    V3,
    V4,
    V5,
    V6,
    V7,
    V8,
    V9,
    V10,
    V11,
    V12,
    V13,
    V14,
    V15,
    V16,
    V17,
    V18,
    V19,
    V20,
    V21,
    V22,
    V23,
    V24,
    V25,
    V26,
    V27,
    V28,
    V29,
    V30,
    V31,
}

impl VReg {
    /// Get 64-bit (double) register name
    pub fn name_d(&self) -> &'static str {
        match self {
            VReg::V0 => "d0",
            VReg::V1 => "d1",
            VReg::V2 => "d2",
            VReg::V3 => "d3",
            VReg::V4 => "d4",
            VReg::V5 => "d5",
            VReg::V6 => "d6",
            VReg::V7 => "d7",
            VReg::V8 => "d8",
            VReg::V9 => "d9",
            VReg::V10 => "d10",
            VReg::V11 => "d11",
            VReg::V12 => "d12",
            VReg::V13 => "d13",
            VReg::V14 => "d14",
            VReg::V15 => "d15",
            VReg::V16 => "d16",
            VReg::V17 => "d17",
            VReg::V18 => "d18",
            VReg::V19 => "d19",
            VReg::V20 => "d20",
            VReg::V21 => "d21",
            VReg::V22 => "d22",
            VReg::V23 => "d23",
            VReg::V24 => "d24",
            VReg::V25 => "d25",
            VReg::V26 => "d26",
            VReg::V27 => "d27",
            VReg::V28 => "d28",
            VReg::V29 => "d29",
            VReg::V30 => "d30",
            VReg::V31 => "d31",
        }
    }

    /// Get 32-bit (float) register name
    pub fn name_s(&self) -> &'static str {
        match self {
            VReg::V0 => "s0",
            VReg::V1 => "s1",
            VReg::V2 => "s2",
            VReg::V3 => "s3",
            VReg::V4 => "s4",
            VReg::V5 => "s5",
            VReg::V6 => "s6",
            VReg::V7 => "s7",
            VReg::V8 => "s8",
            VReg::V9 => "s9",
            VReg::V10 => "s10",
            VReg::V11 => "s11",
            VReg::V12 => "s12",
            VReg::V13 => "s13",
            VReg::V14 => "s14",
            VReg::V15 => "s15",
            VReg::V16 => "s16",
            VReg::V17 => "s17",
            VReg::V18 => "s18",
            VReg::V19 => "s19",
            VReg::V20 => "s20",
            VReg::V21 => "s21",
            VReg::V22 => "s22",
            VReg::V23 => "s23",
            VReg::V24 => "s24",
            VReg::V25 => "s25",
            VReg::V26 => "s26",
            VReg::V27 => "s27",
            VReg::V28 => "s28",
            VReg::V29 => "s29",
            VReg::V30 => "s30",
            VReg::V31 => "s31",
        }
    }

    /// Get register name for a given size (32 = float, 64 = double)
    pub fn name_for_size(&self, bits: u32) -> &'static str {
        if bits <= 32 {
            self.name_s()
        } else {
            self.name_d()
        }
    }

    /// Is this a callee-saved FP register?
    /// AAPCS64: v8-v15 (lower 64 bits) are callee-saved
    pub fn is_callee_saved(&self) -> bool {
        matches!(
            self,
            VReg::V8
                | VReg::V9
                | VReg::V10
                | VReg::V11
                | VReg::V12
                | VReg::V13
                | VReg::V14
                | VReg::V15
        )
    }

    /// FP argument registers in order (AAPCS64)
    pub fn arg_regs() -> &'static [VReg] {
        &[
            VReg::V0,
            VReg::V1,
            VReg::V2,
            VReg::V3,
            VReg::V4,
            VReg::V5,
            VReg::V6,
            VReg::V7,
        ]
    }

    /// All allocatable FP registers
    /// Note: V16, V17, V18 are reserved as scratch registers for codegen
    pub fn allocatable() -> &'static [VReg] {
        &[
            VReg::V0,
            VReg::V1,
            VReg::V2,
            VReg::V3,
            VReg::V4,
            VReg::V5,
            VReg::V6,
            VReg::V7,
            VReg::V8,
            VReg::V9,
            VReg::V10,
            VReg::V11,
            VReg::V12,
            VReg::V13,
            VReg::V14,
            VReg::V15,
            // V16, V17, V18 reserved for scratch
            VReg::V19,
            VReg::V20,
            VReg::V21,
            VReg::V22,
            VReg::V23,
            VReg::V24,
            VReg::V25,
            VReg::V26,
            VReg::V27,
            VReg::V28,
            VReg::V29,
            VReg::V30,
            VReg::V31,
        ]
    }
}

// ============================================================================
// Operand - Location of a value (register or memory)
// ============================================================================

/// Location of a value
#[derive(Debug, Clone)]
pub enum Loc {
    /// In a register
    Reg(Reg),
    /// In a floating-point register
    VReg(VReg),
    /// On the stack at [sp + offset] (positive offset from sp after allocation)
    Stack(i32),
    /// Immediate constant
    Imm(i64),
    /// Floating-point immediate constant
    FImm(f64),
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

/// Simple linear scan register allocator for AArch64
pub struct RegAlloc {
    /// Mapping from pseudo to location
    locations: HashMap<PseudoId, Loc>,
    /// Free registers
    free_regs: Vec<Reg>,
    /// Free FP registers
    free_fp_regs: Vec<VReg>,
    /// Active intervals (sorted by end point)
    active: Vec<(LiveInterval, Reg)>,
    /// Active FP intervals (sorted by end point)
    active_fp: Vec<(LiveInterval, VReg)>,
    /// Next stack slot offset
    stack_offset: i32,
    /// Callee-saved registers that were used
    used_callee_saved: Vec<Reg>,
    /// Callee-saved FP registers that were used
    used_callee_saved_fp: Vec<VReg>,
    /// Set of pseudos that need FP registers (determined by analyzing the IR)
    fp_pseudos: std::collections::HashSet<PseudoId>,
}

impl RegAlloc {
    pub fn new() -> Self {
        Self {
            locations: HashMap::new(),
            free_regs: Reg::allocatable().to_vec(),
            free_fp_regs: VReg::allocatable().to_vec(),
            active: Vec::new(),
            active_fp: Vec::new(),
            stack_offset: 0,
            used_callee_saved: Vec::new(),
            used_callee_saved_fp: Vec::new(),
            fp_pseudos: std::collections::HashSet::new(),
        }
    }

    /// Perform register allocation for a function
    pub fn allocate(&mut self, func: &Function) -> HashMap<PseudoId, Loc> {
        // Reset state
        self.locations.clear();
        self.free_regs = Reg::allocatable().to_vec();
        self.free_fp_regs = VReg::allocatable().to_vec();
        self.active.clear();
        self.active_fp.clear();
        self.stack_offset = 0;
        self.used_callee_saved.clear();
        self.used_callee_saved_fp.clear();
        self.fp_pseudos.clear();

        // Identify which pseudos need FP registers
        self.identify_fp_pseudos(func);

        // Pre-allocate argument registers (integer and FP separately)
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = VReg::arg_regs();
        let mut int_arg_idx = 0usize;
        let mut fp_arg_idx = 0usize;

        // AAPCS64: Detect if there's a hidden return pointer for large struct returns.
        // Unlike x86-64 where sret goes in RDI (first arg), AAPCS64 uses X8 (indirect
        // result register) which does NOT shift other arguments.
        let sret_pseudo = func
            .pseudos
            .iter()
            .find(|p| matches!(p.kind, PseudoKind::Arg(0)) && p.name.as_deref() == Some("__sret"));

        // If there's a hidden return pointer, allocate X8 for it
        if let Some(sret) = sret_pseudo {
            self.locations.insert(sret.id, Loc::Reg(Reg::X8));
            // X8 is not in allocatable list, so no need to remove from free_regs
        }

        // Offset for matching arg_idx: if sret exists, real params have arg_idx starting at 1
        let arg_idx_offset: u32 = if sret_pseudo.is_some() { 1 } else { 0 };

        for (i, (_name, typ)) in func.params.iter().enumerate() {
            // Find the pseudo for this argument
            for pseudo in &func.pseudos {
                if let PseudoKind::Arg(arg_idx) = pseudo.kind {
                    // Match by adjusted index (sret shifts arg_idx but not register allocation)
                    if arg_idx == (i as u32) + arg_idx_offset {
                        let is_fp = typ.is_float();
                        if is_fp {
                            if fp_arg_idx < fp_arg_regs.len() {
                                self.locations
                                    .insert(pseudo.id, Loc::VReg(fp_arg_regs[fp_arg_idx]));
                                self.free_fp_regs.retain(|&r| r != fp_arg_regs[fp_arg_idx]);
                                self.fp_pseudos.insert(pseudo.id);
                            } else {
                                // FP argument on stack
                                let offset = 16 + (fp_arg_idx - fp_arg_regs.len()) as i32 * 8;
                                self.locations.insert(pseudo.id, Loc::Stack(offset));
                                self.fp_pseudos.insert(pseudo.id);
                            }
                            fp_arg_idx += 1;
                        } else {
                            if int_arg_idx < int_arg_regs.len() {
                                self.locations
                                    .insert(pseudo.id, Loc::Reg(int_arg_regs[int_arg_idx]));
                                self.free_regs.retain(|&r| r != int_arg_regs[int_arg_idx]);
                            } else {
                                // Integer argument on stack
                                let offset = 16 + (int_arg_idx - int_arg_regs.len()) as i32 * 8;
                                self.locations.insert(pseudo.id, Loc::Stack(offset));
                            }
                            int_arg_idx += 1;
                        }
                        break;
                    }
                }
            }
        }

        // Compute live intervals
        let intervals = self.compute_live_intervals(func);

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
                        self.locations.insert(interval.pseudo, Loc::FImm(*v));
                        self.fp_pseudos.insert(interval.pseudo);
                        continue;
                    }
                    PseudoKind::Sym(name) => {
                        // Check if this is a local variable or a global symbol
                        if func.locals.contains_key(name) {
                            // Local variable - allocate stack space
                            self.stack_offset += 8;
                            self.locations
                                .insert(interval.pseudo, Loc::Stack(-self.stack_offset));
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

            // Check if this pseudo needs an FP register
            let needs_fp = self.fp_pseudos.contains(&interval.pseudo);

            if needs_fp {
                // Try to allocate an FP register
                if let Some(reg) = self.free_fp_regs.pop() {
                    if reg.is_callee_saved() && !self.used_callee_saved_fp.contains(&reg) {
                        self.used_callee_saved_fp.push(reg);
                    }
                    self.locations.insert(interval.pseudo, Loc::VReg(reg));
                    self.active_fp.push((interval.clone(), reg));
                    self.active_fp.sort_by_key(|(i, _)| i.end);
                } else {
                    // Spill to stack
                    self.stack_offset += 8;
                    self.locations
                        .insert(interval.pseudo, Loc::Stack(-self.stack_offset));
                }
            } else {
                // Try to allocate an integer register
                if let Some(reg) = self.free_regs.pop() {
                    if reg.is_callee_saved() && !self.used_callee_saved.contains(&reg) {
                        self.used_callee_saved.push(reg);
                    }
                    self.locations.insert(interval.pseudo, Loc::Reg(reg));
                    self.active.push((interval.clone(), reg));
                    self.active.sort_by_key(|(i, _)| i.end);
                } else {
                    // Spill to stack
                    self.stack_offset += 8;
                    self.locations
                        .insert(interval.pseudo, Loc::Stack(-self.stack_offset));
                }
            }
        }

        self.locations.clone()
    }

    /// Identify which pseudos need FP registers by scanning the IR
    fn identify_fp_pseudos(&mut self, func: &Function) {
        for block in &func.blocks {
            for insn in &block.insns {
                // Check if this instruction produces a floating-point result
                let is_fp_op = matches!(
                    insn.op,
                    Opcode::FAdd
                        | Opcode::FSub
                        | Opcode::FMul
                        | Opcode::FDiv
                        | Opcode::FNeg
                        | Opcode::FCvtF
                        | Opcode::UCvtF
                        | Opcode::SCvtF
                );

                if is_fp_op {
                    if let Some(target) = insn.target {
                        self.fp_pseudos.insert(target);
                    }
                }

                // Also check if the type is floating point
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

        // Also mark pseudos that are FVal constants
        for pseudo in &func.pseudos {
            if matches!(pseudo.kind, PseudoKind::FVal(_)) {
                self.fp_pseudos.insert(pseudo.id);
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

        // Expire FP register intervals
        let mut to_remove_fp = Vec::new();
        for (i, (interval, reg)) in self.active_fp.iter().enumerate() {
            if interval.end < point {
                self.free_fp_regs.push(*reg);
                to_remove_fp.push(i);
            }
        }
        for i in to_remove_fp.into_iter().rev() {
            self.active_fp.remove(i);
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
        for (src_bb, pseudo) in phi_sources {
            if let Some(&end_pos) = block_end_pos.get(&src_bb) {
                if let Some(info) = intervals.get_mut(&pseudo) {
                    info.last_use = info.last_use.max(end_pos);
                    info.last_def = info.last_def.max(end_pos);
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
// AArch64 Code Generator
// ============================================================================

/// AArch64 code generator
pub struct Aarch64CodeGen {
    /// Target info
    target: Target,
    /// Output buffer
    output: String,
    /// LIR instruction buffer (for deferred emission and future peephole optimization)
    lir_buffer: Vec<Aarch64Inst>,
    /// Current function's register allocation
    locations: HashMap<PseudoId, Loc>,
    /// Current function's pseudos (for looking up values)
    pseudos: Vec<Pseudo>,
    /// Current function name (for generating unique labels)
    current_fn: String,
    /// Total frame size for current function
    frame_size: i32,
    /// Whether to emit basic unwind tables (cfi_startproc/cfi_endproc)
    emit_unwind_tables: bool,
    /// Last emitted source line (for avoiding duplicate .loc directives)
    last_debug_line: u32,
    /// Last emitted source file index
    last_debug_file: u16,
    /// Whether to emit debug info (.file/.loc directives)
    emit_debug: bool,
}

impl Aarch64CodeGen {
    pub fn new(target: Target) -> Self {
        Self {
            target,
            output: String::new(),
            lir_buffer: Vec::new(),
            locations: HashMap::new(),
            pseudos: Vec::new(),
            current_fn: String::new(),
            frame_size: 0,
            emit_unwind_tables: true, // Default to emitting basic unwind tables
            last_debug_line: 0,
            last_debug_file: 0,
            emit_debug: false,
        }
    }

    /// Push a LIR instruction to the buffer (deferred emission)
    fn push_lir(&mut self, inst: Aarch64Inst) {
        self.lir_buffer.push(inst);
    }

    /// Emit all buffered LIR instructions to the output string
    fn emit_all(&mut self) {
        use crate::arch::lir::EmitAsm;
        for inst in &self.lir_buffer {
            inst.emit(&self.target, &mut self.output);
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
                self.push_lir(Aarch64Inst::Directive(Directive::loc(
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
        self.push_lir(Aarch64Inst::Directive(Directive::Comment(
            "Generated by pcc (AArch64)".into(),
        )));
        self.push_lir(Aarch64Inst::Directive(Directive::Text));
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
            self.push_lir(Aarch64Inst::Directive(Directive::comm(name, size, align)));
            return;
        }

        // Data section
        self.push_lir(Aarch64Inst::Directive(Directive::Data));

        // Global visibility (if not static)
        if !is_static {
            self.push_lir(Aarch64Inst::Directive(Directive::global(name)));
        }

        // ELF-only type and size (handled by Directive::emit which skips on macOS)
        self.push_lir(Aarch64Inst::Directive(Directive::type_object(name)));
        self.push_lir(Aarch64Inst::Directive(Directive::size(name, size)));

        // Alignment
        if align > 1 {
            self.push_lir(Aarch64Inst::Directive(Directive::Align(
                align.trailing_zeros(),
            )));
        }

        // Label
        self.push_lir(Aarch64Inst::Directive(Directive::global_label(name)));

        // Emit initializer
        match init {
            Initializer::None => {
                // For static uninitialized, use .zero (not .comm)
                self.push_lir(Aarch64Inst::Directive(Directive::Zero(size)));
            }
            Initializer::Int(val) => match size {
                1 => self.push_lir(Aarch64Inst::Directive(Directive::Byte(*val))),
                2 => self.push_lir(Aarch64Inst::Directive(Directive::Short(*val))),
                4 => self.push_lir(Aarch64Inst::Directive(Directive::Long(*val))),
                _ => self.push_lir(Aarch64Inst::Directive(Directive::Quad(*val))),
            },
            Initializer::Float(val) => {
                if size == 4 {
                    // float - emit as 32-bit IEEE 754
                    let bits = (*val as f32).to_bits();
                    self.push_lir(Aarch64Inst::Directive(Directive::Long(bits as i64)));
                } else {
                    // double - emit as 64-bit IEEE 754
                    let bits = val.to_bits();
                    self.push_lir(Aarch64Inst::Directive(Directive::Quad(bits as i64)));
                }
            }
        }
    }

    fn emit_strings(&mut self, strings: &[(String, String)]) {
        if strings.is_empty() {
            return;
        }

        // Read-only data section
        self.push_lir(Aarch64Inst::Directive(Directive::Rodata));

        for (label, content) in strings {
            // Local label for string literal
            self.push_lir(Aarch64Inst::Directive(Directive::local_label(label)));
            self.push_lir(Aarch64Inst::Directive(Directive::Asciz(
                Self::escape_string(content),
            )));
        }

        // Switch back to text section for functions
        self.push_lir(Aarch64Inst::Directive(Directive::Text));
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
                    // Escape non-printable as octal
                    for byte in c.to_string().as_bytes() {
                        result.push_str(&format!("\\{:03o}", byte));
                    }
                }
            }
        }
        result
    }

    fn emit_function(&mut self, func: &Function) {
        // Register allocation
        let mut alloc = RegAlloc::new();
        self.locations = alloc.allocate(func);
        self.pseudos = func.pseudos.clone();

        let stack_size = alloc.stack_size();
        let callee_saved = alloc.callee_saved_used().to_vec();

        // Calculate total frame size
        // Need space for: fp/lr (16 bytes) + callee-saved regs + local vars
        // Round up callee-saved count to even for 16-byte alignment
        let callee_saved_pairs = (callee_saved.len() + 1) / 2;
        let callee_saved_size = callee_saved_pairs as i32 * 16;
        let total_frame = 16 + callee_saved_size + stack_size;
        // Ensure 16-byte alignment
        let total_frame = (total_frame + 15) & !15;

        // Save function name and frame size for label generation
        self.current_fn = func.name.clone();
        self.frame_size = total_frame;

        // Function prologue
        self.push_lir(Aarch64Inst::Directive(Directive::Blank));
        self.push_lir(Aarch64Inst::Directive(Directive::Text));

        // Skip .globl for static functions (internal linkage)
        if !func.is_static {
            self.push_lir(Aarch64Inst::Directive(Directive::global(&func.name)));
        }

        // ELF-only type (handled by Directive::emit which skips on macOS)
        self.push_lir(Aarch64Inst::Directive(Directive::type_func(&func.name)));

        // Alignment
        self.push_lir(Aarch64Inst::Directive(Directive::Align(2)));

        // Function label
        self.push_lir(Aarch64Inst::Directive(Directive::global_label(&func.name)));

        // CFI: Start procedure (enables stack unwinding for this function)
        if self.emit_unwind_tables {
            self.push_lir(Aarch64Inst::Directive(Directive::CfiStartProc));
        }

        // Prologue: save frame pointer and link register, allocate stack
        let (scratch0, _scratch1) = Reg::scratch_regs();
        let fp = Reg::fp();
        let lr = Reg::lr();
        // Reference platform_reserved to acknowledge its existence
        let _ = Reg::platform_reserved();

        if total_frame > 0 {
            // Combined push and allocate: stp x29, x30, [sp, #-N]!
            self.push_lir(Aarch64Inst::Stp {
                size: OperandSize::B64,
                src1: fp,
                src2: lr,
                addr: MemAddr::PreIndex {
                    base: Reg::X29, // sp
                    offset: -total_frame,
                },
            });
            if self.emit_debug {
                // CFA is now at sp + total_frame (previous SP value)
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_def_cfa(
                    "sp",
                    total_frame,
                )));
                // x29 (fp) is saved at [sp+0], x30 (lr) is saved at [sp+8]
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_offset(
                    "x29",
                    -total_frame,
                )));
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_offset(
                    "x30",
                    -(total_frame - 8),
                )));
            }
            // Set up frame pointer: mov x29, sp
            self.push_lir(Aarch64Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::X29), // sp
                dst: fp,
            });
            if self.emit_debug {
                // CFA is now tracked by x29 + total_frame
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_def_cfa_register(
                    "x29",
                )));
            }

            // Save callee-saved registers in pairs
            let mut offset = 16; // Start after fp/lr
            let mut i = 0;
            while i < callee_saved.len() {
                if i + 1 < callee_saved.len() {
                    self.push_lir(Aarch64Inst::Stp {
                        size: OperandSize::B64,
                        src1: callee_saved[i],
                        src2: callee_saved[i + 1],
                        addr: MemAddr::BaseOffset {
                            base: Reg::X29, // sp
                            offset,
                        },
                    });
                    if self.emit_debug {
                        let cfi_offset1 = -(total_frame - offset);
                        let cfi_offset2 = -(total_frame - offset - 8);
                        self.push_lir(Aarch64Inst::Directive(Directive::cfi_offset(
                            callee_saved[i].name64(),
                            cfi_offset1,
                        )));
                        self.push_lir(Aarch64Inst::Directive(Directive::cfi_offset(
                            callee_saved[i + 1].name64(),
                            cfi_offset2,
                        )));
                    }
                    i += 2;
                } else {
                    self.push_lir(Aarch64Inst::Str {
                        size: OperandSize::B64,
                        src: callee_saved[i],
                        addr: MemAddr::BaseOffset {
                            base: Reg::X29, // sp
                            offset,
                        },
                    });
                    if self.emit_debug {
                        let cfi_offset = -(total_frame - offset);
                        self.push_lir(Aarch64Inst::Directive(Directive::cfi_offset(
                            callee_saved[i].name64(),
                            cfi_offset,
                        )));
                    }
                    i += 1;
                }
                offset += 16;
            }
        } else {
            // Minimal frame: stp x29, x30, [sp, #-16]!
            self.push_lir(Aarch64Inst::Stp {
                size: OperandSize::B64,
                src1: fp,
                src2: lr,
                addr: MemAddr::PreIndex {
                    base: Reg::X29, // sp
                    offset: -16,
                },
            });
            if self.emit_debug {
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_def_cfa("sp", 16)));
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_offset("x29", -16)));
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_offset("x30", -8)));
            }
            // mov x29, sp
            self.push_lir(Aarch64Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::X29), // sp
                dst: fp,
            });
            if self.emit_debug {
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_def_cfa_register(
                    "x29",
                )));
            }
        }

        // Store scratch register for later use in this function
        let _ = scratch0;

        // AAPCS64: Detect if there's a hidden return pointer (sret) for large struct returns.
        // Unlike x86-64, AAPCS64 uses X8 for indirect result, which doesn't shift other args.
        let has_sret = func
            .pseudos
            .iter()
            .any(|p| matches!(p.kind, PseudoKind::Arg(0)) && p.name.as_deref() == Some("__sret"));
        let arg_idx_offset: u32 = if has_sret { 1 } else { 0 };

        // If there's a sret, spill X8 to stack if needed
        if has_sret {
            if let Some(sret) = func.pseudos.iter().find(|p| {
                matches!(p.kind, PseudoKind::Arg(0)) && p.name.as_deref() == Some("__sret")
            }) {
                if let Some(Loc::Stack(offset)) = self.locations.get(&sret.id) {
                    if *offset < 0 {
                        self.push_lir(Aarch64Inst::Str {
                            size: OperandSize::B64,
                            src: Reg::X8,
                            addr: MemAddr::BaseOffset {
                                base: Reg::X29, // sp
                                offset: total_frame + offset,
                            },
                        });
                    }
                }
            }
        }

        // Move arguments from registers to their allocated locations if needed
        // Note: On AAPCS64, sret uses X8, so regular args still start at X0
        let arg_regs = Reg::arg_regs();
        for (i, (_name, _typ)) in func.params.iter().enumerate() {
            if i < arg_regs.len() {
                // Find the pseudo for this argument
                for pseudo in &func.pseudos {
                    if let PseudoKind::Arg(arg_idx) = pseudo.kind {
                        // With sret, params have arg_idx = i + 1, but still use arg_regs[i]
                        if arg_idx == (i as u32) + arg_idx_offset {
                            if let Some(Loc::Stack(offset)) = self.locations.get(&pseudo.id) {
                                // Move from arg register to stack
                                if *offset < 0 {
                                    self.push_lir(Aarch64Inst::Str {
                                        size: OperandSize::B64,
                                        src: arg_regs[i],
                                        addr: MemAddr::BaseOffset {
                                            base: Reg::X29, // sp
                                            offset: total_frame + offset,
                                        },
                                    });
                                }
                            }
                            break;
                        }
                    }
                }
            }
        }

        // Store frame size for epilogue
        let frame_info = (total_frame, callee_saved.clone());

        // Emit basic blocks
        for block in &func.blocks {
            self.emit_block(block, &frame_info);
        }

        // CFI: End procedure
        if self.emit_unwind_tables {
            self.push_lir(Aarch64Inst::Directive(Directive::CfiEndProc));
        }
    }

    fn emit_block(&mut self, block: &crate::ir::BasicBlock, frame_info: &(i32, Vec<Reg>)) {
        // Emit block label using LIR (include function name for uniqueness)
        if let Some(label) = &block.label {
            // LIR: named block label (using Raw since format differs from standard)
            self.push_lir(Aarch64Inst::Directive(Directive::Raw(format!(
                ".L_{}_{}:",
                self.current_fn, label
            ))));
        } else {
            // LIR: numbered block label
            self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(Label::new(
                &self.current_fn,
                block.id.0,
            ))));
        }

        // Emit instructions
        for insn in &block.insns {
            self.emit_insn(insn, frame_info);
        }
    }

    fn emit_insn(&mut self, insn: &Instruction, frame_info: &(i32, Vec<Reg>)) {
        // Emit .loc directive for debug info
        self.emit_loc(insn);

        let (total_frame, callee_saved) = frame_info;

        match insn.op {
            Opcode::Entry => {
                // Already handled in function prologue
            }

            Opcode::Ret => {
                // Move return value to x0 (integer) or v0 (float) if present
                if let Some(&src) = insn.src.first() {
                    let src_loc = self.get_location(src);
                    let is_fp = matches!(src_loc, Loc::VReg(_) | Loc::FImm(_));

                    if is_fp {
                        // FP return value goes in V0
                        self.emit_fp_move(src, VReg::V0, insn.size, *total_frame);
                    } else {
                        // Integer return value goes in X0
                        self.emit_move(src, Reg::X0, insn.size, *total_frame);
                    }
                }

                // Epilogue: restore callee-saved registers
                if *total_frame > 16 {
                    let mut offset = 16;
                    let mut i = 0;
                    while i < callee_saved.len() {
                        if i + 1 < callee_saved.len() {
                            // LIR: ldp pair restore
                            self.push_lir(Aarch64Inst::Ldp {
                                size: OperandSize::B64,
                                addr: MemAddr::BaseOffset {
                                    base: Reg::sp(),
                                    offset,
                                },
                                dst1: callee_saved[i],
                                dst2: callee_saved[i + 1],
                            });
                            i += 2;
                        } else {
                            // LIR: ldr single restore
                            self.push_lir(Aarch64Inst::Ldr {
                                size: OperandSize::B64,
                                addr: MemAddr::BaseOffset {
                                    base: Reg::sp(),
                                    offset,
                                },
                                dst: callee_saved[i],
                            });
                            i += 1;
                        }
                        offset += 16;
                    }
                }

                // Restore fp/lr and deallocate stack
                let fp = Reg::fp();
                let lr = Reg::lr();
                let dealloc = if *total_frame > 0 { *total_frame } else { 16 };
                // LIR: ldp with post-increment to restore fp/lr and deallocate
                self.push_lir(Aarch64Inst::Ldp {
                    size: OperandSize::B64,
                    addr: MemAddr::PostIndex {
                        base: Reg::sp(),
                        offset: dealloc,
                    },
                    dst1: fp,
                    dst2: lr,
                });
                // LIR: ret
                self.push_lir(Aarch64Inst::Ret);
            }

            Opcode::Br => {
                if let Some(target) = insn.bb_true {
                    // LIR: unconditional branch
                    self.push_lir(Aarch64Inst::B {
                        target: Label::new(&self.current_fn, target.0),
                    });
                }
            }

            Opcode::Cbr => {
                if let Some(&cond) = insn.src.first() {
                    let loc = self.get_location(cond);
                    let (scratch0, _) = Reg::scratch_regs();
                    match &loc {
                        Loc::Reg(r) => {
                            // LIR: compare register with zero
                            self.push_lir(Aarch64Inst::Cmp {
                                size: OperandSize::B64,
                                src1: *r,
                                src2: GpOperand::Imm(0),
                            });
                        }
                        Loc::Stack(offset) => {
                            let actual_offset = if *offset < 0 {
                                *total_frame + *offset
                            } else {
                                *offset
                            };
                            // LIR: load from stack
                            self.push_lir(Aarch64Inst::Ldr {
                                size: OperandSize::B64,
                                addr: MemAddr::BaseOffset {
                                    base: Reg::sp(),
                                    offset: actual_offset,
                                },
                                dst: scratch0,
                            });
                            // LIR: compare with zero
                            self.push_lir(Aarch64Inst::Cmp {
                                size: OperandSize::B64,
                                src1: scratch0,
                                src2: GpOperand::Imm(0),
                            });
                        }
                        Loc::Imm(v) => {
                            if *v != 0 {
                                if let Some(target) = insn.bb_true {
                                    // LIR: unconditional branch (constant true)
                                    self.push_lir(Aarch64Inst::B {
                                        target: Label::new(&self.current_fn, target.0),
                                    });
                                }
                                return;
                            } else {
                                if let Some(target) = insn.bb_false {
                                    // LIR: unconditional branch (constant false)
                                    self.push_lir(Aarch64Inst::B {
                                        target: Label::new(&self.current_fn, target.0),
                                    });
                                }
                                return;
                            }
                        }
                        Loc::Global(name) => {
                            self.emit_load_global(name, scratch0);
                            // LIR: compare with zero
                            self.push_lir(Aarch64Inst::Cmp {
                                size: OperandSize::B64,
                                src1: scratch0,
                                src2: GpOperand::Imm(0),
                            });
                        }
                        Loc::VReg(v) => {
                            // LIR: compare FP register with zero
                            self.push_lir(Aarch64Inst::FcmpZero {
                                size: FpSize::Double,
                                src: *v,
                            });
                        }
                        Loc::FImm(f) => {
                            // FP immediate as condition - branch based on non-zero
                            if *f != 0.0 {
                                if let Some(target) = insn.bb_true {
                                    // LIR: unconditional branch (constant true)
                                    self.push_lir(Aarch64Inst::B {
                                        target: Label::new(&self.current_fn, target.0),
                                    });
                                }
                                return;
                            } else {
                                if let Some(target) = insn.bb_false {
                                    // LIR: unconditional branch (constant false)
                                    self.push_lir(Aarch64Inst::B {
                                        target: Label::new(&self.current_fn, target.0),
                                    });
                                }
                                return;
                            }
                        }
                    }
                    if let Some(target) = insn.bb_true {
                        // LIR: conditional branch (not equal = true)
                        self.push_lir(Aarch64Inst::BCond {
                            cond: Cond::Ne,
                            target: Label::new(&self.current_fn, target.0),
                        });
                    }
                    if let Some(target) = insn.bb_false {
                        // LIR: unconditional branch (fallthrough to false)
                        self.push_lir(Aarch64Inst::B {
                            target: Label::new(&self.current_fn, target.0),
                        });
                    }
                }
            }

            Opcode::Switch => {
                // Switch uses target as the value to switch on
                if let Some(val) = insn.target {
                    let loc = self.get_location(val);
                    let (scratch0, scratch1) = Reg::scratch_regs();
                    let size = insn.size.max(32);
                    let op_size = OperandSize::from_bits(size);

                    // Move switch value to scratch0
                    match &loc {
                        Loc::Reg(r) => {
                            if *r != scratch0 {
                                // LIR: move register to scratch
                                self.push_lir(Aarch64Inst::Mov {
                                    size: op_size,
                                    src: GpOperand::Reg(*r),
                                    dst: scratch0,
                                });
                            }
                        }
                        Loc::Stack(offset) => {
                            let actual_offset = if *offset < 0 {
                                *total_frame + *offset
                            } else {
                                *offset
                            };
                            // LIR: load from stack
                            self.push_lir(Aarch64Inst::Ldr {
                                size: op_size,
                                addr: MemAddr::BaseOffset {
                                    base: Reg::sp(),
                                    offset: actual_offset,
                                },
                                dst: scratch0,
                            });
                        }
                        Loc::Imm(v) => {
                            // LIR: move immediate
                            self.push_lir(Aarch64Inst::Mov {
                                size: op_size,
                                src: GpOperand::Imm(*v),
                                dst: scratch0,
                            });
                        }
                        Loc::Global(name) => {
                            self.emit_load_global(name, scratch0);
                        }
                        Loc::VReg(_) | Loc::FImm(_) => {
                            // FP values shouldn't be used in switch statements
                            // This is unreachable in valid C code
                        }
                    }

                    // Generate comparisons for each case
                    for (case_val, target_bb) in &insn.switch_cases {
                        // Load case value to scratch1 if it doesn't fit in immediate
                        if *case_val >= 0 && *case_val < 4096 {
                            // LIR: compare with immediate
                            self.push_lir(Aarch64Inst::Cmp {
                                size: op_size,
                                src1: scratch0,
                                src2: GpOperand::Imm(*case_val),
                            });
                        } else {
                            // LIR: load large constant
                            self.push_lir(Aarch64Inst::Mov {
                                size: op_size,
                                src: GpOperand::Imm(*case_val),
                                dst: scratch1,
                            });
                            // LIR: compare registers
                            self.push_lir(Aarch64Inst::Cmp {
                                size: op_size,
                                src1: scratch0,
                                src2: GpOperand::Reg(scratch1),
                            });
                        }
                        // LIR: conditional branch on equal
                        self.push_lir(Aarch64Inst::BCond {
                            cond: Cond::Eq,
                            target: Label::new(&self.current_fn, target_bb.0),
                        });
                    }

                    // Jump to default
                    if let Some(default_bb) = insn.switch_default {
                        // LIR: unconditional branch to default
                        self.push_lir(Aarch64Inst::B {
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
                self.emit_binop(insn, *total_frame);
            }

            Opcode::Mul => {
                self.emit_mul(insn, *total_frame);
            }

            Opcode::DivS | Opcode::DivU | Opcode::ModS | Opcode::ModU => {
                self.emit_div(insn, *total_frame);
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
                self.emit_compare(insn, *total_frame);
            }

            Opcode::Neg => {
                self.emit_neg(insn, *total_frame);
            }

            Opcode::Not => {
                self.emit_not(insn, *total_frame);
            }

            Opcode::Load => {
                self.emit_load(insn, *total_frame);
            }

            Opcode::Store => {
                self.emit_store(insn, *total_frame);
            }

            Opcode::Call => {
                self.emit_call(insn, *total_frame);
            }

            Opcode::SetVal => {
                if let Some(target) = insn.target {
                    if let Some(pseudo) = self.pseudos.iter().find(|p| p.id == target) {
                        match self.locations.get(&target).cloned() {
                            Some(Loc::Reg(r)) => {
                                if let PseudoKind::Val(v) = &pseudo.kind {
                                    self.emit_mov_imm(r, *v, insn.size);
                                }
                            }
                            Some(Loc::VReg(v)) => {
                                if let PseudoKind::FVal(f) = &pseudo.kind {
                                    // Load FP constant using integer register
                                    let (scratch0, _) = Reg::scratch_regs();
                                    let bits = if insn.size <= 32 {
                                        (*f as f32).to_bits() as i64
                                    } else {
                                        f.to_bits() as i64
                                    };
                                    self.emit_mov_imm(scratch0, bits, 64);
                                    // LIR: fmov from GP to FP register
                                    let fp_size = if insn.size <= 32 {
                                        FpSize::Single
                                    } else {
                                        FpSize::Double
                                    };
                                    self.push_lir(Aarch64Inst::FmovFromGp {
                                        size: fp_size,
                                        src: scratch0,
                                        dst: v,
                                    });
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }

            Opcode::Copy => {
                if let (Some(target), Some(&src)) = (insn.target, insn.src.first()) {
                    // Pass the type for proper sign/zero extension
                    self.emit_copy_with_type(
                        src,
                        target,
                        insn.size,
                        insn.typ.as_ref(),
                        *total_frame,
                    );
                }
            }

            Opcode::SymAddr => {
                if let (Some(target), Some(&src)) = (insn.target, insn.src.first()) {
                    if let Some(Loc::Reg(dst)) = self.locations.get(&target).cloned() {
                        if let Some(Loc::Global(name)) = self.locations.get(&src).cloned() {
                            self.emit_load_addr(&name, dst);
                        }
                    }
                }
            }

            Opcode::Select => {
                self.emit_select(insn, *total_frame);
            }

            Opcode::Zext | Opcode::Sext | Opcode::Trunc => {
                self.emit_extend(insn, *total_frame);
            }

            // Floating-point arithmetic operations
            Opcode::FAdd | Opcode::FSub | Opcode::FMul | Opcode::FDiv => {
                self.emit_fp_binop(insn, *total_frame);
            }

            // Floating-point negation
            Opcode::FNeg => {
                self.emit_fp_neg(insn, *total_frame);
            }

            // Floating-point comparisons
            Opcode::FCmpOEq
            | Opcode::FCmpONe
            | Opcode::FCmpOLt
            | Opcode::FCmpOLe
            | Opcode::FCmpOGt
            | Opcode::FCmpOGe => {
                self.emit_fp_compare(insn, *total_frame);
            }

            // Int to float conversions
            Opcode::UCvtF | Opcode::SCvtF => {
                self.emit_int_to_float(insn, *total_frame);
            }

            // Float to int conversions
            Opcode::FCvtU | Opcode::FCvtS => {
                self.emit_float_to_int(insn, *total_frame);
            }

            // Float to float conversions (size changes)
            Opcode::FCvtF => {
                self.emit_float_to_float(insn, *total_frame);
            }

            // ================================================================
            // Variadic function support (va_* builtins)
            // ================================================================
            Opcode::VaStart => {
                self.emit_va_start(insn, *total_frame);
            }

            Opcode::VaArg => {
                self.emit_va_arg(insn, *total_frame);
            }

            Opcode::VaEnd => {
                // va_end is a no-op on all platforms
                // The C standard says it must be called, but it does nothing
            }

            Opcode::VaCopy => {
                self.emit_va_copy(insn, *total_frame);
            }

            // ================================================================
            // Byte-swapping builtins
            // ================================================================
            Opcode::Bswap16 => {
                self.emit_bswap16(insn, *total_frame);
            }

            Opcode::Bswap32 => {
                self.emit_bswap32(insn, *total_frame);
            }

            Opcode::Bswap64 => {
                self.emit_bswap64(insn, *total_frame);
            }

            Opcode::Alloca => {
                self.emit_alloca(insn, *total_frame);
            }

            // Skip no-ops and unimplemented
            _ => {}
        }
    }

    fn get_location(&self, pseudo: PseudoId) -> Loc {
        self.locations.get(&pseudo).cloned().unwrap_or(Loc::Imm(0))
    }

    /// Load address of a global symbol into a register
    fn emit_load_addr(&mut self, name: &str, dst: Reg) {
        // Local labels (starting with '.') don't get the _ prefix on macOS
        let sym = if name.starts_with('.') {
            Symbol::local(name)
        } else {
            Symbol::global(name)
        };

        // ADRP + ADD sequence for PIC address loading
        self.push_lir(Aarch64Inst::Adrp {
            sym: sym.clone(),
            dst,
        });
        self.push_lir(Aarch64Inst::AddSymOffset {
            sym,
            base: dst,
            dst,
        });
    }

    /// Load value of a global symbol into a register
    fn emit_load_global(&mut self, name: &str, dst: Reg) {
        // Local labels (starting with '.') don't get the _ prefix on macOS
        let sym = if name.starts_with('.') {
            Symbol::local(name)
        } else {
            Symbol::global(name)
        };

        // ADRP + LDR sequence for PIC value loading
        self.push_lir(Aarch64Inst::Adrp {
            sym: sym.clone(),
            dst,
        });
        self.push_lir(Aarch64Inst::LdrSymOffset {
            size: OperandSize::B64,
            sym,
            base: dst,
            dst,
        });
    }

    /// Move immediate value to register
    fn emit_mov_imm(&mut self, dst: Reg, val: i64, size: u32) {
        let op_size = OperandSize::from_bits(size.max(32));

        // AArch64 can only move 16-bit immediates directly
        // For larger values, we need movz + movk sequence
        if (0..=0xFFFF).contains(&val) {
            // LIR: simple mov immediate
            self.push_lir(Aarch64Inst::Mov {
                size: op_size,
                src: GpOperand::Imm(val),
                dst,
            });
        } else if (-0x8000..0).contains(&val) {
            // Small negative number - use mov (assembler handles movn)
            self.push_lir(Aarch64Inst::Mov {
                size: op_size,
                src: GpOperand::Imm(val),
                dst,
            });
        } else {
            // Use movz + movk for larger values
            let uval = val as u64;
            // LIR: movz base
            self.push_lir(Aarch64Inst::Movz {
                size: OperandSize::B64,
                imm: (uval & 0xFFFF) as u16,
                shift: 0,
                dst,
            });
            if (uval >> 16) & 0xFFFF != 0 {
                // LIR: movk shift 16
                self.push_lir(Aarch64Inst::Movk {
                    size: OperandSize::B64,
                    imm: ((uval >> 16) & 0xFFFF) as u16,
                    shift: 16,
                    dst,
                });
            }
            if (uval >> 32) & 0xFFFF != 0 {
                // LIR: movk shift 32
                self.push_lir(Aarch64Inst::Movk {
                    size: OperandSize::B64,
                    imm: ((uval >> 32) & 0xFFFF) as u16,
                    shift: 32,
                    dst,
                });
            }
            if (uval >> 48) & 0xFFFF != 0 {
                // LIR: movk shift 48
                self.push_lir(Aarch64Inst::Movk {
                    size: OperandSize::B64,
                    imm: ((uval >> 48) & 0xFFFF) as u16,
                    shift: 48,
                    dst,
                });
            }
        }
    }

    fn emit_move(&mut self, src: PseudoId, dst: Reg, size: u32, frame_size: i32) {
        let size = size.max(32);
        let loc = self.get_location(src);
        let op_size = OperandSize::from_bits(size);

        match loc {
            Loc::Reg(r) if r == dst => {}
            Loc::Reg(r) => {
                // LIR: mov register to register
                self.push_lir(Aarch64Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(r),
                    dst,
                });
            }
            Loc::Stack(offset) => {
                let actual_offset = if offset < 0 {
                    frame_size + offset
                } else {
                    offset
                };
                // LIR: load from stack
                self.push_lir(Aarch64Inst::Ldr {
                    size: op_size,
                    addr: MemAddr::BaseOffset {
                        base: Reg::sp(),
                        offset: actual_offset,
                    },
                    dst,
                });
            }
            Loc::Imm(v) => {
                self.emit_mov_imm(dst, v, size);
            }
            Loc::Global(name) => {
                self.emit_load_global(&name, dst);
            }
            Loc::VReg(v) => {
                // LIR: fmov from FP to GP register
                let fp_size = if size <= 32 {
                    FpSize::Single
                } else {
                    FpSize::Double
                };
                self.push_lir(Aarch64Inst::FmovToGp {
                    size: fp_size,
                    src: v,
                    dst,
                });
            }
            Loc::FImm(f) => {
                // Load FP immediate as integer bits
                let bits = if size <= 32 {
                    (f as f32).to_bits() as i64
                } else {
                    f.to_bits() as i64
                };
                self.emit_mov_imm(dst, bits, size);
            }
        }
    }

    fn emit_move_to_loc(&mut self, src: Reg, dst: &Loc, size: u32, frame_size: i32) {
        // For stack stores, use actual size to properly handle char/short
        // For register-to-register, use minimum 32-bit
        match dst {
            Loc::Reg(r) if *r == src => {}
            Loc::Reg(r) => {
                let reg_size = size.max(32);
                // LIR: mov register to register
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::from_bits(reg_size),
                    src: GpOperand::Reg(src),
                    dst: *r,
                });
            }
            Loc::Stack(offset) => {
                let actual_offset = if *offset < 0 {
                    frame_size + *offset
                } else {
                    *offset
                };
                // LIR: store to stack with appropriate size (strb/strh/str)
                let op_size = OperandSize::from_bits(size);
                self.push_lir(Aarch64Inst::Str {
                    size: op_size,
                    src,
                    addr: MemAddr::BaseOffset {
                        base: Reg::sp(),
                        offset: actual_offset,
                    },
                });
            }
            _ => {}
        }
    }

    fn emit_binop(&mut self, insn: &Instruction, frame_size: i32) {
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
            _ => Reg::X9,
        };

        // Load first operand
        self.emit_move(src1, work_reg, size, frame_size);

        // Get second operand as GpOperand
        let src2_loc = self.get_location(src2);
        let src2_operand = match &src2_loc {
            Loc::Reg(r) => GpOperand::Reg(*r),
            Loc::Imm(v) if *v >= 0 && *v <= 4095 => GpOperand::Imm(*v),
            _ => {
                self.emit_move(src2, Reg::X10, size, frame_size);
                GpOperand::Reg(Reg::X10)
            }
        };

        // Emit the appropriate LIR instruction
        match insn.op {
            Opcode::Add => self.push_lir(Aarch64Inst::Add {
                size: op_size,
                src1: work_reg,
                src2: src2_operand,
                dst: work_reg,
            }),
            Opcode::Sub => self.push_lir(Aarch64Inst::Sub {
                size: op_size,
                src1: work_reg,
                src2: src2_operand,
                dst: work_reg,
            }),
            Opcode::And => self.push_lir(Aarch64Inst::And {
                size: op_size,
                src1: work_reg,
                src2: src2_operand,
                dst: work_reg,
            }),
            Opcode::Or => self.push_lir(Aarch64Inst::Orr {
                size: op_size,
                src1: work_reg,
                src2: src2_operand,
                dst: work_reg,
            }),
            Opcode::Xor => self.push_lir(Aarch64Inst::Eor {
                size: op_size,
                src1: work_reg,
                src2: src2_operand,
                dst: work_reg,
            }),
            Opcode::Shl => self.push_lir(Aarch64Inst::Lsl {
                size: op_size,
                src: work_reg,
                amount: src2_operand,
                dst: work_reg,
            }),
            Opcode::Lsr => self.push_lir(Aarch64Inst::Lsr {
                size: op_size,
                src: work_reg,
                amount: src2_operand,
                dst: work_reg,
            }),
            Opcode::Asr => self.push_lir(Aarch64Inst::Asr {
                size: op_size,
                src: work_reg,
                amount: src2_operand,
                dst: work_reg,
            }),
            _ => return,
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, size, frame_size);
        }
    }

    fn emit_mul(&mut self, insn: &Instruction, frame_size: i32) {
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
            _ => Reg::X9,
        };

        self.emit_move(src1, Reg::X10, size, frame_size);
        self.emit_move(src2, Reg::X11, size, frame_size);

        self.push_lir(Aarch64Inst::Mul {
            size: op_size,
            src1: Reg::X10,
            src2: Reg::X11,
            dst: dst_reg,
        });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size, frame_size);
        }
    }

    fn emit_div(&mut self, insn: &Instruction, frame_size: i32) {
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

        self.emit_move(src1, Reg::X10, size, frame_size);
        self.emit_move(src2, Reg::X11, size, frame_size);

        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X9,
        };

        // Division instruction
        match insn.op {
            Opcode::DivS | Opcode::ModS => self.push_lir(Aarch64Inst::Sdiv {
                size: op_size,
                src1: Reg::X10,
                src2: Reg::X11,
                dst: dst_reg,
            }),
            Opcode::DivU | Opcode::ModU => self.push_lir(Aarch64Inst::Udiv {
                size: op_size,
                src1: Reg::X10,
                src2: Reg::X11,
                dst: dst_reg,
            }),
            _ => return,
        }

        // For modulo, compute remainder: r = n - (n / d) * d
        // Using msub: msub Rd, Rm, Rn, Ra -> Rd = Ra - Rm * Rn
        if matches!(insn.op, Opcode::ModS | Opcode::ModU) {
            // dst_reg now has quotient, compute: src1 - quotient * src2
            self.push_lir(Aarch64Inst::Msub {
                size: op_size,
                mul1: dst_reg,
                mul2: Reg::X11,
                sub: Reg::X10,
                dst: dst_reg,
            });
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size, frame_size);
        }
    }

    fn emit_compare(&mut self, insn: &Instruction, frame_size: i32) {
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

        self.emit_move(src1, Reg::X10, size, frame_size);

        // Try to use immediate for comparison if possible
        let src2_loc = self.get_location(src2);
        let src2_operand = match &src2_loc {
            Loc::Imm(v) if *v >= 0 && *v <= 4095 => GpOperand::Imm(*v),
            _ => {
                self.emit_move(src2, Reg::X11, size, frame_size);
                GpOperand::Reg(Reg::X11)
            }
        };

        self.push_lir(Aarch64Inst::Cmp {
            size: op_size,
            src1: Reg::X10,
            src2: src2_operand,
        });

        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X9,
        };

        // Use cset to set register based on condition
        let cond = match insn.op {
            Opcode::SetEq => Cond::Eq,
            Opcode::SetNe => Cond::Ne,
            Opcode::SetLt => Cond::Lt,
            Opcode::SetLe => Cond::Le,
            Opcode::SetGt => Cond::Gt,
            Opcode::SetGe => Cond::Ge,
            Opcode::SetB => Cond::Cc,  // unsigned less than (lo)
            Opcode::SetBe => Cond::Ls, // unsigned less than or equal
            Opcode::SetA => Cond::Hi,  // unsigned greater than
            Opcode::SetAe => Cond::Cs, // unsigned greater than or equal (hs)
            _ => return,
        };

        self.push_lir(Aarch64Inst::Cset { cond, dst: dst_reg });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size, frame_size);
        }
    }

    fn emit_neg(&mut self, insn: &Instruction, frame_size: i32) {
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
            _ => Reg::X9,
        };

        self.emit_move(src, work_reg, size, frame_size);
        self.push_lir(Aarch64Inst::Neg {
            size: op_size,
            src: work_reg,
            dst: work_reg,
        });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, size, frame_size);
        }
    }

    fn emit_not(&mut self, insn: &Instruction, frame_size: i32) {
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
            _ => Reg::X9,
        };

        self.emit_move(src, work_reg, size, frame_size);
        self.push_lir(Aarch64Inst::Mvn {
            size: op_size,
            src: work_reg,
            dst: work_reg,
        });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, size, frame_size);
        }
    }

    fn emit_load(&mut self, insn: &Instruction, frame_size: i32) {
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
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X9,
        };

        // Determine if we need sign or zero extension for small types
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

        // Helper to emit the appropriate load instruction
        let emit_load_lir = |this: &mut Self, mem_addr: MemAddr| match mem_size {
            8 if is_unsigned => {
                this.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B8,
                    addr: mem_addr,
                    dst: dst_reg,
                });
            }
            8 => {
                this.push_lir(Aarch64Inst::Ldrs {
                    src_size: OperandSize::B8,
                    dst_size: OperandSize::from_bits(reg_size),
                    addr: mem_addr,
                    dst: dst_reg,
                });
            }
            16 if is_unsigned => {
                this.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B16,
                    addr: mem_addr,
                    dst: dst_reg,
                });
            }
            16 => {
                this.push_lir(Aarch64Inst::Ldrs {
                    src_size: OperandSize::B16,
                    dst_size: OperandSize::from_bits(reg_size),
                    addr: mem_addr,
                    dst: dst_reg,
                });
            }
            _ => {
                this.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::from_bits(mem_size),
                    addr: mem_addr,
                    dst: dst_reg,
                });
            }
        };

        let addr_loc = self.get_location(addr);
        match addr_loc {
            Loc::Reg(r) => {
                let mem_addr = MemAddr::BaseOffset {
                    base: r,
                    offset: insn.offset as i32,
                };
                emit_load_lir(self, mem_addr);
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
                    let total_offset = frame_size + offset + insn.offset as i32;
                    let mem_addr = MemAddr::BaseOffset {
                        base: Reg::sp(),
                        offset: total_offset,
                    };
                    emit_load_lir(self, mem_addr);
                } else {
                    // Spilled address - load address first, then load from that address
                    let adjusted = frame_size + offset;
                    self.push_lir(Aarch64Inst::Ldr {
                        size: OperandSize::B64,
                        addr: MemAddr::BaseOffset {
                            base: Reg::sp(),
                            offset: adjusted,
                        },
                        dst: Reg::X16,
                    });
                    let mem_addr = MemAddr::BaseOffset {
                        base: Reg::X16,
                        offset: insn.offset as i32,
                    };
                    emit_load_lir(self, mem_addr);
                }
            }
            Loc::Global(name) => {
                self.emit_load_global(&name, dst_reg);
            }
            _ => {
                self.emit_move(addr, Reg::X16, 64, frame_size);
                let mem_addr = MemAddr::BaseOffset {
                    base: Reg::X16,
                    offset: insn.offset as i32,
                };
                emit_load_lir(self, mem_addr);
            }
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, reg_size, frame_size);
        }
    }

    fn emit_store(&mut self, insn: &Instruction, frame_size: i32) {
        // Use actual size for memory stores (8, 16, 32, 64 bits)
        let mem_size = insn.size;
        let reg_size = insn.size.max(32);

        let (addr, value) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&a), Some(&v)) => (a, v),
            _ => return,
        };

        // For struct stores (size > 64), we need to copy multiple words
        // The value is a symbol containing the struct data
        if mem_size > 64 {
            self.emit_struct_store(insn, addr, value, frame_size);
            return;
        }

        self.emit_move(value, Reg::X9, reg_size, frame_size);

        // Get the store size based on mem_size
        let store_size = OperandSize::from_bits(mem_size);

        // Helper to emit store instruction
        let emit_store_lir = |this: &mut Self, mem_addr: MemAddr| {
            this.push_lir(Aarch64Inst::Str {
                size: store_size,
                src: Reg::X9,
                addr: mem_addr,
            });
        };

        let addr_loc = self.get_location(addr);
        match addr_loc {
            Loc::Reg(r) => {
                let mem_addr = MemAddr::BaseOffset {
                    base: r,
                    offset: insn.offset as i32,
                };
                emit_store_lir(self, mem_addr);
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
                    let total_offset = frame_size + offset + insn.offset as i32;
                    let mem_addr = MemAddr::BaseOffset {
                        base: Reg::sp(),
                        offset: total_offset,
                    };
                    emit_store_lir(self, mem_addr);
                } else {
                    // Spilled address - load address first, then store through it
                    let adjusted = frame_size + offset;
                    self.push_lir(Aarch64Inst::Ldr {
                        size: OperandSize::B64,
                        addr: MemAddr::BaseOffset {
                            base: Reg::sp(),
                            offset: adjusted,
                        },
                        dst: Reg::X16,
                    });
                    let mem_addr = MemAddr::BaseOffset {
                        base: Reg::X16,
                        offset: insn.offset as i32,
                    };
                    emit_store_lir(self, mem_addr);
                }
            }
            Loc::Global(name) => {
                self.emit_load_addr(&name, Reg::X16);
                let mem_addr = MemAddr::Base(Reg::X16);
                emit_store_lir(self, mem_addr);
            }
            _ => {
                self.emit_move(addr, Reg::X16, 64, frame_size);
                let mem_addr = MemAddr::BaseOffset {
                    base: Reg::X16,
                    offset: insn.offset as i32,
                };
                emit_store_lir(self, mem_addr);
            }
        }
    }

    /// Emit a struct copy (store of size > 64 bits)
    /// The value is a symbol containing the source struct data
    fn emit_struct_store(
        &mut self,
        insn: &Instruction,
        addr: PseudoId,
        value: PseudoId,
        frame_size: i32,
    ) {
        let struct_size = insn.size; // Size in bits
        let num_qwords = (struct_size + 63) / 64;

        // Get source address (where the struct data is)
        let value_loc = self.get_location(value);
        // Get destination address
        let addr_loc = self.get_location(addr);

        // Load source address into X16
        match value_loc {
            Loc::Stack(offset) => {
                let total_offset = frame_size + offset;
                self.push_lir(Aarch64Inst::Add {
                    size: OperandSize::B64,
                    src1: Reg::sp(),
                    src2: GpOperand::Imm(total_offset as i64),
                    dst: Reg::X16,
                });
            }
            Loc::Reg(r) => {
                if r != Reg::X16 {
                    self.push_lir(Aarch64Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: Reg::X16,
                    });
                }
            }
            Loc::Global(ref name) => {
                self.emit_load_addr(name, Reg::X16);
            }
            _ => return,
        }

        // Load destination address into X17
        match addr_loc {
            Loc::Stack(offset) => {
                let total_offset = frame_size + offset + insn.offset as i32;
                self.push_lir(Aarch64Inst::Add {
                    size: OperandSize::B64,
                    src1: Reg::sp(),
                    src2: GpOperand::Imm(total_offset as i64),
                    dst: Reg::X17,
                });
            }
            Loc::Reg(r) => {
                if insn.offset != 0 {
                    self.push_lir(Aarch64Inst::Add {
                        size: OperandSize::B64,
                        src1: r,
                        src2: GpOperand::Imm(insn.offset),
                        dst: Reg::X17,
                    });
                } else if r != Reg::X17 {
                    self.push_lir(Aarch64Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: Reg::X17,
                    });
                }
            }
            Loc::Global(ref name) => {
                self.emit_load_addr(name, Reg::X17);
            }
            _ => return,
        }

        // Copy qword by qword using X9 as temp
        for i in 0..num_qwords {
            let byte_offset = (i * 8) as i32;
            self.push_lir(Aarch64Inst::Ldr {
                size: OperandSize::B64,
                addr: MemAddr::BaseOffset {
                    base: Reg::X16,
                    offset: byte_offset,
                },
                dst: Reg::X9,
            });
            self.push_lir(Aarch64Inst::Str {
                size: OperandSize::B64,
                src: Reg::X9,
                addr: MemAddr::BaseOffset {
                    base: Reg::X17,
                    offset: byte_offset,
                },
            });
        }
    }

    fn emit_call(&mut self, insn: &Instruction, frame_size: i32) {
        let func_name = match &insn.func_name {
            Some(n) => n.clone(),
            None => return,
        };

        // AAPCS64 calling convention:
        // - Integer arguments: X0-X7 (8 registers)
        // - Floating-point arguments: V0-V7 (8 registers)
        // - Indirect result (large struct return): X8
        // Each class has its own register allocation
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = VReg::arg_regs();

        let mut int_arg_idx = 0;
        let mut fp_arg_idx = 0;
        let mut stack_args = 0;

        // Check if this call returns a large struct (> 16 bytes)
        // If so, the first argument is the sret pointer and goes in X8 (not X0)
        let returns_large_struct = insn.typ.as_ref().is_some_and(|t| {
            (t.kind == crate::types::TypeKind::Struct || t.kind == crate::types::TypeKind::Union)
                && t.size_bits() > 128
        });

        // Also check if return type is a pointer to a large struct (linearizer wraps it)
        let returns_large_struct = returns_large_struct
            || insn.typ.as_ref().is_some_and(|t| {
                if let Some(pointee) = t.get_base() {
                    (pointee.kind == crate::types::TypeKind::Struct
                        || pointee.kind == crate::types::TypeKind::Union)
                        && pointee.size_bits() > 128
                } else {
                    false
                }
            });

        let args_start = if returns_large_struct && !insn.src.is_empty() {
            // First argument is sret pointer - move to X8
            self.emit_move(insn.src[0], Reg::X8, 64, frame_size);
            1 // Skip first arg in main loop
        } else {
            0
        };

        // Move arguments to registers
        for (i, &arg) in insn.src.iter().enumerate().skip(args_start) {
            // Get argument type if available, otherwise fall back to location-based detection
            let arg_type = insn.arg_types.get(i);
            let is_fp = if let Some(typ) = arg_type {
                typ.is_float()
            } else {
                // Fall back to location-based detection for backwards compatibility
                let arg_loc = self.get_location(arg);
                matches!(arg_loc, Loc::VReg(_) | Loc::FImm(_))
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
                if fp_arg_idx < fp_arg_regs.len() {
                    self.emit_fp_move(arg, fp_arg_regs[fp_arg_idx], fp_size, frame_size);
                    fp_arg_idx += 1;
                } else {
                    // FP arg on stack
                    self.emit_fp_move(arg, VReg::V16, fp_size, frame_size);
                    // LIR: store FP reg to stack with pre-decrement
                    let fp_sz = if fp_size == 32 {
                        FpSize::Single
                    } else {
                        FpSize::Double
                    };
                    self.push_lir(Aarch64Inst::StrFp {
                        size: fp_sz,
                        src: VReg::V16,
                        addr: MemAddr::PreIndex {
                            base: Reg::SP,
                            offset: -16,
                        },
                    });
                    stack_args += 1;
                }
            } else if int_arg_idx < int_arg_regs.len() {
                self.emit_move(arg, int_arg_regs[int_arg_idx], arg_size, frame_size);
                int_arg_idx += 1;
            } else {
                // Integer arg on stack - always store 8 bytes on aarch64
                self.emit_move(arg, Reg::X9, arg_size, frame_size);
                // LIR: store GP reg to stack with pre-decrement
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: Reg::X9,
                    addr: MemAddr::PreIndex {
                        base: Reg::SP,
                        offset: -16,
                    },
                });
                stack_args += 1;
            }
        }

        // Call the function
        self.push_lir(Aarch64Inst::Bl {
            target: CallTarget::Direct(Symbol::global(&func_name)),
        });

        // Clean up stack arguments
        if stack_args > 0 {
            self.push_lir(Aarch64Inst::Add {
                size: OperandSize::B64,
                src1: Reg::sp(),
                src2: GpOperand::Imm((stack_args * 16) as i64),
                dst: Reg::sp(),
            });
        }

        // Move return value if needed
        if let Some(target) = insn.target {
            let dst_loc = self.get_location(target);
            // Check if return value is floating-point based on type or location
            let is_fp_result = if let Some(ref typ) = insn.typ {
                typ.is_float()
            } else {
                matches!(dst_loc, Loc::VReg(_) | Loc::FImm(_))
            };

            // Get return value size from type
            let ret_size = insn.size.max(32);

            if is_fp_result {
                // FP return value is in V0
                self.emit_fp_move_to_loc(VReg::V0, &dst_loc, ret_size, frame_size);
            } else {
                // Integer return value is in X0
                self.emit_move_to_loc(Reg::X0, &dst_loc, ret_size, frame_size);
            }
        }
    }

    fn emit_select(&mut self, insn: &Instruction, frame_size: i32) {
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
            _ => Reg::X9,
        };

        // Load condition, then and else values
        self.emit_move(cond, Reg::X10, 64, frame_size);
        self.emit_move(then_val, Reg::X11, size, frame_size);
        self.emit_move(else_val, Reg::X12, size, frame_size);

        // LIR: compare condition with zero
        self.push_lir(Aarch64Inst::Cmp {
            size: OperandSize::B64,
            src1: Reg::X10,
            src2: GpOperand::Imm(0),
        });

        // Use csel: if cond != 0, select then_val, else select else_val
        self.push_lir(Aarch64Inst::Csel {
            size: op_size,
            cond: Cond::Ne,
            src_true: Reg::X11,
            src_false: Reg::X12,
            dst: dst_reg,
        });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size, frame_size);
        }
    }

    fn emit_extend(&mut self, insn: &Instruction, frame_size: i32) {
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
            _ => Reg::X9,
        };

        match insn.op {
            Opcode::Zext => {
                // Zero extend: use uxtb, uxth, or just mov for 32->64
                self.emit_move(src, dst_reg, 64, frame_size);
                match insn.size {
                    8 => {
                        self.push_lir(Aarch64Inst::Uxtb {
                            src: dst_reg,
                            dst: dst_reg,
                        });
                    }
                    16 => {
                        self.push_lir(Aarch64Inst::Uxth {
                            src: dst_reg,
                            dst: dst_reg,
                        });
                    }
                    32 => {
                        // 32-bit ops automatically zero-extend on AArch64
                    }
                    _ => {}
                }
            }
            Opcode::Sext => {
                // Sign extend: use sxtb, sxth, sxtw based on source size
                self.emit_move(src, dst_reg, 64, frame_size);
                match insn.src_size {
                    8 => {
                        self.push_lir(Aarch64Inst::Sxtb {
                            dst_size: OperandSize::B64,
                            src: dst_reg,
                            dst: dst_reg,
                        });
                    }
                    16 => {
                        self.push_lir(Aarch64Inst::Sxth {
                            dst_size: OperandSize::B64,
                            src: dst_reg,
                            dst: dst_reg,
                        });
                    }
                    32 => {
                        self.push_lir(Aarch64Inst::Sxtw {
                            src: dst_reg,
                            dst: dst_reg,
                        });
                    }
                    _ => {}
                }
            }
            Opcode::Trunc => {
                // Truncate: move value then mask to target size
                self.emit_move(src, dst_reg, 64, frame_size);
                // Mask to target size using AND
                match insn.size {
                    8 => {
                        self.push_lir(Aarch64Inst::And {
                            size: OperandSize::B32,
                            src1: dst_reg,
                            src2: GpOperand::Imm(0xff),
                            dst: dst_reg,
                        });
                    }
                    16 => {
                        self.push_lir(Aarch64Inst::And {
                            size: OperandSize::B32,
                            src1: dst_reg,
                            src2: GpOperand::Imm(0xffff),
                            dst: dst_reg,
                        });
                    }
                    32 => {
                        // 32-bit already handled - writing to w register zeros upper 32 bits
                    }
                    _ => {}
                }
            }
            _ => {}
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, insn.size, frame_size);
        }
    }

    fn emit_copy_with_type(
        &mut self,
        src: PseudoId,
        dst: PseudoId,
        size: u32,
        typ: Option<&Type>,
        frame_size: i32,
    ) {
        // Keep actual size for handling narrow types
        let actual_size = size;
        let reg_size = size.max(32);
        let dst_loc = self.get_location(dst);
        let src_loc = self.get_location(src);

        // Check if this is a FP copy (source or dest is in VReg or is FImm)
        let is_fp_copy =
            matches!(&src_loc, Loc::VReg(_) | Loc::FImm(_)) || matches!(&dst_loc, Loc::VReg(_));

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
            let dst_vreg = match &dst_loc {
                Loc::VReg(v) => *v,
                _ => VReg::V16, // Use scratch register
            };

            self.emit_fp_move(src, dst_vreg, reg_size, frame_size);

            if !matches!(&dst_loc, Loc::VReg(v) if *v == dst_vreg) {
                self.emit_fp_move_to_loc(dst_vreg, &dst_loc, reg_size, frame_size);
            }
        } else {
            // Integer copy
            match &dst_loc {
                Loc::Reg(r) => {
                    self.emit_move(src, *r, reg_size, frame_size);
                    // For narrow types (8 or 16 bits), extend to correct width
                    // AARCH64: UXTB/UXTH for unsigned, SXTB/SXTH for signed
                    if actual_size == 8 {
                        if is_unsigned {
                            self.push_lir(Aarch64Inst::Uxtb { src: *r, dst: *r });
                        } else {
                            self.push_lir(Aarch64Inst::Sxtb {
                                dst_size: OperandSize::B32,
                                src: *r,
                                dst: *r,
                            });
                        }
                    } else if actual_size == 16 {
                        if is_unsigned {
                            self.push_lir(Aarch64Inst::Uxth { src: *r, dst: *r });
                        } else {
                            self.push_lir(Aarch64Inst::Sxth {
                                dst_size: OperandSize::B32,
                                src: *r,
                                dst: *r,
                            });
                        }
                    }
                }
                Loc::Stack(_) => {
                    self.emit_move(src, Reg::X9, reg_size, frame_size);
                    // For narrow types stored to stack, use the actual size
                    if actual_size <= 16 {
                        self.emit_move_to_loc(Reg::X9, &dst_loc, actual_size, frame_size);
                    } else {
                        self.emit_move_to_loc(Reg::X9, &dst_loc, reg_size, frame_size);
                    }
                }
                _ => {}
            }
        }
    }

    // ========================================================================
    // Floating-Point Operations
    // ========================================================================

    /// Move FP value to a VReg
    fn emit_fp_move(&mut self, src: PseudoId, dst: VReg, size: u32, frame_size: i32) {
        let loc = self.get_location(src);
        let fp_size = FpSize::from_bits(size.max(32));

        match loc {
            Loc::VReg(v) if v == dst => {}
            Loc::VReg(v) => {
                self.push_lir(Aarch64Inst::FmovReg {
                    size: fp_size,
                    src: v,
                    dst,
                });
            }
            Loc::Stack(offset) => {
                let actual_offset = if offset < 0 {
                    frame_size + offset
                } else {
                    offset
                };
                self.push_lir(Aarch64Inst::LdrFp {
                    size: fp_size,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset: actual_offset,
                    },
                    dst,
                });
            }
            Loc::FImm(f) => {
                // Load FP constant using integer register
                let (scratch0, _) = Reg::scratch_regs();
                let bits = if size <= 32 {
                    (f as f32).to_bits() as i64
                } else {
                    f.to_bits() as i64
                };
                self.emit_mov_imm(scratch0, bits, 64);
                self.push_lir(Aarch64Inst::FmovFromGp {
                    size: fp_size,
                    src: scratch0,
                    dst,
                });
            }
            Loc::Reg(r) => {
                // Move from integer register to FP register
                self.push_lir(Aarch64Inst::FmovFromGp {
                    size: fp_size,
                    src: r,
                    dst,
                });
            }
            Loc::Imm(v) => {
                // Load integer immediate and move to FP
                let (scratch0, _) = Reg::scratch_regs();
                self.emit_mov_imm(scratch0, v, 64);
                self.push_lir(Aarch64Inst::FmovFromGp {
                    size: fp_size,
                    src: scratch0,
                    dst,
                });
            }
            Loc::Global(name) => {
                // Load from global
                let (scratch0, _) = Reg::scratch_regs();
                self.emit_load_global(&name, scratch0);
                self.push_lir(Aarch64Inst::FmovFromGp {
                    size: fp_size,
                    src: scratch0,
                    dst,
                });
            }
        }
    }

    /// Move FP register value to a location
    fn emit_fp_move_to_loc(&mut self, src: VReg, dst: &Loc, size: u32, frame_size: i32) {
        let fp_size = FpSize::from_bits(size.max(32));

        match dst {
            Loc::VReg(v) if *v == src => {}
            Loc::VReg(v) => {
                self.push_lir(Aarch64Inst::FmovReg {
                    size: fp_size,
                    src,
                    dst: *v,
                });
            }
            Loc::Stack(offset) => {
                let actual_offset = if *offset < 0 {
                    frame_size + *offset
                } else {
                    *offset
                };
                self.push_lir(Aarch64Inst::StrFp {
                    size: fp_size,
                    src,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset: actual_offset,
                    },
                });
            }
            Loc::Reg(r) => {
                // Move from FP register to integer register
                self.push_lir(Aarch64Inst::FmovToGp {
                    size: fp_size,
                    src,
                    dst: *r,
                });
            }
            _ => {}
        }
    }

    /// Emit FP binary operation (fadd, fsub, fmul, fdiv)
    fn emit_fp_binop(&mut self, insn: &Instruction, frame_size: i32) {
        let size = insn.size.max(32);
        let fp_size = FpSize::from_bits(size);
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
            Loc::VReg(v) => *v,
            _ => VReg::V16,
        };

        // Load operands
        self.emit_fp_move(src1, VReg::V17, size, frame_size);
        self.emit_fp_move(src2, VReg::V18, size, frame_size);

        match insn.op {
            Opcode::FAdd => {
                self.push_lir(Aarch64Inst::Fadd {
                    size: fp_size,
                    src1: VReg::V17,
                    src2: VReg::V18,
                    dst: work_reg,
                });
            }
            Opcode::FSub => {
                self.push_lir(Aarch64Inst::Fsub {
                    size: fp_size,
                    src1: VReg::V17,
                    src2: VReg::V18,
                    dst: work_reg,
                });
            }
            Opcode::FMul => {
                self.push_lir(Aarch64Inst::Fmul {
                    size: fp_size,
                    src1: VReg::V17,
                    src2: VReg::V18,
                    dst: work_reg,
                });
            }
            Opcode::FDiv => {
                self.push_lir(Aarch64Inst::Fdiv {
                    size: fp_size,
                    src1: VReg::V17,
                    src2: VReg::V18,
                    dst: work_reg,
                });
            }
            _ => return,
        }

        if !matches!(&dst_loc, Loc::VReg(v) if *v == work_reg) {
            self.emit_fp_move_to_loc(work_reg, &dst_loc, size, frame_size);
        }
    }

    /// Emit FP negation
    fn emit_fp_neg(&mut self, insn: &Instruction, frame_size: i32) {
        let size = insn.size.max(32);
        let fp_size = FpSize::from_bits(size);
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
            Loc::VReg(v) => *v,
            _ => VReg::V16,
        };

        self.emit_fp_move(src, VReg::V17, size, frame_size);

        self.push_lir(Aarch64Inst::Fneg {
            size: fp_size,
            src: VReg::V17,
            dst: work_reg,
        });

        if !matches!(&dst_loc, Loc::VReg(v) if *v == work_reg) {
            self.emit_fp_move_to_loc(work_reg, &dst_loc, size, frame_size);
        }
    }

    /// Emit FP comparison
    fn emit_fp_compare(&mut self, insn: &Instruction, frame_size: i32) {
        let size = insn.size.max(32);
        let fp_size = FpSize::from_bits(size);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load operands to FP registers
        self.emit_fp_move(src1, VReg::V17, size, frame_size);
        self.emit_fp_move(src2, VReg::V18, size, frame_size);

        // Perform comparison
        self.push_lir(Aarch64Inst::Fcmp {
            size: fp_size,
            src1: VReg::V17,
            src2: VReg::V18,
        });

        // Get result location
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X9,
        };

        // Set result based on condition
        let cond = match insn.op {
            Opcode::FCmpOEq => Cond::Eq,
            Opcode::FCmpONe => Cond::Ne,
            Opcode::FCmpOLt => Cond::Lt,
            Opcode::FCmpOLe => Cond::Le,
            Opcode::FCmpOGt => Cond::Gt,
            Opcode::FCmpOGe => Cond::Ge,
            _ => return,
        };

        self.push_lir(Aarch64Inst::Cset { cond, dst: dst_reg });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, 32, frame_size);
        }
    }

    /// Emit integer to float conversion
    fn emit_int_to_float(&mut self, insn: &Instruction, frame_size: i32) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_size = insn.src_size.max(32);
        let dst_size = insn.size.max(32);
        let fp_size = FpSize::from_bits(dst_size);
        let int_size = OperandSize::from_bits(src_size);

        let dst_loc = self.get_location(target);
        let dst_vreg = match &dst_loc {
            Loc::VReg(v) => *v,
            _ => VReg::V16,
        };

        // Load source to integer register
        let (scratch0, _) = Reg::scratch_regs();
        self.emit_move(src, scratch0, src_size, frame_size);

        // Convert integer to float
        // scvtf/ucvtf: signed/unsigned int to float
        match insn.op {
            Opcode::SCvtF => {
                self.push_lir(Aarch64Inst::Scvtf {
                    int_size,
                    fp_size,
                    src: scratch0,
                    dst: dst_vreg,
                });
            }
            Opcode::UCvtF => {
                self.push_lir(Aarch64Inst::Ucvtf {
                    int_size,
                    fp_size,
                    src: scratch0,
                    dst: dst_vreg,
                });
            }
            _ => return,
        }

        if !matches!(&dst_loc, Loc::VReg(v) if *v == dst_vreg) {
            self.emit_fp_move_to_loc(dst_vreg, &dst_loc, dst_size, frame_size);
        }
    }

    /// Emit float to integer conversion
    fn emit_float_to_int(&mut self, insn: &Instruction, frame_size: i32) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_size = insn.src_size.max(32);
        let dst_size = insn.size.max(32);
        let fp_size = FpSize::from_bits(src_size);
        let int_size = OperandSize::from_bits(dst_size);

        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X9,
        };

        // Load source to FP register
        self.emit_fp_move(src, VReg::V17, src_size, frame_size);

        // Convert float to integer using truncation toward zero
        // fcvtzu/fcvtzs: float to unsigned/signed int with truncation
        match insn.op {
            Opcode::FCvtU => {
                self.push_lir(Aarch64Inst::Fcvtzu {
                    fp_size,
                    int_size,
                    src: VReg::V17,
                    dst: dst_reg,
                });
            }
            Opcode::FCvtS => {
                self.push_lir(Aarch64Inst::Fcvtzs {
                    fp_size,
                    int_size,
                    src: VReg::V17,
                    dst: dst_reg,
                });
            }
            _ => return,
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, dst_size, frame_size);
        }
    }

    /// Emit float to float conversion (size change)
    fn emit_float_to_float(&mut self, insn: &Instruction, frame_size: i32) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_size = insn.src_size.max(32);
        let dst_size = insn.size.max(32);
        let src_fp_size = FpSize::from_bits(src_size);
        let dst_fp_size = FpSize::from_bits(dst_size);

        let dst_loc = self.get_location(target);
        let dst_vreg = match &dst_loc {
            Loc::VReg(v) => *v,
            _ => VReg::V16,
        };

        // Load source to FP register
        self.emit_fp_move(src, VReg::V17, src_size, frame_size);

        // Convert between float sizes
        // fcvt: convert between single and double precision
        self.push_lir(Aarch64Inst::Fcvt {
            src_size: src_fp_size,
            dst_size: dst_fp_size,
            src: VReg::V17,
            dst: dst_vreg,
        });

        if !matches!(&dst_loc, Loc::VReg(v) if *v == dst_vreg) {
            self.emit_fp_move_to_loc(dst_vreg, &dst_loc, dst_size, frame_size);
        }
    }

    // ========================================================================
    // Variadic function support (va_* builtins)
    // ========================================================================
    //
    // On macOS ARM64 (Apple Silicon), va_list is simply a char* pointer
    // pointing to the first variadic argument on the stack. This is much
    // simpler than x86-64 or Linux ARM64.
    //
    // The calling convention places all variadic arguments on the stack,
    // starting at a known offset from the frame pointer.

    /// Emit va_start: Initialize va_list to point to first variadic arg
    fn emit_va_start(&mut self, insn: &Instruction, frame_size: i32) {
        let ap_addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };

        // For macOS ARM64, va_list is char*
        // We need to compute the address of the first variadic argument on the stack
        // The variadic arguments are passed on the stack, after the register save area
        //
        // For now, emit a placeholder that sets ap to point to stack area
        // This will need refinement based on actual stack layout

        let ap_loc = self.get_location(ap_addr);
        let (scratch0, _) = Reg::scratch_regs();

        // On Apple ARM64, variadic args start at frame pointer + 16 (after saved fp, lr)
        // plus any space used for register-passed args that were spilled
        // For simplicity, we'll use fp + 16 as the base for now
        // ap = fp + 16 (where variadic args start on Apple ABI)
        // Linux ARM64 uses the same for now
        self.push_lir(Aarch64Inst::Add {
            size: OperandSize::B64,
            src1: Reg::X29,
            src2: GpOperand::Imm(16),
            dst: scratch0,
        });

        // Store the computed address to ap
        match ap_loc {
            Loc::Stack(offset) => {
                let actual_offset = if offset < 0 {
                    frame_size + offset
                } else {
                    offset
                };
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: scratch0,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset: actual_offset,
                    },
                });
            }
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(scratch0),
                    dst: r,
                });
            }
            _ => {}
        }
    }

    /// Emit va_arg: Get the next variadic argument of the specified type
    fn emit_va_arg(&mut self, insn: &Instruction, frame_size: i32) {
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
        let arg_bytes = (arg_size / 8).max(8) as i64; // Minimum 8 bytes per slot on ARM64

        let ap_loc = self.get_location(ap_addr);
        let dst_loc = self.get_location(target);
        let (scratch0, scratch1) = Reg::scratch_regs();

        // Load current ap value
        match &ap_loc {
            Loc::Stack(offset) => {
                let actual_offset = if *offset < 0 {
                    frame_size + *offset
                } else {
                    *offset
                };
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset: actual_offset,
                    },
                    dst: scratch0,
                });
            }
            Loc::Reg(r) => {
                if *r != scratch0 {
                    self.push_lir(Aarch64Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(*r),
                        dst: scratch0,
                    });
                }
            }
            _ => return,
        }

        // Load the argument from *ap
        if arg_type.is_float() {
            // Load floating point value
            let fp_size = arg_type.size_bits();
            let fp_size_enum = FpSize::from_bits(fp_size);
            self.push_lir(Aarch64Inst::LdrFp {
                size: fp_size_enum,
                addr: MemAddr::Base(scratch0),
                dst: VReg::V16,
            });

            // Store to destination
            match &dst_loc {
                Loc::VReg(v) => {
                    self.push_lir(Aarch64Inst::FmovReg {
                        size: fp_size_enum,
                        src: VReg::V16,
                        dst: *v,
                    });
                }
                Loc::Stack(offset) => {
                    let actual_offset = if *offset < 0 {
                        frame_size + *offset
                    } else {
                        *offset
                    };
                    self.push_lir(Aarch64Inst::StrFp {
                        size: fp_size_enum,
                        src: VReg::V16,
                        addr: MemAddr::BaseOffset {
                            base: Reg::SP,
                            offset: actual_offset,
                        },
                    });
                }
                _ => {}
            }
        } else {
            // Load integer value
            let op_size = OperandSize::from_bits(arg_size);
            self.push_lir(Aarch64Inst::Ldr {
                size: op_size,
                addr: MemAddr::Base(scratch0),
                dst: scratch1,
            });

            // Store to destination
            match &dst_loc {
                Loc::Reg(r) => {
                    if *r != scratch1 {
                        self.push_lir(Aarch64Inst::Mov {
                            size: op_size,
                            src: GpOperand::Reg(scratch1),
                            dst: *r,
                        });
                    }
                }
                Loc::Stack(offset) => {
                    let actual_offset = if *offset < 0 {
                        frame_size + *offset
                    } else {
                        *offset
                    };
                    self.push_lir(Aarch64Inst::Str {
                        size: OperandSize::B64,
                        src: scratch1,
                        addr: MemAddr::BaseOffset {
                            base: Reg::SP,
                            offset: actual_offset,
                        },
                    });
                }
                _ => {}
            }
        }

        // Advance ap by the argument size
        self.push_lir(Aarch64Inst::Add {
            size: OperandSize::B64,
            src1: scratch0,
            src2: GpOperand::Imm(arg_bytes),
            dst: scratch0,
        });

        // Store updated ap back
        match &ap_loc {
            Loc::Stack(offset) => {
                let actual_offset = if *offset < 0 {
                    frame_size + *offset
                } else {
                    *offset
                };
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: scratch0,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset: actual_offset,
                    },
                });
            }
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(scratch0),
                    dst: *r,
                });
            }
            _ => {}
        }
    }

    /// Emit va_copy: Copy a va_list
    fn emit_va_copy(&mut self, insn: &Instruction, frame_size: i32) {
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
        let (scratch0, _) = Reg::scratch_regs();

        // On macOS ARM64, va_list is just a pointer, so copy is simple
        // Load src value
        match &src_loc {
            Loc::Stack(offset) => {
                let actual_offset = if *offset < 0 {
                    frame_size + *offset
                } else {
                    *offset
                };
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset: actual_offset,
                    },
                    dst: scratch0,
                });
            }
            Loc::Reg(r) => {
                if *r != scratch0 {
                    self.push_lir(Aarch64Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(*r),
                        dst: scratch0,
                    });
                }
            }
            _ => return,
        }

        // Store to dest
        match &dest_loc {
            Loc::Stack(offset) => {
                let actual_offset = if *offset < 0 {
                    frame_size + *offset
                } else {
                    *offset
                };
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: scratch0,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset: actual_offset,
                    },
                });
            }
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(scratch0),
                    dst: *r,
                });
            }
            _ => {}
        }
    }

    // =========================================================================
    // Byte-swapping builtins
    // =========================================================================

    /// Emit bswap16: Byte-swap a 16-bit value
    /// ARM64 uses rev16 to reverse bytes within each 16-bit halfword
    fn emit_bswap16(&mut self, insn: &Instruction, frame_size: i32) {
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
        let scratch = Reg::X9;

        // Load source into scratch register
        match src_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(r),
                    dst: scratch,
                });
            }
            Loc::Stack(off) => {
                let adjusted = frame_size - off;
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B16,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset: adjusted,
                    },
                    dst: scratch,
                });
            }
            Loc::Imm(v) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(v),
                    dst: scratch,
                });
            }
            _ => return,
        }

        // rev16 reverses bytes within each halfword
        self.push_lir(Aarch64Inst::Rev16 {
            size: OperandSize::B32,
            src: scratch,
            dst: scratch,
        });

        // Mask to 16 bits
        self.push_lir(Aarch64Inst::And {
            size: OperandSize::B32,
            src1: scratch,
            src2: GpOperand::Imm(0xFFFF),
            dst: scratch,
        });

        // Store result
        match dst_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(scratch),
                    dst: r,
                });
            }
            Loc::Stack(off) => {
                let adjusted = frame_size - off;
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B16,
                    src: scratch,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset: adjusted,
                    },
                });
            }
            _ => {}
        }
    }

    /// Emit bswap32: Byte-swap a 32-bit value
    fn emit_bswap32(&mut self, insn: &Instruction, frame_size: i32) {
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
        let scratch = Reg::X9;

        // Load source into scratch register
        match src_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(r),
                    dst: scratch,
                });
            }
            Loc::Stack(off) => {
                let adjusted = frame_size - off;
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B32,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset: adjusted,
                    },
                    dst: scratch,
                });
            }
            Loc::Imm(v) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(v),
                    dst: scratch,
                });
            }
            _ => return,
        }

        // rev reverses all bytes in the register
        self.push_lir(Aarch64Inst::Rev {
            size: OperandSize::B32,
            src: scratch,
            dst: scratch,
        });

        // Store result
        match dst_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(scratch),
                    dst: r,
                });
            }
            Loc::Stack(off) => {
                let adjusted = frame_size - off;
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B32,
                    src: scratch,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset: adjusted,
                    },
                });
            }
            _ => {}
        }
    }

    /// Emit bswap64: Byte-swap a 64-bit value
    fn emit_bswap64(&mut self, insn: &Instruction, frame_size: i32) {
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
        let scratch = Reg::X9;

        // Load source into scratch register
        match src_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(r),
                    dst: scratch,
                });
            }
            Loc::Stack(off) => {
                let adjusted = frame_size - off;
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset: adjusted,
                    },
                    dst: scratch,
                });
            }
            Loc::Imm(v) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Imm(v),
                    dst: scratch,
                });
            }
            _ => return,
        }

        // rev reverses all bytes in the 64-bit register
        self.push_lir(Aarch64Inst::Rev {
            size: OperandSize::B64,
            src: scratch,
            dst: scratch,
        });

        // Store result
        match dst_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(scratch),
                    dst: r,
                });
            }
            Loc::Stack(off) => {
                let adjusted = frame_size - off;
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: scratch,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset: adjusted,
                    },
                });
            }
            _ => {}
        }
    }

    /// Emit __builtin_alloca - dynamic stack allocation
    fn emit_alloca(&mut self, insn: &Instruction, frame_size: i32) {
        let size = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load size into X9 (scratch register)
        self.emit_move(size, Reg::X9, 64, frame_size);

        // Round up to 16-byte alignment: (size + 15) & ~15
        self.push_lir(Aarch64Inst::Add {
            size: OperandSize::B64,
            src1: Reg::X9,
            src2: GpOperand::Imm(15),
            dst: Reg::X9,
        });
        self.push_lir(Aarch64Inst::And {
            size: OperandSize::B64,
            src1: Reg::X9,
            src2: GpOperand::Imm(-16),
            dst: Reg::X9,
        });

        // Subtract from stack pointer
        self.push_lir(Aarch64Inst::Sub {
            size: OperandSize::B64,
            src1: Reg::SP,
            src2: GpOperand::Reg(Reg::X9),
            dst: Reg::SP,
        });

        // Return new stack pointer
        self.push_lir(Aarch64Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::SP),
            dst: Reg::X9,
        });

        // Store result
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::X9, &dst_loc, 64, frame_size);
    }
}

// ============================================================================
// CodeGenerator trait implementation
// ============================================================================

impl CodeGenerator for Aarch64CodeGen {
    fn generate(&mut self, module: &Module) -> String {
        self.output.clear();
        self.lir_buffer.clear();
        self.last_debug_line = 0;
        self.last_debug_file = 0;
        self.emit_debug = module.debug;

        // Emit file header
        self.emit_header();

        // Emit .file directives unconditionally (useful for diagnostics/profiling)
        for (i, path) in module.source_files.iter().enumerate() {
            // File indices in DWARF start at 1
            self.push_lir(Aarch64Inst::Directive(Directive::file(
                (i + 1) as u32,
                path.as_str(),
            )));
        }

        // Emit globals
        for (name, typ, init) in &module.globals {
            self.emit_global(name, typ, init);
        }

        // Emit string literals
        self.emit_strings(&module.strings);

        // Emit functions
        for func in &module.functions {
            self.emit_function(func);
        }

        // Flush all buffered LIR instructions to output
        self.emit_all();

        self.output.clone()
    }

    fn set_emit_unwind_tables(&mut self, emit: bool) {
        self.emit_unwind_tables = emit;
    }
}
