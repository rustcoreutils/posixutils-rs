//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 Register Allocator
// Linear scan register allocation for AArch64
//

use crate::arch::regalloc::{
    expire_intervals, find_call_positions, interval_crosses_call, LiveInterval,
};
use crate::ir::{BasicBlockId, Function, Opcode, PseudoId, PseudoKind};
use crate::types::TypeTable;
use std::collections::{HashMap, HashSet};

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

    /// Get 8-bit (byte) register name for vector operations
    pub fn name_b(&self) -> &'static str {
        match self {
            VReg::V0 => "b0",
            VReg::V1 => "b1",
            VReg::V2 => "b2",
            VReg::V3 => "b3",
            VReg::V4 => "b4",
            VReg::V5 => "b5",
            VReg::V6 => "b6",
            VReg::V7 => "b7",
            VReg::V8 => "b8",
            VReg::V9 => "b9",
            VReg::V10 => "b10",
            VReg::V11 => "b11",
            VReg::V12 => "b12",
            VReg::V13 => "b13",
            VReg::V14 => "b14",
            VReg::V15 => "b15",
            VReg::V16 => "b16",
            VReg::V17 => "b17",
            VReg::V18 => "b18",
            VReg::V19 => "b19",
            VReg::V20 => "b20",
            VReg::V21 => "b21",
            VReg::V22 => "b22",
            VReg::V23 => "b23",
            VReg::V24 => "b24",
            VReg::V25 => "b25",
            VReg::V26 => "b26",
            VReg::V27 => "b27",
            VReg::V28 => "b28",
            VReg::V29 => "b29",
            VReg::V30 => "b30",
            VReg::V31 => "b31",
        }
    }

    /// Get full vector register name (v0-v31)
    pub fn name_v(&self) -> &'static str {
        match self {
            VReg::V0 => "v0",
            VReg::V1 => "v1",
            VReg::V2 => "v2",
            VReg::V3 => "v3",
            VReg::V4 => "v4",
            VReg::V5 => "v5",
            VReg::V6 => "v6",
            VReg::V7 => "v7",
            VReg::V8 => "v8",
            VReg::V9 => "v9",
            VReg::V10 => "v10",
            VReg::V11 => "v11",
            VReg::V12 => "v12",
            VReg::V13 => "v13",
            VReg::V14 => "v14",
            VReg::V15 => "v15",
            VReg::V16 => "v16",
            VReg::V17 => "v17",
            VReg::V18 => "v18",
            VReg::V19 => "v19",
            VReg::V20 => "v20",
            VReg::V21 => "v21",
            VReg::V22 => "v22",
            VReg::V23 => "v23",
            VReg::V24 => "v24",
            VReg::V25 => "v25",
            VReg::V26 => "v26",
            VReg::V27 => "v27",
            VReg::V28 => "v28",
            VReg::V29 => "v29",
            VReg::V30 => "v30",
            VReg::V31 => "v31",
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
    /// Floating-point immediate constant (value, size in bits)
    FImm(f64, u32),
    /// Global symbol
    Global(String),
}

// ============================================================================
// Register Allocator (Linear Scan)
// ============================================================================

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
    fp_pseudos: HashSet<PseudoId>,
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
            fp_pseudos: HashSet::new(),
        }
    }

    /// Perform register allocation for a function
    pub fn allocate(&mut self, func: &Function, types: &TypeTable) -> HashMap<PseudoId, Loc> {
        self.reset_state();
        self.identify_fp_pseudos(func, types);
        self.allocate_arguments(func, types);

        let intervals = self.compute_live_intervals(func);

        self.spill_args_across_calls(func, &intervals);
        self.allocate_alloca_to_stack(func);
        self.run_linear_scan(func, types, intervals);

        self.locations.clone()
    }

    /// Reset allocator state for a new function
    fn reset_state(&mut self) {
        self.locations.clear();
        self.free_regs = Reg::allocatable().to_vec();
        self.free_fp_regs = VReg::allocatable().to_vec();
        self.active.clear();
        self.active_fp.clear();
        self.stack_offset = 0;
        self.used_callee_saved.clear();
        self.used_callee_saved_fp.clear();
        self.fp_pseudos.clear();
    }

    /// Pre-allocate argument registers per AAPCS64
    fn allocate_arguments(&mut self, func: &Function, types: &TypeTable) {
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = VReg::arg_regs();
        let mut int_arg_idx = 0usize;
        let mut fp_arg_idx = 0usize;

        // Detect hidden return pointer for large struct returns
        let sret_pseudo = func
            .pseudos
            .iter()
            .find(|p| matches!(p.kind, PseudoKind::Arg(0)) && p.name.as_deref() == Some("__sret"));

        // Allocate X8 for hidden return pointer if present
        if let Some(sret) = sret_pseudo {
            self.locations.insert(sret.id, Loc::Reg(Reg::X8));
        }

        let arg_idx_offset: u32 = if sret_pseudo.is_some() { 1 } else { 0 };

        for (i, (_name, typ)) in func.params.iter().enumerate() {
            for pseudo in &func.pseudos {
                if let PseudoKind::Arg(arg_idx) = pseudo.kind {
                    if arg_idx == (i as u32) + arg_idx_offset {
                        let is_fp = types.is_float(*typ);
                        if is_fp {
                            if fp_arg_idx < fp_arg_regs.len() {
                                self.locations
                                    .insert(pseudo.id, Loc::VReg(fp_arg_regs[fp_arg_idx]));
                                self.free_fp_regs.retain(|&r| r != fp_arg_regs[fp_arg_idx]);
                                self.fp_pseudos.insert(pseudo.id);
                            } else {
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
    }

    /// Force alloca results to stack to avoid clobbering issues
    fn allocate_alloca_to_stack(&mut self, func: &Function) {
        for block in &func.blocks {
            for insn in &block.insns {
                if insn.op == Opcode::Alloca {
                    if let Some(target) = insn.target {
                        self.stack_offset += 8;
                        self.locations
                            .insert(target, Loc::Stack(-self.stack_offset));
                    }
                }
            }
        }
    }

    /// Spill arguments in caller-saved registers if their interval crosses a call
    fn spill_args_across_calls(&mut self, func: &Function, intervals: &[LiveInterval]) {
        let call_positions = find_call_positions(func);

        // Check GP arguments in caller-saved registers (x0-x7)
        let int_arg_regs_set: Vec<Reg> = Reg::arg_regs().to_vec();
        for interval in intervals {
            if let Some(Loc::Reg(reg)) = self.locations.get(&interval.pseudo) {
                if int_arg_regs_set.contains(reg)
                    && interval_crosses_call(interval, &call_positions)
                {
                    let reg_to_restore = *reg;
                    self.stack_offset += 8;
                    self.locations
                        .insert(interval.pseudo, Loc::Stack(-self.stack_offset));
                    self.free_regs.push(reg_to_restore);
                }
            }
        }

        // Check FP arguments in caller-saved registers (v0-v7)
        let fp_arg_regs_set: Vec<VReg> = VReg::arg_regs().to_vec();
        for interval in intervals {
            if let Some(Loc::VReg(reg)) = self.locations.get(&interval.pseudo) {
                if fp_arg_regs_set.contains(reg) && interval_crosses_call(interval, &call_positions)
                {
                    let reg_to_restore = *reg;
                    self.stack_offset += 8;
                    self.locations
                        .insert(interval.pseudo, Loc::Stack(-self.stack_offset));
                    self.free_fp_regs.push(reg_to_restore);
                }
            }
        }
    }

    /// Run the linear scan allocation algorithm
    fn run_linear_scan(
        &mut self,
        func: &Function,
        types: &TypeTable,
        intervals: Vec<LiveInterval>,
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
                        if let Some(local) = func.locals.get(name) {
                            let size = (types.size_bits(local.typ) / 8) as i32;
                            let size = size.max(8);
                            let aligned_size = (size + 7) & !7;
                            self.stack_offset += aligned_size;
                            self.locations
                                .insert(interval.pseudo, Loc::Stack(-self.stack_offset));
                            if types.is_float(local.typ) {
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

            if needs_fp {
                if let Some(reg) = self.free_fp_regs.pop() {
                    if reg.is_callee_saved() && !self.used_callee_saved_fp.contains(&reg) {
                        self.used_callee_saved_fp.push(reg);
                    }
                    self.locations.insert(interval.pseudo, Loc::VReg(reg));
                    self.active_fp.push((interval.clone(), reg));
                    self.active_fp.sort_by_key(|(i, _)| i.end);
                } else {
                    self.stack_offset += 8;
                    self.locations
                        .insert(interval.pseudo, Loc::Stack(-self.stack_offset));
                }
            } else if let Some(reg) = self.free_regs.pop() {
                if reg.is_callee_saved() && !self.used_callee_saved.contains(&reg) {
                    self.used_callee_saved.push(reg);
                }
                self.locations.insert(interval.pseudo, Loc::Reg(reg));
                self.active.push((interval.clone(), reg));
                self.active.sort_by_key(|(i, _)| i.end);
            } else {
                self.stack_offset += 8;
                self.locations
                    .insert(interval.pseudo, Loc::Stack(-self.stack_offset));
            }
        }
    }

    /// Identify which pseudos need FP registers by scanning the IR
    fn identify_fp_pseudos(&mut self, func: &Function, types: &TypeTable) {
        for block in &func.blocks {
            for insn in &block.insns {
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

                if let Some(typ) = insn.typ {
                    if types.is_float(typ)
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
        // Expire GP register intervals
        expire_intervals(&mut self.active, &mut self.free_regs, point);
        // Expire FP register intervals
        expire_intervals(&mut self.active_fp, &mut self.free_fp_regs, point);
    }

    fn compute_live_intervals(&self, func: &Function) -> Vec<LiveInterval> {
        struct IntervalInfo {
            pseudo: PseudoId,
            first_def: usize,
            last_def: usize,
            last_use: usize,
        }

        let mut intervals: HashMap<PseudoId, IntervalInfo> = HashMap::new();
        let mut pos = 0usize;

        let mut block_start_pos: HashMap<BasicBlockId, usize> = HashMap::new();
        let mut block_end_pos: HashMap<BasicBlockId, usize> = HashMap::new();
        let mut temp_pos = 0usize;
        for block in &func.blocks {
            block_start_pos.insert(block.id, temp_pos);
            temp_pos += block.insns.len();
            block_end_pos.insert(block.id, temp_pos.saturating_sub(1));
        }

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
        result
    }

    /// Get stack size needed (aligned to 16 bytes)
    pub fn stack_size(&self) -> i32 {
        (self.stack_offset + 15) & !15
    }

    /// Get callee-saved registers that need to be preserved
    pub fn callee_saved_used(&self) -> &[Reg] {
        &self.used_callee_saved
    }

    /// Get callee-saved floating-point registers that need to be preserved
    pub fn callee_saved_fp_used(&self) -> &[VReg] {
        &self.used_callee_saved_fp
    }
}

impl Default for RegAlloc {
    fn default() -> Self {
        Self::new()
    }
}
