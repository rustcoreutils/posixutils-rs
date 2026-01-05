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
// ============================================================================
// REGISTER ALLOCATION POLICY
// ============================================================================
//
// Scratch registers (NEVER allocated to pseudos):
//   X9, X10, X11 - Reserved for codegen temporaries
//   X16, X17     - Linker scratch (IP0/IP1 per AAPCS64)
//
// Codegen MUST use only scratch registers (X9, X10, X11) for temporaries.
// Using allocatable registers (X0-X7, X12-X15, X19-X28) risks clobbering
// live values that were assigned by the register allocator.
//
// Reserved registers:
//   X8          - Indirect result register (large struct returns)
//   X18         - Platform reserved
//   X29 (FP)    - Frame pointer
//   X30 (LR)    - Link register
//   SP          - Stack pointer
//
// Calling convention (AAPCS64):
//   X0-X7       - Integer/pointer arguments and return values
//   V0-V7       - FP/SIMD arguments and return values
//   X19-X28     - Callee-saved
//   V8-V15      - Callee-saved (lower 64 bits)
// ============================================================================

use crate::arch::regalloc::{
    ConstraintPoint, LiveInterval, compute_live_intervals, expire_intervals, find_call_positions,
    find_conflicting_registers, identify_fp_pseudos, interval_crosses_call,
};
use crate::ir::{Function, Instruction, Opcode, PseudoId, PseudoKind};
use crate::types::TypeTable;
use std::collections::{HashMap, HashSet};

/// Get constraint info for an instruction (used by shared compute_live_intervals).
/// AArch64 has no implicit register constraints (unlike x86-64 which needs
/// Rax/Rdx for division and Rcx for shifts), so this always returns None.
#[allow(clippy::unnecessary_wraps)]
pub fn get_constraint_info(_insn: &Instruction) -> Option<(Vec<Reg>, Vec<PseudoId>)> {
    None
}

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
    // X18 is platform-reserved (TLS on Linux), not used by compiler
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
    // Zero register (always reads as zero, discards writes)
    Xzr,
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
            Reg::Xzr => "xzr",
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
            Reg::Xzr => "wzr",
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
    /// Excludes: x8 (indirect result), x9/x10/x11 (codegen scratch),
    ///           x16/x17 (linker scratch), x18 (platform), x29 (fp), x30 (lr), sp
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
            // Skip x9, x10, x11 (codegen scratch registers)
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
    /// x9, x10, x11 are reserved for codegen scratch
    /// x16 (IP0) and x17 (IP1) are linker scratch registers
    pub fn scratch_regs() -> (Reg, Reg, Reg) {
        (Reg::X9, Reg::X10, Reg::X11)
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

/// Information about an argument spilled from a caller-saved register to stack
#[derive(Debug, Clone)]
pub struct SpilledArg {
    /// The pseudo that was spilled
    pub pseudo: PseudoId,
    /// The GP register the argument originally arrived in (if integer arg)
    pub from_gp_reg: Option<Reg>,
    /// The FP register the argument originally arrived in (if FP arg)
    pub from_fp_reg: Option<VReg>,
    /// The stack offset where it was spilled to
    pub to_stack_offset: i32,
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
    fp_pseudos: HashSet<PseudoId>,
    /// Arguments spilled from caller-saved registers to stack
    spilled_args: Vec<SpilledArg>,
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
        self.free_fp_regs = VReg::allocatable().to_vec();
        self.active.clear();
        self.active_fp.clear();
        self.stack_offset = 0;
        self.used_callee_saved.clear();
        self.used_callee_saved_fp.clear();
        self.fp_pseudos.clear();
        self.spilled_args.clear();
    }

    /// Pre-allocate argument registers per AAPCS64
    ///
    /// AAPCS64 passes arguments as follows:
    /// - First 8 integer/pointer args in X0-X7
    /// - First 8 FP args in V0-V7 (D0-D7 for doubles, S0-S7 for floats)
    /// - Remaining args go on the stack in parameter order (not separated by type)
    fn allocate_arguments(&mut self, func: &Function, types: &TypeTable) {
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = VReg::arg_regs();
        let mut int_arg_idx = 0usize;
        let mut fp_arg_idx = 0usize;
        // Stack offset for overflow args - must be shared across all types
        // because AAPCS64 places stack args in parameter order
        let mut stack_arg_offset = 16i32;

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
                                // Stack args are placed in parameter order per AAPCS64
                                self.locations
                                    .insert(pseudo.id, Loc::Stack(stack_arg_offset));
                                self.fp_pseudos.insert(pseudo.id);
                                stack_arg_offset += 8;
                            }
                            fp_arg_idx += 1;
                        } else {
                            if int_arg_idx < int_arg_regs.len() {
                                self.locations
                                    .insert(pseudo.id, Loc::Reg(int_arg_regs[int_arg_idx]));
                                self.free_regs.retain(|&r| r != int_arg_regs[int_arg_idx]);
                            } else {
                                // Stack args are placed in parameter order per AAPCS64
                                self.locations
                                    .insert(pseudo.id, Loc::Stack(stack_arg_offset));
                                stack_arg_offset += 8;
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
    fn spill_args_across_calls(
        &mut self,
        _func: &Function,
        intervals: &[LiveInterval],
        call_positions: &[usize],
    ) {
        // Check GP arguments in caller-saved registers (x0-x7)
        let int_arg_regs_set: Vec<Reg> = Reg::arg_regs().to_vec();
        for interval in intervals {
            if let Some(Loc::Reg(reg)) = self.locations.get(&interval.pseudo) {
                if int_arg_regs_set.contains(reg) && interval_crosses_call(interval, call_positions)
                {
                    let from_reg = *reg;
                    self.stack_offset += 8;
                    let to_stack_offset = -self.stack_offset;

                    // Record the spill for codegen to emit stores in prologue
                    self.spilled_args.push(SpilledArg {
                        pseudo: interval.pseudo,
                        from_gp_reg: Some(from_reg),
                        from_fp_reg: None,
                        to_stack_offset,
                    });

                    self.locations
                        .insert(interval.pseudo, Loc::Stack(to_stack_offset));
                    self.free_regs.push(from_reg);
                }
            }
        }

        // Check FP arguments in caller-saved registers (v0-v7)
        let fp_arg_regs_set: Vec<VReg> = VReg::arg_regs().to_vec();
        for interval in intervals {
            if let Some(Loc::VReg(reg)) = self.locations.get(&interval.pseudo) {
                if fp_arg_regs_set.contains(reg) && interval_crosses_call(interval, call_positions)
                {
                    let from_reg = *reg;
                    self.stack_offset += 8;
                    let to_stack_offset = -self.stack_offset;

                    // Record the spill for codegen to emit stores in prologue
                    self.spilled_args.push(SpilledArg {
                        pseudo: interval.pseudo,
                        from_gp_reg: None,
                        from_fp_reg: Some(from_reg),
                        to_stack_offset,
                    });

                    self.locations
                        .insert(interval.pseudo, Loc::Stack(to_stack_offset));
                    self.free_fp_regs.push(from_reg);
                }
            }
        }
    }

    /// Get arguments that were spilled from caller-saved registers
    pub fn spilled_args(&self) -> &[SpilledArg] {
        &self.spilled_args
    }

    /// Run the linear scan allocation algorithm
    fn run_linear_scan(
        &mut self,
        func: &Function,
        types: &TypeTable,
        intervals: Vec<LiveInterval>,
        call_positions: &[usize],
        constraint_points: &[ConstraintPoint<Reg>],
    ) {
        for interval in intervals {
            self.expire_old_intervals(interval.start);

            if self.locations.contains_key(&interval.pseudo) {
                continue;
            }

            // Find registers that would conflict with constraints at this interval
            let conflicting = find_conflicting_registers(&interval, constraint_points);

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
            let crosses_call = interval_crosses_call(&interval, call_positions);

            if needs_fp {
                if crosses_call {
                    // FP interval crosses a call - must use callee-saved FP register or spill
                    if let Some(idx) = self.free_fp_regs.iter().position(|r| r.is_callee_saved()) {
                        let reg = self.free_fp_regs.remove(idx);
                        if !self.used_callee_saved_fp.contains(&reg) {
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
                } else if let Some(reg) = self.free_fp_regs.pop() {
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
            } else if crosses_call {
                // GP interval crosses a call - must use callee-saved register or spill
                // Also exclude registers that conflict with constraints
                if let Some(idx) = self
                    .free_regs
                    .iter()
                    .position(|r| r.is_callee_saved() && !conflicting.contains(r))
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
                        .insert(interval.pseudo, Loc::Stack(-self.stack_offset));
                }
            } else {
                // Interval doesn't cross a call - prefer caller-saved registers
                // to minimize callee-saved register usage and stack conflicts
                // Also exclude registers that conflict with constraints
                let reg_opt = if let Some(idx) = self
                    .free_regs
                    .iter()
                    .position(|r| !r.is_callee_saved() && !conflicting.contains(r))
                {
                    Some(self.free_regs.remove(idx))
                } else if conflicting.is_empty() {
                    // No constraints - use pop() to maintain original allocation order
                    // (takes from end of list, which preserves existing behavior)
                    self.free_regs.pop()
                } else if let Some(idx) = self
                    .free_regs
                    .iter()
                    .rposition(|r| !conflicting.contains(r))
                {
                    // With constraints - use rposition to prefer registers from end
                    // (like pop() but filtering out conflicting registers)
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
                        .insert(interval.pseudo, Loc::Stack(-self.stack_offset));
                }
            }
        }
    }

    fn expire_old_intervals(&mut self, point: usize) {
        // Expire GP register intervals
        expire_intervals(&mut self.active, &mut self.free_regs, point);
        // Expire FP register intervals
        expire_intervals(&mut self.active_fp, &mut self.free_fp_regs, point);
    }

    fn compute_live_intervals(
        &self,
        func: &Function,
    ) -> (Vec<LiveInterval>, Vec<ConstraintPoint<Reg>>) {
        compute_live_intervals(func, get_constraint_info)
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
