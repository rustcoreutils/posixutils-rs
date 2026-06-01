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
    compute_live_intervals, find_call_positions, identify_addr_taken_syms, identify_fp_pseudos,
    interval_crosses_call, ConstraintPoint, FreeSlot, LiveInterval, LivenessResult,
};
use crate::ir::{Function, Instruction, Opcode, PseudoId, PseudoKind};
use crate::types::{TypeId, TypeKind, TypeTable};
use std::collections::{BTreeMap, HashMap, HashSet};

// ============================================================================
// AArch64 Register Definitions
// ============================================================================

/// AArch64 physical registers
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

    /// Get 16-bit (half) register name
    pub fn name_h(&self) -> &'static str {
        match self {
            VReg::V0 => "h0",
            VReg::V1 => "h1",
            VReg::V2 => "h2",
            VReg::V3 => "h3",
            VReg::V4 => "h4",
            VReg::V5 => "h5",
            VReg::V6 => "h6",
            VReg::V7 => "h7",
            VReg::V8 => "h8",
            VReg::V9 => "h9",
            VReg::V10 => "h10",
            VReg::V11 => "h11",
            VReg::V12 => "h12",
            VReg::V13 => "h13",
            VReg::V14 => "h14",
            VReg::V15 => "h15",
            VReg::V16 => "h16",
            VReg::V17 => "h17",
            VReg::V18 => "h18",
            VReg::V19 => "h19",
            VReg::V20 => "h20",
            VReg::V21 => "h21",
            VReg::V22 => "h22",
            VReg::V23 => "h23",
            VReg::V24 => "h24",
            VReg::V25 => "h25",
            VReg::V26 => "h26",
            VReg::V27 => "h27",
            VReg::V28 => "h28",
            VReg::V29 => "h29",
            VReg::V30 => "h30",
            VReg::V31 => "h31",
        }
    }

    /// Get 128-bit (quad) register name
    pub fn name_q(&self) -> &'static str {
        match self {
            VReg::V0 => "q0",
            VReg::V1 => "q1",
            VReg::V2 => "q2",
            VReg::V3 => "q3",
            VReg::V4 => "q4",
            VReg::V5 => "q5",
            VReg::V6 => "q6",
            VReg::V7 => "q7",
            VReg::V8 => "q8",
            VReg::V9 => "q9",
            VReg::V10 => "q10",
            VReg::V11 => "q11",
            VReg::V12 => "q12",
            VReg::V13 => "q13",
            VReg::V14 => "q14",
            VReg::V15 => "q15",
            VReg::V16 => "q16",
            VReg::V17 => "q17",
            VReg::V18 => "q18",
            VReg::V19 => "q19",
            VReg::V20 => "q20",
            VReg::V21 => "q21",
            VReg::V22 => "q22",
            VReg::V23 => "q23",
            VReg::V24 => "q24",
            VReg::V25 => "q25",
            VReg::V26 => "q26",
            VReg::V27 => "q27",
            VReg::V28 => "q28",
            VReg::V29 => "q29",
            VReg::V30 => "q30",
            VReg::V31 => "q31",
        }
    }

    /// Get register name for a given size (16 = half, 32 = float, 64 = double, 128 = quad)
    pub fn name_for_size(&self, bits: u32) -> &'static str {
        match bits {
            0..=16 => self.name_h(),
            17..=32 => self.name_s(),
            33..=64 => self.name_d(),
            _ => self.name_q(),
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
    Imm(i128),
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

/// Map a single-letter GCC operand-constraint Fixed-register letter
/// to the corresponding aarch64 GP register. AAPCS64 has no
/// "specific register" constraint letters in the C2 vocabulary;
/// all aarch64 inline-asm register operands use `"r"` (Any GP) or
/// `"w"` (Any V), both of which `parse_constraint` handles directly
/// without consulting this table. So the current resolver returns
/// `None` for every letter — but it exists for symmetry with
/// `parse_x86_64_fixed_letter` and as the future-extension point
/// for rare aarch64 letters (`"k"`/`"X"`/`"Z"`, ...) when they're
/// brought in scope.
pub fn parse_aarch64_fixed_letter(_letter: char) -> Option<Reg> {
    None
}

/// Map a clobber-list register name (lowercase, GCC-style) to the
/// corresponding `Reg`. Accepts the 64-bit canonical name (`x0`, `x29`,
/// ...), the 32-bit alias (`w0`, `w29`, ...), and special names
/// (`sp`, `xzr`, `wzr`, `fp`, `lr`). Returns `None` for names the GP
/// table doesn't know about (V registers, `memory`, `cc`, ...).
fn parse_gp_clobber_name(raw: &str) -> Option<Reg> {
    let s = raw.trim_start_matches('%').to_ascii_lowercase();
    Some(match s.as_str() {
        "x0" | "w0" => Reg::X0,
        "x1" | "w1" => Reg::X1,
        "x2" | "w2" => Reg::X2,
        "x3" | "w3" => Reg::X3,
        "x4" | "w4" => Reg::X4,
        "x5" | "w5" => Reg::X5,
        "x6" | "w6" => Reg::X6,
        "x7" | "w7" => Reg::X7,
        "x8" | "w8" => Reg::X8,
        "x9" | "w9" => Reg::X9,
        "x10" | "w10" => Reg::X10,
        "x11" | "w11" => Reg::X11,
        "x12" | "w12" => Reg::X12,
        "x13" | "w13" => Reg::X13,
        "x14" | "w14" => Reg::X14,
        "x15" | "w15" => Reg::X15,
        "x16" | "w16" | "ip0" => Reg::X16,
        "x17" | "w17" | "ip1" => Reg::X17,
        // x18 platform-reserved — not in Reg enum
        "x19" | "w19" => Reg::X19,
        "x20" | "w20" => Reg::X20,
        "x21" | "w21" => Reg::X21,
        "x22" | "w22" => Reg::X22,
        "x23" | "w23" => Reg::X23,
        "x24" | "w24" => Reg::X24,
        "x25" | "w25" => Reg::X25,
        "x26" | "w26" => Reg::X26,
        "x27" | "w27" => Reg::X27,
        "x28" | "w28" => Reg::X28,
        "x29" | "w29" | "fp" => Reg::X29,
        "x30" | "w30" | "lr" => Reg::X30,
        "sp" => Reg::SP,
        "xzr" | "wzr" => Reg::Xzr,
        _ => return None,
    })
}

/// Get constraint info for an instruction (aarch64 — mirror of
/// x86_64's `get_constraint_info`).
///
/// AAPCS64 has no hardware constraints comparable to x86_64's
/// idiv/shift (sdiv/udiv don't clobber, shifts use any GP register).
/// The only source of GP clobbers today is inline asm: the parser
/// collected explicit clobbers into `AsmData.clobbers`; named GP
/// registers in that list become hard clobbers at the asm position.
/// Special tokens (`"memory"`, `"cc"`) are filtered out — see x86_64
/// `get_constraint_info` documentation for the rationale.
pub fn get_constraint_info_aarch64(insn: &Instruction) -> Option<(Vec<Reg>, Vec<PseudoId>)> {
    // Inline-asm path (C2 lowering) — also folds in any C5
    // scratch-clobber predicate hit, so inline asm in a dirty
    // opcode position carries both sets of clobbers.
    if insn.op == Opcode::Asm {
        let ic = build_asm_instr_constraints_aarch64(insn)?;
        let (mut clobbers, involved) =
            lower_instr_constraints_to_constraint_point_aarch64(&ic, insn);
        if opcode_clobbers_aarch64_scratches(insn.op) {
            clobbers.extend(AARCH64_SCRATCH_REGS);
            clobbers.sort();
            clobbers.dedup();
        }
        if clobbers.is_empty() {
            return None;
        }
        return Some((clobbers, involved));
    }

    // Non-asm opcodes — declare scratches clobbered for the broad
    // set of opcodes whose codegen helpers touch them. Mirrors the
    // x86_64 `get_constraint_info` shape (C4).
    let needs_scratches = opcode_clobbers_aarch64_scratches(insn.op);
    if !needs_scratches {
        return None;
    }

    let mut involved = Vec::new();
    if let Some(t) = insn.target {
        involved.push(t);
    }
    involved.extend(insn.src.iter().copied());
    let mut clobbers: Vec<Reg> = AARCH64_SCRATCH_REGS.to_vec();
    clobbers.sort();
    clobbers.dedup();

    Some((clobbers, involved))
}

/// AArch64 codegen scratch registers covered by the C5
/// constraint-declaration infrastructure. See the
/// `AARCH64_SCRATCH_FREEING_DEFERRED` block below for why these
/// are not yet in `Reg::allocatable()`.
///
/// `X9` / `X10` / `X11` are the three documented codegen scratches
/// (the `Reg::scratch_regs()` triple). `X16` / `X17` are AAPCS64
/// linker-scratch (IP0/IP1) that the pair-address legalizer (commit
/// `6af088eb`) reuses freely; they're listed for completeness even
/// though their freeing has its own additional codegen
/// dependencies.
const AARCH64_SCRATCH_REGS: &[Reg] = &[Reg::X9, Reg::X10, Reg::X11, Reg::X16, Reg::X17];

/// Conservative predicate — every IR opcode whose codegen helper
/// is more than a single instruction is assumed to touch at least
/// one of the AAPCS64 codegen scratches. Mirror of
/// `opcode_clobbers_r10_r11` in the x86_64 backend; same exclusion
/// list applies because the trivial opcodes lower identically on
/// both architectures.
fn opcode_clobbers_aarch64_scratches(op: Opcode) -> bool {
    !matches!(
        op,
        Opcode::Br
            | Opcode::Nop
            | Opcode::Phi
            | Opcode::PhiSource
            | Opcode::Unreachable
            | Opcode::Fence
            | Opcode::VaEnd
    )
}

// ============================================================================
// C5 status — AArch64 codegen scratch freeing deferred.
// ============================================================================
//
// Same story as x86_64's C4 (see the deferred-freeing block in
// `cc/arch/x86_64/regalloc.rs`). The per-IR-opcode constraint
// declarations above are the future-extension point for freeing
// X9 / X10 / X11 (and eventually X16 / X17), but the codegen
// paths that use these scratches across instruction boundaries
// (function prologue, callee-saved save/restore, the pair-address
// legalizer's X16 materialization, int128 lo/hi shuttle) all
// require either a wholesale codegen refactor or a pre-IR
// scratch-declaration table before adding the registers to
// `Reg::allocatable()` is safe.
//
// V16 / V17 / V18 (FP scratches in `cc/arch/aarch64/float.rs`)
// additionally need V-bank ConstraintPoint plumbing through
// `color_vreg_bank` — same prerequisite the x86_64 XMM14/XMM15
// freeing has on `color_xmm_bank`.
//
// This commit ships the predicate + integration so the eventual
// freeing change is just `Reg::allocatable()` modification.

/// Build the per-operand `InstrConstraints` view of an inline-asm
/// instruction. Mirror of `build_asm_instr_constraints_x86_64`.
pub fn build_asm_instr_constraints_aarch64(
    insn: &Instruction,
) -> Option<crate::arch::asm_constraints::InstrConstraints<Reg>> {
    use crate::arch::asm_constraints::{parse_constraint, InstrConstraints, OperandSpec};

    let asm_data = insn.asm_data.as_ref()?;
    let mut operands = Vec::new();
    for ac in asm_data.outputs.iter().chain(asm_data.inputs.iter()) {
        if let Ok((kind, constraint)) = parse_constraint(&ac.constraint, parse_aarch64_fixed_letter)
        {
            operands.push(OperandSpec {
                pseudo: ac.pseudo,
                kind,
                constraint,
            });
        }
    }
    let mut clobbers: Vec<Reg> = asm_data
        .clobbers
        .iter()
        .filter_map(|name| parse_gp_clobber_name(name))
        .collect();
    clobbers.sort();
    clobbers.dedup();
    let memory_barrier = asm_data.clobbers.iter().any(|c| c == "memory");
    Some(InstrConstraints {
        operands,
        clobbers,
        memory_barrier,
    })
}

/// Walk a function's inline-asm instructions and collect
/// `(operand_pseudo, fixed_reg)` pairs. Mirror of
/// `collect_asm_fixed_precolors_x86_64`. AAPCS64 currently has no
/// Fixed letters in C2 scope, so this function always returns an
/// empty map today — but the plumbing is wired through
/// `color_gp_bank` for symmetry and so that when a future
/// constraint vocabulary expansion brings Fixed letters in scope
/// the allocator pre-colors them without further changes.
pub fn collect_asm_fixed_precolors_aarch64(func: &Function) -> BTreeMap<PseudoId, Reg> {
    let mut out = BTreeMap::new();
    for block in &func.blocks {
        for insn in &block.insns {
            if insn.op != Opcode::Asm {
                continue;
            }
            let Some(ic) = build_asm_instr_constraints_aarch64(insn) else {
                continue;
            };
            for op in &ic.operands {
                if let crate::arch::asm_constraints::OperandConstraint::Fixed(r) = &op.constraint {
                    out.entry(op.pseudo).or_insert(*r);
                }
            }
        }
    }
    out
}

/// Mirror of `lower_instr_constraints_to_constraint_point` for aarch64.
pub fn lower_instr_constraints_to_constraint_point_aarch64(
    ic: &crate::arch::asm_constraints::InstrConstraints<Reg>,
    insn: &Instruction,
) -> (Vec<Reg>, Vec<PseudoId>) {
    use crate::arch::asm_constraints::{OperandConstraint, OperandKind};

    let mut clobbers = ic.clobbers.clone();
    for op in &ic.operands {
        // See `lower_instr_constraints_to_constraint_point` in
        // the x86_64 mirror for the per-variant rationale.
        match &op.constraint {
            OperandConstraint::Fixed(r) => clobbers.push(*r),
            OperandConstraint::Match(_idx) => { /* C3: coalescing edge */ }
            OperandConstraint::Any | OperandConstraint::Mem | OperandConstraint::Imm => {}
        }
        if matches!(op.kind, OperandKind::EarlyClobber) { /* C3: extra interference */ }
    }
    let _ = ic.memory_barrier;
    clobbers.sort();
    clobbers.dedup();

    let mut involved = Vec::new();
    if let Some(t) = insn.target {
        involved.push(t);
    }
    involved.extend(insn.src.iter().copied());
    for op in &ic.operands {
        if !involved.contains(&op.pseudo) {
            involved.push(op.pseudo);
        }
    }

    (clobbers, involved)
}

/// Opcodes whose aarch64 codegen lowering invokes an external function
/// (libc or otherwise) and therefore clobbers caller-saved registers.
/// Used by `find_call_positions` to drive the chordal allocator's
/// cross-call caller-saved forbidding.
///
/// Beyond the obvious `Call` / `Longjmp` / `Setjmp`:
/// - `Fabs32` / `Fabs64` → `fabsf` / `fabs` libc call (features.rs:901+)
/// - `Signbit32` / `Signbit64` → `__signbitf` / target-specific
///   signbit-double libc call (features.rs:853+)
///
/// Unlike x86_64, aarch64's Memset/Memcpy/Memmove are not lowered to
/// libc calls inside features.rs — they reach the regular `Opcode::Call`
/// path, which `is_call_like_aarch64` already covers.
pub fn is_call_like_aarch64(op: Opcode) -> bool {
    matches!(
        op,
        Opcode::Call
            | Opcode::Longjmp
            | Opcode::Setjmp
            | Opcode::Fabs32
            | Opcode::Fabs64
            | Opcode::Signbit32
            | Opcode::Signbit64
    )
}

/// Simple linear scan register allocator for AArch64
pub struct RegAlloc {
    /// Mapping from pseudo to location
    locations: HashMap<PseudoId, Loc>,
    /// Free GP registers (used by argument pre-allocation and the
    /// spill-args helper; the chordal coloring core ignores it).
    free_regs: Vec<Reg>,
    /// Free FP registers (same role as `free_regs` for the V bank).
    free_fp_regs: Vec<VReg>,
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
    /// Active stack slot intervals (interval, offset, size) for reuse tracking
    active_stack: Vec<crate::arch::regalloc::ActiveSlot>,
    /// Free stack slots keyed by size, available for reuse
    free_stack_slots: BTreeMap<i32, Vec<FreeSlot>>,
    /// Sym pseudos whose address is taken (cannot participate in slot reuse)
    addr_taken_syms: HashSet<PseudoId>,
    /// Per-block live-in sets for interference-based stack coloring
    live_in: Vec<HashSet<PseudoId>>,
    /// Per-block live-out sets for interference-based stack coloring
    live_out: Vec<HashSet<PseudoId>>,
    /// Maximum alignment requirement of any local variable (for dynamic stack alignment)
    max_local_align: i32,
}

impl RegAlloc {
    pub fn new() -> Self {
        Self {
            locations: HashMap::new(),
            free_regs: Reg::allocatable().to_vec(),
            free_fp_regs: VReg::allocatable().to_vec(),
            stack_offset: 0,
            used_callee_saved: Vec::new(),
            used_callee_saved_fp: Vec::new(),
            fp_pseudos: HashSet::new(),
            spilled_args: Vec::new(),
            active_stack: Vec::new(),
            free_stack_slots: BTreeMap::new(),
            addr_taken_syms: HashSet::new(),
            live_in: Vec::new(),
            live_out: Vec::new(),
            max_local_align: 8,
        }
    }

    /// Perform register allocation for a function
    pub fn allocate(
        &mut self,
        func: &Function,
        types: &TypeTable,
    ) -> crate::arch::regalloc::LocationMap<Loc> {
        self.reset_state();
        // Use shared identify_fp_pseudos with type-checker closure
        self.fp_pseudos = identify_fp_pseudos(func, |typ| types.is_float(typ));
        self.addr_taken_syms = identify_addr_taken_syms(func);
        self.allocate_arguments(func, types);

        let result = self.compute_live_intervals(func);
        self.live_in = result.live_in;
        self.live_out = result.live_out;
        let intervals = result.intervals;
        let constraint_points = result.constraint_points;
        let call_positions = find_call_positions(func, is_call_like_aarch64);

        self.spill_args_across_calls(func, &intervals, &call_positions);
        self.allocate_alloca_to_stack(func);
        self.run_chordal_color(func, types, intervals, &call_positions, &constraint_points);

        crate::arch::regalloc::LocationMap::from(self.locations.clone())
    }

    /// Reset allocator state for a new function
    fn reset_state(&mut self) {
        self.locations.clear();
        self.free_regs = Reg::allocatable().to_vec();
        self.free_fp_regs = VReg::allocatable().to_vec();
        self.stack_offset = 0;
        self.used_callee_saved.clear();
        self.used_callee_saved_fp.clear();
        self.fp_pseudos.clear();
        self.spilled_args.clear();
        self.active_stack.clear();
        self.free_stack_slots.clear();
        self.addr_taken_syms.clear();
        self.live_in.clear();
        self.live_out.clear();
        self.max_local_align = 8;
    }

    /// Pre-allocate argument registers per AAPCS64
    ///
    /// AAPCS64 passes arguments as follows:
    /// - First 8 integer/pointer args in X0-X7
    /// - First 8 FP args in V0-V7 (D0-D7 for doubles, S0-S7 for floats)
    /// - Remaining args go on the stack in parameter order (not separated by type)
    fn allocate_arguments(&mut self, func: &Function, types: &TypeTable) {
        use crate::abi::{Aapcs64Abi, ArgClass, RegClass};
        use crate::arch::regalloc::AbiLowering;

        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = VReg::arg_regs();
        let mut int_arg_idx = 0usize;
        let mut fp_arg_idx = 0usize;
        // Stack offset for overflow args — shared across all types because
        // AAPCS64 places stack args in parameter order.
        let mut stack_arg_offset = 16i32;

        // Consume the ABI contract through the shared AbiLowering helper.
        // AAPCS64 is simpler than SysV AMD64: no `_Complex`-vs-two-SSE-
        // struct distinction, no x87 long-double special case. The
        // existing backend dispatched only on `is_float` and `Int128`,
        // and we preserve those two specialized arms; everything else
        // goes through the single-GP-register path.
        let abi = Aapcs64Abi::new();
        let lowering = AbiLowering::new(func, types);

        // Allocate X8 for the hidden sret return pointer if present.
        // (Unlike x86_64, X8 is a dedicated indirect-result register, not
        // one of the eight ordinary arg registers, so the GP arg index is
        // NOT incremented here.)
        if let Some(sret_id) = lowering.sret_pseudo {
            self.locations.insert(sret_id, Loc::Reg(Reg::X8));
        }

        for arg in lowering.iter_args(&abi) {
            let pseudo = arg.pseudo;

            match &arg.class {
                // Float / double / Float16 / long double — single V register
                // (or stack slot when V0–V7 are exhausted).
                ArgClass::Direct { classes, .. }
                    if classes.len() == 1 && classes[0] == RegClass::Sse =>
                {
                    if fp_arg_idx < fp_arg_regs.len() {
                        self.locations
                            .insert(pseudo, Loc::VReg(fp_arg_regs[fp_arg_idx]));
                        self.free_fp_regs.retain(|&r| r != fp_arg_regs[fp_arg_idx]);
                        self.fp_pseudos.insert(pseudo);
                    } else {
                        self.locations.insert(pseudo, Loc::Stack(stack_arg_offset));
                        self.fp_pseudos.insert(pseudo);
                        stack_arg_offset += 8;
                    }
                    fp_arg_idx += 1;
                }
                // __int128: two consecutive GP registers when available.
                // The value always lives in a 16-byte aligned local stack
                // slot; the prologue spills the register pair into it.
                _ if arg.is_int128 => {
                    if int_arg_idx + 1 < int_arg_regs.len() {
                        self.stack_offset += 16;
                        let slot = -self.stack_offset;
                        self.locations.insert(pseudo, Loc::Stack(slot));
                        self.free_regs.retain(|&r| {
                            r != int_arg_regs[int_arg_idx] && r != int_arg_regs[int_arg_idx + 1]
                        });
                    } else {
                        // Overflow: 16 bytes on caller stack.
                        self.locations.insert(pseudo, Loc::Stack(stack_arg_offset));
                        stack_arg_offset += 16;
                    }
                    int_arg_idx += 2;
                }
                // Integer / pointer / extension / mixed aggregate /
                // HFA-or-Indirect-falling-through: all default to a
                // single GP register or 8-byte stack slot. This matches
                // the existing (admittedly limited) AAPCS64 handling
                // that does not yet differentiate HFAs or large
                // aggregates from ordinary integers.
                _ => {
                    if int_arg_idx < int_arg_regs.len() {
                        self.locations
                            .insert(pseudo, Loc::Reg(int_arg_regs[int_arg_idx]));
                        self.free_regs.retain(|&r| r != int_arg_regs[int_arg_idx]);
                    } else {
                        self.locations.insert(pseudo, Loc::Stack(stack_arg_offset));
                        stack_arg_offset += 8;
                    }
                    int_arg_idx += 1;
                }
            }
        }
    }

    /// Force alloca results to stack to avoid clobbering issues
    fn allocate_alloca_to_stack(&mut self, func: &Function) {
        crate::arch::regalloc::assign_alloca_slots(
            func,
            &mut self.stack_offset,
            &mut self.locations,
            |off| Loc::Stack(-off),
        );
    }

    /// Spill arguments in caller-saved registers if their interval crosses a call
    fn spill_args_across_calls(
        &mut self,
        _func: &Function,
        intervals: &[LiveInterval],
        call_positions: &[usize],
    ) {
        let int_arg_regs_set: &[Reg] = Reg::arg_regs();
        let spilled_args = &mut self.spilled_args;
        let free_regs = &mut self.free_regs;
        crate::arch::regalloc::spill_gp_args_across_calls(
            intervals,
            call_positions,
            &mut self.locations,
            &mut self.stack_offset,
            |reg| int_arg_regs_set.contains(&reg),
            |loc| {
                if let Loc::Reg(reg) = loc {
                    Some(*reg)
                } else {
                    None
                }
            },
            |off| Loc::Stack(-off),
            |pseudo, from_reg, to_stack_offset| {
                // M4 regression fix: the shared helper passes the raw
                // (positive) stack_offset; aarch64's codegen expects
                // NEGATIVE offsets for spill slots (per the comment at
                // `store_spilled_args`). Negate here to match the
                // `mk_stack_loc` closure above. Without this, prolog
                // stores landed at `frame_size + offset - 16` (the
                // incoming-stack-arg region) rather than the local
                // frame, corrupting the caller's stack.
                spilled_args.push(SpilledArg {
                    pseudo,
                    from_gp_reg: Some(from_reg),
                    from_fp_reg: None,
                    to_stack_offset: -to_stack_offset,
                });
            },
            |reg| free_regs.push(reg),
        );

        // Check FP arguments in caller-saved registers (v0-v7)
        let fp_arg_regs_set = VReg::arg_regs();
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

    /// Try to reuse a freed stack slot of the given size and alignment.
    fn try_reuse_stack_slot(
        &mut self,
        size: i32,
        alignment: i32,
        candidate_interval: &LiveInterval,
    ) -> Option<(i32, Vec<LiveInterval>)> {
        super::super::regalloc::try_reuse_stack_slot(
            &mut self.free_stack_slots,
            size,
            alignment,
            candidate_interval,
        )
    }

    /// Allocate a stack slot, optionally reusing a freed slot.
    /// AArch64 uses negative offsets from the frame pointer.
    /// Only short-lived spills (not crossing calls/loops) should set `reusable=true`.
    fn alloc_stack_slot(
        &mut self,
        interval: &LiveInterval,
        size: i32,
        alignment: i32,
        reusable: bool,
    ) {
        if alignment > self.max_local_align {
            self.max_local_align = alignment;
        }
        if reusable {
            if let Some((reused, past)) = self.try_reuse_stack_slot(size, alignment, interval) {
                self.locations.insert(interval.pseudo, Loc::Stack(reused));
                self.active_stack.push(crate::arch::regalloc::ActiveSlot {
                    current: interval.clone(),
                    past,
                    offset: reused,
                    size,
                });
                return;
            }
        }
        if alignment > 8 {
            self.stack_offset = (self.stack_offset + alignment - 1) & !(alignment - 1);
        }
        self.stack_offset += size;
        let offset = -self.stack_offset;
        self.locations.insert(interval.pseudo, Loc::Stack(offset));
        if reusable {
            self.active_stack.push(crate::arch::regalloc::ActiveSlot {
                current: interval.clone(),
                past: Vec::new(),
                offset,
                size,
            });
        }
    }

    /// M6 chordal coloring + M7 Belady eviction, AArch64 mirror of the
    /// x86_64 `run_chordal_color` in `cc/arch/x86_64/regalloc.rs`.
    ///
    /// Phases:
    ///   1. Pre-pass: route non-register-allocated pseudos to their
    ///      proper Locs (constants → Imm/FImm, Sym → stack slot,
    ///      __int128 → 16-byte stack, multi-reg call returns →
    ///      stack). The remaining pseudos go into per-bank candidate
    ///      sets.
    ///   2. Color: per-bank chordal coloring. ABI-pinned args become
    ///      pre-colored vertices; cross-call → caller-saved becomes a
    ///      per-vertex forbidden set. In-loop pseudos get a
    ///      callee-first preferred palette (soft, not forbidden).
    ///   3. Commit: write Loc::Reg / Loc::VReg for colored vertices,
    ///      allocate stack slots for spilled vertices, track
    ///      `used_callee_saved` for the prologue.
    #[allow(clippy::too_many_arguments)]
    fn run_chordal_color(
        &mut self,
        func: &Function,
        types: &TypeTable,
        intervals: Vec<LiveInterval>,
        call_positions: &[usize],
        constraint_points: &[ConstraintPoint<Reg>],
    ) {
        // -------- Phase 1: pre-pass --------
        let mut gp_candidates: std::collections::BTreeSet<PseudoId> =
            std::collections::BTreeSet::new();
        let mut vreg_candidates: std::collections::BTreeSet<PseudoId> =
            std::collections::BTreeSet::new();
        for interval in &intervals {
            // Intervals come pre-sorted by start position from
            // compute_live_intervals, so this monotonic expiration
            // recovers the linear-scan-era slot-reuse behavior the
            // chordal sweep would otherwise lose. Without this,
            // int128-heavy functions push stp/ldp offsets past
            // aarch64's [-512, 504] immediate range.
            crate::arch::regalloc::expire_stack_intervals(
                &mut self.active_stack,
                &mut self.free_stack_slots,
                interval.start,
            );
            if self.locations.contains_key(&interval.pseudo) {
                // Already assigned by allocate_arguments / spill_args
                // / alloca passes; arg pseudos become pre-colored
                // graph vertices in Phase 2.
                continue;
            }
            if let Some(pseudo) = func.get_pseudo(interval.pseudo) {
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
                            let natural_align = types.alignment(local.typ) as i32;
                            let alignment = local
                                .explicit_align
                                .map(|a| a as i32)
                                .unwrap_or(natural_align.max(8));
                            let aligned_size = (size + alignment - 1) & !(alignment - 1);
                            // Sym slot reuse disabled — see x86_64
                            // mirror for the rationale.
                            let _ = self.addr_taken_syms.contains(&interval.pseudo);
                            let reusable = false;
                            self.alloc_stack_slot(interval, aligned_size, alignment, reusable);
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

            // __int128 → 16-byte stack slot, never in registers.
            let is_int128 = func.blocks.iter().any(|b| {
                b.insns.iter().any(|insn| {
                    insn.typ
                        .is_some_and(|t| types.kind(t) == crate::types::TypeKind::Int128)
                        && (insn.target == Some(interval.pseudo)
                            || insn.src.contains(&interval.pseudo))
                })
            });
            if is_int128 {
                self.alloc_stack_slot(interval, 16, 16, true);
                continue;
            }

            // Multi-register call returns (complex, struct, union) →
            // stack. These span V0+V1 or X0+X1 and can't live in a
            // single allocator vertex.
            let multi_reg_return_typ: Option<TypeId> = func.blocks.iter().find_map(|b| {
                b.insns.iter().find_map(|insn| {
                    if insn.op == Opcode::Call && insn.target == Some(interval.pseudo) {
                        if let Some(typ) = insn.typ {
                            let kind = types.kind(typ);
                            if types.is_complex(typ)
                                || matches!(kind, TypeKind::Struct | TypeKind::Union)
                            {
                                return Some(typ);
                            }
                        }
                        None
                    } else {
                        None
                    }
                })
            });
            if let Some(typ) = multi_reg_return_typ {
                let size = (types.size_bits(typ) / 8) as i32;
                let size = size.max(8);
                let alignment = types.alignment(typ) as i32;
                let aligned_size = (size + (alignment - 1)) & !(alignment - 1);
                self.alloc_stack_slot(interval, aligned_size, alignment, false);
                continue;
            }

            let needs_fp = self.fp_pseudos.contains(&interval.pseudo);
            if needs_fp {
                // FP cross-call/block → stack (avoids the chordal pass
                // having to model V-bank cross-call eviction, which is
                // not implemented yet; matches the x86_64 XMM policy).
                let crosses_call = interval_crosses_call(interval, call_positions);
                let crosses_block = self.live_out.iter().any(|lo| lo.contains(&interval.pseudo));
                if crosses_call || crosses_block {
                    self.alloc_stack_slot(interval, 8, 8, true);
                    continue;
                }
                vreg_candidates.insert(interval.pseudo);
            } else {
                gp_candidates.insert(interval.pseudo);
            }
        }

        // -------- Phase 2: per-bank chordal coloring --------
        self.color_gp_bank(
            func,
            &intervals,
            call_positions,
            constraint_points,
            &gp_candidates,
        );
        self.color_vreg_bank(func, &intervals, &vreg_candidates);
    }

    fn color_gp_bank(
        &mut self,
        func: &Function,
        intervals: &[LiveInterval],
        call_positions: &[usize],
        constraint_points: &[ConstraintPoint<Reg>],
        gp_candidates: &std::collections::BTreeSet<PseudoId>,
    ) {
        use crate::arch::regalloc::{build_interference_graph, greedy_color, mcs_ordering};
        if gp_candidates.is_empty() {
            return;
        }

        // Pre-colored vertices: any pseudo already mapped to a GP reg
        // (ABI-pinned args from allocate_arguments) plus inline-asm
        // operands with `Fixed(R)` constraints. AAPCS64 has no C2-
        // scope Fixed letters today so the asm-precolor call is
        // currently a no-op, but the plumbing is in place for the
        // future-extension point.
        let mut pre_colored: BTreeMap<PseudoId, Reg> = BTreeMap::new();
        let mut all_vertices: std::collections::BTreeSet<PseudoId> = gp_candidates.clone();
        for (&pid, loc) in self.locations.iter() {
            if let Loc::Reg(r) = loc {
                pre_colored.insert(pid, *r);
                all_vertices.insert(pid);
            }
        }
        for (pid, reg) in collect_asm_fixed_precolors_aarch64(func) {
            if !gp_candidates.contains(&pid) {
                continue;
            }
            // See x86_64 mirror for the rationale: commit whatever
            // the allocator's pre_colored map actually holds, not the
            // register we just tried to add. Otherwise an earlier
            // ABI/asm pin would win in `pre_colored` while
            // `self.locations` would point at a different register,
            // splitting coloring's view from codegen's view.
            let committed = *pre_colored.entry(pid).or_insert(reg);
            all_vertices.insert(pid);
            self.locations.insert(pid, Loc::Reg(committed));
        }

        // GP coloring needs def-vs-src edges: aarch64 `csel` for
        // ternary `(cond)?a:b` materializes the target then reads
        // source values, which would clobber a source sharing the
        // target's register. Same pattern as x86_64's cmov.
        let graph = build_interference_graph(&all_vertices, func, &self.live_out, true);

        // Forbidden colors:
        //   (a) constraint clobbers — for pseudos live across the
        //       constraint that are NOT operands. (AAPCS64 has no
        //       implicit clobbers like x86 idiv/shift, so this is
        //       a no-op today, but the plumbing matches x86_64.)
        //   (b) cross-call → all caller-saved (HARD; the call clobbers
        //       them and the value would be lost).
        let caller_saved: Vec<Reg> = Reg::allocatable()
            .iter()
            .copied()
            .filter(|r| !r.is_callee_saved())
            .collect();
        let mut forbidden: BTreeMap<PseudoId, std::collections::BTreeSet<Reg>> = BTreeMap::new();
        for cp in constraint_points {
            for interval in intervals {
                if !gp_candidates.contains(&interval.pseudo) {
                    continue;
                }
                if interval.start > cp.position || cp.position > interval.end {
                    continue;
                }
                if cp.involved_pseudos.contains(&interval.pseudo) {
                    continue;
                }
                let entry = forbidden.entry(interval.pseudo).or_default();
                for &c in &cp.clobbers {
                    entry.insert(c);
                }
            }
        }
        let mut in_loop_set: std::collections::BTreeSet<PseudoId> =
            std::collections::BTreeSet::new();
        for interval in intervals {
            if !gp_candidates.contains(&interval.pseudo) {
                continue;
            }
            if interval_crosses_call(interval, call_positions) {
                let entry = forbidden.entry(interval.pseudo).or_default();
                for &r in &caller_saved {
                    entry.insert(r);
                }
            } else if interval.in_loop {
                in_loop_set.insert(interval.pseudo);
            }
        }

        let caller_first: Vec<Reg> = {
            let mut v = caller_saved.clone();
            for &r in Reg::allocatable() {
                if r.is_callee_saved() {
                    v.push(r);
                }
            }
            v
        };
        let callee_first: Vec<Reg> = {
            let mut v: Vec<Reg> = Reg::allocatable()
                .iter()
                .copied()
                .filter(|r| r.is_callee_saved())
                .collect();
            for &r in &caller_saved {
                v.push(r);
            }
            v
        };
        let caller_first_c = caller_first.clone();
        let callee_first_c = callee_first.clone();

        let order = mcs_ordering(&graph);
        let result = greedy_color(
            &graph,
            &order,
            Reg::allocatable(),
            &pre_colored,
            &forbidden,
            |v| {
                if in_loop_set.contains(&v) {
                    Some(callee_first_c.clone())
                } else {
                    Some(caller_first_c.clone())
                }
            },
        );

        // -------- M7 Belady eviction --------
        let uses = crate::arch::regalloc::compute_use_positions(func);
        let mut colors = result.colors;
        let mut final_spilled: std::collections::BTreeSet<PseudoId> =
            std::collections::BTreeSet::new();
        for spilled in result.spilled {
            if colors.contains_key(&spilled) {
                continue;
            }
            let interval = match Self::interval_by_pseudo(intervals, spilled) {
                Some(i) => i,
                None => {
                    final_spilled.insert(spilled);
                    continue;
                }
            };
            let empty = std::collections::BTreeSet::new();
            let forbid = forbidden.get(&spilled).unwrap_or(&empty);
            let spilled_next =
                crate::arch::regalloc::next_use_distance(&uses, spilled, interval.start);
            let neighbors: Vec<PseudoId> = graph.neighbors(spilled).collect();
            let mut best_evict: Option<(PseudoId, Reg, usize)> = None;
            for &n in &neighbors {
                if pre_colored.contains_key(&n) {
                    continue;
                }
                let Some(&color) = colors.get(&n) else {
                    continue;
                };
                if forbid.contains(&color) {
                    continue;
                }
                let conflict = neighbors
                    .iter()
                    .any(|&m| m != n && colors.get(&m).copied() == Some(color));
                if conflict {
                    continue;
                }
                let nd = crate::arch::regalloc::next_use_distance(&uses, n, interval.start);
                if nd > spilled_next && best_evict.is_none_or(|(_, _, d)| nd > d) {
                    best_evict = Some((n, color, nd));
                }
            }
            if let Some((evicted, color, _)) = best_evict {
                colors.remove(&evicted);
                colors.insert(spilled, color);
                final_spilled.insert(evicted);
            } else {
                final_spilled.insert(spilled);
            }
        }

        // -------- Phase 3: commit --------
        for (&pid, &reg) in &colors {
            if pre_colored.contains_key(&pid) {
                continue;
            }
            self.locations.insert(pid, Loc::Reg(reg));
            if reg.is_callee_saved() && !self.used_callee_saved.contains(&reg) {
                self.used_callee_saved.push(reg);
            }
        }
        // Process spill commits in interval.start order with
        // monotonic expiration. The earlier `usize::MAX` drain
        // relied on `pseudos_interfere`'s block-level liveness which
        // misses within-block interference. See the x86_64 mirror
        // for the full rationale (CPython
        // `PyThread_acquire_lock_timed` miscompile root cause).
        let mut ordered_spilled: Vec<(usize, PseudoId)> = final_spilled
            .iter()
            .filter_map(|&p| Self::interval_by_pseudo(intervals, p).map(|i| (i.start, p)))
            .collect();
        ordered_spilled.sort_by_key(|&(start, _)| start);
        for (start, spilled) in ordered_spilled {
            if self.locations.contains_key(&spilled) {
                continue;
            }
            crate::arch::regalloc::expire_stack_intervals(
                &mut self.active_stack,
                &mut self.free_stack_slots,
                start,
            );
            if let Some(interval) = Self::interval_by_pseudo(intervals, spilled) {
                self.alloc_stack_slot(interval, 8, 8, true);
            }
        }
    }

    fn color_vreg_bank(
        &mut self,
        func: &Function,
        intervals: &[LiveInterval],
        vreg_candidates: &std::collections::BTreeSet<PseudoId>,
    ) {
        use crate::arch::regalloc::{build_interference_graph, greedy_color, mcs_ordering};
        if vreg_candidates.is_empty() {
            return;
        }
        let mut pre_colored: BTreeMap<PseudoId, VReg> = BTreeMap::new();
        let mut all_vertices: std::collections::BTreeSet<PseudoId> = vreg_candidates.clone();
        for (&pid, loc) in self.locations.iter() {
            if let Loc::VReg(r) = loc {
                pre_colored.insert(pid, *r);
                all_vertices.insert(pid);
            }
        }
        // V-bank coloring does NOT need def-vs-src edges: aarch64 FP
        // ops are three-operand at the architectural level (fadd d0,
        // d1, d2), so target and source can share a register without
        // codegen workarounds. Same rationale as x86_64's XMM bank.
        let graph = build_interference_graph(&all_vertices, func, &self.live_out, false);
        let forbidden: BTreeMap<PseudoId, std::collections::BTreeSet<VReg>> = BTreeMap::new();
        let order = mcs_ordering(&graph);
        let result = greedy_color(
            &graph,
            &order,
            VReg::allocatable(),
            &pre_colored,
            &forbidden,
            |_| None::<Vec<VReg>>,
        );
        for (&pid, &reg) in &result.colors {
            if pre_colored.contains_key(&pid) {
                continue;
            }
            self.locations.insert(pid, Loc::VReg(reg));
            if reg.is_callee_saved() && !self.used_callee_saved_fp.contains(&reg) {
                self.used_callee_saved_fp.push(reg);
            }
        }
        // Same monotonic-by-start spill commit as `color_gp_bank` —
        // see the comment there for why the earlier `usize::MAX`
        // drain was unsafe.
        let mut ordered_spilled: Vec<(usize, PseudoId)> = result
            .spilled
            .iter()
            .filter_map(|&p| Self::interval_by_pseudo(intervals, p).map(|i| (i.start, p)))
            .collect();
        ordered_spilled.sort_by_key(|&(start, _)| start);
        for (start, spilled) in ordered_spilled {
            if self.locations.contains_key(&spilled) {
                continue;
            }
            crate::arch::regalloc::expire_stack_intervals(
                &mut self.active_stack,
                &mut self.free_stack_slots,
                start,
            );
            if let Some(interval) = Self::interval_by_pseudo(intervals, spilled) {
                self.alloc_stack_slot(interval, 8, 8, true);
            }
        }
    }

    fn interval_by_pseudo(intervals: &[LiveInterval], p: PseudoId) -> Option<&LiveInterval> {
        intervals.iter().find(|i| i.pseudo == p)
    }

    fn compute_live_intervals(&self, func: &Function) -> LivenessResult<Reg> {
        compute_live_intervals(func, get_constraint_info_aarch64)
    }

    /// Get stack size needed (aligned to max local alignment, minimum 16).
    /// When max_local_align > 16, rounds base to max_align then adds (max_align - 1)
    /// padding so the aligned base register (x19) always finds an aligned start.
    pub fn stack_size(&self) -> i32 {
        if self.max_local_align > 16 {
            let align = self.max_local_align;
            let base = (self.stack_offset + align - 1) & !(align - 1);
            base + align - 1
        } else {
            (self.stack_offset + 15) & !15
        }
    }

    /// Get the maximum alignment requirement of any local variable
    pub fn max_local_align(&self) -> i32 {
        self.max_local_align
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_gp_clobber_name_64bit_canonical() {
        assert_eq!(parse_gp_clobber_name("x0"), Some(Reg::X0));
        assert_eq!(parse_gp_clobber_name("x9"), Some(Reg::X9));
        assert_eq!(parse_gp_clobber_name("x28"), Some(Reg::X28));
    }

    #[test]
    fn parse_gp_clobber_name_w_aliases() {
        // 32-bit `wN` aliases resolve to the underlying XN.
        assert_eq!(parse_gp_clobber_name("w0"), Some(Reg::X0));
        assert_eq!(parse_gp_clobber_name("w9"), Some(Reg::X9));
        assert_eq!(parse_gp_clobber_name("w28"), Some(Reg::X28));
    }

    #[test]
    fn parse_gp_clobber_name_special() {
        // ABI-named registers.
        assert_eq!(parse_gp_clobber_name("fp"), Some(Reg::X29));
        assert_eq!(parse_gp_clobber_name("lr"), Some(Reg::X30));
        assert_eq!(parse_gp_clobber_name("ip0"), Some(Reg::X16));
        assert_eq!(parse_gp_clobber_name("ip1"), Some(Reg::X17));
        assert_eq!(parse_gp_clobber_name("sp"), Some(Reg::SP));
        assert_eq!(parse_gp_clobber_name("xzr"), Some(Reg::Xzr));
        assert_eq!(parse_gp_clobber_name("wzr"), Some(Reg::Xzr));
    }

    #[test]
    fn parse_gp_clobber_name_leading_percent_and_case() {
        assert_eq!(parse_gp_clobber_name("%x0"), Some(Reg::X0));
        assert_eq!(parse_gp_clobber_name("X9"), Some(Reg::X9));
    }

    #[test]
    fn parse_gp_clobber_name_unknown() {
        // Special tokens, V-bank registers, and unknown names return
        // None — the asm clobber walker filters them silently.
        assert_eq!(parse_gp_clobber_name("memory"), None);
        assert_eq!(parse_gp_clobber_name("cc"), None);
        assert_eq!(parse_gp_clobber_name("v0"), None);
        assert_eq!(parse_gp_clobber_name("d0"), None);
        assert_eq!(parse_gp_clobber_name("x18"), None); // platform reserved
        assert_eq!(parse_gp_clobber_name(""), None);
        assert_eq!(parse_gp_clobber_name("not_a_reg"), None);
    }

    #[test]
    fn is_call_like_aarch64_covers_libc_emitters() {
        assert!(is_call_like_aarch64(Opcode::Call));
        assert!(is_call_like_aarch64(Opcode::Longjmp));
        assert!(is_call_like_aarch64(Opcode::Setjmp));
        assert!(is_call_like_aarch64(Opcode::Fabs32));
        assert!(is_call_like_aarch64(Opcode::Fabs64));
        assert!(is_call_like_aarch64(Opcode::Signbit32));
        assert!(is_call_like_aarch64(Opcode::Signbit64));
        // Aarch64 doesn't lower Memset/Memcpy/Memmove inside features.rs
        // — those reach the regular Opcode::Call path instead.
        assert!(!is_call_like_aarch64(Opcode::Memset));
        assert!(!is_call_like_aarch64(Opcode::Memcpy));
        assert!(!is_call_like_aarch64(Opcode::Memmove));
        assert!(!is_call_like_aarch64(Opcode::Add));
        assert!(!is_call_like_aarch64(Opcode::Asm));
    }

    fn make_asm_insn(clobbers: &[&str]) -> Instruction {
        use crate::ir::AsmData;
        let mut insn = Instruction::new(Opcode::Asm);
        insn.asm_data = Some(Box::new(AsmData {
            template: String::new(),
            outputs: Vec::new(),
            inputs: Vec::new(),
            clobbers: clobbers.iter().map(|s| s.to_string()).collect(),
            goto_labels: Vec::new(),
        }));
        insn
    }

    #[test]
    fn build_asm_instr_constraints_aarch64_propagates_memory_barrier() {
        // Mirror of the x86_64 test. `asm volatile("dmb ish" ::: "memory")`
        // is a heavily-used aarch64 idiom for `__sync_synchronize`-style
        // barriers — the `"memory"` clobber must drive
        // `memory_barrier = true` on the lowered constraint.
        let with_mem = make_asm_insn(&["x0", "memory"]);
        let ic = build_asm_instr_constraints_aarch64(&with_mem).expect("has asm_data");
        assert!(ic.memory_barrier, "\"memory\" clobber must set the flag");

        let no_mem = make_asm_insn(&["x0", "cc"]);
        let ic = build_asm_instr_constraints_aarch64(&no_mem).expect("has asm_data");
        assert!(
            !ic.memory_barrier,
            "asm without \"memory\" clobber must not be a barrier"
        );

        let bare = make_asm_insn(&[]);
        let ic = build_asm_instr_constraints_aarch64(&bare).expect("has asm_data");
        assert!(!ic.memory_barrier);
    }
}
