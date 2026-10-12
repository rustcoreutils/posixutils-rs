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
//   X15          - Immediate legalization (see `legalize.rs`). Never carries a
//                  value across an instruction the legalizer could expand;
//                  `legalize.rs` names the two codegen sequences that borrow
//                  it within that rule, and why they must never overlap.
//   X16, X17     - Linker scratch (IP0/IP1 per AAPCS64)
//
// Codegen MUST use only scratch registers (X9, X10, X11, X16, X17) for
// temporaries. Using allocatable registers (X0-X7, X12-X14, X19-X28) risks
// clobbering live values that were assigned by the register allocator.
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

use crate::abi::aapcs64::{StackSlot, StackedArgs};
use crate::arch::asm_constraints::{operand_pins, AsmRegClass, PinnedGp};
use crate::arch::regalloc::{
    asm_clobbered, compute_live_intervals, find_call_positions, identify_fp_pseudos,
    ConstraintPoint, Crossing, LiveInterval, LivenessResult, SlotShape,
};
use crate::float::FloatVal;
use crate::ir::{AsmData, Function, Instruction, Opcode, PseudoId, PseudoKind};
use crate::types::{TypeId, TypeKind, TypeTable};
use crate::units::{Bits, ByteSize};
use std::collections::{BTreeMap, HashMap, HashSet};

// AArch64 Register Definitions

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

    pub fn name_for_size(&self, bits: u32) -> &'static str {
        match bits {
            8 | 16 | 32 => self.name32(),
            _ => self.name64(),
        }
    }

    /// This register's DWARF number, for debug location expressions.
    ///
    /// AAPCS64 numbers `x0`-`x30` as 0-30 and `sp` as 31. Spelled out rather
    /// than taken from the discriminant: **x18 is absent from this enum** --
    /// it is platform-reserved and the compiler never allocates it -- so every
    /// variant after `X17` sits one below its register number. Reading the
    /// discriminant made the frame pointer `x29` come out as 28, and gdb
    /// resolved every local against the wrong register.
    pub fn dwarf_number(&self) -> u16 {
        match self {
            Reg::X0 => 0,
            Reg::X1 => 1,
            Reg::X2 => 2,
            Reg::X3 => 3,
            Reg::X4 => 4,
            Reg::X5 => 5,
            Reg::X6 => 6,
            Reg::X7 => 7,
            Reg::X8 => 8,
            Reg::X9 => 9,
            Reg::X10 => 10,
            Reg::X11 => 11,
            Reg::X12 => 12,
            Reg::X13 => 13,
            Reg::X14 => 14,
            Reg::X15 => 15,
            Reg::X16 => 16,
            Reg::X17 => 17,
            Reg::X19 => 19,
            Reg::X20 => 20,
            Reg::X21 => 21,
            Reg::X22 => 22,
            Reg::X23 => 23,
            Reg::X24 => 24,
            Reg::X25 => 25,
            Reg::X26 => 26,
            Reg::X27 => 27,
            Reg::X28 => 28,
            Reg::X29 => 29,
            Reg::X30 => 30,
            Reg::SP => 31,
            // The zero register has no DWARF number; nothing is ever located
            // relative to it.
            Reg::Xzr => 31,
        }
    }
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
    ///           x15 (immediate legalization), x16/x17 (linker scratch),
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
            // Skip x9, x10, x11 (codegen scratch registers)
            Reg::X12,
            Reg::X13,
            Reg::X14,
            // Skip x15 (immediate legalization)
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

/// The register a function's locals are addressed through.
///
/// A local whose alignment exceeds the 16 bytes SP already guarantees cannot
/// live at a fixed displacement from X29, because X29 itself is only known to
/// be 16-byte aligned. Such a frame rounds the pointer up into a second base
/// register and addresses locals through that instead.
///
/// This is a decision about the whole frame, so it is one value rather than a
/// flag and an alignment that could disagree. It is also settled *before*
/// allocation begins: the base register is only safe if it is withheld from
/// the allocatable pool, and the pool is seeded at the top of `allocate`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FrameBase {
    /// The frame pointer. Every local is satisfied by the stack's own
    /// alignment, so no register is spent.
    Fp,
    /// `reg` holds the frame pointer rounded up to `align`, and every local
    /// is addressed from there. `reg` must not be allocatable.
    Aligned { reg: Reg, align: i32 },
}

impl FrameBase {
    /// Registers this function's inline asm claims for itself: its explicit
    /// clobbers, and the registers its register-variable operands are
    /// pinned to, which the function loads itself.
    /// Withholding the frame base from `allocatable_regs` does not cover them
    /// -- a clobber reaches only the constraint points, which spill
    /// *pseudos*. The frame base is not a pseudo.
    fn asm_claimed_regs(func: &Function) -> std::collections::BTreeSet<Reg> {
        let mut claimed = std::collections::BTreeSet::new();
        for block in &func.blocks {
            for insn in &block.insns {
                if insn.op != Opcode::Asm {
                    continue;
                }
                let Some(ic) = build_asm_instr_constraints_aarch64(insn) else {
                    continue;
                };
                claimed.extend(ic.clobbers.iter().copied());
                claimed.extend(ic.pinned.iter().map(|&(_, r)| r));
            }
        }
        claimed
    }

    /// Decide from the function's declared locals.
    ///
    /// Over-alignment can only originate here: every other frame slot -- a
    /// spill, an argument's, a call's multi-register return -- is aligned to
    /// at most 16. The alignment
    /// expression must stay in step with the one the Sym arm applies when it
    /// lays the slot out, or the frame would be padded for one alignment and
    /// addressed for another.
    fn of(func: &Function, types: &TypeTable, global_registers: &[Reg]) -> FrameBase {
        let align = func
            .locals
            .values()
            .map(|local| {
                local
                    .explicit_align
                    .map(|a| a as i32)
                    .unwrap_or_else(|| crate::abi::slot_align(types.align(local.typ)).max(8))
            })
            .max()
            .unwrap_or(8);
        if align <= 16 {
            return FrameBase::Fp;
        }

        // The base must be callee-saved -- it has to survive the calls this
        // function makes -- and it must be one the function's own inline asm
        // has not named. The prologue writes it once and every local is
        // addressed from it for the rest of the body, so an `asm` that clobbers
        // it invalidates every local at a stroke: hard-coding x19 made
        // `asm("..." ::: "x19")` beside an over-aligned array segfault on code
        // gcc accepts.
        let mut claimed = Self::asm_claimed_regs(func);
        claimed.extend(global_registers.iter().copied());
        const CANDIDATES: [Reg; 10] = [
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
        ];
        match CANDIDATES.iter().find(|r| !claimed.contains(r)) {
            Some(&reg) => FrameBase::Aligned { reg, align },
            None => {
                // Nothing left to hold the aligned base. Refusing is the only
                // honest answer; reusing a claimed register would corrupt every
                // local in the function.
                let pos = func
                    .blocks
                    .iter()
                    .flat_map(|b| b.insns.iter())
                    .find(|i| i.op == Opcode::Asm)
                    .and_then(|i| i.pos)
                    .unwrap_or_default();
                crate::diag::error(
                    pos,
                    "inline asm claims every callee-saved register, leaving none \
                     to address this function's over-aligned locals",
                );
                FrameBase::Fp
            }
        }
    }

    /// The register held back from allocation, if any.
    pub fn reg(self) -> Option<Reg> {
        match self {
            FrameBase::Fp => None,
            FrameBase::Aligned { reg, .. } => Some(reg),
        }
    }

    /// The alignment the locals area is laid out for.
    pub fn align(self) -> i32 {
        match self {
            FrameBase::Fp => 16,
            FrameBase::Aligned { align, .. } => align,
        }
    }
}

// AArch64 Floating-Point Register Definitions

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

// Operand - Location of a value (register or memory)

/// A slot in the **callee's** frame -- a local, a spill slot, or an alloca.
///
/// Held as the negative displacement the backend expects, which
/// [`LocalSlot::from_displacement`] checks. A distinct type, rather than a
/// signed `i32`, keeps the callee's frame from being confused with the
/// caller's.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct LocalSlot(i64);

impl LocalSlot {
    /// No slot yet: a record made before its slot is placed, and completed
    /// when it is.
    const PENDING: LocalSlot = LocalSlot(0);

    /// The slot at `displacement`, below the frame pointer.
    pub fn from_displacement(displacement: i64) -> Self {
        debug_assert!(
            displacement < 0,
            "a local slot lies below the frame pointer"
        );
        LocalSlot(displacement)
    }

    /// The displacement the addressing helpers work in.
    pub fn displacement(self) -> i64 {
        self.0
    }
}

/// Where AAPCS64 places one named parameter.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ParamPlace {
    /// Not passed at all: a zero-sized parameter, which AAPCS64 gives no
    /// class (`ArgClass::Ignore`) and the call site skips.
    Absent,
    /// `count` consecutive general registers from X`first`.
    Gp { first: usize, count: usize },
    /// `count` consecutive V registers from V`first`.
    Fp { first: usize, count: usize },
    /// On the caller's stack at `at`; `fp` when it is a floating-point value
    /// or an HFA.
    Stack { at: IncomingOff, fp: bool },
}

/// Every named parameter's place, and what they consumed in total.
pub struct ParamLayout {
    /// One entry per `Function::params`, in order.
    pub places: Vec<ParamPlace>,
    /// General registers the named parameters used up (NGRN, at most 8).
    pub ngrn: usize,
    /// V registers the named parameters used up (NSRN, at most 8).
    pub nsrn: usize,
    /// Just past the last stacked named parameter.
    pub stack_end: IncomingOff,
}

/// Lay out a function's named parameters by AAPCS64 §6.4.2 stage C, stacking
/// the ones that overflow their registers by `stacked` -- the rule the caller
/// placed them by.
///
/// The one implementation of the rule: `allocate_arguments` binds each
/// parameter's pseudo to its place, and `va_start` reads the totals to know
/// where the variadic arguments begin. It used to be two walks that each
/// classified for themselves, and they disagreed twice -- a composite over
/// sixteen bytes travels as a pointer (one register, or one eight-byte stack
/// slot) where the tally charged its whole size, and a zero-sized parameter
/// takes nothing where the tally charged a register -- so `va_arg` read the
/// wrong slot.
pub fn param_layout(
    params: &[(String, TypeId)],
    types: &TypeTable,
    stacked: StackedArgs,
) -> ParamLayout {
    use crate::abi::{Aapcs64Abi, Abi, ArgClass, RegClass};
    const REGS: usize = 8;
    let abi = Aapcs64Abi::new();
    let mut ngrn = 0usize;
    let mut nsrn = 0usize;
    let mut next = IncomingOff::FIRST;
    let mut places = Vec::with_capacity(params.len());
    for (_, typ) in params {
        let typ = *typ;
        let stack = |next: &mut IncomingOff, fp: bool| ParamPlace::Stack {
            at: IncomingOff::take(next, stacked.slot(types, typ)),
            fp,
        };
        let place = match abi.classify_param(typ, types) {
            // Float / double / _Float16 / long double: one V register.
            ArgClass::Direct { classes, .. }
                if classes.len() == 1 && classes[0] == RegClass::Sse =>
            {
                let place = if nsrn < REGS {
                    ParamPlace::Fp {
                        first: nsrn,
                        count: 1,
                    }
                } else {
                    stack(&mut next, true)
                };
                nsrn += 1;
                place
            }
            // An HFA -- a `_Complex`, or a small homogeneous struct -- in
            // `count` consecutive V registers. Once one is stacked, NSRN
            // becomes 8 and every later floating-point argument follows it
            // there (§6.4.2), unlike System V.
            ArgClass::Hfa { count, .. } => {
                let count = count as usize;
                if nsrn + count <= REGS {
                    let place = ParamPlace::Fp { first: nsrn, count };
                    nsrn += count;
                    place
                } else {
                    nsrn = REGS;
                    stack(&mut next, true)
                }
            }
            // A 128-bit integer, or a composite of at most sixteen bytes that
            // is not an HFA: two consecutive X registers, starting at an even
            // NGRN when the type is 16-aligned (stage C.10). Not fitting sends
            // it, and everything after it, to the stack (C.11).
            class
                if matches!(&class, ArgClass::Direct { classes, .. }
                    if classes.len() == 2
                        && classes.iter().all(|c| *c == RegClass::Integer)) =>
            {
                match crate::abi::aapcs64::gr_run_start(types, typ, ngrn, 2, REGS) {
                    Some(first) => {
                        ngrn = first + 2;
                        ParamPlace::Gp { first, count: 2 }
                    }
                    None => {
                        ngrn = REGS;
                        stack(&mut next, false)
                    }
                }
            }
            ArgClass::Ignore => ParamPlace::Absent,
            // gcc's floating vector of four bytes or fewer: on the stack
            // whatever is left, and NGRN becomes 8 behind it.
            ArgClass::Stacked { .. } => {
                ngrn = REGS;
                stack(&mut next, false)
            }
            // Integer, pointer and everything else -- including a composite
            // over sixteen bytes, which travels as a pointer to a copy (stage
            // C.4) -- takes one X register or one eight-byte slot.
            _ => {
                if ngrn < REGS {
                    ngrn += 1;
                    ParamPlace::Gp {
                        first: ngrn - 1,
                        count: 1,
                    }
                } else {
                    stack(&mut next, false)
                }
            }
        };
        places.push(place);
    }
    ParamLayout {
        places,
        ngrn: ngrn.min(REGS),
        nsrn: nsrn.min(REGS),
        stack_end: next,
    }
}

/// An offset into the **caller's** frame: an argument passed on the stack.
///
/// Starts at 16 -- past the saved frame pointer and return address -- and grows
/// upwards, which is the opposite direction from [`LocalSlot`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct IncomingOff(i64);

impl IncomingOff {
    /// The first incoming stack argument, just above the saved FP and LR.
    pub const FIRST: IncomingOff = IncomingOff(16);

    /// Reserve `slot` for the next stacked argument, and hand back where it
    /// starts.
    ///
    /// The placement is [`StackSlot::place`], the same call the caller lays
    /// its outgoing area out with; this only translates between the argument
    /// area's own offsets and the frame displacement it sits at. The rounding
    /// belongs to the offset within the area -- the NSAA of stage C -- and not
    /// to the displacement: the two differ by the saved FP and LR pair, and
    /// rounding the displacement directly charged an over-aligned argument
    /// for those 16 bytes and started it a whole alignment unit too high.
    ///
    /// Summed in `i64` and saturated: two stacked arguments each inside the
    /// frame ceiling can still overflow their total. The caller checks the
    /// end once, with [`crate::arch::regalloc::check_incoming_area`].
    pub fn take(next: &mut IncomingOff, slot: StackSlot) -> Self {
        let base = IncomingOff::FIRST.0;
        let (at, end) = slot.place(next.0 - base);
        next.0 = base.saturating_add(end);
        IncomingOff(base.saturating_add(at))
    }

    /// The displacement the addressing helpers work in.
    pub fn displacement(self) -> i64 {
        self.0
    }
}

/// Location of a value
#[derive(Debug, Clone, PartialEq)]
pub enum Loc {
    /// In a register
    Reg(Reg),
    /// In a floating-point register
    VReg(VReg),
    /// A slot in the **callee's own** frame: a local, a spill slot or an
    /// alloca, addressed below the frame pointer -- or below the aligned base
    /// when the frame has one.
    ///
    /// Carries a [`LocalSlot`] rather than a bare `i32` so it cannot be
    /// confused with [`Loc::IncomingArg`]: both frame spaces are perfectly
    /// valid `i32`s, and only the type tells them apart.
    Stack(LocalSlot),
    /// An incoming stack argument, in the **caller's** frame: at +16 past the
    /// saved FP and LR, growing upwards.
    IncomingArg(IncomingOff),
    /// Immediate constant
    Imm(i128),
    /// Floating-point immediate constant (value, size in bits)
    FImm(FloatVal, Bits),
    /// Global symbol
    Global(String),
}

// Register Allocator (Linear Scan)

/// A register as the allocation verifier compares them: an X register, or
/// one half of a V register -- a call keeps the low half of V8-V15 only.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum RegUnit {
    X(Reg),
    VLo(VReg),
    VHi(VReg),
}

/// Every X register that can hold a value: all but the stack pointer and
/// the zero register.
const EVERY_X: [Reg; 30] = [
    Reg::X0,
    Reg::X1,
    Reg::X2,
    Reg::X3,
    Reg::X4,
    Reg::X5,
    Reg::X6,
    Reg::X7,
    Reg::X8,
    Reg::X9,
    Reg::X10,
    Reg::X11,
    Reg::X12,
    Reg::X13,
    Reg::X14,
    Reg::X15,
    Reg::X16,
    Reg::X17,
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
    Reg::X29,
    Reg::X30,
];

/// Information about an argument spilled from a caller-saved register to stack
#[derive(Debug, Clone)]
pub struct SpilledArg {
    /// The pseudo that was spilled
    pub pseudo: PseudoId,
    /// The GP register the argument originally arrived in (if integer arg)
    pub from_gp_reg: Option<Reg>,
    /// The FP register the argument originally arrived in (if FP arg)
    pub from_fp_reg: Option<VReg>,
    /// The slot it was spilled to.
    pub to_stack_offset: LocalSlot,
    /// Bytes the value occupies.
    ///
    /// A `long double` is binary128 here, so an FP argument spilled across a
    /// call needs all sixteen bytes, not a fixed eight.
    pub bytes: i32,
}

/// Bytes an argument pseudo's value occupies, from the function's parameter
/// list. Eight when it cannot be identified, which is what every argument but
/// a binary128 one needs anyway.
fn fp_arg_bytes(
    func: &Function,
    lowering: &crate::arch::regalloc::AbiLowering,
    pseudo: PseudoId,
    types: &TypeTable,
) -> i32 {
    func.get_pseudo(pseudo)
        .and_then(|p| match p.kind {
            PseudoKind::Arg(idx) => lowering.param_type(func, idx),
            _ => None,
        })
        .map(|typ| {
            if types.byte_size(typ) > ByteSize::new(8) {
                16
            } else {
                8
            }
        })
        .unwrap_or(8)
}

/// Map a clobber-list register name (lowercase, GCC-style) to the
/// corresponding `Reg`. Accepts the 64-bit canonical name (`x0`, `x29`,
/// ...), the 32-bit alias (`w0`, `w29`, ...), and special names
/// (`sp`, `xzr`, `wzr`, `fp`, `lr`). Returns `None` for names the GP
/// table doesn't know about (V registers, `memory`, `cc`, ...).
pub(crate) fn parse_gp_clobber_name(raw: &str) -> Option<Reg> {
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

/// The SIMD/FP register a clobber-list name means -- `v8`, `q8`, `d8`,
/// `s8`, `h8` and `b8` all write register 8, optionally with a leading `%` --
/// among those the allocator hands out. `None` for any other name, and for
/// the codegen scratch registers, which hold nothing across an instruction.
fn parse_fp_clobber_name(raw: &str) -> Option<VReg> {
    let s = raw.trim_start_matches('%').to_ascii_lowercase();
    let n: u8 = s
        .strip_prefix(['v', 'q', 'd', 's', 'h', 'b'])?
        .parse()
        .ok()?;
    VReg::allocatable()
        .iter()
        .copied()
        .find(|v| v.name_d().strip_prefix('d') == Some(n.to_string().as_str()))
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
/// `tls` is how this target obtains a thread-local's address, which decides
/// what a `TlsAddr` clobbers.
pub fn get_constraint_info_aarch64(
    insn: &Instruction,
    tls: crate::target::TlsAccess,
) -> Option<(Vec<Reg>, Vec<PseudoId>)> {
    // Inline-asm path — also folds in any scratch-clobber predicate
    // hit, so inline asm in a dirty opcode position carries both sets
    // of clobbers.
    if insn.op == Opcode::Asm {
        let ic = build_asm_instr_constraints_aarch64(insn)?;
        let (mut clobbers, involved) = ic.to_constraint_point();
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

    if insn.returns_twice() {
        let mut clobbers = Reg::allocatable().to_vec();
        clobbers.extend(AARCH64_SCRATCH_REGS);
        return Some(crate::arch::regalloc::returns_twice_constraint(
            insn, &clobbers,
        ));
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
    // The atomic emitters keep their source values live across the writes to
    // their fixed scratch registers, so unlike the other opcodes here their
    // sources are not exempt from the clobber set.
    if !insn.op.is_atomic() {
        involved.extend(insn.src.iter().copied());
    }
    let mut clobbers: Vec<Reg> = AARCH64_SCRATCH_REGS.to_vec();
    // The atomic emitters additionally use X0/X1/X2 (and X8 for the store-
    // exclusive status) as fixed scratch. Those *are* in Reg::allocatable(),
    // so without declaring them a live pseudo the allocator placed there is
    // destroyed by any atomic operation its range crosses.
    if insn.op.is_atomic() {
        clobbers.extend([Reg::X0, Reg::X1, Reg::X2, Reg::X8]);
    }

    // The TLS descriptor sequence is fixed to x0 (the descriptor address, and
    // the offset the resolver returns) and uses x1 for the resolver entry
    // point. Nothing else is clobbered -- the resolver preserves every other
    // register -- which is why this is a plain clobber and deliberately *not*
    // an entry in `is_call_like_aarch64`. Declaring it call-like would spill
    // every live floating-point value for a sequence that needs none of it.
    //
    // The Mach-O TLV getter takes its descriptor and returns the address in
    // x0, and preserves x1-x28 and q0-q31 (LLVM's `CSR_Darwin_AArch64_TLS`);
    // it also clobbers x16, x17 and the link register, none of which is ever
    // allocated. So on Darwin x0 is the whole of it.
    if insn.op == Opcode::TlsAddr {
        match tls {
            crate::target::TlsAccess::MachOTlv => clobbers.push(Reg::X0),
            _ => clobbers.extend([Reg::X0, Reg::X1]),
        }
    }
    clobbers.sort();
    clobbers.dedup();

    Some((clobbers, involved))
}

/// Whether a constraint point may leave `p` in a register it clobbers: only
/// when `p` is one of the point's own operands, live across it or not. The
/// one rule for aarch64, asked both when coloring and when deciding whether
/// an ABI-pinned argument must leave its register.
fn exempt_from_clobber(cp: &ConstraintPoint<Reg>, p: PseudoId, _across: bool) -> bool {
    cp.involved_pseudos.contains(&p)
}

/// What crosses each call and each constraint point that matters, by exact
/// liveness ([`crate::arch::regalloc::crossings`]): the one answer every
/// clobber question of this allocator reads.
struct Clobbering {
    crossings: HashMap<usize, Crossing>,
    /// The calls' positions.
    calls: Vec<usize>,
    /// The calls that return twice ([`Instruction::returns_twice`]): control
    /// comes back to them with no register restored but the frame's.
    returns_twice: HashSet<usize>,
    /// Each inline asm that clobbers a V register, with the ones it names.
    fp_asms: Vec<(usize, Vec<VReg>)>,
    /// The constraint points that clobber a register a value may be in, or
    /// an argument arrives in: the others clobber only codegen scratch.
    points: Vec<ConstraintPoint<Reg>>,
}

impl Clobbering {
    fn of(
        func: &Function,
        live_out: &[HashSet<PseudoId>],
        calls: &[usize],
        constraint_points: &[ConstraintPoint<Reg>],
        allocatable: &[Reg],
    ) -> Self {
        let points: Vec<ConstraintPoint<Reg>> = constraint_points
            .iter()
            .filter(|cp| {
                cp.clobbers.iter().any(|r| {
                    allocatable.contains(r) || Reg::arg_regs().contains(r) || *r == Reg::X8
                })
            })
            .cloned()
            .collect();
        let insns: Vec<&Instruction> = func.blocks.iter().flat_map(|b| &b.insns).collect();
        let returns_twice = calls
            .iter()
            .copied()
            .filter(|&pos| insns[pos].returns_twice())
            .collect();
        let fp_asms: Vec<(usize, Vec<VReg>)> = insns
            .iter()
            .enumerate()
            .map(|(pos, insn)| (pos, asm_clobbered(insn, parse_fp_clobber_name)))
            .filter(|(_, regs)| !regs.is_empty())
            .collect();
        let mut positions: std::collections::BTreeSet<usize> = calls.iter().copied().collect();
        positions.extend(fp_asms.iter().map(|&(pos, _)| pos));
        positions.extend(points.iter().map(|cp| cp.position));
        Self {
            crossings: crate::arch::regalloc::crossings(func, live_out, &positions),
            calls: calls.to_vec(),
            returns_twice,
            fp_asms,
            points,
        }
    }

    /// Every pseudo some call touches: live across it, or an argument.
    fn gp_call_touched(&self) -> HashSet<PseudoId> {
        self.calls
            .iter()
            .flat_map(|pos| self.crossings[pos].call_crossers())
            .collect()
    }

    /// The V registers each of `candidates` may not take.
    ///
    /// Where it is live across a call, every caller-saved one: AAPCS64
    /// preserves V8-V15 only. And only their low 64 bits (§6.1.2), so a value
    /// wider than that -- not in `narrow` -- may take no V register at all;
    /// nor may anything a call that returns twice touches, which comes back
    /// with none of them restored
    /// ([`crate::arch::regalloc::returns_twice_constraint`]).
    ///
    /// Where it is only an argument of the call, V0-V7: the call's lowering
    /// moves its arguments into them one by one, and would overwrite one
    /// waiting in a later argument's register. Any other register keeps it
    /// until it is read, and it is dead after.
    ///
    /// Where it crosses an inline asm ([`Crossing::bank_crossers`]), the V
    /// registers the asm's clobber list names.
    fn vreg_forbidden(
        &self,
        candidates: &std::collections::BTreeSet<PseudoId>,
        narrow: &HashSet<PseudoId>,
    ) -> BTreeMap<PseudoId, std::collections::BTreeSet<VReg>> {
        let every: &[VReg] = VReg::allocatable();
        let caller_saved: Vec<VReg> = every
            .iter()
            .copied()
            .filter(|v| !v.is_callee_saved())
            .collect();
        let mut forbidden: BTreeMap<PseudoId, std::collections::BTreeSet<VReg>> = BTreeMap::new();
        for pos in &self.calls {
            let crossing = &self.crossings[pos];
            let resumes_bare = self.returns_twice.contains(pos);
            let across = crossing.across.iter().map(|&p| (p, true));
            let args = crossing.uses.iter().map(|&p| (p, false));
            for (p, live_across) in across.chain(args) {
                if !candidates.contains(&p) {
                    continue;
                }
                let regs: &[VReg] = if resumes_bare || (live_across && !narrow.contains(&p)) {
                    every
                } else if live_across {
                    &caller_saved
                } else {
                    VReg::arg_regs()
                };
                forbidden.entry(p).or_default().extend(regs);
            }
        }
        for (pos, regs) in &self.fp_asms {
            for p in self.crossings[pos].bank_crossers() {
                if candidates.contains(&p) {
                    forbidden.entry(p).or_default().extend(regs);
                }
            }
        }
        forbidden
    }
}

/// The floating-point and vector values known to fit the low 64 bits of a V
/// register: by their first def's width, an asm output's, or a parameter's
/// type. Any other may be wider -- a binary128 `long double`, a 128-bit
/// vector -- and loses its high half in V8-V15 across a call.
fn narrow_v_values(func: &Function, types: &TypeTable) -> HashSet<PseudoId> {
    let mut narrow: HashSet<PseudoId> = HashSet::new();
    let mut seen: HashSet<PseudoId> = HashSet::new();
    for insn in func.blocks.iter().flat_map(|b| &b.insns) {
        if let Some(t) = insn.target {
            if seen.insert(t) && insn.size > Bits::new(0) && insn.size <= Bits::new(64) {
                narrow.insert(t);
            }
        }
        if let Some(asm) = insn.extra().asm_data.as_deref() {
            for out in asm.outputs.iter().filter(|o| !o.is_memory()) {
                if seen.insert(out.pseudo) && out.size > Bits::new(0) && out.size <= Bits::new(64) {
                    narrow.insert(out.pseudo);
                }
            }
        }
    }
    for (p, t) in crate::arch::regalloc::arg_pseudo_types(func) {
        if types.byte_size(t) <= ByteSize::new(8) {
            narrow.insert(p);
        }
    }
    narrow
}

/// Bytes a spilled floating-point pseudo needs on the stack.
///
/// Every FP spill used a hardcoded 8. That is right for a float or a double and
/// wrong for IEEE binary128, which is 16 bytes and 16-byte aligned: the two
/// halves of a `long double _Complex` were handed slots 8 bytes apart and then
/// written with `str q`, so they overlapped, and the resulting offsets were not
/// multiples of 16 either -- which the assembler rejects outright for `ldr q`
/// ("immediate offset out of range").
///
/// The defining instruction carries the width, exactly as the `FVal` case above
/// reads it for an immediate.
fn fp_pseudo_bytes(func: &Function, pseudo: PseudoId) -> i32 {
    let bits = func
        .blocks
        .iter()
        .flat_map(|b| &b.insns)
        .find(|insn| insn.target == Some(pseudo))
        .map(|insn| insn.size)
        .unwrap_or(Bits::new(64));
    if bits > Bits::new(64) {
        16
    } else {
        8
    }
}

/// AArch64 codegen scratch registers covered by the C5
/// constraint-declaration infrastructure. See the
/// `AARCH64_SCRATCH_FREEING_DEFERRED` block below for why these
/// are not yet in `Reg::allocatable()`.
///
/// `X9` / `X10` / `X11` are the three documented codegen scratches
/// (the `Reg::scratch_regs()` triple). `X16` / `X17` are AAPCS64
/// linker-scratch (IP0/IP1) that codegen uses freely; they're listed for
/// completeness even though their freeing has its own additional codegen
/// dependencies. `X15` is not here: it holds nothing across an instruction
/// boundary, so no constraint point needs to know about it.
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
            | Opcode::Trap
            | Opcode::Fence
            | Opcode::VaEnd
    )
}

/// The register each operand of an inline-asm statement is pinned to --
/// outputs, then inputs, in order -- or `None` for one the allocator places.
///
/// No aarch64 constraint letter names a register; a register variable
/// (`register long x asm("x8")`) does, and a tied input takes its output's.
pub fn asm_pinned_regs_aarch64(asm: &AsmData) -> Vec<Option<Reg>> {
    operand_pins(asm, |c| match c.class.reg {
        Some(AsmRegClass::Pinned(PinnedGp::Aarch64(r))) => Some(r),
        _ => None,
    })
}

/// The allocator's view of an inline-asm instruction: its pinned operands
/// and its clobbers. `None` if the instruction has no `AsmData`.
pub fn build_asm_instr_constraints_aarch64(
    insn: &Instruction,
) -> Option<crate::arch::asm_constraints::InstrConstraints<Reg>> {
    let asm = insn.extra().asm_data.as_ref()?;
    Some(crate::arch::asm_constraints::InstrConstraints::of_asm(
        asm,
        &asm_pinned_regs_aarch64(asm),
        parse_gp_clobber_name,
    ))
}

/// Opcodes whose aarch64 codegen lowering invokes an external function
/// (libc or otherwise) and therefore clobbers caller-saved registers.
/// Used by `find_call_positions` to drive the chordal allocator's
/// cross-call caller-saved forbidding.
///
/// Beyond the obvious `Call` / `Longjmp` / `Setjmp`, `Memset`/`Memcpy`/
/// `Memmove` are here: they lower to a `bl` to the libc function of the same
/// name. The comment that used to sit
/// here said they reached the regular `Opcode::Call` path instead — nothing
/// performed that conversion, so they reached codegen's `_ => {}` arm and
/// emitted nothing at all.
pub fn is_call_like_aarch64(op: Opcode) -> bool {
    matches!(
        op,
        Opcode::Call
            | Opcode::Longjmp
            | Opcode::Setjmp
            | Opcode::Memcpy
            | Opcode::Memmove
            | Opcode::Memset
    )
}

/// Simple linear scan register allocator for AArch64
pub struct RegAlloc {
    /// Mapping from pseudo to location
    locations: HashMap<PseudoId, Loc>,
    /// How the target obtains a thread-local's address, which decides what a
    /// `TlsAddr` clobbers. Set with [`RegAlloc::with_tls_access`].
    tls_access: crate::target::TlsAccess,
    /// Whether the function gets a stack-protector canary; set with
    /// [`RegAlloc::with_stack_guard`].
    guard_requested: bool,
    /// The canary's slot: the frame's first, so above every other local.
    guard_slot: Option<LocalSlot>,
    /// Registers the unit's file-scope register variables are: never
    /// allocated, saved or restored in any of its functions. Set with
    /// `with_global_registers`.
    global_registers: Vec<Reg>,
    /// Next stack slot offset
    stack_offset: i64,
    /// A source position for this function, for the frame diagnostics
    /// `grow_frame` and [`crate::abi::slot_bytes`] emit. `new_frame_slot` and
    /// `IncomingOff::take` have no `&Function` to recover one from.
    func_pos: crate::diag::Position,
    /// Callee-saved registers that were used
    used_callee_saved: Vec<Reg>,
    /// Callee-saved FP registers that were used
    used_callee_saved_fp: Vec<VReg>,
    /// Set of pseudos that need FP registers (determined by analyzing the IR)
    fp_pseudos: HashSet<PseudoId>,
    /// The values that fit the low half of a V register
    /// ([`narrow_v_values`]): the only ones V8-V15 keep across a call.
    narrow_v: HashSet<PseudoId>,
    /// Arguments spilled from caller-saved registers to stack
    spilled_args: Vec<SpilledArg>,
    /// The pseudos that live in a frame slot, with the slot each needs:
    /// spills, arguments moved out of their registers, 128-bit values, a
    /// call's multi-register return, alloca pointers. Their offsets are
    /// decided together, once both banks are colored
    /// ([`crate::arch::regalloc::color_stack_slots`]); a spilled argument's
    /// record is completed then.
    stack_homed: BTreeMap<PseudoId, SlotShape>,
    /// Per-block live-in sets for interference-based stack coloring
    live_in: Vec<HashSet<PseudoId>>,
    /// Per-block live-out sets for interference-based stack coloring
    live_out: Vec<HashSet<PseudoId>>,
    /// Maximum alignment requirement of any local variable (for dynamic stack alignment)
    /// Which register locals are addressed through, decided before allocation.
    frame_base: FrameBase,
    /// How the target stacks named parameters that overflow their registers.
    stacked_args: StackedArgs,
    /// The size of every frame slot handed to a pseudo; see
    /// [`crate::arch::regalloc::SlotBytes`].
    slot_bytes: crate::arch::regalloc::SlotBytes,
    /// An argument arriving in more than one register -- an HFA, a
    /// two-register composite -- by how many. Its location names the first.
    arg_span: HashMap<PseudoId, usize>,
}

impl RegAlloc {
    /// The allocator for a target whose thread-locals are reached by
    /// `access`; see [`crate::target::Target::tls_access`].
    pub fn with_tls_access(mut self, access: crate::target::TlsAccess) -> Self {
        self.tls_access = access;
        self
    }

    /// The allocator for a unit whose file-scope register variables are
    /// `regs` (`register long r asm("r14")`): each is the variable for the
    /// whole unit, so no function may allocate it, use it for its frame
    /// base, or save and restore it -- that would undo a write to the
    /// variable on return.
    pub fn with_global_registers(mut self, regs: &[Reg]) -> Self {
        self.global_registers = regs.to_vec();
        self
    }

    /// An allocator for a target that stacks overflowing named parameters
    /// by `stacked_args` -- [`StackedArgs::of`] the target.
    pub fn new(stacked_args: StackedArgs) -> Self {
        Self {
            stacked_args,
            locations: HashMap::new(),
            stack_offset: 0,
            func_pos: crate::diag::Position::default(),
            used_callee_saved: Vec::new(),
            used_callee_saved_fp: Vec::new(),
            fp_pseudos: HashSet::new(),
            narrow_v: HashSet::new(),
            spilled_args: Vec::new(),
            stack_homed: BTreeMap::new(),
            live_in: Vec::new(),
            live_out: Vec::new(),
            frame_base: FrameBase::Fp,
            tls_access: crate::target::TlsAccess::ElfStatic,
            guard_requested: false,
            guard_slot: None,
            global_registers: Vec::new(),
            slot_bytes: HashMap::new(),
            arg_span: HashMap::new(),
        }
    }

    /// An allocator that reserves a stack-protector canary slot when
    /// `guarded`; see `arch::stack_protect`.
    pub fn with_stack_guard(mut self, guarded: bool) -> Self {
        self.guard_requested = guarded;
        self
    }

    /// The canary's slot, if [`RegAlloc::with_stack_guard`] asked for one.
    pub fn stack_guard_slot(&self) -> Option<LocalSlot> {
        self.guard_slot
    }

    /// Perform register allocation for a function
    pub fn allocate(
        &mut self,
        func: &Function,
        types: &TypeTable,
    ) -> crate::arch::regalloc::LocationMap<Loc> {
        self.reset_state();
        self.func_pos = crate::arch::func_pos(func);
        // Before the pool is seeded with anything: an over-aligned frame
        // claims a register for its base, and the prologue overwrites it
        // unconditionally, so it must never be handed to a pseudo.
        self.frame_base = FrameBase::of(func, types, &self.global_registers);
        let allocatable = self.allocatable_regs();
        // Before any other slot: the first is the highest, just under the
        // variadic save area and the saved registers, which is where the
        // canary has to be -- and the arrays right under it.
        self.guard_slot = self
            .guard_requested
            .then(|| LocalSlot::from_displacement(self.new_frame_slot(8, 8)));
        let (arrays, rest) = crate::arch::regalloc::LocalSet::of(self.guard_requested);
        if let Some(arrays) = arrays {
            let early = self.compute_live_intervals(func);
            self.place_locals(func, types, &early.intervals, arrays);
        }
        if func.receives_nonlocal_goto() {
            self.used_callee_saved
                .extend(allocatable.iter().filter(|r| r.is_callee_saved()));
            self.used_callee_saved_fp
                .extend(VReg::allocatable().iter().filter(|r| r.is_callee_saved()));
        }
        // A callee-saved register an inline asm clobbers is the function's to
        // preserve, as any it allocates is, though no pseudo lives in it --
        // but not a global register, which is the variable's.
        for reg in FrameBase::asm_claimed_regs(func) {
            if reg.is_callee_saved()
                && allocatable.contains(&reg)
                && !self.used_callee_saved.contains(&reg)
            {
                self.used_callee_saved.push(reg);
            }
        }
        let fp_clobbers = func
            .blocks
            .iter()
            .flat_map(|block| &block.insns)
            .flat_map(|insn| asm_clobbered(insn, parse_fp_clobber_name));
        for reg in fp_clobbers {
            if reg.is_callee_saved() && !self.used_callee_saved_fp.contains(&reg) {
                self.used_callee_saved_fp.push(reg);
            }
        }
        // Use shared identify_fp_pseudos with type-checker closure
        self.fp_pseudos = identify_fp_pseudos(func, |typ| types.is_float(typ));
        // An asm operand in a `"w"` register is defined only by the asm,
        // which carries no type: without this it looked like an integer and
        // got an X register, which a sixteen-byte vector does not fit.
        for insn in func.blocks.iter().flat_map(|b| &b.insns) {
            if let Some(asm) = insn.extra().asm_data.as_ref() {
                self.fp_pseudos
                    .extend(super::inline_asm::vector_operand_pseudos(asm));
            }
        }
        self.allocate_arguments(func, types);

        let result = self.compute_live_intervals(func);
        self.live_in = result.live_in;
        self.live_out = result.live_out;
        let intervals = result.intervals;
        let constraint_points = result.constraint_points;
        let call_positions = find_call_positions(func, is_call_like_aarch64);
        let clobbering = Clobbering::of(
            func,
            &self.live_out,
            &call_positions,
            &constraint_points,
            &self.allocatable_regs(),
        );
        self.narrow_v = narrow_v_values(func, types);

        self.spill_args_across_calls(func, types, &intervals, &clobbering);
        self.allocate_alloca_to_stack(func);
        self.place_locals(func, types, &intervals, rest);
        self.run_chordal_color(func, types, intervals, &clobbering);
        self.place_stack_slots(func);
        // In register order, so the prologue saves them in pairs as gcc
        // does: `stp d8, d9`.
        self.used_callee_saved_fp.sort();
        if cfg!(debug_assertions) {
            self.verify(func, types, &constraint_points, &call_positions);
        }

        crate::arch::regalloc::LocationMap::from(self.locations.clone())
    }

    /// Reset allocator state for a new function
    fn reset_state(&mut self) {
        self.locations.clear();
        self.stack_offset = 0;
        self.used_callee_saved.clear();
        self.used_callee_saved_fp.clear();
        self.fp_pseudos.clear();
        self.narrow_v.clear();
        self.spilled_args.clear();
        self.stack_homed.clear();
        self.live_in.clear();
        self.live_out.clear();
        self.frame_base = FrameBase::Fp;
        self.slot_bytes.clear();
        self.arg_span.clear();
    }

    /// Pre-allocate argument registers per AAPCS64.
    ///
    /// Where each parameter arrives is [`param_layout`]'s answer; this only
    /// binds the parameters' pseudos to it. `va_start` reads the same layout's
    /// totals, so the two cannot disagree about where the variadic arguments
    /// begin.
    fn allocate_arguments(&mut self, func: &Function, types: &TypeTable) {
        use crate::arch::regalloc::AbiLowering;

        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = VReg::arg_regs();
        let lowering = AbiLowering::new(func);

        // Allocate X8 for the hidden sret return pointer if present.
        // (Unlike x86_64, X8 is a dedicated indirect-result register, not
        // one of the eight ordinary arg registers, so the GP arg index is
        // NOT incremented here.)
        if let Some(sret_id) = lowering.sret_pseudo {
            self.locations.insert(sret_id, Loc::Reg(Reg::X8));
        }

        let layout = param_layout(&func.params, types, self.stacked_args);
        for (i, place) in layout.places.iter().enumerate() {
            let arg_idx = i + lowering.arg_idx_offset as usize;
            let Some(pseudo) = lowering.arg_pseudos.get(arg_idx).copied().flatten() else {
                continue;
            };
            let typ = func.params[i].1;
            match *place {
                ParamPlace::Absent => {}
                ParamPlace::Fp { first, count } => {
                    self.locations.insert(pseudo, Loc::VReg(fp_arg_regs[first]));
                    self.arg_span.insert(pseudo, count);
                    self.fp_pseudos.insert(pseudo);
                }
                // __int128 arrives in an even-aligned register pair, but
                // always lives in a 16-byte local slot; the prologue spills
                // the pair into it.
                ParamPlace::Gp { .. } if types.is_plain_int128(typ) => {
                    self.home_on_stack(pseudo, SlotShape::new(16, 16));
                }
                // A scalar, or a composite of at most sixteen bytes in
                // consecutive X registers: the prologue writes those into the
                // parameter's local, so what matters here is the reservation.
                ParamPlace::Gp { first, count } => {
                    self.locations.insert(pseudo, Loc::Reg(int_arg_regs[first]));
                    self.arg_span.insert(pseudo, count);
                }
                ParamPlace::Stack { at, fp } => {
                    self.locations.insert(pseudo, Loc::IncomingArg(at));
                    if fp {
                        self.fp_pseudos.insert(pseudo);
                    }
                }
            }
        }
        crate::arch::regalloc::check_incoming_area(layout.stack_end.displacement(), self.func_pos);
    }

    /// Alloca results live in the frame.
    fn allocate_alloca_to_stack(&mut self, func: &Function) {
        for p in crate::arch::regalloc::alloca_results(func) {
            self.home_on_stack(p, SlotShape::new(8, 8));
        }
    }

    /// Move an argument out of the register it arrived in when that register
    /// is overwritten while the argument is live: a general register by the
    /// rule every value is colored by ([`RegAlloc::gp_forbidden`]) -- a call
    /// it crosses, or a constraint point that clobbers it -- and a V register
    /// by [`Clobbering::vreg_forbidden`]: a call it crosses, or an asm that
    /// clobbers it.
    fn spill_args_across_calls(
        &mut self,
        func: &Function,
        types: &TypeTable,
        intervals: &[LiveInterval],
        clobbering: &Clobbering,
    ) {
        let args: std::collections::BTreeSet<PseudoId> = self
            .locations
            .iter()
            .filter(|(_, loc)| {
                matches!(loc, Loc::Reg(r) if Reg::arg_regs().contains(r) || *r == Reg::X8)
            })
            .map(|(&p, _)| p)
            .collect();
        let forbidden = self.gp_forbidden(clobbering, &args);
        self.spill_gp_args(intervals, |p, reg| {
            forbidden.get(&p).is_some_and(|regs| regs.contains(&reg))
        });

        // A V argument whose register is overwritten while it is live: a
        // scalar one is colored afresh with the other V values -- into V8-V15
        // across a call, or a slot -- and the prologue moves it there
        // (`store_args_to_stack`); one arriving in several registers, an HFA
        // or a complex, is stored to a slot.
        let fp_arg_regs_set = VReg::arg_regs();
        let lowering = crate::arch::regalloc::AbiLowering::new(func);
        let v_args: std::collections::BTreeSet<PseudoId> = self
            .locations
            .iter()
            .filter(|(_, loc)| matches!(loc, Loc::VReg(r) if fp_arg_regs_set.contains(r)))
            .map(|(&p, _)| p)
            .collect();
        let forbidden = clobbering.vreg_forbidden(&v_args, &self.narrow_v);
        for interval in intervals {
            if let Some(Loc::VReg(reg)) = self.locations.get(&interval.pseudo) {
                let overwritten = forbidden
                    .get(&interval.pseudo)
                    .is_some_and(|regs| regs.contains(reg));
                if fp_arg_regs_set.contains(reg) && overwritten {
                    if self.arg_span.get(&interval.pseudo).copied().unwrap_or(1) == 1 {
                        self.locations.remove(&interval.pseudo);
                        continue;
                    }
                    let from_reg = *reg;
                    // Reserve what the value actually needs: a binary128
                    // argument is sixteen bytes, and eight left half of it
                    // overlapping whatever came next.
                    let bytes = fp_arg_bytes(func, &lowering, interval.pseudo, types);
                    // Record the spill for codegen to emit stores in prologue;
                    // the slot is filled in once it is placed.
                    self.spilled_args.push(SpilledArg {
                        pseudo: interval.pseudo,
                        from_gp_reg: None,
                        from_fp_reg: Some(from_reg),
                        to_stack_offset: LocalSlot::PENDING,
                        bytes,
                    });
                    self.home_on_stack(interval.pseudo, SlotShape::new(bytes, bytes));
                }
            }
        }
    }

    /// Spill a GP argument -- including the sret pointer in X8 -- out of its
    /// ABI register when `overwritten` says something destroys that register
    /// while the argument is live. See
    /// [`crate::arch::regalloc::gp_args_overwritten`].
    ///
    /// X8 carries the sret pointer, which a call overwrites -- the callee may
    /// use X8 freely, and a call that itself returns a large aggregate loads
    /// X8 with *its* buffer. Left out of this set, a function returning a
    /// large struct that called anything stored its result through the last
    /// callee's buffer: the caller received zeros. An atomic's fixed scratch
    /// clobbers X8 too.
    fn spill_gp_args(
        &mut self,
        intervals: &[LiveInterval],
        overwritten: impl Fn(PseudoId, Reg) -> bool,
    ) {
        let spills = crate::arch::regalloc::gp_args_overwritten(
            intervals,
            |p| match self.locations.get(&p) {
                Some(Loc::Reg(r)) if Reg::arg_regs().contains(r) || *r == Reg::X8 => Some(*r),
                _ => None,
            },
            overwritten,
        );
        for (pseudo, from_reg) in spills {
            // The slot is filled in once it is placed.
            self.spilled_args.push(SpilledArg {
                pseudo,
                from_gp_reg: Some(from_reg),
                from_fp_reg: None,
                to_stack_offset: LocalSlot::PENDING,
                bytes: 8,
            });
            self.home_on_stack(pseudo, SlotShape::new(8, 8));
        }
    }

    /// Get arguments that were spilled from caller-saved registers
    pub fn spilled_args(&self) -> &[SpilledArg] {
        &self.spilled_args
    }

    /// A fresh frame slot of `size` bytes at `alignment`, shared with
    /// nothing, as a displacement.
    fn new_frame_slot(&mut self, size: i64, alignment: i32) -> i64 {
        debug_assert!(
            alignment <= self.frame_base.align(),
            "slot wants {alignment}-byte alignment but the frame was laid out \
             for {}; `FrameBase::of` missed a source of over-alignment",
            self.frame_base.align(),
        );
        -crate::arch::regalloc::grow_frame(
            &mut self.stack_offset,
            size,
            alignment,
            self.frame_base.align(),
            self.func_pos,
        )
    }

    /// Give every local its frame slot; see `arch::regalloc::place_locals`.
    fn place_locals(
        &mut self,
        func: &Function,
        types: &TypeTable,
        intervals: &[LiveInterval],
        set: crate::arch::regalloc::LocalSet,
    ) {
        let pos = self.func_pos;
        let placed =
            crate::arch::regalloc::place_locals(func, types, pos, intervals, set, |b, a| {
                self.new_frame_slot(b, a)
            });
        for (local, offset, bytes) in placed {
            self.locations
                .insert(local, Loc::Stack(LocalSlot::from_displacement(offset)));
            self.slot_bytes.insert(local, bytes);
            if func.local_of(local).is_some_and(|l| types.is_float(l.typ)) {
                self.fp_pseudos.insert(local);
            }
        }
    }

    /// `pseudo` lives in a frame slot of `shape`, wherever it was before. The
    /// slot is placed by [`RegAlloc::place_stack_slots`].
    fn home_on_stack(&mut self, pseudo: PseudoId, shape: SlotShape) {
        self.locations.remove(&pseudo);
        self.stack_homed.insert(pseudo, shape);
    }

    /// Has `pseudo` a home already: a location, or a frame slot to come?
    fn is_placed(&self, pseudo: PseudoId) -> bool {
        self.locations.contains_key(&pseudo) || self.stack_homed.contains_key(&pseudo)
    }

    /// Lay out the frame slots of every stack-homed pseudo, sharing between
    /// those never live together
    /// ([`crate::arch::regalloc::color_stack_slots`]), and complete the
    /// spilled arguments' records with where they went. The most aligned
    /// slots first, so that rounding pads the frame once.
    fn place_stack_slots(&mut self, func: &Function) {
        let homed = std::mem::take(&mut self.stack_homed);
        let coloring = crate::arch::regalloc::color_stack_slots(func, &self.live_out, &homed);
        let mut order: Vec<usize> = (0..coloring.slots.len()).collect();
        order.sort_by_key(|&n| (std::cmp::Reverse(coloring.slots[n].align), n));
        let mut slot = vec![LocalSlot::PENDING; coloring.slots.len()];
        for n in order {
            let shape = coloring.slots[n];
            slot[n] =
                LocalSlot::from_displacement(self.new_frame_slot(shape.bytes.into(), shape.align));
        }
        for (&p, &n) in &coloring.slot_of {
            self.locations.insert(p, Loc::Stack(slot[n]));
            self.slot_bytes.insert(p, homed[&p].bytes.into());
        }
        for arg in &mut self.spilled_args {
            arg.to_stack_offset = slot[coloring.slot_of[&arg.pseudo]];
        }
    }

    /// Chordal coloring plus Belady eviction, AArch64 mirror of the
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
    ///      per-vertex forbidden set, in both banks (`gp_forbidden`,
    ///      `Clobbering::vreg_forbidden`). In-loop general values get a
    ///      callee-first preferred palette (soft, not forbidden).
    ///   3. Commit: write Loc::Reg / Loc::VReg for colored vertices,
    ///      allocate stack slots for spilled vertices, track
    ///      `used_callee_saved` for the prologue.
    fn run_chordal_color(
        &mut self,
        func: &Function,
        types: &TypeTable,
        intervals: Vec<LiveInterval>,
        clobbering: &Clobbering,
    ) {
        // -------- Phase 1: pre-pass --------
        // What the pre-pass asks about each interval, indexed once: asking by
        // scanning the function per interval made it quadratic.
        let setval_sizes = crate::arch::regalloc::setval_sizes(func);
        let mut int128_pseudos: HashSet<PseudoId> = crate::arch::regalloc::arg_pseudo_types(func)
            .into_iter()
            .filter(|(_, t)| types.is_plain_int128(*t))
            .map(|(p, _)| p)
            .collect();
        let mut multi_reg_returns: HashMap<PseudoId, TypeId> = HashMap::new();
        for insn in func.blocks.iter().flat_map(|b| &b.insns) {
            let Some(typ) = insn.typ else { continue };
            if types.is_plain_int128(typ) {
                int128_pseudos.extend(insn.target);
                int128_pseudos.extend(insn.src.iter().copied());
            }
            // A multi-register call return: complex, struct or union.
            if insn.op == Opcode::Call
                && (types.is_complex_float(typ)
                    || matches!(types.kind(typ), TypeKind::Struct | TypeKind::Union))
            {
                if let Some(target) = insn.target {
                    multi_reg_returns.entry(target).or_insert(typ);
                }
            }
        }
        let mut gp_candidates: std::collections::BTreeSet<PseudoId> =
            std::collections::BTreeSet::new();
        let mut vreg_candidates: std::collections::BTreeSet<PseudoId> =
            std::collections::BTreeSet::new();
        for interval in &intervals {
            if self.is_placed(interval.pseudo) {
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
                        let size = setval_sizes
                            .get(&interval.pseudo)
                            .copied()
                            .unwrap_or(Bits::new(64));
                        self.locations.insert(interval.pseudo, Loc::FImm(*v, size));
                        self.fp_pseudos.insert(interval.pseudo);
                        continue;
                    }
                    PseudoKind::Sym(name) => {
                        // By identity, not by name -- see the x86_64 mirror.
                        // A local has its slot already: `place_locals`.
                        debug_assert!(func.local_of(interval.pseudo).is_none());
                        self.locations
                            .insert(interval.pseudo, Loc::Global(name.clone()));
                        continue;
                    }
                    _ => {}
                }
            }

            // __int128 → 16-byte stack slot, never in registers.
            let is_int128 = int128_pseudos.contains(&interval.pseudo);
            if is_int128 {
                self.home_on_stack(interval.pseudo, SlotShape::new(16, 16));
                continue;
            }

            // Multi-register call returns (complex, struct, union) →
            // stack. These span V0+V1 or X0+X1 and can't live in a
            // single allocator vertex.
            let multi_reg_return_typ = multi_reg_returns.get(&interval.pseudo).copied();
            if let Some(typ) = multi_reg_return_typ {
                let size = crate::abi::slot_bytes(
                    types.byte_size(typ),
                    self.func_pos,
                    "an aggregate returned by a call",
                );
                let size = i32::try_from(size.max(8))
                    .expect("a value returned in registers is at most four of them");
                let alignment = crate::abi::slot_align(types.align(typ));
                self.home_on_stack(interval.pseudo, SlotShape::new(size, alignment));
                continue;
            }

            if self.fp_pseudos.contains(&interval.pseudo) {
                vreg_candidates.insert(interval.pseudo);
            } else {
                gp_candidates.insert(interval.pseudo);
            }
        }

        // -------- Phase 2: per-bank chordal coloring --------
        self.color_gp_bank(func, &intervals, clobbering, &gp_candidates);
        self.color_vreg_bank(func, clobbering, &vreg_candidates);
    }

    /// The X registers each of `candidates` may not take: every register a
    /// constraint point it touches clobbers, unless it is one of the point's
    /// operands ([`exempt_from_clobber`]), and every caller-saved register --
    /// X8, the sret pointer's, included -- where it crosses a call
    /// ([`Crossing::call_crossers`]).
    fn gp_forbidden(
        &self,
        clobbering: &Clobbering,
        candidates: &std::collections::BTreeSet<PseudoId>,
    ) -> BTreeMap<PseudoId, std::collections::BTreeSet<Reg>> {
        let mut forbidden = crate::arch::regalloc::constraint_clobbers(
            &clobbering.points,
            &clobbering.crossings,
            candidates,
            exempt_from_clobber,
        );
        let mut caller_saved: Vec<Reg> = self
            .allocatable_regs()
            .into_iter()
            .filter(|r| !r.is_callee_saved())
            .collect();
        for &r in Reg::arg_regs().iter().chain(&[Reg::X8]) {
            if !caller_saved.contains(&r) {
                caller_saved.push(r);
            }
        }
        for p in clobbering.gp_call_touched() {
            if candidates.contains(&p) {
                forbidden
                    .entry(p)
                    .or_default()
                    .extend(caller_saved.iter().copied());
            }
        }
        forbidden
    }

    fn color_gp_bank(
        &mut self,
        func: &Function,
        intervals: &[LiveInterval],
        clobbering: &Clobbering,
        gp_candidates: &std::collections::BTreeSet<PseudoId>,
    ) {
        let by_pseudo = crate::arch::regalloc::intervals_by_pseudo(intervals);
        use crate::arch::regalloc::{build_interference_graph, greedy_color, mcs_ordering};
        if gp_candidates.is_empty() {
            return;
        }

        // Pre-colored vertices: any pseudo already mapped to a GP reg
        // (ABI-pinned args from allocate_arguments). No aarch64 asm
        // constraint letter pins an operand.
        let mut pre_colored: BTreeMap<PseudoId, Reg> = BTreeMap::new();
        let mut all_vertices: std::collections::BTreeSet<PseudoId> = gp_candidates.clone();
        for (&pid, loc) in self.locations.iter() {
            if let Loc::Reg(r) = loc {
                pre_colored.insert(pid, *r);
                all_vertices.insert(pid);
            }
        }

        // GP coloring needs def-vs-src edges: aarch64 `csel` for
        // ternary `(cond)?a:b` materializes the target then reads
        // source values, which would clobber a source sharing the
        // target's register. Same pattern as x86_64's cmov.
        let graph = build_interference_graph(&all_vertices, func, &self.live_out, true);

        // Forbidden colors (`gp_forbidden`): constraint clobbers -- an
        // atomic's fixed scratch, a thread-local sequence, inline asm -- for
        // the pseudos a constraint touches that are not its operands, and
        // every caller-saved register for one that crosses a call (HARD; the
        // call clobbers them and the value would be lost). In-loop is only a
        // preference, for a value no call touches.
        let allocatable = self.allocatable_regs();
        let caller_saved: Vec<Reg> = allocatable
            .iter()
            .copied()
            .filter(|r| !r.is_callee_saved())
            .collect();
        let forbidden = self.gp_forbidden(clobbering, gp_candidates);
        let touched = clobbering.gp_call_touched();
        let in_loop_set: std::collections::BTreeSet<PseudoId> = intervals
            .iter()
            .filter(|i| i.in_loop && gp_candidates.contains(&i.pseudo))
            .filter(|i| !touched.contains(&i.pseudo))
            .map(|i| i.pseudo)
            .collect();

        let caller_first: Vec<Reg> = {
            let mut v = caller_saved.clone();
            for &r in &allocatable {
                if r.is_callee_saved() {
                    v.push(r);
                }
            }
            v
        };
        let callee_first: Vec<Reg> = {
            let mut v: Vec<Reg> = allocatable
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

        // Asm register operands are colored first: gcc guarantees each
        // one a register, so it is the other values that spill.
        let asm_ops = crate::arch::regalloc::asm_register_operands(func);
        let order = crate::arch::regalloc::asm_operands_first(mcs_ordering(&graph), &asm_ops);
        let result = greedy_color(
            &graph,
            &order,
            &allocatable,
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

        // -------- Belady eviction --------
        let uses = crate::arch::regalloc::compute_use_positions(func);
        let mut colors = result.colors;
        let mut final_spilled: std::collections::BTreeSet<PseudoId> =
            std::collections::BTreeSet::new();
        for spilled in result.spilled {
            if colors.contains_key(&spilled) {
                continue;
            }
            let interval = match by_pseudo.get(&spilled).copied() {
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
                // Don't evict ABI-pinned args, or an asm register operand:
                // the template needs it in a register.
                if pre_colored.contains_key(&n) || asm_ops.contains(&n) {
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

        // -------- M9b post-coloring Copy coalescing --------
        // See the x86_64 mirror for the rationale. Mirror of the
        // logic adapted to aarch64's `Reg` palette.
        let candidates = crate::arch::regalloc::find_copy_coalesce_candidates(func);
        for (t, s) in candidates {
            if !gp_candidates.contains(&t) || !gp_candidates.contains(&s) {
                continue;
            }
            if pre_colored.contains_key(&t) {
                continue;
            }
            let t_loc = self.locations.get(&t).cloned();
            let s_loc = self.locations.get(&s).cloned();
            let (Some(Loc::Reg(t_reg)), Some(Loc::Reg(s_reg))) = (t_loc, s_loc) else {
                continue;
            };
            if t_reg == s_reg {
                continue;
            }
            if graph.neighbors(t).any(|n| n == s) {
                continue;
            }
            let empty = std::collections::BTreeSet::new();
            let t_forbid = forbidden.get(&t).unwrap_or(&empty);
            if t_forbid.contains(&s_reg) {
                continue;
            }
            let conflict = graph.neighbors(t).any(|n| {
                self.locations
                    .get(&n)
                    .map(|l| matches!(l, Loc::Reg(r) if *r == s_reg))
                    .unwrap_or(false)
            });
            if conflict {
                continue;
            }
            self.locations.insert(t, Loc::Reg(s_reg));
            if s_reg.is_callee_saved() && !self.used_callee_saved.contains(&s_reg) {
                self.used_callee_saved.push(s_reg);
            }
        }

        for spilled in final_spilled {
            if !self.is_placed(spilled) {
                let bytes = fp_pseudo_bytes(func, spilled);
                self.home_on_stack(spilled, SlotShape::new(bytes, bytes));
            }
        }
    }

    /// Color the floating-point and vector values. One that crosses a call
    /// may take only V8-V15 ([`Clobbering::vreg_forbidden`]), and so is
    /// spilled once those are taken; every other value tries the
    /// caller-saved registers first, so that only what has to survive a call
    /// costs the prologue a save.
    fn color_vreg_bank(
        &mut self,
        func: &Function,
        clobbering: &Clobbering,
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
        let mut graph = build_interference_graph(&all_vertices, func, &self.live_out, false);
        crate::arch::regalloc::args_interfere(&mut graph, func, &all_vertices);
        let mut forbidden = clobbering.vreg_forbidden(vreg_candidates, &self.narrow_v);
        // An argument here is one `spill_args_across_calls` took out of the
        // register it arrived in. The prologue moves the arguments one at a
        // time, so none may land in another's arrival register.
        for &p in vreg_candidates {
            if func
                .get_pseudo(p)
                .is_some_and(|ps| matches!(ps.kind, PseudoKind::Arg(_)))
            {
                forbidden.entry(p).or_default().extend(VReg::arg_regs());
            }
        }
        let caller_first: Vec<VReg> = {
            let (callee, mut caller): (Vec<VReg>, Vec<VReg>) = VReg::allocatable()
                .iter()
                .partition(|v| v.is_callee_saved());
            caller.extend(callee);
            caller
        };
        let order = mcs_ordering(&graph);
        let result = greedy_color(
            &graph,
            &order,
            VReg::allocatable(),
            &pre_colored,
            &forbidden,
            |_| Some(caller_first.clone()),
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
        for spilled in result.spilled {
            if !self.is_placed(spilled) {
                // Sixteen bytes unless the value is known to fit eight: a
                // binary128 argument has no def to read its width from.
                let bytes = if self.narrow_v.contains(&spilled) {
                    8
                } else {
                    16
                };
                self.home_on_stack(spilled, SlotShape::new(bytes, bytes));
            }
        }
    }

    /// Check the finished assignment against exact liveness; see
    /// [`crate::arch::regalloc::verify_assignment`]. A debug build only.
    ///
    /// What this backend tells the verifier: where each location is -- a V
    /// register as its low and high halves, the high one only for a value
    /// wider than 64 bits (or one of unknown width), since AAPCS64 preserves
    /// only the low half of V8-V15 across a call and writing a narrow value
    /// zeroes the high half; what each instruction overwrites -- a call every
    /// caller-saved X register, the low half of every caller-saved V register
    /// and the high half of all of them, a constraint point its clobbers, an
    /// asm the V registers it names; the canary's slot; and that an X
    /// register or a frame slot may not hold a def and a source of the same
    /// instruction (`csel` writes its target before reading both sources),
    /// while a V register may, and an identity `Copy` is no instruction.
    fn verify(
        &self,
        func: &Function,
        types: &TypeTable,
        constraint_points: &[ConstraintPoint<Reg>],
        call_positions: &[usize],
    ) {
        use crate::arch::regalloc::{Assignment, Cell, Clobber, Spared};
        let narrow = narrow_v_values(func, types);
        let cells = |p: PseudoId, loc: &Loc| -> Vec<Cell<RegUnit>> {
            let span = self.arg_span.get(&p).copied().unwrap_or(1);
            match loc {
                Loc::Reg(r) => {
                    let regs = Reg::arg_regs();
                    match regs.iter().position(|x| x == r) {
                        Some(first) if span > 1 => regs[first..first + span]
                            .iter()
                            .map(|&x| Cell::Reg(RegUnit::X(x)))
                            .collect(),
                        _ => vec![Cell::Reg(RegUnit::X(*r))],
                    }
                }
                Loc::VReg(v) => {
                    let regs = VReg::arg_regs();
                    let run: Vec<VReg> = match regs.iter().position(|x| x == v) {
                        Some(first) if span > 1 => regs[first..first + span].to_vec(),
                        _ => vec![*v],
                    };
                    let wide = !narrow.contains(&p);
                    run.into_iter()
                        .flat_map(|v| {
                            std::iter::once(Cell::Reg(RegUnit::VLo(v)))
                                .chain(wide.then_some(Cell::Reg(RegUnit::VHi(v))))
                        })
                        .collect()
                }
                Loc::Stack(slot) => {
                    let bytes = self.slot_bytes.get(&p).copied().unwrap_or_else(|| {
                        panic!(
                            "internal compiler error: {p} of `{}` has frame slot {} \
                             but no recorded size",
                            func.name,
                            slot.displacement()
                        )
                    });
                    let lo = slot.displacement();
                    vec![Cell::Frame { lo, hi: lo + bytes }]
                }
                Loc::IncomingArg(o) => vec![Cell::Incoming(o.displacement())],
                Loc::Imm(_) | Loc::FImm(..) | Loc::Global(_) => Vec::new(),
            }
        };

        let mut clobbers: Vec<Clobber<RegUnit>> = constraint_points
            .iter()
            .map(|cp| Clobber {
                position: cp.position,
                regs: cp.clobbers.iter().map(|&r| RegUnit::X(r)).collect(),
                spared: Spared::Always(cp.involved_pseudos.clone()),
                why: "the instruction",
            })
            .collect();
        let caller_saved: Vec<RegUnit> = Reg::allocatable()
            .iter()
            .filter(|r| !r.is_callee_saved())
            .map(|&r| RegUnit::X(r))
            .chain(
                VReg::allocatable()
                    .iter()
                    .filter(|v| !v.is_callee_saved())
                    .map(|&v| RegUnit::VLo(v)),
            )
            .chain(VReg::allocatable().iter().map(|&v| RegUnit::VHi(v)))
            .collect();
        clobbers.extend(call_positions.iter().map(|&position| Clobber {
            position,
            regs: caller_saved.clone(),
            spared: Spared::Operands,
            why: "a call",
        }));
        for (position, insn) in func.blocks.iter().flat_map(|b| &b.insns).enumerate() {
            let named: Vec<RegUnit> = asm_clobbered(insn, parse_fp_clobber_name)
                .into_iter()
                .flat_map(|v| [RegUnit::VLo(v), RegUnit::VHi(v)])
                .collect();
            if !named.is_empty() {
                clobbers.push(Clobber {
                    position,
                    regs: named,
                    spared: Spared::Nothing,
                    why: "the asm's clobber list",
                });
            }
        }

        let mut reserved: Vec<(&'static str, Cell<RegUnit>)> = self
            .guard_slot
            .map(|slot| {
                let lo = slot.displacement();
                ("the canary's slot", Cell::Frame { lo, hi: lo + 8 })
            })
            .into_iter()
            .collect();
        // Every X register this function may not colour with -- the codegen
        // and linker scratches, x8, the frame and link registers, the frame
        // base, the global register variables -- and the scratch V registers.
        // The sret pointer arriving in x8 is an argument, which may.
        let allocatable = self.allocatable_regs();
        let held_back = EVERY_X
            .iter()
            .filter(|r| !allocatable.contains(r))
            .map(|&r| RegUnit::X(r))
            .chain(
                [VReg::V16, VReg::V17, VReg::V18]
                    .into_iter()
                    .flat_map(|v| [RegUnit::VLo(v), RegUnit::VHi(v)]),
            );
        reserved.extend(held_back.map(|u| ("a register held back from allocation", Cell::Reg(u))));
        let def_may_share_src = |insn: &Instruction, cell: &Cell<RegUnit>| {
            insn.op == Opcode::Copy || matches!(cell, Cell::Reg(RegUnit::VLo(_) | RegUnit::VHi(_)))
        };
        crate::arch::regalloc::verify_assignment(&Assignment {
            func,
            live_out: &self.live_out,
            locations: &self.locations,
            cells: &cells,
            clobbers,
            reserved,
            def_may_share_src: &def_may_share_src,
        });
    }

    fn compute_live_intervals(&self, func: &Function) -> LivenessResult<Reg> {
        let tls = self.tls_access;
        compute_live_intervals(func, |insn| get_constraint_info_aarch64(insn, tls))
    }

    /// Get stack size needed (aligned to max local alignment, minimum 16).
    /// When max_local_align > 16, rounds base to max_align then adds (max_align - 1)
    /// padding so the aligned base register (x19) always finds an aligned start.
    pub fn stack_size(&self) -> i64 {
        if let FrameBase::Aligned { align, .. } = self.frame_base {
            let align = i64::from(align);
            let base = (self.stack_offset + align - 1) & !(align - 1);
            base + align - 1
        } else {
            (self.stack_offset + 15) & !15
        }
    }

    /// How this function's locals are addressed.
    pub fn frame_base(&self) -> FrameBase {
        self.frame_base
    }

    /// The registers this function may color with: the machine's allocatable
    /// set, less whatever the frame has claimed for its base.
    ///
    /// Every consumer must come through here rather than reading
    /// `Reg::allocatable()` — the colorer builds its own preference orders,
    /// and withholding the base from `free_regs` alone left it free to hand
    /// out the very register the prologue overwrites.
    fn allocatable_regs(&self) -> Vec<Reg> {
        let claimed = self.frame_base.reg();
        Reg::allocatable()
            .iter()
            .copied()
            .filter(|r| Some(*r) != claimed && !self.global_registers.contains(r))
            .collect()
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

#[cfg(test)]
mod tests {
    use super::*;

    /// Stacked arguments are laid out from the argument area's base, not from
    /// the frame displacement.
    ///
    /// The two differ by the saved FP and LR pair, and rounding the displacement
    /// directly charged an over-aligned argument for those 16 bytes: an
    /// argument wanting 32-byte alignment and arriving first went to offset 32
    /// instead of 16, one whole alignment unit past where the caller -- which
    /// measures from its outgoing area's own base -- had written it.
    ///
    /// Only alignment past 16 can tell the two bases apart, since 16 is
    /// already a multiple of 8 and of 16. The 8 and 16 cases are asserted so
    /// that stays true.
    #[test]
    fn incoming_args_are_laid_out_from_the_area_base() {
        let slot = |bytes: u64, align: u64| StackSlot {
            bytes: ByteSize::new(bytes),
            align: crate::units::Align::of_bytes(align),
        };
        let first = IncomingOff::FIRST.0;

        // Alignment up to the base's own: unchanged, and packed at 8.
        let mut next = IncomingOff::FIRST;
        assert_eq!(
            IncomingOff::take(&mut next, slot(8, 8)).displacement(),
            first
        );
        assert_eq!(
            IncomingOff::take(&mut next, slot(8, 8)).displacement(),
            first + 8
        );
        assert_eq!(
            IncomingOff::take(&mut next, slot(16, 16)).displacement(),
            first + 16
        );

        // A 16-aligned argument landing on an odd 8-byte slot still rounds.
        let mut next = IncomingOff::FIRST;
        assert_eq!(
            IncomingOff::take(&mut next, slot(8, 8)).displacement(),
            first
        );
        assert_eq!(
            IncomingOff::take(&mut next, slot(16, 16)).displacement(),
            first + 16
        );

        // Past the base's alignment is where the bases diverge. Arriving
        // first, an over-aligned argument must start *at* the base.
        for align in [32u64, 64] {
            let mut next = IncomingOff::FIRST;
            assert_eq!(
                IncomingOff::take(&mut next, slot(align, align)).displacement(),
                first,
                "an {align}-aligned argument arriving first starts at the area base"
            );
        }

        // And after an 8-byte argument it rounds to the next multiple of its
        // alignment measured from the base, not from the displacement.
        for align in [32u64, 64] {
            let mut next = IncomingOff::FIRST;
            assert_eq!(
                IncomingOff::take(&mut next, slot(8, 8)).displacement(),
                first
            );
            assert_eq!(
                IncomingOff::take(&mut next, slot(align, align)).displacement(),
                first + align as i64,
                "an {align}-aligned argument rounds within the argument area"
            );
        }
    }

    /// Named parameters past the registers, read back by each platform's
    /// rule: eight ints and eight doubles fill x0-x7 and v0-v7, and then
    /// `char, short, char, int, double, char` arrive on the stack.
    ///
    /// AAPCS64 gives each an eight-byte granule; Apple packs each at its
    /// natural size and alignment. The caller's side of the same sequence is
    /// asserted in `cc/tests/codegen/cross_abi.rs`.
    #[test]
    fn stacked_params_follow_the_platform_rule() {
        use crate::target::{Arch, Os, Target};
        for (os, stacked, want) in [
            (Os::Linux, StackedArgs::Granules, [0, 8, 16, 24, 32, 40]),
            (Os::MacOS, StackedArgs::Natural, [0, 2, 4, 8, 16, 24]),
        ] {
            let target = Target::new(Arch::Aarch64, os);
            assert_eq!(StackedArgs::of(&target), stacked);
            let types = TypeTable::new(&target);
            let mut params: Vec<(String, TypeId)> = Vec::new();
            params.extend((0..8).map(|i| (format!("i{i}"), types.int_id)));
            params.extend((0..8).map(|i| (format!("d{i}"), types.double_id)));
            for t in [
                types.char_id,
                types.short_id,
                types.char_id,
                types.int_id,
                types.double_id,
                types.char_id,
            ] {
                params.push((format!("s{}", params.len()), t));
            }
            let layout = param_layout(&params, &types, stacked);
            let got: Vec<i64> = layout.places[16..]
                .iter()
                .map(|p| match p {
                    ParamPlace::Stack { at, .. } => {
                        at.displacement() - IncomingOff::FIRST.displacement()
                    }
                    other => panic!("expected a stacked parameter on {os:?}, got {other:?}"),
                })
                .collect();
            assert_eq!(got, want, "stacked parameter offsets on {os:?}");
            let end = layout.stack_end.displacement() - IncomingOff::FIRST.displacement();
            assert_eq!(end, if os == Os::MacOS { 25 } else { 48 });
        }
    }

    #[test]
    fn legalization_register_is_never_allocated() {
        // X15 carries the expansion of one unencodable instruction and
        // nothing else; a pseudo living there would be overwritten by the
        // next far offset.
        assert!(!Reg::allocatable().contains(&Reg::X15));
    }

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
    fn parse_fp_clobber_name_every_width() {
        for name in ["v8", "q8", "d8", "s8", "h8", "b8", "%D8"] {
            assert_eq!(parse_fp_clobber_name(name), Some(VReg::V8), "{name}");
        }
        assert_eq!(parse_fp_clobber_name("v31"), Some(VReg::V31));
        // Codegen scratch, out of range, and not registers at all.
        for name in ["v16", "v32", "sp", "x8", "memory", "d"] {
            assert_eq!(parse_fp_clobber_name(name), None, "{name}");
        }
    }

    #[test]
    fn is_call_like_aarch64_covers_libc_emitters() {
        assert!(is_call_like_aarch64(Opcode::Call));
        assert!(is_call_like_aarch64(Opcode::Longjmp));
        assert!(is_call_like_aarch64(Opcode::Setjmp));
        // Aarch64 lowers Memset/Memcpy/Memmove to a libc `bl`, so they are
        // call-like here exactly as on x86-64. They were excluded before,
        // on the strength of a comment saying they reached the ordinary
        // `Opcode::Call` path -- nothing performed that conversion, so they
        // reached codegen's catch-all arm and emitted nothing at all.
        assert!(is_call_like_aarch64(Opcode::Memset));
        assert!(is_call_like_aarch64(Opcode::Memcpy));
        assert!(is_call_like_aarch64(Opcode::Memmove));
        // The sign operations are computed in place.
        assert!(!is_call_like_aarch64(Opcode::Fabs));
        assert!(!is_call_like_aarch64(Opcode::CopySign));
        assert!(!is_call_like_aarch64(Opcode::Sqrt));
        assert!(!is_call_like_aarch64(Opcode::Fma));
        assert!(!is_call_like_aarch64(Opcode::FMin));
        assert!(!is_call_like_aarch64(Opcode::RoundToIntegral(
            crate::float::IntegralRounding::Floor
        )));
        assert!(!is_call_like_aarch64(Opcode::Signbit));
        assert!(!is_call_like_aarch64(Opcode::Add));
        assert!(!is_call_like_aarch64(Opcode::Asm));
    }

    fn make_asm_insn(clobbers: &[&str], operands: &[(&str, PseudoId)]) -> Instruction {
        use crate::arch::asm_constraints::AsmAccess;
        use crate::ir::{AsmConstraint, AsmData};
        let (outputs, inputs): (Vec<_>, Vec<_>) = operands
            .iter()
            .map(|&(c, pseudo)| {
                AsmConstraint::new(pseudo, c, crate::target::Arch::Aarch64, Bits::new(64))
            })
            .partition(|c| c.class.access != AsmAccess::Read);
        let mut insn = Instruction::new(Opcode::Asm);
        insn.extra_mut().asm_data = Some(Box::new(AsmData {
            template: String::new(),
            outputs,
            inputs,
            clobbers: clobbers.iter().map(|s| s.to_string()).collect(),
            goto_labels: Vec::new(),
        }));
        insn
    }

    /// No operand of an asm statement may live in a register it clobbers:
    /// the template may write that register before reading its operands.
    /// So an asm's declared clobbers exempt none of its operands, register
    /// or memory -- unlike an instruction such as `idivq`, which reads its
    /// operands before touching the registers it claims.
    #[test]
    fn tls_addr_clobbers_follow_the_model() {
        use crate::target::TlsAccess;
        let insn = Instruction::tls_addr(PseudoId(1), PseudoId(2), crate::types::TypeId::INVALID);
        let (elf, _) = get_constraint_info_aarch64(&insn, TlsAccess::ElfDescriptor).unwrap();
        let (macho, _) = get_constraint_info_aarch64(&insn, TlsAccess::MachOTlv).unwrap();
        // The descriptor sequence loads its resolver into x1; the Mach-O
        // getter keeps x1-x28 and returns through x0.
        assert!(elf.contains(&Reg::X0) && elf.contains(&Reg::X1));
        assert!(macho.contains(&Reg::X0) && !macho.contains(&Reg::X1));
        for kept in [Reg::X2, Reg::X7, Reg::X12, Reg::X19, Reg::X28] {
            assert!(
                !macho.contains(&kept),
                "{kept:?} is preserved by the getter"
            );
        }
    }

    #[test]
    fn asm_operands_are_not_exempt_from_the_statements_clobbers() {
        let insn = make_asm_insn(
            &["x0", "memory"],
            &[("=r", PseudoId(1)), ("r", PseudoId(2)), ("m", PseudoId(5))],
        );
        let (clobbers, exempt) =
            get_constraint_info_aarch64(&insn, crate::target::TlsAccess::ElfStatic)
                .expect("a register is claimed");
        assert!(clobbers.contains(&Reg::X0));
        assert!(exempt.is_empty(), "{exempt:?}");
    }
}
