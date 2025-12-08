//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 Low-level Intermediate Representation (LIR)
//
// Strongly-typed representation of AArch64 assembly instructions.
// Each instruction variant encodes all operands in typed fields,
// enabling peephole optimizations before final assembly emission.
//

use super::regalloc::{Reg, VReg};
use crate::arch::lir::{Directive, EmitAsm, FpSize, Label, OperandSize, Symbol};
use crate::target::{Os, Target};
use std::fmt::{self, Write};

// ============================================================================
// Memory Addressing Modes
// ============================================================================

/// AArch64 memory addressing mode
#[allow(dead_code)] // Documents full instruction set
#[derive(Debug, Clone, PartialEq)]
pub enum MemAddr {
    /// [base] - Register indirect
    Base(Reg),

    /// [base, #offset] - Register indirect with immediate offset
    BaseOffset { base: Reg, offset: i32 },

    /// [base, Xm] - Register indirect with register offset
    BaseReg { base: Reg, index: Reg },

    /// [base, Xm, LSL #shift] - Register indirect with shifted register offset
    BaseRegShift { base: Reg, index: Reg, shift: u8 },

    /// [base, #offset]! - Pre-indexed (base updated before access)
    PreIndex { base: Reg, offset: i32 },

    /// [base], #offset - Post-indexed (base updated after access)
    PostIndex { base: Reg, offset: i32 },
}

impl MemAddr {
    /// Format memory operand in AArch64 syntax
    pub fn format(&self) -> String {
        match self {
            MemAddr::Base(base) => format!("[{}]", base.name64()),
            MemAddr::BaseOffset { base, offset } => {
                if *offset == 0 {
                    format!("[{}]", base.name64())
                } else {
                    format!("[{}, #{}]", base.name64(), offset)
                }
            }
            MemAddr::BaseReg { base, index } => {
                format!("[{}, {}]", base.name64(), index.name64())
            }
            MemAddr::BaseRegShift { base, index, shift } => {
                format!("[{}, {}, lsl #{}]", base.name64(), index.name64(), shift)
            }
            MemAddr::PreIndex { base, offset } => {
                format!("[{}, #{}]!", base.name64(), offset)
            }
            MemAddr::PostIndex { base, offset } => {
                format!("[{}], #{}", base.name64(), offset)
            }
        }
    }
}

// ============================================================================
// General-Purpose Operands
// ============================================================================

/// AArch64 general-purpose operand (register or immediate)
#[derive(Debug, Clone, PartialEq)]
pub enum GpOperand {
    /// Register operand
    Reg(Reg),
    /// Immediate integer value (12-bit for most ALU ops)
    Imm(i64),
}

impl GpOperand {
    /// Format operand in AArch64 syntax for given size
    pub fn format(&self, size: OperandSize) -> String {
        match self {
            GpOperand::Reg(r) => r.name_for_size(size.bits()).to_string(),
            GpOperand::Imm(v) => format!("#{}", v),
        }
    }
}

// ============================================================================
// FP/SIMD Operands
// ============================================================================

/// AArch64 FP/SIMD operand (register)
#[allow(dead_code)] // Documents full instruction set
#[derive(Debug, Clone, PartialEq)]
pub enum FpOperand {
    /// FP register operand
    Reg(VReg),
}

// ============================================================================
// Condition Codes
// ============================================================================

/// AArch64 condition codes for comparisons and conditional operations
#[allow(dead_code)] // Documents full instruction set
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Cond {
    /// Equal (Z=1)
    Eq,
    /// Not equal (Z=0)
    Ne,
    /// Carry set / unsigned higher or same (C=1)
    Cs,
    /// Carry clear / unsigned lower (C=0)
    Cc,
    /// Minus / negative (N=1)
    Mi,
    /// Plus / positive or zero (N=0)
    Pl,
    /// Overflow (V=1)
    Vs,
    /// No overflow (V=0)
    Vc,
    /// Unsigned higher (C=1 and Z=0)
    Hi,
    /// Unsigned lower or same (C=0 or Z=1)
    Ls,
    /// Signed greater than or equal (N=V)
    Ge,
    /// Signed less than (N!=V)
    Lt,
    /// Signed greater than (Z=0 and N=V)
    Gt,
    /// Signed less than or equal (Z=1 or N!=V)
    Le,
    /// Always (unconditional)
    Al,
    /// Never (unconditional inverse, rarely used)
    Nv,
}

impl Cond {
    /// AArch64 condition code suffix
    pub fn suffix(&self) -> &'static str {
        match self {
            Cond::Eq => "eq",
            Cond::Ne => "ne",
            Cond::Cs => "cs",
            Cond::Cc => "cc",
            Cond::Mi => "mi",
            Cond::Pl => "pl",
            Cond::Vs => "vs",
            Cond::Vc => "vc",
            Cond::Hi => "hi",
            Cond::Ls => "ls",
            Cond::Ge => "ge",
            Cond::Lt => "lt",
            Cond::Gt => "gt",
            Cond::Le => "le",
            Cond::Al => "al",
            Cond::Nv => "nv",
        }
    }
}

impl fmt::Display for Cond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.suffix())
    }
}

// ============================================================================
// Shift Type
// ============================================================================

/// Shift type for shifted register operands
#[allow(dead_code)] // Documents full instruction set
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ShiftType {
    /// Logical shift left
    Lsl,
    /// Logical shift right
    Lsr,
    /// Arithmetic shift right
    Asr,
    /// Rotate right
    Ror,
}

// ============================================================================
// Extend Type
// ============================================================================

/// Extend type for extended register operands
#[allow(dead_code)] // Documents full instruction set
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExtendType {
    /// Unsigned extend byte
    Uxtb,
    /// Unsigned extend halfword
    Uxth,
    /// Unsigned extend word
    Uxtw,
    /// Unsigned extend doubleword (same as LSL for 64-bit)
    Uxtx,
    /// Signed extend byte
    Sxtb,
    /// Signed extend halfword
    Sxth,
    /// Signed extend word
    Sxtw,
    /// Signed extend doubleword
    Sxtx,
}

// ============================================================================
// Call Target
// ============================================================================

/// Target for call instructions
#[allow(dead_code)] // Documents full instruction set
#[derive(Debug, Clone, PartialEq)]
pub enum CallTarget {
    /// Direct call to symbol
    Direct(Symbol),
    /// Indirect call through register
    Indirect(Reg),
}

// ============================================================================
// AArch64 LIR Instructions
// ============================================================================

/// AArch64 Low-level IR instruction
#[allow(dead_code)] // Documents full instruction set
#[derive(Debug, Clone)]
pub enum Aarch64Inst {
    // ========================================================================
    // Data Movement
    // ========================================================================
    /// MOV - Move register or immediate
    Mov {
        size: OperandSize,
        src: GpOperand,
        dst: Reg,
    },

    /// MOVZ - Move wide with zero (clear other bits)
    Movz {
        size: OperandSize,
        imm: u16,
        shift: u8, // 0, 16, 32, or 48
        dst: Reg,
    },

    /// MOVK - Move wide with keep (keep other bits)
    Movk {
        size: OperandSize,
        imm: u16,
        shift: u8, // 0, 16, 32, or 48
        dst: Reg,
    },

    /// MOVN - Move wide with NOT (inverted immediate)
    Movn {
        size: OperandSize,
        imm: u16,
        shift: u8,
        dst: Reg,
    },

    /// LDR - Load register
    Ldr {
        size: OperandSize,
        addr: MemAddr,
        dst: Reg,
    },

    /// LDRSB/LDRSH/LDRSW - Load register with sign extension
    Ldrs {
        src_size: OperandSize,
        dst_size: OperandSize,
        addr: MemAddr,
        dst: Reg,
    },

    /// STR - Store register
    Str {
        size: OperandSize,
        src: Reg,
        addr: MemAddr,
    },

    /// LDP - Load pair of registers
    Ldp {
        size: OperandSize,
        addr: MemAddr,
        dst1: Reg,
        dst2: Reg,
    },

    /// STP - Store pair of registers
    Stp {
        size: OperandSize,
        src1: Reg,
        src2: Reg,
        addr: MemAddr,
    },

    /// ADRP - Form PC-relative address to 4KB page
    Adrp { sym: Symbol, dst: Reg },

    /// ADD with symbol page offset (used with ADRP)
    /// macOS: add dst, base, symbol@PAGEOFF
    /// Linux: add dst, base, :lo12:symbol
    AddSymOffset { sym: Symbol, base: Reg, dst: Reg },

    /// LDR with symbol page offset (used with ADRP)
    /// macOS: ldr dst, [base, symbol@PAGEOFF]
    /// Linux: ldr dst, [base, :lo12:symbol]
    LdrSymOffset {
        size: OperandSize,
        sym: Symbol,
        base: Reg,
        dst: Reg,
    },

    // ========================================================================
    // Integer Arithmetic
    // ========================================================================
    /// ADD - Add
    Add {
        size: OperandSize,
        src1: Reg,
        src2: GpOperand,
        dst: Reg,
    },

    /// SUB - Subtract
    Sub {
        size: OperandSize,
        src1: Reg,
        src2: GpOperand,
        dst: Reg,
    },

    /// MUL - Multiply
    Mul {
        size: OperandSize,
        src1: Reg,
        src2: Reg,
        dst: Reg,
    },

    /// SDIV - Signed divide
    Sdiv {
        size: OperandSize,
        src1: Reg,
        src2: Reg,
        dst: Reg,
    },

    /// UDIV - Unsigned divide
    Udiv {
        size: OperandSize,
        src1: Reg,
        src2: Reg,
        dst: Reg,
    },

    /// MSUB - Multiply-subtract (for modulo: r = a - (a/b)*b)
    Msub {
        size: OperandSize,
        mul1: Reg,
        mul2: Reg,
        sub: Reg,
        dst: Reg,
    },

    /// NEG - Negate
    Neg {
        size: OperandSize,
        src: Reg,
        dst: Reg,
    },

    // ========================================================================
    // Bitwise Operations
    // ========================================================================
    /// AND - Bitwise AND
    And {
        size: OperandSize,
        src1: Reg,
        src2: GpOperand,
        dst: Reg,
    },

    /// ORR - Bitwise OR
    Orr {
        size: OperandSize,
        src1: Reg,
        src2: GpOperand,
        dst: Reg,
    },

    /// EOR - Bitwise exclusive OR
    Eor {
        size: OperandSize,
        src1: Reg,
        src2: GpOperand,
        dst: Reg,
    },

    /// MVN - Bitwise NOT (move NOT)
    Mvn {
        size: OperandSize,
        src: Reg,
        dst: Reg,
    },

    /// LSL - Logical shift left
    Lsl {
        size: OperandSize,
        src: Reg,
        amount: GpOperand,
        dst: Reg,
    },

    /// LSR - Logical shift right
    Lsr {
        size: OperandSize,
        src: Reg,
        amount: GpOperand,
        dst: Reg,
    },

    /// ASR - Arithmetic shift right
    Asr {
        size: OperandSize,
        src: Reg,
        amount: GpOperand,
        dst: Reg,
    },

    /// ROR - Rotate right
    Ror {
        size: OperandSize,
        src: Reg,
        amount: GpOperand,
        dst: Reg,
    },

    /// REV - Reverse bytes (byte swap)
    Rev {
        size: OperandSize,
        src: Reg,
        dst: Reg,
    },

    /// REV16 - Reverse bytes in halfwords
    Rev16 {
        size: OperandSize,
        src: Reg,
        dst: Reg,
    },

    // ========================================================================
    // Comparison and Conditional
    // ========================================================================
    /// CMP - Compare (sets flags based on dst - src)
    Cmp {
        size: OperandSize,
        src1: Reg,
        src2: GpOperand,
    },

    /// TST - Test (sets flags based on AND)
    Tst {
        size: OperandSize,
        src1: Reg,
        src2: GpOperand,
    },

    /// CSET - Conditional set (set to 1 if condition true, else 0)
    Cset { cond: Cond, dst: Reg },

    /// CSEL - Conditional select
    Csel {
        size: OperandSize,
        cond: Cond,
        src_true: Reg,
        src_false: Reg,
        dst: Reg,
    },

    // ========================================================================
    // Sign/Zero Extension
    // ========================================================================
    /// SXTB - Sign extend byte to word/doubleword
    Sxtb {
        dst_size: OperandSize,
        src: Reg,
        dst: Reg,
    },

    /// SXTH - Sign extend halfword to word/doubleword
    Sxth {
        dst_size: OperandSize,
        src: Reg,
        dst: Reg,
    },

    /// SXTW - Sign extend word to doubleword
    Sxtw { src: Reg, dst: Reg },

    /// UXTB - Zero extend byte (AND with 0xFF)
    Uxtb { src: Reg, dst: Reg },

    /// UXTH - Zero extend halfword (AND with 0xFFFF)
    Uxth { src: Reg, dst: Reg },

    // ========================================================================
    // Control Flow
    // ========================================================================
    /// B - Unconditional branch
    B { target: Label },

    /// B.cond - Conditional branch
    BCond { cond: Cond, target: Label },

    /// BL - Branch with link (function call)
    Bl { target: CallTarget },

    /// BLR - Branch to register with link
    Blr { target: Reg },

    /// RET - Return from subroutine
    Ret,

    // ========================================================================
    // Floating-Point (Scalar)
    // ========================================================================
    /// FMOV - FP move (between FP regs or GP<->FP)
    FmovReg { size: FpSize, src: VReg, dst: VReg },

    /// FMOV - Move from GP to FP register
    FmovFromGp { size: FpSize, src: Reg, dst: VReg },

    /// FMOV - Move from FP to GP register
    FmovToGp { size: FpSize, src: VReg, dst: Reg },

    /// LDR (FP) - Load FP register
    LdrFp {
        size: FpSize,
        addr: MemAddr,
        dst: VReg,
    },

    /// LDR (FP) with symbol page offset (used with ADRP for global FP loads)
    /// macOS: ldr dst, [base, symbol@PAGEOFF]
    /// Linux: ldr dst, [base, :lo12:symbol]
    LdrFpSymOffset {
        size: FpSize,
        sym: Symbol,
        base: Reg,
        dst: VReg,
    },

    /// STR (FP) - Store FP register
    StrFp {
        size: FpSize,
        src: VReg,
        addr: MemAddr,
    },

    /// FADD - FP add
    Fadd {
        size: FpSize,
        src1: VReg,
        src2: VReg,
        dst: VReg,
    },

    /// FSUB - FP subtract
    Fsub {
        size: FpSize,
        src1: VReg,
        src2: VReg,
        dst: VReg,
    },

    /// FMUL - FP multiply
    Fmul {
        size: FpSize,
        src1: VReg,
        src2: VReg,
        dst: VReg,
    },

    /// FDIV - FP divide
    Fdiv {
        size: FpSize,
        src1: VReg,
        src2: VReg,
        dst: VReg,
    },

    /// FNEG - FP negate
    Fneg { size: FpSize, src: VReg, dst: VReg },

    /// FCMP - FP compare
    Fcmp {
        size: FpSize,
        src1: VReg,
        src2: VReg,
    },

    /// FCMP with zero
    FcmpZero { size: FpSize, src: VReg },

    /// SCVTF - Signed int to FP
    Scvtf {
        int_size: OperandSize,
        fp_size: FpSize,
        src: Reg,
        dst: VReg,
    },

    /// UCVTF - Unsigned int to FP
    Ucvtf {
        int_size: OperandSize,
        fp_size: FpSize,
        src: Reg,
        dst: VReg,
    },

    /// FCVTZS - FP to signed int (truncate toward zero)
    Fcvtzs {
        fp_size: FpSize,
        int_size: OperandSize,
        src: VReg,
        dst: Reg,
    },

    /// FCVTZU - FP to unsigned int (truncate toward zero)
    Fcvtzu {
        fp_size: FpSize,
        int_size: OperandSize,
        src: VReg,
        dst: Reg,
    },

    /// FCVT - FP to FP (size conversion)
    Fcvt {
        src_size: FpSize,
        dst_size: FpSize,
        src: VReg,
        dst: VReg,
    },

    // ========================================================================
    // Directives (Architecture-Independent)
    // ========================================================================
    /// Assembler directives (labels, sections, CFI, .loc, data, etc.)
    Directive(Directive),
}

// ============================================================================
// EmitAsm Implementation
// ============================================================================

impl EmitAsm for Aarch64Inst {
    fn emit(&self, target: &Target, out: &mut String) {
        match self {
            // Data Movement
            Aarch64Inst::Mov { size, src, dst } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    mov {}, {}",
                    dst.name_for_size(sz),
                    src.format(OperandSize::from_bits(sz))
                );
            }

            Aarch64Inst::Movz {
                size,
                imm,
                shift,
                dst,
            } => {
                let sz = size.bits().max(32);
                if *shift == 0 {
                    let _ = writeln!(out, "    movz {}, #{}", dst.name_for_size(sz), imm);
                } else {
                    let _ = writeln!(
                        out,
                        "    movz {}, #{}, lsl #{}",
                        dst.name_for_size(sz),
                        imm,
                        shift
                    );
                }
            }

            Aarch64Inst::Movk {
                size,
                imm,
                shift,
                dst,
            } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    movk {}, #{}, lsl #{}",
                    dst.name_for_size(sz),
                    imm,
                    shift
                );
            }

            Aarch64Inst::Movn {
                size,
                imm,
                shift,
                dst,
            } => {
                let sz = size.bits().max(32);
                if *shift == 0 {
                    let _ = writeln!(out, "    movn {}, #{}", dst.name_for_size(sz), imm);
                } else {
                    let _ = writeln!(
                        out,
                        "    movn {}, #{}, lsl #{}",
                        dst.name_for_size(sz),
                        imm,
                        shift
                    );
                }
            }

            Aarch64Inst::Ldr { size, addr, dst } => {
                let insn = match size {
                    OperandSize::B8 => "ldrb",
                    OperandSize::B16 => "ldrh",
                    _ => "ldr",
                };
                let reg = dst.name_for_size(size.bits().max(32));
                let _ = writeln!(out, "    {} {}, {}", insn, reg, addr.format());
            }

            Aarch64Inst::Ldrs {
                src_size,
                dst_size,
                addr,
                dst,
            } => {
                let insn = match (src_size, dst_size) {
                    (OperandSize::B8, OperandSize::B32) => "ldrsb",
                    (OperandSize::B8, OperandSize::B64) => "ldrsb",
                    (OperandSize::B16, OperandSize::B32) => "ldrsh",
                    (OperandSize::B16, OperandSize::B64) => "ldrsh",
                    (OperandSize::B32, OperandSize::B64) => "ldrsw",
                    _ => "ldr",
                };
                let reg = dst.name_for_size(dst_size.bits());
                let _ = writeln!(out, "    {} {}, {}", insn, reg, addr.format());
            }

            Aarch64Inst::Str { size, src, addr } => {
                let insn = match size {
                    OperandSize::B8 => "strb",
                    OperandSize::B16 => "strh",
                    _ => "str",
                };
                let reg = src.name_for_size(size.bits().max(32));
                let _ = writeln!(out, "    {} {}, {}", insn, reg, addr.format());
            }

            Aarch64Inst::Ldp {
                size,
                addr,
                dst1,
                dst2,
            } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    ldp {}, {}, {}",
                    dst1.name_for_size(sz),
                    dst2.name_for_size(sz),
                    addr.format()
                );
            }

            Aarch64Inst::Stp {
                size,
                src1,
                src2,
                addr,
            } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    stp {}, {}, {}",
                    src1.name_for_size(sz),
                    src2.name_for_size(sz),
                    addr.format()
                );
            }

            Aarch64Inst::Adrp { sym, dst } => {
                let sym_name = sym.format_for_target(target);
                match target.os {
                    Os::MacOS => {
                        let _ = writeln!(out, "    adrp {}, {}@PAGE", dst.name64(), sym_name);
                    }
                    Os::Linux | Os::FreeBSD => {
                        let _ = writeln!(out, "    adrp {}, {}", dst.name64(), sym_name);
                    }
                }
            }

            Aarch64Inst::AddSymOffset { sym, base, dst } => {
                let sym_name = sym.format_for_target(target);
                match target.os {
                    Os::MacOS => {
                        let _ = writeln!(
                            out,
                            "    add {}, {}, {}@PAGEOFF",
                            dst.name64(),
                            base.name64(),
                            sym_name
                        );
                    }
                    Os::Linux | Os::FreeBSD => {
                        let _ = writeln!(
                            out,
                            "    add {}, {}, :lo12:{}",
                            dst.name64(),
                            base.name64(),
                            sym_name
                        );
                    }
                }
            }

            Aarch64Inst::LdrSymOffset {
                size,
                sym,
                base,
                dst,
            } => {
                let sym_name = sym.format_for_target(target);
                let sz = size.bits().max(32);
                let ldr_insn = match size {
                    OperandSize::B8 => "ldrb",
                    OperandSize::B16 => "ldrh",
                    OperandSize::B32 => "ldr",
                    OperandSize::B64 => "ldr",
                };
                match target.os {
                    Os::MacOS => {
                        let _ = writeln!(
                            out,
                            "    {} {}, [{}, {}@PAGEOFF]",
                            ldr_insn,
                            dst.name_for_size(sz),
                            base.name64(),
                            sym_name
                        );
                    }
                    Os::Linux | Os::FreeBSD => {
                        let _ = writeln!(
                            out,
                            "    {} {}, [{}, :lo12:{}]",
                            ldr_insn,
                            dst.name_for_size(sz),
                            base.name64(),
                            sym_name
                        );
                    }
                }
            }

            // Integer Arithmetic
            Aarch64Inst::Add {
                size,
                src1,
                src2,
                dst,
            } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    add {}, {}, {}",
                    dst.name_for_size(sz),
                    src1.name_for_size(sz),
                    src2.format(OperandSize::from_bits(sz))
                );
            }

            Aarch64Inst::Sub {
                size,
                src1,
                src2,
                dst,
            } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    sub {}, {}, {}",
                    dst.name_for_size(sz),
                    src1.name_for_size(sz),
                    src2.format(OperandSize::from_bits(sz))
                );
            }

            Aarch64Inst::Mul {
                size,
                src1,
                src2,
                dst,
            } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    mul {}, {}, {}",
                    dst.name_for_size(sz),
                    src1.name_for_size(sz),
                    src2.name_for_size(sz)
                );
            }

            Aarch64Inst::Sdiv {
                size,
                src1,
                src2,
                dst,
            } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    sdiv {}, {}, {}",
                    dst.name_for_size(sz),
                    src1.name_for_size(sz),
                    src2.name_for_size(sz)
                );
            }

            Aarch64Inst::Udiv {
                size,
                src1,
                src2,
                dst,
            } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    udiv {}, {}, {}",
                    dst.name_for_size(sz),
                    src1.name_for_size(sz),
                    src2.name_for_size(sz)
                );
            }

            Aarch64Inst::Msub {
                size,
                mul1,
                mul2,
                sub,
                dst,
            } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    msub {}, {}, {}, {}",
                    dst.name_for_size(sz),
                    mul1.name_for_size(sz),
                    mul2.name_for_size(sz),
                    sub.name_for_size(sz)
                );
            }

            Aarch64Inst::Neg { size, src, dst } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    neg {}, {}",
                    dst.name_for_size(sz),
                    src.name_for_size(sz)
                );
            }

            // Bitwise Operations
            Aarch64Inst::And {
                size,
                src1,
                src2,
                dst,
            } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    and {}, {}, {}",
                    dst.name_for_size(sz),
                    src1.name_for_size(sz),
                    src2.format(OperandSize::from_bits(sz))
                );
            }

            Aarch64Inst::Orr {
                size,
                src1,
                src2,
                dst,
            } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    orr {}, {}, {}",
                    dst.name_for_size(sz),
                    src1.name_for_size(sz),
                    src2.format(OperandSize::from_bits(sz))
                );
            }

            Aarch64Inst::Eor {
                size,
                src1,
                src2,
                dst,
            } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    eor {}, {}, {}",
                    dst.name_for_size(sz),
                    src1.name_for_size(sz),
                    src2.format(OperandSize::from_bits(sz))
                );
            }

            Aarch64Inst::Mvn { size, src, dst } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    mvn {}, {}",
                    dst.name_for_size(sz),
                    src.name_for_size(sz)
                );
            }

            Aarch64Inst::Lsl {
                size,
                src,
                amount,
                dst,
            } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    lsl {}, {}, {}",
                    dst.name_for_size(sz),
                    src.name_for_size(sz),
                    amount.format(OperandSize::from_bits(sz))
                );
            }

            Aarch64Inst::Lsr {
                size,
                src,
                amount,
                dst,
            } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    lsr {}, {}, {}",
                    dst.name_for_size(sz),
                    src.name_for_size(sz),
                    amount.format(OperandSize::from_bits(sz))
                );
            }

            Aarch64Inst::Asr {
                size,
                src,
                amount,
                dst,
            } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    asr {}, {}, {}",
                    dst.name_for_size(sz),
                    src.name_for_size(sz),
                    amount.format(OperandSize::from_bits(sz))
                );
            }

            Aarch64Inst::Ror {
                size,
                src,
                amount,
                dst,
            } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    ror {}, {}, {}",
                    dst.name_for_size(sz),
                    src.name_for_size(sz),
                    amount.format(OperandSize::from_bits(sz))
                );
            }

            Aarch64Inst::Rev { size, src, dst } => {
                let sz = size.bits().max(32);
                // rev for 32-bit uses w registers, rev for 64-bit uses x registers
                let _ = writeln!(
                    out,
                    "    rev {}, {}",
                    dst.name_for_size(sz),
                    src.name_for_size(sz)
                );
            }

            Aarch64Inst::Rev16 { size, src, dst } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    rev16 {}, {}",
                    dst.name_for_size(sz),
                    src.name_for_size(sz)
                );
            }

            // Comparison and Conditional
            Aarch64Inst::Cmp { size, src1, src2 } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    cmp {}, {}",
                    src1.name_for_size(sz),
                    src2.format(OperandSize::from_bits(sz))
                );
            }

            Aarch64Inst::Tst { size, src1, src2 } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    tst {}, {}",
                    src1.name_for_size(sz),
                    src2.format(OperandSize::from_bits(sz))
                );
            }

            Aarch64Inst::Cset { cond, dst } => {
                let _ = writeln!(out, "    cset {}, {}", dst.name64(), cond.suffix());
            }

            Aarch64Inst::Csel {
                size,
                cond,
                src_true,
                src_false,
                dst,
            } => {
                let sz = size.bits().max(32);
                let _ = writeln!(
                    out,
                    "    csel {}, {}, {}, {}",
                    dst.name_for_size(sz),
                    src_true.name_for_size(sz),
                    src_false.name_for_size(sz),
                    cond.suffix()
                );
            }

            // Sign/Zero Extension
            Aarch64Inst::Sxtb { dst_size, src, dst } => {
                let sz = dst_size.bits().max(32);
                let _ = writeln!(out, "    sxtb {}, {}", dst.name_for_size(sz), src.name32());
            }

            Aarch64Inst::Sxth { dst_size, src, dst } => {
                let sz = dst_size.bits().max(32);
                let _ = writeln!(out, "    sxth {}, {}", dst.name_for_size(sz), src.name32());
            }

            Aarch64Inst::Sxtw { src, dst } => {
                let _ = writeln!(out, "    sxtw {}, {}", dst.name64(), src.name32());
            }

            Aarch64Inst::Uxtb { src, dst } => {
                let _ = writeln!(out, "    uxtb {}, {}", dst.name32(), src.name32());
            }

            Aarch64Inst::Uxth { src, dst } => {
                let _ = writeln!(out, "    uxth {}, {}", dst.name32(), src.name32());
            }

            // Control Flow
            Aarch64Inst::B { target: lbl } => {
                let _ = writeln!(out, "    b {}", lbl.name());
            }

            Aarch64Inst::BCond { cond, target: lbl } => {
                let _ = writeln!(out, "    b.{} {}", cond.suffix(), lbl.name());
            }

            Aarch64Inst::Bl {
                target: call_target,
            } => match call_target {
                CallTarget::Direct(sym) => {
                    let sym_name = sym.format_for_target(target);
                    let _ = writeln!(out, "    bl {}", sym_name);
                }
                CallTarget::Indirect(reg) => {
                    let _ = writeln!(out, "    blr {}", reg.name64());
                }
            },

            Aarch64Inst::Blr { target: reg } => {
                let _ = writeln!(out, "    blr {}", reg.name64());
            }

            Aarch64Inst::Ret => {
                let _ = writeln!(out, "    ret");
            }

            // Floating-Point
            Aarch64Inst::FmovReg { size, src, dst } => {
                let name = match size {
                    FpSize::Single => (src.name_s(), dst.name_s()),
                    FpSize::Double => (src.name_d(), dst.name_d()),
                };
                let _ = writeln!(out, "    fmov {}, {}", name.1, name.0);
            }

            Aarch64Inst::FmovFromGp { size, src, dst } => {
                let fp_name = match size {
                    FpSize::Single => dst.name_s(),
                    FpSize::Double => dst.name_d(),
                };
                let gp_name = match size {
                    FpSize::Single => src.name32(),
                    FpSize::Double => src.name64(),
                };
                let _ = writeln!(out, "    fmov {}, {}", fp_name, gp_name);
            }

            Aarch64Inst::FmovToGp { size, src, dst } => {
                let fp_name = match size {
                    FpSize::Single => src.name_s(),
                    FpSize::Double => src.name_d(),
                };
                let gp_name = match size {
                    FpSize::Single => dst.name32(),
                    FpSize::Double => dst.name64(),
                };
                let _ = writeln!(out, "    fmov {}, {}", gp_name, fp_name);
            }

            Aarch64Inst::LdrFp { size, addr, dst } => {
                let name = match size {
                    FpSize::Single => dst.name_s(),
                    FpSize::Double => dst.name_d(),
                };
                let _ = writeln!(out, "    ldr {}, {}", name, addr.format());
            }

            Aarch64Inst::StrFp { size, src, addr } => {
                let name = match size {
                    FpSize::Single => src.name_s(),
                    FpSize::Double => src.name_d(),
                };
                let _ = writeln!(out, "    str {}, {}", name, addr.format());
            }

            Aarch64Inst::LdrFpSymOffset {
                size,
                sym,
                base,
                dst,
            } => {
                let sym_name = sym.format_for_target(target);
                let fp_name = match size {
                    FpSize::Single => dst.name_s(),
                    FpSize::Double => dst.name_d(),
                };
                match target.os {
                    Os::MacOS => {
                        let _ = writeln!(
                            out,
                            "    ldr {}, [{}, {}@PAGEOFF]",
                            fp_name,
                            base.name64(),
                            sym_name
                        );
                    }
                    Os::Linux | Os::FreeBSD => {
                        let _ = writeln!(
                            out,
                            "    ldr {}, [{}, :lo12:{}]",
                            fp_name,
                            base.name64(),
                            sym_name
                        );
                    }
                }
            }

            Aarch64Inst::Fadd {
                size,
                src1,
                src2,
                dst,
            } => {
                let _ = writeln!(
                    out,
                    "    fadd {}, {}, {}",
                    dst.name_for_size(size_bits(*size)),
                    src1.name_for_size(size_bits(*size)),
                    src2.name_for_size(size_bits(*size))
                );
            }

            Aarch64Inst::Fsub {
                size,
                src1,
                src2,
                dst,
            } => {
                let _ = writeln!(
                    out,
                    "    fsub {}, {}, {}",
                    dst.name_for_size(size_bits(*size)),
                    src1.name_for_size(size_bits(*size)),
                    src2.name_for_size(size_bits(*size))
                );
            }

            Aarch64Inst::Fmul {
                size,
                src1,
                src2,
                dst,
            } => {
                let _ = writeln!(
                    out,
                    "    fmul {}, {}, {}",
                    dst.name_for_size(size_bits(*size)),
                    src1.name_for_size(size_bits(*size)),
                    src2.name_for_size(size_bits(*size))
                );
            }

            Aarch64Inst::Fdiv {
                size,
                src1,
                src2,
                dst,
            } => {
                let _ = writeln!(
                    out,
                    "    fdiv {}, {}, {}",
                    dst.name_for_size(size_bits(*size)),
                    src1.name_for_size(size_bits(*size)),
                    src2.name_for_size(size_bits(*size))
                );
            }

            Aarch64Inst::Fneg { size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    fneg {}, {}",
                    dst.name_for_size(size_bits(*size)),
                    src.name_for_size(size_bits(*size))
                );
            }

            Aarch64Inst::Fcmp { size, src1, src2 } => {
                let _ = writeln!(
                    out,
                    "    fcmp {}, {}",
                    src1.name_for_size(size_bits(*size)),
                    src2.name_for_size(size_bits(*size))
                );
            }

            Aarch64Inst::FcmpZero { size, src } => {
                let _ = writeln!(
                    out,
                    "    fcmp {}, #0.0",
                    src.name_for_size(size_bits(*size))
                );
            }

            Aarch64Inst::Scvtf {
                int_size,
                fp_size,
                src,
                dst,
            } => {
                let fp_name = dst.name_for_size(size_bits(*fp_size));
                let gp_name = src.name_for_size(int_size.bits().max(32));
                let _ = writeln!(out, "    scvtf {}, {}", fp_name, gp_name);
            }

            Aarch64Inst::Ucvtf {
                int_size,
                fp_size,
                src,
                dst,
            } => {
                let fp_name = dst.name_for_size(size_bits(*fp_size));
                let gp_name = src.name_for_size(int_size.bits().max(32));
                let _ = writeln!(out, "    ucvtf {}, {}", fp_name, gp_name);
            }

            Aarch64Inst::Fcvtzs {
                fp_size,
                int_size,
                src,
                dst,
            } => {
                let fp_name = src.name_for_size(size_bits(*fp_size));
                let gp_name = dst.name_for_size(int_size.bits().max(32));
                let _ = writeln!(out, "    fcvtzs {}, {}", gp_name, fp_name);
            }

            Aarch64Inst::Fcvtzu {
                fp_size,
                int_size,
                src,
                dst,
            } => {
                let fp_name = src.name_for_size(size_bits(*fp_size));
                let gp_name = dst.name_for_size(int_size.bits().max(32));
                let _ = writeln!(out, "    fcvtzu {}, {}", gp_name, fp_name);
            }

            Aarch64Inst::Fcvt {
                src_size,
                dst_size,
                src,
                dst,
            } => {
                let _ = writeln!(
                    out,
                    "    fcvt {}, {}",
                    dst.name_for_size(size_bits(*dst_size)),
                    src.name_for_size(size_bits(*src_size))
                );
            }

            // Directives - delegate to shared implementation
            Aarch64Inst::Directive(dir) => {
                dir.emit(target, out);
            }
        }
    }
}

/// Helper to convert FpSize to bits
fn size_bits(size: FpSize) -> u32 {
    match size {
        FpSize::Single => 32,
        FpSize::Double => 64,
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::target::{Arch, Os};

    fn linux_target() -> Target {
        Target::new(Arch::Aarch64, Os::Linux)
    }

    fn macos_target() -> Target {
        Target::new(Arch::Aarch64, Os::MacOS)
    }

    #[test]
    fn test_mov_emit() {
        let target = linux_target();
        let mut out = String::new();

        let inst = Aarch64Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::X1),
            dst: Reg::X0,
        };
        inst.emit(&target, &mut out);
        assert_eq!(out.trim(), "mov x0, x1");
    }

    #[test]
    fn test_mov_imm() {
        let target = linux_target();
        let mut out = String::new();

        let inst = Aarch64Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Imm(42),
            dst: Reg::X0,
        };
        inst.emit(&target, &mut out);
        assert_eq!(out.trim(), "mov w0, #42");
    }

    #[test]
    fn test_ldr_str() {
        let target = linux_target();

        let mut out = String::new();
        let inst = Aarch64Inst::Ldr {
            size: OperandSize::B64,
            addr: MemAddr::BaseOffset {
                base: Reg::X29,
                offset: -8,
            },
            dst: Reg::X0,
        };
        inst.emit(&target, &mut out);
        assert_eq!(out.trim(), "ldr x0, [x29, #-8]");

        let mut out = String::new();
        let inst = Aarch64Inst::Str {
            size: OperandSize::B8,
            src: Reg::X1,
            addr: MemAddr::BaseOffset {
                base: Reg::X29,
                offset: 0,
            },
        };
        inst.emit(&target, &mut out);
        assert_eq!(out.trim(), "strb w1, [x29]");
    }

    #[test]
    fn test_stp_ldp() {
        let target = linux_target();

        // Test stp with SP as base (valid prologue pattern)
        let mut out = String::new();
        let inst = Aarch64Inst::Stp {
            size: OperandSize::B64,
            src1: Reg::X29,
            src2: Reg::X30,
            addr: MemAddr::PreIndex {
                base: Reg::SP,
                offset: -16,
            },
        };
        inst.emit(&target, &mut out);
        assert!(out.contains("stp x29, x30, [sp, #-16]!"));
    }

    #[test]
    fn test_arithmetic() {
        let target = linux_target();

        let mut out = String::new();
        let inst = Aarch64Inst::Add {
            size: OperandSize::B64,
            src1: Reg::X0,
            src2: GpOperand::Imm(100),
            dst: Reg::X0,
        };
        inst.emit(&target, &mut out);
        assert_eq!(out.trim(), "add x0, x0, #100");

        let mut out = String::new();
        let inst = Aarch64Inst::Mul {
            size: OperandSize::B32,
            src1: Reg::X0,
            src2: Reg::X1,
            dst: Reg::X2,
        };
        inst.emit(&target, &mut out);
        assert_eq!(out.trim(), "mul w2, w0, w1");
    }

    #[test]
    fn test_cmp_cset() {
        let target = linux_target();

        let mut out = String::new();
        let inst = Aarch64Inst::Cmp {
            size: OperandSize::B64,
            src1: Reg::X0,
            src2: GpOperand::Imm(0),
        };
        inst.emit(&target, &mut out);
        assert_eq!(out.trim(), "cmp x0, #0");

        let mut out = String::new();
        let inst = Aarch64Inst::Cset {
            cond: Cond::Eq,
            dst: Reg::X0,
        };
        inst.emit(&target, &mut out);
        assert_eq!(out.trim(), "cset x0, eq");
    }

    #[test]
    fn test_branches() {
        let target = linux_target();

        let mut out = String::new();
        let inst = Aarch64Inst::B {
            target: Label::new("main", 1),
        };
        inst.emit(&target, &mut out);
        assert_eq!(out.trim(), "b .Lmain_1");

        let mut out = String::new();
        let inst = Aarch64Inst::BCond {
            cond: Cond::Ne,
            target: Label::new("main", 2),
        };
        inst.emit(&target, &mut out);
        assert_eq!(out.trim(), "b.ne .Lmain_2");
    }

    #[test]
    fn test_fp_instructions() {
        let target = linux_target();

        let mut out = String::new();
        let inst = Aarch64Inst::Fadd {
            size: FpSize::Double,
            src1: VReg::V0,
            src2: VReg::V1,
            dst: VReg::V2,
        };
        inst.emit(&target, &mut out);
        assert_eq!(out.trim(), "fadd d2, d0, d1");

        let mut out = String::new();
        let inst = Aarch64Inst::Scvtf {
            int_size: OperandSize::B32,
            fp_size: FpSize::Double,
            src: Reg::X0,
            dst: VReg::V0,
        };
        inst.emit(&target, &mut out);
        assert_eq!(out.trim(), "scvtf d0, w0");
    }

    #[test]
    fn test_symbol_macos_prefix() {
        let linux = linux_target();
        let macos = macos_target();

        // Test BL with symbol
        let mut out = String::new();
        let inst = Aarch64Inst::Bl {
            target: CallTarget::Direct(Symbol::global("printf")),
        };
        inst.emit(&linux, &mut out);
        assert!(out.contains("bl printf"));

        let mut out = String::new();
        inst.emit(&macos, &mut out);
        assert!(out.contains("bl _printf"));
    }
}
