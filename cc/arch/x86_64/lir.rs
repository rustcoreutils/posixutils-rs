//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 Low-level Intermediate Representation (LIR)
//
// Strongly-typed representation of x86-64 assembly instructions.
// Each instruction variant encodes all operands in typed fields,
// enabling peephole optimizations before final assembly emission.
//

use super::regalloc::{Reg, XmmReg};
use crate::arch::lir::{
    CallTarget, CondCode, Directive, EmitAsm, FpSize, Label, OperandSize, Symbol,
};
use crate::target::{Os, Target};
use std::fmt::Write;

// ============================================================================
// Memory Addressing Modes
// ============================================================================

/// x86-64 memory addressing mode
#[derive(Debug, Clone, PartialEq)]
pub enum MemAddr {
    /// [base + offset] - Register indirect with displacement
    BaseOffset { base: Reg, offset: i32 },

    /// [rip + symbol] - PC-relative addressing for position-independent code
    RipRelative(Symbol),

    /// symbol@GOTPCREL(%rip) - GOT-relative addressing for external symbols on macOS
    GotPcrel(Symbol),
}

impl MemAddr {
    /// Format memory operand in AT&T syntax
    pub fn format(&self, target: &Target) -> String {
        match self {
            MemAddr::BaseOffset { base, offset } => {
                if *offset == 0 {
                    format!("({})", base.name64())
                } else {
                    format!("{}({})", offset, base.name64())
                }
            }
            MemAddr::RipRelative(sym) => {
                format!("{}(%rip)", sym.format_for_target(target))
            }
            MemAddr::GotPcrel(sym) => {
                format!("{}@GOTPCREL(%rip)", sym.format_for_target(target))
            }
        }
    }
}

// ============================================================================
// General-Purpose Operands
// ============================================================================

/// x86-64 general-purpose operand (register, memory, or immediate)
#[derive(Debug, Clone, PartialEq)]
pub enum GpOperand {
    /// Register operand
    Reg(Reg),
    /// Memory operand
    Mem(MemAddr),
    /// Immediate integer value
    Imm(i64),
}

impl GpOperand {
    /// Format operand in AT&T syntax for given size
    pub fn format(&self, size: OperandSize, target: &Target) -> String {
        match self {
            GpOperand::Reg(r) => r.name_for_size(size.bits()).to_string(),
            GpOperand::Mem(addr) => addr.format(target),
            GpOperand::Imm(v) => format!("${}", v),
        }
    }
}

// ============================================================================
// XMM (Floating-Point) Operands
// ============================================================================

/// x86-64 XMM operand (register or memory)
#[derive(Debug, Clone, PartialEq)]
pub enum XmmOperand {
    /// XMM register operand
    Reg(XmmReg),
    /// Memory operand
    Mem(MemAddr),
}

impl XmmOperand {
    /// Format operand in AT&T syntax
    pub fn format(&self, target: &Target) -> String {
        match self {
            XmmOperand::Reg(r) => r.name().to_string(),
            XmmOperand::Mem(addr) => addr.format(target),
        }
    }
}

// ============================================================================
// Shift Count
// ============================================================================

/// Shift/rotate count specifier
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShiftCount {
    /// Immediate shift count (0-63)
    Imm(u8),
    /// Use %cl register for count
    Cl,
}

// ============================================================================
// x86-64 LIR Instructions
// ============================================================================

/// x86-64 Low-level IR instruction
#[derive(Debug, Clone)]
pub enum X86Inst {
    // ========================================================================
    // Data Movement
    // ========================================================================
    /// MOV - Move data between registers/memory
    Mov {
        size: OperandSize,
        src: GpOperand,
        dst: GpOperand,
    },

    /// MOVABS - Move 64-bit immediate to register
    MovAbs { imm: i64, dst: Reg },

    /// MOVZX - Move with zero extension
    Movzx {
        src_size: OperandSize,
        dst_size: OperandSize,
        src: GpOperand,
        dst: Reg,
    },

    /// MOVSX - Move with sign extension
    Movsx {
        src_size: OperandSize,
        dst_size: OperandSize,
        src: GpOperand,
        dst: Reg,
    },

    /// LEA - Load effective address
    Lea { addr: MemAddr, dst: Reg },

    /// PUSH - Push value onto stack
    Push { src: GpOperand },

    /// POP - Pop value from stack
    Pop { dst: Reg },

    // ========================================================================
    // Integer Arithmetic
    // ========================================================================
    /// ADD - Integer addition
    Add {
        size: OperandSize,
        src: GpOperand,
        dst: Reg,
    },

    /// SUB - Integer subtraction
    Sub {
        size: OperandSize,
        src: GpOperand,
        dst: Reg,
    },

    /// IMUL - Signed multiply (2-operand form: dst *= src)
    IMul2 {
        size: OperandSize,
        src: GpOperand,
        dst: Reg,
    },

    /// IDIV - Signed divide (RDX:RAX / src -> RAX quotient, RDX remainder)
    IDiv {
        size: OperandSize,
        divisor: GpOperand,
    },

    /// DIV - Unsigned divide (RDX:RAX / src -> RAX quotient, RDX remainder)
    Div {
        size: OperandSize,
        divisor: GpOperand,
    },

    /// NEG - Two's complement negation
    Neg { size: OperandSize, dst: Reg },

    // ========================================================================
    // Bitwise Operations
    // ========================================================================
    /// NOT - One's complement (bitwise NOT)
    Not { size: OperandSize, dst: Reg },

    /// AND - Bitwise AND
    And {
        size: OperandSize,
        src: GpOperand,
        dst: Reg,
    },

    /// OR - Bitwise OR
    Or {
        size: OperandSize,
        src: GpOperand,
        dst: Reg,
    },

    /// XOR - Bitwise exclusive OR
    Xor {
        size: OperandSize,
        src: GpOperand,
        dst: Reg,
    },

    /// SHL/SAL - Shift left
    Shl {
        size: OperandSize,
        count: ShiftCount,
        dst: Reg,
    },

    /// SHR - Logical shift right
    Shr {
        size: OperandSize,
        count: ShiftCount,
        dst: Reg,
    },

    /// SAR - Arithmetic shift right
    Sar {
        size: OperandSize,
        count: ShiftCount,
        dst: Reg,
    },

    /// ROR - Rotate right
    Ror {
        size: OperandSize,
        count: ShiftCount,
        dst: Reg,
    },

    // ========================================================================
    // Comparison and Conditional
    // ========================================================================
    /// CMP - Compare (sets flags based on dst - src)
    Cmp {
        size: OperandSize,
        src: GpOperand,
        dst: GpOperand,
    },

    /// TEST - Test (sets flags based on dst AND src)
    Test {
        size: OperandSize,
        src: GpOperand,
        dst: GpOperand,
    },

    /// SETcc - Set byte based on condition
    SetCC { cc: CondCode, dst: Reg },

    /// CMOVcc - Conditional move
    CMov {
        cc: CondCode,
        size: OperandSize,
        src: GpOperand,
        dst: Reg,
    },

    // ========================================================================
    // Control Flow
    // ========================================================================
    /// JMP - Unconditional jump
    Jmp { target: Label },

    /// Jcc - Conditional jump
    Jcc { cc: CondCode, target: Label },

    /// CALL - Function call
    Call { target: CallTarget<Reg> },

    /// RET - Return from function
    Ret,

    /// UD2 - Undefined instruction (trap)
    /// Used for __builtin_unreachable() to signal unreachable code
    Ud2,

    // ========================================================================
    // Floating-Point (SSE)
    // ========================================================================
    /// MOVSS/MOVSD - Move scalar floating-point
    MovFp {
        size: FpSize,
        src: XmmOperand,
        dst: XmmOperand,
    },

    /// MOVD/MOVQ - Move between GP and XMM registers
    MovGpXmm {
        size: OperandSize,
        src: Reg,
        dst: XmmReg,
    },

    /// MOVD/MOVQ - Move from XMM to GP register
    MovXmmGp {
        size: OperandSize,
        src: XmmReg,
        dst: Reg,
    },

    /// ADDSS/ADDSD - Add scalar floating-point
    AddFp {
        size: FpSize,
        src: XmmOperand,
        dst: XmmReg,
    },

    /// SUBSS/SUBSD - Subtract scalar floating-point
    SubFp {
        size: FpSize,
        src: XmmOperand,
        dst: XmmReg,
    },

    /// MULSS/MULSD - Multiply scalar floating-point
    MulFp {
        size: FpSize,
        src: XmmOperand,
        dst: XmmReg,
    },

    /// DIVSS/DIVSD - Divide scalar floating-point
    DivFp {
        size: FpSize,
        src: XmmOperand,
        dst: XmmReg,
    },

    /// XORPS/XORPD - XOR packed floating-point (used for zeroing/negation)
    XorFp {
        size: FpSize,
        src: XmmReg,
        dst: XmmReg,
    },

    /// UCOMISS/UCOMISD - Unordered compare scalar floating-point
    UComiFp {
        size: FpSize,
        src: XmmOperand,
        dst: XmmReg,
    },

    /// CVTSI2SS/CVTSI2SD - Convert integer to scalar floating-point
    CvtIntToFp {
        int_size: OperandSize,
        fp_size: FpSize,
        src: GpOperand,
        dst: XmmReg,
    },

    /// CVTTSS2SI/CVTTSD2SI - Convert scalar FP to integer (truncate toward zero)
    CvtFpToInt {
        fp_size: FpSize,
        int_size: OperandSize,
        src: XmmOperand,
        dst: Reg,
    },

    /// CVTSS2SD/CVTSD2SS - Convert between FP sizes
    CvtFpFp {
        src_size: FpSize,
        dst_size: FpSize,
        src: XmmReg,
        dst: XmmReg,
    },

    // ========================================================================
    // Special Instructions
    // ========================================================================
    /// CLTD/CDQ - Sign extend EAX into EDX:EAX (32-bit)
    Cltd,

    /// CQTO/CQO - Sign extend RAX into RDX:RAX (64-bit)
    Cqto,

    /// BSWAP - Byte swap
    Bswap { size: OperandSize, reg: Reg },

    /// BSF - Bit scan forward (find lowest set bit)
    /// Returns index of least significant set bit
    /// Result is undefined if src is 0
    Bsf {
        size: OperandSize,
        src: GpOperand,
        dst: Reg,
    },

    /// BSR - Bit scan reverse (find highest set bit)
    /// Returns index of most significant set bit
    /// Result is undefined if src is 0
    Bsr {
        size: OperandSize,
        src: GpOperand,
        dst: Reg,
    },

    /// POPCNT - Population count (count set bits)
    /// Returns the number of 1 bits in the source operand
    Popcnt {
        size: OperandSize,
        src: GpOperand,
        dst: Reg,
    },

    /// XORPS with same register - Fast zero XMM register
    XorpsSelf { reg: XmmReg },

    // ========================================================================
    // Directives (Architecture-Independent)
    // ========================================================================
    /// Assembler directives (labels, sections, CFI, .loc, data, etc.)
    /// These are architecture-independent and use the shared Directive type.
    Directive(Directive),
}

// ============================================================================
// LirInst Implementation
// ============================================================================

impl crate::arch::lir::LirInst for X86Inst {
    fn from_directive(dir: Directive) -> Self {
        X86Inst::Directive(dir)
    }
}

// ============================================================================
// EmitAsm Implementation
// ============================================================================

impl EmitAsm for X86Inst {
    fn emit(&self, target: &Target, out: &mut String) {
        match self {
            // Data Movement
            X86Inst::Mov { size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    mov{} {}, {}",
                    size.x86_suffix(),
                    src.format(*size, target),
                    dst.format(*size, target)
                );
            }

            X86Inst::MovAbs { imm, dst } => {
                let _ = writeln!(out, "    movabsq ${}, {}", imm, dst.name64());
            }

            X86Inst::Movzx {
                src_size,
                dst_size,
                src,
                dst,
            } => {
                let op = match (src_size, dst_size) {
                    (OperandSize::B8, OperandSize::B16) => "movzbw",
                    (OperandSize::B8, OperandSize::B32) => "movzbl",
                    (OperandSize::B8, OperandSize::B64) => "movzbq",
                    (OperandSize::B16, OperandSize::B32) => "movzwl",
                    (OperandSize::B16, OperandSize::B64) => "movzwq",
                    _ => "movzbl", // fallback
                };
                let _ = writeln!(
                    out,
                    "    {} {}, {}",
                    op,
                    src.format(*src_size, target),
                    dst.name_for_size(dst_size.bits())
                );
            }

            X86Inst::Movsx {
                src_size,
                dst_size,
                src,
                dst,
            } => {
                let op = match (src_size, dst_size) {
                    (OperandSize::B8, OperandSize::B16) => "movsbw",
                    (OperandSize::B8, OperandSize::B32) => "movsbl",
                    (OperandSize::B8, OperandSize::B64) => "movsbq",
                    (OperandSize::B16, OperandSize::B32) => "movswl",
                    (OperandSize::B16, OperandSize::B64) => "movswq",
                    (OperandSize::B32, OperandSize::B64) => "movslq",
                    _ => "movsbl", // fallback
                };
                let _ = writeln!(
                    out,
                    "    {} {}, {}",
                    op,
                    src.format(*src_size, target),
                    dst.name_for_size(dst_size.bits())
                );
            }

            X86Inst::Lea { addr, dst } => {
                let _ = writeln!(out, "    leaq {}, {}", addr.format(target), dst.name64());
            }

            X86Inst::Push { src } => {
                let _ = writeln!(out, "    pushq {}", src.format(OperandSize::B64, target));
            }

            X86Inst::Pop { dst } => {
                let _ = writeln!(out, "    popq {}", dst.name64());
            }

            // Integer Arithmetic
            X86Inst::Add { size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    add{} {}, {}",
                    size.x86_suffix(),
                    src.format(*size, target),
                    dst.name_for_size(size.bits())
                );
            }

            X86Inst::Sub { size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    sub{} {}, {}",
                    size.x86_suffix(),
                    src.format(*size, target),
                    dst.name_for_size(size.bits())
                );
            }

            X86Inst::IMul2 { size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    imul{} {}, {}",
                    size.x86_suffix(),
                    src.format(*size, target),
                    dst.name_for_size(size.bits())
                );
            }

            X86Inst::IDiv { size, divisor } => {
                let _ = writeln!(
                    out,
                    "    idiv{} {}",
                    size.x86_suffix(),
                    divisor.format(*size, target)
                );
            }

            X86Inst::Div { size, divisor } => {
                let _ = writeln!(
                    out,
                    "    div{} {}",
                    size.x86_suffix(),
                    divisor.format(*size, target)
                );
            }

            X86Inst::Neg { size, dst } => {
                let _ = writeln!(
                    out,
                    "    neg{} {}",
                    size.x86_suffix(),
                    dst.name_for_size(size.bits())
                );
            }

            // Bitwise Operations
            X86Inst::Not { size, dst } => {
                let _ = writeln!(
                    out,
                    "    not{} {}",
                    size.x86_suffix(),
                    dst.name_for_size(size.bits())
                );
            }

            X86Inst::And { size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    and{} {}, {}",
                    size.x86_suffix(),
                    src.format(*size, target),
                    dst.name_for_size(size.bits())
                );
            }

            X86Inst::Or { size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    or{} {}, {}",
                    size.x86_suffix(),
                    src.format(*size, target),
                    dst.name_for_size(size.bits())
                );
            }

            X86Inst::Xor { size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    xor{} {}, {}",
                    size.x86_suffix(),
                    src.format(*size, target),
                    dst.name_for_size(size.bits())
                );
            }

            X86Inst::Shl { size, count, dst } => {
                let count_str = match count {
                    ShiftCount::Imm(n) => format!("${}", n),
                    ShiftCount::Cl => "%cl".to_string(),
                };
                let _ = writeln!(
                    out,
                    "    shl{} {}, {}",
                    size.x86_suffix(),
                    count_str,
                    dst.name_for_size(size.bits())
                );
            }

            X86Inst::Shr { size, count, dst } => {
                let count_str = match count {
                    ShiftCount::Imm(n) => format!("${}", n),
                    ShiftCount::Cl => "%cl".to_string(),
                };
                let _ = writeln!(
                    out,
                    "    shr{} {}, {}",
                    size.x86_suffix(),
                    count_str,
                    dst.name_for_size(size.bits())
                );
            }

            X86Inst::Sar { size, count, dst } => {
                let count_str = match count {
                    ShiftCount::Imm(n) => format!("${}", n),
                    ShiftCount::Cl => "%cl".to_string(),
                };
                let _ = writeln!(
                    out,
                    "    sar{} {}, {}",
                    size.x86_suffix(),
                    count_str,
                    dst.name_for_size(size.bits())
                );
            }

            X86Inst::Ror { size, count, dst } => {
                let count_str = match count {
                    ShiftCount::Imm(n) => format!("${}", n),
                    ShiftCount::Cl => "%cl".to_string(),
                };
                let _ = writeln!(
                    out,
                    "    ror{} {}, {}",
                    size.x86_suffix(),
                    count_str,
                    dst.name_for_size(size.bits())
                );
            }

            // Comparison and Conditional
            X86Inst::Cmp { size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    cmp{} {}, {}",
                    size.x86_suffix(),
                    src.format(*size, target),
                    dst.format(*size, target)
                );
            }

            X86Inst::Test { size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    test{} {}, {}",
                    size.x86_suffix(),
                    src.format(*size, target),
                    dst.format(*size, target)
                );
            }

            X86Inst::SetCC { cc, dst } => {
                let _ = writeln!(out, "    set{} {}", cc.x86_suffix(), dst.name8());
            }

            X86Inst::CMov { cc, size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    cmov{}{} {}, {}",
                    cc.x86_suffix(),
                    size.x86_suffix(),
                    src.format(*size, target),
                    dst.name_for_size(size.bits())
                );
            }

            // Control Flow
            X86Inst::Jmp { target: lbl } => {
                let _ = writeln!(out, "    jmp {}", lbl.name());
            }

            X86Inst::Jcc { cc, target: lbl } => {
                let _ = writeln!(out, "    j{} {}", cc.x86_suffix(), lbl.name());
            }

            X86Inst::Call {
                target: call_target,
            } => match call_target {
                CallTarget::Direct(sym) => {
                    // On Linux/FreeBSD, external symbols need @PLT for PIC
                    let sym_name = sym.format_for_target(target);
                    match target.os {
                        Os::Linux | Os::FreeBSD if !sym.is_local => {
                            let _ = writeln!(out, "    call {}@PLT", sym_name);
                        }
                        _ => {
                            let _ = writeln!(out, "    call {}", sym_name);
                        }
                    }
                }
                CallTarget::Indirect(reg) => {
                    // Indirect call through register: call *%reg
                    let _ = writeln!(out, "    call *{}", reg.name64());
                }
            },

            X86Inst::Ret => {
                let _ = writeln!(out, "    ret");
            }

            X86Inst::Ud2 => {
                let _ = writeln!(out, "    ud2");
            }

            // Floating-Point
            X86Inst::MovFp { size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    mov{} {}, {}",
                    size.x86_suffix(),
                    src.format(target),
                    dst.format(target)
                );
            }

            X86Inst::MovGpXmm { size, src, dst } => {
                let op = if size.bits() <= 32 { "movd" } else { "movq" };
                let _ = writeln!(
                    out,
                    "    {} {}, {}",
                    op,
                    src.name_for_size(size.bits()),
                    dst.name()
                );
            }

            X86Inst::MovXmmGp { size, src, dst } => {
                let op = if size.bits() <= 32 { "movd" } else { "movq" };
                let _ = writeln!(
                    out,
                    "    {} {}, {}",
                    op,
                    src.name(),
                    dst.name_for_size(size.bits())
                );
            }

            X86Inst::AddFp { size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    add{} {}, {}",
                    size.x86_suffix(),
                    src.format(target),
                    dst.name()
                );
            }

            X86Inst::SubFp { size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    sub{} {}, {}",
                    size.x86_suffix(),
                    src.format(target),
                    dst.name()
                );
            }

            X86Inst::MulFp { size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    mul{} {}, {}",
                    size.x86_suffix(),
                    src.format(target),
                    dst.name()
                );
            }

            X86Inst::DivFp { size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    div{} {}, {}",
                    size.x86_suffix(),
                    src.format(target),
                    dst.name()
                );
            }

            X86Inst::XorFp { size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    xor{} {}, {}",
                    size.x86_packed_suffix(),
                    src.name(),
                    dst.name()
                );
            }

            X86Inst::UComiFp { size, src, dst } => {
                let _ = writeln!(
                    out,
                    "    ucomi{} {}, {}",
                    size.x86_suffix(),
                    src.format(target),
                    dst.name()
                );
            }

            X86Inst::CvtIntToFp {
                int_size,
                fp_size,
                src,
                dst,
            } => {
                let int_suffix = if int_size.bits() <= 32 { "l" } else { "q" };
                let _ = writeln!(
                    out,
                    "    cvtsi2{}{} {}, {}",
                    fp_size.x86_suffix(),
                    int_suffix,
                    src.format(*int_size, target),
                    dst.name()
                );
            }

            X86Inst::CvtFpToInt {
                fp_size,
                int_size,
                src,
                dst,
            } => {
                let int_suffix = if int_size.bits() <= 32 { "l" } else { "q" };
                let _ = writeln!(
                    out,
                    "    cvtt{}2si{} {}, {}",
                    fp_size.x86_suffix(),
                    int_suffix,
                    src.format(target),
                    dst.name_for_size(int_size.bits())
                );
            }

            X86Inst::CvtFpFp {
                src_size,
                dst_size,
                src,
                dst,
            } => {
                let _ = writeln!(
                    out,
                    "    cvt{}2{} {}, {}",
                    src_size.x86_suffix(),
                    dst_size.x86_suffix(),
                    src.name(),
                    dst.name()
                );
            }

            // Special Instructions
            X86Inst::Cltd => {
                let _ = writeln!(out, "    cltd");
            }

            X86Inst::Cqto => {
                let _ = writeln!(out, "    cqto");
            }

            X86Inst::Bswap { size, reg } => {
                if size.bits() == 16 {
                    // x86 bswap doesn't work for 16-bit, use xchg or rol
                    let _ = writeln!(out, "    rolw $8, {}", reg.name16());
                } else {
                    let _ = writeln!(
                        out,
                        "    bswap{}",
                        if size.bits() == 32 {
                            format!(" {}", reg.name32())
                        } else {
                            format!(" {}", reg.name64())
                        }
                    );
                }
            }

            X86Inst::Bsf { size, src, dst } => {
                // BSF (bit scan forward) finds the index of the least significant set bit
                // Using "rep bsf" which is TZCNT on BMI1-capable CPUs, BSF on older CPUs
                // TZCNT has defined behavior for 0 (returns operand size), BSF doesn't
                // Since __builtin_ctz has undefined behavior for 0, either is fine
                let _ = writeln!(
                    out,
                    "    bsf{} {}, {}",
                    size.x86_suffix(),
                    src.format(*size, target),
                    dst.name_for_size(size.bits())
                );
            }

            X86Inst::Bsr { size, src, dst } => {
                // BSR (bit scan reverse) finds the index of the most significant set bit
                // Result is undefined if src is 0
                // Since __builtin_clz has undefined behavior for 0, this is fine
                let _ = writeln!(
                    out,
                    "    bsr{} {}, {}",
                    size.x86_suffix(),
                    src.format(*size, target),
                    dst.name_for_size(size.bits())
                );
            }

            X86Inst::Popcnt { size, src, dst } => {
                // POPCNT counts the number of set bits
                // Requires SSE4.2 or POPCNT feature (AMD ABM)
                let _ = writeln!(
                    out,
                    "    popcnt{} {}, {}",
                    size.x86_suffix(),
                    src.format(*size, target),
                    dst.name_for_size(size.bits())
                );
            }

            X86Inst::XorpsSelf { reg } => {
                let _ = writeln!(out, "    xorps {}, {}", reg.name(), reg.name());
            }

            // Directives - delegate to shared implementation
            X86Inst::Directive(dir) => {
                dir.emit(target, out);
            }
        }
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
        Target::new(Arch::X86_64, Os::Linux)
    }

    fn macos_target() -> Target {
        Target::new(Arch::X86_64, Os::MacOS)
    }

    #[test]
    fn test_mov_emit() {
        let target = linux_target();
        let mut out = String::new();

        let inst = X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rax),
            dst: GpOperand::Reg(Reg::Rbx),
        };
        inst.emit(&target, &mut out);
        assert_eq!(out.trim(), "movq %rax, %rbx");
    }

    #[test]
    fn test_mov_sizes() {
        let target = linux_target();

        // Test all sizes
        for (size, expected_suffix) in [
            (OperandSize::B8, "b"),
            (OperandSize::B16, "w"),
            (OperandSize::B32, "l"),
            (OperandSize::B64, "q"),
        ] {
            let mut out = String::new();
            let inst = X86Inst::Mov {
                size,
                src: GpOperand::Imm(42),
                dst: GpOperand::Reg(Reg::Rax),
            };
            inst.emit(&target, &mut out);
            assert!(
                out.contains(&format!("mov{}", expected_suffix)),
                "Expected mov{}, got: {}",
                expected_suffix,
                out
            );
        }
    }

    #[test]
    fn test_mem_addr_format() {
        let target = linux_target();

        let addr = MemAddr::BaseOffset {
            base: Reg::Rbp,
            offset: -8,
        };
        assert_eq!(addr.format(&target), "-8(%rbp)");

        let addr = MemAddr::BaseOffset {
            base: Reg::Rsp,
            offset: 0,
        };
        assert_eq!(addr.format(&target), "(%rsp)");

        let addr = MemAddr::RipRelative(Symbol::global("printf"));
        assert_eq!(addr.format(&target), "printf(%rip)");
    }

    #[test]
    fn test_symbol_macos_prefix() {
        let linux = linux_target();
        let macos = macos_target();

        let sym = Symbol::global("main");
        assert_eq!(sym.format_for_target(&linux), "main");
        assert_eq!(sym.format_for_target(&macos), "_main");

        let mut out = String::new();
        let inst = X86Inst::Call {
            target: CallTarget::Direct(Symbol::global("printf")),
        };
        inst.emit(&macos, &mut out);
        assert!(out.contains("_printf"));
    }

    #[test]
    fn test_conditional_jump() {
        let target = linux_target();

        for (cc, expected) in [
            (CondCode::Eq, "je"),
            (CondCode::Ne, "jne"),
            (CondCode::Slt, "jl"),
            (CondCode::Sgt, "jg"),
            (CondCode::Ult, "jb"),
            (CondCode::Ugt, "ja"),
        ] {
            let mut out = String::new();
            let inst = X86Inst::Jcc {
                cc,
                target: Label::new("test", 1),
            };
            inst.emit(&target, &mut out);
            assert!(
                out.contains(expected),
                "Expected {}, got: {}",
                expected,
                out
            );
        }
    }

    #[test]
    fn test_fp_instructions() {
        let target = linux_target();

        let mut out = String::new();
        let inst = X86Inst::AddFp {
            size: FpSize::Double,
            src: XmmOperand::Reg(XmmReg::Xmm1),
            dst: XmmReg::Xmm0,
        };
        inst.emit(&target, &mut out);
        assert!(out.contains("addsd"));

        let mut out = String::new();
        let inst = X86Inst::AddFp {
            size: FpSize::Single,
            src: XmmOperand::Reg(XmmReg::Xmm1),
            dst: XmmReg::Xmm0,
        };
        inst.emit(&target, &mut out);
        assert!(out.contains("addss"));
    }
}
