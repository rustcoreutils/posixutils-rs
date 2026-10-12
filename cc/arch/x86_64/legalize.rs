//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 displacement legalization: every instruction the backend emits
// passes through `legalize`, which rewrites one whose displacement or
// immediate no encoding can hold.
//

//! One rule, one implementation, for x86-64's 32-bit displacements.
//!
//! A memory operand's displacement and an arithmetic immediate are both
//! sign-extended 32-bit fields. Every frame displacement fits one until a
//! frame passes two gigabytes, and then the locals at its far end, the
//! `subq` that allocates it, and the variadic save area above them do not.
//! gcc reaches them through `movabsq`; so does c17.
//!
//! `X86_64CodeGen::push_lir` hands each instruction to [`legalize`]. One that
//! encodes passes through unchanged -- that is the invariant the tests below
//! pin, and why no frame under two gigabytes changes. One that does not is
//! rewritten through the function's *far scratch*: a callee-saved register
//! the allocator withholds from a function whose frame may reach past
//! `i32` (`far_scratch_of` in regalloc.rs), so it holds nothing an instruction
//! could need, and is saved and restored like any other callee-saved
//! register.
//!
//! - a memory operand `disp(%base)` becomes `movabsq $disp, %scratch` and
//!   `(%base,%scratch)`;
//! - an immediate operand becomes `movabsq $imm, %scratch` and the register
//!   form of the instruction.
//!
//! `movabsq` writes no flags, so the rewrite is exact wherever it lands.
//! Neither R10 nor R11 could be the scratch: emitters keep values in them
//! from one instruction to the next.

use super::lir::{GpOperand, MemAddr, X86Inst, XmmOperand};
use super::regalloc::Reg;
use crate::arch::lir::OperandSize;

/// Does `v` fit a sign-extended 32-bit displacement or immediate?
pub(super) fn fits_i32(v: i64) -> bool {
    i32::try_from(v).is_ok()
}

/// Append `inst` to `out`, rewritten through `far` if a displacement or
/// immediate of it does not encode.
///
/// `far` is the function's far scratch, if it has one. An instruction that
/// needs one in a function without it is an internal compiler error: the
/// allocator's bound (`crate::arch::regalloc::frame_reach`) is an upper
/// bound on every frame displacement, so it cannot happen, and printing the
/// displacement would only move the failure to the assembler.
pub(super) fn legalize(mut inst: X86Inst, far: Option<Reg>, out: &mut Vec<X86Inst>) {
    let mut wide: Vec<i64> = Vec::new();
    for addr in memory_operands(&mut inst) {
        if let Some(disp) = far_displacement(addr) {
            wide.push(disp);
            let scratch = scratch(far, "a frame displacement past 32 bits");
            rebase(addr, scratch);
        }
    }
    if let Some(imm) = immediate_operand(&mut inst) {
        if let GpOperand::Imm(v) = *imm {
            if !fits_i32(v) {
                wide.push(v);
                *imm = GpOperand::Reg(scratch(far, "an immediate past 32 bits"));
            }
        }
    }
    match wide.as_slice() {
        [] => {}
        [v] => out.push(X86Inst::MovAbs {
            imm: *v,
            dst: scratch(far, "a displacement past 32 bits"),
        }),
        _ => panic!(
            "internal compiler error: an x86-64 instruction with two operands past 32 bits: {inst:?}"
        ),
    }
    out.push(inst);
}

/// The far scratch, which a rewrite needs.
fn scratch(far: Option<Reg>, what: &str) -> Reg {
    match far {
        Some(reg) => reg,
        None => {
            panic!("internal compiler error: {what} in a function given no far scratch register")
        }
    }
}

/// The displacement of `addr` if it is one no encoding holds.
fn far_displacement(addr: &MemAddr) -> Option<i64> {
    match addr {
        MemAddr::BaseOffset { offset, .. } if !fits_i32(*offset) => Some(*offset),
        MemAddr::Seg { addr, .. } => far_displacement(addr),
        _ => None,
    }
}

/// Rewrite the far `disp(%base)` in `addr` as `(%base,%scratch)`.
fn rebase(addr: &mut MemAddr, scratch: Reg) {
    match addr {
        MemAddr::BaseOffset { base, .. } => {
            *addr = MemAddr::BaseIndex {
                base: *base,
                index: scratch,
            }
        }
        MemAddr::Seg { addr, .. } => rebase(addr, scratch),
        _ => unreachable!("only a base-and-displacement address has a displacement"),
    }
}

/// The memory operands of `inst`.
///
/// Every variant is named, so that a new one has to be placed here: one
/// with a memory operand this does not return would print a displacement
/// the assembler rejects.
fn memory_operands(inst: &mut X86Inst) -> Vec<&mut MemAddr> {
    fn gp(op: &mut GpOperand) -> Option<&mut MemAddr> {
        match op {
            GpOperand::Mem(addr) => Some(addr),
            GpOperand::Reg(_) | GpOperand::Imm(_) => None,
        }
    }
    fn xmm(op: &mut XmmOperand) -> Option<&mut MemAddr> {
        match op {
            XmmOperand::Mem(addr) => Some(addr),
            XmmOperand::Reg(_) => None,
        }
    }
    match inst {
        X86Inst::Mov { src, dst, .. }
        | X86Inst::Cmp { src, dst, .. }
        | X86Inst::Test { src, dst, .. } => gp(src).into_iter().chain(gp(dst)).collect(),
        X86Inst::Movzx { src, .. }
        | X86Inst::Movsx { src, .. }
        | X86Inst::Push { src }
        | X86Inst::Add { src, .. }
        | X86Inst::Sub { src, .. }
        | X86Inst::IMul2 { src, .. }
        | X86Inst::Adc { src, .. }
        | X86Inst::Sbb { src, .. }
        | X86Inst::Mul1 { src, .. }
        | X86Inst::And { src, .. }
        | X86Inst::Or { src, .. }
        | X86Inst::Xor { src, .. }
        | X86Inst::CMov { src, .. }
        | X86Inst::CvtIntToFp { src, .. }
        | X86Inst::Bsf { src, .. }
        | X86Inst::Bsr { src, .. }
        | X86Inst::IDiv { divisor: src, .. }
        | X86Inst::Div { divisor: src, .. } => gp(src).into_iter().collect(),
        X86Inst::MovFp { src, dst, .. } => xmm(src).into_iter().chain(xmm(dst)).collect(),
        X86Inst::Fma4 { add, mul, .. } => xmm(add).into_iter().chain(xmm(mul)).collect(),
        X86Inst::AddFp { src, .. }
        | X86Inst::SubFp { src, .. }
        | X86Inst::MulFp { src, .. }
        | X86Inst::DivFp { src, .. }
        | X86Inst::SqrtFp { src, .. }
        | X86Inst::RoundFp { src, .. }
        | X86Inst::ComiFp { src, .. }
        | X86Inst::CvtFpToInt { src, .. }
        | X86Inst::FmaAddend { addend: src, .. }
        | X86Inst::FmaAccumulate { mul: src, .. } => xmm(src).into_iter().collect(),
        X86Inst::Lea { addr, .. }
        | X86Inst::StackProbe { addr }
        | X86Inst::X87Load { addr }
        | X86Inst::X87Store { addr }
        | X86Inst::X87LoadFloat { addr }
        | X86Inst::X87LoadDouble { addr }
        | X86Inst::X87StoreFloat { addr }
        | X86Inst::X87StoreDouble { addr }
        | X86Inst::X87LoadInt32 { addr }
        | X86Inst::X87LoadInt64 { addr }
        | X86Inst::X87StoreInt { addr, .. }
        | X86Inst::X87StoreControlWord { addr }
        | X86Inst::X87LoadControlWord { addr }
        | X86Inst::Xchg { mem: addr, .. }
        | X86Inst::LockCmpxchg { mem: addr, .. }
        | X86Inst::LockXadd { mem: addr, .. } => vec![addr],
        X86Inst::MovAbs { .. }
        | X86Inst::CallGot { .. }
        | X86Inst::MovSymAddr { .. }
        | X86Inst::Pop { .. }
        | X86Inst::Neg { .. }
        | X86Inst::Not { .. }
        | X86Inst::Shl { .. }
        | X86Inst::Shr { .. }
        | X86Inst::Sar { .. }
        | X86Inst::Shld { .. }
        | X86Inst::Shrd { .. }
        | X86Inst::Ror { .. }
        | X86Inst::SetCC { .. }
        | X86Inst::Jmp { .. }
        | X86Inst::JmpIndirect { .. }
        | X86Inst::Jcc { .. }
        | X86Inst::Call { .. }
        | X86Inst::NotrackCall { .. }
        | X86Inst::TlsDescCall { .. }
        | X86Inst::TlvCall
        | X86Inst::Ret
        | X86Inst::RepStosq
        | X86Inst::RepMovsq
        | X86Inst::Ud2
        | X86Inst::Endbr64
        | X86Inst::ClearRegs { .. }
        | X86Inst::BranchModes { .. }
        | X86Inst::ProfileCall { .. }
        | X86Inst::Rdssp { .. }
        | X86Inst::Incssp { .. }
        | X86Inst::MovGpXmm { .. }
        | X86Inst::MovXmmGp { .. }
        | X86Inst::XorFp { .. }
        | X86Inst::AndFp { .. }
        | X86Inst::CvtFpFp { .. }
        | X86Inst::X87BinOp { .. }
        | X86Inst::X87Neg
        | X86Inst::X87Abs
        | X86Inst::X87Sqrt
        | X86Inst::X87CmpPop { .. }
        | X86Inst::X87Pop
        | X86Inst::Cltd
        | X86Inst::Cqto
        | X86Inst::Bswap { .. }
        | X86Inst::XorpsSelf { .. }
        | X86Inst::Packed { .. }
        | X86Inst::PackedShiftImm { .. }
        | X86Inst::PackedShuffle { .. }
        | X86Inst::Mfence
        | X86Inst::Directive(_) => Vec::new(),
    }
}

/// The operand of `inst` that takes a sign-extended 32-bit immediate, if it
/// has one: the source of a 64-bit arithmetic, logical or compare
/// instruction, and of a store. A `movq` into a register is left alone,
/// since the assembler writes a wide one as `movabsq` itself.
fn immediate_operand(inst: &mut X86Inst) -> Option<&mut GpOperand> {
    match inst {
        X86Inst::Add { size, src, .. }
        | X86Inst::Sub { size, src, .. }
        | X86Inst::Adc { size, src, .. }
        | X86Inst::Sbb { size, src, .. }
        | X86Inst::And { size, src, .. }
        | X86Inst::Or { size, src, .. }
        | X86Inst::Xor { size, src, .. }
        | X86Inst::IMul2 { size, src, .. }
        | X86Inst::Cmp { size, src, .. }
        | X86Inst::Test { size, src, .. }
            if *size == OperandSize::B64 =>
        {
            Some(src)
        }
        X86Inst::Mov {
            size: OperandSize::B64,
            src,
            dst: GpOperand::Mem(_),
        } => Some(src),
        X86Inst::Push { src } => Some(src),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arch::lir::{EmitAsm, FpSize};
    use crate::arch::x86_64::regalloc::XmmReg;
    use crate::target::{Arch, Os, Target};

    const FAR: Option<Reg> = Some(Reg::R15);

    fn run(inst: X86Inst, far: Option<Reg>) -> Vec<X86Inst> {
        let mut out = Vec::new();
        legalize(inst, far, &mut out);
        out
    }

    fn asm(insts: &[X86Inst]) -> String {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let mut s = String::new();
        for i in insts {
            i.emit(&target, &mut s);
        }
        s
    }

    fn rbp(offset: i64) -> MemAddr {
        MemAddr::BaseOffset {
            base: Reg::Rbp,
            offset,
        }
    }

    fn load(offset: i64) -> X86Inst {
        X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(rbp(offset)),
            dst: GpOperand::Reg(Reg::Rax),
        }
    }

    /// The invariant every frame under two gigabytes depends on: an
    /// instruction that encodes comes out alone and unchanged, with a far
    /// scratch or without one.
    #[test]
    fn test_encodable_passes_through() {
        for far in [None, FAR] {
            for offset in [0, -8, i64::from(i32::MIN), i64::from(i32::MAX)] {
                assert_eq!(run(load(offset), far), vec![load(offset)], "{offset}");
            }
            let sub = X86Inst::Sub {
                size: OperandSize::B64,
                src: GpOperand::Imm(i64::from(i32::MAX)),
                dst: Reg::Rsp,
            };
            assert_eq!(run(sub.clone(), far), vec![sub]);
            // A 32-bit operation takes any 32-bit pattern, and a `movq` into
            // a register is the assembler's to widen.
            for inst in [
                X86Inst::And {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(0xFFFF_FFFF),
                    dst: Reg::Rax,
                },
                X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Imm(1 << 40),
                    dst: GpOperand::Reg(Reg::Rax),
                },
            ] {
                assert_eq!(run(inst.clone(), far), vec![inst]);
            }
        }
    }

    #[test]
    fn test_far_displacement_goes_through_the_scratch() {
        let tib = 1i64 << 40;
        assert_eq!(
            asm(&run(load(-tib - 16), FAR)),
            "    movabsq $-1099511627792, %r15\n    movq (%rbp,%r15), %rax\n"
        );
        // Just past either end of the 32-bit field.
        for offset in [i64::from(i32::MIN) - 1, i64::from(i32::MAX) + 1] {
            let out = run(load(offset), FAR);
            assert_eq!(out.len(), 2, "{offset}");
            assert_eq!(
                out[0],
                X86Inst::MovAbs {
                    imm: offset,
                    dst: Reg::R15
                }
            );
        }
        // A store, an address, a probe, an SSE and an x87 access.
        let store = X86Inst::Mov {
            size: OperandSize::B8,
            src: GpOperand::Imm(98),
            dst: GpOperand::Mem(rbp(-tib)),
        };
        assert_eq!(
            asm(&run(store, FAR)),
            "    movabsq $-1099511627776, %r15\n    movb $98, (%rbp,%r15)\n"
        );
        let lea = X86Inst::Lea {
            addr: MemAddr::BaseOffset {
                base: Reg::Rsp,
                offset: -tib,
            },
            dst: Reg::R11,
        };
        assert_eq!(
            asm(&run(lea, FAR)),
            "    movabsq $-1099511627776, %r15\n    leaq (%rsp,%r15), %r11\n"
        );
        for inst in [
            X86Inst::StackProbe { addr: rbp(-tib) },
            X86Inst::MovFp {
                size: FpSize::Double,
                src: XmmOperand::Mem(rbp(-tib)),
                dst: XmmOperand::Reg(XmmReg::Xmm0),
            },
            X86Inst::X87Load { addr: rbp(-tib) },
        ] {
            let out = run(inst, FAR);
            assert_eq!(out.len(), 2);
            assert!(asm(&out).contains("(%rbp,%r15)"), "{}", asm(&out));
        }
    }

    /// The prologue's allocation of a frame past two gigabytes, and the
    /// release of a stacked argument area as large after its call.
    #[test]
    fn test_far_immediate_goes_through_the_scratch() {
        let sub = X86Inst::Sub {
            size: OperandSize::B64,
            src: GpOperand::Imm(1 << 40),
            dst: Reg::Rsp,
        };
        assert_eq!(
            asm(&run(sub, FAR)),
            "    movabsq $1099511627776, %r15\n    subq %r15, %rsp\n"
        );
        let add = X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Imm(i64::from(u32::MAX)),
            dst: Reg::Rsp,
        };
        assert_eq!(
            asm(&run(add, FAR)),
            "    movabsq $4294967295, %r15\n    addq %r15, %rsp\n"
        );
    }

    /// A function the allocator gave no scratch cannot have needed one; one
    /// that does is an internal error, not assembly the assembler refuses.
    #[test]
    #[should_panic(expected = "no far scratch")]
    fn test_far_displacement_without_a_scratch_is_an_ice() {
        run(load(1 << 40), None);
    }
}
