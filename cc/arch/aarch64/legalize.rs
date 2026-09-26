//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 immediate legalization: every instruction the backend emits passes
// through `legalize`, which rewrites one whose immediate no encoding can hold.
//

//! One rule, one implementation, for AArch64's immediate ranges.
//!
//! The backend builds instructions with whatever offset or constant the
//! program needs, and `Aarch64CodeGen::push_lir` hands each one to
//! [`legalize`]. An instruction that already encodes passes through
//! unchanged -- that is the invariant the tests below pin -- and one that
//! does not is expanded through X15, which the register allocator never
//! hands out. X15 never carries a value across an instruction the legalizer
//! could expand. Two other users keep to that rule: an inline-asm memory
//! operand's address, set up last, immediately before the template; and the
//! Initial Exec TLS sequence, which holds the thread pointer in it from the
//! `mrs` to the `add` that follows, neither of which is ever expanded.
//!
//! The ranges, in bytes:
//!
//! - a single load or store, `[base, #off]`: a multiple of the access size up
//!   to 4095 of them, or anything in -256..=255 (the assembler picks
//!   `ldur`/`stur` for those itself);
//! - a pair, `ldp`/`stp`: a multiple of the register size, -64..=63 of them;
//! - load-acquire, store-release and the exclusives: no offset at all;
//! - `add`/`sub` and their flag-setting forms: twelve bits, optionally shifted
//!   left by twelve. The assembler flips a negative one to the opposite
//!   operation (`cmp` to `cmn`), which is exact, so the magnitude is what has
//!   to fit.
//!
//! A pre- or post-indexed address has no expansion that keeps its write-back
//! meaning, so the backend must only ever build encodable ones; one that is
//! not is reported rather than printed.

use super::lir::{Aarch64Inst, GpOperand, MemAddr};
use super::regalloc::Reg;
use crate::arch::lir::{FpSize, OperandSize};

/// The register this module expands through.
pub(super) const LEGALIZE_REG: Reg = Reg::X15;

/// An instruction the legalizer cannot make encodable.
#[derive(Debug, PartialEq)]
pub(super) struct Unencodable(pub &'static str);

/// Append `inst` to `out`, expanded if its immediate does not encode.
///
/// On `Err` the instruction is still appended unchanged, so the caller can
/// report the error and carry on; the assembler would reject it anyway.
pub(super) fn legalize(inst: Aarch64Inst, out: &mut Vec<Aarch64Inst>) -> Result<(), Unencodable> {
    match inst {
        Aarch64Inst::Ldr { size, addr, dst } => {
            let addr = single_addr(addr, size_bytes(size), out)?;
            out.push(Aarch64Inst::Ldr { size, addr, dst });
        }
        Aarch64Inst::Ldrs {
            src_size,
            dst_size,
            addr,
            dst,
        } => {
            let addr = single_addr(addr, size_bytes(src_size), out)?;
            out.push(Aarch64Inst::Ldrs {
                src_size,
                dst_size,
                addr,
                dst,
            });
        }
        Aarch64Inst::Str { size, src, addr } => {
            let addr = single_addr(addr, size_bytes(size), out)?;
            out.push(Aarch64Inst::Str { size, src, addr });
        }
        Aarch64Inst::LdrFp { size, addr, dst } => {
            let addr = single_addr(addr, fp_bytes(size), out)?;
            out.push(Aarch64Inst::LdrFp { size, addr, dst });
        }
        Aarch64Inst::StrFp { size, src, addr } => {
            let addr = single_addr(addr, fp_bytes(size), out)?;
            out.push(Aarch64Inst::StrFp { size, src, addr });
        }
        Aarch64Inst::Ldp {
            size,
            addr,
            dst1,
            dst2,
        } => {
            let addr = pair_addr(addr, pair_bytes(size), out)?;
            out.push(Aarch64Inst::Ldp {
                size,
                addr,
                dst1,
                dst2,
            });
        }
        Aarch64Inst::Stp {
            size,
            src1,
            src2,
            addr,
        } => {
            let addr = pair_addr(addr, pair_bytes(size), out)?;
            out.push(Aarch64Inst::Stp {
                size,
                src1,
                src2,
                addr,
            });
        }
        Aarch64Inst::LdpFp {
            size,
            addr,
            dst1,
            dst2,
        } => {
            let addr = pair_addr(addr, fp_bytes(size), out)?;
            out.push(Aarch64Inst::LdpFp {
                size,
                addr,
                dst1,
                dst2,
            });
        }
        Aarch64Inst::StpFp {
            size,
            src1,
            src2,
            addr,
        } => {
            let addr = pair_addr(addr, fp_bytes(size), out)?;
            out.push(Aarch64Inst::StpFp {
                size,
                src1,
                src2,
                addr,
            });
        }
        Aarch64Inst::Ldar { size, addr, dst } => {
            let addr = bare_addr(addr, out)?;
            out.push(Aarch64Inst::Ldar { size, addr, dst });
        }
        Aarch64Inst::Stlr { size, src, addr } => {
            let addr = bare_addr(addr, out)?;
            out.push(Aarch64Inst::Stlr { size, src, addr });
        }
        Aarch64Inst::Ldaxr { size, addr, dst } => {
            let addr = bare_addr(addr, out)?;
            out.push(Aarch64Inst::Ldaxr { size, addr, dst });
        }
        Aarch64Inst::Stlxr {
            size,
            src,
            addr,
            status,
        } => {
            let addr = bare_addr(addr, out)?;
            out.push(Aarch64Inst::Stlxr {
                size,
                src,
                addr,
                status,
            });
        }
        Aarch64Inst::Add {
            size,
            src1,
            src2: GpOperand::Imm(imm),
            dst,
        } if !add_imm_fits(imm) => add_sub(true, size, src1, imm, dst, out),
        Aarch64Inst::Sub {
            size,
            src1,
            src2: GpOperand::Imm(imm),
            dst,
        } if !add_imm_fits(imm) => add_sub(false, size, src1, imm, dst, out),
        // The flag-setting forms are materialized, never split: a split
        // leaves the flags of the second half alone.
        Aarch64Inst::Adds {
            size,
            src1,
            src2: GpOperand::Imm(imm),
            dst,
        } if !add_imm_fits(imm) => {
            let src2 = materialize(imm, src1, out);
            out.push(Aarch64Inst::Adds {
                size,
                src1,
                src2,
                dst,
            });
        }
        Aarch64Inst::Subs {
            size,
            src1,
            src2: GpOperand::Imm(imm),
            dst,
        } if !add_imm_fits(imm) => {
            let src2 = materialize(imm, src1, out);
            out.push(Aarch64Inst::Subs {
                size,
                src1,
                src2,
                dst,
            });
        }
        Aarch64Inst::Cmp {
            size,
            src1,
            src2: GpOperand::Imm(imm),
        } if !add_imm_fits(imm) => {
            let src2 = materialize(imm, src1, out);
            out.push(Aarch64Inst::Cmp { size, src1, src2 });
        }
        other => out.push(other),
    }
    Ok(())
}

/// Bytes a load or store of `size` moves.
fn size_bytes(size: OperandSize) -> i64 {
    i64::from(size.bits() / 8)
}

/// Bytes a floating load or store of `size` moves.
fn fp_bytes(size: FpSize) -> i64 {
    match size {
        FpSize::Half => 2,
        FpSize::Single => 4,
        FpSize::Double => 8,
        FpSize::Quad | FpSize::Extended => 16,
    }
}

/// Bytes per register of an integer pair, which is printed at 32 bits at
/// least.
fn pair_bytes(size: OperandSize) -> i64 {
    i64::from(size.bits().max(32) / 8)
}

/// Does `off` encode in a single load or store of `bytes`?
pub(super) fn single_offset_fits(off: i64, bytes: i64) -> bool {
    (off >= 0 && off % bytes == 0 && off / bytes <= 4095) || (-256..=255).contains(&off)
}

/// Does `off` encode in a pair load or store of `bytes` per register?
pub(super) fn pair_offset_fits(off: i64, bytes: i64) -> bool {
    off % bytes == 0 && (-64..=63).contains(&(off / bytes))
}

/// Does `imm` encode as an `add`/`sub` immediate, either sign?
pub(super) fn add_imm_fits(imm: i64) -> bool {
    let a = imm.unsigned_abs();
    a <= 0xFFF || (a & 0xFFF == 0 && a >> 12 <= 0xFFF)
}

/// The address of a single load or store, made encodable.
fn single_addr(
    addr: MemAddr,
    bytes: i64,
    out: &mut Vec<Aarch64Inst>,
) -> Result<MemAddr, Unencodable> {
    match addr {
        MemAddr::BaseOffset { base, offset } if !single_offset_fits(offset.into(), bytes) => {
            debug_assert_ne!(base, LEGALIZE_REG);
            let off = i64::from(offset);
            // Keep an encodable low part in the instruction when the high
            // part is one shifted add: two instructions rather than three.
            let lo = off & 0xFFF;
            let hi = off - lo;
            if off > 0 && hi >> 12 <= 0xFFF && single_offset_fits(lo, bytes) {
                out.push(Aarch64Inst::Add {
                    size: OperandSize::B64,
                    src1: base,
                    src2: GpOperand::Imm(hi),
                    dst: LEGALIZE_REG,
                });
                return Ok(MemAddr::BaseOffset {
                    base: LEGALIZE_REG,
                    offset: lo as i32,
                });
            }
            address_into_scratch(base, off, out);
            Ok(MemAddr::Base(LEGALIZE_REG))
        }
        MemAddr::PreIndex { offset, .. } | MemAddr::PostIndex { offset, .. }
            if !(-256..=255).contains(&offset) =>
        {
            Err(Unencodable(
                "a pre- or post-indexed load or store past -256..255",
            ))
        }
        other => Ok(other),
    }
}

/// The address of a pair load or store, made encodable.
fn pair_addr(
    addr: MemAddr,
    bytes: i64,
    out: &mut Vec<Aarch64Inst>,
) -> Result<MemAddr, Unencodable> {
    match addr {
        MemAddr::BaseOffset { base, offset } if !pair_offset_fits(offset.into(), bytes) => {
            debug_assert_ne!(base, LEGALIZE_REG);
            address_into_scratch(base, offset.into(), out);
            Ok(MemAddr::Base(LEGALIZE_REG))
        }
        MemAddr::PreIndex { offset, .. } | MemAddr::PostIndex { offset, .. }
            if !pair_offset_fits(offset.into(), bytes) =>
        {
            Err(Unencodable(
                "a pre- or post-indexed pair load or store past its scaled range",
            ))
        }
        other => Ok(other),
    }
}

/// The address of an acquire, release or exclusive access, which takes none.
fn bare_addr(addr: MemAddr, out: &mut Vec<Aarch64Inst>) -> Result<MemAddr, Unencodable> {
    match addr {
        MemAddr::BaseOffset { base, offset } if offset != 0 => {
            debug_assert_ne!(base, LEGALIZE_REG);
            address_into_scratch(base, offset.into(), out);
            Ok(MemAddr::Base(LEGALIZE_REG))
        }
        MemAddr::PreIndex { .. } | MemAddr::PostIndex { .. } => Err(Unencodable(
            "an acquire, release or exclusive access with a write-back address",
        )),
        other => Ok(other),
    }
}

/// `x15 = base + off`, for any `off`.
fn address_into_scratch(base: Reg, off: i64, out: &mut Vec<Aarch64Inst>) {
    let add = Aarch64Inst::Add {
        size: OperandSize::B64,
        src1: base,
        src2: GpOperand::Imm(off),
        dst: LEGALIZE_REG,
    };
    if add_imm_fits(off) {
        out.push(add);
    } else {
        // The scratch is the destination as well as the materialized
        // immediate; `add_sub` handles that order.
        add_sub(true, OperandSize::B64, base, off, LEGALIZE_REG, out);
    }
}

/// `dst = src1 +/- imm` for an immediate that does not encode.
///
/// Normalized to a magnitude and the matching operation. Under 2^24 it is two
/// immediates, the shifted part first -- no register needed, and the stack
/// pointer only ever moves in one direction. Past that the magnitude goes into
/// X15 and the register form, which the assembler writes as the
/// extended-register form when `sp` is involved.
fn add_sub(
    add: bool,
    size: OperandSize,
    src1: Reg,
    imm: i64,
    dst: Reg,
    out: &mut Vec<Aarch64Inst>,
) {
    let add = add == (imm >= 0);
    let mag = imm.unsigned_abs();
    let make = |src1: Reg, src2: GpOperand| {
        if add {
            Aarch64Inst::Add {
                size,
                src1,
                src2,
                dst,
            }
        } else {
            Aarch64Inst::Sub {
                size,
                src1,
                src2,
                dst,
            }
        }
    };
    if mag < 1 << 24 {
        let hi = (mag & !0xFFF) as i64;
        let lo = (mag & 0xFFF) as i64;
        out.push(make(src1, GpOperand::Imm(hi)));
        if lo != 0 {
            out.push(make(dst, GpOperand::Imm(lo)));
        }
        return;
    }
    debug_assert_ne!(src1, LEGALIZE_REG);
    out.push(Aarch64Inst::Mov {
        size: OperandSize::B64,
        src: GpOperand::Imm(mag as i64),
        dst: LEGALIZE_REG,
    });
    out.push(make(src1, GpOperand::Reg(LEGALIZE_REG)));
}

/// Put `imm` in X15, for a flag-setting instruction's register form.
fn materialize(imm: i64, src1: Reg, out: &mut Vec<Aarch64Inst>) -> GpOperand {
    debug_assert_ne!(src1, LEGALIZE_REG);
    out.push(Aarch64Inst::Mov {
        size: OperandSize::B64,
        src: GpOperand::Imm(imm),
        dst: LEGALIZE_REG,
    });
    GpOperand::Reg(LEGALIZE_REG)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arch::lir::EmitAsm;
    use crate::target::Target;

    fn run(inst: Aarch64Inst) -> Vec<Aarch64Inst> {
        let mut out = Vec::new();
        legalize(inst, &mut out).expect("legalizable");
        out
    }

    fn asm(insts: &[Aarch64Inst]) -> String {
        let target = Target::new(crate::target::Arch::Aarch64, crate::target::Os::Linux);
        let mut s = String::new();
        for i in insts {
            i.emit(&target, &mut s);
        }
        s
    }

    fn ldr(size: OperandSize, off: i32) -> Aarch64Inst {
        Aarch64Inst::Ldr {
            size,
            addr: MemAddr::BaseOffset {
                base: Reg::X29,
                offset: off,
            },
            dst: Reg::X0,
        }
    }

    fn stp(off: i32) -> Aarch64Inst {
        Aarch64Inst::Stp {
            size: OperandSize::B64,
            src1: Reg::X0,
            src2: Reg::X1,
            addr: MemAddr::BaseOffset {
                base: Reg::X29,
                offset: off,
            },
        }
    }

    fn add(src1: Reg, imm: i64, dst: Reg) -> Aarch64Inst {
        Aarch64Inst::Add {
            size: OperandSize::B64,
            src1,
            src2: GpOperand::Imm(imm),
            dst,
        }
    }

    /// Every instruction the legalizer produced encodes by its own rules.
    fn all_encodable(insts: &[Aarch64Inst]) -> bool {
        insts.iter().all(|i| {
            let mut again = Vec::new();
            legalize(i.clone(), &mut again).is_ok() && again.len() == 1
        })
    }

    #[test]
    fn test_single_offset_boundaries() {
        for (bytes, size) in [
            (1, OperandSize::B8),
            (2, OperandSize::B16),
            (4, OperandSize::B32),
            (8, OperandSize::B64),
        ] {
            let top = 4095 * bytes;
            assert_eq!(run(ldr(size, top)).len(), 1, "{bytes}: {top}");
            assert!(run(ldr(size, top + bytes)).len() > 1, "{bytes}");
            assert_eq!(run(ldr(size, -256)).len(), 1);
            assert!(run(ldr(size, -257)).len() > 1);
        }
        // Misaligned: only the unscaled range.
        assert_eq!(run(ldr(OperandSize::B64, 255)).len(), 1);
        assert!(run(ldr(OperandSize::B64, 257)).len() > 1);
        assert_eq!(run(ldr(OperandSize::B32, 4001)).len(), 2);
        assert!(single_offset_fits(65520, 16) && !single_offset_fits(65536, 16));
    }

    #[test]
    fn test_pair_offset_boundaries() {
        for off in [-512, 504, 0, 8] {
            assert_eq!(run(stp(off)).len(), 1, "{off}");
        }
        for off in [-520, 512, 4, 40000] {
            let out = run(stp(off));
            assert!(out.len() > 1, "{off}");
            assert!(all_encodable(&out), "{off}: {}", asm(&out));
        }
        assert!(pair_offset_fits(252, 4) && !pair_offset_fits(256, 4));
        assert!(pair_offset_fits(1008, 16) && !pair_offset_fits(1024, 16));
    }

    #[test]
    fn test_far_load_expands_through_the_scratch() {
        let out = run(ldr(OperandSize::B64, 40000));
        assert_eq!(
            asm(&out),
            "    add x15, x29, #36864\n    ldr x0, [x15, #3136]\n"
        );
        let out = run(ldr(OperandSize::B64, 20 << 20));
        assert!(all_encodable(&out), "{}", asm(&out));
        let out = run(ldr(OperandSize::B64, -40000));
        assert!(all_encodable(&out), "{}", asm(&out));
        assert!(asm(&out).contains("sub x15, x29"), "{}", asm(&out));
    }

    #[test]
    fn test_add_sub_immediates() {
        // Encodable, including negatives the assembler flips.
        for imm in [0, 4095, 4096, 0xFFF000, -5, -4095] {
            assert_eq!(run(add(Reg::X1, imm, Reg::X0)).len(), 1, "{imm}");
        }
        // Split: two immediates, no scratch.
        let out = run(add(Reg::X29, 40272, Reg::X9));
        assert_eq!(
            asm(&out),
            "    add x9, x29, #36864\n    add x9, x9, #3408\n"
        );
        let out = run(add(Reg::X29, -40272, Reg::X9));
        assert_eq!(
            asm(&out),
            "    sub x9, x29, #36864\n    sub x9, x9, #3408\n"
        );
        // Past 2^24: through the scratch, register form.
        let out = run(add(Reg::X29, 20 << 20, Reg::X9));
        assert!(
            asm(&out).ends_with("    add x9, x29, x15\n"),
            "{}",
            asm(&out)
        );
        assert!(all_encodable(&out));
    }

    #[test]
    fn test_stack_pointer_forms() {
        let sub_sp = Aarch64Inst::Sub {
            size: OperandSize::B64,
            src1: Reg::SP,
            src2: GpOperand::Imm(2_000_000_000),
            dst: Reg::SP,
        };
        let out = run(sub_sp);
        assert!(
            asm(&out).ends_with("    sub sp, sp, x15\n"),
            "{}",
            asm(&out)
        );
        let out = run(add(Reg::SP, 4736 * 1000, Reg::SP));
        assert!(all_encodable(&out), "{}", asm(&out));
    }

    #[test]
    fn test_flag_setting_forms_are_materialized() {
        let cmp = Aarch64Inst::Cmp {
            size: OperandSize::B64,
            src1: Reg::X0,
            src2: GpOperand::Imm(0x12345),
        };
        let out = run(cmp);
        assert_eq!(out.len(), 2);
        assert!(asm(&out).ends_with("    cmp x0, x15\n"), "{}", asm(&out));
        let cmp = Aarch64Inst::Cmp {
            size: OperandSize::B64,
            src1: Reg::X0,
            src2: GpOperand::Imm(-5),
        };
        assert_eq!(run(cmp).len(), 1);
    }

    #[test]
    fn test_acquire_and_exclusive_take_no_offset() {
        let ldar = Aarch64Inst::Ldar {
            size: OperandSize::B64,
            addr: MemAddr::BaseOffset {
                base: Reg::X29,
                offset: 16,
            },
            dst: Reg::X0,
        };
        let out = run(ldar);
        assert_eq!(asm(&out), "    add x15, x29, #16\n    ldar x0, [x15]\n");
    }

    #[test]
    fn test_write_back_out_of_range_is_an_error() {
        let bad = Aarch64Inst::Stp {
            size: OperandSize::B64,
            src1: Reg::X29,
            src2: Reg::X30,
            addr: MemAddr::PreIndex {
                base: Reg::SP,
                offset: -1024,
            },
        };
        let mut out = Vec::new();
        assert!(legalize(bad, &mut out).is_err());
    }

    /// The invariant: an instruction that encodes is passed through as is.
    #[test]
    fn test_encodable_input_is_untouched() {
        let inputs = [
            ldr(OperandSize::B64, 32760),
            ldr(OperandSize::B8, 4095),
            ldr(OperandSize::B32, -256),
            stp(-512),
            add(Reg::SP, 4095, Reg::SP),
            add(Reg::X29, 0xFFF000, Reg::X16),
            Aarch64Inst::LdrFp {
                size: FpSize::Quad,
                addr: MemAddr::BaseOffset {
                    base: Reg::X29,
                    offset: 65520,
                },
                dst: super::super::regalloc::VReg::V0,
            },
            Aarch64Inst::Stp {
                size: OperandSize::B64,
                src1: Reg::X29,
                src2: Reg::X30,
                addr: MemAddr::PreIndex {
                    base: Reg::SP,
                    offset: -512,
                },
            },
        ];
        for inst in inputs {
            let before = asm(std::slice::from_ref(&inst));
            let out = run(inst);
            assert_eq!(out.len(), 1);
            assert_eq!(asm(&out), before);
        }
    }
}
