//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 Microsoft x64 calling convention (`__attribute__((ms_abi))`):
// the calls to such a function, and the entry, exit and `va_start` of one.
//
// The classification -- which argument is a value, which a pointer to a
// copy, how the value comes back -- is `abi::Win64Abi`'s, carried on each
// call as `CallAbiInfo` and recomputed for a definition's own parameters.
// What lives here is only where the convention puts things: argument
// position `n` in RCX/RDX/R8/R9 or XMM0-XMM3 when `n < 4` and in the
// eight-byte slot at `n * 8` of the outgoing area otherwise, the first four
// slots being the callee's shadow area.
//
// The callee side homes every register position into its shadow slot on
// entry, so each parameter lives at `16 + 8n(%rbp)` for the whole function
// and nothing has to resolve which argument register it arrived in. The
// caller side does the converse: it writes every argument to its slot, then
// loads the register positions from their slots, which sidesteps the
// parallel-move problem of an argument sitting in another one's register.
//

use super::codegen::X86_64CodeGen;
use super::lir::{GpOperand, MemAddr, X86Inst, XmmOperand};
use super::regalloc::{Reg, XmmReg};
use crate::abi::{
    Abi, ArgClass, RegClass, Win64Abi, WIN64_POSITION_BYTES, WIN64_REG_POSITIONS,
    WIN64_SHADOW_BYTES,
};
use crate::arch::lir::{Directive, FpSize, OperandSize};
use crate::ir::{Function, Instruction, PseudoId};
use crate::types::{TypeId, TypeKind, TypeTable};

/// The integer register of each register position.
pub(super) const GP_ARGS: [Reg; WIN64_REG_POSITIONS] = [Reg::Rcx, Reg::Rdx, Reg::R8, Reg::R9];

/// The XMM register of each register position.
pub(super) const XMM_ARGS: [XmmReg; WIN64_REG_POSITIONS] =
    [XmmReg::Xmm0, XmmReg::Xmm1, XmmReg::Xmm2, XmmReg::Xmm3];

/// The general registers Win64 makes callee-saved and System V does not.
/// An `ms_abi` function saves them unconditionally: the back end reaches for
/// RDI and RSI implicitly (`rep movsq`, System V argument setup), so whether
/// one is written is not something the allocator alone knows.
pub(super) const CALLEE_SAVED_GP: [Reg; 2] = [Reg::Rsi, Reg::Rdi];

/// The XMM registers Win64 makes callee-saved -- all of their 128 bits.
pub(super) const CALLEE_SAVED_XMM: [XmmReg; 10] = [
    XmmReg::Xmm6,
    XmmReg::Xmm7,
    XmmReg::Xmm8,
    XmmReg::Xmm9,
    XmmReg::Xmm10,
    XmmReg::Xmm11,
    XmmReg::Xmm12,
    XmmReg::Xmm13,
    XmmReg::Xmm14,
    XmmReg::Xmm15,
];

/// Where argument position `n` arrives, as an `%rbp` displacement in the
/// callee: past the saved `%rbp` and the return address, in the caller's
/// outgoing area -- the shadow slot for a register position.
pub(super) fn incoming_offset(position: usize) -> i32 {
    16 + (position * WIN64_POSITION_BYTES) as i32
}

/// The register a position's value travels in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum PositionReg {
    Gp(Reg),
    Xmm(XmmReg),
}

/// Whether a value of class `class` travels in the XMM file.
fn is_sse(class: &ArgClass) -> bool {
    matches!(class, ArgClass::Direct { classes, .. } if classes.as_slice() == [RegClass::Sse])
}

/// The register position `position` of a value of class `class` arrives
/// in, or `None` for a stacked one.
fn position_reg(position: usize, class: &ArgClass) -> Option<PositionReg> {
    if position >= WIN64_REG_POSITIONS {
        return None;
    }
    Some(if is_sse(class) {
        PositionReg::Xmm(XMM_ARGS[position])
    } else {
        PositionReg::Gp(GP_ARGS[position])
    })
}

/// The register positions an `ms_abi` function spills to its shadow area on
/// entry, each with the register it arrived in.
///
/// Each parameter's own -- the hidden return pointer counts as position 0 --
/// and, for a variadic function, every general register past them: a
/// variadic floating-point argument is passed in both files, and
/// `__builtin_va_arg` reads every position from memory.
pub(super) fn homed_positions(
    func: &Function,
    types: &TypeTable,
    variadic: bool,
) -> Vec<(usize, PositionReg)> {
    let abi = Win64Abi::new();
    let sret = func.sret_arg().is_some();
    let mut classes: Vec<ArgClass> = Vec::with_capacity(func.params.len() + 1);
    if sret {
        classes.push(abi.classify_param(types.void_ptr_id, types));
    }
    classes.extend(
        func.params
            .iter()
            .map(|(_, t)| abi.classify_param(*t, types)),
    );
    let named = classes.len();
    let mut homes: Vec<(usize, PositionReg)> = classes
        .iter()
        .enumerate()
        .filter_map(|(n, class)| position_reg(n, class).map(|r| (n, r)))
        .collect();
    if variadic {
        homes.extend((named..WIN64_REG_POSITIONS).map(|n| (n, PositionReg::Gp(GP_ARGS[n]))));
    }
    homes
}

impl X86_64CodeGen {
    /// Spill the register positions to their shadow slots: the first thing
    /// an `ms_abi` function does, before anything can overwrite them.
    pub(super) fn emit_win64_homing(&mut self, homes: &[(usize, PositionReg)]) {
        for &(position, reg) in homes {
            let slot = MemAddr::BaseOffset {
                base: Reg::Rbp,
                offset: incoming_offset(position),
            };
            match reg {
                PositionReg::Gp(r) => self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(r),
                    dst: GpOperand::Mem(slot),
                }),
                PositionReg::Xmm(x) => self.push_lir(X86Inst::MovFp {
                    size: FpSize::Double,
                    src: XmmOperand::Reg(x),
                    dst: XmmOperand::Mem(slot),
                }),
            }
        }
    }

    /// The `%rbp` displacement of the save slot of the `k`th saved XMM
    /// register: sixteen-byte slots below the pushed general registers, at a
    /// sixteen-byte aligned address since `%rbp` is one.
    fn win64_xmm_slot(&self, k: usize) -> i32 {
        let gp_pushed = (self.callee_saved_regs.len() as i32 * 8 + 15) & !15;
        -(gp_pushed + 16 * (k as i32 + 1))
    }

    /// The bytes the XMM save area adds below the pushed registers,
    /// including the padding that makes it start sixteen-byte aligned.
    pub(super) fn win64_xmm_area_bytes(&self) -> i32 {
        if self.win64_xmm_saves.is_empty() {
            return 0;
        }
        let gp_bytes = self.callee_saved_regs.len() as i32 * 8;
        ((gp_bytes + 15) & !15) - gp_bytes + 16 * self.win64_xmm_saves.len() as i32
    }

    /// Reserve the XMM save area and save every register in it, with the
    /// unwind rule for each: whole registers, as Win64 makes all 128 bits
    /// callee-saved.
    pub(super) fn emit_win64_xmm_saves(&mut self) {
        let area = self.win64_xmm_area_bytes();
        if area == 0 {
            return;
        }
        self.push_lir(X86Inst::Sub {
            size: OperandSize::B64,
            src: GpOperand::Imm(area as i64),
            dst: Reg::Rsp,
        });
        for (k, xmm) in self.win64_xmm_saves.clone().into_iter().enumerate() {
            let offset = self.win64_xmm_slot(k);
            self.push_lir(X86Inst::MovFp {
                size: FpSize::Quad,
                src: XmmOperand::Reg(xmm),
                dst: XmmOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset,
                }),
            });
            if self.base.emit_debug {
                // The CFA is `%rbp + 16` from here on.
                self.push_lir(X86Inst::Directive(Directive::cfi_offset(
                    xmm.dwarf_number().to_string(),
                    offset - 16,
                )));
            }
        }
    }

    /// Reload the saved XMM registers, ahead of the epilogue proper.
    pub(super) fn emit_win64_xmm_restores(&mut self) {
        for (k, xmm) in self.win64_xmm_saves.clone().into_iter().enumerate() {
            let offset = self.win64_xmm_slot(k);
            self.push_lir(X86Inst::MovFp {
                size: FpSize::Quad,
                src: XmmOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset,
                }),
                dst: XmmOperand::Reg(xmm),
            });
        }
    }

    /// `__builtin_ms_va_start(ap, last)`: point `ap` at the first position
    /// past the named ones. Every position is in memory by now -- the
    /// register ones homed on entry -- so the list is a plain pointer.
    pub(super) fn emit_win64_va_start(&mut self, insn: &Instruction) {
        let Some(&ap) = insn.src.first() else {
            return;
        };
        self.push_lir(X86Inst::Lea {
            addr: MemAddr::BaseOffset {
                base: Reg::Rbp,
                offset: self.named_incoming_end,
            },
            dst: Reg::R11,
        });
        let addr = self.scalar_mem_operand(ap, 0, Reg::R10);
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst: GpOperand::Mem(addr),
        });
    }

    /// A call to an `ms_abi` function.
    ///
    /// Every argument is first written to its slot of the outgoing area --
    /// the register positions to the shadow slots, which the callee owns and
    /// may overwrite but has not yet -- and the register positions are then
    /// loaded from there. A floating-point register argument also goes in
    /// the integer register of its position, which the convention asks for
    /// whenever the callee may be variadic -- a variadic or unprototyped one
    /// reads every position from its shadow area, where it spills the
    /// integer registers -- and which any other callee ignores. There is no
    /// `%al`.
    pub(super) fn emit_win64_call(
        &mut self,
        insn: &Instruction,
        func_name: &str,
        types: &TypeTable,
    ) {
        let classes = insn
            .extra()
            .abi_info
            .as_ref()
            .expect("abi_info must be populated for Call instructions")
            .params
            .clone();
        let positions = insn.src.len().max(WIN64_REG_POSITIONS);
        let area = ((positions * WIN64_POSITION_BYTES) as i64 + 15) & !15;
        debug_assert!(area >= WIN64_SHADOW_BYTES as i64);
        self.push_lir(X86Inst::Sub {
            size: OperandSize::B64,
            src: GpOperand::Imm(area),
            dst: Reg::Rsp,
        });
        for (n, &arg) in insn.src.iter().enumerate() {
            let typ = insn.extra().arg_types.get(n).copied();
            self.store_win64_arg(arg, typ, &classes[n], n, types);
        }
        for (n, class) in classes.iter().enumerate().take(WIN64_REG_POSITIONS) {
            let typ = insn.extra().arg_types.get(n).copied();
            self.load_win64_position(n, class, typ, types);
        }
        if let Some(func_addr) = insn.extra().indirect_target {
            self.emit_move(func_addr, Reg::R11, 64);
        }
        self.emit_call_instruction(insn, func_name);
        self.cleanup_call_stack(area as usize);
        self.handle_call_return_value(insn, types);
    }

    /// The outgoing slot of position `n`, while the area is reserved.
    fn win64_outgoing_slot(n: usize) -> MemAddr {
        MemAddr::BaseOffset {
            base: Reg::Rsp,
            offset: (n * WIN64_POSITION_BYTES) as i32,
        }
    }

    /// Write argument `arg` to the outgoing slot of its position.
    fn store_win64_arg(
        &mut self,
        arg: PseudoId,
        typ: Option<TypeId>,
        class: &ArgClass,
        n: usize,
        types: &TypeTable,
    ) {
        let slot = Self::win64_outgoing_slot(n);
        if is_sse(class) {
            let fmt = self.fp_format(typ, typ.map_or(64, |t| types.size_bits(t)), types);
            self.emit_fp_move(arg, XmmReg::Xmm15, fmt);
            self.push_lir(X86Inst::MovFp {
                size: fmt,
                src: XmmOperand::Reg(XmmReg::Xmm15),
                dst: XmmOperand::Mem(slot),
            });
            return;
        }
        self.load_win64_gp_value(arg, typ, class, Reg::R11, types);
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst: GpOperand::Mem(slot),
        });
    }

    /// Load the integer-position value of `arg` into `dst`: a pointer for an
    /// argument passed by reference -- the linearizer's copy -- and otherwise
    /// the value's own bits, which for a `_Float16` gcc puts in the integer
    /// register too.
    fn load_win64_gp_value(
        &mut self,
        arg: PseudoId,
        typ: Option<TypeId>,
        class: &ArgClass,
        dst: Reg,
        types: &TypeTable,
    ) {
        if matches!(class, ArgClass::Indirect { .. }) {
            self.emit_move(arg, dst, 64);
            return;
        }
        let size = typ.map_or(64, |t| types.size_bits(t));
        if typ.is_some_and(|t| types.is_float(t)) {
            let fmt = self.fp_format(typ, size, types);
            self.emit_fp_move(arg, XmmReg::Xmm15, fmt);
            self.push_lir(X86Inst::MovXmmGp {
                size: OperandSize::B32,
                src: XmmReg::Xmm15,
                dst,
            });
            return;
        }
        self.emit_move(arg, dst, size);
    }

    /// Load register position `n` from its shadow slot: the integer
    /// register always, and the XMM one too for a floating-point value.
    fn load_win64_position(
        &mut self,
        n: usize,
        class: &ArgClass,
        typ: Option<TypeId>,
        types: &TypeTable,
    ) {
        let slot = Self::win64_outgoing_slot(n);
        if is_sse(class) {
            let fmt = self.fp_format(typ, typ.map_or(64, |t| types.size_bits(t)), types);
            self.push_lir(X86Inst::MovFp {
                size: fmt,
                src: XmmOperand::Mem(slot.clone()),
                dst: XmmOperand::Reg(XMM_ARGS[n]),
            });
        }
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(slot),
            dst: GpOperand::Reg(GP_ARGS[n]),
        });
    }

    /// Put an `ms_abi` function's return value where its caller looks for
    /// it: RAX for anything of 1, 2, 4 or 8 bytes -- an aggregate, a complex
    /// value, a `_Float16` -- and for the hidden return pointer, XMM0 for a
    /// `float`, a `double` or a whole `__int128`.
    pub(super) fn emit_win64_ret_value(&mut self, insn: &Instruction, types: &TypeTable) {
        let (Some(&src), Some(typ)) = (insn.src.first(), insn.typ) else {
            return;
        };
        if types.kind(typ) == TypeKind::Void {
            return;
        }
        let size = types.size_bits(typ);
        let class = Win64Abi::new().classify_return(typ, types);
        if types.is_plain_int128(typ) {
            let loc = self.get_location(src);
            let addr = self.int128_lo_mem_loc(&loc);
            self.push_lir(X86Inst::MovFp {
                size: FpSize::Quad,
                src: XmmOperand::Mem(addr),
                dst: XmmOperand::Reg(XmmReg::Xmm0),
            });
        } else if is_sse(&class) {
            let fmt = self.fp_format(Some(typ), size, types);
            self.emit_fp_move(src, XmmReg::Xmm0, fmt);
        } else if types.is_complex(typ) {
            // A complex `Ret` carries the value's address; its bits go back.
            let addr = self.scalar_mem_operand(src, 0, Reg::R11);
            self.load_bits(addr, types.size_bytes(typ), Reg::Rax);
        } else {
            self.load_win64_gp_value(src, Some(typ), &class, Reg::Rax, types);
        }
    }

    /// Load the `bytes` (1, 2, 4 or 8) bytes at `addr` into `dst`.
    fn load_bits(&mut self, addr: MemAddr, bytes: usize, dst: Reg) {
        let size = OperandSize::from_bits((bytes * 8) as u32);
        match size {
            OperandSize::B8 | OperandSize::B16 => self.push_lir(X86Inst::Movzx {
                src_size: size,
                dst_size: OperandSize::B32,
                src: GpOperand::Mem(addr),
                dst,
            }),
            _ => self.push_lir(X86Inst::Mov {
                size,
                src: GpOperand::Mem(addr),
                dst: GpOperand::Reg(dst),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn positions_arrive_in_their_shadow_slots_and_past_them() {
        assert_eq!(incoming_offset(0), 16);
        assert_eq!(incoming_offset(3), 40);
        assert_eq!(incoming_offset(4), 48);
    }

    #[test]
    fn each_position_takes_one_register_of_its_class() {
        let int = ArgClass::Direct {
            classes: vec![RegClass::Integer],
            size_bits: 64,
        };
        let sse = ArgClass::Direct {
            classes: vec![RegClass::Sse],
            size_bits: 64,
        };
        let by_ref = ArgClass::Indirect {
            align: 8,
            size_bytes: 24,
        };
        assert_eq!(position_reg(0, &int), Some(PositionReg::Gp(Reg::Rcx)));
        assert_eq!(position_reg(1, &sse), Some(PositionReg::Xmm(XmmReg::Xmm1)));
        assert_eq!(position_reg(2, &by_ref), Some(PositionReg::Gp(Reg::R8)));
        assert_eq!(position_reg(3, &sse), Some(PositionReg::Xmm(XmmReg::Xmm3)));
        assert_eq!(position_reg(4, &int), None);
    }
}
