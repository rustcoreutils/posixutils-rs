//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 Feature Code Generation (Variadic Functions, Byte Swapping, Bit Counting)
//

use super::codegen::Aarch64CodeGen;
use super::lir::{Aarch64Inst, GpOperand, MemAddr};
use super::regalloc::{Loc, Reg, VReg};
use crate::arch::codegen::BswapSize;
use crate::arch::lir::{CallTarget, CondCode, FpSize, OperandSize, Symbol};
use crate::ir::Instruction;
use crate::types::TypeTable;

/// Bytes of the variadic register save area holding x0-x7.
pub(super) const VA_GR_SAVE_BYTES: i32 = 8 * 8;
/// Bytes of the variadic register save area holding q0-q7.
///
/// Whole `q` registers, not `d`: an unnamed `long double` is binary128 on
/// AAPCS64 and fills the slot.
pub(super) const VA_VR_SAVE_BYTES: i32 = 8 * 16;

// AAPCS64 `va_list` field offsets (Linux/FreeBSD). The type is
//
//     typedef struct {
//         void *__stack;    // +0   next stack argument
//         void *__gr_top;   // +8   one past the end of the GP save area
//         void *__vr_top;   // +16  one past the end of the SIMD/FP save area
//         int   __gr_offs;  // +24  byte offset from __gr_top; < 0 while GP regs remain
//         int   __vr_offs;  // +28  byte offset from __vr_top; < 0 while FP regs remain
//     } va_list[1];
//
// which is 32 bytes, and `TypeTable::va_list_size_bits` has always said so.
// Codegen, however, treated `ap` as a bare `char*` walking the GP save area,
// which meant `va_arg(ap, double)` read a GP slot -- the incoming d0-d7 were
// never spilled at all -- and an `ap` handed to `vsnprintf` did not match what
// libc expected.
const VA_STACK_OFF: i32 = 0;
const VA_GR_TOP_OFF: i32 = 8;
const VA_VR_TOP_OFF: i32 = 16;
const VA_GR_OFFS_OFF: i32 = 24;
const VA_VR_OFFS_OFF: i32 = 28;

impl Aarch64CodeGen {
    // ========================================================================
    // Variadic function support (va_* builtins)
    // ========================================================================
    //
    // Platform-specific va_list handling:
    //
    // Linux/FreeBSD (AAPCS64): the 32-byte struct above, with two register
    // save areas built in the prologue (see `emit_variadic_save_area`).
    //
    // Darwin (macOS/iOS):
    //   - Variadic args are passed on the stack by the caller
    //   - va_list is a char* pointing to the caller's stack
    //   - va_start computes: FP + frame_size (where caller placed variadic args)

    /// Load the address of a `va_list` object into `dst`, returning the
    /// register that actually holds it.
    ///
    /// The operand is the *address* of the `va_list` (produced by `symaddr`),
    /// so a `Loc::Stack` operand needs one more load than a `Loc::Reg` one.
    fn va_list_addr(&mut self, loc: &Loc, dst: Reg) -> Option<Reg> {
        match loc {
            Loc::Stack(offset) => {
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    addr: self.stack_mem(*offset),
                    dst,
                });
                Some(dst)
            }
            Loc::Reg(r) => Some(*r),
            _ => None,
        }
    }

    /// Store a 32-bit immediate into a `va_list` field.
    fn store_va_i32(&mut self, ap: Reg, field: i32, value: i32, scratch: Reg) {
        // Build the bit pattern rather than the signed value: the offsets are
        // negative, and only the low 32 bits reach memory.
        self.emit_mov_imm(scratch, (value as u32) as i64, 32);
        self.push_lir(Aarch64Inst::Str {
            size: OperandSize::B32,
            src: scratch,
            addr: MemAddr::BaseOffset {
                base: ap,
                offset: field,
            },
        });
    }

    /// Store `x29 + offset` into a `va_list` pointer field.
    fn store_va_frame_ptr(&mut self, ap: Reg, field: i32, frame_offset: i32, scratch: Reg) {
        self.push_lir(Aarch64Inst::Add {
            size: OperandSize::B64,
            src1: Reg::X29,
            src2: GpOperand::Imm(frame_offset as i64),
            dst: scratch,
        });
        self.push_lir(Aarch64Inst::Str {
            size: OperandSize::B64,
            src: scratch,
            addr: MemAddr::BaseOffset {
                base: ap,
                offset: field,
            },
        });
    }

    /// Emit va_start: Initialize va_list to point to first variadic arg
    /// Note: ap_addr is the ADDRESS of the va_list variable (from symaddr), not the va_list itself
    pub(super) fn emit_va_start(&mut self, insn: &Instruction) {
        let ap_addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };

        let ap_loc = self.get_location(ap_addr);
        let (scratch0, scratch1, _) = Reg::scratch_regs();

        if self.base.target.os == crate::target::Os::MacOS {
            // Darwin passes every variadic argument on the stack and spells
            // va_list as a plain pointer to it, starting at the original SP.
            let Some(ap) = self.va_list_addr(&ap_loc, scratch1) else {
                return;
            };
            self.store_va_frame_ptr(ap, 0, self.frame_size, scratch0);
            return;
        }

        // AAPCS64. `__gr_top` / `__vr_top` sit one past the end of their save
        // areas and the offsets count backwards from there, so the first
        // unnamed argument of each class is at `top + offs` with `offs` at its
        // most negative. Named parameters have already consumed registers, so
        // start past them.
        let gr_top = self.reg_save_area_offset + VA_GR_SAVE_BYTES;
        let vr_top = gr_top + VA_VR_SAVE_BYTES;
        let gr_offs = -((8 - self.num_fixed_gp_params.min(8) as i32) * 8);
        let vr_offs = -((8 - self.num_fixed_fp_params.min(8) as i32) * 16);

        let Some(ap) = self.va_list_addr(&ap_loc, scratch1) else {
            return;
        };

        // Arguments that did not fit in registers are in the caller's outgoing
        // area, which begins at the SP value on entry -- past any named
        // parameter that landed there first.
        let first_stack_arg = self.frame_size + self.named_stack_param_bytes;
        self.store_va_frame_ptr(ap, VA_STACK_OFF, first_stack_arg, scratch0);
        self.store_va_frame_ptr(ap, VA_GR_TOP_OFF, gr_top, scratch0);
        self.store_va_frame_ptr(ap, VA_VR_TOP_OFF, vr_top, scratch0);
        self.store_va_i32(ap, VA_GR_OFFS_OFF, gr_offs, scratch0);
        self.store_va_i32(ap, VA_VR_OFFS_OFF, vr_offs, scratch0);
    }

    /// Emit va_arg: Get the next variadic argument of the specified type
    /// Note: ap_addr is the ADDRESS of the va_list variable (from symaddr), not the va_list itself
    pub(super) fn emit_va_arg(&mut self, insn: &Instruction, types: &TypeTable) {
        let ap_addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let arg_type = insn.typ.unwrap_or(types.int_id);
        let type_bits = types.size_bits(arg_type);
        // Aggregates are not classified here; they take the integer path, as
        // they always have.
        let is_fp = types.is_float(arg_type);

        let ap_loc = self.get_location(ap_addr);
        let dst_loc = self.get_location(target);

        if self.base.target.os == crate::target::Os::MacOS {
            self.emit_va_arg_darwin(&ap_loc, &dst_loc, type_bits, is_fp);
        } else {
            self.emit_va_arg_aapcs64(&ap_loc, &dst_loc, type_bits, is_fp);
        }
    }

    /// Darwin: every variadic argument is on the stack, so `ap` is just a
    /// cursor that walks it 8 bytes at a time.
    fn emit_va_arg_darwin(&mut self, ap_loc: &Loc, dst_loc: &Loc, type_bits: u32, is_fp: bool) {
        let (scratch0, _, scratch2) = Reg::scratch_regs();
        let Some(ap) = self.va_list_addr_pinned(ap_loc, scratch2) else {
            return;
        };

        self.push_lir(Aarch64Inst::Ldr {
            size: OperandSize::B64,
            addr: MemAddr::Base(ap),
            dst: scratch0,
        });

        self.emit_va_arg_load(dst_loc, scratch0, type_bits, is_fp);

        let step = Self::va_slot_bytes(type_bits).max(8);
        self.push_lir(Aarch64Inst::Add {
            size: OperandSize::B64,
            src1: scratch0,
            src2: GpOperand::Imm(step),
            dst: scratch0,
        });
        self.push_lir(Aarch64Inst::Str {
            size: OperandSize::B64,
            src: scratch0,
            addr: MemAddr::Base(ap),
        });
    }

    /// Bytes an argument of `type_bits` occupies in a stack slot: its size
    /// rounded up to the 8-byte stack granule.
    fn va_slot_bytes(type_bits: u32) -> i64 {
        let bytes = type_bits.div_ceil(8).max(1) as i64;
        (bytes + 7) & !7
    }

    /// Linux/FreeBSD: read the next argument per AAPCS64 Appendix B.
    ///
    /// Each class keeps a negative byte offset from the end of its save area.
    /// The argument comes out of registers while the offset stays negative and
    /// off the caller's stack once it does not, and the two cases are chosen
    /// with `csel` rather than a branch -- `reg_step` is positive, so
    /// `offs + reg_step <= 0` is true exactly when what remains of the save
    /// area still holds the whole argument. Committing the new offset even on
    /// the stack path is deliberate: it leaves the field non-negative, which
    /// is what pins every later argument of that class to the stack, as the
    /// ABI requires.
    fn emit_va_arg_aapcs64(&mut self, ap_loc: &Loc, dst_loc: &Loc, type_bits: u32, is_fp: bool) {
        let (scratch0, scratch1, scratch2) = Reg::scratch_regs();
        let Some(ap) = self.va_list_addr_pinned(ap_loc, scratch2) else {
            return;
        };

        let (offs_field, top_field) = if is_fp {
            (VA_VR_OFFS_OFF, VA_VR_TOP_OFF)
        } else {
            (VA_GR_OFFS_OFF, VA_GR_TOP_OFF)
        };

        let stack_step = Self::va_slot_bytes(type_bits);
        // Every SIMD slot is a full 16 bytes regardless of the type stored in
        // it; GP slots are 8, and a 16-byte integer takes two of them.
        let reg_step = if is_fp { 16 } else { stack_step };
        // A 16-byte argument is 16-byte aligned on the stack.
        let stack_align16 = type_bits > 64;

        // x9 = offs, x10 = offs + reg_step, committed back immediately.
        self.push_lir(Aarch64Inst::Ldr {
            size: OperandSize::B32,
            addr: MemAddr::BaseOffset {
                base: ap,
                offset: offs_field,
            },
            dst: scratch0,
        });
        self.push_lir(Aarch64Inst::Sxtw {
            src: scratch0,
            dst: scratch0,
        });
        self.push_lir(Aarch64Inst::Add {
            size: OperandSize::B64,
            src1: scratch0,
            src2: GpOperand::Imm(reg_step),
            dst: scratch1,
        });
        self.push_lir(Aarch64Inst::Str {
            size: OperandSize::B32,
            src: scratch1,
            addr: MemAddr::BaseOffset {
                base: ap,
                offset: offs_field,
            },
        });

        // One compare drives both selects below; everything between them is
        // loads, moves and non-flag-setting arithmetic.
        self.push_lir(Aarch64Inst::Cmp {
            size: OperandSize::B64,
            src1: scratch1,
            src2: GpOperand::Imm(0),
        });

        // x16 = save-area slot = top + offs (offs is negative here).
        self.push_lir(Aarch64Inst::Ldr {
            size: OperandSize::B64,
            addr: MemAddr::BaseOffset {
                base: ap,
                offset: top_field,
            },
            dst: Reg::X16,
        });
        self.push_lir(Aarch64Inst::Add {
            size: OperandSize::B64,
            src1: Reg::X16,
            src2: GpOperand::Reg(scratch0),
            dst: Reg::X16,
        });

        // x17 keeps __stack unrounded so the register path can write it back
        // untouched; x10 becomes the aligned stack slot.
        self.push_lir(Aarch64Inst::Ldr {
            size: OperandSize::B64,
            addr: MemAddr::BaseOffset {
                base: ap,
                offset: VA_STACK_OFF,
            },
            dst: Reg::X17,
        });
        if stack_align16 {
            self.push_lir(Aarch64Inst::Add {
                size: OperandSize::B64,
                src1: Reg::X17,
                src2: GpOperand::Imm(15),
                dst: scratch0,
            });
            self.emit_mov_imm(scratch1, -16, 64);
            self.push_lir(Aarch64Inst::And {
                size: OperandSize::B64,
                src1: scratch0,
                src2: GpOperand::Reg(scratch1),
                dst: scratch1,
            });
        } else {
            self.push_lir(Aarch64Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::X17),
                dst: scratch1,
            });
        }

        // __stack advances only when the argument actually came off the stack.
        self.push_lir(Aarch64Inst::Add {
            size: OperandSize::B64,
            src1: scratch1,
            src2: GpOperand::Imm(stack_step),
            dst: scratch0,
        });
        self.push_lir(Aarch64Inst::Csel {
            size: OperandSize::B64,
            cond: CondCode::Sle,
            src_true: Reg::X17,
            src_false: scratch0,
            dst: scratch0,
        });
        self.push_lir(Aarch64Inst::Str {
            size: OperandSize::B64,
            src: scratch0,
            addr: MemAddr::BaseOffset {
                base: ap,
                offset: VA_STACK_OFF,
            },
        });

        // x9 = the address to read the argument from.
        self.push_lir(Aarch64Inst::Csel {
            size: OperandSize::B64,
            cond: CondCode::Sle,
            src_true: Reg::X16,
            src_false: scratch1,
            dst: scratch0,
        });

        self.emit_va_arg_load(dst_loc, scratch0, type_bits, is_fp);
    }

    /// Load the address of a `va_list` into `pin`, moving it there even when
    /// it already sits in some other register, so the caller can rely on it
    /// staying put across a long instruction sequence.
    fn va_list_addr_pinned(&mut self, ap_loc: &Loc, pin: Reg) -> Option<Reg> {
        let ap = self.va_list_addr(ap_loc, pin)?;
        if ap != pin {
            self.push_lir(Aarch64Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(ap),
                dst: pin,
            });
        }
        Some(pin)
    }

    /// Read the argument at `addr` and place it in `dst_loc`.
    fn emit_va_arg_load(&mut self, dst_loc: &Loc, addr: Reg, type_bits: u32, is_fp: bool) {
        let (_, scratch1, _) = Reg::scratch_regs();

        if is_fp {
            let fp_size = FpSize::from_bits(type_bits, &self.base.target);
            self.push_lir(Aarch64Inst::LdrFp {
                size: fp_size,
                addr: MemAddr::Base(addr),
                dst: VReg::V16,
            });
            match dst_loc {
                Loc::VReg(v) => {
                    self.push_lir(Aarch64Inst::FmovReg {
                        size: fp_size,
                        src: VReg::V16,
                        dst: *v,
                    });
                }
                Loc::Stack(offset) => {
                    self.push_lir(Aarch64Inst::StrFp {
                        size: fp_size,
                        src: VReg::V16,
                        addr: self.stack_mem(*offset),
                    });
                }
                _ => {}
            }
            return;
        }

        let op_size = OperandSize::from_bits(type_bits.max(32));
        self.push_lir(Aarch64Inst::Ldr {
            size: op_size,
            addr: MemAddr::Base(addr),
            dst: scratch1,
        });
        match dst_loc {
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
                self.push_lir(Aarch64Inst::Str {
                    size: op_size,
                    src: scratch1,
                    addr: self.stack_mem(*offset),
                });
            }
            _ => {}
        }
    }

    /// Emit va_copy: Copy a va_list
    /// Note: Both addresses are pointers to va_list variables (from symaddr)
    pub(super) fn emit_va_copy(&mut self, insn: &Instruction) {
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
        let (scratch0, scratch1, scratch2) = Reg::scratch_regs();

        // Darwin's va_list is one pointer; everywhere else it is the 32-byte
        // AAPCS64 record, and copying only the first 8 bytes left the copy
        // with a garbage `__gr_top` and offsets.
        let bytes: i32 = if self.base.target.os == crate::target::Os::MacOS {
            8
        } else {
            32
        };

        let Some(src_ptr) = self.va_list_addr_pinned(&src_loc, scratch2) else {
            return;
        };
        // The destination address has to survive the loads below, so it goes
        // in x16 rather than a scratch that the copy itself uses.
        let Some(dest_ptr) = self.va_list_addr(&dest_loc, Reg::X16) else {
            return;
        };
        if dest_ptr != Reg::X16 {
            self.push_lir(Aarch64Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(dest_ptr),
                dst: Reg::X16,
            });
        }

        // Pairs first, then a single register for any 8-byte remainder. The
        // pair step has to be gated on a *full* 16 bytes remaining: Darwin's
        // va_list is 8 bytes, and copying it with one `stp` wrote 8 bytes past
        // the destination, over whatever the frame held next.
        let mut offset = 0;
        while bytes - offset >= 16 {
            self.push_lir(Aarch64Inst::Ldp {
                size: OperandSize::B64,
                addr: MemAddr::BaseOffset {
                    base: src_ptr,
                    offset,
                },
                dst1: scratch0,
                dst2: scratch1,
            });
            self.push_lir(Aarch64Inst::Stp {
                size: OperandSize::B64,
                src1: scratch0,
                src2: scratch1,
                addr: MemAddr::BaseOffset {
                    base: Reg::X16,
                    offset,
                },
            });
            offset += 16;
        }
        while bytes - offset >= 8 {
            self.push_lir(Aarch64Inst::Ldr {
                size: OperandSize::B64,
                addr: MemAddr::BaseOffset {
                    base: src_ptr,
                    offset,
                },
                dst: scratch0,
            });
            self.push_lir(Aarch64Inst::Str {
                size: OperandSize::B64,
                src: scratch0,
                addr: MemAddr::BaseOffset {
                    base: Reg::X16,
                    offset,
                },
            });
            offset += 8;
        }
    }

    // =========================================================================
    // Byte-swapping builtins
    // =========================================================================

    /// Emit byte-swap instruction for 16/32/64-bit values
    pub(super) fn emit_bswap(&mut self, insn: &Instruction, swap_size: BswapSize) {
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
        let op_size = match swap_size {
            BswapSize::B16 => OperandSize::B16,
            BswapSize::B32 => OperandSize::B32,
            BswapSize::B64 => OperandSize::B64,
        };
        // For moves, 16-bit uses 32-bit register operations
        let mov_size = if matches!(swap_size, BswapSize::B16) {
            OperandSize::B32
        } else {
            op_size
        };

        // Load source into scratch register
        match &src_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: mov_size,
                    src: GpOperand::Reg(*r),
                    dst: scratch,
                });
            }
            Loc::Stack(off) => {
                self.push_lir(Aarch64Inst::Ldr {
                    size: op_size,
                    addr: self.stack_mem(*off),
                    dst: scratch,
                });
            }
            Loc::Imm(v) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: mov_size,
                    src: GpOperand::Imm(*v as i64),
                    dst: scratch,
                });
            }
            _ => return,
        }

        // Perform byte-swap: 16-bit uses Rev16+mask, 32/64-bit uses Rev
        match swap_size {
            BswapSize::B16 => {
                self.push_lir(Aarch64Inst::Rev16 {
                    size: OperandSize::B32,
                    src: scratch,
                    dst: scratch,
                });
                self.push_lir(Aarch64Inst::And {
                    size: OperandSize::B32,
                    src1: scratch,
                    src2: GpOperand::Imm(0xFFFF),
                    dst: scratch,
                });
            }
            BswapSize::B32 | BswapSize::B64 => {
                self.push_lir(Aarch64Inst::Rev {
                    size: op_size,
                    src: scratch,
                    dst: scratch,
                });
            }
        }

        // Store result
        match &dst_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: mov_size,
                    src: GpOperand::Reg(scratch),
                    dst: *r,
                });
            }
            Loc::Stack(off) => {
                self.push_lir(Aarch64Inst::Str {
                    size: op_size,
                    src: scratch,
                    addr: self.stack_mem(*off),
                });
            }
            _ => {}
        }
    }

    /// Emit count trailing zeros: on AArch64, CTZ = CLZ(RBIT(x))
    pub(super) fn emit_ctz(&mut self, insn: &Instruction, src_size: OperandSize) {
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
                    size: src_size,
                    src: GpOperand::Reg(r),
                    dst: scratch,
                });
            }
            Loc::Stack(off) => {
                self.push_lir(Aarch64Inst::Ldr {
                    size: src_size,
                    addr: self.stack_mem(off),
                    dst: scratch,
                });
            }
            Loc::Imm(v) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: src_size,
                    src: GpOperand::Imm(v as i64),
                    dst: scratch,
                });
            }
            _ => return,
        }

        // Reverse bits: RBIT
        self.push_lir(Aarch64Inst::Rbit {
            size: src_size,
            src: scratch,
            dst: scratch,
        });

        // Count leading zeros: CLZ - this gives us the count of trailing zeros
        self.push_lir(Aarch64Inst::Clz {
            size: src_size,
            src: scratch,
            dst: scratch,
        });

        // Store result (return type is int, always 32-bit)
        match dst_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(scratch),
                    dst: r,
                });
            }
            Loc::Stack(off) => {
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B32,
                    src: scratch,
                    addr: self.stack_mem(off),
                });
            }
            _ => {}
        }
    }

    /// Emit count leading zeros (AArch64 has a direct CLZ instruction)
    pub(super) fn emit_clz(&mut self, insn: &Instruction, src_size: OperandSize) {
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
                    size: src_size,
                    src: GpOperand::Reg(r),
                    dst: scratch,
                });
            }
            Loc::Stack(off) => {
                self.push_lir(Aarch64Inst::Ldr {
                    size: src_size,
                    addr: self.stack_mem(off),
                    dst: scratch,
                });
            }
            Loc::Imm(v) => {
                // Use emit_mov_imm which handles large immediates with movz/movk
                self.emit_mov_imm(scratch, v as i64, src_size.bits());
            }
            _ => return,
        }

        // Count leading zeros using CLZ instruction
        self.push_lir(Aarch64Inst::Clz {
            size: src_size,
            src: scratch,
            dst: scratch,
        });

        // Store result (return type is int, always 32-bit)
        match dst_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(scratch),
                    dst: r,
                });
            }
            Loc::Stack(off) => {
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B32,
                    src: scratch,
                    addr: self.stack_mem(off),
                });
            }
            _ => {}
        }
    }

    /// Emit population count. On AArch64:
    /// fmov d0, x0; cnt v0.8b, v0.8b; addv b0, v0.8b; fmov w0, s0
    pub(super) fn emit_popcount(&mut self, insn: &Instruction, src_size: OperandSize) {
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
        let fp_scratch = VReg::V16; // Use reserved scratch FP register

        // Load source into scratch register
        match src_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: src_size,
                    src: GpOperand::Reg(r),
                    dst: scratch,
                });
            }
            Loc::Stack(off) => {
                self.push_lir(Aarch64Inst::Ldr {
                    size: src_size,
                    addr: self.stack_mem(off),
                    dst: scratch,
                });
            }
            Loc::Imm(v) => {
                // Use emit_mov_imm which handles large immediates with movz/movk
                self.emit_mov_imm(scratch, v as i64, src_size.bits());
            }
            _ => return,
        }

        // Move to SIMD register (always use 64-bit for the fmov)
        self.push_lir(Aarch64Inst::FmovFromGp {
            size: FpSize::Double,
            src: scratch,
            dst: fp_scratch,
        });

        // Count bits per byte
        self.push_lir(Aarch64Inst::Cnt {
            src: fp_scratch,
            dst: fp_scratch,
        });

        // Sum all bytes
        self.push_lir(Aarch64Inst::Addv {
            src: fp_scratch,
            dst: fp_scratch,
        });

        // Move result back to GP register (use Single since result is in b0/s0)
        self.push_lir(Aarch64Inst::FmovToGp {
            size: FpSize::Single,
            src: fp_scratch,
            dst: scratch,
        });

        // Store result (return type is int, always 32-bit)
        match dst_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(scratch),
                    dst: r,
                });
            }
            Loc::Stack(off) => {
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B32,
                    src: scratch,
                    addr: self.stack_mem(off),
                });
            }
            _ => {}
        }
    }

    // ========================================================================
    // setjmp/longjmp/alloca support
    // ========================================================================

    /// Emit setjmp(env) - saves execution context
    /// AAPCS64: env in X0, returns int in W0
    pub(super) fn emit_setjmp(&mut self, insn: &Instruction) {
        let env = match insn.src.first() {
            Some(&e) => e,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Put env argument in X0 (first argument register)
        self.emit_move(env, Reg::X0, 64);

        // Call setjmp
        self.push_lir(Aarch64Inst::Bl {
            target: CallTarget::Direct(Symbol::global("setjmp")),
        });

        // Store result from W0 to target
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::X0, &dst_loc, u32::BITS);
    }

    /// Emit longjmp(env, val) - restores execution context (noreturn)
    /// AAPCS64: env in X0, val in X1
    pub(super) fn emit_longjmp(&mut self, insn: &Instruction) {
        let env = match insn.src.first() {
            Some(&e) => e,
            None => return,
        };
        let val = match insn.src.get(1) {
            Some(&v) => v,
            None => return,
        };

        // CONSTRAINT: Load val into X1 BEFORE loading env into X0.
        // If env is loaded into X0 first and val happens to be in X0 (first
        // function argument), it would be overwritten. This is a manual constraint
        // that will be expressible through the constraint system when inline asm
        // support is added.
        self.emit_move(val, Reg::X1, 32);

        // Put env argument in X0 (first argument register)
        self.emit_move(env, Reg::X0, 64);

        // Call longjmp (noreturn - control never comes back)
        self.push_lir(Aarch64Inst::Bl {
            target: CallTarget::Direct(Symbol::global("longjmp")),
        });

        // Emit brk after longjmp since it never returns
        // This helps catch any bugs where longjmp somehow returns
        self.push_lir(Aarch64Inst::Brk { imm: 1 });
    }

    /// Emit __builtin_frame_address(level) - return frame pointer at given level
    pub(super) fn emit_frame_address(&mut self, insn: &Instruction) {
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // For level 0, return the current frame pointer (x29)
        // Use x9 as scratch register to hold the frame pointer
        self.push_lir(Aarch64Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::X29),
            dst: Reg::X9,
        });

        // Store result
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::X9, &dst_loc, 64);
    }

    /// Emit __builtin_return_address(level) - return address at given level
    pub(super) fn emit_return_address(&mut self, insn: &Instruction) {
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // For level 0, return the link register (x30)
        // Use x9 as scratch register to hold the return address
        self.push_lir(Aarch64Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::X30),
            dst: Reg::X9,
        });

        // Store result
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::X9, &dst_loc, 64);
    }

    /// Emit __builtin_alloca - dynamic stack allocation
    pub(super) fn emit_alloca(&mut self, insn: &Instruction) {
        let size = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load size into X9 (scratch register)
        self.emit_move(size, Reg::X9, 64);

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
        self.emit_move_to_loc(Reg::X9, &dst_loc, 64);
    }

    /// Emit __builtin_signbitf - test sign bit of float
    pub(super) fn emit_signbit32(&mut self, insn: &Instruction, types: &TypeTable) {
        let arg = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load argument into V0 (first FP argument register)
        self.emit_fp_move(arg, VReg::V0, None, 32, types);

        // Call __signbitf from libc (C99: signbit is a macro that calls __signbitf)
        self.push_lir(Aarch64Inst::Bl {
            target: CallTarget::Direct(Symbol::global("__signbitf")),
        });

        // Result is in W0 (integer return), store to target
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::X0, &dst_loc, u32::BITS);
    }

    /// Emit __builtin_signbit - test sign bit of double
    pub(super) fn emit_signbit64(&mut self, insn: &Instruction, types: &TypeTable) {
        let arg = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load argument into V0 (first FP argument register)
        self.emit_fp_move(arg, VReg::V0, None, 64, types);

        // Call signbit function from libc
        self.push_lir(Aarch64Inst::Bl {
            target: CallTarget::Direct(Symbol::global(self.base.target.os.signbit_double_fn())),
        });

        // Result is in W0 (integer return), store to target
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::X0, &dst_loc, u32::BITS);
    }

    /// Emit __builtin_fabsf/__builtin_fabs - absolute value of float/double
    pub(super) fn emit_fabs(&mut self, insn: &Instruction, types: &TypeTable, is_double: bool) {
        let arg = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let (size, func_name) = if is_double {
            (64, "fabs")
        } else {
            (32, "fabsf")
        };

        // Load argument into V0 (first FP argument register)
        self.emit_fp_move(arg, VReg::V0, None, size, types);

        // Call fabs/fabsf from libc
        self.push_lir(Aarch64Inst::Bl {
            target: CallTarget::Direct(Symbol::global(func_name)),
        });

        // Result is in V0, store to target
        let dst_loc = self.get_location(target);
        self.emit_fp_move_to_loc(VReg::V0, &dst_loc, None, size, types);
    }
}
