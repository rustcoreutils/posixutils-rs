//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 Call Code Generation
//

use super::codegen::X86_64CodeGen;
use super::lir::{GpOperand, MemAddr, X86Inst, XmmOperand};
use super::regalloc::{Loc, Reg, XmmReg};
use crate::abi::{Abi, ArgClass, RegClass};
use crate::arch::lir::{complex_fp_info, CallTarget, FpSize, OperandSize, Symbol};
use crate::ir::{Instruction, PseudoId};
use crate::types::{TypeKind, TypeTable};
use std::collections::HashMap;

/// The alignment a stacked argument must start on: its type's, at least 8.
fn arg_align(insn: &Instruction, types: &TypeTable, i: usize) -> i32 {
    insn.arg_types
        .get(i)
        .map(|&t| types.alignment(t) as i32)
        .unwrap_or(8)
}

/// Information about call argument classification
pub(super) struct CallArgInfo {
    /// Indices of arguments that go on the stack
    pub stack_arg_indices: Vec<usize>,
    /// Where each of those starts, in bytes from the outgoing area's base.
    /// Parallel to `stack_arg_indices`.
    ///
    /// SysV AMD64 3.2.3 rounds each stacked argument's address up to
    /// `max(8, alignof(type))`, so the area has to be *walked* in parameter
    /// order rather than summed. Pushing in reverse and padding once at the
    /// top -- which is what this replaced -- cannot express a gap between two
    /// arguments, so a sixteen-byte-aligned one after an odd number of
    /// eight-byte slots landed eight bytes low.
    pub stack_offsets: Vec<i32>,
    /// Total bytes to reserve, rounded to the 16-byte call boundary.
    pub stack_bytes: i32,
}

impl X86_64CodeGen {
    /// Classify call arguments into register vs stack arguments using ABI info.
    pub(super) fn classify_call_args(&self, insn: &Instruction, types: &TypeTable) -> CallArgInfo {
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = XmmReg::arg_regs();

        let mut stack_arg_indices = Vec::with_capacity(insn.src.len());
        let mut temp_int_idx = 0;
        let mut temp_fp_idx = 0;
        // The outgoing area is walked, not summed: each argument begins at
        // its own alignment. `at` is the running byte offset.
        let mut stack_offsets = Vec::with_capacity(insn.src.len());
        let mut at: i32 = 0;
        // Reserve `bytes` for the argument at `i`, starting it on `align`.
        let place = |i: usize,
                     bytes: usize,
                     align: i32,
                     indices: &mut Vec<usize>,
                     offsets: &mut Vec<i32>,
                     at: &mut i32| {
            let align = align.max(8);
            *at = (*at + align - 1) & !(align - 1);
            indices.push(i);
            offsets.push(*at);
            *at += ((bytes as i32) + 7) & !7;
        };

        let abi_info = insn
            .abi_info
            .as_ref()
            .expect("abi_info must be populated for Call instructions");

        for (i, arg_class) in abi_info.params.iter().enumerate() {
            // Check if this is a long double argument - these are always passed
            // BY VALUE on the stack per System V AMD64 ABI (not via pointer)
            // `long double _Complex` also has kind LongDouble, but it is 32
            // bytes rather than 16 and is classified COMPLEX_X87 (MEMORY), so
            // it must fall through to the Indirect arm below and get its real
            // size counted — not be treated as a 2-qword scalar.
            let is_longdouble = insn
                .arg_types
                .get(i)
                .is_some_and(|&ty| types.kind(ty) == TypeKind::LongDouble && !types.is_complex(ty));

            if is_longdouble {
                // Long double is always passed on the stack by value (16 bytes = 2 qwords)
                place(
                    i,
                    16,
                    16,
                    &mut stack_arg_indices,
                    &mut stack_offsets,
                    &mut at,
                );
                continue;
            }

            match arg_class {
                ArgClass::Direct { classes, .. } => {
                    // Count GP and FP registers needed
                    let gp_needed = classes.iter().filter(|c| **c == RegClass::Integer).count();
                    let fp_needed = classes.iter().filter(|c| **c == RegClass::Sse).count();

                    // Check if we have enough registers
                    let has_gp = gp_needed == 0 || temp_int_idx + gp_needed <= int_arg_regs.len();
                    let has_fp = fp_needed == 0 || temp_fp_idx + fp_needed <= fp_arg_regs.len();

                    if !has_gp || !has_fp {
                        // Every eightbyte lands on the stack, so a mixed pair
                        // takes two qwords -- `max` counted it as one and left
                        // the outgoing area a qword short.
                        let bytes = classes.len().max(1) * 8;
                        let align = arg_align(insn, types, i);
                        place(
                            i,
                            bytes,
                            align,
                            &mut stack_arg_indices,
                            &mut stack_offsets,
                            &mut at,
                        );
                        // §3.2.3 step 5: an argument that does not fit goes to
                        // memory *whole* and consumes no registers, so the ones
                        // it did not fit in remain for later arguments.
                        // Counting them here disagreed with `setup_register_args`,
                        // which skips stack arguments without advancing its own
                        // index — so the two sides picked different registers.
                        continue;
                    }
                    temp_int_idx += gp_needed;
                    temp_fp_idx += fp_needed;
                }
                ArgClass::Indirect { size_bits, .. } => {
                    // Large struct parameters (> 16 bytes): passed by value on the stack
                    // per SysV AMD64 ABI MEMORY class. Always a stack arg — never in
                    // a register. Don't consume a GP register.
                    let align = arg_align(insn, types, i);
                    place(
                        i,
                        (*size_bits as usize).div_ceil(8),
                        align,
                        &mut stack_arg_indices,
                        &mut stack_offsets,
                        &mut at,
                    );
                }
                ArgClass::Extend { .. } => {
                    // Extended small integers use one GP register
                    if temp_int_idx >= int_arg_regs.len() {
                        place(i, 8, 8, &mut stack_arg_indices, &mut stack_offsets, &mut at);
                    }
                    temp_int_idx += 1;
                }
                ArgClass::Hfa { count, .. } => {
                    // HFA uses FP registers (primarily AArch64, but handle for completeness)
                    if temp_fp_idx + (*count as usize) > fp_arg_regs.len() {
                        place(i, 8, 8, &mut stack_arg_indices, &mut stack_offsets, &mut at);
                    }
                    temp_fp_idx += *count as usize;
                }
                ArgClass::X87 { .. } => {
                    // X87 is only used for return values, not parameters
                    // Long double parameters are passed via Indirect (pointer in memory)
                    unreachable!("X87 classification only applies to return values");
                }
                ArgClass::Ignore => {
                    // Zero-sized type, skip
                }
            }
        }

        // The call boundary is 16-byte aligned.
        let stack_bytes = (at + 15) & !15;

        CallArgInfo {
            stack_arg_indices,
            stack_offsets,
            stack_bytes,
        }
    }

    /// Push stack arguments in reverse order (returns number of args pushed)
    pub(super) fn push_stack_args(
        &mut self,
        insn: &Instruction,
        info: &CallArgInfo,
        types: &TypeTable,
    ) -> usize {
        if info.stack_bytes == 0 {
            return 0;
        }
        // Reserve the whole outgoing area at once, then write each argument at
        // the offset the layout gave it. Pushing in reverse could not express a
        // gap between two arguments, which is what an alignment boundary is.
        self.push_lir(X86Inst::Sub {
            size: OperandSize::B64,
            src: GpOperand::Imm(info.stack_bytes as i64),
            dst: Reg::Rsp,
        });
        // Locals are addressed from %rsp when the frame is over-aligned, so
        // every source read below has to account for the reservation just made.
        self.rsp_adjust += info.stack_bytes;

        for (n, &i) in info.stack_arg_indices.iter().enumerate() {
            let base_off = info.stack_offsets[n];
            let arg = insn.src[i];
            let arg_type = insn.arg_types.get(i).copied();

            // MEMORY class first: a large aggregate, a `long double _Complex`,
            // or any complex value that ran out of registers — reaching this
            // loop at all means it is going to memory. The arg pseudo holds the
            // value's address; copy it to the stack a qword at a time. Checked
            // ahead of the FP tests because a complex type carries its base's
            // kind, so `long double _Complex` would otherwise be mistaken for a
            // 16-byte scalar long double and only half of it copied.
            if let Some(bytes) = arg_type.and_then(|t| {
                crate::arch::lir::memory_class_bytes(types, t)
                    .or_else(|| {
                        types
                            .is_complex(t)
                            .then(|| (types.size_bits(t) / 8) as usize)
                    })
                    // A register-pair struct that ran out of registers goes on
                    // the stack *whole*. Without this it fell through to the
                    // scalar path below and pushed eight bytes of a sixteen-byte
                    // value -- the callee then read half of it plus whatever
                    // followed.
                    .or_else(|| {
                        crate::abi::struct_param_classes(t, types)
                            .map(|_| (types.size_bits(t) / 8) as usize)
                    })
            }) {
                let num_qwords = bytes.div_ceil(8);
                let base = self.address_of_pseudo(arg);
                for q in 0..num_qwords {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base,
                            offset: (q * 8) as i32,
                        }),
                        dst: GpOperand::Reg(Reg::Rax),
                    });
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::Rax),
                        dst: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rsp,
                            offset: base_off + (q * 8) as i32,
                        }),
                    });
                }
                continue;
            }

            let is_fp = if let Some(typ) = arg_type {
                types.is_float(typ)
            } else {
                let arg_loc = self.get_location(arg);
                matches!(arg_loc, Loc::Xmm(_) | Loc::FImm(..))
            };

            if is_fp {
                let fp_size = if let Some(typ) = arg_type {
                    types.size_bits(typ)
                } else {
                    64
                };
                // The *type* decides the register format: x87 extended and
                // binary128 are both 128 bits wide here, and only the type
                // tells them apart.
                let fp_fmt = self.fp_format(arg_type, fp_size, types);
                let is_longdouble = arg_type.is_some_and(|t| types.kind(t) == TypeKind::LongDouble);

                // Long double uses x87, needs 16 bytes on stack
                if is_longdouble {
                    let src_addr = self.get_x87_mem_addr(arg);
                    self.push_lir(X86Inst::X87Load { addr: src_addr });
                    self.push_lir(X86Inst::X87Store {
                        addr: MemAddr::BaseOffset {
                            base: Reg::Rsp,
                            offset: base_off,
                        },
                    });
                    continue;
                }

                // A binary128 is two eightbytes, and it moves as one 16-byte
                // quantity: reserving eight and storing it as a scalar wrote
                // half the value under an instruction that does not exist.
                self.emit_fp_move(arg, XmmReg::Xmm15, fp_fmt);
                self.push_lir(X86Inst::MovFp {
                    size: fp_fmt,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rsp,
                        offset: base_off,
                    }),
                });
            } else {
                // Check if this is an __int128 arg (needs 16 bytes = 2 stack slots)
                let is_int128 = arg_type.is_some_and(|t| types.kind(t) == TypeKind::Int128);
                if is_int128 {
                    let arg_loc = self.get_location(arg).clone();
                    for (half, off) in [
                        (self.int128_lo_mem_loc(&arg_loc), base_off),
                        (self.int128_hi_mem_loc(&arg_loc), base_off + 8),
                    ] {
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::B64,
                            src: GpOperand::Mem(half),
                            dst: GpOperand::Reg(Reg::Rax),
                        });
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::B64,
                            src: GpOperand::Reg(Reg::Rax),
                            dst: GpOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::Rsp,
                                offset: off,
                            }),
                        });
                    }
                    continue;
                }
                // (Large aggregates are handled by the MEMORY-class arm above.)
                let arg_size = if let Some(typ) = arg_type {
                    types.size_bits(typ).max(32)
                } else {
                    64
                };
                self.emit_move(arg, Reg::Rax, arg_size);
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rsp,
                        offset: base_off,
                    }),
                });
            }
        }

        // Deliberately *not* undone here: %rsp stays lowered until
        // `cleanup_call_stack` runs after the call, and the register arguments
        // are set up inside that window. Clearing it here made every local read
        // by `save_clobbered_arg_regs` and `setup_register_args` miss by the
        // size of the outgoing area, whenever the frame addresses locals
        // through %rsp.
        info.stack_bytes as usize
    }

    /// Save argument registers that will be clobbered by earlier argument setup
    pub(super) fn save_clobbered_arg_regs(
        &mut self,
        insn: &Instruction,
        info: &CallArgInfo,
        types: &TypeTable,
    ) -> HashMap<Reg, Reg> {
        let int_arg_regs = Reg::arg_regs();
        let scratch_regs = [Reg::R10, Reg::R11];
        let mut saved_arg_regs: HashMap<Reg, Reg> = HashMap::new();
        let mut scratch_idx = 0;

        // Collect which argument registers we'll write to (in order)
        let mut regs_to_write: Vec<Reg> = Vec::new();
        let mut temp_int_idx = 0;
        for i in 0..insn.src.len() {
            if info.stack_arg_indices.contains(&i) {
                continue;
            }
            let arg_type = insn.arg_types.get(i).copied();
            let is_fp = if let Some(typ) = arg_type {
                types.is_float(typ)
            } else {
                let arg_loc = self.get_location(insn.src[i]);
                matches!(arg_loc, Loc::Xmm(_) | Loc::FImm(..))
            };
            let is_complex = arg_type.is_some_and(|t| types.is_complex(t));

            if !is_fp && !is_complex {
                if temp_int_idx < int_arg_regs.len() {
                    regs_to_write.push(int_arg_regs[temp_int_idx]);
                }
                temp_int_idx += 1;
            }
        }

        // Check which argument sources are in registers that will be clobbered
        temp_int_idx = 0;
        for i in 0..insn.src.len() {
            if info.stack_arg_indices.contains(&i) {
                continue;
            }
            let arg = insn.src[i];
            let arg_type = insn.arg_types.get(i).copied();
            let is_fp = if let Some(typ) = arg_type {
                types.is_float(typ)
            } else {
                let arg_loc = self.get_location(arg);
                matches!(arg_loc, Loc::Xmm(_) | Loc::FImm(..))
            };
            let is_complex = arg_type.is_some_and(|t| types.is_complex(t));

            if !is_fp && !is_complex && temp_int_idx < int_arg_regs.len() {
                let arg_loc = self.get_location(arg);
                if let Loc::Reg(src_reg) = arg_loc {
                    let my_dest_idx = temp_int_idx;
                    for (write_idx, &write_reg) in regs_to_write.iter().enumerate() {
                        if write_idx < my_dest_idx && write_reg == src_reg {
                            if !saved_arg_regs.contains_key(&src_reg)
                                && scratch_idx < scratch_regs.len()
                            {
                                let scratch = scratch_regs[scratch_idx];
                                scratch_idx += 1;
                                self.push_lir(X86Inst::Mov {
                                    size: OperandSize::B64,
                                    src: GpOperand::Reg(src_reg),
                                    dst: GpOperand::Reg(scratch),
                                });
                                saved_arg_regs.insert(src_reg, scratch);
                            }
                            break;
                        }
                    }
                }
                temp_int_idx += 1;
            }
        }

        saved_arg_regs
    }

    /// Set up register arguments (returns number of FP args for variadic AL)
    /// The register holding a struct argument's *address*.
    ///
    /// The argument pseudo carries a pointer (from `symaddr`), not the struct's
    /// bytes, so a spilled one is loaded with `mov`: `lea` would give the
    /// address of the slot holding the pointer, which is a pointer to a
    /// pointer and reads as garbage.
    fn struct_arg_base(&mut self, arg: PseudoId) -> Reg {
        match self.get_location(arg) {
            Loc::Reg(r) => r,
            Loc::Stack(offset) => {
                let addr = self.stack_mem(offset);
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(addr),
                    dst: GpOperand::Reg(Reg::R11),
                });
                Reg::R11
            }
            _ => {
                self.emit_move(arg, Reg::R11, 64);
                Reg::R11
            }
        }
    }

    pub(super) fn setup_register_args(
        &mut self,
        insn: &Instruction,
        info: &CallArgInfo,
        saved_arg_regs: &HashMap<Reg, Reg>,
        types: &TypeTable,
    ) -> usize {
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = XmmReg::arg_regs();
        let mut int_arg_idx = 0;
        let mut fp_arg_idx = 0;

        for i in 0..insn.src.len() {
            if info.stack_arg_indices.contains(&i) {
                continue;
            }
            let arg = insn.src[i];
            let arg_type = insn.arg_types.get(i).copied();
            let is_complex = arg_type.is_some_and(|t| types.is_complex(t));
            let is_fp = if let Some(typ) = arg_type {
                types.is_float(typ)
            } else {
                let arg_loc = self.get_location(arg);
                matches!(arg_loc, Loc::Xmm(_) | Loc::FImm(..))
            };

            // Get actual arg size without clamping - emit_move handles sub-32-bit properly
            // This is needed for Float16 (16-bit) to get correct zero-extension
            let arg_size = if let Some(typ) = arg_type {
                types.size_bits(typ)
            } else {
                64
            };

            if is_complex {
                // §3.2.3, by base type:
                //   long double _Complex -> COMPLEX_X87, passed in memory.
                //     Already in `stack_arg_indices`, so there is nothing to
                //     put in a register — and nothing *can* be: an x87 value
                //     has no XMM form, which is what produced `movt %xmm0`.
                //   float _Complex       -> one eightbyte, both halves packed
                //                           into a single XMM.
                //   double _Complex      -> two eightbytes, XMM(n) and XMM(n+1).
                let base = types.complex_base(arg_type.unwrap());
                if types.kind(base) == TypeKind::LongDouble {
                    continue;
                }
                let packed = types.size_bits(arg_type.unwrap()) <= 64;
                self.setup_complex_arg(
                    arg,
                    arg_type,
                    fp_arg_regs[fp_arg_idx],
                    // Only read for the two-register form.
                    fp_arg_regs[(fp_arg_idx + 1).min(fp_arg_regs.len() - 1)],
                    types,
                );
                fp_arg_idx += if packed { 1 } else { 2 };
            } else if is_fp {
                let fp_size = if let Some(typ) = arg_type {
                    types.size_bits(typ)
                } else {
                    64
                };
                // The *type* decides the register format: x87 extended and
                // binary128 are both 128 bits wide here, and only the type
                // tells them apart.
                let fp_fmt = self.fp_format(arg_type, fp_size, types);
                // Long double uses x87 and is passed on stack, not in XMM registers
                // Skip it here - it's handled by push_stack_args
                let is_longdouble = arg_type.is_some_and(|t| types.kind(t) == TypeKind::LongDouble);
                if is_longdouble {
                    continue;
                }
                self.emit_fp_move(arg, fp_arg_regs[fp_arg_idx], fp_fmt);
                fp_arg_idx += 1;
            } else if arg_type.is_some_and(|t| {
                // Eight bytes or fewer, and its single eightbyte is SSE: the
                // argument pseudo holds the *value* at this size, not an
                // address, so it moves straight into an XMM register the way a
                // floating scalar does. It used to take a general register,
                // while the return side -- which asks the class rather than the
                // size -- had it right.
                types.size_bits(t) <= 64 && crate::abi::sse_struct_regs(t, types).is_some()
            }) {
                if fp_arg_idx < fp_arg_regs.len() {
                    let size_bits = arg_type.map_or(64, |t| types.size_bits(t));
                    let fmt = FpSize::for_sse_aggregate(size_bits);
                    let dst = fp_arg_regs[fp_arg_idx];
                    self.emit_fp_move(arg, dst, fmt);
                    fp_arg_idx += 1;
                }
            } else if arg_type.is_some_and(|t| {
                let k = types.kind(t);
                (k == TypeKind::Struct || k == TypeKind::Union)
                    && types.size_bits(t) > 64
                    && types.size_bits(t) <= 128
            }) {
                // Medium struct (9-16 bytes, e.g., {double, double}):
                // Load two 8-byte fields into register pairs per ABI classification.
                // The arg pseudo holds the struct's address.
                let abi = crate::abi::SysVAmd64Abi;
                let arg_class = abi.classify_param(arg_type.unwrap(), types);
                if let crate::abi::ArgClass::Direct { ref classes, .. } = arg_class {
                    if !classes.is_empty()
                        && classes.iter().all(|c| *c == crate::abi::RegClass::Sse)
                    {
                        // Two SSE registers: load two 8-byte doubles from struct address.
                        // The arg pseudo holds a pointer (from symaddr), not struct bytes.
                        // For Loc::Stack, MOV loads the pointer; LEA would give the
                        // address of the stack slot itself (pointer-to-pointer → garbage).
                        let arg_loc = self.get_location(arg);
                        let base = match arg_loc {
                            Loc::Reg(r) => r,
                            Loc::Stack(offset) => {
                                let adjusted = offset + self.callee_saved_offset;
                                self.push_lir(X86Inst::Mov {
                                    size: OperandSize::B64,
                                    src: GpOperand::Mem(MemAddr::BaseOffset {
                                        base: Reg::Rbp,
                                        offset: -adjusted,
                                    }),
                                    dst: GpOperand::Reg(Reg::R11),
                                });
                                Reg::R11
                            }
                            _ => {
                                self.emit_move(arg, Reg::R11, 64);
                                Reg::R11
                            }
                        };
                        // Two doubles are one register each. A lone binary128
                        // is SSE+SSEUP: one register carrying all sixteen bytes,
                        // so loading two eight-byte halves into two registers
                        // would hand a gcc-compiled callee only the first.
                        if classes.len() == 1 {
                            self.push_lir(X86Inst::MovFp {
                                size: FpSize::for_sse_aggregate(
                                    arg_type.map_or(64, |t| types.size_bits(t)),
                                ),
                                src: XmmOperand::Mem(MemAddr::BaseOffset { base, offset: 0 }),
                                dst: XmmOperand::Reg(fp_arg_regs[fp_arg_idx]),
                            });
                        } else {
                            self.push_lir(X86Inst::MovFp {
                                size: FpSize::Double,
                                src: XmmOperand::Mem(MemAddr::BaseOffset { base, offset: 0 }),
                                dst: XmmOperand::Reg(fp_arg_regs[fp_arg_idx]),
                            });
                            self.push_lir(X86Inst::MovFp {
                                size: FpSize::Double,
                                src: XmmOperand::Mem(MemAddr::BaseOffset { base, offset: 8 }),
                                dst: XmmOperand::Reg(fp_arg_regs[fp_arg_idx + 1]),
                            });
                        }
                        fp_arg_idx += classes.len();
                    } else if classes.iter().all(|c| *c == crate::abi::RegClass::Integer)
                        && classes.len() == 2
                    {
                        // Two integer registers
                        let arg_loc = self.get_location(arg);
                        let addr = match arg_loc {
                            Loc::Reg(r) => r,
                            _ => {
                                self.emit_move(arg, Reg::R10, 64);
                                Reg::R10
                            }
                        };
                        // Load first 8 bytes
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::B64,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: addr,
                                offset: 0,
                            }),
                            dst: GpOperand::Reg(int_arg_regs[int_arg_idx]),
                        });
                        // Load second 8 bytes
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::B64,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: addr,
                                offset: 8,
                            }),
                            dst: GpOperand::Reg(int_arg_regs[int_arg_idx + 1]),
                        });
                        int_arg_idx += 2;
                    } else {
                        // Mixed: one eightbyte to a general register and one to
                        // an SSE register, in the order the class vector gives.
                        // Falling through to a single integer register handed
                        // the callee a pointer's worth of one half.
                        let base = self.struct_arg_base(arg);
                        for (i, class) in classes.iter().enumerate() {
                            let addr = MemAddr::BaseOffset {
                                base,
                                offset: (i * 8) as i32,
                            };
                            if *class == crate::abi::RegClass::Sse {
                                self.push_lir(X86Inst::MovFp {
                                    size: FpSize::Double,
                                    src: XmmOperand::Mem(addr),
                                    dst: XmmOperand::Reg(fp_arg_regs[fp_arg_idx]),
                                });
                                fp_arg_idx += 1;
                            } else {
                                self.push_lir(X86Inst::Mov {
                                    size: OperandSize::B64,
                                    src: GpOperand::Mem(addr),
                                    dst: GpOperand::Reg(int_arg_regs[int_arg_idx]),
                                });
                                int_arg_idx += 1;
                            }
                        }
                    }
                } else {
                    // Indirect — shouldn't happen for medium structs but handle anyway
                    self.setup_int_arg(arg, arg_size, int_arg_regs[int_arg_idx], saved_arg_regs);
                    int_arg_idx += 1;
                }
            } else if arg_type.is_some_and(|t| types.kind(t) == TypeKind::Int128) {
                // __int128 argument: load lo and hi halves into two consecutive GP registers
                if int_arg_idx + 1 < int_arg_regs.len() {
                    let arg_loc = self.get_location(arg).clone();
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(self.int128_lo_mem_loc(&arg_loc)),
                        dst: GpOperand::Reg(int_arg_regs[int_arg_idx]),
                    });
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(self.int128_hi_mem_loc(&arg_loc)),
                        dst: GpOperand::Reg(int_arg_regs[int_arg_idx + 1]),
                    });
                }
                int_arg_idx += 2;
            } else {
                self.setup_int_arg(arg, arg_size, int_arg_regs[int_arg_idx], saved_arg_regs);
                int_arg_idx += 1;
            }
        }

        fp_arg_idx
    }

    /// Set up a complex number argument (real + imaginary in two XMM registers)
    fn setup_complex_arg(
        &mut self,
        arg: PseudoId,
        arg_type: Option<crate::types::TypeId>,
        real_reg: XmmReg,
        imag_reg: XmmReg,
        types: &TypeTable,
    ) {
        let arg_loc = self.get_location(arg);
        let complex_ty = arg_type.unwrap();
        let (fp_size, imag_offset) = complex_fp_info(types, &self.base.target, complex_ty);

        // `float _Complex` is a single eightbyte: both floats live in the low
        // 64 bits of one XMM. Loading the halves into two registers left the
        // imaginary part somewhere the callee never looks — `cimagf` read 0
        // while `crealf` happened to be right, a silent wrong answer. One
        // 64-bit move carries the whole value, mirroring what the *return*
        // path has always done.
        let packed = types.size_bits(complex_ty) <= 64;
        let load_size = if packed { FpSize::Double } else { fp_size };

        match arg_loc {
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                // A symbol's slot *is* the complex value; a temp's slot holds
                // a pointer to it. A call returning complex yields the former
                // (`__cret_N`), so unconditionally loading the slot as a
                // pointer read the value's bytes as an address — `f(g())`
                // faulted in the callee.
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == arg)
                    .is_some_and(|p| matches!(p.kind, crate::ir::PseudoKind::Sym(_)));
                if is_symbol {
                    self.push_lir(X86Inst::Lea {
                        dst: Reg::R11,
                        addr: MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -adjusted,
                        },
                    });
                } else {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -adjusted,
                        }),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                }
                self.push_lir(X86Inst::MovFp {
                    size: load_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: 0,
                    }),
                    dst: XmmOperand::Reg(real_reg),
                });
                if !packed {
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: imag_offset,
                        }),
                        dst: XmmOperand::Reg(imag_reg),
                    });
                }
            }
            Loc::Reg(r) => {
                self.push_lir(X86Inst::MovFp {
                    size: load_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset { base: r, offset: 0 }),
                    dst: XmmOperand::Reg(real_reg),
                });
                if !packed {
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: r,
                            offset: imag_offset,
                        }),
                        dst: XmmOperand::Reg(imag_reg),
                    });
                }
            }
            _ => {}
        }
    }

    /// Set up an integer argument, using saved register if source was clobbered
    fn setup_int_arg(
        &mut self,
        arg: PseudoId,
        arg_size: u32,
        dest_reg: Reg,
        saved_arg_regs: &HashMap<Reg, Reg>,
    ) {
        let arg_loc = self.get_location(arg);
        if let Loc::Reg(src_reg) = arg_loc {
            if let Some(&saved_reg) = saved_arg_regs.get(&src_reg) {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::from_bits(arg_size),
                    src: GpOperand::Reg(saved_reg),
                    dst: GpOperand::Reg(dest_reg),
                });
                return;
            }
        }
        self.emit_move(arg, dest_reg, arg_size);
    }

    /// Handle call return value using ABI classification.
    pub(super) fn handle_call_return_value(&mut self, insn: &Instruction, types: &TypeTable) {
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_loc = self.get_location(target);
        let ret_size = insn
            .typ
            .map(|t| types.size_bits(t).max(32))
            .unwrap_or(insn.size.max(32));
        let ret_fmt = self.fp_format(insn.typ, ret_size, types);

        let abi_info = insn
            .abi_info
            .as_ref()
            .expect("abi_info must be populated for Call instructions");

        match &abi_info.ret {
            ArgClass::Direct { classes, size_bits } => {
                // Check for two-register struct return (9-16 bytes)
                if *size_bits > 64 && classes.len() == 2 {
                    // Check if it's two INTEGER (struct) vs two SSE (complex) vs mixed
                    if classes.iter().all(|c| *c == RegClass::Integer) {
                        self.handle_two_reg_return(&dst_loc);
                        return;
                    }
                    // Check for mixed SSE+INTEGER return (e.g., {double, int})
                    if classes.contains(&RegClass::Sse) && classes.contains(&RegClass::Integer) {
                        self.handle_mixed_return(&dst_loc, classes);
                        return;
                    }
                }
                // Check for complex return (two SSE registers)
                if classes.len() == 2 && classes.iter().all(|c| *c == RegClass::Sse) {
                    let is_complex_result = insn.typ.is_some_and(|t| {
                        types.is_complex(t) && crate::arch::lir::complex_sse_regs(types, t) > 0
                    });
                    if is_complex_result {
                        self.handle_complex_return(insn, &dst_loc, types);
                        return;
                    }
                    // Two SSE struct return (not complex)
                    self.handle_two_sse_return(&dst_loc);
                    return;
                }
                // Check for single SSE return
                if classes.first() == Some(&RegClass::Sse) {
                    // An aggregate's type does not say how wide the move is --
                    // `fp_format` sees a struct and answers `Double` -- so for
                    // one the class's size decides. A scalar keeps its own
                    // type's answer, which is the more precise one.
                    let fmt = if insn.typ.is_some_and(|t| {
                        matches!(types.kind(t), TypeKind::Struct | TypeKind::Union)
                    }) {
                        FpSize::for_sse_aggregate(*size_bits)
                    } else {
                        ret_fmt
                    };
                    self.emit_fp_move_from_xmm(XmmReg::Xmm0, &dst_loc, fmt);
                    return;
                }
                // Integer return
                self.emit_move_to_loc(Reg::Rax, &dst_loc, ret_size);
            }
            ArgClass::Indirect { .. } => {
                // sret: return value already written to memory, nothing to do
            }
            ArgClass::Hfa { count, base } => {
                // HFA returns (primarily AArch64, but handle for completeness)
                // Complex types are similar - return in XMM0, XMM1
                if *count == 2 {
                    let is_complex_result = insn.typ.is_some_and(|t| {
                        types.is_complex(t) && crate::arch::lir::complex_sse_regs(types, t) > 0
                    });
                    if is_complex_result {
                        self.handle_complex_return(insn, &dst_loc, types);
                        return;
                    }
                }
                // For other HFA cases, treat as FP return
                let size_bits = match base {
                    crate::abi::HfaBase::Float16 => 16,
                    crate::abi::HfaBase::Float32 => 32,
                    crate::abi::HfaBase::Float64 => 64,
                    // System V has no HFA concept and `long double` is x87
                    // there, so this classification never reaches x86_64.
                    crate::abi::HfaBase::Float128 => {
                        unreachable!("binary128 HFA is an AAPCS64 classification")
                    }
                };
                self.emit_fp_move_from_xmm(
                    XmmReg::Xmm0,
                    &dst_loc,
                    FpSize::from_bits(size_bits, &self.base.target),
                );
            }
            ArgClass::Extend { .. } => {
                // Extended return value in RAX
                self.emit_move_to_loc(Reg::Rax, &dst_loc, ret_size);
            }
            ArgClass::X87 { .. } => {
                let is_complex_x87 = insn.typ.is_some_and(|t| {
                    types.is_complex(t) && crate::arch::lir::complex_sse_regs(types, t) == 0
                });
                if is_complex_x87 {
                    // COMPLEX_X87: st(0) holds the real part and st(1) the
                    // imaginary one. `fstpt` pops, so storing st(0) brings the
                    // imaginary part to the top for the second store. Both
                    // must be popped or the x87 stack leaks across the call.
                    let base = types.complex_base(insn.typ.unwrap());
                    let imag_off = (types.size_bits(base) / 8) as i32;
                    let base_addr = self.address_of_pseudo(target);
                    self.push_lir(X86Inst::X87Store {
                        addr: MemAddr::BaseOffset {
                            base: base_addr,
                            offset: 0,
                        },
                    });
                    self.push_lir(X86Inst::X87Store {
                        addr: MemAddr::BaseOffset {
                            base: base_addr,
                            offset: imag_off,
                        },
                    });
                } else {
                    // Long double returned in ST(0) - store to destination
                    let dst_addr = self.get_x87_mem_addr(target);
                    self.push_lir(X86Inst::X87Store { addr: dst_addr });
                }
            }
            ArgClass::Ignore => {
                // Void return, nothing to do
            }
        }
    }

    /// Handle two-register struct return (RAX + RDX)
    fn handle_two_reg_return(&mut self, dst_loc: &Loc) {
        match dst_loc {
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rdx),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted + 8,
                    }),
                });
            }
            Loc::Reg(r) => {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *r,
                        offset: 0,
                    }),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rdx),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *r,
                        offset: 8,
                    }),
                });
            }
            _ => {}
        }
    }

    /// Handle complex return value.
    ///
    /// For float _Complex (8 bytes): entire value is packed in XMM0
    ///   - Real part in bits 0-31, imag part in bits 32-63
    ///   - Use a single 64-bit (movq) store to write both parts
    ///
    /// For double _Complex (16 bytes): split across XMM0 and XMM1
    ///   - Real part in XMM0, imag part in XMM1
    ///   - Use two separate 64-bit stores
    fn handle_complex_return(&mut self, insn: &Instruction, dst_loc: &Loc, types: &TypeTable) {
        let (fp_size, imag_offset) = complex_fp_info(types, &self.base.target, insn.typ.unwrap());

        // float _Complex: packed in XMM0 (8 bytes total)
        // double _Complex: XMM0 (real) + XMM1 (imag)
        let is_float_complex = fp_size == FpSize::Single;

        match dst_loc {
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                if is_float_complex {
                    // Store entire 64-bit value from XMM0 (packed real + imag)
                    self.push_lir(X86Inst::MovFp {
                        size: FpSize::Double, // 64-bit movq
                        src: XmmOperand::Reg(XmmReg::Xmm0),
                        dst: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -adjusted,
                        }),
                    });
                } else {
                    // Store XMM0 (real) and XMM1 (imag) separately
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Reg(XmmReg::Xmm0),
                        dst: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -adjusted,
                        }),
                    });
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Reg(XmmReg::Xmm1),
                        dst: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -adjusted + imag_offset,
                        }),
                    });
                }
            }
            Loc::Reg(r) => {
                if is_float_complex {
                    // Store entire 64-bit value from XMM0 (packed real + imag)
                    self.push_lir(X86Inst::MovFp {
                        size: FpSize::Double, // 64-bit movq
                        src: XmmOperand::Reg(XmmReg::Xmm0),
                        dst: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: *r,
                            offset: 0,
                        }),
                    });
                } else {
                    // Store XMM0 (real) and XMM1 (imag) separately
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Reg(XmmReg::Xmm0),
                        dst: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: *r,
                            offset: 0,
                        }),
                    });
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Reg(XmmReg::Xmm1),
                        dst: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: *r,
                            offset: imag_offset,
                        }),
                    });
                }
            }
            _ => {}
        }
    }

    /// Handle mixed SSE+INTEGER return (e.g., struct {double, int})
    fn handle_mixed_return(&mut self, dst_loc: &Loc, classes: &[RegClass]) {
        // For mixed returns: SSE eightbytes use XMM0, XMM1; INTEGER use RAX, RDX
        // Order of classes determines memory layout
        match dst_loc {
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                let mut xmm_idx = 0;
                let mut gp_idx = 0;
                for (i, &class) in classes.iter().enumerate() {
                    let mem_offset = i as i32 * 8;
                    match class {
                        RegClass::Sse => {
                            let xmm = if xmm_idx == 0 {
                                XmmReg::Xmm0
                            } else {
                                XmmReg::Xmm1
                            };
                            self.push_lir(X86Inst::MovFp {
                                size: FpSize::Double,
                                src: XmmOperand::Reg(xmm),
                                dst: XmmOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rbp,
                                    offset: -adjusted + mem_offset,
                                }),
                            });
                            xmm_idx += 1;
                        }
                        RegClass::Integer => {
                            let gp = if gp_idx == 0 { Reg::Rax } else { Reg::Rdx };
                            self.push_lir(X86Inst::Mov {
                                size: OperandSize::B64,
                                src: GpOperand::Reg(gp),
                                dst: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rbp,
                                    offset: -adjusted + mem_offset,
                                }),
                            });
                            gp_idx += 1;
                        }
                        _ => {}
                    }
                }
            }
            Loc::Reg(r) => {
                let mut xmm_idx = 0;
                let mut gp_idx = 0;
                for (i, &class) in classes.iter().enumerate() {
                    let mem_offset = i as i32 * 8;
                    match class {
                        RegClass::Sse => {
                            let xmm = if xmm_idx == 0 {
                                XmmReg::Xmm0
                            } else {
                                XmmReg::Xmm1
                            };
                            self.push_lir(X86Inst::MovFp {
                                size: FpSize::Double,
                                src: XmmOperand::Reg(xmm),
                                dst: XmmOperand::Mem(MemAddr::BaseOffset {
                                    base: *r,
                                    offset: mem_offset,
                                }),
                            });
                            xmm_idx += 1;
                        }
                        RegClass::Integer => {
                            let gp = if gp_idx == 0 { Reg::Rax } else { Reg::Rdx };
                            self.push_lir(X86Inst::Mov {
                                size: OperandSize::B64,
                                src: GpOperand::Reg(gp),
                                dst: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: *r,
                                    offset: mem_offset,
                                }),
                            });
                            gp_idx += 1;
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    /// Handle two SSE register return (XMM0 + XMM1) for struct with all FP fields
    fn handle_two_sse_return(&mut self, dst_loc: &Loc) {
        match dst_loc {
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                self.push_lir(X86Inst::MovFp {
                    size: FpSize::Double,
                    src: XmmOperand::Reg(XmmReg::Xmm0),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                });
                self.push_lir(X86Inst::MovFp {
                    size: FpSize::Double,
                    src: XmmOperand::Reg(XmmReg::Xmm1),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted + 8,
                    }),
                });
            }
            Loc::Reg(r) => {
                self.push_lir(X86Inst::MovFp {
                    size: FpSize::Double,
                    src: XmmOperand::Reg(XmmReg::Xmm0),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: *r,
                        offset: 0,
                    }),
                });
                self.push_lir(X86Inst::MovFp {
                    size: FpSize::Double,
                    src: XmmOperand::Reg(XmmReg::Xmm1),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: *r,
                        offset: 8,
                    }),
                });
            }
            _ => {}
        }
    }

    /// Emit the actual call instruction (direct or indirect)
    pub(super) fn emit_call_instruction(&mut self, insn: &Instruction, func_name: &str) {
        if insn.indirect_target.is_some() {
            self.push_lir(X86Inst::Call {
                target: CallTarget::Indirect(Reg::R11),
            });
        } else {
            self.push_lir(X86Inst::Call {
                target: CallTarget::Direct(Symbol::global(func_name.to_string())),
            });
        }
    }

    /// Clean up stack after call
    pub(super) fn cleanup_call_stack(&mut self, stack_cleanup: usize) {
        // The outgoing area is gone from here on, so locals are once again
        // where the frame layout says they are.
        self.rsp_adjust -= stack_cleanup as i32;
        if stack_cleanup > 0 {
            self.push_lir(X86Inst::Add {
                size: OperandSize::B64,
                src: GpOperand::Imm(stack_cleanup as i64),
                dst: Reg::Rsp,
            });
        }
    }

    /// Set AL to number of XMM registers used (for variadic calls)
    pub(super) fn set_variadic_fp_count(&mut self, fp_arg_count: usize) {
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B8,
            src: GpOperand::Imm(fp_arg_count as i64),
            dst: GpOperand::Reg(Reg::Rax),
        });
    }
}
