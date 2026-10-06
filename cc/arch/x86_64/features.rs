//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 Feature Code Generation (Variadic Functions, Byte Swapping, Bit Counting)
//

use super::call::{BlockDst, UNROLL_LIMIT_BYTES};
use super::codegen::X86_64CodeGen;
use super::lir::{popcount_sequence, GpOperand, MemAddr, ShiftCount, X86Inst};
use super::regalloc::{Loc, Reg};
use crate::arch::codegen::BswapSize;
use crate::arch::lir::{CallTarget, CondCode, Directive, Label, OperandSize, Symbol};
use crate::ir::Instruction;
use crate::types::TypeTable;

/// Where an aggregate `va_arg` result is written.
#[derive(Clone, Copy)]
enum VaAggDst {
    /// A stack slot; the slot is the aggregate.
    Slot(i32),
    /// A register holding the aggregate's address.
    Addr(Reg),
    /// A register that *is* the aggregate, which fits in it.
    Value(Reg),
}

impl X86_64CodeGen {
    // ========================================================================
    // Variadic function support (va_* builtins)
    // ========================================================================
    //
    // On x86-64 System V ABI, va_list is a 24-byte struct:
    //   struct {
    //       unsigned int gp_offset;     // offset to next GP reg in save area
    //       unsigned int fp_offset;     // offset to next FP reg in save area
    //       void *overflow_arg_area;    // pointer to stack arguments
    //       void *reg_save_area;        // pointer to register save area
    //   };
    //
    // This implementation provides a simplified version that works with
    // stack-based arguments. Full register save area support would require
    // function prologue changes.

    /// Emit va_start: Initialize va_list
    pub(super) fn emit_va_start(&mut self, insn: &Instruction) {
        let ap_addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };

        let ap_loc = self.get_location(ap_addr);

        // For x86-64 System V ABI:
        // va_list is a 24-byte struct. We initialize:
        // - gp_offset = fixed_gp_params * 8 (offset to first variadic GP arg in save area)
        // - fp_offset = 48 + fixed_fp_params * 16 (offset to first variadic FP arg)
        // - overflow_arg_area = where the variadic stack arguments begin
        // - reg_save_area = pointer to where we saved the argument registers

        let gp_offset = (self.named_gp_regs * 8) as i32;
        let fp_offset = 48 + (self.named_fp_regs * 16) as i32;
        let reg_save_base = self.reg_save_area_offset;

        match ap_loc {
            Loc::Stack(offset) => {
                // gp_offset = offset to next variadic GP arg
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(gp_offset as i64),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset,
                    }),
                });
                // fp_offset = offset to next variadic FP arg (48 + fixed_fp_params * 16)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(fp_offset as i64),
                    dst: GpOperand::Mem(self.stack_field(offset, 4)),
                });
                // Past every named parameter that occupies the incoming
                // area -- which is not the same as every named parameter that
                // overflowed a register file. An X87 or MEMORY-class named
                // parameter takes bytes here and no register at all, and
                // alignment may pad between them, so the allocator hands us
                // the displacement rather than a slot count.
                let overflow_offset = self.named_incoming_end;
                self.push_lir(X86Inst::Lea {
                    // Not a stack slot: the overflow argument area is a real
                    // `%rbp + 16` address, above the return address, where the
                    // caller's stacked arguments begin.
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: overflow_offset,
                    },
                    dst: Reg::Rax,
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(self.stack_field(offset, 8)),
                });
                // reg_save_area = pointer to saved registers
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -reg_save_base,
                    },
                    dst: Reg::Rax,
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(self.stack_field(offset, 16)),
                });
            }
            Loc::Reg(r) => {
                // Register contains the address of the va_list struct
                // Use R10 as scratch to avoid clobbering the va_list address if r == Rax
                // gp_offset = offset to next variadic GP arg
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(gp_offset as i64),
                    dst: GpOperand::Mem(MemAddr::BaseOffset { base: r, offset: 0 }),
                });
                // fp_offset = offset to next variadic FP arg
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(fp_offset as i64),
                    dst: GpOperand::Mem(MemAddr::BaseOffset { base: r, offset: 4 }),
                });
                // Past every named parameter that occupies the incoming
                // area; see the sibling arm above.
                let overflow_offset = self.named_incoming_end;
                self.push_lir(X86Inst::Lea {
                    // Not a stack slot: the overflow argument area is a real
                    // `%rbp + 16` address, above the return address, where the
                    // caller's stacked arguments begin.
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: overflow_offset,
                    },
                    dst: Reg::R10,
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Mem(MemAddr::BaseOffset { base: r, offset: 8 }),
                });
                // reg_save_area = pointer to saved registers
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -reg_save_base,
                    },
                    dst: Reg::R10,
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: r,
                        offset: 16,
                    }),
                });
            }
            _ => {}
        }
    }

    /// Helper for emit_va_arg: emit integer path for va_arg
    fn emit_va_arg_int(
        &mut self,
        ap_base: Reg,
        ap_base_offset: i32,
        dst_loc: &Loc,
        arg_size: u32,
        arg_bytes: i32,
        label_suffix: u32,
    ) {
        // Registers used here: R10 holds `gp_offset`, RAX the address being
        // read from, RCX the value in transit.
        //
        // **R11 is deliberately not among them.** `emit_va_arg` materializes
        // the `va_list` pointer into R11 when `ap` is a slot holding a pointer
        // rather than the object itself, so R11 *is* `ap_base` in that shape.
        // Using it as a shuttle overwrote the base before the `gp_offset`
        // write-back, which then stored through the value just loaded -- the
        // `movl %r10d, (%r11)` that faulted. The overflow path had the same
        // shape twice over: it loaded `overflow_arg_area` into R11 and then
        // wrote the advanced pointer back to `8(%r11)`, an address derived
        // from the pointer it had just destroyed.
        let overflow_label = Label::internal("va_overflow", label_suffix);
        let done_label = Label::internal("va_done", label_suffix);
        let lir_arg_size = OperandSize::from_bits(arg_size);

        // gp_offset -> R10d
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset,
            }),
            dst: GpOperand::Reg(Reg::R10),
        });
        self.push_lir(X86Inst::Cmp {
            size: OperandSize::B32,
            src: GpOperand::Imm(48),
            dst: GpOperand::Reg(Reg::R10),
        });
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Uge,
            target: overflow_label.clone(),
        });

        // Register save area path: RAX = reg_save_area + gp_offset.
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 16,
            }),
            dst: GpOperand::Reg(Reg::Rax),
        });
        self.push_lir(X86Inst::Movsx {
            src_size: OperandSize::B32,
            dst_size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R10),
            dst: Reg::Rcx,
        });
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rcx),
            dst: Reg::Rax,
        });

        // gp_offset += 8, committed *before* the value is read. The value has
        // to land somewhere, and its destination may be any register the
        // allocator picked -- including RAX, which still holds the address, or
        // R10, which still holds the cursor. Committing first means nothing
        // the store could overwrite is needed afterwards.
        self.push_lir(X86Inst::Add {
            size: OperandSize::B32,
            src: GpOperand::Imm(8),
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Reg(Reg::R10),
            dst: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset,
            }),
        });

        self.va_store_scalar(lir_arg_size, Reg::Rax, dst_loc);

        self.push_lir(X86Inst::Jmp {
            target: done_label.clone(),
        });

        // Overflow path: RAX = overflow_arg_area.
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(overflow_label)));
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 8,
            }),
            dst: GpOperand::Reg(Reg::Rax),
        });

        // overflow_arg_area += arg_bytes, again committed before the read:
        // `va_store_scalar` may load the value straight into RAX when that is
        // the destination, and RAX is the pointer being advanced.
        self.push_lir(X86Inst::Lea {
            addr: MemAddr::BaseOffset {
                base: Reg::Rax,
                offset: arg_bytes,
            },
            dst: Reg::Rcx,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rcx),
            dst: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 8,
            }),
        });

        self.va_store_scalar(lir_arg_size, Reg::Rax, dst_loc);

        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
    }

    /// `va_arg` of an `__int128`, which occupies **two** INTEGER eightbytes.
    ///
    /// System V AMD64 psABI 3.2.3 assigns them to the next two available
    /// general registers, with no even-pair requirement -- that rule is
    /// AAPCS64 stage C.10, not this ABI, and gcc confirms it by emitting the
    /// plain `cmpl $39, %edx; ja` with no mask on `gp_offset`. Both eightbytes
    /// must fit or the whole argument goes to the overflow area, which is why
    /// the guard is `gp_offset <= 48 - 16` rather than the scalar `< 48`.
    ///
    /// Before this existed, `__int128` fell into the scalar path, where
    /// `OperandSize::from_bits(128)` saturates at 64: only the low eightbyte
    /// was ever moved, the guard was the scalar one, and `gp_offset` advanced
    /// by 8 -- so the next `va_arg` re-read the high half of this one.
    fn emit_va_arg_int128(
        &mut self,
        ap_base: Reg,
        ap_base_offset: i32,
        dst_loc: &Loc,
        label_suffix: u32,
    ) {
        let overflow_label = Label::internal("va_overflow", label_suffix);
        let done_label = Label::internal("va_done", label_suffix);

        // gp_offset -> R10d; both eightbytes must fit in the save area.
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset,
            }),
            dst: GpOperand::Reg(Reg::R10),
        });
        self.push_lir(X86Inst::Cmp {
            size: OperandSize::B32,
            src: GpOperand::Imm(48 - 16 + 1),
            dst: GpOperand::Reg(Reg::R10),
        });
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Uge,
            target: overflow_label.clone(),
        });

        // RAX = reg_save_area + gp_offset.
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 16,
            }),
            dst: GpOperand::Reg(Reg::Rax),
        });
        self.push_lir(X86Inst::Movsx {
            src_size: OperandSize::B32,
            dst_size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R10),
            dst: Reg::Rcx,
        });
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rcx),
            dst: Reg::Rax,
        });

        // The cursor is committed before the copy, for the reason given in
        // `emit_va_arg_int`.
        self.push_lir(X86Inst::Add {
            size: OperandSize::B32,
            src: GpOperand::Imm(16),
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Reg(Reg::R10),
            dst: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset,
            }),
        });

        self.va_store_pair(Reg::Rax, dst_loc);

        self.push_lir(X86Inst::Jmp {
            target: done_label.clone(),
        });

        // Overflow path. The type is 16-byte aligned, so the area pointer is
        // rounded up before the read -- gcc emits the same `addq $15; andq
        // $-16` here -- and advanced by 16 afterwards.
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(overflow_label)));
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 8,
            }),
            dst: GpOperand::Reg(Reg::Rax),
        });
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Imm(15),
            dst: Reg::Rax,
        });
        self.push_lir(X86Inst::And {
            size: OperandSize::B64,
            src: GpOperand::Imm(-16),
            dst: Reg::Rax,
        });

        self.push_lir(X86Inst::Lea {
            addr: MemAddr::BaseOffset {
                base: Reg::Rax,
                offset: 16,
            },
            dst: Reg::Rcx,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rcx),
            dst: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 8,
            }),
        });

        self.va_store_pair(Reg::Rax, dst_loc);

        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
    }

    /// Copy the two eightbytes at `[src_ptr]` into a 16-byte destination slot.
    ///
    /// RCX is the shuttle for the same reason as in `va_store_scalar`, and the
    /// high half goes through `int128_hi_mem_loc`, which is the one place that
    /// knows where a 128-bit slot's upper eightbyte lives.
    fn va_store_pair(&mut self, src_ptr: Reg, dst_loc: &Loc) {
        match dst_loc {
            Loc::Stack(dst_offset) => {
                for (byte, dst) in [
                    (0, self.stack_mem(*dst_offset)),
                    (8, self.int128_hi_mem_loc(dst_loc)),
                ] {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: src_ptr,
                            offset: byte,
                        }),
                        dst: GpOperand::Reg(Reg::Rcx),
                    });
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::Rcx),
                        dst: GpOperand::Mem(dst),
                    });
                }
            }
            other => {
                // A 128-bit result needs sixteen bytes of storage, and the
                // linearizer gives it a slot. Anything else here means that
                // stopped being true, and emitting nothing would leave the
                // value undefined rather than say so.
                crate::diag::error(
                    crate::diag::Position::default(),
                    &format!("internal error: va_arg of __int128 into {other:?}"),
                );
            }
        }
    }

    /// Copy one scalar `va_arg` result from `[src_ptr]` to its destination.
    ///
    /// RCX is the shuttle, because a memory destination needs one and the two
    /// registers that must survive -- `ap_base` and `src_ptr` -- may be any
    /// of the others. The destination goes through `stack_mem`, which picks
    /// the frame's base register; spelling `-(slot + callee_saved_offset)(%rbp)`
    /// by hand wrote outside the frame whenever a local forced the stack to be
    /// realigned.
    fn va_store_scalar(&mut self, size: OperandSize, src_ptr: Reg, dst_loc: &Loc) {
        match dst_loc {
            Loc::Reg(r) => {
                self.push_lir(X86Inst::Mov {
                    size,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: src_ptr,
                        offset: 0,
                    }),
                    dst: GpOperand::Reg(*r),
                });
            }
            Loc::Stack(dst_offset) => {
                self.push_lir(X86Inst::Mov {
                    size,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: src_ptr,
                        offset: 0,
                    }),
                    dst: GpOperand::Reg(Reg::Rcx),
                });
                self.push_lir(X86Inst::Mov {
                    size,
                    src: GpOperand::Reg(Reg::Rcx),
                    dst: GpOperand::Mem(self.stack_mem(*dst_offset)),
                });
            }
            _ => {}
        }
    }

    /// Where an aggregate `va_arg` result goes, resolved once so the copy
    /// cannot collide with the registers used to find the source.
    ///
    /// The result pseudo is allocated like any other and can land in `%rax` --
    /// which is where the save-area pointer lives -- so an address held in a
    /// register is moved into reserved scratch before anything else is
    /// touched.
    fn va_agg_dst(&mut self, dst_loc: &Loc, dst_is_addr: bool) -> Option<VaAggDst> {
        Some(match dst_loc {
            Loc::Stack(slot) => VaAggDst::Slot(*slot),
            Loc::Reg(r) if dst_is_addr => {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(*r),
                    dst: GpOperand::Reg(Reg::R10),
                });
                VaAggDst::Addr(Reg::R10)
            }
            // At or below eight bytes the result *is* the register.
            Loc::Reg(r) => VaAggDst::Value(*r),
            _ => return None,
        })
    }

    /// Copy `nbytes` from `[src_base + src_off]` to `dst` at `dst_off`, in the
    /// descending power-of-two chunks `block_chunks` gives, so nothing past the
    /// object is written.
    ///
    /// `%rcx` is the shuttle: it is declared clobbered by `VaArg`, so no live
    /// value is in it, and unlike `%r11` it cannot be `ap_base` (the va_list
    /// pointer lands there when it comes from a stack slot).
    ///
    /// Past [`UNROLL_LIMIT_BYTES`] the whole eightbytes go through `rep movsq`
    /// and only the ragged tail is unrolled. Without a bound this was linear in
    /// the aggregate -- 4 KB cost about 1100 instructions and 256 KB would be
    /// the compile-time explosion the IR's own limit exists to prevent -- and
    /// `va_arg` is the one place a whole aggregate is copied where the size is
    /// the program's to choose.
    fn va_copy_bytes(
        &mut self,
        src_base: Reg,
        src_off: i32,
        dst: VaAggDst,
        dst_off: i32,
        nbytes: i32,
    ) {
        // A register destination *is* the aggregate, so it takes one load of
        // the whole slot. Chunking into it wrote each piece over the last, so
        // a five-byte aggregate kept only its fifth byte -- and where the
        // destination register was also the source base, the first write moved
        // the base out from under the reads that followed. Every slot is at
        // least eight bytes, in the save area and on the stack alike.
        if let VaAggDst::Value(r) = dst {
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Mem(MemAddr::BaseOffset {
                    base: src_base,
                    offset: src_off,
                }),
                dst: GpOperand::Reg(r),
            });
            return;
        }
        let mut at = 0;
        if i64::from(nbytes) > UNROLL_LIMIT_BYTES {
            let qwords = i64::from(nbytes) / 8;
            // Neither base is `%rsp`, so the helper's pushes leave both where
            // they are; a frame slot is not `%rsp`-relative either.
            let to = match dst {
                VaAggDst::Slot(slot) => BlockDst::At(self.stack_field(slot, dst_off)),
                VaAggDst::Addr(base) => BlockDst::At(MemAddr::BaseOffset {
                    base,
                    offset: dst_off,
                }),
                VaAggDst::Value(_) => unreachable!("a register destination returned above"),
            };
            self.emit_rep_movsq(
                MemAddr::BaseOffset {
                    base: src_base,
                    offset: src_off,
                },
                to,
                qwords,
            );
            at = (qwords * 8) as i32;
        }
        for (off, chunk) in crate::ir::memexpand::block_chunks(i64::from(nbytes - at)) {
            let size = OperandSize::from_bits(chunk.bits());
            let done = at + off as i32;
            self.push_lir(X86Inst::Mov {
                size,
                src: GpOperand::Mem(MemAddr::BaseOffset {
                    base: src_base,
                    offset: src_off + done,
                }),
                dst: GpOperand::Reg(Reg::Rcx),
            });
            let into = match dst {
                VaAggDst::Slot(slot) => GpOperand::Mem(self.stack_field(slot, dst_off + done)),
                VaAggDst::Addr(base) => GpOperand::Mem(MemAddr::BaseOffset {
                    base,
                    offset: dst_off + done,
                }),
                VaAggDst::Value(r) => GpOperand::Reg(r),
            };
            self.push_lir(X86Inst::Mov {
                size,
                src: GpOperand::Reg(Reg::Rcx),
                dst: into,
            });
        }
    }

    /// Read an aggregate argument, per SysV AMD64 §3.5.7.
    ///
    /// An aggregate in registers is *not* contiguous in the save area: its
    /// eightbytes are taken from the general and SSE areas independently, and
    /// those advance by 8 and 16 bytes respectively. So each eightbyte is
    /// fetched from the area its own class names and packed at the
    /// destination. On the overflow path the argument is already laid out as
    /// itself and is copied straight across.
    fn emit_va_arg_aggregate(
        &mut self,
        ap_base: Reg,
        ap_off: i32,
        dst_loc: &Loc,
        arg_type: crate::types::TypeId,
        types: &TypeTable,
        label_suffix: u32,
    ) {
        use crate::abi::{ArgClass, RegClass};
        let size_bytes = crate::abi::slot_bytes(
            types.size_bytes(arg_type).max(1),
            self.base.func_pos,
            "a variadic argument",
        );
        // Resolved before anything is clobbered: `%rax` carries the save-area
        // pointer here and the result pseudo can be allocated to it.
        let Some(dst) = self.va_agg_dst(dst_loc, size_bytes > 8) else {
            return;
        };

        let abi = crate::abi::get_abi_for_conv(crate::abi::CallingConv::C, &self.base.target);
        let classes: Vec<RegClass> = match abi.classify_param(arg_type, types) {
            ArgClass::Direct { classes, .. } => classes,
            // MEMORY class: it was passed on the stack, so it is only ever in
            // the overflow area and no register guard applies.
            _ => Vec::new(),
        };
        let num_gp = classes.iter().filter(|c| **c == RegClass::Integer).count() as i32;
        let num_sse = classes.iter().filter(|c| **c == RegClass::Sse).count() as i32;

        let overflow_label = Label::internal("va_agg_overflow", label_suffix);
        let done_label = Label::internal("va_agg_done", label_suffix);

        // GP_OFFSET_MAX is 48 (six general registers), FP_OFFSET_MAX 176
        // (48 plus eight SSE registers of 16 bytes). An aggregate needs all of
        // its eightbytes in registers or none of them.
        let mut guarded = false;
        for (field, avail, need, step) in [(0i32, 48i64, num_gp, 8i64), (4, 176, num_sse, 16)] {
            if need == 0 {
                continue;
            }
            guarded = true;
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B32,
                src: GpOperand::Mem(MemAddr::BaseOffset {
                    base: ap_base,
                    offset: ap_off + field,
                }),
                dst: GpOperand::Reg(Reg::Rcx),
            });
            self.push_lir(X86Inst::Cmp {
                size: OperandSize::B32,
                src: GpOperand::Imm(avail - step * need as i64),
                dst: GpOperand::Reg(Reg::Rcx),
            });
            self.push_lir(X86Inst::Jcc {
                cc: CondCode::Ugt,
                target: overflow_label.clone(),
            });
        }

        if guarded {
            // How many bytes one SSE entry accounts for.
            //
            // `classes` counts *registers*, not eightbytes, and the two differ
            // for exactly one shape: an SSE+SSEUP pair is a single register
            // carrying all sixteen bytes, as `sse_struct_regs` documents. A
            // struct of two doubles is two entries of eight; a struct holding
            // a `__float128` is one entry of sixteen. Walking both as eight
            // copied half of the latter and left the rest of the destination
            // as it was.
            let sse_bytes = if classes.iter().all(|c| *c == RegClass::Sse)
                && (classes.len() as i32) * 8 < size_bytes
            {
                16
            } else {
                8
            };
            // Register path: one entry at a time, each from its own area.
            let mut at = 0i32;
            for class in classes.iter() {
                let (field, step) = match class {
                    RegClass::Sse => (4i32, 16i64),
                    _ => (0, 8),
                };
                // %rax = reg_save_area + offset.
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: ap_base,
                        offset: ap_off + field,
                    }),
                    dst: GpOperand::Reg(Reg::Rcx),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: ap_base,
                        offset: ap_off + 16,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Movsx {
                    src_size: OperandSize::B32,
                    dst_size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rcx),
                    dst: Reg::Rcx,
                });
                self.push_lir(X86Inst::Add {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rcx),
                    dst: Reg::Rax,
                });
                // Advance and commit before the copy: the copy may write the
                // destination register, and for a value-sized result that
                // register is the last thing this sequence should touch.
                // Committed per eightbyte, because the general and SSE areas
                // advance by different amounts.
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: ap_base,
                        offset: ap_off + field,
                    }),
                    dst: GpOperand::Reg(Reg::Rcx),
                });
                self.push_lir(X86Inst::Add {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(step),
                    dst: Reg::Rcx,
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rcx),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: ap_base,
                        offset: ap_off + field,
                    }),
                });
                let covers = match class {
                    RegClass::Sse => sse_bytes,
                    _ => 8,
                };
                let bytes = (size_bytes - at).min(covers);
                self.va_copy_bytes(Reg::Rax, 0, dst, at, bytes);
                at += covers;
            }
            self.push_lir(X86Inst::Jmp {
                target: done_label.clone(),
            });
            self.push_lir(X86Inst::Directive(Directive::BlockLabel(overflow_label)));
        }

        // Overflow path: the argument sits in the caller's frame as itself.
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_off + 8,
            }),
            dst: GpOperand::Reg(Reg::Rax),
        });
        // An over-aligned aggregate starts on *its own* alignment, not on 16.
        // Rounding to a fixed 16 left a `__attribute__((aligned (32)))`
        // argument sixteen bytes low, because the caller had placed it at a
        // 32-byte boundary within an area whose base is 32-byte aligned.
        let arg_align = (types.alignment(arg_type) as i64).max(8);
        if arg_align > 8 {
            self.push_lir(X86Inst::Add {
                size: OperandSize::B64,
                src: GpOperand::Imm(arg_align - 1),
                dst: Reg::Rax,
            });
            self.push_lir(X86Inst::And {
                size: OperandSize::B64,
                src: GpOperand::Imm(-arg_align),
                dst: Reg::Rax,
            });
        }
        // Advance before the copy, so the copy is free to write %rax.
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rax),
            dst: GpOperand::Reg(Reg::Rcx),
        });
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Imm(((size_bytes + 7) & !7) as i64),
            dst: Reg::Rcx,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rcx),
            dst: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_off + 8,
            }),
        });
        // On the stack the argument is laid out as itself, so it copies across
        // in one contiguous run rather than eightbyte by eightbyte.
        self.va_copy_bytes(Reg::Rax, 0, dst, 0, size_bytes);

        if guarded {
            self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
        }
    }

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
        // A zero-sized argument was never passed, so there is nothing to read
        // and nothing to step over.
        //
        // System V AMD64 psABI 3.2.3 gives such a type no class and no
        // eightbytes, and the call site already agrees -- `param_is_ignored`
        // is what puts it in `ignored_arg_indices` there. Asking the same
        // predicate here is what keeps the two sides in step: the aggregate
        // path below rounds `size_bytes` up to one and folds `ArgClass::Ignore`
        // into the same empty class vector as MEMORY, so it took the overflow
        // path, copied a byte the object does not own, and advanced
        // `overflow_arg_area` by eight -- putting every later `va_arg` in the
        // list eight bytes out.
        if crate::abi::param_is_ignored(arg_type, types) {
            return;
        }

        let arg_size = types.size_bits(arg_type).max(32);
        let arg_bytes = (arg_size / 8).max(8) as i32;

        let ap_loc = self.get_location(ap_addr);
        let dst_loc = self.get_location(target);

        let label_suffix = self.unique_label_counter;
        self.unique_label_counter += 1;

        // The va_arg helpers (`emit_va_arg_int`/`_float`) read the va_list
        // structure from a (base register, offset) pair. There are two
        // distinct shapes for the `ap_addr` operand:
        //
        // 1. `ap_addr` is a `Sym` pseudo — the address of a stack-allocated
        //    local `va_list`. The stack slot itself *is* the va_list, so we
        //    can address its fields with `(rbp + sym_offset)` directly.
        //
        // 2. `ap_addr` is any other pseudo (Arg, Reg, Copy result, …) that
        //    *holds* a pointer to a va_list (e.g. inside
        //    `va_arg(*p_va, …)`). The pseudo's location merely stores the
        //    pointer value; the va_list lives at the address that pointer
        //    refers to, so we must load the pointer first and use *that*
        //    as the base register with offset 0.
        //
        // Shape (2) is detected explicitly: the pointer is materialized
        // into R11 before delegating to the helpers -- wherever it was,
        // registers included. The helpers use RAX, RCX and R10 as scratch, so
        // a pointer left in one of those was overwritten under them; and none
        // of them may use R11, which is the one register reserved for it.
        let is_sym = self.pseudos.is_sym(ap_addr);

        let (base_reg, base_offset) = match &ap_loc {
            // The slot *is* the va_list, so its own address is the base.
            // Asking `stack_mem` rather than composing the displacement by hand
            // is what keeps this right when locals are addressed off `%rsp`
            // instead of `%rbp`, under dynamic stack alignment.
            Loc::Stack(ap_offset) if is_sym => match self.stack_mem(*ap_offset) {
                MemAddr::BaseOffset { base, offset } => (base, offset),
                other => unreachable!("stack_mem gave a non-BaseOffset address: {other:?}"),
            },
            Loc::Stack(ap_offset) => {
                // Stack slot holds a pointer; load it into R11 first.
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(self.stack_field(*ap_offset, 0)),
                    dst: GpOperand::Reg(Reg::R11),
                });
                (Reg::R11, 0)
            }
            Loc::Reg(ap_reg) => {
                if *ap_reg != Reg::R11 {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(*ap_reg),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                }
                (Reg::R11, 0)
            }
            _ => return,
        };

        // A complex value is read exactly as the equivalent struct is: its
        // classification (psABI 3.2.3) names each eightbyte's register area,
        // or MEMORY for `long double _Complex`, and the aggregate path
        // follows that. It must be asked first, because a complex type
        // carries its base's kind: `long double _Complex` would otherwise
        // take the x87 path and `float _Complex` the scalar SSE one, each
        // reading one half and stepping over the wrong amount.
        if types.is_aggregate_or_complex(arg_type) {
            self.emit_va_arg_aggregate(
                base_reg,
                base_offset,
                &dst_loc,
                arg_type,
                types,
                label_suffix,
            );
        } else if types.kind(arg_type) == crate::types::TypeKind::LongDouble {
            self.emit_va_arg_x87(base_reg, base_offset, &dst_loc);
        } else if types.kind(arg_type) == crate::types::TypeKind::Int128 {
            // Two INTEGER eightbytes, not one saturated at 64 bits. `kind`
            // suffices rather than `is_plain_int128`: the aggregate-or-complex
            // arm above has already taken every complex type, for the reason
            // given there.
            self.emit_va_arg_int128(base_reg, base_offset, &dst_loc, label_suffix);
        } else if types.is_float(arg_type) {
            self.emit_va_arg_float(
                base_reg,
                base_offset,
                &dst_loc,
                arg_type,
                label_suffix,
                types,
            );
        } else {
            self.emit_va_arg_int(
                base_reg,
                base_offset,
                &dst_loc,
                arg_size,
                arg_bytes,
                label_suffix,
            );
        }
    }

    /// Emit va_copy: Copy a va_list (24 bytes)
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

        // Copy 24 bytes from src to dest
        // Both src_loc and dest_loc contain addresses of va_list structs
        match (&src_loc, &dest_loc) {
            (Loc::Stack(src_off), Loc::Stack(dst_off)) => {
                // Copy gp_offset (4 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(self.stack_field(*src_off, 0)),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(self.stack_field(*dst_off, 0)),
                });
                // Copy fp_offset (4 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(self.stack_field(*src_off, 4)),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(self.stack_field(*dst_off, 4)),
                });
                // Copy overflow_arg_area (8 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(self.stack_field(*src_off, 8)),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(self.stack_field(*dst_off, 8)),
                });
                // Copy reg_save_area (8 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(self.stack_field(*src_off, 16)),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(self.stack_field(*dst_off, 16)),
                });
            }
            (Loc::Reg(src_reg), Loc::Reg(dst_reg)) => {
                // Both src and dest are in registers (containing addresses)
                // Choose a temp register that doesn't conflict with src or dst
                let temp = if *src_reg != Reg::Rax && *dst_reg != Reg::Rax {
                    Reg::Rax
                } else if *src_reg != Reg::Rdx && *dst_reg != Reg::Rdx {
                    Reg::Rdx
                } else {
                    Reg::Rcx
                };
                // Copy gp_offset (4 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 0,
                    }),
                    dst: GpOperand::Reg(temp),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(temp),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 0,
                    }),
                });
                // Copy fp_offset (4 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 4,
                    }),
                    dst: GpOperand::Reg(temp),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(temp),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 4,
                    }),
                });
                // Copy overflow_arg_area (8 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 8,
                    }),
                    dst: GpOperand::Reg(temp),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(temp),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 8,
                    }),
                });
                // Copy reg_save_area (8 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 16,
                    }),
                    dst: GpOperand::Reg(temp),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(temp),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 16,
                    }),
                });
            }
            (Loc::Reg(src_reg), Loc::Stack(dst_off)) => {
                // Src in register, dest on stack
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 0,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(self.stack_field(*dst_off, 0)),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 4,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(self.stack_field(*dst_off, 4)),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 8,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(self.stack_field(*dst_off, 8)),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *src_reg,
                        offset: 16,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(self.stack_field(*dst_off, 16)),
                });
            }
            (Loc::Stack(src_off), Loc::Reg(dst_reg)) => {
                // Src on stack, dest in register
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(self.stack_field(*src_off, 0)),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 0,
                    }),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(self.stack_field(*src_off, 4)),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 4,
                    }),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(self.stack_field(*src_off, 8)),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 8,
                    }),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(self.stack_field(*src_off, 16)),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: *dst_reg,
                        offset: 16,
                    }),
                });
            }
            _ => {}
        }
    }

    // Byte-swapping builtins

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
        let op_size = match swap_size {
            BswapSize::B16 => OperandSize::B16,
            BswapSize::B32 => OperandSize::B32,
            BswapSize::B64 => OperandSize::B64,
        };

        // Load source into R10 (scratch register)
        match (&src_loc, &swap_size) {
            // 16-bit: use zero-extending moves
            (Loc::Reg(r), BswapSize::B16) if *r != Reg::R10 => {
                self.push_lir(X86Inst::Movzx {
                    src_size: OperandSize::B16,
                    dst_size: OperandSize::B32,
                    src: GpOperand::Reg(*r),
                    dst: Reg::R10,
                });
            }
            (Loc::Stack(off), BswapSize::B16) => {
                self.push_lir(X86Inst::Movzx {
                    src_size: OperandSize::B16,
                    dst_size: OperandSize::B32,
                    src: GpOperand::Mem(self.stack_field(*off, 0)),
                    dst: Reg::R10,
                });
            }
            // 32/64-bit: use regular moves
            (Loc::Reg(r), _) if *r != Reg::R10 => {
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(*r),
                    dst: GpOperand::Reg(Reg::R10),
                });
            }
            (Loc::Stack(off), _) => {
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Mem(self.stack_field(*off, 0)),
                    dst: GpOperand::Reg(Reg::R10),
                });
            }
            (Loc::Imm(v), _) => {
                self.push_lir(X86Inst::Mov {
                    size: if matches!(swap_size, BswapSize::B16) {
                        OperandSize::B32
                    } else {
                        op_size
                    },
                    src: GpOperand::Imm(*v as i64),
                    dst: GpOperand::Reg(Reg::R10),
                });
            }
            (Loc::Reg(_), _) => {} // Already in R10
            _ => return,
        }

        // Perform byte-swap: 16-bit uses ROR, 32/64-bit uses BSWAP
        match swap_size {
            BswapSize::B16 => {
                self.push_lir(X86Inst::Ror {
                    size: OperandSize::B16,
                    count: ShiftCount::Imm(8),
                    dst: Reg::R10,
                });
            }
            BswapSize::B32 | BswapSize::B64 => {
                self.push_lir(X86Inst::Bswap {
                    size: op_size,
                    reg: Reg::R10,
                });
            }
        }

        // Store result
        match (&dst_loc, &swap_size) {
            // 16-bit: use zero-extending move for register destination
            (Loc::Reg(r), BswapSize::B16) if *r != Reg::R10 => {
                self.push_lir(X86Inst::Movzx {
                    src_size: OperandSize::B16,
                    dst_size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::R10),
                    dst: *r,
                });
            }
            (Loc::Stack(off), BswapSize::B16) => {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B16,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Mem(self.stack_field(*off, 0)),
                });
            }
            // 32/64-bit: use regular moves
            (Loc::Reg(r), _) if *r != Reg::R10 => {
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Reg(*r),
                });
            }
            (Loc::Stack(off), _) => {
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Mem(self.stack_field(*off, 0)),
                });
            }
            _ => {}
        }
    }

    /// Emit count trailing zeros
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

        // BSF (bit scan forward) finds index of least significant set bit
        // which is equivalent to count of trailing zeros
        // Use R10 as scratch register
        match src_loc {
            Loc::Reg(r) => {
                self.push_lir(X86Inst::Bsf {
                    size: src_size,
                    src: GpOperand::Reg(r),
                    dst: Reg::R10,
                });
            }
            Loc::Stack(off) => {
                self.push_lir(X86Inst::Bsf {
                    size: src_size,
                    src: GpOperand::Mem(self.stack_field(off, 0)),
                    dst: Reg::R10,
                });
            }
            Loc::Imm(v) => {
                // Load immediate first, then BSF
                self.push_lir(X86Inst::Mov {
                    size: src_size,
                    src: GpOperand::Imm(v as i64),
                    dst: GpOperand::Reg(Reg::R10),
                });
                self.push_lir(X86Inst::Bsf {
                    size: src_size,
                    src: GpOperand::Reg(Reg::R10),
                    dst: Reg::R10,
                });
            }
            _ => return,
        }

        // Store result (return type is int, always 32-bit)
        match dst_loc {
            Loc::Reg(r) => {
                if r != Reg::R10 {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Reg(Reg::R10),
                        dst: GpOperand::Reg(r),
                    });
                }
            }
            Loc::Stack(off) => {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Mem(self.stack_field(off, 0)),
                });
            }
            _ => {}
        }
    }

    /// Emit count leading zeros: CLZ(x) = operand_bits - 1 - BSR(x)
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

        // BSR (bit scan reverse) finds index of most significant set bit
        // CLZ = (operand_size - 1) - BSR_result
        // Use R10 as scratch register
        match src_loc {
            Loc::Reg(r) => {
                self.push_lir(X86Inst::Bsr {
                    size: src_size,
                    src: GpOperand::Reg(r),
                    dst: Reg::R10,
                });
            }
            Loc::Stack(off) => {
                self.push_lir(X86Inst::Bsr {
                    size: src_size,
                    src: GpOperand::Mem(self.stack_field(off, 0)),
                    dst: Reg::R10,
                });
            }
            Loc::Imm(v) => {
                // Load immediate first, then BSR
                self.push_lir(X86Inst::Mov {
                    size: src_size,
                    src: GpOperand::Imm(v as i64),
                    dst: GpOperand::Reg(Reg::R10),
                });
                self.push_lir(X86Inst::Bsr {
                    size: src_size,
                    src: GpOperand::Reg(Reg::R10),
                    dst: Reg::R10,
                });
            }
            _ => return,
        }

        // XOR R10 with (size - 1) to convert BSR result to CLZ
        // Since BSR gives index from LSB, we need (size_bits - 1) - result
        // XOR with (size_bits - 1) achieves this for valid inputs (non-zero)
        let xor_value = (src_size.bits() - 1) as i64;
        self.push_lir(X86Inst::Xor {
            size: OperandSize::B32, // Result is always 32-bit int
            src: GpOperand::Imm(xor_value),
            dst: Reg::R10,
        });

        // Store result (return type is int, always 32-bit)
        match dst_loc {
            Loc::Reg(r) => {
                if r != Reg::R10 {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Reg(Reg::R10),
                        dst: GpOperand::Reg(r),
                    });
                }
            }
            Loc::Stack(off) => {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Mem(self.stack_field(off, 0)),
                });
            }
            _ => {}
        }
    }

    /// Emit population count with the baseline sequence of
    /// [`popcount_sequence`] -- never `popcnt`, which is not in x86-64-v1.
    /// Works in the R10/R11 scratch pair; the result is an `int`.
    pub(super) fn emit_popcount(&mut self, insn: &Instruction, src_size: OperandSize) {
        let (Some(&src), Some(dst)) = (insn.src.first(), insn.target) else {
            return;
        };
        self.emit_move(src, Reg::R10, src_size.bits());
        for inst in popcount_sequence(src_size, Reg::R10, Reg::R11) {
            self.push_lir(inst);
        }
        let dst_loc = self.get_location(dst);
        self.emit_move_to_loc(Reg::R10, &dst_loc, u32::BITS);
    }

    // setjmp/longjmp/alloca support

    /// Emit setjmp(env) - saves execution context
    /// System V AMD64 ABI: env in RDI, returns int in EAX
    pub(super) fn emit_setjmp(&mut self, insn: &Instruction) {
        let env = match insn.src.first() {
            Some(&e) => e,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Put env argument in RDI (first argument register)
        self.emit_move(env, Reg::Rdi, 64);

        // Call setjmp
        self.push_lir(X86Inst::Call {
            target: CallTarget::Direct(Symbol::global(insn.library_callee())),
        });

        // Store result from EAX to target
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::Rax, &dst_loc, u32::BITS);
    }

    /// Emit longjmp(env, val) - restores execution context (noreturn)
    /// System V AMD64 ABI: env in RDI, val in RSI
    pub(super) fn emit_longjmp(&mut self, insn: &Instruction) {
        let env = match insn.src.first() {
            Some(&e) => e,
            None => return,
        };
        let val = match insn.src.get(1) {
            Some(&v) => v,
            None => return,
        };

        // IMPORTANT: Load val first into RSI, THEN env into RDI.
        // If we loaded env into RDI first and val was passed as the first
        // function argument (in RDI), it would get overwritten.
        // Put val argument in RSI (second argument register) FIRST
        self.emit_move(val, Reg::Rsi, 32);

        // Put env argument in RDI (first argument register)
        self.emit_move(env, Reg::Rdi, 64);

        // Call longjmp (noreturn - control never comes back)
        self.push_lir(X86Inst::Call {
            target: CallTarget::Direct(Symbol::global(insn.library_callee())),
        });

        // Emit ud2 after longjmp since it never returns
        // This helps catch any bugs where longjmp somehow returns
        self.push_lir(X86Inst::Ud2);
    }

    /// gcc's `__builtin_setjmp(buf)`: inline, with no library call.
    ///
    /// The five-word buffer gets gcc's layout ([`SjljLayout`]) -- the frame
    /// pointer, the resume address, under `-fcf-protection=return` the
    /// shadow-stack pointer, then the stack pointer -- and the result is 0
    /// straight through and 1 at the resume point:
    ///
    /// ```text
    ///     movq %rbp, 0(buf); leaq resume(%rip), t; movq t, 8(buf)
    ///     [movl $0, t; rdsspq t; movq t, 16(buf)]          # return
    ///     movq %rsp, sp(buf); movl $0, r; jmp done
    /// resume:                     # %rbp and %rsp are back, nothing else is
    ///     [endbr64]                                         # branch
    ///     movl $1, r
    /// done:
    /// ```
    ///
    /// The shadow-stack word is zeroed before `rdsspq`, which does nothing
    /// when the shadow stack is off, so it reads 0 then, as gcc's does. A
    /// `longjmp` reaches the resume point by an indirect jump, which is why
    /// it starts with the landing pad indirect-branch tracking requires.
    ///
    /// Nothing else survives the jump. The allocator keeps no value in a
    /// register across this instruction (`get_constraint_info`) and the
    /// prologue saves every callee-saved register, so the only state to
    /// re-establish is an over-aligned frame's base register.
    pub(super) fn emit_builtin_setjmp(&mut self, insn: &Instruction) {
        let (Some(&env), Some(target)) = (insn.src.first(), insn.target) else {
            return;
        };
        let cf = self.base.cf_protection;
        let layout = SjljLayout::for_protection(cf);
        let id = self.unique_label_counter;
        self.unique_label_counter += 1;
        let resume = Label::internal("sjlj_resume", id);
        let done = Label::internal("sjlj_done", id);
        let word = |offset| {
            GpOperand::Mem(MemAddr::BaseOffset {
                base: Reg::R10,
                offset,
            })
        };

        self.emit_move(env, Reg::R10, 64);
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rbp),
            dst: word(SjljLayout::FP),
        });
        self.push_lir(X86Inst::Lea {
            addr: MemAddr::RipRelative(resume.symbol()),
            dst: Reg::R11,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst: word(SjljLayout::RESUME),
        });
        if let Some(ssp) = layout.ssp {
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B32,
                src: GpOperand::Imm(0),
                dst: GpOperand::Reg(Reg::R11),
            });
            self.push_lir(X86Inst::Rdssp { dst: Reg::R11 });
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::R11),
                dst: word(ssp),
            });
        }
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rsp),
            dst: word(layout.sp),
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Imm(0),
            dst: GpOperand::Reg(Reg::R11),
        });
        self.push_lir(X86Inst::Jmp {
            target: done.clone(),
        });

        self.push_lir(X86Inst::Directive(Directive::BlockLabel(resume)));
        if cf.branch {
            self.push_lir(X86Inst::Endbr64);
        }
        self.emit_frame_base_latch_from_rbp();
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Imm(1),
            dst: GpOperand::Reg(Reg::R11),
        });

        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done)));
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::R11, &dst_loc, u32::BITS);
    }

    /// gcc's `__builtin_longjmp(buf, 1)`: put back the frame and stack
    /// pointers the matching `__builtin_setjmp` saved, and jump to its resume
    /// address. The buffer's address is read into a scratch register first,
    /// since it may well be addressed from the `%rbp` being replaced.
    ///
    /// Under `-fcf-protection=return` the shadow stack is unwound to where
    /// the setjmp found it first, by gcc's own sequence: `incsspq` pops at
    /// most 255 entries at a time.
    ///
    /// ```text
    ///     movl $0, %eax; rdsspq %rax; subq 16(buf), %rax; je done
    ///     negq %rax; shrq $3, %rax; cmpq $255, %rax; jbe last
    /// loop:
    ///     movl $255, %ecx; incsspq %rcx; subq $255, %rax
    ///     cmpq $255, %rax; ja loop
    /// last:
    ///     incsspq %rax
    /// done:
    /// ```
    ///
    /// With the shadow stack off `rdsspq` leaves the 0 and the setjmp saved
    /// 0, so the difference is 0 and no `incsspq` -- which faults then -- is
    /// reached. Every register is free to use: control never comes back.
    pub(super) fn emit_builtin_longjmp(&mut self, insn: &Instruction) {
        let Some(&env) = insn.src.first() else {
            return;
        };
        let layout = SjljLayout::for_protection(self.base.cf_protection);
        let word = |offset| {
            GpOperand::Mem(MemAddr::BaseOffset {
                base: Reg::R11,
                offset,
            })
        };
        self.emit_move(env, Reg::R11, 64);
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: word(SjljLayout::RESUME),
            dst: GpOperand::Reg(Reg::R10),
        });
        if let Some(ssp) = layout.ssp {
            self.emit_shadow_stack_unwind(word(ssp));
        }
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: word(SjljLayout::FP),
            dst: GpOperand::Reg(Reg::Rbp),
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: word(layout.sp),
            dst: GpOperand::Reg(Reg::Rsp),
        });
        self.push_lir(X86Inst::JmpIndirect { reg: Reg::R10 });
    }

    /// Pop the shadow stack back to the pointer `saved` holds, as described
    /// at [`Self::emit_builtin_longjmp`]. Uses `%rax` and `%rcx`.
    fn emit_shadow_stack_unwind(&mut self, saved: GpOperand) {
        let id = self.unique_label_counter;
        self.unique_label_counter += 1;
        let pop_loop = Label::internal("sjlj_ssp_loop", id);
        let last = Label::internal("sjlj_ssp_last", id);
        let done = Label::internal("sjlj_ssp_done", id);
        let max = GpOperand::Imm(255);

        self.push_lir(X86Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Imm(0),
            dst: GpOperand::Reg(Reg::Rax),
        });
        self.push_lir(X86Inst::Rdssp { dst: Reg::Rax });
        self.push_lir(X86Inst::Sub {
            size: OperandSize::B64,
            src: saved,
            dst: Reg::Rax,
        });
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Eq,
            target: done.clone(),
        });
        // Bytes to entries: the saved pointer is the higher one.
        self.push_lir(X86Inst::Neg {
            size: OperandSize::B64,
            dst: Reg::Rax,
        });
        self.push_lir(X86Inst::Shr {
            size: OperandSize::B64,
            count: ShiftCount::Imm(3),
            dst: Reg::Rax,
        });
        self.push_lir(X86Inst::Cmp {
            size: OperandSize::B64,
            src: max.clone(),
            dst: GpOperand::Reg(Reg::Rax),
        });
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Ule,
            target: last.clone(),
        });

        self.push_lir(X86Inst::Directive(Directive::BlockLabel(pop_loop.clone())));
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B32,
            src: max.clone(),
            dst: GpOperand::Reg(Reg::Rcx),
        });
        self.push_lir(X86Inst::Incssp { count: Reg::Rcx });
        self.push_lir(X86Inst::Sub {
            size: OperandSize::B64,
            src: max.clone(),
            dst: Reg::Rax,
        });
        self.push_lir(X86Inst::Cmp {
            size: OperandSize::B64,
            src: max,
            dst: GpOperand::Reg(Reg::Rax),
        });
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Ugt,
            target: pop_loop,
        });

        self.push_lir(X86Inst::Directive(Directive::BlockLabel(last)));
        self.push_lir(X86Inst::Incssp { count: Reg::Rax });
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done)));
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

        // Load size into R10 (scratch register)
        self.emit_move(size, Reg::R10, 64);

        // Round up to 16-byte alignment: (size + 15) & ~15
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Imm(15),
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::And {
            size: OperandSize::B64,
            src: GpOperand::Imm(-16),
            dst: Reg::R10,
        });

        // Subtract from stack pointer
        self.push_lir(X86Inst::Sub {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R10),
            dst: Reg::Rsp,
        });

        // Return new stack pointer
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rsp),
            dst: GpOperand::Reg(Reg::R10),
        });

        // Store result
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::R10, &dst_loc, 64);
    }

    /// Capture %rsp so a later restore can put it back.
    ///
    /// R10 is the reserved scratch; going through it keeps this the same shape
    /// as `emit_alloca`, which ends by moving %rsp into R10 as well.
    pub(super) fn emit_stack_save(&mut self, insn: &Instruction) {
        let Some(target) = insn.target else {
            return;
        };
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rsp),
            dst: GpOperand::Reg(Reg::R10),
        });
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::R10, &dst_loc, 64);
    }

    /// Put %rsp back to a saved value, releasing everything alloca'd since.
    pub(super) fn emit_stack_restore(&mut self, insn: &Instruction) {
        let Some(&src) = insn.src.first() else {
            return;
        };
        self.emit_move(src, Reg::R10, 64);
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R10),
            dst: GpOperand::Reg(Reg::Rsp),
        });
    }

    /// Emit __builtin_memset(dest, c, n) - calls memset
    /// System V AMD64 ABI: dest in RDI, c in RSI, n in RDX, returns dest in RAX
    pub(super) fn emit_memset(&mut self, insn: &Instruction) {
        let dest = match insn.src.first() {
            Some(&d) => d,
            None => return,
        };
        let c = match insn.src.get(1) {
            Some(&c) => c,
            None => return,
        };
        let n = match insn.src.get(2) {
            Some(&n) => n,
            None => return,
        };
        let target = insn.target;

        // Load arguments in reverse order to avoid clobbering
        self.emit_move(n, Reg::Rdx, 64);
        self.emit_move(c, Reg::Rsi, 32); // c is int (32-bit)
        self.emit_move(dest, Reg::Rdi, 64);

        // Call memset
        self.push_lir(X86Inst::Call {
            target: CallTarget::Direct(Symbol::global(insn.library_callee())),
        });

        // Store result from RAX to target (returns dest)
        if let Some(target) = target {
            let dst_loc = self.get_location(target);
            self.emit_move_to_loc(Reg::Rax, &dst_loc, 64);
        }
    }

    /// Emit __builtin_memcpy(dest, src, n) - calls memcpy
    /// System V AMD64 ABI: dest in RDI, src in RSI, n in RDX, returns dest in RAX
    pub(super) fn emit_memcpy(&mut self, insn: &Instruction) {
        let dest = match insn.src.first() {
            Some(&d) => d,
            None => return,
        };
        let src = match insn.src.get(1) {
            Some(&s) => s,
            None => return,
        };
        let n = match insn.src.get(2) {
            Some(&n) => n,
            None => return,
        };
        let target = insn.target;

        // Load arguments in reverse order to avoid clobbering
        self.emit_move(n, Reg::Rdx, 64);
        self.emit_move(src, Reg::Rsi, 64);
        self.emit_move(dest, Reg::Rdi, 64);

        // Call memcpy
        self.push_lir(X86Inst::Call {
            target: CallTarget::Direct(Symbol::global(insn.library_callee())),
        });

        // Store result from RAX to target (returns dest)
        if let Some(target) = target {
            let dst_loc = self.get_location(target);
            self.emit_move_to_loc(Reg::Rax, &dst_loc, 64);
        }
    }

    /// Emit __builtin_memmove(dest, src, n) - calls memmove
    /// System V AMD64 ABI: dest in RDI, src in RSI, n in RDX, returns dest in RAX
    pub(super) fn emit_memmove(&mut self, insn: &Instruction) {
        let dest = match insn.src.first() {
            Some(&d) => d,
            None => return,
        };
        let src = match insn.src.get(1) {
            Some(&s) => s,
            None => return,
        };
        let n = match insn.src.get(2) {
            Some(&n) => n,
            None => return,
        };
        let target = insn.target;

        // Load arguments in reverse order to avoid clobbering
        self.emit_move(n, Reg::Rdx, 64);
        self.emit_move(src, Reg::Rsi, 64);
        self.emit_move(dest, Reg::Rdi, 64);

        // Call memmove
        self.push_lir(X86Inst::Call {
            target: CallTarget::Direct(Symbol::global(insn.library_callee())),
        });

        // Store result from RAX to target (returns dest)
        if let Some(target) = target {
            let dst_loc = self.get_location(target);
            self.emit_move_to_loc(Reg::Rax, &dst_loc, 64);
        }
    }

    /// Leave the frame pointer `levels` frames up in R10.
    ///
    /// Every c17 function pushes `%rbp` and points `%rbp` at it, so `(%rbp)`
    /// is the caller's `%rbp` and `8(%rbp)` the return address: a chain of
    /// frame records that each step follows once.
    fn emit_frame_walk(&mut self, levels: u32) {
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rbp),
            dst: GpOperand::Reg(Reg::R10),
        });
        for _ in 0..levels {
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::R10,
                    offset: 0,
                }),
                dst: GpOperand::Reg(Reg::R10),
            });
        }
    }

    /// Emit __builtin_frame_address(level) - return frame pointer at given level
    pub(super) fn emit_frame_address(&mut self, insn: &Instruction) {
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        self.emit_frame_walk(insn.frame_level());
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::R10, &dst_loc, 64);
    }

    /// Emit __builtin_return_address(level) - return address at given level
    pub(super) fn emit_return_address(&mut self, insn: &Instruction) {
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        // The return address sits just above the saved frame pointer.
        self.emit_frame_walk(insn.frame_level());
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: Reg::R10,
                offset: 8,
            }),
            dst: GpOperand::Reg(Reg::R10),
        });
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::R10, &dst_loc, 64);
    }
}

/// Where gcc's `__builtin_setjmp` buffer keeps each word on x86-64, the one
/// layout c17's `__builtin_setjmp` and `__builtin_longjmp` both follow, so
/// either can meet a gcc-built other half.
///
/// It depends on `-fcf-protection`: with return protection gcc saves the
/// shadow-stack pointer in the third word and moves the stack pointer to the
/// fourth, whether or not the shadow stack is on when the program runs.
struct SjljLayout {
    /// The shadow-stack pointer's word, under return protection only.
    ssp: Option<i32>,
    /// The stack pointer's word.
    sp: i32,
}

impl SjljLayout {
    /// The frame pointer's word.
    const FP: i32 = 0;
    /// The resume address's word.
    const RESUME: i32 = 8;

    fn for_protection(cf: crate::target::CfProtection) -> Self {
        if cf.ret {
            Self {
                ssp: Some(16),
                sp: 24,
            }
        } else {
            Self { ssp: None, sp: 16 }
        }
    }
}
