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

use super::codegen::X86_64CodeGen;
use super::lir::{GpOperand, MemAddr, ShiftCount, X86Inst};
use super::regalloc::{Loc, Reg, XmmReg};
use crate::arch::codegen::BswapSize;
use crate::arch::lir::{CallTarget, CondCode, Directive, FpSize, Label, OperandSize, Symbol};
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
        // - overflow_arg_area = rbp + 16 (where stack args start, for overflow)
        // - reg_save_area = pointer to where we saved the argument registers

        let gp_offset = (self.num_fixed_gp_params * 8) as i32;
        let fp_offset = 48 + (self.num_fixed_fp_params * 16) as i32;
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
                // overflow_arg_area = rbp + 16 + (fixed_stack_params * 8)
                // Skip past fixed parameters that were passed on the stack
                // (when there are >6 int or >8 FP fixed params)
                let overflow_offset = 16 + (self.num_fixed_stack_params * 8) as i32;
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
                // overflow_arg_area = rbp + 16 + (fixed_stack_params * 8)
                let overflow_offset = 16 + (self.num_fixed_stack_params * 8) as i32;
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
    pub(super) fn emit_va_arg_int(
        &mut self,
        ap_base: Reg,
        ap_base_offset: i32,
        dst_loc: &Loc,
        arg_size: u32,
        arg_bytes: i32,
        label_suffix: u32,
    ) {
        let overflow_label = Label::new("va_overflow", label_suffix);
        let done_label = Label::new("va_done", label_suffix);
        let lir_arg_size = OperandSize::from_bits(arg_size);

        // This function uses Rax for reg_save_area/overflow pointer, Rcx for sign-extended offset.
        // The constraint-aware register allocator ensures ap_base is never in Rax or Rcx
        // by declaring VaArg as clobbering those registers in opcode_constraints().

        // Load gp_offset into R10d (using R10 as scratch for gp_offset)
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset,
            }),
            dst: GpOperand::Reg(Reg::R10),
        });
        // Compare with 48
        self.push_lir(X86Inst::Cmp {
            size: OperandSize::B32,
            src: GpOperand::Imm(48),
            dst: GpOperand::Reg(Reg::R10),
        });
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Uge,
            target: overflow_label.clone(),
        });

        // Register save area path
        // Load reg_save_area into Rax
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 16,
            }),
            dst: GpOperand::Reg(Reg::Rax),
        });
        // Sign-extend gp_offset (in R10) to 64-bit and add to reg_save_area
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

        // Store value from [Rax] to destination
        match dst_loc {
            Loc::Reg(r) => {
                self.push_lir(X86Inst::Mov {
                    size: lir_arg_size,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rax,
                        offset: 0,
                    }),
                    dst: GpOperand::Reg(*r),
                });
            }
            Loc::Stack(dst_offset) => {
                let adjusted_offset = -(*dst_offset + self.callee_saved_offset);
                self.push_lir(X86Inst::Mov {
                    size: lir_arg_size,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rax,
                        offset: 0,
                    }),
                    dst: GpOperand::Reg(Reg::R11),
                });
                self.push_lir(X86Inst::Mov {
                    size: lir_arg_size,
                    src: GpOperand::Reg(Reg::R11),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: adjusted_offset,
                    }),
                });
            }
            _ => {}
        }

        // Increment gp_offset by 8 and store back
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
        self.push_lir(X86Inst::Jmp {
            target: done_label.clone(),
        });

        // Overflow path
        // Bug fix: Load overflow_arg_area pointer into R11 FIRST, then load value into Rax.
        // This prevents the pointer from being clobbered when storing to a register destination.
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(overflow_label)));
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 8,
            }),
            dst: GpOperand::Reg(Reg::R11),
        });

        // Load value from [R11] into Rax, then store to destination
        self.push_lir(X86Inst::Mov {
            size: lir_arg_size,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: Reg::R11,
                offset: 0,
            }),
            dst: GpOperand::Reg(Reg::Rax),
        });

        // Store value from Rax to destination
        match dst_loc {
            Loc::Reg(r) => {
                if *r != Reg::Rax {
                    self.push_lir(X86Inst::Mov {
                        size: lir_arg_size,
                        src: GpOperand::Reg(Reg::Rax),
                        dst: GpOperand::Reg(*r),
                    });
                }
            }
            Loc::Stack(dst_offset) => {
                let adjusted_offset = -(*dst_offset + self.callee_saved_offset);
                self.push_lir(X86Inst::Mov {
                    size: lir_arg_size,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: adjusted_offset,
                    }),
                });
            }
            _ => {}
        }

        // Advance overflow_arg_area (using R11 which still has the original pointer)
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Imm(arg_bytes as i64),
            dst: Reg::R11,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 8,
            }),
        });

        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
    }

    /// Where an aggregate `va_arg` result goes, resolved once so the copy
    /// cannot collide with the registers used to find the source.
    ///
    /// The result pseudo is allocated like any other and can land in `%rax` --
    /// which is where the save-area pointer lives -- so an address held in a
    /// register is moved into reserved scratch before anything else is
    /// touched. `[%rax]` as both source and destination is exactly what the
    /// first version emitted.
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

    /// Copy `nbytes` from `[src_base + src_off]` to `dst` at `dst_off`, in
    /// descending power-of-two chunks so nothing past the object is written.
    ///
    /// `%rcx` is the shuttle: it is declared clobbered by `VaArg`, so no live
    /// value is in it, and unlike `%r11` it cannot be `ap_base` (the va_list
    /// pointer lands there when it comes from a stack slot).
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
        let mut done = 0;
        while done < nbytes {
            let chunk = [8, 4, 2, 1]
                .into_iter()
                .find(|c| *c <= nbytes - done)
                .unwrap_or(1);
            let size = OperandSize::from_bits(chunk as u32 * 8);
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
            done += chunk;
        }
    }

    /// Read an aggregate argument, per SysV AMD64 §3.5.7.
    ///
    /// Every aggregate used to go through [`Self::emit_va_arg_int`], which
    /// pulls one value of the type's whole width out of the general-register
    /// save area. For anything the classifier put in SSE registers that is
    /// unrelated data -- `struct { float a, b, c, d; }` arrives in `xmm0` and
    /// `xmm1` and came back as whatever the integer area happened to hold.
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
        let size_bytes = (types.size_bits(arg_type) / 8).max(1) as i32;
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

        let overflow_label = Label::new("va_agg_overflow", label_suffix);
        let done_label = Label::new("va_agg_done", label_suffix);

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
            // Register path: one eightbyte at a time, each from its own area.
            for (i, class) in classes.iter().enumerate() {
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
                let at = i as i32 * 8;
                let bytes = (size_bytes - at).min(8);
                self.va_copy_bytes(Reg::Rax, 0, dst, at, bytes);
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
        // An aggregate needing 16-byte alignment starts at a 16-byte boundary.
        if types.alignment(arg_type) >= 16 {
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
        // The historical Loc::Stack path conflated the two: it always
        // treated `rbp + offset` as the va_list, which is correct only for
        // shape (1). Optimization passes (copyprop, instcombine identity
        // folds) that eliminate the Copy that previously kept shape (2)'s
        // pointer in a register expose the bug. Detect the pointer-in-
        // stack-slot case explicitly and materialize the pointer into R11
        // (reserved scratch) before delegating to the helpers.
        let is_sym = self
            .pseudos
            .iter()
            .any(|p| p.id == ap_addr && matches!(&p.kind, crate::ir::PseudoKind::Sym(_)));

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
            Loc::Reg(ap_reg) => (*ap_reg, 0),
            _ => return,
        };

        let is_aggregate = matches!(
            types.kind(arg_type),
            crate::types::TypeKind::Struct
                | crate::types::TypeKind::Union
                | crate::types::TypeKind::Array
        );
        if types.kind(arg_type) == crate::types::TypeKind::LongDouble {
            self.emit_va_arg_x87(base_reg, base_offset, &dst_loc);
        } else if is_aggregate {
            self.emit_va_arg_aggregate(
                base_reg,
                base_offset,
                &dst_loc,
                arg_type,
                types,
                label_suffix,
            );
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

    /// Emit population count
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

        // POPCNT instruction directly counts set bits
        // Use R10 as scratch register
        match src_loc {
            Loc::Reg(r) => {
                self.push_lir(X86Inst::Popcnt {
                    size: src_size,
                    src: GpOperand::Reg(r),
                    dst: Reg::R10,
                });
            }
            Loc::Stack(off) => {
                self.push_lir(X86Inst::Popcnt {
                    size: src_size,
                    src: GpOperand::Mem(self.stack_field(off, 0)),
                    dst: Reg::R10,
                });
            }
            Loc::Imm(v) => {
                // Load immediate first, then POPCNT
                self.push_lir(X86Inst::Mov {
                    size: src_size,
                    src: GpOperand::Imm(v as i64),
                    dst: GpOperand::Reg(Reg::R10),
                });
                self.push_lir(X86Inst::Popcnt {
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

    // ========================================================================
    // setjmp/longjmp/alloca support
    // ========================================================================

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
            target: CallTarget::Direct(Symbol::global("setjmp".to_string())),
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
            target: CallTarget::Direct(Symbol::global("longjmp".to_string())),
        });

        // Emit ud2 after longjmp since it never returns
        // This helps catch any bugs where longjmp somehow returns
        self.push_lir(X86Inst::Ud2);
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
            target: CallTarget::Direct(Symbol::global("memset".to_string())),
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
            target: CallTarget::Direct(Symbol::global("memcpy".to_string())),
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
            target: CallTarget::Direct(Symbol::global("memmove".to_string())),
        });

        // Store result from RAX to target (returns dest)
        if let Some(target) = target {
            let dst_loc = self.get_location(target);
            self.emit_move_to_loc(Reg::Rax, &dst_loc, 64);
        }
    }

    /// Emit __builtin_frame_address(level) - return frame pointer at given level
    pub(super) fn emit_frame_address(&mut self, insn: &Instruction) {
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // For level 0, return the current frame pointer (rbp)
        // For other levels, we'd need to walk the frame chain, but we simplify
        // by always returning the current frame pointer
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rbp),
            dst: GpOperand::Reg(Reg::R10),
        });

        // Store result
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::R10, &dst_loc, 64);
    }

    /// Emit __builtin_return_address(level) - return address at given level
    pub(super) fn emit_return_address(&mut self, insn: &Instruction) {
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // For level 0, return [rbp+8] (the saved return address)
        // For other levels, we'd need to walk the frame chain
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: Reg::Rbp,
                offset: 8,
            }),
            dst: GpOperand::Reg(Reg::R10),
        });

        // Store result
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::R10, &dst_loc, 64);
    }

    /// Emit __builtin_fabsf - absolute value of float
    pub(super) fn emit_fabs32(&mut self, insn: &Instruction) {
        let arg = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load argument into XMM0 (first FP argument register)
        self.emit_fp_move(arg, XmmReg::Xmm0, FpSize::Single);

        // Call fabsf from libc
        self.push_lir(X86Inst::Call {
            target: CallTarget::Direct(Symbol::global("fabsf".to_string())),
        });

        // Result is in XMM0, store to target
        let dst_loc = self.get_location(target);
        self.emit_fp_move_from_xmm(XmmReg::Xmm0, &dst_loc, FpSize::Single);
    }

    /// Emit __builtin_fabs - absolute value of double
    pub(super) fn emit_fabs64(&mut self, insn: &Instruction) {
        let arg = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load argument into XMM0 (first FP argument register)
        self.emit_fp_move(arg, XmmReg::Xmm0, FpSize::Double);

        // Call fabs from libc
        self.push_lir(X86Inst::Call {
            target: CallTarget::Direct(Symbol::global("fabs".to_string())),
        });

        // Result is in XMM0, store to target
        let dst_loc = self.get_location(target);
        self.emit_fp_move_from_xmm(XmmReg::Xmm0, &dst_loc, FpSize::Double);
    }

    /// Emit __builtin_signbitf - test sign bit of float
    pub(super) fn emit_signbit32(&mut self, insn: &Instruction) {
        let arg = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load argument into XMM0 (first FP argument register)
        self.emit_fp_move(arg, XmmReg::Xmm0, FpSize::Single);

        // Call __signbitf from libc (C99: signbit is a macro that calls __signbitf)
        self.push_lir(X86Inst::Call {
            target: CallTarget::Direct(Symbol::global("__signbitf".to_string())),
        });

        // Result is in EAX (integer return), store to target
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::Rax, &dst_loc, u32::BITS);
    }

    /// Emit __builtin_signbit - test sign bit of double
    pub(super) fn emit_signbit64(&mut self, insn: &Instruction) {
        let arg = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load argument into XMM0 (first FP argument register)
        self.emit_fp_move(arg, XmmReg::Xmm0, FpSize::Double);

        // Call signbit function from libc
        self.push_lir(X86Inst::Call {
            target: CallTarget::Direct(Symbol::global(
                self.base.target.os.signbit_double_fn().to_string(),
            )),
        });

        // Result is in EAX (integer return), store to target
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::Rax, &dst_loc, u32::BITS);
    }
}
