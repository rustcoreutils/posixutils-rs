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
use crate::arch::lir::{complex_fp_info, CallTarget, FpSize, OperandSize, Symbol};
use crate::ir::{Instruction, PseudoId};
use crate::types::TypeTable;
use std::collections::HashMap;

/// Information about call argument classification
pub(super) struct CallArgInfo {
    /// Indices of arguments that go on the stack
    pub stack_arg_indices: Vec<usize>,
    /// Whether stack padding is needed for alignment
    pub needs_padding: bool,
}

impl X86_64CodeGen {
    /// Classify call arguments into register vs stack arguments
    pub(super) fn classify_call_args(&self, insn: &Instruction, types: &TypeTable) -> CallArgInfo {
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = XmmReg::arg_regs();

        let mut stack_arg_indices = Vec::with_capacity(insn.src.len());
        let mut temp_int_idx = 0;
        let mut temp_fp_idx = 0;

        for i in 0..insn.src.len() {
            let arg_type = insn.arg_types.get(i).copied();
            let is_complex = arg_type.is_some_and(|t| types.is_complex(t));
            let is_fp = if let Some(typ) = arg_type {
                types.is_float(typ)
            } else {
                let arg_loc = self.get_location(insn.src[i]);
                matches!(arg_loc, Loc::Xmm(_) | Loc::FImm(..))
            };

            if is_complex {
                // Complex uses TWO consecutive XMM registers
                if temp_fp_idx + 1 >= fp_arg_regs.len() {
                    stack_arg_indices.push(i);
                }
                temp_fp_idx += 2;
            } else if is_fp {
                if temp_fp_idx >= fp_arg_regs.len() {
                    stack_arg_indices.push(i);
                }
                temp_fp_idx += 1;
            } else {
                if temp_int_idx >= int_arg_regs.len() {
                    stack_arg_indices.push(i);
                }
                temp_int_idx += 1;
            }
        }

        let needs_padding = stack_arg_indices.len() % 2 == 1;

        CallArgInfo {
            stack_arg_indices,
            needs_padding,
        }
    }

    /// Push stack arguments in reverse order (returns number of args pushed)
    pub(super) fn push_stack_args(
        &mut self,
        insn: &Instruction,
        info: &CallArgInfo,
        types: &TypeTable,
    ) -> usize {
        // Add padding for 16-byte alignment if needed
        if info.needs_padding {
            self.push_lir(X86Inst::Sub {
                size: OperandSize::B64,
                src: GpOperand::Imm(8),
                dst: Reg::Rsp,
            });
        }

        let mut stack_args = 0;

        // Push stack arguments in reverse order
        for &i in info.stack_arg_indices.iter().rev() {
            let arg = insn.src[i];
            let arg_type = insn.arg_types.get(i).copied();
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
                self.emit_fp_move(arg, XmmReg::Xmm15, fp_size);
                self.push_lir(X86Inst::Sub {
                    size: OperandSize::B64,
                    src: GpOperand::Imm(8),
                    dst: Reg::Rsp,
                });
                let fp_lir_size = FpSize::from_bits(fp_size);
                self.push_lir(X86Inst::MovFp {
                    size: fp_lir_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rsp,
                        offset: 0,
                    }),
                });
            } else {
                let arg_size = if let Some(typ) = arg_type {
                    types.size_bits(typ).max(32)
                } else {
                    64
                };
                self.emit_move(arg, Reg::Rax, arg_size);
                self.push_lir(X86Inst::Push {
                    src: GpOperand::Reg(Reg::Rax),
                });
            }
            stack_args += 1;
        }

        stack_args
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

            let arg_size = if let Some(typ) = arg_type {
                types.size_bits(typ).max(32)
            } else {
                64
            };

            if is_complex {
                self.setup_complex_arg(
                    arg,
                    arg_type,
                    fp_arg_regs[fp_arg_idx],
                    fp_arg_regs[fp_arg_idx + 1],
                    types,
                );
                fp_arg_idx += 2;
            } else if is_fp {
                let fp_size = if let Some(typ) = arg_type {
                    types.size_bits(typ)
                } else {
                    64
                };
                self.emit_fp_move(arg, fp_arg_regs[fp_arg_idx], fp_size);
                fp_arg_idx += 1;
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
        let (fp_size, imag_offset) = complex_fp_info(types, arg_type.unwrap());

        match arg_loc {
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
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: 0,
                    }),
                    dst: XmmOperand::Reg(real_reg),
                });
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: imag_offset,
                    }),
                    dst: XmmOperand::Reg(imag_reg),
                });
            }
            Loc::Reg(r) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset { base: r, offset: 0 }),
                    dst: XmmOperand::Reg(real_reg),
                });
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: r,
                        offset: imag_offset,
                    }),
                    dst: XmmOperand::Reg(imag_reg),
                });
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

    /// Handle call return value
    pub(super) fn handle_call_return_value(&mut self, insn: &Instruction, types: &TypeTable) {
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_loc = self.get_location(target);
        let is_complex_result = insn.typ.is_some_and(|t| types.is_complex(t));
        let is_fp_result = if let Some(typ) = insn.typ {
            types.is_float(typ)
        } else {
            matches!(dst_loc, Loc::Xmm(_) | Loc::FImm(..))
        };
        let ret_size = insn.size.max(32);

        if insn.is_two_reg_return {
            self.handle_two_reg_return(&dst_loc);
        } else if is_complex_result {
            self.handle_complex_return(insn, &dst_loc, types);
        } else if is_fp_result {
            self.emit_fp_move_from_xmm(XmmReg::Xmm0, &dst_loc, ret_size);
        } else {
            self.emit_move_to_loc(Reg::Rax, &dst_loc, ret_size);
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

    /// Handle complex return value (XMM0 + XMM1)
    fn handle_complex_return(&mut self, insn: &Instruction, dst_loc: &Loc, types: &TypeTable) {
        let (fp_size, imag_offset) = complex_fp_info(types, insn.typ.unwrap());
        match dst_loc {
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
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
            Loc::Reg(r) => {
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
    pub(super) fn cleanup_call_stack(&mut self, stack_args: usize, needs_padding: bool) {
        let stack_cleanup = stack_args * 8 + if needs_padding { 8 } else { 0 };
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
