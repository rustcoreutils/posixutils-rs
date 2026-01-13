//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 Call Code Generation
//

use super::codegen::Aarch64CodeGen;
use super::lir::{Aarch64Inst, GpOperand, MemAddr};
use super::regalloc::{Loc, Reg, VReg};
use crate::abi::{ArgClass, HfaBase, RegClass};
use crate::arch::lir::{complex_fp_info, CallTarget, FpSize, OperandSize, Symbol};
use crate::ir::{Instruction, PseudoId};
use crate::types::{TypeId, TypeKind, TypeTable};

impl Aarch64CodeGen {
    /// Handle sret (hidden struct return pointer) argument
    pub(super) fn setup_sret_arg(&mut self, insn: &Instruction, frame_size: i32) -> usize {
        if insn.is_sret_call && !insn.src.is_empty() {
            // First argument is sret pointer - move to X8
            self.emit_move(insn.src[0], Reg::X8, 64, frame_size);
            1 // Skip first arg in main loop
        } else {
            0
        }
    }

    /// Handle Darwin variadic call arguments (all variadic args go on stack)
    pub(super) fn setup_darwin_variadic_args(
        &mut self,
        insn: &Instruction,
        args_start: usize,
        types: &TypeTable,
        frame_size: i32,
    ) -> i32 {
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = VReg::arg_regs();
        let variadic_start = insn.variadic_arg_start.unwrap_or(usize::MAX);
        let mut int_arg_idx = 0;
        let mut fp_arg_idx = 0;
        let mut stack_args = 0;

        // Collect variadic args for stack
        let mut variadic_args: Vec<(PseudoId, bool, u32)> = Vec::new();

        for (i, &arg) in insn.src.iter().enumerate().skip(args_start) {
            let arg_type = insn.arg_types.get(i).copied();
            let is_fp = if let Some(typ) = arg_type {
                types.is_float(typ)
            } else {
                let arg_loc = self.get_location(arg);
                matches!(arg_loc, Loc::VReg(_) | Loc::FImm(..))
            };

            let arg_size = if let Some(typ) = arg_type {
                types.size_bits(typ).max(32)
            } else {
                64
            };

            if i >= variadic_start {
                variadic_args.push((arg, is_fp, arg_size));
            } else {
                // Fixed arg - use registers
                if is_fp {
                    let fp_size = if let Some(typ) = arg_type {
                        types.size_bits(typ)
                    } else {
                        64
                    };
                    if fp_arg_idx < fp_arg_regs.len() {
                        self.emit_fp_move(
                            arg,
                            fp_arg_regs[fp_arg_idx],
                            arg_type,
                            fp_size,
                            frame_size,
                            types,
                        );
                        fp_arg_idx += 1;
                    }
                } else if int_arg_idx < int_arg_regs.len() {
                    self.emit_move(arg, int_arg_regs[int_arg_idx], arg_size, frame_size);
                    int_arg_idx += 1;
                }
            }
        }

        // Store variadic args on stack
        let num_variadic = variadic_args.len();
        if num_variadic > 0 {
            let variadic_bytes = (num_variadic * 8) as i32;
            let aligned_bytes = (variadic_bytes + 15) & !15;

            self.push_lir(Aarch64Inst::Sub {
                size: OperandSize::B64,
                src1: Reg::sp(),
                src2: GpOperand::Imm(aligned_bytes as i64),
                dst: Reg::sp(),
            });

            for (idx, (arg, is_fp, arg_size)) in variadic_args.into_iter().enumerate() {
                let offset = (idx * 8) as i32;
                if is_fp {
                    // Variadic FP args don't have precise type info, use size-based detection
                    self.emit_fp_move(arg, VReg::V16, None, arg_size, frame_size, types);
                    self.push_lir(Aarch64Inst::StrFp {
                        size: FpSize::Double,
                        src: VReg::V16,
                        addr: MemAddr::BaseOffset {
                            base: Reg::SP,
                            offset,
                        },
                    });
                } else {
                    self.emit_move(arg, Reg::X9, arg_size, frame_size);
                    self.push_lir(Aarch64Inst::Str {
                        size: OperandSize::B64,
                        src: Reg::X9,
                        addr: MemAddr::BaseOffset {
                            base: Reg::SP,
                            offset,
                        },
                    });
                }
            }

            stack_args = (aligned_bytes + 15) / 16;
        }

        stack_args
    }

    /// Set up register arguments for standard AAPCS64 calls
    ///
    /// AAPCS64 requires stack arguments to be placed in parameter order at
    /// consecutive 8-byte slots starting from SP. Unlike x86-64, we don't use
    /// push instructions - instead we pre-allocate space and store directly.
    pub(super) fn setup_register_args(
        &mut self,
        insn: &Instruction,
        args_start: usize,
        types: &TypeTable,
        frame_size: i32,
    ) -> i32 {
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = VReg::arg_regs();

        // First pass: identify which args go to registers vs stack
        // Collect stack args with their info for the second pass
        struct StackArg {
            pseudo: PseudoId,
            is_fp: bool,
            size: u32,
            typ: Option<TypeId>,
        }
        let mut stack_args_info: Vec<StackArg> = Vec::new();
        let mut int_arg_idx = 0;
        let mut fp_arg_idx = 0;

        for (i, &arg) in insn.src.iter().enumerate().skip(args_start) {
            let arg_type = insn.arg_types.get(i).copied();
            let is_complex = arg_type.is_some_and(|t| types.is_complex(t));
            let is_fp = if let Some(typ) = arg_type {
                types.is_float(typ)
            } else {
                let arg_loc = self.get_location(arg);
                matches!(arg_loc, Loc::VReg(_) | Loc::FImm(..))
            };

            let arg_size = if let Some(typ) = arg_type {
                types.size_bits(typ).max(32)
            } else {
                64
            };

            if is_complex {
                if fp_arg_idx + 1 < fp_arg_regs.len() {
                    self.setup_complex_arg(
                        arg,
                        arg_type,
                        fp_arg_regs[fp_arg_idx],
                        fp_arg_regs[fp_arg_idx + 1],
                        types,
                        frame_size,
                    );
                    fp_arg_idx += 2;
                } else {
                    // Complex on stack needs 2 slots
                    stack_args_info.push(StackArg {
                        pseudo: arg,
                        is_fp: true,
                        size: arg_size,
                        typ: arg_type,
                    });
                    stack_args_info.push(StackArg {
                        pseudo: arg,
                        is_fp: true,
                        size: arg_size,
                        typ: arg_type,
                    });
                }
            } else if is_fp {
                if fp_arg_idx < fp_arg_regs.len() {
                    let fp_size = if let Some(typ) = arg_type {
                        types.size_bits(typ)
                    } else {
                        64
                    };
                    self.emit_fp_move(
                        arg,
                        fp_arg_regs[fp_arg_idx],
                        arg_type,
                        fp_size,
                        frame_size,
                        types,
                    );
                    fp_arg_idx += 1;
                } else {
                    stack_args_info.push(StackArg {
                        pseudo: arg,
                        is_fp: true,
                        size: arg_size,
                        typ: arg_type,
                    });
                }
            } else if int_arg_idx < int_arg_regs.len() {
                self.emit_move(arg, int_arg_regs[int_arg_idx], arg_size, frame_size);
                int_arg_idx += 1;
            } else {
                stack_args_info.push(StackArg {
                    pseudo: arg,
                    is_fp: false,
                    size: arg_size,
                    typ: arg_type,
                });
            }
        }

        // If no stack args, we're done
        if stack_args_info.is_empty() {
            return 0;
        }

        // Pre-allocate stack space for all stack args (8 bytes each, 16-byte aligned)
        let num_stack_args = stack_args_info.len();
        let stack_bytes = (num_stack_args * 8) as i32;
        let aligned_bytes = (stack_bytes + 15) & !15;

        self.push_lir(Aarch64Inst::Sub {
            size: OperandSize::B64,
            src1: Reg::sp(),
            src2: GpOperand::Imm(aligned_bytes as i64),
            dst: Reg::sp(),
        });

        // Store each stack arg at its proper offset from SP (in parameter order)
        for (idx, stack_arg) in stack_args_info.into_iter().enumerate() {
            let offset = (idx * 8) as i32;
            if stack_arg.is_fp {
                // Use type info for proper FP size determination
                self.emit_fp_move(
                    stack_arg.pseudo,
                    VReg::V16,
                    stack_arg.typ,
                    stack_arg.size,
                    frame_size,
                    types,
                );
                let fp_sz = stack_arg
                    .typ
                    .map(|t| {
                        if types.kind(t) == TypeKind::Float {
                            FpSize::Single
                        } else {
                            FpSize::Double
                        }
                    })
                    .unwrap_or_else(|| {
                        if stack_arg.size == 32 {
                            FpSize::Single
                        } else {
                            FpSize::Double
                        }
                    });
                self.push_lir(Aarch64Inst::StrFp {
                    size: fp_sz,
                    src: VReg::V16,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset,
                    },
                });
            } else {
                self.emit_move(stack_arg.pseudo, Reg::X9, stack_arg.size, frame_size);
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: Reg::X9,
                    addr: MemAddr::BaseOffset {
                        base: Reg::SP,
                        offset,
                    },
                });
            }
        }

        // Return number of 16-byte units allocated (for cleanup)
        (aligned_bytes + 15) / 16
    }

    /// Set up a complex number argument (real + imaginary in two V registers)
    fn setup_complex_arg(
        &mut self,
        arg: PseudoId,
        arg_type: Option<crate::types::TypeId>,
        real_reg: VReg,
        imag_reg: VReg,
        types: &TypeTable,
        frame_size: i32,
    ) {
        let arg_loc = self.get_location(arg);
        let (fp_size, imag_offset) = complex_fp_info(types, &self.base.target, arg_type.unwrap());

        match arg_loc {
            Loc::Stack(offset) => {
                // Complex value is stored directly on stack, load both parts
                let actual_offset = self.stack_offset(frame_size, offset);
                self.push_lir(Aarch64Inst::LdrFp {
                    size: fp_size,
                    dst: real_reg,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                });
                self.push_lir(Aarch64Inst::LdrFp {
                    size: fp_size,
                    dst: imag_reg,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset + imag_offset,
                    },
                });
            }
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::LdrFp {
                    size: fp_size,
                    dst: real_reg,
                    addr: MemAddr::BaseOffset { base: r, offset: 0 },
                });
                self.push_lir(Aarch64Inst::LdrFp {
                    size: fp_size,
                    dst: imag_reg,
                    addr: MemAddr::BaseOffset {
                        base: r,
                        offset: imag_offset,
                    },
                });
            }
            _ => {}
        }
    }

    /// Emit the actual call instruction (direct or indirect)
    pub(super) fn emit_call_instruction(&mut self, insn: &Instruction, func_name: &str) {
        if insn.indirect_target.is_some() {
            self.push_lir(Aarch64Inst::Bl {
                target: CallTarget::Indirect(Reg::X16),
            });
        } else {
            self.push_lir(Aarch64Inst::Bl {
                target: CallTarget::Direct(Symbol::global(func_name)),
            });
        }
    }

    /// Clean up stack after call
    pub(super) fn cleanup_call_stack(&mut self, stack_args: i32) {
        if stack_args > 0 {
            self.push_lir(Aarch64Inst::Add {
                size: OperandSize::B64,
                src1: Reg::sp(),
                src2: GpOperand::Imm((stack_args * 16) as i64),
                dst: Reg::sp(),
            });
        }
    }

    /// Handle call return value using ABI classification.
    pub(super) fn handle_call_return_value(
        &mut self,
        insn: &Instruction,
        types: &TypeTable,
        frame_size: i32,
    ) {
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_loc = self.get_location(target);
        let ret_size = insn.size.max(32);

        let abi_info = insn
            .abi_info
            .as_ref()
            .expect("abi_info must be populated for Call instructions");

        match &abi_info.ret {
            ArgClass::Direct { classes, size_bits } => {
                // Two-register return (9-16 bytes)
                if *size_bits > 64 && classes.len() == 2 {
                    if classes.iter().all(|c| *c == RegClass::Integer) {
                        self.handle_two_reg_return(&dst_loc, frame_size);
                        return;
                    }
                    // Two SSE registers (could be HFA with 2 doubles)
                    if classes.iter().all(|c| *c == RegClass::Sse) {
                        let is_complex_result = insn.typ.is_some_and(|t| types.is_complex(t));
                        if is_complex_result {
                            self.handle_complex_return(insn, &dst_loc, types, frame_size);
                        } else {
                            self.handle_two_fp_return(&dst_loc, frame_size);
                        }
                        return;
                    }
                }
                // Single SSE return
                if classes.first() == Some(&RegClass::Sse) {
                    self.emit_fp_move_to_loc(
                        VReg::V0,
                        &dst_loc,
                        insn.typ,
                        ret_size,
                        frame_size,
                        types,
                    );
                    return;
                }
                // Integer return
                self.emit_move_to_loc(Reg::X0, &dst_loc, ret_size, frame_size);
            }
            ArgClass::Indirect { .. } => {
                // sret: return value already written to memory via X8
            }
            ArgClass::Hfa { count, base } => {
                // HFA: values in V0-V3
                let is_complex_result = insn.typ.is_some_and(|t| types.is_complex(t));
                if *count == 2 && is_complex_result {
                    self.handle_complex_return(insn, &dst_loc, types, frame_size);
                    return;
                }
                // Handle HFA with 1-4 elements
                self.handle_hfa_return(&dst_loc, *count, *base, frame_size);
            }
            ArgClass::Extend { .. } => {
                self.emit_move_to_loc(Reg::X0, &dst_loc, ret_size, frame_size);
            }
            ArgClass::X87 { .. } => {
                unreachable!("x87 FPU returns not available on AArch64");
            }
            ArgClass::Ignore => {
                // Void return, nothing to do
            }
        }
    }

    /// Handle two-register struct return (X0 + X1)
    fn handle_two_reg_return(&mut self, dst_loc: &Loc, frame_size: i32) {
        match dst_loc {
            Loc::Stack(offset) => {
                let actual_offset = self.stack_offset(frame_size, *offset);
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: Reg::X0,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                });
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: Reg::X1,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset + 8,
                    },
                });
            }
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: Reg::X0,
                    addr: MemAddr::BaseOffset {
                        base: *r,
                        offset: 0,
                    },
                });
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: Reg::X1,
                    addr: MemAddr::BaseOffset {
                        base: *r,
                        offset: 8,
                    },
                });
            }
            _ => {}
        }
    }

    /// Handle complex return value (V0 + V1)
    fn handle_complex_return(
        &mut self,
        insn: &Instruction,
        dst_loc: &Loc,
        types: &TypeTable,
        frame_size: i32,
    ) {
        let (fp_size, imag_offset) = complex_fp_info(types, &self.base.target, insn.typ.unwrap());
        match dst_loc {
            Loc::Stack(offset) => {
                let actual_offset = self.stack_offset(frame_size, *offset);
                self.push_lir(Aarch64Inst::StrFp {
                    size: fp_size,
                    src: VReg::V0,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                });
                self.push_lir(Aarch64Inst::StrFp {
                    size: fp_size,
                    src: VReg::V1,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset + imag_offset,
                    },
                });
            }
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::StrFp {
                    size: fp_size,
                    src: VReg::V0,
                    addr: MemAddr::BaseOffset {
                        base: *r,
                        offset: 0,
                    },
                });
                self.push_lir(Aarch64Inst::StrFp {
                    size: fp_size,
                    src: VReg::V1,
                    addr: MemAddr::BaseOffset {
                        base: *r,
                        offset: imag_offset,
                    },
                });
            }
            _ => {}
        }
    }

    /// Handle two FP register return (V0 + V1) for non-complex structs
    fn handle_two_fp_return(&mut self, dst_loc: &Loc, frame_size: i32) {
        match dst_loc {
            Loc::Stack(offset) => {
                let actual_offset = self.stack_offset(frame_size, *offset);
                self.push_lir(Aarch64Inst::StrFp {
                    size: FpSize::Double,
                    src: VReg::V0,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                });
                self.push_lir(Aarch64Inst::StrFp {
                    size: FpSize::Double,
                    src: VReg::V1,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset + 8,
                    },
                });
            }
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::StrFp {
                    size: FpSize::Double,
                    src: VReg::V0,
                    addr: MemAddr::BaseOffset {
                        base: *r,
                        offset: 0,
                    },
                });
                self.push_lir(Aarch64Inst::StrFp {
                    size: FpSize::Double,
                    src: VReg::V1,
                    addr: MemAddr::BaseOffset {
                        base: *r,
                        offset: 8,
                    },
                });
            }
            _ => {}
        }
    }

    /// Handle HFA (Homogeneous Floating-Point Aggregate) return (V0-V3)
    fn handle_hfa_return(&mut self, dst_loc: &Loc, count: u8, base: HfaBase, frame_size: i32) {
        let (fp_size, elem_size) = match base {
            HfaBase::Float32 => (FpSize::Single, 4),
            HfaBase::Float64 => (FpSize::Double, 8),
        };

        let vregs = [VReg::V0, VReg::V1, VReg::V2, VReg::V3];

        match dst_loc {
            Loc::Stack(offset) => {
                let actual_offset = self.stack_offset(frame_size, *offset);
                for i in 0..count.min(4) {
                    self.push_lir(Aarch64Inst::StrFp {
                        size: fp_size,
                        src: vregs[i as usize],
                        addr: MemAddr::BaseOffset {
                            base: Reg::X29,
                            offset: actual_offset + (i as i32 * elem_size),
                        },
                    });
                }
            }
            Loc::Reg(r) => {
                for i in 0..count.min(4) {
                    self.push_lir(Aarch64Inst::StrFp {
                        size: fp_size,
                        src: vregs[i as usize],
                        addr: MemAddr::BaseOffset {
                            base: *r,
                            offset: i as i32 * elem_size,
                        },
                    });
                }
            }
            _ => {}
        }
    }
}
