//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 Code Generator
// Converts IR to x86-64 assembly (AT&T syntax)
//
// Uses linear scan register allocation and System V AMD64 ABI.
//

use crate::arch::codegen::{is_variadic_function, BswapSize, CodeGenBase, CodeGenerator, UnaryOp};
use crate::arch::lir::{
    complex_fp_info, CallTarget, CondCode, Directive, FpSize, Label, OperandSize, Symbol,
};
use crate::arch::x86_64::lir::{GpOperand, MemAddr, ShiftCount, X86Inst, XmmOperand};
use crate::arch::x86_64::regalloc::{Loc, Reg, RegAlloc, XmmReg};
use crate::ir::{Function, Instruction, Module, Opcode, Pseudo, PseudoId, PseudoKind};
use crate::target::Target;
use crate::types::{TypeId, TypeTable};
use std::collections::{HashMap, HashSet};

// ============================================================================
// x86-64 Code Generator
// ============================================================================

/// x86-64 code generator
pub struct X86_64CodeGen {
    /// Common code generation infrastructure
    base: CodeGenBase<X86Inst>,
    /// Current function's register allocation
    locations: HashMap<PseudoId, Loc>,
    /// Current function's pseudos (for looking up values)
    pseudos: Vec<Pseudo>,
    /// Callee-saved registers used in current function (for epilogue)
    callee_saved_regs: Vec<Reg>,
    /// Offset to add to stack locations to account for callee-saved registers
    callee_saved_offset: i32,
    /// Offset from rbp to register save area (for variadic functions)
    reg_save_area_offset: i32,
    /// Number of fixed GP parameters (for variadic functions)
    num_fixed_gp_params: usize,
    /// Number of fixed FP parameters (for variadic functions)
    num_fixed_fp_params: usize,
    /// Counter for generating unique internal labels
    unique_label_counter: u32,
}

impl X86_64CodeGen {
    pub fn new(target: Target) -> Self {
        Self {
            base: CodeGenBase::new(target),
            locations: HashMap::new(),
            pseudos: Vec::new(),
            callee_saved_regs: Vec::new(),
            callee_saved_offset: 0,
            reg_save_area_offset: 0,
            num_fixed_gp_params: 0,
            num_fixed_fp_params: 0,
            unique_label_counter: 0,
        }
    }

    /// Push a LIR instruction to the buffer (deferred emission)
    #[inline]
    fn push_lir(&mut self, inst: X86Inst) {
        self.base.push_lir(inst);
    }

    /// Convert a Loc to a GpOperand for LIR
    fn loc_to_gp_operand(&self, loc: &Loc) -> GpOperand {
        match loc {
            Loc::Reg(r) => GpOperand::Reg(*r),
            Loc::Stack(offset) => {
                let adjusted = *offset + self.callee_saved_offset;
                GpOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: -adjusted,
                })
            }
            Loc::Imm(v) => GpOperand::Imm(*v),
            Loc::FImm(_, _) => GpOperand::Imm(0), // FP immediates handled separately
            Loc::Xmm(_) => GpOperand::Imm(0),     // XMM handled separately
            Loc::Global(name) => GpOperand::Mem(MemAddr::RipRelative(Symbol::global(name.clone()))),
        }
    }

    /// Emit .loc directive for source line tracking (delegates to base)
    #[inline]
    fn emit_loc(&mut self, insn: &Instruction) {
        self.base.emit_loc(insn);
    }

    /// Emit file header (delegates to base)
    #[inline]
    fn emit_header(&mut self) {
        self.base.emit_header();
    }

    /// Emit a global variable (delegates to base)
    #[inline]
    fn emit_global(
        &mut self,
        name: &str,
        typ: &TypeId,
        init: &crate::ir::Initializer,
        types: &TypeTable,
    ) {
        self.base.emit_global(name, typ, init, types);
    }

    fn emit_function(&mut self, func: &Function, types: &TypeTable) {
        // Save current function name for unique label generation
        self.base.current_fn = func.name.clone();

        // Check if this function uses varargs
        let is_variadic = is_variadic_function(func);

        // Register allocation
        let mut alloc = RegAlloc::new();
        self.locations = alloc.allocate(func, types);
        self.pseudos = func.pseudos.clone();

        let stack_size = alloc.stack_size();
        self.callee_saved_regs = alloc.callee_saved_used().to_vec();
        self.callee_saved_offset = self.callee_saved_regs.len() as i32 * 8;

        // For variadic functions, we need extra space for the register save area
        // 6 GP regs * 8 bytes = 48 bytes for GP registers
        // 8 XMM regs * 16 bytes = 128 bytes for FP registers
        // Total = 176 bytes
        let reg_save_area_size: i32 = if is_variadic { 176 } else { 0 };
        self.reg_save_area_offset = if is_variadic {
            // The register save area will be at the end of the stack frame
            self.callee_saved_offset + stack_size + reg_save_area_size
        } else {
            0
        };

        // Function prologue
        self.push_lir(X86Inst::Directive(Directive::Blank));
        self.push_lir(X86Inst::Directive(Directive::Text));

        // Skip .globl for static functions (internal linkage)
        if !func.is_static {
            self.push_lir(X86Inst::Directive(Directive::global(&func.name)));
        }

        // ELF-only type (handled by Directive::emit which skips on macOS)
        self.push_lir(X86Inst::Directive(Directive::type_func(&func.name)));

        // Function label
        self.push_lir(X86Inst::Directive(Directive::global_label(&func.name)));

        // CFI: Start procedure (enables stack unwinding for this function)
        if self.base.emit_unwind_tables {
            self.push_lir(X86Inst::Directive(Directive::CfiStartProc));
        }

        // Prologue: save frame pointer and allocate stack
        let bp = Reg::bp();
        let sp = Reg::sp();
        self.push_lir(X86Inst::Push {
            src: GpOperand::Reg(bp),
        });
        if self.base.emit_debug {
            // After pushq %rbp: CFA is now at %rsp+16, and %rbp is saved at CFA-16
            self.push_lir(X86Inst::Directive(Directive::CfiDefCfaOffset(16)));
            self.push_lir(X86Inst::Directive(Directive::cfi_offset("%rbp", -16)));
        }
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(sp),
            dst: GpOperand::Reg(bp),
        });
        if self.base.emit_debug {
            // After movq %rsp, %rbp: CFA is now tracked by %rbp+16
            self.push_lir(X86Inst::Directive(Directive::cfi_def_cfa_register("%rbp")));
        }

        // Save callee-saved registers
        let mut cfi_offset = -24i32; // First callee-saved is at -24 (after rbp at -16)
        for reg in &self.callee_saved_regs.clone() {
            self.push_lir(X86Inst::Push {
                src: GpOperand::Reg(*reg),
            });
            if self.base.emit_debug {
                self.push_lir(X86Inst::Directive(Directive::cfi_offset(
                    reg.name64(),
                    cfi_offset,
                )));
            }
            cfi_offset -= 8;
        }

        // Allocate stack space for locals + register save area (if variadic)
        let total_stack =
            stack_size + (self.callee_saved_regs.len() as i32 * 8) + reg_save_area_size;
        // Ensure 16-byte alignment
        let aligned_stack = (total_stack + 15) & !15;
        if aligned_stack > self.callee_saved_regs.len() as i32 * 8 {
            self.push_lir(X86Inst::Sub {
                size: OperandSize::B64,
                src: GpOperand::Imm(
                    (aligned_stack - (self.callee_saved_regs.len() as i32 * 8)) as i64,
                ),
                dst: Reg::Rsp,
            });
        }

        // Emit stores for arguments spilled from caller-saved registers to stack
        // These must be stored early before any call can clobber them
        for spilled in alloc.spilled_args() {
            let adjusted = spilled.to_stack_offset + self.callee_saved_offset;
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(spilled.from_reg),
                dst: GpOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: -adjusted,
                }),
            });
        }

        // For variadic functions, save argument registers to the register save area
        // This must be done before any other code uses these registers
        // AMD64 ABI: rdi at offset 0, rsi at offset 8, rdx at offset 16, etc.
        if is_variadic {
            let int_arg_regs = Reg::arg_regs();
            // Save all 6 GP argument registers at their ABI-specified offsets
            for (i, reg) in int_arg_regs.iter().enumerate() {
                // offset from rbp = reg_save_area_offset - (i * 8)
                // reg at ABI offset i*8 relative to reg_save_area base
                let offset = self.reg_save_area_offset - (i as i32 * 8);
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(*reg),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -offset,
                    }),
                });
            }

            // Save XMM0-XMM7 at offsets 48-175 from register save area base
            // AMD64 ABI: XMM regs start at offset 48 from reg_save_area base
            // Each XMM slot is 16 bytes (128-bit aligned)
            let xmm_arg_regs = XmmReg::arg_regs();
            for (i, xmm) in xmm_arg_regs.iter().enumerate() {
                // XMM save area starts at offset 48 from reg_save_area base
                // Each slot is 16 bytes: base_offset = reg_save_area_offset - 48 - (i * 16)
                let offset = self.reg_save_area_offset - 48 - (i as i32 * 16);
                self.push_lir(X86Inst::MovFp {
                    size: FpSize::Double, // movsd - save 64-bit double
                    src: XmmOperand::Reg(*xmm),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -offset,
                    }),
                });
            }
        }

        // Move arguments from registers to their allocated locations if needed
        // System V AMD64 ABI: integer args in RDI, RSI, RDX, RCX, R8, R9
        //                     FP args in XMM0-XMM7 (separate counters)
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = XmmReg::arg_regs();
        let mut int_arg_idx = 0;
        let mut fp_arg_idx = 0;

        // Track which pseudos were already spilled via spill_args_across_calls
        // to avoid double-storing them here
        let spilled_pseudos: HashSet<PseudoId> =
            alloc.spilled_args().iter().map(|s| s.pseudo).collect();

        // Detect if there's a hidden return pointer (for functions returning large structs)
        // The __sret pseudo has arg_idx=0 and shifts all other arg indices by 1
        let has_sret = func
            .pseudos
            .iter()
            .any(|p| matches!(p.kind, PseudoKind::Arg(0)) && p.name.as_deref() == Some("__sret"));
        let arg_idx_offset: u32 = if has_sret { 1 } else { 0 };

        // If there's a hidden return pointer, it takes RDI, so params start from RSI
        if has_sret {
            int_arg_idx = 1;
        }

        for (i, (_name, typ)) in func.params.iter().enumerate() {
            // Find the pseudo for this argument
            for pseudo in &func.pseudos {
                if let PseudoKind::Arg(arg_idx) = pseudo.kind {
                    if arg_idx == (i as u32) + arg_idx_offset {
                        // Skip pseudos already stored via spilled_args
                        if spilled_pseudos.contains(&pseudo.id) {
                            // Still need to count this arg for register assignment tracking
                            let is_fp = types.is_float(*typ);
                            let is_complex = types.is_complex(*typ);
                            if is_complex {
                                fp_arg_idx += 2;
                            } else if is_fp {
                                fp_arg_idx += 1;
                            } else {
                                int_arg_idx += 1;
                            }
                            break;
                        }
                        let is_fp = types.is_float(*typ);
                        let is_complex = types.is_complex(*typ);
                        if is_complex {
                            // Complex argument - uses TWO consecutive XMM registers
                            // double _Complex: XMM0+XMM1, XMM2+XMM3, etc.
                            // Look up the local variable (same name as param) for stack location
                            if fp_arg_idx + 1 < fp_arg_regs.len() {
                                // Find the local for this parameter by name
                                let param_name = &func.params[i].0;
                                if let Some(local) = func.locals.get(param_name) {
                                    if let Some(Loc::Stack(offset)) = self.locations.get(&local.sym)
                                    {
                                        let adjusted = offset + self.callee_saved_offset;
                                        let (fp_size, imag_offset) = complex_fp_info(types, *typ);
                                        // Store real part from first XMM register
                                        self.push_lir(X86Inst::MovFp {
                                            size: fp_size,
                                            src: XmmOperand::Reg(fp_arg_regs[fp_arg_idx]),
                                            dst: XmmOperand::Mem(MemAddr::BaseOffset {
                                                base: Reg::Rbp,
                                                offset: -adjusted,
                                            }),
                                        });
                                        // Store imag part from second XMM register
                                        self.push_lir(X86Inst::MovFp {
                                            size: fp_size,
                                            src: XmmOperand::Reg(fp_arg_regs[fp_arg_idx + 1]),
                                            dst: XmmOperand::Mem(MemAddr::BaseOffset {
                                                base: Reg::Rbp,
                                                offset: -adjusted + imag_offset,
                                            }),
                                        });
                                    }
                                }
                            }
                            fp_arg_idx += 2; // Complex uses two XMM registers
                        } else if is_fp {
                            // FP argument
                            if fp_arg_idx < fp_arg_regs.len() {
                                if let Some(Loc::Stack(offset)) = self.locations.get(&pseudo.id) {
                                    // Move from FP arg register to stack
                                    let adjusted = offset + self.callee_saved_offset;
                                    let fp_size = if types.size_bits(*typ) == 32 {
                                        FpSize::Single
                                    } else {
                                        FpSize::Double
                                    };
                                    self.push_lir(X86Inst::MovFp {
                                        size: fp_size,
                                        src: XmmOperand::Reg(fp_arg_regs[fp_arg_idx]),
                                        dst: XmmOperand::Mem(MemAddr::BaseOffset {
                                            base: Reg::Rbp,
                                            offset: -adjusted,
                                        }),
                                    });
                                }
                            }
                            fp_arg_idx += 1;
                        } else {
                            // Integer argument
                            if int_arg_idx < int_arg_regs.len() {
                                if let Some(Loc::Stack(offset)) = self.locations.get(&pseudo.id) {
                                    // Move from arg register to stack
                                    let adjusted = offset + self.callee_saved_offset;
                                    self.push_lir(X86Inst::Mov {
                                        size: OperandSize::B64,
                                        src: GpOperand::Reg(int_arg_regs[int_arg_idx]),
                                        dst: GpOperand::Mem(MemAddr::BaseOffset {
                                            base: Reg::Rbp,
                                            offset: -adjusted,
                                        }),
                                    });
                                }
                            }
                            int_arg_idx += 1;
                        }
                        break;
                    }
                }
            }
        }

        // Save number of fixed GP and FP params for va_start
        // Count how many non-FP params and FP params we have
        if is_variadic {
            self.num_fixed_gp_params = func
                .params
                .iter()
                .filter(|(_, typ)| !types.is_float(*typ))
                .count();
            if has_sret {
                self.num_fixed_gp_params += 1; // Account for hidden sret pointer
            }
            self.num_fixed_fp_params = func
                .params
                .iter()
                .filter(|(_, typ)| types.is_float(*typ))
                .count();
        }

        // Emit basic blocks
        for block in &func.blocks {
            self.emit_block(block, types);
        }

        // CFI: End procedure
        if self.base.emit_unwind_tables {
            self.push_lir(X86Inst::Directive(Directive::CfiEndProc));
        }
    }

    fn emit_block(&mut self, block: &crate::ir::BasicBlock, types: &TypeTable) {
        // Always emit block ID label for consistency with jumps
        // (jumps reference blocks by ID, not by C label name)
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(Label::new(
            &self.base.current_fn,
            block.id.0,
        ))));

        // Emit instructions
        for insn in &block.insns {
            self.emit_insn(insn, types);
        }
    }

    /// Emit return instruction: move return value to registers and emit epilogue
    fn emit_ret(&mut self, insn: &Instruction, types: &TypeTable) {
        // Move return value to appropriate register if present
        // System V AMD64 ABI: integers in RAX, floats in XMM0, complex in XMM0+XMM1
        // Two-register struct returns (9-16 bytes) go in RAX+RDX
        if let Some(src) = insn.src.first() {
            let src_loc = self.get_location(*src);
            let is_complex = insn.typ.is_some_and(|t| types.is_complex(t));
            let is_fp = matches!(src_loc, Loc::Xmm(_) | Loc::FImm(..))
                || insn.typ.is_some_and(|t| types.is_float(t));
            if insn.is_two_reg_return {
                // Two-register struct return: src[0] -> RAX, src[1] -> RDX
                self.emit_move(*src, Reg::Rax, 64);
                if let Some(&src2) = insn.src.get(1) {
                    self.emit_move(src2, Reg::Rdx, 64);
                }
            } else if is_complex {
                // Complex return value: load real into XMM0, imag into XMM1
                let (fp_size, imag_offset) = complex_fp_info(types, insn.typ.unwrap());
                match src_loc {
                    Loc::Stack(offset) => {
                        let adjusted = offset + self.callee_saved_offset;
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::B64,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::Rbp,
                                offset: -adjusted,
                            }),
                            dst: GpOperand::Reg(Reg::Rax),
                        });
                        self.push_lir(X86Inst::MovFp {
                            size: fp_size,
                            src: XmmOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::Rax,
                                offset: 0,
                            }),
                            dst: XmmOperand::Reg(XmmReg::Xmm0),
                        });
                        self.push_lir(X86Inst::MovFp {
                            size: fp_size,
                            src: XmmOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::Rax,
                                offset: imag_offset,
                            }),
                            dst: XmmOperand::Reg(XmmReg::Xmm1),
                        });
                    }
                    Loc::Reg(r) => {
                        self.push_lir(X86Inst::MovFp {
                            size: fp_size,
                            src: XmmOperand::Mem(MemAddr::BaseOffset { base: r, offset: 0 }),
                            dst: XmmOperand::Reg(XmmReg::Xmm0),
                        });
                        self.push_lir(X86Inst::MovFp {
                            size: fp_size,
                            src: XmmOperand::Mem(MemAddr::BaseOffset {
                                base: r,
                                offset: imag_offset,
                            }),
                            dst: XmmOperand::Reg(XmmReg::Xmm1),
                        });
                    }
                    _ => {}
                }
            } else if is_fp {
                self.emit_fp_move(*src, XmmReg::Xmm0, insn.size);
            } else {
                self.emit_move(*src, Reg::Rax, insn.size);
            }
        }

        // Epilogue: restore callee-saved registers and return
        let bp = Reg::bp();
        let num_callee_saved = self.callee_saved_regs.len();
        if num_callee_saved > 0 {
            let offset = num_callee_saved * 8;
            self.push_lir(X86Inst::Lea {
                addr: MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: -(offset as i32),
                },
                dst: Reg::Rsp,
            });
            let callee_saved: Vec<Reg> = self.callee_saved_regs.iter().rev().copied().collect();
            for reg in callee_saved {
                self.push_lir(X86Inst::Pop { dst: reg });
            }
        } else {
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::Rbp),
                dst: GpOperand::Reg(Reg::Rsp),
            });
        }
        self.push_lir(X86Inst::Pop { dst: bp });
        self.push_lir(X86Inst::Ret);
    }

    /// Emit conditional branch: test condition and branch accordingly
    /// Returns true if an early return was taken (for constant conditions)
    fn emit_cbr(&mut self, insn: &Instruction) -> bool {
        let Some(&cond) = insn.src.first() else {
            return false;
        };

        let loc = self.get_location(cond);
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(size);

        match &loc {
            Loc::Reg(r) => {
                self.push_lir(X86Inst::Test {
                    size: op_size,
                    src: GpOperand::Reg(*r),
                    dst: GpOperand::Reg(*r),
                });
            }
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                self.push_lir(X86Inst::Cmp {
                    size: op_size,
                    src: GpOperand::Imm(0),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                });
            }
            Loc::Imm(v) => {
                let target = if *v != 0 { insn.bb_true } else { insn.bb_false };
                if let Some(target) = target {
                    self.push_lir(X86Inst::Jmp {
                        target: Label::new(&self.base.current_fn, target.0),
                    });
                }
                return true;
            }
            Loc::Global(_) => {
                self.emit_move(cond, Reg::R10, size);
                self.push_lir(X86Inst::Test {
                    size: op_size,
                    src: GpOperand::Reg(Reg::R10),
                    dst: GpOperand::Reg(Reg::R10),
                });
            }
            Loc::Xmm(x) => {
                self.push_lir(X86Inst::XorpsSelf { reg: XmmReg::Xmm15 });
                self.push_lir(X86Inst::UComiFp {
                    size: FpSize::Single,
                    src: XmmOperand::Reg(*x),
                    dst: XmmReg::Xmm15,
                });
            }
            Loc::FImm(v, _) => {
                let target = if *v != 0.0 {
                    insn.bb_true
                } else {
                    insn.bb_false
                };
                if let Some(target) = target {
                    self.push_lir(X86Inst::Jmp {
                        target: Label::new(&self.base.current_fn, target.0),
                    });
                }
                return true;
            }
        }

        if let Some(target) = insn.bb_true {
            self.push_lir(X86Inst::Jcc {
                cc: CondCode::Ne,
                target: Label::new(&self.base.current_fn, target.0),
            });
        }
        if let Some(target) = insn.bb_false {
            self.push_lir(X86Inst::Jmp {
                target: Label::new(&self.base.current_fn, target.0),
            });
        }
        false
    }

    fn emit_insn(&mut self, insn: &Instruction, types: &TypeTable) {
        // Emit .loc directive for debug info
        self.emit_loc(insn);

        match insn.op {
            Opcode::Entry => {
                // Already handled in function prologue
            }

            Opcode::Ret => {
                self.emit_ret(insn, types);
            }

            Opcode::Br => {
                if let Some(target) = insn.bb_true {
                    self.push_lir(X86Inst::Jmp {
                        target: Label::new(&self.base.current_fn, target.0),
                    });
                }
            }

            Opcode::Cbr => if self.emit_cbr(insn) {},

            Opcode::Switch => {
                // Switch uses target as the value to switch on
                if let Some(val) = insn.target {
                    let size = insn.size.max(32);
                    let op_size = OperandSize::from_bits(size);
                    // Move switch value to R10 (scratch register)
                    self.emit_move(val, Reg::R10, size);

                    // Generate comparisons for each case
                    for (case_val, target_bb) in &insn.switch_cases {
                        // LIR: compare with case value
                        self.push_lir(X86Inst::Cmp {
                            size: op_size,
                            src: GpOperand::Imm(*case_val),
                            dst: GpOperand::Reg(Reg::R10),
                        });
                        // LIR: conditional jump on equal
                        self.push_lir(X86Inst::Jcc {
                            cc: CondCode::Eq,
                            target: Label::new(&self.base.current_fn, target_bb.0),
                        });
                    }

                    // Jump to default (or fall through if no default)
                    if let Some(default_bb) = insn.switch_default {
                        // LIR: unconditional jump to default
                        self.push_lir(X86Inst::Jmp {
                            target: Label::new(&self.base.current_fn, default_bb.0),
                        });
                    }
                }
            }

            Opcode::Add
            | Opcode::Sub
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
            | Opcode::Shl
            | Opcode::Lsr
            | Opcode::Asr => {
                self.emit_binop(insn);
            }

            Opcode::Mul => {
                self.emit_mul(insn);
            }

            Opcode::DivS | Opcode::DivU | Opcode::ModS | Opcode::ModU => {
                self.emit_div(insn);
            }

            // Floating-point arithmetic operations
            Opcode::FAdd | Opcode::FSub | Opcode::FMul | Opcode::FDiv => {
                self.emit_fp_binop(insn);
            }

            Opcode::FNeg => {
                self.emit_fp_neg(insn);
            }

            // Floating-point comparisons
            Opcode::FCmpOEq
            | Opcode::FCmpONe
            | Opcode::FCmpOLt
            | Opcode::FCmpOLe
            | Opcode::FCmpOGt
            | Opcode::FCmpOGe => {
                self.emit_fp_compare(insn);
            }

            // Integer to float conversions
            Opcode::UCvtF | Opcode::SCvtF => {
                self.emit_int_to_float(insn);
            }

            // Float to integer conversions
            Opcode::FCvtU | Opcode::FCvtS => {
                self.emit_float_to_int(insn);
            }

            // Float to float conversions (e.g., float to double)
            Opcode::FCvtF => {
                self.emit_float_to_float(insn);
            }

            Opcode::SetEq
            | Opcode::SetNe
            | Opcode::SetLt
            | Opcode::SetLe
            | Opcode::SetGt
            | Opcode::SetGe
            | Opcode::SetB
            | Opcode::SetBe
            | Opcode::SetA
            | Opcode::SetAe => {
                self.emit_compare(insn);
            }

            Opcode::Neg => self.emit_unary_op(insn, UnaryOp::Neg),
            Opcode::Not => self.emit_unary_op(insn, UnaryOp::Not),

            Opcode::Load => {
                self.emit_load(insn, types);
            }

            Opcode::Store => {
                self.emit_store(insn, types);
            }

            Opcode::Call => {
                self.emit_call(insn, types);
            }

            Opcode::SetVal => {
                if let Some(target) = insn.target {
                    if let Some(pseudo) = self.pseudos.iter().find(|p| p.id == target) {
                        let target_loc = self.locations.get(&target).cloned();
                        match &pseudo.kind {
                            PseudoKind::Val(v) => {
                                if let Some(Loc::Reg(r)) = target_loc {
                                    self.push_lir(X86Inst::Mov {
                                        size: OperandSize::from_bits(insn.size),
                                        src: GpOperand::Imm(*v),
                                        dst: GpOperand::Reg(r),
                                    });
                                }
                            }
                            PseudoKind::FVal(v) => {
                                // Only emit code if the target is in an XMM register
                                // FImm locations are materialized inline at use sites
                                if let Some(Loc::Xmm(_)) = target_loc {
                                    self.emit_fp_const_load(target, *v, insn.size);
                                }
                                // For FImm locations, do nothing - the value will be
                                // loaded inline when used in operations
                            }
                            _ => {}
                        }
                    }
                }
            }

            Opcode::Copy => {
                if let (Some(target), Some(&src)) = (insn.target, insn.src.first()) {
                    // Pass the type to emit_copy for proper sign/zero extension
                    self.emit_copy_with_type(src, target, insn.size, insn.typ, types);
                }
            }

            Opcode::SymAddr => {
                if let (Some(target), Some(&src)) = (insn.target, insn.src.first()) {
                    let dst_loc = self.get_location(target);
                    let dst_reg = match &dst_loc {
                        Loc::Reg(r) => *r,
                        _ => Reg::Rax,
                    };
                    let src_loc = self.get_location(src);
                    match src_loc {
                        Loc::Global(name) => {
                            // Check if it's a local label (starts with '.') or global symbol
                            let is_local_label = name.starts_with('.');
                            self.push_lir(X86Inst::Lea {
                                addr: MemAddr::RipRelative(Symbol {
                                    name: name.clone(),
                                    is_local: is_local_label,
                                }),
                                dst: dst_reg,
                            });
                        }
                        Loc::Stack(offset) => {
                            // Get address of stack location
                            let adjusted = offset + self.callee_saved_offset;
                            self.push_lir(X86Inst::Lea {
                                addr: MemAddr::BaseOffset {
                                    base: Reg::Rbp,
                                    offset: -adjusted,
                                },
                                dst: dst_reg,
                            });
                        }
                        _ => {}
                    }
                    // Move to final destination if needed
                    if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
                        self.emit_move_to_loc(dst_reg, &dst_loc, 64);
                    }
                }
            }

            Opcode::Select => {
                self.emit_select(insn);
            }

            Opcode::Zext | Opcode::Sext | Opcode::Trunc => {
                self.emit_extend(insn);
            }

            // ================================================================
            // Variadic function support (va_* builtins)
            // ================================================================
            Opcode::VaStart => {
                self.emit_va_start(insn);
            }

            Opcode::VaArg => {
                self.emit_va_arg(insn, types);
            }

            Opcode::VaEnd => {
                // va_end is a no-op on all platforms
            }

            Opcode::VaCopy => {
                self.emit_va_copy(insn);
            }

            // Byte-swapping builtins
            Opcode::Bswap16 => self.emit_bswap(insn, BswapSize::B16),
            Opcode::Bswap32 => self.emit_bswap(insn, BswapSize::B32),
            Opcode::Bswap64 => self.emit_bswap(insn, BswapSize::B64),

            // ================================================================
            // Count trailing zeros builtins
            Opcode::Ctz32 => self.emit_ctz(insn, OperandSize::B32),
            Opcode::Ctz64 => self.emit_ctz(insn, OperandSize::B64),
            // Count leading zeros builtins
            Opcode::Clz32 => self.emit_clz(insn, OperandSize::B32),
            Opcode::Clz64 => self.emit_clz(insn, OperandSize::B64),
            // Population count builtins
            Opcode::Popcount32 => self.emit_popcount(insn, OperandSize::B32),
            Opcode::Popcount64 => self.emit_popcount(insn, OperandSize::B64),

            Opcode::Alloca => {
                self.emit_alloca(insn);
            }

            Opcode::Unreachable => {
                // Emit ud2 instruction - undefined instruction that traps
                // This is used for __builtin_unreachable() to indicate code
                // that should never be reached. If it is reached, the CPU
                // will generate a SIGILL.
                self.push_lir(X86Inst::Ud2);
            }

            // ================================================================
            // setjmp/longjmp support
            // ================================================================
            Opcode::Setjmp => {
                self.emit_setjmp(insn);
            }

            Opcode::Longjmp => {
                self.emit_longjmp(insn);
            }

            Opcode::Asm => {
                self.emit_inline_asm(insn);
            }

            // Skip no-ops and unimplemented
            _ => {}
        }
    }

    fn get_location(&self, pseudo: PseudoId) -> Loc {
        self.locations.get(&pseudo).cloned().unwrap_or(Loc::Imm(0))
    }

    fn emit_move(&mut self, src: PseudoId, dst: Reg, size: u32) {
        let size = size.max(32);
        let op_size = OperandSize::from_bits(size);
        let loc = self.get_location(src);
        match loc {
            Loc::Reg(r) if r == dst => {
                // No-op: same register
            }
            Loc::Reg(r) => {
                // LIR: register-to-register move
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(r),
                    dst: GpOperand::Reg(dst),
                });
            }
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                // LIR: memory-to-register move
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                    dst: GpOperand::Reg(dst),
                });
            }
            Loc::Imm(v) => {
                // x86-64: movl sign-extends to 64-bit, movq only works with 32-bit signed immediates
                // For values outside 32-bit signed range, use movabsq
                if size == 64 && (v > i32::MAX as i64 || v < i32::MIN as i64) {
                    // LIR: 64-bit immediate move
                    self.push_lir(X86Inst::MovAbs { imm: v, dst });
                } else {
                    // LIR: immediate-to-register move
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Imm(v),
                        dst: GpOperand::Reg(dst),
                    });
                }
            }
            Loc::Global(name) => {
                // LIR: RIP-relative memory-to-register move
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Mem(MemAddr::RipRelative(Symbol::global(name.clone()))),
                    dst: GpOperand::Reg(dst),
                });
            }
            Loc::Xmm(x) => {
                // Move from XMM to general-purpose register
                // LIR: XMM to GP move
                self.push_lir(X86Inst::MovXmmGp {
                    size: OperandSize::B64,
                    src: x,
                    dst,
                });
            }
            Loc::FImm(..) => {
                // Float immediate cannot be directly moved to GP register
                // This should not normally happen
            }
        }
    }

    fn emit_move_to_loc(&mut self, src: Reg, dst: &Loc, size: u32) {
        // For stack stores, use actual size to properly handle char/short
        // For register-to-register, use minimum 32-bit
        match dst {
            Loc::Reg(r) if *r == src => {
                // No-op: same register
            }
            Loc::Reg(r) => {
                let reg_size = size.max(32);
                let op_size = OperandSize::from_bits(reg_size);
                // LIR: register-to-register move
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(src),
                    dst: GpOperand::Reg(*r),
                });
            }
            Loc::Stack(offset) => {
                // Use actual size for memory stores (8, 16, 32, 64 bits)
                let adjusted = offset + self.callee_saved_offset;
                let op_size = OperandSize::from_bits(size);
                // LIR: register-to-memory move
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(src),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                });
            }
            _ => {}
        }
    }

    fn emit_binop(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(size);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        let work_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::Rax,
        };

        if matches!(insn.op, Opcode::Shl | Opcode::Lsr | Opcode::Asr) {
            let src2_loc = self.get_location(src2);
            match src2_loc {
                Loc::Imm(v) => {
                    // Immediate shift - can use work_reg directly
                    self.emit_move(src1, work_reg, size);
                    // LIR: shift by immediate
                    let shift_count = ShiftCount::Imm(v as u8);
                    match insn.op {
                        Opcode::Shl => self.push_lir(X86Inst::Shl {
                            size: op_size,
                            count: shift_count,
                            dst: work_reg,
                        }),
                        Opcode::Lsr => self.push_lir(X86Inst::Shr {
                            size: op_size,
                            count: shift_count,
                            dst: work_reg,
                        }),
                        Opcode::Asr => self.push_lir(X86Inst::Sar {
                            size: op_size,
                            count: shift_count,
                            dst: work_reg,
                        }),
                        _ => {}
                    }
                }
                _ => {
                    // Variable shift - shift count goes in Rcx/Cl
                    // IMPORTANT: If work_reg is Rcx, we can't use it for the value
                    // because emit_move(src2, Rcx) will clobber it. Use R10 instead.
                    let shift_reg = if work_reg == Reg::Rcx {
                        Reg::R10
                    } else {
                        work_reg
                    };
                    self.emit_move(src1, shift_reg, size);
                    self.emit_move(src2, Reg::Rcx, 8);
                    // LIR: shift by %cl
                    let shift_count = ShiftCount::Cl;
                    match insn.op {
                        Opcode::Shl => self.push_lir(X86Inst::Shl {
                            size: op_size,
                            count: shift_count,
                            dst: shift_reg,
                        }),
                        Opcode::Lsr => self.push_lir(X86Inst::Shr {
                            size: op_size,
                            count: shift_count,
                            dst: shift_reg,
                        }),
                        Opcode::Asr => self.push_lir(X86Inst::Sar {
                            size: op_size,
                            count: shift_count,
                            dst: shift_reg,
                        }),
                        _ => {}
                    }
                    // If we used R10, move result back to work_reg (Rcx)
                    if shift_reg != work_reg {
                        self.push_lir(X86Inst::Mov {
                            size: op_size,
                            src: GpOperand::Reg(shift_reg),
                            dst: GpOperand::Reg(work_reg),
                        });
                    }
                }
            }
        } else {
            // Non-shift binary ops: move src1 into work_reg first
            self.emit_move(src1, work_reg, size);
            let src2_loc = self.get_location(src2);
            // For 64-bit operations, check if immediate fits in 32-bit signed range.
            // x86-64 binary ops (add, sub, and, or, xor) only support 32-bit sign-extended immediates.
            // If the immediate doesn't fit, load it into R11 first.
            let src2_gp = match &src2_loc {
                Loc::Imm(v) if size == 64 && (*v > i32::MAX as i64 || *v < i32::MIN as i64) => {
                    // Large 64-bit immediate - load into R11 first
                    self.push_lir(X86Inst::MovAbs {
                        imm: *v,
                        dst: Reg::R11,
                    });
                    GpOperand::Reg(Reg::R11)
                }
                _ => self.loc_to_gp_operand(&src2_loc),
            };
            // LIR: binary operation (add, sub, and, or, xor)
            match insn.op {
                Opcode::Add => self.push_lir(X86Inst::Add {
                    size: op_size,
                    src: src2_gp,
                    dst: work_reg,
                }),
                Opcode::Sub => self.push_lir(X86Inst::Sub {
                    size: op_size,
                    src: src2_gp,
                    dst: work_reg,
                }),
                Opcode::And => self.push_lir(X86Inst::And {
                    size: op_size,
                    src: src2_gp,
                    dst: work_reg,
                }),
                Opcode::Or => self.push_lir(X86Inst::Or {
                    size: op_size,
                    src: src2_gp,
                    dst: work_reg,
                }),
                Opcode::Xor => self.push_lir(X86Inst::Xor {
                    size: op_size,
                    src: src2_gp,
                    dst: work_reg,
                }),
                _ => {}
            }
        }
        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, size);
        }
    }

    fn emit_mul(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(size);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10, // Use scratch register R10
        };
        self.emit_move(src1, dst_reg, size);
        let src2_loc = self.get_location(src2);
        // LIR: 2-operand imul instruction
        let src2_gp = self.loc_to_gp_operand(&src2_loc);
        self.push_lir(X86Inst::IMul2 {
            size: op_size,
            src: src2_gp,
            dst: dst_reg,
        });
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size);
        }
    }

    fn emit_div(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(size);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // IMPORTANT: Check divisor location BEFORE cltd/cqto clobbers RDX!
        // If divisor is in Rax or Rdx, we must save it to R10 first.
        let src2_loc = self.get_location(src2);
        let divisor_reg = match &src2_loc {
            Loc::Reg(r) if *r != Reg::Rax && *r != Reg::Rdx => *r,
            _ => {
                // Divisor is in Rax, Rdx, stack, or immediate - move to R10 first
                // This MUST happen before emit_move(src1, Rax) in case src2 is in Rax
                self.emit_move(src2, Reg::R10, size);
                Reg::R10
            }
        };

        // Now safe to move dividend to Rax
        self.emit_move(src1, Reg::Rax, size);

        // Sign-extend or zero-extend for division (this clobbers RDX)
        match insn.op {
            Opcode::DivS | Opcode::ModS => {
                if size == 64 {
                    // LIR: sign-extend RAX to RDX:RAX
                    self.push_lir(X86Inst::Cqto);
                } else {
                    // LIR: sign-extend EAX to EDX:EAX
                    self.push_lir(X86Inst::Cltd);
                }
            }
            Opcode::DivU | Opcode::ModU => {
                // LIR: zero RDX for unsigned division
                self.push_lir(X86Inst::Xor {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rdx),
                    dst: Reg::Rdx,
                });
            }
            _ => {}
        }
        // LIR: division instruction
        match insn.op {
            Opcode::DivS | Opcode::ModS => {
                self.push_lir(X86Inst::IDiv {
                    size: op_size,
                    divisor: GpOperand::Reg(divisor_reg),
                });
            }
            _ => {
                self.push_lir(X86Inst::Div {
                    size: op_size,
                    divisor: GpOperand::Reg(divisor_reg),
                });
            }
        }
        let dst_loc = self.get_location(target);
        let result_reg = match insn.op {
            Opcode::DivS | Opcode::DivU => Reg::Rax,
            Opcode::ModS | Opcode::ModU => Reg::Rdx,
            _ => Reg::Rax,
        };
        self.emit_move_to_loc(result_reg, &dst_loc, size);
    }

    fn emit_compare(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(size);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Get target location to determine work register
        let dst_loc = self.get_location(target);
        // Use target's register if available, otherwise use R11 as scratch
        // This avoids clobbering other live registers (like Rax)
        let work_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R11,
        };

        // Get src1 location to see if we need to move it
        let src1_loc = self.get_location(src1);
        let cmp_reg = match &src1_loc {
            Loc::Reg(r) => *r, // Compare directly in src1's register
            _ => {
                // src1 is not in a register, move to work_reg
                self.emit_move(src1, work_reg, size);
                work_reg
            }
        };

        let src2_loc = self.get_location(src2);
        // x86-64 cmp instruction only supports 32-bit signed immediates
        // For larger values, we need to load into a register first
        let src2_gp = match &src2_loc {
            Loc::Imm(v) if *v > i32::MAX as i64 || *v < i32::MIN as i64 => {
                // Large immediate - load into a different scratch register
                let scratch = if work_reg == Reg::R10 {
                    Reg::R11
                } else {
                    Reg::R10
                };
                self.push_lir(X86Inst::MovAbs {
                    imm: *v,
                    dst: scratch,
                });
                GpOperand::Reg(scratch)
            }
            _ => self.loc_to_gp_operand(&src2_loc),
        };

        // LIR: compare instruction
        self.push_lir(X86Inst::Cmp {
            size: op_size,
            src: src2_gp,
            dst: GpOperand::Reg(cmp_reg),
        });

        // Map opcode to condition code
        let cc = match insn.op {
            Opcode::SetEq => CondCode::Eq,
            Opcode::SetNe => CondCode::Ne,
            Opcode::SetLt => CondCode::Slt,
            Opcode::SetLe => CondCode::Sle,
            Opcode::SetGt => CondCode::Sgt,
            Opcode::SetGe => CondCode::Sge,
            Opcode::SetB => CondCode::Ult,
            Opcode::SetBe => CondCode::Ule,
            Opcode::SetA => CondCode::Ugt,
            Opcode::SetAe => CondCode::Uge,
            _ => return,
        };

        // LIR: setCC instruction - use work_reg for the result
        self.push_lir(X86Inst::SetCC { cc, dst: work_reg });
        // LIR: zero extend low byte to 32-bit
        self.push_lir(X86Inst::Movzx {
            src_size: OperandSize::B8,
            dst_size: OperandSize::B32,
            src: GpOperand::Reg(work_reg),
            dst: work_reg,
        });

        // Move to final destination if not already there
        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, 32);
        }
    }

    fn emit_unary_op(&mut self, insn: &Instruction, op: UnaryOp) {
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(size);
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        let work_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10, // Use scratch register R10
        };
        self.emit_move(src, work_reg, size);
        self.push_lir(match op {
            UnaryOp::Neg => X86Inst::Neg {
                size: op_size,
                dst: work_reg,
            },
            UnaryOp::Not => X86Inst::Not {
                size: op_size,
                dst: work_reg,
            },
        });
        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, size);
        }
    }

    // ========================================================================
    // Floating-Point Instructions (SSE)
    // ========================================================================

    /// Emit a floating-point load operation
    fn emit_fp_load(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm15,
        };
        let addr_loc = self.get_location(addr);

        let fp_size = if size <= 32 {
            FpSize::Single
        } else {
            FpSize::Double
        };

        match addr_loc {
            Loc::Reg(r) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: r,
                        offset: insn.offset as i32,
                    }),
                    dst: XmmOperand::Reg(dst_xmm),
                });
            }
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                if is_symbol {
                    // Local variable - load directly from stack slot
                    let total_offset = offset - insn.offset as i32 + self.callee_saved_offset;
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -total_offset,
                        }),
                        dst: XmmOperand::Reg(dst_xmm),
                    });
                } else {
                    // Spilled address - load address first, then load from that address
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
                            offset: insn.offset as i32,
                        }),
                        dst: XmmOperand::Reg(dst_xmm),
                    });
                }
            }
            Loc::Global(name) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::RipRelative(Symbol {
                        name: name.to_string(),
                        is_local: false,
                    })),
                    dst: XmmOperand::Reg(dst_xmm),
                });
            }
            _ => {
                // Load address into R11, then load from that address
                self.emit_move(addr, Reg::R11, 64);
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: insn.offset as i32,
                    }),
                    dst: XmmOperand::Reg(dst_xmm),
                });
            }
        }

        // If destination is not the XMM register we loaded to, move it
        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, size);
        }
    }

    /// Emit a floating-point store operation
    fn emit_fp_store(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let (addr, value) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&a), Some(&v)) => (a, v),
            _ => return,
        };
        let fp_size = if size <= 32 {
            FpSize::Single
        } else {
            FpSize::Double
        };

        // IMPORTANT: Check address location BEFORE emit_fp_move, because emit_fp_move
        // may clobber RAX when loading immediate values. If addr is in RAX, we need
        // to save it to R11 first.
        let addr_loc = self.get_location(addr);
        let addr_reg = match &addr_loc {
            Loc::Reg(Reg::Rax) => {
                // Address is in RAX - move it to R11 before emit_fp_move clobbers it
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Reg(Reg::R11),
                });
                Some(Reg::R11)
            }
            Loc::Reg(r) => Some(*r),
            _ => None,
        };

        // Move value to XMM15 (scratch register) - this may clobber RAX
        self.emit_fp_move(value, XmmReg::Xmm15, size);

        match addr_loc {
            Loc::Reg(_) => {
                // Use the saved register (R11 if it was RAX, otherwise original)
                let r = addr_reg.unwrap_or(Reg::Rax);
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: r,
                        offset: insn.offset as i32,
                    }),
                });
            }
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                if is_symbol {
                    // Local variable - store directly to stack slot
                    let total_offset = offset - insn.offset as i32 + self.callee_saved_offset;
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Reg(XmmReg::Xmm15),
                        dst: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -total_offset,
                        }),
                    });
                } else {
                    // Spilled address - load address first, then store through it
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
                        src: XmmOperand::Reg(XmmReg::Xmm15),
                        dst: XmmOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: insn.offset as i32,
                        }),
                    });
                }
            }
            Loc::Global(name) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Mem(MemAddr::RipRelative(Symbol {
                        name: name.to_string(),
                        is_local: false,
                    })),
                });
            }
            _ => {
                // Load address into R11, then store
                self.emit_move(addr, Reg::R11, 64);
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: insn.offset as i32,
                    }),
                });
            }
        }
    }

    /// Emit floating-point binary operation (addss/addsd, subss/subsd, etc.)
    fn emit_fp_binop(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_loc = self.get_location(target);
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm0, // Use XMM0 as work register
        };
        let fp_size = FpSize::from_bits(size);

        // Helper to emit the FP binop LIR instruction
        let emit_fp_binop_lir = |cg: &mut Self, src: XmmOperand, dst: XmmReg| match insn.op {
            Opcode::FAdd => cg.push_lir(X86Inst::AddFp {
                size: fp_size,
                src,
                dst,
            }),
            Opcode::FSub => cg.push_lir(X86Inst::SubFp {
                size: fp_size,
                src,
                dst,
            }),
            Opcode::FMul => cg.push_lir(X86Inst::MulFp {
                size: fp_size,
                src,
                dst,
            }),
            Opcode::FDiv => cg.push_lir(X86Inst::DivFp {
                size: fp_size,
                src,
                dst,
            }),
            _ => {}
        };

        // Move first operand to destination XMM register
        self.emit_fp_move(src1, dst_xmm, size);

        // Apply operation with second operand
        let src2_loc = self.get_location(src2);
        match src2_loc {
            Loc::Xmm(x) => {
                emit_fp_binop_lir(self, XmmOperand::Reg(x), dst_xmm);
            }
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                emit_fp_binop_lir(
                    self,
                    XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                    dst_xmm,
                );
            }
            Loc::FImm(v, _) => {
                // Load float immediate to a scratch register, then operate
                // Use XMM14 if dst is XMM15, otherwise use XMM15
                let scratch = if dst_xmm == XmmReg::Xmm15 {
                    XmmReg::Xmm14
                } else {
                    XmmReg::Xmm15
                };
                self.emit_fp_imm_to_xmm(v, scratch, size);
                emit_fp_binop_lir(self, XmmOperand::Reg(scratch), dst_xmm);
            }
            _ => {
                // Move to a scratch register first
                // Use XMM14 if dst is XMM15, otherwise use XMM15
                let scratch = if dst_xmm == XmmReg::Xmm15 {
                    XmmReg::Xmm14
                } else {
                    XmmReg::Xmm15
                };
                self.emit_fp_move(src2, scratch, size);
                emit_fp_binop_lir(self, XmmOperand::Reg(scratch), dst_xmm);
            }
        }

        // Move result to destination if not already there
        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, size);
        }
    }

    /// Emit floating-point negation
    fn emit_fp_neg(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_loc = self.get_location(target);
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm0,
        };
        let fp_size = FpSize::from_bits(size);

        // Move source to destination
        self.emit_fp_move(src, dst_xmm, size);

        // XOR with sign bit mask to negate
        // For float: 0x80000000, for double: 0x8000000000000000
        // Use a scratch register that's not dst_xmm to hold the sign mask
        let scratch_xmm = if dst_xmm == XmmReg::Xmm15 {
            XmmReg::Xmm14
        } else {
            XmmReg::Xmm15
        };
        if size <= 32 {
            // Create sign mask in scratch register: all zeros except sign bit
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B32,
                src: GpOperand::Imm(0x80000000),
                dst: GpOperand::Reg(Reg::Rax),
            });
            self.push_lir(X86Inst::MovGpXmm {
                size: OperandSize::B32,
                src: Reg::Rax,
                dst: scratch_xmm,
            });
            self.push_lir(X86Inst::XorFp {
                size: fp_size,
                src: scratch_xmm,
                dst: dst_xmm,
            });
        } else {
            self.push_lir(X86Inst::MovAbs {
                imm: 0x8000000000000000u64 as i64,
                dst: Reg::Rax,
            });
            self.push_lir(X86Inst::MovGpXmm {
                size: OperandSize::B64,
                src: Reg::Rax,
                dst: scratch_xmm,
            });
            self.push_lir(X86Inst::XorFp {
                size: fp_size,
                src: scratch_xmm,
                dst: dst_xmm,
            });
        }

        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, size);
        }
    }

    /// Emit floating-point comparison
    fn emit_fp_compare(&mut self, insn: &Instruction) {
        let size = insn.size.max(32);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let fp_size = FpSize::from_bits(size);

        // Load first operand to XMM0
        self.emit_fp_move(src1, XmmReg::Xmm0, size);

        // Compare with second operand using ucomiss/ucomisd
        let src2_loc = self.get_location(src2);
        match src2_loc {
            Loc::Xmm(x) => {
                self.push_lir(X86Inst::UComiFp {
                    size: fp_size,
                    src: XmmOperand::Reg(x),
                    dst: XmmReg::Xmm0,
                });
            }
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                self.push_lir(X86Inst::UComiFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                    dst: XmmReg::Xmm0,
                });
            }
            Loc::FImm(v, _) => {
                self.emit_fp_imm_to_xmm(v, XmmReg::Xmm15, size);
                self.push_lir(X86Inst::UComiFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmReg::Xmm0,
                });
            }
            _ => {
                self.emit_fp_move(src2, XmmReg::Xmm15, size);
                self.push_lir(X86Inst::UComiFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmReg::Xmm0,
                });
            }
        }

        // Set result based on comparison type
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::Rax,
        };

        // Use appropriate setcc instruction
        // Note: FP comparisons set flags differently - need to handle unordered (NaN) cases
        let cc = match insn.op {
            Opcode::FCmpOEq => CondCode::Eq,  // Equal (ZF=1, PF=0)
            Opcode::FCmpONe => CondCode::Ne,  // Not equal
            Opcode::FCmpOLt => CondCode::Ult, // Below (CF=1) - for ordered less than
            Opcode::FCmpOLe => CondCode::Ule, // Below or equal
            Opcode::FCmpOGt => CondCode::Ugt, // Above (CF=0, ZF=0)
            Opcode::FCmpOGe => CondCode::Uge, // Above or equal
            _ => return,
        };

        self.push_lir(X86Inst::SetCC { cc, dst: dst_reg });
        self.push_lir(X86Inst::Movzx {
            src_size: OperandSize::B8,
            dst_size: OperandSize::B32,
            src: GpOperand::Reg(dst_reg),
            dst: dst_reg,
        });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, 32);
        }
    }

    /// Emit integer to float conversion
    fn emit_int_to_float(&mut self, insn: &Instruction) {
        let dst_size = insn.size.max(32);
        let src_size = insn.src_size.max(32);
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_loc = self.get_location(target);
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm0,
        };

        // Move integer to R10 first (scratch register)
        self.emit_move(src, Reg::R10, src_size);

        // Convert using cvtsi2ss/cvtsi2sd
        let int_size = OperandSize::from_bits(src_size);
        let fp_size = FpSize::from_bits(dst_size);
        self.push_lir(X86Inst::CvtIntToFp {
            int_size,
            fp_size,
            src: GpOperand::Reg(Reg::R10),
            dst: dst_xmm,
        });

        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, dst_size);
        }
    }

    /// Emit float to integer conversion
    fn emit_float_to_int(&mut self, insn: &Instruction) {
        let dst_size = insn.size.max(32);
        let src_size = insn.src_size.max(32);
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Move float to XMM0
        self.emit_fp_move(src, XmmReg::Xmm0, src_size);

        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10, // Use scratch register R10
        };

        // Convert using cvttss2si/cvttsd2si (truncate toward zero)
        let fp_size = FpSize::from_bits(src_size);
        let int_size = OperandSize::from_bits(dst_size);
        self.push_lir(X86Inst::CvtFpToInt {
            fp_size,
            int_size,
            src: XmmOperand::Reg(XmmReg::Xmm0),
            dst: dst_reg,
        });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, dst_size);
        }
    }

    /// Emit float to float conversion (e.g., float to double)
    fn emit_float_to_float(&mut self, insn: &Instruction) {
        let dst_size = insn.size.max(32);
        let src_size = insn.src_size.max(32);
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_loc = self.get_location(target);
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm0,
        };

        // Move source to XMM0
        self.emit_fp_move(src, XmmReg::Xmm0, src_size);

        // Convert
        if src_size <= 32 && dst_size > 32 {
            // float to double: cvtss2sd
            self.push_lir(X86Inst::CvtFpFp {
                src_size: FpSize::Single,
                dst_size: FpSize::Double,
                src: XmmReg::Xmm0,
                dst: dst_xmm,
            });
        } else if src_size > 32 && dst_size <= 32 {
            // double to float: cvtsd2ss
            self.push_lir(X86Inst::CvtFpFp {
                src_size: FpSize::Double,
                dst_size: FpSize::Single,
                src: XmmReg::Xmm0,
                dst: dst_xmm,
            });
        } else {
            // Same size, just move
            if dst_xmm != XmmReg::Xmm0 {
                let fp_size = FpSize::from_bits(dst_size);
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm0),
                    dst: XmmOperand::Reg(dst_xmm),
                });
            }
        }

        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, dst_size);
        }
    }

    /// Load a floating-point constant into an XMM register
    fn emit_fp_const_load(&mut self, target: PseudoId, value: f64, size: u32) {
        let dst_loc = self.get_location(target);
        let dst_xmm = match &dst_loc {
            Loc::Xmm(x) => *x,
            _ => XmmReg::Xmm0,
        };

        self.emit_fp_imm_to_xmm(value, dst_xmm, size);

        if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
            self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, size);
        }
    }

    /// Load a float immediate value into an XMM register
    fn emit_fp_imm_to_xmm(&mut self, value: f64, xmm: XmmReg, size: u32) {
        if value == 0.0 {
            // Use xorps/xorpd to zero the register (faster)
            let fp_size = FpSize::from_bits(size);
            self.push_lir(X86Inst::XorFp {
                size: fp_size,
                src: xmm,
                dst: xmm,
            });
        } else if size <= 32 {
            // Float: load via integer register (use R10 scratch)
            let bits = (value as f32).to_bits();
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B32,
                src: GpOperand::Imm(bits as i64),
                dst: GpOperand::Reg(Reg::R10),
            });
            self.push_lir(X86Inst::MovGpXmm {
                size: OperandSize::B32,
                src: Reg::R10,
                dst: xmm,
            });
        } else {
            // Double: load via integer register (use R10 scratch)
            let bits = value.to_bits();
            self.push_lir(X86Inst::MovAbs {
                imm: bits as i64,
                dst: Reg::R10,
            });
            self.push_lir(X86Inst::MovGpXmm {
                size: OperandSize::B64,
                src: Reg::R10,
                dst: xmm,
            });
        }
    }

    /// Move a value to an XMM register
    fn emit_fp_move(&mut self, src: PseudoId, dst: XmmReg, size: u32) {
        let src_loc = self.get_location(src);
        let fp_size = FpSize::from_bits(size);

        match src_loc {
            Loc::Xmm(x) if x == dst => {
                // Already in destination
            }
            Loc::Xmm(x) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(x),
                    dst: XmmOperand::Reg(dst),
                });
            }
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                    dst: XmmOperand::Reg(dst),
                });
            }
            Loc::FImm(v, imm_size) => {
                // Use the size from the FImm, not the passed-in size
                // This ensures float constants are loaded as float, not double
                self.emit_fp_imm_to_xmm(v, dst, imm_size);
            }
            Loc::Reg(r) => {
                // Move from GP register to XMM (unusual but possible)
                self.push_lir(X86Inst::MovGpXmm {
                    size: OperandSize::B64,
                    src: r,
                    dst,
                });
            }
            Loc::Imm(v) => {
                // Integer immediate to float
                if size <= 32 {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Imm(v),
                        dst: GpOperand::Reg(Reg::Rax),
                    });
                    self.push_lir(X86Inst::MovGpXmm {
                        size: OperandSize::B32,
                        src: Reg::Rax,
                        dst,
                    });
                } else {
                    self.push_lir(X86Inst::MovAbs {
                        imm: v,
                        dst: Reg::Rax,
                    });
                    self.push_lir(X86Inst::MovGpXmm {
                        size: OperandSize::B64,
                        src: Reg::Rax,
                        dst,
                    });
                }
            }
            Loc::Global(name) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(MemAddr::RipRelative(Symbol::global(name.clone()))),
                    dst: XmmOperand::Reg(dst),
                });
            }
        }
    }

    /// Move from XMM register to a location
    fn emit_fp_move_from_xmm(&mut self, src: XmmReg, dst: &Loc, size: u32) {
        let fp_size = FpSize::from_bits(size);

        match dst {
            Loc::Xmm(x) if *x == src => {
                // Already in destination
            }
            Loc::Xmm(x) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(src),
                    dst: XmmOperand::Reg(*x),
                });
            }
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(src),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                });
            }
            Loc::Reg(r) => {
                // Move from XMM to GP register
                self.push_lir(X86Inst::MovXmmGp {
                    size: OperandSize::B64,
                    src,
                    dst: *r,
                });
            }
            _ => {}
        }
    }

    fn emit_load(&mut self, insn: &Instruction, types: &TypeTable) {
        let mem_size = insn.size;
        let reg_size = insn.size.max(32);
        let addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);

        // Check if this is an FP load
        let is_fp = insn.typ.is_some_and(|t| types.is_float(t)) || matches!(dst_loc, Loc::Xmm(_));

        if is_fp {
            self.emit_fp_load(insn);
            return;
        }

        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10, // Use scratch register R10
        };

        // Determine if we need sign or zero extension for small types
        // is_unsigned() returns true for explicitly unsigned types
        // For plain char, use target.char_signed to determine signedness
        let is_unsigned = insn.typ.is_some_and(|t| {
            if types.is_unsigned(t) {
                true
            } else if types.is_plain_char(t) {
                // Plain char: unsigned if target says char is not signed
                !self.base.target.char_signed
            } else {
                false
            }
        });

        let addr_loc = self.get_location(addr);
        match addr_loc {
            Loc::Reg(r) => {
                if mem_size <= 16 {
                    // Use sign/zero extending load
                    // LIR: use Movzx or Movsx
                    let src_size = OperandSize::from_bits(mem_size);
                    if is_unsigned {
                        self.push_lir(X86Inst::Movzx {
                            src_size,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: r,
                                offset: insn.offset as i32,
                            }),
                            dst: dst_reg,
                        });
                    } else {
                        self.push_lir(X86Inst::Movsx {
                            src_size,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: r,
                                offset: insn.offset as i32,
                            }),
                            dst: dst_reg,
                        });
                    }
                } else {
                    // 32/64-bit load
                    let op_size = OperandSize::from_bits(reg_size);
                    // LIR: simple Mov
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: r,
                            offset: insn.offset as i32,
                        }),
                        dst: GpOperand::Reg(dst_reg),
                    });
                }
            }
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                if is_symbol {
                    // Local variable - load directly from stack slot
                    let total_offset = offset - insn.offset as i32 + self.callee_saved_offset;
                    if mem_size <= 16 {
                        // LIR: sign/zero extending load from stack
                        let src_size = OperandSize::from_bits(mem_size);
                        if is_unsigned {
                            self.push_lir(X86Inst::Movzx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rbp,
                                    offset: -total_offset,
                                }),
                                dst: dst_reg,
                            });
                        } else {
                            self.push_lir(X86Inst::Movsx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rbp,
                                    offset: -total_offset,
                                }),
                                dst: dst_reg,
                            });
                        }
                    } else {
                        // LIR: regular load from stack
                        let op_size = OperandSize::from_bits(reg_size);
                        self.push_lir(X86Inst::Mov {
                            size: op_size,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::Rbp,
                                offset: -total_offset,
                            }),
                            dst: GpOperand::Reg(dst_reg),
                        });
                    }
                } else {
                    // Spilled address - load address first, then load from that address
                    let adjusted = offset + self.callee_saved_offset;
                    // LIR: load spilled address
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -adjusted,
                        }),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    if mem_size <= 16 {
                        // LIR: sign/zero extending load through R11
                        let src_size = OperandSize::from_bits(mem_size);
                        if is_unsigned {
                            self.push_lir(X86Inst::Movzx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::R11,
                                    offset: insn.offset as i32,
                                }),
                                dst: dst_reg,
                            });
                        } else {
                            self.push_lir(X86Inst::Movsx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::R11,
                                    offset: insn.offset as i32,
                                }),
                                dst: dst_reg,
                            });
                        }
                    } else {
                        // LIR: regular load through R11
                        let op_size = OperandSize::from_bits(reg_size);
                        self.push_lir(X86Inst::Mov {
                            size: op_size,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::R11,
                                offset: insn.offset as i32,
                            }),
                            dst: GpOperand::Reg(dst_reg),
                        });
                    }
                }
            }
            Loc::Global(name) => {
                if mem_size <= 16 {
                    // LIR: sign/zero extending load from global
                    let src_size = OperandSize::from_bits(mem_size);
                    if is_unsigned {
                        self.push_lir(X86Inst::Movzx {
                            src_size,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Mem(MemAddr::RipRelative(Symbol::global(name.clone()))),
                            dst: dst_reg,
                        });
                    } else {
                        self.push_lir(X86Inst::Movsx {
                            src_size,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Mem(MemAddr::RipRelative(Symbol::global(name.clone()))),
                            dst: dst_reg,
                        });
                    }
                } else {
                    // LIR: regular load from global
                    let op_size = OperandSize::from_bits(reg_size);
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Mem(MemAddr::RipRelative(Symbol::global(name.clone()))),
                        dst: GpOperand::Reg(dst_reg),
                    });
                }
            }
            _ => {
                self.emit_move(addr, Reg::R11, 64);
                if mem_size <= 16 {
                    // LIR: sign/zero extending load through R11
                    let src_size = OperandSize::from_bits(mem_size);
                    if is_unsigned {
                        self.push_lir(X86Inst::Movzx {
                            src_size,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::R11,
                                offset: insn.offset as i32,
                            }),
                            dst: dst_reg,
                        });
                    } else {
                        self.push_lir(X86Inst::Movsx {
                            src_size,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::R11,
                                offset: insn.offset as i32,
                            }),
                            dst: dst_reg,
                        });
                    }
                } else {
                    // LIR: regular load through R11
                    let op_size = OperandSize::from_bits(reg_size);
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: insn.offset as i32,
                        }),
                        dst: GpOperand::Reg(dst_reg),
                    });
                }
            }
        }
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, reg_size);
        }
    }

    fn emit_store(&mut self, insn: &Instruction, types: &TypeTable) {
        // Use actual size for memory stores (8, 16, 32, 64 bits)
        // This is critical for char/short types that need byte/word stores
        let mem_size = insn.size;
        // Register operations use minimum 32-bit
        let reg_size = insn.size.max(32);

        let (addr, value) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&a), Some(&v)) => (a, v),
            _ => return,
        };

        // Check if this is an FP store
        let value_loc = self.get_location(value);
        let is_fp = insn.typ.is_some_and(|t| types.is_float(t))
            || matches!(value_loc, Loc::Xmm(_) | Loc::FImm(..));

        if is_fp {
            self.emit_fp_store(insn);
            return;
        }

        // For struct stores (size > 64), we need to copy multiple words
        // The value is a symbol containing the struct data
        if mem_size > 64 {
            self.emit_struct_store(insn, addr, value);
            return;
        }

        // Get the value to a register - use its existing register if available,
        // otherwise use R10 as scratch (avoiding Rax which may hold other live values)
        let value_reg = match &value_loc {
            Loc::Reg(r) => *r,
            _ => {
                self.emit_move(value, Reg::R10, reg_size);
                Reg::R10
            }
        };

        let addr_loc = self.get_location(addr);
        match addr_loc {
            Loc::Reg(r) => {
                let op_size = OperandSize::from_bits(mem_size);
                // LIR: store through register
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(value_reg),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: r,
                        offset: insn.offset as i32,
                    }),
                });
            }
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                let op_size = OperandSize::from_bits(mem_size);
                if is_symbol {
                    // Local variable - store directly to stack slot
                    let total_offset = offset - insn.offset as i32 + self.callee_saved_offset;
                    // LIR: store to stack slot
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Reg(value_reg),
                        dst: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -total_offset,
                        }),
                    });
                } else {
                    // Spilled address - load address first, then store through it
                    let adjusted = offset + self.callee_saved_offset;
                    // LIR: load spilled address
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -adjusted,
                        }),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    // LIR: store through loaded address
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Reg(value_reg),
                        dst: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: insn.offset as i32,
                        }),
                    });
                }
            }
            Loc::Global(name) => {
                let op_size = OperandSize::from_bits(mem_size);
                // LIR: store to global via RIP-relative
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(value_reg),
                    dst: GpOperand::Mem(MemAddr::RipRelative(Symbol::global(name.clone()))),
                });
            }
            _ => {
                self.emit_move(addr, Reg::R11, 64);
                let op_size = OperandSize::from_bits(mem_size);
                // LIR: store through R11
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(value_reg),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: insn.offset as i32,
                    }),
                });
            }
        }
    }

    /// Emit a struct copy (store of size > 64 bits)
    /// The value is a symbol containing the source struct data
    fn emit_struct_store(&mut self, insn: &Instruction, addr: PseudoId, value: PseudoId) {
        let struct_size = insn.size; // Size in bits
        let num_qwords = struct_size.div_ceil(64);

        // Get source address (where the struct data is)
        let value_loc = self.get_location(value);
        // Get destination address
        let addr_loc = self.get_location(addr);

        // Load source address into R10
        match value_loc {
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                // LIR: lea for source address
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    },
                    dst: Reg::R10,
                });
            }
            Loc::Reg(r) => {
                if r != Reg::R10 {
                    // LIR: mov for source address
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: GpOperand::Reg(Reg::R10),
                    });
                }
            }
            Loc::Global(ref name) => {
                // LIR: lea for global source address
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::RipRelative(Symbol::global(name.clone())),
                    dst: Reg::R10,
                });
            }
            _ => return,
        }

        // Load destination address into R11
        match addr_loc {
            Loc::Stack(offset) => {
                let adjusted = offset - insn.offset as i32 + self.callee_saved_offset;
                // LIR: lea for destination address
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    },
                    dst: Reg::R11,
                });
            }
            Loc::Reg(r) => {
                if insn.offset != 0 {
                    // LIR: lea with offset
                    self.push_lir(X86Inst::Lea {
                        addr: MemAddr::BaseOffset {
                            base: r,
                            offset: insn.offset as i32,
                        },
                        dst: Reg::R11,
                    });
                } else if r != Reg::R11 {
                    // LIR: mov for destination address
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                }
            }
            Loc::Global(ref name) => {
                // LIR: lea for global destination address
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::RipRelative(Symbol::global(name.clone())),
                    dst: Reg::R11,
                });
            }
            _ => return,
        }

        // Copy qword by qword
        for i in 0..num_qwords {
            let byte_offset = (i * 8) as i32;
            // LIR: load from source
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::R10,
                    offset: byte_offset,
                }),
                dst: GpOperand::Reg(Reg::Rax),
            });
            // LIR: store to destination
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::Rax),
                dst: GpOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: byte_offset,
                }),
            });
        }
    }

    fn emit_call(&mut self, insn: &Instruction, types: &TypeTable) {
        // Check if this is an indirect call (through function pointer)
        let is_indirect = insn.indirect_target.is_some();

        // For direct calls, we need a function name
        let func_name = if is_indirect {
            "<indirect>".to_string()
        } else {
            match &insn.func_name {
                Some(n) => n.clone(),
                None => return,
            }
        };

        // For indirect calls, load function pointer address into R11 before argument setup
        // R11 is caller-saved and not used for arguments in System V AMD64 ABI
        if let Some(func_addr) = insn.indirect_target {
            self.emit_move(func_addr, Reg::R11, 64);
        }

        // System V AMD64 ABI:
        // - Integer arguments: RDI, RSI, RDX, RCX, R8, R9 (6 registers)
        // - Floating-point arguments: XMM0-XMM7 (8 registers)
        // - Each class has its own counter (int and FP are independent)
        // - For variadic functions: float args go in XMM registers (printf needs this),
        //   but variadic integer args go on stack (our va_arg impl reads from stack)
        // - AL must be set to the number of XMM registers used for variadic calls
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = XmmReg::arg_regs();
        let mut int_arg_idx = 0;
        let mut fp_arg_idx = 0;
        let mut stack_args = 0;

        let variadic_start = insn.variadic_arg_start.unwrap_or(usize::MAX);

        // First pass: determine which args go on stack
        // For variadic calls: variadic INTEGER args go on stack, variadic FLOAT args use XMM
        // For non-variadic calls: overflow args go on stack
        let mut stack_arg_indices = Vec::new();
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

            // Note: We pass all args (fixed and variadic) the same way per SysV ABI
            // Variadic args start at index variadic_start, but use same register sequence
            let _is_variadic_arg = i >= variadic_start;

            if is_complex {
                // Complex uses TWO consecutive XMM registers
                if temp_fp_idx + 1 >= fp_arg_regs.len() {
                    stack_arg_indices.push(i);
                }
                temp_fp_idx += 2;
            } else if is_fp {
                // Float args go in XMM registers (all args, including variadic)
                if temp_fp_idx >= fp_arg_regs.len() {
                    stack_arg_indices.push(i);
                }
                temp_fp_idx += 1;
            } else {
                // Integer args go in GP registers (all args, including variadic)
                // per x86-64 SysV ABI
                if temp_int_idx >= int_arg_regs.len() {
                    stack_arg_indices.push(i);
                }
                temp_int_idx += 1;
            }
        }

        // Ensure 16-byte stack alignment before call
        // If we're pushing an odd number of 8-byte args, add padding
        let needs_padding = stack_arg_indices.len() % 2 == 1;
        if needs_padding {
            self.push_lir(X86Inst::Sub {
                size: OperandSize::B64,
                src: GpOperand::Imm(8),
                dst: Reg::Rsp,
            });
        }

        // Push stack arguments in reverse order
        for &i in stack_arg_indices.iter().rev() {
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

        // Pre-pass: Save argument values that are in argument registers to avoid clobbering.
        // If arg[j]'s source is in an argument register that will be written by arg[k] where k < j,
        // we need to save arg[j]'s value before we start setting up arguments.
        // Use a HashMap to track saved locations: original_reg -> scratch_reg
        let mut saved_arg_regs: std::collections::HashMap<Reg, Reg> =
            std::collections::HashMap::new();
        let scratch_regs = [Reg::R10, Reg::R11]; // Available scratch registers
        let mut scratch_idx = 0;

        // Collect which argument registers we'll write to (in order)
        let mut regs_to_write: Vec<Reg> = Vec::new();
        let mut temp_int_idx2 = 0;
        for i in 0..insn.src.len() {
            if stack_arg_indices.contains(&i) {
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
                if temp_int_idx2 < int_arg_regs.len() {
                    regs_to_write.push(int_arg_regs[temp_int_idx2]);
                }
                temp_int_idx2 += 1;
            }
        }

        // Now check which argument sources are in registers that will be clobbered
        temp_int_idx2 = 0;
        for i in 0..insn.src.len() {
            if stack_arg_indices.contains(&i) {
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

            if !is_fp && !is_complex && temp_int_idx2 < int_arg_regs.len() {
                let arg_loc = self.get_location(arg);
                if let Loc::Reg(src_reg) = arg_loc {
                    // Check if this source register will be written by an earlier argument
                    let my_dest_idx = temp_int_idx2;
                    for (write_idx, &write_reg) in regs_to_write.iter().enumerate() {
                        if write_idx < my_dest_idx && write_reg == src_reg {
                            // This source will be clobbered! Save it if not already saved
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
                temp_int_idx2 += 1;
            }
        }

        // Now handle register arguments (with saved values)
        for i in 0..insn.src.len() {
            // Skip args that went to stack
            if stack_arg_indices.contains(&i) {
                continue;
            }
            let arg = insn.src[i];
            // Get argument type if available, otherwise fall back to location-based detection
            let arg_type = insn.arg_types.get(i).copied();
            let is_complex = arg_type.is_some_and(|t| types.is_complex(t));
            let is_fp = if let Some(typ) = arg_type {
                types.is_float(typ)
            } else {
                // Fall back to location-based detection for backwards compatibility
                let arg_loc = self.get_location(arg);
                matches!(arg_loc, Loc::Xmm(_) | Loc::FImm(..))
            };

            // Get argument size from type, with minimum 32-bit for register ops
            let arg_size = if let Some(typ) = arg_type {
                types.size_bits(typ).max(32)
            } else {
                64 // Default for backwards compatibility
            };

            if is_complex {
                // Complex type: load both parts and put in 2 consecutive XMM registers
                // The arg pseudo contains the ADDRESS of the complex value
                let arg_loc = self.get_location(arg);
                let (fp_size, imag_offset) = complex_fp_info(types, arg_type.unwrap());
                match arg_loc {
                    Loc::Stack(offset) => {
                        // Stack slot contains the address of the complex value
                        // First load the address into a temp register
                        let adjusted = offset + self.callee_saved_offset;
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::B64,
                            src: GpOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::Rbp,
                                offset: -adjusted,
                            }),
                            dst: GpOperand::Reg(Reg::R11),
                        });
                        // Load real part from the complex value
                        self.push_lir(X86Inst::MovFp {
                            size: fp_size,
                            src: XmmOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::R11,
                                offset: 0,
                            }),
                            dst: XmmOperand::Reg(fp_arg_regs[fp_arg_idx]),
                        });
                        // Load imag part from the complex value
                        self.push_lir(X86Inst::MovFp {
                            size: fp_size,
                            src: XmmOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::R11,
                                offset: imag_offset,
                            }),
                            dst: XmmOperand::Reg(fp_arg_regs[fp_arg_idx + 1]),
                        });
                    }
                    Loc::Reg(r) => {
                        // Address in register - load through it
                        self.push_lir(X86Inst::MovFp {
                            size: fp_size,
                            src: XmmOperand::Mem(MemAddr::BaseOffset { base: r, offset: 0 }),
                            dst: XmmOperand::Reg(fp_arg_regs[fp_arg_idx]),
                        });
                        self.push_lir(X86Inst::MovFp {
                            size: fp_size,
                            src: XmmOperand::Mem(MemAddr::BaseOffset {
                                base: r,
                                offset: imag_offset,
                            }),
                            dst: XmmOperand::Reg(fp_arg_regs[fp_arg_idx + 1]),
                        });
                    }
                    _ => {}
                }
                fp_arg_idx += 2;
            } else if is_fp {
                // FP size from type (32 for float, 64 for double)
                let fp_size = if let Some(typ) = arg_type {
                    types.size_bits(typ)
                } else {
                    64
                };
                // Float args go in XMM registers
                self.emit_fp_move(arg, fp_arg_regs[fp_arg_idx], fp_size);
                fp_arg_idx += 1;
            } else {
                // Integer args go in GP registers
                // Check if this arg's source was saved to a scratch register
                let arg_loc = self.get_location(arg);
                if let Loc::Reg(src_reg) = arg_loc {
                    if let Some(&saved_reg) = saved_arg_regs.get(&src_reg) {
                        // Use the saved register instead of the original (which may be clobbered)
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::from_bits(arg_size),
                            src: GpOperand::Reg(saved_reg),
                            dst: GpOperand::Reg(int_arg_regs[int_arg_idx]),
                        });
                    } else {
                        self.emit_move(arg, int_arg_regs[int_arg_idx], arg_size);
                    }
                } else {
                    self.emit_move(arg, int_arg_regs[int_arg_idx], arg_size);
                }
                int_arg_idx += 1;
            }
        }

        // For variadic function calls, set AL to the number of XMM registers used
        // This is required by the System V AMD64 ABI for variadic functions
        if insn.variadic_arg_start.is_some() {
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B8,
                src: GpOperand::Imm(fp_arg_idx as i64),
                dst: GpOperand::Reg(Reg::Rax),
            });
        }

        // Emit the call
        if is_indirect {
            // Indirect call through R11 (function pointer was loaded there earlier)
            self.push_lir(X86Inst::Call {
                target: CallTarget::Indirect(Reg::R11),
            });
        } else {
            // Direct call to named function
            self.push_lir(X86Inst::Call {
                target: CallTarget::Direct(Symbol::global(func_name.clone())),
            });
        }

        // Clean up stack arguments (including padding if any)
        let stack_cleanup = stack_args * 8 + if needs_padding { 8 } else { 0 };
        if stack_cleanup > 0 {
            self.push_lir(X86Inst::Add {
                size: OperandSize::B64,
                src: GpOperand::Imm(stack_cleanup as i64),
                dst: Reg::Rsp,
            });
        }

        // Handle return value
        if let Some(target) = insn.target {
            let dst_loc = self.get_location(target);
            // Check if return value is complex or floating-point
            let is_complex_result = insn.typ.is_some_and(|t| types.is_complex(t));
            let is_fp_result = if let Some(typ) = insn.typ {
                types.is_float(typ)
            } else {
                matches!(dst_loc, Loc::Xmm(_) | Loc::FImm(..))
            };

            // Get return value size from type
            let ret_size = insn.size.max(32);

            if insn.is_two_reg_return {
                // Two-register struct return: RAX has low 8 bytes, RDX has high 8 bytes
                // Store both to the target location (which must be a stack slot)
                match dst_loc {
                    Loc::Stack(offset) => {
                        let adjusted = offset + self.callee_saved_offset;
                        // Store RAX (low 8 bytes)
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::B64,
                            src: GpOperand::Reg(Reg::Rax),
                            dst: GpOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::Rbp,
                                offset: -adjusted,
                            }),
                        });
                        // Store RDX (high 8 bytes)
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
                        // Address in register - store through it
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::B64,
                            src: GpOperand::Reg(Reg::Rax),
                            dst: GpOperand::Mem(MemAddr::BaseOffset { base: r, offset: 0 }),
                        });
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::B64,
                            src: GpOperand::Reg(Reg::Rdx),
                            dst: GpOperand::Mem(MemAddr::BaseOffset { base: r, offset: 8 }),
                        });
                    }
                    _ => {}
                }
            } else if is_complex_result {
                // Complex return value is in XMM0 (real) + XMM1 (imag)
                // Store both parts to the target location
                let (fp_size, imag_offset) = complex_fp_info(types, insn.typ.unwrap());
                match dst_loc {
                    Loc::Stack(offset) => {
                        let adjusted = offset + self.callee_saved_offset;
                        // Store real part from XMM0
                        self.push_lir(X86Inst::MovFp {
                            size: fp_size,
                            src: XmmOperand::Reg(XmmReg::Xmm0),
                            dst: XmmOperand::Mem(MemAddr::BaseOffset {
                                base: Reg::Rbp,
                                offset: -adjusted,
                            }),
                        });
                        // Store imag part from XMM1
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
                        // Address in register - store through it
                        self.push_lir(X86Inst::MovFp {
                            size: fp_size,
                            src: XmmOperand::Reg(XmmReg::Xmm0),
                            dst: XmmOperand::Mem(MemAddr::BaseOffset { base: r, offset: 0 }),
                        });
                        self.push_lir(X86Inst::MovFp {
                            size: fp_size,
                            src: XmmOperand::Reg(XmmReg::Xmm1),
                            dst: XmmOperand::Mem(MemAddr::BaseOffset {
                                base: r,
                                offset: imag_offset,
                            }),
                        });
                    }
                    _ => {}
                }
            } else if is_fp_result {
                // FP return value is in XMM0
                self.emit_fp_move_from_xmm(XmmReg::Xmm0, &dst_loc, ret_size);
            } else {
                // Integer return value is in RAX
                self.emit_move_to_loc(Reg::Rax, &dst_loc, ret_size);
            }
        }
    }

    fn emit_select(&mut self, insn: &Instruction) {
        let (cond, then_val, else_val) = match (insn.src.first(), insn.src.get(1), insn.src.get(2))
        {
            (Some(&c), Some(&t), Some(&e)) => (c, t, e),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(size);
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10, // Use scratch register R10
        };
        self.emit_move(else_val, dst_reg, size);
        let cond_loc = self.get_location(cond);
        match &cond_loc {
            Loc::Reg(r) => {
                // LIR: test register with itself
                self.push_lir(X86Inst::Test {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(*r),
                    dst: GpOperand::Reg(*r),
                });
            }
            Loc::Imm(v) => {
                if *v != 0 {
                    self.emit_move(then_val, dst_reg, size);
                    if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
                        self.emit_move_to_loc(dst_reg, &dst_loc, size);
                    }
                    return;
                }
                if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
                    self.emit_move_to_loc(dst_reg, &dst_loc, size);
                }
                return;
            }
            _ => {
                self.emit_move(cond, Reg::R11, 64);
                // LIR: test R11 with itself
                self.push_lir(X86Inst::Test {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::R11),
                    dst: GpOperand::Reg(Reg::R11),
                });
            }
        }
        self.emit_move(then_val, Reg::R10, size);
        // LIR: conditional move if not equal (non-zero)
        self.push_lir(X86Inst::CMov {
            cc: CondCode::Ne,
            size: op_size,
            src: GpOperand::Reg(Reg::R10),
            dst: dst_reg,
        });
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size);
        }
    }

    fn emit_extend(&mut self, insn: &Instruction) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10, // Use scratch register R10
        };
        match insn.op {
            Opcode::Zext => {
                self.emit_move(src, dst_reg, insn.size);
            }
            Opcode::Sext => {
                // Move source at its original size, then sign-extend
                self.emit_move(src, dst_reg, insn.src_size.max(32));
                match insn.src_size {
                    8 => {
                        // LIR: sign-extend byte to qword
                        self.push_lir(X86Inst::Movsx {
                            src_size: OperandSize::B8,
                            dst_size: OperandSize::B64,
                            src: GpOperand::Reg(dst_reg),
                            dst: dst_reg,
                        });
                    }
                    16 => {
                        // LIR: sign-extend word to qword
                        self.push_lir(X86Inst::Movsx {
                            src_size: OperandSize::B16,
                            dst_size: OperandSize::B64,
                            src: GpOperand::Reg(dst_reg),
                            dst: dst_reg,
                        });
                    }
                    32 => {
                        // LIR: sign-extend dword to qword
                        self.push_lir(X86Inst::Movsx {
                            src_size: OperandSize::B32,
                            dst_size: OperandSize::B64,
                            src: GpOperand::Reg(dst_reg),
                            dst: dst_reg,
                        });
                    }
                    _ => {}
                }
            }
            Opcode::Trunc => {
                // Move source value to register, then mask to target size
                self.emit_move(src, dst_reg, 64);
                // Truncate by masking to the target size
                match insn.size {
                    8 => {
                        // LIR: zero-extend byte to dword (masks to 8 bits)
                        self.push_lir(X86Inst::Movzx {
                            src_size: OperandSize::B8,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Reg(dst_reg),
                            dst: dst_reg,
                        });
                    }
                    16 => {
                        // LIR: zero-extend word to dword (masks to 16 bits)
                        self.push_lir(X86Inst::Movzx {
                            src_size: OperandSize::B16,
                            dst_size: OperandSize::B32,
                            src: GpOperand::Reg(dst_reg),
                            dst: dst_reg,
                        });
                    }
                    32 => {
                        // LIR: mov dword to self (32-bit ops zero upper 32 bits)
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::B32,
                            src: GpOperand::Reg(dst_reg),
                            dst: GpOperand::Reg(dst_reg),
                        });
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, insn.size);
        }
    }

    fn emit_copy_with_type(
        &mut self,
        src: PseudoId,
        dst: PseudoId,
        size: u32,
        typ: Option<TypeId>,
        types: &TypeTable,
    ) {
        // Keep actual size for handling narrow types
        let actual_size = size;
        let reg_size = size.max(32);
        let dst_loc = self.get_location(dst);
        let src_loc = self.get_location(src);

        // Check if this is a FP copy (source or dest is in XMM or is FImm)
        let is_fp_copy =
            matches!(&src_loc, Loc::Xmm(_) | Loc::FImm(..)) || matches!(&dst_loc, Loc::Xmm(_));

        // Determine if the type is unsigned (for proper sign/zero extension)
        // For plain char, use target.char_signed to determine signedness
        let is_unsigned = typ.is_some_and(|t| {
            if types.is_unsigned(t) {
                true
            } else if types.is_plain_char(t) {
                // Plain char: unsigned if target says char is not signed
                !self.base.target.char_signed
            } else {
                false
            }
        });

        if is_fp_copy {
            // Handle FP copy
            let dst_xmm = match &dst_loc {
                Loc::Xmm(x) => *x,
                _ => XmmReg::Xmm0,
            };

            self.emit_fp_move(src, dst_xmm, reg_size);

            if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
                self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, reg_size);
            }
        } else {
            // Integer copy
            match &dst_loc {
                Loc::Reg(r) => {
                    self.emit_move(src, *r, reg_size);
                    // For narrow types (8 or 16 bits), truncate to correct width
                    // Unsigned: zero-extend (AND mask)
                    // Signed: sign-extend (shift left then arithmetic shift right)
                    if actual_size == 8 {
                        if is_unsigned {
                            // LIR: zero-extend with AND mask
                            self.push_lir(X86Inst::And {
                                size: OperandSize::B32,
                                src: GpOperand::Imm(0xFF),
                                dst: *r,
                            });
                        } else {
                            // Sign-extend: shift left 24 bits then arithmetic shift right 24 bits
                            // LIR: shift left
                            self.push_lir(X86Inst::Shl {
                                size: OperandSize::B32,
                                count: ShiftCount::Imm(24),
                                dst: *r,
                            });
                            // LIR: arithmetic shift right
                            self.push_lir(X86Inst::Sar {
                                size: OperandSize::B32,
                                count: ShiftCount::Imm(24),
                                dst: *r,
                            });
                        }
                    } else if actual_size == 16 {
                        if is_unsigned {
                            // LIR: zero-extend with AND mask
                            self.push_lir(X86Inst::And {
                                size: OperandSize::B32,
                                src: GpOperand::Imm(0xFFFF),
                                dst: *r,
                            });
                        } else {
                            // Sign-extend: shift left 16 bits then arithmetic shift right 16 bits
                            // LIR: shift left
                            self.push_lir(X86Inst::Shl {
                                size: OperandSize::B32,
                                count: ShiftCount::Imm(16),
                                dst: *r,
                            });
                            // LIR: arithmetic shift right
                            self.push_lir(X86Inst::Sar {
                                size: OperandSize::B32,
                                count: ShiftCount::Imm(16),
                                dst: *r,
                            });
                        }
                    }
                }
                Loc::Stack(_) => {
                    self.emit_move(src, Reg::R10, reg_size);
                    // For narrow types stored to stack, use the actual size
                    if actual_size <= 16 {
                        self.emit_move_to_loc(Reg::R10, &dst_loc, actual_size);
                    } else {
                        self.emit_move_to_loc(Reg::R10, &dst_loc, reg_size);
                    }
                }
                _ => {}
            }
        }
    }

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
    fn emit_va_start(&mut self, insn: &Instruction) {
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
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: offset + 4,
                    }),
                });
                // overflow_arg_area = rbp + 16
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: 16,
                    },
                    dst: Reg::Rax,
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: offset + 8,
                    }),
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
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: offset + 16,
                    }),
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
                // overflow_arg_area = rbp + 16
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: 16,
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

    /// Emit va_arg: Get the next variadic argument
    /// Helper for emit_va_arg: stores float value from XMM15 to destination
    fn emit_va_arg_store_float(&mut self, dst_loc: &Loc, fp_size: FpSize) {
        match dst_loc {
            Loc::Xmm(x) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Reg(*x),
                });
            }
            Loc::Stack(dst_offset) => {
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Reg(XmmReg::Xmm15),
                    dst: XmmOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *dst_offset,
                    }),
                });
            }
            _ => {}
        }
    }

    /// Helper for emit_va_arg: stores int value to destination
    fn emit_va_arg_store_int(&mut self, dst_loc: &Loc, src_reg: Reg, arg_size: OperandSize) {
        match dst_loc {
            Loc::Reg(r) => {
                self.push_lir(X86Inst::Mov {
                    size: arg_size,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: src_reg,
                        offset: 0,
                    }),
                    dst: GpOperand::Reg(*r),
                });
            }
            Loc::Stack(dst_offset) => {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: src_reg,
                        offset: 0,
                    }),
                    dst: GpOperand::Reg(Reg::R11),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::R11),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *dst_offset,
                    }),
                });
            }
            _ => {}
        }
    }

    /// Helper for emit_va_arg: emit float path for va_arg
    /// ap_base: base register for va_list access
    /// ap_base_offset: added to all va_list field offsets (0 for Reg, ap_offset for Stack)
    fn emit_va_arg_float(
        &mut self,
        ap_base: Reg,
        ap_base_offset: i32,
        dst_loc: &Loc,
        arg_type: TypeId,
        label_suffix: u32,
        types: &TypeTable,
    ) {
        let overflow_label = Label::new("va_fp_overflow", label_suffix);
        let done_label = Label::new("va_fp_done", label_suffix);

        let fp_size = types.size_bits(arg_type);
        let lir_fp_size = if fp_size <= 32 {
            FpSize::Single
        } else {
            FpSize::Double
        };

        // Load fp_offset from va_list (at offset 4)
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 4,
            }),
            dst: GpOperand::Reg(Reg::Rax),
        });
        // Compare with 176 (end of XMM save area)
        self.push_lir(X86Inst::Cmp {
            size: OperandSize::B32,
            src: GpOperand::Imm(176),
            dst: GpOperand::Reg(Reg::Rax),
        });
        // Jump if above or equal (fp_offset >= 176 means use overflow)
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Uge,
            target: overflow_label.clone(),
        });

        // Register save area path: load from reg_save_area + fp_offset
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 16,
            }),
            dst: GpOperand::Reg(Reg::R10),
        });
        self.push_lir(X86Inst::Movsx {
            src_size: OperandSize::B32,
            dst_size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rax),
            dst: Reg::R11,
        });
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst: Reg::R10,
        });

        // Load float value from save area
        self.push_lir(X86Inst::MovFp {
            size: lir_fp_size,
            src: XmmOperand::Mem(MemAddr::BaseOffset {
                base: Reg::R10,
                offset: 0,
            }),
            dst: XmmOperand::Reg(XmmReg::Xmm15),
        });
        self.emit_va_arg_store_float(dst_loc, lir_fp_size);

        // Increment fp_offset by 16 (XMM slot size)
        self.push_lir(X86Inst::Add {
            size: OperandSize::B32,
            src: GpOperand::Imm(16),
            dst: Reg::Rax,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Reg(Reg::Rax),
            dst: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 4,
            }),
        });
        self.push_lir(X86Inst::Jmp {
            target: done_label.clone(),
        });

        // Overflow path: use overflow_arg_area
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(overflow_label)));
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 8,
            }),
            dst: GpOperand::Reg(Reg::Rax),
        });

        self.push_lir(X86Inst::MovFp {
            size: lir_fp_size,
            src: XmmOperand::Mem(MemAddr::BaseOffset {
                base: Reg::Rax,
                offset: 0,
            }),
            dst: XmmOperand::Reg(XmmReg::Xmm15),
        });
        self.emit_va_arg_store_float(dst_loc, lir_fp_size);

        // Advance overflow_arg_area by 8
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Imm(8),
            dst: Reg::Rax,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rax),
            dst: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 8,
            }),
        });

        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
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

        self.emit_va_arg_store_int(dst_loc, Reg::Rax, lir_arg_size);

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
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(overflow_label)));
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 8,
            }),
            dst: GpOperand::Reg(Reg::Rax),
        });

        self.emit_va_arg_store_int(dst_loc, Reg::Rax, lir_arg_size);

        // Advance overflow_arg_area
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Imm(arg_bytes as i64),
            dst: Reg::Rax,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rax),
            dst: GpOperand::Mem(MemAddr::BaseOffset {
                base: ap_base,
                offset: ap_base_offset + 8,
            }),
        });

        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
    }

    fn emit_va_arg(&mut self, insn: &Instruction, types: &TypeTable) {
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

        match &ap_loc {
            Loc::Stack(ap_offset) => {
                if types.is_float(arg_type) {
                    self.emit_va_arg_float(
                        Reg::Rbp,
                        *ap_offset,
                        &dst_loc,
                        arg_type,
                        label_suffix,
                        types,
                    );
                } else {
                    self.emit_va_arg_int(
                        Reg::Rbp,
                        *ap_offset,
                        &dst_loc,
                        arg_size,
                        arg_bytes,
                        label_suffix,
                    );
                }
            }
            Loc::Reg(ap_reg) => {
                if types.is_float(arg_type) {
                    self.emit_va_arg_float(*ap_reg, 0, &dst_loc, arg_type, label_suffix, types);
                } else {
                    self.emit_va_arg_int(*ap_reg, 0, &dst_loc, arg_size, arg_bytes, label_suffix);
                }
            }
            _ => {}
        }
    }

    /// Emit va_copy: Copy a va_list (24 bytes)
    fn emit_va_copy(&mut self, insn: &Instruction) {
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
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *src_off,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *dst_off,
                    }),
                });
                // Copy fp_offset (4 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 4,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 4,
                    }),
                });
                // Copy overflow_arg_area (8 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 8,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 8,
                    }),
                });
                // Copy reg_save_area (8 bytes)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 16,
                    }),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 16,
                    }),
                });
            }
            (Loc::Reg(src_reg), Loc::Reg(dst_reg)) => {
                // Both src and dest are in registers (containing addresses)
                // Copy gp_offset (4 bytes)
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
                // Copy overflow_arg_area (8 bytes)
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
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *dst_off,
                    }),
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
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 4,
                    }),
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
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 8,
                    }),
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
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: dst_off + 16,
                    }),
                });
            }
            (Loc::Stack(src_off), Loc::Reg(dst_reg)) => {
                // Src on stack, dest in register
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *src_off,
                    }),
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
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 4,
                    }),
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
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 8,
                    }),
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
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: src_off + 16,
                    }),
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
    fn emit_bswap(&mut self, insn: &Instruction, swap_size: BswapSize) {
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
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *off,
                    }),
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
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *off,
                    }),
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
                    src: GpOperand::Imm(*v),
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
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *off,
                    }),
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
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *off,
                    }),
                });
            }
            _ => {}
        }
    }

    /// Emit count trailing zeros
    fn emit_ctz(&mut self, insn: &Instruction, src_size: OperandSize) {
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
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                    dst: Reg::R10,
                });
            }
            Loc::Imm(v) => {
                // Load immediate first, then BSF
                self.push_lir(X86Inst::Mov {
                    size: src_size,
                    src: GpOperand::Imm(v),
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
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                });
            }
            _ => {}
        }
    }

    /// Emit count leading zeros: CLZ(x) = operand_bits - 1 - BSR(x)
    fn emit_clz(&mut self, insn: &Instruction, src_size: OperandSize) {
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
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                    dst: Reg::R10,
                });
            }
            Loc::Imm(v) => {
                // Load immediate first, then BSR
                self.push_lir(X86Inst::Mov {
                    size: src_size,
                    src: GpOperand::Imm(v),
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
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                });
            }
            _ => {}
        }
    }

    /// Emit population count
    fn emit_popcount(&mut self, insn: &Instruction, src_size: OperandSize) {
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
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                    dst: Reg::R10,
                });
            }
            Loc::Imm(v) => {
                // Load immediate first, then POPCNT
                self.push_lir(X86Inst::Mov {
                    size: src_size,
                    src: GpOperand::Imm(v),
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
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: off,
                    }),
                });
            }
            _ => {}
        }
    }

    /// Emit setjmp(env) - saves execution context
    /// System V AMD64 ABI: env in RDI, returns int in EAX
    fn emit_setjmp(&mut self, insn: &Instruction) {
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
        self.emit_move_to_loc(Reg::Rax, &dst_loc, 32);
    }

    /// Emit longjmp(env, val) - restores execution context (noreturn)
    /// System V AMD64 ABI: env in RDI, val in RSI
    fn emit_longjmp(&mut self, insn: &Instruction) {
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
    fn emit_alloca(&mut self, insn: &Instruction) {
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

    // ========================================================================
    // Inline Assembly Support
    // ========================================================================

    /// Emit inline assembly
    fn emit_inline_asm(&mut self, insn: &Instruction) {
        let asm_data = match &insn.asm_data {
            Some(data) => data.as_ref(),
            None => return,
        };

        // First pass: collect all specific register constraints to build reserved set
        // This prevents non-specific operands from using registers needed by specific constraints
        let mut reserved_regs: std::collections::HashSet<Reg> = std::collections::HashSet::new();
        for output in &asm_data.outputs {
            if let Some(r) = Self::constraint_to_specific_reg(&output.constraint) {
                reserved_regs.insert(r);
            }
        }
        for input in &asm_data.inputs {
            if let Some(r) = Self::constraint_to_specific_reg(&input.constraint) {
                reserved_regs.insert(r);
            }
        }

        // Build operand strings for asm substitution
        // For constraints requiring specific registers (a,b,c,d,S,D), we use those registers
        // and emit mov instructions to/from the actual locations.
        let mut operand_regs: Vec<Option<Reg>> = Vec::new();
        let mut operand_mem: Vec<Option<String>> = Vec::new();
        let mut operand_names: Vec<Option<String>> = Vec::new();

        // Track which outputs need to be moved from specific registers after asm
        // (output_idx, specific_reg, actual_loc)
        let mut output_moves: Vec<(usize, Reg, Loc)> = Vec::new();

        // Track which inputs need to be moved to specific registers before asm
        // (specific_reg, actual_loc)
        let mut input_moves: Vec<(Reg, Loc)> = Vec::new();

        // Track register remaps: if an allocated reg conflicts with reserved, use temp
        // (original_reg, temp_reg, actual_loc for restore)
        let mut remap_setup: Vec<(Reg, Reg, Loc)> = Vec::new();
        let mut remap_restore: Vec<(Reg, Reg, Loc)> = Vec::new();

        // Track which pseudos have been assigned temp registers (for +r where input/output share pseudo)
        let mut pseudo_to_temp: std::collections::HashMap<PseudoId, Reg> =
            std::collections::HashMap::new();

        // Helper to find a temp register not in reserved or already used
        let find_temp_reg = |reserved: &std::collections::HashSet<Reg>,
                             used: &std::collections::HashSet<Reg>|
         -> Reg {
            // Try R10, R11 first (caller-saved, rarely used for args)
            for r in [Reg::R10, Reg::R11, Reg::R8, Reg::R9, Reg::Rsi, Reg::Rdi] {
                if !reserved.contains(&r) && !used.contains(&r) {
                    return r;
                }
            }
            Reg::R10 // Fallback
        };

        let mut used_regs: std::collections::HashSet<Reg> = reserved_regs.clone();

        // Process output operands (they go first: %0, %1, etc.)
        for (idx, output) in asm_data.outputs.iter().enumerate() {
            let loc = self.get_location(output.pseudo);
            operand_names.push(output.name.clone());

            // Check for specific register constraint
            if let Some(specific_reg) = Self::constraint_to_specific_reg(&output.constraint) {
                // Output goes to specific register, then we'll move to actual loc after asm
                operand_regs.push(Some(specific_reg));
                operand_mem.push(None);
                // Only need to move if actual loc is different from specific reg
                if loc != Loc::Reg(specific_reg) {
                    output_moves.push((idx, specific_reg, loc));
                }
            } else {
                // No specific register - use allocated location
                match loc {
                    Loc::Reg(r) => {
                        // Check if allocated reg conflicts with reserved
                        if reserved_regs.contains(&r) {
                            // Use a temp register instead
                            let temp = find_temp_reg(&reserved_regs, &used_regs);
                            used_regs.insert(temp);
                            operand_regs.push(Some(temp));
                            operand_mem.push(None);
                            // For outputs, move from temp to actual loc after asm
                            remap_restore.push((temp, r, loc.clone()));
                            // Track this pseudo -> temp mapping for +r inputs
                            pseudo_to_temp.insert(output.pseudo, temp);
                        } else {
                            operand_regs.push(Some(r));
                            operand_mem.push(None);
                            used_regs.insert(r);
                        }
                    }
                    _ => {
                        // Memory or other location - emit as memory operand
                        let mem_str = self.loc_to_asm_string(&loc);
                        operand_regs.push(None);
                        operand_mem.push(Some(mem_str));
                    }
                }
            }
        }

        let num_outputs = asm_data.outputs.len();

        // Process input operands
        for input in &asm_data.inputs {
            // Handle matching constraints - use the matched output's location/register
            let (loc, constraint_for_reg) = if let Some(match_idx) = input.matching_output {
                if match_idx < num_outputs {
                    // Use the same register/location as the matched output
                    (
                        self.get_location(asm_data.outputs[match_idx].pseudo),
                        &asm_data.outputs[match_idx].constraint,
                    )
                } else {
                    (self.get_location(input.pseudo), &input.constraint)
                }
            } else {
                (self.get_location(input.pseudo), &input.constraint)
            };

            operand_names.push(input.name.clone());

            // Check for specific register constraint
            if let Some(specific_reg) = Self::constraint_to_specific_reg(constraint_for_reg) {
                // Input must go to specific register
                operand_regs.push(Some(specific_reg));
                operand_mem.push(None);
                // Only need to move if actual loc is different from specific reg
                if loc != Loc::Reg(specific_reg) {
                    input_moves.push((specific_reg, loc));
                }
            } else {
                // Check if this input shares a pseudo with an output that was remapped
                // This happens with +r constraints where input and output share the same pseudo
                if let Some(&temp) = pseudo_to_temp.get(&input.pseudo) {
                    // Reuse the same temp register as the output
                    operand_regs.push(Some(temp));
                    operand_mem.push(None);
                    // Add setup move to load value into temp
                    remap_setup.push((temp, temp, loc.clone()));
                } else {
                    // No specific register - use allocated location
                    match loc {
                        Loc::Reg(r) => {
                            // Check if allocated reg conflicts with reserved
                            if reserved_regs.contains(&r) {
                                // Use a temp register instead
                                let temp = find_temp_reg(&reserved_regs, &used_regs);
                                used_regs.insert(temp);
                                operand_regs.push(Some(temp));
                                operand_mem.push(None);
                                // For inputs, move from actual loc to temp before asm
                                remap_setup.push((r, temp, loc.clone()));
                            } else {
                                operand_regs.push(Some(r));
                                operand_mem.push(None);
                                used_regs.insert(r);
                            }
                        }
                        Loc::Imm(v) => {
                            // Immediate value
                            operand_regs.push(None);
                            operand_mem.push(Some(format!("${}", v)));
                        }
                        _ => {
                            // Memory or other location
                            let mem_str = self.loc_to_asm_string(&loc);
                            operand_regs.push(None);
                            operand_mem.push(Some(mem_str));
                        }
                    }
                }
            }
        }

        // Emit remap setup moves (for inputs that conflicted with reserved regs)
        for (_orig, temp, actual_loc) in &remap_setup {
            self.emit_raw_mov_from_loc(actual_loc, *temp);
        }

        // Emit moves from actual locations to specific registers (for inputs)
        for (specific_reg, actual_loc) in &input_moves {
            self.emit_raw_mov_from_loc(actual_loc, *specific_reg);
        }

        // Convert goto_labels from (BasicBlockId, String) to (label_string, label_name)
        let goto_labels_formatted: Vec<(String, String)> = asm_data
            .goto_labels
            .iter()
            .map(|(bb_id, name)| {
                // Format label as .Lfunc_bbid (same format as Label::name())
                let label_str = format!(".L{}_{}", self.base.current_fn, bb_id.0);
                (label_str, name.clone())
            })
            .collect();

        // Substitute %0, %1, %[name], %l0, %l[name], etc. in the template with actual operands
        let asm_output = self.substitute_asm_operands(
            &asm_data.template,
            &operand_regs,
            &operand_mem,
            &operand_names,
            &goto_labels_formatted,
        );

        // Emit the inline assembly as raw text
        // Split by newlines and emit each line
        for line in asm_output.lines() {
            let trimmed = line.trim();
            if !trimmed.is_empty() {
                self.push_lir(X86Inst::Directive(Directive::Raw(trimmed.to_string())));
            }
        }

        // Emit moves from specific registers to actual locations (for outputs)
        for (_idx, specific_reg, actual_loc) in &output_moves {
            self.emit_raw_mov_to_loc(*specific_reg, actual_loc);
        }

        // Emit remap restore moves (for outputs that conflicted with reserved regs)
        for (temp, _orig, actual_loc) in &remap_restore {
            self.emit_raw_mov_to_loc(*temp, actual_loc);
        }

        // Handle clobbers - for now just emit comments for documentation
        // Our simple codegen doesn't do sophisticated register allocation across asm
        for clobber in &asm_data.clobbers {
            match clobber.as_str() {
                "memory" => {
                    // Memory clobber - acts as compiler memory barrier
                    // Our codegen doesn't reorder loads/stores, so this is mostly informational
                }
                "cc" => {
                    // Condition codes clobbered - informational for our simple codegen
                }
                _ => {
                    // Register clobber - could save/restore if needed
                    // For now, trust that the register allocator has handled this
                }
            }
        }
    }

    /// Emit a raw mov instruction from a location to a register (for asm input setup)
    fn emit_raw_mov_from_loc(&mut self, loc: &Loc, dest_reg: Reg) {
        let dest_name = self.reg_name_64(dest_reg);
        match loc {
            Loc::Reg(src_reg) => {
                let src_name = self.reg_name_64(*src_reg);
                self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                    "movq %{}, %{}",
                    src_name, dest_name
                ))));
            }
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                    "movl -{}(%rbp), %{}",
                    adjusted,
                    self.sized_reg_name(dest_reg, 'k') // 32-bit for int
                ))));
            }
            Loc::Imm(v) => {
                self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                    "movl ${}, %{}",
                    v,
                    self.sized_reg_name(dest_reg, 'k')
                ))));
            }
            Loc::Global(name) => {
                self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                    "movl {}(%rip), %{}",
                    name,
                    self.sized_reg_name(dest_reg, 'k')
                ))));
            }
            _ => {
                // Other locations - use generic string
                let loc_str = self.loc_to_asm_string(loc);
                self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                    "movl {}, %{}",
                    loc_str,
                    self.sized_reg_name(dest_reg, 'k')
                ))));
            }
        }
    }

    /// Emit a raw mov instruction from a register to a location (for asm output store)
    fn emit_raw_mov_to_loc(&mut self, src_reg: Reg, loc: &Loc) {
        let src_name = self.sized_reg_name(src_reg, 'k'); // 32-bit for int
        match loc {
            Loc::Reg(dest_reg) => {
                let dest_name = self.reg_name_64(*dest_reg);
                self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                    "movq %{}, %{}",
                    self.reg_name_64(src_reg),
                    dest_name
                ))));
            }
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                    "movl %{}, -{}(%rbp)",
                    src_name, adjusted
                ))));
            }
            Loc::Global(name) => {
                self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                    "movl %{}, {}(%rip)",
                    src_name, name
                ))));
            }
            _ => {
                // Other locations - use generic string
                let loc_str = self.loc_to_asm_string(loc);
                self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                    "movl %{}, {}",
                    src_name, loc_str
                ))));
            }
        }
    }

    /// Convert a location to an asm operand string
    fn loc_to_asm_string(&self, loc: &Loc) -> String {
        match loc {
            Loc::Reg(r) => format!("%{}", self.reg_name_64(*r)),
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                format!("-{}(%rbp)", adjusted)
            }
            Loc::Imm(v) => format!("${}", v),
            Loc::Xmm(xmm) => xmm.name().to_string(),
            Loc::FImm(_, _) => {
                // Float immediates not directly usable in inline asm
                panic!("Float immediate not supported in inline asm operand")
            }
            Loc::Global(name) => format!("{}(%rip)", name),
        }
    }

    /// Get the 64-bit register name
    fn reg_name_64(&self, reg: Reg) -> &'static str {
        match reg {
            Reg::Rax => "rax",
            Reg::Rbx => "rbx",
            Reg::Rcx => "rcx",
            Reg::Rdx => "rdx",
            Reg::Rsi => "rsi",
            Reg::Rdi => "rdi",
            Reg::Rbp => "rbp",
            Reg::Rsp => "rsp",
            Reg::R8 => "r8",
            Reg::R9 => "r9",
            Reg::R10 => "r10",
            Reg::R11 => "r11",
            Reg::R12 => "r12",
            Reg::R13 => "r13",
            Reg::R14 => "r14",
            Reg::R15 => "r15",
        }
    }

    /// Extract the specific register required by an x86 asm constraint.
    /// Returns Some(Reg) if the constraint requires a specific register,
    /// None if any register is acceptable (e.g., "r").
    fn constraint_to_specific_reg(constraint: &str) -> Option<Reg> {
        // Scan constraint for specific register indicators
        // Skip modifiers like =, +, &, %
        for c in constraint.chars() {
            match c {
                'a' => return Some(Reg::Rax),
                'b' => return Some(Reg::Rbx),
                'c' => return Some(Reg::Rcx),
                'd' => return Some(Reg::Rdx),
                'S' => return Some(Reg::Rsi),
                'D' => return Some(Reg::Rdi),
                _ => {}
            }
        }
        None
    }

    /// Substitute %0, %1, %[name], %l0, %l[name], etc. with actual operand strings
    /// goto_labels: (label_string, label_name) - label_string is the fully formatted label
    fn substitute_asm_operands(
        &self,
        template: &str,
        regs: &[Option<Reg>],
        mems: &[Option<String>],
        names: &[Option<String>],
        goto_labels: &[(String, String)],
    ) -> String {
        crate::arch::substitute_asm_operands(self, template, regs, mems, names, goto_labels)
    }

    /// Get a sized register name based on modifier
    fn sized_reg_name(&self, reg: Reg, size_mod: char) -> &'static str {
        match (reg, size_mod) {
            // 8-bit (b)
            (Reg::Rax, 'b') => "al",
            (Reg::Rbx, 'b') => "bl",
            (Reg::Rcx, 'b') => "cl",
            (Reg::Rdx, 'b') => "dl",
            (Reg::Rsi, 'b') => "sil",
            (Reg::Rdi, 'b') => "dil",
            (Reg::R8, 'b') => "r8b",
            (Reg::R9, 'b') => "r9b",
            (Reg::R10, 'b') => "r10b",
            (Reg::R11, 'b') => "r11b",
            (Reg::R12, 'b') => "r12b",
            (Reg::R13, 'b') => "r13b",
            (Reg::R14, 'b') => "r14b",
            (Reg::R15, 'b') => "r15b",
            // 16-bit (w)
            (Reg::Rax, 'w') => "ax",
            (Reg::Rbx, 'w') => "bx",
            (Reg::Rcx, 'w') => "cx",
            (Reg::Rdx, 'w') => "dx",
            (Reg::Rsi, 'w') => "si",
            (Reg::Rdi, 'w') => "di",
            (Reg::R8, 'w') => "r8w",
            (Reg::R9, 'w') => "r9w",
            (Reg::R10, 'w') => "r10w",
            (Reg::R11, 'w') => "r11w",
            (Reg::R12, 'w') => "r12w",
            (Reg::R13, 'w') => "r13w",
            (Reg::R14, 'w') => "r14w",
            (Reg::R15, 'w') => "r15w",
            // 32-bit (k or l)
            (Reg::Rax, 'k') => "eax",
            (Reg::Rbx, 'k') => "ebx",
            (Reg::Rcx, 'k') => "ecx",
            (Reg::Rdx, 'k') => "edx",
            (Reg::Rsi, 'k') => "esi",
            (Reg::Rdi, 'k') => "edi",
            (Reg::R8, 'k') => "r8d",
            (Reg::R9, 'k') => "r9d",
            (Reg::R10, 'k') => "r10d",
            (Reg::R11, 'k') => "r11d",
            (Reg::R12, 'k') => "r12d",
            (Reg::R13, 'k') => "r13d",
            (Reg::R14, 'k') => "r14d",
            (Reg::R15, 'k') => "r15d",
            // 64-bit (q) - default
            _ => self.reg_name_64(reg),
        }
    }
}

// ============================================================================
// AsmOperandFormatter trait implementation
// ============================================================================

impl crate::arch::AsmOperandFormatter for X86_64CodeGen {
    type Reg = Reg;

    fn size_modifiers(&self) -> &'static [char] {
        &['b', 'w', 'k', 'q'] // 8, 16, 32, 64-bit
    }

    fn format_reg_sized(&self, reg: Reg, size_mod: char) -> String {
        format!("%{}", self.sized_reg_name(reg, size_mod))
    }

    fn format_reg_default(&self, reg: Reg) -> String {
        // x86 inline asm defaults to 32-bit for GCC compatibility
        format!("%{}", self.sized_reg_name(reg, 'k'))
    }
}

// ============================================================================
// CodeGenerator trait implementation
// ============================================================================

impl CodeGenerator for X86_64CodeGen {
    fn generate(&mut self, module: &Module, types: &TypeTable) -> String {
        self.base.output.clear();
        self.base.reset_debug_state();
        self.base.emit_debug = module.debug;

        // Emit file header
        self.emit_header();

        // Emit .file directives unconditionally (useful for diagnostics/profiling)
        for (i, path) in module.source_files.iter().enumerate() {
            // File indices in DWARF start at 1
            self.base
                .push_directive(Directive::file((i + 1) as u32, path.clone()));
        }

        // Emit globals
        for (name, typ, init) in &module.globals {
            self.emit_global(name, typ, init, types);
        }

        // Emit string literals
        if !module.strings.is_empty() {
            self.base.emit_strings(&module.strings);
        }

        // Emit functions
        for func in &module.functions {
            self.emit_function(func, types);
        }

        // Emit all buffered LIR instructions to output string
        self.base.emit_all();

        self.base.output.clone()
    }

    fn set_emit_unwind_tables(&mut self, emit: bool) {
        self.base.emit_unwind_tables = emit;
    }
}
