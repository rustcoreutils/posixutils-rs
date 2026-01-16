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

use crate::abi::{get_abi, ArgClass, RegClass};
use crate::arch::codegen::{is_variadic_function, BswapSize, CodeGenBase, CodeGenerator, UnaryOp};
use crate::arch::lir::{complex_fp_info, CondCode, Directive, FpSize, Label, OperandSize, Symbol};
use crate::arch::x86_64::float::f64_to_f16_bits;
use crate::arch::x86_64::lir::{GpOperand, MemAddr, ShiftCount, X86Inst, XmmOperand};
use crate::arch::x86_64::regalloc::{Loc, Reg, RegAlloc, XmmReg};
use crate::ir::{Function, Instruction, Module, Opcode, Pseudo, PseudoId, PseudoKind};
use crate::target::{Os, Target};
use crate::types::{TypeId, TypeKind, TypeTable};
use std::collections::{HashMap, HashSet};

// ============================================================================
// x86-64 Code Generator
// ============================================================================

/// x86-64 code generator
pub struct X86_64CodeGen {
    /// Common code generation infrastructure
    pub(super) base: CodeGenBase<X86Inst>,
    /// Current function's register allocation
    locations: HashMap<PseudoId, Loc>,
    /// Current function's pseudos (for looking up values)
    pub(super) pseudos: Vec<Pseudo>,
    /// Callee-saved registers used in current function (for epilogue)
    callee_saved_regs: Vec<Reg>,
    /// Offset to add to stack locations to account for callee-saved registers
    pub(super) callee_saved_offset: i32,
    /// Offset from rbp to register save area (for variadic functions)
    pub(super) reg_save_area_offset: i32,
    /// Number of fixed GP parameters (for variadic functions)
    pub(super) num_fixed_gp_params: usize,
    /// Number of fixed FP parameters (for variadic functions)
    pub(super) num_fixed_fp_params: usize,
    /// Counter for generating unique internal labels
    pub(super) unique_label_counter: u32,
    /// External symbols (need GOT access on macOS)
    pub(super) extern_symbols: HashSet<String>,
    /// Position-independent code mode (for shared libraries)
    pic_mode: bool,
    /// Long double constants to emit (label_bits -> value_bits)
    pub(super) ld_constants: HashMap<u64, [u8; 16]>,
    /// Double constants to emit (label_bits -> f64 value)
    pub(super) double_constants: HashMap<u64, f64>,
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
            extern_symbols: HashSet::new(),
            pic_mode: false,
            ld_constants: HashMap::new(),
            double_constants: HashMap::new(),
        }
    }

    /// Push a LIR instruction to the buffer (deferred emission)
    #[inline]
    pub(super) fn push_lir(&mut self, inst: X86Inst) {
        self.base.push_lir(inst);
    }

    /// Convert a Loc to a GpOperand for LIR
    pub(super) fn loc_to_gp_operand(&self, loc: &Loc) -> GpOperand {
        match loc {
            Loc::Reg(r) => GpOperand::Reg(*r),
            Loc::Stack(offset) => {
                let adjusted = *offset + self.callee_saved_offset;
                GpOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: -adjusted,
                })
            }
            Loc::IncomingArg(offset) => {
                // Incoming stack argument: at [rbp + offset] (positive offset)
                // No callee_saved_offset adjustment needed - these are above the return address
                GpOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: *offset,
                })
            }
            Loc::Imm(v) => GpOperand::Imm(*v),
            Loc::FImm(_, _) => GpOperand::Imm(0), // FP immediates handled separately
            Loc::Xmm(_) => GpOperand::Imm(0),     // XMM handled separately
            Loc::Global(name) => {
                let symbol = if name.starts_with('.') {
                    Symbol::local(name.clone())
                } else {
                    Symbol::global(name.clone())
                };
                GpOperand::Mem(MemAddr::RipRelative(symbol))
            }
        }
    }

    /// Check if a symbol needs GOT access
    /// - In PIC mode: all external symbols need GOT access
    /// - On macOS: external symbols always need GOT access (even without PIC)
    #[inline]
    pub(super) fn needs_got_access(&self, name: &str) -> bool {
        // External symbols always need GOT access:
        // - On macOS: required for dynamic linking
        // - On Linux: required for PIE (default) and when linking with shared libs
        // - In PIC mode: always required
        // Using GOT unconditionally for external symbols is safe and matches GCC/Clang behavior
        self.extern_symbols.contains(name)
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
        // Skip extern symbols - they're defined elsewhere
        if self.extern_symbols.contains(name) {
            return;
        }
        self.base.emit_global(name, typ, init, types);
    }

    /// Emit long double constants collected during codegen
    fn emit_ld_constants(&mut self) {
        if self.ld_constants.is_empty() {
            return;
        }

        // Emit in rodata section
        self.base.push_directive(Directive::Rodata);

        // Emit each constant
        for (label_bits, bytes) in &self.ld_constants {
            let label = format!(".Lld_const_{}", label_bits);
            // Align to 16 bytes (power of 2: 4 means 2^4 = 16)
            self.base.push_directive(Directive::Align(4));
            self.base.push_directive(Directive::local_label(&label));

            // Emit the 16 bytes as .byte directives
            let mut byte_str = String::from(".byte ");
            for (i, b) in bytes.iter().enumerate() {
                if i > 0 {
                    byte_str.push_str(", ");
                }
                byte_str.push_str(&format!("0x{:02x}", b));
            }
            self.base.push_directive(Directive::Raw(byte_str));
        }
    }

    /// Emit double constants collected during codegen (for x87 conversions)
    fn emit_double_constants(&mut self) {
        if self.double_constants.is_empty() {
            return;
        }

        // Emit in rodata section
        self.base.push_directive(Directive::Rodata);

        // Emit each constant
        for (label_bits, value) in &self.double_constants {
            let label = format!(".Ldbl_const_{}", label_bits);
            // Align to 8 bytes (power of 2: 3 means 2^3 = 8)
            self.base.push_directive(Directive::Align(3));
            self.base.push_directive(Directive::local_label(&label));

            // Emit as .quad (8 bytes)
            let bits = value.to_bits();
            self.base
                .push_directive(Directive::Raw(format!(".quad 0x{:016x}", bits)));
        }
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

        // Emit function header (directives, label, CFI start)
        self.emit_function_header(func.is_static, &func.name);

        // Emit prologue (push rbp, callee-saved regs, allocate stack)
        self.emit_prologue(stack_size, reg_save_area_size);

        // Store spilled arguments before any calls can clobber them
        self.store_spilled_args(&alloc);

        // For variadic functions, save argument registers to the register save area
        if is_variadic {
            self.emit_variadic_save_area();
        }

        // Move arguments from registers to their allocated stack locations
        self.store_args_to_stack(func, types, &alloc);

        // Save number of fixed GP and FP params for va_start
        if is_variadic {
            self.count_fixed_params(func, types);
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

    /// Emit function header directives (text section, visibility, type, label, CFI start)
    fn emit_function_header(&mut self, is_static: bool, name: &str) {
        self.push_lir(X86Inst::Directive(Directive::Blank));
        self.push_lir(X86Inst::Directive(Directive::Text));

        // Skip .globl for static functions (internal linkage)
        if !is_static {
            self.push_lir(X86Inst::Directive(Directive::global(name)));
        }

        // ELF-only type (handled by Directive::emit which skips on macOS)
        self.push_lir(X86Inst::Directive(Directive::type_func(name)));

        // Function label
        self.push_lir(X86Inst::Directive(Directive::global_label(name)));

        // CFI: Start procedure (enables stack unwinding for this function)
        if self.base.emit_unwind_tables {
            self.push_lir(X86Inst::Directive(Directive::CfiStartProc));
        }
    }

    /// Emit function prologue: push rbp, save callee-saved registers, allocate stack
    fn emit_prologue(&mut self, stack_size: i32, reg_save_area_size: i32) {
        let bp = Reg::bp();
        let sp = Reg::sp();

        // Push frame pointer
        self.push_lir(X86Inst::Push {
            src: GpOperand::Reg(bp),
        });
        if self.base.emit_debug {
            // After pushq %rbp: CFA is now at %rsp+16, and %rbp is saved at CFA-16
            self.push_lir(X86Inst::Directive(Directive::CfiDefCfaOffset(16)));
            self.push_lir(X86Inst::Directive(Directive::cfi_offset("%rbp", -16)));
        }

        // Set up frame pointer
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
    }

    /// Emit stores for arguments spilled from caller-saved registers to stack
    fn store_spilled_args(&mut self, alloc: &RegAlloc) {
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
    }

    /// Save argument registers to the register save area for variadic functions
    fn emit_variadic_save_area(&mut self) {
        // AMD64 ABI: rdi at offset 0, rsi at offset 8, rdx at offset 16, etc.
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

    /// Move arguments from registers to their allocated stack locations
    fn store_args_to_stack(&mut self, func: &Function, types: &TypeTable, alloc: &RegAlloc) {
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
                            let is_longdouble =
                                types.kind(*typ) == crate::types::TypeKind::LongDouble;
                            if is_complex {
                                fp_arg_idx += 2;
                            } else if is_longdouble {
                                // Long double is on stack, doesn't use XMM registers
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
                                        let (fp_size, imag_offset) =
                                            complex_fp_info(types, &self.base.target, *typ);
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
                        } else if types.kind(*typ) == crate::types::TypeKind::LongDouble {
                            // Long double is passed on the stack per System V AMD64 ABI
                            // No XMM register move needed - already at IncomingArg offset
                            // Don't increment fp_arg_idx - long double doesn't use XMM
                        } else if is_fp {
                            // FP argument (float/double)
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
    }

    /// Count and save number of fixed GP and FP params for va_start
    fn count_fixed_params(&mut self, func: &Function, types: &TypeTable) {
        let has_sret = func
            .pseudos
            .iter()
            .any(|p| matches!(p.kind, PseudoKind::Arg(0)) && p.name.as_deref() == Some("__sret"));

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
            .filter(|(_, typ)| {
                types.is_float(*typ) && types.kind(*typ) != crate::types::TypeKind::LongDouble
            })
            .count();
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
        // Struct returns depend on ABI classification (SSE for all-float structs)
        if let Some(src) = insn.src.first() {
            let src_loc = self.get_location(*src);
            let is_complex = insn.typ.is_some_and(|t| types.is_complex(t));
            let is_fp = matches!(src_loc, Loc::Xmm(_) | Loc::FImm(..))
                || insn.typ.is_some_and(|t| types.is_float(t));

            // Check if return type is a struct/union
            let ret_typ = insn.typ;
            let is_struct_or_union = ret_typ.is_some_and(|t| {
                let kind = types.kind(t);
                kind == TypeKind::Struct || kind == TypeKind::Union
            });

            if insn.is_two_reg_return {
                // Two-register struct return: check ABI for SSE vs INTEGER
                if is_struct_or_union {
                    if let Some(typ) = ret_typ {
                        let abi = get_abi(&self.base.target);
                        if let ArgClass::Direct { classes, .. } = abi.classify_return(typ, types) {
                            // Handle mixed SSE+INTEGER returns per ABI classification
                            // Each SSE class uses XMM0, XMM1; each INTEGER uses RAX, RDX
                            let mut xmm_idx = 0;
                            let mut gp_idx = 0;
                            let srcs = [Some(*src), insn.src.get(1).copied()];

                            for (i, &class) in classes.iter().enumerate() {
                                if let Some(s) = srcs.get(i).copied().flatten() {
                                    match class {
                                        RegClass::Sse => {
                                            let xmm = if xmm_idx == 0 {
                                                XmmReg::Xmm0
                                            } else {
                                                XmmReg::Xmm1
                                            };
                                            self.emit_fp_move(s, xmm, 64);
                                            xmm_idx += 1;
                                        }
                                        RegClass::Integer => {
                                            let gp = if gp_idx == 0 { Reg::Rax } else { Reg::Rdx };
                                            self.emit_move(s, gp, 64);
                                            gp_idx += 1;
                                        }
                                        _ => {
                                            // NoClass, Memory - shouldn't happen for Direct
                                            let gp = if gp_idx == 0 { Reg::Rax } else { Reg::Rdx };
                                            self.emit_move(s, gp, 64);
                                            gp_idx += 1;
                                        }
                                    }
                                }
                            }
                        } else {
                            self.emit_move(*src, Reg::Rax, 64);
                            if let Some(&src2) = insn.src.get(1) {
                                self.emit_move(src2, Reg::Rdx, 64);
                            }
                        }
                    } else {
                        self.emit_move(*src, Reg::Rax, 64);
                        if let Some(&src2) = insn.src.get(1) {
                            self.emit_move(src2, Reg::Rdx, 64);
                        }
                    }
                } else {
                    self.emit_move(*src, Reg::Rax, 64);
                    if let Some(&src2) = insn.src.get(1) {
                        self.emit_move(src2, Reg::Rdx, 64);
                    }
                }
            } else if is_struct_or_union && !is_complex {
                // Single-register struct return: use ABI classification
                if let Some(typ) = ret_typ {
                    let abi = get_abi(&self.base.target);
                    if let ArgClass::Direct { classes, size_bits } = abi.classify_return(typ, types)
                    {
                        if classes.iter().all(|c| *c == RegClass::Sse) {
                            if classes.len() == 2 {
                                // Two SSE regs for 9-16 byte float struct
                                match src_loc {
                                    Loc::Stack(offset) => {
                                        let adjusted = offset + self.callee_saved_offset;
                                        self.push_lir(X86Inst::MovFp {
                                            size: FpSize::Double,
                                            src: XmmOperand::Mem(MemAddr::BaseOffset {
                                                base: Reg::Rbp,
                                                offset: -adjusted,
                                            }),
                                            dst: XmmOperand::Reg(XmmReg::Xmm0),
                                        });
                                        self.push_lir(X86Inst::MovFp {
                                            size: FpSize::Double,
                                            src: XmmOperand::Mem(MemAddr::BaseOffset {
                                                base: Reg::Rbp,
                                                offset: -adjusted + 8,
                                            }),
                                            dst: XmmOperand::Reg(XmmReg::Xmm1),
                                        });
                                    }
                                    Loc::Reg(r) => {
                                        self.push_lir(X86Inst::MovFp {
                                            size: FpSize::Double,
                                            src: XmmOperand::Mem(MemAddr::BaseOffset {
                                                base: r,
                                                offset: 0,
                                            }),
                                            dst: XmmOperand::Reg(XmmReg::Xmm0),
                                        });
                                        self.push_lir(X86Inst::MovFp {
                                            size: FpSize::Double,
                                            src: XmmOperand::Mem(MemAddr::BaseOffset {
                                                base: r,
                                                offset: 8,
                                            }),
                                            dst: XmmOperand::Reg(XmmReg::Xmm1),
                                        });
                                    }
                                    _ => {}
                                }
                            } else {
                                // Single SSE reg for small float struct (<=8 bytes)
                                let fp_size = if size_bits <= 32 {
                                    FpSize::Single
                                } else {
                                    FpSize::Double
                                };
                                match src_loc {
                                    Loc::Stack(offset) => {
                                        let adjusted = offset + self.callee_saved_offset;
                                        self.push_lir(X86Inst::MovFp {
                                            size: fp_size,
                                            src: XmmOperand::Mem(MemAddr::BaseOffset {
                                                base: Reg::Rbp,
                                                offset: -adjusted,
                                            }),
                                            dst: XmmOperand::Reg(XmmReg::Xmm0),
                                        });
                                    }
                                    Loc::Reg(r) => {
                                        // GP reg contains struct value, move to XMM
                                        self.push_lir(X86Inst::MovGpXmm {
                                            size: OperandSize::B64,
                                            src: r,
                                            dst: XmmReg::Xmm0,
                                        });
                                    }
                                    _ => self.emit_move(*src, Reg::Rax, insn.size),
                                }
                            }
                        } else {
                            // INTEGER class - return in RAX
                            self.emit_move(*src, Reg::Rax, insn.size);
                        }
                    } else {
                        self.emit_move(*src, Reg::Rax, insn.size);
                    }
                } else {
                    self.emit_move(*src, Reg::Rax, insn.size);
                }
            } else if is_complex {
                // Complex return value handling:
                // - float _Complex (8 bytes): packed in XMM0 (real in low 32, imag in high 32)
                // - double _Complex (16 bytes): real in XMM0, imag in XMM1
                let (fp_size, imag_offset) =
                    complex_fp_info(types, &self.base.target, insn.typ.unwrap());
                let is_float_complex = fp_size == FpSize::Single;

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
                        if is_float_complex {
                            // Load entire 64-bit packed value into XMM0
                            self.push_lir(X86Inst::MovFp {
                                size: FpSize::Double, // 64-bit movq
                                src: XmmOperand::Mem(MemAddr::BaseOffset {
                                    base: Reg::Rax,
                                    offset: 0,
                                }),
                                dst: XmmOperand::Reg(XmmReg::Xmm0),
                            });
                        } else {
                            // Load real into XMM0, imag into XMM1
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
                    }
                    Loc::Reg(r) => {
                        if is_float_complex {
                            // Load entire 64-bit packed value into XMM0
                            self.push_lir(X86Inst::MovFp {
                                size: FpSize::Double, // 64-bit movq
                                src: XmmOperand::Mem(MemAddr::BaseOffset { base: r, offset: 0 }),
                                dst: XmmOperand::Reg(XmmReg::Xmm0),
                            });
                        } else {
                            // Load real into XMM0, imag into XMM1
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
                    }
                    _ => {}
                }
            } else if is_fp {
                // Check for long double - return in ST(0) per x86-64 ABI
                let is_longdouble = insn
                    .typ
                    .is_some_and(|t| types.kind(t) == TypeKind::LongDouble);
                if is_longdouble {
                    // Load long double to x87 ST(0) for return
                    let src_addr = self.get_x87_mem_addr(*src);
                    self.push_lir(X86Inst::X87Load { addr: src_addr });
                } else {
                    // Use type-aware size for FP return
                    let fp_typ = insn.typ.expect("FP return must have type");
                    let fp_size = types.size_bits(fp_typ).max(32);
                    self.emit_fp_move(*src, XmmReg::Xmm0, fp_size);
                }
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
            Loc::IncomingArg(offset) => {
                self.push_lir(X86Inst::Cmp {
                    size: op_size,
                    src: GpOperand::Imm(0),
                    dst: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: *offset,
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
                if self.is_longdouble_op(insn, types) {
                    self.emit_x87_binop(insn);
                } else {
                    self.emit_fp_binop(insn, types);
                }
            }

            Opcode::FNeg => {
                if self.is_longdouble_op(insn, types) {
                    self.emit_x87_neg(insn);
                } else {
                    self.emit_fp_neg(insn, types);
                }
            }

            // Floating-point comparisons
            Opcode::FCmpOEq
            | Opcode::FCmpONe
            | Opcode::FCmpOLt
            | Opcode::FCmpOLe
            | Opcode::FCmpOGt
            | Opcode::FCmpOGe => {
                if self.is_longdouble_op(insn, types) {
                    self.emit_x87_compare(insn);
                } else {
                    self.emit_fp_compare(insn, types);
                }
            }

            // Integer to float conversions
            Opcode::UCvtF | Opcode::SCvtF => {
                // Use x87 for long double destination
                let dst_is_longdouble = insn
                    .typ
                    .is_some_and(|t| types.kind(t) == TypeKind::LongDouble);
                if dst_is_longdouble {
                    self.emit_x87_int_to_float(insn);
                } else {
                    self.emit_int_to_float(insn, types);
                }
            }

            // Float to integer conversions
            Opcode::FCvtU | Opcode::FCvtS => {
                // Use x87 for long double source
                let src_is_longdouble = insn
                    .src_typ
                    .is_some_and(|t| types.kind(t) == TypeKind::LongDouble);
                if src_is_longdouble {
                    self.emit_x87_float_to_int(insn);
                } else {
                    self.emit_float_to_int(insn, types);
                }
            }

            // Float to float conversions (e.g., float to double)
            Opcode::FCvtF => {
                // Use x87 when long double is involved
                let dst_is_longdouble = insn
                    .typ
                    .is_some_and(|t| types.kind(t) == TypeKind::LongDouble);
                let src_is_longdouble = insn
                    .src_typ
                    .is_some_and(|t| types.kind(t) == TypeKind::LongDouble);
                if dst_is_longdouble || src_is_longdouble {
                    self.emit_x87_fp_cvt(insn, types);
                } else {
                    self.emit_float_to_float(insn, types);
                }
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
                    // Use R10 as scratch to avoid clobbering live values in Rax
                    let dst_reg = match &dst_loc {
                        Loc::Reg(r) => *r,
                        _ => Reg::R10,
                    };
                    let src_loc = self.get_location(src);
                    match src_loc {
                        Loc::Global(name) => {
                            // Check if it's a local label (starts with '.') or global symbol
                            let is_local_label = name.starts_with('.');
                            if self.needs_got_access(&name) {
                                // External symbols on macOS need GOT access
                                self.push_lir(X86Inst::Mov {
                                    size: OperandSize::B64,
                                    src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(
                                        name.clone(),
                                    ))),
                                    dst: GpOperand::Reg(dst_reg),
                                });
                            } else {
                                self.push_lir(X86Inst::Lea {
                                    addr: MemAddr::RipRelative(Symbol {
                                        name: name.clone(),
                                        is_local: is_local_label,
                                        is_extern: false,
                                    }),
                                    dst: dst_reg,
                                });
                            }
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

            Opcode::Fabs32 => {
                self.emit_fabs32(insn);
            }

            Opcode::Fabs64 => {
                self.emit_fabs64(insn);
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

            // ================================================================
            // Atomic Operations (C11 _Atomic support)
            // ================================================================
            Opcode::AtomicLoad => {
                self.emit_atomic_load(insn, types);
            }

            Opcode::AtomicStore => {
                self.emit_atomic_store(insn, types);
            }

            Opcode::AtomicSwap => {
                self.emit_atomic_swap(insn, types);
            }

            Opcode::AtomicCas => {
                self.emit_atomic_cas(insn, types);
            }

            Opcode::AtomicFetchAdd => {
                self.emit_atomic_fetch_add(insn, types);
            }

            Opcode::AtomicFetchSub => {
                self.emit_atomic_fetch_sub(insn, types);
            }

            Opcode::AtomicFetchAnd => {
                self.emit_atomic_fetch_and(insn, types);
            }

            Opcode::AtomicFetchOr => {
                self.emit_atomic_fetch_or(insn, types);
            }

            Opcode::AtomicFetchXor => {
                self.emit_atomic_fetch_xor(insn, types);
            }

            Opcode::Fence => {
                self.emit_fence(insn);
            }

            // Skip no-ops and unimplemented
            _ => {}
        }
    }

    pub(super) fn get_location(&self, pseudo: PseudoId) -> Loc {
        self.locations.get(&pseudo).cloned().unwrap_or(Loc::Imm(0))
    }

    pub(super) fn emit_move(&mut self, src: PseudoId, dst: Reg, size: u32) {
        let actual_size = size; // Keep original size for sub-32-bit handling
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
                // For sub-32-bit values, use zero-extending load to avoid garbage in upper bits
                if actual_size < 32 {
                    // LIR: zero-extending memory-to-register move
                    self.push_lir(X86Inst::Movzx {
                        src_size: OperandSize::from_bits(actual_size),
                        dst_size: OperandSize::B32,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -adjusted,
                        }),
                        dst,
                    });
                } else {
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
            }
            Loc::IncomingArg(offset) => {
                // For sub-32-bit values, use zero-extending load
                if actual_size < 32 {
                    // LIR: zero-extending memory-to-register move from incoming stack arg
                    self.push_lir(X86Inst::Movzx {
                        src_size: OperandSize::from_bits(actual_size),
                        dst_size: OperandSize::B32,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset,
                        }),
                        dst,
                    });
                } else {
                    // LIR: memory-to-register move from incoming stack arg
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset,
                        }),
                        dst: GpOperand::Reg(dst),
                    });
                }
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
                // Use local symbol for labels starting with '.' (e.g., .LC0 for string constants)
                if self.needs_got_access(&name) {
                    // External symbols on macOS: load address from GOT, then load value
                    // Use R11 as temp if dst is R11, otherwise use dst
                    let temp_reg = if dst == Reg::R11 { Reg::R10 } else { Reg::R11 };
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(name.clone()))),
                        dst: GpOperand::Reg(temp_reg),
                    });
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: temp_reg,
                            offset: 0,
                        }),
                        dst: GpOperand::Reg(dst),
                    });
                } else {
                    let symbol = if name.starts_with('.') {
                        Symbol::local(name.clone())
                    } else {
                        Symbol::global(name.clone())
                    };
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Mem(MemAddr::RipRelative(symbol)),
                        dst: GpOperand::Reg(dst),
                    });
                }
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
            Loc::FImm(v, fp_size) => {
                // Float immediate to GP register - used for Float16 rtlib calls
                // Convert the float value to its bit representation and load as integer
                if fp_size == 16 {
                    // Float16: convert to 16-bit representation
                    let bits = f64_to_f16_bits(v);
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Imm(bits as i64),
                        dst: GpOperand::Reg(dst),
                    });
                } else if fp_size == 32 {
                    // float: convert to 32-bit representation
                    let bits = (v as f32).to_bits();
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Imm(bits as i64),
                        dst: GpOperand::Reg(dst),
                    });
                } else {
                    // double: convert to 64-bit representation
                    let bits = v.to_bits();
                    self.push_lir(X86Inst::MovAbs {
                        imm: bits as i64,
                        dst,
                    });
                }
            }
        }
    }

    pub(super) fn emit_move_to_loc(&mut self, src: Reg, dst: &Loc, size: u32) {
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
                let adjusted = offset + self.callee_saved_offset;
                // For sub-32-bit values, first zero-extend to 32 bits then store 32 bits.
                // This ensures that subsequent 32-bit loads get correct values.
                if size < 32 {
                    // Use scratch register to avoid modifying source
                    let scratch = if src == Reg::R10 { Reg::R11 } else { Reg::R10 };
                    // Copy to scratch
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Reg(src),
                        dst: GpOperand::Reg(scratch),
                    });
                    // Zero-extend by masking to the appropriate size
                    let mask = (1i64 << size) - 1;
                    self.push_lir(X86Inst::And {
                        size: OperandSize::B32,
                        src: GpOperand::Imm(mask),
                        dst: scratch,
                    });
                    // Store 32 bits
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B32,
                        src: GpOperand::Reg(scratch),
                        dst: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -adjusted,
                        }),
                    });
                } else {
                    // Store with actual size
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
            }
            Loc::Xmm(xmm) => {
                // Move from GP register to XMM register (used for Float16 rtlib returns)
                let op_size = if size <= 32 {
                    OperandSize::B32
                } else {
                    OperandSize::B64
                };
                self.push_lir(X86Inst::MovGpXmm {
                    size: op_size,
                    src,
                    dst: *xmm,
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
            // Long double uses x87, not SSE
            if self.is_longdouble_op(insn, types) {
                self.emit_x87_load(insn);
            } else {
                self.emit_fp_load(insn, types);
            }
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
                // Use local symbol for labels starting with '.' (e.g., .LC0 for string constants)
                let is_local_label = name.starts_with('.');
                if self.needs_got_access(&name) {
                    // External symbols on macOS: load address from GOT, then load value
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(name.clone()))),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    if mem_size <= 16 {
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
                } else {
                    let symbol = if is_local_label {
                        Symbol::local(name.clone())
                    } else {
                        Symbol::global(name.clone())
                    };
                    if mem_size <= 16 {
                        // LIR: sign/zero extending load from global
                        let src_size = OperandSize::from_bits(mem_size);
                        if is_unsigned {
                            self.push_lir(X86Inst::Movzx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::RipRelative(symbol.clone())),
                                dst: dst_reg,
                            });
                        } else {
                            self.push_lir(X86Inst::Movsx {
                                src_size,
                                dst_size: OperandSize::B32,
                                src: GpOperand::Mem(MemAddr::RipRelative(symbol.clone())),
                                dst: dst_reg,
                            });
                        }
                    } else {
                        // LIR: regular load from global
                        let op_size = OperandSize::from_bits(reg_size);
                        self.push_lir(X86Inst::Mov {
                            size: op_size,
                            src: GpOperand::Mem(MemAddr::RipRelative(symbol)),
                            dst: GpOperand::Reg(dst_reg),
                        });
                    }
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
            // Long double uses x87, not SSE
            if self.is_longdouble_op(insn, types) {
                self.emit_x87_store(insn);
            } else {
                self.emit_fp_store(insn, types);
            }
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
                // Use local symbol for labels starting with '.' (e.g., .LC0 for string constants)
                let is_local_label = name.starts_with('.');
                let op_size = OperandSize::from_bits(mem_size);
                if self.needs_got_access(&name) {
                    // External symbols on macOS: load address from GOT, then store
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(name.clone()))),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Reg(value_reg),
                        dst: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::R11,
                            offset: insn.offset as i32,
                        }),
                    });
                } else {
                    let symbol = if is_local_label {
                        Symbol::local(name.clone())
                    } else {
                        Symbol::global(name.clone())
                    };
                    // LIR: store to global via RIP-relative
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Reg(value_reg),
                        dst: GpOperand::Mem(MemAddr::RipRelative(symbol)),
                    });
                }
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

        // Special case: if value is immediate 0, zero the struct instead of copying
        if let Loc::Imm(0) = value_loc {
            self.emit_struct_zero(insn, addr, num_qwords);
            return;
        }

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
                if self.needs_got_access(name) {
                    // External symbols on macOS: load address from GOT
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(name.clone()))),
                        dst: GpOperand::Reg(Reg::R10),
                    });
                } else {
                    let symbol = if name.starts_with('.') {
                        Symbol::local(name.clone())
                    } else {
                        Symbol::global(name.clone())
                    };
                    self.push_lir(X86Inst::Lea {
                        addr: MemAddr::RipRelative(symbol),
                        dst: Reg::R10,
                    });
                }
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
                if self.needs_got_access(name) {
                    // External symbols on macOS: load address from GOT
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(name.clone()))),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                } else {
                    let symbol = if name.starts_with('.') {
                        Symbol::local(name.clone())
                    } else {
                        Symbol::global(name.clone())
                    };
                    self.push_lir(X86Inst::Lea {
                        addr: MemAddr::RipRelative(symbol),
                        dst: Reg::R11,
                    });
                }
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

    /// Emit code to zero a struct (for struct = {0} initialization)
    fn emit_struct_zero(&mut self, insn: &Instruction, addr: PseudoId, num_qwords: u32) {
        let addr_loc = self.get_location(addr);

        // Load destination address into R11
        match addr_loc {
            Loc::Stack(offset) => {
                let adjusted = offset - insn.offset as i32 + self.callee_saved_offset;
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
                    self.push_lir(X86Inst::Lea {
                        addr: MemAddr::BaseOffset {
                            base: r,
                            offset: insn.offset as i32,
                        },
                        dst: Reg::R11,
                    });
                } else if r != Reg::R11 {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                }
            }
            Loc::Global(ref name) => {
                if self.needs_got_access(name) {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(name.clone()))),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                } else {
                    let symbol = if name.starts_with('.') {
                        Symbol::local(name.clone())
                    } else {
                        Symbol::global(name.clone())
                    };
                    self.push_lir(X86Inst::Lea {
                        addr: MemAddr::RipRelative(symbol),
                        dst: Reg::R11,
                    });
                }
                if insn.offset != 0 {
                    self.push_lir(X86Inst::Add {
                        size: OperandSize::B64,
                        src: GpOperand::Imm(insn.offset),
                        dst: Reg::R11,
                    });
                }
            }
            _ => return,
        }

        // Load 0 into R10 once
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Imm(0),
            dst: GpOperand::Reg(Reg::R10),
        });

        // Store zeros to each qword
        for i in 0..num_qwords {
            let byte_offset = (i * 8) as i32;
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::R10),
                dst: GpOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: byte_offset,
                }),
            });
        }
    }

    fn emit_call(&mut self, insn: &Instruction, types: &TypeTable) {
        // Get function name (or placeholder for indirect calls)
        let func_name = if insn.indirect_target.is_some() {
            "<indirect>".to_string()
        } else {
            match &insn.func_name {
                Some(n) => n.clone(),
                None => return,
            }
        };

        // For indirect calls, load function pointer into R11
        if let Some(func_addr) = insn.indirect_target {
            self.emit_move(func_addr, Reg::R11, 64);
        }

        // Classify arguments into register vs stack
        let info = self.classify_call_args(insn, types);

        // Push stack arguments
        let stack_args = self.push_stack_args(insn, &info, types);

        // Save registers that would be clobbered by argument setup
        let saved_arg_regs = self.save_clobbered_arg_regs(insn, &info, types);

        // Set up register arguments
        let fp_arg_count = self.setup_register_args(insn, &info, &saved_arg_regs, types);

        // For variadic calls, set AL to number of XMM registers used
        if insn.variadic_arg_start.is_some() {
            self.set_variadic_fp_count(fp_arg_count);
        }

        // Emit the call instruction
        self.emit_call_instruction(insn, &func_name);

        // Clean up stack
        self.cleanup_call_stack(stack_args, info.needs_padding);

        // Handle return value
        self.handle_call_return_value(insn, types);
    }

    /// Emit a select (ternary) instruction using CMOVcc
    /// This is used for pure ternary expressions: cond ? a : b
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

        // Move else value into destination first (default if condition is false)
        self.emit_move(else_val, dst_reg, size);

        // Test condition
        let cond_loc = self.get_location(cond);
        match &cond_loc {
            Loc::Reg(r) => {
                // Test register with itself
                self.push_lir(X86Inst::Test {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(*r),
                    dst: GpOperand::Reg(*r),
                });
            }
            Loc::Imm(v) => {
                // Constant condition - just use appropriate value
                if *v != 0 {
                    self.emit_move(then_val, dst_reg, size);
                    if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
                        self.emit_move_to_loc(dst_reg, &dst_loc, size);
                    }
                    return;
                }
                // else_val already in dst_reg
                if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
                    self.emit_move_to_loc(dst_reg, &dst_loc, size);
                }
                return;
            }
            _ => {
                // Load condition to scratch register and test
                self.emit_move(cond, Reg::R11, 64);
                self.push_lir(X86Inst::Test {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::R11),
                    dst: GpOperand::Reg(Reg::R11),
                });
            }
        }

        // Conditional move: if condition is non-zero (NE), use then_val
        // Use R11 for then_val when dst_reg is R10 to avoid clobbering else value
        let then_reg = if dst_reg == Reg::R10 {
            Reg::R11
        } else {
            Reg::R10
        };
        self.emit_move(then_val, then_reg, size);
        self.push_lir(X86Inst::CMov {
            cc: CondCode::Ne,
            size: op_size,
            src: GpOperand::Reg(then_reg),
            dst: dst_reg,
        });

        // Move to final destination if needed
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size);
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

        // Check if this is a FP copy (source or dest is in XMM, is FImm, or type is float)
        let is_fp_copy = matches!(&src_loc, Loc::Xmm(_) | Loc::FImm(..))
            || matches!(&dst_loc, Loc::Xmm(_))
            || typ.is_some_and(|t| types.is_float(t));

        // Check if this is long double (uses x87, not XMM)
        let is_longdouble = typ.is_some_and(|t| types.kind(t) == TypeKind::LongDouble);

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
            // Long double uses x87, not XMM
            if is_longdouble {
                let src_addr = self.get_x87_mem_addr(src);
                self.push_lir(X86Inst::X87Load { addr: src_addr });
                let dst_addr = self.get_x87_mem_addr(dst);
                self.push_lir(X86Inst::X87Store { addr: dst_addr });
            } else {
                // Handle regular FP copy (float/double)
                let dst_xmm = match &dst_loc {
                    Loc::Xmm(x) => *x,
                    _ => XmmReg::Xmm0,
                };

                // Use type-aware size for FP operations
                let fp_size = typ.map(|t| types.size_bits(t)).unwrap_or(reg_size).max(32);
                self.emit_fp_move(src, dst_xmm, fp_size);

                if !matches!(&dst_loc, Loc::Xmm(x) if *x == dst_xmm) {
                    self.emit_fp_move_from_xmm(dst_xmm, &dst_loc, fp_size);
                }
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
        let operand_count = asm_data.outputs.len() + asm_data.inputs.len();
        let mut operand_regs: Vec<Option<Reg>> = Vec::with_capacity(operand_count);
        let mut operand_mem: Vec<Option<String>> = Vec::with_capacity(operand_count);
        let mut operand_names: Vec<Option<String>> = Vec::with_capacity(operand_count);

        // Track which outputs need to be moved from specific registers after asm
        // (output_idx, specific_reg, actual_loc)
        let mut output_moves: Vec<(usize, Reg, Loc)> = Vec::with_capacity(asm_data.outputs.len());

        // Track which inputs need to be moved to specific registers before asm
        // (specific_reg, actual_loc)
        let mut input_moves: Vec<(Reg, Loc)> = Vec::with_capacity(asm_data.inputs.len());

        // Track register remaps: if an allocated reg conflicts with reserved, use temp
        // (original_reg, temp_reg, actual_loc for restore)
        let mut remap_setup: Vec<(Reg, Reg, Loc)> = Vec::with_capacity(operand_count);
        let mut remap_restore: Vec<(Reg, Reg, Loc)> = Vec::with_capacity(operand_count);

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
                if self.needs_got_access(name) {
                    // External symbols on macOS: load address from GOT, then load value
                    self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                        "movq {}@GOTPCREL(%rip), %r11",
                        self.format_symbol_name(name)
                    ))));
                    self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                        "movl (%r11), %{}",
                        self.sized_reg_name(dest_reg, 'k')
                    ))));
                } else {
                    self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                        "movl {}(%rip), %{}",
                        self.format_symbol_name(name),
                        self.sized_reg_name(dest_reg, 'k')
                    ))));
                }
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
                if self.needs_got_access(name) {
                    // External symbols on macOS: load address from GOT, then store
                    self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                        "movq {}@GOTPCREL(%rip), %r11",
                        self.format_symbol_name(name)
                    ))));
                    self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                        "movl %{}, (%r11)",
                        src_name
                    ))));
                } else {
                    self.push_lir(X86Inst::Directive(Directive::Raw(format!(
                        "movl %{}, {}(%rip)",
                        src_name,
                        self.format_symbol_name(name)
                    ))));
                }
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
            Loc::IncomingArg(offset) => {
                format!("{}(%rbp)", offset)
            }
            Loc::Imm(v) => format!("${}", v),
            Loc::Xmm(xmm) => xmm.name().to_string(),
            Loc::FImm(_, _) => {
                // Float immediates not directly usable in inline asm
                panic!("Float immediate not supported in inline asm operand")
            }
            Loc::Global(name) => {
                format!("{}(%rip)", self.format_symbol_name(name))
            }
        }
    }

    /// Format a symbol name with platform-specific prefix
    fn format_symbol_name(&self, name: &str) -> String {
        if self.base.target.os == Os::MacOS && !name.starts_with('.') {
            format!("_{}", name)
        } else {
            name.to_string()
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

    // ========================================================================
    // Atomic Operations (C11 _Atomic support)
    // ========================================================================

    /// Emit atomic load
    /// On x86-64, aligned loads are already atomic - just use regular mov
    fn emit_atomic_load(&mut self, insn: &Instruction, types: &TypeTable) {
        // Atomic load is identical to regular load on x86-64 for aligned data
        // Memory ordering is handled by x86's strong memory model
        self.emit_load(insn, types);
    }

    /// Emit atomic store
    /// On x86-64, aligned stores are atomic. For SeqCst, use XCHG for full barrier.
    fn emit_atomic_store(&mut self, insn: &Instruction, types: &TypeTable) {
        use crate::ir::MemoryOrder;

        let target = insn.target.expect("atomic store needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        // For SeqCst, use XCHG which provides full barrier
        // For weaker orderings, regular store + optional SFENCE is sufficient
        if insn.memory_order == MemoryOrder::SeqCst {
            // Load value into a register
            let value_loc = self.get_location(value);
            let addr_loc = self.get_location(addr);

            // Move value to R10
            self.emit_mov_to_reg(value_loc, Reg::R10, size);

            // Get address into R11
            let mem_addr = match addr_loc {
                Loc::Reg(r) => MemAddr::BaseOffset { base: r, offset: 0 },
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
                    MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: 0,
                    }
                }
                Loc::Global(name) => {
                    let symbol = Symbol::global(self.format_symbol_name(&name));
                    self.push_lir(X86Inst::Lea {
                        addr: MemAddr::RipRelative(symbol),
                        dst: Reg::R11,
                    });
                    MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: 0,
                    }
                }
                _ => {
                    // Handle other cases
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Imm(0),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: 0,
                    }
                }
            };

            // XCHG provides atomic store with full barrier
            self.push_lir(X86Inst::Xchg {
                size: op_size,
                reg: Reg::R10,
                mem: mem_addr,
            });
        } else {
            // For release/relaxed, regular store is sufficient on x86
            self.emit_store(insn, types);
        }

        // Target is void, but we need to assign something
        self.locations.insert(target, Loc::Imm(0));
    }

    /// Emit atomic exchange (swap)
    fn emit_atomic_swap(&mut self, insn: &Instruction, _types: &TypeTable) {
        let target = insn.target.expect("atomic swap needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let value_loc = self.get_location(value);
        let addr_loc = self.get_location(addr);

        // Get address FIRST (before loading value, in case addr is in RAX)
        let mem_addr = self.get_mem_addr_for_atomic(addr_loc);

        // Move new value to RAX (will hold old value after XCHG)
        self.emit_mov_to_reg(value_loc, Reg::Rax, size);

        // XCHG atomically swaps RAX with memory
        self.push_lir(X86Inst::Xchg {
            size: op_size,
            reg: Reg::Rax,
            mem: mem_addr,
        });

        // Result (old value) is in RAX
        self.locations.insert(target, Loc::Reg(Reg::Rax));
    }

    /// Emit atomic compare-and-swap
    fn emit_atomic_cas(&mut self, insn: &Instruction, _types: &TypeTable) {
        let target = insn.target.expect("atomic cas needs target");
        let addr = insn.src[0];
        let expected_ptr = insn.src[1];
        let desired = insn.src[2];
        let size = insn.size; // Element size in bits

        let op_size = OperandSize::from_bits(size);

        let addr_loc = self.get_location(addr);
        let expected_loc = self.get_location(expected_ptr);
        let desired_loc = self.get_location(desired);

        // Load expected_ptr (pointer to expected value) into R9
        // We need this for later store-back on failure
        self.emit_mov_to_reg(expected_loc, Reg::R9, 64);

        // Load address of atomic variable into R11
        self.emit_mov_to_reg(addr_loc, Reg::R11, 64);

        // Load expected value from *expected_ptr (R9) into RAX
        self.push_lir(X86Inst::Mov {
            size: op_size,
            src: GpOperand::Mem(MemAddr::BaseOffset {
                base: Reg::R9,
                offset: 0,
            }),
            dst: GpOperand::Reg(Reg::Rax),
        });

        // Load desired value into R10
        self.emit_mov_to_reg(desired_loc, Reg::R10, size);

        // LOCK CMPXCHG: if *addr == RAX, set *addr = R10 and ZF=1
        //               else RAX = *addr and ZF=0
        self.push_lir(X86Inst::LockCmpxchg {
            size: op_size,
            src: Reg::R10,
            mem: MemAddr::BaseOffset {
                base: Reg::R11,
                offset: 0,
            },
        });

        // SETE stores 1 if ZF=1 (success), 0 otherwise (use R8 to avoid clobbering)
        self.push_lir(X86Inst::SetCC {
            cc: CondCode::Eq,
            dst: Reg::R8,
        });

        // On failure, store RAX (actual value) to *expected (R9 has expected_ptr)
        let label_suffix = self.unique_label_counter;
        self.unique_label_counter += 1;
        let skip_label = Label::new("cas_done", label_suffix);
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Eq, // Jump if equal (success)
            target: skip_label.clone(),
        });
        // Failed: store actual value to *expected
        self.push_lir(X86Inst::Mov {
            size: op_size,
            src: GpOperand::Reg(Reg::Rax),
            dst: GpOperand::Mem(MemAddr::BaseOffset {
                base: Reg::R9,
                offset: 0,
            }),
        });
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(skip_label)));

        // Zero-extend result (0 or 1) to full register
        self.push_lir(X86Inst::Movzx {
            src_size: OperandSize::B8,
            dst_size: OperandSize::B32,
            src: GpOperand::Reg(Reg::R8),
            dst: Reg::Rax,
        });

        // Result (success flag) is in RAX
        self.locations.insert(target, Loc::Reg(Reg::Rax));
    }

    /// Emit atomic fetch-and-add
    fn emit_atomic_fetch_add(&mut self, insn: &Instruction, _types: &TypeTable) {
        let target = insn.target.expect("atomic fetch_add needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let value_loc = self.get_location(value);
        let addr_loc = self.get_location(addr);

        // Get address FIRST (before loading value, in case addr is in RAX)
        let mem_addr = self.get_mem_addr_for_atomic(addr_loc);

        // Move value to RAX
        self.emit_mov_to_reg(value_loc, Reg::Rax, size);

        // LOCK XADD: atomically adds RAX to *addr, returns old value in RAX
        self.push_lir(X86Inst::LockXadd {
            size: op_size,
            reg: Reg::Rax,
            mem: mem_addr,
        });

        // Result (old value) is in RAX
        self.locations.insert(target, Loc::Reg(Reg::Rax));
    }

    /// Emit atomic fetch-and-subtract
    fn emit_atomic_fetch_sub(&mut self, insn: &Instruction, _types: &TypeTable) {
        let target = insn.target.expect("atomic fetch_sub needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let value_loc = self.get_location(value);
        let addr_loc = self.get_location(addr);

        // Get address FIRST (before loading value, in case addr is in RAX)
        let mem_addr = self.get_mem_addr_for_atomic(addr_loc);

        // Negate value: sub is add of negative
        self.emit_mov_to_reg(value_loc, Reg::Rax, size);
        self.push_lir(X86Inst::Neg {
            size: op_size,
            dst: Reg::Rax,
        });

        // LOCK XADD with negated value
        self.push_lir(X86Inst::LockXadd {
            size: op_size,
            reg: Reg::Rax,
            mem: mem_addr,
        });

        // Result (old value) is in RAX
        self.locations.insert(target, Loc::Reg(Reg::Rax));
    }

    /// Emit atomic fetch-and-and
    fn emit_atomic_fetch_and(&mut self, insn: &Instruction, _types: &TypeTable) {
        self.emit_atomic_fetch_bitop(insn, AtomicBitOp::And);
    }

    /// Emit atomic fetch-and-or
    fn emit_atomic_fetch_or(&mut self, insn: &Instruction, _types: &TypeTable) {
        self.emit_atomic_fetch_bitop(insn, AtomicBitOp::Or);
    }

    /// Emit atomic fetch-and-xor
    fn emit_atomic_fetch_xor(&mut self, insn: &Instruction, _types: &TypeTable) {
        self.emit_atomic_fetch_bitop(insn, AtomicBitOp::Xor);
    }

    /// Helper for atomic fetch bitwise operations (AND, OR, XOR)
    /// Uses CMPXCHG loop since x86 doesn't have LOCK AND/OR/XOR that return old value
    fn emit_atomic_fetch_bitop(&mut self, insn: &Instruction, op: AtomicBitOp) {
        let target = insn.target.expect("atomic fetch needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let value_loc = self.get_location(value);
        let addr_loc = self.get_location(addr);

        // Get address into R11
        let mem_addr = self.get_mem_addr_for_atomic(addr_loc);

        // Move operand value to R10
        self.emit_mov_to_reg(value_loc, Reg::R10, size);

        // Load current value into RAX
        self.push_lir(X86Inst::Mov {
            size: op_size,
            src: GpOperand::Mem(mem_addr.clone()),
            dst: GpOperand::Reg(Reg::Rax),
        });

        // Loop label
        let label_suffix = self.unique_label_counter;
        self.unique_label_counter += 1;
        let loop_label = Label::new("atomic_bitop", label_suffix);
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(
            loop_label.clone(),
        )));

        // Copy old value to RCX for computing new value
        self.push_lir(X86Inst::Mov {
            size: op_size,
            src: GpOperand::Reg(Reg::Rax),
            dst: GpOperand::Reg(Reg::Rcx),
        });

        // Apply bitwise operation: RCX = RCX op R10
        match op {
            AtomicBitOp::And => {
                self.push_lir(X86Inst::And {
                    size: op_size,
                    src: GpOperand::Reg(Reg::R10),
                    dst: Reg::Rcx,
                });
            }
            AtomicBitOp::Or => {
                self.push_lir(X86Inst::Or {
                    size: op_size,
                    src: GpOperand::Reg(Reg::R10),
                    dst: Reg::Rcx,
                });
            }
            AtomicBitOp::Xor => {
                self.push_lir(X86Inst::Xor {
                    size: op_size,
                    src: GpOperand::Reg(Reg::R10),
                    dst: Reg::Rcx,
                });
            }
        }

        // LOCK CMPXCHG: if *addr == RAX, set *addr = RCX
        self.push_lir(X86Inst::LockCmpxchg {
            size: op_size,
            src: Reg::Rcx,
            mem: mem_addr,
        });

        // If failed (ZF=0), retry - RAX now has actual value
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Ne,
            target: loop_label,
        });

        // Result (old value) is in RAX
        self.locations.insert(target, Loc::Reg(Reg::Rax));
    }

    /// Emit memory fence
    fn emit_fence(&mut self, insn: &Instruction) {
        use crate::ir::MemoryOrder;

        let target = insn.target.expect("fence needs target");

        // Emit appropriate fence based on memory ordering
        match insn.memory_order {
            MemoryOrder::SeqCst | MemoryOrder::AcqRel => {
                self.push_lir(X86Inst::Mfence);
            }
            MemoryOrder::Acquire | MemoryOrder::Consume => {
                // LFENCE - but x86 loads have acquire semantics anyway
                self.push_lir(X86Inst::Lfence);
            }
            MemoryOrder::Release => {
                // SFENCE - but x86 stores have release semantics anyway
                self.push_lir(X86Inst::Sfence);
            }
            MemoryOrder::Relaxed => {
                // No fence needed for relaxed
            }
        }

        self.locations.insert(target, Loc::Imm(0));
    }

    /// Helper to move a value to a register
    fn emit_mov_to_reg(&mut self, loc: Loc, reg: Reg, size: u32) {
        let op_size = OperandSize::from_bits(size);
        match loc {
            Loc::Reg(src) => {
                if src != reg {
                    self.push_lir(X86Inst::Mov {
                        size: op_size,
                        src: GpOperand::Reg(src),
                        dst: GpOperand::Reg(reg),
                    });
                }
            }
            Loc::Imm(v) => {
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Imm(v),
                    dst: GpOperand::Reg(reg),
                });
            }
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Mem(MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }),
                    dst: GpOperand::Reg(reg),
                });
            }
            Loc::Global(name) => {
                let symbol = Symbol::global(self.format_symbol_name(&name));
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Mem(MemAddr::RipRelative(symbol)),
                    dst: GpOperand::Reg(reg),
                });
            }
            _ => {
                // Default: load 0
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Imm(0),
                    dst: GpOperand::Reg(reg),
                });
            }
        }
    }

    /// Helper to get memory address for atomic operations
    /// Always copies the address to R11 to avoid conflicts with RAX used for values
    fn get_mem_addr_for_atomic(&mut self, loc: Loc) -> MemAddr {
        match loc {
            Loc::Reg(r) => {
                // Always copy to R11 to avoid conflicts when RAX is used for values
                if r != Reg::R11 {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                }
                MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: 0,
                }
            }
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                // Load address into R11
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    },
                    dst: Reg::R11,
                });
                MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: 0,
                }
            }
            Loc::Global(name) => {
                let symbol = Symbol::global(self.format_symbol_name(&name));
                self.push_lir(X86Inst::Lea {
                    addr: MemAddr::RipRelative(symbol),
                    dst: Reg::R11,
                });
                MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: 0,
                }
            }
            _ => {
                // Load zero address into R11 (this shouldn't happen)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Imm(0),
                    dst: GpOperand::Reg(Reg::R11),
                });
                MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: 0,
                }
            }
        }
    }
}

/// Helper enum for atomic bitwise operations
#[derive(Clone, Copy)]
enum AtomicBitOp {
    And,
    Or,
    Xor,
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
        self.extern_symbols = module.extern_symbols.clone();

        // Emit file header
        self.emit_header();

        // Emit .file directives unconditionally (useful for diagnostics/profiling)
        // Use "." as placeholder for empty paths (synthetic files like <paste>)
        // to keep file numbers sequential for .loc directives
        for (i, path) in module.source_files.iter().enumerate() {
            let file_path = if path.is_empty() {
                ".".to_string()
            } else {
                path.clone()
            };
            // File indices in DWARF start at 1
            self.base
                .push_directive(Directive::file((i + 1) as u32, file_path));
        }

        // Emit globals
        for (name, typ, init) in &module.globals {
            self.emit_global(name, typ, init, types);
        }

        // Emit string literals
        if !module.strings.is_empty() {
            self.base.emit_strings(&module.strings);
        }

        // Emit wide string literals
        if !module.wide_strings.is_empty() {
            self.base.emit_wide_strings(&module.wide_strings);
        }

        // Emit text start label for DWARF debug info (before first function)
        if module.debug && !module.functions.is_empty() {
            self.base.push_directive(Directive::local_label(".Ltext0"));
        }

        // Emit functions
        for func in &module.functions {
            self.emit_function(func, types);
        }

        // Emit text end label for DWARF debug info (after last function)
        if module.debug && !module.functions.is_empty() {
            self.base
                .push_directive(Directive::local_label(".Ltext_end"));
        }

        // Emit long double constants collected during codegen
        if !self.ld_constants.is_empty() {
            self.emit_ld_constants();
        }

        // Emit double constants collected during x87 conversions
        if !self.double_constants.is_empty() {
            self.emit_double_constants();
        }

        // Generate DWARF debug sections if debug mode is enabled
        if module.debug {
            let producer = format!("pcc {}", env!("CARGO_PKG_VERSION"));
            let source_name = module.source_name.as_deref().unwrap_or("unknown");
            let comp_dir = module.comp_dir.as_deref().unwrap_or(".");

            // Only reference text labels if we have code (functions)
            // Data-only files use 0 for low_pc/high_pc
            let (low_pc, high_pc) = if module.functions.is_empty() {
                (None, None)
            } else {
                (Some(".Ltext0"), Some(".Ltext_end"))
            };

            super::super::dwarf::generate_abbrev_table(&mut self.base);
            super::super::dwarf::generate_debug_info(
                &mut self.base,
                &producer,
                source_name,
                comp_dir,
                low_pc,
                high_pc,
            );
        }

        // Emit .note.GNU-stack section to mark stack as non-executable (ELF only)
        // This prevents the "missing .note.GNU-stack section" linker warning
        // Used on Linux, FreeBSD, and other ELF platforms (not macOS which uses Mach-O)
        if !matches!(self.base.target.os, Os::MacOS) {
            self.base.push_directive(Directive::Raw(
                ".section .note.GNU-stack,\"\",@progbits".into(),
            ));
        }

        // Emit all buffered LIR instructions to output string
        self.base.emit_all();

        self.base.output.clone()
    }

    fn set_emit_unwind_tables(&mut self, emit: bool) {
        self.base.emit_unwind_tables = emit;
    }

    fn set_pic_mode(&mut self, pic: bool) {
        self.pic_mode = pic;
    }
}
