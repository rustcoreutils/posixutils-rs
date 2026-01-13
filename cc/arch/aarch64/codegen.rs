//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 Code Generator
// Converts IR to AArch64 assembly
//
// Uses linear scan register allocation and AAPCS64 calling convention.
//
// Stack Pointer Policy: All stack variable accesses use the frame pointer (X29/FP)
// rather than the stack pointer (SP). This is required for alloca support, since
// alloca modifies SP at runtime, invalidating SP-relative offsets. SP is only used
// for prologue/epilogue, call stack argument passing, and alloca itself.
//

use crate::arch::aarch64::lir::{Aarch64Inst, GpOperand, MemAddr};
use crate::arch::aarch64::regalloc::{Loc, Reg, RegAlloc, VReg};
use crate::arch::codegen::{is_variadic_function, BswapSize, CodeGenBase, CodeGenerator, UnaryOp};
use crate::arch::lir::{complex_fp_info, CondCode, Directive, FpSize, Label, OperandSize, Symbol};
use crate::ir::{Function, Instruction, Module, Opcode, Pseudo, PseudoId, PseudoKind};
use crate::target::{Os, Target};
use crate::types::{TypeId, TypeKind, TypeTable};
use std::collections::{HashMap, HashSet};

// ============================================================================
// AArch64 Code Generator
// ============================================================================

/// AArch64 code generator
pub struct Aarch64CodeGen {
    /// Common code generation infrastructure
    pub(super) base: CodeGenBase<Aarch64Inst>,
    /// Current function's register allocation
    locations: HashMap<PseudoId, Loc>,
    /// Current function's pseudos (for looking up values)
    pub(super) pseudos: Vec<Pseudo>,
    /// Total frame size for current function
    frame_size: i32,
    /// Size of callee-saved register area (for computing local variable offsets)
    callee_saved_size: i32,
    /// Offset from FP to register save area (for variadic functions)
    pub(super) reg_save_area_offset: i32,
    /// Size of register save area (for variadic functions)
    /// Used to compute correct FP-relative offsets for local variables
    pub(super) reg_save_area_size: i32,
    /// Number of fixed GP parameters (for variadic functions)
    pub(super) num_fixed_gp_params: usize,
    /// External symbols (need GOT access on macOS)
    pub(super) extern_symbols: HashSet<String>,
    /// Position-independent code mode (for shared libraries)
    pic_mode: bool,
}

/// Result of computing a memory address for load/store operations
enum ComputedAddr {
    /// Direct memory address - no setup instructions needed
    Direct(MemAddr),
    /// Memory address after emitting setup instructions (e.g., loading spilled address)
    WithSetup(MemAddr),
    /// Global symbol - needs special handling (different for load vs store)
    Global(String),
}

impl Aarch64CodeGen {
    pub fn new(target: Target) -> Self {
        Self {
            base: CodeGenBase::new(target),
            locations: HashMap::new(),
            pseudos: Vec::new(),
            frame_size: 0,
            callee_saved_size: 0,
            reg_save_area_offset: 0,
            reg_save_area_size: 0,
            num_fixed_gp_params: 0,
            extern_symbols: HashSet::new(),
            pic_mode: false,
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

    /// Compute the actual FP-relative offset for a stack location.
    /// For local variables (negative offsets), this accounts for the
    /// register save area in varargs functions which is placed at the
    /// end of the frame (after locals).
    #[inline]
    pub(super) fn stack_offset(&self, frame_size: i32, offset: i32) -> i32 {
        if offset < 0 {
            // Local variable: use frame size minus reg_save_area
            // Layout: [fp/lr][callee-saved][locals][reg_save_area]
            // Locals are at offsets from (frame_size - reg_save_area_size)
            (frame_size - self.reg_save_area_size) + offset
        } else {
            // Positive offset = stack args (passed by caller)
            // regalloc uses 16 as base (x86_64 convention: saved rbp + return addr)
            // but aarch64 places stack args at [x29 + frame_size + slot_offset]
            // where slot_offset = offset - 16
            frame_size + offset - 16
        }
    }

    /// Push a LIR instruction to the buffer (deferred emission)
    #[inline]
    pub(super) fn push_lir(&mut self, inst: Aarch64Inst) {
        self.base.push_lir(inst);
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

    fn emit_function(&mut self, func: &Function, types: &TypeTable) {
        // Check if this function uses varargs
        let is_variadic = is_variadic_function(func);

        // Register allocation
        let mut alloc = RegAlloc::new();
        self.locations = alloc.allocate(func, types);
        self.pseudos = func.pseudos.clone();

        let stack_size = alloc.stack_size();
        let callee_saved = alloc.callee_saved_used().to_vec();
        let callee_saved_fp = alloc.callee_saved_fp_used().to_vec();

        // For variadic functions on Linux/FreeBSD, we need extra space for the register save area
        // AAPCS64: 8 GP regs (x0-x7) * 8 bytes = 64 bytes
        // On Darwin (macOS/iOS), variadic args are passed on the stack by the caller,
        // so we don't need a register save area.
        let is_darwin = self.base.target.os == crate::target::Os::MacOS;
        let reg_save_area_size: i32 = if is_variadic && !is_darwin { 64 } else { 0 };

        // Calculate total frame size
        // Need space for: fp/lr (16 bytes) + GP callee-saved + FP callee-saved + local vars + reg save area
        // Round up callee-saved counts to even for 16-byte alignment
        // Note: AAPCS64 only requires the lower 64 bits of V8-V15 to be preserved (d8-d15)
        let callee_saved_gp_pairs = callee_saved.len().div_ceil(2);
        let callee_saved_gp_size = callee_saved_gp_pairs as i32 * 16;
        let callee_saved_fp_pairs = callee_saved_fp.len().div_ceil(2);
        let callee_saved_fp_size = callee_saved_fp_pairs as i32 * 16; // 8 bytes per d-reg, 16 per pair
        let callee_saved_size = callee_saved_gp_size + callee_saved_fp_size;
        let total_frame = 16 + callee_saved_size + stack_size + reg_save_area_size;
        // Ensure 16-byte alignment
        let total_frame = (total_frame + 15) & !15;

        // Track register save area offset for va_start (offset from FP)
        // Layout: [fp/lr][GP callee-saved][FP callee-saved][locals][reg_save_area]
        // The save area is at FP + 16 + callee_saved_size + stack_size
        self.reg_save_area_offset = if is_variadic {
            16 + callee_saved_size + stack_size
        } else {
            0
        };

        // Save function name, frame size, and callee-saved size for label generation and offset calculation
        self.base.current_fn = func.name.clone();
        self.frame_size = total_frame;
        self.callee_saved_size = callee_saved_size;
        self.reg_save_area_size = reg_save_area_size;

        // Emit function header (directives, label, CFI start)
        self.emit_function_header(func.is_static, &func.name);

        // Emit prologue (save fp/lr, callee-saved regs, allocate stack)
        self.emit_prologue(total_frame, &callee_saved, &callee_saved_fp);

        // Detect sret and store X8 to stack if needed
        let has_sret = func
            .pseudos
            .iter()
            .any(|p| matches!(p.kind, PseudoKind::Arg(0)) && p.name.as_deref() == Some("__sret"));
        if has_sret {
            self.store_sret_if_needed(func, total_frame);
        }

        // For variadic functions on Linux/FreeBSD, save argument registers
        if is_variadic && !is_darwin {
            self.emit_variadic_save_area();
        }

        // Count fixed GP parameters for va_start
        if is_variadic {
            self.num_fixed_gp_params = func
                .params
                .iter()
                .filter(|(_, typ)| !types.is_float(*typ))
                .count();
        }

        // Store spilled arguments before any calls can clobber them
        self.store_spilled_args(&alloc, total_frame);

        // Move arguments from registers to their allocated stack locations
        self.store_args_to_stack(func, types, &alloc, total_frame);

        // Store frame size for epilogue
        let frame_info = (total_frame, callee_saved.clone(), callee_saved_fp.clone());

        // Emit basic blocks
        for block in &func.blocks {
            self.emit_block(block, &frame_info, types);
        }

        // CFI: End procedure
        if self.base.emit_unwind_tables {
            self.push_lir(Aarch64Inst::Directive(Directive::CfiEndProc));
        }
    }

    /// Emit function header directives (text section, visibility, alignment, label, CFI start)
    fn emit_function_header(&mut self, is_static: bool, name: &str) {
        self.push_lir(Aarch64Inst::Directive(Directive::Blank));
        self.push_lir(Aarch64Inst::Directive(Directive::Text));

        // Skip .globl for static functions (internal linkage)
        if !is_static {
            self.push_lir(Aarch64Inst::Directive(Directive::global(name)));
        }

        // ELF-only type (handled by Directive::emit which skips on macOS)
        self.push_lir(Aarch64Inst::Directive(Directive::type_func(name)));

        // Alignment
        self.push_lir(Aarch64Inst::Directive(Directive::Align(2)));

        // Function label
        self.push_lir(Aarch64Inst::Directive(Directive::global_label(name)));

        // CFI: Start procedure (enables stack unwinding for this function)
        if self.base.emit_unwind_tables {
            self.push_lir(Aarch64Inst::Directive(Directive::CfiStartProc));
        }
    }

    /// Emit prologue: save fp/lr, allocate stack, save callee-saved registers
    fn emit_prologue(&mut self, total_frame: i32, callee_saved: &[Reg], callee_saved_fp: &[VReg]) {
        let fp = Reg::fp();
        let lr = Reg::lr();

        if total_frame > 0 {
            // Combined push and allocate: stp x29, x30, [sp, #-N]!
            self.push_lir(Aarch64Inst::Stp {
                size: OperandSize::B64,
                src1: fp,
                src2: lr,
                addr: MemAddr::PreIndex {
                    base: Reg::SP,
                    offset: -total_frame,
                },
            });
            if self.base.emit_debug {
                // CFA is now at sp + total_frame (previous SP value)
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_def_cfa(
                    "sp",
                    total_frame,
                )));
                // x29 (fp) is saved at [sp+0], x30 (lr) is saved at [sp+8]
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_offset(
                    "x29",
                    -total_frame,
                )));
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_offset(
                    "x30",
                    -(total_frame - 8),
                )));
            }
            // Set up frame pointer: mov x29, sp
            self.push_lir(Aarch64Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::SP),
                dst: fp,
            });
            if self.base.emit_debug {
                // CFA is now tracked by x29 + total_frame
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_def_cfa_register(
                    "x29",
                )));
            }

            // Save callee-saved GP registers in pairs
            self.save_callee_saved_gp_regs(total_frame, callee_saved);

            // Save callee-saved FP registers in pairs
            let gp_offset = 16 + (callee_saved.len().div_ceil(2) as i32 * 16);
            self.save_callee_saved_fp_regs(total_frame, callee_saved_fp, gp_offset);
        } else {
            // Minimal frame: stp x29, x30, [sp, #-16]!
            self.push_lir(Aarch64Inst::Stp {
                size: OperandSize::B64,
                src1: fp,
                src2: lr,
                addr: MemAddr::PreIndex {
                    base: Reg::SP,
                    offset: -16,
                },
            });
            if self.base.emit_debug {
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_def_cfa("sp", 16)));
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_offset("x29", -16)));
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_offset("x30", -8)));
            }
            // mov x29, sp
            self.push_lir(Aarch64Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::SP),
                dst: fp,
            });
            if self.base.emit_debug {
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_def_cfa_register(
                    "x29",
                )));
            }
        }
    }

    /// Save callee-saved GP registers in pairs (or single if odd count)
    fn save_callee_saved_gp_regs(&mut self, total_frame: i32, callee_saved: &[Reg]) {
        let mut offset = 16; // Start after fp/lr
        let mut i = 0;
        while i < callee_saved.len() {
            if i + 1 < callee_saved.len() {
                self.push_lir(Aarch64Inst::Stp {
                    size: OperandSize::B64,
                    src1: callee_saved[i],
                    src2: callee_saved[i + 1],
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29, // fp
                        offset,
                    },
                });
                if self.base.emit_debug {
                    let cfi_offset1 = -(total_frame - offset);
                    let cfi_offset2 = -(total_frame - offset - 8);
                    self.push_lir(Aarch64Inst::Directive(Directive::cfi_offset(
                        callee_saved[i].name64(),
                        cfi_offset1,
                    )));
                    self.push_lir(Aarch64Inst::Directive(Directive::cfi_offset(
                        callee_saved[i + 1].name64(),
                        cfi_offset2,
                    )));
                }
                i += 2;
            } else {
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: callee_saved[i],
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29, // fp
                        offset,
                    },
                });
                if self.base.emit_debug {
                    let cfi_offset = -(total_frame - offset);
                    self.push_lir(Aarch64Inst::Directive(Directive::cfi_offset(
                        callee_saved[i].name64(),
                        cfi_offset,
                    )));
                }
                i += 1;
            }
            offset += 16;
        }
    }

    /// Save callee-saved FP registers (d8-d15) in pairs
    fn save_callee_saved_fp_regs(
        &mut self,
        total_frame: i32,
        callee_saved_fp: &[VReg],
        start_offset: i32,
    ) {
        let mut offset = start_offset;
        let mut i = 0;
        while i < callee_saved_fp.len() {
            if i + 1 < callee_saved_fp.len() {
                self.push_lir(Aarch64Inst::StpFp {
                    size: FpSize::Double,
                    src1: callee_saved_fp[i],
                    src2: callee_saved_fp[i + 1],
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29, // fp
                        offset,
                    },
                });
                if self.base.emit_debug {
                    let cfi_offset1 = -(total_frame - offset);
                    let cfi_offset2 = -(total_frame - offset - 8);
                    self.push_lir(Aarch64Inst::Directive(Directive::cfi_offset(
                        callee_saved_fp[i].name_d(),
                        cfi_offset1,
                    )));
                    self.push_lir(Aarch64Inst::Directive(Directive::cfi_offset(
                        callee_saved_fp[i + 1].name_d(),
                        cfi_offset2,
                    )));
                }
                i += 2;
            } else {
                self.push_lir(Aarch64Inst::StrFp {
                    size: FpSize::Double,
                    src: callee_saved_fp[i],
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29, // fp
                        offset,
                    },
                });
                if self.base.emit_debug {
                    let cfi_offset = -(total_frame - offset);
                    self.push_lir(Aarch64Inst::Directive(Directive::cfi_offset(
                        callee_saved_fp[i].name_d(),
                        cfi_offset,
                    )));
                }
                i += 1;
            }
            offset += 16;
        }
    }

    /// Store sret pointer to stack if needed (for large struct returns via X8)
    fn store_sret_if_needed(&mut self, func: &Function, total_frame: i32) {
        if let Some(sret) = func
            .pseudos
            .iter()
            .find(|p| matches!(p.kind, PseudoKind::Arg(0)) && p.name.as_deref() == Some("__sret"))
        {
            if let Some(Loc::Stack(offset)) = self.locations.get(&sret.id) {
                if *offset < 0 {
                    let actual_offset = self.stack_offset(total_frame, *offset);
                    self.push_lir(Aarch64Inst::Str {
                        size: OperandSize::B64,
                        src: Reg::X8,
                        addr: MemAddr::BaseOffset {
                            base: Reg::X29, // fp
                            offset: actual_offset,
                        },
                    });
                }
            }
        }
    }

    /// Save argument registers to the register save area for variadic functions (Linux/FreeBSD)
    fn emit_variadic_save_area(&mut self) {
        // AAPCS64: x0-x7 are saved at reg_save_area_offset from FP
        let arg_regs = Reg::arg_regs();
        for (i, reg) in arg_regs.iter().enumerate() {
            // Each register at offset: reg_save_area_offset + (i * 8)
            let offset = self.reg_save_area_offset + (i as i32 * 8);
            self.push_lir(Aarch64Inst::Str {
                size: OperandSize::B64,
                src: *reg,
                addr: MemAddr::BaseOffset {
                    base: Reg::X29, // fp
                    offset,
                },
            });
        }
    }

    /// Emit stores for arguments spilled from caller-saved registers to stack
    fn store_spilled_args(&mut self, alloc: &RegAlloc, total_frame: i32) {
        for spilled in alloc.spilled_args() {
            // spilled.to_stack_offset is negative (e.g., -8, -16, etc.)
            // Convert to FP-relative offset
            let actual_offset = self.stack_offset(total_frame, spilled.to_stack_offset);
            if let Some(gp_reg) = spilled.from_gp_reg {
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: gp_reg,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29, // fp
                        offset: actual_offset,
                    },
                });
            } else if let Some(fp_reg) = spilled.from_fp_reg {
                self.push_lir(Aarch64Inst::StrFp {
                    size: FpSize::Double,
                    src: fp_reg,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29, // fp
                        offset: actual_offset,
                    },
                });
            }
        }
    }

    /// Move arguments from registers to their allocated stack locations
    fn store_args_to_stack(
        &mut self,
        func: &Function,
        types: &TypeTable,
        alloc: &RegAlloc,
        total_frame: i32,
    ) {
        // AAPCS64: integer args in X0-X7, FP args in D0-D7 (separate counters)
        // Note: sret uses X8, so regular args still start at X0
        // Complex parameters use two consecutive FP registers (D0+D1, D2+D3, etc.)
        let arg_regs = Reg::arg_regs();
        let fp_arg_regs = VReg::arg_regs();
        let mut int_arg_idx = 0;
        let mut fp_arg_idx = 0;

        // Track which pseudos were already spilled via spill_args_across_calls
        let spilled_pseudos: HashSet<PseudoId> =
            alloc.spilled_args().iter().map(|s| s.pseudo).collect();

        // Detect sret for arg_idx offset
        let has_sret = func
            .pseudos
            .iter()
            .any(|p| matches!(p.kind, PseudoKind::Arg(0)) && p.name.as_deref() == Some("__sret"));
        let arg_idx_offset: u32 = if has_sret { 1 } else { 0 };

        for (i, (_name, typ)) in func.params.iter().enumerate() {
            let is_complex = types.is_complex(*typ);
            let is_fp = types.is_float(*typ);

            // Find the pseudo for this argument
            for pseudo in &func.pseudos {
                if let PseudoKind::Arg(arg_idx) = pseudo.kind {
                    // With sret, params have arg_idx = i + 1, but still use arg_regs[i]
                    if arg_idx == (i as u32) + arg_idx_offset {
                        // Skip pseudos already stored via spilled_args
                        if spilled_pseudos.contains(&pseudo.id) {
                            // Still need to count this arg for register assignment tracking
                            if is_complex {
                                fp_arg_idx += 2;
                            } else if is_fp {
                                fp_arg_idx += 1;
                            } else {
                                int_arg_idx += 1;
                            }
                            break;
                        }
                        if is_complex {
                            // Complex argument - uses TWO consecutive FP registers
                            if fp_arg_idx + 1 < fp_arg_regs.len() {
                                let param_name = &func.params[i].0;
                                if let Some(local) = func.locals.get(param_name) {
                                    if let Some(Loc::Stack(offset)) = self.locations.get(&local.sym)
                                    {
                                        let actual_offset = self.stack_offset(total_frame, *offset);
                                        let (fp_size, imag_offset) =
                                            complex_fp_info(types, &self.base.target, *typ);
                                        // Store real part from first FP register
                                        self.push_lir(Aarch64Inst::StrFp {
                                            size: fp_size,
                                            src: fp_arg_regs[fp_arg_idx],
                                            addr: MemAddr::BaseOffset {
                                                base: Reg::X29,
                                                offset: actual_offset,
                                            },
                                        });
                                        // Store imag part from second FP register
                                        self.push_lir(Aarch64Inst::StrFp {
                                            size: fp_size,
                                            src: fp_arg_regs[fp_arg_idx + 1],
                                            addr: MemAddr::BaseOffset {
                                                base: Reg::X29,
                                                offset: actual_offset + imag_offset,
                                            },
                                        });
                                    }
                                }
                            }
                            fp_arg_idx += 2;
                        } else if is_fp {
                            // FP argument
                            if fp_arg_idx < fp_arg_regs.len() {
                                if let Some(Loc::Stack(offset)) = self.locations.get(&pseudo.id) {
                                    if *offset < 0 {
                                        let actual_offset = self.stack_offset(total_frame, *offset);
                                        let fp_size = if types.size_bits(*typ) == 32 {
                                            FpSize::Single
                                        } else {
                                            FpSize::Double
                                        };
                                        self.push_lir(Aarch64Inst::StrFp {
                                            size: fp_size,
                                            src: fp_arg_regs[fp_arg_idx],
                                            addr: MemAddr::BaseOffset {
                                                base: Reg::X29,
                                                offset: actual_offset,
                                            },
                                        });
                                    }
                                }
                            }
                            fp_arg_idx += 1;
                        } else {
                            // GP argument
                            if int_arg_idx < arg_regs.len() {
                                if let Some(Loc::Stack(offset)) = self.locations.get(&pseudo.id) {
                                    // Move from arg register to stack
                                    if *offset < 0 {
                                        let actual_offset = self.stack_offset(total_frame, *offset);
                                        self.push_lir(Aarch64Inst::Str {
                                            size: OperandSize::B64,
                                            src: arg_regs[int_arg_idx],
                                            addr: MemAddr::BaseOffset {
                                                base: Reg::X29, // fp
                                                offset: actual_offset,
                                            },
                                        });
                                    }
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

    fn emit_block(
        &mut self,
        block: &crate::ir::BasicBlock,
        frame_info: &(i32, Vec<Reg>, Vec<VReg>),
        types: &TypeTable,
    ) {
        // Always emit block ID label for consistency with jumps
        // (jumps reference blocks by ID, not by C label name)
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(Label::new(
            &self.base.current_fn,
            block.id.0,
        ))));

        // Emit instructions
        for insn in &block.insns {
            self.emit_insn(insn, frame_info, types);
        }
    }

    /// Emit return instruction: move return value and emit epilogue
    fn emit_ret(
        &mut self,
        insn: &Instruction,
        total_frame: i32,
        callee_saved: &[Reg],
        callee_saved_fp: &[VReg],
        types: &TypeTable,
    ) {
        // Move return value to x0 (integer), v0 (float), or v0+v1 (complex) if present
        if let Some(&src) = insn.src.first() {
            let src_loc = self.get_location(src);
            let is_complex = insn.typ.is_some_and(|t| types.is_complex(t));
            let is_fp = matches!(src_loc, Loc::VReg(_) | Loc::FImm(..));

            if insn.is_two_reg_return {
                self.emit_move(src, Reg::X0, 64, total_frame);
                if let Some(&src2) = insn.src.get(1) {
                    self.emit_move(src2, Reg::X1, 64, total_frame);
                }
            } else if is_complex {
                let (fp_size, imag_offset) =
                    complex_fp_info(types, &self.base.target, insn.typ.unwrap());
                match src_loc {
                    Loc::Stack(offset) => {
                        let actual_offset = self.stack_offset(total_frame, offset);
                        self.push_lir(Aarch64Inst::Ldr {
                            size: OperandSize::B64,
                            dst: Reg::X9,
                            addr: MemAddr::BaseOffset {
                                base: Reg::X29,
                                offset: actual_offset,
                            },
                        });
                        self.push_lir(Aarch64Inst::LdrFp {
                            size: fp_size,
                            dst: VReg::V0,
                            addr: MemAddr::BaseOffset {
                                base: Reg::X9,
                                offset: 0,
                            },
                        });
                        self.push_lir(Aarch64Inst::LdrFp {
                            size: fp_size,
                            dst: VReg::V1,
                            addr: MemAddr::BaseOffset {
                                base: Reg::X9,
                                offset: imag_offset,
                            },
                        });
                    }
                    Loc::Reg(r) => {
                        self.push_lir(Aarch64Inst::LdrFp {
                            size: fp_size,
                            dst: VReg::V0,
                            addr: MemAddr::BaseOffset { base: r, offset: 0 },
                        });
                        self.push_lir(Aarch64Inst::LdrFp {
                            size: fp_size,
                            dst: VReg::V1,
                            addr: MemAddr::BaseOffset {
                                base: r,
                                offset: imag_offset,
                            },
                        });
                    }
                    _ => {}
                }
            } else if is_fp {
                self.emit_fp_move(src, VReg::V0, insn.typ, insn.size, total_frame, types);
            } else {
                self.emit_move(src, Reg::X0, insn.size, total_frame);
            }
        }

        // Epilogue: reset SP to FP
        self.push_lir(Aarch64Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::X29),
            dst: Reg::SP,
        });

        // Restore callee-saved registers
        if total_frame > 16 {
            let mut offset = 16;
            let mut i = 0;
            while i < callee_saved.len() {
                if i + 1 < callee_saved.len() {
                    self.push_lir(Aarch64Inst::Ldp {
                        size: OperandSize::B64,
                        addr: MemAddr::BaseOffset {
                            base: Reg::sp(),
                            offset,
                        },
                        dst1: callee_saved[i],
                        dst2: callee_saved[i + 1],
                    });
                    i += 2;
                } else {
                    self.push_lir(Aarch64Inst::Ldr {
                        size: OperandSize::B64,
                        addr: MemAddr::BaseOffset {
                            base: Reg::sp(),
                            offset,
                        },
                        dst: callee_saved[i],
                    });
                    i += 1;
                }
                offset += 16;
            }

            // Restore FP callee-saved registers (d8-d15)
            let mut i = 0;
            while i < callee_saved_fp.len() {
                if i + 1 < callee_saved_fp.len() {
                    self.push_lir(Aarch64Inst::LdpFp {
                        size: FpSize::Double,
                        addr: MemAddr::BaseOffset {
                            base: Reg::sp(),
                            offset,
                        },
                        dst1: callee_saved_fp[i],
                        dst2: callee_saved_fp[i + 1],
                    });
                    i += 2;
                } else {
                    self.push_lir(Aarch64Inst::LdrFp {
                        size: FpSize::Double,
                        addr: MemAddr::BaseOffset {
                            base: Reg::sp(),
                            offset,
                        },
                        dst: callee_saved_fp[i],
                    });
                    i += 1;
                }
                offset += 16;
            }
        }

        // Restore fp/lr and deallocate stack
        let dealloc = if total_frame > 0 { total_frame } else { 16 };
        self.push_lir(Aarch64Inst::Ldp {
            size: OperandSize::B64,
            addr: MemAddr::PostIndex {
                base: Reg::sp(),
                offset: dealloc,
            },
            dst1: Reg::fp(),
            dst2: Reg::lr(),
        });
        self.push_lir(Aarch64Inst::Ret);
    }

    /// Emit conditional branch: test condition and branch accordingly
    /// Returns true if an early return was taken (for constant conditions)
    fn emit_cbr(&mut self, insn: &Instruction, total_frame: i32) -> bool {
        let Some(&cond) = insn.src.first() else {
            return false;
        };

        let loc = self.get_location(cond);
        let (scratch0, _, _) = Reg::scratch_regs();

        match &loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Cmp {
                    size: OperandSize::B64,
                    src1: *r,
                    src2: GpOperand::Imm(0),
                });
            }
            Loc::Stack(offset) => {
                let actual_offset = self.stack_offset(total_frame, *offset);
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                    dst: scratch0,
                });
                self.push_lir(Aarch64Inst::Cmp {
                    size: OperandSize::B64,
                    src1: scratch0,
                    src2: GpOperand::Imm(0),
                });
            }
            Loc::Imm(v) => {
                let target = if *v != 0 { insn.bb_true } else { insn.bb_false };
                if let Some(target) = target {
                    self.push_lir(Aarch64Inst::B {
                        target: Label::new(&self.base.current_fn, target.0),
                    });
                }
                return true;
            }
            Loc::Global(name) => {
                self.emit_load_global(name, scratch0, OperandSize::B64);
                self.push_lir(Aarch64Inst::Cmp {
                    size: OperandSize::B64,
                    src1: scratch0,
                    src2: GpOperand::Imm(0),
                });
            }
            Loc::VReg(v) => {
                self.push_lir(Aarch64Inst::FcmpZero {
                    size: FpSize::Double,
                    src: *v,
                });
            }
            Loc::FImm(f, _) => {
                let target = if *f != 0.0 {
                    insn.bb_true
                } else {
                    insn.bb_false
                };
                if let Some(target) = target {
                    self.push_lir(Aarch64Inst::B {
                        target: Label::new(&self.base.current_fn, target.0),
                    });
                }
                return true;
            }
        }

        if let Some(target) = insn.bb_true {
            self.push_lir(Aarch64Inst::BCond {
                cond: CondCode::Ne,
                target: Label::new(&self.base.current_fn, target.0),
            });
        }
        if let Some(target) = insn.bb_false {
            self.push_lir(Aarch64Inst::B {
                target: Label::new(&self.base.current_fn, target.0),
            });
        }
        false
    }

    /// Emit switch statement: compare value against cases and branch
    fn emit_switch(&mut self, insn: &Instruction, total_frame: i32) {
        let Some(val) = insn.target else { return };

        let loc = self.get_location(val);
        let (scratch0, scratch1, _) = Reg::scratch_regs();
        let size = insn.size.max(32);
        let op_size = OperandSize::from_bits(size);

        // Move switch value to scratch0
        match &loc {
            Loc::Reg(r) => {
                if *r != scratch0 {
                    self.push_lir(Aarch64Inst::Mov {
                        size: op_size,
                        src: GpOperand::Reg(*r),
                        dst: scratch0,
                    });
                }
            }
            Loc::Stack(offset) => {
                let actual_offset = self.stack_offset(total_frame, *offset);
                self.push_lir(Aarch64Inst::Ldr {
                    size: op_size,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                    dst: scratch0,
                });
            }
            Loc::Imm(v) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: op_size,
                    src: GpOperand::Imm(*v),
                    dst: scratch0,
                });
            }
            Loc::Global(name) => {
                self.emit_load_global(name, scratch0, op_size);
            }
            Loc::VReg(_) | Loc::FImm(..) => {}
        }

        // Generate comparisons for each case
        for (case_val, target_bb) in &insn.switch_cases {
            if *case_val >= 0 && *case_val < 4096 {
                self.push_lir(Aarch64Inst::Cmp {
                    size: op_size,
                    src1: scratch0,
                    src2: GpOperand::Imm(*case_val),
                });
            } else {
                self.push_lir(Aarch64Inst::Mov {
                    size: op_size,
                    src: GpOperand::Imm(*case_val),
                    dst: scratch1,
                });
                self.push_lir(Aarch64Inst::Cmp {
                    size: op_size,
                    src1: scratch0,
                    src2: GpOperand::Reg(scratch1),
                });
            }
            self.push_lir(Aarch64Inst::BCond {
                cond: CondCode::Eq,
                target: Label::new(&self.base.current_fn, target_bb.0),
            });
        }

        if let Some(default_bb) = insn.switch_default {
            self.push_lir(Aarch64Inst::B {
                target: Label::new(&self.base.current_fn, default_bb.0),
            });
        }
    }

    fn emit_insn(
        &mut self,
        insn: &Instruction,
        frame_info: &(i32, Vec<Reg>, Vec<VReg>),
        types: &TypeTable,
    ) {
        // Emit .loc directive for debug info
        self.emit_loc(insn);

        let (total_frame, callee_saved, callee_saved_fp) = frame_info;

        match insn.op {
            Opcode::Entry => {
                // Already handled in function prologue
            }

            Opcode::Ret => {
                self.emit_ret(insn, *total_frame, callee_saved, callee_saved_fp, types);
            }

            Opcode::Br => {
                if let Some(target) = insn.bb_true {
                    self.push_lir(Aarch64Inst::B {
                        target: Label::new(&self.base.current_fn, target.0),
                    });
                }
            }

            Opcode::Cbr => if self.emit_cbr(insn, *total_frame) {},

            Opcode::Switch => {
                self.emit_switch(insn, *total_frame);
            }

            Opcode::Add
            | Opcode::Sub
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
            | Opcode::Shl
            | Opcode::Lsr
            | Opcode::Asr => {
                self.emit_binop(insn, *total_frame);
            }

            Opcode::Mul => {
                self.emit_mul(insn, *total_frame);
            }

            Opcode::DivS | Opcode::DivU | Opcode::ModS | Opcode::ModU => {
                self.emit_div(insn, *total_frame);
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
                self.emit_compare(insn, *total_frame);
            }

            Opcode::Neg => self.emit_unary_op(insn, UnaryOp::Neg, *total_frame),
            Opcode::Not => self.emit_unary_op(insn, UnaryOp::Not, *total_frame),

            Opcode::Load => {
                self.emit_load(insn, *total_frame, types);
            }

            Opcode::Store => {
                self.emit_store(insn, *total_frame);
            }

            Opcode::Call => {
                self.emit_call(insn, *total_frame, types);
            }

            Opcode::SetVal => {
                if let Some(target) = insn.target {
                    if let Some(pseudo) = self.pseudos.iter().find(|p| p.id == target) {
                        match self.locations.get(&target).cloned() {
                            Some(Loc::Reg(r)) => {
                                if let PseudoKind::Val(v) = &pseudo.kind {
                                    self.emit_mov_imm(r, *v, insn.size);
                                }
                            }
                            Some(Loc::VReg(v)) => {
                                if let PseudoKind::FVal(f) = &pseudo.kind {
                                    // Load FP constant using integer register
                                    // Use type to determine float vs double, fall back to size
                                    let is_float = insn
                                        .typ
                                        .map(|t| types.kind(t) == TypeKind::Float)
                                        .unwrap_or(insn.size <= 32);
                                    let (scratch0, _, _) = Reg::scratch_regs();
                                    let bits = if is_float {
                                        (*f as f32).to_bits() as i64
                                    } else {
                                        f.to_bits() as i64
                                    };
                                    self.emit_mov_imm(scratch0, bits, 64);
                                    // LIR: fmov from GP to FP register
                                    let fp_size = if is_float {
                                        FpSize::Single
                                    } else {
                                        FpSize::Double
                                    };
                                    self.push_lir(Aarch64Inst::FmovFromGp {
                                        size: fp_size,
                                        src: scratch0,
                                        dst: v,
                                    });
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }

            Opcode::Copy => {
                if let (Some(target), Some(&src)) = (insn.target, insn.src.first()) {
                    // Pass the type for proper sign/zero extension
                    self.emit_copy_with_type(src, target, insn.size, insn.typ, *total_frame, types);
                }
            }

            Opcode::SymAddr => {
                if let (Some(target), Some(&src)) = (insn.target, insn.src.first()) {
                    let dst_loc = self.get_location(target);
                    // Use X16 as scratch to avoid clobbering live values
                    // X16 is the intra-procedure-call scratch register (IP0)
                    let dst_reg = match &dst_loc {
                        Loc::Reg(r) => *r,
                        _ => Reg::X16,
                    };
                    let src_loc = self.get_location(src);
                    match src_loc {
                        Loc::Global(name) => {
                            self.emit_load_addr(&name, dst_reg);
                        }
                        Loc::Stack(offset) => {
                            // Get address of stack location (FP-relative for alloca safety)
                            let adjusted = self.stack_offset(*total_frame, offset);
                            self.push_lir(Aarch64Inst::Add {
                                size: OperandSize::B64,
                                src1: Reg::X29,
                                src2: GpOperand::Imm(adjusted as i64),
                                dst: dst_reg,
                            });
                        }
                        _ => {}
                    }
                    // Move to final destination if needed
                    if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
                        self.emit_move_to_loc(dst_reg, &dst_loc, 64, *total_frame);
                    }
                }
            }

            Opcode::Select => {
                self.emit_select(insn, *total_frame);
            }

            Opcode::Zext | Opcode::Sext | Opcode::Trunc => {
                self.emit_extend(insn, *total_frame);
            }

            // Floating-point arithmetic operations
            Opcode::FAdd | Opcode::FSub | Opcode::FMul | Opcode::FDiv => {
                self.emit_fp_binop(insn, *total_frame, types);
            }

            // Floating-point negation
            Opcode::FNeg => {
                self.emit_fp_neg(insn, *total_frame, types);
            }

            // Floating-point comparisons
            Opcode::FCmpOEq
            | Opcode::FCmpONe
            | Opcode::FCmpOLt
            | Opcode::FCmpOLe
            | Opcode::FCmpOGt
            | Opcode::FCmpOGe => {
                self.emit_fp_compare(insn, *total_frame, types);
            }

            // Int to float conversions
            Opcode::UCvtF | Opcode::SCvtF => {
                self.emit_int_to_float(insn, *total_frame, types);
            }

            // Float to int conversions
            Opcode::FCvtU | Opcode::FCvtS => {
                self.emit_float_to_int(insn, *total_frame, types);
            }

            // Float to float conversions (size changes)
            Opcode::FCvtF => {
                self.emit_float_to_float(insn, *total_frame, types);
            }

            // ================================================================
            // Variadic function support (va_* builtins)
            // ================================================================
            Opcode::VaStart => {
                self.emit_va_start(insn, *total_frame);
            }

            Opcode::VaArg => {
                self.emit_va_arg(insn, *total_frame, types);
            }

            Opcode::VaEnd => {
                // va_end is a no-op on all platforms
                // The C standard says it must be called, but it does nothing
            }

            Opcode::VaCopy => {
                self.emit_va_copy(insn, *total_frame);
            }

            // Byte-swapping builtins
            Opcode::Bswap16 => self.emit_bswap(insn, *total_frame, BswapSize::B16),
            Opcode::Bswap32 => self.emit_bswap(insn, *total_frame, BswapSize::B32),
            Opcode::Bswap64 => self.emit_bswap(insn, *total_frame, BswapSize::B64),

            // ================================================================
            // Count trailing zeros builtins
            Opcode::Ctz32 => self.emit_ctz(insn, *total_frame, OperandSize::B32),
            Opcode::Ctz64 => self.emit_ctz(insn, *total_frame, OperandSize::B64),
            // Count leading zeros builtins
            Opcode::Clz32 => self.emit_clz(insn, *total_frame, OperandSize::B32),
            Opcode::Clz64 => self.emit_clz(insn, *total_frame, OperandSize::B64),
            // Population count builtins
            Opcode::Popcount32 => self.emit_popcount(insn, *total_frame, OperandSize::B32),
            Opcode::Popcount64 => self.emit_popcount(insn, *total_frame, OperandSize::B64),

            Opcode::Alloca => {
                self.emit_alloca(insn, *total_frame);
            }

            Opcode::Unreachable => {
                // Emit brk #1 instruction - software breakpoint that traps
                // This is used for __builtin_unreachable() to indicate code
                // that should never be reached. If it is reached, the CPU
                // will generate a SIGTRAP.
                self.push_lir(Aarch64Inst::Brk { imm: 1 });
            }

            // ================================================================
            // setjmp/longjmp support
            // ================================================================
            Opcode::Setjmp => {
                self.emit_setjmp(insn, *total_frame);
            }

            Opcode::Longjmp => {
                self.emit_longjmp(insn, *total_frame);
            }

            // ================================================================
            // Inline Assembly
            // ================================================================
            Opcode::Asm => {
                self.emit_inline_asm(insn, *total_frame);
            }

            // Skip no-ops and unimplemented
            _ => {}
        }
    }

    pub(super) fn get_location(&self, pseudo: PseudoId) -> Loc {
        self.locations.get(&pseudo).cloned().unwrap_or(Loc::Imm(0))
    }

    /// Load address of a global symbol into a register
    pub(super) fn emit_load_addr(&mut self, name: &str, dst: Reg) {
        // Local labels (starting with '.') don't get the _ prefix on macOS
        let sym = if name.starts_with('.') {
            Symbol::local(name)
        } else {
            Symbol::global(name)
        };

        if self.needs_got_access(name) {
            // External symbols on macOS: load address from GOT
            // ADRP + LDR from GOT
            let extern_sym = Symbol::extern_sym(name);
            self.push_lir(Aarch64Inst::AdrpGotPage {
                sym: extern_sym.clone(),
                dst,
            });
            self.push_lir(Aarch64Inst::LdrSymGotPageOff {
                sym: extern_sym,
                base: dst,
                dst,
            });
        } else {
            // ADRP + ADD sequence for PIC address loading
            self.push_lir(Aarch64Inst::Adrp {
                sym: sym.clone(),
                dst,
            });
            self.push_lir(Aarch64Inst::AddSymOffset {
                sym,
                base: dst,
                dst,
            });
        }
    }

    /// Load value of a global symbol into a register with specified size
    pub(super) fn emit_load_global(&mut self, name: &str, dst: Reg, size: OperandSize) {
        // Local labels (starting with '.') don't get the _ prefix on macOS
        let sym = if name.starts_with('.') {
            Symbol::local(name)
        } else {
            Symbol::global(name)
        };

        if self.needs_got_access(name) {
            // External symbols on macOS: load address from GOT, then load value
            // ADRP + LDR from GOT gets address, then LDR value
            let extern_sym = Symbol::extern_sym(name);
            self.push_lir(Aarch64Inst::AdrpGotPage {
                sym: extern_sym.clone(),
                dst,
            });
            self.push_lir(Aarch64Inst::LdrSymGotPageOff {
                sym: extern_sym,
                base: dst,
                dst,
            });
            // Now dst contains the address, load the actual value
            self.push_lir(Aarch64Inst::Ldr {
                size,
                addr: MemAddr::Base(dst),
                dst,
            });
        } else {
            // ADRP + LDR sequence for PIC value loading
            self.push_lir(Aarch64Inst::Adrp {
                sym: sym.clone(),
                dst,
            });
            self.push_lir(Aarch64Inst::LdrSymOffset {
                size,
                sym,
                base: dst,
                dst,
            });
        }
    }

    /// Move immediate value to register
    pub(super) fn emit_mov_imm(&mut self, dst: Reg, val: i64, size: u32) {
        let op_size = OperandSize::from_bits(size.max(32));

        // AArch64 can only move 16-bit immediates directly
        // For larger values, we need movz + movk sequence
        if (0..=0xFFFF).contains(&val) {
            // LIR: simple mov immediate
            self.push_lir(Aarch64Inst::Mov {
                size: op_size,
                src: GpOperand::Imm(val),
                dst,
            });
        } else if (-0x8000..0).contains(&val) {
            // Small negative number - use mov (assembler handles movn)
            self.push_lir(Aarch64Inst::Mov {
                size: op_size,
                src: GpOperand::Imm(val),
                dst,
            });
        } else {
            // Use movz + movk for larger values
            let uval = val as u64;
            // LIR: movz base
            self.push_lir(Aarch64Inst::Movz {
                size: OperandSize::B64,
                imm: (uval & 0xFFFF) as u16,
                shift: 0,
                dst,
            });
            if (uval >> 16) & 0xFFFF != 0 {
                // LIR: movk shift 16
                self.push_lir(Aarch64Inst::Movk {
                    size: OperandSize::B64,
                    imm: ((uval >> 16) & 0xFFFF) as u16,
                    shift: 16,
                    dst,
                });
            }
            if (uval >> 32) & 0xFFFF != 0 {
                // LIR: movk shift 32
                self.push_lir(Aarch64Inst::Movk {
                    size: OperandSize::B64,
                    imm: ((uval >> 32) & 0xFFFF) as u16,
                    shift: 32,
                    dst,
                });
            }
            if (uval >> 48) & 0xFFFF != 0 {
                // LIR: movk shift 48
                self.push_lir(Aarch64Inst::Movk {
                    size: OperandSize::B64,
                    imm: ((uval >> 48) & 0xFFFF) as u16,
                    shift: 48,
                    dst,
                });
            }
        }
    }

    pub(super) fn emit_move(&mut self, src: PseudoId, dst: Reg, size: u32, frame_size: i32) {
        let actual_size = size; // Keep original size for sub-32-bit stack loads
        let size = size.max(32);
        let loc = self.get_location(src);
        let op_size = OperandSize::from_bits(size);

        match loc {
            Loc::Reg(r) if r == dst => {}
            Loc::Reg(r) => {
                // LIR: mov register to register
                self.push_lir(Aarch64Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(r),
                    dst,
                });
            }
            Loc::Stack(offset) => {
                let actual_offset = self.stack_offset(frame_size, offset);
                // For sub-32-bit values, use sized load (ldrb/ldrh) which zero-extends.
                // This avoids reading garbage from adjacent stack bytes.
                let load_size = OperandSize::from_bits(actual_size.max(8));
                // LIR: load from stack (FP-relative for alloca safety)
                self.push_lir(Aarch64Inst::Ldr {
                    size: load_size,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                    dst,
                });
            }
            Loc::Imm(v) => {
                self.emit_mov_imm(dst, v, size);
            }
            Loc::Global(name) => {
                let load_size = OperandSize::from_bits(size.max(32));
                self.emit_load_global(&name, dst, load_size);
            }
            Loc::VReg(v) => {
                // LIR: fmov from FP to GP register
                let fp_size = if size <= 32 {
                    FpSize::Single
                } else {
                    FpSize::Double
                };
                self.push_lir(Aarch64Inst::FmovToGp {
                    size: fp_size,
                    src: v,
                    dst,
                });
            }
            Loc::FImm(f, imm_size) => {
                // Use the size from the FImm, not the passed-in size
                // This ensures float constants are loaded as float, not double
                let bits = if imm_size <= 32 {
                    (f as f32).to_bits() as i64
                } else {
                    f.to_bits() as i64
                };
                self.emit_mov_imm(dst, bits, imm_size);
            }
        }
    }

    pub(super) fn emit_move_to_loc(&mut self, src: Reg, dst: &Loc, size: u32, frame_size: i32) {
        // For stack stores, use actual size to properly handle char/short
        // For register-to-register, use minimum 32-bit
        match dst {
            Loc::Reg(r) if *r == src => {}
            Loc::Reg(r) => {
                let reg_size = size.max(32);
                // LIR: mov register to register
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::from_bits(reg_size),
                    src: GpOperand::Reg(src),
                    dst: *r,
                });
            }
            Loc::Stack(offset) => {
                let actual_offset = self.stack_offset(frame_size, *offset);
                // LIR: store to stack (FP-relative for alloca safety)
                let op_size = OperandSize::from_bits(size);
                self.push_lir(Aarch64Inst::Str {
                    size: op_size,
                    src,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                });
            }
            _ => {}
        }
    }

    /// Compute memory address from a pseudo, handling all location types.
    /// For Loc::Stack with spilled addresses and other non-register/non-symbol locations,
    /// emits setup instructions to load the address into temp_reg.
    fn compute_mem_addr(
        &mut self,
        addr: PseudoId,
        insn_offset: i64,
        frame_size: i32,
        temp_reg: Reg,
    ) -> ComputedAddr {
        let addr_loc = self.get_location(addr);
        match addr_loc {
            Loc::Reg(r) => ComputedAddr::Direct(MemAddr::BaseOffset {
                base: r,
                offset: insn_offset as i32,
            }),
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                if is_symbol {
                    // Local variable - access directly from stack slot (FP-relative for alloca safety)
                    let total_offset = self.stack_offset(frame_size, offset) + insn_offset as i32;
                    ComputedAddr::Direct(MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: total_offset,
                    })
                } else {
                    // Spilled address - load address first (FP-relative for alloca safety)
                    let adjusted = self.stack_offset(frame_size, offset);
                    self.push_lir(Aarch64Inst::Ldr {
                        size: OperandSize::B64,
                        addr: MemAddr::BaseOffset {
                            base: Reg::X29,
                            offset: adjusted,
                        },
                        dst: temp_reg,
                    });
                    ComputedAddr::WithSetup(MemAddr::BaseOffset {
                        base: temp_reg,
                        offset: insn_offset as i32,
                    })
                }
            }
            Loc::Global(name) => ComputedAddr::Global(name.clone()),
            _ => {
                // Other location types - emit move to temp register
                self.emit_move(addr, temp_reg, 64, frame_size);
                ComputedAddr::WithSetup(MemAddr::BaseOffset {
                    base: temp_reg,
                    offset: insn_offset as i32,
                })
            }
        }
    }

    fn emit_load(&mut self, insn: &Instruction, frame_size: i32, types: &TypeTable) {
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
        let is_fp = insn.typ.is_some_and(|t| types.is_float(t)) || matches!(dst_loc, Loc::VReg(_));

        if is_fp {
            self.emit_fp_load(insn, frame_size, types);
            return;
        }

        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X9,
        };

        // Determine if we need sign or zero extension for small types
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

        // Helper to emit the appropriate load instruction
        let emit_load_lir = |this: &mut Self, mem_addr: MemAddr| match mem_size {
            8 if is_unsigned => {
                this.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B8,
                    addr: mem_addr,
                    dst: dst_reg,
                });
            }
            8 => {
                this.push_lir(Aarch64Inst::Ldrs {
                    src_size: OperandSize::B8,
                    dst_size: OperandSize::from_bits(reg_size),
                    addr: mem_addr,
                    dst: dst_reg,
                });
            }
            16 if is_unsigned => {
                this.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B16,
                    addr: mem_addr,
                    dst: dst_reg,
                });
            }
            16 => {
                this.push_lir(Aarch64Inst::Ldrs {
                    src_size: OperandSize::B16,
                    dst_size: OperandSize::from_bits(reg_size),
                    addr: mem_addr,
                    dst: dst_reg,
                });
            }
            _ => {
                this.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::from_bits(mem_size),
                    addr: mem_addr,
                    dst: dst_reg,
                });
            }
        };

        match self.compute_mem_addr(addr, insn.offset, frame_size, Reg::X16) {
            ComputedAddr::Direct(mem_addr) | ComputedAddr::WithSetup(mem_addr) => {
                emit_load_lir(self, mem_addr);
            }
            ComputedAddr::Global(name) => {
                let load_size = OperandSize::from_bits(mem_size);
                self.emit_load_global(&name, dst_reg, load_size);
            }
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, reg_size, frame_size);
        }
    }

    fn emit_store(&mut self, insn: &Instruction, frame_size: i32) {
        // Use actual size for memory stores (8, 16, 32, 64 bits)
        let mem_size = insn.size;
        let reg_size = insn.size.max(32);

        let (addr, value) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&a), Some(&v)) => (a, v),
            _ => return,
        };

        // For struct stores (size > 64), we need to copy multiple words
        // The value is a symbol containing the struct data
        if mem_size > 64 {
            self.emit_struct_store(insn, addr, value, frame_size);
            return;
        }

        self.emit_move(value, Reg::X9, reg_size, frame_size);

        // Get the store size based on mem_size
        let store_size = OperandSize::from_bits(mem_size);

        // Helper to emit store instruction
        let emit_store_lir = |this: &mut Self, mem_addr: MemAddr| {
            this.push_lir(Aarch64Inst::Str {
                size: store_size,
                src: Reg::X9,
                addr: mem_addr,
            });
        };

        match self.compute_mem_addr(addr, insn.offset, frame_size, Reg::X16) {
            ComputedAddr::Direct(mem_addr) | ComputedAddr::WithSetup(mem_addr) => {
                emit_store_lir(self, mem_addr);
            }
            ComputedAddr::Global(name) => {
                self.emit_load_addr(&name, Reg::X16);
                emit_store_lir(self, MemAddr::Base(Reg::X16));
            }
        }
    }

    /// Emit a struct copy (store of size > 64 bits)
    /// The value is a symbol containing the source struct data
    fn emit_struct_store(
        &mut self,
        insn: &Instruction,
        addr: PseudoId,
        value: PseudoId,
        frame_size: i32,
    ) {
        let struct_size = insn.size; // Size in bits
        let num_qwords = struct_size.div_ceil(64);

        // Get source address (where the struct data is)
        let value_loc = self.get_location(value);

        // Special case: if value is immediate 0, zero the struct instead of copying
        if let Loc::Imm(0) = value_loc {
            self.emit_struct_zero(insn, addr, num_qwords, frame_size);
            return;
        }

        // Get destination address
        let addr_loc = self.get_location(addr);

        // Load source address into X16 (FP-relative for alloca safety)
        match value_loc {
            Loc::Stack(offset) => {
                let total_offset = self.stack_offset(frame_size, offset);
                self.push_lir(Aarch64Inst::Add {
                    size: OperandSize::B64,
                    src1: Reg::X29,
                    src2: GpOperand::Imm(total_offset as i64),
                    dst: Reg::X16,
                });
            }
            Loc::Reg(r) => {
                if r != Reg::X16 {
                    self.push_lir(Aarch64Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: Reg::X16,
                    });
                }
            }
            Loc::Global(ref name) => {
                self.emit_load_addr(name, Reg::X16);
            }
            _ => return,
        }

        // Load destination address into X17 (FP-relative for alloca safety)
        match addr_loc {
            Loc::Stack(offset) => {
                let total_offset = self.stack_offset(frame_size, offset) + insn.offset as i32;
                self.push_lir(Aarch64Inst::Add {
                    size: OperandSize::B64,
                    src1: Reg::X29,
                    src2: GpOperand::Imm(total_offset as i64),
                    dst: Reg::X17,
                });
            }
            Loc::Reg(r) => {
                if insn.offset != 0 {
                    self.push_lir(Aarch64Inst::Add {
                        size: OperandSize::B64,
                        src1: r,
                        src2: GpOperand::Imm(insn.offset),
                        dst: Reg::X17,
                    });
                } else if r != Reg::X17 {
                    self.push_lir(Aarch64Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: Reg::X17,
                    });
                }
            }
            Loc::Global(ref name) => {
                self.emit_load_addr(name, Reg::X17);
            }
            _ => return,
        }

        // Copy qword by qword using X9 as temp
        for i in 0..num_qwords {
            let byte_offset = (i * 8) as i32;
            self.push_lir(Aarch64Inst::Ldr {
                size: OperandSize::B64,
                addr: MemAddr::BaseOffset {
                    base: Reg::X16,
                    offset: byte_offset,
                },
                dst: Reg::X9,
            });
            self.push_lir(Aarch64Inst::Str {
                size: OperandSize::B64,
                src: Reg::X9,
                addr: MemAddr::BaseOffset {
                    base: Reg::X17,
                    offset: byte_offset,
                },
            });
        }
    }

    /// Emit code to zero a struct (for struct = {0} initialization)
    fn emit_struct_zero(
        &mut self,
        insn: &Instruction,
        addr: PseudoId,
        num_qwords: u32,
        frame_size: i32,
    ) {
        let addr_loc = self.get_location(addr);

        // Load destination address into X17
        match addr_loc {
            Loc::Stack(offset) => {
                let total_offset = self.stack_offset(frame_size, offset) + insn.offset as i32;
                self.push_lir(Aarch64Inst::Add {
                    size: OperandSize::B64,
                    src1: Reg::X29,
                    src2: GpOperand::Imm(total_offset as i64),
                    dst: Reg::X17,
                });
            }
            Loc::Reg(r) => {
                if insn.offset != 0 {
                    self.push_lir(Aarch64Inst::Add {
                        size: OperandSize::B64,
                        src1: r,
                        src2: GpOperand::Imm(insn.offset),
                        dst: Reg::X17,
                    });
                } else if r != Reg::X17 {
                    self.push_lir(Aarch64Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(r),
                        dst: Reg::X17,
                    });
                }
            }
            Loc::Global(ref name) => {
                self.emit_load_addr(name, Reg::X17);
                if insn.offset != 0 {
                    self.push_lir(Aarch64Inst::Add {
                        size: OperandSize::B64,
                        src1: Reg::X17,
                        src2: GpOperand::Imm(insn.offset),
                        dst: Reg::X17,
                    });
                }
            }
            _ => return,
        }

        // Store zeros using XZR (zero register) - aarch64 has hardware zero reg!
        for i in 0..num_qwords {
            let byte_offset = (i * 8) as i32;
            self.push_lir(Aarch64Inst::Str {
                size: OperandSize::B64,
                src: Reg::Xzr,
                addr: MemAddr::BaseOffset {
                    base: Reg::X17,
                    offset: byte_offset,
                },
            });
        }
    }

    fn emit_call(&mut self, insn: &Instruction, frame_size: i32, types: &TypeTable) {
        // Get function name (or placeholder for indirect calls)
        let func_name = if insn.indirect_target.is_some() {
            "<indirect>".to_string()
        } else {
            match &insn.func_name {
                Some(n) => n.clone(),
                None => return,
            }
        };

        // For indirect calls, load function pointer into X16
        if let Some(func_addr) = insn.indirect_target {
            self.emit_move(func_addr, Reg::X16, 64, frame_size);
        }

        // Handle sret (hidden struct return pointer) argument
        let args_start = self.setup_sret_arg(insn, frame_size);

        // Determine if this is a Darwin variadic call
        let is_darwin_variadic =
            self.base.target.os == crate::target::Os::MacOS && insn.variadic_arg_start.is_some();

        // Set up arguments and get stack cleanup count
        let stack_args = if is_darwin_variadic {
            self.setup_darwin_variadic_args(insn, args_start, types, frame_size)
        } else {
            self.setup_register_args(insn, args_start, types, frame_size)
        };

        // Emit the call instruction
        self.emit_call_instruction(insn, &func_name);

        // Clean up stack
        self.cleanup_call_stack(stack_args);

        // Handle return value
        self.handle_call_return_value(insn, types, frame_size);
    }

    /// Emit a select (ternary) instruction using CSEL
    /// This is used for pure ternary expressions: cond ? a : b
    fn emit_select(&mut self, insn: &Instruction, frame_size: i32) {
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
        // Use X16 as default scratch to avoid clobbering live values
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X16,
        };

        // Pick non-conflicting temp registers for cond/then/else values
        // If dst_reg is one of our default temps, shift allocation to avoid conflicts
        let (cond_reg, then_reg, else_reg) = if dst_reg == Reg::X10 {
            (Reg::X11, Reg::X12, Reg::X13)
        } else if dst_reg == Reg::X11 {
            (Reg::X10, Reg::X12, Reg::X13)
        } else if dst_reg == Reg::X12 {
            (Reg::X10, Reg::X11, Reg::X13)
        } else {
            (Reg::X10, Reg::X11, Reg::X12) // Original allocation
        };

        // Load condition, then and else values
        self.emit_move(cond, cond_reg, 64, frame_size);
        self.emit_move(then_val, then_reg, size, frame_size);
        self.emit_move(else_val, else_reg, size, frame_size);

        // Compare condition with zero
        self.push_lir(Aarch64Inst::Cmp {
            size: OperandSize::B64,
            src1: cond_reg,
            src2: GpOperand::Imm(0),
        });

        // Use csel: if cond != 0, select then_val, else select else_val
        self.push_lir(Aarch64Inst::Csel {
            size: op_size,
            cond: CondCode::Ne,
            src_true: then_reg,
            src_false: else_reg,
            dst: dst_reg,
        });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size, frame_size);
        }
    }

    fn emit_copy_with_type(
        &mut self,
        src: PseudoId,
        dst: PseudoId,
        size: u32,
        typ: Option<TypeId>,
        frame_size: i32,
        types: &TypeTable,
    ) {
        // Keep actual size for handling narrow types
        let actual_size = size;
        let reg_size = size.max(32);
        let dst_loc = self.get_location(dst);
        let src_loc = self.get_location(src);

        // Check if this is a FP copy (source or dest is in VReg or is FImm)
        let is_fp_copy =
            matches!(&src_loc, Loc::VReg(_) | Loc::FImm(..)) || matches!(&dst_loc, Loc::VReg(_));

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
            let dst_vreg = match &dst_loc {
                Loc::VReg(v) => *v,
                _ => VReg::V16, // Use scratch register
            };

            self.emit_fp_move(src, dst_vreg, typ, reg_size, frame_size, types);

            if !matches!(&dst_loc, Loc::VReg(v) if *v == dst_vreg) {
                self.emit_fp_move_to_loc(dst_vreg, &dst_loc, typ, reg_size, frame_size, types);
            }
        } else {
            // Integer copy
            match &dst_loc {
                Loc::Reg(r) => {
                    self.emit_move(src, *r, reg_size, frame_size);
                    // For narrow types (8 or 16 bits), extend to correct width
                    // AARCH64: UXTB/UXTH for unsigned, SXTB/SXTH for signed
                    if actual_size == 8 {
                        if is_unsigned {
                            self.push_lir(Aarch64Inst::Uxtb { src: *r, dst: *r });
                        } else {
                            self.push_lir(Aarch64Inst::Sxtb {
                                dst_size: OperandSize::B32,
                                src: *r,
                                dst: *r,
                            });
                        }
                    } else if actual_size == 16 {
                        if is_unsigned {
                            self.push_lir(Aarch64Inst::Uxth { src: *r, dst: *r });
                        } else {
                            self.push_lir(Aarch64Inst::Sxth {
                                dst_size: OperandSize::B32,
                                src: *r,
                                dst: *r,
                            });
                        }
                    }
                }
                Loc::Stack(_) => {
                    self.emit_move(src, Reg::X9, reg_size, frame_size);
                    // For narrow types stored to stack, use the actual size
                    if actual_size <= 16 {
                        self.emit_move_to_loc(Reg::X9, &dst_loc, actual_size, frame_size);
                    } else {
                        self.emit_move_to_loc(Reg::X9, &dst_loc, reg_size, frame_size);
                    }
                }
                _ => {}
            }
        }
    }

    // Floating-Point Operations - see float.rs

    // ========================================================================
    // Inline Assembly Support
    // ========================================================================

    /// Emit inline assembly instruction
    fn emit_inline_asm(&mut self, insn: &Instruction, _frame_size: i32) {
        let asm_data = match &insn.asm_data {
            Some(data) => data,
            None => return,
        };

        // Build operand strings for asm substitution
        // We use the actual locations assigned by the register allocator
        // so that subsequent Store instructions work correctly.
        let mut operand_regs: Vec<Option<Reg>> = Vec::new();
        let mut operand_mem: Vec<Option<String>> = Vec::new();
        let mut operand_names: Vec<Option<String>> = Vec::new();

        // Process output operands (they go first: %0, %1, etc.)
        for output in &asm_data.outputs {
            let loc = self.get_location(output.pseudo);
            operand_names.push(output.name.clone());
            match loc {
                Loc::Reg(r) => {
                    operand_regs.push(Some(r));
                    operand_mem.push(None);
                }
                _ => {
                    // Memory or other location - emit as memory operand
                    let mem_str = self.loc_to_asm_string(&loc);
                    operand_regs.push(None);
                    operand_mem.push(Some(mem_str));
                }
            }
        }

        let num_outputs = asm_data.outputs.len();

        // Process input operands
        for input in &asm_data.inputs {
            // Handle matching constraints - use the matched output's location
            let loc = if let Some(match_idx) = input.matching_output {
                if match_idx < num_outputs {
                    self.get_location(asm_data.outputs[match_idx].pseudo)
                } else {
                    self.get_location(input.pseudo)
                }
            } else {
                self.get_location(input.pseudo)
            };

            operand_names.push(input.name.clone());
            match loc {
                Loc::Reg(r) => {
                    operand_regs.push(Some(r));
                    operand_mem.push(None);
                }
                Loc::Imm(v) => {
                    // Immediate value
                    operand_regs.push(None);
                    operand_mem.push(Some(format!("#{}", v)));
                }
                _ => {
                    // Memory or other location
                    let mem_str = self.loc_to_asm_string(&loc);
                    operand_regs.push(None);
                    operand_mem.push(Some(mem_str));
                }
            }
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
                self.push_lir(Aarch64Inst::Directive(Directive::Raw(trimmed.to_string())));
            }
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

    /// Convert a location to an asm operand string for AArch64
    fn loc_to_asm_string(&self, loc: &Loc) -> String {
        match loc {
            Loc::Reg(r) => asm_reg_name_64(*r).to_string(),
            Loc::Stack(offset) => {
                // AArch64 uses offsets from FP (x29)
                format!("[x29, #-{}]", offset)
            }
            Loc::Imm(v) => format!("#{}", v),
            Loc::VReg(vreg) => vreg.name_d().to_string(),
            Loc::FImm(_, _) => {
                // Float immediates not directly usable in inline asm
                panic!("Float immediate not supported in inline asm operand")
            }
            Loc::Global(name) => name.clone(),
        }
    }

    /// Substitute %0, %1, %[name], %l0, %l[name], etc. in asm template with actual operands
    /// goto_labels: (label_string, label_name) - label_string is the fully formatted label
    fn substitute_asm_operands(
        &self,
        template: &str,
        operand_regs: &[Option<Reg>],
        operand_mem: &[Option<String>],
        operand_names: &[Option<String>],
        goto_labels: &[(String, String)],
    ) -> String {
        crate::arch::substitute_asm_operands(
            self,
            template,
            operand_regs,
            operand_mem,
            operand_names,
            goto_labels,
        )
    }
}

// ============================================================================
// Inline Assembly Helper Functions
// ============================================================================

/// Get the 64-bit register name for inline asm
fn asm_reg_name_64(reg: Reg) -> &'static str {
    match reg {
        Reg::X0 => "x0",
        Reg::X1 => "x1",
        Reg::X2 => "x2",
        Reg::X3 => "x3",
        Reg::X4 => "x4",
        Reg::X5 => "x5",
        Reg::X6 => "x6",
        Reg::X7 => "x7",
        Reg::X8 => "x8",
        Reg::X9 => "x9",
        Reg::X10 => "x10",
        Reg::X11 => "x11",
        Reg::X12 => "x12",
        Reg::X13 => "x13",
        Reg::X14 => "x14",
        Reg::X15 => "x15",
        Reg::X16 => "x16",
        Reg::X17 => "x17",
        Reg::X19 => "x19",
        Reg::X20 => "x20",
        Reg::X21 => "x21",
        Reg::X22 => "x22",
        Reg::X23 => "x23",
        Reg::X24 => "x24",
        Reg::X25 => "x25",
        Reg::X26 => "x26",
        Reg::X27 => "x27",
        Reg::X28 => "x28",
        Reg::X29 => "x29",
        Reg::X30 => "x30",
        Reg::SP => "sp",
        Reg::Xzr => "xzr",
    }
}

/// Get the 32-bit register name for inline asm
fn asm_reg_name_32(reg: Reg) -> &'static str {
    match reg {
        Reg::X0 => "w0",
        Reg::X1 => "w1",
        Reg::X2 => "w2",
        Reg::X3 => "w3",
        Reg::X4 => "w4",
        Reg::X5 => "w5",
        Reg::X6 => "w6",
        Reg::X7 => "w7",
        Reg::X8 => "w8",
        Reg::X9 => "w9",
        Reg::X10 => "w10",
        Reg::X11 => "w11",
        Reg::X12 => "w12",
        Reg::X13 => "w13",
        Reg::X14 => "w14",
        Reg::X15 => "w15",
        Reg::X16 => "w16",
        Reg::X17 => "w17",
        Reg::X19 => "w19",
        Reg::X20 => "w20",
        Reg::X21 => "w21",
        Reg::X22 => "w22",
        Reg::X23 => "w23",
        Reg::X24 => "w24",
        Reg::X25 => "w25",
        Reg::X26 => "w26",
        Reg::X27 => "w27",
        Reg::X28 => "w28",
        Reg::X29 => "w29",
        Reg::X30 => "w30",
        Reg::SP => "wsp",
        Reg::Xzr => "wzr",
    }
}

// ============================================================================
// AsmOperandFormatter trait implementation
// ============================================================================

impl crate::arch::AsmOperandFormatter for Aarch64CodeGen {
    type Reg = Reg;

    fn size_modifiers(&self) -> &'static [char] {
        &['w', 'x'] // 32, 64-bit
    }

    fn format_reg_sized(&self, reg: Reg, size_mod: char) -> String {
        // AArch64 doesn't use % prefix for register names
        match size_mod {
            'w' => asm_reg_name_32(reg).to_string(),
            _ => asm_reg_name_64(reg).to_string(),
        }
    }

    fn format_reg_default(&self, reg: Reg) -> String {
        // AArch64 inline asm defaults to 64-bit
        asm_reg_name_64(reg).to_string()
    }
}

// ============================================================================
// CodeGenerator trait implementation
// ============================================================================

impl CodeGenerator for Aarch64CodeGen {
    fn generate(&mut self, module: &Module, types: &TypeTable) -> String {
        self.base.output.clear();
        self.base.clear_lir();
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

        // Flush all buffered LIR instructions to output
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
