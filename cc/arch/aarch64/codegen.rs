//
// Copyright (c) 2024 Jeff Garzik
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

use crate::arch::aarch64::lir::{Aarch64Inst, CallTarget, Cond, GpOperand, MemAddr};
use crate::arch::aarch64::regalloc::{Loc, Reg, RegAlloc, VReg};
use crate::arch::codegen::{escape_string, is_variadic_function, CodeGenerator};
use crate::arch::lir::{Directive, FpSize, Label, OperandSize, Symbol};
use crate::arch::DEFAULT_LIR_BUFFER_CAPACITY;
use crate::ir::{Function, Initializer, Instruction, Module, Opcode, Pseudo, PseudoId, PseudoKind};
use crate::target::Target;
use crate::types::{TypeId, TypeModifiers, TypeTable};
use std::collections::HashMap;

// ============================================================================
// AArch64 Code Generator
// ============================================================================

/// AArch64 code generator
pub struct Aarch64CodeGen {
    /// Target info
    target: Target,
    /// Output buffer
    output: String,
    /// LIR instruction buffer (for deferred emission and future peephole optimization)
    lir_buffer: Vec<Aarch64Inst>,
    /// Current function's register allocation
    locations: HashMap<PseudoId, Loc>,
    /// Current function's pseudos (for looking up values)
    pseudos: Vec<Pseudo>,
    /// Current function name (for generating unique labels)
    current_fn: String,
    /// Total frame size for current function
    frame_size: i32,
    /// Size of callee-saved register area (for computing local variable offsets)
    callee_saved_size: i32,
    /// Whether to emit basic unwind tables (cfi_startproc/cfi_endproc)
    emit_unwind_tables: bool,
    /// Last emitted source line (for avoiding duplicate .loc directives)
    last_debug_line: u32,
    /// Last emitted source file index
    last_debug_file: u16,
    /// Whether to emit debug info (.file/.loc directives)
    emit_debug: bool,
    /// Offset from FP to register save area (for variadic functions)
    reg_save_area_offset: i32,
    /// Size of register save area (for variadic functions)
    /// Used to compute correct FP-relative offsets for local variables
    reg_save_area_size: i32,
    /// Number of fixed GP parameters (for variadic functions)
    num_fixed_gp_params: usize,
}

impl Aarch64CodeGen {
    pub fn new(target: Target) -> Self {
        Self {
            target,
            output: String::new(),
            lir_buffer: Vec::with_capacity(DEFAULT_LIR_BUFFER_CAPACITY),
            locations: HashMap::new(),
            pseudos: Vec::new(),
            current_fn: String::new(),
            frame_size: 0,
            callee_saved_size: 0,
            emit_unwind_tables: true, // Default to emitting basic unwind tables
            last_debug_line: 0,
            last_debug_file: 0,
            emit_debug: false,
            reg_save_area_offset: 0,
            reg_save_area_size: 0,
            num_fixed_gp_params: 0,
        }
    }

    /// Compute the actual FP-relative offset for a stack location.
    /// For local variables (negative offsets), this accounts for the
    /// register save area in varargs functions which is placed at the
    /// end of the frame (after locals).
    #[inline]
    fn stack_offset(&self, frame_size: i32, offset: i32) -> i32 {
        if offset < 0 {
            // Local variable: use frame size minus reg_save_area
            // Layout: [fp/lr][callee-saved][locals][reg_save_area]
            // Locals are at offsets from (frame_size - reg_save_area_size)
            (frame_size - self.reg_save_area_size) + offset
        } else {
            // Positive offset (arguments passed on stack) - use as-is
            offset
        }
    }

    /// Push a LIR instruction to the buffer (deferred emission)
    fn push_lir(&mut self, inst: Aarch64Inst) {
        self.lir_buffer.push(inst);
    }

    /// Emit all buffered LIR instructions to the output string
    fn emit_all(&mut self) {
        use crate::arch::lir::EmitAsm;
        for inst in &self.lir_buffer {
            inst.emit(&self.target, &mut self.output);
        }
    }

    /// Emit .loc directive for source line tracking (if debug is enabled and line changed)
    fn emit_loc(&mut self, insn: &Instruction) {
        if !self.emit_debug {
            return;
        }
        if let Some(pos) = &insn.pos {
            // Only emit if line changed (avoid duplicate .loc directives)
            let file = pos.stream + 1; // DWARF file indices start at 1
            let line = pos.line;
            if line != self.last_debug_line || file != self.last_debug_file {
                self.push_lir(Aarch64Inst::Directive(Directive::loc(
                    file.into(),
                    line,
                    pos.col.into(),
                )));
                self.last_debug_line = line;
                self.last_debug_file = file;
            }
        }
    }

    fn emit_header(&mut self) {
        // Header comments with compiler and target info (GCC-style)
        for comment in crate::arch::codegen::generate_header_comments(&self.target) {
            self.push_lir(Aarch64Inst::Directive(Directive::Comment(comment)));
        }
        self.push_lir(Aarch64Inst::Directive(Directive::Text));
    }

    fn emit_global(&mut self, name: &str, typ: &TypeId, init: &Initializer, types: &TypeTable) {
        let size = types.size_bits(*typ) / 8;
        let size = if size == 0 { 8 } else { size }; // Default to 8 bytes

        // Check storage class - skip .globl for static
        let is_static = types.get(*typ).modifiers.contains(TypeModifiers::STATIC);

        // Get alignment from type info
        let align = types.alignment(*typ) as u32;

        // Use .comm for uninitialized external (non-static) globals
        let use_bss = matches!(init, Initializer::None) && !is_static;

        if use_bss {
            // Use .comm for uninitialized external globals
            self.push_lir(Aarch64Inst::Directive(Directive::comm(name, size, align)));
            return;
        }

        // Data section
        self.push_lir(Aarch64Inst::Directive(Directive::Data));

        // Check if this is a local symbol (starts with '.')
        let is_local = name.starts_with('.');

        // Global visibility (if not static and not local)
        if !is_static && !is_local {
            self.push_lir(Aarch64Inst::Directive(Directive::global(name)));
        }

        // ELF-only type and size (handled by Directive::emit which skips on macOS)
        self.push_lir(Aarch64Inst::Directive(Directive::type_object(name)));
        self.push_lir(Aarch64Inst::Directive(Directive::size(name, size)));

        // Alignment
        if align > 1 {
            self.push_lir(Aarch64Inst::Directive(Directive::Align(
                align.trailing_zeros(),
            )));
        }

        // Label - use local_label for names starting with '.'
        if is_local {
            self.push_lir(Aarch64Inst::Directive(Directive::local_label(name)));
        } else {
            self.push_lir(Aarch64Inst::Directive(Directive::global_label(name)));
        }

        // Emit initializer
        self.emit_initializer_data(init, size as usize);
    }

    /// Emit data for an initializer, recursively handling complex types
    fn emit_initializer_data(&mut self, init: &Initializer, size: usize) {
        match init {
            Initializer::None => {
                // Zero-fill
                self.push_lir(Aarch64Inst::Directive(Directive::Zero(size as u32)));
            }
            Initializer::Int(val) => match size {
                1 => self.push_lir(Aarch64Inst::Directive(Directive::Byte(*val))),
                2 => self.push_lir(Aarch64Inst::Directive(Directive::Short(*val))),
                4 => self.push_lir(Aarch64Inst::Directive(Directive::Long(*val))),
                _ => self.push_lir(Aarch64Inst::Directive(Directive::Quad(*val))),
            },
            Initializer::Float(val) => {
                if size == 4 {
                    // float - emit as 32-bit IEEE 754
                    let bits = (*val as f32).to_bits();
                    self.push_lir(Aarch64Inst::Directive(Directive::Long(bits as i64)));
                } else {
                    // double - emit as 64-bit IEEE 754
                    let bits = val.to_bits();
                    self.push_lir(Aarch64Inst::Directive(Directive::Quad(bits as i64)));
                }
            }
            Initializer::String(s) => {
                // Emit string as .ascii (without null terminator)
                self.push_lir(Aarch64Inst::Directive(Directive::Ascii(escape_string(s))));
                // Zero-fill remaining bytes if array is larger than string
                let string_len = s.len() + 1; // +1 for null terminator
                if size > string_len {
                    self.push_lir(Aarch64Inst::Directive(Directive::Zero(
                        (size - string_len) as u32,
                    )));
                } else if size > s.len() {
                    // Need null terminator
                    self.push_lir(Aarch64Inst::Directive(Directive::Byte(0)));
                }
            }
            Initializer::Array {
                elem_size,
                total_size,
                elements,
            } => {
                // Emit array elements with gaps filled by zeros
                let mut current_offset = 0usize;

                for (offset, elem_init) in elements {
                    // Zero-fill gap before this element
                    if *offset > current_offset {
                        self.push_lir(Aarch64Inst::Directive(Directive::Zero(
                            (*offset - current_offset) as u32,
                        )));
                    }

                    // Emit element
                    self.emit_initializer_data(elem_init, *elem_size);
                    current_offset = offset + elem_size;
                }

                // Zero-fill remaining space
                if *total_size > current_offset {
                    self.push_lir(Aarch64Inst::Directive(Directive::Zero(
                        (*total_size - current_offset) as u32,
                    )));
                }
            }
            Initializer::Struct { total_size, fields } => {
                // Emit struct fields with gaps (padding) filled by zeros
                let mut current_offset = 0usize;

                for (offset, field_size, field_init) in fields {
                    // Zero-fill gap before this field (padding)
                    if *offset > current_offset {
                        self.push_lir(Aarch64Inst::Directive(Directive::Zero(
                            (*offset - current_offset) as u32,
                        )));
                    }

                    // Emit field with its proper size
                    self.emit_initializer_data(field_init, *field_size);
                    current_offset = offset + field_size;
                }

                // Zero-fill remaining space (trailing padding)
                if *total_size > current_offset {
                    self.push_lir(Aarch64Inst::Directive(Directive::Zero(
                        (*total_size - current_offset) as u32,
                    )));
                }
            }
            Initializer::SymAddr(name) => {
                // Emit symbol address as 64-bit relocatable reference
                use crate::arch::lir::Symbol;
                let sym = if name.starts_with('.') {
                    Symbol::local(name.clone())
                } else {
                    Symbol::global(name.clone())
                };
                self.push_lir(Aarch64Inst::Directive(Directive::QuadSym(sym)));
            }
        }
    }

    fn emit_strings(&mut self, strings: &[(String, String)]) {
        if strings.is_empty() {
            return;
        }

        // Read-only data section
        self.push_lir(Aarch64Inst::Directive(Directive::Rodata));

        for (label, content) in strings {
            // Local label for string literal
            self.push_lir(Aarch64Inst::Directive(Directive::local_label(label)));
            self.push_lir(Aarch64Inst::Directive(Directive::Asciz(escape_string(
                content,
            ))));
        }

        // Switch back to text section for functions
        self.push_lir(Aarch64Inst::Directive(Directive::Text));
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
        let is_darwin = self.target.os == crate::target::Os::MacOS;
        let reg_save_area_size: i32 = if is_variadic && !is_darwin { 64 } else { 0 };

        // Calculate total frame size
        // Need space for: fp/lr (16 bytes) + GP callee-saved + FP callee-saved + local vars + reg save area
        // Round up callee-saved counts to even for 16-byte alignment
        // Note: AAPCS64 only requires the lower 64 bits of V8-V15 to be preserved (d8-d15)
        let callee_saved_gp_pairs = (callee_saved.len() + 1) / 2;
        let callee_saved_gp_size = callee_saved_gp_pairs as i32 * 16;
        let callee_saved_fp_pairs = (callee_saved_fp.len() + 1) / 2;
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
        self.current_fn = func.name.clone();
        self.frame_size = total_frame;
        self.callee_saved_size = callee_saved_size;
        self.reg_save_area_size = reg_save_area_size;

        // Function prologue
        self.push_lir(Aarch64Inst::Directive(Directive::Blank));
        self.push_lir(Aarch64Inst::Directive(Directive::Text));

        // Skip .globl for static functions (internal linkage)
        if !func.is_static {
            self.push_lir(Aarch64Inst::Directive(Directive::global(&func.name)));
        }

        // ELF-only type (handled by Directive::emit which skips on macOS)
        self.push_lir(Aarch64Inst::Directive(Directive::type_func(&func.name)));

        // Alignment
        self.push_lir(Aarch64Inst::Directive(Directive::Align(2)));

        // Function label
        self.push_lir(Aarch64Inst::Directive(Directive::global_label(&func.name)));

        // CFI: Start procedure (enables stack unwinding for this function)
        if self.emit_unwind_tables {
            self.push_lir(Aarch64Inst::Directive(Directive::CfiStartProc));
        }

        // Prologue: save frame pointer and link register, allocate stack
        let (scratch0, _scratch1) = Reg::scratch_regs();
        let fp = Reg::fp();
        let lr = Reg::lr();
        // Reference platform_reserved to acknowledge its existence
        let _ = Reg::platform_reserved();

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
            if self.emit_debug {
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
            if self.emit_debug {
                // CFA is now tracked by x29 + total_frame
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_def_cfa_register(
                    "x29",
                )));
            }

            // Save callee-saved registers in pairs
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
                    if self.emit_debug {
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
                    if self.emit_debug {
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

            // Save FP callee-saved registers (d8-d15) in pairs
            // AAPCS64 only requires preserving the lower 64 bits
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
                    if self.emit_debug {
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
                    if self.emit_debug {
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
            if self.emit_debug {
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
            if self.emit_debug {
                self.push_lir(Aarch64Inst::Directive(Directive::cfi_def_cfa_register(
                    "x29",
                )));
            }
        }

        // Store scratch register for later use in this function
        let _ = scratch0;

        // AAPCS64: Detect if there's a hidden return pointer (sret) for large struct returns.
        // Unlike x86-64, AAPCS64 uses X8 for indirect result, which doesn't shift other args.
        let has_sret = func
            .pseudos
            .iter()
            .any(|p| matches!(p.kind, PseudoKind::Arg(0)) && p.name.as_deref() == Some("__sret"));
        let arg_idx_offset: u32 = if has_sret { 1 } else { 0 };

        // If there's a sret, spill X8 to stack if needed
        if has_sret {
            if let Some(sret) = func.pseudos.iter().find(|p| {
                matches!(p.kind, PseudoKind::Arg(0)) && p.name.as_deref() == Some("__sret")
            }) {
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

        // For variadic functions on Linux/FreeBSD, save ALL argument registers to the register save area
        // This must be done BEFORE any code uses these registers, so va_arg can access them
        // AAPCS64: x0-x7 are saved at reg_save_area_offset from FP
        // On Darwin, variadic args are passed on the stack by the caller, so we skip this.
        if is_variadic && !is_darwin {
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

        // Count fixed GP parameters for va_start
        if is_variadic {
            self.num_fixed_gp_params = func
                .params
                .iter()
                .filter(|(_, typ)| !types.is_float(*typ))
                .count();
        }

        // Move arguments from registers to their allocated locations if needed
        // Note: On AAPCS64, sret uses X8, so regular args still start at X0
        let arg_regs = Reg::arg_regs();
        for (i, (_name, _typ)) in func.params.iter().enumerate() {
            if i < arg_regs.len() {
                // Find the pseudo for this argument
                for pseudo in &func.pseudos {
                    if let PseudoKind::Arg(arg_idx) = pseudo.kind {
                        // With sret, params have arg_idx = i + 1, but still use arg_regs[i]
                        if arg_idx == (i as u32) + arg_idx_offset {
                            if let Some(Loc::Stack(offset)) = self.locations.get(&pseudo.id) {
                                // Move from arg register to stack
                                if *offset < 0 {
                                    let actual_offset = self.stack_offset(total_frame, *offset);
                                    self.push_lir(Aarch64Inst::Str {
                                        size: OperandSize::B64,
                                        src: arg_regs[i],
                                        addr: MemAddr::BaseOffset {
                                            base: Reg::X29, // fp
                                            offset: actual_offset,
                                        },
                                    });
                                }
                            }
                            break;
                        }
                    }
                }
            }
        }

        // Store frame size for epilogue
        let frame_info = (total_frame, callee_saved.clone(), callee_saved_fp.clone());

        // Emit basic blocks
        for block in &func.blocks {
            self.emit_block(block, &frame_info, types);
        }

        // CFI: End procedure
        if self.emit_unwind_tables {
            self.push_lir(Aarch64Inst::Directive(Directive::CfiEndProc));
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
            &self.current_fn,
            block.id.0,
        ))));

        // Emit instructions
        for insn in &block.insns {
            self.emit_insn(insn, frame_info, types);
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
                // Move return value to x0 (integer) or v0 (float) if present
                if let Some(&src) = insn.src.first() {
                    let src_loc = self.get_location(src);
                    let is_fp = matches!(src_loc, Loc::VReg(_) | Loc::FImm(..));

                    if is_fp {
                        // FP return value goes in V0
                        self.emit_fp_move(src, VReg::V0, insn.size, *total_frame);
                    } else {
                        // Integer return value goes in X0
                        self.emit_move(src, Reg::X0, insn.size, *total_frame);
                    }
                }

                // Epilogue: reset SP to FP first (required for alloca support, since
                // alloca may have moved SP during function execution)
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::X29),
                    dst: Reg::SP,
                });

                // Restore callee-saved registers (now safe since SP == FP)
                if *total_frame > 16 {
                    let mut offset = 16;
                    let mut i = 0;
                    while i < callee_saved.len() {
                        if i + 1 < callee_saved.len() {
                            // LIR: ldp pair restore
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
                            // LIR: ldr single restore
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
                let fp = Reg::fp();
                let lr = Reg::lr();
                let dealloc = if *total_frame > 0 { *total_frame } else { 16 };
                // LIR: ldp with post-increment to restore fp/lr and deallocate
                self.push_lir(Aarch64Inst::Ldp {
                    size: OperandSize::B64,
                    addr: MemAddr::PostIndex {
                        base: Reg::sp(),
                        offset: dealloc,
                    },
                    dst1: fp,
                    dst2: lr,
                });
                // LIR: ret
                self.push_lir(Aarch64Inst::Ret);
            }

            Opcode::Br => {
                if let Some(target) = insn.bb_true {
                    // LIR: unconditional branch
                    self.push_lir(Aarch64Inst::B {
                        target: Label::new(&self.current_fn, target.0),
                    });
                }
            }

            Opcode::Cbr => {
                if let Some(&cond) = insn.src.first() {
                    let loc = self.get_location(cond);
                    let (scratch0, _) = Reg::scratch_regs();
                    match &loc {
                        Loc::Reg(r) => {
                            // LIR: compare register with zero
                            self.push_lir(Aarch64Inst::Cmp {
                                size: OperandSize::B64,
                                src1: *r,
                                src2: GpOperand::Imm(0),
                            });
                        }
                        Loc::Stack(offset) => {
                            let actual_offset = self.stack_offset(*total_frame, *offset);
                            // LIR: load from stack (FP-relative for alloca safety)
                            self.push_lir(Aarch64Inst::Ldr {
                                size: OperandSize::B64,
                                addr: MemAddr::BaseOffset {
                                    base: Reg::X29,
                                    offset: actual_offset,
                                },
                                dst: scratch0,
                            });
                            // LIR: compare with zero
                            self.push_lir(Aarch64Inst::Cmp {
                                size: OperandSize::B64,
                                src1: scratch0,
                                src2: GpOperand::Imm(0),
                            });
                        }
                        Loc::Imm(v) => {
                            if *v != 0 {
                                if let Some(target) = insn.bb_true {
                                    // LIR: unconditional branch (constant true)
                                    self.push_lir(Aarch64Inst::B {
                                        target: Label::new(&self.current_fn, target.0),
                                    });
                                }
                                return;
                            } else {
                                if let Some(target) = insn.bb_false {
                                    // LIR: unconditional branch (constant false)
                                    self.push_lir(Aarch64Inst::B {
                                        target: Label::new(&self.current_fn, target.0),
                                    });
                                }
                                return;
                            }
                        }
                        Loc::Global(name) => {
                            self.emit_load_global(name, scratch0, OperandSize::B64);
                            // LIR: compare with zero
                            self.push_lir(Aarch64Inst::Cmp {
                                size: OperandSize::B64,
                                src1: scratch0,
                                src2: GpOperand::Imm(0),
                            });
                        }
                        Loc::VReg(v) => {
                            // LIR: compare FP register with zero
                            self.push_lir(Aarch64Inst::FcmpZero {
                                size: FpSize::Double,
                                src: *v,
                            });
                        }
                        Loc::FImm(f, _) => {
                            // FP immediate as condition - branch based on non-zero
                            if *f != 0.0 {
                                if let Some(target) = insn.bb_true {
                                    // LIR: unconditional branch (constant true)
                                    self.push_lir(Aarch64Inst::B {
                                        target: Label::new(&self.current_fn, target.0),
                                    });
                                }
                                return;
                            } else {
                                if let Some(target) = insn.bb_false {
                                    // LIR: unconditional branch (constant false)
                                    self.push_lir(Aarch64Inst::B {
                                        target: Label::new(&self.current_fn, target.0),
                                    });
                                }
                                return;
                            }
                        }
                    }
                    if let Some(target) = insn.bb_true {
                        // LIR: conditional branch (not equal = true)
                        self.push_lir(Aarch64Inst::BCond {
                            cond: Cond::Ne,
                            target: Label::new(&self.current_fn, target.0),
                        });
                    }
                    if let Some(target) = insn.bb_false {
                        // LIR: unconditional branch (fallthrough to false)
                        self.push_lir(Aarch64Inst::B {
                            target: Label::new(&self.current_fn, target.0),
                        });
                    }
                }
            }

            Opcode::Switch => {
                // Switch uses target as the value to switch on
                if let Some(val) = insn.target {
                    let loc = self.get_location(val);
                    let (scratch0, scratch1) = Reg::scratch_regs();
                    let size = insn.size.max(32);
                    let op_size = OperandSize::from_bits(size);

                    // Move switch value to scratch0
                    match &loc {
                        Loc::Reg(r) => {
                            if *r != scratch0 {
                                // LIR: move register to scratch
                                self.push_lir(Aarch64Inst::Mov {
                                    size: op_size,
                                    src: GpOperand::Reg(*r),
                                    dst: scratch0,
                                });
                            }
                        }
                        Loc::Stack(offset) => {
                            let actual_offset = self.stack_offset(*total_frame, *offset);
                            // LIR: load from stack (FP-relative for alloca safety)
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
                            // LIR: move immediate
                            self.push_lir(Aarch64Inst::Mov {
                                size: op_size,
                                src: GpOperand::Imm(*v),
                                dst: scratch0,
                            });
                        }
                        Loc::Global(name) => {
                            self.emit_load_global(name, scratch0, op_size);
                        }
                        Loc::VReg(_) | Loc::FImm(..) => {
                            // FP values shouldn't be used in switch statements
                            // This is unreachable in valid C code
                        }
                    }

                    // Generate comparisons for each case
                    for (case_val, target_bb) in &insn.switch_cases {
                        // Load case value to scratch1 if it doesn't fit in immediate
                        if *case_val >= 0 && *case_val < 4096 {
                            // LIR: compare with immediate
                            self.push_lir(Aarch64Inst::Cmp {
                                size: op_size,
                                src1: scratch0,
                                src2: GpOperand::Imm(*case_val),
                            });
                        } else {
                            // LIR: load large constant
                            self.push_lir(Aarch64Inst::Mov {
                                size: op_size,
                                src: GpOperand::Imm(*case_val),
                                dst: scratch1,
                            });
                            // LIR: compare registers
                            self.push_lir(Aarch64Inst::Cmp {
                                size: op_size,
                                src1: scratch0,
                                src2: GpOperand::Reg(scratch1),
                            });
                        }
                        // LIR: conditional branch on equal
                        self.push_lir(Aarch64Inst::BCond {
                            cond: Cond::Eq,
                            target: Label::new(&self.current_fn, target_bb.0),
                        });
                    }

                    // Jump to default
                    if let Some(default_bb) = insn.switch_default {
                        // LIR: unconditional branch to default
                        self.push_lir(Aarch64Inst::B {
                            target: Label::new(&self.current_fn, default_bb.0),
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

            Opcode::Neg => {
                self.emit_neg(insn, *total_frame);
            }

            Opcode::Not => {
                self.emit_not(insn, *total_frame);
            }

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
                                    let (scratch0, _) = Reg::scratch_regs();
                                    let bits = if insn.size <= 32 {
                                        (*f as f32).to_bits() as i64
                                    } else {
                                        f.to_bits() as i64
                                    };
                                    self.emit_mov_imm(scratch0, bits, 64);
                                    // LIR: fmov from GP to FP register
                                    let fp_size = if insn.size <= 32 {
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
                    let dst_reg = match &dst_loc {
                        Loc::Reg(r) => *r,
                        _ => Reg::X9,
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
                self.emit_fp_binop(insn, *total_frame);
            }

            // Floating-point negation
            Opcode::FNeg => {
                self.emit_fp_neg(insn, *total_frame);
            }

            // Floating-point comparisons
            Opcode::FCmpOEq
            | Opcode::FCmpONe
            | Opcode::FCmpOLt
            | Opcode::FCmpOLe
            | Opcode::FCmpOGt
            | Opcode::FCmpOGe => {
                self.emit_fp_compare(insn, *total_frame);
            }

            // Int to float conversions
            Opcode::UCvtF | Opcode::SCvtF => {
                self.emit_int_to_float(insn, *total_frame);
            }

            // Float to int conversions
            Opcode::FCvtU | Opcode::FCvtS => {
                self.emit_float_to_int(insn, *total_frame);
            }

            // Float to float conversions (size changes)
            Opcode::FCvtF => {
                self.emit_float_to_float(insn, *total_frame);
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

            // ================================================================
            // Byte-swapping builtins
            // ================================================================
            Opcode::Bswap16 => {
                self.emit_bswap16(insn, *total_frame);
            }

            Opcode::Bswap32 => {
                self.emit_bswap32(insn, *total_frame);
            }

            Opcode::Bswap64 => {
                self.emit_bswap64(insn, *total_frame);
            }

            // ================================================================
            // Count trailing zeros builtins
            // ================================================================
            Opcode::Ctz32 => {
                self.emit_ctz32(insn, *total_frame);
            }

            Opcode::Ctz64 => {
                self.emit_ctz64(insn, *total_frame);
            }

            // ================================================================
            // Count leading zeros builtins
            // ================================================================
            Opcode::Clz32 => {
                self.emit_clz32(insn, *total_frame);
            }

            Opcode::Clz64 => {
                self.emit_clz64(insn, *total_frame);
            }

            // ================================================================
            // Population count builtins
            // ================================================================
            Opcode::Popcount32 => {
                self.emit_popcount32(insn, *total_frame);
            }

            Opcode::Popcount64 => {
                self.emit_popcount64(insn, *total_frame);
            }

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

            // Skip no-ops and unimplemented
            _ => {}
        }
    }

    fn get_location(&self, pseudo: PseudoId) -> Loc {
        self.locations.get(&pseudo).cloned().unwrap_or(Loc::Imm(0))
    }

    /// Load address of a global symbol into a register
    fn emit_load_addr(&mut self, name: &str, dst: Reg) {
        // Local labels (starting with '.') don't get the _ prefix on macOS
        let sym = if name.starts_with('.') {
            Symbol::local(name)
        } else {
            Symbol::global(name)
        };

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

    /// Load value of a global symbol into a register with specified size
    fn emit_load_global(&mut self, name: &str, dst: Reg, size: OperandSize) {
        // Local labels (starting with '.') don't get the _ prefix on macOS
        let sym = if name.starts_with('.') {
            Symbol::local(name)
        } else {
            Symbol::global(name)
        };

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

    /// Move immediate value to register
    fn emit_mov_imm(&mut self, dst: Reg, val: i64, size: u32) {
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

    fn emit_move(&mut self, src: PseudoId, dst: Reg, size: u32, frame_size: i32) {
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
                // LIR: load from stack (FP-relative for alloca safety)
                self.push_lir(Aarch64Inst::Ldr {
                    size: op_size,
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

    fn emit_move_to_loc(&mut self, src: Reg, dst: &Loc, size: u32, frame_size: i32) {
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

    fn emit_binop(&mut self, insn: &Instruction, frame_size: i32) {
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
            _ => Reg::X9,
        };

        // Load first operand
        self.emit_move(src1, work_reg, size, frame_size);

        // Get second operand as GpOperand
        let src2_loc = self.get_location(src2);
        let src2_operand = match &src2_loc {
            Loc::Reg(r) => GpOperand::Reg(*r),
            Loc::Imm(v) if *v >= 0 && *v <= 4095 => GpOperand::Imm(*v),
            _ => {
                self.emit_move(src2, Reg::X10, size, frame_size);
                GpOperand::Reg(Reg::X10)
            }
        };

        // Emit the appropriate LIR instruction
        match insn.op {
            Opcode::Add => self.push_lir(Aarch64Inst::Add {
                size: op_size,
                src1: work_reg,
                src2: src2_operand,
                dst: work_reg,
            }),
            Opcode::Sub => self.push_lir(Aarch64Inst::Sub {
                size: op_size,
                src1: work_reg,
                src2: src2_operand,
                dst: work_reg,
            }),
            Opcode::And => self.push_lir(Aarch64Inst::And {
                size: op_size,
                src1: work_reg,
                src2: src2_operand,
                dst: work_reg,
            }),
            Opcode::Or => self.push_lir(Aarch64Inst::Orr {
                size: op_size,
                src1: work_reg,
                src2: src2_operand,
                dst: work_reg,
            }),
            Opcode::Xor => self.push_lir(Aarch64Inst::Eor {
                size: op_size,
                src1: work_reg,
                src2: src2_operand,
                dst: work_reg,
            }),
            Opcode::Shl => self.push_lir(Aarch64Inst::Lsl {
                size: op_size,
                src: work_reg,
                amount: src2_operand,
                dst: work_reg,
            }),
            Opcode::Lsr => self.push_lir(Aarch64Inst::Lsr {
                size: op_size,
                src: work_reg,
                amount: src2_operand,
                dst: work_reg,
            }),
            Opcode::Asr => self.push_lir(Aarch64Inst::Asr {
                size: op_size,
                src: work_reg,
                amount: src2_operand,
                dst: work_reg,
            }),
            _ => return,
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, size, frame_size);
        }
    }

    fn emit_mul(&mut self, insn: &Instruction, frame_size: i32) {
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
            _ => Reg::X9,
        };

        self.emit_move(src1, Reg::X10, size, frame_size);
        self.emit_move(src2, Reg::X11, size, frame_size);

        self.push_lir(Aarch64Inst::Mul {
            size: op_size,
            src1: Reg::X10,
            src2: Reg::X11,
            dst: dst_reg,
        });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size, frame_size);
        }
    }

    fn emit_div(&mut self, insn: &Instruction, frame_size: i32) {
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

        self.emit_move(src1, Reg::X10, size, frame_size);
        self.emit_move(src2, Reg::X11, size, frame_size);

        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X9,
        };

        // Division instruction
        match insn.op {
            Opcode::DivS | Opcode::ModS => self.push_lir(Aarch64Inst::Sdiv {
                size: op_size,
                src1: Reg::X10,
                src2: Reg::X11,
                dst: dst_reg,
            }),
            Opcode::DivU | Opcode::ModU => self.push_lir(Aarch64Inst::Udiv {
                size: op_size,
                src1: Reg::X10,
                src2: Reg::X11,
                dst: dst_reg,
            }),
            _ => return,
        }

        // For modulo, compute remainder: r = n - (n / d) * d
        // Using msub: msub Rd, Rm, Rn, Ra -> Rd = Ra - Rm * Rn
        if matches!(insn.op, Opcode::ModS | Opcode::ModU) {
            // dst_reg now has quotient, compute: src1 - quotient * src2
            self.push_lir(Aarch64Inst::Msub {
                size: op_size,
                mul1: dst_reg,
                mul2: Reg::X11,
                sub: Reg::X10,
                dst: dst_reg,
            });
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size, frame_size);
        }
    }

    fn emit_compare(&mut self, insn: &Instruction, frame_size: i32) {
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

        self.emit_move(src1, Reg::X10, size, frame_size);

        // Try to use immediate for comparison if possible
        let src2_loc = self.get_location(src2);
        let src2_operand = match &src2_loc {
            Loc::Imm(v) if *v >= 0 && *v <= 4095 => GpOperand::Imm(*v),
            _ => {
                self.emit_move(src2, Reg::X11, size, frame_size);
                GpOperand::Reg(Reg::X11)
            }
        };

        self.push_lir(Aarch64Inst::Cmp {
            size: op_size,
            src1: Reg::X10,
            src2: src2_operand,
        });

        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X9,
        };

        // Use cset to set register based on condition
        let cond = match insn.op {
            Opcode::SetEq => Cond::Eq,
            Opcode::SetNe => Cond::Ne,
            Opcode::SetLt => Cond::Lt,
            Opcode::SetLe => Cond::Le,
            Opcode::SetGt => Cond::Gt,
            Opcode::SetGe => Cond::Ge,
            Opcode::SetB => Cond::Cc,  // unsigned less than (lo)
            Opcode::SetBe => Cond::Ls, // unsigned less than or equal
            Opcode::SetA => Cond::Hi,  // unsigned greater than
            Opcode::SetAe => Cond::Cs, // unsigned greater than or equal (hs)
            _ => return,
        };

        self.push_lir(Aarch64Inst::Cset { cond, dst: dst_reg });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size, frame_size);
        }
    }

    fn emit_neg(&mut self, insn: &Instruction, frame_size: i32) {
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
            _ => Reg::X9,
        };

        self.emit_move(src, work_reg, size, frame_size);
        self.push_lir(Aarch64Inst::Neg {
            size: op_size,
            src: work_reg,
            dst: work_reg,
        });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, size, frame_size);
        }
    }

    fn emit_not(&mut self, insn: &Instruction, frame_size: i32) {
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
            _ => Reg::X9,
        };

        self.emit_move(src, work_reg, size, frame_size);
        self.push_lir(Aarch64Inst::Mvn {
            size: op_size,
            src: work_reg,
            dst: work_reg,
        });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == work_reg) {
            self.emit_move_to_loc(work_reg, &dst_loc, size, frame_size);
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
            self.emit_fp_load(insn, frame_size);
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
                !self.target.char_signed
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

        let addr_loc = self.get_location(addr);
        match addr_loc {
            Loc::Reg(r) => {
                let mem_addr = MemAddr::BaseOffset {
                    base: r,
                    offset: insn.offset as i32,
                };
                emit_load_lir(self, mem_addr);
            }
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                if is_symbol {
                    // Local variable - load directly from stack slot (FP-relative for alloca safety)
                    let total_offset = self.stack_offset(frame_size, offset) + insn.offset as i32;
                    let mem_addr = MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: total_offset,
                    };
                    emit_load_lir(self, mem_addr);
                } else {
                    // Spilled address - load address first (FP-relative for alloca safety)
                    let adjusted = self.stack_offset(frame_size, offset);
                    self.push_lir(Aarch64Inst::Ldr {
                        size: OperandSize::B64,
                        addr: MemAddr::BaseOffset {
                            base: Reg::X29,
                            offset: adjusted,
                        },
                        dst: Reg::X16,
                    });
                    let mem_addr = MemAddr::BaseOffset {
                        base: Reg::X16,
                        offset: insn.offset as i32,
                    };
                    emit_load_lir(self, mem_addr);
                }
            }
            Loc::Global(name) => {
                // Load from global with correct size
                let load_size = OperandSize::from_bits(mem_size);
                self.emit_load_global(&name, dst_reg, load_size);
            }
            _ => {
                self.emit_move(addr, Reg::X16, 64, frame_size);
                let mem_addr = MemAddr::BaseOffset {
                    base: Reg::X16,
                    offset: insn.offset as i32,
                };
                emit_load_lir(self, mem_addr);
            }
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, reg_size, frame_size);
        }
    }

    /// Emit a floating-point load instruction
    fn emit_fp_load(&mut self, insn: &Instruction, frame_size: i32) {
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
        let dst_vreg = match &dst_loc {
            Loc::VReg(v) => *v,
            _ => VReg::V17,
        };
        let addr_loc = self.get_location(addr);

        let fp_size = if size <= 32 {
            FpSize::Single
        } else {
            FpSize::Double
        };

        match addr_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::LdrFp {
                    size: fp_size,
                    addr: MemAddr::BaseOffset {
                        base: r,
                        offset: insn.offset as i32,
                    },
                    dst: dst_vreg,
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
                    // Local variable - load directly from stack slot (FP-relative for alloca safety)
                    let total_offset = self.stack_offset(frame_size, offset) + insn.offset as i32;
                    self.push_lir(Aarch64Inst::LdrFp {
                        size: fp_size,
                        addr: MemAddr::BaseOffset {
                            base: Reg::X29,
                            offset: total_offset,
                        },
                        dst: dst_vreg,
                    });
                } else {
                    // Spilled address - load address first (FP-relative for alloca safety)
                    let adjusted = self.stack_offset(frame_size, offset);
                    self.push_lir(Aarch64Inst::Ldr {
                        size: OperandSize::B64,
                        addr: MemAddr::BaseOffset {
                            base: Reg::X29,
                            offset: adjusted,
                        },
                        dst: Reg::X16,
                    });
                    self.push_lir(Aarch64Inst::LdrFp {
                        size: fp_size,
                        addr: MemAddr::BaseOffset {
                            base: Reg::X16,
                            offset: insn.offset as i32,
                        },
                        dst: dst_vreg,
                    });
                }
            }
            Loc::Global(name) => {
                // Load FP value from global using ADRP + LDR
                let sym = if name.starts_with('.') {
                    Symbol::local(&name)
                } else {
                    Symbol::global(&name)
                };
                let (scratch0, _) = Reg::scratch_regs();
                self.push_lir(Aarch64Inst::Adrp {
                    sym: sym.clone(),
                    dst: scratch0,
                });
                self.push_lir(Aarch64Inst::LdrFpSymOffset {
                    size: fp_size,
                    sym,
                    base: scratch0,
                    dst: dst_vreg,
                });
            }
            _ => {
                self.emit_move(addr, Reg::X16, 64, frame_size);
                self.push_lir(Aarch64Inst::LdrFp {
                    size: fp_size,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X16,
                        offset: insn.offset as i32,
                    },
                    dst: dst_vreg,
                });
            }
        }

        // Move to final destination if needed
        if !matches!(&dst_loc, Loc::VReg(v) if *v == dst_vreg) {
            self.emit_fp_move_to_loc(dst_vreg, &dst_loc, size, frame_size);
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

        let addr_loc = self.get_location(addr);
        match addr_loc {
            Loc::Reg(r) => {
                let mem_addr = MemAddr::BaseOffset {
                    base: r,
                    offset: insn.offset as i32,
                };
                emit_store_lir(self, mem_addr);
            }
            Loc::Stack(offset) => {
                // Check if the address operand is a symbol (local variable) or a temp (spilled address)
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                if is_symbol {
                    // Local variable - store directly to stack slot (FP-relative for alloca safety)
                    let total_offset = self.stack_offset(frame_size, offset) + insn.offset as i32;
                    let mem_addr = MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: total_offset,
                    };
                    emit_store_lir(self, mem_addr);
                } else {
                    // Spilled address - load address first (FP-relative for alloca safety)
                    let adjusted = self.stack_offset(frame_size, offset);
                    self.push_lir(Aarch64Inst::Ldr {
                        size: OperandSize::B64,
                        addr: MemAddr::BaseOffset {
                            base: Reg::X29,
                            offset: adjusted,
                        },
                        dst: Reg::X16,
                    });
                    let mem_addr = MemAddr::BaseOffset {
                        base: Reg::X16,
                        offset: insn.offset as i32,
                    };
                    emit_store_lir(self, mem_addr);
                }
            }
            Loc::Global(name) => {
                self.emit_load_addr(&name, Reg::X16);
                let mem_addr = MemAddr::Base(Reg::X16);
                emit_store_lir(self, mem_addr);
            }
            _ => {
                self.emit_move(addr, Reg::X16, 64, frame_size);
                let mem_addr = MemAddr::BaseOffset {
                    base: Reg::X16,
                    offset: insn.offset as i32,
                };
                emit_store_lir(self, mem_addr);
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
        let num_qwords = (struct_size + 63) / 64;

        // Get source address (where the struct data is)
        let value_loc = self.get_location(value);
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

    fn emit_call(&mut self, insn: &Instruction, frame_size: i32, types: &TypeTable) {
        let func_name = match &insn.func_name {
            Some(n) => n.clone(),
            None => return,
        };

        // AAPCS64 calling convention:
        // - Integer arguments: X0-X7 (8 registers)
        // - Floating-point arguments: V0-V7 (8 registers)
        // - Indirect result (large struct return): X8
        // Each class has its own register allocation
        //
        // Apple ARM64 (Darwin) divergence for variadic functions:
        // ALL variadic arguments (after the last named parameter) must be passed
        // on the stack, not in registers. This differs from standard AAPCS64.
        let int_arg_regs = Reg::arg_regs();
        let fp_arg_regs = VReg::arg_regs();

        let mut int_arg_idx = 0;
        let mut fp_arg_idx = 0;
        let mut stack_args = 0;

        // For Darwin variadic calls, determine where variadic args start
        let variadic_start = insn.variadic_arg_start.unwrap_or(usize::MAX);
        let is_darwin_variadic =
            self.target.os == crate::target::Os::MacOS && insn.variadic_arg_start.is_some();

        // Check if this call returns a large struct via sret (hidden pointer argument).
        // The linearizer sets is_sret_call=true and puts the sret pointer as the first arg.
        let args_start = if insn.is_sret_call && !insn.src.is_empty() {
            // First argument is sret pointer - move to X8
            self.emit_move(insn.src[0], Reg::X8, 64, frame_size);
            1 // Skip first arg in main loop
        } else {
            0
        };

        // For Darwin variadic calls, we need to:
        // 1. First pass fixed args in registers as normal
        // 2. Allocate stack space for variadic args (with 16-byte alignment)
        // 3. Store variadic args at 8-byte offsets from sp (like clang does)
        if is_darwin_variadic {
            // Collect variadic args and fixed args separately
            let mut variadic_args: Vec<(PseudoId, bool, u32)> = Vec::new();

            // Process all arguments
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
                    // Variadic arg - collect for stack storing
                    variadic_args.push((arg, is_fp, arg_size));
                } else {
                    // Fixed arg - use registers as normal
                    if is_fp {
                        let fp_size = if let Some(typ) = arg_type {
                            types.size_bits(typ)
                        } else {
                            64
                        };
                        if fp_arg_idx < fp_arg_regs.len() {
                            self.emit_fp_move(arg, fp_arg_regs[fp_arg_idx], fp_size, frame_size);
                            fp_arg_idx += 1;
                        }
                    } else if int_arg_idx < int_arg_regs.len() {
                        self.emit_move(arg, int_arg_regs[int_arg_idx], arg_size, frame_size);
                        int_arg_idx += 1;
                    }
                }
            }

            // Calculate stack space for variadic args: 8 bytes each, rounded up to 16
            let num_variadic = variadic_args.len();
            if num_variadic > 0 {
                let variadic_bytes = (num_variadic * 8) as i32;
                let aligned_bytes = (variadic_bytes + 15) & !15; // Round up to 16

                // Allocate stack space for variadic args
                self.push_lir(Aarch64Inst::Sub {
                    size: OperandSize::B64,
                    src1: Reg::sp(),
                    src2: GpOperand::Imm(aligned_bytes as i64),
                    dst: Reg::sp(),
                });

                // Store variadic args at 8-byte offsets from sp
                // First variadic arg at [sp+0], second at [sp+8], etc.
                for (idx, (arg, is_fp, arg_size)) in variadic_args.into_iter().enumerate() {
                    let offset = (idx * 8) as i32;
                    if is_fp {
                        self.emit_fp_move(arg, VReg::V16, arg_size, frame_size);
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

                // Track stack space for cleanup
                stack_args = (aligned_bytes + 15) / 16; // Number of 16-byte units
            }
        } else {
            // Standard AAPCS64 (Linux, FreeBSD) or non-variadic calls
            // Move arguments to registers
            for (i, &arg) in insn.src.iter().enumerate().skip(args_start) {
                // Get argument type if available, otherwise fall back to location-based detection
                let arg_type = insn.arg_types.get(i).copied();
                let is_fp = if let Some(typ) = arg_type {
                    types.is_float(typ)
                } else {
                    // Fall back to location-based detection for backwards compatibility
                    let arg_loc = self.get_location(arg);
                    matches!(arg_loc, Loc::VReg(_) | Loc::FImm(..))
                };

                // Get argument size from type, with minimum 32-bit for register ops
                let arg_size = if let Some(typ) = arg_type {
                    types.size_bits(typ).max(32)
                } else {
                    64 // Default for backwards compatibility
                };

                if is_fp {
                    // FP size from type (32 for float, 64 for double)
                    let fp_size = if let Some(typ) = arg_type {
                        types.size_bits(typ)
                    } else {
                        64
                    };
                    if fp_arg_idx < fp_arg_regs.len() {
                        self.emit_fp_move(arg, fp_arg_regs[fp_arg_idx], fp_size, frame_size);
                        fp_arg_idx += 1;
                    } else {
                        // FP arg on stack
                        self.emit_fp_move(arg, VReg::V16, fp_size, frame_size);
                        // LIR: store FP reg to stack with pre-decrement
                        let fp_sz = if fp_size == 32 {
                            FpSize::Single
                        } else {
                            FpSize::Double
                        };
                        self.push_lir(Aarch64Inst::StrFp {
                            size: fp_sz,
                            src: VReg::V16,
                            addr: MemAddr::PreIndex {
                                base: Reg::SP,
                                offset: -16,
                            },
                        });
                        stack_args += 1;
                    }
                } else if int_arg_idx < int_arg_regs.len() {
                    self.emit_move(arg, int_arg_regs[int_arg_idx], arg_size, frame_size);
                    int_arg_idx += 1;
                } else {
                    // Integer arg on stack - always store 8 bytes on aarch64
                    self.emit_move(arg, Reg::X9, arg_size, frame_size);
                    // LIR: store GP reg to stack with pre-decrement
                    self.push_lir(Aarch64Inst::Str {
                        size: OperandSize::B64,
                        src: Reg::X9,
                        addr: MemAddr::PreIndex {
                            base: Reg::SP,
                            offset: -16,
                        },
                    });
                    stack_args += 1;
                }
            }
        }

        // Call the function
        self.push_lir(Aarch64Inst::Bl {
            target: CallTarget::Direct(Symbol::global(&func_name)),
        });

        // Clean up stack arguments
        if stack_args > 0 {
            self.push_lir(Aarch64Inst::Add {
                size: OperandSize::B64,
                src1: Reg::sp(),
                src2: GpOperand::Imm((stack_args * 16) as i64),
                dst: Reg::sp(),
            });
        }

        // Move return value if needed
        if let Some(target) = insn.target {
            let dst_loc = self.get_location(target);
            // Check if return value is floating-point based on type or location
            let is_fp_result = if let Some(typ) = insn.typ {
                types.is_float(typ)
            } else {
                matches!(dst_loc, Loc::VReg(_) | Loc::FImm(..))
            };

            // Get return value size from type
            let ret_size = insn.size.max(32);

            if is_fp_result {
                // FP return value is in V0
                self.emit_fp_move_to_loc(VReg::V0, &dst_loc, ret_size, frame_size);
            } else {
                // Integer return value is in X0
                self.emit_move_to_loc(Reg::X0, &dst_loc, ret_size, frame_size);
            }
        }
    }

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
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X9,
        };

        // Load condition, then and else values
        self.emit_move(cond, Reg::X10, 64, frame_size);
        self.emit_move(then_val, Reg::X11, size, frame_size);
        self.emit_move(else_val, Reg::X12, size, frame_size);

        // LIR: compare condition with zero
        self.push_lir(Aarch64Inst::Cmp {
            size: OperandSize::B64,
            src1: Reg::X10,
            src2: GpOperand::Imm(0),
        });

        // Use csel: if cond != 0, select then_val, else select else_val
        self.push_lir(Aarch64Inst::Csel {
            size: op_size,
            cond: Cond::Ne,
            src_true: Reg::X11,
            src_false: Reg::X12,
            dst: dst_reg,
        });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size, frame_size);
        }
    }

    fn emit_extend(&mut self, insn: &Instruction, frame_size: i32) {
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
            _ => Reg::X9,
        };

        match insn.op {
            Opcode::Zext => {
                // Zero extend: use uxtb, uxth, or just mov for 32->64
                self.emit_move(src, dst_reg, 64, frame_size);
                match insn.size {
                    8 => {
                        self.push_lir(Aarch64Inst::Uxtb {
                            src: dst_reg,
                            dst: dst_reg,
                        });
                    }
                    16 => {
                        self.push_lir(Aarch64Inst::Uxth {
                            src: dst_reg,
                            dst: dst_reg,
                        });
                    }
                    32 => {
                        // 32-bit ops automatically zero-extend on AArch64
                    }
                    _ => {}
                }
            }
            Opcode::Sext => {
                // Sign extend: use sxtb, sxth, sxtw based on source size
                self.emit_move(src, dst_reg, 64, frame_size);
                match insn.src_size {
                    8 => {
                        self.push_lir(Aarch64Inst::Sxtb {
                            dst_size: OperandSize::B64,
                            src: dst_reg,
                            dst: dst_reg,
                        });
                    }
                    16 => {
                        self.push_lir(Aarch64Inst::Sxth {
                            dst_size: OperandSize::B64,
                            src: dst_reg,
                            dst: dst_reg,
                        });
                    }
                    32 => {
                        self.push_lir(Aarch64Inst::Sxtw {
                            src: dst_reg,
                            dst: dst_reg,
                        });
                    }
                    _ => {}
                }
            }
            Opcode::Trunc => {
                // Truncate: move value then mask to target size
                self.emit_move(src, dst_reg, 64, frame_size);
                // Mask to target size using AND
                match insn.size {
                    8 => {
                        self.push_lir(Aarch64Inst::And {
                            size: OperandSize::B32,
                            src1: dst_reg,
                            src2: GpOperand::Imm(0xff),
                            dst: dst_reg,
                        });
                    }
                    16 => {
                        self.push_lir(Aarch64Inst::And {
                            size: OperandSize::B32,
                            src1: dst_reg,
                            src2: GpOperand::Imm(0xffff),
                            dst: dst_reg,
                        });
                    }
                    32 => {
                        // 32-bit already handled - writing to w register zeros upper 32 bits
                    }
                    _ => {}
                }
            }
            _ => {}
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, insn.size, frame_size);
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
                !self.target.char_signed
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

            self.emit_fp_move(src, dst_vreg, reg_size, frame_size);

            if !matches!(&dst_loc, Loc::VReg(v) if *v == dst_vreg) {
                self.emit_fp_move_to_loc(dst_vreg, &dst_loc, reg_size, frame_size);
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

    // ========================================================================
    // Floating-Point Operations
    // ========================================================================

    /// Move FP value to a VReg
    fn emit_fp_move(&mut self, src: PseudoId, dst: VReg, size: u32, frame_size: i32) {
        let loc = self.get_location(src);
        let fp_size = FpSize::from_bits(size.max(32));

        match loc {
            Loc::VReg(v) if v == dst => {}
            Loc::VReg(v) => {
                self.push_lir(Aarch64Inst::FmovReg {
                    size: fp_size,
                    src: v,
                    dst,
                });
            }
            Loc::Stack(offset) => {
                let actual_offset = self.stack_offset(frame_size, offset);
                // FP-relative for alloca safety
                self.push_lir(Aarch64Inst::LdrFp {
                    size: fp_size,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                    dst,
                });
            }
            Loc::FImm(f, imm_size) => {
                // Load FP constant using integer register
                // Use the size from the FImm for correct constant representation
                let (scratch0, _) = Reg::scratch_regs();
                let bits = if imm_size <= 32 {
                    (f as f32).to_bits() as i64
                } else {
                    f.to_bits() as i64
                };
                self.emit_mov_imm(scratch0, bits, 64);
                self.push_lir(Aarch64Inst::FmovFromGp {
                    size: fp_size,
                    src: scratch0,
                    dst,
                });
            }
            Loc::Reg(r) => {
                // Move from integer register to FP register
                self.push_lir(Aarch64Inst::FmovFromGp {
                    size: fp_size,
                    src: r,
                    dst,
                });
            }
            Loc::Imm(v) => {
                // Load integer immediate and move to FP
                let (scratch0, _) = Reg::scratch_regs();
                self.emit_mov_imm(scratch0, v, 64);
                self.push_lir(Aarch64Inst::FmovFromGp {
                    size: fp_size,
                    src: scratch0,
                    dst,
                });
            }
            Loc::Global(name) => {
                // Load from global - use size matching FP precision
                let (scratch0, _) = Reg::scratch_regs();
                let load_size = match fp_size {
                    FpSize::Single => OperandSize::B32,
                    FpSize::Double => OperandSize::B64,
                };
                self.emit_load_global(&name, scratch0, load_size);
                self.push_lir(Aarch64Inst::FmovFromGp {
                    size: fp_size,
                    src: scratch0,
                    dst,
                });
            }
        }
    }

    /// Move FP register value to a location
    fn emit_fp_move_to_loc(&mut self, src: VReg, dst: &Loc, size: u32, frame_size: i32) {
        let fp_size = FpSize::from_bits(size.max(32));

        match dst {
            Loc::VReg(v) if *v == src => {}
            Loc::VReg(v) => {
                self.push_lir(Aarch64Inst::FmovReg {
                    size: fp_size,
                    src,
                    dst: *v,
                });
            }
            Loc::Stack(offset) => {
                let actual_offset = self.stack_offset(frame_size, *offset);
                // FP-relative for alloca safety
                self.push_lir(Aarch64Inst::StrFp {
                    size: fp_size,
                    src,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                });
            }
            Loc::Reg(r) => {
                // Move from FP register to integer register
                self.push_lir(Aarch64Inst::FmovToGp {
                    size: fp_size,
                    src,
                    dst: *r,
                });
            }
            _ => {}
        }
    }

    /// Emit FP binary operation (fadd, fsub, fmul, fdiv)
    fn emit_fp_binop(&mut self, insn: &Instruction, frame_size: i32) {
        let size = insn.size.max(32);
        let fp_size = FpSize::from_bits(size);
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
            Loc::VReg(v) => *v,
            _ => VReg::V16,
        };

        // Load operands
        self.emit_fp_move(src1, VReg::V17, size, frame_size);
        self.emit_fp_move(src2, VReg::V18, size, frame_size);

        match insn.op {
            Opcode::FAdd => {
                self.push_lir(Aarch64Inst::Fadd {
                    size: fp_size,
                    src1: VReg::V17,
                    src2: VReg::V18,
                    dst: work_reg,
                });
            }
            Opcode::FSub => {
                self.push_lir(Aarch64Inst::Fsub {
                    size: fp_size,
                    src1: VReg::V17,
                    src2: VReg::V18,
                    dst: work_reg,
                });
            }
            Opcode::FMul => {
                self.push_lir(Aarch64Inst::Fmul {
                    size: fp_size,
                    src1: VReg::V17,
                    src2: VReg::V18,
                    dst: work_reg,
                });
            }
            Opcode::FDiv => {
                self.push_lir(Aarch64Inst::Fdiv {
                    size: fp_size,
                    src1: VReg::V17,
                    src2: VReg::V18,
                    dst: work_reg,
                });
            }
            _ => return,
        }

        if !matches!(&dst_loc, Loc::VReg(v) if *v == work_reg) {
            self.emit_fp_move_to_loc(work_reg, &dst_loc, size, frame_size);
        }
    }

    /// Emit FP negation
    fn emit_fp_neg(&mut self, insn: &Instruction, frame_size: i32) {
        let size = insn.size.max(32);
        let fp_size = FpSize::from_bits(size);
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
            Loc::VReg(v) => *v,
            _ => VReg::V16,
        };

        self.emit_fp_move(src, VReg::V17, size, frame_size);

        self.push_lir(Aarch64Inst::Fneg {
            size: fp_size,
            src: VReg::V17,
            dst: work_reg,
        });

        if !matches!(&dst_loc, Loc::VReg(v) if *v == work_reg) {
            self.emit_fp_move_to_loc(work_reg, &dst_loc, size, frame_size);
        }
    }

    /// Emit FP comparison
    fn emit_fp_compare(&mut self, insn: &Instruction, frame_size: i32) {
        let size = insn.size.max(32);
        let fp_size = FpSize::from_bits(size);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load operands to FP registers
        self.emit_fp_move(src1, VReg::V17, size, frame_size);
        self.emit_fp_move(src2, VReg::V18, size, frame_size);

        // Perform comparison
        self.push_lir(Aarch64Inst::Fcmp {
            size: fp_size,
            src1: VReg::V17,
            src2: VReg::V18,
        });

        // Get result location
        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X9,
        };

        // Set result based on condition
        let cond = match insn.op {
            Opcode::FCmpOEq => Cond::Eq,
            Opcode::FCmpONe => Cond::Ne,
            Opcode::FCmpOLt => Cond::Lt,
            Opcode::FCmpOLe => Cond::Le,
            Opcode::FCmpOGt => Cond::Gt,
            Opcode::FCmpOGe => Cond::Ge,
            _ => return,
        };

        self.push_lir(Aarch64Inst::Cset { cond, dst: dst_reg });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, 32, frame_size);
        }
    }

    /// Emit integer to float conversion
    fn emit_int_to_float(&mut self, insn: &Instruction, frame_size: i32) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_size = insn.src_size.max(32);
        let dst_size = insn.size.max(32);
        let fp_size = FpSize::from_bits(dst_size);
        let int_size = OperandSize::from_bits(src_size);

        let dst_loc = self.get_location(target);
        let dst_vreg = match &dst_loc {
            Loc::VReg(v) => *v,
            _ => VReg::V16,
        };

        // Load source to integer register
        let (scratch0, _) = Reg::scratch_regs();
        self.emit_move(src, scratch0, src_size, frame_size);

        // Convert integer to float
        // scvtf/ucvtf: signed/unsigned int to float
        match insn.op {
            Opcode::SCvtF => {
                self.push_lir(Aarch64Inst::Scvtf {
                    int_size,
                    fp_size,
                    src: scratch0,
                    dst: dst_vreg,
                });
            }
            Opcode::UCvtF => {
                self.push_lir(Aarch64Inst::Ucvtf {
                    int_size,
                    fp_size,
                    src: scratch0,
                    dst: dst_vreg,
                });
            }
            _ => return,
        }

        if !matches!(&dst_loc, Loc::VReg(v) if *v == dst_vreg) {
            self.emit_fp_move_to_loc(dst_vreg, &dst_loc, dst_size, frame_size);
        }
    }

    /// Emit float to integer conversion
    fn emit_float_to_int(&mut self, insn: &Instruction, frame_size: i32) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_size = insn.src_size.max(32);
        let dst_size = insn.size.max(32);
        let fp_size = FpSize::from_bits(src_size);
        let int_size = OperandSize::from_bits(dst_size);

        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X9,
        };

        // Load source to FP register
        self.emit_fp_move(src, VReg::V17, src_size, frame_size);

        // Convert float to integer using truncation toward zero
        // fcvtzu/fcvtzs: float to unsigned/signed int with truncation
        match insn.op {
            Opcode::FCvtU => {
                self.push_lir(Aarch64Inst::Fcvtzu {
                    fp_size,
                    int_size,
                    src: VReg::V17,
                    dst: dst_reg,
                });
            }
            Opcode::FCvtS => {
                self.push_lir(Aarch64Inst::Fcvtzs {
                    fp_size,
                    int_size,
                    src: VReg::V17,
                    dst: dst_reg,
                });
            }
            _ => return,
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, dst_size, frame_size);
        }
    }

    /// Emit float to float conversion (size change)
    fn emit_float_to_float(&mut self, insn: &Instruction, frame_size: i32) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_size = insn.src_size.max(32);
        let dst_size = insn.size.max(32);
        let src_fp_size = FpSize::from_bits(src_size);
        let dst_fp_size = FpSize::from_bits(dst_size);

        let dst_loc = self.get_location(target);
        let dst_vreg = match &dst_loc {
            Loc::VReg(v) => *v,
            _ => VReg::V16,
        };

        // Load source to FP register
        self.emit_fp_move(src, VReg::V17, src_size, frame_size);

        // Convert between float sizes if they differ
        // fcvt: convert between single and double precision
        // Note: On Apple Silicon, long double == double (both 64-bit),
        // so skip fcvt when sizes are equal to avoid invalid "fcvt d, d"
        if src_fp_size != dst_fp_size {
            self.push_lir(Aarch64Inst::Fcvt {
                src_size: src_fp_size,
                dst_size: dst_fp_size,
                src: VReg::V17,
                dst: dst_vreg,
            });
        } else if dst_vreg != VReg::V17 {
            // Same size, just move if needed
            self.push_lir(Aarch64Inst::FmovReg {
                size: dst_fp_size,
                src: VReg::V17,
                dst: dst_vreg,
            });
        }

        if !matches!(&dst_loc, Loc::VReg(v) if *v == dst_vreg) {
            self.emit_fp_move_to_loc(dst_vreg, &dst_loc, dst_size, frame_size);
        }
    }

    // ========================================================================
    // Variadic function support (va_* builtins)
    // ========================================================================
    //
    // Platform-specific va_list handling:
    //
    // Linux/FreeBSD (AAPCS64):
    //   - va_list is a char* pointing to register save area
    //   - In prologue, we save x0-x7 to the register save area
    //   - va_start computes: reg_save_area + (num_fixed_gp_params * 8)
    //
    // Darwin (macOS/iOS):
    //   - Variadic args are passed on the stack by the caller
    //   - va_list is a char* pointing to the caller's stack
    //   - va_start computes: FP + frame_size (where caller placed variadic args)

    /// Emit va_start: Initialize va_list to point to first variadic arg
    /// Note: ap_addr is the ADDRESS of the va_list variable (from symaddr), not the va_list itself
    fn emit_va_start(&mut self, insn: &Instruction, frame_size: i32) {
        let ap_addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };

        let ap_loc = self.get_location(ap_addr);
        let (scratch0, scratch1) = Reg::scratch_regs();

        // Compute address of first variadic argument
        let is_darwin = self.target.os == crate::target::Os::MacOS;
        let vararg_offset = if is_darwin {
            // Darwin: Variadic args are on the stack, placed by the caller
            // They start at FP + frame_size (the original SP before prologue)
            frame_size
        } else {
            // Linux/FreeBSD: Variadic args are in the register save area
            // First variadic arg is at: reg_save_area_offset + (num_fixed_gp_params * 8)
            self.reg_save_area_offset + (self.num_fixed_gp_params as i32 * 8)
        };

        self.push_lir(Aarch64Inst::Add {
            size: OperandSize::B64,
            src1: Reg::X29,
            src2: GpOperand::Imm(vararg_offset as i64),
            dst: scratch0,
        });

        // ap_loc contains the ADDRESS of the va_list variable
        // We need to store scratch0 TO that address (indirect store)
        match ap_loc {
            Loc::Stack(offset) => {
                // The stack location contains the address of ap, load it first
                let actual_offset = self.stack_offset(frame_size, offset);
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                    dst: scratch1,
                });
                // Store the computed va_list pointer to *scratch1
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: scratch0,
                    addr: MemAddr::Base(scratch1),
                });
            }
            Loc::Reg(r) => {
                // r contains the address of ap, store scratch0 to [r]
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: scratch0,
                    addr: MemAddr::Base(r),
                });
            }
            _ => {}
        }
    }

    /// Emit va_arg: Get the next variadic argument of the specified type
    /// Note: ap_addr is the ADDRESS of the va_list variable (from symaddr), not the va_list itself
    fn emit_va_arg(&mut self, insn: &Instruction, frame_size: i32, types: &TypeTable) {
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
        let arg_bytes = (arg_size / 8).max(8) as i64; // Minimum 8 bytes per slot on ARM64

        let ap_loc = self.get_location(ap_addr);
        let dst_loc = self.get_location(target);
        let (scratch0, scratch1) = Reg::scratch_regs();

        // ap_loc contains the ADDRESS of the va_list variable (from symaddr)
        // First, get the address of ap into scratch1, then load ap value from there
        let ap_ptr_reg = match &ap_loc {
            Loc::Stack(offset) => {
                // Stack location contains the address of ap
                let actual_offset = self.stack_offset(frame_size, *offset);
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                    dst: scratch1,
                });
                scratch1
            }
            Loc::Reg(r) => {
                // Register contains the address of ap
                if *r != scratch1 {
                    self.push_lir(Aarch64Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(*r),
                        dst: scratch1,
                    });
                }
                scratch1
            }
            _ => return,
        };

        // Load the current ap pointer value from [ap_ptr_reg]
        self.push_lir(Aarch64Inst::Ldr {
            size: OperandSize::B64,
            addr: MemAddr::Base(ap_ptr_reg),
            dst: scratch0,
        });

        // Load the argument from *ap
        if types.is_float(arg_type) {
            // Load floating point value
            let fp_size = types.size_bits(arg_type);
            let fp_size_enum = FpSize::from_bits(fp_size);
            self.push_lir(Aarch64Inst::LdrFp {
                size: fp_size_enum,
                addr: MemAddr::Base(scratch0),
                dst: VReg::V16,
            });

            // Store to destination
            match &dst_loc {
                Loc::VReg(v) => {
                    self.push_lir(Aarch64Inst::FmovReg {
                        size: fp_size_enum,
                        src: VReg::V16,
                        dst: *v,
                    });
                }
                Loc::Stack(offset) => {
                    // FP-relative for alloca safety
                    let actual_offset = self.stack_offset(frame_size, *offset);
                    self.push_lir(Aarch64Inst::StrFp {
                        size: fp_size_enum,
                        src: VReg::V16,
                        addr: MemAddr::BaseOffset {
                            base: Reg::X29,
                            offset: actual_offset,
                        },
                    });
                }
                _ => {}
            }
        } else {
            // Load integer value
            let op_size = OperandSize::from_bits(arg_size);
            self.push_lir(Aarch64Inst::Ldr {
                size: op_size,
                addr: MemAddr::Base(scratch0),
                dst: scratch1,
            });

            // Store to destination
            match &dst_loc {
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
                    // FP-relative for alloca safety
                    let actual_offset = self.stack_offset(frame_size, *offset);
                    self.push_lir(Aarch64Inst::Str {
                        size: OperandSize::B64,
                        src: scratch1,
                        addr: MemAddr::BaseOffset {
                            base: Reg::X29,
                            offset: actual_offset,
                        },
                    });
                }
                _ => {}
            }
        }

        // Advance ap by the argument size
        self.push_lir(Aarch64Inst::Add {
            size: OperandSize::B64,
            src1: scratch0,
            src2: GpOperand::Imm(arg_bytes),
            dst: scratch0,
        });

        // Store updated ap back to [&ap] (ap_loc contains the address of ap)
        // Need to reload the address since scratch1 may have been clobbered
        match &ap_loc {
            Loc::Stack(offset) => {
                // Reload the address of ap into scratch1
                let actual_offset = self.stack_offset(frame_size, *offset);
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                    dst: scratch1,
                });
                // Store updated ap to [scratch1]
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: scratch0,
                    addr: MemAddr::Base(scratch1),
                });
            }
            Loc::Reg(r) => {
                // r contains the address of ap, store scratch0 to [r]
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: scratch0,
                    addr: MemAddr::Base(*r),
                });
            }
            _ => {}
        }
    }

    /// Emit va_copy: Copy a va_list
    /// Note: Both addresses are pointers to va_list variables (from symaddr)
    fn emit_va_copy(&mut self, insn: &Instruction, frame_size: i32) {
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
        let (scratch0, scratch1) = Reg::scratch_regs();

        // Both src_loc and dest_loc contain ADDRESSES of va_list variables
        // Get the address of src va_list into scratch1
        let src_ptr_reg = match &src_loc {
            Loc::Stack(offset) => {
                let actual_offset = self.stack_offset(frame_size, *offset);
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                    dst: scratch1,
                });
                scratch1
            }
            Loc::Reg(r) => *r,
            _ => return,
        };

        // Load the src va_list pointer value from [src_ptr_reg]
        self.push_lir(Aarch64Inst::Ldr {
            size: OperandSize::B64,
            addr: MemAddr::Base(src_ptr_reg),
            dst: scratch0,
        });

        // Get the address of dest va_list and store scratch0 there
        match &dest_loc {
            Loc::Stack(offset) => {
                let actual_offset = self.stack_offset(frame_size, *offset);
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: actual_offset,
                    },
                    dst: scratch1,
                });
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: scratch0,
                    addr: MemAddr::Base(scratch1),
                });
            }
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: scratch0,
                    addr: MemAddr::Base(*r),
                });
            }
            _ => {}
        }
    }

    // =========================================================================
    // Byte-swapping builtins
    // =========================================================================

    /// Emit bswap16: Byte-swap a 16-bit value
    /// ARM64 uses rev16 to reverse bytes within each 16-bit halfword
    fn emit_bswap16(&mut self, insn: &Instruction, frame_size: i32) {
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
                    size: OperandSize::B32,
                    src: GpOperand::Reg(r),
                    dst: scratch,
                });
            }
            Loc::Stack(off) => {
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B16,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                    dst: scratch,
                });
            }
            Loc::Imm(v) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(v),
                    dst: scratch,
                });
            }
            _ => return,
        }

        // rev16 reverses bytes within each halfword
        self.push_lir(Aarch64Inst::Rev16 {
            size: OperandSize::B32,
            src: scratch,
            dst: scratch,
        });

        // Mask to 16 bits
        self.push_lir(Aarch64Inst::And {
            size: OperandSize::B32,
            src1: scratch,
            src2: GpOperand::Imm(0xFFFF),
            dst: scratch,
        });

        // Store result
        match dst_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(scratch),
                    dst: r,
                });
            }
            Loc::Stack(off) => {
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B16,
                    src: scratch,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                });
            }
            _ => {}
        }
    }

    /// Emit bswap32: Byte-swap a 32-bit value
    fn emit_bswap32(&mut self, insn: &Instruction, frame_size: i32) {
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
                    size: OperandSize::B32,
                    src: GpOperand::Reg(r),
                    dst: scratch,
                });
            }
            Loc::Stack(off) => {
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B32,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                    dst: scratch,
                });
            }
            Loc::Imm(v) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Imm(v),
                    dst: scratch,
                });
            }
            _ => return,
        }

        // rev reverses all bytes in the register
        self.push_lir(Aarch64Inst::Rev {
            size: OperandSize::B32,
            src: scratch,
            dst: scratch,
        });

        // Store result
        match dst_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B32,
                    src: GpOperand::Reg(scratch),
                    dst: r,
                });
            }
            Loc::Stack(off) => {
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B32,
                    src: scratch,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                });
            }
            _ => {}
        }
    }

    /// Emit bswap64: Byte-swap a 64-bit value
    fn emit_bswap64(&mut self, insn: &Instruction, frame_size: i32) {
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
                    size: OperandSize::B64,
                    src: GpOperand::Reg(r),
                    dst: scratch,
                });
            }
            Loc::Stack(off) => {
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Ldr {
                    size: OperandSize::B64,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                    dst: scratch,
                });
            }
            Loc::Imm(v) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Imm(v),
                    dst: scratch,
                });
            }
            _ => return,
        }

        // rev reverses all bytes in the 64-bit register
        self.push_lir(Aarch64Inst::Rev {
            size: OperandSize::B64,
            src: scratch,
            dst: scratch,
        });

        // Store result
        match dst_loc {
            Loc::Reg(r) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(scratch),
                    dst: r,
                });
            }
            Loc::Stack(off) => {
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: scratch,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                });
            }
            _ => {}
        }
    }

    /// Emit __builtin_ctz - count trailing zeros for 32-bit value
    fn emit_ctz32(&mut self, insn: &Instruction, frame_size: i32) {
        self.emit_ctz(insn, frame_size, OperandSize::B32);
    }

    /// Emit __builtin_ctzl/__builtin_ctzll - count trailing zeros for 64-bit value
    fn emit_ctz64(&mut self, insn: &Instruction, frame_size: i32) {
        self.emit_ctz(insn, frame_size, OperandSize::B64);
    }

    /// Emit count trailing zeros - shared implementation
    /// On AArch64, CTZ is computed as CLZ(RBIT(x))
    fn emit_ctz(&mut self, insn: &Instruction, frame_size: i32, src_size: OperandSize) {
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
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Ldr {
                    size: src_size,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                    dst: scratch,
                });
            }
            Loc::Imm(v) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: src_size,
                    src: GpOperand::Imm(v),
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
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B32,
                    src: scratch,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                });
            }
            _ => {}
        }
    }

    /// Emit __builtin_clz - count leading zeros for 32-bit value
    fn emit_clz32(&mut self, insn: &Instruction, frame_size: i32) {
        self.emit_clz(insn, frame_size, OperandSize::B32);
    }

    /// Emit __builtin_clzl/__builtin_clzll - count leading zeros for 64-bit value
    fn emit_clz64(&mut self, insn: &Instruction, frame_size: i32) {
        self.emit_clz(insn, frame_size, OperandSize::B64);
    }

    /// Emit count leading zeros - shared implementation
    /// AArch64 has a direct CLZ instruction
    fn emit_clz(&mut self, insn: &Instruction, frame_size: i32, src_size: OperandSize) {
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
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Ldr {
                    size: src_size,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                    dst: scratch,
                });
            }
            Loc::Imm(v) => {
                // Use emit_mov_imm which handles large immediates with movz/movk
                self.emit_mov_imm(scratch, v, src_size.bits());
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
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B32,
                    src: scratch,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                });
            }
            _ => {}
        }
    }

    /// Emit __builtin_popcount - population count for 32-bit value
    fn emit_popcount32(&mut self, insn: &Instruction, frame_size: i32) {
        self.emit_popcount(insn, frame_size, OperandSize::B32);
    }

    /// Emit __builtin_popcountl/__builtin_popcountll - population count for 64-bit value
    fn emit_popcount64(&mut self, insn: &Instruction, frame_size: i32) {
        self.emit_popcount(insn, frame_size, OperandSize::B64);
    }

    /// Emit population count - shared implementation
    /// On AArch64, popcount is computed as:
    /// 1. fmov d0, x0   ; move GP to SIMD register
    /// 2. cnt v0.8b, v0.8b  ; count bits per byte
    /// 3. addv b0, v0.8b  ; sum all bytes
    /// 4. fmov w0, s0   ; move result back to GP
    fn emit_popcount(&mut self, insn: &Instruction, frame_size: i32, src_size: OperandSize) {
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
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Ldr {
                    size: src_size,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                    dst: scratch,
                });
            }
            Loc::Imm(v) => {
                // Use emit_mov_imm which handles large immediates with movz/movk
                self.emit_mov_imm(scratch, v, src_size.bits());
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
                // FP-relative for alloca safety
                let adjusted = frame_size + off;
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B32,
                    src: scratch,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: adjusted,
                    },
                });
            }
            _ => {}
        }
    }

    /// Emit setjmp(env) - saves execution context
    /// AAPCS64: env in X0, returns int in W0
    fn emit_setjmp(&mut self, insn: &Instruction, frame_size: i32) {
        let env = match insn.src.first() {
            Some(&e) => e,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Put env argument in X0 (first argument register)
        self.emit_move(env, Reg::X0, 64, frame_size);

        // Call setjmp
        self.push_lir(Aarch64Inst::Bl {
            target: CallTarget::Direct(Symbol::global("setjmp")),
        });

        // Store result from W0 to target
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::X0, &dst_loc, 32, frame_size);
    }

    /// Emit longjmp(env, val) - restores execution context (noreturn)
    /// AAPCS64: env in X0, val in X1
    fn emit_longjmp(&mut self, insn: &Instruction, frame_size: i32) {
        let env = match insn.src.first() {
            Some(&e) => e,
            None => return,
        };
        let val = match insn.src.get(1) {
            Some(&v) => v,
            None => return,
        };

        // IMPORTANT: Load val first into X1, THEN env into X0.
        // If we loaded env into X0 first and val was passed as the first
        // function argument (in X0), it would get overwritten.
        // Put val argument in X1 (second argument register) FIRST
        self.emit_move(val, Reg::X1, 32, frame_size);

        // Put env argument in X0 (first argument register)
        self.emit_move(env, Reg::X0, 64, frame_size);

        // Call longjmp (noreturn - control never comes back)
        self.push_lir(Aarch64Inst::Bl {
            target: CallTarget::Direct(Symbol::global("longjmp")),
        });

        // Emit brk after longjmp since it never returns
        // This helps catch any bugs where longjmp somehow returns
        self.push_lir(Aarch64Inst::Brk { imm: 1 });
    }

    /// Emit __builtin_alloca - dynamic stack allocation
    fn emit_alloca(&mut self, insn: &Instruction, frame_size: i32) {
        let size = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load size into X9 (scratch register)
        self.emit_move(size, Reg::X9, 64, frame_size);

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
        self.emit_move_to_loc(Reg::X9, &dst_loc, 64, frame_size);
    }
}

// ============================================================================
// CodeGenerator trait implementation
// ============================================================================

impl CodeGenerator for Aarch64CodeGen {
    fn generate(&mut self, module: &Module, types: &TypeTable) -> String {
        self.output.clear();
        self.lir_buffer.clear();
        self.last_debug_line = 0;
        self.last_debug_file = 0;
        self.emit_debug = module.debug;

        // Emit file header
        self.emit_header();

        // Emit .file directives unconditionally (useful for diagnostics/profiling)
        for (i, path) in module.source_files.iter().enumerate() {
            // File indices in DWARF start at 1
            self.push_lir(Aarch64Inst::Directive(Directive::file(
                (i + 1) as u32,
                path.as_str(),
            )));
        }

        // Emit globals
        for (name, typ, init) in &module.globals {
            self.emit_global(name, typ, init, types);
        }

        // Emit string literals
        self.emit_strings(&module.strings);

        // Emit functions
        for func in &module.functions {
            self.emit_function(func, types);
        }

        // Flush all buffered LIR instructions to output
        self.emit_all();

        self.output.clone()
    }

    fn set_emit_unwind_tables(&mut self, emit: bool) {
        self.emit_unwind_tables = emit;
    }
}
