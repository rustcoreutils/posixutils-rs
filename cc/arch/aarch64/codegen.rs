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

use crate::abi::{get_abi_for_conv, ArgClass, CallingConv};
use crate::arch::aarch64::features::{VA_GR_SAVE_BYTES, VA_VR_SAVE_BYTES};
use crate::arch::aarch64::lir::{Aarch64Inst, DmbOption, GpOperand, MemAddr};
use crate::arch::aarch64::regalloc::{FrameBase, IncomingOff, Loc, LocalSlot, Reg, RegAlloc, VReg};
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
    /// Current function's register allocation. M2 routes every PseudoId
    /// → Loc lookup through `LocationMap` so codegen never derives an
    /// alternative location from `PseudoKind`. The intrinsic-result
    /// sites that write into the map are the only post-allocate writers
    /// and remain visible as `.set` calls.
    locations: crate::arch::regalloc::LocationMap<Loc>,
    /// Current function's pseudos (for looking up values)
    pub(super) pseudos: Vec<Pseudo>,
    /// Total frame size for current function
    pub(super) frame_size: i32,
    /// Size of callee-saved register area (for computing local variable offsets)
    callee_saved_size: i32,
    /// Offset from FP to register save area (for variadic functions)
    pub(super) reg_save_area_offset: i32,
    /// Size of register save area (for variadic functions)
    /// Used to compute correct FP-relative offsets for local variables
    pub(super) reg_save_area_size: i32,
    /// Number of fixed GP parameters (for variadic functions)
    pub(super) num_fixed_gp_params: usize,
    /// Number of fixed FP/SIMD parameters (for variadic functions)
    pub(super) num_fixed_fp_params: usize,
    /// Bytes of incoming stack arguments consumed by named parameters
    /// (for variadic functions whose named parameters overflow the registers)
    pub(super) named_stack_param_bytes: i32,
    /// External symbols (need GOT access on macOS)
    pub(super) extern_symbols: HashSet<String>,
    /// Thread-local storage symbols (need TLS access)
    pub(super) tls_symbols: HashSet<String>,
    /// Position-independent code mode (for shared libraries)
    pic_mode: bool,
    /// Counter for generating unique labels (atomic loops, etc.)
    unique_label_counter: u32,
    /// Stack allocation size for locals (for zero_stack_frame)
    stack_alloc_size: i32,
    /// Sym pseudo ID → type size in bits (for distinguishing scalar vs struct stores)
    sym_type_sizes: HashMap<PseudoId, u32>,
    /// Which register this function's locals are addressed through
    frame_base: FrameBase,
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
            locations: crate::arch::regalloc::LocationMap::new(),
            pseudos: Vec::new(),
            frame_size: 0,
            callee_saved_size: 0,
            reg_save_area_offset: 0,
            reg_save_area_size: 0,
            num_fixed_gp_params: 0,
            num_fixed_fp_params: 0,
            named_stack_param_bytes: 0,
            extern_symbols: HashSet::new(),
            tls_symbols: HashSet::new(),
            pic_mode: false,
            unique_label_counter: 0,
            stack_alloc_size: 0,
            sym_type_sizes: HashMap::new(),
            frame_base: FrameBase::Fp,
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
    pub(super) fn stack_offset(&self, offset: i32) -> i32 {
        if offset < 0 {
            if let FrameBase::Aligned { align, .. } = self.frame_base {
                // Over-aligned locals: x19-relative addressing.
                // stack_alloc_size = base_rounded + (max_align - 1), where base_rounded
                // is round_up(stack_offset, max_align). x19 is the max_align-aligned
                // start of the locals area. Offset from x19 = base_rounded + regalloc_offset.
                // Since base_rounded is a multiple of max_align and regalloc aligns each
                // local's position to its alignment, the result preserves alignment.
                let base_rounded = self.stack_alloc_size - (align - 1);
                base_rounded + offset
            } else {
                // Local variable: use frame size minus reg_save_area
                // Layout: [fp/lr][callee-saved][locals][reg_save_area]
                // Locals are at offsets from (frame_size - reg_save_area_size)
                (self.frame_size - self.reg_save_area_size) + offset
            }
        } else {
            // Positive offset = stack args (passed by caller)
            // regalloc uses 16 as base (x86_64 convention: saved rbp + return addr)
            // but aarch64 places stack args at [x29 + frame_size + slot_offset]
            // where slot_offset = offset - 16
            self.frame_size + offset - 16
        }
    }

    /// Get the base register for a raw stack displacement.
    /// Normal locals use X29 (FP). Over-aligned locals use X19 (aligned base).
    /// Incoming args always use X29.
    ///
    /// Private and raw: callers name their frame through the typed helpers
    /// below, so that the sign is applied by the producer rather than
    /// re-derived here. See #C34.
    #[inline]
    fn stack_base_reg(&self, raw_offset: i32) -> Reg {
        match self.frame_base.reg() {
            Some(base) if raw_offset < 0 => base,
            _ => Reg::X29,
        }
    }

    #[inline]
    fn raw_mem(&self, raw_offset: i32, extra: i32) -> MemAddr {
        MemAddr::BaseOffset {
            base: self.stack_base_reg(raw_offset),
            offset: self.stack_offset(raw_offset) + extra,
        }
    }

    /// The address of a slot in the callee's own frame.
    #[inline]
    pub(super) fn stack_mem(&self, slot: LocalSlot) -> MemAddr {
        self.raw_mem(slot.displacement(), 0)
    }

    /// The address of an incoming stack argument, in the caller's frame.
    #[inline]
    pub(super) fn incoming_mem(&self, off: IncomingOff) -> MemAddr {
        self.raw_mem(off.displacement(), 0)
    }

    /// The address of byte `byte` of the object in callee-frame slot `slot`.
    #[inline]
    pub(super) fn stack_mem_plus(&self, slot: LocalSlot, extra: i32) -> MemAddr {
        self.raw_mem(slot.displacement(), extra)
    }

    /// The address of byte `byte` of an incoming stack argument.
    #[inline]
    pub(super) fn incoming_mem_plus(&self, off: IncomingOff, extra: i32) -> MemAddr {
        self.raw_mem(off.displacement(), extra)
    }

    /// The address of byte `byte` of the object in stack slot `slot`.
    #[inline]
    fn stack_field(&self, slot: LocalSlot, byte: i32) -> MemAddr {
        self.stack_mem_plus(slot, byte)
    }

    /// The base register and displacement addressing whatever frame `loc`
    /// names, for the sites that materialize an address with an `add` rather
    /// than folding it into a load or store.
    #[inline]
    pub(super) fn loc_addr_parts(&self, loc: &Loc) -> Option<(Reg, i32)> {
        let raw = match loc {
            Loc::Stack(slot) => slot.displacement(),
            Loc::IncomingArg(off) => off.displacement(),
            _ => return None,
        };
        Some((self.stack_base_reg(raw), self.stack_offset(raw)))
    }

    /// [`Self::loc_mem`] plus a byte offset into the object.
    #[inline]
    pub(super) fn loc_mem_plus(&self, loc: &Loc, extra: i32) -> Option<MemAddr> {
        let raw = match loc {
            Loc::Stack(slot) => slot.displacement(),
            Loc::IncomingArg(off) => off.displacement(),
            _ => return None,
        };
        Some(self.raw_mem(raw, extra))
    }

    /// The address a `Loc` names, whichever frame it lives in.
    ///
    /// The one place that still decides between the two, so a `match` that
    /// forgets `IncomingArg` cannot quietly address the wrong frame.
    #[inline]
    pub(super) fn loc_mem(&self, loc: &Loc) -> Option<MemAddr> {
        match loc {
            Loc::Stack(slot) => Some(self.stack_mem(*slot)),
            Loc::IncomingArg(off) => Some(self.incoming_mem(*off)),
            _ => None,
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

    /// Emit file header, declaring the ISA extensions this backend uses.
    ///
    /// `_Float16` is lowered to native half-precision instructions (`fmov h0`,
    /// `fadd h0, h1, h2`), which are an ARMv8.2-A extension. GNU as defaults to
    /// plain armv8-a and rejects every one of them with "selected processor
    /// does not support", so any translation unit touching `_Float16` failed to
    /// assemble on Linux. Apple's assembler enables fp16 for its own targets,
    /// which is why macOS never saw this.
    ///
    /// `+fp16` is added to the base architecture rather than raising it to
    /// armv8.2-a, so nothing else about the ISA baseline changes. Mach-O does
    /// not use `.arch`, so it is emitted only for ELF targets.
    fn emit_header(&mut self) {
        self.base.emit_header();
        if self.base.target.os != Os::MacOS {
            self.base
                .push_directive(Directive::Raw(".arch armv8-a+fp16".into()));
        }
    }

    /// Emit a global variable (delegates to base)
    #[inline]
    fn emit_global(&mut self, global: &crate::ir::GlobalDef, types: &TypeTable) {
        // Skip extern symbols - they're defined elsewhere
        if self.extern_symbols.contains(&global.name) {
            return;
        }
        self.base.emit_global(global, types);
    }

    fn emit_function(&mut self, func: &Function, types: &TypeTable) {
        // Check if this function uses varargs
        let is_variadic = is_variadic_function(func);

        // Register allocation
        let mut alloc = RegAlloc::new();
        self.locations = alloc.allocate(func, types);
        self.pseudos = func.pseudos.clone();

        // Build sym type size map for emit_store to distinguish struct fields from scalars
        self.sym_type_sizes.clear();
        for pseudo in &func.pseudos {
            if let PseudoKind::Sym(name) = &pseudo.kind {
                if let Some(local_var) = func.locals.get(name) {
                    self.sym_type_sizes
                        .insert(pseudo.id, types.size_bits(local_var.typ));
                }
            }
        }

        let stack_size = alloc.stack_size();
        self.frame_base = alloc.frame_base();
        let mut callee_saved = alloc.callee_saved_used().to_vec();
        let callee_saved_fp = alloc.callee_saved_fp_used().to_vec();

        // When using aligned base, x19 must be callee-saved
        if let Some(base) = self.frame_base.reg() {
            if !callee_saved.contains(&base) {
                callee_saved.push(base);
            }
        }

        // For variadic functions on Linux/FreeBSD, we need extra space for the register save area.
        // AAPCS64 va_list reads unnamed arguments out of *two* save areas: the
        // GP one (x0-x7, 8 bytes each = 64) and the SIMD/FP one (q0-q7, 16
        // bytes each = 128). Only the GP half used to be reserved, so a
        // `va_arg(ap, double)` read whatever the caller left in the GP slots
        // -- the incoming d0-d7 were never spilled anywhere.
        // On Darwin (macOS/iOS), variadic args are passed on the stack by the caller,
        // so we don't need a register save area.
        let is_darwin = self.base.target.os == crate::target::Os::MacOS;
        let reg_save_area_size: i32 = if is_variadic && !is_darwin {
            VA_GR_SAVE_BYTES + VA_VR_SAVE_BYTES
        } else {
            0
        };

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
        // Local labels are derived from this and are compiler-internal, so
        // they take the plain name: a verbatim asm-label marker belongs only
        // on the symbol the assembler is asked for.
        self.base.current_fn = crate::arch::lir::undecorated(&func.name).to_string();
        self.frame_size = total_frame;
        self.callee_saved_size = callee_saved_size;
        self.reg_save_area_size = reg_save_area_size;
        self.stack_alloc_size = stack_size;

        // Emit function header (directives, label, CFI start)
        self.emit_function_header(func);

        // Emit prologue (save fp/lr, callee-saved regs, allocate stack)
        self.emit_prologue(total_frame, &callee_saved, &callee_saved_fp);

        // Zero-initialize the local variable area BEFORE storing any arguments.
        // This ensures all stack slots start as zero, so narrow writes (8/16/32-bit)
        // leave zero in the unwritten upper bytes.
        // Uses stp xzr, xzr which doesn't clobber any registers.
        self.zero_stack_frame();

        // Compute aligned base register (x19) for over-aligned locals
        if let FrameBase::Aligned {
            reg: base,
            align: max_align,
        } = self.frame_base
        {
            let base_offset = 16 + self.callee_saved_size;
            // base = (FP + base_offset + max_align - 1) & ~(max_align - 1)
            self.push_lir(Aarch64Inst::Add {
                size: OperandSize::B64,
                dst: base,
                src1: Reg::X29,
                src2: GpOperand::Imm((base_offset + max_align - 1) as i64),
            });
            // AND with bitmask: aarch64 AND (immediate) encodes bitmasks
            self.push_lir(Aarch64Inst::And {
                size: OperandSize::B64,
                dst: base,
                src1: base,
                src2: GpOperand::Imm(-(max_align as i64)),
            });
        }

        // Detect sret and store X8 to stack if needed
        let has_sret = func
            .pseudos
            .iter()
            .any(|p| matches!(p.kind, PseudoKind::Arg(0)) && p.name.as_deref() == Some("__sret"));
        if has_sret {
            self.store_sret_if_needed(func);
        }

        // For variadic functions on Linux/FreeBSD, save argument registers
        if is_variadic && !is_darwin {
            self.emit_variadic_save_area();
        }

        // Measure what the named parameters consumed, for va_start
        if is_variadic {
            // AAPCS64 assigns named parameters to the two register banks
            // independently, and `va_start` has to skip exactly what they
            // consumed in each. Both saturate at 8; a named parameter that
            // arrives after its bank is full lands on the stack instead, and
            // `__stack` has to start past those too.
            let mut ngrn = 0usize;
            let mut nsrn = 0usize;
            let mut named_stack = 0i32;
            let abi = crate::abi::get_abi_for_conv(CallingConv::C, &self.base.target);
            for (_, typ) in &func.params {
                // Mirror `allocate_arguments`: it dispatches on the ABI class,
                // and this has to agree with it or `va_start` skips the wrong
                // number of slots. Asking `is_float` instead counted a
                // `_Complex` -- two V registers -- as one general one, and an
                // HFA of any size likewise.
                let (bank, count) = match abi.classify_param(*typ, types) {
                    ArgClass::Direct { ref classes, .. }
                        if classes.len() == 1 && classes[0] == crate::abi::RegClass::Sse =>
                    {
                        (&mut nsrn, 1)
                    }
                    ArgClass::Hfa { count, .. } => (&mut nsrn, count as usize),
                    _ if types.kind(*typ) == TypeKind::Int128 => (&mut ngrn, 2),
                    // A composite of at most sixteen bytes takes two general
                    // registers. Counting it as one left `va_start` pointing a
                    // slot short, so the first variadic argument of a function
                    // whose named parameter was such a composite came back as
                    // the composite's own upper half.
                    ArgClass::Direct { ref classes, .. }
                        if classes.len() == 2
                            && classes.iter().all(|c| *c == crate::abi::RegClass::Integer) =>
                    {
                        (&mut ngrn, 2)
                    }
                    // Everything else takes a single general register.
                    _ => (&mut ngrn, 1),
                };
                if *bank + count <= 8 {
                    *bank += count;
                } else {
                    let bytes = types.size_bits(*typ).div_ceil(8).max(1) as i32;
                    named_stack += (bytes + 7) & !7;
                }
            }
            self.num_fixed_gp_params = ngrn;
            self.num_fixed_fp_params = nsrn;
            self.named_stack_param_bytes = named_stack;
        }

        // Store spilled arguments before any calls can clobber them
        self.store_spilled_args(&alloc);

        // Move arguments from registers to their allocated stack locations
        self.store_args_to_stack(func, types, &alloc);

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
    fn emit_function_header(&mut self, func: &Function) {
        let (is_static, name) = (func.is_static, func.name.as_str());
        self.push_lir(Aarch64Inst::Directive(Directive::Blank));
        self.push_lir(Aarch64Inst::Directive(Directive::Text));

        // A named section replaces .text for this function.
        if let Some(sec) = &func.symbol_attrs.section {
            self.push_lir(Aarch64Inst::Directive(Directive::NamedSection {
                name: sec.clone(),
                executable: true,
                writable: false,
            }));
        }

        // Skip .globl for static functions (internal linkage)
        if !is_static {
            if func.symbol_attrs.weak {
                self.push_lir(Aarch64Inst::Directive(Directive::Weak(
                    Symbol::global(name),
                    crate::arch::lir::WeakKind::Definition,
                )));
            } else {
                self.push_lir(Aarch64Inst::Directive(Directive::global(name)));
            }
        }
        if let Some(how) = &func.symbol_attrs.visibility {
            self.push_lir(Aarch64Inst::Directive(Directive::Visibility(
                Symbol::global(name),
                how.clone(),
            )));
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

        // AArch64 stp/ldp pre/post-indexed addressing has a limited offset range: [-512, 504]
        // For large frames, we must use separate sub/add and stp/ldp instructions
        const MAX_STP_OFFSET: i32 = 504;

        if total_frame > 0 {
            if total_frame <= MAX_STP_OFFSET {
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
            } else {
                // Large frame: separate sub and stp
                // sub sp, sp, #total_frame
                self.emit_sub_sp_imm(total_frame);
                // stp x29, x30, [sp]
                self.push_lir(Aarch64Inst::Stp {
                    size: OperandSize::B64,
                    src1: fp,
                    src2: lr,
                    addr: MemAddr::Base(Reg::SP),
                });
            }
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

    /// Emit sub sp, sp, #imm handling large immediates
    fn emit_sub_sp_imm(&mut self, imm: i32) {
        // AArch64 add/sub immediate can encode values up to 4095 (12 bits)
        // For larger values, we need multiple instructions or use a register
        const MAX_IMM12: i32 = 4095;

        if imm <= MAX_IMM12 {
            self.push_lir(Aarch64Inst::Sub {
                size: OperandSize::B64,
                src1: Reg::SP,
                src2: GpOperand::Imm(imm as i64),
                dst: Reg::SP,
            });
        } else if imm <= MAX_IMM12 * 2 {
            // Two sub instructions for values up to 8190
            self.push_lir(Aarch64Inst::Sub {
                size: OperandSize::B64,
                src1: Reg::SP,
                src2: GpOperand::Imm(MAX_IMM12 as i64),
                dst: Reg::SP,
            });
            self.push_lir(Aarch64Inst::Sub {
                size: OperandSize::B64,
                src1: Reg::SP,
                src2: GpOperand::Imm((imm - MAX_IMM12) as i64),
                dst: Reg::SP,
            });
        } else {
            // For very large values, load into scratch register
            let scratch = Reg::X9;
            self.emit_mov_imm(scratch, imm as i64, 64);
            self.push_lir(Aarch64Inst::Sub {
                size: OperandSize::B64,
                src1: Reg::SP,
                src2: GpOperand::Reg(scratch),
                dst: Reg::SP,
            });
        }
    }

    // ========================================================================
    // Pair-addressing legalization (stp / ldp / stpfp / ldpfp)
    // ========================================================================
    //
    // stp/ldp accept signed 7-bit immediate offsets scaled by element
    // size:
    //   B64 / Double : [-512,  504] step 8
    //   B32 / Single : [-256,  252] step 4
    //   Quad (128b)  : [-1024, 1008] step 16
    //
    // A deep stack frame (large alloca, int128-heavy locals, many
    // spills) routinely overflows these. The pre-fix codegen blindly
    // emitted out-of-range offsets and the assembler rejected them
    // ("index must be a multiple of 8 in range [-512, 504]").
    //
    // Every body-emitted pair instruction that takes a `BaseOffset`
    // routes through `emit_{stp,ldp,stp_fp,ldp_fp}_legalized`. The
    // legalizer:
    //   * leaves in-range offsets untouched (zero overhead);
    //   * materializes out-of-range addresses into the `X16` scratch
    //     register and rewrites the addr to `[X16]`.
    //
    // `X16` is AAPCS64 IP0 — linker scratch, never in the allocator
    // palette, and never used by other codegen helpers as a *data*
    // shuttle (they use x9–x11). Reserving it specifically for
    // address materialization keeps the scratch convention clean.
    //
    // `PreIndex` / `PostIndex` addresses are NOT legalized here. They
    // appear only in the prologue/epilogue, which already handles its
    // own large-frame split (see `emit_prologue` / `emit_epilogue`).

    /// stp/ldp signed-7-bit-scaled immediate range for `size`.
    /// Returns `(min, max, step)` in bytes.
    #[inline]
    fn pair_offset_range(size: OperandSize) -> (i32, i32, i32) {
        match size {
            OperandSize::B32 => (-256, 252, 4),
            _ => (-512, 504, 8),
        }
    }

    /// True if `offset` fits the stp/ldp encoding for `size`.
    #[inline]
    fn pair_offset_fits(offset: i32, size: OperandSize) -> bool {
        let (min, max, step) = Self::pair_offset_range(size);
        offset >= min && offset <= max && offset % step == 0
    }

    /// Emit `dst = base + offset`, picking the cheapest encoding:
    ///   * add/sub with 12-bit immediate (single instruction);
    ///   * two add/sub when the offset fits within 2 × 12-bit;
    ///   * fall back to `mov dst, imm; add dst, base, dst` for the
    ///     extreme tail.
    ///
    /// `dst` must be a register the caller is free to clobber — the
    /// pair-legalization path passes X16.
    fn emit_add_offset(&mut self, dst: Reg, base: Reg, offset: i32) {
        const MAX_IMM12: i32 = 4095;
        if offset == 0 {
            self.push_lir(Aarch64Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(base),
                dst,
            });
            return;
        }
        let positive = offset >= 0;
        let abs = offset.unsigned_abs() as i64;
        if abs <= MAX_IMM12 as i64 {
            self.push_lir(if positive {
                Aarch64Inst::Add {
                    size: OperandSize::B64,
                    src1: base,
                    src2: GpOperand::Imm(abs),
                    dst,
                }
            } else {
                Aarch64Inst::Sub {
                    size: OperandSize::B64,
                    src1: base,
                    src2: GpOperand::Imm(abs),
                    dst,
                }
            });
        } else if abs <= 2 * MAX_IMM12 as i64 {
            // Two-step: first to `dst`, then chain to `dst`.
            self.push_lir(if positive {
                Aarch64Inst::Add {
                    size: OperandSize::B64,
                    src1: base,
                    src2: GpOperand::Imm(MAX_IMM12 as i64),
                    dst,
                }
            } else {
                Aarch64Inst::Sub {
                    size: OperandSize::B64,
                    src1: base,
                    src2: GpOperand::Imm(MAX_IMM12 as i64),
                    dst,
                }
            });
            self.push_lir(if positive {
                Aarch64Inst::Add {
                    size: OperandSize::B64,
                    src1: dst,
                    src2: GpOperand::Imm(abs - MAX_IMM12 as i64),
                    dst,
                }
            } else {
                Aarch64Inst::Sub {
                    size: OperandSize::B64,
                    src1: dst,
                    src2: GpOperand::Imm(abs - MAX_IMM12 as i64),
                    dst,
                }
            });
        } else {
            self.emit_mov_imm(dst, offset as i64, 64);
            self.push_lir(Aarch64Inst::Add {
                size: OperandSize::B64,
                src1: base,
                src2: GpOperand::Reg(dst),
                dst,
            });
        }
    }

    /// If `addr` is a `BaseOffset` with an out-of-range offset,
    /// materialize `base + offset` into X16 and rewrite the address
    /// to `[X16]`. Otherwise returns `addr` unchanged.
    ///
    /// Convention: X16 is clobbered iff legalization fires. Callers
    /// must not rely on X16 being alive past the emit_*_legalized
    /// call. In practice this is fine — the pattern is always
    /// "compute source addr (may use X16) → load into X9/X10 →
    /// legalize destination addr (may reuse X16) → store" — the
    /// source's use of X16 is dead by the time the destination's
    /// legalization runs.
    fn legalize_pair_addr(&mut self, size: OperandSize, addr: MemAddr) -> MemAddr {
        if let MemAddr::BaseOffset { base, offset } = addr {
            if !Self::pair_offset_fits(offset, size) {
                self.emit_add_offset(Reg::X16, base, offset);
                return MemAddr::Base(Reg::X16);
            }
        }
        addr
    }

    /// Emit `stp src1, src2, addr` with pair-address legalization.
    pub(super) fn emit_stp_legalized(
        &mut self,
        size: OperandSize,
        src1: Reg,
        src2: Reg,
        addr: MemAddr,
    ) {
        let addr = self.legalize_pair_addr(size, addr);
        self.push_lir(Aarch64Inst::Stp {
            size,
            src1,
            src2,
            addr,
        });
    }

    /// Emit `ldp dst1, dst2, addr` with pair-address legalization.
    pub(super) fn emit_ldp_legalized(
        &mut self,
        size: OperandSize,
        addr: MemAddr,
        dst1: Reg,
        dst2: Reg,
    ) {
        let addr = self.legalize_pair_addr(size, addr);
        self.push_lir(Aarch64Inst::Ldp {
            size,
            addr,
            dst1,
            dst2,
        });
    }

    // FP pair legalization helpers (emit_{stp,ldp}_fp_legalized) are
    // intentionally absent — every current StpFp/LdpFp site emits
    // either callee-saved save/restore (offset bounded by the small
    // callee-saved set: ≤288 bytes) or prologue PreIndex (handled by
    // its own large-frame split). Add them the moment an FP pair
    // instruction needs body-emission with a possibly-large offset.

    /// Zero-initialize the local variable area of the stack frame.
    /// This ensures all stack slots start as zero, so narrow writes (8/16/32-bit)
    /// leave zero in the unwritten upper bytes.
    ///
    /// For small frames (max offset ≤ 504): uses `stp xzr, xzr, [x29, #offset]`
    /// (16 bytes per instruction, but signed 7-bit offset limited to [-512, 504]).
    /// For large frames: uses `str xzr, [x29, #offset]` per qword
    /// (unsigned 12-bit offset scaled by 8, range [0, 32760]).
    fn zero_stack_frame(&mut self) {
        let alloc_size = self.stack_alloc_size;
        if alloc_size <= 0 {
            return;
        }
        // Local variable area starts at FP + 16 + callee_saved_size
        let base_offset = 16 + self.callee_saved_size;
        // stp signed offset range is [-512, 504] for 64-bit registers
        let max_stp_offset = base_offset + alloc_size - 16;
        if max_stp_offset <= 504 {
            // Small frame: use stp xzr, xzr (16 bytes per instruction)
            let mut offset = 0;
            while offset + 16 <= alloc_size {
                self.push_lir(Aarch64Inst::Stp {
                    size: OperandSize::B64,
                    src1: Reg::Xzr,
                    src2: Reg::Xzr,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: base_offset + offset,
                    },
                });
                offset += 16;
            }
            if offset < alloc_size {
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: Reg::Xzr,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: base_offset + offset,
                    },
                });
            }
        } else {
            // Large frame: use str xzr (8 bytes per instruction)
            // str unsigned offset range is [0, 32760] for 64-bit — handles all practical frames
            let mut offset = 0;
            while offset < alloc_size {
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: Reg::Xzr,
                    addr: MemAddr::BaseOffset {
                        base: Reg::X29,
                        offset: base_offset + offset,
                    },
                });
                offset += 8;
            }
        }
    }

    /// Emit add sp, sp, #imm handling large immediates
    fn emit_add_sp_imm(&mut self, imm: i32) {
        const MAX_IMM12: i32 = 4095;

        if imm <= MAX_IMM12 {
            self.push_lir(Aarch64Inst::Add {
                size: OperandSize::B64,
                src1: Reg::SP,
                src2: GpOperand::Imm(imm as i64),
                dst: Reg::SP,
            });
        } else if imm <= MAX_IMM12 * 2 {
            self.push_lir(Aarch64Inst::Add {
                size: OperandSize::B64,
                src1: Reg::SP,
                src2: GpOperand::Imm(MAX_IMM12 as i64),
                dst: Reg::SP,
            });
            self.push_lir(Aarch64Inst::Add {
                size: OperandSize::B64,
                src1: Reg::SP,
                src2: GpOperand::Imm((imm - MAX_IMM12) as i64),
                dst: Reg::SP,
            });
        } else {
            let scratch = Reg::X9;
            self.emit_mov_imm(scratch, imm as i64, 64);
            self.push_lir(Aarch64Inst::Add {
                size: OperandSize::B64,
                src1: Reg::SP,
                src2: GpOperand::Reg(scratch),
                dst: Reg::SP,
            });
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
    fn store_sret_if_needed(&mut self, func: &Function) {
        if let Some(sret) = func
            .pseudos
            .iter()
            .find(|p| matches!(p.kind, PseudoKind::Arg(0)) && p.name.as_deref() == Some("__sret"))
        {
            if let Some(Loc::Stack(offset)) = self.locations.get_ref(sret.id) {
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: Reg::X8,
                    addr: self.stack_mem(*offset),
                });
            }
        }
    }

    /// Save argument registers to the register save area for variadic functions (Linux/FreeBSD)
    ///
    /// Layout, from `reg_save_area_offset`: x0-x7 (8 bytes each), then q0-q7
    /// (16 bytes each). `va_start` derives `__gr_top` / `__vr_top` from the
    /// ends of the two halves, and `__gr_offs` / `__vr_offs` count backwards
    /// from there, so the order here is what those offsets mean.
    fn emit_variadic_save_area(&mut self) {
        let arg_regs = Reg::arg_regs();
        for (i, reg) in arg_regs.iter().enumerate() {
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

        // The SIMD half stores whole q registers: a `long double` unnamed
        // argument is binary128 and occupies the full 16-byte slot.
        for (i, vreg) in VReg::arg_regs().iter().enumerate() {
            let offset = self.reg_save_area_offset + VA_GR_SAVE_BYTES + (i as i32 * 16);
            self.push_lir(Aarch64Inst::StrFp {
                size: FpSize::Quad,
                src: *vreg,
                addr: MemAddr::BaseOffset {
                    base: Reg::X29,
                    offset,
                },
            });
        }
    }

    /// Emit stores for arguments spilled from caller-saved registers to stack
    fn store_spilled_args(&mut self, alloc: &RegAlloc) {
        for spilled in alloc.spilled_args() {
            if let Some(gp_reg) = spilled.from_gp_reg {
                self.push_lir(Aarch64Inst::Str {
                    size: OperandSize::B64,
                    src: gp_reg,
                    addr: self.stack_mem(spilled.to_stack_offset),
                });
            } else if let Some(fp_reg) = spilled.from_fp_reg {
                // The recorded width, not a fixed eight bytes: `long double` is
                // binary128 here, and half of one was left behind.
                self.push_lir(Aarch64Inst::StrFp {
                    size: if spilled.bytes > 8 {
                        FpSize::Quad
                    } else {
                        FpSize::Double
                    },
                    src: fp_reg,
                    addr: self.stack_mem(spilled.to_stack_offset),
                });
            }
        }
    }

    /// Move arguments from registers to their allocated stack locations
    /// Copy a stack-passed two-element floating-point argument (a `_Complex`
    /// or a two-element HFA) into the parameter's local.
    ///
    /// The register-passed path stores straight from the argument registers;
    /// this is the overflow case, where the value already sits in the caller's
    /// outgoing area and only needs moving to where the body looks for it.
    /// V16 is a scratch V register, not part of the argument sequence.
    /// Element size and FP width for a two-element floating-point argument.
    ///
    /// A `_Complex` and a two-element HFA are both passed in a V-register pair
    /// but their element sizes come from different places: the complex base
    /// type, or the ABI's HFA classification. `complex_fp_info` answers
    /// `(Double, 8)` for anything that is not complex -- including a
    /// `struct { float x, y; }`, whose elements are 4 bytes -- so using it for
    /// both copied HFAs at twice their stride, reading and writing 8 bytes
    /// past each end.
    fn two_element_fp_info(&self, typ: TypeId, types: &TypeTable) -> (FpSize, i32) {
        if types.is_complex(typ) {
            return complex_fp_info(types, &self.base.target, typ);
        }
        use crate::abi::HfaBase;
        let abi = get_abi_for_conv(CallingConv::C, &self.base.target);
        match abi.classify_param(typ, types) {
            ArgClass::Hfa { base, .. } => match base {
                HfaBase::Float16 => (FpSize::Half, 2),
                HfaBase::Float32 => (FpSize::Single, 4),
                HfaBase::Float64 => (FpSize::Double, 8),
                HfaBase::Float128 => (FpSize::Quad, 16),
            },
            _ => (FpSize::Double, 8),
        }
    }

    /// Copy an `elems`-element floating-point parameter that the caller laid
    /// on the stack into the local the body reads.
    fn copy_stacked_fp_elems_to_local(
        &mut self,
        func: &Function,
        param_idx: usize,
        typ: TypeId,
        types: &TypeTable,
        pseudo: PseudoId,
        elems: i32,
    ) {
        let param_name = &func.params[param_idx].0;
        let Some(local) = func.locals.get(param_name) else {
            return;
        };
        let Some(&Loc::Stack(local_off)) = self.locations.get_ref(local.sym) else {
            return;
        };
        // The incoming argument's slot, as assigned by allocate_arguments.
        // `IncomingArg`, not `Stack`: this one lives in the *caller's* frame,
        // and matching the wrong variant makes this return without emitting
        // the copy at all, leaving the parameter uninitialized.
        let Some(&Loc::IncomingArg(incoming_off)) = self.locations.get_ref(pseudo) else {
            return;
        };

        let (fp_size, elem_bytes) = self.two_element_fp_info(typ, types);

        for step in 0..elems {
            let delta = step * elem_bytes;
            self.push_lir(Aarch64Inst::LdrFp {
                size: fp_size,
                dst: VReg::V16,
                addr: self.incoming_mem_plus(incoming_off, delta),
            });
            self.push_lir(Aarch64Inst::StrFp {
                size: fp_size,
                src: VReg::V16,
                addr: self.stack_mem_plus(local_off, delta),
            });
        }
    }

    fn store_args_to_stack(&mut self, func: &Function, types: &TypeTable, alloc: &RegAlloc) {
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
            // A composite of at most sixteen bytes that is not an HFA arrives
            // in two consecutive X registers, and the prologue writes them
            // into the parameter's local -- the same arrangement the HFA arm
            // below uses. It used to fall to the single-register arm, which
            // stored one register and advanced by one, so the aggregate was
            // half wrong and every later integer parameter moved as well.
            let gp_pair = !is_complex
                && !is_fp
                && matches!(types.kind(*typ), TypeKind::Struct | TypeKind::Union)
                && {
                    let abi = get_abi_for_conv(CallingConv::C, &self.base.target);
                    matches!(
                        abi.classify_param(*typ, types),
                        ArgClass::Direct { ref classes, .. }
                            if classes.len() == 2
                                && classes.iter().all(|c| *c == crate::abi::RegClass::Integer)
                    )
                };
            // How many consecutive V registers this parameter arrives in, if
            // it arrives in V registers at all. A `_Complex` is two by
            // definition; a struct is whatever the ABI's HFA classification
            // says, which is one through four.
            //
            // This used to ask for `count: 2` and let every other count fall
            // through to the GP arm below, so a three- or four-element HFA was
            // read out of an integer register -- and consumed an integer slot,
            // which shifted every following integer parameter by one.
            let fp_reg_count: Option<usize> = if is_complex {
                Some(2)
            } else if !is_fp && matches!(types.kind(*typ), TypeKind::Struct | TypeKind::Union) {
                let abi = get_abi_for_conv(CallingConv::C, &self.base.target);
                match abi.classify_param(*typ, types) {
                    ArgClass::Hfa { count, .. } => Some(count as usize),
                    _ => None,
                }
            } else {
                None
            };

            // Find the pseudo for this argument
            for pseudo in &func.pseudos {
                if let PseudoKind::Arg(arg_idx) = pseudo.kind {
                    // With sret, params have arg_idx = i + 1, but still use arg_regs[i]
                    if arg_idx == (i as u32) + arg_idx_offset {
                        // Skip pseudos already stored via spilled_args
                        if spilled_pseudos.contains(&pseudo.id) {
                            // Still need to count this arg for register assignment tracking
                            if let Some(count) = fp_reg_count {
                                fp_arg_idx += count;
                            } else if gp_pair {
                                int_arg_idx += 2;
                            } else if is_fp {
                                fp_arg_idx += 1;
                            } else if types.kind(*typ) == TypeKind::Int128 {
                                int_arg_idx += 2;
                            } else {
                                int_arg_idx += 1;
                            }
                            break;
                        }
                        if gp_pair {
                            let param_name = &func.params[i].0;
                            let local_off = func
                                .locals
                                .get(param_name)
                                .and_then(|local| self.locations.get_ref(local.sym))
                                .and_then(|loc| match loc {
                                    Loc::Stack(off) => Some(*off),
                                    _ => None,
                                });
                            if let Some(local_off) = local_off {
                                if int_arg_idx + 1 < arg_regs.len() {
                                    self.emit_stp_legalized(
                                        OperandSize::B64,
                                        arg_regs[int_arg_idx],
                                        arg_regs[int_arg_idx + 1],
                                        self.stack_mem(local_off),
                                    );
                                } else if let Some(&Loc::IncomingArg(incoming)) =
                                    self.locations.get_ref(pseudo.id)
                                {
                                    // Out of registers, so the caller laid the
                                    // composite in its own frame. The prologue
                                    // still has to copy it into the local the
                                    // body reads, exactly as the spilled-HFA
                                    // case does; without this the parameter was
                                    // left uninitialized.
                                    let bytes = (types.size_bits(*typ) / 8) as i32;
                                    let mut done = 0;
                                    while done < bytes {
                                        let chunk = [8, 4, 2, 1]
                                            .into_iter()
                                            .find(|c| *c <= bytes - done)
                                            .unwrap_or(1);
                                        let size = OperandSize::from_bits(chunk as u32 * 8);
                                        self.push_lir(Aarch64Inst::Ldr {
                                            size,
                                            addr: self.incoming_mem_plus(incoming, done),
                                            dst: Reg::X16,
                                        });
                                        self.push_lir(Aarch64Inst::Str {
                                            size,
                                            src: Reg::X16,
                                            addr: self.stack_mem_plus(local_off, done),
                                        });
                                        done += chunk;
                                    }
                                }
                            }
                            int_arg_idx += 2;
                        } else if let Some(count) = fp_reg_count {
                            // Complex or HFA argument — `count` consecutive V registers
                            if fp_arg_idx + count <= fp_arg_regs.len() {
                                let param_name = &func.params[i].0;
                                if let Some(local) = func.locals.get(param_name) {
                                    if let Some(&Loc::Stack(offset)) =
                                        self.locations.get_ref(local.sym)
                                    {
                                        let (fp_size, elem_bytes) =
                                            self.two_element_fp_info(*typ, types);
                                        for elem in 0..count {
                                            self.push_lir(Aarch64Inst::StrFp {
                                                size: fp_size,
                                                src: fp_arg_regs[fp_arg_idx + elem],
                                                addr: self.stack_mem_plus(
                                                    offset,
                                                    elem as i32 * elem_bytes,
                                                ),
                                            });
                                        }
                                    }
                                }
                            } else {
                                // AAPCS64 §6.4.2: the argument did not fit in
                                // the V registers, so the caller laid it on the
                                // stack. The prologue still has to copy it into
                                // the parameter's local, which is what the body
                                // reads -- without this the local was left
                                // uninitialized and the parameter read as
                                // garbage (#H13). regalloc has already assigned
                                // the incoming slot; find it and shuttle both
                                // elements through V16.
                                self.copy_stacked_fp_elems_to_local(
                                    func,
                                    i,
                                    *typ,
                                    types,
                                    pseudo.id,
                                    count as i32,
                                );
                            }
                            fp_arg_idx += count;
                        } else if is_fp {
                            // FP argument
                            if fp_arg_idx < fp_arg_regs.len() {
                                if let Some(Loc::Stack(offset)) = self.locations.get_ref(pseudo.id)
                                {
                                    // From the type, not a `32 or else`
                                    // guess: `long double` is binary128
                                    // here, and storing it as a double
                                    // dropped its top eight bytes, so the
                                    // *second* such parameter came back
                                    // truncated while the first, stored
                                    // elsewhere, was whole.
                                    let fp_size = FpSize::from_type_or_bits(
                                        Some(*typ),
                                        types.size_bits(*typ),
                                        types,
                                        &self.base.target,
                                    );
                                    self.push_lir(Aarch64Inst::StrFp {
                                        size: fp_size,
                                        src: fp_arg_regs[fp_arg_idx],
                                        addr: self.stack_mem(*offset),
                                    });
                                }
                            }
                            fp_arg_idx += 1;
                        } else if types.kind(*typ) == TypeKind::Int128 {
                            // __int128 argument — uses TWO consecutive GP registers,
                            // even-aligned per AAPCS64 stage C.10, so an odd NGRN
                            // skips one. Asked through the same helper the caller
                            // and the allocator use: computing the pair here
                            // independently is how the prologue came to read a
                            // different pair than the caller wrote.
                            //
                            // Store to the arg pseudo's stack slot (allocated in
                            // allocate_arguments). The IR will Copy from arg
                            // pseudo → local variable.
                            if let Some(start) =
                                crate::arch::aarch64::int128_pair_start(int_arg_idx, arg_regs.len())
                            {
                                int_arg_idx = start;
                                if let Some(Loc::Stack(offset)) = self.locations.get_ref(pseudo.id)
                                {
                                    self.emit_stp_legalized(
                                        OperandSize::B64,
                                        arg_regs[int_arg_idx],
                                        arg_regs[int_arg_idx + 1],
                                        self.stack_mem(*offset),
                                    );
                                }
                                int_arg_idx += 2;
                            } else {
                                // Stage C.11: NGRN becomes 8.
                                int_arg_idx = arg_regs.len();
                            }
                        } else {
                            // GP argument
                            if int_arg_idx < arg_regs.len() {
                                if let Some(Loc::Stack(offset)) = self.locations.get_ref(pseudo.id)
                                {
                                    // Move from arg register to stack
                                    self.push_lir(Aarch64Inst::Str {
                                        size: OperandSize::B64,
                                        src: arg_regs[int_arg_idx],
                                        addr: self.stack_mem(*offset),
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
        callee_saved: &[Reg],
        callee_saved_fp: &[VReg],
        types: &TypeTable,
    ) {
        // Move return value to x0 (integer), v0 (float), v0+v1 (complex/HFA-2) if present
        if let Some(&src) = insn.src.first() {
            let src_loc = self.get_location(src);
            let is_complex = insn.typ.is_some_and(|t| types.is_complex(t));
            // Decide by *type* first, not only by where the value happens to
            // sit. A `long double` produced by an rtlib call lands on the
            // stack, so a location-only test sent it out through emit_move and
            // returned it in x0 instead of q0.
            let is_fp = insn.typ.is_some_and(|t| types.is_float(t))
                || matches!(src_loc, Loc::VReg(_) | Loc::FImm(..));
            // Check for HFA-2 struct return (e.g., {double, double}).
            // Compute the HFA FP size once; None means not an HFA-2.
            let hfa_ret: Option<(FpSize, u8)> = if !is_complex {
                insn.typ.and_then(|t| {
                    let k = types.kind(t);
                    // No lower bound: a struct holding one `float`, `double`
                    // or `_Float16` is a one-element HFA and comes back in V0.
                    // Requiring more than eight bytes sent all three out
                    // through a general register, while the *caller* read the
                    // FP one -- so the two sides disagreed inside one
                    // translation unit.
                    // No size bound in either direction. A struct holding
                    // one `float`, `double` or `_Float16` is a one-element HFA
                    // and comes back in V0; four `double`s is a valid
                    // four-element HFA at thirty-two bytes, and capping this at
                    // sixteen sent it back as an address instead. The
                    // classifier is what decides -- it already applies the
                    // four-element limit.
                    if k == TypeKind::Struct || k == TypeKind::Union {
                        let abi = get_abi_for_conv(CallingConv::C, &self.base.target);
                        match abi.classify_return(t, types) {
                            ArgClass::Hfa { base, count } => {
                                use crate::abi::HfaBase;
                                Some((
                                    match base {
                                        HfaBase::Float16 => FpSize::Half,
                                        HfaBase::Float32 => FpSize::Single,
                                        HfaBase::Float64 => FpSize::Double,
                                        HfaBase::Float128 => FpSize::Quad,
                                    },
                                    count,
                                ))
                            }
                            _ => None,
                        }
                    } else {
                        None
                    }
                })
            } else {
                None
            };
            // A one-element HFA is one register holding the whole value, not a
            // pair. `struct { long double v; }` is that on aarch64 Linux, where
            // `long double` is binary128: moving it out of a general register
            // hit `a binary128 value does not fit one X register` and killed
            // the compiler.
            // Two elements through four: one V register each. Only the
            // two-element case was handled, so a three- or four-element HFA
            // fell through to the general-register return below and came back
            // as the address of the callee's own frame slot.
            let hfa_multi: Option<(FpSize, u8)> = hfa_ret.filter(|&(_, count)| count >= 2);

            // Derive return size from type to avoid 32-bit truncation
            let ret_typ = insn.typ;
            let ret_size = ret_typ
                .map(|t| types.size_bits(t).max(32))
                .unwrap_or(insn.size.max(32));

            if let Some((fp_size, 1)) = hfa_ret {
                // One V register carries all of it. The linearizer hands the
                // two halves over as general registers, which is how a
                // sixteen-byte value reaches here; assemble them into the
                // register's two lanes, exactly as gcc does.
                if insn.src.len() == 2 {
                    self.emit_move(src, Reg::X9, 64);
                    if fp_size == FpSize::Quad {
                        self.push_lir(Aarch64Inst::FmovFromGp {
                            size: FpSize::Double,
                            src: Reg::X9,
                            dst: VReg::V0,
                        });
                        if let Some(&src2) = insn.src.get(1) {
                            self.emit_move(src2, Reg::X9, 64);
                            self.push_lir(Aarch64Inst::InsGpToVecD {
                                lane: 1,
                                src: Reg::X9,
                                dst: VReg::V0,
                            });
                        }
                    } else {
                        self.push_lir(Aarch64Inst::FmovFromGp {
                            size: fp_size,
                            src: Reg::X9,
                            dst: VReg::V0,
                        });
                    }
                } else {
                    let hfa_bits = insn.typ.map_or(0, |t| types.size_bits(t));
                    match src_loc {
                        // Eight bytes or fewer, already loaded into a general
                        // register: move it across. This is the common shape --
                        // the linearizer loads a small struct's value rather
                        // than handing over its address -- and leaving it in
                        // `x0` is what made the callee and the caller, which
                        // reads `s0`/`d0`, disagree inside one program.
                        Loc::Reg(r) if hfa_bits <= 64 => {
                            self.push_lir(Aarch64Inst::FmovFromGp {
                                size: fp_size,
                                src: r,
                                dst: VReg::V0,
                            });
                        }
                        // Eight bytes or fewer: the slot holds the value.
                        Loc::Stack(offset) if hfa_bits <= 64 => {
                            self.push_lir(Aarch64Inst::LdrFp {
                                size: fp_size,
                                dst: VReg::V0,
                                addr: self.stack_mem(offset),
                            });
                        }
                        // Wider than a register, with its address already in
                        // one: load the value straight out of it.
                        Loc::Reg(r) => {
                            self.push_lir(Aarch64Inst::LdrFp {
                                size: fp_size,
                                dst: VReg::V0,
                                addr: MemAddr::BaseOffset { base: r, offset: 0 },
                            });
                        }
                        // Wider than a register: the slot holds its address.
                        Loc::Stack(offset) => {
                            self.push_lir(Aarch64Inst::Ldr {
                                size: OperandSize::B64,
                                dst: Reg::X9,
                                addr: self.stack_mem(offset),
                            });
                            self.push_lir(Aarch64Inst::LdrFp {
                                size: fp_size,
                                dst: VReg::V0,
                                addr: MemAddr::BaseOffset {
                                    base: Reg::X9,
                                    offset: 0,
                                },
                            });
                        }
                        _ => self.emit_move(src, Reg::X0, ret_size),
                    }
                }
            } else if let Some((fp_size, count)) = hfa_multi {
                let count = count as usize;
                let elem_offset = match fp_size {
                    FpSize::Half => 2,
                    FpSize::Single => 4,
                    FpSize::Quad => 16,
                    _ => 8,
                };
                let ret_regs = [VReg::V0, VReg::V1, VReg::V2, VReg::V3];
                if insn.src.len() == 2 && count == 2 {
                    // Two-source path: linearizer pre-loaded struct halves as integers.
                    // Move from GP registers to V0/V1 via FmovFromGp.
                    self.emit_move(src, Reg::X9, 64);
                    self.push_lir(Aarch64Inst::FmovFromGp {
                        size: fp_size,
                        src: Reg::X9,
                        dst: VReg::V0,
                    });
                    if let Some(&src2) = insn.src.get(1) {
                        self.emit_move(src2, Reg::X9, 64);
                        self.push_lir(Aarch64Inst::FmovFromGp {
                            size: fp_size,
                            src: Reg::X9,
                            dst: VReg::V1,
                        });
                    }
                } else {
                    // Single-source path: `src` locates the aggregate.
                    //
                    // Eight bytes or fewer, so the slot holds the value rather
                    // than a pointer to it -- `struct { _Float16 a, b; }` is
                    // four. Dereferencing the value's own bytes is what a
                    // two-element half-precision HFA started doing the moment
                    // it became an HFA at all.
                    let hfa_bits = insn.typ.map_or(0, |t| types.size_bits(t));
                    match src_loc {
                        Loc::Stack(offset) if hfa_bits <= 64 => {
                            for (i, &dst) in ret_regs[..count].iter().enumerate() {
                                self.push_lir(Aarch64Inst::LdrFp {
                                    size: fp_size,
                                    dst,
                                    addr: self.stack_field(offset, i as i32 * elem_offset),
                                });
                            }
                        }
                        Loc::Stack(offset) => {
                            self.push_lir(Aarch64Inst::Ldr {
                                size: OperandSize::B64,
                                dst: Reg::X9,
                                addr: self.stack_mem(offset),
                            });
                            for (i, &dst) in ret_regs[..count].iter().enumerate() {
                                self.push_lir(Aarch64Inst::LdrFp {
                                    size: fp_size,
                                    dst,
                                    addr: MemAddr::BaseOffset {
                                        base: Reg::X9,
                                        offset: i as i32 * elem_offset,
                                    },
                                });
                            }
                        }
                        // Eight bytes or fewer: the register holds the packed
                        // value, not a pointer to it, so the elements come out
                        // of it by shifting rather than by loading. Treating it
                        // as an address dereferenced the value's own bits.
                        Loc::Reg(r) if hfa_bits <= 64 => {
                            for (i, &dst) in ret_regs[..count].iter().enumerate() {
                                let gp = if i == 0 {
                                    r
                                } else {
                                    self.push_lir(Aarch64Inst::Lsr {
                                        size: OperandSize::B64,
                                        src: r,
                                        amount: GpOperand::Imm((i as i32 * elem_offset * 8) as i64),
                                        dst: Reg::X9,
                                    });
                                    Reg::X9
                                };
                                self.push_lir(Aarch64Inst::FmovFromGp {
                                    size: fp_size,
                                    src: gp,
                                    dst,
                                });
                            }
                        }
                        Loc::Reg(r) => {
                            for (i, &dst) in ret_regs[..count].iter().enumerate() {
                                self.push_lir(Aarch64Inst::LdrFp {
                                    size: fp_size,
                                    dst,
                                    addr: MemAddr::BaseOffset {
                                        base: r,
                                        offset: i as i32 * elem_offset,
                                    },
                                });
                            }
                        }
                        _ => {}
                    }
                }
            } else if insn.returns_two_regs() {
                self.emit_move(src, Reg::X0, 64);
                if let Some(&src2) = insn.src.get(1) {
                    self.emit_move(src2, Reg::X1, 64);
                }
            } else if is_complex {
                let (fp_size, imag_offset) =
                    complex_fp_info(types, &self.base.target, insn.typ.unwrap());
                match src_loc {
                    Loc::Stack(offset) => {
                        self.push_lir(Aarch64Inst::Ldr {
                            size: OperandSize::B64,
                            dst: Reg::X9,
                            addr: self.stack_mem(offset),
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
            } else if insn.typ.is_some_and(|t| types.kind(t) == TypeKind::Int128) {
                // __int128 return: lo half → X0, hi half → X1
                let loc = self.get_location(src);
                if let Loc::Stack(offset) = loc {
                    let mem = self.stack_mem(offset);
                    self.emit_ldp_legalized(OperandSize::B64, mem, Reg::X0, Reg::X1);
                } else {
                    // Fallback: load lo half to X0, zero X1
                    self.emit_move(src, Reg::X0, 64);
                    self.push_lir(Aarch64Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Reg(Reg::Xzr),
                        dst: Reg::X1,
                    });
                }
            } else if is_fp {
                self.emit_fp_move(src, VReg::V0, insn.typ, ret_size, types);
            } else {
                self.emit_move(src, Reg::X0, ret_size);
            }
        }

        // Epilogue: reset SP to FP
        self.push_lir(Aarch64Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::X29),
            dst: Reg::SP,
        });

        // Restore callee-saved registers
        if self.frame_size > 16 {
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
        // AArch64 ldp post-indexed addressing has a limited offset range: [-512, 504]
        const MAX_LDP_OFFSET: i32 = 504;
        let dealloc = if self.frame_size > 0 {
            self.frame_size
        } else {
            16
        };

        if dealloc <= MAX_LDP_OFFSET {
            // Combined restore and deallocate: ldp x29, x30, [sp], #N
            self.push_lir(Aarch64Inst::Ldp {
                size: OperandSize::B64,
                addr: MemAddr::PostIndex {
                    base: Reg::sp(),
                    offset: dealloc,
                },
                dst1: Reg::fp(),
                dst2: Reg::lr(),
            });
        } else {
            // Large frame: separate ldp and add
            // ldp x29, x30, [sp]
            self.push_lir(Aarch64Inst::Ldp {
                size: OperandSize::B64,
                addr: MemAddr::Base(Reg::sp()),
                dst1: Reg::fp(),
                dst2: Reg::lr(),
            });
            // add sp, sp, #dealloc
            self.emit_add_sp_imm(dealloc);
        }
        self.push_lir(Aarch64Inst::Ret);
    }

    /// Emit conditional branch: test condition and branch accordingly
    /// Returns true if an early return was taken (for constant conditions)
    fn emit_cbr(&mut self, insn: &Instruction) -> bool {
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
            loc @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                // A condition can be an incoming stack argument as readily as
                // a local; both are just a value in memory here.
                let mem = self.loc_mem(loc).unwrap();
                if insn.size >= 128 {
                    // 128-bit: load both halves and ORR them to check for non-zero
                    let (_, scratch1, _) = Reg::scratch_regs();
                    self.emit_ldp_legalized(OperandSize::B64, mem, scratch0, scratch1);
                    self.push_lir(Aarch64Inst::Orr {
                        size: OperandSize::B64,
                        src1: scratch0,
                        src2: GpOperand::Reg(scratch1),
                        dst: scratch0,
                    });
                    self.push_lir(Aarch64Inst::Cmp {
                        size: OperandSize::B64,
                        src1: scratch0,
                        src2: GpOperand::Imm(0),
                    });
                } else {
                    self.push_lir(Aarch64Inst::Ldr {
                        size: OperandSize::B64,
                        addr: mem,
                        dst: scratch0,
                    });
                    self.push_lir(Aarch64Inst::Cmp {
                        size: OperandSize::B64,
                        src1: scratch0,
                        src2: GpOperand::Imm(0),
                    });
                }
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
                let bit_size = if insn.size == 0 { 64 } else { insn.size };
                let fp_size = if bit_size <= 32 {
                    FpSize::Single
                } else {
                    FpSize::Double
                };
                self.push_lir(Aarch64Inst::FcmpZero {
                    size: fp_size,
                    src: *v,
                });
            }
            Loc::FImm(f, _) => {
                let target = if !f.is_zero() {
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
    fn emit_switch(&mut self, insn: &Instruction, types: &TypeTable) {
        let Some(&val) = insn.src.first() else { return };

        let loc = self.get_location(val);
        let (scratch0, scratch1, _) = Reg::scratch_regs();
        let switch_size = insn
            .typ
            .map(|t| types.size_bits(t).max(32))
            .unwrap_or(insn.size.max(32));
        let op_size = OperandSize::from_bits(switch_size);

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
            loc @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                self.push_lir(Aarch64Inst::Ldr {
                    size: op_size,
                    addr: self.loc_mem(loc).unwrap(),
                    dst: scratch0,
                });
            }
            Loc::Imm(v) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: op_size,
                    src: GpOperand::Imm(*v as i64),
                    dst: scratch0,
                });
            }
            Loc::Global(name) => {
                self.emit_load_global(name, scratch0, op_size);
            }
            Loc::VReg(_) | Loc::FImm(..) => {}
        }

        // Compare `src` against a constant, materializing it when it does
        // not fit the 12-bit unsigned immediate `cmp` accepts.
        let cmp_const = |gen: &mut Self, src: Reg, v: i64| {
            if (0..4096).contains(&v) {
                gen.push_lir(Aarch64Inst::Cmp {
                    size: op_size,
                    src1: src,
                    src2: GpOperand::Imm(v),
                });
            } else {
                gen.push_lir(Aarch64Inst::Mov {
                    size: op_size,
                    src: GpOperand::Imm(v),
                    dst: scratch1,
                });
                gen.push_lir(Aarch64Inst::Cmp {
                    size: op_size,
                    src1: src,
                    src2: GpOperand::Reg(scratch1),
                });
            }
        };

        // Generate comparisons for each case
        for (lo, hi, target_bb) in insn.switch_cases.clone() {
            let target = Label::new(&self.base.current_fn, target_bb.0);
            if lo == hi {
                cmp_const(self, scratch0, lo);
                self.push_lir(Aarch64Inst::BCond {
                    cond: CondCode::Eq,
                    target,
                });
                continue;
            }

            // A GNU range `case lo ... hi:`, tested as
            // `(x - lo) <=unsigned (hi - lo)`: subtracting the low endpoint
            // wraps everything below it to a large unsigned value, so a single
            // unsigned comparison decides both ends. Expanding a range into one
            // compare per value is not an option -- `case 0 ... 1000000:` is
            // legal C.
            //
            // scratch0 holds the switch value and later cases reuse it, so the
            // difference goes to scratch2.
            let (_, _, scratch2) = Reg::scratch_regs();
            if (0..4096).contains(&lo) {
                self.push_lir(Aarch64Inst::Sub {
                    size: op_size,
                    src1: scratch0,
                    src2: GpOperand::Imm(lo),
                    dst: scratch2,
                });
            } else {
                self.push_lir(Aarch64Inst::Mov {
                    size: op_size,
                    src: GpOperand::Imm(lo),
                    dst: scratch1,
                });
                self.push_lir(Aarch64Inst::Sub {
                    size: op_size,
                    src1: scratch0,
                    src2: GpOperand::Reg(scratch1),
                    dst: scratch2,
                });
            }
            cmp_const(self, scratch2, hi.wrapping_sub(lo));
            self.push_lir(Aarch64Inst::BCond {
                cond: CondCode::Ule,
                target,
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
        // `-fverbose-asm`: hang the source-level names off the first
        // instruction this one produces. Recorded before emission, since
        // the index is the position the next push will take.
        if self.base.verbose_asm {
            if let Some(text) = crate::arch::codegen::verbose_annotation(insn, &self.pseudos) {
                self.base.annotate_next(text);
            }
        }

        let (_total_frame, callee_saved, callee_saved_fp) = frame_info;

        match insn.op {
            Opcode::Entry => {
                // Already handled in function prologue
            }

            Opcode::Ret => {
                self.emit_ret(insn, callee_saved, callee_saved_fp, types);
            }

            Opcode::Br => {
                if let Some(target) = insn.bb_true {
                    self.push_lir(Aarch64Inst::B {
                        target: Label::new(&self.base.current_fn, target.0),
                    });
                }
            }

            Opcode::Cbr => if self.emit_cbr(insn) {},

            // GNU computed goto: branch through the address in src[0]. The

            // CFG edges to every address-taken label are recorded on the

            // block, so liveness and DCE already see the real successors.
            Opcode::IndirectBr => {
                if let Some(&val) = insn.src.first() {
                    let (scratch, _, _) = Reg::scratch_regs();

                    self.emit_move(val, scratch, 64);

                    self.push_lir(Aarch64Inst::BrReg { reg: scratch });
                }
            }

            Opcode::Switch => {
                self.emit_switch(insn, types);
            }

            Opcode::Add
            | Opcode::Sub
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
            | Opcode::Shl
            | Opcode::Lsr
            | Opcode::Asr => {
                self.emit_binop(insn, types);
            }

            Opcode::Mul => {
                self.emit_mul(insn, types);
            }

            Opcode::DivS | Opcode::DivU | Opcode::ModS | Opcode::ModU => {
                self.emit_div(insn, types);
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
                self.emit_compare(insn, types);
            }

            Opcode::Neg => self.emit_unary_op(insn, UnaryOp::Neg, types),
            Opcode::Not => self.emit_unary_op(insn, UnaryOp::Not, types),

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
                        match self.locations.get(target) {
                            Some(Loc::Reg(r)) => {
                                if let PseudoKind::Val(v) = &pseudo.kind {
                                    self.emit_mov_imm(r, *v as i64, insn.size);
                                }
                            }
                            Some(Loc::VReg(v)) => {
                                if let PseudoKind::FVal(f) = &pseudo.kind {
                                    // Load FP constant using integer register
                                    // Use type to determine float16 vs float vs double
                                    let typ = insn.typ.expect("FP constant must have type");
                                    let type_kind = types.kind(typ);
                                    let (scratch0, _, _) = Reg::scratch_regs();
                                    let (bits, fp_size) = match type_kind {
                                        TypeKind::Float16 => {
                                            // Convert f64 to IEEE 754 half-precision bits
                                            (f64_to_f16_bits(f.to_f64()) as i64, FpSize::Half)
                                        }
                                        TypeKind::Float => {
                                            ((f.to_f64() as f32).to_bits() as i64, FpSize::Single)
                                        }
                                        _ => (f.to_f64().to_bits() as i64, FpSize::Double),
                                    };
                                    self.emit_mov_imm(scratch0, bits, 64);
                                    // LIR: fmov from GP to FP register
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
                    self.emit_copy_with_type(src, target, insn.size, insn.typ, types);
                }
            }

            Opcode::TlsAddr => {
                if let (Some(target), Some(&src)) = (insn.target, insn.src.first()) {
                    let dst_loc = self.get_location(target);
                    // X17 is a reserved scratch, safe when the result is on
                    // the stack. X16 is used by the sequence itself.
                    let dst_reg = match &dst_loc {
                        Loc::Reg(r) => *r,
                        _ => Reg::X17,
                    };
                    if let Loc::Global(name) = self.get_location(src) {
                        self.emit_tls_addr(&name, dst_reg);
                        if !matches!(dst_loc, Loc::Reg(_)) {
                            self.emit_move_to_loc(dst_reg, &dst_loc, 64);
                        }
                    }
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
                        ref loc @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                            // Get address of stack location (FP-relative for
                            // alloca safety). An incoming stack argument can
                            // have its address taken too, and the `_` arm
                            // below would have emitted nothing at all for it.
                            let (base, adjusted) = self.loc_addr_parts(loc).unwrap();
                            self.push_lir(Aarch64Inst::Add {
                                size: OperandSize::B64,
                                src1: base,
                                src2: GpOperand::Imm(adjusted as i64),
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
                self.emit_select(insn, types);
            }

            Opcode::Zext | Opcode::Sext | Opcode::Trunc => {
                self.emit_extend(insn);
            }

            // Floating-point arithmetic operations
            Opcode::FAdd | Opcode::FSub | Opcode::FMul | Opcode::FDiv => {
                self.emit_fp_binop(insn, types);
            }

            // Floating-point negation
            Opcode::FNeg => {
                self.emit_fp_neg(insn, types);
            }

            // Floating-point comparisons
            Opcode::FCmpOEq
            | Opcode::FCmpONe
            | Opcode::FCmpOLt
            | Opcode::FCmpOLe
            | Opcode::FCmpOGt
            | Opcode::FCmpOGe => {
                self.emit_fp_compare(insn, types);
            }

            // Int to float conversions
            Opcode::UCvtF | Opcode::SCvtF => {
                self.emit_int_to_float(insn, types);
            }

            // Float to int conversions
            Opcode::FCvtU | Opcode::FCvtS => {
                self.emit_float_to_int(insn, types);
            }

            // Float to float conversions (size changes)
            Opcode::FCvtF => {
                self.emit_float_to_float(insn, types);
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
                // The C standard says it must be called, but it does nothing
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

            Opcode::StackSave => self.emit_stack_save(insn),
            Opcode::StackRestore => self.emit_stack_restore(insn),

            Opcode::Fabs32 => self.emit_fabs(insn, types, false),
            Opcode::Fabs64 => self.emit_fabs(insn, types, true),

            Opcode::Signbit32 => self.emit_signbit32(insn, types),
            Opcode::Signbit64 => self.emit_signbit64(insn, types),

            Opcode::Unreachable => {
                // Emit brk #1 instruction - software breakpoint that traps
                // This is used for __builtin_unreachable() to indicate code
                // that should never be reached. If it is reached, the CPU
                // will generate a SIGTRAP.
                self.push_lir(Aarch64Inst::Brk { imm: 1 });
            }

            Opcode::FrameAddress => {
                // __builtin_frame_address(level)
                self.emit_frame_address(insn);
            }

            Opcode::ReturnAddress => {
                // __builtin_return_address(level)
                self.emit_return_address(insn);
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

            // ================================================================
            // Inline Assembly
            // ================================================================
            Opcode::Asm => {
                self.emit_inline_asm(insn);
            }

            // ================================================================
            // Atomic Operations
            // ================================================================
            Opcode::AtomicLoad => {
                self.emit_atomic_load(insn);
            }

            Opcode::AtomicStore => {
                self.emit_atomic_store(insn);
            }

            Opcode::AtomicSwap => {
                self.emit_atomic_swap(insn);
            }

            Opcode::AtomicCas => {
                self.emit_atomic_cas(insn);
            }

            Opcode::AtomicFetchAdd => {
                self.emit_atomic_fetch_add(insn);
            }

            Opcode::AtomicFetchSub => {
                self.emit_atomic_fetch_sub(insn);
            }

            Opcode::AtomicFetchAnd => {
                self.emit_atomic_fetch_and(insn);
            }

            Opcode::AtomicFetchOr => {
                self.emit_atomic_fetch_or(insn);
            }

            Opcode::AtomicFetchXor => {
                self.emit_atomic_fetch_xor(insn);
            }

            Opcode::Fence => {
                self.emit_fence(insn);
            }

            // Int128 decomposition ops (from mapping pass expansion)
            Opcode::Lo64 => self.emit_lo64(insn),
            Opcode::Hi64 => self.emit_hi64(insn),
            Opcode::Pair64 => self.emit_pair64(insn),
            Opcode::AddC => self.emit_addc(insn, false),
            Opcode::AdcC => self.emit_addc(insn, true),
            Opcode::SubC => self.emit_subc(insn, false),
            Opcode::SbcC => self.emit_subc(insn, true),
            Opcode::UMulHi => self.emit_umulhi(insn),

            // Skip no-ops and unimplemented
            _ => {}
        }
    }

    pub(super) fn get_location(&self, pseudo: PseudoId) -> Loc {
        self.locations.get(pseudo).unwrap_or(Loc::Imm(0))
    }

    /// Load address of a global symbol into a register
    pub(super) fn emit_load_addr(&mut self, name: &str, dst: Reg) {
        // Thread-local storage: compute TLS address directly (Linux ELF only)
        if self.tls_symbols.contains(name) && self.base.target.os == Os::Linux {
            self.emit_tls_addr(name, dst);
            return;
        }

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

    /// Whether accessing the thread-local `name` needs the Initial Exec model
    /// rather than Local Exec. See [`CodeGenBase::use_tls_ie`].
    fn use_tls_ie(&self, name: &str) -> bool {
        self.base.use_tls_ie(self.extern_symbols.contains(name))
    }

    /// Emit TLS address computation into dst register.
    /// After this call, dst holds the address of the TLS variable.
    fn emit_tls_addr(&mut self, name: &str, dst: Reg) {
        let sym = Symbol::global(name);
        if self.base.use_tls_dynamic() {
            // TLS descriptor, the dynamic model:
            //   adrp  x0, :tlsdesc:sym
            //   ldr   x1, [x0, #:tlsdesc_lo12:sym]   ; resolver entry point
            //   add   x0, x0, :tlsdesc_lo12:sym      ; descriptor address
            //   .tlsdesccall sym
            //   blr   x1                             ; returns an OFFSET in x0
            //   mrs   tmp, tpidr_el0
            //   add   dst, tmp, x0                   ; plus the thread pointer
            //
            // x0 and x1 are fixed by the descriptor calling convention, and
            // the `.tlsdesccall` marker must immediately precede the `blr` for
            // the linker to relax the sequence.
            let entry = Reg::X1;
            self.push_lir(Aarch64Inst::AdrpTlsdesc {
                sym: sym.clone(),
                dst: Reg::X0,
            });
            self.push_lir(Aarch64Inst::LdrTlsdescLo12 {
                sym: sym.clone(),
                base: Reg::X0,
                dst: entry,
            });
            self.push_lir(Aarch64Inst::AddTlsdescLo12 {
                sym: sym.clone(),
                base: Reg::X0,
                dst: Reg::X0,
            });
            self.push_lir(Aarch64Inst::TlsdescCall { sym });
            self.push_lir(Aarch64Inst::Blr { reg: entry });
            let tp = Reg::X16;
            self.push_lir(Aarch64Inst::Mrs {
                sysreg: "tpidr_el0",
                dst: tp,
            });
            self.push_lir(Aarch64Inst::Add {
                size: OperandSize::B64,
                src1: tp,
                src2: GpOperand::Reg(Reg::X0),
                dst,
            });
            return;
        }
        if self.use_tls_ie(name) {
            // Initial Exec model (extern TLS or shared library):
            //   adrp  dst, :gottpoff:sym
            //   ldr   dst, [dst, :gottpoff_lo12:sym]
            //   mrs   tmp, tpidr_el0
            //   add   dst, tmp, dst
            let tmp = Reg::X16; // scratch register
            self.push_lir(Aarch64Inst::AdrpGottpoff {
                sym: sym.clone(),
                dst,
            });
            self.push_lir(Aarch64Inst::LdrGottpoffLo12 {
                sym,
                base: dst,
                dst,
            });
            self.push_lir(Aarch64Inst::Mrs {
                sysreg: "tpidr_el0",
                dst: tmp,
            });
            self.push_lir(Aarch64Inst::Add {
                size: OperandSize::B64,
                src1: tmp,
                src2: GpOperand::Reg(dst),
                dst,
            });
        } else {
            // Local Exec model (locally-defined TLS in executable):
            //   mrs   dst, tpidr_el0
            //   add   dst, dst, :tprel_hi12:sym
            //   add   dst, dst, :tprel_lo12_nc:sym
            self.push_lir(Aarch64Inst::Mrs {
                sysreg: "tpidr_el0",
                dst,
            });
            self.push_lir(Aarch64Inst::AddTprelHi12 {
                sym: sym.clone(),
                base: dst,
                dst,
            });
            self.push_lir(Aarch64Inst::AddTprelLo12Nc {
                sym,
                base: dst,
                dst,
            });
        }
    }

    /// Load value of a global symbol into a register with specified size
    pub(super) fn emit_load_global(&mut self, name: &str, dst: Reg, size: OperandSize) {
        // Thread-local storage: compute TLS address, then load value (Linux ELF only)
        if self.tls_symbols.contains(name) && self.base.target.os == Os::Linux {
            self.emit_tls_addr(name, dst);
            // dst now holds the address of the TLS variable; load from it
            self.push_lir(Aarch64Inst::Ldr {
                size,
                addr: MemAddr::Base(dst),
                dst,
            });
            return;
        }

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

    pub(super) fn emit_move(&mut self, src: PseudoId, dst: Reg, size: u32) {
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
            ref l @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                // For sub-32-bit values, use sized load (ldrb/ldrh) which zero-extends.
                // This avoids reading garbage from adjacent stack bytes.
                let load_size = OperandSize::from_bits(actual_size.max(8));
                // LIR: load from stack (FP-relative for alloca safety)
                self.push_lir(Aarch64Inst::Ldr {
                    size: load_size,
                    addr: self.loc_mem(l).unwrap(),
                    dst,
                });
            }
            Loc::Imm(v) => {
                self.emit_mov_imm(dst, v as i64, size);
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
                // This ensures float constants are loaded correctly for their type
                let bits = if imm_size == 16 {
                    f64_to_f16_bits(f.to_f64()) as i64
                } else if imm_size == 32 {
                    (f.to_f64() as f32).to_bits() as i64
                } else {
                    f.to_f64().to_bits() as i64
                };
                self.emit_mov_imm(dst, bits, imm_size);
            }
        }
    }

    /// Move a 128-bit value from a source pseudo to a stack destination.
    /// Both halves (lo/hi) are loaded into scratch registers and stored via STP.
    pub(super) fn emit_int128_move_to_stack(&mut self, src: PseudoId, dst_offset: LocalSlot) {
        let loc = self.get_location(src);
        match loc {
            ref loc @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                // Stack-to-stack copy: load lo/hi via LDP, store via STP.
                // A 128-bit argument that arrived on the stack is copied the
                // same way; without its arm it fell to the `_` case below and
                // was truncated to 64 bits.
                let src_mem = self.loc_mem(loc).unwrap();
                let dst_mem = self.stack_mem(dst_offset);
                self.emit_ldp_legalized(OperandSize::B64, src_mem, Reg::X9, Reg::X10);
                self.emit_stp_legalized(OperandSize::B64, Reg::X9, Reg::X10, dst_mem);
            }
            Loc::Imm(v) => {
                let lo = v as u64 as i64;
                let hi = (v >> 64) as u64 as i64;
                self.emit_mov_imm(Reg::X9, lo, 64);
                self.emit_mov_imm(Reg::X10, hi, 64);
                let dst_mem = self.stack_mem(dst_offset);
                self.emit_stp_legalized(OperandSize::B64, Reg::X9, Reg::X10, dst_mem);
            }
            _ => {
                // For other locations, load as 64-bit and zero-extend
                self.emit_move(src, Reg::X9, 64);
                let dst_mem = self.stack_mem(dst_offset);
                self.emit_stp_legalized(OperandSize::B64, Reg::X9, Reg::Xzr, dst_mem);
            }
        }
    }

    pub(super) fn emit_move_to_loc(&mut self, src: Reg, dst: &Loc, size: u32) {
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
                // LIR: store to stack (FP-relative for alloca safety)
                let op_size = OperandSize::from_bits(size);
                self.push_lir(Aarch64Inst::Str {
                    size: op_size,
                    src,
                    addr: self.stack_mem(*offset),
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
                    ComputedAddr::Direct(self.stack_mem_plus(offset, insn_offset as i32))
                } else {
                    // Spilled address - load address first (FP-relative for alloca safety)
                    self.push_lir(Aarch64Inst::Ldr {
                        size: OperandSize::B64,
                        addr: self.stack_mem(offset),
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
                self.emit_move(addr, temp_reg, 64);
                ComputedAddr::WithSetup(MemAddr::BaseOffset {
                    base: temp_reg,
                    offset: insn_offset as i32,
                })
            }
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
        let is_fp = insn.typ.is_some_and(|t| types.is_float(t)) || matches!(dst_loc, Loc::VReg(_));

        if is_fp {
            self.emit_fp_load(insn, types);
            return;
        }

        // 128-bit integer load: load both halves to destination stack slot
        if mem_size == 128 {
            if let Loc::Stack(dst_offset) = dst_loc {
                match self.compute_mem_addr(addr, insn.offset, Reg::X16) {
                    ComputedAddr::Direct(mem_addr) | ComputedAddr::WithSetup(mem_addr) => {
                        self.emit_ldp_legalized(OperandSize::B64, mem_addr, Reg::X9, Reg::X10);
                        self.emit_stp_legalized(
                            OperandSize::B64,
                            Reg::X9,
                            Reg::X10,
                            self.stack_mem(dst_offset),
                        );
                    }
                    ComputedAddr::Global(name) => {
                        self.emit_load_addr(&name, Reg::X16);
                        self.push_lir(Aarch64Inst::Ldp {
                            size: OperandSize::B64,
                            addr: MemAddr::Base(Reg::X16),
                            dst1: Reg::X9,
                            dst2: Reg::X10,
                        });
                        self.emit_stp_legalized(
                            OperandSize::B64,
                            Reg::X9,
                            Reg::X10,
                            self.stack_mem(dst_offset),
                        );
                    }
                }
            }
            return;
        }

        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X9,
        };

        // Sign- or zero-extend by the type's signedness. `is_unsigned`
        // answers for plain `char` per the target, so there is nothing to
        // special-case here.
        let is_unsigned = insn.typ.is_some_and(|t| types.is_unsigned(t));

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

        match self.compute_mem_addr(addr, insn.offset, Reg::X16) {
            ComputedAddr::Direct(mem_addr) | ComputedAddr::WithSetup(mem_addr) => {
                emit_load_lir(self, mem_addr);
            }
            ComputedAddr::Global(name) => {
                let load_size = OperandSize::from_bits(mem_size);
                self.emit_load_global(&name, dst_reg, load_size);
            }
        }

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, reg_size);
        }
    }

    fn emit_store(&mut self, insn: &Instruction, types: &TypeTable) {
        // Use actual size for memory stores (8, 16, 32, 64 bits)
        let mem_size = insn.size;

        let (addr, value) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&a), Some(&v)) => (a, v),
            _ => return,
        };

        // Floating-point stores need the FP path, as they do on x86_64 -- this
        // dispatch was missing entirely here. A 128-bit `long double` therefore
        // fell through to emit_struct_store below, whose operand match ends in
        // `_ => return` and so *silently dropped the store* when the value was
        // in a V register. That, not the FpSize mapping, is why assigning a
        // long double to a global produced no instruction at all (#H4).
        let value_loc = self.get_location(value);
        let is_fp = insn.typ.is_some_and(|t| types.is_float(t))
            || matches!(value_loc, Loc::VReg(_) | Loc::FImm(..));
        if is_fp {
            self.emit_fp_store(insn, types);
            return;
        }

        // For struct stores (size > 64), we need to copy multiple words
        // The value is a symbol containing the struct data
        if mem_size > 64 {
            // Check for Int128 immediate stores
            if mem_size == 128 {
                let value_loc = self.get_location(value);
                if let Loc::Imm(v) = value_loc {
                    self.emit_int128_imm_store(insn, addr, v);
                    return;
                }
            }
            self.emit_struct_store(insn, addr, value);
            return;
        }

        // Widen 32-bit stores at offset 0 to 64-bit to prevent stale
        // upper bits when a 32-bit result is stored into a 64-bit
        // local (e.g., int-to-long, int-to-pointer assignments).
        // Only widen for known local variables (in sym_type_sizes).
        // Do NOT widen stores to globals/statics (not in sym_type_sizes)
        // or stores through pointers — widening could clobber adjacent data.
        // Exception: struct/union fields at offset 0 must use exact
        // size to avoid clobbering the adjacent field at offset 4.
        let store_size = if mem_size == 32 && insn.offset == 0 {
            if let Some(&sym_bits) = self.sym_type_sizes.get(&addr) {
                // Known local variable — safe to widen if scalar and > 32 bits
                if sym_bits > 64 {
                    OperandSize::from_bits(mem_size) // struct field: exact size
                } else if sym_bits > 32 {
                    OperandSize::B64 // scalar/pointer local: safe to widen
                } else {
                    OperandSize::from_bits(mem_size)
                }
            } else {
                OperandSize::from_bits(mem_size) // global/static/pointer: exact size
            }
        } else {
            OperandSize::from_bits(mem_size)
        };

        // Use widened size for register load when store is widened
        let reg_size = if store_size == OperandSize::B64 {
            64
        } else {
            insn.size.max(32)
        };

        self.emit_move(value, Reg::X9, reg_size);

        // Helper to emit store instruction
        let emit_store_lir = |this: &mut Self, mem_addr: MemAddr| {
            this.push_lir(Aarch64Inst::Str {
                size: store_size,
                src: Reg::X9,
                addr: mem_addr,
            });
        };

        match self.compute_mem_addr(addr, insn.offset, Reg::X16) {
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
    fn emit_struct_store(&mut self, insn: &Instruction, addr: PseudoId, value: PseudoId) {
        let struct_size = insn.size; // Size in bits
        let num_qwords = struct_size.div_ceil(64);

        // Get source address (where the struct data is)
        let value_loc = self.get_location(value);

        // Special case: if value is immediate 0, zero the struct instead of copying
        if let Loc::Imm(0) = value_loc {
            self.emit_struct_zero(insn, addr, num_qwords);
            return;
        }

        // Get destination address
        let addr_loc = self.get_location(addr);

        // Load source address into X16 (FP-relative for alloca safety)
        match value_loc {
            ref loc @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                let (base, total_offset) = self.loc_addr_parts(loc).unwrap();
                self.push_lir(Aarch64Inst::Add {
                    size: OperandSize::B64,
                    src1: base,
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
                // Distinguish symbol (local variable — use direct address) vs
                // temp/spilled pointer (load the pointer value from the slot).
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));

                if is_symbol {
                    let (base, base_off) = self.loc_addr_parts(&addr_loc).unwrap();
                    let total_offset = base_off + insn.offset as i32;
                    self.push_lir(Aarch64Inst::Add {
                        size: OperandSize::B64,
                        src1: base,
                        src2: GpOperand::Imm(total_offset as i64),
                        dst: Reg::X17,
                    });
                } else {
                    // Spilled pointer — load the pointer value, then add offset
                    self.push_lir(Aarch64Inst::Ldr {
                        size: OperandSize::B64,
                        addr: self.stack_mem(offset),
                        dst: Reg::X17,
                    });
                    if insn.offset != 0 {
                        self.push_lir(Aarch64Inst::Add {
                            size: OperandSize::B64,
                            src1: Reg::X17,
                            src2: GpOperand::Imm(insn.offset),
                            dst: Reg::X17,
                        });
                    }
                }
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
    fn emit_struct_zero(&mut self, insn: &Instruction, addr: PseudoId, num_qwords: u32) {
        let addr_loc = self.get_location(addr);

        // Load destination address into X17
        match addr_loc {
            ref loc @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                let (base, base_off) = self.loc_addr_parts(loc).unwrap();
                let total_offset = base_off + insn.offset as i32;
                self.push_lir(Aarch64Inst::Add {
                    size: OperandSize::B64,
                    src1: base,
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

    /// Store a 128-bit immediate value to a memory destination
    fn emit_int128_imm_store(&mut self, insn: &Instruction, addr: PseudoId, v: i128) {
        let lo = v as u64 as i64;
        let hi = (v >> 64) as u64 as i64;
        self.emit_mov_imm(Reg::X9, lo, 64);
        self.emit_mov_imm(Reg::X10, hi, 64);

        match self.compute_mem_addr(addr, insn.offset, Reg::X16) {
            ComputedAddr::Direct(mem_addr) | ComputedAddr::WithSetup(mem_addr) => {
                self.emit_stp_legalized(OperandSize::B64, Reg::X9, Reg::X10, mem_addr);
            }
            ComputedAddr::Global(name) => {
                self.emit_load_addr(&name, Reg::X16);
                self.push_lir(Aarch64Inst::Stp {
                    size: OperandSize::B64,
                    src1: Reg::X9,
                    src2: Reg::X10,
                    addr: MemAddr::Base(Reg::X16),
                });
            }
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

        // For indirect calls, load function pointer into X16
        if let Some(func_addr) = insn.indirect_target {
            self.emit_move(func_addr, Reg::X16, 64);
        }

        // Handle sret (hidden struct return pointer) argument
        let args_start = self.setup_sret_arg(insn);

        // Determine if this is a Darwin variadic call
        let is_darwin_variadic =
            self.base.target.os == crate::target::Os::MacOS && insn.variadic_arg_start.is_some();

        // Set up arguments and get stack cleanup count
        let stack_args = if is_darwin_variadic {
            self.setup_darwin_variadic_args(insn, args_start, types)
        } else {
            self.setup_register_args(insn, args_start, types)
        };

        // Emit the call instruction
        self.emit_call_instruction(insn, &func_name);

        // Clean up stack
        self.cleanup_call_stack(stack_args);

        // Handle return value
        self.handle_call_return_value(insn, types);
    }

    /// Emit a select (ternary) instruction using CSEL (integers) or
    /// conditional branch (floats, since CSEL only works on GP registers).
    fn emit_select(&mut self, insn: &Instruction, types: &TypeTable) {
        let (cond, then_val, else_val) = match (insn.src.first(), insn.src.get(1), insn.src.get(2))
        {
            (Some(&c), Some(&t), Some(&e)) => (c, t, e),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let size = insn
            .typ
            .map(|t| types.size_bits(t).max(32))
            .unwrap_or(insn.size.max(32));

        // Check if this is a floating-point select
        let is_fp = insn.typ.is_some_and(|t| types.is_float(t))
            || matches!(self.get_location(then_val), Loc::VReg(_) | Loc::FImm(..))
            || matches!(self.get_location(else_val), Loc::VReg(_) | Loc::FImm(..));

        if is_fp {
            self.emit_select_fp(cond, then_val, else_val, target, insn.typ, size, types);
        } else {
            self.emit_select_int(cond, then_val, else_val, target, size);
        }
    }

    /// Emit FP select using conditional branch (CSEL doesn't work on VRegs)
    #[allow(clippy::too_many_arguments)]
    fn emit_select_fp(
        &mut self,
        cond: PseudoId,
        then_val: PseudoId,
        else_val: PseudoId,
        target: PseudoId,
        typ: Option<TypeId>,
        size: u32,
        types: &TypeTable,
    ) {
        let dst_loc = self.get_location(target);

        // Check if condition is a constant
        let cond_loc = self.get_location(cond);
        if let Loc::Imm(v) = cond_loc {
            let val = if v != 0 { then_val } else { else_val };
            self.emit_fp_move(val, VReg::V17, typ, size, types);
            self.emit_fp_move_to_loc(VReg::V17, &dst_loc, typ, size, types);
            return;
        }

        // Load condition to GP register and compare with zero
        self.emit_move(cond, Reg::X16, 64);
        self.push_lir(Aarch64Inst::Cmp {
            size: OperandSize::B64,
            src1: Reg::X16,
            src2: GpOperand::Imm(0),
        });

        let then_label = self.next_unique_label("sel_then");
        let done_label = self.next_unique_label("sel_done");

        // Branch if cond != 0
        self.push_lir(Aarch64Inst::BCond {
            cond: CondCode::Ne,
            target: then_label.clone(),
        });

        // Else: load else_val to V17
        self.emit_fp_move(else_val, VReg::V17, typ, size, types);
        self.push_lir(Aarch64Inst::B {
            target: done_label.clone(),
        });

        // Then: load then_val to V17
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(then_label)));
        self.emit_fp_move(then_val, VReg::V17, typ, size, types);

        // Done: store V17 to destination
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(done_label)));
        self.emit_fp_move_to_loc(VReg::V17, &dst_loc, typ, size, types);
    }

    /// Emit integer select using CSEL
    fn emit_select_int(
        &mut self,
        cond: PseudoId,
        then_val: PseudoId,
        else_val: PseudoId,
        target: PseudoId,
        size: u32,
    ) {
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
        self.emit_move(cond, cond_reg, 64);
        self.emit_move(then_val, then_reg, size);
        self.emit_move(else_val, else_reg, size);

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

        // Check if this is a FP copy (source or dest is in VReg or is FImm)
        let is_fp_copy =
            matches!(&src_loc, Loc::VReg(_) | Loc::FImm(..)) || matches!(&dst_loc, Loc::VReg(_));

        // M9a — identity-Copy elision (aarch64 mirror of x86_64
        // path). Same gating: same location, no narrow-type
        // extension, not FP. See x86_64::emit_copy_with_type for
        // the full rationale.
        if src_loc == dst_loc && actual_size >= 32 && !is_fp_copy {
            return;
        }

        // Sign- or zero-extend by the type's signedness. `is_unsigned`
        // answers for plain `char` per the target, so there is nothing to
        // special-case here.
        let is_unsigned = typ.is_some_and(|t| types.is_unsigned(t));

        // Handle 128-bit integer copy.
        //
        // Only integers: a 128-bit `long double` is binary128, and this path
        // moves the low 64 bits through a general-purpose register. That
        // truncated an `FImm` to its *f64* encoding (and a `VReg` to its low
        // lane), so `long double a = 3.14159...L;` landed a denormal on the
        // stack. FP goes through `emit_fp_move`, which assembles both halves.
        if actual_size == 128 && !is_fp_copy {
            if let Loc::Stack(dst_offset) = dst_loc {
                self.emit_int128_move_to_stack(src, dst_offset);
            }
            return;
        }

        if is_fp_copy {
            // Handle FP copy
            let dst_vreg = match &dst_loc {
                Loc::VReg(v) => *v,
                _ => VReg::V16, // Use scratch register
            };

            self.emit_fp_move(src, dst_vreg, typ, reg_size, types);

            if !matches!(&dst_loc, Loc::VReg(v) if *v == dst_vreg) {
                self.emit_fp_move_to_loc(dst_vreg, &dst_loc, typ, reg_size, types);
            }
        } else {
            // Integer copy
            match &dst_loc {
                Loc::Reg(r) => {
                    self.emit_move(src, *r, reg_size);
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
                    self.emit_move(src, Reg::X9, reg_size);
                    // For narrow types stored to stack, use the actual size
                    if actual_size <= 16 {
                        self.emit_move_to_loc(Reg::X9, &dst_loc, actual_size);
                    } else {
                        self.emit_move_to_loc(Reg::X9, &dst_loc, reg_size);
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
    fn emit_inline_asm(&mut self, insn: &Instruction) {
        let asm_data = match &insn.asm_data {
            Some(data) => data,
            None => return,
        };

        // Build operand slots for asm substitution. Each `AsmOperandSlot`
        // bundles (reg, mem, size, name) so per-operand pushes can't go
        // out of sync — see the x86_64 mirror's commit for the
        // motivation.
        let operand_count = asm_data.outputs.len() + asm_data.inputs.len();
        let mut slots: Vec<crate::arch::AsmOperandSlot<Reg>> = Vec::with_capacity(operand_count);

        // Scratch budgets, shared by the output and input loops so two
        // operands can never be handed the same register. `Reg::scratch_regs`
        // reserves X9/X10/X11 and `VReg::allocatable` reserves V16/V17/V18;
        // both loops used to take the first of each unconditionally, so
        // `"r"(a), "r"(b)` rendered `fmov x9, d0; fmov x9, d1; add x0,x9,x9`
        // -- operand 1 destroyed, result 2*b, no diagnostic. Popped from the
        // end so X9 and V16 go first, as before.
        let mut gp_scratch: Vec<Reg> = vec![Reg::X11, Reg::X10, Reg::X9];
        let mut v_scratch: Vec<VReg> = vec![VReg::V18, VReg::V17, VReg::V16];
        // Vector operands copied back out of their scratch after the template.
        let mut vec_output_moves: Vec<(VReg, Loc, u32)> = Vec::new();
        // Vector operands copied into their scratch before it.
        let mut vec_input_moves: Vec<(VReg, Loc, u32)> = Vec::new();

        // Process output operands (they go first: %0, %1, etc.)
        for output in &asm_data.outputs {
            let loc = self.get_location(output.pseudo);
            let requires_mem = Self::constraint_requires_memory(&output.constraint);
            let op_size = output.size;
            let op_name = output.name.clone();
            let mk = |reg: Option<Reg>, mem: Option<String>| crate::arch::AsmOperandSlot {
                reg,
                mem,
                size: op_size,
                name: op_name.clone(),
            };
            match loc {
                Loc::Reg(r) if requires_mem => {
                    // Memory-class constraint with the address in a
                    // register: render `[xN]` so `ldr`/`str` see a
                    // valid AAPCS64 memory operand. Without this the
                    // asm template substitutes `wN`/`xN` and the
                    // assembler rejects (`ldr w8, w0` → "expected
                    // label or encodable integer pc offset").
                    slots.push(mk(None, Some(format!("[{}]", asm_reg_name_64(r)))));
                }
                // A vector-class output. Without this arm the output loop
                // had no notion of one at all, so a `"=w"` output took
                // whatever the allocator gave the pseudo -- a general
                // register -- and emitted `fmov d17, x0` around a template the
                // assembler then rejected. The x86-64 side grew the same arm
                // as #C139; this target was recorded as sound and was not.
                _ if Self::constraint_requires_vector(&output.constraint) => {
                    match v_scratch.pop() {
                        Some(v) => {
                            slots.push(mk(None, Some(Self::vreg_name(v, op_size).to_string())));
                            vec_output_moves.push((v, loc.clone(), op_size));
                            if output.constraint.contains('+') {
                                vec_input_moves.push((v, loc.clone(), op_size));
                            }
                        }
                        None => {
                            crate::diag::error(
                                insn.pos.unwrap_or_default(),
                                "too many vector register constraints in one asm \
                                 statement; c17 has three scratch registers to give",
                            );
                            slots.push(mk(None, Some(VReg::V16.name_d().to_string())));
                        }
                    }
                }
                Loc::Reg(r) => {
                    slots.push(mk(Some(r), None));
                }
                _ => {
                    // Memory or other location - emit as memory operand
                    let mem_str = self.loc_to_asm_string(&loc, op_size);
                    slots.push(mk(None, Some(mem_str)));
                }
            }
        }

        let num_outputs = asm_data.outputs.len();

        // Whether V16 has already been spent materializing a floating
        // constant for a vector-class constraint. There is only the one.

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
            let requires_mem = Self::constraint_requires_memory(&input.constraint);
            let op_size = input.size;
            let op_name = input.name.clone();
            let mk = |reg: Option<Reg>, mem: Option<String>| crate::arch::AsmOperandSlot {
                reg,
                mem,
                size: op_size,
                name: op_name.clone(),
            };
            match loc {
                Loc::Reg(r) if requires_mem => {
                    // See output-side note: memory-class input with
                    // its address in a register renders as `[xN]`.
                    slots.push(mk(None, Some(format!("[{}]", asm_reg_name_64(r)))));
                }
                Loc::Reg(r) => {
                    slots.push(mk(Some(r), None));
                }
                Loc::Imm(v) => {
                    // Immediate value
                    slots.push(mk(None, Some(format!("#{}", v as i64))));
                }
                // A floating constant has no address, so a memory-class
                // constraint cannot be satisfied. gcc says the same and stops.
                Loc::FImm(..) if requires_mem => {
                    crate::diag::error(
                        insn.pos.unwrap_or_default(),
                        &format!("memory input {} is not directly addressable", slots.len()),
                    );
                    slots.push(mk(None, Some("[sp]".to_string())));
                }
                // A register-class constraint wants the value in a register,
                // and a constant is never allocated one. Materialize it into
                // a scratch: nothing else is live there across the asm.
                Loc::FImm(v, imm_size)
                    if Self::constraint_requires_reg_class(&input.constraint) =>
                {
                    let bits = v.to_bits_at_width(imm_size);
                    let Some(scratch) = gp_scratch.pop() else {
                        crate::diag::error(
                            insn.pos.unwrap_or_default(),
                            "too many register constraints in one asm statement; \
                             c17 has three general scratch registers to give",
                        );
                        slots.push(mk(Some(Reg::X9), None));
                        continue;
                    };
                    self.emit_mov_imm(scratch, bits, 64);
                    if Self::constraint_requires_vector(&input.constraint) {
                        // Three scratch V registers are reserved, not one.
                        let Some(vreg) = v_scratch.pop() else {
                            crate::diag::error(
                                insn.pos.unwrap_or_default(),
                                "too many vector register constraints in one asm \
                                 statement; c17 has three scratch registers to give",
                            );
                            slots.push(mk(None, Some(VReg::V16.name_d().to_string())));
                            continue;
                        };
                        let fp_size = match imm_size {
                            16 => FpSize::Half,
                            32 => FpSize::Single,
                            _ => FpSize::Double,
                        };
                        self.push_lir(Aarch64Inst::FmovFromGp {
                            size: fp_size,
                            src: scratch,
                            dst: vreg,
                        });
                        // A vector operand has to be pre-rendered: the slot
                        // carries only a general register, and `%w`-style
                        // width modifiers do not apply to one of these.
                        slots.push(mk(None, Some(Self::vreg_name(vreg, imm_size).to_string())));
                    } else {
                        // A *register* slot, not a pre-rendered name: the
                        // template decides the width it wants, and `%w1`
                        // against a hard-coded `x9` assembled as
                        // `mov w0, x9`.
                        slots.push(mk(Some(scratch), None));
                    }
                }
                // An FP *value* under a general-register constraint. Nothing
                // put it in a general register, and rendering the vector
                // register's name gave `mov x0, d0` -- the assembler reads
                // `d0` as an undefined symbol. Pre-existing, and reachable
                // from any FP variable passed as `"r"`, not just a constant:
                // `-0.0` arrives here rather than as an `FImm` because it is
                // computed as `fneg` of zero.
                // Already in a vector register, and that is what was asked
                // for: name it directly at the operand's width.
                Loc::VReg(v) if Self::constraint_requires_vector(&input.constraint) => {
                    slots.push(mk(None, Some(Self::vreg_name(v, op_size).to_string())));
                }
                // A vector-class input that is not in a vector register. The
                // output loop's note applies: without this the operand named
                // whatever the allocator gave the pseudo.
                _ if Self::constraint_requires_vector(&input.constraint) => {
                    let Some(vreg) = v_scratch.pop() else {
                        crate::diag::error(
                            insn.pos.unwrap_or_default(),
                            "too many vector register constraints in one asm \
                             statement; c17 has three scratch registers to give",
                        );
                        slots.push(mk(None, Some(VReg::V16.name_d().to_string())));
                        continue;
                    };
                    slots.push(mk(None, Some(Self::vreg_name(vreg, op_size).to_string())));
                    vec_input_moves.push((vreg, loc.clone(), op_size));
                }
                Loc::VReg(v) if !Self::constraint_requires_vector(&input.constraint) => {
                    let Some(scratch) = gp_scratch.pop() else {
                        crate::diag::error(
                            insn.pos.unwrap_or_default(),
                            "too many register constraints in one asm statement; \
                             c17 has three general scratch registers to give",
                        );
                        slots.push(mk(Some(Reg::X9), None));
                        continue;
                    };
                    let fp_size = match op_size {
                        16 => FpSize::Half,
                        32 => FpSize::Single,
                        _ => FpSize::Double,
                    };
                    self.push_lir(Aarch64Inst::FmovToGp {
                        size: fp_size,
                        src: v,
                        dst: scratch,
                    });
                    slots.push(mk(Some(scratch), None));
                }
                _ => {
                    // Memory or other location
                    let mem_str = self.loc_to_asm_string(&loc, op_size);
                    slots.push(mk(None, Some(mem_str)));
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

        // Load vector operands into their scratch before the template runs.
        for (vreg, loc, size) in &vec_input_moves {
            self.emit_vec_load_from_loc(*vreg, loc, *size, insn.pos);
        }

        // Substitute %0, %1, %[name], %l0, %l[name], etc. in the template with actual operands
        let asm_output =
            self.substitute_asm_operands(&asm_data.template, &slots, &goto_labels_formatted);

        // Emit the inline assembly as raw text
        // Split by newlines and emit each line
        for line in asm_output.lines() {
            let trimmed = line.trim();
            if !trimmed.is_empty() {
                self.push_lir(Aarch64Inst::Directive(Directive::Raw(trimmed.to_string())));
            }
        }

        // Copy vector outputs out of their scratch into where the operand
        // actually lives.
        for (vreg, loc, size) in &vec_output_moves {
            self.emit_vec_store_to_loc(*vreg, loc, *size, insn.pos);
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

    /// Check whether an inline-asm constraint string requires the
    /// operand to be a memory operand. Mirrors x86_64's equivalent —
    /// memory-class only (`m`/`o`/`V`/`Q`) returns true; any non-
    /// memory class letter (`r`/`w`/`i`/`n`/`g`/`X`/`I`...`O` and the
    /// aarch64 class letters `S`/`Y`/`Z`) defeats the requirement
    /// because the operand can take its non-memory form. C9 multi-
    /// alternative `"rm"` therefore returns false (register or
    /// memory both work; codegen picks register if available).
    /// Move an operand's value into a scratch vector register.
    fn emit_vec_load_from_loc(
        &mut self,
        dst: VReg,
        loc: &Loc,
        size: u32,
        pos: Option<crate::diag::Position>,
    ) {
        let fp_size = match size {
            16 => FpSize::Half,
            32 => FpSize::Single,
            _ => FpSize::Double,
        };
        match loc {
            Loc::VReg(src) => {
                if *src != dst {
                    self.push_lir(Aarch64Inst::FmovReg {
                        size: fp_size,
                        src: *src,
                        dst,
                    });
                }
            }
            Loc::Reg(src) => self.push_lir(Aarch64Inst::FmovFromGp {
                size: fp_size,
                src: *src,
                dst,
            }),
            Loc::Stack(off) => {
                let addr = self.stack_mem(*off);
                self.push_lir(Aarch64Inst::LdrFp {
                    size: fp_size,
                    addr,
                    dst,
                });
            }
            _ => crate::diag::error(
                pos.unwrap_or_default(),
                "a vector asm operand cannot be read from this location",
            ),
        }
    }

    /// Move a scratch vector register's value back to where the operand lives.
    fn emit_vec_store_to_loc(
        &mut self,
        src: VReg,
        loc: &Loc,
        size: u32,
        pos: Option<crate::diag::Position>,
    ) {
        let fp_size = match size {
            16 => FpSize::Half,
            32 => FpSize::Single,
            _ => FpSize::Double,
        };
        match loc {
            Loc::VReg(dst) => {
                if *dst != src {
                    self.push_lir(Aarch64Inst::FmovReg {
                        size: fp_size,
                        src,
                        dst: *dst,
                    });
                }
            }
            Loc::Reg(dst) => self.push_lir(Aarch64Inst::FmovToGp {
                size: fp_size,
                src,
                dst: *dst,
            }),
            Loc::Stack(off) => {
                let addr = self.stack_mem(*off);
                self.push_lir(Aarch64Inst::StrFp {
                    size: fp_size,
                    src,
                    addr,
                });
            }
            _ => crate::diag::error(
                pos.unwrap_or_default(),
                "a vector asm output cannot be written back to this location",
            ),
        }
    }

    /// The vector register spelling an operand of `size_bits` wants.
    ///
    /// AArch64 names the same register `b`/`h`/`s`/`d`/`q` by the width being
    /// operated on, and an instruction that wants `d0` rejects `v0`.
    fn vreg_name(v: VReg, size_bits: u32) -> &'static str {
        match size_bits {
            0..=8 => v.name_b(),
            16 => v.name_h(),
            32 => v.name_s(),
            64 => v.name_d(),
            _ => v.name_q(),
        }
    }

    /// Whether the constraint asks for a vector (SIMD/FP) register.
    fn constraint_requires_vector(constraint: &str) -> bool {
        constraint.chars().any(|c| matches!(c, 'w' | 'x' | 'y'))
    }

    /// Whether the constraint asks for the operand in a register at all,
    /// general or vector — as opposed to an immediate or memory class.
    fn constraint_requires_reg_class(constraint: &str) -> bool {
        constraint
            .chars()
            .any(|c| matches!(c, 'r' | 'w' | 'x' | 'y'))
    }

    fn constraint_requires_memory(constraint: &str) -> bool {
        let mut has_mem_class = false;
        let mut has_non_mem_class = false;
        for c in constraint.chars() {
            match c {
                'm' | 'o' | 'V' | 'Q' => has_mem_class = true,
                'r' | 'w' | 'i' | 'n' | 'g' | 'X' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O'
                | 'S' | 'Y' | 'Z' => has_non_mem_class = true,
                _ => {}
            }
        }
        has_mem_class && !has_non_mem_class
    }

    /// Convert a location to an asm operand string for AArch64
    fn loc_to_asm_string(&self, loc: &Loc, size_bits: u32) -> String {
        match loc {
            Loc::Reg(r) => {
                if size_bits <= 32 {
                    asm_reg_name_32(*r).to_string()
                } else {
                    asm_reg_name_64(*r).to_string()
                }
            }
            loc @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                // AArch64 addresses a local from whichever base the frame uses
                let (base, actual) = self.loc_addr_parts(loc).unwrap();
                format!("[{}, #{}]", asm_reg_name_64(base), actual)
            }
            Loc::Imm(v) => format!("#{}", *v as i64),
            Loc::VReg(vreg) => vreg.name_d().to_string(),
            // An immediate-class constraint takes the constant's bit pattern:
            // there is no other way to name a floating value in an assembler
            // operand. The register and memory classes never reach here --
            // `emit_inline_asm` materializes or diagnoses them first -- and
            // this used to `panic!` for all three.
            Loc::FImm(v, fp_size) => format!("#{}", v.to_bits_at_width(*fp_size)),
            Loc::Global(name) => name.clone(),
        }
    }

    /// Substitute %0, %1, %[name], %l0, %l[name], etc. in asm template with actual operands
    /// goto_labels: (label_string, label_name) - label_string is the fully formatted label
    fn substitute_asm_operands(
        &self,
        template: &str,
        slots: &[crate::arch::AsmOperandSlot<Reg>],
        goto_labels: &[(String, String)],
    ) -> String {
        crate::arch::substitute_asm_operands(self, template, slots, goto_labels)
    }

    // ========================================================================
    // Atomic Operations (ARMv8.1 LSE)
    // ========================================================================

    /// Emit atomic load
    fn emit_atomic_load(&mut self, insn: &Instruction) {
        let target = insn.target.expect("atomic load needs target");
        let addr = insn.src[0];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let addr_loc = self.get_location(addr);

        // Load pointer value into X10 (pointer is 64-bit)
        self.emit_mov_to_reg(addr_loc, Reg::X10, 64);

        // LDAR provides acquire semantics (sufficient for SeqCst on AArch64)
        self.push_lir(Aarch64Inst::Ldar {
            size: op_size,
            addr: MemAddr::Base(Reg::X10),
            dst: Reg::X0,
        });

        // The instruction leaves the result in a fixed scratch register, but
        // the allocator gave this pseudo its own location. Overwriting that
        // made every atomic result alias the same register, so two atomic
        // reads in one expression collapsed into one -- `x + y` on two
        // _Atomic ints returned `y + y`. Move it where the allocator expects.
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::X0) {
            self.emit_move_to_loc(Reg::X0, &dst_loc, size.max(32));
        }
    }

    /// Emit atomic store
    fn emit_atomic_store(&mut self, insn: &Instruction) {
        let addr = insn.src[0];
        let value = insn.src[1];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let addr_loc = self.get_location(addr);
        let value_loc = self.get_location(value);

        // Load pointer into X10 FIRST (before value, in case addr is in X0)
        self.emit_mov_to_reg(addr_loc, Reg::X10, 64);

        // Load value into X0
        self.emit_mov_to_reg(value_loc, Reg::X0, size);

        // STLR provides release semantics (sufficient for SeqCst on AArch64)
        self.push_lir(Aarch64Inst::Stlr {
            size: op_size,
            src: Reg::X0,
            addr: MemAddr::Base(Reg::X10),
        });

        // Atomic store has no result value
        if let Some(target) = insn.target {
            self.locations.set(target, Loc::Imm(0));
        }
    }

    /// Emit atomic swap using LL/SC (LDAXR/STLXR loop)
    fn emit_atomic_swap(&mut self, insn: &Instruction) {
        let target = insn.target.expect("atomic swap needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let addr_loc = self.get_location(addr);
        let value_loc = self.get_location(value);

        // Load pointer into X10 FIRST (before value, in case addr is in X0)
        self.emit_mov_to_reg(addr_loc, Reg::X10, 64);

        // Load new value into X0
        self.emit_mov_to_reg(value_loc, Reg::X0, size);

        // LL/SC loop for atomic swap
        let loop_label = self.next_unique_label("swap_loop");

        // Loop label
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(
            loop_label.clone(),
        )));

        // LDAXR: Load-acquire exclusive old value into X1
        self.push_lir(Aarch64Inst::Ldaxr {
            size: op_size,
            addr: MemAddr::Base(Reg::X10),
            dst: Reg::X1,
        });

        // STLXR: Try to store X0 (new value), status in W8
        self.push_lir(Aarch64Inst::Stlxr {
            size: op_size,
            src: Reg::X0,
            addr: MemAddr::Base(Reg::X10),
            status: Reg::X8,
        });

        // CBNZ: Retry if store failed (status != 0)
        self.push_lir(Aarch64Inst::Cbnz {
            size: OperandSize::B32, // Status is always 32-bit
            src: Reg::X8,
            target: loop_label,
        });

        // Result: X1 = old value
        // The instruction leaves the result in a fixed scratch register, but
        // the allocator gave this pseudo its own location. Overwriting that
        // made every atomic result alias the same register, so two atomic
        // reads in one expression collapsed into one -- `x + y` on two
        // _Atomic ints returned `y + y`. Move it where the allocator expects.
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::X1) {
            self.emit_move_to_loc(Reg::X1, &dst_loc, size.max(32));
        }
    }

    /// Emit atomic compare-and-swap using LL/SC
    fn emit_atomic_cas(&mut self, insn: &Instruction) {
        let target = insn.target.expect("atomic CAS needs target");
        let addr = insn.src[0];
        let expected_ptr = insn.src[1];
        let desired = insn.src[2];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let addr_loc = self.get_location(addr);
        let expected_loc = self.get_location(expected_ptr);
        let desired_loc = self.get_location(desired);

        // Load pointer to atomic variable into X10 FIRST
        // (before other loads in case addr is in X11, X9, or X1)
        self.emit_mov_to_reg(addr_loc, Reg::X10, 64);

        // Load expected_ptr (pointer to expected value) into X11
        // Then load the expected value from that address into X9
        self.emit_mov_to_reg(expected_loc, Reg::X11, 64);
        self.push_lir(Aarch64Inst::Ldr {
            size: op_size,
            addr: MemAddr::Base(Reg::X11),
            dst: Reg::X9,
        });

        // Load desired value into X1
        self.emit_mov_to_reg(desired_loc, Reg::X1, size);

        // LL/SC loop for CAS
        let loop_label = self.next_unique_label("cas_loop");
        let fail_label = self.next_unique_label("cas_fail");
        let done_label = self.next_unique_label("cas_done");

        // Loop label
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(
            loop_label.clone(),
        )));

        // LDAXR: Load-acquire exclusive current value into X0
        self.push_lir(Aarch64Inst::Ldaxr {
            size: op_size,
            addr: MemAddr::Base(Reg::X10),
            dst: Reg::X0,
        });

        // Compare current value (X0) with expected (X9)
        self.push_lir(Aarch64Inst::Cmp {
            size: op_size,
            src1: Reg::X0,
            src2: GpOperand::Reg(Reg::X9),
        });

        // If not equal, branch to fail
        self.push_lir(Aarch64Inst::BCond {
            cond: CondCode::Ne,
            target: fail_label.clone(),
        });

        // STLXR: Try to store desired (X1), status in W8
        self.push_lir(Aarch64Inst::Stlxr {
            size: op_size,
            src: Reg::X1,
            addr: MemAddr::Base(Reg::X10),
            status: Reg::X8,
        });

        // CBNZ: Retry loop if store failed
        self.push_lir(Aarch64Inst::Cbnz {
            size: OperandSize::B32,
            src: Reg::X8,
            target: loop_label,
        });

        // Success: set result to 1
        self.push_lir(Aarch64Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Imm(1),
            dst: Reg::X2,
        });
        self.push_lir(Aarch64Inst::B {
            target: done_label.clone(),
        });

        // Fail label: CAS failed (value != expected)
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(fail_label)));

        // Store actual value to *expected_ptr (X11 has expected_ptr)
        self.push_lir(Aarch64Inst::Str {
            size: op_size,
            src: Reg::X0,
            addr: MemAddr::Base(Reg::X11),
        });

        // Set result to 0 (failure)
        self.push_lir(Aarch64Inst::Mov {
            size: OperandSize::B32,
            src: GpOperand::Imm(0),
            dst: Reg::X2,
        });

        // Done label
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(done_label)));

        // The instruction leaves the result in a fixed scratch register, but
        // the allocator gave this pseudo its own location. Overwriting that
        // made every atomic result alias the same register, so two atomic
        // reads in one expression collapsed into one -- `x + y` on two
        // _Atomic ints returned `y + y`. Move it where the allocator expects.
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::X2) {
            self.emit_move_to_loc(Reg::X2, &dst_loc, size.max(32));
        }
    }

    /// Emit atomic fetch-and-add using LL/SC
    fn emit_atomic_fetch_add(&mut self, insn: &Instruction) {
        let target = insn.target.expect("atomic fetch_add needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let addr_loc = self.get_location(addr);
        let value_loc = self.get_location(value);

        // Load pointer into X10 FIRST (before value, in case addr is in X0)
        self.emit_mov_to_reg(addr_loc, Reg::X10, 64);

        // Load addend value into X0
        self.emit_mov_to_reg(value_loc, Reg::X0, size);

        // LL/SC loop for fetch_add
        let loop_label = self.next_unique_label("fadd_loop");

        // Loop label
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(
            loop_label.clone(),
        )));

        // LDAXR: Load-acquire exclusive old value into X1
        self.push_lir(Aarch64Inst::Ldaxr {
            size: op_size,
            addr: MemAddr::Base(Reg::X10),
            dst: Reg::X1,
        });

        // ADD: X2 = X1 (old) + X0 (addend)
        self.push_lir(Aarch64Inst::Add {
            size: op_size,
            src1: Reg::X1,
            src2: GpOperand::Reg(Reg::X0),
            dst: Reg::X2,
        });

        // STLXR: Try to store X2 (new value), status in W8
        self.push_lir(Aarch64Inst::Stlxr {
            size: op_size,
            src: Reg::X2,
            addr: MemAddr::Base(Reg::X10),
            status: Reg::X8,
        });

        // CBNZ: Retry if store failed
        self.push_lir(Aarch64Inst::Cbnz {
            size: OperandSize::B32,
            src: Reg::X8,
            target: loop_label,
        });

        // Result: X1 = old value
        // The instruction leaves the result in a fixed scratch register, but
        // the allocator gave this pseudo its own location. Overwriting that
        // made every atomic result alias the same register, so two atomic
        // reads in one expression collapsed into one -- `x + y` on two
        // _Atomic ints returned `y + y`. Move it where the allocator expects.
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::X1) {
            self.emit_move_to_loc(Reg::X1, &dst_loc, size.max(32));
        }
    }

    /// Emit atomic fetch-and-subtract using LL/SC
    fn emit_atomic_fetch_sub(&mut self, insn: &Instruction) {
        let target = insn.target.expect("atomic fetch_sub needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let addr_loc = self.get_location(addr);
        let value_loc = self.get_location(value);

        // Load pointer into X10 FIRST (before value, in case addr is in X0)
        self.emit_mov_to_reg(addr_loc, Reg::X10, 64);

        // Load subtrahend value into X0
        self.emit_mov_to_reg(value_loc, Reg::X0, size);

        // LL/SC loop for fetch_sub
        let loop_label = self.next_unique_label("fsub_loop");

        // Loop label
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(
            loop_label.clone(),
        )));

        // LDAXR: Load-acquire exclusive old value into X1
        self.push_lir(Aarch64Inst::Ldaxr {
            size: op_size,
            addr: MemAddr::Base(Reg::X10),
            dst: Reg::X1,
        });

        // SUB: X2 = X1 (old) - X0 (subtrahend)
        self.push_lir(Aarch64Inst::Sub {
            size: op_size,
            src1: Reg::X1,
            src2: GpOperand::Reg(Reg::X0),
            dst: Reg::X2,
        });

        // STLXR: Try to store X2 (new value), status in W8
        self.push_lir(Aarch64Inst::Stlxr {
            size: op_size,
            src: Reg::X2,
            addr: MemAddr::Base(Reg::X10),
            status: Reg::X8,
        });

        // CBNZ: Retry if store failed
        self.push_lir(Aarch64Inst::Cbnz {
            size: OperandSize::B32,
            src: Reg::X8,
            target: loop_label,
        });

        // Result: X1 = old value
        // The instruction leaves the result in a fixed scratch register, but
        // the allocator gave this pseudo its own location. Overwriting that
        // made every atomic result alias the same register, so two atomic
        // reads in one expression collapsed into one -- `x + y` on two
        // _Atomic ints returned `y + y`. Move it where the allocator expects.
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::X1) {
            self.emit_move_to_loc(Reg::X1, &dst_loc, size.max(32));
        }
    }

    /// Emit atomic fetch-and-AND using LL/SC
    fn emit_atomic_fetch_and(&mut self, insn: &Instruction) {
        self.emit_atomic_fetch_bitop(insn, AtomicBitOp::And);
    }

    /// Emit atomic fetch-and-OR using LL/SC
    fn emit_atomic_fetch_or(&mut self, insn: &Instruction) {
        self.emit_atomic_fetch_bitop(insn, AtomicBitOp::Or);
    }

    /// Emit atomic fetch-and-XOR using LL/SC
    fn emit_atomic_fetch_xor(&mut self, insn: &Instruction) {
        self.emit_atomic_fetch_bitop(insn, AtomicBitOp::Xor);
    }

    /// Helper for atomic fetch bitwise operations (AND, OR, XOR)
    /// Uses LL/SC loop with LDAXR/STLXR
    fn emit_atomic_fetch_bitop(&mut self, insn: &Instruction, op: AtomicBitOp) {
        let target = insn.target.expect("atomic fetch bitop needs target");
        let addr = insn.src[0];
        let value = insn.src[1];
        let size = insn.size;
        let op_size = OperandSize::from_bits(size);

        let addr_loc = self.get_location(addr);
        let value_loc = self.get_location(value);

        // Load pointer into X10 FIRST (before value, in case addr is in X0)
        self.emit_mov_to_reg(addr_loc, Reg::X10, 64);

        // Load operand value into X0
        self.emit_mov_to_reg(value_loc, Reg::X0, size);

        // LL/SC loop
        let loop_label = self.next_unique_label("atomic_bitop");

        // Loop label
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(
            loop_label.clone(),
        )));

        // LDAXR: Load-acquire exclusive old value into X1
        self.push_lir(Aarch64Inst::Ldaxr {
            size: op_size,
            addr: MemAddr::Base(Reg::X10),
            dst: Reg::X1,
        });

        // Apply bitwise operation: X2 = X1 (old) op X0 (operand)
        match op {
            AtomicBitOp::And => {
                self.push_lir(Aarch64Inst::And {
                    size: op_size,
                    src1: Reg::X1,
                    src2: GpOperand::Reg(Reg::X0),
                    dst: Reg::X2,
                });
            }
            AtomicBitOp::Or => {
                self.push_lir(Aarch64Inst::Orr {
                    size: op_size,
                    src1: Reg::X1,
                    src2: GpOperand::Reg(Reg::X0),
                    dst: Reg::X2,
                });
            }
            AtomicBitOp::Xor => {
                self.push_lir(Aarch64Inst::Eor {
                    size: op_size,
                    src1: Reg::X1,
                    src2: GpOperand::Reg(Reg::X0),
                    dst: Reg::X2,
                });
            }
        }

        // STLXR: Try to store X2 (new value), status in W8
        self.push_lir(Aarch64Inst::Stlxr {
            size: op_size,
            src: Reg::X2,
            addr: MemAddr::Base(Reg::X10),
            status: Reg::X8,
        });

        // CBNZ: Retry if store failed
        self.push_lir(Aarch64Inst::Cbnz {
            size: OperandSize::B32,
            src: Reg::X8,
            target: loop_label,
        });

        // Result: X1 = old value
        // The instruction leaves the result in a fixed scratch register, but
        // the allocator gave this pseudo its own location. Overwriting that
        // made every atomic result alias the same register, so two atomic
        // reads in one expression collapsed into one -- `x + y` on two
        // _Atomic ints returned `y + y`. Move it where the allocator expects.
        let dst_loc = self.get_location(target);
        if !matches!(&dst_loc, Loc::Reg(r) if *r == Reg::X1) {
            self.emit_move_to_loc(Reg::X1, &dst_loc, size.max(32));
        }
    }

    /// Emit memory fence
    fn emit_fence(&mut self, insn: &Instruction) {
        use crate::ir::MemoryOrder;

        // Emit appropriate fence based on memory ordering
        match insn.memory_order {
            MemoryOrder::SeqCst | MemoryOrder::AcqRel => {
                // Full barrier
                self.push_lir(Aarch64Inst::Dmb {
                    option: DmbOption::Ish,
                });
            }
            MemoryOrder::Acquire | MemoryOrder::Consume => {
                // Load barrier
                self.push_lir(Aarch64Inst::Dmb {
                    option: DmbOption::Ishld,
                });
            }
            MemoryOrder::Release => {
                // Store barrier
                self.push_lir(Aarch64Inst::Dmb {
                    option: DmbOption::Ishst,
                });
            }
            MemoryOrder::Relaxed => {
                // No fence needed for relaxed
            }
        }

        // Fence has no result value, but set target to 0 if present
        if let Some(target) = insn.target {
            self.locations.set(target, Loc::Imm(0));
        }
    }

    /// Generate a unique label with the given prefix
    pub(super) fn next_unique_label(&mut self, prefix: &str) -> Label {
        let id = self.unique_label_counter;
        self.unique_label_counter += 1;
        Label::new(prefix, id)
    }

    /// Helper to move a value into a register
    fn emit_mov_to_reg(&mut self, loc: Loc, reg: Reg, size: u32) {
        let op_size = OperandSize::from_bits(size);
        match loc {
            Loc::Reg(src) => {
                if src != reg {
                    self.push_lir(Aarch64Inst::Mov {
                        size: op_size,
                        src: GpOperand::Reg(src),
                        dst: reg,
                    });
                }
            }
            Loc::Imm(v) => {
                self.push_lir(Aarch64Inst::Mov {
                    size: op_size,
                    src: GpOperand::Imm(v as i64),
                    dst: reg,
                });
            }
            ref l @ (Loc::Stack(_) | Loc::IncomingArg(_)) => {
                // Address through the frame pointer, as every other path in
                // this backend does ("FP-relative for alloca safety"). This
                // used its own SP-relative arithmetic, which is wrong the
                // moment anything moves SP: the atomic CAS loop allocates its
                // expected-value slot with alloc_local_temp, whose Alloca does
                // exactly that, and the pointer was then read back 16 bytes
                // off -- from the saved LR slot.
                self.push_lir(Aarch64Inst::Ldr {
                    size: op_size,
                    addr: self.loc_mem(l).unwrap(),
                    dst: reg,
                });
            }
            Loc::Global(name) => {
                self.emit_load_global(&name, reg, op_size);
            }
            // A floating-point value has to cross to the general-purpose file
            // as its bit pattern. Falling through to a zero immediate here is
            // what made every _Atomic float/double operation store 0 -- the
            // x86_64 twin of this function was fixed for exactly that and this
            // one was missed.
            Loc::VReg(v) => {
                self.push_lir(Aarch64Inst::FmovToGp {
                    size: if size <= 32 {
                        FpSize::Single
                    } else {
                        FpSize::Double
                    },
                    src: v,
                    dst: reg,
                });
            }
            Loc::FImm(f, imm_size) => {
                let bits = if imm_size == 16 {
                    super::f64_to_f16_bits(f.to_f64()) as i64
                } else if imm_size == 32 {
                    (f.to_f64() as f32).to_bits() as i64
                } else {
                    f.to_f64().to_bits() as i64
                };
                self.emit_mov_imm(reg, bits, 64);
            }
        }
    }
}

// Import shared helper from parent module
use super::f64_to_f16_bits;

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
        // `w`/`x` name a general register's two widths; `b`/`h`/`s`/`d`/`q`
        // name a vector register's five. Without the vector set a `"w"`
        // operand could not be referenced at all -- `fsqrt %d0, %d1` reached
        // the assembler with the `%d0` intact -- so there was no way to write
        // a working vector asm.
        //
        // A vector operand is pre-rendered at its own type's width, and the
        // substitution emits that text, so the modifier selects the operand
        // rather than re-widening it. That agrees with the width the modifier
        // names in every case where the two are consistent, which is what
        // real code writes.
        &['w', 'x', 'b', 'h', 's', 'd', 'q']
    }

    fn format_reg_sized(&self, reg: Reg, size_mod: char) -> String {
        // AArch64 doesn't use % prefix for register names
        match size_mod {
            'w' => asm_reg_name_32(reg).to_string(),
            _ => asm_reg_name_64(reg).to_string(),
        }
    }

    fn format_reg_default(&self, reg: Reg, size_bits: u32) -> String {
        // Select register width matching the operand's declared size
        if size_bits <= 32 {
            asm_reg_name_32(reg).to_string()
        } else {
            asm_reg_name_64(reg).to_string()
        }
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

        // Collect thread-local storage symbols (both defined and extern)
        self.tls_symbols = module
            .globals
            .iter()
            .filter(|g| g.is_thread_local)
            .map(|g| g.name.clone())
            .chain(module.extern_tls_symbols.iter().cloned())
            .collect();

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
        for global in &module.globals {
            self.emit_global(global, types);
        }

        self.base.emit_declared_symbol_attrs(module);

        // Emit string literals
        if !module.strings.is_empty() {
            self.base.emit_strings(&module.strings);
        }

        // Emit wide string literals
        if !module.wide_strings.is_empty() {
            self.base.emit_wide_strings(&module.wide_strings);
        }

        // Emit char16_t / char32_t string literals
        if !module.utf16_strings.is_empty() {
            self.base.emit_utf16_strings(&module.utf16_strings);
        }
        if !module.utf32_strings.is_empty() {
            self.base.emit_utf32_strings(&module.utf32_strings);
        }

        // Emit text start label for DWARF debug info (before first function)
        // Must be in .text section — emit .text first since globals may leave us in .data
        if module.debug && !module.functions.is_empty() {
            self.push_lir(Aarch64Inst::Directive(Directive::Text));
            self.base.push_directive(Directive::local_label(".Ltext0"));
        }

        // Emit functions
        for func in &module.functions {
            // An inline definition is kept in the module so the inliner can
            // use it, but provides no external definition -- see `Function::emit`.
            if !func.emit {
                continue;
            }
            self.emit_function(func, types);
        }

        // Emit text end label for DWARF debug info (after last function)
        if module.debug && !module.functions.is_empty() {
            self.base
                .push_directive(Directive::local_label(".Ltext_end"));
        }

        // Emit the constructor / destructor pointer arrays
        self.base.emit_init_arrays(&module.functions);

        // Generate DWARF debug sections if debug mode is enabled
        if module.debug {
            let producer = format!("c17 {}", env!("CARGO_PKG_VERSION"));
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

    fn set_shared_mode(&mut self, shared: bool) {
        self.base.shared_mode = shared;
    }

    fn set_verbose_asm(&mut self, verbose: bool) {
        self.base.verbose_asm = verbose;
    }
}

/// Helper enum for atomic bitwise operations
#[derive(Clone, Copy)]
enum AtomicBitOp {
    And,
    Or,
    Xor,
}
