//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 function entry and exit: prologue, epilogue, frame layout,
// callee-saved registers, incoming-argument marshalling and return-value
// lowering
//

use crate::abi::{get_abi_for_conv, ArgClass, CallingConv};
use crate::arch::aarch64::codegen::Aarch64CodeGen;
use crate::arch::aarch64::features::{VA_GR_SAVE_BYTES, VA_VR_SAVE_BYTES};
use crate::arch::aarch64::lir::{Aarch64Inst, GpOperand, MemAddr};
use crate::arch::aarch64::regalloc::{FrameBase, Loc, Reg, RegAlloc, VReg};
use crate::arch::codegen::is_variadic_function;
use crate::arch::lir::{complex_fp_info, Directive, FpSize, OperandSize, Symbol};
use crate::ir::{Function, Instruction, PseudoId, PseudoKind};
use crate::types::{TypeId, TypeKind, TypeTable};
use std::collections::HashSet;

impl Aarch64CodeGen {
    pub(super) fn emit_function(&mut self, func: &Function, types: &TypeTable) {
        // Check if this function uses varargs
        let is_variadic = is_variadic_function(func);

        // Register allocation
        let mut alloc = RegAlloc::new();
        self.locations = alloc.allocate(func, types);
        self.pseudos = func.pseudos.clone();

        // Build sym type size map for emit_store to distinguish struct fields from scalars
        self.sym_type_sizes.clear();
        for pseudo in &func.pseudos {
            // By identity: a global whose name collides with a parameter's
            // would otherwise be recorded with the parameter's type size.
            if let Some(local_var) = func.local_of(pseudo.id) {
                self.sym_type_sizes
                    .insert(pseudo.id, types.size_bits(local_var.typ));
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
        // bytes each = 128).
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

        // The label `DW_AT_high_pc` names, before `.size` so it marks the end
        // of the code rather than a point after the directive.
        if self.base.emit_debug {
            let end = format!(".Lfunc_end_{}", self.base.current_fn);
            self.push_lir(Aarch64Inst::Directive(Directive::local_label(&end)));
            self.collect_fn_die(func, types, end);
        }

        // `.size f, .-f`: without it the symbol records st_size = 0 and a
        // debugger cannot tell which function owns an address inside it.
        self.push_lir(Aarch64Inst::Directive(Directive::size_to_here(&func.name)));
    }

    /// Record what `-g` has to say about this function.
    ///
    /// Only here can it be said: a variable's home is the register allocator's
    /// answer for *this* function, and `self.locations` is overwritten by the
    /// next one. A local with no stack slot -- promoted to a register, or gone
    /// entirely -- gets no location rather than a made-up one.
    fn collect_fn_die(&mut self, func: &Function, types: &TypeTable, end_label: String) {
        let params: std::collections::HashSet<&str> =
            func.params.iter().map(|(n, _)| n.as_str()).collect();
        let mut names: Vec<&String> = func.locals.keys().collect();
        names.sort();

        let mut vars = Vec::new();
        for name in names {
            let local = &func.locals[name];
            // Compiler-introduced storage has no name a debugger should show.
            if name.starts_with("__") || name.starts_with('.') {
                continue;
            }
            let loc = self
                .locations
                .get(local.sym)
                .and_then(|l| self.loc_addr_parts(&l))
                .map(|(reg, off)| crate::arch::dwarf::VarLocation {
                    reg: reg.dwarf_number(),
                    offset: off as i64,
                });
            vars.push(crate::arch::dwarf::VarDie {
                name: name.clone(),
                typ: local.typ,
                decl_line: 0,
                is_param: params.contains(name.as_str()),
                loc,
            });
        }

        let ret_typ = (types.kind(func.return_type) != TypeKind::Void).then_some(func.return_type);
        let decl_line = func
            .blocks
            .iter()
            .flat_map(|b| b.insns.iter())
            .find_map(|i| i.pos.as_ref())
            .map(|p| p.line)
            .unwrap_or(0);

        self.base.fn_dies.push(crate::arch::dwarf::FnDie {
            name: func.name.clone(),
            external: !func.is_static,
            decl_line,
            end_label,
            ret_typ,
            vars,
        });
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

        // The line the prologue belongs to, before the procedure starts --
        // otherwise the function's entry address has no row in the line table
        // and a debugger cannot place a breakpoint on the function at all.
        self.base.emit_function_entry_loc(func);

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

    /// stp/ldp signed-7-bit-scaled immediate range for `size`.
    /// Returns `(min, max, step)` in bytes.
    fn pair_offset_range(size: OperandSize) -> (i32, i32, i32) {
        match size {
            OperandSize::B32 => (-256, 252, 4),
            _ => (-512, 504, 8),
        }
    }

    /// True if `offset` fits the stp/ldp encoding for `size`.
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
            // below uses.
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
                                // reads. regalloc has already assigned the
                                // incoming slot; find it and shuttle both
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

    /// Emit return instruction: move return value and emit epilogue
    pub(super) fn emit_ret(
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
}
