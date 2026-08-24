//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 function entry and exit: prologue, epilogue, frame layout,
// incoming-argument marshalling and return-value lowering
//

use crate::abi::{get_abi, Abi, ArgClass, RegClass};
use crate::arch::codegen::is_variadic_function;
use crate::arch::lir::{complex_fp_info, complex_sse_regs, Directive, FpSize, OperandSize, Symbol};
use crate::arch::x86_64::codegen::X86_64CodeGen;
use crate::arch::x86_64::lir::{GpOperand, MemAddr, X86Inst, XmmOperand};
use crate::arch::x86_64::regalloc::{FrameBase, Loc, Reg, RegAlloc, XmmReg};
use crate::ir::{Function, Instruction, PseudoId, PseudoKind};
use crate::types::{TypeId, TypeKind, TypeTable};
use std::collections::HashSet;

impl X86_64CodeGen {
    /// Store a register-pair struct parameter into its local.
    ///
    /// Two eightbytes arrive in two registers -- both general, or one of each,
    /// in the order the ABI class vector gives. The parameter's local is the
    /// storage the body reads, so the prologue writes them there; the
    /// linearizer deliberately emits no copy for these.
    #[allow(clippy::too_many_arguments)]
    fn store_reg_pair_param_to_local(
        &mut self,
        func: &Function,
        param_idx: usize,
        pseudo: PseudoId,
        classes: &[crate::abi::RegClass],
        int_arg_idx: &mut usize,
        fp_arg_idx: &mut usize,
        int_arg_regs: &[Reg],
        fp_arg_regs: &[XmmReg],
        type_size_bits: u32,
    ) {
        let gp_needed = classes
            .iter()
            .filter(|c| **c != crate::abi::RegClass::Sse)
            .count();
        let sse_needed = classes.len() - gp_needed;
        // Out of registers: the caller left the bytes in the incoming argument
        // area instead, so copy them from there.
        if *int_arg_idx + gp_needed > int_arg_regs.len()
            || *fp_arg_idx + sse_needed > fp_arg_regs.len()
        {
            self.copy_incoming_arg_to_local(
                func,
                &func.params[param_idx].0,
                pseudo,
                (type_size_bits / 8) as i32,
            );
            return;
        }

        // Spend the registers first. The argument occupies them whether or
        // not there is anywhere left to put it: at -O an unread parameter has
        // no local, and returning here without charging for it made the next
        // floating-point argument read the register this one arrived in.
        let pair_start_int = *int_arg_idx;
        let pair_start_fp = *fp_arg_idx;
        *int_arg_idx += gp_needed;
        *fp_arg_idx += sse_needed;

        let param_name = &func.params[param_idx].0;
        let Some(local) = func.locals.get(param_name) else {
            return;
        };
        let Some(&Loc::Stack(offset)) = self.locations.get_ref(local.sym) else {
            return;
        };

        let mut next_int = pair_start_int;
        let mut next_fp = pair_start_fp;
        for (i, class) in classes.iter().enumerate() {
            let delta = (i * 8) as i32;
            if *class == crate::abi::RegClass::Sse {
                let src = fp_arg_regs[next_fp];
                next_fp += 1;
                self.push_lir(X86Inst::MovFp {
                    size: FpSize::Double,
                    src: XmmOperand::Reg(src),
                    dst: XmmOperand::Mem(self.stack_mem(offset - delta)),
                });
            } else {
                let src = int_arg_regs[next_int];
                next_int += 1;
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(src),
                    dst: GpOperand::Mem(self.stack_mem(offset - delta)),
                });
            }
        }
    }

    fn copy_incoming_arg_to_local(
        &mut self,
        func: &crate::ir::Function,
        param_name: &str,
        arg_pseudo: crate::ir::PseudoId,
        bytes: i32,
    ) {
        let Some(Loc::IncomingArg(src_offset)) = self.locations.get(arg_pseudo) else {
            return;
        };
        let Some(local) = func.locals.get(param_name) else {
            return;
        };
        let Some(Loc::Stack(dst_offset)) = self.locations.get_ref(local.sym) else {
            return;
        };
        let dst_offset = *dst_offset;
        let mut copied = 0;
        while copied < bytes {
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: src_offset + copied,
                }),
                dst: GpOperand::Reg(Reg::R10),
            });
            // Locals grow downward from `dst_offset`, so later bytes sit at a
            // smaller offset — the same convention `stack_mem` encodes.
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::R10),
                dst: GpOperand::Mem(self.stack_mem(dst_offset - copied)),
            });
            copied += 8;
        }
    }

    pub(super) fn emit_function(&mut self, func: &Function, types: &TypeTable) {
        // Save current function name for unique label generation
        // Local labels are derived from this and are compiler-internal, so
        // they take the plain name: a verbatim asm-label marker belongs only
        // on the symbol the assembler is asked for.
        self.base.current_fn = crate::arch::lir::undecorated(&func.name).to_string();

        // Check if this function uses varargs
        let is_variadic = is_variadic_function(func);

        // Register allocation
        let mut alloc = RegAlloc::new();
        self.locations = alloc.allocate(func, types);
        self.int128_pseudos = alloc.int128_pseudos().clone();
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
        self.callee_saved_regs = alloc.callee_saved_used().to_vec();
        self.max_local_align = alloc.max_local_align();
        self.frame_base = alloc.frame_base();
        // Pad callee_saved_offset to multiple of 16 so that 16-byte-aligned
        // stack_offset values produce 16-byte-aligned final addresses.
        // rbp is 16-aligned (ABI), so -(padded_offset + aligned_stack_offset) is also aligned.
        self.callee_saved_offset = ((self.callee_saved_regs.len() as i32 * 8) + 15) & !15;

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
        self.emit_function_header(func);

        // Emit prologue (push rbp, callee-saved regs, allocate stack)
        self.emit_prologue(stack_size, reg_save_area_size);

        // Zero-initialize the stack frame BEFORE storing any arguments.
        // This ensures all 8-byte stack slots start as zero, so narrow
        // writes (8/16/32-bit) leave zero in the unwritten upper bytes.
        // Uses R10/R11 to save/restore RDI/RCX (which may hold arguments).
        self.zero_stack_frame();

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

        // The label `DW_AT_high_pc` names, before `.size` so it marks the end
        // of the code rather than a point after the directive.
        if self.base.emit_debug {
            let end = format!(".Lfunc_end_{}", self.base.current_fn);
            self.push_lir(X86Inst::Directive(Directive::local_label(&end)));
            self.collect_fn_die(func, types, end);
        }

        // `.size f, .-f`: without it the symbol records st_size = 0 and a
        // debugger cannot tell which function owns an address inside it.
        self.push_lir(X86Inst::Directive(Directive::size_to_here(&func.name)));
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
            let loc = match self.locations.get_ref(local.sym) {
                Some(Loc::Stack(off)) => match self.stack_mem(*off) {
                    MemAddr::BaseOffset { base, offset } => Some(crate::arch::dwarf::VarLocation {
                        reg: base.dwarf_number(),
                        offset: offset as i64,
                    }),
                    _ => None,
                },
                _ => None,
            };
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

    /// Emit function header directives (text section, visibility, type, label, CFI start)
    fn emit_function_header(&mut self, func: &Function) {
        let (is_static, name) = (func.is_static, func.name.as_str());
        self.push_lir(X86Inst::Directive(Directive::Blank));
        self.push_lir(X86Inst::Directive(Directive::Text));

        // A named section replaces .text for this function.
        if let Some(sec) = &func.symbol_attrs.section {
            self.push_lir(X86Inst::Directive(Directive::NamedSection {
                name: sec.clone(),
                executable: true,
                writable: false,
            }));
        }

        // Skip .globl for static functions (internal linkage)
        if !is_static {
            if func.symbol_attrs.weak {
                self.push_lir(X86Inst::Directive(Directive::Weak(
                    Symbol::global(name),
                    crate::arch::lir::WeakKind::Definition,
                )));
            } else {
                self.push_lir(X86Inst::Directive(Directive::global(name)));
            }
        }
        if let Some(how) = &func.symbol_attrs.visibility {
            self.push_lir(X86Inst::Directive(Directive::Visibility(
                Symbol::global(name),
                how.clone(),
            )));
        }

        // ELF-only type (handled by Directive::emit which skips on macOS)
        self.push_lir(X86Inst::Directive(Directive::type_func(name)));

        // Function label
        self.push_lir(X86Inst::Directive(Directive::global_label(name)));

        // The line the prologue belongs to, before the procedure starts --
        // otherwise the function's entry address has no row in the line table
        // and a debugger cannot place a breakpoint on the function at all.
        self.base.emit_function_entry_loc(func);

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

        // Allocate stack space for locals + callee-saved padding + register save area
        // callee_saved_offset is already padded to 16; use it instead of raw pushed bytes
        let total_stack = stack_size + self.callee_saved_offset + reg_save_area_size;
        // Ensure 16-byte alignment
        let aligned_stack = (total_stack + 15) & !15;
        // Subtract only the actual pushed bytes (not the padded offset)
        let alloc_size = aligned_stack - (self.callee_saved_regs.len() as i32 * 8);
        // An over-aligned frame addresses each local as `%rsp + (alloc_size -
        // slot)`. The `andq` below makes `%rsp` itself a multiple of the
        // alignment and the allocator makes each slot one, so the address is
        // aligned only if `alloc_size` is too -- and it is a 16-byte-rounded
        // total less eight bytes per pushed register, so an odd number of
        // callee-saved pushes left every over-aligned local off by eight.
        let alloc_size = if let FrameBase::Aligned { align, .. } = self.frame_base {
            (alloc_size + align - 1) & !(align - 1)
        } else {
            alloc_size
        };
        if alloc_size > 0 {
            self.push_lir(X86Inst::Sub {
                size: OperandSize::B64,
                src: GpOperand::Imm(alloc_size as i64),
                dst: Reg::Rsp,
            });
        }

        // Dynamic stack alignment for locals with alignment > 16.
        // After sub, emit andq to force RSP to the required alignment, then
        // latch that value into the frame base register.
        //
        // The latch is the point. `%rsp` holds the right address only until
        // something moves it -- `alloca`, which the inliner can splice into a
        // caller that has none -- and every local was measured from it. The
        // base register is written once here and read for the rest of the
        // function, so a moving `%rsp` no longer reaches the locals. The
        // epilogue restores `%rsp` from `%rbp` and pops the base back, so
        // nothing else has to know.
        if let FrameBase::Aligned { reg, align } = self.frame_base {
            self.push_lir(X86Inst::And {
                size: OperandSize::B64,
                src: GpOperand::Imm(-(align as i64)),
                dst: Reg::Rsp,
            });
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(Reg::Rsp),
                dst: GpOperand::Reg(reg),
            });
        }

        // Store alloc_size for use in zero_stack_frame()
        self.stack_alloc_size = alloc_size;
    }

    /// Zero-initialize the stack frame AFTER argument registers have been
    /// spilled to their stack slots. This prevents stale bytes from being
    /// read when narrow values (8/16/32-bit) are stored to 8-byte stack
    /// slots and later loaded at wider widths.
    ///
    /// Must be called after store_spilled_args() and emit_variadic_save_area()
    /// because rep stosq clobbers RAX, RCX, RDI.
    fn zero_stack_frame(&mut self) {
        let alloc_size = self.stack_alloc_size;
        if alloc_size <= 0 {
            return;
        }
        let qwords = alloc_size / 8;
        if qwords <= 0 {
            return;
        }
        // Save RDI and RCX to scratch registers R10/R11 — they may hold
        // function arguments (SysV ABI: RDI=arg0, RCX=arg3).
        // R10 and R11 are caller-saved scratch, NOT used for arg passing.
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rdi),
            dst: GpOperand::Reg(Reg::R10),
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rcx),
            dst: GpOperand::Reg(Reg::R11),
        });
        // RDI = RSP (start of stack frame to zero)
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::Rsp),
            dst: GpOperand::Reg(Reg::Rdi),
        });
        // RCX = number of qwords
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Imm(qwords as i64),
            dst: GpOperand::Reg(Reg::Rcx),
        });
        // RAX = 0 (value to fill)
        self.push_lir(X86Inst::Xor {
            size: OperandSize::B32,
            src: GpOperand::Reg(Reg::Rax),
            dst: Reg::Rax,
        });
        // rep stosq: zero [RDI] for RCX qwords
        self.push_lir(X86Inst::RepStosq);
        // Restore RDI and RCX
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R10),
            dst: GpOperand::Reg(Reg::Rdi),
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst: GpOperand::Reg(Reg::Rcx),
        });
    }

    /// Emit stores for arguments spilled from caller-saved registers to stack
    fn store_spilled_args(&mut self, alloc: &RegAlloc) {
        for spilled in alloc.spilled_args() {
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Reg(spilled.from_reg),
                dst: GpOperand::Mem(self.stack_mem(spilled.to_stack_offset)),
            });
        }
        // Spill XMM function parameters to stack
        for spilled in alloc.spilled_xmm_args() {
            self.push_lir(X86Inst::MovFp {
                size: spilled.size,
                src: XmmOperand::Reg(spilled.from_xmm),
                dst: XmmOperand::Mem(self.stack_mem(spilled.to_stack_offset)),
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

    /// Charge one parameter against the argument registers, emitting nothing.
    ///
    /// Which register an argument arrives in is a property of the *signature*:
    /// every parameter ahead of it spends its registers whether or not the
    /// body ever reads it. So this has to be applied to a parameter the
    /// prologue has no store to make for -- one spilled elsewhere, or one
    /// optimised away entirely -- or every floating-point argument behind it
    /// is read from a register one too low.
    fn advance_arg_regs(
        typ: TypeId,
        types: &TypeTable,
        int_arg_idx: &mut usize,
        fp_arg_idx: &mut usize,
        int_arg_reg_count: usize,
    ) {
        let is_complex = types.is_complex(typ);
        let kind = types.kind(typ);
        let is_aggregate = !is_complex
            && (kind == crate::types::TypeKind::Struct || kind == crate::types::TypeKind::Union);

        if is_complex {
            *fp_arg_idx += complex_sse_regs(types, typ);
        } else if kind == TypeKind::Int128 {
            // Only when it actually took the pair: 3.2.3 step 5 leaves the
            // registers it did not fit in available to later arguments.
            if *int_arg_idx + 1 < int_arg_reg_count {
                *int_arg_idx += 2;
            }
        } else if is_aggregate {
            // The class, not the size. Asking whether the aggregate was
            // larger than eight bytes sent `struct { float a, b; }` -- one
            // whole XMM by its class -- to the general-register tally.
            let abi = crate::abi::SysVAmd64Abi;
            match abi.classify_param(typ, types) {
                crate::abi::ArgClass::Direct { ref classes, .. } => {
                    *fp_arg_idx += classes
                        .iter()
                        .filter(|c| **c == crate::abi::RegClass::Sse)
                        .count();
                    *int_arg_idx += classes
                        .iter()
                        .filter(|c| **c == crate::abi::RegClass::Integer)
                        .count();
                }
                _ => *int_arg_idx += 1,
            }
        } else if kind == crate::types::TypeKind::LongDouble {
            // Passed in memory; spends no XMM register.
        } else if types.is_float(typ) {
            *fp_arg_idx += 1;
        } else {
            *int_arg_idx += 1;
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
        // Track incoming stack arg position (after saved RBP + return addr = 16)

        // Track which pseudos were already spilled via spill_args_across_calls
        // to avoid double-storing them here
        let spilled_pseudos: HashSet<PseudoId> = alloc
            .spilled_args()
            .iter()
            .map(|s| s.pseudo)
            .chain(alloc.spilled_xmm_args().iter().map(|s| s.pseudo))
            .collect();

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
            // An unread parameter still spends its registers. At -O its pseudo
            // is gone, and walking only the surviving pseudos charged nothing
            // for it, so `g(MIX unused, D2 f)` read `f` from the pair the
            // dead argument was still occupying.
            if crate::abi::param_is_memory_class(*typ, types) {
                continue;
            }
            let has_pseudo = func
                .pseudos
                .iter()
                .any(|p| matches!(p.kind, PseudoKind::Arg(a) if a == (i as u32) + arg_idx_offset));
            if !has_pseudo {
                Self::advance_arg_regs(
                    *typ,
                    types,
                    &mut int_arg_idx,
                    &mut fp_arg_idx,
                    int_arg_regs.len(),
                );
                continue;
            }
            // Find the pseudo for this argument
            for pseudo in &func.pseudos {
                if let PseudoKind::Arg(arg_idx) = pseudo.kind {
                    if arg_idx == (i as u32) + arg_idx_offset {
                        // A MEMORY-class struct arrives on the stack by
                        // value and uses no GP register — skip it entirely.
                        let type_size_bits = types.size_bits(*typ);
                        let is_large_struct_param = crate::abi::param_is_memory_class(*typ, types);
                        if is_large_struct_param {
                            break;
                        }

                        // Skip pseudos already stored via spilled_args
                        if spilled_pseudos.contains(&pseudo.id) {
                            // Still need to count this arg for register
                            // assignment tracking.
                            Self::advance_arg_regs(
                                *typ,
                                types,
                                &mut int_arg_idx,
                                &mut fp_arg_idx,
                                int_arg_regs.len(),
                            );
                            break;
                        }
                        let is_fp = types.is_float(*typ);
                        let is_complex = types.is_complex(*typ);
                        // An all-SSE struct, and the number of registers it
                        // takes: two doubles take two, a lone binary128 takes
                        // one whole register for all sixteen bytes.
                        let sse_struct = crate::abi::sse_struct_regs(*typ, types);
                        let is_two_sse_struct = sse_struct.is_some();
                        // How many SSE registers this argument actually occupies.
                        // A complex type depends on its base (see
                        // `complex_sse_regs`).
                        let sse_regs = if let Some(n) = sse_struct {
                            n
                        } else if is_complex {
                            complex_sse_regs(types, *typ)
                        } else {
                            0
                        };

                        // Two eightbytes in two registers, at least one of
                        // them general. The all-SSE shapes are handled by the
                        // block below; this is the integer and mixed remainder.
                        if let Some(classes) = crate::abi::struct_param_classes(*typ, types) {
                            if !is_two_sse_struct {
                                self.store_reg_pair_param_to_local(
                                    func,
                                    i,
                                    pseudo.id,
                                    &classes,
                                    &mut int_arg_idx,
                                    &mut fp_arg_idx,
                                    int_arg_regs,
                                    fp_arg_regs,
                                    type_size_bits,
                                );
                                break;
                            }
                        }

                        if (is_complex || is_two_sse_struct) && sse_regs > 0 {
                            // Look up the local variable (same name as param) for stack location
                            if fp_arg_idx + sse_regs > fp_arg_regs.len() {
                                // Spilled: the caller wrote the value into the
                                // incoming argument area, so copy it into the
                                // local rather than reading registers that hold
                                // something else.
                                self.copy_incoming_arg_to_local(
                                    func,
                                    &func.params[i].0,
                                    pseudo.id,
                                    (type_size_bits / 8) as i32,
                                );
                                // A memory-class argument consumes no XMM
                                // registers; the ones it did not fit in stay
                                // available to the arguments that follow.
                                break;
                            }
                            {
                                // Find the local for this parameter by name
                                let param_name = &func.params[i].0;
                                if let Some(local) = func.locals.get(param_name) {
                                    if let Some(Loc::Stack(offset)) =
                                        self.locations.get_ref(local.sym)
                                    {
                                        let offset = *offset;
                                        let (fp_size, imag_offset) = if let Some(n) = sse_struct {
                                            // Two doubles are eight bytes each;
                                            // a lone binary128 is one register
                                            // holding all sixteen.
                                            if n == 1 {
                                                (FpSize::for_sse_aggregate(type_size_bits), 0)
                                            } else {
                                                (FpSize::Double, 8)
                                            }
                                        } else {
                                            complex_fp_info(types, &self.base.target, *typ)
                                        };
                                        if sse_regs == 1 {
                                            // One register holding the whole
                                            // value. For `float _Complex` that
                                            // is one eightbyte with both
                                            // halves in it, so a 64-bit store
                                            // writes all of it; for an
                                            // aggregate it is whatever the
                                            // class's size says, which is
                                            // sixteen bytes for a binary128.
                                            let whole = if sse_struct.is_some() {
                                                fp_size
                                            } else {
                                                FpSize::Double
                                            };
                                            self.push_lir(X86Inst::MovFp {
                                                size: whole,
                                                src: XmmOperand::Reg(fp_arg_regs[fp_arg_idx]),
                                                dst: XmmOperand::Mem(self.stack_mem(offset)),
                                            });
                                        } else {
                                            // Store real part from first XMM register
                                            self.push_lir(X86Inst::MovFp {
                                                size: fp_size,
                                                src: XmmOperand::Reg(fp_arg_regs[fp_arg_idx]),
                                                dst: XmmOperand::Mem(self.stack_mem(offset)),
                                            });
                                            // Store imag part from second XMM register
                                            self.push_lir(X86Inst::MovFp {
                                                size: fp_size,
                                                src: XmmOperand::Reg(fp_arg_regs[fp_arg_idx + 1]),
                                                dst: XmmOperand::Mem(
                                                    self.stack_mem(offset - imag_offset),
                                                ),
                                            });
                                        }
                                    }
                                }
                            }
                            fp_arg_idx += sse_regs;
                        } else if types.kind(*typ) == crate::types::TypeKind::LongDouble {
                            // Long double is passed on the stack per System V AMD64 ABI
                            // No XMM register move needed - already at IncomingArg offset
                            // Don't increment fp_arg_idx - long double doesn't use XMM
                        } else if is_fp {
                            // FP argument (float/double)
                            if fp_arg_idx < fp_arg_regs.len() {
                                if let Some(Loc::Stack(offset)) = self.locations.get_ref(pseudo.id)
                                {
                                    // Move from FP arg register to stack
                                    // From the type: a `__float128` argument
                                    // arrives as a whole XMM, and storing it
                                    // as a `double` dropped its top half.
                                    let fp_size =
                                        self.fp_format(Some(*typ), types.size_bits(*typ), types);
                                    self.push_lir(X86Inst::MovFp {
                                        size: fp_size,
                                        src: XmmOperand::Reg(fp_arg_regs[fp_arg_idx]),
                                        dst: XmmOperand::Mem(self.stack_mem(*offset)),
                                    });
                                }
                            }
                            fp_arg_idx += 1;
                        } else if types.kind(*typ) == TypeKind::Int128 {
                            // __int128 argument — uses TWO consecutive GP registers
                            // Store to the arg pseudo's stack slot (allocated by regalloc)
                            let int128_in_regs = int_arg_idx + 1 < int_arg_regs.len();
                            let pair_start = int_arg_idx;
                            if int128_in_regs {
                                int_arg_idx += 2;
                                if let Some(loc) = self.locations.get(pseudo.id) {
                                    // Store lo half from first GP register
                                    self.push_lir(X86Inst::Mov {
                                        size: OperandSize::B64,
                                        src: GpOperand::Reg(int_arg_regs[pair_start]),
                                        dst: GpOperand::Mem(self.int128_lo_mem_loc(&loc)),
                                    });
                                    // Store hi half from second GP register
                                    self.push_lir(X86Inst::Mov {
                                        size: OperandSize::B64,
                                        src: GpOperand::Reg(int_arg_regs[pair_start + 1]),
                                        dst: GpOperand::Mem(self.int128_hi_mem_loc(&loc)),
                                    });
                                }
                            } else {
                                // Stack-passed int128: copy from incoming arg area
                                // to the local stack slot. The allocator laid the
                                // incoming area out and recorded where this one
                                // landed; recomputing it here with a second
                                // counter -- which only this arm advanced -- read
                                // the preceding argument as soon as anything else
                                // was stacked.
                                let incoming_stack_offset = alloc
                                    .int128_incoming(pseudo.id)
                                    .expect("stack-passed __int128 has an incoming offset");
                                if let Some(loc) = self.locations.get(pseudo.id) {
                                    // Load lo from incoming, store to local
                                    self.push_lir(X86Inst::Mov {
                                        size: OperandSize::B64,
                                        src: GpOperand::Mem(MemAddr::BaseOffset {
                                            base: Reg::Rbp,
                                            offset: incoming_stack_offset,
                                        }),
                                        dst: GpOperand::Reg(Reg::R10),
                                    });
                                    self.push_lir(X86Inst::Mov {
                                        size: OperandSize::B64,
                                        src: GpOperand::Reg(Reg::R10),
                                        dst: GpOperand::Mem(self.int128_lo_mem_loc(&loc)),
                                    });
                                    // Load hi from incoming, store to local
                                    self.push_lir(X86Inst::Mov {
                                        size: OperandSize::B64,
                                        src: GpOperand::Mem(MemAddr::BaseOffset {
                                            base: Reg::Rbp,
                                            offset: incoming_stack_offset + 8,
                                        }),
                                        dst: GpOperand::Reg(Reg::R10),
                                    });
                                    self.push_lir(X86Inst::Mov {
                                        size: OperandSize::B64,
                                        src: GpOperand::Reg(Reg::R10),
                                        dst: GpOperand::Mem(self.int128_hi_mem_loc(&loc)),
                                    });
                                }
                                // No advance here: 3.2.3 step 5 sends an
                                // argument that does not fit to memory *whole*,
                                // consuming no registers, so the ones it did
                                // not fit in remain for later arguments. The
                                // allocator applies the same rule, and the two
                                // have to agree on which register a following
                                // argument arrives in.
                            }
                        } else {
                            // Integer argument
                            if int_arg_idx < int_arg_regs.len() {
                                if let Some(Loc::Stack(offset)) = self.locations.get_ref(pseudo.id)
                                {
                                    // Move from arg register to stack
                                    self.push_lir(X86Inst::Mov {
                                        size: OperandSize::B64,
                                        src: GpOperand::Reg(int_arg_regs[int_arg_idx]),
                                        dst: GpOperand::Mem(self.stack_mem(*offset)),
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

        // A struct that arrives in two registers spends two, not one -- and a
        // mixed one spends a general register *and* an SSE register. Counting
        // it as a single GP parameter put `va_start`'s register-save-area index
        // one slot out, so the first variadic argument was read from the wrong
        // place.
        let mut gp = 0usize;
        let mut fp = 0usize;
        for (_, typ) in &func.params {
            let kind = types.kind(*typ);
            if let Some(classes) = crate::abi::struct_param_classes(*typ, types) {
                // Nine to sixteen bytes: one register per eightbyte, from
                // whichever file its class names.
                for class in &classes {
                    if *class == crate::abi::RegClass::Sse {
                        fp += 1;
                    } else {
                        gp += 1;
                    }
                }
            } else if let Some(n) = crate::abi::sse_struct_regs(*typ, types) {
                // An all-SSE aggregate: `n` XMM registers. Both the eight-byte
                // and smaller shapes and the sixteen-byte SSE+SSEUP one -- a
                // lone `__float128` -- land here, the latter because it answers
                // a single class rather than the pair `struct_param_classes`
                // above looks for.
                fp += n;
            } else if crate::abi::param_is_memory_class(*typ, types) {
                // MEMORY class travels on the stack and spends no register.
            } else if types.is_complex(*typ) {
                // Two XMMs for `double _Complex`, one for `float _Complex`
                // (both halves in one eightbyte), none for
                // `long double _Complex`, which is COMPLEX_X87 and is passed
                // in memory.
                fp += complex_sse_regs(types, *typ);
            } else if kind == TypeKind::Int128 {
                gp += 2;
            } else if types.is_float(*typ) {
                if kind != TypeKind::LongDouble {
                    fp += 1;
                }
            } else {
                gp += 1;
            }
        }
        self.num_fixed_gp_params = gp;
        if has_sret {
            self.num_fixed_gp_params += 1; // Account for hidden sret pointer
        }
        self.num_fixed_fp_params = fp;

        // Count fixed params that overflow to the stack (beyond register capacity)
        let gp_overflow = self.num_fixed_gp_params.saturating_sub(6);
        let fp_overflow = self.num_fixed_fp_params.saturating_sub(8);
        self.num_fixed_stack_params = gp_overflow + fp_overflow;
    }

    /// Emit return instruction: move return value to registers and emit epilogue
    pub(super) fn emit_ret(&mut self, insn: &Instruction, types: &TypeTable) {
        // Move return value to appropriate register if present
        // System V AMD64 ABI: integers in RAX, floats in XMM0, complex in XMM0+XMM1
        // Struct returns depend on ABI classification (SSE for all-float structs)
        if let Some(src) = insn.src.first() {
            let src_loc = self.get_location(*src);
            // `long double _Complex` is COMPLEX_X87 and returns through the
            // hidden pointer, not in XMM registers — it has no XMM form, and
            // trying to give it one is what emitted `movt %xmm0`.
            let is_complex = insn.typ.is_some_and(|t| {
                types.is_complex(t) && crate::arch::lir::complex_sse_regs(types, t) > 0
            });
            // `long double _Complex` is COMPLEX_X87: st(0) and st(1), never
            // XMM. It needs its own arm — `is_float` deliberately excludes
            // complex types, so it never reaches the FP path below.
            let is_complex_x87 = insn.typ.is_some_and(|t| {
                types.is_complex(t) && crate::arch::lir::complex_sse_regs(types, t) == 0
            });
            let is_fp = matches!(src_loc, Loc::Xmm(_) | Loc::FImm(..))
                || insn.typ.is_some_and(|t| types.is_float(t));

            // Check if return type is a struct/union
            let ret_typ = insn.typ;
            let is_struct_or_union = ret_typ.is_some_and(|t| {
                let kind = types.kind(t);
                kind == TypeKind::Struct || kind == TypeKind::Union
            });

            // Derive return size from type to avoid 32-bit truncation
            let ret_size = ret_typ
                .map(|t| types.size_bits(t).max(32))
                .unwrap_or(insn.size.max(32));

            let one_sse_ret = insn.abi_info.as_ref().is_some_and(|ai| {
                matches!(&ai.ret, ArgClass::Direct { classes, .. }
                         if classes.len() == 1 && classes[0] == RegClass::Sse)
            });
            if one_sse_ret && is_struct_or_union && !is_complex {
                // One SSE register holding the whole aggregate. The `Ret`
                // carries its address, so move every byte at once: a lone
                // binary128 is SSE+SSEUP, and two eight-byte moves into two
                // registers is what a gcc-compiled caller does not expect.
                let base = self.address_of_pseudo(*src);
                self.push_lir(X86Inst::MovFp {
                    size: FpSize::for_sse_aggregate(ret_size),
                    src: XmmOperand::Mem(MemAddr::BaseOffset { base, offset: 0 }),
                    dst: XmmOperand::Reg(XmmReg::Xmm0),
                });
            } else if insn.returns_via_x87() && is_struct_or_union {
                // An aggregate that is nothing but a `long double` comes back
                // in st(0). The `Ret` carries its address, so load it onto the
                // FPU stack the way the bare scalar is returned.
                let base = self.address_of_pseudo(*src);
                self.push_lir(X86Inst::X87Load {
                    addr: MemAddr::BaseOffset { base, offset: 0 },
                });
            } else if insn.returns_two_regs() {
                // Two-register struct return: check ABI for SSE vs INTEGER
                if is_struct_or_union {
                    if let Some(typ) = ret_typ {
                        let abi = get_abi(&self.base.target);
                        if let ArgClass::Direct { classes, .. } = abi.classify_return(typ, types) {
                            // Handle two-register struct returns per ABI classification.
                            // Must solve the parallel-move problem: if src[1] is in
                            // Rax, moving src[0]→Rax first would clobber it.
                            let srcs: Vec<Option<PseudoId>> =
                                vec![Some(*src), insn.src.get(1).copied()];

                            // Collect (src, dst_reg) pairs
                            let mut gp_moves: Vec<(PseudoId, Reg)> = Vec::new();
                            let mut xmm_idx = 0;
                            let mut gp_idx = 0;
                            for (i, &class) in classes.iter().enumerate() {
                                if let Some(s) = srcs.get(i).copied().flatten() {
                                    match class {
                                        RegClass::Sse => {
                                            let xmm = if xmm_idx == 0 {
                                                XmmReg::Xmm0
                                            } else {
                                                XmmReg::Xmm1
                                            };
                                            self.emit_fp_move(s, xmm, FpSize::Double);
                                            xmm_idx += 1;
                                        }
                                        _ => {
                                            let gp = if gp_idx == 0 { Reg::Rax } else { Reg::Rdx };
                                            gp_moves.push((s, gp));
                                            gp_idx += 1;
                                        }
                                    }
                                }
                            }

                            // Emit GP moves, handling clobber: if src[1] lives in Rax,
                            // move it to Rdx FIRST, then src[0] to Rax.
                            if gp_moves.len() == 2 {
                                let src1_in_rax =
                                    matches!(self.get_location(gp_moves[1].0), Loc::Reg(Reg::Rax));
                                if src1_in_rax {
                                    // Move second (Rdx) first to avoid clobbering
                                    self.emit_move(gp_moves[1].0, gp_moves[1].1, 64);
                                    self.emit_move(gp_moves[0].0, gp_moves[0].1, 64);
                                } else {
                                    self.emit_move(gp_moves[0].0, gp_moves[0].1, 64);
                                    self.emit_move(gp_moves[1].0, gp_moves[1].1, 64);
                                }
                            } else {
                                for (s, gp) in &gp_moves {
                                    self.emit_move(*s, *gp, 64);
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
                                        self.push_lir(X86Inst::MovFp {
                                            size: FpSize::Double,
                                            src: XmmOperand::Mem(self.stack_mem(offset)),
                                            dst: XmmOperand::Reg(XmmReg::Xmm0),
                                        });
                                        self.push_lir(X86Inst::MovFp {
                                            size: FpSize::Double,
                                            src: XmmOperand::Mem(self.stack_mem(offset - 8)),
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
                                // One SSE register carrying the aggregate. The
                                // width comes from the class's size: a struct's
                                // own type answers `Double` whatever it holds,
                                // which is two bytes too wide for a `_Float16`
                                // and half as wide as a binary128 needs.
                                let fp_size = FpSize::for_sse_aggregate(size_bits);
                                match src_loc {
                                    Loc::Stack(offset) => {
                                        self.push_lir(X86Inst::MovFp {
                                            size: fp_size,
                                            src: XmmOperand::Mem(self.stack_mem(offset)),
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
                                    _ => self.emit_move(*src, Reg::Rax, ret_size),
                                }
                            }
                        } else {
                            // INTEGER class - return in RAX
                            self.emit_move(*src, Reg::Rax, ret_size);
                        }
                    } else {
                        self.emit_move(*src, Reg::Rax, ret_size);
                    }
                } else {
                    self.emit_move(*src, Reg::Rax, ret_size);
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
                        self.push_lir(X86Inst::Mov {
                            size: OperandSize::B64,
                            src: GpOperand::Mem(self.stack_mem(offset)),
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
            } else if is_complex_x87 {
                // Real in st(0), imaginary in st(1). The x87 register file is
                // a stack, so push the imaginary part first and the real part
                // second to leave them in that order.
                let base = types.complex_base(insn.typ.unwrap());
                let imag_off = (types.size_bits(base) / 8) as i32;
                let base_addr = self.address_of_pseudo(*src);
                self.push_lir(X86Inst::X87Load {
                    addr: MemAddr::BaseOffset {
                        base: base_addr,
                        offset: imag_off,
                    },
                });
                self.push_lir(X86Inst::X87Load {
                    addr: MemAddr::BaseOffset {
                        base: base_addr,
                        offset: 0,
                    },
                });
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
                    let fp_fmt = self.fp_format(Some(fp_typ), fp_size, types);
                    self.emit_fp_move(*src, XmmReg::Xmm0, fp_fmt);
                }
            } else if ret_typ.is_some_and(|t| types.kind(t) == TypeKind::Int128) {
                // __int128 return: lo half → RAX, hi half → RDX
                let loc = self.get_location(*src).clone();
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(self.int128_lo_mem_loc(&loc)),
                    dst: GpOperand::Reg(Reg::Rax),
                });
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(self.int128_hi_mem_loc(&loc)),
                    dst: GpOperand::Reg(Reg::Rdx),
                });
            } else {
                self.emit_move(*src, Reg::Rax, ret_size);
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
}
