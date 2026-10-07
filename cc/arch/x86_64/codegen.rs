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
// Uses linear scan register allocation and System V AMD64 ABI; an `ms_abi`
// function, and a call to one, use the Microsoft x64 convention (win64.rs).
//

use crate::arch::codegen::SelectOperands;
use crate::arch::codegen::{AsmModifierError, AsmOperandSlot, AsmOperandValue};
use crate::arch::codegen::{BswapSize, CodeGenBase, CodeGenerator, UnaryOp};
use crate::arch::lir::{is_private_name, CondCode, Directive, FpSize, Label, OperandSize, Symbol};
use crate::arch::x86_64::lir::{GpOperand, MemAddr, X86Inst, XmmOperand};
use crate::arch::x86_64::regalloc::{FrameBase, Loc, Reg, X87ControlWords, X87Scratch, XmmReg};
use crate::arch::x86_64::x87::{is_x87_float_to_int, is_x87_fp_cvt, is_x87_int_to_float};
use crate::ir::{Instruction, Module, NanCompare, Opcode, PseudoId, PseudoKind};
use crate::parse::ast::JmpKind;
use crate::target::{Os, Target};
use crate::types::TypeTable;
use std::collections::{HashMap, HashSet};

// x86-64 Code Generator

/// x86-64 code generator
pub struct X86_64CodeGen {
    /// Common code generation infrastructure
    pub(super) base: CodeGenBase<X86Inst>,
    /// Current function's register allocation. Every PseudoId → Loc
    /// lookup goes through `LocationMap`, so codegen never derives an
    /// alternative location from `PseudoKind`. The intrinsic-result and
    /// inline-asm sites that write into the map are the only post-
    /// allocate writers and remain visible as `.set` calls.
    pub(super) locations: crate::arch::regalloc::LocationMap<Loc>,
    /// Current function's pseudos (for looking up values)
    pub(super) pseudos: crate::arch::codegen::PseudoTable,
    /// Callee-saved registers used in current function (for epilogue)
    pub(super) callee_saved_regs: Vec<Reg>,
    /// Offset to add to stack locations to account for callee-saved registers
    pub(super) callee_saved_offset: i32,
    /// Bytes the prologue allocates for locals: what a dynamically aligned
    /// frame addresses them from.
    pub(super) stack_alloc_size: i32,
    /// The stack-protector canary's slot, when this function has one.
    pub(super) stack_guard: Option<i32>,
    /// Offset from rbp to register save area (for variadic functions)
    pub(super) reg_save_area_offset: i32,
    /// GP argument registers the named parameters consumed, for `va_start`'s
    /// `gp_offset` (variadic functions only).
    pub(super) named_gp_regs: usize,
    /// The same for SSE registers, for `fp_offset`.
    pub(super) named_fp_regs: usize,
    /// The `%rbp` displacement where the variadic arguments begin, which is
    /// `va_start`'s `overflow_arg_area`.
    ///
    /// A displacement and not a slot count: a named `long double` or a named
    /// MEMORY-class aggregate occupies bytes in the incoming area without
    /// consuming a register, and `IncomingOff::take` may insert alignment
    /// padding, so no multiple of eight derived from register overflow can
    /// express where the area actually ends.
    pub(super) named_incoming_end: i32,
    /// Counter for generating unique internal labels
    pub(super) unique_label_counter: u32,
    /// External symbols (need GOT access on macOS)
    pub(super) extern_symbols: HashSet<String>,
    /// Thread-local storage symbols (need TLS access via FS segment)
    pub(super) tls_symbols: HashSet<String>,
    /// Position-independent code mode (for shared libraries and PIE)
    pic_mode: bool,
    /// Long double constants to emit (label_bits -> value_bits).
    /// BTreeMap so the .rodata emission order in `emit_ld_constants`
    /// is deterministic (HashMap iteration would vary the layout
    /// across runs, breaking reproducible builds).
    pub(super) ld_constants: std::collections::BTreeMap<u128, [u8; 16]>,
    /// Double constants to emit (label_bits -> f64 value).
    /// BTreeMap for reproducible order, as `ld_constants`.
    pub(super) double_constants: std::collections::BTreeMap<u64, f64>,
    /// binary128 constants to emit (pool key -> the 16-byte image).
    /// BTreeMap for reproducible order, as `ld_constants`.
    pub(super) quad_constants: std::collections::BTreeMap<u128, [u8; 16]>,
    /// Sym pseudo ID → what its stack slot holds, for [`SymSlot`].
    pub(super) sym_slots: HashMap<PseudoId, crate::arch::codegen::SymSlot>,
    /// How this function's locals are addressed.
    pub(super) frame_base: FrameBase,
    /// Maximum local alignment (for andq in prologue)
    pub(super) max_local_align: i32,
    /// Pseudos that are 128-bit integers (need full 16-byte copies)
    pub(super) int128_pseudos: HashSet<PseudoId>,
    /// Where the allocator put the x87 scratch, when this function stages a
    /// value through it.
    pub(super) x87_scratch: Option<X87Scratch>,
    /// Where the allocator put the x87 control words, when this function
    /// converts a long double to an integer.
    pub(super) x87_control_words: Option<X87ControlWords>,
    /// The current function's calling convention.
    pub(super) func_conv: crate::abi::CallingConv,
    /// The XMM registers an `ms_abi` function saves in its prologue, in
    /// save-slot order. Empty for any other function.
    pub(super) win64_xmm_saves: Vec<XmmReg>,
    /// The `Arg` pseudos whose parameter is a pointer. An incoming slot of
    /// one holds the pointer, where the slot of a by-value aggregate *is*
    /// the object -- [`Self::address_of_pseudo`] loads the one and takes the
    /// address of the other.
    pub(super) incoming_pointers: HashSet<PseudoId>,
}

impl X86_64CodeGen {
    pub fn new(target: Target) -> Self {
        Self {
            base: CodeGenBase::new(target),
            locations: crate::arch::regalloc::LocationMap::new(),
            pseudos: Default::default(),
            callee_saved_regs: Vec::new(),
            callee_saved_offset: 0,
            stack_alloc_size: 0,
            stack_guard: None,
            reg_save_area_offset: 0,
            named_gp_regs: 0,
            named_fp_regs: 0,
            named_incoming_end: 16,
            unique_label_counter: 0,
            extern_symbols: HashSet::new(),
            tls_symbols: HashSet::new(),
            pic_mode: false,
            ld_constants: std::collections::BTreeMap::new(),
            double_constants: std::collections::BTreeMap::new(),
            quad_constants: std::collections::BTreeMap::new(),
            sym_slots: HashMap::new(),
            frame_base: FrameBase::Rbp,
            max_local_align: 16,
            int128_pseudos: HashSet::new(),
            x87_scratch: None,
            x87_control_words: None,
            func_conv: crate::abi::CallingConv::C,
            win64_xmm_saves: Vec::new(),
            incoming_pointers: HashSet::new(),
        }
    }

    /// Push a LIR instruction to the buffer (deferred emission)
    pub(super) fn push_lir(&mut self, inst: X86Inst) {
        self.base.push_lir(inst);
    }

    /// Compute the memory address for a stack offset.
    /// In normal mode: [rbp - (offset + callee_saved_offset)]
    /// In dynamic alignment mode: [base + (stack_alloc_size - offset)]
    ///
    /// Nothing corrects for the outgoing-argument area here. Reserving one
    /// moves `%rsp`, which is exactly why the aligned base is a register the
    /// prologue sets once instead.
    pub(super) fn stack_mem(&self, offset: i32) -> MemAddr {
        if let FrameBase::Aligned { reg, .. } = self.frame_base {
            MemAddr::BaseOffset {
                base: reg,
                offset: self.stack_alloc_size - offset,
            }
        } else {
            MemAddr::BaseOffset {
                base: Reg::Rbp,
                offset: -(offset + self.callee_saved_offset),
            }
        }
    }

    /// The address of byte `byte` of the object in stack slot `slot`.
    ///
    /// A slot index is not an `%rbp` displacement: [`Self::stack_mem`] turns it
    /// into `-(slot + callee_saved_offset)`, which is where the object starts,
    /// and the bytes above that run *downwards* in slot-index terms. Writing
    /// `%rbp + slot + byte` by hand -- as several emitters did -- addresses the
    /// caller's incoming-argument area instead, which is a different frame
    /// entirely.
    pub(super) fn stack_field(&self, slot: i32, byte: i32) -> MemAddr {
        self.stack_mem(slot - byte)
    }

    /// Convert a Loc to a GpOperand for LIR
    /// `v` as an immediate operand of an instruction `size` bits wide, or
    /// `None` when the encoding has no room for it.
    ///
    /// x86-64 takes at most a sign-extended 32-bit immediate everywhere but
    /// `movabs` to a register: a 64-bit instruction cannot add, compare or
    /// store anything wider. A 32-bit instruction takes any 32-bit pattern.
    /// The one statement of that rule; three emitters checked it themselves
    /// and the x87 conversion did not, storing `$9223372036854775807` to
    /// memory, which the assembler rejects.
    pub(super) fn imm_operand(v: i128, size: u32) -> Option<GpOperand> {
        let fits = if size <= 32 {
            i32::try_from(v).is_ok() || u32::try_from(v).is_ok()
        } else {
            i32::try_from(v).is_ok()
        };
        fits.then_some(GpOperand::Imm(v as i64))
    }

    /// The operand for `src`, at location `loc`, of an instruction `size`
    /// bits wide: what `loc_to_gp_operand` gives, except that an immediate the
    /// encoding cannot take is materialized into `scratch` first.
    pub(super) fn gp_operand_via(
        &mut self,
        src: PseudoId,
        loc: &Loc,
        size: u32,
        scratch: Reg,
    ) -> GpOperand {
        match loc {
            Loc::Imm(v) if Self::imm_operand(*v, size).is_none() => {
                self.emit_move(src, scratch, size);
                GpOperand::Reg(scratch)
            }
            _ => self.loc_to_gp_operand(loc),
        }
    }

    /// Store the constant `v`, `size` bits wide, to `addr` -- through
    /// `scratch` when no immediate encoding holds it. `scratch` must not be a
    /// register `addr` is formed from.
    pub(super) fn store_imm(&mut self, v: i128, size: u32, addr: MemAddr, scratch: Reg) {
        let op_size = OperandSize::from_bits(size.max(32));
        let src = match Self::imm_operand(v, size) {
            Some(imm) => imm,
            None => {
                self.push_lir(X86Inst::MovAbs {
                    imm: v as i64,
                    dst: scratch,
                });
                GpOperand::Reg(scratch)
            }
        };
        self.push_lir(X86Inst::Mov {
            size: op_size,
            src,
            dst: GpOperand::Mem(addr),
        });
    }

    pub(super) fn loc_to_gp_operand(&self, loc: &Loc) -> GpOperand {
        match loc {
            Loc::Reg(r) => GpOperand::Reg(*r),
            Loc::Stack(offset) => GpOperand::Mem(self.stack_mem(*offset)),
            Loc::IncomingArg(offset) => {
                // Incoming stack argument: at [rbp + offset] (positive offset)
                // No callee_saved_offset adjustment needed - these are above the return address
                GpOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: *offset,
                })
            }
            Loc::Imm(v) => GpOperand::Imm(*v as i64),
            Loc::FImm(_, _) => GpOperand::Imm(0), // FP immediates handled separately
            Loc::Xmm(_) => GpOperand::Imm(0),     // XMM handled separately
            Loc::Global(name) => {
                // Note: For GOT access (PIC mode/external symbols), special handling
                // is needed - see emit_global_load* and emit_global_store* functions
                // which generate the two-instruction GOT sequence
                self.reject_tls_operand(name);
                GpOperand::Mem(MemAddr::RipRelative(Symbol::named(name.clone())))
            }
        }
    }

    /// Report `name` as an internal error if it is a thread-local. For the
    /// paths that print a global operand without asking the TLS model.
    ///
    /// A thread-local reaches the backend as a symbol only under the static
    /// ELF models, and then only through the paths that choose Local or
    /// Initial Exec for it -- `global_mem`, the load and store paths, an
    /// inline-asm memory operand. A register operand arrives already loaded.
    /// So nothing brings one to a model-blind path, and any operand such a
    /// path could print -- `%fs:sym@TPOFF`, `sym(%rip)` -- would be wrong for
    /// an `extern` or shared-mode thread-local.
    pub(super) fn reject_tls_operand(&self, name: &str) {
        if self.is_tls_symbol(name) {
            crate::arch::codegen::report_tls_operand(self.base.func_pos, name);
        }
    }

    /// Whether `name` is a thread-local this backend must access through the
    /// FS segment: an ELF thread-local, on Linux or FreeBSD alike. On Darwin a
    /// thread-local never reaches here as a symbol -- `ir::tls` turns every
    /// reference into a `TlsAddr` -- so it is not one of these.
    ///
    /// This used to ask for Linux alone, which sent every FreeBSD
    /// thread-local through ordinary global access: one copy for all threads.
    pub(super) fn is_tls_symbol(&self, name: &str) -> bool {
        self.tls_symbols.contains(name) && self.base.target.os != Os::MacOS
    }

    /// Whether accessing the thread-local `name` needs the Initial Exec model
    /// rather than Local Exec. See [`CodeGenBase::use_tls_ie`].
    pub(super) fn use_tls_ie(&self, name: &str) -> bool {
        self.base.use_tls_ie(self.extern_symbols.contains(name))
    }

    /// Compute the *address* of a thread-local into `dst`.
    ///
    /// Loading a thread-local's value takes one instruction, because the FS
    /// segment override does the addition. Taking its address does not: the
    /// thread pointer has to be materialized first, since `%fs:sym@TPOFF` is a
    /// memory operand, not a value. Getting this wrong is invisible on a read
    /// -- the bad address often still points at something mapped -- and
    /// segfaults on a write.
    fn emit_tls_addr(&mut self, name: &str, dst: Reg) {
        let symbol = Symbol::global(name.to_string());
        if self.base.tls_access() == crate::target::TlsAccess::MachOTlv {
            // Mach-O thread-local variable descriptor (clang's sequence):
            //   movq _v@TLVP(%rip), %rdi    ; the descriptor
            //   call *(%rdi)                ; its getter: the ADDRESS in %rax
            //
            // The getter preserves the callee-saved registers plus %rcx,
            // %rdx, %rsi and %r8-%r11 (LLVM's `CSR_64_TLS_Darwin`), so %rax
            // and %rdi are the general registers it clobbers, which
            // `get_constraint_info` declares -- and no XMM register, which
            // `RegAlloc::fp_call_positions` accounts for. ld64 relaxes the
            // `movq` to a `leaq` of the descriptor when it is local, so it
            // has to stay a `movq` from `@TLVP`.
            //
            // Reached only through a `TlsAddr`: `ir::tls` rewrites every
            // Darwin thread-local reference into one.
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Mem(MemAddr::Tlvp(symbol)),
                dst: GpOperand::Reg(Reg::Rdi),
            });
            self.push_lir(X86Inst::TlvCall);
            if dst != Reg::Rax {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Reg(dst),
                });
            }
            return;
        }
        if self.base.use_tls_dynamic() {
            // TLS descriptor, the dynamic model:
            //   leaq sym@TLSDESC(%rip), %rax
            //   call *sym@TLSCALL(%rax)      ; returns an OFFSET in %rax
            //   addq %fs:0, %rax             ; plus the thread pointer
            //
            // The resolver returns the offset from the thread pointer, not an
            // address -- the same convention Initial Exec uses. gcc hides this
            // by folding the addition into the access as `%fs:(%rax)`; here
            // the whole point is to produce a plain pointer, so the thread
            // pointer is added explicitly.
            //
            // `%rax` is not a choice -- the `@TLSCALL` relocation names it,
            // and the linker matches the `leaq`/`call` pair when relaxing to a
            // static model, so nothing may come between them.
            self.push_lir(X86Inst::Lea {
                addr: MemAddr::TlsDesc(symbol.clone()),
                dst: Reg::Rax,
            });
            self.push_lir(X86Inst::TlsDescCall { sym: symbol });
            self.push_lir(X86Inst::Add {
                size: OperandSize::B64,
                src: GpOperand::Mem(MemAddr::FsAbsolute(0)),
                dst: Reg::Rax,
            });
            if dst != Reg::Rax {
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::Rax),
                    dst: GpOperand::Reg(dst),
                });
            }
            return;
        }

        // Initial Exec for a symbol defined elsewhere, matching what the load
        // and store paths choose.
        if self.use_tls_ie(name) {
            // movq sym@GOTTPOFF(%rip), %dst   ; the offset from the thread pointer
            // addq %fs:0, %dst                ; plus the thread pointer itself
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Mem(MemAddr::TlsGottpoff(symbol)),
                dst: GpOperand::Reg(dst),
            });
            self.push_lir(X86Inst::Add {
                size: OperandSize::B64,
                src: GpOperand::Mem(MemAddr::FsAbsolute(0)),
                dst,
            });
        } else {
            // movq %fs:0, %dst                ; the thread pointer
            // leaq sym@TPOFF(%dst), %dst      ; plus the link-time offset
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Mem(MemAddr::FsAbsolute(0)),
                dst: GpOperand::Reg(dst),
            });
            self.push_lir(X86Inst::Lea {
                addr: MemAddr::TlsTpoffBase {
                    sym: symbol,
                    base: dst,
                },
                dst,
            });
        }
    }

    /// Check if a symbol needs GOT access
    /// - In PIC mode: all non-local symbols need GOT access (interposition)
    /// - On macOS: external symbols always need GOT access (even without PIC)
    pub(super) fn needs_got_access(&self, name: &str) -> bool {
        // In PIC mode, all non-local symbols need GOT access because they
        // could be interposed at runtime (the default for global symbols).
        // A name c17 made up is local and can't be interposed.
        if self.pic_mode && !is_private_name(name) {
            return true;
        }
        // External symbols need GOT access on macOS for dynamic linking.
        if self.base.target.os == Os::MacOS {
            return self.extern_symbols.contains(name);
        }
        false
    }

    /// The memory operand for byte `offset` of the global `name`, for an access
    /// emitted immediately after. Any setup it needs is emitted now, through
    /// `scratch`, which must stay untouched until that access.
    ///
    /// One implementation of "how to reach a global" for the floating-point
    /// and x87 load and store paths, which each used to build their own
    /// operand and knew only RIP-relative and GOT access. A thread-local came
    /// out as `movsd tv(%rip)`: the variable's initialization image rather
    /// than this thread's copy, and for an `extern` one a non-TLS reference
    /// the linker rejects. And the RIP-relative form dropped `offset`.
    pub(super) fn global_mem(&mut self, name: &str, offset: i32, scratch: Reg) -> MemAddr {
        if self.is_tls_symbol(name) {
            let symbol = Symbol::global(name.to_string());
            if offset == 0 {
                if !self.use_tls_ie(name) {
                    return MemAddr::TlsLocalExec(symbol);
                }
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(MemAddr::TlsGottpoff(symbol)),
                    dst: GpOperand::Reg(scratch),
                });
                return MemAddr::FsBase(scratch);
            }
            self.emit_tls_addr(name, scratch);
            return MemAddr::BaseOffset {
                base: scratch,
                offset,
            };
        }
        if self.needs_got_access(name) {
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Mem(MemAddr::GotPcrel(Symbol::extern_sym(name.to_string()))),
                dst: GpOperand::Reg(scratch),
            });
            return MemAddr::BaseOffset {
                base: scratch,
                offset,
            };
        }
        let symbol = Symbol::named(name.to_string());
        if offset == 0 {
            return MemAddr::RipRelative(symbol);
        }
        self.push_lir(X86Inst::Lea {
            addr: MemAddr::RipRelative(symbol),
            dst: scratch,
        });
        MemAddr::BaseOffset {
            base: scratch,
            offset,
        }
    }

    /// Emit .loc directive for source line tracking (delegates to base)
    fn emit_loc(&mut self, insn: &Instruction) {
        self.base.emit_loc(insn);
    }

    /// Emit file header (delegates to base)
    fn emit_header(&mut self) {
        self.base.emit_header();
    }

    /// Emit a global variable (delegates to base)
    fn emit_global(&mut self, global: &crate::ir::GlobalDef, types: &TypeTable) {
        // Skip extern symbols - they're defined elsewhere
        if self.extern_symbols.contains(&global.name) {
            return;
        }
        self.base.emit_global(global, types);
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
            let label = crate::arch::lir::internal_label("ld_const", label_bits);
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

    /// Emit the binary128 constant pool.
    ///
    /// A `__float128` has no immediate form and cannot be built in a general
    /// register — it is 16 bytes — so every constant is loaded from `.rodata`.
    fn emit_quad_constants(&mut self) {
        if self.quad_constants.is_empty() {
            return;
        }
        self.base.push_directive(Directive::Rodata);
        for (key, bytes) in &self.quad_constants {
            let label = crate::arch::lir::internal_label("quad_const", key);
            self.base.push_directive(Directive::Align(4));
            self.base.push_directive(Directive::local_label(&label));
            let mut byte_str = String::from(".byte ");
            for (i, b) in bytes.iter().enumerate() {
                if i > 0 {
                    byte_str.push_str(", ");
                }
                byte_str.push_str(&b.to_string());
            }
            self.base.push_directive(Directive::Raw(byte_str));
        }
        self.base.push_directive(Directive::Text);
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
            let label = crate::arch::lir::internal_label("dbl_const", label_bits);
            // Align to 8 bytes (power of 2: 3 means 2^3 = 8)
            self.base.push_directive(Directive::Align(3));
            self.base.push_directive(Directive::local_label(&label));

            // Emit as .quad (8 bytes)
            let bits = value.to_bits();
            self.base
                .push_directive(Directive::Raw(format!(".quad 0x{:016x}", bits)));
        }
    }

    pub(super) fn emit_block(&mut self, block: &crate::ir::BasicBlock, types: &TypeTable) {
        // Always emit block ID label for consistency with jumps
        // (jumps reference blocks by ID, not by C label name)
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(Label::block(
            &self.base.current_fn,
            block.id.0,
        ))));

        // Emit instructions
        for insn in &block.insns {
            self.emit_insn(insn, types);
        }
    }

    /// Emit conditional branch: test condition and branch accordingly
    /// Returns true if an early return was taken (for constant conditions)
    /// Set the flags for `cond != 0`, reading `cond` at the width its value
    /// is held at -- see [`crate::arch::codegen::ValueWidths`] -- through
    /// `scratch` where it is neither in a register nor in the frame. The one
    /// test `Cbr` and `Select` both make.
    fn emit_condition_test(&mut self, cond: PseudoId, scratch: Reg) {
        let bits = self.base.value_widths.bits(cond).clamp(8, 64);
        let size = OperandSize::from_bits(bits);
        match self.get_location(cond) {
            Loc::Reg(r) => self.push_lir(X86Inst::Test {
                size,
                src: GpOperand::Reg(r),
                dst: GpOperand::Reg(r),
            }),
            Loc::Stack(offset) => self.push_lir(X86Inst::Cmp {
                size,
                src: GpOperand::Imm(0),
                dst: GpOperand::Mem(self.stack_mem(offset)),
            }),
            Loc::IncomingArg(offset) => self.push_lir(X86Inst::Cmp {
                size,
                src: GpOperand::Imm(0),
                dst: GpOperand::Mem(MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset,
                }),
            }),
            _ => {
                self.emit_move(cond, scratch, bits);
                self.push_lir(X86Inst::Test {
                    size,
                    src: GpOperand::Reg(scratch),
                    dst: GpOperand::Reg(scratch),
                });
            }
        }
    }

    fn emit_cbr(&mut self, insn: &Instruction) -> bool {
        let Some(&cond) = insn.src.first() else {
            return false;
        };

        let loc = self.get_location(cond);
        let size = self.base.value_widths.bits(cond);

        // Handle 128-bit integer stack values: OR both halves together and test
        if self.int128_pseudos.contains(&insn.src[0]) {
            if let Loc::Stack(_) | Loc::IncomingArg(_) = &loc {
                let lo_mem = self.int128_lo_mem_loc(&loc);
                let hi_mem = self.int128_hi_mem_loc(&loc);
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(lo_mem),
                    dst: GpOperand::Reg(Reg::R10),
                });
                self.push_lir(X86Inst::Or {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(hi_mem),
                    dst: Reg::R10,
                });
                // Test result: NE if nonzero
                if let Some(target) = insn.bb_true {
                    self.push_lir(X86Inst::Jcc {
                        cc: CondCode::Ne,
                        target: Label::block(&self.base.current_fn, target.0),
                    });
                }
                if let Some(target) = insn.bb_false {
                    self.push_lir(X86Inst::Jmp {
                        target: Label::block(&self.base.current_fn, target.0),
                    });
                }
                return false;
            }
        }

        match &loc {
            Loc::Reg(_) | Loc::Stack(_) | Loc::IncomingArg(_) | Loc::Global(_) => {
                self.emit_condition_test(cond, Reg::R10);
            }
            Loc::Imm(v) => {
                let target = if *v != 0 { insn.bb_true } else { insn.bb_false };
                if let Some(target) = target {
                    self.push_lir(X86Inst::Jmp {
                        target: Label::block(&self.base.current_fn, target.0),
                    });
                }
                return true;
            }
            Loc::Xmm(x) => {
                let fp_size = if size <= 32 {
                    FpSize::Single
                } else {
                    FpSize::Double
                };
                self.push_lir(X86Inst::XorpsSelf { reg: XmmReg::Xmm15 });
                self.push_lir(X86Inst::ComiFp {
                    nan: NanCompare::Quiet,
                    size: fp_size,
                    src: XmmOperand::Reg(*x),
                    dst: XmmReg::Xmm15,
                });
            }
            Loc::FImm(v, _) => {
                let target = if !v.is_zero() {
                    insn.bb_true
                } else {
                    insn.bb_false
                };
                if let Some(target) = target {
                    self.push_lir(X86Inst::Jmp {
                        target: Label::block(&self.base.current_fn, target.0),
                    });
                }
                return true;
            }
        }

        if let Some(target) = insn.bb_true {
            self.push_lir(X86Inst::Jcc {
                cc: CondCode::Ne,
                target: Label::block(&self.base.current_fn, target.0),
            });
        }
        if let Some(target) = insn.bb_false {
            self.push_lir(X86Inst::Jmp {
                target: Label::block(&self.base.current_fn, target.0),
            });
        }
        false
    }

    /// Lower a `switch` to a chain of compare-and-branch.
    ///
    /// The aarch64 backend has the same method; keeping the two the same shape
    /// is deliberate.
    fn emit_switch(&mut self, insn: &Instruction, types: &TypeTable) {
        let Some(&val) = insn.src.first() else {
            return;
        };
        // Derive comparison size from type to handle long/pointer switches
        let switch_size = insn
            .typ
            .map(|t| types.size_bits(t).max(32))
            .unwrap_or(insn.size.max(32));
        self.emit_move(val, Reg::R10, switch_size);
        let op_size = if switch_size > 32 {
            OperandSize::B64
        } else {
            OperandSize::B32
        };

        for (lo, hi, target_bb) in insn.extra().switch_cases.clone() {
            let target = Label::block(&self.base.current_fn, target_bb.0);
            if lo == hi {
                self.emit_switch_cmp(op_size, lo);
                self.push_lir(X86Inst::Jcc {
                    cc: CondCode::Eq,
                    target,
                });
                continue;
            }
            self.emit_switch_range(val, switch_size, op_size, lo, hi, target);
        }

        // Jump to default (or fall through if no default)
        if let Some(default_bb) = insn.extra().switch_default {
            // LIR: unconditional jump to default
            self.push_lir(X86Inst::Jmp {
                target: Label::block(&self.base.current_fn, default_bb.0),
            });
        }
    }

    /// Compare the switch value in R10 against `v`, using R11 when a 64-bit
    /// constant does not fit a sign-extended 32-bit immediate.
    fn emit_switch_cmp(&mut self, op_size: OperandSize, v: i64) {
        let fits_in_simm32 = v >= i32::MIN as i64 && v <= i32::MAX as i64;
        if op_size == OperandSize::B64 && !fits_in_simm32 {
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Imm(v),
                dst: GpOperand::Reg(Reg::R11),
            });
            self.push_lir(X86Inst::Cmp {
                size: op_size,
                src: GpOperand::Reg(Reg::R11),
                dst: GpOperand::Reg(Reg::R10),
            });
        } else {
            self.push_lir(X86Inst::Cmp {
                size: op_size,
                src: GpOperand::Imm(v),
                dst: GpOperand::Reg(Reg::R10),
            });
        }
    }

    /// A GNU range `case lo ... hi:`. Tested as
    /// `(x - lo) <=unsigned (hi - lo)`: subtracting the low endpoint makes
    /// everything below it wrap to a large unsigned value, so one unsigned
    /// comparison decides both ends. Expanding the range into one compare per
    /// value is not an option -- `case 0 ... 1000000:` is legal C.
    ///
    /// R10 holds the switch value and is reused by later cases, so the
    /// subtraction goes to R11.
    fn emit_switch_range(
        &mut self,
        val: PseudoId,
        switch_size: u32,
        op_size: OperandSize,
        lo: i64,
        hi: i64,
        target: Label,
    ) {
        self.emit_move(val, Reg::R11, switch_size);
        let span = hi.wrapping_sub(lo);
        let fits = |v: i64| v >= i32::MIN as i64 && v <= i32::MAX as i64;
        if op_size == OperandSize::B64 && !fits(lo) {
            // No scratch left for a wide immediate, so build it
            // in R10 and restore R10 afterwards.
            self.push_lir(X86Inst::Push {
                src: GpOperand::Reg(Reg::R10),
            });
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Imm(lo),
                dst: GpOperand::Reg(Reg::R10),
            });
            self.push_lir(X86Inst::Sub {
                size: op_size,
                src: GpOperand::Reg(Reg::R10),
                dst: Reg::R11,
            });
            self.push_lir(X86Inst::Pop { dst: Reg::R10 });
        } else {
            self.push_lir(X86Inst::Sub {
                size: op_size,
                src: GpOperand::Imm(lo),
                dst: Reg::R11,
            });
        }
        if op_size == OperandSize::B64 && !fits(span) {
            self.push_lir(X86Inst::Push {
                src: GpOperand::Reg(Reg::R10),
            });
            self.push_lir(X86Inst::Mov {
                size: OperandSize::B64,
                src: GpOperand::Imm(span),
                dst: GpOperand::Reg(Reg::R10),
            });
            self.push_lir(X86Inst::Cmp {
                size: op_size,
                src: GpOperand::Reg(Reg::R10),
                dst: GpOperand::Reg(Reg::R11),
            });
            self.push_lir(X86Inst::Pop { dst: Reg::R10 });
        } else {
            self.push_lir(X86Inst::Cmp {
                size: op_size,
                src: GpOperand::Imm(span),
                dst: GpOperand::Reg(Reg::R11),
            });
        }
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Ule,
            target,
        });
    }

    fn emit_set_val(&mut self, insn: &Instruction, types: &TypeTable) {
        if let Some(target) = insn.target {
            if let Some(pseudo) = self.pseudos.get(target) {
                let target_loc = self.locations.get(target);
                match &pseudo.kind {
                    PseudoKind::Val(v) => match target_loc {
                        Some(Loc::Reg(r)) => {
                            self.push_lir(X86Inst::Mov {
                                size: OperandSize::from_bits(insn.size),
                                src: GpOperand::Imm(*v as i64),
                                dst: GpOperand::Reg(r),
                            });
                        }
                        // A 128-bit constant lives in a sixteen-byte
                        // slot, since that is the only place its
                        // consumers can address it. Without this the
                        // slot was allocated and never written, so the
                        // constant read back as zero.
                        Some(loc @ (Loc::Stack(_) | Loc::IncomingArg(_))) => {
                            let (v, loc) = (*v, loc.clone());
                            self.store_int128_imm(v, &loc);
                        }
                        _ => {}
                    },
                    PseudoKind::FVal(v) => {
                        // Only emit code if the target is in an XMM register
                        // FImm locations are materialized inline at use sites
                        if let Some(Loc::Xmm(_)) = target_loc {
                            let fmt = self.fp_format(insn.typ, insn.size, types);
                            self.emit_fp_const_load(target, *v, fmt);
                        }
                        // For FImm locations, do nothing - the value will be
                        // loaded inline when used in operations
                    }
                    _ => {}
                }
            }
        }
    }

    fn emit_sym_addr(&mut self, insn: &Instruction) {
        if let (Some(target), Some(&src)) = (insn.target, insn.src.first()) {
            let dst_loc = self.get_location(target);
            // Use R10 as scratch to avoid clobbering live values in Rax
            let dst_reg = match &dst_loc {
                Loc::Reg(r) => *r,
                _ => Reg::R10,
            };
            let src_loc = self.get_location(src);
            match src_loc {
                Loc::Global(name) if self.is_tls_symbol(&name) => {
                    self.emit_tls_addr(&name, dst_reg);
                }
                Loc::Global(name) => {
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
                            addr: MemAddr::RipRelative(Symbol::named(name)),
                            dst: dst_reg,
                        });
                    }
                }
                Loc::Stack(offset) => {
                    // Get address of stack location
                    self.push_lir(X86Inst::Lea {
                        addr: self.stack_mem(offset),
                        dst: dst_reg,
                    });
                }
                Loc::IncomingArg(offset) => {
                    // Get address of incoming stack argument (e.g., large struct param)
                    self.push_lir(X86Inst::Lea {
                        addr: MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset,
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

    fn emit_tls_addr_insn(&mut self, insn: &Instruction) {
        if let (Some(target), Some(&src)) = (insn.target, insn.src.first()) {
            let dst_loc = self.get_location(target);
            // R10 is reserved scratch, so it is safe when the result
            // lives on the stack.
            let dst_reg = match &dst_loc {
                Loc::Reg(r) => *r,
                _ => Reg::R10,
            };
            if let Loc::Global(name) = self.get_location(src) {
                self.emit_tls_addr(&name, dst_reg);
                if !matches!(dst_loc, Loc::Reg(_)) {
                    self.emit_move_to_loc(dst_reg, &dst_loc, 64);
                }
            }
        }
    }

    fn emit_insn(&mut self, insn: &Instruction, types: &TypeTable) {
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
                        target: Label::block(&self.base.current_fn, target.0),
                    });
                }
            }

            Opcode::Cbr => if self.emit_cbr(insn) {},

            // GNU computed goto: jump through the address in src[0]. The
            // CFG edges to every address-taken label are recorded on the
            // block, so liveness and DCE already see the real successors.
            Opcode::IndirectBr => {
                if let Some(&val) = insn.src.first() {
                    self.emit_move(val, Reg::R10, 64);
                    self.push_lir(X86Inst::JmpIndirect { reg: Reg::R10 });
                }
            }

            Opcode::Switch => self.emit_switch(insn, types),

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

            Opcode::Fabs => {
                if self.is_longdouble_op(insn, types) {
                    self.emit_x87_abs(insn);
                } else {
                    self.emit_fp_abs(insn, types);
                }
            }

            Opcode::CopySign => {
                if self.is_longdouble_op(insn, types) {
                    self.emit_x87_copysign(insn);
                } else {
                    self.emit_fp_copysign(insn, types);
                }
            }

            // A binary128 root never gets here: it is a call by now (see
            // `arch::mapping::call_library_fallbacks`).
            Opcode::Sqrt => {
                if self.is_longdouble_op(insn, types) {
                    self.emit_x87_sqrt(insn);
                } else {
                    self.emit_fp_sqrt(insn, types);
                }
            }

            // Only a `float` or `double` `floor`, `ceil`, `trunc` or `rint`
            // gets here; the rest are calls by now.
            Opcode::RoundToIntegral(how) => self.emit_fp_round_to_integral(insn, how, types),
            Opcode::FMin | Opcode::FMax | Opcode::Fma => {
                unreachable!("{:?} is a call on x86-64 (see computes_in_place)", insn.op)
            }

            // The operand's format is its `src_typ`: `typ` is the `int`.
            Opcode::Signbit => {
                if self.fp_format(insn.src_typ, insn.src_size, types) == FpSize::Extended {
                    self.emit_x87_signbit(insn);
                } else {
                    self.emit_fp_signbit(insn, types);
                }
            }

            // Floating-point comparisons
            op if op.is_float_comparison() => {
                if self.is_longdouble_op(insn, types) {
                    self.emit_x87_compare(insn);
                } else {
                    self.emit_fp_compare(insn, types);
                }
            }

            // Integer to float conversions
            Opcode::UCvtF | Opcode::SCvtF => {
                if is_x87_int_to_float(insn, types) {
                    self.emit_x87_int_to_float(insn);
                } else {
                    self.emit_int_to_float(insn, types);
                }
            }

            // Float to integer conversions
            Opcode::FCvtU | Opcode::FCvtS => {
                if is_x87_float_to_int(insn, types) {
                    self.emit_x87_float_to_int(insn);
                } else {
                    self.emit_float_to_int(insn, types);
                }
            }

            // Float to float conversions (e.g., float to double)
            Opcode::FCvtF => {
                if is_x87_fp_cvt(insn, types) {
                    self.emit_x87_fp_cvt(insn, types);
                } else {
                    self.emit_float_to_float(insn, types);
                }
            }

            op if op.is_int_comparison() => {
                self.emit_compare(insn, types);
            }

            Opcode::Simd(op) => self.emit_simd(insn, op, types),

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

            Opcode::SetVal => self.emit_set_val(insn, types),

            Opcode::Copy => {
                if let (Some(target), Some(&src)) = (insn.target, insn.src.first()) {
                    // Pass the type to emit_copy for proper sign/zero extension
                    self.emit_copy_with_type(src, target, insn.size, insn.typ, types);
                }
            }

            Opcode::TlsAddr => self.emit_tls_addr_insn(insn),

            Opcode::SymAddr => self.emit_sym_addr(insn),

            Opcode::Select => {
                self.emit_select(insn, types);
            }

            Opcode::Zext | Opcode::Sext | Opcode::Trunc => {
                self.emit_extend(insn, types);
            }

            // Variadic function support (va_* builtins)
            Opcode::VaStart if self.func_conv == crate::abi::CallingConv::Win64 => {
                self.emit_win64_va_start(insn);
            }
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

            Opcode::StackSave => self.emit_stack_save(insn),
            Opcode::StackRestore => self.emit_stack_restore(insn),

            Opcode::Memset => {
                self.emit_memset(insn);
            }

            Opcode::Memcpy => {
                self.emit_memcpy(insn);
            }

            Opcode::Memmove => {
                self.emit_memmove(insn);
            }

            Opcode::Unreachable => {
                // Emit ud2 instruction - undefined instruction that traps
                // This is used for __builtin_unreachable() to indicate code
                // that should never be reached. If it is reached, the CPU
                // will generate a SIGILL.
                self.push_lir(X86Inst::Ud2);
            }

            Opcode::FrameAddress => {
                // __builtin_frame_address(level)
                self.emit_frame_address(insn);
            }

            Opcode::ReturnAddress => {
                // __builtin_return_address(level)
                self.emit_return_address(insn);
            }

            // setjmp/longjmp support
            Opcode::Setjmp => match insn.jmp_kind() {
                JmpKind::Library => self.emit_setjmp(insn),
                JmpKind::Builtin => self.emit_builtin_setjmp(insn),
            },

            Opcode::Longjmp => match insn.jmp_kind() {
                JmpKind::Library => self.emit_longjmp(insn),
                JmpKind::Builtin => self.emit_builtin_longjmp(insn),
            },

            Opcode::Asm => {
                self.emit_inline_asm(insn);
            }

            // Atomic Operations (C11 _Atomic support)
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

    fn emit_call(&mut self, insn: &Instruction, types: &TypeTable) {
        // Get function name (or placeholder for indirect calls)
        let func_name = if insn.extra().indirect_target.is_some() {
            "<indirect>".to_string()
        } else {
            match &insn.extra().func_name {
                Some(n) => n.clone(),
                None => return,
            }
        };

        let conv = insn.extra().abi_info.as_ref().map(|ai| ai.conv);
        if conv == Some(crate::abi::CallingConv::Win64) {
            self.emit_win64_call(insn, &func_name, types);
            return;
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
        if insn.extra().variadic_arg_start.is_some() {
            self.set_variadic_fp_count(fp_arg_count);
        }

        // For an indirect call, load the function pointer into R11 *after*
        // the arguments are in place. R10 and R11 are the argument setup's
        // own scratch registers -- `save_clobbered_arg_regs` shuttles through
        // them and the complex-argument path addresses its value through R11 --
        // so a target parked there before the setup was overwritten, and the
        // `call *%r11` jumped into whatever the last argument had addressed.
        if let Some(func_addr) = insn.extra().indirect_target {
            self.emit_move(func_addr, Reg::R11, 64);
        }

        // Emit the call instruction
        self.emit_call_instruction(insn, &func_name);

        // Clean up stack
        self.cleanup_call_stack(stack_args);

        // Handle return value
        self.handle_call_return_value(insn, types);
    }

    /// Emit a select (ternary) instruction using CMOVcc (integers) or
    /// conditional branch (floats, since CMov only works on GP registers).
    fn emit_select(&mut self, insn: &Instruction, types: &TypeTable) {
        let Some(ops) = SelectOperands::of(insn, types) else {
            return;
        };
        let (then_val, else_val, size) = (ops.then_val, ops.else_val, ops.width);

        // Check if this is a floating-point select
        let is_fp = insn.typ.is_some_and(|t| types.is_float(t))
            || matches!(self.get_location(then_val), Loc::Xmm(_) | Loc::FImm(..))
            || matches!(self.get_location(else_val), Loc::Xmm(_) | Loc::FImm(..));

        if is_fp {
            let fmt = self.fp_format(insn.typ, size, types);
            if fmt == FpSize::Extended {
                self.emit_x87_select(ops);
            } else {
                self.emit_select_fp(ops, fmt);
            }
        } else {
            self.emit_select_int(ops);
        }
    }

    /// Emit FP select using conditional branch (CMov doesn't work on XMM regs).
    ///
    /// Uses XMM15 as scratch for the merged value. XMM15 is documented as
    /// codegen-reserved (see `XmmReg::allocatable` — it returns xmm0–xmm13,
    /// leaving xmm14 and xmm15 out of the allocator's palette specifically
    /// so codegen helpers like this one can use them without coordinating
    /// with the allocator. Using xmm0 here clobbers any live xmm0-allocated
    /// pseudo (chordal coloring may legitimately assign xmm0 to a pseudo
    /// whose interval doesn't cross a call) and is the classic
    /// silent-corruption case: the value-loss only manifests in
    /// downstream computations, often as infinite loops or wrong results.
    fn emit_select_fp(&mut self, ops: SelectOperands, size: FpSize) {
        let SelectOperands {
            cond,
            then_val,
            else_val,
            target,
            ..
        } = ops;
        let dst_loc = self.get_location(target);

        // Load condition to R11. FP value computation (fneg, fadd, etc.)
        // may clobber GP registers like RAX for immediate loading. We must
        // reload the condition from its STACK slot, not trust the register.
        let cond_loc = self.get_location(cond);
        match &cond_loc {
            Loc::Imm(v) => {
                let val = if *v != 0 { then_val } else { else_val };
                self.emit_fp_move(val, XmmReg::Xmm15, size);
                self.emit_fp_move_from_xmm(XmmReg::Xmm15, &dst_loc, size);
                return;
            }
            _ => self.emit_condition_test(cond, Reg::R11),
        }

        // Branch: load else_val, skip over then_val load if condition is false
        let then_suffix = self.unique_label_counter;
        self.unique_label_counter += 1;
        let done_suffix = self.unique_label_counter;
        self.unique_label_counter += 1;
        let then_label = Label::internal("sel_then", then_suffix);
        let done_label = Label::internal("sel_done", done_suffix);
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Ne,
            target: then_label.clone(),
        });
        // Else branch: load else_val into the reserved scratch xmm15.
        self.emit_fp_move(else_val, XmmReg::Xmm15, size);
        self.push_lir(X86Inst::Jmp {
            target: done_label.clone(),
        });
        // Then branch: load then_val into xmm15.
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(then_label)));
        self.emit_fp_move(then_val, XmmReg::Xmm15, size);
        // Done: move xmm15 → dst.
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
        self.emit_fp_move_from_xmm(XmmReg::Xmm15, &dst_loc, size);
    }

    /// Emit a `long double` select: the chosen operand is `fldt`-loaded on
    /// either side of a branch and `fstpt`-stored once. An x87 value lives in
    /// memory and has no XMM form, so the XMM path's `movt` was no
    /// instruction at all: bash's `seq` builtin, `if (ret == -0.0) ret =
    /// 0.0;` if-converted, failed to assemble. `fldt` changes no flags, so
    /// the one test serves the branch.
    fn emit_x87_select(&mut self, ops: SelectOperands) {
        let SelectOperands {
            cond,
            then_val,
            else_val,
            target,
            ..
        } = ops;
        let dst = self.get_x87_mem_addr(target);
        if let Loc::Imm(v) = self.get_location(cond) {
            let src = self.get_x87_mem_addr(if v != 0 { then_val } else { else_val });
            self.push_lir(X86Inst::X87Load { addr: src });
            self.push_lir(X86Inst::X87Store { addr: dst });
            return;
        }
        self.emit_condition_test(cond, Reg::R11);
        let then_label = Label::internal("sel_then", self.unique_label_counter);
        let done_label = Label::internal("sel_done", self.unique_label_counter + 1);
        self.unique_label_counter += 2;
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Ne,
            target: then_label.clone(),
        });
        let else_addr = self.get_x87_mem_addr(else_val);
        self.push_lir(X86Inst::X87Load { addr: else_addr });
        self.push_lir(X86Inst::Jmp {
            target: done_label.clone(),
        });
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(then_label)));
        let then_addr = self.get_x87_mem_addr(then_val);
        self.push_lir(X86Inst::X87Load { addr: then_addr });
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
        self.push_lir(X86Inst::X87Store { addr: dst });
    }

    /// Emit integer select using CMOVcc
    fn emit_select_int(&mut self, ops: SelectOperands) {
        let SelectOperands {
            cond,
            then_val,
            else_val,
            target,
            width: size,
        } = ops;
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
            _ => self.emit_condition_test(cond, Reg::R11),
        }

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

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, size);
        }
    }

    // Inline Assembly Support

    // Atomic Operations (C11 _Atomic support)
}

// AsmOperandFormatter trait implementation

impl crate::arch::AsmOperandFormatter for X86_64CodeGen {
    type Reg = Reg;

    /// gcc's x86 operand modifiers, those real code uses: the register widths
    /// `b`/`w`/`k`/`q` and the high byte `h`; `c`, `P` and `p`, a constant or
    /// symbol without its `$`; `a`, an operand as an address; `V`, a register
    /// without its `%`; `z`, the instruction suffix for the operand's size
    /// (`mov%z0`); and `x`/`t`/`g`, a vector register named as its XMM, YMM
    /// or ZMM form. A width or `P` leaves anything that is not a general
    /// register as a bare `%0` prints it, as gcc does.
    fn format_operand(
        &self,
        slot: &AsmOperandSlot<Reg>,
        modifier: Option<char>,
    ) -> Result<String, AsmModifierError> {
        use AsmOperandValue as V;
        Ok(match (modifier, &slot.value) {
            (Some(m @ ('b' | 'w' | 'k' | 'q')), V::Reg(r)) => {
                format!("%{}", self.sized_reg_name(*r, m))
            }
            (Some('h'), V::Reg(r)) => match r {
                Reg::Rax => "%ah".to_string(),
                Reg::Rbx => "%bh".to_string(),
                Reg::Rcx => "%ch".to_string(),
                Reg::Rdx => "%dh".to_string(),
                // No high byte: left for the assembler to reject, as gcc
                // rejects it.
                _ => format!("%{}h", self.reg_name_64(*r)),
            },
            (Some('P' | 'p'), V::Int(v)) => v.to_string(),
            (Some('P' | 'p'), V::Symbol(sym)) => sym.clone(),
            (Some('z'), _) => match slot.size {
                8 => "b",
                16 => "w",
                32 => "l",
                64 => "q",
                _ => return Err(AsmModifierError::Inapplicable),
            }
            .to_string(),
            (Some(m @ ('x' | 't' | 'g')), V::RegName(text)) => {
                let width = match m {
                    'x' => "xmm",
                    't' => "ymm",
                    _ => "zmm",
                };
                match ["%xmm", "%ymm", "%zmm"]
                    .iter()
                    .find_map(|prefix| text.strip_prefix(prefix))
                {
                    Some(number) => format!("%{width}{number}"),
                    None => return Err(AsmModifierError::Inapplicable),
                }
            }
            (Some('x' | 't' | 'g'), _) => return Err(AsmModifierError::Inapplicable),
            (Some('a'), V::Reg(r)) => format!("(%{})", self.reg_name_64(*r)),
            (Some('a'), V::Int(v)) => v.to_string(),
            (Some('a'), V::Symbol(sym)) => format!("{sym}(%rip)"),
            (Some('V'), V::Reg(r)) => self.asm_default_reg(*r, slot.size),
            (Some('a'), _) | (Some('V'), V::RegName(_)) => {
                return Err(AsmModifierError::Inapplicable)
            }
            (None | Some('b' | 'w' | 'k' | 'q' | 'h' | 'P' | 'p' | 'V'), value) => match value {
                V::Reg(r) => format!("%{}", self.asm_default_reg(*r, slot.size)),
                V::RegName(text) | V::Mem(text) => text.clone(),
                V::Int(v) => format!("${v}"),
                V::Float(v, bits) => format!("${}", v.to_bits_at_width(*bits)),
                V::Symbol(sym) => format!("${sym}"),
            },
            _ => return Err(AsmModifierError::Unsupported),
        })
    }

    fn asm_dialects(&self) -> bool {
        crate::arch::asm_dialects(self.base.target.arch)
    }
}

impl X86_64CodeGen {
    /// A general register at the operand's width, without the `%`.
    fn asm_default_reg(&self, reg: Reg, size_bits: u32) -> String {
        let size_mod = match size_bits {
            8 => 'b',
            16 => 'w',
            64 => 'q',
            _ => 'k', // 32-bit default
        };
        self.sized_reg_name(reg, size_mod).to_string()
    }
}

// CodeGenerator trait implementation

impl CodeGenerator for X86_64CodeGen {
    fn generate(&mut self, module: &Module, types: &TypeTable) -> String {
        self.base.output.clear();
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

        // The definitions, in runs between the file-scope asm statements:
        // each run's globals, then its functions, then the asm after it.
        for run in 0..=module.toplevel_asm.len() {
            for global in module.globals.iter().filter(|g| g.asm_before == run) {
                self.emit_global(global, types);
            }
            if run == 0 {
                self.base.emit_unit_data(module);
            }
            // An inline definition is kept in the module so the inliner can
            // use it, but provides no external definition -- see `Function::emit`.
            for func in module
                .functions
                .iter()
                .filter(|f| f.emit && f.asm_before == run)
            {
                self.emit_function(func, types);
            }
            self.base.emit_toplevel_asm(module, run);
        }

        self.base.emit_text_end(module);

        // Emit the constructor / destructor pointer arrays
        self.base.emit_init_arrays(&module.functions);

        // Emit long double constants collected during codegen
        if !self.ld_constants.is_empty() {
            self.emit_ld_constants();
        }

        // Emit binary128 constants collected during codegen. Its own
        // condition: a translation unit can use `__float128` without ever
        // mentioning `long double`.
        if !self.quad_constants.is_empty() {
            self.emit_quad_constants();
        }

        // Emit double constants collected during x87 conversions
        if !self.double_constants.is_empty() {
            self.emit_double_constants();
        }

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
            let fns = std::mem::take(&mut self.base.fn_dies);
            let unit = super::super::dwarf::UnitInfo {
                producer: &producer,
                source_name,
                comp_dir,
                low_pc_label: low_pc,
                high_pc_label: high_pc,
            };
            super::super::dwarf::generate_debug_info(&mut self.base, &unit, &fns, types);
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

    fn set_tls_policy(&mut self, tls: crate::target::TlsPolicy) {
        self.base.tls = tls;
    }

    fn set_verbose_asm(&mut self, verbose: bool) {
        self.base.verbose_asm = verbose;
    }

    fn set_cf_protection(&mut self, cf_protection: crate::target::CfProtection) {
        self.base.cf_protection = cf_protection;
    }

    fn set_stack_clash(&mut self, on: bool) {
        self.base.stack_clash = on;
    }

    fn set_stack_protector(&mut self, level: crate::target::StackProtector) {
        self.base.stack_protector = level;
    }
}
