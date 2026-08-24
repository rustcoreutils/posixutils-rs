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

use crate::arch::codegen::{BswapSize, CodeGenBase, CodeGenerator, UnaryOp};
use crate::arch::lir::{CondCode, Directive, FpSize, Label, OperandSize, Symbol};
use crate::arch::x86_64::lir::{GpOperand, MemAddr, X86Inst, XmmOperand};
use crate::arch::x86_64::regalloc::{FrameBase, Loc, Reg, XmmReg};
use crate::ir::{Instruction, Module, Opcode, Pseudo, PseudoId, PseudoKind};
use crate::target::{Os, Target};
use crate::types::{TypeKind, TypeTable};
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
    pub(super) pseudos: Vec<Pseudo>,
    /// Callee-saved registers used in current function (for epilogue)
    pub(super) callee_saved_regs: Vec<Reg>,
    /// Offset to add to stack locations to account for callee-saved registers
    pub(super) callee_saved_offset: i32,
    /// Stack allocation size (for zero_stack_frame)
    pub(super) stack_alloc_size: i32,
    /// Offset from rbp to register save area (for variadic functions)
    pub(super) reg_save_area_offset: i32,
    /// Number of fixed GP parameters (for variadic functions)
    pub(super) num_fixed_gp_params: usize,
    /// Number of fixed FP parameters (for variadic functions)
    pub(super) num_fixed_fp_params: usize,
    /// Number of fixed parameters passed on the stack (overflow beyond registers)
    pub(super) num_fixed_stack_params: usize,
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
    /// Sym pseudo ID → type size in bits (for distinguishing scalar vs struct stores)
    pub(super) sym_type_sizes: HashMap<PseudoId, u32>,
    /// How this function's locals are addressed.
    pub(super) frame_base: FrameBase,
    /// Maximum local alignment (for andq in prologue)
    pub(super) max_local_align: i32,
    /// Pseudos that are 128-bit integers (need full 16-byte copies)
    pub(super) int128_pseudos: HashSet<PseudoId>,
}

impl X86_64CodeGen {
    pub fn new(target: Target) -> Self {
        Self {
            base: CodeGenBase::new(target),
            locations: crate::arch::regalloc::LocationMap::new(),
            pseudos: Vec::new(),
            callee_saved_regs: Vec::new(),
            callee_saved_offset: 0,
            stack_alloc_size: 0,
            reg_save_area_offset: 0,
            num_fixed_gp_params: 0,
            num_fixed_fp_params: 0,
            num_fixed_stack_params: 0,
            unique_label_counter: 0,
            extern_symbols: HashSet::new(),
            tls_symbols: HashSet::new(),
            pic_mode: false,
            ld_constants: std::collections::BTreeMap::new(),
            double_constants: std::collections::BTreeMap::new(),
            quad_constants: std::collections::BTreeMap::new(),
            sym_type_sizes: HashMap::new(),
            frame_base: FrameBase::Rbp,
            max_local_align: 16,
            int128_pseudos: HashSet::new(),
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
                let symbol = if name.starts_with('.') {
                    Symbol::local(name.clone())
                } else {
                    Symbol::global(name.clone())
                };
                // Use TLS addressing for thread-local variables (Linux only)
                if self.tls_symbols.contains(name) && self.base.target.os == Os::Linux {
                    GpOperand::Mem(MemAddr::TlsLocalExec(symbol))
                } else {
                    // Note: For GOT access (PIC mode/external symbols), special handling
                    // is needed - see emit_global_load* and emit_global_store* functions
                    // which generate the two-instruction GOT sequence
                    GpOperand::Mem(MemAddr::RipRelative(symbol))
                }
            }
        }
    }

    /// Whether `name` is a thread-local this backend must access through the
    /// FS segment. TLS lowering here is Linux-only; the other targets fall
    /// through to ordinary global access.
    fn is_tls_symbol(&self, name: &str) -> bool {
        self.tls_symbols.contains(name) && self.base.target.os == Os::Linux
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
        // Local symbols (starting with '.') don't need GOT access since
        // they can't be interposed.
        if self.pic_mode && !name.starts_with('.') {
            return true;
        }
        // External symbols need GOT access on macOS for dynamic linking.
        if self.base.target.os == Os::MacOS {
            return self.extern_symbols.contains(name);
        }
        false
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
            let label = format!(".Lquad_const_{}", key);
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

    pub(super) fn emit_block(&mut self, block: &crate::ir::BasicBlock, types: &TypeTable) {
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

    /// Emit conditional branch: test condition and branch accordingly
    /// Returns true if an early return was taken (for constant conditions)
    fn emit_cbr(&mut self, insn: &Instruction, types: &TypeTable) -> bool {
        let Some(&cond) = insn.src.first() else {
            return false;
        };

        let loc = self.get_location(cond);
        // Derive size from type when available, falling back to 64-bit
        // when size is unset (0) to avoid truncating 64-bit condition values.
        let size = insn
            .typ
            .map(|t| types.size_bits(t).max(32))
            .unwrap_or(if insn.size == 0 {
                64
            } else {
                insn.size.max(32)
            });

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
                        target: Label::new(&self.base.current_fn, target.0),
                    });
                }
                if let Some(target) = insn.bb_false {
                    self.push_lir(X86Inst::Jmp {
                        target: Label::new(&self.base.current_fn, target.0),
                    });
                }
                return false;
            }
        }

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
                self.push_lir(X86Inst::Cmp {
                    size: op_size,
                    src: GpOperand::Imm(0),
                    dst: GpOperand::Mem(self.stack_mem(*offset)),
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
                let fp_size = if size <= 32 {
                    FpSize::Single
                } else {
                    FpSize::Double
                };
                self.push_lir(X86Inst::XorpsSelf { reg: XmmReg::Xmm15 });
                self.push_lir(X86Inst::UComiFp {
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

        for (lo, hi, target_bb) in insn.switch_cases.clone() {
            let target = Label::new(&self.base.current_fn, target_bb.0);
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
        if let Some(default_bb) = insn.switch_default {
            // LIR: unconditional jump to default
            self.push_lir(X86Inst::Jmp {
                target: Label::new(&self.base.current_fn, default_bb.0),
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
            if let Some(pseudo) = self.pseudos.iter().find(|p| p.id == target) {
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
                        target: Label::new(&self.base.current_fn, target.0),
                    });
                }
            }

            Opcode::Cbr => if self.emit_cbr(insn, types) {},

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
                self.emit_extend(insn);
            }

            // Variadic function support (va_* builtins)
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

            Opcode::Fabs32 => {
                self.emit_fabs32(insn);
            }

            Opcode::Fabs64 => {
                self.emit_fabs64(insn);
            }

            Opcode::Signbit32 => {
                self.emit_signbit32(insn);
            }

            Opcode::Signbit64 => {
                self.emit_signbit64(insn);
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
            Opcode::Setjmp => {
                self.emit_setjmp(insn);
            }

            Opcode::Longjmp => {
                self.emit_longjmp(insn);
            }

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
        self.cleanup_call_stack(stack_args);

        // Handle return value
        self.handle_call_return_value(insn, types);
    }

    /// Emit a select (ternary) instruction using CMOVcc (integers) or
    /// conditional branch (floats, since CMov only works on GP registers).
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
            || matches!(self.get_location(then_val), Loc::Xmm(_) | Loc::FImm(..))
            || matches!(self.get_location(else_val), Loc::Xmm(_) | Loc::FImm(..));

        if is_fp {
            let fmt = self.fp_format(insn.typ, size, types);
            self.emit_select_fp(cond, then_val, else_val, target, fmt);
        } else {
            self.emit_select_int(cond, then_val, else_val, target, insn, types, size);
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
    fn emit_select_fp(
        &mut self,
        cond: PseudoId,
        then_val: PseudoId,
        else_val: PseudoId,
        target: PseudoId,
        size: FpSize,
    ) {
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
            Loc::Stack(offset) => {
                // Reload directly from stack (safe from clobber)
                self.push_lir(X86Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Mem(self.stack_mem(*offset)),
                    dst: GpOperand::Reg(Reg::R11),
                });
            }
            _ => {
                // For other locations (Reg, Global), use emit_move
                self.emit_move(cond, Reg::R11, 64);
            }
        }
        self.push_lir(X86Inst::Test {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst: GpOperand::Reg(Reg::R11),
        });

        // Branch: load else_val, skip over then_val load if condition is false
        let then_suffix = self.unique_label_counter;
        self.unique_label_counter += 1;
        let done_suffix = self.unique_label_counter;
        self.unique_label_counter += 1;
        let then_label = Label::new("sel_then", then_suffix);
        let done_label = Label::new("sel_done", done_suffix);
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

    /// Emit integer select using CMOVcc
    #[allow(clippy::too_many_arguments)]
    fn emit_select_int(
        &mut self,
        cond: PseudoId,
        then_val: PseudoId,
        else_val: PseudoId,
        target: PseudoId,
        insn: &Instruction,
        _types: &TypeTable,
        size: u32,
    ) {
        let _ = insn;
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
                self.push_lir(X86Inst::Test {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::R11),
                    dst: GpOperand::Reg(Reg::R11),
                });
            }
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

    fn size_modifiers(&self) -> &'static [char] {
        &['b', 'w', 'k', 'q'] // 8, 16, 32, 64-bit
    }

    fn format_reg_sized(&self, reg: Reg, size_mod: char) -> String {
        format!("%{}", self.sized_reg_name(reg, size_mod))
    }

    fn format_reg_default(&self, reg: Reg, size_bits: u32) -> String {
        // Select register width matching the operand's declared size
        let size_mod = match size_bits {
            8 => 'b',
            16 => 'w',
            64 => 'q',
            _ => 'k', // 32-bit default
        };
        format!("%{}", self.sized_reg_name(reg, size_mod))
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
            self.push_lir(X86Inst::Directive(Directive::Text));
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

    fn set_shared_mode(&mut self, shared: bool) {
        self.base.shared_mode = shared;
    }

    fn set_verbose_asm(&mut self, verbose: bool) {
        self.base.verbose_asm = verbose;
    }
}
