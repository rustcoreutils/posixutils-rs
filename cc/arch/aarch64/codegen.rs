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

use crate::arch::aarch64::inline_asm::{asm_reg_name_32, asm_reg_name_64};
use crate::arch::aarch64::lir::{Aarch64Inst, GpOperand, MemAddr};
use crate::arch::aarch64::regalloc::{FrameBase, IncomingOff, Loc, LocalSlot, Reg, VReg};
use crate::arch::codegen::SelectOperands;
use crate::arch::codegen::{AsmModifierError, AsmOperandSlot, AsmOperandValue};
use crate::arch::codegen::{BswapSize, CodeGenBase, CodeGenerator, UnaryOp};
use crate::arch::lir::{CondCode, Directive, FpSize, Label, OperandSize, Symbol};
use crate::float::FloatVal;
use crate::ir::{Instruction, Module, Opcode, PseudoId, PseudoKind};
use crate::parse::ast::JmpKind;
use crate::target::{Os, Target};
use crate::types::{TypeId, TypeTable};
use std::collections::{HashMap, HashSet};

// AArch64 Code Generator

/// AArch64 code generator
pub struct Aarch64CodeGen {
    /// Common code generation infrastructure
    pub(super) base: CodeGenBase<Aarch64Inst>,
    /// Current function's register allocation. Every PseudoId → Loc
    /// lookup goes through `LocationMap`, so codegen never derives an
    /// alternative location from `PseudoKind`. The intrinsic-result
    /// sites that write into the map are the only post-allocate writers
    /// and remain visible as `.set` calls.
    pub(super) locations: crate::arch::regalloc::LocationMap<Loc>,
    /// Current function's pseudos (for looking up values)
    pub(super) pseudos: crate::arch::codegen::PseudoTable,
    /// Total frame size for current function
    pub(super) frame_size: i32,
    /// Size of callee-saved register area (for computing local variable offsets)
    pub(super) callee_saved_size: i32,
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
    /// Where the pre-call `%sp` was saved, for a Darwin variadic call whose
    /// argument area needs more than the sixteen bytes `%sp` is guaranteed.
    ///
    /// Such a call realigns `%sp` by an amount only known at run time, so the
    /// area cannot be released with the matching `add` every other call uses;
    /// the old value is read back from this offset instead.
    pub(super) darwin_va_sp_slot: Option<i32>,
    /// External symbols (need GOT access on macOS)
    pub(super) extern_symbols: HashSet<String>,
    /// Thread-local storage symbols (need TLS access)
    pub(super) tls_symbols: HashSet<String>,
    /// Every data symbol's known alignment in bytes -- definitions here and
    /// `extern` declarations -- for [`Self::lo12_folds`].
    pub(super) sym_align: HashMap<String, u32>,
    /// Position-independent code mode (for shared libraries)
    pic_mode: bool,
    /// Counter for generating unique labels (atomic loops, etc.)
    unique_label_counter: u32,
    /// Bytes the prologue allocates for locals: what a dynamically aligned
    /// frame addresses them from.
    pub(super) stack_alloc_size: i32,
    /// Sym pseudo ID → type size in bits (for distinguishing scalar vs struct stores)
    pub(super) sym_slots: HashMap<PseudoId, crate::arch::codegen::SymSlot>,
    /// Which register this function's locals are addressed through
    pub(super) frame_base: FrameBase,
}

impl Aarch64CodeGen {
    pub fn new(target: Target) -> Self {
        Self {
            base: CodeGenBase::new(target),
            locations: crate::arch::regalloc::LocationMap::new(),
            pseudos: Default::default(),
            frame_size: 0,
            callee_saved_size: 0,
            reg_save_area_offset: 0,
            reg_save_area_size: 0,
            num_fixed_gp_params: 0,
            num_fixed_fp_params: 0,
            named_stack_param_bytes: 0,
            darwin_va_sp_slot: None,
            extern_symbols: HashSet::new(),
            tls_symbols: HashSet::new(),
            sym_align: HashMap::new(),
            pic_mode: false,
            unique_label_counter: 0,
            stack_alloc_size: 0,
            sym_slots: HashMap::new(),
            frame_base: FrameBase::Fp,
        }
    }

    /// Check if a symbol needs GOT access
    /// - In PIC mode: all external symbols need GOT access
    /// - On macOS: external symbols always need GOT access (even without PIC)
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
    fn stack_offset(&self, offset: i32) -> i32 {
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
    /// re-derived here.
    fn stack_base_reg(&self, raw_offset: i32) -> Reg {
        match self.frame_base.reg() {
            Some(base) if raw_offset < 0 => base,
            _ => Reg::X29,
        }
    }

    fn raw_mem(&self, raw_offset: i32, extra: i32) -> MemAddr {
        MemAddr::BaseOffset {
            base: self.stack_base_reg(raw_offset),
            offset: self.stack_offset(raw_offset) + extra,
        }
    }

    /// The address of a slot in the callee's own frame.
    pub(super) fn stack_mem(&self, slot: LocalSlot) -> MemAddr {
        self.raw_mem(slot.displacement(), 0)
    }

    /// The address of an incoming stack argument, in the caller's frame.
    fn incoming_mem(&self, off: IncomingOff) -> MemAddr {
        self.raw_mem(off.displacement(), 0)
    }

    /// The address of byte `byte` of the object in callee-frame slot `slot`.
    pub(super) fn stack_mem_plus(&self, slot: LocalSlot, extra: i32) -> MemAddr {
        self.raw_mem(slot.displacement(), extra)
    }

    /// The address of byte `byte` of an incoming stack argument.
    pub(super) fn incoming_mem_plus(&self, off: IncomingOff, extra: i32) -> MemAddr {
        self.raw_mem(off.displacement(), extra)
    }

    /// The address of byte `byte` of the object in stack slot `slot`.
    pub(super) fn stack_field(&self, slot: LocalSlot, byte: i32) -> MemAddr {
        self.stack_mem_plus(slot, byte)
    }

    /// The base register and displacement addressing whatever frame `loc`
    /// names, for the sites that materialize an address with an `add` rather
    /// than folding it into a load or store.
    pub(super) fn loc_addr_parts(&self, loc: &Loc) -> Option<(Reg, i32)> {
        let raw = match loc {
            Loc::Stack(slot) => slot.displacement(),
            Loc::IncomingArg(off) => off.displacement(),
            _ => return None,
        };
        Some((self.stack_base_reg(raw), self.stack_offset(raw)))
    }

    /// [`Self::loc_mem`] plus a byte offset into the object.
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
    pub(super) fn loc_mem(&self, loc: &Loc) -> Option<MemAddr> {
        match loc {
            Loc::Stack(slot) => Some(self.stack_mem(*slot)),
            Loc::IncomingArg(off) => Some(self.incoming_mem(*off)),
            _ => None,
        }
    }

    /// Push a LIR instruction to the buffer (deferred emission).
    ///
    /// Every instruction passes through [`super::legalize::legalize`], so an
    /// offset or immediate no encoding can hold is expanded here, once, rather
    /// than by each site that builds one.
    pub(super) fn push_lir(&mut self, inst: Aarch64Inst) {
        let mut out = Vec::with_capacity(1);
        if let Err(super::legalize::Unencodable(what)) = super::legalize::legalize(inst, &mut out) {
            crate::diag::error_args(
                self.base.func_pos,
                "internal error: c17 built {0}, which no aarch64 instruction encodes",
                &[what],
            );
        }
        for inst in out {
            self.base.push_lir(inst);
        }
    }

    /// Emit .loc directive for source line tracking (delegates to base)
    fn emit_loc(&mut self, insn: &Instruction) {
        self.base.emit_loc(insn);
    }

    /// Emit file header, declaring the ISA extensions this backend uses.
    ///
    /// `_Float16` is lowered to native half-precision instructions (`fmov h0`,
    /// `fadd h0, h1, h2`), which are an ARMv8.2-A extension. GNU as defaults to
    /// plain armv8-a and rejects every one of them with "selected processor
    /// does not support", so any translation unit touching `_Float16` failed to
    /// assemble on Linux. Apple's assembler enables fp16 for its own targets.
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
    fn emit_global(&mut self, global: &crate::ir::GlobalDef, types: &TypeTable) {
        // Skip extern symbols - they're defined elsewhere
        if self.extern_symbols.contains(&global.name) {
            return;
        }
        self.base.emit_global(global, types);
    }

    pub(super) fn emit_block(
        &mut self,
        block: &crate::ir::BasicBlock,
        frame_info: &(i32, Vec<Reg>, Vec<VReg>),
        types: &TypeTable,
    ) {
        // Always emit block ID label for consistency with jumps
        // (jumps reference blocks by ID, not by C label name)
        self.push_lir(Aarch64Inst::Directive(Directive::BlockLabel(Label::block(
            &self.base.current_fn,
            block.id.0,
        ))));

        // Emit instructions
        for insn in &block.insns {
            self.emit_insn(insn, frame_info, types);
        }
    }

    /// Emit conditional branch: test condition and branch accordingly
    /// Returns true if an early return was taken (for constant conditions)
    /// Set the flags for `cond != 0`, reading `cond` at the width its value
    /// is held at -- see [`crate::arch::codegen::ValueWidths`] -- through
    /// `scratch` where it is not already in a register. The one test `Cbr`
    /// and `Select` both make. A wider-than-64-bit condition also uses X10,
    /// or X11 when `scratch` is X10, so neither may hold anything live.
    fn emit_condition_test(&mut self, cond: PseudoId, scratch: Reg) {
        let bits = self.base.value_widths.bits(cond);
        let loc = self.get_location(cond);
        if bits > 64 {
            if let l @ (Loc::Stack(_) | Loc::IncomingArg(_)) = &loc {
                // Both halves, ORed: nonzero if either is.
                let other = if scratch == Reg::X10 {
                    Reg::X11
                } else {
                    Reg::X10
                };
                self.push_lir(Aarch64Inst::Ldp {
                    size: OperandSize::B64,
                    addr: self.loc_mem(l).unwrap(),
                    dst1: scratch,
                    dst2: other,
                });
                self.push_lir(Aarch64Inst::Orr {
                    size: OperandSize::B64,
                    src1: scratch,
                    src2: GpOperand::Reg(other),
                    dst: scratch,
                });
                self.push_lir(Aarch64Inst::Cmp {
                    size: OperandSize::B64,
                    src1: scratch,
                    src2: GpOperand::Imm(0),
                });
                return;
            }
        }
        let bits = bits.min(64);
        let reg = match loc {
            Loc::Reg(r) if bits >= 32 => r,
            _ => {
                self.emit_move(cond, scratch, bits);
                // A register keeps whatever lies above a narrow value.
                match bits {
                    8 => self.push_lir(Aarch64Inst::Uxtb {
                        src: scratch,
                        dst: scratch,
                    }),
                    16 => self.push_lir(Aarch64Inst::Uxth {
                        src: scratch,
                        dst: scratch,
                    }),
                    _ => {}
                }
                scratch
            }
        };
        self.push_lir(Aarch64Inst::Cmp {
            size: if bits <= 32 {
                OperandSize::B32
            } else {
                OperandSize::B64
            },
            src1: reg,
            src2: GpOperand::Imm(0),
        });
    }

    fn emit_cbr(&mut self, insn: &Instruction) -> bool {
        let Some(&cond) = insn.src.first() else {
            return false;
        };

        let loc = self.get_location(cond);
        let (scratch0, _, _) = Reg::scratch_regs();

        match &loc {
            Loc::Reg(_) | Loc::Stack(_) | Loc::IncomingArg(_) | Loc::Global(_) => {
                self.emit_condition_test(cond, scratch0);
            }
            Loc::Imm(v) => {
                let target = if *v != 0 { insn.bb_true } else { insn.bb_false };
                if let Some(target) = target {
                    self.push_lir(Aarch64Inst::B {
                        target: Label::block(&self.base.current_fn, target.0),
                    });
                }
                return true;
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
                        target: Label::block(&self.base.current_fn, target.0),
                    });
                }
                return true;
            }
        }

        if let Some(target) = insn.bb_true {
            self.push_lir(Aarch64Inst::BCond {
                cond: CondCode::Ne,
                target: Label::block(&self.base.current_fn, target.0),
            });
        }
        if let Some(target) = insn.bb_false {
            self.push_lir(Aarch64Inst::B {
                target: Label::block(&self.base.current_fn, target.0),
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
        for (lo, hi, target_bb) in insn.extra().switch_cases.clone() {
            let target = Label::block(&self.base.current_fn, target_bb.0);
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

        if let Some(default_bb) = insn.extra().switch_default {
            self.push_lir(Aarch64Inst::B {
                target: Label::block(&self.base.current_fn, default_bb.0),
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
                        target: Label::block(&self.base.current_fn, target.0),
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

            op if op.is_int_comparison() => {
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
                    if let Some(pseudo) = self.pseudos.get(target) {
                        match self.locations.get(target) {
                            Some(Loc::Reg(r)) => {
                                if let PseudoKind::Val(v) = &pseudo.kind {
                                    self.emit_mov_imm(r, *v as i64, insn.size);
                                }
                            }
                            Some(Loc::VReg(v)) => {
                                if let PseudoKind::FVal(f) = &pseudo.kind {
                                    // The constant at its type's precision. A
                                    // `long double` fell through to `double`
                                    // here and was loaded as a double's bits
                                    // into a binary128 register.
                                    let fp_size =
                                        self.fp_size_from_type(insn.typ, insn.size, types);
                                    let (lo, hi) = super::float::fp_const_bits(*f, fp_size);
                                    self.emit_fp_bits(lo, hi, fp_size, v);
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
                                src2: GpOperand::Imm(adjusted.into()),
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

            Opcode::Simd(op) => self.emit_simd(insn, op, types),

            Opcode::Select => {
                self.emit_select(insn, types);
            }

            Opcode::Zext | Opcode::Sext | Opcode::Trunc => {
                self.emit_extend(insn, types);
            }

            // Floating-point arithmetic operations
            Opcode::FAdd
            | Opcode::FSub
            | Opcode::FMul
            | Opcode::FDiv
            | Opcode::FMin
            | Opcode::FMax => {
                self.emit_fp_binop(insn, types);
            }

            // Floating-point negation
            Opcode::FNeg => {
                self.emit_fp_neg(insn, types);
            }

            // Floating-point comparisons
            op if op.is_float_comparison() => {
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

            // Variadic function support (va_* builtins)
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

            Opcode::Fabs => self.emit_fp_abs(insn, types),

            Opcode::CopySign => self.emit_fp_copysign(insn, types),
            Opcode::Sqrt => self.emit_fp_sqrt(insn, types),
            Opcode::RoundToIntegral(how) => self.emit_fp_round_to_integral(insn, how, types),
            Opcode::Fma => self.emit_fp_fma(insn, types),
            Opcode::Signbit => self.emit_fp_signbit(insn, types),

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

            // setjmp/longjmp support
            Opcode::Setjmp => match insn.jmp_kind() {
                JmpKind::Library => self.emit_setjmp(insn),
                JmpKind::Builtin => self.emit_builtin_setjmp(insn),
            },

            Opcode::Longjmp => match insn.jmp_kind() {
                JmpKind::Library => self.emit_longjmp(insn),
                JmpKind::Builtin => self.emit_builtin_longjmp(insn),
            },

            // Inline Assembly
            Opcode::Asm => {
                self.emit_inline_asm(insn);
            }

            // Atomic Operations
            Opcode::AtomicLoad => {
                self.emit_atomic_load(insn);
            }

            Opcode::AtomicStore => {
                self.emit_atomic_store(insn);
            }

            Opcode::AtomicCas => {
                self.emit_atomic_cas(insn);
            }

            Opcode::AtomicSwap
            | Opcode::AtomicFetchAdd
            | Opcode::AtomicFetchSub
            | Opcode::AtomicFetchAnd
            | Opcode::AtomicFetchOr
            | Opcode::AtomicFetchXor => {
                self.emit_atomic_rmw(insn);
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

            // The memory builtins are a call to the libc function of the same
            // name. x86-64 lowers these in its own features.rs; aarch64 did
            // not lower them at all, and the `_ => {}` below meant the
            // instruction produced nothing rather than failing -- so
            // `__builtin_memcpy` silently copied nothing on every aarch64
            // build.
            Opcode::Memcpy => self.emit_mem_libcall(insn),
            Opcode::Memmove => self.emit_mem_libcall(insn),
            Opcode::Memset => self.emit_mem_libcall(insn),

            // Skip no-ops and unimplemented
            _ => {}
        }
    }

    pub(super) fn get_location(&self, pseudo: PseudoId) -> Loc {
        self.locations.get(pseudo).unwrap_or(Loc::Imm(0))
    }

    /// Whether accessing the thread-local `name` needs the Initial Exec model
    /// rather than Local Exec. See [`CodeGenBase::use_tls_ie`].
    pub(super) fn use_tls_ie(&self, name: &str) -> bool {
        self.base.use_tls_ie(self.extern_symbols.contains(name))
    }

    /// Emit TLS address computation into dst register.
    /// After this call, dst holds the address of the TLS variable.
    pub(super) fn emit_tls_addr(&mut self, name: &str, dst: Reg) {
        let sym = Symbol::global(name);
        if self.base.tls_access() == crate::target::TlsAccess::MachOTlv {
            // Mach-O thread-local variable descriptor (clang's sequence):
            //   adrp  x0, _v@TLVPPAGE
            //   ldr   x0, [x0, _v@TLVPPAGEOFF]    ; the descriptor
            //   ldr   x16, [x0]                   ; its getter
            //   blr   x16                         ; returns the ADDRESS in x0
            //
            // The getter preserves every register but x0, x16, x17 and the
            // link register -- LLVM's `CSR_Darwin_AArch64_TLS` keeps x1-x28
            // and q0-q31 -- so x0 is the one allocatable register it
            // clobbers, which `get_constraint_info_aarch64` declares.
            // X16/X17 are never allocated and LR is saved by every prologue.
            //
            // Reached only through a `TlsAddr`: `ir::tls` rewrites every
            // Darwin thread-local reference into one, so the allocator sees
            // the call.
            self.push_lir(Aarch64Inst::AdrpTlvpPage {
                sym: sym.clone(),
                dst: Reg::X0,
            });
            self.push_lir(Aarch64Inst::LdrTlvpPageOff {
                sym,
                base: Reg::X0,
                dst: Reg::X0,
            });
            self.push_lir(Aarch64Inst::Ldr {
                size: OperandSize::B64,
                addr: MemAddr::Base(Reg::X0),
                dst: Reg::X16,
            });
            self.push_lir(Aarch64Inst::Blr { reg: Reg::X16 });
            if dst != Reg::X0 {
                self.push_lir(Aarch64Inst::Mov {
                    size: OperandSize::B64,
                    src: GpOperand::Reg(Reg::X0),
                    dst,
                });
            }
            return;
        }
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
            //   adrp  dst, :gottprel:sym
            //   ldr   dst, [dst, #:gottprel_lo12:sym]
            //   mrs   tmp, tpidr_el0
            //   add   dst, tmp, dst
            //
            // The thread pointer needs a register of its own, and no scratch
            // the callers use is free: a store passes X16 as `dst` with the
            // value in X9, and a struct copy computes one address into X17
            // while its other cursor is live in X16. Borrowing X16, as this
            // did, turned a store into `mrs x16, ...; add x16, x16, x16` --
            // twice the thread pointer, a wild write. X15 is free by
            // construction: the legalizer cannot touch it between the `mrs`
            // and the `add`, since neither is ever expanded.
            let tmp = super::legalize::LEGALIZE_REG;
            // The one caller that holds X15 across instructions, inline asm,
            // never asks this sequence for an address in X15; see
            // `OperandRegs` in inline_asm.rs.
            debug_assert_ne!(
                dst, tmp,
                "the Initial Exec sequence needs X15 as its temporary"
            );
            self.push_lir(Aarch64Inst::AdrpGottprel {
                sym: sym.clone(),
                dst,
            });
            self.push_lir(Aarch64Inst::LdrGottprelLo12 {
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

        // Handle sret (hidden struct return pointer) argument
        let args_start = self.setup_sret_arg(insn);

        // Determine if this is a Darwin variadic call
        let is_darwin_variadic = self.base.target.os == crate::target::Os::MacOS
            && insn.extra().variadic_arg_start.is_some();

        // Set up arguments and get stack cleanup count
        let stack_args = if is_darwin_variadic {
            self.setup_darwin_variadic_args(insn, args_start, types)
        } else {
            self.setup_register_args(insn, args_start, types)
        };

        // For an indirect call, load the function pointer into X16 *after*
        // the arguments are in place. X16 is AAPCS64's IP0 and this backend's
        // address-materialization scratch -- the stacked-argument copy
        // shuttles through it -- so a target parked there before the setup was
        // overwritten and the call branched into the argument data.
        if let Some(func_addr) = insn.extra().indirect_target {
            self.emit_move(func_addr, Reg::X16, 64);
        }

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
        let Some(ops) = SelectOperands::of(insn, types) else {
            return;
        };
        let (then_val, else_val) = (ops.then_val, ops.else_val);

        // Check if this is a floating-point select
        let is_fp = insn.typ.is_some_and(|t| types.is_float(t))
            || matches!(self.get_location(then_val), Loc::VReg(_) | Loc::FImm(..))
            || matches!(self.get_location(else_val), Loc::VReg(_) | Loc::FImm(..));

        if is_fp {
            self.emit_select_fp(ops, insn.typ, types);
        } else {
            self.emit_select_int(ops);
        }
    }

    /// Emit FP select using conditional branch (CSEL doesn't work on VRegs)
    fn emit_select_fp(&mut self, ops: SelectOperands, typ: Option<TypeId>, types: &TypeTable) {
        let SelectOperands {
            cond,
            then_val,
            else_val,
            target,
            width: size,
        } = ops;
        let dst_loc = self.get_location(target);

        // Check if condition is a constant
        let cond_loc = self.get_location(cond);
        if let Loc::Imm(v) = cond_loc {
            let val = if v != 0 { then_val } else { else_val };
            self.emit_fp_move(val, VReg::V17, typ, size, types);
            self.emit_fp_move_to_loc(VReg::V17, &dst_loc, typ, size, types);
            return;
        }

        self.emit_condition_test(cond, Reg::X16);

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
        // Use X16 as default scratch to avoid clobbering live values
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X16,
        };

        // The three codegen scratches, which the allocator never hands out,
        // so none can be `dst_reg` or hold anything live. This used X12 and
        // X13, which are allocatable: a value the allocator had put there was
        // overwritten by the select's operand, and read back wrong after it.
        let (cond_reg, then_reg, else_reg) = Reg::scratch_regs();

        // The test first, while the other two scratches are still free for
        // it; loading the arms after it leaves the flags alone.
        self.emit_condition_test(cond, cond_reg);
        self.emit_move(then_val, then_reg, size);
        self.emit_move(else_val, else_reg, size);

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

    // Floating-Point Operations - see float.rs

    // Inline Assembly Support

    // Atomic Operations (ARMv8.1 LSE)

    /// Generate a unique label with the given prefix
    pub(super) fn next_unique_label(&mut self, prefix: &str) -> Label {
        let id = self.unique_label_counter;
        self.unique_label_counter += 1;
        Label::internal(prefix, id)
    }
}

impl crate::arch::AsmOperandFormatter for Aarch64CodeGen {
    type Reg = Reg;

    /// gcc's aarch64 operand modifiers, those real code uses: `w`/`x`, a
    /// general register's two widths, and the zero register for a constant
    /// zero; `b`/`h`/`s`/`d`/`q`, a vector register's five; `a`, an operand
    /// as an address. A constant prints bare, with no `#`: gcc leaves that to
    /// the template, so `.word %0` assembles and `add x0, x1, %2` still does.
    fn format_operand(
        &self,
        slot: &AsmOperandSlot<Reg>,
        modifier: Option<char>,
    ) -> Result<String, AsmModifierError> {
        use AsmOperandValue as V;
        let zero = |v: &V<Reg>| match v {
            V::Int(0) => true,
            V::Float(f, _) => f.is_positive_zero(),
            _ => false,
        };
        Ok(match (modifier, &slot.value) {
            (Some('w'), V::Reg(r)) => asm_reg_name_32(*r).to_string(),
            (Some('x'), V::Reg(r)) => asm_reg_name_64(*r).to_string(),
            (Some('w'), v) if zero(v) => "wzr".to_string(),
            (Some('x'), v) if zero(v) => "xzr".to_string(),
            // A vector operand is rendered as `vN`; the modifier renames
            // the same register at the width it names.
            (Some(m @ ('b' | 'h' | 's' | 'd' | 'q')), V::RegName(name)) => {
                format!("{m}{}", &name[1..])
            }
            (Some('a'), V::Reg(r)) => format!("[{}]", asm_reg_name_64(*r)),
            (Some('a'), V::Int(v)) => v.to_string(),
            (Some('a'), V::Symbol(sym)) => sym.clone(),
            (Some('b' | 'h' | 's' | 'd' | 'q' | 'a'), _) => {
                return Err(AsmModifierError::Inapplicable)
            }
            // Unmodified, a vector register is `vN`, whatever the operand's
            // width -- gcc's spelling, which `%0.4s` and the other
            // arrangement suffixes are written after. A scalar instruction
            // names its width with a modifier, `%d0`.
            (None, V::RegName(name)) => format!("v{}", &name[1..]),
            (None | Some('w' | 'x'), value) => match value {
                V::Reg(r) if slot.size <= 32 => asm_reg_name_32(*r).to_string(),
                V::Reg(r) => asm_reg_name_64(*r).to_string(),
                V::RegName(text) | V::Mem(text) => text.clone(),
                V::Int(v) => v.to_string(),
                V::Float(v, _) => float_immediate(*v).ok_or(AsmModifierError::Unencodable(
                    "aarch64 writes only a floating constant `fmov` encodes as an immediate",
                ))?,
                V::Symbol(sym) => sym.clone(),
            },
            _ => return Err(AsmModifierError::Unsupported),
        })
    }
}

/// A floating constant as gcc writes it into an aarch64 template: `0` for
/// positive zero, else in decimal (`1.5e+0`) if `fmov` can encode it --
/// `±n/16 * 2^r`, `n` in 16..=31 and `r` in -3..=4. Anything else has no
/// immediate form.
fn float_immediate(v: FloatVal) -> Option<String> {
    if v.is_positive_zero() {
        return Some("0".to_string());
    }
    let x = v.to_f64();
    let encodable = x.is_finite()
        && (-3..=4).any(|r| {
            let n = x.abs() * 16.0 / 2f64.powi(r);
            n.fract() == 0.0 && (16.0..=31.0).contains(&n)
        });
    if !encodable {
        return None;
    }
    // gcc's spelling: a mantissa with a point, an exponent with a sign.
    let text = format!("{x:e}");
    let (mantissa, exp) = text.split_once('e')?;
    let point = if mantissa.contains('.') { "" } else { ".0" };
    let sign = if exp.starts_with('-') { "" } else { "+" };
    Some(format!("{mantissa}{point}e{sign}{exp}"))
}

// CodeGenerator trait implementation

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

        self.sym_align = module
            .globals
            .iter()
            .map(|g| {
                (
                    g.name.clone(),
                    crate::arch::codegen::global_alignment(g, types),
                )
            })
            .chain(
                module
                    .extern_object_align
                    .iter()
                    .map(|(n, a)| (n.clone(), *a)),
            )
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
        self.base.emit_symbol_aliases(module);

        // Emit string literals
        if !module.strings.is_empty() {
            self.base.emit_strings(&module.strings);
        }

        // Emit char16_t, char32_t and wchar_t string literals
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

#[cfg(test)]
mod tests {
    use super::float_immediate;
    use crate::float::FloatVal;

    /// gcc's spelling of the floating constants `fmov` encodes, and nothing
    /// for the rest.
    #[test]
    fn float_immediates() {
        let imm = |v: f64| float_immediate(FloatVal::from_f64(v));
        assert_eq!(imm(0.0).as_deref(), Some("0"));
        assert_eq!(imm(1.0).as_deref(), Some("1.0e+0"));
        assert_eq!(imm(1.5).as_deref(), Some("1.5e+0"));
        assert_eq!(imm(-0.125).as_deref(), Some("-1.25e-1"));
        assert_eq!(imm(31.0).as_deref(), Some("3.1e+1"));
        assert_eq!(imm(0.1328125).as_deref(), Some("1.328125e-1"));
        for v in [-0.0, 1.1, 32.0, 0.0625, f64::INFINITY, f64::NAN] {
            assert_eq!(imm(v), None, "{v}");
        }
    }
}
