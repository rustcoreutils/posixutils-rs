//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 x87 FPU Code Generation (Long Double / 80-bit Extended Precision)
//
// The x87 FPU uses a stack-based architecture:
// - ST(0) is the top of the stack
// - ST(1) through ST(7) are below
// - Operations implicitly use ST(0) and may push/pop the stack
//
// Since System V AMD64 ABI classifies long double as Indirect (memory-based),
// we use simple load/op/store sequences without x87 register allocation.
// All long double values reside in memory, so each operation:
// 1. Loads operand(s) from memory to x87 stack
// 2. Performs the operation
// 3. Stores result to memory and pops the stack
//

use super::codegen::X86_64CodeGen;
use super::inline_asm::x87_operands;
use super::lir::{GpOperand, MemAddr, ShiftCount, X86Inst, X87BinOp, X87IntWidth, XmmOperand};
use super::regalloc::{Loc, Reg, X87ControlWords, XmmReg};
use crate::arch::lir::{CondCode, Directive, FpSize, Label, OperandSize};
use crate::float::FpFormat;
use crate::ir::{FloatCmp, Instruction, NanCompare, Opcode, PseudoId};
use crate::types::{TypeKind, TypeTable};

/// 2^63 as the bit pattern of a `double`.
///
/// The split that converts a float to a 64-bit unsigned integer needs this
/// value in the FPU, and `fld` has no immediate form, so it is staged through
/// a general register and the x87 scratch.
const TWO_POW_63_BITS: i64 = 0x43e0_0000_0000_0000u64 as i64;

impl X86_64CodeGen {
    /// The reserved scratch address used to stage a value into the FPU.
    ///
    /// `fild`/`fld` have no register form, so an immediate or a general
    /// register has to go through memory: the function's
    /// [`super::regalloc::X87Scratch`]. Never address it by hand.
    fn x87_scratch_addr(&self) -> MemAddr {
        self.stack_mem(self.x87_scratch_slot())
    }

    fn x87_scratch_slot(&self) -> i32 {
        self.x87_scratch
            .expect("the allocator reserves the x87 scratch for every `uses_x87_scratch`")
            .slot()
    }

    /// Push an inline-asm operand onto the x87 stack.
    ///
    /// A `long double` lives in memory and is loaded from there. A `float` or
    /// `double` may be anywhere -- an XMM or general register, a constant, a
    /// slot -- and `fld` reads only memory, so it goes through the reserved
    /// XMM scratch into the x87 scratch. A constant is read at the width it is
    /// pooled at, not its type's: see `emit_fp_imm_to_xmm`.
    pub(super) fn emit_x87_asm_push(&mut self, pseudo: PseudoId, size: u32) {
        if size > 64 {
            let addr = self.get_x87_mem_addr(pseudo);
            self.push_lir(X86Inst::X87Load { addr });
            return;
        }
        let bits = match self.get_location(pseudo) {
            Loc::FImm(_, imm_bits) => imm_bits,
            _ => size,
        };
        let fp_size = if bits <= 32 {
            FpSize::Single
        } else {
            FpSize::Double
        };
        let scratch = self.x87_scratch_addr();
        self.emit_fp_move(pseudo, XmmReg::Xmm15, fp_size);
        self.push_lir(X86Inst::MovFp {
            size: fp_size,
            src: XmmOperand::Reg(XmmReg::Xmm15),
            dst: XmmOperand::Mem(scratch.clone()),
        });
        self.push_lir(match fp_size {
            FpSize::Single => X86Inst::X87LoadFloat { addr: scratch },
            _ => X86Inst::X87LoadDouble { addr: scratch },
        });
    }

    /// Pop the top of the x87 stack into an inline-asm output's home.
    ///
    /// The mirror of `emit_x87_asm_push`: a `long double` is stored to its
    /// memory, and a `float` or `double` rounded through the x87 scratch and
    /// the reserved XMM scratch into wherever the allocator put it.
    pub(super) fn emit_x87_asm_pop(&mut self, pseudo: PseudoId, size: u32) {
        if size > 64 {
            let addr = self.get_x87_mem_addr(pseudo);
            self.push_lir(X86Inst::X87Store { addr });
            return;
        }
        let fp_size = if size <= 32 {
            FpSize::Single
        } else {
            FpSize::Double
        };
        let scratch = self.x87_scratch_addr();
        self.push_lir(match fp_size {
            FpSize::Single => X86Inst::X87StoreFloat {
                addr: scratch.clone(),
            },
            _ => X86Inst::X87StoreDouble {
                addr: scratch.clone(),
            },
        });
        self.push_lir(X86Inst::MovFp {
            size: fp_size,
            src: XmmOperand::Mem(scratch),
            dst: XmmOperand::Reg(XmmReg::Xmm15),
        });
        let home = self.get_location(pseudo);
        self.emit_fp_move_from_xmm(XmmReg::Xmm15, &home, fp_size);
    }

    /// Does this instruction operate on x87 `long double` values?
    ///
    /// Asked of its *operands*: a comparison of two `long double`s produces
    /// an `int`, and is an x87 operation all the same.
    pub fn is_longdouble_op(&self, insn: &Instruction, types: &TypeTable) -> bool {
        insn.operand_width() >= 80
            && insn
                .operand_type()
                .is_some_and(|t| types.kind(t) == TypeKind::LongDouble)
    }

    /// Emit x87 load operation (Load instruction for long double)
    ///
    /// Pattern:
    ///   mov    addr, %r11      ; get address
    ///   fldt   (%r11)          ; load to ST(0)
    ///   fstpt  dst(%rbp)       ; store to destination
    pub(super) fn emit_x87_load(&mut self, insn: &Instruction) {
        let addr = match insn.src.first() {
            Some(&a) => a,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Get the address to load from
        // For Load instruction, addr is a pseudo representing a memory location
        // (either a stack local, or a pointer in a register/memory)
        let addr_loc = self.get_location(addr);
        let src_addr = match addr_loc {
            Loc::Reg(r) => {
                // addr is a pointer in a register
                MemAddr::BaseOffset {
                    base: r,
                    offset: insn.displacement(),
                }
            }
            Loc::Stack(offset) => {
                // Same distinction as the store side: a symbol's slot is the
                // variable's storage, a temp's slot holds a *pointer* to
                // storage allocated elsewhere. Loading from the slot directly
                // in the second case read the pointer bits as a float — which
                // is where the NaNs came from — and at a non-zero offset read
                // past the frame entirely.
                let is_symbol = self.pseudos.is_sym(addr);
                if is_symbol {
                    self.stack_field(offset, insn.displacement())
                } else {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(self.stack_mem(offset)),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: insn.displacement(),
                    }
                }
            }
            Loc::Global(name) => self.global_mem(&name, insn.displacement(), Reg::R11),
            _ => {
                // Load address into R11
                self.emit_move(addr, Reg::R11, 64);
                MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: insn.displacement(),
                }
            }
        };

        // Load from source to ST(0)
        self.push_lir(X86Inst::X87Load { addr: src_addr });

        // Store to destination
        let dst_addr = self.get_x87_mem_addr(target);
        self.push_lir(X86Inst::X87Store { addr: dst_addr });
    }

    /// Emit x87 store operation (Store instruction for long double)
    ///
    /// Pattern:
    ///   fldt   src(%rbp)       ; load value to ST(0)
    ///   fstpt  dst(%rbp)       ; store to destination
    /// `va_arg` for an x87 `long double`.
    ///
    /// SysV AMD64 classifies `long double` as X87/X87UP, a class that is never
    /// passed in a register: every one of them sits in the caller's overflow
    /// area, 16-byte aligned and 16 bytes wide. `emit_va_arg_float` models
    /// only the SSE class, so it went looking in the XMM save area and read a
    /// `double`-sized hole -- the argument always came back as zero.
    pub(super) fn emit_va_arg_x87(&mut self, ap_base: Reg, ap_base_offset: i32, dst_loc: &Loc) {
        let Loc::Stack(dst_offset) = dst_loc else {
            // An x87 value only ever lives in a stack slot; without one there
            // is nowhere to pop ST(0) to, and pushing it would unbalance the
            // x87 stack.
            return;
        };

        let overflow = MemAddr::BaseOffset {
            base: ap_base,
            offset: ap_base_offset + 8,
        };

        // r10 = overflow_arg_area rounded up to the type's 16-byte alignment.
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(overflow.clone()),
            dst: GpOperand::Reg(Reg::R10),
        });
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

        self.push_lir(X86Inst::X87Load {
            addr: MemAddr::BaseOffset {
                base: Reg::R10,
                offset: 0,
            },
        });

        // overflow_arg_area advances past the whole 16-byte slot. R10 is free
        // again once the value is loaded; R11 never is here, because it holds
        // the va_list itself whenever `ap` is a pointer to one, and `overflow`
        // is addressed through it.
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Imm(16),
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R10),
            dst: GpOperand::Mem(overflow),
        });

        self.push_lir(X86Inst::X87Store {
            addr: self.stack_mem(*dst_offset),
        });
    }

    pub(super) fn emit_x87_store(&mut self, insn: &Instruction) {
        let (addr, value) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&a), Some(&v)) => (a, v),
            _ => return,
        };

        // Load value to ST(0)
        let src_addr = self.get_x87_mem_addr(value);
        self.push_lir(X86Inst::X87Load { addr: src_addr });

        // Get destination address
        // For Store instruction, addr is a pseudo representing a memory location
        // (either a stack local, or a pointer in a register/memory)
        let addr_loc = self.get_location(addr);
        let dst_addr = match addr_loc {
            Loc::Reg(r) => {
                // addr is a pointer in a register
                MemAddr::BaseOffset {
                    base: r,
                    offset: insn.displacement(),
                }
            }
            Loc::Stack(offset) => {
                // A stack slot means one of two different things, exactly as
                // in the integer store path: for a *symbol* pseudo the slot is
                // the variable's storage, but for a temp it holds a *pointer*
                // to storage allocated elsewhere. Treating the second case
                // like the first wrote the value over the pointer itself —
                // and, at a non-zero offset, past the end of the frame into
                // the caller's. That is what made a `long double _Complex`
                // return corrupt the stack.
                let is_symbol = self.pseudos.is_sym(addr);
                if is_symbol {
                    self.stack_field(offset, insn.displacement())
                } else {
                    // Load the pointer, then address through it.
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(self.stack_mem(offset)),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: insn.displacement(),
                    }
                }
            }
            Loc::Global(name) => self.global_mem(&name, insn.displacement(), Reg::R11),
            _ => {
                self.emit_move(addr, Reg::R11, 64);
                MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: insn.displacement(),
                }
            }
        };

        // Store from ST(0)
        self.push_lir(X86Inst::X87Store { addr: dst_addr });
    }

    /// Emit x87 binary operation (add, sub, mul, div)
    ///
    /// Pattern for a + b:
    ///   fldt   a(%rbp)       ; load a to ST(0)
    ///   fldt   b(%rbp)       ; load b to ST(0), a moves to ST(1)
    ///   faddp  %st, %st(1)   ; ST(0) = ST(1) + ST(0), pop
    ///   fstpt  result(%rbp)  ; store result, pop
    pub(super) fn emit_x87_binop(&mut self, insn: &Instruction) {
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load first operand to ST(0)
        let src1_addr = self.get_x87_mem_addr(src1);
        self.push_lir(X86Inst::X87Load { addr: src1_addr });

        // Load second operand to ST(0), first moves to ST(1)
        let src2_addr = self.get_x87_mem_addr(src2);
        self.push_lir(X86Inst::X87Load { addr: src2_addr });

        // Perform operation: ST(0) = ST(1) op ST(0), pop
        let op = match insn.op {
            Opcode::FAdd => X87BinOp::Add,
            Opcode::FSub => X87BinOp::Sub,
            Opcode::FMul => X87BinOp::Mul,
            Opcode::FDiv => X87BinOp::Div,
            _ => return,
        };
        self.push_lir(X86Inst::X87BinOp { op });

        // Store result to destination and pop
        let dst_addr = self.get_x87_mem_addr(target);
        self.push_lir(X86Inst::X87Store { addr: dst_addr });
    }

    /// Emit x87 negation
    ///
    /// Pattern:
    ///   fldt   a(%rbp)       ; load to ST(0)
    ///   fchs                 ; negate ST(0)
    ///   fstpt  result(%rbp)  ; store and pop
    pub(super) fn emit_x87_neg(&mut self, insn: &Instruction) {
        self.emit_x87_unary_op(insn, X86Inst::X87Neg);
    }

    /// Emit `Fabs` of a `long double`: `fabs` between the same load and
    /// store as negation. An 80-bit `fldt`/`fstpt` converts nothing and
    /// raises nothing, not even for a signalling NaN, and `fabs` clears only
    /// the sign, so the payload survives.
    pub(super) fn emit_x87_abs(&mut self, insn: &Instruction) {
        self.emit_x87_unary_op(insn, X86Inst::X87Abs);
    }

    /// Emit `Sqrt` of a `long double`: `fsqrt`, correctly rounded at the
    /// extended precision the control word is left at.
    pub(super) fn emit_x87_sqrt(&mut self, insn: &Instruction) {
        self.emit_x87_unary_op(insn, X86Inst::X87Sqrt);
    }

    /// The address of the sign-and-exponent word of the `long double`
    /// `pseudo`: bytes 8 and 9, above the 64-bit significand, with the sign
    /// as bit 15. An address that takes no displacement (a RIP-relative
    /// constant) is first taken into `scratch`.
    fn x87_sign_word_addr(&mut self, pseudo: PseudoId, scratch: Reg) -> MemAddr {
        match self.get_x87_mem_addr(pseudo) {
            MemAddr::BaseOffset { base, offset } => MemAddr::BaseOffset {
                base,
                offset: offset + 8,
            },
            addr => {
                self.push_lir(X86Inst::Lea { addr, dst: scratch });
                MemAddr::BaseOffset {
                    base: scratch,
                    offset: 8,
                }
            }
        }
    }

    /// Load the sign-and-exponent word of the `long double` `pseudo`,
    /// zero-extended, into `dst`.
    fn emit_x87_sign_word_load(&mut self, pseudo: PseudoId, dst: Reg) {
        let addr = self.x87_sign_word_addr(pseudo, dst);
        self.push_lir(X86Inst::Movzx {
            src_size: OperandSize::B16,
            dst_size: OperandSize::B32,
            src: GpOperand::Mem(addr),
            dst,
        });
    }

    /// Emit `Signbit` of a `long double`: bit 15 of its sign-and-exponent
    /// word, read from memory, 0 or 1. Nothing is loaded onto the x87 stack.
    pub(super) fn emit_x87_signbit(&mut self, insn: &Instruction) {
        let (Some(&src), Some(target)) = (insn.src.first(), insn.target) else {
            return;
        };
        self.emit_x87_sign_word_load(src, Reg::R10);
        self.push_lir(X86Inst::Shr {
            size: OperandSize::B32,
            count: ShiftCount::Imm(15),
            dst: Reg::R10,
        });
        let dst_loc = self.get_location(target);
        self.emit_move_to_loc(Reg::R10, &dst_loc, u32::BITS);
    }

    /// Emit `CopySign` of a `long double`, on its memory image: the
    /// significand of the first operand copied as it is, and a sign word
    /// made of the first operand's exponent and the second's sign.
    ///
    /// Integer moves only, so every encoding -- a signalling NaN, a payload,
    /// one the FPU would not even load -- comes through bit for bit. Both
    /// sign words are read before the result is written, which may be the
    /// storage of either operand.
    pub(super) fn emit_x87_copysign(&mut self, insn: &Instruction) {
        let (Some(&x), Some(&y), Some(target)) = (insn.src.first(), insn.src.get(1), insn.target)
        else {
            return;
        };
        self.emit_x87_sign_word_load(y, Reg::R11);
        self.push_lir(X86Inst::And {
            size: OperandSize::B32,
            src: GpOperand::Imm(0x8000),
            dst: Reg::R11,
        });
        self.emit_x87_sign_word_load(x, Reg::R10);
        self.push_lir(X86Inst::And {
            size: OperandSize::B32,
            src: GpOperand::Imm(0x7fff),
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::Or {
            size: OperandSize::B32,
            src: GpOperand::Reg(Reg::R11),
            dst: Reg::R10,
        });

        let x_addr = self.get_x87_mem_addr(x);
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(x_addr),
            dst: GpOperand::Reg(Reg::R11),
        });
        let dst_addr = self.get_x87_mem_addr(target);
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst: GpOperand::Mem(dst_addr),
        });
        let dst_word = self.x87_sign_word_addr(target, Reg::R11);
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B16,
            src: GpOperand::Reg(Reg::R10),
            dst: GpOperand::Mem(dst_word),
        });
    }

    /// Load a `long double`, apply the one-operand instruction `op` to ST(0),
    /// and store the result.
    fn emit_x87_unary_op(&mut self, insn: &Instruction, op: X86Inst) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_addr = self.get_x87_mem_addr(src);
        self.push_lir(X86Inst::X87Load { addr: src_addr });
        self.push_lir(op);

        let dst_addr = self.get_x87_mem_addr(target);
        self.push_lir(X86Inst::X87Store { addr: dst_addr });
    }

    /// Emit x87 comparison
    ///
    /// Pattern:
    ///   fldt    b(%rbp)      ; load b to ST(0)
    ///   fldt    a(%rbp)      ; load a to ST(0), b moves to ST(1)
    ///   fucomip %st(1), %st  ; compare ST(0) with ST(1), set EFLAGS, pop
    ///   fstp    %st(0)       ; discard remaining value
    ///   setcc   %al          ; set result based on condition
    ///
    /// `fcomip` for C's relational operators and `fucomip` for the rest:
    /// the flags are the same, and only a quiet NaN's invalid differs.
    ///
    /// Both report an unordered result -- either operand a NaN -- by
    /// setting CF, ZF *and* PF together. Those are exactly the flags the
    /// unsigned condition codes read as "below" and "equal", so a naive
    /// mapping makes every NaN comparison answer as though the operands were
    /// equal: `n == n` true, `n != n` false, `n < a` true. Two cases need
    /// more than a condition code, and two need their operands swapped:
    ///
    ///   a <  b   ->  compare as b > a   (`Ugt` is false when unordered)
    ///   a <= b   ->  compare as b >= a  (`Uge` likewise)
    ///   a == b   ->  ZF=1 and PF=0
    ///   a != b   ->  ZF=0 or  PF=1
    ///
    /// `>` and `>=` were already correct for the same reason the swap works.
    pub(super) fn emit_x87_compare(&mut self, insn: &Instruction) {
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let Some(cmp) = insn.op.float_cmp() else {
            return;
        };

        // `<` and `<=` are evaluated as the mirrored `>` / `>=` so that the
        // unordered case falls out false; that means loading the operands the
        // other way round.
        let swap = matches!(cmp, FloatCmp::Lt(_) | FloatCmp::Le(_));
        let (first, second) = if swap { (src2, src1) } else { (src1, src2) };

        // Load in reverse order so the comparison reads first op second.
        let second_addr = self.get_x87_mem_addr(second);
        self.push_lir(X86Inst::X87Load { addr: second_addr });

        let first_addr = self.get_x87_mem_addr(first);
        self.push_lir(X86Inst::X87Load { addr: first_addr });

        // Compare ST(0) with ST(1), set EFLAGS, pop ST(0)
        self.push_lir(X86Inst::X87CmpPop { nan: cmp.nan() });

        // Discard remaining ST(0)
        self.push_lir(X86Inst::X87Pop);

        let cc = match cmp {
            FloatCmp::Eq => CondCode::Eq,
            FloatCmp::Ne => CondCode::Ne,
            FloatCmp::Lt(_) => CondCode::Ugt, // mirrored: b > a
            FloatCmp::Le(_) => CondCode::Uge, // mirrored: b >= a
            FloatCmp::Gt(_) => CondCode::Ugt, // CF=0 and ZF=0
            FloatCmp::Ge(_) => CondCode::Uge, // CF=0
        };

        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::R10,
        };

        // Set byte based on condition
        self.push_lir(X86Inst::SetCC { cc, dst: dst_reg });

        // Equality has to consult the parity flag as well, because ZF alone
        // cannot tell "equal" from "unordered": both set it. R11 is the
        // reserved scratch (see the note on emit_fp_move).
        match cmp {
            FloatCmp::Eq => {
                // equal  =  ZF=1 and not unordered
                self.push_lir(X86Inst::SetCC {
                    cc: CondCode::Np,
                    dst: Reg::R11,
                });
                self.push_lir(X86Inst::And {
                    size: OperandSize::B8,
                    src: GpOperand::Reg(Reg::R11),
                    dst: dst_reg,
                });
            }
            FloatCmp::Ne => {
                // not equal  =  ZF=0 or unordered
                self.push_lir(X86Inst::SetCC {
                    cc: CondCode::P,
                    dst: Reg::R11,
                });
                self.push_lir(X86Inst::Or {
                    size: OperandSize::B8,
                    src: GpOperand::Reg(Reg::R11),
                    dst: dst_reg,
                });
            }
            _ => {}
        }

        // Zero-extend to 32-bit
        self.push_lir(X86Inst::Movzx {
            src_size: OperandSize::B8,
            dst_size: OperandSize::B32,
            src: GpOperand::Reg(dst_reg),
            dst: dst_reg,
        });

        // Move to final destination if needed
        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, u32::BITS);
        }
    }

    /// Materialize the *address* of a pseudo's storage into a register.
    ///
    /// A stack slot means one of two things: for a symbol pseudo the slot is
    /// the object's storage, so its address is `lea`'d; for a temp the slot
    /// holds a pointer to storage elsewhere, so the pointer is loaded. Getting
    /// this backwards reads a value's bytes as an address, which is how
    /// passing a call's complex result — whose result local is a symbol —
    /// faulted in the callee.
    ///
    /// Needed wherever a multi-part value is addressed from a common base: a
    /// `long double _Complex`'s two halves, or a MEMORY-class argument copied
    /// to the stack.
    pub(super) fn address_of_pseudo(&mut self, pseudo: PseudoId) -> Reg {
        let loc = self.get_location(pseudo);
        match loc {
            Loc::Reg(r) => r,
            Loc::Stack(offset) => {
                // `stack_mem` picks the base register: an over-aligned frame
                // addresses its locals through a second base, and spelling
                // `-(offset + callee_saved_offset)(%rbp)` by hand named a
                // different address entirely. A stacked aggregate argument
                // passed from such a frame then copied from a garbage pointer.
                let addr = self.stack_mem(offset);
                let is_symbol = self.pseudos.is_sym(pseudo);
                if is_symbol {
                    // The slot *is* the storage: take its address.
                    self.push_lir(X86Inst::Lea {
                        dst: Reg::R11,
                        addr,
                    });
                } else {
                    // The slot holds a pointer to the storage.
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(addr),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                }
                Reg::R11
            }
            Loc::IncomingArg(offset) => {
                // The same two meanings as a stack slot, by what the
                // parameter is: an aggregate passed by value lies in the
                // incoming area, while a pointer parameter's slot holds the
                // pointer -- which is how a Win64 by-reference argument past
                // the fourth arrives.
                let addr = MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset,
                };
                if self.incoming_pointers.contains(&pseudo) {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(addr),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                } else {
                    self.push_lir(X86Inst::Lea {
                        dst: Reg::R11,
                        addr,
                    });
                }
                Reg::R11
            }
            _ => {
                // Fall back to the scalar path's address and take it.
                let addr = self.get_x87_mem_addr(pseudo);
                self.push_lir(X86Inst::Lea {
                    dst: Reg::R11,
                    addr,
                });
                Reg::R11
            }
        }
    }

    pub(super) fn get_x87_mem_addr(&mut self, pseudo: PseudoId) -> MemAddr {
        let loc = self.get_location(pseudo);
        match loc {
            Loc::Stack(offset) => self.stack_mem(offset),
            Loc::IncomingArg(offset) => MemAddr::BaseOffset {
                base: Reg::Rbp,
                offset,
            },
            Loc::Global(name) => {
                // For global long doubles, use RIP-relative addressing
                MemAddr::RipRelative(crate::arch::lir::Symbol {
                    name,
                    is_local: false,
                    is_extern: false,
                })
            }
            Loc::Reg(r) => {
                // Pointer to long double in a register
                MemAddr::BaseOffset { base: r, offset: 0 }
            }
            Loc::FImm(v, _) => {
                // Long double immediate: register the exact 80-bit image for
                // later emission in the data section.
                //
                // The pool is keyed on the full encoding, not on the value
                // rounded to a double -- two long doubles differing only below
                // the 53rd significand bit are different constants, and an
                // f64-derived key silently merged them into one.
                let label_bits = v.pool_key();
                let temp_label = crate::arch::lir::internal_label("ld_const", label_bits);

                let ld_bytes = v.to_x87_bytes();
                self.ld_constants.insert(label_bits, ld_bytes);

                MemAddr::RipRelative(crate::arch::lir::Symbol::local(temp_label))
            }
            _ => {
                // Fallback - should not happen for proper long double handling
                MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: 0,
                }
            }
        }
    }

    /// Emit float-to-float conversion involving long double.
    /// Handles conversions between float/double and long double using x87 FPU.
    pub(super) fn emit_x87_fp_cvt(&mut self, insn: &Instruction, types: &TypeTable) {
        // Use type info for proper FP size detection, fall back to size
        let src_kind = insn.src_typ.map(|t| types.kind(t));
        let dst_kind = insn.typ.map(|t| types.kind(t));
        let src_is_longdouble = src_kind == Some(TypeKind::LongDouble);
        let dst_is_longdouble = dst_kind == Some(TypeKind::LongDouble);
        let src_is_float = src_kind.expect("x87 convert must have src_typ") == TypeKind::Float;
        let dst_is_float = dst_kind.expect("x87 convert must have typ") == TypeKind::Float;
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Get source memory address
        let src_loc = self.get_location(src);
        let dst_loc = self.get_location(target);

        // Helper to get memory address for non-long-double operand
        let get_mem_addr = |loc: &Loc, this: &Self| -> MemAddr {
            match loc {
                Loc::Stack(offset) => this.stack_mem(*offset),
                Loc::IncomingArg(offset) => MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: *offset,
                },
                Loc::Global(name) => MemAddr::RipRelative(crate::arch::lir::Symbol {
                    name: name.clone(),
                    is_local: false,
                    is_extern: false,
                }),
                Loc::Reg(r) => MemAddr::BaseOffset {
                    base: *r,
                    offset: 0,
                },
                _ => MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: 0,
                },
            }
        };

        if src_is_longdouble && !dst_is_longdouble {
            // Long double -> float/double
            // Load long double to ST(0), store as float or double
            let src_addr = self.get_x87_mem_addr(src);
            self.push_lir(X86Inst::X87Load { addr: src_addr });

            // Check if destination is XMM register - need scratch memory
            let needs_xmm_load = matches!(&dst_loc, Loc::Xmm(_));
            let dst_addr = if needs_xmm_load {
                // Use scratch location for x87 -> XMM transfer
                // Must be after callee-saved register area to avoid collision
                self.x87_scratch_addr()
            } else {
                get_mem_addr(&dst_loc, self)
            };

            if dst_is_float {
                // Store as float (32-bit)
                self.push_lir(X86Inst::X87StoreFloat {
                    addr: dst_addr.clone(),
                });
            } else {
                // Store as double (64-bit)
                self.push_lir(X86Inst::X87StoreDouble {
                    addr: dst_addr.clone(),
                });
            }

            // If destination is XMM, load from scratch location
            if let Loc::Xmm(xmm_reg) = &dst_loc {
                let fp_size = if dst_is_float {
                    FpSize::Single
                } else {
                    FpSize::Double
                };
                self.push_lir(X86Inst::MovFp {
                    size: fp_size,
                    src: XmmOperand::Mem(dst_addr),
                    dst: XmmOperand::Reg(*xmm_reg),
                });
            }
        } else if !src_is_longdouble && dst_is_longdouble {
            // Float/double -> long double
            //
            // The load width has to match how the source is *stored*, which is
            // not always what its C type says: a constant goes into the
            // 8-byte double pool whatever its type, so a `float` one must
            // still be read with `fldl`. Reading it with `flds` took the low
            // half of the double and produced 0 -- `long double x = 1.5f;`
            // came out as zero, and so did every `INFINITY` reaching a long
            // double, since <math.h> spells it `__builtin_inff()`.
            let mut load_as_float = src_is_float;
            let src_addr = match &src_loc {
                Loc::Xmm(xmm_reg) => {
                    // XMM register - store to scratch memory first
                    // Must be after callee-saved register area to avoid collision
                    let scratch = self.x87_scratch_addr();
                    let fp_size = if src_is_float {
                        FpSize::Single
                    } else {
                        FpSize::Double
                    };
                    self.push_lir(X86Inst::MovFp {
                        size: fp_size,
                        src: XmmOperand::Reg(*xmm_reg),
                        dst: XmmOperand::Mem(scratch.clone()),
                    });
                    scratch
                }
                Loc::FImm(val, _) => {
                    // Float/double immediate - create constant in rodata.
                    // `double_constants` is emitted as `.quad`, so this is a
                    // 64-bit object regardless of the expression's type: the
                    // value as its own type holds it, widened to double.
                    // Widening is exact for a number, and for a NaN it quiets
                    // just as the `flds` it stands in for would.
                    let src_fmt = if src_is_float {
                        FpFormat::Binary32
                    } else {
                        FpFormat::Binary64
                    };
                    let val = val.convert(src_fmt, FpFormat::Binary64).to_f64();
                    let bits = val.to_bits();
                    let label = crate::arch::lir::internal_label("dbl_const", bits);
                    self.double_constants.insert(bits, val);
                    load_as_float = false;
                    MemAddr::RipRelative(crate::arch::lir::Symbol::local(label))
                }
                _ => get_mem_addr(&src_loc, self),
            };

            // Load as float/double to x87
            if load_as_float {
                // Load float (32-bit)
                self.push_lir(X86Inst::X87LoadFloat { addr: src_addr });
            } else {
                // Load double (64-bit)
                self.push_lir(X86Inst::X87LoadDouble { addr: src_addr });
            }

            let dst_addr = self.get_x87_mem_addr(target);
            self.push_lir(X86Inst::X87Store { addr: dst_addr });
        } else {
            // Long double -> long double (just copy)
            let src_addr = self.get_x87_mem_addr(src);
            self.push_lir(X86Inst::X87Load { addr: src_addr });
            let dst_addr = self.get_x87_mem_addr(target);
            self.push_lir(X86Inst::X87Store { addr: dst_addr });
        }
    }

    /// Emit integer to long double conversion.
    ///
    /// Pattern:
    ///   movl   %src, temp(%rbp)    ; store integer to memory (if in register)
    ///   fildl  temp(%rbp)          ; load as int, convert to x87 extended
    ///   fstpt  dst(%rbp)           ; store as long double
    pub(super) fn emit_x87_int_to_float(&mut self, insn: &Instruction) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_size = insn.src_size.max(32);
        let src_loc = self.get_location(src);

        // We need the integer in memory for fild. If it's in a register, store it first.
        let src_addr = match &src_loc {
            Loc::Stack(offset) => self.stack_mem(*offset),
            Loc::IncomingArg(offset) => MemAddr::BaseOffset {
                base: Reg::Rbp,
                offset: *offset,
            },
            Loc::Reg(r) => {
                // Integer is in a register - store to a temp location first
                // Use a scratch stack location (reuse destination if it's on stack)
                let dst_loc = self.get_location(target);
                let temp_addr = if let Loc::Stack(offset) = &dst_loc {
                    // Use the bottom part of the destination (which is 16 bytes)
                    self.stack_mem(*offset)
                } else {
                    // Use a fixed scratch location after callee-saved area
                    self.x87_scratch_addr()
                };

                let op_size = if src_size <= 32 {
                    OperandSize::B32
                } else {
                    OperandSize::B64
                };
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Reg(*r),
                    dst: GpOperand::Mem(temp_addr.clone()),
                });
                temp_addr
            }
            Loc::Imm(val) => {
                // Immediate - store to temp location after callee-saved area
                let temp_addr = self.x87_scratch_addr();
                let width = if src_size <= 32 { 32 } else { 64 };
                self.store_imm(*val, width, temp_addr.clone(), Reg::R10);
                temp_addr
            }
            Loc::Global(name) => MemAddr::RipRelative(crate::arch::lir::Symbol {
                name: name.clone(),
                is_local: false,
                is_extern: false,
            }),
            _ => {
                // Fallback - should not happen
                MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: 0,
                }
            }
        };

        // Load integer with fildl/fildq - converts to x87 extended precision
        if src_size <= 32 {
            self.push_lir(X86Inst::X87LoadInt32 { addr: src_addr });
        } else {
            self.push_lir(X86Inst::X87LoadInt64 { addr: src_addr });
        }

        // Store as long double
        let dst_addr = self.get_x87_mem_addr(target);
        self.push_lir(X86Inst::X87Store { addr: dst_addr });
    }

    /// Emit long double to integer conversion.
    ///
    /// The value is converted by the narrowest `fistp` that holds every value
    /// of the destination type -- see [`fistp_width`] -- under a truncating
    /// control word ([`Self::emit_x87_truncating_store`]), through the x87
    /// scratch into `%r10`, and from there to the destination:
    ///
    /// ```text
    ///     fldt    src
    ///     <truncating fistpl scratch>
    ///     movl    scratch, %r10d
    /// ```
    ///
    /// `unsigned long` is the one integer no `fistp` holds; it takes
    /// [`Self::emit_x87_float_to_u64`].
    pub(super) fn emit_x87_float_to_int(&mut self, insn: &Instruction) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_size = insn.size;
        let dst_loc = self.get_location(target);

        // Load long double to x87 stack
        let src_addr = self.get_x87_mem_addr(src);
        self.push_lir(X86Inst::X87Load { addr: src_addr });

        let Some(width) = fistp_width(dst_size, insn.op == Opcode::FCvtS) else {
            self.emit_x87_float_to_u64(&dst_loc);
            return;
        };
        let result = self.x87_scratch_addr();
        self.emit_x87_truncating_store(width, result.clone());
        // A 16-bit store is only ever read by a destination of 16 bits or
        // fewer; every wider one is read at the destination's own width.
        self.push_lir(if width == X87IntWidth::W16 {
            X86Inst::Movsx {
                src_size: OperandSize::B16,
                dst_size: OperandSize::B32,
                src: GpOperand::Mem(result),
                dst: Reg::R10,
            }
        } else {
            X86Inst::Mov {
                size: OperandSize::from_bits(dst_size.max(32)),
                src: GpOperand::Mem(result),
                dst: GpOperand::Reg(Reg::R10),
            }
        });
        self.emit_move_to_loc(Reg::R10, &dst_loc, dst_size);
    }

    /// Store ST(0) to `addr` as an integer of `width`, truncating, and pop.
    ///
    /// `fistp` rounds as the control word says, and `fisttp` -- which always
    /// truncates -- is SSE3, past the x86-64 baseline. So, as gcc does, set
    /// the rounding-control field (bits 10-11) to truncate for the one
    /// instruction and put back the word that was there:
    ///
    /// ```text
    ///     fnstcw  saved
    ///     movzwl  saved, %r11d
    ///     orl     $0xc00, %r11d
    ///     movw    %r11w, truncating
    ///     fldcw   truncating
    ///     fistpl  addr
    ///     fldcw   saved
    /// ```
    ///
    /// The words live in the function's [`X87ControlWords`] slot, which the
    /// allocator reserves for exactly the functions that get here.
    fn emit_x87_truncating_store(&mut self, width: X87IntWidth, addr: MemAddr) {
        let words = self
            .x87_control_words
            .expect("the allocator reserves the x87 control words for every conversion");
        let saved = self.stack_field(words.slot(), X87ControlWords::SAVED);
        let truncating = self.stack_field(words.slot(), X87ControlWords::TRUNCATING);
        self.push_lir(X86Inst::X87StoreControlWord {
            addr: saved.clone(),
        });
        self.push_lir(X86Inst::Movzx {
            src_size: OperandSize::B16,
            dst_size: OperandSize::B32,
            src: GpOperand::Mem(saved.clone()),
            dst: Reg::R11,
        });
        self.push_lir(X86Inst::Or {
            size: OperandSize::B32,
            src: GpOperand::Imm(X87_ROUND_TOWARD_ZERO),
            dst: Reg::R11,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B16,
            src: GpOperand::Reg(Reg::R11),
            dst: GpOperand::Mem(truncating.clone()),
        });
        self.push_lir(X86Inst::X87LoadControlWord { addr: truncating });
        self.push_lir(X86Inst::X87StoreInt { width, addr });
        self.push_lir(X86Inst::X87LoadControlWord { addr: saved });
    }

    /// Convert the long double in ST(0) to an `unsigned long` in `dst_loc`.
    ///
    /// `fistpq` stores a *signed* 64-bit integer and answers
    /// 0x8000000000000000 for any value at or above 2^63, well inside the
    /// unsigned range. So the range is split, exactly as the SSE path does in
    /// `emit_float_to_u64`, and as gcc does:
    ///
    /// ```text
    ///     movabs $0x43e0000000000000, %r11   # 2^63 as a double
    ///     mov    %r11, 8+scratch
    ///     fldl   8+scratch                   # ST0 = 2^63, ST1 = x
    ///     fucomip %st(1), %st                # compare, pop; ST0 = x
    ///     jbe    .big                        # 2^63 <= x
    ///     <truncating fistpq scratch>
    ///     mov    scratch, %r10
    ///     jmp    .done
    ///   .big:
    ///     fldl   8+scratch
    ///     fsubrp                             # ST0 = x - 2^63
    ///     <truncating fistpq scratch>
    ///     mov    scratch, %r10
    ///     movabs $1<<63, %r11
    ///     xor    %r11, %r10                  # put the bit back
    ///   .done:
    /// ```
    ///
    /// The subtraction is exact -- 2^63 is a power of two and x87's 64-bit
    /// significand covers the whole of `unsigned long long`.
    fn emit_x87_float_to_u64(&mut self, dst_loc: &Loc) {
        // Two disjoint halves of the 16-byte x87 scratch: the result goes in
        // the first, the 2^63 constant in byte 8 of the same object.
        let result_addr = self.x87_scratch_addr();
        let const_addr = self.stack_field(self.x87_scratch_slot(), 8);

        let uid = self.unique_label_counter;
        self.unique_label_counter += 1;
        let big_label = Label::block(&self.base.current_fn, 10000 + uid * 2);
        let done_label = Label::block(&self.base.current_fn, 10000 + uid * 2 + 1);

        self.push_lir(X86Inst::MovAbs {
            imm: TWO_POW_63_BITS,
            dst: Reg::R11,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst: GpOperand::Mem(const_addr.clone()),
        });
        self.push_lir(X86Inst::X87LoadDouble {
            addr: const_addr.clone(),
        });
        self.push_lir(X86Inst::X87CmpPop {
            nan: NanCompare::Quiet,
        });
        self.push_lir(X86Inst::Jcc {
            cc: CondCode::Ule,
            target: big_label.clone(),
        });

        self.emit_x87_truncating_store(X87IntWidth::W64, result_addr.clone());
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(result_addr.clone()),
            dst: GpOperand::Reg(Reg::R10),
        });
        self.push_lir(X86Inst::Jmp {
            target: done_label.clone(),
        });

        self.push_lir(X86Inst::Directive(Directive::BlockLabel(big_label)));
        self.push_lir(X86Inst::X87LoadDouble { addr: const_addr });
        self.push_lir(X86Inst::X87BinOp { op: X87BinOp::Sub });
        self.emit_x87_truncating_store(X87IntWidth::W64, result_addr.clone());
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Mem(result_addr),
            dst: GpOperand::Reg(Reg::R10),
        });
        // Put back the bit the subtraction removed. It is known clear in
        // the converted result, so `xor` and `add` agree here.
        self.push_lir(X86Inst::MovAbs {
            imm: i64::MIN,
            dst: Reg::R11,
        });
        self.push_lir(X86Inst::Xor {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst: Reg::R10,
        });
        self.push_lir(X86Inst::Directive(Directive::BlockLabel(done_label)));
        self.emit_move_to_loc(Reg::R10, dst_loc, 64);
    }
}

/// Whether `insn` converts a long double to an integer.
///
/// The one rule for both the codegen dispatch and the allocator, which
/// reserves the [`X87ControlWords`] slot for exactly these.
pub(super) fn is_x87_float_to_int(insn: &Instruction, types: &TypeTable) -> bool {
    matches!(insn.op, Opcode::FCvtS | Opcode::FCvtU) && is_long_double(insn.src_typ, types)
}

/// An integer to `long double` conversion, which `emit_x87_int_to_float`
/// computes.
pub(super) fn is_x87_int_to_float(insn: &Instruction, types: &TypeTable) -> bool {
    matches!(insn.op, Opcode::SCvtF | Opcode::UCvtF) && is_long_double(insn.typ, types)
}

/// A conversion between `long double` and another floating type, which
/// `emit_x87_fp_cvt` computes.
pub(super) fn is_x87_fp_cvt(insn: &Instruction, types: &TypeTable) -> bool {
    insn.op == Opcode::FCvtF
        && (is_long_double(insn.typ, types) || is_long_double(insn.src_typ, types))
}

/// Whether the code for `insn` may stage a value through the x87 scratch:
/// the three conversions above, and an `asm` with a `float` or `double`
/// operand on the x87 stack. The allocator reserves the scratch by this, so
/// a new user of `x87_scratch_addr` must be named here.
pub(super) fn uses_x87_scratch(insn: &Instruction, types: &TypeTable) -> bool {
    is_x87_int_to_float(insn, types)
        || is_x87_float_to_int(insn, types)
        || is_x87_fp_cvt(insn, types)
        || (insn.op == Opcode::Asm
            && insn
                .extra()
                .asm_data
                .as_ref()
                .is_some_and(|asm| x87_operands(asm).any(|c| c.size <= 64)))
}

fn is_long_double(typ: Option<crate::types::TypeId>, types: &TypeTable) -> bool {
    typ.is_some_and(|t| types.kind(t) == TypeKind::LongDouble)
}

/// The narrowest `fistp` holding every value of a `bits`-bit integer, or
/// `None` when none does -- `unsigned long`, which needs a 65-bit signed
/// store.
///
/// An unsigned destination needs one bit more than its width, since `fistp`
/// stores a signed integer: an `unsigned int` is converted at 64 bits and an
/// `unsigned short` at 32, as gcc does. Any value representable in the
/// destination is then representable in the store, and C leaves every other
/// value undefined.
fn fistp_width(bits: u32, signed: bool) -> Option<X87IntWidth> {
    match bits + u32::from(!signed) {
        0..=16 => Some(X87IntWidth::W16),
        17..=32 => Some(X87IntWidth::W32),
        33..=64 => Some(X87IntWidth::W64),
        _ => None,
    }
}

/// The x87 control word's rounding-control field set to round toward zero.
const X87_ROUND_TOWARD_ZERO: i64 = 0xc00;

#[cfg(test)]
mod tests {
    use super::*;

    /// Signed destinations store at their own width (with `char` widened to
    /// the narrowest `fistp`), unsigned ones one bit wider, and `unsigned
    /// long` -- which no `fistp` holds -- takes the split.
    #[test]
    fn fistp_width_holds_every_destination_value() {
        use X87IntWidth::*;
        for (bits, signed, want) in [
            (8, true, Some(W16)),
            (16, true, Some(W16)),
            (32, true, Some(W32)),
            (64, true, Some(W64)),
            (8, false, Some(W16)),
            (16, false, Some(W32)),
            (32, false, Some(W64)),
            (64, false, None),
        ] {
            assert_eq!(
                fistp_width(bits, signed),
                want,
                "{bits} bits, signed {signed}"
            );
        }
    }
}
