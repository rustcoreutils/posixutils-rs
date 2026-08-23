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
use super::lir::{GpOperand, MemAddr, X86Inst, X87BinOp};
use super::regalloc::{Loc, Reg, X87_SCRATCH_BYTES};
use crate::arch::lir::{CondCode, OperandSize};
use crate::ir::PseudoKind;
use crate::ir::{Instruction, Opcode, PseudoId};
use crate::types::{TypeKind, TypeTable};

impl X86_64CodeGen {
    /// The reserved scratch address used to stage a value into the FPU.
    ///
    /// `fild`/`fld` have no register form, so an immediate or a general
    /// register has to go through memory. The region is reserved by
    /// [`X87_SCRATCH_BYTES`]; never address it by hand.
    fn x87_scratch_addr(&self) -> MemAddr {
        MemAddr::BaseOffset {
            base: Reg::Rbp,
            offset: -(self.callee_saved_offset + X87_SCRATCH_BYTES),
        }
    }

    /// Check if this instruction operates on long double (80-bit x87)
    pub fn is_longdouble_op(&self, insn: &Instruction, types: &TypeTable) -> bool {
        insn.size >= 80
            && insn
                .typ
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
                    offset: insn.offset as i32,
                }
            }
            Loc::Stack(offset) => {
                // Same distinction as the store side: a symbol's slot is the
                // variable's storage, a temp's slot holds a *pointer* to
                // storage allocated elsewhere. Loading from the slot directly
                // in the second case read the pointer bits as a float — which
                // is where the NaNs came from — and at a non-zero offset read
                // past the frame entirely.
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));
                let adjusted = offset + self.callee_saved_offset;
                if is_symbol {
                    MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -(adjusted) + insn.offset as i32,
                    }
                } else {
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
                        offset: insn.offset as i32,
                    }
                }
            }
            Loc::Global(name) => {
                // For globals, use RIP-relative addressing
                if self.needs_got_access(&name) {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(
                            crate::arch::lir::Symbol::extern_sym(name.clone()),
                        )),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: insn.offset as i32,
                    }
                } else {
                    MemAddr::RipRelative(crate::arch::lir::Symbol {
                        name,
                        is_local: false,
                        is_extern: false,
                    })
                }
            }
            _ => {
                // Load address into R11
                self.emit_move(addr, Reg::R11, 64);
                MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: insn.offset as i32,
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

        // overflow_arg_area advances past the whole 16-byte slot.
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R10),
            dst: GpOperand::Reg(Reg::R11),
        });
        self.push_lir(X86Inst::Add {
            size: OperandSize::B64,
            src: GpOperand::Imm(16),
            dst: Reg::R11,
        });
        self.push_lir(X86Inst::Mov {
            size: OperandSize::B64,
            src: GpOperand::Reg(Reg::R11),
            dst: GpOperand::Mem(overflow),
        });

        let adjusted_offset = -(*dst_offset + self.callee_saved_offset);
        self.push_lir(X86Inst::X87Store {
            addr: MemAddr::BaseOffset {
                base: Reg::Rbp,
                offset: adjusted_offset,
            },
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
                    offset: insn.offset as i32,
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
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));
                let adjusted = offset + self.callee_saved_offset;
                if is_symbol {
                    MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -(adjusted) + insn.offset as i32,
                    }
                } else {
                    // Load the pointer, then address through it.
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
                        offset: insn.offset as i32,
                    }
                }
            }
            Loc::Global(name) => {
                if self.needs_got_access(&name) {
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::GotPcrel(
                            crate::arch::lir::Symbol::extern_sym(name.clone()),
                        )),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                    MemAddr::BaseOffset {
                        base: Reg::R11,
                        offset: insn.offset as i32,
                    }
                } else {
                    MemAddr::RipRelative(crate::arch::lir::Symbol {
                        name,
                        is_local: false,
                        is_extern: false,
                    })
                }
            }
            _ => {
                self.emit_move(addr, Reg::R11, 64);
                MemAddr::BaseOffset {
                    base: Reg::R11,
                    offset: insn.offset as i32,
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
        self.push_lir(X86Inst::X87Neg);

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
    /// `fucomip` reports an unordered result -- either operand a NaN -- by
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

        // `<` and `<=` are evaluated as the mirrored `>` / `>=` so that the
        // unordered case falls out false; that means loading the operands the
        // other way round.
        let swap = matches!(insn.op, Opcode::FCmpOLt | Opcode::FCmpOLe);
        let (first, second) = if swap { (src2, src1) } else { (src1, src2) };

        // Load in reverse order so the comparison reads first op second.
        let second_addr = self.get_x87_mem_addr(second);
        self.push_lir(X86Inst::X87Load { addr: second_addr });

        let first_addr = self.get_x87_mem_addr(first);
        self.push_lir(X86Inst::X87Load { addr: first_addr });

        // Compare ST(0) with ST(1), set EFLAGS, pop ST(0)
        self.push_lir(X86Inst::X87CmpPop);

        // Discard remaining ST(0)
        self.push_lir(X86Inst::X87Pop);

        let cc = match insn.op {
            Opcode::FCmpOEq => CondCode::Eq,
            Opcode::FCmpONe => CondCode::Ne,
            Opcode::FCmpOLt => CondCode::Ugt, // mirrored: b > a
            Opcode::FCmpOLe => CondCode::Uge, // mirrored: b >= a
            Opcode::FCmpOGt => CondCode::Ugt, // CF=0 and ZF=0
            Opcode::FCmpOGe => CondCode::Uge, // CF=0
            _ => return,
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
        match insn.op {
            Opcode::FCmpOEq => {
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
            Opcode::FCmpONe => {
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
                let adjusted = offset + self.callee_saved_offset;
                let is_symbol = self
                    .pseudos
                    .iter()
                    .find(|p| p.id == pseudo)
                    .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));
                if is_symbol {
                    // The slot *is* the storage: take its address.
                    self.push_lir(X86Inst::Lea {
                        dst: Reg::R11,
                        addr: MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -adjusted,
                        },
                    });
                } else {
                    // The slot holds a pointer to the storage.
                    self.push_lir(X86Inst::Mov {
                        size: OperandSize::B64,
                        src: GpOperand::Mem(MemAddr::BaseOffset {
                            base: Reg::Rbp,
                            offset: -adjusted,
                        }),
                        dst: GpOperand::Reg(Reg::R11),
                    });
                }
                Reg::R11
            }
            Loc::IncomingArg(offset) => {
                self.push_lir(X86Inst::Lea {
                    dst: Reg::R11,
                    addr: MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset,
                    },
                });
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
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: -adjusted,
                }
            }
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
                let temp_label = format!(".Lld_const_{}", label_bits);

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
                Loc::Stack(offset) => {
                    let adjusted = offset + this.callee_saved_offset;
                    MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }
                }
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
                use super::lir::XmmOperand;
                use crate::arch::lir::FpSize;
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
                    use super::lir::XmmOperand;
                    use crate::arch::lir::FpSize;
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
                    // 64-bit object regardless of the expression's type. The
                    // f64 holds the f32 value exactly, so reading it as a
                    // double is both correct and lossless.
                    let val = val.to_f64();
                    let bits = val.to_bits();
                    let label = format!(".Ldbl_const_{}", bits);
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
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: -adjusted,
                }
            }
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
                    let adjusted = offset + self.callee_saved_offset;
                    MemAddr::BaseOffset {
                        base: Reg::Rbp,
                        offset: -adjusted,
                    }
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
                let op_size = if src_size <= 32 {
                    OperandSize::B32
                } else {
                    OperandSize::B64
                };
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Imm(*val as i64),
                    dst: GpOperand::Mem(temp_addr.clone()),
                });
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
    /// Pattern:
    ///   fldt   src(%rbp)          ; load long double to ST(0)
    ///   fisttpl dst(%rbp)         ; convert with truncation and store
    pub(super) fn emit_x87_float_to_int(&mut self, insn: &Instruction) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let dst_size = insn.size.max(32);
        let dst_loc = self.get_location(target);

        // Load long double to x87 stack
        let src_addr = self.get_x87_mem_addr(src);
        self.push_lir(X86Inst::X87Load { addr: src_addr });

        // Determine where to store the result
        let needs_load_to_reg = matches!(&dst_loc, Loc::Reg(_) | Loc::Xmm(_));

        let store_addr = match &dst_loc {
            Loc::Stack(offset) => {
                let adjusted = offset + self.callee_saved_offset;
                MemAddr::BaseOffset {
                    base: Reg::Rbp,
                    offset: -adjusted,
                }
            }
            Loc::IncomingArg(offset) => MemAddr::BaseOffset {
                base: Reg::Rbp,
                offset: *offset,
            },
            _ => {
                // Store to scratch location, will load to register afterwards
                // Must be after callee-saved register area to avoid collision
                self.x87_scratch_addr()
            }
        };

        // Convert and store with fisttp (truncation toward zero)
        if dst_size <= 32 {
            self.push_lir(X86Inst::X87StoreInt32 {
                addr: store_addr.clone(),
            });
        } else {
            self.push_lir(X86Inst::X87StoreInt64 {
                addr: store_addr.clone(),
            });
        }

        // If destination is a register, load the result
        if needs_load_to_reg {
            if let Loc::Reg(dst_reg) = &dst_loc {
                let op_size = if dst_size <= 32 {
                    OperandSize::B32
                } else {
                    OperandSize::B64
                };
                self.push_lir(X86Inst::Mov {
                    size: op_size,
                    src: GpOperand::Mem(store_addr),
                    dst: GpOperand::Reg(*dst_reg),
                });
            }
        }
    }
}
