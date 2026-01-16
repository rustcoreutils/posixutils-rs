//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 Floating-Point Code Generation
//

use super::codegen::Aarch64CodeGen;
use super::lir::{Aarch64Inst, MemAddr};
use super::regalloc::{Loc, Reg, VReg};
use crate::arch::lir::{CondCode, FpSize, OperandSize, Symbol};
use crate::ir::{Instruction, Opcode, PseudoId, PseudoKind};
use crate::types::{TypeId, TypeKind, TypeTable};

impl Aarch64CodeGen {
    /// Get FP size from type on aarch64.
    ///
    /// On aarch64:
    /// - Float16 uses native FP16 instructions (AArch64 supports half-precision)
    /// - LongDouble is treated as Double (64-bit on macOS)
    fn fp_size_from_type(typ: Option<TypeId>, size: u32, types: &TypeTable) -> FpSize {
        typ.map(|t| match types.kind(t) {
            TypeKind::Float16 => FpSize::Half,
            TypeKind::Float => FpSize::Single,
            TypeKind::Double | TypeKind::LongDouble => FpSize::Double,
            _ => FpSize::from_bits(size.max(32)),
        })
        .unwrap_or_else(|| FpSize::from_bits(size.max(32)))
    }

    /// Get size in bits from type, with fallback to provided size.
    fn size_from_type(typ: Option<TypeId>, size: u32, types: &TypeTable) -> u32 {
        typ.map(|t| types.size_bits(t)).unwrap_or(size).max(32)
    }
    /// Emit a floating-point load instruction
    pub(super) fn emit_fp_load(&mut self, insn: &Instruction, frame_size: i32, types: &TypeTable) {
        let size = Self::size_from_type(insn.typ, insn.size, types);
        let addr = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };
        let dst_loc = self.get_location(target);
        let dst_vreg = match dst_loc {
            Loc::VReg(v) => v,
            _ => VReg::V17,
        };
        let addr_loc = self.get_location(addr);

        let fp_size = Self::fp_size_from_type(insn.typ, insn.size, types);

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
                let (scratch0, _, _) = Reg::scratch_regs();
                if self.needs_got_access(&name) {
                    // External symbols on macOS: load address from GOT, then load FP value
                    let extern_sym = Symbol::extern_sym(&name);
                    self.push_lir(Aarch64Inst::AdrpGotPage {
                        sym: extern_sym.clone(),
                        dst: scratch0,
                    });
                    self.push_lir(Aarch64Inst::LdrSymGotPageOff {
                        sym: extern_sym,
                        base: scratch0,
                        dst: scratch0,
                    });
                    // Now scratch0 has the address, load the FP value
                    self.push_lir(Aarch64Inst::LdrFp {
                        size: fp_size,
                        addr: MemAddr::BaseOffset {
                            base: scratch0,
                            offset: insn.offset as i32,
                        },
                        dst: dst_vreg,
                    });
                } else {
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
            self.emit_fp_move_to_loc(dst_vreg, &dst_loc, insn.typ, size, frame_size, types);
        }
    }

    /// Move FP value to a VReg
    pub(super) fn emit_fp_move(
        &mut self,
        src: PseudoId,
        dst: VReg,
        typ: Option<TypeId>,
        size: u32,
        frame_size: i32,
        types: &TypeTable,
    ) {
        let loc = self.get_location(src);
        let fp_size = Self::fp_size_from_type(typ, size, types);

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
                let (scratch0, _, _) = Reg::scratch_regs();
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
                let (scratch0, _, _) = Reg::scratch_regs();
                self.emit_mov_imm(scratch0, v, 64);
                self.push_lir(Aarch64Inst::FmovFromGp {
                    size: fp_size,
                    src: scratch0,
                    dst,
                });
            }
            Loc::Global(name) => {
                // Load from global - use size matching FP precision
                let (scratch0, _, _) = Reg::scratch_regs();
                let load_size = match fp_size {
                    FpSize::Half => OperandSize::B16,
                    FpSize::Single => OperandSize::B32,
                    FpSize::Double => OperandSize::B64,
                    FpSize::Extended => unreachable!("x87 extended not available on AArch64"),
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
    pub(super) fn emit_fp_move_to_loc(
        &mut self,
        src: VReg,
        dst: &Loc,
        typ: Option<TypeId>,
        size: u32,
        frame_size: i32,
        types: &TypeTable,
    ) {
        let fp_size = Self::fp_size_from_type(typ, size, types);

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
    pub(super) fn emit_fp_binop(&mut self, insn: &Instruction, frame_size: i32, types: &TypeTable) {
        let size = Self::size_from_type(insn.typ, insn.size, types);
        let fp_size = Self::fp_size_from_type(insn.typ, insn.size, types);
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
        self.emit_fp_move(src1, VReg::V17, insn.typ, size, frame_size, types);
        self.emit_fp_move(src2, VReg::V18, insn.typ, size, frame_size, types);

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
            self.emit_fp_move_to_loc(work_reg, &dst_loc, insn.typ, size, frame_size, types);
        }
    }

    /// Emit FP negation
    pub(super) fn emit_fp_neg(&mut self, insn: &Instruction, frame_size: i32, types: &TypeTable) {
        let size = Self::size_from_type(insn.typ, insn.size, types);
        let fp_size = Self::fp_size_from_type(insn.typ, insn.size, types);
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

        self.emit_fp_move(src, VReg::V17, insn.typ, size, frame_size, types);

        self.push_lir(Aarch64Inst::Fneg {
            size: fp_size,
            src: VReg::V17,
            dst: work_reg,
        });

        if !matches!(&dst_loc, Loc::VReg(v) if *v == work_reg) {
            self.emit_fp_move_to_loc(work_reg, &dst_loc, insn.typ, size, frame_size, types);
        }
    }

    /// Emit FP comparison
    pub(super) fn emit_fp_compare(
        &mut self,
        insn: &Instruction,
        frame_size: i32,
        types: &TypeTable,
    ) {
        let size = Self::size_from_type(insn.typ, insn.size, types);
        let fp_size = Self::fp_size_from_type(insn.typ, insn.size, types);
        let (src1, src2) = match (insn.src.first(), insn.src.get(1)) {
            (Some(&s1), Some(&s2)) => (s1, s2),
            _ => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        // Load operands to FP registers
        self.emit_fp_move(src1, VReg::V17, insn.typ, size, frame_size, types);
        self.emit_fp_move(src2, VReg::V18, insn.typ, size, frame_size, types);

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
            _ => Reg::X16,
        };

        // Set result based on condition
        let cond = match insn.op {
            Opcode::FCmpOEq => CondCode::Eq,
            Opcode::FCmpONe => CondCode::Ne,
            Opcode::FCmpOLt => CondCode::Slt,
            Opcode::FCmpOLe => CondCode::Sle,
            Opcode::FCmpOGt => CondCode::Sgt,
            Opcode::FCmpOGe => CondCode::Sge,
            _ => return,
        };

        self.push_lir(Aarch64Inst::Cset { cond, dst: dst_reg });

        if !matches!(&dst_loc, Loc::Reg(r) if *r == dst_reg) {
            self.emit_move_to_loc(dst_reg, &dst_loc, 32, frame_size);
        }
    }

    /// Emit integer to float conversion
    pub(super) fn emit_int_to_float(
        &mut self,
        insn: &Instruction,
        frame_size: i32,
        types: &TypeTable,
    ) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_size = Self::size_from_type(insn.src_typ, insn.src_size, types);
        let dst_size = Self::size_from_type(insn.typ, insn.size, types);
        let fp_size = Self::fp_size_from_type(insn.typ, insn.size, types);
        let int_size = OperandSize::from_bits(src_size);

        let dst_loc = self.get_location(target);
        let dst_vreg = match &dst_loc {
            Loc::VReg(v) => *v,
            _ => VReg::V16,
        };

        // Load source to integer register
        let (scratch0, _, _) = Reg::scratch_regs();
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
            self.emit_fp_move_to_loc(dst_vreg, &dst_loc, insn.typ, dst_size, frame_size, types);
        }
    }

    /// Emit float to integer conversion
    pub(super) fn emit_float_to_int(
        &mut self,
        insn: &Instruction,
        frame_size: i32,
        types: &TypeTable,
    ) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_size = Self::size_from_type(insn.src_typ, insn.src_size, types);
        let dst_size = Self::size_from_type(insn.typ, insn.size, types);
        let fp_size = Self::fp_size_from_type(insn.src_typ, insn.src_size, types);
        let int_size = OperandSize::from_bits(dst_size);

        let dst_loc = self.get_location(target);
        let dst_reg = match &dst_loc {
            Loc::Reg(r) => *r,
            _ => Reg::X16,
        };

        // Load source to FP register
        self.emit_fp_move(src, VReg::V17, insn.src_typ, src_size, frame_size, types);

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
    pub(super) fn emit_float_to_float(
        &mut self,
        insn: &Instruction,
        frame_size: i32,
        types: &TypeTable,
    ) {
        let src = match insn.src.first() {
            Some(&s) => s,
            None => return,
        };
        let target = match insn.target {
            Some(t) => t,
            None => return,
        };

        let src_size = Self::size_from_type(insn.src_typ, insn.src_size, types);
        let dst_size = Self::size_from_type(insn.typ, insn.size, types);
        let src_fp_size = Self::fp_size_from_type(insn.src_typ, insn.src_size, types);
        let dst_fp_size = Self::fp_size_from_type(insn.typ, insn.size, types);

        let dst_loc = self.get_location(target);
        let dst_vreg = match &dst_loc {
            Loc::VReg(v) => *v,
            _ => VReg::V16,
        };

        // Load source to FP register
        self.emit_fp_move(src, VReg::V17, insn.src_typ, src_size, frame_size, types);

        // Convert between float sizes if types differ
        // fcvt: convert between single and double precision
        // Check types directly rather than FpSizes to properly distinguish conversions
        let src_kind = insn.src_typ.map(|t| types.kind(t));
        let dst_kind = insn.typ.map(|t| types.kind(t));
        let needs_convert = match (src_kind, dst_kind) {
            (Some(TypeKind::Float), Some(TypeKind::Double | TypeKind::LongDouble)) => true,
            (Some(TypeKind::Double | TypeKind::LongDouble), Some(TypeKind::Float)) => true,
            // On aarch64, Double and LongDouble are the same, no conversion needed
            _ => false,
        };

        if needs_convert {
            self.push_lir(Aarch64Inst::Fcvt {
                src_size: src_fp_size,
                dst_size: dst_fp_size,
                src: VReg::V17,
                dst: dst_vreg,
            });
        } else if dst_vreg != VReg::V17 {
            // Same type (or types unknown), just move if needed
            self.push_lir(Aarch64Inst::FmovReg {
                size: dst_fp_size,
                src: VReg::V17,
                dst: dst_vreg,
            });
        }

        if !matches!(&dst_loc, Loc::VReg(v) if *v == dst_vreg) {
            self.emit_fp_move_to_loc(dst_vreg, &dst_loc, insn.typ, dst_size, frame_size, types);
        }
    }
}
