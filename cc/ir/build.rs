//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Building the instructions that replace one instruction, in an optimizer
// pass
//
// A pass that rewrites one instruction into several -- `memexpand` turning a
// `memcpy` into loads and stores, `libcall_fold` turning `strlen(p + i)` into
// a subtraction -- makes new pseudos, new constants and new instructions, all
// at the position of the one it replaces. This is that, once.
//

use super::constfold::at_width;
use super::{Function, Instruction, Opcode, Pseudo, PseudoId};
use crate::diag::Position;
use crate::float::FloatVal;
use crate::types::{TypeId, TypeTable};

/// The instructions that replace one instruction, appended to `out` in
/// order, each at the replaced instruction's source position.
pub(crate) struct Builder<'a> {
    pub(crate) func: &'a mut Function,
    pub(crate) types: &'a TypeTable,
    pos: Option<Position>,
    out: &'a mut Vec<Instruction>,
}

impl<'a> Builder<'a> {
    pub(crate) fn new(
        func: &'a mut Function,
        types: &'a TypeTable,
        pos: Option<Position>,
        out: &'a mut Vec<Instruction>,
    ) -> Self {
        Builder {
            func,
            types,
            pos,
            out,
        }
    }

    pub(crate) fn push(&mut self, mut insn: Instruction) {
        insn.pos = self.pos;
        self.out.push(insn);
    }

    /// A load of `typ`, `size` bits wide, from `addr + at`.
    ///
    /// Marked volatile when `typ` is, for the same reason and by the same rule
    /// as `Linearizer::mark_volatile_access`: an access this builds is as
    /// observable as one the program wrote, and a pass must not be able to
    /// introduce an unmarked access to a volatile object.
    pub(crate) fn load(&mut self, addr: PseudoId, at: i64, typ: TypeId, size: u32) -> PseudoId {
        let v = self.func.alloc_pseudo();
        let vol = self.types.contains_volatile(typ);
        self.push(Instruction::load(v, addr, at, typ, size).with_volatile(vol));
        v
    }

    /// A store of `v`, of `typ` and `size` bits wide, to `addr + at`.
    pub(crate) fn store(&mut self, v: PseudoId, addr: PseudoId, at: i64, typ: TypeId, size: u32) {
        let vol = self.types.contains_volatile(typ);
        self.push(Instruction::store(v, addr, at, typ, size).with_volatile(vol));
    }

    /// A new integer constant of `typ` at `size` bits, with the `SetVal`
    /// that gives it its width.
    pub(crate) fn constant(&mut self, v: i128, typ: TypeId, size: u32) -> PseudoId {
        let id = self.func.create_const_pseudo(at_width(v, size, true));
        self.push(Instruction::set_val(id, typ, size));
        id
    }

    /// A new float constant of `typ` at `size` bits, with the `SetVal` that
    /// gives it its width -- without one it would be read at 64 bits.
    pub(crate) fn float_constant(&mut self, v: FloatVal, typ: TypeId, size: u32) -> PseudoId {
        let id = self.func.alloc_pseudo();
        self.func.add_pseudo(Pseudo::fval(id, v));
        self.push(Instruction::set_val(id, typ, size));
        id
    }

    /// The address of the symbol `name`, as a `typ`.
    pub(crate) fn sym_addr(&mut self, name: String, typ: TypeId) -> PseudoId {
        let sym = self.func.alloc_pseudo();
        self.func.add_pseudo(Pseudo::sym(sym, name));
        let p = self.func.alloc_pseudo();
        self.push(Instruction::sym_addr(p, sym, typ));
        p
    }

    /// `op` of `a` and `b` into a new pseudo of `typ` at `size` bits.
    pub(crate) fn binop(
        &mut self,
        op: Opcode,
        a: PseudoId,
        b: PseudoId,
        typ: TypeId,
        size: u32,
    ) -> PseudoId {
        let t = self.func.alloc_pseudo();
        self.push(Instruction::binop(op, t, a, b, typ, size));
        t
    }

    /// A comparison `op` of `a` and `b`, of `typ` at `size` bits, into a new
    /// `int` pseudo that is 0 or 1.
    pub(crate) fn compare(
        &mut self,
        op: Opcode,
        a: PseudoId,
        b: PseudoId,
        typ: TypeId,
        size: u32,
    ) -> PseudoId {
        let t = self.func.alloc_pseudo();
        let int = self.types.int_id;
        let int_bits = self.types.size_bits(int);
        self.push(Instruction::compare(
            op,
            t,
            (a, b),
            (typ, size),
            (int, int_bits),
        ));
        t
    }

    /// A conversion `op` of `src`, of `from` at `from_size` bits, to `to` at
    /// `size` bits.
    pub(crate) fn convert(
        &mut self,
        op: Opcode,
        src: PseudoId,
        (from, from_size): (TypeId, u32),
        (to, size): (TypeId, u32),
    ) -> PseudoId {
        let t = self.func.alloc_pseudo();
        let mut insn = Instruction::unop(op, t, src, to, size);
        insn.src_size = from_size;
        insn.src_typ = Some(from);
        self.push(insn);
        t
    }

    /// `cond ? a : b`, of `typ` at `size` bits.
    pub(crate) fn select(
        &mut self,
        cond: PseudoId,
        a: PseudoId,
        b: PseudoId,
        typ: TypeId,
        size: u32,
    ) -> PseudoId {
        let t = self.func.alloc_pseudo();
        self.push(Instruction::select(t, cond, a, b, typ, size));
        t
    }

    /// `target` defined as a copy of `src`, of `typ` at `size` bits: how a
    /// rewritten instruction's own result is given its new value, keeping the
    /// pseudo every use already names.
    pub(crate) fn copy_into(&mut self, target: PseudoId, src: PseudoId, typ: TypeId, size: u32) {
        self.push(Instruction::unop(Opcode::Copy, target, src, typ, size));
    }
}
