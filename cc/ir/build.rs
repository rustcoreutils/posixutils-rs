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
use super::{Function, Instruction, Opcode, PseudoId};
use crate::diag::Position;
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
    pub(crate) fn load(&mut self, addr: PseudoId, at: i64, typ: TypeId, size: u32) -> PseudoId {
        let v = self.func.alloc_pseudo();
        self.push(Instruction::load(v, addr, at, typ, size));
        v
    }

    /// A store of `v`, of `typ` and `size` bits wide, to `addr + at`.
    pub(crate) fn store(&mut self, v: PseudoId, addr: PseudoId, at: i64, typ: TypeId, size: u32) {
        self.push(Instruction::store(v, addr, at, typ, size));
    }

    /// A new integer constant of `typ` at `size` bits, with the `SetVal`
    /// that gives it its width.
    pub(crate) fn constant(&mut self, v: i128, typ: TypeId, size: u32) -> PseudoId {
        let id = self.func.create_const_pseudo(at_width(v, size, true));
        self.push(
            Instruction::new(Opcode::SetVal)
                .with_target(id)
                .with_type_and_size(typ, size),
        );
        id
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
