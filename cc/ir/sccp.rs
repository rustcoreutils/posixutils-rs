//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// SCCP: sparse conditional constant propagation (Wegman & Zadeck).
//
// What this does that `instcombine` cannot: it propagates constants along
// *reachable paths only*. A branch whose condition is a known constant makes
// its untaken successor unreachable, so a phi at the merge no longer has to
// meet the value arriving from there -- which is how
//
//     if (0) { link_error(); }
//
// and, less obviously,
//
//     int x; if (1) x = 3; else x = 4; if (x != 3) boom();
//
// both collapse. `instcombine` sees each instruction in isolation and has no
// notion of an edge, so neither is within its reach.
//
// The solver -- seeding, edge marking, worklists, and the rewrite of what
// the solution proves -- is `dataflow`'s, shared with `vrp`; see there for
// the decisions that keep it sound and for the memory-ordering contract.
// This file is the lattice and its transfer function.
//
// Dominance: not used. Wegman-Zadeck needs only executable-edge marking, so
// this pass never builds a dominator tree -- which also means it cannot hold
// a stale one. An extension that wants dominance must call
// `dominate::domtree_build` *fresh*, after the CFG edits here.
//

use super::constfold::{eval_int, int_fold_arity, is_int_foldable, Outcomes};
use super::dataflow::{Lattice, Selector, Sparse, SparseAnalysis};
use super::facts::ConstMap;
use super::propagate::cbr_taken;
use super::{BasicBlockId, Function, Instruction, Opcode, PseudoId};

/// The lattice, of height three.
///
/// Moves downward only -- `Top` to `Const` to `Bottom` -- which is the whole
/// termination argument: each cell changes at most twice.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Val {
    /// No information yet. In this version only unreached definitions hold it
    /// at fixpoint; see the note on `Undef` in `dataflow`.
    Top,
    /// A known integer, held as the raw bit pattern exactly as
    /// `PseudoKind::Val` does. Reading it at a given width is `constfold`'s
    /// job, not this lattice's.
    Const(i128),
    /// Overdefined: could be anything.
    Bottom,
}

impl Lattice for Val {
    const TOP: Val = Val::Top;
    const BOTTOM: Val = Val::Bottom;

    fn meet(self, other: Val) -> Val {
        match (self, other) {
            (Val::Top, x) | (x, Val::Top) => x,
            (Val::Const(a), Val::Const(b)) if a == b => Val::Const(a),
            (Val::Bottom, _) | (_, Val::Bottom) => Val::Bottom,
            _ => Val::Bottom,
        }
    }
}

struct Solver {
    core: Sparse<Val>,
    /// Which pseudos are copies of which, for deciding `x - x` and `x == x`
    /// when the two sides are one value under two names.
    consts: ConstMap,
}

/// Run SCCP over `func`, returning whether anything changed.
pub fn run(func: &mut Function) -> bool {
    Solver {
        core: Sparse::new(func, Val::Const),
        consts: ConstMap::new(func),
    }
    .run(func)
}

impl Solver {
    fn get(&self, id: PseudoId) -> Val {
        self.core.get(id)
    }

    /// An integer operation `constfold` evaluates, over its operands'
    /// lattice values.
    fn int_op(&self, insn: &Instruction) -> Val {
        if int_fold_arity(insn.op) != Some(insn.src.len()) {
            return Val::Bottom;
        }
        let mut ops = [0i128; 2];
        let (mut known, mut pending) = (true, false);
        for (slot, s) in ops.iter_mut().zip(&insn.src) {
            match self.get(*s) {
                Val::Const(c) => *slot = c,
                Val::Top => (known, pending) = (false, true),
                Val::Bottom => known = false,
            }
        }
        if known {
            // `None` is an operation undefined for these operands -- a zero
            // divisor, an out-of-range shift count. Not a constant.
            return eval_int(insn, &ops[..insn.src.len()]).map_or(Val::Bottom, Val::Const);
        }
        // A comparison of a value with itself is decided without knowing
        // the value, exactly as `instcombine` does it.
        if let ([a, b], Some((mask, domain))) = (insn.src.as_slice(), Outcomes::of_op(insn.op)) {
            let w = insn.operand_width();
            if self.consts.same(*a, *b, w) {
                if let Some(v) = mask.decide(domain.reflexive()) {
                    return Val::Const(i128::from(v));
                }
            }
        }
        if pending {
            Val::Top
        } else {
            Val::Bottom
        }
    }
}

impl SparseAnalysis for Solver {
    type V = Val;

    fn core(&self) -> &Sparse<Val> {
        &self.core
    }

    fn core_mut(&mut self) -> &mut Sparse<Val> {
        &mut self.core
    }

    fn selector(&self, _block: BasicBlockId, id: PseudoId) -> Selector {
        match self.get(id) {
            Val::Top => Selector::Pending,
            Val::Const(v) => Selector::Value(v),
            Val::Bottom => Selector::Unknown,
        }
    }

    fn constant(&self, _block: BasicBlockId, target: PseudoId) -> Option<i128> {
        match self.get(target) {
            Val::Const(v) => Some(v),
            _ => None,
        }
    }

    /// The lattice value an instruction produces.
    ///
    /// The default arm is `Bottom`, and that is the safety property of the
    /// whole pass: an unmodelled opcode answering `Top` would be a licence to
    /// prove any branch below it dead. Adding an opcode here means knowing
    /// its semantics exactly.
    fn transfer(&self, insn: &Instruction, block: BasicBlockId) -> Val {
        match insn.op {
            // A conduit. Never rewritten -- see `apply`.
            Opcode::Copy | Opcode::PhiSource => match insn.src.first() {
                Some(s) => self.get(*s),
                None => Val::Bottom,
            },

            // The value is on the target pseudo and was seeded there.
            Opcode::SetVal => self.get(insn.target.unwrap_or(PseudoId(0))),

            Opcode::Phi => {
                let mut acc = Val::Top;
                for (pred, incoming) in &insn.phi_list {
                    if self.core.is_edge_executable(*pred, block) {
                        acc = acc.meet(self.get(*incoming));
                    }
                }
                acc
            }

            Opcode::Select => {
                if insn.src.len() != 3 {
                    return Val::Bottom;
                }
                match self.get(insn.src[0]) {
                    Val::Const(c) => match cbr_taken(c) {
                        Some(true) => self.get(insn.src[1]),
                        Some(false) => self.get(insn.src[2]),
                        None => self.get(insn.src[1]).meet(self.get(insn.src[2])),
                    },
                    Val::Top => Val::Top,
                    Val::Bottom => self.get(insn.src[1]).meet(self.get(insn.src[2])),
                }
            }

            // `__builtin_constant_p`, which asks this pass its own question:
            // is the operand a constant once propagation has run? `Top` is
            // not yet an answer -- a value still `Top` at fixpoint is in
            // unreachable code, and `ir::lower` answers 0 for whatever is
            // left over.
            Opcode::ConstantP => match insn.src.first().map(|s| self.get(*s)) {
                Some(Val::Const(_)) => Val::Const(1),
                Some(Val::Top) => Val::Top,
                _ => Val::Const(0),
            },

            op if is_int_foldable(op) => self.int_op(insn),

            // Everything else. See the doc comment above.
            //
            // `Lo64`/`Hi64`/`Pair64`/`AddC`/`AdcC`/`SubC`/`SbcC`/`UMulHi`
            // land here deliberately and must stay: `arch::mapping` runs
            // before the optimizer, so they are present, and neither is
            // ordinary arithmetic. `AdcC`/`SbcC` take their carry from a
            // *flag*; their third operand names the producing instruction so
            // the scheduler keeps the pair adjacent, and reading it as a
            // value would be reading something else entirely.
            _ => Val::Bottom,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, Pseudo};
    use crate::target::Target;
    use crate::types::TypeTable;

    /// The type table every test drives the pass with.
    fn host_types() -> TypeTable {
        TypeTable::new(&Target::host())
    }

    /// A diamond: `entry` branches on `cond` to `then`/`els`, both of which
    /// reach `merge`, where a `Phi` meets `then_val` and `else_val`.
    ///
    /// Built by hand rather than linearized because SCCP runs on IR that is
    /// already in SSA form, so the phi and its two `PhiSource` conduits have
    /// to be present exactly as `ssa_convert` leaves them.
    fn diamond(cond: Pseudo, then_val: i128, else_val: i128) -> Function {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("test", types.int_id);
        let int = types.int_id;

        let c = cond.id;
        func.add_pseudo(cond);
        func.add_pseudo(Pseudo::val(PseudoId(10), then_val));
        func.add_pseudo(Pseudo::val(PseudoId(11), else_val));
        func.add_pseudo(Pseudo::reg(PseudoId(12), 12)); // then's PhiSource
        func.add_pseudo(Pseudo::reg(PseudoId(13), 13)); // else's PhiSource
        func.add_pseudo(Pseudo::reg(PseudoId(14), 14)); // the phi
        func.next_pseudo = 20;

        let (entry, then, els, merge) = (
            BasicBlockId(0),
            BasicBlockId(1),
            BasicBlockId(2),
            BasicBlockId(3),
        );

        let mut b0 = BasicBlock::new(entry);
        b0.add_insn(Instruction::new(Opcode::Entry));
        b0.add_insn(Instruction::cbr(c, then, els));
        b0.children = vec![then, els];

        let mut b1 = BasicBlock::new(then);
        let mut ps1 = Instruction::phi_source(PseudoId(12), PseudoId(10), int, 32);
        ps1.phi_list = vec![(merge, PseudoId(14))];
        b1.add_insn(ps1);
        b1.add_insn(Instruction::br(merge));
        b1.parents = vec![entry];
        b1.children = vec![merge];

        let mut b2 = BasicBlock::new(els);
        let mut ps2 = Instruction::phi_source(PseudoId(13), PseudoId(11), int, 32);
        ps2.phi_list = vec![(merge, PseudoId(14))];
        b2.add_insn(ps2);
        b2.add_insn(Instruction::br(merge));
        b2.parents = vec![entry];
        b2.children = vec![merge];

        let mut b3 = BasicBlock::new(merge);
        let mut phi = Instruction::phi(PseudoId(14), int, 32);
        phi.phi_list = vec![(then, PseudoId(12)), (els, PseudoId(13))];
        b3.add_insn(phi);
        b3.add_insn(Instruction::ret(Some(PseudoId(14))));
        b3.parents = vec![then, els];

        for bb in [b0, b1, b2, b3] {
            func.add_block(bb);
        }
        func.entry = entry;
        func
    }

    fn terminator(func: &Function, b: usize) -> &Instruction {
        func.blocks[b].insns.last().unwrap()
    }

    // Branch folding

    #[test]
    fn sccp_folds_cbr_on_constant_true() {
        let mut func = diamond(Pseudo::val(PseudoId(1), 1), 5, 9);
        assert!(run(&mut func));
        let t = terminator(&func, 0);
        assert_eq!(t.op, Opcode::Br);
        assert_eq!(t.bb_true, Some(BasicBlockId(1)));
        assert_eq!(t.bb_false, None, "a folded Cbr must not keep bb_false");
        assert_eq!(func.blocks[0].children, vec![BasicBlockId(1)]);
        assert!(
            !func.blocks[2].parents.contains(&BasicBlockId(0)),
            "the dropped successor must lose its parent edge too"
        );
    }

    #[test]
    fn sccp_folds_cbr_on_constant_false() {
        let mut func = diamond(Pseudo::val(PseudoId(1), 0), 5, 9);
        assert!(run(&mut func));
        let t = terminator(&func, 0);
        assert_eq!(t.op, Opcode::Br);
        assert_eq!(t.bb_true, Some(BasicBlockId(2)));
        assert_eq!(func.blocks[0].children, vec![BasicBlockId(2)]);
    }

    #[test]
    fn sccp_does_not_fold_cbr_on_an_argument() {
        let mut func = diamond(Pseudo::arg(PseudoId(1), 0), 5, 9);
        assert!(!run(&mut func), "nothing is knowable here");
        assert_eq!(terminator(&func, 0).op, Opcode::Cbr);
    }

    /// A value whose low 32 bits are zero but which is not zero proves
    /// nothing: the branch reads it at a width this pass cannot ask for.
    #[test]
    fn sccp_does_not_fold_cbr_on_a_width_ambiguous_constant() {
        let mut func = diamond(Pseudo::val(PseudoId(1), 1i128 << 32), 5, 9);
        run(&mut func);
        assert_eq!(terminator(&func, 0).op, Opcode::Cbr);
    }

    // Phi

    /// The headline: the phi folds only because one incoming edge is proved
    /// dead. `instcombine` has no notion of an edge and cannot do this.
    #[test]
    fn sccp_phi_over_a_dead_edge_folds() {
        let mut func = diamond(Pseudo::val(PseudoId(1), 1), 5, 9);
        assert!(run(&mut func));
        let phi = &func.blocks[3].insns[0];
        assert_eq!(phi.op, Opcode::Copy);
        assert!(phi.phi_list.is_empty(), "a folded phi keeps no incoming");
        assert_eq!(func.const_val(phi.src[0]), Some(5));

        let mut other = diamond(Pseudo::val(PseudoId(1), 1), 5, 9);
        assert!(
            !crate::ir::instcombine::run(&mut other, &host_types()),
            "if instcombine can already do this the test proves nothing"
        );
    }

    #[test]
    fn sccp_phi_with_differing_constants_does_not_fold() {
        let mut func = diamond(Pseudo::arg(PseudoId(1), 0), 5, 9);
        run(&mut func);
        assert_eq!(func.blocks[3].insns[0].op, Opcode::Phi);
    }

    /// `lower::eliminate_phi_nodes` finds a phi's incoming value by scanning
    /// for the opcode, so rewriting one deletes that value silently.
    #[test]
    fn sccp_never_rewrites_a_phisource() {
        let mut func = diamond(Pseudo::val(PseudoId(1), 1), 5, 5);
        run(&mut func);
        assert_eq!(func.blocks[1].insns[0].op, Opcode::PhiSource);
    }

    /// SCCP leaves the orphaned `PhiSource` for the `dce` that follows it in
    /// `opt::optimize_function`. If anyone moves SCCP out of that loop, this
    /// is the coupling that breaks.
    #[test]
    fn sccp_leaves_the_dead_phisource_for_dce() {
        let mut func = diamond(Pseudo::val(PseudoId(1), 1), 5, 9);
        run(&mut func);
        crate::ir::dce::run(&mut func);

        // The untaken arm has no predecessor left, so `dce` deletes it.
        assert!(
            func.get_block(BasicBlockId(2)).is_none(),
            "the untaken arm should be unreachable and gone"
        );
        // And the surviving arm's conduit has no reader now that the phi is
        // a `Copy`, so it goes too. Neither happens without the `dce` run.
        assert!(
            func.blocks
                .iter()
                .flat_map(|b| &b.insns)
                .all(|i| i.op != Opcode::PhiSource),
            "no PhiSource should survive a folded phi"
        );
    }

    /// A branch on a population count of a constant is decided, and the
    /// count is taken at the operand's width: `1 << 40` has one bit set as a
    /// 64-bit value and none as a 32-bit one.
    #[test]
    fn sccp_folds_a_branch_on_a_constant_popcount() {
        let types = TypeTable::new(&Target::host());
        for (op, size, taken) in [
            (Opcode::Popcount64, 64, BasicBlockId(1)),
            (Opcode::Popcount32, 32, BasicBlockId(2)),
        ] {
            let mut func = Function::new("t", types.int_id);
            func.add_pseudo(Pseudo::val(PseudoId(1), 1 << 40));
            func.add_pseudo(Pseudo::reg(PseudoId(2), 2));
            func.next_pseudo = 5;

            let insn = Instruction::new(op)
                .with_target(PseudoId(2))
                .with_src(PseudoId(1))
                .with_size(size)
                .with_type(types.int_id);

            let mut b0 = BasicBlock::new(BasicBlockId(0));
            b0.add_insn(Instruction::new(Opcode::Entry));
            b0.add_insn(insn);
            b0.add_insn(Instruction::cbr(
                PseudoId(2),
                BasicBlockId(1),
                BasicBlockId(2),
            ));
            b0.children = vec![BasicBlockId(1), BasicBlockId(2)];
            func.add_block(b0);
            for id in [BasicBlockId(1), BasicBlockId(2)] {
                let mut bb = BasicBlock::new(id);
                bb.add_insn(Instruction::ret(None));
                bb.parents = vec![BasicBlockId(0)];
                func.add_block(bb);
            }
            func.entry = BasicBlockId(0);

            assert!(run(&mut func), "{op:?}");
            let t = terminator(&func, 0);
            assert_eq!(t.op, Opcode::Br, "{op:?}");
            assert_eq!(t.bb_true, Some(taken), "{op:?}");
        }
    }

    /// `x == y` with `y` a copy of an unknown `x` is decided by the two
    /// being one value, which is a question about where `y` came from and
    /// not about its pseudo id: after promotion out of memory every read of
    /// a local is its own `Copy`.
    #[test]
    fn sccp_decides_a_comparison_of_a_value_with_its_copy() {
        let types = TypeTable::new(&Target::host());
        let int = types.int_id;
        let mut func = Function::new("t", int);
        func.add_pseudo(Pseudo::arg(PseudoId(1), 0));
        for id in 2..=3 {
            func.add_pseudo(Pseudo::reg(PseudoId(id), id));
        }
        func.next_pseudo = 5;

        let mut b0 = BasicBlock::new(BasicBlockId(0));
        b0.add_insn(Instruction::new(Opcode::Entry));
        b0.add_insn(
            Instruction::new(Opcode::Copy)
                .with_target(PseudoId(2))
                .with_src(PseudoId(1))
                .with_type_and_size(int, 32),
        );
        b0.add_insn(Instruction::compare(
            Opcode::SetEq,
            PseudoId(3),
            (PseudoId(1), PseudoId(2)),
            (int, 32),
            (int, 32),
        ));
        b0.add_insn(Instruction::cbr(
            PseudoId(3),
            BasicBlockId(1),
            BasicBlockId(2),
        ));
        b0.children = vec![BasicBlockId(1), BasicBlockId(2)];
        func.add_block(b0);
        for id in [BasicBlockId(1), BasicBlockId(2)] {
            let mut bb = BasicBlock::new(id);
            bb.add_insn(Instruction::ret(None));
            bb.parents = vec![BasicBlockId(0)];
            func.add_block(bb);
        }
        func.entry = BasicBlockId(0);

        assert!(run(&mut func));
        let t = terminator(&func, 0);
        assert_eq!(t.op, Opcode::Br, "x == copy(x) is always true");
        assert_eq!(t.bb_true, Some(BasicBlockId(1)));
    }

    // Safety

    /// The default arm of the transfer function must be `Bottom`. `Top`
    /// there would make every opcode this pass does not model a licence to
    /// prove the branch below it dead -- and it would look like it was
    /// working, because it optimizes more.
    #[test]
    fn sccp_treats_unmodelled_opcodes_as_overdefined() {
        let types = TypeTable::new(&Target::host());
        for op in [
            Opcode::Load,
            Opcode::Call,
            Opcode::UMulHi,
            Opcode::AdcC,
            Opcode::Lo64,
            Opcode::FAdd,
            Opcode::VaArg,
        ] {
            let mut func = Function::new("t", types.int_id);
            func.add_pseudo(Pseudo::val(PseudoId(1), 1));
            func.add_pseudo(Pseudo::reg(PseudoId(2), 2));
            func.next_pseudo = 5;

            let mut insn = Instruction::new(op);
            insn.target = Some(PseudoId(2));
            insn.src = vec![PseudoId(1), PseudoId(1)];
            insn.size = 32;

            let mut b0 = BasicBlock::new(BasicBlockId(0));
            b0.add_insn(Instruction::new(Opcode::Entry));
            b0.add_insn(insn);
            b0.add_insn(Instruction::cbr(
                PseudoId(2),
                BasicBlockId(1),
                BasicBlockId(2),
            ));
            b0.children = vec![BasicBlockId(1), BasicBlockId(2)];
            func.add_block(b0);
            for id in [BasicBlockId(1), BasicBlockId(2)] {
                let mut bb = BasicBlock::new(id);
                bb.add_insn(Instruction::ret(None));
                bb.parents = vec![BasicBlockId(0)];
                func.add_block(bb);
            }
            func.entry = BasicBlockId(0);

            run(&mut func);
            assert_eq!(
                terminator(&func, 0).op,
                Opcode::Cbr,
                "{op:?} must not be treated as knowable"
            );
        }
    }

    /// The `Copy` that replaces a folded comparison has the comparison's
    /// result type, an `int`, and not its operands': claiming a float
    /// operand's type would put the constant in an SSE register.
    #[test]
    fn sccp_retypes_a_folded_comparison_to_its_result() {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("t", types.int_id);
        func.add_pseudo(Pseudo::val(PseudoId(1), 3));
        func.add_pseudo(Pseudo::val(PseudoId(2), 4));
        func.add_pseudo(Pseudo::reg(PseudoId(3), 3));
        func.next_pseudo = 8;

        let mut b0 = BasicBlock::new(BasicBlockId(0));
        b0.add_insn(Instruction::new(Opcode::Entry));
        b0.add_insn(Instruction::compare(
            Opcode::SetLt,
            PseudoId(3),
            (PseudoId(1), PseudoId(2)),
            (types.long_id, 64),
            (types.int_id, 32),
        ));
        b0.add_insn(Instruction::ret(Some(PseudoId(3))));
        func.add_block(b0);
        func.entry = BasicBlockId(0);

        assert!(run(&mut func));
        let folded = &func.blocks[0].insns[1];
        assert_eq!(folded.op, Opcode::Copy);
        assert_eq!(func.const_val(folded.src[0]), Some(1));
        assert_eq!(folded.typ, Some(types.int_id), "not the operand type");
        assert_eq!(folded.size, types.size_bits(types.int_id));
    }

    /// A 128-bit constant needs a correctly sized `SetVal` to get its stack
    /// slot; a minted pseudo has none, and the aarch64 allocator has no
    /// 128-bit immediate case at all.
    #[test]
    fn sccp_does_not_materialize_at_128_bits() {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("t", types.int_id);
        func.add_pseudo(Pseudo::val(PseudoId(1), 3));
        func.add_pseudo(Pseudo::val(PseudoId(2), 4));
        func.add_pseudo(Pseudo::reg(PseudoId(3), 3));
        func.next_pseudo = 8;

        let mut b0 = BasicBlock::new(BasicBlockId(0));
        b0.add_insn(Instruction::new(Opcode::Entry));
        b0.add_insn(Instruction::binop(
            Opcode::Add,
            PseudoId(3),
            PseudoId(1),
            PseudoId(2),
            types.int_id,
            128,
        ));
        b0.add_insn(Instruction::ret(Some(PseudoId(3))));
        func.add_block(b0);
        func.entry = BasicBlockId(0);

        run(&mut func);
        assert_eq!(func.blocks[0].insns[1].op, Opcode::Add);
    }

    /// An `asm goto` block ends in an ordinary `Br` to its fallthrough; the
    /// branch targets live in `asm_data.goto_labels`. Marking only what the
    /// terminator names leaves them looking unreachable, and `dce` then
    /// deletes the arm the assembly jumps to.
    #[test]
    fn sccp_marks_asm_goto_targets_executable() {
        use crate::ir::AsmData;
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("t", types.int_id);
        func.next_pseudo = 4;

        let (entry, label, fall) = (BasicBlockId(0), BasicBlockId(1), BasicBlockId(2));
        let mut b0 = BasicBlock::new(entry);
        b0.add_insn(Instruction::new(Opcode::Entry));
        let mut asm = Instruction::new(Opcode::Asm);
        asm.extra_mut().asm_data = Some(Box::new(AsmData {
            template: String::new(),
            outputs: Vec::new(),
            inputs: Vec::new(),
            clobbers: Vec::new(),
            goto_labels: vec![(label, "done".to_string())],
        }));
        b0.add_insn(asm);
        b0.add_insn(Instruction::br(fall));
        b0.children = vec![fall, label];
        func.add_block(b0);

        for id in [label, fall] {
            let mut bb = BasicBlock::new(id);
            bb.add_insn(Instruction::ret(None));
            bb.parents = vec![entry];
            func.add_block(bb);
        }
        func.entry = entry;

        run(&mut func);
        crate::ir::dce::run(&mut func);
        assert!(
            func.get_block(label).is_some(),
            "the asm goto target must not be deleted as unreachable"
        );
    }

    // Well-formedness

    #[test]
    fn sccp_preserves_the_ir_invariants() {
        for cond in [
            Pseudo::val(PseudoId(1), 1),
            Pseudo::val(PseudoId(1), 0),
            Pseudo::arg(PseudoId(1), 0),
        ] {
            let mut func = diamond(cond, 5, 9);
            run(&mut func);
            crate::ir::dce::run(&mut func);
            assert!(
                crate::ir::validate::validate_function(&func).is_ok(),
                "{:?}",
                crate::ir::validate::validate_function(&func).err()
            );
        }
    }

    #[test]
    fn sccp_is_idempotent() {
        let mut func = diamond(Pseudo::val(PseudoId(1), 1), 5, 9);
        assert!(run(&mut func));
        assert!(
            !run(&mut func),
            "a second run must find nothing, or the fixpoint loop burns its budget"
        );
    }

    #[test]
    fn sccp_keeps_parents_and_children_consistent() {
        let mut func = diamond(Pseudo::val(PseudoId(1), 1), 5, 9);
        run(&mut func);
        for bb in &func.blocks {
            for child in &bb.children {
                let c = func.get_block(*child).expect("child must exist");
                assert!(
                    c.parents.contains(&bb.id),
                    "{} lists {} as a child but is not its parent",
                    bb.id,
                    child
                );
            }
        }
    }

    // __builtin_constant_p
    //
    // The one opcode that asks this pass its own question, so its answer is
    // the lattice value of its operand rather than a computation over it.

    /// A function with `ConstantP` over a pseudo of the given kind; returns
    /// the constant it folded to, or `None` if it did not fold.
    fn constant_p_over(operand: Pseudo) -> Option<i128> {
        let types = host_types();
        let mut func = Function::new("t", types.int_id);
        let operand_id = operand.id;
        func.add_pseudo(operand);
        func.add_pseudo(Pseudo::reg(PseudoId(2), 2));
        func.next_pseudo = 8;

        let mut b0 = BasicBlock::new(BasicBlockId(0));
        b0.add_insn(Instruction::new(Opcode::Entry));
        b0.add_insn(
            Instruction::new(Opcode::ConstantP)
                .with_target(PseudoId(2))
                .with_src(operand_id)
                .with_type_and_size(types.int_id, 32),
        );
        b0.add_insn(Instruction::ret(Some(PseudoId(2))));
        func.add_block(b0);
        func.entry = BasicBlockId(0);

        run(&mut func);
        let insn = &func.blocks[0].insns[1];
        if insn.op != Opcode::Copy {
            return None;
        }
        func.const_val(insn.src[0])
    }

    #[test]
    fn constant_p_answers_one_for_a_proved_constant() {
        assert_eq!(constant_p_over(Pseudo::val(PseudoId(1), 42)), Some(1));
    }

    /// An argument is never a constant, and saying so is the whole point:
    /// `__builtin_constant_p` guards the branch a program takes when the
    /// value is *not* known.
    #[test]
    fn constant_p_answers_zero_for_an_unknown_value() {
        assert_eq!(constant_p_over(Pseudo::arg(PseudoId(1), 0)), Some(0));
    }
}
