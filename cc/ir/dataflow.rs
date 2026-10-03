//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Sparse conditional dataflow (Wegman & Zadeck), the one solver `sccp` and
// `vrp` both run.
//
// A pass supplies a lattice and three answers -- what an instruction
// produces, what a branch selector is known to be, and what constant a
// solved pseudo holds -- and this module owns everything else: the seeding,
// the executable-edge marking, the worklists, the step budget, and rewriting
// what the solution proves. The decisions here are each a way the algorithm
// turns unsound when missed, which is why there is exactly one copy of them:
//
// * `Undef` is seeded `Bottom`, not `Top`. A value still `Top` at fixpoint
//   that feeds a conditional branch marks *neither* successor executable, so
//   both look unreachable and their side effects are deleted.
// * A `Sym` names storage, not a value: a struct-returning call writes
//   through one as its `target`. Folding a use of it would replace an
//   address with a number.
// * An inline-asm output is a second definition of its pseudo that invariant
//   I1 deliberately exempts; folding a use of one past the asm's write is a
//   wrong value.
// * A block whose address is taken (`&&label`) is reachable by a route that
//   has no CFG edge, and an `asm goto` block ends in an ordinary `Br` while
//   the assembly may jump to any of its labels. Both are marked from what
//   `children` records, not from the terminator.
//
// Memory-ordering contract: nothing here moves code. Every value rewrite is
// an in-place substitution at a single instruction site, and the only
// structural change is dropping a CFG edge that has been *proved* not taken.
// Blocks left unreachable are deleted by the `dce` that follows.
//

use super::propagate::{self, cbr_taken, switch_taken};
use super::{BasicBlockId, Function, Instruction, Opcode, PseudoId, PseudoKind, Site};
use std::collections::{HashMap, HashSet, VecDeque};

/// A lattice a solve descends: `TOP` is "not reached yet", `BOTTOM`
/// "could be anything", and `meet` only ever moves toward `BOTTOM`.
pub(crate) trait Lattice: Copy + PartialEq {
    const TOP: Self;
    const BOTTOM: Self;
    fn meet(self, other: Self) -> Self;
}

/// What the solution says about a branch selector.
pub(crate) enum Selector {
    /// Not reached yet: take no edge, and come back when it lowers.
    Pending,
    /// A known raw bit pattern, read the way `cbr_taken`/`switch_taken` read
    /// one.
    Value(i128),
    /// Anything.
    Unknown,
}

/// Which way a `Cbr` or `Switch` goes.
enum Taken {
    Pending,
    One(BasicBlockId),
    Any,
}

/// The solver state every analysis shares.
pub(crate) struct Sparse<V> {
    /// Lattice value per pseudo, indexed by `PseudoId.0`.
    vals: Vec<V>,
    /// Blocks proved reachable.
    executable_block: HashSet<BasicBlockId>,
    /// CFG edges proved taken, as `(pred, succ)`.
    executable_edge: HashSet<(BasicBlockId, BasicBlockId)>,
    cfg_worklist: VecDeque<(BasicBlockId, BasicBlockId)>,
    /// Blocks to re-evaluate whole, for a fact that is not a pseudo's value.
    block_worklist: VecDeque<BasicBlockId>,
    ssa_worklist: VecDeque<PseudoId>,
    /// Instruction sites that read each pseudo.
    uses: HashMap<PseudoId, Vec<Site>>,
    /// Pseudos that must never be folded whatever the lattice says.
    unfoldable: HashSet<PseudoId>,
    steps: usize,
}

impl<V: Lattice> Sparse<V> {
    /// Index `func`'s readers and seed every pseudo. A constant's seed is the
    /// analysis's to choose: `constant` maps its raw bits to a lattice value.
    pub(crate) fn new(func: &Function, constant: impl Fn(i128) -> V) -> Self {
        let mut s = Sparse {
            vals: vec![V::TOP; func.next_pseudo as usize + 1],
            executable_block: HashSet::new(),
            executable_edge: HashSet::new(),
            cfg_worklist: VecDeque::new(),
            block_worklist: VecDeque::new(),
            ssa_worklist: VecDeque::new(),
            uses: HashMap::new(),
            unfoldable: HashSet::new(),
            steps: 0,
        };
        for (b, bb) in func.blocks.iter().enumerate() {
            for (i, insn) in bb.insns.iter().enumerate() {
                for u in insn.uses() {
                    s.uses.entry(u).or_default().push((b, i));
                }
            }
        }
        s.seed(func, constant);
        s
    }

    fn seed(&mut self, func: &Function, constant: impl Fn(i128) -> V) {
        for p in &func.pseudos {
            let Some(cell) = self.vals.get_mut(p.id.0 as usize) else {
                continue;
            };
            *cell = match &p.kind {
                PseudoKind::Val(v) => constant(*v),
                PseudoKind::Sym(_) => {
                    self.unfoldable.insert(p.id);
                    V::BOTTOM
                }
                // An argument can be anything the caller passes. Float
                // constants are in neither lattice: a folded one would need a
                // correctly sized `SetVal`, and both allocators default an
                // `FVal` with no defining `SetVal` to 64 bits.
                PseudoKind::Arg(_) | PseudoKind::Undef | PseudoKind::FVal(_) => V::BOTTOM,
                _ => V::TOP,
            };
        }
        // See `Function::asm_defined_pseudos`.
        for out in func.asm_defined_pseudos() {
            self.unfoldable.insert(out);
            if let Some(cell) = self.vals.get_mut(out.0 as usize) {
                *cell = V::BOTTOM;
            }
        }
    }

    pub(crate) fn get(&self, id: PseudoId) -> V {
        self.vals.get(id.0 as usize).copied().unwrap_or(V::BOTTOM)
    }

    pub(crate) fn is_executable(&self, block: BasicBlockId) -> bool {
        self.executable_block.contains(&block)
    }

    pub(crate) fn is_edge_executable(&self, from: BasicBlockId, to: BasicBlockId) -> bool {
        self.executable_edge.contains(&(from, to))
    }

    /// Re-evaluate `site` whenever `id` moves.
    pub(crate) fn watch(&mut self, id: PseudoId, site: Site) {
        let sites = self.uses.entry(id).or_default();
        if !sites.contains(&site) {
            sites.push(site);
        }
    }

    /// Re-evaluate all of `block`.
    pub(crate) fn revisit(&mut self, block: BasicBlockId) {
        self.block_worklist.push_back(block);
    }

    #[cfg(test)]
    pub(crate) fn readers(&self, id: PseudoId) -> &[Site] {
        self.uses.get(&id).map_or(&[], Vec::as_slice)
    }
}

/// An analysis the sparse solver drives.
pub(crate) trait SparseAnalysis {
    type V: Lattice;

    /// Total instruction evaluations before the solution is abandoned, for a
    /// lattice whose height alone does not bound the work.
    const STEP_BUDGET: Option<usize> = None;

    fn core(&self) -> &Sparse<Self::V>;
    fn core_mut(&mut self) -> &mut Sparse<Self::V>;

    /// The value `insn`, in `block`, produces. Never asked of a terminator.
    ///
    /// The default must be `BOTTOM`: an unmodelled opcode answering anything
    /// else is a licence to prove any branch below it dead.
    fn transfer(&self, insn: &Instruction, block: BasicBlockId) -> Self::V;

    /// What the branch selector `id` is, as seen from `block`.
    fn selector(&self, block: BasicBlockId, id: PseudoId) -> Selector;

    /// The constant `target`, defined in `block`, is proved to hold, in the
    /// form `propagate::fold_target_to_const` takes.
    fn constant(&self, block: BasicBlockId, target: PseudoId) -> Option<i128>;

    /// Called after a terminator's edges are marked, for facts that hang off
    /// an edge rather than a pseudo.
    fn after_terminator(&mut self, _func: &Function, _site: Site) {}

    /// What a cell that is about to move to `merged` actually moves to.
    /// Widening lives here; the default is no widening at all, which is
    /// right only for a lattice of finite height.
    fn widen(&mut self, _id: PseudoId, merged: Self::V) -> Self::V {
        merged
    }

    /// Solve `func`, then rewrite what the solution proves. Returns whether
    /// `func` changed. A solve that ran out of budget changes nothing: cells
    /// not yet driven down are *too precise*, and acting on them deletes
    /// live code.
    fn run(&mut self, func: &mut Function) -> bool {
        if func.blocks.is_empty() || !self.solve(func) {
            return false;
        }
        self.apply(func)
    }

    /// Lower `id` to `v`, pushing its readers if it moved.
    fn set(&mut self, id: PseudoId, v: Self::V) {
        let old = self.core().get(id);
        let merged = old.meet(v);
        if merged == old {
            return;
        }
        let merged = self.widen(id, merged);
        if merged == old {
            return;
        }
        let core = self.core_mut();
        if let Some(cell) = core.vals.get_mut(id.0 as usize) {
            *cell = merged;
            core.ssa_worklist.push_back(id);
        }
    }

    /// Run to fixpoint. Returns whether the solve finished inside its budget.
    fn solve(&mut self, func: &Function) -> bool {
        self.mark_block(func, func.entry);
        for bb in &func.blocks {
            if bb.addr_taken {
                self.mark_block(func, bb.id);
            }
        }
        loop {
            if Self::STEP_BUDGET.is_some_and(|max| self.core().steps > max) {
                return false;
            }
            if let Some((_, to)) = self.core_mut().cfg_worklist.pop_front() {
                // A newly executable edge changes what the target's phis meet
                // over, even when no value moved.
                self.mark_block(func, to);
                if let Some(idx) = func.block_index(to) {
                    for i in 0..func.blocks[idx].insns.len() {
                        if func.blocks[idx].insns[i].op == Opcode::Phi {
                            self.eval_site(func, (idx, i));
                        }
                    }
                }
                continue;
            }
            if let Some(b) = self.core_mut().block_worklist.pop_front() {
                if let Some(idx) = func.block_index(b) {
                    for i in 0..func.blocks[idx].insns.len() {
                        self.eval_site(func, (idx, i));
                    }
                }
                continue;
            }
            if let Some(id) = self.core_mut().ssa_worklist.pop_front() {
                let sites = self.core().uses.get(&id).cloned().unwrap_or_default();
                for site in sites {
                    if self.core().is_executable(func.blocks[site.0].id) {
                        self.eval_site(func, site);
                    }
                }
                continue;
            }
            return true;
        }
    }

    fn mark_block(&mut self, func: &Function, id: BasicBlockId) {
        if !self.core_mut().executable_block.insert(id) {
            return;
        }
        let Some(idx) = func.block_index(id) else {
            return;
        };
        for i in 0..func.blocks[idx].insns.len() {
            self.eval_site(func, (idx, i));
        }
        // Edges the terminator alone does not show: what is not understood
        // is assumed to happen.
        let modelled_terminator = matches!(
            func.blocks[idx].insns.last().map(|i| i.op),
            Some(Opcode::Br) | Some(Opcode::Cbr) | Some(Opcode::Switch) | Some(Opcode::IndirectBr)
        );
        if func.blocks[idx].has_asm_goto() || !modelled_terminator {
            self.mark_all_successors(func, idx);
        }
    }

    fn mark_edge(&mut self, from: BasicBlockId, to: BasicBlockId) {
        let core = self.core_mut();
        if core.executable_edge.insert((from, to)) {
            core.cfg_worklist.push_back((from, to));
        }
    }

    fn mark_all_successors(&mut self, func: &Function, b: usize) {
        let block_id = func.blocks[b].id;
        for &succ in &func.blocks[b].children {
            self.mark_edge(block_id, succ);
        }
    }

    /// Evaluate one instruction: update its target's lattice value, or for a
    /// terminator, mark the successor edges it can take.
    fn eval_site(&mut self, func: &Function, (b, i): Site) {
        self.core_mut().steps += 1;
        let insn = &func.blocks[b].insns[i];
        let block_id = func.blocks[b].id;

        match insn.op {
            Opcode::Br => {
                if let Some(t) = insn.bb_true {
                    self.mark_edge(block_id, t);
                }
            }
            Opcode::Cbr | Opcode::Switch => {
                match taken(self, insn, block_id) {
                    Taken::Pending => {}
                    Taken::One(t) => self.mark_edge(block_id, t),
                    Taken::Any => self.mark_all_successors(func, b),
                }
                self.after_terminator(func, (b, i));
            }
            Opcode::IndirectBr => self.mark_all_successors(func, b),
            _ => {
                let Some(target) = insn.target else {
                    return;
                };
                if self.core().unfoldable.contains(&target) {
                    return;
                }
                let v = self.transfer(insn, block_id);
                self.set(target, v);
            }
        }
    }

    /// Rewrite what the solution proves, returning whether anything changed.
    fn apply(&self, func: &mut Function) -> bool {
        let mut changed = false;
        // One pseudo per distinct constant per run, so repeated folds do not
        // inflate the pseudo table.
        let mut minted: HashMap<i128, PseudoId> = HashMap::new();

        // Values first: a folded condition is what lets the terminator below
        // it fold in this same run.
        for b in 0..func.blocks.len() {
            let block_id = func.blocks[b].id;
            if !self.core().is_executable(block_id) {
                continue;
            }
            for i in 0..func.blocks[b].insns.len() {
                let Some(target) = func.blocks[b].insns[i].target else {
                    continue;
                };
                if self.core().unfoldable.contains(&target) {
                    continue;
                }
                if let Some(v) = self.constant(block_id, target) {
                    changed |= propagate::fold_target_to_const(func, (b, i), v, &mut minted);
                }
            }
        }

        for b in 0..func.blocks.len() {
            let block_id = func.blocks[b].id;
            if !self.core().is_executable(block_id) {
                continue;
            }
            let Some(insn) = func.blocks[b].insns.last() else {
                continue;
            };
            if !matches!(insn.op, Opcode::Cbr | Opcode::Switch) {
                continue;
            }
            if let Taken::One(t) = taken(self, insn, block_id) {
                changed |= propagate::retarget_terminator(func, b, t);
            }
        }
        changed
    }
}

/// Which way the `Cbr` or `Switch` `insn` in `block` goes, by `a`'s solution.
fn taken<A: SparseAnalysis + ?Sized>(a: &A, insn: &Instruction, block: BasicBlockId) -> Taken {
    let Some(&cond) = insn.src.first() else {
        return Taken::Any;
    };
    let v = match a.selector(block, cond) {
        Selector::Pending => return Taken::Pending,
        Selector::Unknown => return Taken::Any,
        Selector::Value(v) => v,
    };
    let target = match insn.op {
        // A width-ambiguous constant proves nothing.
        Opcode::Cbr => cbr_taken(v).and_then(|t| if t { insn.bb_true } else { insn.bb_false }),
        Opcode::Switch => switch_taken(insn, v),
        _ => None,
    };
    target.map_or(Taken::Any, Taken::One)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, Pseudo};
    use crate::target::Target;
    use crate::types::TypeTable;

    #[derive(Clone, Copy, PartialEq, Debug)]
    enum Flat {
        Top,
        Const(i128),
        Bottom,
    }

    impl Lattice for Flat {
        const TOP: Flat = Flat::Top;
        const BOTTOM: Flat = Flat::Bottom;

        fn meet(self, other: Flat) -> Flat {
            match (self, other) {
                (Flat::Top, x) | (x, Flat::Top) => x,
                (a, b) if a == b => a,
                _ => Flat::Bottom,
            }
        }
    }

    /// An analysis that knows only what the seeding tells it, so every
    /// outcome below is the shared solver's doing. `STARVED` gives it no
    /// step budget at all.
    struct Probe<const STARVED: bool> {
        core: Sparse<Flat>,
    }

    impl<const STARVED: bool> Probe<STARVED> {
        fn new(func: &Function) -> Self {
            Probe {
                core: Sparse::new(func, Flat::Const),
            }
        }
    }

    impl<const STARVED: bool> SparseAnalysis for Probe<STARVED> {
        type V = Flat;

        const STEP_BUDGET: Option<usize> = if STARVED { Some(0) } else { None };

        fn core(&self) -> &Sparse<Flat> {
            &self.core
        }

        fn core_mut(&mut self) -> &mut Sparse<Flat> {
            &mut self.core
        }

        fn transfer(&self, _insn: &Instruction, _block: BasicBlockId) -> Flat {
            Flat::Bottom
        }

        fn selector(&self, _block: BasicBlockId, id: PseudoId) -> Selector {
            match self.core.get(id) {
                Flat::Top => Selector::Pending,
                Flat::Const(v) => Selector::Value(v),
                Flat::Bottom => Selector::Unknown,
            }
        }

        fn constant(&self, _block: BasicBlockId, _target: PseudoId) -> Option<i128> {
            None
        }
    }

    /// `entry: cbr %1, .L1, .L2`, each arm returning.
    fn branch_on(cond: Pseudo) -> Function {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("test", types.int_id);
        let c = cond.id;
        func.add_pseudo(cond);
        func.next_pseudo = 4;
        let (entry, then, els) = (BasicBlockId(0), BasicBlockId(1), BasicBlockId(2));

        let mut b0 = BasicBlock::new(entry);
        b0.add_insn(Instruction::new(Opcode::Entry));
        b0.add_insn(Instruction::cbr(c, then, els));
        b0.children = vec![then, els];
        func.add_block(b0);
        for id in [then, els] {
            let mut b = BasicBlock::new(id);
            b.add_insn(Instruction::ret(None));
            b.parents = vec![entry];
            func.add_block(b);
        }
        func.entry = entry;
        func
    }

    #[test]
    fn dataflow_folds_a_branch_its_selector_decides() {
        let mut func = branch_on(Pseudo::val(PseudoId(1), 0));
        assert!(Probe::<false>::new(&func).run(&mut func));
        let t = func.blocks[0].insns.last().unwrap();
        assert_eq!(t.op, Opcode::Br);
        assert_eq!(t.bb_true, Some(BasicBlockId(2)));
        assert_eq!(func.blocks[0].children, vec![BasicBlockId(2)]);
    }

    /// Cells not yet driven down are too precise, so a solve cut short by
    /// its budget must change nothing -- even what it had already decided.
    #[test]
    fn dataflow_applies_nothing_from_an_unfinished_solve() {
        let mut func = branch_on(Pseudo::val(PseudoId(1), 0));
        assert!(!Probe::<true>::new(&func).run(&mut func));
        assert_eq!(func.blocks[0].insns.last().unwrap().op, Opcode::Cbr);
        assert_eq!(func.blocks[0].children.len(), 2);
    }

    /// A selector still `TOP` takes no edge yet; one that is overdefined
    /// takes every edge.
    #[test]
    fn dataflow_marks_edges_by_what_the_selector_is() {
        let func = branch_on(Pseudo::reg(PseudoId(1), 1));
        let mut pending = Probe::<false>::new(&func);
        assert!(pending.solve(&func));
        assert!(pending.core.is_executable(BasicBlockId(0)));
        assert!(!pending.core.is_executable(BasicBlockId(1)));
        assert!(!pending.core.is_executable(BasicBlockId(2)));

        let func = branch_on(Pseudo::undef(PseudoId(1)));
        let mut unknown = Probe::<false>::new(&func);
        assert!(unknown.solve(&func));
        assert!(unknown
            .core
            .is_edge_executable(BasicBlockId(0), BasicBlockId(1)));
        assert!(unknown
            .core
            .is_edge_executable(BasicBlockId(0), BasicBlockId(2)));
    }

    /// The seeding decisions every analysis inherits. `Undef` in particular
    /// must not be `TOP`: at fixpoint, a `TOP` branch condition marks neither
    /// arm, and both would be deleted.
    #[test]
    fn dataflow_seeds_what_the_caller_cannot_know_as_bottom() {
        let mut func = branch_on(Pseudo::val(PseudoId(1), 7));
        func.add_pseudo(Pseudo::undef(PseudoId(2)));
        func.add_pseudo(Pseudo::reg(PseudoId(3), 3));
        let core = Sparse::new(&func, Flat::Const);
        assert_eq!(core.get(PseudoId(1)), Flat::Const(7));
        assert_eq!(core.get(PseudoId(2)), Flat::Bottom);
        assert_eq!(core.get(PseudoId(3)), Flat::Top);
        assert_eq!(core.get(PseudoId(99)), Flat::Bottom, "out of range");
    }
}
