//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Value-range propagation: what set of values can a pseudo hold, and what
// does a branch condition tell you about its operands on each arm.
//
// Wegman-Zadeck in the shape of `sccp`, with `range::Range` in place of a
// single constant and one thing `sccp` has no notion of: a fact attached to
// a CFG *edge*. `if (var <= 0) ... else ...` says nothing about `var` in
// general and says `var >= 1` on the else arm, and that is the whole reason
// this pass exists -- `sccp` cannot represent it, and `instcombine` can only
// relate two comparisons over the same operand pair.
//
// Three things here are load-bearing and easy to get wrong.
//
// **A fact belongs to an edge, and may only be used where that edge is the
// only way in.** There is no critical-edge splitting in this compiler, so a
// fact on `(P, B)` is usable at the top of `B` only when every execution of
// `B` arrives over it. `refining_pred` decides that, and excludes the entry
// block and any block whose address is taken -- `&&label` sets `addr_taken`
// and adds *no* CFG edge, so a single recorded predecessor there is a lie.
//
// **Termination is stated over moves, not over lattice height.** `sccp`
// descends at most twice per cell because its lattice is three tall. A range
// can descend through `2^width` sets, and `Range::union` is not monotone in
// its arguments, so neither argument survives. Instead every cell and every
// edge-fact map is allowed `MAX_MOVES` changes and is then pinned at the
// bottom of its chain, which bounds the work outright.
//
// **A partial solve must never be applied.** Cells that have not yet been
// driven down are *too precise*, so acting on them deletes live code. A
// budget overrun discards the whole solution rather than keeping the part
// that looks finished.
//

use super::constfold::{
    cmp_mask, cmp_operand_width, eval_binop, eval_unop, get_cmp_info, is_comparison,
    result_type_of, CMP_ALL, CMP_EQ, CMP_GT, CMP_LT,
};
use super::facts::{CmpFacts, ConstMap};
use super::propagate::{self, cbr_taken, switch_taken, Site};
use super::range::{allowed_by_predicate, possible_orderings, Range};
use super::{BasicBlockId, Function, Instruction, Opcode, PseudoId, PseudoKind};
use crate::types::TypeTable;
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};

/// How many times one cell may move before it is pinned at the bottom of its
/// chain. Not a tuning knob: the termination proof.
const MAX_MOVES: u16 = 4;

/// How deep the boolification peel goes when deriving an edge fact.
const MAX_PEEL: usize = 4;

/// Total instruction evaluations before the solution is abandoned.
const MAX_STEPS: usize = 200_000;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum RVal {
    /// Not yet reached.
    Top,
    Known(Range),
    /// Nothing known, and no width at which to say so. Distinct from a full
    /// range, because a pseudo can have no knowable width at all.
    Bottom,
}

impl RVal {
    fn meet(self, other: RVal) -> RVal {
        match (self, other) {
            (RVal::Top, x) | (x, RVal::Top) => x,
            (RVal::Bottom, _) | (_, RVal::Bottom) => RVal::Bottom,
            (RVal::Known(a), RVal::Known(b)) if a.width() == b.width() => RVal::Known(a.union(&b)),
            // Two widths are two facts about two different things.
            _ => RVal::Bottom,
        }
    }
}

/// Run value-range propagation over `func`, returning whether it changed.
pub fn run(func: &mut Function, types: &TypeTable) -> bool {
    if func.blocks.is_empty() {
        return false;
    }
    let mut solver = Solver::new(func, types);
    if !solver.solve(func) {
        // Budget exhausted. The partial solution is too precise to act on.
        return false;
    }
    solver.apply(func, types)
}

struct Solver<'a> {
    types: &'a TypeTable,
    consts: ConstMap,
    cmps: CmpFacts,
    /// Width of the value each pseudo's definition produces; 0 = unknown.
    widths: Vec<u32>,
    vals: Vec<RVal>,
    moves: Vec<u16>,
    /// Predecessors, derived from `children` rather than read from
    /// `parents`: `dce::fold_branches_to_unreachable` updates only the
    /// former, so `parents` goes stale and a missing entry would make the
    /// refinement test answer yes when two paths reach a block.
    preds: HashMap<BasicBlockId, Vec<BasicBlockId>>,
    /// Ranges that hold on a CFG edge. `BTreeMap` so iteration order, and
    /// therefore the generated code, does not depend on a hash seed.
    edge_facts: HashMap<(BasicBlockId, BasicBlockId), BTreeMap<PseudoId, Range>>,
    edge_moves: HashMap<(BasicBlockId, BasicBlockId), u16>,
    executable_block: HashSet<BasicBlockId>,
    executable_edge: HashSet<(BasicBlockId, BasicBlockId)>,
    cfg_worklist: VecDeque<(BasicBlockId, BasicBlockId)>,
    block_worklist: VecDeque<BasicBlockId>,
    ssa_worklist: VecDeque<PseudoId>,
    uses: HashMap<PseudoId, Vec<Site>>,
    unfoldable: HashSet<PseudoId>,
    steps: usize,
}

impl<'a> Solver<'a> {
    fn new(func: &Function, types: &'a TypeTable) -> Self {
        let consts = ConstMap::new(func);
        let cmps = CmpFacts::new(func, &consts);
        let n = func.next_pseudo as usize + 1;
        let mut s = Solver {
            types,
            consts,
            cmps,
            widths: vec![0; n],
            vals: vec![RVal::Top; n],
            moves: vec![0; n],
            preds: build_preds(func),
            edge_facts: HashMap::new(),
            edge_moves: HashMap::new(),
            executable_block: HashSet::new(),
            executable_edge: HashSet::new(),
            cfg_worklist: VecDeque::new(),
            block_worklist: VecDeque::new(),
            ssa_worklist: VecDeque::new(),
            uses: HashMap::new(),
            unfoldable: HashSet::new(),
            steps: 0,
        };
        s.index(func);
        s.seed(func);
        s
    }

    fn index(&mut self, func: &Function) {
        for (b, bb) in func.blocks.iter().enumerate() {
            for (i, insn) in bb.insns.iter().enumerate() {
                for u in insn.uses() {
                    self.uses.entry(u).or_default().push((b, i));
                }
                // The width a definition produces comes from
                // `result_type_of`, never from `insn.size`: on a comparison
                // that field describes the *operands*.
                if let Some(t) = insn.target {
                    let idx = t.0 as usize;
                    if idx < self.widths.len() {
                        self.widths[idx] = result_type_of(insn, self.types).1;
                    }
                }
            }
        }
    }

    /// Initial values and the entry edge.
    ///
    /// Copied from `sccp::seed`, decision for decision: `Undef` is `Bottom`
    /// and not `Top`, a `Sym` names storage rather than a value, an
    /// inline-asm output is a second definition invariant I1 exempts, and a
    /// block whose address is taken is reachable with no CFG edge. Each of
    /// those is a way the algorithm turns unsound, not a refinement.
    fn seed(&mut self, func: &Function) {
        for p in &func.pseudos {
            let idx = p.id.0 as usize;
            if idx >= self.vals.len() {
                continue;
            }
            self.vals[idx] = match &p.kind {
                // A constant carries no width of its own; it is read at
                // whatever width its consumer asks for. See `operand`.
                PseudoKind::Val(_) => RVal::Top,
                PseudoKind::Arg(_) => RVal::Bottom,
                PseudoKind::Sym(_) => {
                    self.unfoldable.insert(p.id);
                    RVal::Bottom
                }
                PseudoKind::Undef => RVal::Bottom,
                PseudoKind::FVal(_) => RVal::Bottom,
                _ => RVal::Top,
            };
        }
        for bb in &func.blocks {
            for insn in &bb.insns {
                if let Some(ref asm) = insn.asm_data {
                    for out in &asm.outputs {
                        self.unfoldable.insert(out.pseudo);
                        let idx = out.pseudo.0 as usize;
                        if idx < self.vals.len() {
                            self.vals[idx] = RVal::Bottom;
                        }
                    }
                }
            }
        }
        let entry = func.entry;
        self.mark_block(func, entry);
        for bb in &func.blocks {
            if bb.addr_taken {
                self.mark_block(func, bb.id);
            }
        }
    }

    fn get(&self, id: PseudoId) -> RVal {
        self.vals
            .get(id.0 as usize)
            .copied()
            .unwrap_or(RVal::Bottom)
    }

    fn width_of(&self, id: PseudoId) -> u32 {
        self.widths.get(id.0 as usize).copied().unwrap_or(0)
    }

    /// Lower `id` to `v`, pushing its readers if it moved.
    ///
    /// Widening lives here: a cell that has moved `MAX_MOVES` times is
    /// forced to the bottom of its chain and can never move again, which is
    /// what makes the whole solve terminate.
    fn set(&mut self, id: PseudoId, v: RVal) {
        let idx = id.0 as usize;
        if idx >= self.vals.len() {
            return;
        }
        let merged = self.vals[idx].meet(v);
        if merged == self.vals[idx] {
            return;
        }
        self.moves[idx] = self.moves[idx].saturating_add(1);
        let merged = if self.moves[idx] >= MAX_MOVES {
            match merged {
                RVal::Known(r) => RVal::Known(Range::full(r.width())),
                other => other,
            }
        } else {
            merged
        };
        if merged == self.vals[idx] {
            return;
        }
        self.vals[idx] = merged;
        self.ssa_worklist.push_back(id);
    }

    /// The range of `id` as an instruction in `block` sees it, read at
    /// `width` bits.
    fn operand(&self, block: BasicBlockId, id: PseudoId, width: u32) -> RVal {
        let Some(width) = Range::at(width) else {
            return RVal::Bottom;
        };
        // A constant is width-polymorphic: it is read at the consumer's
        // width, through the one bridge that honours the IR's raw-bit
        // convention. This is where a range is strictly better than a bare
        // `i128` -- `unambiguous_at` exists because a raw constant cannot
        // say whether `4294967295` is `-1`, and a range at a stated width
        // never has that question.
        let root = self.consts.root(id, width);
        let base = if let Some(v) = self.const_of(root) {
            RVal::Known(Range::from_const(width, v))
        } else {
            match self.get(root) {
                RVal::Known(r) if r.width() == width => RVal::Known(r),
                RVal::Known(r) if r.width() > width => RVal::Known(r.trunc(width)),
                // A narrower fact says nothing about the wider read: a
                // widening in this IR is an explicit `Zext`/`Sext`.
                RVal::Known(_) => RVal::Known(Range::full(width)),
                RVal::Top => RVal::Top,
                RVal::Bottom => RVal::Known(Range::full(width)),
            }
        };
        // Then narrow by whatever the edge into this block proved.
        match (base, self.entry_fact(block, root)) {
            (RVal::Known(r), Some(f)) if f.width() == r.width() => {
                RVal::Known(r.intersect_exact(&f).unwrap_or(r))
            }
            (b, _) => b,
        }
    }

    fn const_of(&self, id: PseudoId) -> Option<i128> {
        self.consts.get(id)
    }

    /// The refinement that holds for `id` at the top of `block`.
    fn entry_fact(&self, block: BasicBlockId, id: PseudoId) -> Option<Range> {
        let pred = self.refining_pred(block)?;
        self.edge_facts.get(&(pred, block))?.get(&id).copied()
    }

    /// The single predecessor whose edge facts govern all of `b`, if any.
    fn refining_pred(&self, b: BasicBlockId) -> Option<BasicBlockId> {
        match self.preds.get(&b)?.as_slice() {
            [p] => Some(*p),
            _ => None,
        }
    }

    fn mark_block(&mut self, func: &Function, id: BasicBlockId) {
        if !self.executable_block.insert(id) {
            return;
        }
        let Some(idx) = func.block_index(id) else {
            return;
        };
        for i in 0..func.blocks[idx].insns.len() {
            self.eval_site(func, (idx, i));
        }
        // `asm goto`'s block ends in an ordinary `Br` to the fallthrough and
        // keeps its real targets in `asm_data.goto_labels`, so marking only
        // what the terminator names would make those arms look unreachable.
        let has_asm_goto = func.blocks[idx].insns.iter().any(|i| {
            i.asm_data
                .as_ref()
                .is_some_and(|d| !d.goto_labels.is_empty())
        });
        let modelled_terminator = matches!(
            func.blocks[idx].insns.last().map(|i| i.op),
            Some(Opcode::Br) | Some(Opcode::Cbr) | Some(Opcode::Switch) | Some(Opcode::IndirectBr)
        );
        if has_asm_goto || !modelled_terminator {
            let block_id = func.blocks[idx].id;
            let succs: Vec<BasicBlockId> = func.blocks[idx].children.clone();
            for s in succs {
                self.mark_edge(block_id, s);
            }
            for i in 0..func.blocks[idx].insns.len() {
                let labels: Vec<BasicBlockId> = func.blocks[idx].insns[i]
                    .asm_data
                    .as_ref()
                    .map(|d| d.goto_labels.iter().map(|(b, _)| *b).collect())
                    .unwrap_or_default();
                for target in labels {
                    self.mark_edge(block_id, target);
                }
            }
        }
    }

    fn mark_edge(&mut self, from: BasicBlockId, to: BasicBlockId) {
        if self.executable_edge.insert((from, to)) {
            self.cfg_worklist.push_back((from, to));
        }
    }

    /// Returns whether the solve finished inside its budget.
    fn solve(&mut self, func: &Function) -> bool {
        loop {
            if self.steps > MAX_STEPS {
                return false;
            }
            if let Some((_, to)) = self.cfg_worklist.pop_front() {
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
            if let Some(b) = self.block_worklist.pop_front() {
                if let Some(idx) = func.block_index(b) {
                    for i in 0..func.blocks[idx].insns.len() {
                        self.eval_site(func, (idx, i));
                    }
                }
                continue;
            }
            if let Some(id) = self.ssa_worklist.pop_front() {
                for site in self.uses.get(&id).cloned().unwrap_or_default() {
                    if self.executable_block.contains(&func.blocks[site.0].id) {
                        self.eval_site(func, site);
                    }
                }
                continue;
            }
            return true;
        }
    }

    fn eval_site(&mut self, func: &Function, (b, i): Site) {
        self.steps += 1;
        let insn = &func.blocks[b].insns[i];
        let block_id = func.blocks[b].id;

        match insn.op {
            Opcode::Br => {
                if let Some(t) = insn.bb_true {
                    self.mark_edge(block_id, t);
                }
                return;
            }
            Opcode::Cbr => {
                let (Some(t), Some(f)) = (insn.bb_true, insn.bb_false) else {
                    return;
                };
                let cond = insn.src.first().copied();
                match cond.map(|c| self.branch_value(block_id, c)) {
                    Some(Some(true)) => self.mark_edge(block_id, t),
                    Some(Some(false)) => self.mark_edge(block_id, f),
                    Some(None) => {
                        self.mark_edge(block_id, t);
                        self.mark_edge(block_id, f);
                    }
                    None => {
                        self.mark_edge(block_id, t);
                        self.mark_edge(block_id, f);
                    }
                }
                // A branch with both arms on one block distinguishes
                // nothing, so it proves nothing about its condition.
                if t != f {
                    if let Some(c) = cond {
                        self.refine_edge(func, (b, i), block_id, t, c, true);
                        self.refine_edge(func, (b, i), block_id, f, c, false);
                    }
                }
                return;
            }
            Opcode::Switch => {
                let taken = insn
                    .src
                    .first()
                    .and_then(|s| self.single_const(block_id, *s))
                    .and_then(|v| switch_taken(insn, v));
                match taken {
                    Some(t) => self.mark_edge(block_id, t),
                    None => {
                        let succs: Vec<BasicBlockId> = func.blocks[b].children.clone();
                        for s in succs {
                            self.mark_edge(block_id, s);
                        }
                    }
                }
                return;
            }
            Opcode::IndirectBr => {
                let succs: Vec<BasicBlockId> = func.blocks[b].children.clone();
                for s in succs {
                    self.mark_edge(block_id, s);
                }
                return;
            }
            _ => {}
        }

        let Some(target) = insn.target else {
            return;
        };
        let v = self.transfer(insn, block_id);
        self.set(target, v);
    }

    /// The condition's value as a branch would test it, if it is known.
    fn branch_value(&self, block: BasicBlockId, cond: PseudoId) -> Option<bool> {
        let v = self.single_const(block, cond)?;
        cbr_taken(v)
    }

    /// `id`'s value when its range holds exactly one.
    fn single_const(&self, block: BasicBlockId, id: PseudoId) -> Option<i128> {
        let w = self
            .width_of(self.consts.root(id, 1))
            .max(self.width_of(id));
        let w = Range::at(w)?;
        match self.operand(block, id, w) {
            RVal::Known(r) => r.single_value().map(|v| v as i128),
            _ => None,
        }
    }

    /// Record what `cond` being `want` proves about its operands on the edge
    /// into `succ`.
    fn refine_edge(
        &mut self,
        func: &Function,
        site: Site,
        pred: BasicBlockId,
        succ: BasicBlockId,
        cond: PseudoId,
        want: bool,
    ) {
        // A fact is only usable where the edge is the only way in, so there
        // is no point deriving one otherwise.
        if self.refining_pred(succ) != Some(pred) {
            return;
        }
        if succ == func.entry {
            return;
        }
        // `&&label` sets `addr_taken` and adds no CFG edge, so one recorded
        // predecessor does not mean one way in.
        if func.get_block(succ).is_some_and(|b| b.addr_taken) {
            return;
        }
        let key = (pred, succ);
        if self.edge_moves.get(&key).copied().unwrap_or(0) >= MAX_MOVES {
            return;
        }

        let mut out: BTreeMap<PseudoId, Range> = BTreeMap::new();
        let mut work: Vec<(PseudoId, bool)> = vec![(cond, want)];
        let mut seen = 0usize;
        while let Some((p, w)) = work.pop() {
            seen += 1;
            if seen > MAX_PEEL {
                break;
            }
            let width = self.width_of(p);
            let Some(width) = Range::at(width) else {
                continue;
            };
            let Some(fact) = self.cmps.get_through(&self.consts, p, width) else {
                continue;
            };
            let m = if w { fact.mask } else { !fact.mask & CMP_ALL };
            // `(_Bool)(x <= 0)` reaches a branch as `setne %t, 0` over the
            // real comparison, so the fact at the `Cbr` is about a boolean.
            // Peel through to what it tests, then keep going: `x != 0` also
            // refines `x` itself, exactly.
            if self.consts.get(fact.rhs) == Some(0) {
                if fact.mask == CMP_EQ {
                    work.push((fact.lhs, !w));
                } else if fact.mask == CMP_LT | CMP_GT {
                    work.push((fact.lhs, w));
                }
            }
            // The fact is read off the operands' *ranges*, and a range can
            // widen without the comparison's own value changing at all: a
            // loop counter growing from `{0}` never makes `count == bound`
            // decidable either way. Re-evaluating this branch only when its
            // condition moves therefore froze the first fact derived -- the
            // narrowest, and so the strongest -- for the rest of the solve,
            // and `bound` came out of the loop a constant. Making the branch
            // a reader of what it reads is what keeps the fact honest.
            self.watch(fact.lhs, site);
            self.watch(fact.rhs, site);
            let lhs_r = self.range_or_full(pred, fact.lhs, fact.width);
            let rhs_r = self.range_or_full(pred, fact.rhs, fact.width);
            record_fact(
                &mut out,
                fact.lhs,
                allowed_by_predicate(m, fact.signed, &rhs_r),
            );
            record_fact(
                &mut out,
                fact.rhs,
                allowed_by_predicate(super::constfold::mirror_mask(m), fact.signed, &lhs_r),
            );
        }

        if self.edge_facts.get(&key) == Some(&out) {
            return;
        }
        *self.edge_moves.entry(key).or_insert(0) += 1;
        if self.edge_moves[&key] >= MAX_MOVES {
            // Bottom of this chain: no refinement at all, permanently.
            self.edge_facts.insert(key, BTreeMap::new());
        } else {
            self.edge_facts.insert(key, out);
        }
        self.block_worklist.push_back(succ);
    }

    /// Re-evaluate `site` whenever `id` moves.
    fn watch(&mut self, id: PseudoId, site: Site) {
        let sites = self.uses.entry(id).or_default();
        if !sites.contains(&site) {
            sites.push(site);
        }
    }

    /// `id`'s range as seen from `block`, or the whole space.
    fn range_or_full(&self, block: BasicBlockId, id: PseudoId, width: u32) -> Range {
        match self.operand(block, id, width) {
            RVal::Known(r) => r,
            _ => Range::at(width).map_or(Range::full(1), Range::full),
        }
    }

    fn transfer(&self, insn: &Instruction, block: BasicBlockId) -> RVal {
        let target_width = insn.target.map(|t| self.width_of(t)).unwrap_or(insn.size);

        match insn.op {
            Opcode::Copy | Opcode::PhiSource => self.operand(block, insn.src[0], insn.size),

            Opcode::SetVal => match insn.target.and_then(|t| self.const_of(t)) {
                Some(v) => match Range::at(insn.size) {
                    Some(w) => RVal::Known(Range::from_const(w, v)),
                    None => RVal::Bottom,
                },
                None => RVal::Bottom,
            },

            Opcode::Phi => {
                let mut acc = RVal::Top;
                for (pred, src) in &insn.phi_list {
                    if !self.executable_edge.contains(&(*pred, block)) {
                        continue;
                    }
                    acc = acc.meet(self.operand(block, *src, insn.size));
                }
                acc
            }

            Opcode::Select if insn.src.len() == 3 => match self.branch_value(block, insn.src[0]) {
                Some(true) => self.operand(block, insn.src[1], insn.size),
                Some(false) => self.operand(block, insn.src[2], insn.size),
                None => self
                    .operand(block, insn.src[1], insn.size)
                    .meet(self.operand(block, insn.src[2], insn.size)),
            },

            Opcode::Neg | Opcode::Not => self.unary(insn, block, target_width),
            Opcode::Zext | Opcode::Sext | Opcode::Trunc => self.convert(insn, block),

            _ if is_comparison(insn.op) && !insn.op.to_string().starts_with("fcmp") => {
                self.compare(insn, block, target_width)
            }

            _ if is_modelled_binop(insn.op) => self.binop(insn, block, target_width),

            // Everything else. An unmodelled opcode answering anything but
            // `Bottom` would be a licence to prove any branch below it dead,
            // so `Lo64`, `UMulHi`, `Load`, `Call` and the rest land here
            // deliberately.
            _ => RVal::Bottom,
        }
    }

    /// The `constfold` fast path, which keeps this pass from ever
    /// disagreeing with `sccp` about a constant -- and inherits every
    /// refusal it makes for undefined behaviour.
    fn folded(&self, insn: &Instruction, block: BasicBlockId, width: u32) -> Option<RVal> {
        let w = Range::at(width)?;
        let vals: Option<Vec<i128>> = insn
            .src
            .iter()
            .map(
                |s| match self.operand(block, *s, self.operand_width(insn)) {
                    RVal::Known(r) => r.single_value().map(|v| v as i128),
                    _ => None,
                },
            )
            .collect();
        let vals = vals?;
        let v = match vals.len() {
            1 => eval_unop(insn, vals[0])?,
            2 => eval_binop(insn, vals[0], vals[1])?,
            _ => return None,
        };
        Some(RVal::Known(Range::from_const(w, v)))
    }

    /// The width an instruction reads its operands at.
    fn operand_width(&self, insn: &Instruction) -> u32 {
        if is_comparison(insn.op) {
            cmp_operand_width(insn)
        } else if matches!(insn.op, Opcode::Zext | Opcode::Sext) {
            insn.src_size
        } else {
            insn.size
        }
    }

    fn unary(&self, insn: &Instruction, block: BasicBlockId, width: u32) -> RVal {
        if insn.src.len() != 1 {
            return RVal::Bottom;
        }
        if let Some(v) = self.folded(insn, block, width) {
            return v;
        }
        let Some(w) = Range::at(insn.size) else {
            return RVal::Bottom;
        };
        let RVal::Known(a) = self.operand(block, insn.src[0], w) else {
            return RVal::Bottom;
        };
        RVal::Known(match insn.op {
            Opcode::Neg => a.neg(),
            Opcode::Not => a.not(),
            _ => return RVal::Bottom,
        })
    }

    fn convert(&self, insn: &Instruction, block: BasicBlockId) -> RVal {
        if insn.src.len() != 1 {
            return RVal::Bottom;
        }
        let dst = insn.size;
        let src = if insn.op == Opcode::Trunc {
            insn.size.max(insn.src_size)
        } else {
            insn.src_size
        };
        // An extension with no recorded source width cannot be read at all;
        // guessing from the destination makes it the identity, which is how
        // a negative `char` comes back positive.
        let (Some(src), Some(dst)) = (Range::at(src), Range::at(dst)) else {
            return RVal::Bottom;
        };
        if let Some(v) = self.folded(insn, block, dst) {
            return v;
        }
        let RVal::Known(a) = self.operand(block, insn.src[0], src) else {
            return RVal::Bottom;
        };
        RVal::Known(match insn.op {
            Opcode::Zext => a.zext(dst),
            Opcode::Sext => a.sext(dst),
            Opcode::Trunc => a.trunc(dst),
            _ => return RVal::Bottom,
        })
    }

    fn binop(&self, insn: &Instruction, block: BasicBlockId, width: u32) -> RVal {
        if insn.src.len() != 2 {
            return RVal::Bottom;
        }
        if let Some(v) = self.folded(insn, block, width) {
            return v;
        }
        let Some(w) = Range::at(insn.size) else {
            return RVal::Bottom;
        };
        let (RVal::Known(a), RVal::Known(b)) = (
            self.operand(block, insn.src[0], w),
            self.operand(block, insn.src[1], w),
        ) else {
            return RVal::Bottom;
        };
        RVal::Known(match insn.op {
            Opcode::Add => a.add(&b),
            Opcode::Sub => a.sub(&b),
            Opcode::Mul => a.mul(&b),
            Opcode::DivU => a.udiv(&b),
            Opcode::ModU => a.umod(&b),
            // Signed division has the `SMIN / -1` overflow and a rounding
            // discontinuity at zero; the singleton case is already answered
            // by the `constfold` path above.
            Opcode::DivS | Opcode::ModS => Range::full(w),
            Opcode::And => a.and(&b),
            Opcode::Or => a.or(&b),
            Opcode::Xor => a.xor(&b),
            Opcode::Shl => a.shl(&b),
            Opcode::Lsr => a.lshr(&b),
            Opcode::Asr => a.ashr(&b),
            _ => return RVal::Bottom,
        })
    }

    fn compare(&self, insn: &Instruction, block: BasicBlockId, result_width: u32) -> RVal {
        if insn.src.len() != 2 {
            return RVal::Bottom;
        }
        let (Some(rw), Some(ow)) = (Range::at(result_width), Range::at(cmp_operand_width(insn)))
        else {
            return RVal::Bottom;
        };
        let Some(mask) = cmp_mask(insn.op) else {
            return RVal::Bottom;
        };
        let signed = get_cmp_info(insn.op).map(|i| i.signed).unwrap_or(true);
        // Comparing a value with itself is decided without knowing it.
        if self.consts.root(insn.src[0], ow) == self.consts.root(insn.src[1], ow) {
            if let Some(info) = get_cmp_info(insn.op) {
                return RVal::Known(Range::from_const(rw, info.identity_result));
            }
        }
        let (RVal::Known(a), RVal::Known(b)) = (
            self.operand(block, insn.src[0], ow),
            self.operand(block, insn.src[1], ow),
        ) else {
            return RVal::Bottom;
        };
        let possible = possible_orderings(&a, &b, signed);
        if possible == 0 {
            return RVal::Bottom;
        }
        if possible & mask == 0 {
            return RVal::Known(Range::from_const(rw, 0));
        }
        if possible & !mask & CMP_ALL == 0 {
            return RVal::Known(Range::from_const(rw, 1));
        }
        RVal::Known(Range::inclusive(rw, 0, 1))
    }

    fn apply(&mut self, func: &mut Function, types: &TypeTable) -> bool {
        let mut changed = false;
        let mut minted: HashMap<i128, PseudoId> = HashMap::new();

        for b in 0..func.blocks.len() {
            if !self.executable_block.contains(&func.blocks[b].id) {
                continue;
            }
            let block_id = func.blocks[b].id;
            for i in 0..func.blocks[b].insns.len() {
                let Some(target) = func.blocks[b].insns[i].target else {
                    continue;
                };
                if self.unfoldable.contains(&target) {
                    continue;
                }
                let w = self.width_of(target);
                let Some(w) = Range::at(w) else { continue };
                let RVal::Known(r) = self.operand(block_id, target, w) else {
                    continue;
                };
                let Some(v) = r.single_value() else { continue };
                let v = super::constfold::at_width(v as i128, w, true);
                changed |= propagate::fold_target_to_const(func, types, (b, i), v, &mut minted);
            }
        }

        for b in 0..func.blocks.len() {
            if !self.executable_block.contains(&func.blocks[b].id) {
                continue;
            }
            let block_id = func.blocks[b].id;
            let Some(insn) = func.blocks[b].insns.last() else {
                continue;
            };
            let taken = match insn.op {
                Opcode::Cbr => {
                    let (Some(t), Some(f)) = (insn.bb_true, insn.bb_false) else {
                        continue;
                    };
                    let Some(c) = insn.src.first().copied() else {
                        continue;
                    };
                    match self.branch_value(block_id, c) {
                        Some(true) => t,
                        Some(false) => f,
                        None => continue,
                    }
                }
                Opcode::Switch => {
                    let Some(c) = insn.src.first().copied() else {
                        continue;
                    };
                    let Some(v) = self.single_const(block_id, c) else {
                        continue;
                    };
                    match switch_taken(insn, v) {
                        Some(t) => t,
                        None => continue,
                    }
                }
                _ => continue,
            };
            changed |= propagate::retarget_terminator(func, b, taken);
        }
        changed
    }
}

/// Merge `r` into whatever is already recorded for `id`, keeping the first
/// when the two do not intersect as a single interval.
fn record_fact(out: &mut BTreeMap<PseudoId, Range>, id: PseudoId, r: Range) {
    if r.is_full() {
        return;
    }
    match out.get(&id) {
        Some(existing) => {
            if let Some(merged) = existing.intersect_exact(&r) {
                out.insert(id, merged);
            }
        }
        None => {
            out.insert(id, r);
        }
    }
}

/// Predecessors, from every block's recorded `children` plus every
/// terminator's targets and every `asm goto` label.
///
/// The union over-counts rather than under-counts, and over-counting only
/// ever suppresses a refinement. Reading `BasicBlock::parents` instead would
/// trust bookkeeping that `dce` does not maintain.
fn build_preds(func: &Function) -> HashMap<BasicBlockId, Vec<BasicBlockId>> {
    let mut preds: HashMap<BasicBlockId, Vec<BasicBlockId>> = HashMap::new();
    let add = |from: BasicBlockId, to: BasicBlockId, m: &mut HashMap<_, Vec<_>>| {
        let e: &mut Vec<BasicBlockId> = m.entry(to).or_default();
        if !e.contains(&from) {
            e.push(from);
        }
    };
    for bb in &func.blocks {
        for c in &bb.children {
            add(bb.id, *c, &mut preds);
        }
        for insn in &bb.insns {
            for t in propagate::terminator_targets(insn) {
                add(bb.id, t, &mut preds);
            }
            if let Some(ref asm) = insn.asm_data {
                for (t, _) in &asm.goto_labels {
                    add(bb.id, *t, &mut preds);
                }
            }
        }
    }
    preds
}

/// Binary opcodes with a range transfer function.
fn is_modelled_binop(op: Opcode) -> bool {
    matches!(
        op,
        Opcode::Add
            | Opcode::Sub
            | Opcode::Mul
            | Opcode::DivS
            | Opcode::DivU
            | Opcode::ModS
            | Opcode::ModU
            | Opcode::Shl
            | Opcode::Lsr
            | Opcode::Asr
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, Pseudo};
    use crate::target::Target;

    fn host_types() -> TypeTable {
        TypeTable::new(&Target::host())
    }

    /// The `20041114-1` shape, built by hand in the form `ssa_convert`
    /// leaves it -- the `PhiSource` conduits included.
    ///
    /// ```text
    /// .L0: %4 = setle.32 %var, 0        ; var <= 0
    ///      %5 = setne.32 %4, 0          ; boolification
    ///      phisrc 1 -> .L2:%21
    ///      cbr %5, .L2, .L1
    /// .L1: %10 = sub.32 %var, 1         ; var >= 1 on this edge
    ///      %11 = zext.32to64 %10
    ///      %18 = setb.64 %11, 0xFFFFFFFF
    ///      phisrc %18 -> .L2:%21
    ///      br .L2
    /// .L2: %21 = phi(1, %18)
    ///      cbr %21, .L5, .L3            ; .L3 is the arm to delete
    /// ```
    fn range_guard(extra_pred_on_l1: bool) -> Function {
        let types = host_types();
        let (i32t, i64t) = (types.int_id, types.long_id);
        let mut f = Function::new("foo", types.void_id);
        f.add_pseudo(Pseudo::arg(PseudoId(0), 0));
        f.add_pseudo(Pseudo::val(PseudoId(3), 0));
        f.add_pseudo(Pseudo::val(PseudoId(6), 0));
        f.add_pseudo(Pseudo::val(PseudoId(7), 1));
        f.add_pseudo(Pseudo::val(PseudoId(9), 1));
        f.add_pseudo(Pseudo::val(PseudoId(17), 0xFFFF_FFFF));
        f.next_pseudo = 40;

        let mut l0 = BasicBlock::new(BasicBlockId(0));
        l0.add_insn(Instruction::new(Opcode::Entry));
        l0.add_insn(Instruction::binop(
            Opcode::SetLe,
            PseudoId(4),
            PseudoId(0),
            PseudoId(3),
            i32t,
            32,
        ));
        l0.add_insn(Instruction::binop(
            Opcode::SetNe,
            PseudoId(5),
            PseudoId(4),
            PseudoId(6),
            i32t,
            32,
        ));
        let mut ps0 = Instruction::new(Opcode::PhiSource)
            .with_target(PseudoId(22))
            .with_src(PseudoId(7))
            .with_type_and_size(i32t, 32);
        ps0.phi_list.push((BasicBlockId(2), PseudoId(21)));
        l0.add_insn(ps0);
        let mut cbr = Instruction::new(Opcode::Cbr).with_src(PseudoId(5));
        cbr.bb_true = Some(BasicBlockId(2));
        cbr.bb_false = Some(BasicBlockId(1));
        l0.add_insn(cbr);
        l0.children = vec![BasicBlockId(2), BasicBlockId(1)];

        let mut l1 = BasicBlock::new(BasicBlockId(1));
        l1.add_insn(Instruction::binop(
            Opcode::Sub,
            PseudoId(10),
            PseudoId(0),
            PseudoId(9),
            i32t,
            32,
        ));
        let mut zext = Instruction::new(Opcode::Zext)
            .with_target(PseudoId(11))
            .with_src(PseudoId(10))
            .with_type_and_size(i64t, 64);
        zext.src_size = 32;
        l1.add_insn(zext);
        l1.add_insn(Instruction::binop(
            Opcode::SetB,
            PseudoId(18),
            PseudoId(11),
            PseudoId(17),
            i64t,
            64,
        ));
        let mut ps1 = Instruction::new(Opcode::PhiSource)
            .with_target(PseudoId(23))
            .with_src(PseudoId(18))
            .with_type_and_size(i32t, 32);
        ps1.phi_list.push((BasicBlockId(2), PseudoId(21)));
        l1.add_insn(ps1);
        let mut br = Instruction::new(Opcode::Br);
        br.bb_true = Some(BasicBlockId(2));
        l1.add_insn(br);
        l1.children = vec![BasicBlockId(2)];
        l1.parents = vec![BasicBlockId(0)];

        let mut l2 = BasicBlock::new(BasicBlockId(2));
        let mut phi = Instruction::new(Opcode::Phi)
            .with_target(PseudoId(21))
            .with_type_and_size(i32t, 32);
        phi.phi_list.push((BasicBlockId(0), PseudoId(22)));
        phi.phi_list.push((BasicBlockId(1), PseudoId(23)));
        l2.add_insn(phi);
        let mut guard = Instruction::new(Opcode::Cbr).with_src(PseudoId(21));
        guard.bb_true = Some(BasicBlockId(5));
        guard.bb_false = Some(BasicBlockId(3));
        l2.add_insn(guard);
        l2.children = vec![BasicBlockId(5), BasicBlockId(3)];
        l2.parents = vec![BasicBlockId(0), BasicBlockId(1)];

        let mut l3 = BasicBlock::new(BasicBlockId(3));
        l3.add_insn(Instruction::call(
            None,
            "link_failure",
            vec![],
            vec![],
            types.void_id,
            0,
        ));
        let mut br3 = Instruction::new(Opcode::Br);
        br3.bb_true = Some(BasicBlockId(5));
        l3.add_insn(br3);
        l3.children = vec![BasicBlockId(5)];
        l3.parents = vec![BasicBlockId(2)];

        let mut l5 = BasicBlock::new(BasicBlockId(5));
        l5.add_insn(Instruction::ret(None));
        l5.parents = vec![BasicBlockId(2), BasicBlockId(3)];

        // A second way into `.L1` makes the edge fact unusable.
        let mut l9 = BasicBlock::new(BasicBlockId(9));
        if extra_pred_on_l1 {
            let mut b9 = Instruction::new(Opcode::Br);
            b9.bb_true = Some(BasicBlockId(1));
            l9.add_insn(b9);
            l9.children = vec![BasicBlockId(1)];
            l1.parents.push(BasicBlockId(9));
        }

        f.add_block(l0);
        f.add_block(l1);
        f.add_block(l2);
        f.add_block(l3);
        f.add_block(l5);
        if extra_pred_on_l1 {
            f.add_block(l9);
        }
        f.entry = BasicBlockId(0);
        f
    }

    fn folds_the_guard(func: &Function) -> bool {
        let guard = func
            .get_block(BasicBlockId(2))
            .and_then(|b| b.insns.last())
            .unwrap();
        guard.op == Opcode::Br && guard.bb_true == Some(BasicBlockId(5))
    }

    /// The headline: `var <= 0` being false gives `var >= 1` on that edge,
    /// which carries through `sub`, `zext` and an unsigned compare to prove
    /// the guard and delete the `link_failure` arm.
    #[test]
    fn vrp_folds_a_guard_proved_by_an_edge_range() {
        let types = host_types();
        let mut func = range_guard(false);
        assert!(run(&mut func, &types), "should change something");
        assert!(
            folds_the_guard(&func),
            "guard should be unconditional:\n{func:?}"
        );
    }

    /// The same shape reached two ways: the fact belongs to one edge, and
    /// there is no critical-edge splitting to make it hold for the block.
    #[test]
    fn vrp_does_not_refine_a_block_with_two_predecessors() {
        let types = host_types();
        let mut func = range_guard(true);
        run(&mut func, &types);
        assert!(
            !folds_the_guard(&func),
            "a fact from one edge must not govern a block reached another way"
        );
    }

    /// `&&label` sets `addr_taken` and adds no CFG edge, so one recorded
    /// predecessor does not mean one way in.
    #[test]
    fn vrp_does_not_refine_an_address_taken_block() {
        let types = host_types();
        let mut func = range_guard(false);
        func.get_block_mut(BasicBlockId(1)).unwrap().addr_taken = true;
        run(&mut func, &types);
        assert!(
            !folds_the_guard(&func),
            "an addr_taken block has a hidden entry"
        );
    }

    /// Mirror of `sccp_treats_unmodelled_opcodes_as_overdefined`: an opcode
    /// with no transfer function must answer "anything", or it becomes a
    /// licence to prove every branch below it dead.
    #[test]
    fn vrp_treats_unmodelled_opcodes_as_overdefined() {
        let types = host_types();
        let mut func = range_guard(false);
        // Replace the subtraction with a load, which VRP does not model.
        let bb = func.get_block_mut(BasicBlockId(1)).unwrap();
        bb.insns[0] = Instruction::new(Opcode::Load)
            .with_target(PseudoId(10))
            .with_src(PseudoId(0))
            .with_type_and_size(types.int_id, 32);
        run(&mut func, &types);
        assert!(
            !folds_the_guard(&func),
            "a value from an unmodelled opcode proves nothing"
        );
    }

    /// A second run must find nothing, or the pass burns the whole
    /// ten-iteration budget in `opt::optimize_function`.
    #[test]
    fn vrp_is_idempotent() {
        let types = host_types();
        let mut func = range_guard(false);
        assert!(run(&mut func, &types));
        assert!(!run(&mut func, &types), "a second run must find nothing");
    }

    /// Folding a terminator must repair the CFG, or `dce` later deletes a
    /// block a live branch still names.
    #[test]
    fn vrp_repairs_the_cfg_when_it_folds_a_terminator() {
        let types = host_types();
        let mut func = range_guard(false);
        run(&mut func, &types);
        let l2 = func.get_block(BasicBlockId(2)).unwrap();
        assert_eq!(l2.children, vec![BasicBlockId(5)], "dropped edge removed");
        assert_eq!(l2.insns.last().unwrap().bb_false, None, "no stale target");
        let l3 = func.get_block(BasicBlockId(3)).unwrap();
        assert!(
            !l3.parents.contains(&BasicBlockId(2)),
            "successor's parents repaired"
        );
        assert!(crate::ir::validate::validate_function(&func).is_ok());
    }

    /// An unbounded loop must not spin: widening pins each cell after
    /// `MAX_MOVES` and the solve terminates whatever the ranges do.
    #[test]
    fn vrp_terminates_on_an_unbounded_loop() {
        let types = host_types();
        let i32t = types.int_id;
        let mut f = Function::new("loop", types.void_id);
        f.add_pseudo(Pseudo::val(PseudoId(1), 0));
        f.add_pseudo(Pseudo::val(PseudoId(2), 1));
        f.add_pseudo(Pseudo::arg(PseudoId(9), 0));
        f.next_pseudo = 30;

        let mut entry = BasicBlock::new(BasicBlockId(0));
        entry.add_insn(Instruction::new(Opcode::Entry));
        let mut ps = Instruction::new(Opcode::PhiSource)
            .with_target(PseudoId(3))
            .with_src(PseudoId(1))
            .with_type_and_size(i32t, 32);
        ps.phi_list.push((BasicBlockId(1), PseudoId(5)));
        entry.add_insn(ps);
        let mut b = Instruction::new(Opcode::Br);
        b.bb_true = Some(BasicBlockId(1));
        entry.add_insn(b);
        entry.children = vec![BasicBlockId(1)];

        let mut body = BasicBlock::new(BasicBlockId(1));
        let mut phi = Instruction::new(Opcode::Phi)
            .with_target(PseudoId(5))
            .with_type_and_size(i32t, 32);
        phi.phi_list.push((BasicBlockId(0), PseudoId(3)));
        phi.phi_list.push((BasicBlockId(1), PseudoId(7)));
        body.add_insn(phi);
        body.add_insn(Instruction::binop(
            Opcode::Add,
            PseudoId(6),
            PseudoId(5),
            PseudoId(2),
            i32t,
            32,
        ));
        let mut ps2 = Instruction::new(Opcode::PhiSource)
            .with_target(PseudoId(7))
            .with_src(PseudoId(6))
            .with_type_and_size(i32t, 32);
        ps2.phi_list.push((BasicBlockId(1), PseudoId(5)));
        body.add_insn(ps2);
        // Exit on something the pass cannot know.
        let mut cbr = Instruction::new(Opcode::Cbr).with_src(PseudoId(9));
        cbr.bb_true = Some(BasicBlockId(1));
        cbr.bb_false = Some(BasicBlockId(2));
        body.add_insn(cbr);
        body.children = vec![BasicBlockId(1), BasicBlockId(2)];
        body.parents = vec![BasicBlockId(0), BasicBlockId(1)];

        let mut exit = BasicBlock::new(BasicBlockId(2));
        exit.add_insn(Instruction::ret(None));
        exit.parents = vec![BasicBlockId(1)];

        f.add_block(entry);
        f.add_block(body);
        f.add_block(exit);
        f.entry = BasicBlockId(0);

        // The point is that this returns at all.
        run(&mut f, &types);
        let term = f.get_block(BasicBlockId(1)).unwrap().insns.last().unwrap();
        assert_eq!(term.op, Opcode::Cbr, "a data-dependent exit stays a branch");
    }

    /// A loop-carried counter compared against an unknown bound.
    ///
    /// ```text
    /// .L0: phisrc 0 -> .L1:%count      ; count = 0
    /// .L1: %count = phi(0, %next)
    ///      cbr %keep_going, .L2, .L4
    /// .L2: %next = %count + 1
    ///      %eq = seteq %next, %bound   ; bound is an argument
    ///      cbr %eq, .L3, .L5
    /// .L3: %ret = copy %bound          ; <- must NOT become `copy 1`
    ///      ret %ret
    /// .L5: phisrc %next -> .L1:%count
    ///      br .L1
    /// .L4: ret %count
    /// ```
    ///
    /// The first time the solver reaches the inner `cbr`, `%count` is still
    /// `{0}`, so `%next` is `{1}` and the true edge appears to prove
    /// `%bound == 1`. That fact is derived from the *ranges* of the
    /// comparison's operands, and those ranges grow as the loop is
    /// analyzed -- but the comparison's own lattice cell settles at "either
    /// answer is possible" and never moves again, so nothing re-derived the
    /// fact. The stale, too-strong fact rewrote the `%bound` read into a
    /// constant `1`, which is what made `"AAA".replace("A", "", 3)` answer
    /// `"AA"`.
    ///
    /// Whether the stale fact survives depends on the order the solver
    /// happens to reach things in, so this shape is the behavioural guard
    /// and [`vrp_a_branch_reads_what_its_fact_is_derived_from`] pins the
    /// mechanism directly.
    fn counting_loop() -> (Function, TypeTable) {
        let types = host_types();
        let i64t = types.long_id;
        let mut f = Function::new("f", i64t);
        f.add_pseudo(Pseudo::arg(PseudoId(0), 0)); // %bound
        f.add_pseudo(Pseudo::arg(PseudoId(1), 1)); // %keep_going
        f.add_pseudo(Pseudo::val(PseudoId(2), 0));
        f.add_pseudo(Pseudo::val(PseudoId(3), 1));
        f.next_pseudo = 40;

        let mut l0 = BasicBlock::new(BasicBlockId(0));
        l0.add_insn(Instruction::new(Opcode::Entry));
        let mut ps0 = Instruction::new(Opcode::PhiSource)
            .with_target(PseudoId(10))
            .with_src(PseudoId(2))
            .with_type_and_size(i64t, 64);
        ps0.phi_list.push((BasicBlockId(1), PseudoId(12)));
        l0.add_insn(ps0);
        let mut br = Instruction::new(Opcode::Br);
        br.bb_true = Some(BasicBlockId(1));
        l0.add_insn(br);
        l0.children = vec![BasicBlockId(1)];

        let mut l1 = BasicBlock::new(BasicBlockId(1));
        let mut phi = Instruction::new(Opcode::Phi)
            .with_target(PseudoId(12))
            .with_type_and_size(i64t, 64);
        phi.src = vec![PseudoId(10), PseudoId(11)];
        phi.phi_list = vec![
            (BasicBlockId(0), PseudoId(10)),
            (BasicBlockId(5), PseudoId(11)),
        ];
        l1.add_insn(phi);
        let mut cbr = Instruction::new(Opcode::Cbr).with_src(PseudoId(1));
        cbr.bb_true = Some(BasicBlockId(2));
        cbr.bb_false = Some(BasicBlockId(4));
        l1.add_insn(cbr);
        l1.children = vec![BasicBlockId(2), BasicBlockId(4)];

        let mut l2 = BasicBlock::new(BasicBlockId(2));
        l2.add_insn(Instruction::binop(
            Opcode::Add,
            PseudoId(13),
            PseudoId(12),
            PseudoId(3),
            i64t,
            64,
        ));
        // The `Copy` indirections matter: they are what SSA promotion
        // leaves around every value that came out of a local, and they are
        // why the comparison's own lattice cell stops moving long before its
        // operands' ranges do.
        l2.add_insn(
            Instruction::new(Opcode::Copy)
                .with_target(PseudoId(15))
                .with_src(PseudoId(13))
                .with_type_and_size(i64t, 64),
        );
        l2.add_insn(
            Instruction::new(Opcode::Copy)
                .with_target(PseudoId(16))
                .with_src(PseudoId(0))
                .with_type_and_size(i64t, 64),
        );
        l2.add_insn(Instruction::binop(
            Opcode::SetEq,
            PseudoId(14),
            PseudoId(15),
            PseudoId(16),
            i64t,
            64,
        ));
        l2.add_insn(
            Instruction::new(Opcode::Copy)
                .with_target(PseudoId(17))
                .with_src(PseudoId(14))
                .with_type_and_size(types.int_id, 32),
        );
        let mut cbr2 = Instruction::new(Opcode::Cbr).with_src(PseudoId(17));
        cbr2.bb_true = Some(BasicBlockId(3));
        cbr2.bb_false = Some(BasicBlockId(5));
        l2.add_insn(cbr2);
        l2.children = vec![BasicBlockId(3), BasicBlockId(5)];

        let mut l3 = BasicBlock::new(BasicBlockId(3));
        l3.add_insn(
            Instruction::new(Opcode::Copy)
                .with_target(PseudoId(20))
                .with_src(PseudoId(0))
                .with_type_and_size(i64t, 64),
        );
        l3.add_insn(
            Instruction::new(Opcode::Ret)
                .with_src(PseudoId(20))
                .with_type_and_size(i64t, 64),
        );

        let mut l4 = BasicBlock::new(BasicBlockId(4));
        l4.add_insn(
            Instruction::new(Opcode::Ret)
                .with_src(PseudoId(12))
                .with_type_and_size(i64t, 64),
        );

        let mut l5 = BasicBlock::new(BasicBlockId(5));
        let mut ps1 = Instruction::new(Opcode::PhiSource)
            .with_target(PseudoId(11))
            .with_src(PseudoId(13))
            .with_type_and_size(i64t, 64);
        ps1.phi_list.push((BasicBlockId(1), PseudoId(12)));
        l5.add_insn(ps1);
        let mut back = Instruction::new(Opcode::Br);
        back.bb_true = Some(BasicBlockId(1));
        l5.add_insn(back);
        l5.children = vec![BasicBlockId(1)];

        f.entry = BasicBlockId(0);
        f.blocks = vec![l0, l1, l2, l3, l4, l5];
        f.rebuild_block_idx();
        (f, types)
    }

    #[test]
    fn vrp_re_derives_an_edge_fact_when_its_operands_widen() {
        let (mut f, types) = counting_loop();
        run(&mut f, &types);

        // `.L3` returns the bound. Nothing in this function says what the
        // bound is, so the copy must still read the argument.
        let l3 = f.blocks.iter().find(|b| b.id == BasicBlockId(3)).unwrap();
        let copy = l3
            .insns
            .iter()
            .find(|i| i.target == Some(PseudoId(20)))
            .expect("the copy survives");
        let v = copy.src[0];
        assert!(
            f.const_val(v).is_none(),
            "the bound was replaced by the constant {:?}",
            f.const_val(v)
        );
    }

    /// A branch is a reader of the pseudos its edge facts are derived from.
    ///
    /// This is the fix stated as an invariant rather than as an outcome. A
    /// comparison's own lattice cell reaches "either answer is possible"
    /// early and stops moving, while the ranges underneath it keep widening
    /// for the rest of the solve; if the branch is not on those pseudos'
    /// reader lists, the first and narrowest fact it derived is the one that
    /// stands.
    #[test]
    fn vrp_a_branch_reads_what_its_fact_is_derived_from() {
        let (f, types) = counting_loop();
        let mut solver = Solver::new(&f, &types);
        solver.solve(&f);

        // The inner `cbr` is the last instruction of `.L2`.
        let b = f.block_index(BasicBlockId(2)).unwrap();
        let site = (b, f.blocks[b].insns.len() - 1);
        assert_eq!(f.blocks[b].insns[site.1].op, Opcode::Cbr);

        // `%0` is the bound: the operand whose range the fact is about.
        let readers = solver.uses.get(&PseudoId(0)).cloned().unwrap_or_default();
        assert!(
            readers.contains(&site),
            "the branch must be re-evaluated when the bound's range moves"
        );
    }

    /// Undefined behaviour is not assumed away: a divisor that could be
    /// zero must not let the pass conclude anything.
    ///
    /// The divisor is a *second* argument, deliberately. Dividing `var` by
    /// itself would be foldable and correctly so -- the edge already proves
    /// `var >= 1` there, so the divisor is known non-zero.
    #[test]
    fn vrp_does_not_assume_division_by_zero_away() {
        let types = host_types();
        let mut func = range_guard(false);
        func.add_pseudo(Pseudo::arg(PseudoId(30), 1));
        let bb = func.get_block_mut(BasicBlockId(1)).unwrap();
        bb.insns[0] = Instruction::binop(
            Opcode::DivU,
            PseudoId(10),
            PseudoId(0),
            PseudoId(30),
            types.int_id,
            32,
        );
        run(&mut func, &types);
        assert!(
            !folds_the_guard(&func),
            "a divisor that may be zero proves nothing about the quotient"
        );
    }

    /// And the converse, which is the more interesting half: where the edge
    /// *does* prove the divisor non-zero, folding is correct rather than
    /// lucky.
    #[test]
    fn vrp_folds_a_division_the_edge_proves_safe() {
        let types = host_types();
        let mut func = range_guard(false);
        // `var / var` on an edge where `var >= 1`.
        let bb = func.get_block_mut(BasicBlockId(1)).unwrap();
        bb.insns[0] = Instruction::binop(
            Opcode::DivU,
            PseudoId(10),
            PseudoId(0),
            PseudoId(0),
            types.int_id,
            32,
        );
        assert!(run(&mut func, &types));
        assert!(folds_the_guard(&func));
    }
}
