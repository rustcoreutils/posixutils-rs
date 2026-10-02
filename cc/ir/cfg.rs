//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The control-flow graph: what a block's successors are, and every edit that
// changes one.
//
// A block's instructions are what decide where control goes -- its
// terminator's targets, and the labels an `asm goto` in it may jump to.
// `BasicBlock::children` and `parents` are a cache of that, kept because
// nearly every pass wants predecessors and computing them is a walk of the
// whole function. A cache is only as good as its upkeep, and edges were
// dropped in one place and not the other often enough that three passes
// rebuilt predecessors themselves rather than trust `parents`. So every
// edit to an edge goes through this file, and `validate` checks the cache
// against the instructions at every stage.
//
// The one exception to "the instructions decide" is `IndirectBr`: a computed
// `goto` names no target, and its edges are every address-taken label, which
// the linearizer records when it builds the dispatch block.
//
// ## Critical edges, and the contract with CFG simplification
//
// An edge is critical when its source has more than one successor and its
// destination more than one predecessor. Phi elimination places the copies
// for an incoming edge at the end of its source, so on a critical edge they
// also run when control takes the source's *other* edges -- the lost-copy
// problem -- and an `asm goto` jumps away before its block's end, skipping
// them altogether. `split_critical_edges` gives each such edge a block of its
// own for the copies to live in.
//
// That is the opposite of what CFG simplification does: it deletes exactly
// such empty forwarding blocks. The two are kept apart by *when* they run.
// Simplification belongs to the optimizer's fixed-point loop; splitting
// happens once, at the top of `lower::lower_module`, immediately before the
// phis are eliminated, and nothing merges blocks after it. A pass that wanted
// to simplify after lowering would reintroduce every lost copy.
//

use super::{BasicBlock, BasicBlockId, Function, InsnExtra, Instruction, Opcode};
use std::collections::{HashMap, HashSet};

/// Every block an [`Instruction`] can transfer control to, as an iterator of
/// references to its slots, repeats included: the true and false targets,
/// each switch case and the default, then the `asm goto` labels. Expanded
/// once over a shared instruction and once (with `mut`) over a unique one,
/// so the walk that reads the targets and the walks that rewrite them are
/// one list.
macro_rules! target_slots {
    ($insn:expr $(, $m:tt)?) => {{
        let Instruction {
            bb_true,
            bb_false,
            extra,
            ..
        } = $insn;
        let (cases, default, asm) = match extra {
            Some(e) => {
                let InsnExtra {
                    switch_cases,
                    switch_default,
                    asm_data,
                    ..
                } = &$($m)? **e;
                (Some(switch_cases), Some(switch_default), Some(asm_data))
            }
            None => (None, None, None),
        };
        bb_true
            .into_iter()
            .chain(bb_false)
            .chain(cases.into_iter().flatten().map(|(_, _, b)| b))
            .chain(default.into_iter().flatten())
            .chain(
                asm.into_iter()
                    .flatten()
                    .flat_map(|d| &$($m)? d.goto_labels)
                    .map(|(b, _)| b),
            )
    }};
}

impl Instruction {
    /// Every block this instruction can transfer control to: a branch's and a
    /// switch's targets, and the labels of an `asm goto`. Empty for anything
    /// else, `IndirectBr` included -- it names no target.
    pub fn control_targets(&self) -> Vec<BasicBlockId> {
        let mut out = Vec::new();
        // A switch can name thousands of cases, most landing together.
        let mut seen = HashSet::new();
        let mut add = |b: BasicBlockId| {
            if seen.insert(b) {
                out.push(b);
            }
        };
        target_slots!(self).copied().for_each(&mut add);
        out
    }

    /// Make every reference this instruction holds to a key of `map` refer
    /// to its value instead.
    ///
    /// One pass over the instruction whatever the map holds: a switch whose
    /// thousands of edges are all being split is rewritten once, not once per
    /// edge.
    ///
    /// Only the control targets: a `Phi`'s predecessors name the edges *into*
    /// its block, which no change to this block's successors moves.
    fn retarget(&mut self, map: &HashMap<BasicBlockId, BasicBlockId>) {
        for b in target_slots!(self, mut) {
            if let Some(to) = map.get(b) {
                *b = *to;
            }
        }
    }

    /// Rewrite every block this instruction names: each control target
    /// [`Self::control_targets`] reads, then the predecessor of each phi
    /// operand -- for a `PhiSource`, the block of the `Phi` it feeds.
    ///
    /// For copying an instruction into another block numbering, where every
    /// block id changes; an edit to one edge wants [`Self::retarget`].
    pub fn for_each_block_mut(&mut self, mut f: impl FnMut(&mut BasicBlockId)) {
        target_slots!(self, mut).for_each(&mut f);
        self.phi_list.iter_mut().for_each(|(b, _)| f(b));
    }
}

impl BasicBlock {
    /// The successors this block's instructions name, in first-mention order.
    ///
    /// `None` for a block ending in `IndirectBr`, whose successors no
    /// instruction names.
    pub fn named_successors(&self) -> Option<Vec<BasicBlockId>> {
        if self.ends_in_computed_goto() {
            return None;
        }
        let mut out: Vec<BasicBlockId> = Vec::new();
        let mut seen = HashSet::new();
        for insn in &self.insns {
            for t in insn.control_targets() {
                if seen.insert(t) {
                    out.push(t);
                }
            }
        }
        Some(out)
    }

    /// Does this block end in a computed `goto`, whose successors no
    /// instruction names and whose edges cannot be split?
    pub fn ends_in_computed_goto(&self) -> bool {
        self.insns
            .last()
            .is_some_and(|i| i.op == Opcode::IndirectBr)
    }

    /// Does an `asm goto` in this block name targets besides the
    /// terminator's? Such a block ends in an ordinary `Br` to the
    /// fallthrough, so an analysis that reads edges off the terminator alone
    /// would miss the rest of them and find the labels unreachable.
    pub fn has_asm_goto(&self) -> bool {
        self.insns.iter().any(|i| {
            i.extra()
                .asm_data
                .as_ref()
                .is_some_and(|d| !d.goto_labels.is_empty())
        })
    }

    fn has_phi(&self) -> bool {
        self.insns.iter().any(|i| i.op == Opcode::Phi)
    }

    /// Make every record of `old` as a predecessor of this block -- in
    /// `parents`, and as the edge each phi takes an operand along -- name
    /// `new` instead.
    pub(super) fn rename_predecessor(&mut self, old: BasicBlockId, new: BasicBlockId) {
        for p in &mut self.parents {
            if *p == old {
                *p = new;
            }
        }
        for insn in &mut self.insns {
            if insn.op == Opcode::Phi {
                for (p, _) in &mut insn.phi_list {
                    if *p == old {
                        *p = new;
                    }
                }
            }
        }
    }

    /// Where this block sends control when it does nothing else: the target
    /// of a block that is a lone `br`.
    fn forwards_to(&self) -> Option<BasicBlockId> {
        let mut real = self.insns.iter().filter(|i| i.op != Opcode::Nop);
        let br = real.next().filter(|i| i.op == Opcode::Br)?;
        if real.next().is_some() {
            return None;
        }
        br.bb_true.filter(|t| *t != self.id)
    }
}

impl Function {
    /// A block id no block of this function uses.
    pub fn fresh_block_id(&self) -> BasicBlockId {
        BasicBlockId(self.blocks.iter().map(|b| b.id.0 + 1).max().unwrap_or(0))
    }

    /// Record the edge `from` -> `to`. The instructions of `from` must already
    /// name it; this updates the cache.
    pub fn add_edge(&mut self, from: BasicBlockId, to: BasicBlockId) {
        if let Some(b) = self.get_block_mut(from) {
            if !b.children.contains(&to) {
                b.children.push(to);
            }
        }
        if let Some(b) = self.get_block_mut(to) {
            if !b.parents.contains(&from) {
                b.parents.push(from);
            }
        }
    }

    /// Forget the edge `from` -> `to`, which no instruction of `from` names any
    /// more (or `from` is about to be deleted).
    ///
    /// Everything that described the edge goes with it: the entry in each
    /// list, the operand each phi of `to` took along it, and the `PhiSource`
    /// in `from` that supplied that operand -- left behind, phi elimination
    /// would still turn it into a copy into the phi's target.
    pub fn remove_edge(&mut self, from: BasicBlockId, to: BasicBlockId) {
        if let Some(b) = self.get_block_mut(from) {
            b.children.retain(|c| *c != to);
            for insn in &mut b.insns {
                if insn
                    .phi_source_dest()
                    .is_some_and(|(phi_bb, _)| phi_bb == to)
                {
                    insn.kill();
                }
            }
        }
        if let Some(b) = self.get_block_mut(to) {
            b.parents.retain(|p| *p != from);
            for insn in &mut b.insns {
                if insn.op == Opcode::Phi {
                    insn.phi_list.retain(|(p, _)| *p != from);
                }
            }
        }
    }

    /// Give every critical edge into a block with phis a block of its own.
    ///
    /// See the module comment for why, and for why this runs only at the top
    /// of lowering. Only an edge a phi reads along needs splitting: the
    /// copies are the only thing that must run on one edge and not another.
    /// An edge out of an `IndirectBr` cannot be split -- its source jumps to
    /// an address, not to a block it names -- and is left for phi
    /// elimination to handle.
    ///
    /// Returns how many edges were split.
    pub fn split_critical_edges(&mut self) -> usize {
        let mut edges: Vec<(BasicBlockId, BasicBlockId)> = Vec::new();
        for b in &self.blocks {
            if b.children.len() < 2 || b.ends_in_computed_goto() {
                continue;
            }
            for &s in &b.children {
                let into = self.get_block(s).expect("a child exists");
                if into.parents.len() >= 2 && into.has_phi() {
                    edges.push((b.id, s));
                }
            }
        }
        if edges.is_empty() {
            return 0;
        }

        // Every new block is made first and laid out once at the end, right
        // after its source; and each source's instructions are rewritten once
        // for all of its split edges. One edge at a time -- a fresh id search,
        // a rebuilt block index and a rescan of the source's terminator per
        // edge -- was quadratic, and a 70,000-case switch took three minutes.
        let first = self.fresh_block_id().0;
        let mut by_source: Vec<(BasicBlockId, HashMap<BasicBlockId, BasicBlockId>)> = Vec::new();
        for (n, &(from, to)) in (first..).zip(&edges) {
            let mid = BasicBlockId(n);
            match by_source.last_mut() {
                Some((f, map)) if *f == from => {
                    map.insert(to, mid);
                }
                _ => by_source.push((from, HashMap::from([(to, mid)]))),
            }
        }
        let mut after: HashMap<BasicBlockId, Vec<BasicBlock>> = HashMap::new();
        for (from, map) in by_source {
            let new = self.split_edges_from(from, &map);
            after.insert(from, new);
        }
        let old = std::mem::take(&mut self.blocks);
        for b in old {
            let id = b.id;
            self.blocks.push(b);
            if let Some(new) = after.remove(&id) {
                self.blocks.extend(new);
            }
        }
        self.rebuild_block_idx();
        edges.len()
    }

    /// Put a new block on each edge `from` -> `to` for `to -> mid` in `map`,
    /// and move onto it what belonged to the edge: the phi operands taken
    /// along it and the `PhiSource` instructions that supply them. Returns
    /// the new blocks, in `map`'s edge order, for the caller to lay out.
    fn split_edges_from(
        &mut self,
        from: BasicBlockId,
        map: &HashMap<BasicBlockId, BasicBlockId>,
    ) -> Vec<BasicBlock> {
        let src = self.get_block_mut(from).expect("edge source exists");
        let mut moved: HashMap<BasicBlockId, Vec<Instruction>> = HashMap::new();
        let mut kept = Vec::with_capacity(src.insns.len());
        for mut insn in std::mem::take(&mut src.insns) {
            let feeds = insn
                .phi_source_dest()
                .map(|(to, _)| to)
                .filter(|to| map.contains_key(to));
            match feeds {
                Some(to) => moved.entry(to).or_default().push(insn),
                None => {
                    insn.retarget(map);
                    kept.push(insn);
                }
            }
        }
        src.insns = kept;
        let order: Vec<BasicBlockId> = src.children.clone();
        for c in &mut src.children {
            if let Some(mid) = map.get(c) {
                *c = *mid;
            }
        }

        let mut blocks = Vec::new();
        for to in order.into_iter().filter(|t| map.contains_key(t)) {
            let mid = map[&to];
            let mut block = BasicBlock::new(mid);
            block.insns = moved.remove(&to).unwrap_or_default();
            block.insns.push(Instruction::br(to));
            block.parents = vec![from];
            block.children = vec![to];
            blocks.push(block);

            self.get_block_mut(to)
                .expect("edge target exists")
                .rename_predecessor(from, mid);
        }
        blocks
    }

    /// Remove every block no path from the entry reaches, and every edge and
    /// phi source it contributed.
    ///
    /// Also the linearizer's last step on a function, at every level: gcc
    /// emits no code no path reaches even at `-O0` -- the arm of a constant
    /// condition, what follows a `return` or a `goto` -- and a program may
    /// depend on it, by calling a function that exists nowhere from such an
    /// arm. A block reached through a label, `case`, `default` or a taken
    /// address is kept.
    pub fn remove_unreachable_blocks(&mut self) -> bool {
        let reachable = self.reachable_blocks();
        let before = self.blocks.len();
        self.blocks.retain(|bb| reachable.contains(&bb.id));
        if self.blocks.len() == before {
            return false;
        }
        // Every edge out of a dead block goes with it, taking the phi operand
        // it carried into a live successor -- in one pass over the survivors,
        // as `remove_edge` per edge is quadratic when many dead blocks lead
        // into one.
        for bb in &mut self.blocks {
            bb.parents.retain(|p| reachable.contains(p));
            for insn in &mut bb.insns {
                if insn.op == Opcode::Phi {
                    insn.phi_list.retain(|(p, _)| reachable.contains(p));
                }
            }
        }
        self.rebuild_block_idx();
        true
    }

    /// Simplify the graph without changing what runs: drop unreachable
    /// blocks, send each edge into an empty forwarding block straight to
    /// where it forwards, and merge a block into its predecessor when each
    /// is the other's only neighbour. Returns whether anything changed.
    ///
    /// For the optimizer's loop only -- see the module comment: nothing may
    /// merge blocks once `split_critical_edges` has run.
    pub fn simplify_cfg(&mut self) -> bool {
        let mut changed = self.remove_unreachable_blocks();
        if self.thread_forwarders() {
            self.remove_unreachable_blocks();
            changed = true;
        }
        changed | self.merge_chains()
    }

    /// The blocks an edge may skip, each with where it finally forwards to.
    ///
    /// A forwarder must not be the entry or have its address taken, which
    /// are ways in that no edge records; and its destination must have no
    /// phis, which would need an operand for each new predecessor that
    /// nothing supplies. A cycle of forwarders is an infinite loop the
    /// program asked for, and is left alone.
    fn forwarders(&self) -> HashMap<BasicBlockId, BasicBlockId> {
        let step: HashMap<BasicBlockId, BasicBlockId> = self
            .blocks
            .iter()
            .filter(|b| b.id != self.entry && !b.addr_taken)
            .filter_map(|b| Some((b.id, b.forwards_to()?)))
            .collect();
        // Each chain is walked once, and every block on it is given the
        // answer: consecutive `case` labels make a chain as long as the
        // switch, and walking it from every link is quadratic.
        let mut resolved: HashMap<BasicBlockId, Option<BasicBlockId>> = HashMap::new();
        for &start in step.keys() {
            let mut path = Vec::new();
            let mut on_path = HashSet::new();
            let mut cur = start;
            let lands = loop {
                if let Some(&known) = resolved.get(&cur) {
                    break known;
                }
                match step.get(&cur) {
                    None => break self.get_block(cur).filter(|b| !b.has_phi()).map(|b| b.id),
                    Some(_) if !on_path.insert(cur) => break None,
                    Some(&next) => {
                        path.push(cur);
                        cur = next;
                    }
                }
            };
            for p in path {
                resolved.insert(p, lands);
            }
        }
        resolved
            .into_iter()
            .filter_map(|(from, to)| Some((from, to?)))
            .collect()
    }

    /// Point every edge into a forwarder at its destination instead.
    fn thread_forwarders(&mut self) -> bool {
        let map = self.forwarders();
        if map.is_empty() {
            return false;
        }
        let mut changed = false;
        for b in 0..self.blocks.len() {
            let block = &self.blocks[b];
            // A computed `goto` names no target to retarget, and the blocks it
            // reaches have their address taken, so none is a forwarder.
            if block.ends_in_computed_goto() || map.contains_key(&block.id) {
                continue;
            }
            let from = block.id;
            let edges: Vec<(BasicBlockId, BasicBlockId)> = block
                .children
                .iter()
                .filter_map(|c| Some((*c, *map.get(c)?)))
                .collect();
            if edges.is_empty() {
                continue;
            }
            // One rewrite of this block's edges for all of them: a switch can
            // have tens of thousands, and an edge at a time is quadratic.
            let block = &mut self.blocks[b];
            for insn in &mut block.insns {
                insn.retarget(&map);
            }
            let mut seen = HashSet::new();
            let children: Vec<BasicBlockId> = std::mem::take(&mut block.children)
                .into_iter()
                .map(|c| map.get(&c).copied().unwrap_or(c))
                .filter(|c| seen.insert(*c))
                .collect();
            block.children = children;
            // A forwarder has no phis, so nothing on these edges but the
            // lists themselves.
            for (via, to) in edges {
                if let Some(v) = self.get_block_mut(via) {
                    v.parents.retain(|p| *p != from);
                }
                let t = self.get_block_mut(to).expect("a forwarder's target exists");
                if !t.parents.contains(&from) {
                    t.parents.push(from);
                }
            }
            // Both arms of a branch may now land on one block.
            let term = self.blocks[b]
                .insns
                .last()
                .expect("a block ends in a terminator");
            if term.op == Opcode::Cbr && term.bb_true == term.bb_false {
                let to = term.bb_true.expect("a cbr names its targets");
                super::propagate::retarget_terminator(self, b, to);
            }
            changed = true;
        }
        changed
    }

    /// Merge each block into its predecessor while it has just the one and
    /// is that predecessor's only successor.
    fn merge_chains(&mut self) -> bool {
        let mut absorbed: HashSet<BasicBlockId> = HashSet::new();
        for a in 0..self.blocks.len() {
            if absorbed.contains(&self.blocks[a].id) {
                continue;
            }
            while let Some(b) = self.sole_successor(a) {
                self.absorb(a, b);
                absorbed.insert(b);
            }
        }
        if absorbed.is_empty() {
            return false;
        }
        self.blocks.retain(|bb| !absorbed.contains(&bb.id));
        self.rebuild_block_idx();
        true
    }

    /// The block `a` falls into and nothing else reaches, if there is one.
    fn sole_successor(&self, a: usize) -> Option<BasicBlockId> {
        let block = &self.blocks[a];
        let [b] = block.children[..] else {
            return None;
        };
        let ends_in_br = block.insns.last().is_some_and(|t| t.op == Opcode::Br);
        if !ends_in_br || block.has_asm_goto() || b == block.id || b == self.entry {
            return None;
        }
        let next = self.get_block(b)?;
        (!next.addr_taken && next.parents == [block.id]).then_some(b)
    }

    /// Move block `b`'s instructions onto the end of block `a`, its only
    /// predecessor, in place of `a`'s branch to it. `b` is left empty and
    /// unlinked for the caller to drop.
    ///
    /// A phi with one predecessor is a copy of what that predecessor
    /// supplies, and the `PhiSource` supplying it is a copy too.
    fn absorb(&mut self, a: usize, b: BasicBlockId) {
        let a_id = self.blocks[a].id;
        let bi = self.block_index(b).expect("a successor exists");
        let mut insns = std::mem::take(&mut self.blocks[bi].insns);
        let children = std::mem::take(&mut self.blocks[bi].children);
        self.blocks[bi].parents.clear();
        for insn in &mut insns {
            if insn.op == Opcode::Phi {
                let [(_, value)] = insn.phi_list[..] else {
                    panic!("a phi has an operand per predecessor");
                };
                insn.op = Opcode::Copy;
                insn.src = vec![value];
                insn.phi_list.clear();
            }
        }

        let block = &mut self.blocks[a];
        block.insns.pop();
        for insn in &mut block.insns {
            if insn
                .phi_source_dest()
                .is_some_and(|(phi_bb, _)| phi_bb == b)
            {
                insn.op = Opcode::Copy;
                insn.phi_list.clear();
            }
        }
        block.insns.extend(insns);
        block.children = children.clone();
        for c in children {
            self.get_block_mut(c)
                .expect("a successor exists")
                .rename_predecessor(b, a_id);
        }
    }

    /// Every block's successors, by id: `children`, which the validator holds
    /// to what the instructions name, `asm goto` labels included.
    pub fn successor_map(&self) -> HashMap<BasicBlockId, Vec<BasicBlockId>> {
        self.blocks
            .iter()
            .map(|b| (b.id, b.children.clone()))
            .collect()
    }

    /// Every block's predecessors, by id: `parents`, which the validator
    /// holds to the exact inverse of `children`.
    pub fn predecessor_map(&self) -> HashMap<BasicBlockId, Vec<BasicBlockId>> {
        self.blocks
            .iter()
            .map(|b| (b.id, b.parents.clone()))
            .collect()
    }

    /// Set every block's `parents` to the inverse of `children`.
    ///
    /// For IR a test builds by hand, which states each block's successors
    /// and nothing else. The passes never need it: every edit they make goes
    /// through `add_edge`/`remove_edge`.
    #[cfg(test)]
    pub(crate) fn rebuild_parents(&mut self) {
        let edges: Vec<(BasicBlockId, BasicBlockId)> = self
            .blocks
            .iter()
            .flat_map(|b| b.children.iter().map(move |c| (b.id, *c)))
            .collect();
        for b in &mut self.blocks {
            b.parents.clear();
        }
        self.rebuild_block_idx();
        for (from, to) in edges {
            if let Some(b) = self.get_block_mut(to) {
                if !b.parents.contains(&from) {
                    b.parents.push(from);
                }
            }
        }
    }

    /// Every block reachable from the entry, and from any block whose address
    /// is taken, along recorded edges.
    pub fn reachable_blocks(&self) -> HashSet<BasicBlockId> {
        let mut seen = HashSet::new();
        let mut work: Vec<BasicBlockId> = vec![self.entry];
        work.extend(self.blocks.iter().filter(|b| b.addr_taken).map(|b| b.id));
        while let Some(b) = work.pop() {
            if seen.insert(b) {
                if let Some(bb) = self.get_block(b) {
                    work.extend(bb.children.iter().copied());
                }
            }
        }
        seen
    }
}

#[cfg(test)]
mod tests {
    use crate::ir::validate::validate_function;
    use crate::ir::{BasicBlock, BasicBlockId, Function, Instruction, Opcode, Pseudo, PseudoId};
    use crate::target::Target;
    use crate::types::TypeTable;

    /// A function of `blocks`, each `(id, instructions, successors)`, with
    /// `%1` a condition and `%2` a value to hand along edges.
    fn function(blocks: Vec<(u32, Vec<Instruction>, Vec<u32>)>) -> Function {
        let types = TypeTable::new(&Target::host());
        let mut f = Function::new("f", types.int_id);
        f.add_pseudo(Pseudo::arg(PseudoId(1), 0));
        f.add_pseudo(Pseudo::arg(PseudoId(2), 1));
        f.next_pseudo = 20;
        for (id, insns, children) in blocks {
            let mut bb = BasicBlock::new(BasicBlockId(id));
            for i in insns {
                bb.add_insn(i);
            }
            bb.children = children.into_iter().map(BasicBlockId).collect();
            f.blocks.push(bb);
        }
        f.entry = BasicBlockId(0);
        f.rebuild_parents();
        f
    }

    fn entry() -> Instruction {
        Instruction::new(Opcode::Entry)
    }

    fn br(to: u32) -> Instruction {
        Instruction::br(BasicBlockId(to))
    }

    fn cbr(t: u32, e: u32) -> Instruction {
        Instruction::cbr(PseudoId(1), BasicBlockId(t), BasicBlockId(e))
    }

    fn ret() -> Instruction {
        Instruction::ret(None)
    }

    fn ids(f: &Function) -> Vec<u32> {
        f.blocks.iter().map(|b| b.id.0).collect()
    }

    fn valid(f: &Function) {
        if let Err(e) = validate_function(f) {
            panic!("invalid after simplify_cfg: {e:?}\n{f:?}");
        }
    }

    /// A block with one predecessor that has one successor is the rest of
    /// that predecessor, and its phi a copy of what the predecessor supplies.
    #[test]
    fn simplify_cfg_merges_a_chain_and_turns_its_phis_into_copies() {
        let int = TypeTable::new(&Target::host()).int_id;
        let mut src = Instruction::phi_source(PseudoId(10), PseudoId(2), int, 32);
        src.phi_list = vec![(BasicBlockId(1), PseudoId(11))];
        let mut phi = Instruction::phi(PseudoId(11), int, 32);
        phi.phi_list = vec![(BasicBlockId(0), PseudoId(10))];
        let mut f = function(vec![
            (0, vec![entry(), src, br(1)], vec![1]),
            (1, vec![phi, Instruction::ret(Some(PseudoId(11)))], vec![]),
        ]);
        assert!(f.simplify_cfg());
        assert_eq!(ids(&f), vec![0]);
        let ops: Vec<Opcode> = f.blocks[0].insns.iter().map(|i| i.op).collect();
        assert_eq!(
            ops,
            vec![Opcode::Entry, Opcode::Copy, Opcode::Copy, Opcode::Ret]
        );
        assert_eq!(f.blocks[0].insns[2].src, vec![PseudoId(10)]);
        assert!(f.blocks[0].insns.iter().all(|i| i.phi_list.is_empty()));
        valid(&f);
        assert!(!f.simplify_cfg(), "a second run finds nothing");
    }

    /// An edge into a block that only branches on goes where it branches,
    /// and the forwarder, unreached, goes.
    #[test]
    fn simplify_cfg_threads_an_edge_through_an_empty_block() {
        let mut f = function(vec![
            (0, vec![entry(), cbr(1, 2)], vec![1, 2]),
            (1, vec![br(3)], vec![3]),
            (2, vec![Instruction::new(Opcode::Nop), br(3)], vec![3]),
            (3, vec![ret()], vec![]),
        ]);
        assert!(f.simplify_cfg());
        // Both arms forwarded to `.L3`, so the branch decides nothing, and
        // what is left is one block.
        assert_eq!(ids(&f), vec![0]);
        assert_eq!(f.blocks[0].insns.last().unwrap().op, Opcode::Ret);
        valid(&f);
    }

    /// A chain of forwarders -- what consecutive `case` labels make -- is
    /// followed to its end, every link resolved by one walk.
    #[test]
    fn simplify_cfg_follows_a_chain_of_forwarders_to_its_end() {
        let n = 2000;
        let mut blocks = vec![(0, vec![entry(), cbr(1, n + 1)], vec![1, n + 1])];
        for i in 1..=n {
            blocks.push((i, vec![br(i + 1)], vec![i + 1]));
        }
        blocks.push((n + 1, vec![ret()], vec![]));
        let mut f = function(blocks);
        assert!(f.simplify_cfg());
        assert_eq!(
            ids(&f),
            vec![0],
            "every forwarder goes, and the branch with them"
        );
        valid(&f);
    }

    /// A destination with phis needs an operand per predecessor, and a
    /// forwarder supplies none of its own, so the edge is left as it is.
    #[test]
    fn simplify_cfg_does_not_thread_into_a_phi() {
        let int = TypeTable::new(&Target::host()).int_id;
        let mut s1 = Instruction::phi_source(PseudoId(10), PseudoId(2), int, 32);
        s1.phi_list = vec![(BasicBlockId(3), PseudoId(12))];
        let mut s2 = Instruction::phi_source(PseudoId(11), PseudoId(1), int, 32);
        s2.phi_list = vec![(BasicBlockId(3), PseudoId(12))];
        let mut phi = Instruction::phi(PseudoId(12), int, 32);
        phi.phi_list = vec![
            (BasicBlockId(1), PseudoId(10)),
            (BasicBlockId(2), PseudoId(11)),
        ];
        let mut f = function(vec![
            (0, vec![entry(), cbr(1, 2)], vec![1, 2]),
            (1, vec![s1, br(3)], vec![3]),
            (2, vec![s2, br(3)], vec![3]),
            (3, vec![phi, Instruction::ret(Some(PseudoId(12)))], vec![]),
        ]);
        assert!(!f.simplify_cfg());
        assert_eq!(ids(&f), vec![0, 1, 2, 3]);
        valid(&f);
    }

    /// The ways in that no edge records -- the entry, a taken address --
    /// keep a block whole, and a cycle of forwarders is a loop the program
    /// asked for.
    #[test]
    fn simplify_cfg_keeps_what_an_edge_does_not_account_for() {
        let mut f = function(vec![
            (0, vec![entry(), cbr(1, 2)], vec![1, 2]),
            (1, vec![br(3)], vec![3]),
            (2, vec![br(4)], vec![4]),
            (3, vec![br(2)], vec![2]),
            (4, vec![br(3)], vec![3]),
        ]);
        f.blocks[1].addr_taken = true;
        let before = ids(&f);
        f.simplify_cfg();
        assert!(ids(&f).contains(&1), "an address-taken block stays");
        assert!(
            f.blocks.iter().any(|b| b.forwards_to().is_some()),
            "the forwarding cycle is still there: {before:?} -> {:?}",
            ids(&f)
        );
        valid(&f);
    }

    /// Retargeting moves control targets only. A phi operand's predecessor
    /// names an edge *into* the block, so a loop's back edge -- the block
    /// both branching to `1` and receiving from it -- keeps its phi operand
    /// when the branch is split.
    #[test]
    fn retarget_moves_control_targets_and_not_phi_predecessors() {
        let types = TypeTable::new(&Target::host());
        let mut phi = Instruction::phi(PseudoId(9), types.int_id, 32);
        phi.phi_list = vec![(BasicBlockId(1), PseudoId(2))];
        let mut sw = Instruction::switch_insn(
            PseudoId(1),
            vec![(0, 0, BasicBlockId(1)), (1, 1, BasicBlockId(2))],
            Some(BasicBlockId(1)),
            32,
        );
        let map = std::collections::HashMap::from([(BasicBlockId(1), BasicBlockId(5))]);
        phi.retarget(&map);
        sw.retarget(&map);
        assert_eq!(phi.phi_list, [(BasicBlockId(1), PseudoId(2))]);
        assert_eq!(sw.control_targets(), [5, 2].map(BasicBlockId));
    }
}
