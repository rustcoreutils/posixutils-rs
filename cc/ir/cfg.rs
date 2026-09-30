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

use super::{BasicBlock, BasicBlockId, Function, Instruction, Opcode};
use std::collections::{HashMap, HashSet};

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
        self.bb_true.into_iter().for_each(&mut add);
        self.bb_false.into_iter().for_each(&mut add);
        self.switch_cases.iter().for_each(|(_, _, b)| add(*b));
        self.switch_default.into_iter().for_each(&mut add);
        if let Some(asm) = &self.asm_data {
            asm.goto_labels.iter().for_each(|(b, _)| add(*b));
        }
        out
    }

    /// Make every reference this instruction holds to a key of `map` refer
    /// to its value instead.
    ///
    /// One pass over the instruction whatever the map holds: a switch whose
    /// thousands of edges are all being split is rewritten once, not once per
    /// edge.
    fn retarget(&mut self, map: &HashMap<BasicBlockId, BasicBlockId>) {
        let swap = |b: &mut BasicBlockId| {
            if let Some(to) = map.get(b) {
                *b = *to;
            }
        };
        self.bb_true.iter_mut().for_each(swap);
        self.bb_false.iter_mut().for_each(swap);
        self.switch_cases.iter_mut().for_each(|(_, _, b)| swap(b));
        self.switch_default.iter_mut().for_each(swap);
        if let Some(asm) = &mut self.asm_data {
            asm.goto_labels.iter_mut().for_each(|(b, _)| swap(b));
        }
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
            i.asm_data
                .as_ref()
                .is_some_and(|d| !d.goto_labels.is_empty())
        })
    }

    fn has_phi(&self) -> bool {
        self.insns.iter().any(|i| i.op == Opcode::Phi)
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
                if insn.op == Opcode::PhiSource && insn.phi_list.first().is_some_and(|p| p.0 == to)
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
            let feeds = (insn.op == Opcode::PhiSource)
                .then(|| insn.phi_list.first().map(|p| p.0))
                .flatten()
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

            let into = self.get_block_mut(to).expect("edge target exists");
            for p in &mut into.parents {
                if *p == from {
                    *p = mid;
                }
            }
            for insn in &mut into.insns {
                if insn.op == Opcode::Phi {
                    for (p, _) in &mut insn.phi_list {
                        if *p == from {
                            *p = mid;
                        }
                    }
                }
            }
        }
        blocks
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
