//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Dominator tree computation.
//
// - Dominator tree: "A simple, fast dominance algorithm" by Cooper, Harvey, Kennedy
// - IDF computation: "A Linear Time Algorithm for Placing phi-nodes" by Sreedhar and Gao
//

use super::{BasicBlockId, Function};
use std::collections::{HashMap, HashSet};

/// The dominator tree of one function, as of the moment it was computed.
///
/// This used to live on the IR itself -- `idom`, `dom_level`, `dom_children`
/// and `dom_frontier` on every `BasicBlock`, `max_dom_level` on the
/// `Function` -- computed once during linearization and never recomputed.
/// Inlining splices whole CFGs into callers and DCE deletes blocks, both
/// afterwards, so from that point the fields described a control-flow graph
/// that no longer existed. It was not a live defect only because nothing
/// downstream read them; the first pass that did -- GVN or LICM, next on the
/// roadmap -- would have read stale data and miscompiled silently.
///
/// Holding the answer in a value returned by the query makes that
/// unrepresentable: a `DomTree` is a snapshot, a pass that changes the CFG
/// drops it, and code that needs one asks again. There is no field to forget
/// to invalidate.
#[derive(Debug, Clone, Default)]
pub struct DomTree {
    /// Position of each block in the parallel vectors below.
    index: HashMap<BasicBlockId, usize>,
    idom: Vec<Option<BasicBlockId>>,
    level: Vec<u32>,
    children: Vec<Vec<BasicBlockId>>,
    max_level: u32,
}

impl DomTree {
    fn slot(&self, id: BasicBlockId) -> Option<usize> {
        self.index.get(&id).copied()
    }

    /// The closest strict dominator of `id`, or `None` for the entry block and
    /// for anything unreachable.
    pub fn idom(&self, id: BasicBlockId) -> Option<BasicBlockId> {
        self.slot(id).and_then(|i| self.idom[i])
    }

    /// Depth of `id` in the dominator tree; the entry block is 0.
    pub fn level(&self, id: BasicBlockId) -> u32 {
        self.slot(id).map(|i| self.level[i]).unwrap_or(0)
    }

    /// The blocks `id` immediately dominates.
    pub fn children(&self, id: BasicBlockId) -> &[BasicBlockId] {
        match self.slot(id) {
            Some(i) => &self.children[i],
            None => &[],
        }
    }

    /// The deepest level in the tree.
    pub fn max_level(&self) -> u32 {
        self.max_level
    }

    /// Whether `a` dominates `b`.
    #[cfg(test)]
    pub fn dominates(&self, a: BasicBlockId, b: BasicBlockId) -> bool {
        if a == b {
            return true;
        }
        let mut current = b;
        while let Some(idom) = self.idom(current) {
            if idom == a {
                return true;
            }
            current = idom;
        }
        false
    }
}

const DEFAULT_POSTORDER_CAPACITY: usize = 16;
const DEFAULT_IDF_CAPACITY: usize = 8;

// Reverse Postorder Computation

/// Compute reverse postorder numbering for all blocks.
///
/// Returns the block IDs in reverse postorder. It used to also stash each
/// block's postorder number in `dom_level` "temporarily"; nothing ever read
/// that, and the level computation below overwrote it.
fn compute_postorder(func: &Function) -> Vec<BasicBlockId> {
    let mut visited = HashSet::with_capacity(DEFAULT_POSTORDER_CAPACITY);
    let mut postorder = Vec::with_capacity(DEFAULT_POSTORDER_CAPACITY);

    fn dfs(
        func: &Function,
        bb_id: BasicBlockId,
        visited: &mut HashSet<BasicBlockId>,
        postorder: &mut Vec<BasicBlockId>,
    ) {
        if visited.contains(&bb_id) {
            return;
        }
        visited.insert(bb_id);

        if let Some(bb) = func.get_block(bb_id) {
            // Visit children in reverse order for consistent numbering
            for &child in bb.children.iter().rev() {
                dfs(func, child, visited, postorder);
            }
        }
        postorder.push(bb_id);
    }

    dfs(func, func.entry, &mut visited, &mut postorder);

    // Reverse to get reverse postorder
    postorder.reverse();
    postorder
}

// Dominator Tree Construction (Cooper et al.)

/// Build the dominator tree for a function.
///
/// Uses the algorithm from:
/// "A simple, fast dominance algorithm" by K. D. Cooper, T. J. Harvey, and K. Kennedy
///
/// The result is returned rather than written into the blocks, so it cannot
/// outlive the CFG it describes -- see [`DomTree`].
pub fn domtree_build(func: &Function) -> DomTree {
    let mut dom = DomTree {
        index: func
            .blocks
            .iter()
            .enumerate()
            .map(|(i, bb)| (bb.id, i))
            .collect(),
        idom: vec![None; func.blocks.len()],
        level: vec![0; func.blocks.len()],
        children: vec![Vec::new(); func.blocks.len()],
        max_level: 0,
    };
    if func.blocks.is_empty() {
        return dom;
    }

    // Step 1: Compute reverse postorder
    let rpo = compute_postorder(func);
    let size = rpo.len();
    if size == 0 {
        return dom;
    }

    // Create postorder number lookup
    let mut postorder_nr: HashMap<BasicBlockId, usize> = HashMap::with_capacity(size);
    for (i, &bb_id) in rpo.iter().rev().enumerate() {
        postorder_nr.insert(bb_id, i);
    }

    // Entry block has highest postorder number
    let entry = func.entry;
    let entry_nr = size - 1;

    // Initialize dominators array
    // doms[postorder_nr] = immediate dominator's postorder_nr
    let mut doms: Vec<Option<usize>> = vec![None; size];
    doms[entry_nr] = Some(entry_nr); // Entry dominates itself

    // Helper: intersect two dominators using postorder numbers
    let intersect = |doms: &[Option<usize>], mut b1: usize, mut b2: usize| -> usize {
        while b1 != b2 {
            while b1 < b2 {
                if let Some(d) = doms[b1] {
                    b1 = d;
                } else {
                    break;
                }
            }
            while b2 < b1 {
                if let Some(d) = doms[b2] {
                    b2 = d;
                } else {
                    break;
                }
            }
        }
        b1
    };

    // Iterate until fixed point
    let mut changed = true;
    while changed {
        changed = false;

        for &bb_id in &rpo {
            if bb_id == entry {
                continue;
            }

            let bb_nr = postorder_nr[&bb_id];

            // Get parents of this block
            let parents: Vec<BasicBlockId> = func
                .get_block(bb_id)
                .map(|bb| bb.parents.clone())
                .unwrap_or_default();

            // Find new idom as intersection of all processed predecessors
            let mut new_idom: Option<usize> = None;
            for parent_id in parents {
                // Skip predecessors that weren't reached during DFS (unreachable blocks)
                let parent_nr = match postorder_nr.get(&parent_id) {
                    Some(&nr) => nr,
                    None => continue,
                };
                if doms[parent_nr].is_none() {
                    continue;
                }
                new_idom = Some(match new_idom {
                    None => parent_nr,
                    Some(current) => intersect(&doms, parent_nr, current),
                });
            }

            if let Some(idom) = new_idom {
                if doms[bb_nr] != Some(idom) {
                    doms[bb_nr] = Some(idom);
                    changed = true;
                }
            }
        }
    }

    // Create reverse mapping: postorder_nr -> BasicBlockId
    let mut nr_to_bb: HashMap<usize, BasicBlockId> = HashMap::with_capacity(size);
    for (&bb_id, &nr) in &postorder_nr {
        nr_to_bb.insert(nr, bb_id);
    }

    // Set idom links
    for (i, bb) in func.blocks.iter().enumerate() {
        if bb.id == entry {
            continue;
        }
        // Skip unreachable blocks (not in postorder)
        let bb_nr = match postorder_nr.get(&bb.id) {
            Some(&nr) => nr,
            None => continue, // Unreachable block
        };
        if let Some(idom_nr) = doms[bb_nr] {
            if idom_nr != bb_nr {
                // Map back to BasicBlockId
                if let Some(&idom_id) = nr_to_bb.get(&idom_nr) {
                    dom.idom[i] = Some(idom_id);
                }
            }
        }
    }

    // Build dom_children lists
    for (i, bb) in func.blocks.iter().enumerate() {
        if let Some(idom_id) = dom.idom[i] {
            if let Some(&slot) = dom.index.get(&idom_id) {
                dom.children[slot].push(bb.id);
            }
        }
    }

    // Compute dominator tree levels.
    // Entry is level 0, children are level+1. Walking in reverse postorder
    // means a block's idom already has its final level.
    for &bb_id in &rpo {
        let level = if bb_id == entry {
            0
        } else {
            dom.idom(bb_id).map(|i| dom.level(i)).unwrap_or(0) + 1
        };
        if let Some(&slot) = dom.index.get(&bb_id) {
            dom.level[slot] = level;
        }
        if level > dom.max_level {
            dom.max_level = level;
        }
    }

    dom
}

// Iterated Dominance Frontier (IDF) Computation

/// Priority queue based on dominator tree level (higher level = higher priority).
/// Used by the Sreedhar-Gao algorithm.
struct LevelQueue {
    /// Buckets indexed by level
    buckets: Vec<Vec<BasicBlockId>>,
    /// Current maximum non-empty level
    max_level: usize,
}

impl LevelQueue {
    fn new(max_level: u32) -> Self {
        Self {
            buckets: vec![Vec::new(); max_level as usize + 1],
            max_level: 0,
        }
    }

    fn push(&mut self, bb_id: BasicBlockId, level: u32) {
        let level = level as usize;
        self.buckets[level].push(bb_id);
        if level > self.max_level {
            self.max_level = level;
        }
    }

    fn pop(&mut self) -> Option<BasicBlockId> {
        loop {
            if let Some(bb) = self.buckets[self.max_level].pop() {
                return Some(bb);
            }
            if self.max_level == 0 {
                return None;
            }
            self.max_level -= 1;
        }
    }
}

/// Compute the iterated dominance frontier of a set of blocks.
///
/// The IDF of a set S is DF*(S) = DF(S) ∪ DF(DF(S)) ∪ DF(DF(DF(S))) ∪ ...
/// until fixed point.
///
/// Uses the linear time algorithm from:
/// "A Linear Time Algorithm for Placing phi-nodes" by Sreedhar and Gao
pub fn idf_compute(func: &Function, dom: &DomTree, alpha: &[BasicBlockId]) -> Vec<BasicBlockId> {
    if dom.max_level() == 0 && func.blocks.len() > 1 {
        // Dominator tree not built
        return Vec::new();
    }

    let mut visited = HashSet::with_capacity(DEFAULT_IDF_CAPACITY);
    let mut in_idf = HashSet::with_capacity(DEFAULT_IDF_CAPACITY);
    let mut in_alpha: HashSet<BasicBlockId> = alpha.iter().copied().collect();
    let mut idf = Vec::with_capacity(DEFAULT_IDF_CAPACITY);

    let mut queue = LevelQueue::new(dom.max_level());

    // Initialize: put all alpha blocks in the queue
    for &bb_id in alpha {
        if func.get_block(bb_id).is_some() {
            queue.push(bb_id, dom.level(bb_id));
        }
    }

    // Process queue
    while let Some(x) = queue.pop() {
        visited.insert(x);

        let x_level = dom.level(x);

        // Get children (successors) of x
        let children: Vec<BasicBlockId> = func
            .get_block(x)
            .map(|bb| bb.children.clone())
            .unwrap_or_default();

        for y in children {
            // Skip if y is dominated by x (not a J-edge)
            if dom.idom(y) == Some(x) {
                continue;
            }

            // y must be at same or lower level than x to be in DF
            let y_level = dom.level(y);
            if y_level > x_level {
                continue;
            }

            // Add y to IDF if not already there
            if !in_idf.contains(&y) {
                in_idf.insert(y);
                idf.push(y);

                // If y is not in alpha, add it to the queue for further exploration
                if !in_alpha.contains(&y) {
                    queue.push(y, y_level);
                }
            }
        }

        // Visit dominator tree children
        let dom_children: Vec<BasicBlockId> = dom.children(x).to_vec();

        for child in dom_children {
            if !visited.contains(&child) {
                // Recursively visit in dominator tree order
                // For proper IDF, we need to visit subtree
                visit_domtree(
                    func,
                    dom,
                    child,
                    x_level,
                    &mut visited,
                    &mut in_idf,
                    &mut in_alpha,
                    &mut idf,
                    &mut queue,
                );
            }
        }
    }

    idf
}

#[allow(clippy::too_many_arguments)]
fn visit_domtree(
    func: &Function,
    dom: &DomTree,
    bb_id: BasicBlockId,
    curr_level: u32,
    visited: &mut HashSet<BasicBlockId>,
    in_idf: &mut HashSet<BasicBlockId>,
    in_alpha: &mut HashSet<BasicBlockId>,
    idf: &mut Vec<BasicBlockId>,
    queue: &mut LevelQueue,
) {
    visited.insert(bb_id);

    // Check successors
    let children: Vec<BasicBlockId> = func
        .get_block(bb_id)
        .map(|bb| bb.children.clone())
        .unwrap_or_default();

    for y in children {
        // Skip if y is dominated by bb_id (not a J-edge)
        if dom.idom(y) == Some(bb_id) {
            continue;
        }

        // y must be at same or lower level
        let y_level = dom.level(y);
        if y_level > curr_level {
            continue;
        }

        if !in_idf.contains(&y) {
            in_idf.insert(y);
            idf.push(y);

            if !in_alpha.contains(&y) {
                queue.push(y, y_level);
            }
        }
    }

    // Recurse into dominator tree children
    let dom_children: Vec<BasicBlockId> = dom.children(bb_id).to_vec();

    for child in dom_children {
        if !visited.contains(&child) {
            visit_domtree(
                func, dom, child, curr_level, visited, in_idf, in_alpha, idf, queue,
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, Instruction, Opcode};
    use crate::target::Target;
    use crate::types::TypeTable;

    fn make_test_cfg() -> Function {
        // Create a simple CFG:

        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("test", types.void_id);

        let mut entry = BasicBlock::new(BasicBlockId(0));
        entry.children = vec![BasicBlockId(1), BasicBlockId(2)];
        entry.add_insn(Instruction::new(Opcode::Entry));
        entry.add_insn(Instruction::cbr(
            crate::ir::PseudoId(0),
            BasicBlockId(1),
            BasicBlockId(2),
        ));

        let mut bb1 = BasicBlock::new(BasicBlockId(1));
        bb1.parents = vec![BasicBlockId(0)];
        bb1.children = vec![BasicBlockId(3)];
        bb1.add_insn(Instruction::br(BasicBlockId(3)));

        let mut bb2 = BasicBlock::new(BasicBlockId(2));
        bb2.parents = vec![BasicBlockId(0)];
        bb2.children = vec![BasicBlockId(3)];
        bb2.add_insn(Instruction::br(BasicBlockId(3)));

        let mut merge = BasicBlock::new(BasicBlockId(3));
        merge.parents = vec![BasicBlockId(1), BasicBlockId(2)];
        merge.children = vec![BasicBlockId(4)];
        merge.add_insn(Instruction::br(BasicBlockId(4)));

        let mut exit = BasicBlock::new(BasicBlockId(4));
        exit.parents = vec![BasicBlockId(3)];
        exit.add_insn(Instruction::ret(None));

        func.entry = BasicBlockId(0);
        func.blocks = vec![entry, bb1, bb2, merge, exit];
        func.rebuild_block_idx();
        func
    }

    #[test]
    fn test_domtree_build() {
        let func = make_test_cfg();
        let dom = domtree_build(&func);

        // Entry should have no idom
        assert!(dom.idom(BasicBlockId(0)).is_none());
        assert_eq!(dom.level(BasicBlockId(0)), 0);

        // bb1 and bb2 should have entry as idom
        assert_eq!(dom.idom(BasicBlockId(1)), Some(BasicBlockId(0)));
        assert_eq!(dom.level(BasicBlockId(1)), 1);

        assert_eq!(dom.idom(BasicBlockId(2)), Some(BasicBlockId(0)));
        assert_eq!(dom.level(BasicBlockId(2)), 1);

        // merge should have entry as idom (not bb1 or bb2)
        assert_eq!(dom.idom(BasicBlockId(3)), Some(BasicBlockId(0)));
        assert_eq!(dom.level(BasicBlockId(3)), 1);

        // exit should have merge as idom
        assert_eq!(dom.idom(BasicBlockId(4)), Some(BasicBlockId(3)));
        assert_eq!(dom.level(BasicBlockId(4)), 2);
    }

    #[test]
    fn test_idf_compute() {
        let func = make_test_cfg();
        let dom = domtree_build(&func);

        // IDF of {bb1} should be {merge}
        let idf = idf_compute(&func, &dom, &[BasicBlockId(1)]);
        assert!(idf.contains(&BasicBlockId(3)));

        // IDF of {bb1, bb2} should be {merge}
        let idf2 = idf_compute(&func, &dom, &[BasicBlockId(1), BasicBlockId(2)]);
        assert!(idf2.contains(&BasicBlockId(3)));
    }

    #[test]
    fn test_dominates() {
        let func = make_test_cfg();
        let dom = domtree_build(&func);

        // Entry dominates everything
        assert!(dom.dominates(BasicBlockId(0), BasicBlockId(0)));
        assert!(dom.dominates(BasicBlockId(0), BasicBlockId(1)));
        assert!(dom.dominates(BasicBlockId(0), BasicBlockId(2)));
        assert!(dom.dominates(BasicBlockId(0), BasicBlockId(3)));
        assert!(dom.dominates(BasicBlockId(0), BasicBlockId(4)));

        // bb1 dominates only itself
        assert!(dom.dominates(BasicBlockId(1), BasicBlockId(1)));
        assert!(!dom.dominates(BasicBlockId(1), BasicBlockId(3)));
        assert!(!dom.dominates(BasicBlockId(1), BasicBlockId(4)));

        // merge dominates exit
        assert!(dom.dominates(BasicBlockId(3), BasicBlockId(4)));
        assert!(!dom.dominates(BasicBlockId(3), BasicBlockId(1)));
    }

    /// A tree is a snapshot: it describes the CFG it was built from.
    ///
    /// This is the property the old design could not have. The dominator data
    /// lived on the blocks, was computed once during linearization, and was
    /// never recomputed -- so after inlining spliced a CFG in or DCE deleted a
    /// block, every reader got an answer about a graph that no longer existed.
    /// Now the answer is a value, and asking again after the change is the
    /// only way to get one.
    #[test]
    fn test_domtree_is_a_snapshot_of_the_cfg_it_was_built_from() {
        let mut func = make_test_cfg();
        let before = domtree_build(&func);
        assert_eq!(before.idom(BasicBlockId(4)), Some(BasicBlockId(3)));

        // Re-route the exit so entry reaches it directly: exit's immediate
        // dominator is now the entry, not the merge.
        func.get_block_mut(BasicBlockId(0)).unwrap().children = vec![BasicBlockId(4)];
        func.get_block_mut(BasicBlockId(4)).unwrap().parents = vec![BasicBlockId(0)];

        let after = domtree_build(&func);
        assert_eq!(after.idom(BasicBlockId(4)), Some(BasicBlockId(0)));

        // The old tree still answers about the old graph -- it is a value, not
        // a view, so nothing about it silently changed underneath.
        assert_eq!(before.idom(BasicBlockId(4)), Some(BasicBlockId(3)));
    }
}
