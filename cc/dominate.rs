//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Dominator tree computation for pcc C99 compiler
//
// Based on sparse's flowgraph.c and dominate.c:
// - Dominator tree: "A simple, fast dominance algorithm" by Cooper, Harvey, Kennedy
// - IDF computation: "A Linear Time Algorithm for Placing phi-nodes" by Sreedhar and Gao
//

use crate::ir::{BasicBlockId, Function};
use std::collections::{HashMap, HashSet};

// ============================================================================
// Reverse Postorder Computation
// ============================================================================

/// Compute reverse postorder numbering for all blocks.
/// Returns a vector of block IDs in reverse postorder, and updates each block
/// with its postorder number (stored temporarily in dom_level for now).
fn compute_postorder(func: &mut Function) -> Vec<BasicBlockId> {
    let mut visited = HashSet::new();
    let mut postorder = Vec::new();

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
            // Visit children in reverse order (like sparse)
            for &child in bb.children.iter().rev() {
                dfs(func, child, visited, postorder);
            }
        }
        postorder.push(bb_id);
    }

    dfs(func, func.entry, &mut visited, &mut postorder);

    // Assign postorder numbers
    let mut postorder_map = HashMap::new();
    for (i, &bb_id) in postorder.iter().enumerate() {
        postorder_map.insert(bb_id, i as u32);
    }

    // Store postorder numbers in blocks (temporarily in dom_level)
    for bb in &mut func.blocks {
        if let Some(&nr) = postorder_map.get(&bb.id) {
            bb.dom_level = nr;
        }
    }

    // Reverse to get reverse postorder
    postorder.reverse();
    postorder
}

// ============================================================================
// Dominator Tree Construction (Cooper et al.)
// ============================================================================

/// Build the dominator tree for a function.
///
/// Uses the algorithm from:
/// "A simple, fast dominance algorithm" by K. D. Cooper, T. J. Harvey, and K. Kennedy
///
/// This populates:
/// - `bb.idom` - immediate dominator for each block
/// - `bb.dom_level` - depth in dominator tree
/// - `bb.dom_children` - blocks immediately dominated by this block
pub fn domtree_build(func: &mut Function) {
    if func.blocks.is_empty() {
        return;
    }

    // Step 1: Compute reverse postorder
    let rpo = compute_postorder(func);
    let size = rpo.len();
    if size == 0 {
        return;
    }

    // Create postorder number lookup
    let mut postorder_nr: HashMap<BasicBlockId, usize> = HashMap::new();
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
                let parent_nr = postorder_nr[&parent_id];
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
    let mut nr_to_bb: HashMap<usize, BasicBlockId> = HashMap::new();
    for (&bb_id, &nr) in &postorder_nr {
        nr_to_bb.insert(nr, bb_id);
    }

    // Clear old dominator info
    for bb in &mut func.blocks {
        bb.idom = None;
        bb.dom_children.clear();
    }

    // Set idom links
    for bb in &mut func.blocks {
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
                    bb.idom = Some(idom_id);
                }
            }
        }
    }

    // Build dom_children lists
    let idom_pairs: Vec<(BasicBlockId, BasicBlockId)> = func
        .blocks
        .iter()
        .filter_map(|bb| bb.idom.map(|idom| (idom, bb.id)))
        .collect();

    for (idom_id, child_id) in idom_pairs {
        if let Some(idom_bb) = func.get_block_mut(idom_id) {
            idom_bb.dom_children.push(child_id);
        }
    }

    // Compute dominator tree levels
    // Entry is level 0, children are level+1
    let mut max_level = 0u32;
    for &bb_id in &rpo {
        let level = if bb_id == entry {
            0
        } else {
            let idom_level = func
                .get_block(bb_id)
                .and_then(|bb| bb.idom)
                .and_then(|idom_id| func.get_block(idom_id))
                .map(|bb| bb.dom_level)
                .unwrap_or(0);
            idom_level + 1
        };

        if let Some(bb) = func.get_block_mut(bb_id) {
            bb.dom_level = level;
        }

        if level > max_level {
            max_level = level;
        }
    }

    func.max_dom_level = max_level;
}

// ============================================================================
// Dominance Frontier Computation
// ============================================================================

/// Compute the dominance frontier for all blocks.
///
/// The dominance frontier of a block B is the set of blocks where B's dominance ends.
/// More precisely: DF(B) = {D | B dominates a predecessor of D, but B does not strictly dominate D}
///
/// Uses the algorithm from Cytron et al.
pub fn compute_dominance_frontiers(func: &mut Function) {
    // Clear existing frontiers
    for bb in &mut func.blocks {
        bb.dom_frontier.clear();
    }

    // For each block
    let block_ids: Vec<BasicBlockId> = func.blocks.iter().map(|bb| bb.id).collect();

    for &bb_id in &block_ids {
        // Get block's parents
        let parents: Vec<BasicBlockId> = func
            .get_block(bb_id)
            .map(|bb| bb.parents.clone())
            .unwrap_or_default();

        // If block has multiple predecessors (join point)
        if parents.len() >= 2 {
            // Get block's immediate dominator
            let idom = func.get_block(bb_id).and_then(|bb| bb.idom);

            // For each predecessor
            for pred_id in parents {
                // Walk up the dominator tree from pred until we reach idom
                let mut runner = Some(pred_id);
                while runner.is_some() && runner != idom {
                    let runner_id = runner.unwrap();

                    // Add bb_id to runner's dominance frontier
                    if let Some(runner_bb) = func.get_block_mut(runner_id) {
                        if !runner_bb.dom_frontier.contains(&bb_id) {
                            runner_bb.dom_frontier.push(bb_id);
                        }
                    }

                    // Move up to idom
                    runner = func.get_block(runner_id).and_then(|bb| bb.idom);
                }
            }
        }
    }
}

// ============================================================================
// Iterated Dominance Frontier (IDF) Computation
// ============================================================================

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
///
/// # Arguments
/// * `func` - The function (must have dominator tree built)
/// * `alpha` - The set of defining blocks
///
/// # Returns
/// * Vector of blocks in the IDF
pub fn idf_compute(func: &Function, alpha: &[BasicBlockId]) -> Vec<BasicBlockId> {
    if func.max_dom_level == 0 && func.blocks.len() > 1 {
        // Dominator tree not built
        return Vec::new();
    }

    let mut visited = HashSet::new();
    let mut in_idf = HashSet::new();
    let mut in_alpha: HashSet<BasicBlockId> = alpha.iter().copied().collect();
    let mut idf = Vec::new();

    let mut queue = LevelQueue::new(func.max_dom_level);

    // Initialize: put all alpha blocks in the queue
    for &bb_id in alpha {
        if let Some(bb) = func.get_block(bb_id) {
            queue.push(bb_id, bb.dom_level);
        }
    }

    // Process queue
    while let Some(x) = queue.pop() {
        visited.insert(x);

        let x_level = func.get_block(x).map(|bb| bb.dom_level).unwrap_or(0);

        // Get children (successors) of x
        let children: Vec<BasicBlockId> = func
            .get_block(x)
            .map(|bb| bb.children.clone())
            .unwrap_or_default();

        for y in children {
            // Skip if y is dominated by x (not a J-edge)
            let y_idom = func.get_block(y).and_then(|bb| bb.idom);
            if y_idom == Some(x) {
                continue;
            }

            // y must be at same or lower level than x to be in DF
            let y_level = func.get_block(y).map(|bb| bb.dom_level).unwrap_or(0);
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
        let dom_children: Vec<BasicBlockId> = func
            .get_block(x)
            .map(|bb| bb.dom_children.clone())
            .unwrap_or_default();

        for child in dom_children {
            if !visited.contains(&child) {
                // Recursively visit in dominator tree order
                // For proper IDF, we need to visit subtree
                visit_domtree(
                    func,
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
        let y_idom = func.get_block(y).and_then(|bb| bb.idom);
        if y_idom == Some(bb_id) {
            continue;
        }

        // y must be at same or lower level
        let y_level = func.get_block(y).map(|bb| bb.dom_level).unwrap_or(0);
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
    let dom_children: Vec<BasicBlockId> = func
        .get_block(bb_id)
        .map(|bb| bb.dom_children.clone())
        .unwrap_or_default();

    for child in dom_children {
        if !visited.contains(&child) {
            visit_domtree(
                func, child, curr_level, visited, in_idf, in_alpha, idf, queue,
            );
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, Instruction, Opcode};
    use crate::types::TypeTable;

    fn make_test_cfg() -> Function {
        // Create a simple CFG:
        //       entry(0)
        //       /     \
        //      v       v
        //    bb1(1)   bb2(2)
        //       \     /
        //        v   v
        //       merge(3)
        //          |
        //          v
        //        exit(4)

        let types = TypeTable::new();
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
        func
    }

    #[test]
    fn test_domtree_build() {
        let mut func = make_test_cfg();
        domtree_build(&mut func);

        // Entry should have no idom
        let entry = func.get_block(BasicBlockId(0)).unwrap();
        assert!(entry.idom.is_none());
        assert_eq!(entry.dom_level, 0);

        // bb1 and bb2 should have entry as idom
        let bb1 = func.get_block(BasicBlockId(1)).unwrap();
        assert_eq!(bb1.idom, Some(BasicBlockId(0)));
        assert_eq!(bb1.dom_level, 1);

        let bb2 = func.get_block(BasicBlockId(2)).unwrap();
        assert_eq!(bb2.idom, Some(BasicBlockId(0)));
        assert_eq!(bb2.dom_level, 1);

        // merge should have entry as idom (not bb1 or bb2)
        let merge = func.get_block(BasicBlockId(3)).unwrap();
        assert_eq!(merge.idom, Some(BasicBlockId(0)));
        assert_eq!(merge.dom_level, 1);

        // exit should have merge as idom
        let exit = func.get_block(BasicBlockId(4)).unwrap();
        assert_eq!(exit.idom, Some(BasicBlockId(3)));
        assert_eq!(exit.dom_level, 2);
    }

    #[test]
    fn test_dominance_frontiers() {
        let mut func = make_test_cfg();
        domtree_build(&mut func);
        compute_dominance_frontiers(&mut func);

        // bb1's DF should be {merge} - bb1 dominates itself but not merge
        let bb1 = func.get_block(BasicBlockId(1)).unwrap();
        assert!(bb1.dom_frontier.contains(&BasicBlockId(3)));

        // bb2's DF should be {merge}
        let bb2 = func.get_block(BasicBlockId(2)).unwrap();
        assert!(bb2.dom_frontier.contains(&BasicBlockId(3)));

        // merge's DF should be empty
        let merge = func.get_block(BasicBlockId(3)).unwrap();
        assert!(merge.dom_frontier.is_empty());
    }

    #[test]
    fn test_idf_compute() {
        let mut func = make_test_cfg();
        domtree_build(&mut func);

        // IDF of {bb1} should be {merge}
        let idf = idf_compute(&func, &[BasicBlockId(1)]);
        assert!(idf.contains(&BasicBlockId(3)));

        // IDF of {bb1, bb2} should be {merge}
        let idf2 = idf_compute(&func, &[BasicBlockId(1), BasicBlockId(2)]);
        assert!(idf2.contains(&BasicBlockId(3)));
    }

    #[test]
    fn test_dominates() {
        let mut func = make_test_cfg();
        domtree_build(&mut func);

        // Entry dominates everything
        assert!(func.dominates(BasicBlockId(0), BasicBlockId(0)));
        assert!(func.dominates(BasicBlockId(0), BasicBlockId(1)));
        assert!(func.dominates(BasicBlockId(0), BasicBlockId(2)));
        assert!(func.dominates(BasicBlockId(0), BasicBlockId(3)));
        assert!(func.dominates(BasicBlockId(0), BasicBlockId(4)));

        // bb1 only dominates itself
        assert!(func.dominates(BasicBlockId(1), BasicBlockId(1)));
        assert!(!func.dominates(BasicBlockId(1), BasicBlockId(3)));
        assert!(!func.dominates(BasicBlockId(1), BasicBlockId(4)));

        // merge dominates exit
        assert!(func.dominates(BasicBlockId(3), BasicBlockId(4)));
        assert!(!func.dominates(BasicBlockId(3), BasicBlockId(1)));
    }
}
