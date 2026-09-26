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

    /// Whether `a` dominates `b`: every path from entry to `b` passes
    /// through `a`. Reflexive.
    ///
    /// An O(depth) walk up the immediate-dominator chain, not an interval
    /// test -- fine for the occasional query an optimizer pass makes, and
    /// worth revisiting if one ever asks per-instruction.
    ///
    /// Test-only until a pass outside `ssa` needs it; un-gate it there
    /// rather than here, so it is never dead code.
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

const DEFAULT_IDF_CAPACITY: usize = 8;

/// The blocks control can reach from `bb`.
///
/// `children` plus the targets an `asm goto` names, which live in
/// `asm_data.goto_labels` and nowhere else -- the same edges `ir/sccp.rs`
/// has to go and find by hand.
fn successors(bb: &crate::ir::BasicBlock) -> Vec<BasicBlockId> {
    let mut out = bb.children.clone();
    for insn in &bb.insns {
        if let Some(ref asm) = insn.asm_data {
            for (t, _) in &asm.goto_labels {
                if !out.contains(t) {
                    out.push(*t);
                }
            }
        }
    }
    out
}

// Dominator Tree Construction (Lengauer-Tarjan)

/// A DFS index with no node behind it: the root's parent and ancestor.
const NONE: usize = usize::MAX;

/// The depth-first spanning tree from the entry.
struct Numbering {
    /// Each reached block's DFS number.
    number: HashMap<BasicBlockId, usize>,
    /// The block numbered `n`.
    vertex: Vec<BasicBlockId>,
    /// The node `n` was first reached from; `NONE` for the entry.
    parent: Vec<usize>,
    /// The successors of node `n`, as [`successors`] gives them.
    succs: Vec<Vec<BasicBlockId>>,
}

/// Number the blocks reachable from the entry in depth-first preorder.
///
/// Iterative, because a function of a few hundred thousand blocks in a chain
/// is a legal input, and recursion would spend one host stack frame per block.
fn dfs_numbering(func: &Function) -> Numbering {
    let cap = func.blocks.len();
    let mut t = Numbering {
        number: HashMap::with_capacity(cap),
        vertex: Vec::with_capacity(cap),
        parent: Vec::with_capacity(cap),
        succs: Vec::with_capacity(cap),
    };
    // (node, index of the next successor to try)
    let mut stack: Vec<(usize, usize)> = Vec::new();
    let visit = |t: &mut Numbering, id: BasicBlockId, from: usize| {
        let n = t.vertex.len();
        t.number.insert(id, n);
        t.vertex.push(id);
        t.parent.push(from);
        t.succs
            .push(func.get_block(id).map(successors).unwrap_or_default());
        n
    };
    stack.push((visit(&mut t, func.entry, NONE), 0));
    while let Some(&(n, next)) = stack.last() {
        match t.succs[n].get(next).copied() {
            Some(s) => {
                stack.last_mut().unwrap().1 += 1;
                if !t.number.contains_key(&s) {
                    stack.push((visit(&mut t, s, n), 0));
                }
            }
            None => {
                stack.pop();
            }
        }
    }
    t
}

/// Build the dominator tree for a function.
///
/// Lengauer and Tarjan, "A Fast Algorithm for Finding Dominators in a
/// Flowgraph" (1979), in its simple form: semidominators computed over a
/// depth-first spanning tree with a path-compressing `eval`, then immediate
/// dominators from the semidominator buckets. O(E log V), whatever the shape.
///
/// It replaced Cooper, Harvey and Kennedy's iterative algorithm, whose
/// `intersect` walks the dominator chain once per predecessor: a join with
/// thousands of predecessors under a deep chain -- the two labels every
/// `if ... goto` in `compile/20001226-1` jumps to, or the end of a big
/// `switch` -- cost predecessors x depth on every build, and a function is
/// built for twice per optimization run (`ssa_convert`, `loadfwd`).
///
/// The graph is the one [`successors`] describes, and nothing else: a block
/// the walk from the entry never reaches has no immediate dominator. The
/// result depends only on the graph, so the tree is the one any correct
/// algorithm computes; `children` are listed in `func.blocks` order.
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

    let Numbering {
        number,
        vertex,
        parent,
        succs,
    } = dfs_numbering(func);
    let size = vertex.len();

    // Predecessors, by DFS number, of every reached node. An edge from a block
    // the walk never reached is not in the graph.
    let mut preds: Vec<Vec<usize>> = vec![Vec::new(); size];
    for (v, out) in succs.iter().enumerate() {
        for s in out {
            preds[number[s]].push(v);
        }
    }

    // `semi[w]` is w's semidominator's number; `label`/`ancestor` are the
    // link-eval forest, compressed as it is walked.
    let mut semi: Vec<usize> = (0..size).collect();
    let mut label: Vec<usize> = (0..size).collect();
    let mut ancestor: Vec<usize> = vec![NONE; size];
    let mut idom: Vec<usize> = vec![NONE; size];
    let mut bucket: Vec<Vec<usize>> = vec![Vec::new(); size];
    let mut path: Vec<usize> = Vec::new();

    // The node with the least semidominator on the forest path above `v`.
    let eval = |v: usize,
                semi: &[usize],
                label: &mut [usize],
                ancestor: &mut [usize],
                path: &mut Vec<usize>|
     -> usize {
        if ancestor[v] == NONE {
            return v;
        }
        // Compress the path, root end first, as the recursive form would.
        let mut x = v;
        while ancestor[ancestor[x]] != NONE {
            path.push(x);
            x = ancestor[x];
        }
        while let Some(y) = path.pop() {
            let a = ancestor[y];
            if semi[label[a]] < semi[label[y]] {
                label[y] = label[a];
            }
            ancestor[y] = ancestor[a];
        }
        label[v]
    };

    for w in (1..size).rev() {
        for &v in &preds[w] {
            let u = eval(v, &semi, &mut label, &mut ancestor, &mut path);
            if semi[u] < semi[w] {
                semi[w] = semi[u];
            }
        }
        bucket[semi[w]].push(w);
        let p = parent[w];
        ancestor[w] = p;
        for v in std::mem::take(&mut bucket[p]) {
            let u = eval(v, &semi, &mut label, &mut ancestor, &mut path);
            idom[v] = if semi[u] < semi[v] { u } else { p };
        }
    }
    for w in 1..size {
        if idom[w] != semi[w] {
            idom[w] = idom[idom[w]];
        }
    }

    // Record, and level by DFS number: a node's immediate dominator is a DFS
    // ancestor, so it is numbered, and levelled, first.
    // A successor id with no block behind it is a node of the graph -- the
    // walk reaches it -- but has no slot to record; it counts at level 1, as
    // it always has.
    let mut level: Vec<u32> = vec![0; size];
    for w in 1..size {
        level[w] = level[idom[w]] + 1;
        match dom.index.get(&vertex[w]) {
            Some(&slot) => {
                dom.idom[slot] = Some(vertex[idom[w]]);
                dom.level[slot] = level[w];
                dom.max_level = dom.max_level.max(level[w]);
            }
            None => dom.max_level = dom.max_level.max(1),
        }
    }

    // Children in `func.blocks` order.
    for (i, bb) in func.blocks.iter().enumerate() {
        if let Some(idom_id) = dom.idom[i] {
            if let Some(&slot) = dom.index.get(&idom_id) {
                dom.children[slot].push(bb.id);
            }
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

/// Visit the dominator subtree under `root` in pre-order -- a block, then
/// each dominated child's subtree in order -- recording the J-edges that
/// leave it at or above `curr_level`.
///
/// On an explicit stack rather than the call stack: a function of n
/// sequential `if` statements has a dominator tree n levels deep, and a
/// recursion per level overflowed the compiler's stack. Children are pushed
/// in reverse so they are popped, and so visited, in order.
#[allow(clippy::too_many_arguments)]
fn visit_domtree(
    func: &Function,
    dom: &DomTree,
    root: BasicBlockId,
    curr_level: u32,
    visited: &mut HashSet<BasicBlockId>,
    in_idf: &mut HashSet<BasicBlockId>,
    in_alpha: &mut HashSet<BasicBlockId>,
    idf: &mut Vec<BasicBlockId>,
    queue: &mut LevelQueue,
) {
    let mut stack = vec![root];
    while let Some(bb_id) = stack.pop() {
        if !visited.insert(bb_id) {
            continue;
        }

        // Check successors
        let children: &[BasicBlockId] = func
            .get_block(bb_id)
            .map(|bb| bb.children.as_slice())
            .unwrap_or(&[]);

        for &y in children {
            // Skip if y is dominated by bb_id (not a J-edge)
            if dom.idom(y) == Some(bb_id) {
                continue;
            }

            // y must be at same or lower level
            let y_level = dom.level(y);
            if y_level > curr_level {
                continue;
            }

            if in_idf.insert(y) {
                idf.push(y);
                if !in_alpha.contains(&y) {
                    queue.push(y, y_level);
                }
            }
        }

        stack.extend(
            dom.children(bb_id)
                .iter()
                .rev()
                .copied()
                .filter(|c| !visited.contains(c)),
        );
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

    /// The dominator tree follows `children`, never `parents`.
    ///
    /// `dce::fold_branches_to_unreachable` removes an edge from `children`
    /// and leaves it in `parents`, so the two fields disagree in any function
    /// where a branch was folded. Reading the stale one made the postorder
    /// walk and the fixed point analyze two different graphs.
    #[test]
    fn test_domtree_ignores_stale_parents() {
        let mut func = make_test_cfg();
        // The shape `dce` leaves behind: bb2 no longer reaches the merge,
        // but the merge still lists it as a parent.
        func.get_block_mut(BasicBlockId(2))
            .unwrap()
            .children
            .clear();
        let dom = domtree_build(&func);
        assert_eq!(
            dom.idom(BasicBlockId(3)),
            Some(BasicBlockId(1)),
            "with bb2's edge gone, bb1 is the merge's only predecessor"
        );
    }

    /// An `asm goto` target is a CFG edge that lives only in `asm_data`, so
    /// both halves of the computation have to go and find it.
    #[test]
    fn test_domtree_sees_asm_goto_edges() {
        let mut func = make_test_cfg();
        let mut asm = Instruction::new(Opcode::Asm);
        asm.asm_data = Some(Box::new(crate::ir::AsmData {
            template: String::new(),
            outputs: vec![],
            inputs: vec![],
            clobbers: vec![],
            goto_labels: vec![(BasicBlockId(4), "l".into())],
        }));
        let bb0 = func.get_block_mut(BasicBlockId(0)).unwrap();
        bb0.insns.insert(0, asm);
        let dom = domtree_build(&func);
        assert_eq!(
            dom.idom(BasicBlockId(4)),
            Some(BasicBlockId(0)),
            "the entry can jump straight to the exit, so the merge no longer dominates it"
        );
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

    // ------------------------------------------------------------------
    // Oracle: dominators by their definition -- the iterative dataflow
    // `Dom(n) = {n} U intersection of Dom(p)` over the same graph -- with
    // the immediate dominator read off as the strict dominator whose own
    // set is largest. Obviously correct, cubic, and independent of both the
    // builder and the algorithm it replaced.
    // ------------------------------------------------------------------

    fn oracle(func: &Function) -> DomTree {
        use std::collections::BTreeSet;
        // Nodes: everything the walk from the entry reaches, including a
        // successor id with no block behind it.
        let mut nodes: Vec<BasicBlockId> = vec![func.entry];
        let mut seen: HashSet<BasicBlockId> = [func.entry].into();
        let mut i = 0;
        while i < nodes.len() {
            if let Some(bb) = func.get_block(nodes[i]) {
                for s in successors(bb) {
                    if seen.insert(s) {
                        nodes.push(s);
                    }
                }
            }
            i += 1;
        }
        let mut preds: HashMap<BasicBlockId, Vec<BasicBlockId>> = HashMap::new();
        for &n in &nodes {
            if let Some(bb) = func.get_block(n) {
                for s in successors(bb) {
                    preds.entry(s).or_default().push(n);
                }
            }
        }
        let all: BTreeSet<BasicBlockId> = nodes.iter().copied().collect();
        let mut dom: HashMap<BasicBlockId, BTreeSet<BasicBlockId>> =
            nodes.iter().map(|&n| (n, all.clone())).collect();
        dom.insert(func.entry, [func.entry].into());
        let mut changed = true;
        while changed {
            changed = false;
            for &n in nodes.iter().skip(1) {
                let mut new: Option<BTreeSet<BasicBlockId>> = None;
                for p in preds.get(&n).into_iter().flatten() {
                    let d = &dom[p];
                    new = Some(match new {
                        None => d.clone(),
                        Some(acc) => acc.intersection(d).copied().collect(),
                    });
                }
                let mut new = new.unwrap_or_default();
                new.insert(n);
                if new != dom[&n] {
                    dom.insert(n, new);
                    changed = true;
                }
            }
        }

        let mut tree = DomTree {
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
        for &n in nodes.iter().skip(1) {
            let d = &dom[&n];
            let idom = d
                .iter()
                .filter(|&&x| x != n)
                .max_by_key(|x| dom[*x].len())
                .copied();
            let level = (d.len() - 1) as u32;
            match tree.index.get(&n) {
                Some(&slot) => {
                    tree.idom[slot] = idom;
                    tree.level[slot] = level;
                    tree.max_level = tree.max_level.max(level);
                }
                None => tree.max_level = tree.max_level.max(1),
            }
        }
        for (i, bb) in func.blocks.iter().enumerate() {
            if let Some(p) = tree.idom[i] {
                if let Some(&slot) = tree.index.get(&p) {
                    tree.children[slot].push(bb.id);
                }
            }
        }
        tree
    }

    /// The builder's tree, compared with the oracle's field by field.
    fn assert_matches_oracle(func: &Function, what: &str) {
        let got = domtree_build(func);
        let want = oracle(func);
        for bb in &func.blocks {
            assert_eq!(
                got.idom(bb.id),
                want.idom(bb.id),
                "{what}: idom of {:?}",
                bb.id
            );
            assert_eq!(
                got.level(bb.id),
                want.level(bb.id),
                "{what}: level of {:?}",
                bb.id
            );
            assert_eq!(
                got.children(bb.id),
                want.children(bb.id),
                "{what}: children of {:?}",
                bb.id
            );
        }
        assert_eq!(got.max_level(), want.max_level(), "{what}: max level");
    }

    /// A function whose blocks `0..n` have the given successor lists; block 0
    /// is the entry. A successor id past `n` names no block.
    fn cfg(edges: &[&[u32]]) -> Function {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("t", types.void_id);
        func.blocks = edges
            .iter()
            .enumerate()
            .map(|(i, out)| {
                let mut bb = BasicBlock::new(BasicBlockId(i as u32));
                bb.children = out.iter().map(|&c| BasicBlockId(c)).collect();
                bb
            })
            .collect();
        func.entry = BasicBlockId(0);
        func.rebuild_block_idx();
        func
    }

    #[test]
    fn test_domtree_matches_oracle_on_shapes() {
        let wide: Vec<Vec<u32>> = {
            // A deep chain every link of which also jumps to one join -- the
            // shape that made the old algorithm quadratic.
            let n = 300u32;
            let mut e: Vec<Vec<u32>> = (0..n).map(|i| vec![i + 1, n + 1]).collect();
            e.push(vec![n + 1]);
            e.push(vec![]);
            e
        };
        let wide: Vec<&[u32]> = wide.iter().map(Vec::as_slice).collect();
        let shapes: &[(&str, &[&[u32]])] = &[
            ("single block", &[&[]]),
            ("diamond", &[&[1, 2], &[3], &[3], &[]]),
            ("loop", &[&[1], &[2, 3], &[1], &[]]),
            ("self loop", &[&[1], &[1, 2], &[]]),
            ("nested loops", &[&[1], &[2, 5], &[3], &[2, 4], &[1], &[]]),
            // Irreducible: two entries into the 2<->3 cycle.
            ("irreducible", &[&[1, 2], &[3], &[3, 4], &[2, 4], &[]]),
            ("unreachable block", &[&[1], &[], &[1]]),
            ("unreachable cycle", &[&[1], &[], &[3], &[2, 1]]),
            ("edge to no block", &[&[1, 9], &[]]),
            ("duplicate edge", &[&[1, 1], &[]]),
            ("wide join", &wide),
        ];
        for (what, edges) in shapes {
            assert_matches_oracle(&cfg(edges), what);
        }
    }

    /// Many random graphs, from a fixed seed: reducible or not, with
    /// unreachable blocks, self loops and repeated edges.
    #[test]
    fn test_domtree_matches_oracle_on_random_graphs() {
        let mut seed: u64 = 0x2545_F491_4F6C_DD1D;
        let mut next = |bound: u32| {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            (seed % u64::from(bound)) as u32
        };
        for case in 0..2000 {
            let n = 1 + next(24);
            let edges: Vec<Vec<u32>> = (0..n)
                .map(|_| (0..next(4)).map(|_| next(n)).collect())
                .collect();
            let edges: Vec<&[u32]> = edges.iter().map(Vec::as_slice).collect();
            assert_matches_oracle(&cfg(&edges), &format!("random graph {case}: {edges:?}"));
        }
    }

    /// A chain far deeper than any recursion could walk on a host stack.
    #[test]
    fn test_domtree_handles_a_deep_chain() {
        let n = 200_000u32;
        let edges: Vec<Vec<u32>> = (0..n)
            .map(|i| if i + 1 < n { vec![i + 1] } else { vec![] })
            .collect();
        let edges: Vec<&[u32]> = edges.iter().map(Vec::as_slice).collect();
        let dom = domtree_build(&cfg(&edges));
        assert_eq!(dom.idom(BasicBlockId(n - 1)), Some(BasicBlockId(n - 2)));
        assert_eq!(dom.level(BasicBlockId(n - 1)), n - 1);
        assert_eq!(dom.max_level(), n - 1);
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
