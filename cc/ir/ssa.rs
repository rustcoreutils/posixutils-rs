//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// SSA Conversion for pcc C99 compiler
//
// Converts memory-based local variables to SSA form:
// - Inserts phi nodes at dominance frontiers
// - Renames variables to complete SSA construction
//

use super::dominate::{compute_dominance_frontiers, domtree_build, idf_compute};
use super::{BasicBlockId, Function, InsnRef, Instruction, Opcode, Pseudo, PseudoId, PseudoKind};
use crate::types::{TypeId, TypeTable};
use std::collections::{HashMap, HashSet};

const DEFAULT_SSA_RENAME_CAPACITY: usize = 32;
const DEFAULT_SSA_PHI_CAPACITY: usize = 16;

// ============================================================================
// SSA Conversion State
// ============================================================================

/// State for SSA conversion
struct SsaConverter<'a> {
    func: &'a mut Function,

    /// Variables marked for renaming (name -> type)
    to_rename: HashSet<String>,

    /// All inserted phi nodes for later processing
    all_phis: Vec<(BasicBlockId, usize)>, // (block, instruction index)

    /// Stores to remove after conversion
    dead_stores: Vec<InsnRef>,

    /// Counter for generating new pseudo IDs
    next_pseudo_id: u32,

    /// Counter for generating new register numbers
    next_reg_nr: u32,
}

impl<'a> SsaConverter<'a> {
    fn new(func: &'a mut Function) -> Self {
        // Find the maximum pseudo ID and reg number currently in use
        // Must scan BOTH func.pseudos AND all instruction targets/sources,
        // since alloc_pseudo() in the linearizer doesn't add to func.pseudos
        let mut max_pseudo_id = func.pseudos.iter().map(|p| p.id.0).max().unwrap_or(0);

        // Also scan all instruction targets and sources
        for bb in &func.blocks {
            for insn in &bb.insns {
                if let Some(target) = insn.target {
                    max_pseudo_id = max_pseudo_id.max(target.0);
                }
                for src in &insn.src {
                    max_pseudo_id = max_pseudo_id.max(src.0);
                }
                if let Some(indirect) = insn.indirect_target {
                    max_pseudo_id = max_pseudo_id.max(indirect.0);
                }
                for (_, phi_src) in &insn.phi_list {
                    max_pseudo_id = max_pseudo_id.max(phi_src.0);
                }
            }
        }

        let max_reg_nr = func
            .pseudos
            .iter()
            .filter_map(|p| match &p.kind {
                PseudoKind::Reg(nr) => Some(*nr),
                _ => None,
            })
            .max()
            .unwrap_or(0);

        Self {
            func,
            to_rename: HashSet::with_capacity(DEFAULT_SSA_RENAME_CAPACITY),
            all_phis: Vec::with_capacity(DEFAULT_SSA_PHI_CAPACITY),
            dead_stores: Vec::with_capacity(DEFAULT_SSA_PHI_CAPACITY),
            next_pseudo_id: max_pseudo_id + 1,
            next_reg_nr: max_reg_nr + 1,
        }
    }

    /// Allocate a new phi pseudo
    fn alloc_phi(&mut self) -> PseudoId {
        let id = PseudoId(self.next_pseudo_id);
        self.next_pseudo_id += 1;

        let nr = self.next_reg_nr;
        self.next_reg_nr += 1;

        let pseudo = Pseudo::phi(id, nr);
        self.func.add_pseudo(pseudo);
        id
    }

    /// Create an undef pseudo
    fn undef_pseudo(&mut self) -> PseudoId {
        let id = PseudoId(self.next_pseudo_id);
        self.next_pseudo_id += 1;

        let pseudo = Pseudo::undef(id);
        self.func.add_pseudo(pseudo);
        id
    }
}

// ============================================================================
// Phase 1: Variable Analysis and Phi Placement
// ============================================================================

/// Information about a local variable during SSA conversion
#[derive(Default)]
struct VarInfo {
    /// Type of the variable
    typ: TypeId,
    /// Size of the type in bits
    size: u32,
    /// Blocks that store to this variable
    def_blocks: Vec<BasicBlockId>,
    /// Total number of stores
    store_count: usize,
    /// Total number of uses (loads + stores)
    use_count: usize,
    /// Is the address of this variable taken?
    addr_taken: bool,
    /// Is all usage in a single block?
    single_block: Option<BasicBlockId>,
}

/// Analyze a variable to determine if it can be promoted to SSA.
fn analyze_variable(func: &Function, types: &TypeTable, var_name: &str) -> Option<VarInfo> {
    let local = func.get_local(var_name)?;
    let sym_id = local.sym;
    let typ = local.typ;

    // Check basic promotability - only scalar types can be promoted
    // Volatile and atomic variables must go through memory
    if local.is_volatile || local.is_atomic || !types.is_scalar(typ) {
        return None;
    }

    let mut info = VarInfo {
        typ,
        size: types.size_bits(typ),
        ..Default::default()
    };

    let mut seen_block: Option<BasicBlockId> = None;
    let mut same_block = true;

    // Scan all instructions looking for uses of this variable
    for bb in &func.blocks {
        for insn in &bb.insns {
            match insn.op {
                Opcode::Store => {
                    // Store: check if storing to this variable
                    if !insn.src.is_empty() && insn.src[0] == sym_id {
                        info.store_count += 1;
                        info.use_count += 1;

                        if !info.def_blocks.contains(&bb.id) {
                            info.def_blocks.push(bb.id);
                        }

                        if same_block {
                            if let Some(prev) = seen_block {
                                if prev != bb.id {
                                    same_block = false;
                                }
                            } else {
                                seen_block = Some(bb.id);
                            }
                        }
                    }
                }
                Opcode::Load => {
                    // Load: check if loading from this variable
                    if !insn.src.is_empty() && insn.src[0] == sym_id {
                        info.use_count += 1;

                        if same_block {
                            if let Some(prev) = seen_block {
                                if prev != bb.id {
                                    same_block = false;
                                }
                            } else {
                                seen_block = Some(bb.id);
                            }
                        }
                    }
                }
                Opcode::SymAddr => {
                    // Address taken - can't promote
                    // SymAddr has the symbol in src[0], not target
                    if !insn.src.is_empty() && insn.src[0] == sym_id {
                        info.addr_taken = true;
                    }
                }
                _ => {}
            }
        }
    }

    if info.addr_taken {
        return None;
    }

    if same_block {
        info.single_block = seen_block;
    }

    Some(info)
}

/// Insert phi nodes for a variable at its iterated dominance frontier.
fn insert_phi_nodes(converter: &mut SsaConverter, var_name: &str, var_info: &VarInfo) {
    // Compute IDF of definition blocks
    let idf = idf_compute(converter.func, &var_info.def_blocks);

    // Insert phi node at each IDF block.
    for bb_id in idf {
        let target = converter.alloc_phi();

        // Create phi instruction
        let phi = Instruction::phi(target, var_info.typ, var_info.size);

        // Store the variable name for later phi renaming
        // We'll use the phi_list field to store this temporarily
        // (the actual phi sources will be added during renaming)

        // Insert at beginning of block (after any entry instruction and existing phis)
        if let Some(bb) = converter.func.get_block_mut(bb_id) {
            // Find insertion point: after Entry (if present) and after existing phis
            let mut insert_pos = 0;
            for insn in &bb.insns {
                if insn.op == Opcode::Entry || insn.op == Opcode::Phi {
                    insert_pos += 1;
                } else {
                    break;
                }
            }

            bb.insns.insert(insert_pos, phi);

            // Record for later processing
            converter.all_phis.push((bb_id, insert_pos));

            // Store in phi_map (position is stable because we insert at the end of existing phis)
            bb.phi_map.insert(var_name.to_string(), insert_pos);
        }
    }
}

// ============================================================================
// Phase 2: Variable Renaming
// ============================================================================

/// Definition stack for variable renaming
struct DefStack {
    /// Variable name -> stack of (defining block, defining pseudo)
    stacks: HashMap<String, Vec<(BasicBlockId, PseudoId)>>,
}

impl DefStack {
    fn new() -> Self {
        Self {
            stacks: HashMap::with_capacity(DEFAULT_SSA_RENAME_CAPACITY),
        }
    }

    /// Push a new definition
    fn push(&mut self, var: &str, bb: BasicBlockId, val: PseudoId) {
        self.stacks
            .entry(var.to_string())
            .or_default()
            .push((bb, val));
    }

    /// Get current definition (from top of stack)
    fn current(&self, var: &str) -> Option<PseudoId> {
        self.stacks.get(var).and_then(|s| s.last().map(|(_, v)| *v))
    }

    /// Pop definitions made in a specific block
    fn pop_block(&mut self, bb: BasicBlockId) {
        for stack in self.stacks.values_mut() {
            while stack.last().map(|(b, _)| *b == bb).unwrap_or(false) {
                stack.pop();
            }
        }
    }
}

/// Lookup the current definition of a variable, walking up the dominator tree.
fn lookup_var(
    func: &Function,
    bb_id: BasicBlockId,
    var: &str,
    def_stack: &DefStack,
) -> Option<PseudoId> {
    // First check if there's a definition from processing this block
    if let Some(val) = def_stack.current(var) {
        return Some(val);
    }

    // Walk up dominator tree looking for a phi or store
    let mut current = bb_id;
    while let Some(bb) = func.get_block(current) {
        // Check if there's a phi for this variable in this block
        if let Some(&phi_idx) = bb.phi_map.get(var) {
            if let Some(phi_insn) = bb.insns.get(phi_idx) {
                return phi_insn.target;
            }
        }

        // Move to immediate dominator
        if let Some(idom) = bb.idom {
            current = idom;
        } else {
            break;
        }
    }

    None
}

/// Rename variables in a single instruction.
fn rename_insn(
    converter: &mut SsaConverter,
    bb_id: BasicBlockId,
    insn_idx: usize,
    def_stack: &mut DefStack,
) {
    let insn = if let Some(bb) = converter.func.get_block(bb_id) {
        bb.insns.get(insn_idx).cloned()
    } else {
        return;
    };

    let insn = match insn {
        Some(i) => i,
        None => return,
    };

    match insn.op {
        Opcode::Store => {
            // Check if this is a store to a promotable variable
            if insn.src.len() >= 2 {
                let addr = insn.src[0];

                // Look up what variable this address corresponds to
                let var_name = converter
                    .func
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .and_then(|p| match &p.kind {
                        PseudoKind::Sym(name) => Some(name.clone()),
                        _ => None,
                    });

                if let Some(name) = var_name {
                    if converter.to_rename.contains(&name) {
                        // Get the value being stored
                        let val = insn.src[1];

                        // Push as new definition
                        def_stack.push(&name, bb_id, val);

                        // Mark store for removal
                        converter.dead_stores.push(InsnRef::new(bb_id, insn_idx));
                    }
                }
            }
        }

        Opcode::Load => {
            // Check if this is a load from a promotable variable
            if !insn.src.is_empty() {
                let addr = insn.src[0];

                let var_name = converter
                    .func
                    .pseudos
                    .iter()
                    .find(|p| p.id == addr)
                    .and_then(|p| match &p.kind {
                        PseudoKind::Sym(name) => Some(name.clone()),
                        _ => None,
                    });

                if let Some(name) = var_name {
                    if converter.to_rename.contains(&name) {
                        // Get the reaching definition
                        let val = lookup_var(converter.func, bb_id, &name, def_stack)
                            .unwrap_or_else(|| converter.undef_pseudo());

                        // Replace load with the value
                        if insn.target.is_some() {
                            if let Some(bb) = converter.func.get_block_mut(bb_id) {
                                if let Some(load_insn) = bb.insns.get_mut(insn_idx) {
                                    // Convert to a copy
                                    load_insn.op = Opcode::Copy;
                                    load_insn.src = vec![val];
                                }
                            }
                        }
                    }
                }
            }
        }

        Opcode::Phi => {
            // Record phi as a definition
            if let Some(target) = insn.target {
                // Find which variable this phi is for (from phi_map)
                if let Some(bb) = converter.func.get_block(bb_id) {
                    for (name, &idx) in &bb.phi_map {
                        if idx == insn_idx {
                            def_stack.push(name, bb_id, target);
                            break;
                        }
                    }
                }
            }
        }

        _ => {}
    }
}

/// Rename variables in a block and its dominated children.
fn rename_block(converter: &mut SsaConverter, bb_id: BasicBlockId, def_stack: &mut DefStack) {
    // Get instruction count first
    let insn_count = converter
        .func
        .get_block(bb_id)
        .map(|bb| bb.insns.len())
        .unwrap_or(0);

    // Process all instructions in this block
    for i in 0..insn_count {
        rename_insn(converter, bb_id, i, def_stack);
    }

    // Get dominated children
    let dom_children: Vec<BasicBlockId> = converter
        .func
        .get_block(bb_id)
        .map(|bb| bb.dom_children.clone())
        .unwrap_or_default();

    // Recurse into dominated children
    for child in dom_children {
        rename_block(converter, child, def_stack);
    }

    // Pop definitions made in this block
    def_stack.pop_block(bb_id);
}

/// Fill in phi operands from predecessor blocks.
/// Creates PhiSource instructions in each predecessor block that feed
/// into the phi nodes, following sparse's OP_PHISOURCE design.
fn fill_phi_operands(converter: &mut SsaConverter) {
    // Collect phi info first
    let phi_info: Vec<(BasicBlockId, usize, String)> = converter
        .func
        .blocks
        .iter()
        .flat_map(|bb| {
            bb.phi_map
                .iter()
                .map(move |(name, &idx)| (bb.id, idx, name.clone()))
        })
        .collect();

    for (bb_id, phi_idx, var_name) in phi_info {
        // Get predecessor blocks
        let preds: Vec<BasicBlockId> = converter
            .func
            .get_block(bb_id)
            .map(|bb| bb.parents.clone())
            .unwrap_or_default();

        // Get phi target and type info
        let (phi_target, phi_typ, phi_size) = {
            let bb = match converter.func.get_block(bb_id) {
                Some(bb) => bb,
                None => continue,
            };
            let phi_insn = match bb.insns.get(phi_idx) {
                Some(insn) => insn,
                None => continue,
            };
            let phi_target = match phi_insn.target {
                Some(t) => t,
                None => continue,
            };
            let phi_typ = match phi_insn.typ {
                Some(t) => t,
                None => continue,
            };
            (phi_target, phi_typ, phi_insn.size)
        };

        // For each predecessor, find the reaching definition and create PhiSource
        for pred_id in preds {
            let val = lookup_var_in_pred(converter.func, pred_id, &var_name).unwrap_or_else(|| {
                let id = PseudoId(converter.next_pseudo_id);
                converter.next_pseudo_id += 1;
                let pseudo = Pseudo::undef(id);
                converter.func.add_pseudo(pseudo);
                id
            });

            // Allocate PhiSource target pseudo
            let phisrc_pseudo = converter.alloc_phi();

            // Create PhiSource instruction with back-pointer to owning phi
            let mut phisrc = Instruction::phi_source(phisrc_pseudo, val, phi_typ, phi_size);
            phisrc.phi_list = vec![(bb_id, phi_target)];

            // Insert into predecessor block before terminator
            if let Some(pred_bb) = converter.func.get_block_mut(pred_id) {
                pred_bb.insert_before_terminator(phisrc);
            }

            // Add PhiSource target to phi's phi_list
            if let Some(bb) = converter.func.get_block_mut(bb_id) {
                if let Some(phi_insn) = bb.insns.get_mut(phi_idx) {
                    phi_insn.phi_list.push((pred_id, phisrc_pseudo));
                }
            }
        }
    }
}

/// Find the reaching definition of a variable in a predecessor block.
///
/// For phi operand filling, we need to find the LAST definition of a variable
/// that reaches a predecessor block. This is tricky for back-edges in loops:
///
/// In a loop like:
///   .L1: phi %x = [.L0: %init], [.L2: ???]
///        ... use %x ...
///        %new = %x + 1
///        store %x, %new
///        br .L2
///   .L2: cbr .L1, .L3
///
/// When filling the phi operand from .L2, we need %new (the stored value),
/// not the phi target. So we check stores BEFORE checking phis.
fn lookup_var_in_pred(func: &Function, bb_id: BasicBlockId, var: &str) -> Option<PseudoId> {
    // Walk up the dominator tree from this predecessor looking for a definition
    let mut current = bb_id;

    loop {
        let bb = func.get_block(current)?;

        // Check for store in this block (walking backwards) FIRST
        // This is important for back-edges: the store happens after the phi
        for insn in bb.insns.iter().rev() {
            if insn.op == Opcode::Store && insn.src.len() >= 2 {
                let addr = insn.src[0];

                // Check if this is a store to our variable
                if let Some(pseudo) = func.get_pseudo(addr) {
                    if let PseudoKind::Sym(name) = &pseudo.kind {
                        if name == var {
                            return Some(insn.src[1]);
                        }
                    }
                }
            }
        }

        // Check for phi in this block (only if no store found)
        if let Some(&phi_idx) = bb.phi_map.get(var) {
            if let Some(phi_insn) = bb.insns.get(phi_idx) {
                return phi_insn.target;
            }
        }

        // Move to immediate dominator
        current = bb.idom?;
    }
}

// ============================================================================
// Phase 3: Cleanup
// ============================================================================

/// Remove dead stores that were converted to SSA.
fn remove_dead_stores(func: &mut Function, dead_stores: &[InsnRef]) {
    // Sort by block and index (descending) so we can remove from end first
    let mut sorted: Vec<_> = dead_stores.to_vec();
    sorted.sort_by(|a, b| {
        if a.bb == b.bb {
            b.idx.cmp(&a.idx) // Descending by index
        } else {
            a.bb.0.cmp(&b.bb.0)
        }
    });

    for insn_ref in sorted {
        if let Some(bb) = func.get_block_mut(insn_ref.bb) {
            if insn_ref.idx < bb.insns.len() {
                // Convert to Nop instead of removing to preserve indices
                bb.insns[insn_ref.idx].op = Opcode::Nop;
            }
        }
    }
}

// ============================================================================
// Main Entry Point
// ============================================================================

/// Convert a function to SSA form.
///
/// This promotes eligible local variables from memory to SSA registers,
/// inserting phi nodes at control flow merge points.
///
/// # Algorithm
/// 1. Build dominator tree
/// 2. For each promotable local variable:
///    - Compute its iterated dominance frontier (IDF)
///    - Insert phi nodes at IDF blocks
/// 3. Rename variables:
///    - Walk blocks in dominator tree order
///    - Replace loads with reaching definitions
///    - Record stores as new definitions
///    - Fill in phi operands from predecessors
/// 4. Remove dead stores
pub fn ssa_convert(func: &mut Function, types: &TypeTable) {
    if func.blocks.is_empty() {
        return;
    }

    // Phase 0: Build dominator tree
    domtree_build(func);
    compute_dominance_frontiers(func);

    let mut converter = SsaConverter::new(func);

    // Phase 1: Analyze variables and insert phi nodes
    let local_names: Vec<String> = converter.func.locals.keys().cloned().collect();

    for var_name in &local_names {
        if let Some(var_info) = analyze_variable(converter.func, types, var_name) {
            // Skip if all usage is in a single block (no phi needed)
            if var_info.single_block.is_some() {
                // Could do local rewriting here but skip for now
                continue;
            }

            // Skip if no stores
            if var_info.store_count == 0 {
                continue;
            }

            // Mark for renaming
            converter.to_rename.insert(var_name.clone());

            // Insert phi nodes at IDF
            insert_phi_nodes(&mut converter, var_name, &var_info);
        }
    }

    // Phase 2: Rename variables
    let mut def_stack = DefStack::new();
    let entry = converter.func.entry;
    rename_block(&mut converter, entry, &mut def_stack);

    // Fill in phi operands
    fill_phi_operands(&mut converter);

    // Phase 3: Remove dead stores
    let dead_stores = std::mem::take(&mut converter.dead_stores);
    remove_dead_stores(converter.func, &dead_stores);

    // Update function's next_pseudo to avoid ID collisions with later allocations
    converter.func.next_pseudo = converter.next_pseudo_id;
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::BasicBlock;
    use crate::target::Target;

    fn make_simple_if_cfg(types: &TypeTable) -> Function {
        // Create a CFG with a simple if-then-else:
        //
        // int x = 1;
        // if (cond) x = 2;
        // return x;
        //
        //       entry(0)
        //       /     \
        //      v       v
        //   then(1)  else(2)
        //       \     /
        //        v   v
        //       merge(3)

        let int_id = types.int_id;
        let mut func = Function::new("test", int_id);

        // Create symbol pseudo for local variable 'x'
        let x_sym = PseudoId(0);
        func.add_pseudo(Pseudo::sym(x_sym, "x".to_string()));
        // Variable x is declared in entry block (BasicBlockId(0))
        func.add_local(
            "x",
            x_sym,
            int_id,
            false, // not volatile
            false, // not atomic
            Some(BasicBlockId(0)),
            None, // no explicit alignment
        );

        // Value pseudos
        let val1 = PseudoId(1);
        func.add_pseudo(Pseudo::val(val1, 1));
        let val2 = PseudoId(2);
        func.add_pseudo(Pseudo::val(val2, 2));
        let cond = PseudoId(3);
        func.add_pseudo(Pseudo::val(cond, 1)); // Non-zero condition

        // Entry block: x = 1; if (cond) goto then else goto else
        let mut entry = BasicBlock::new(BasicBlockId(0));
        entry.children = vec![BasicBlockId(1), BasicBlockId(2)];
        entry.add_insn(Instruction::new(Opcode::Entry));
        // Store x = 1
        entry.add_insn(Instruction::store(val1, x_sym, 0, int_id, 32));
        // Conditional branch
        entry.add_insn(Instruction::cbr(cond, BasicBlockId(1), BasicBlockId(2)));

        // Then block: x = 2
        let mut then_bb = BasicBlock::new(BasicBlockId(1));
        then_bb.parents = vec![BasicBlockId(0)];
        then_bb.children = vec![BasicBlockId(3)];
        then_bb.add_insn(Instruction::store(val2, x_sym, 0, int_id, 32));
        then_bb.add_insn(Instruction::br(BasicBlockId(3)));

        // Else block: (no assignment)
        let mut else_bb = BasicBlock::new(BasicBlockId(2));
        else_bb.parents = vec![BasicBlockId(0)];
        else_bb.children = vec![BasicBlockId(3)];
        else_bb.add_insn(Instruction::br(BasicBlockId(3)));

        // Merge block: return x
        let mut merge = BasicBlock::new(BasicBlockId(3));
        merge.parents = vec![BasicBlockId(1), BasicBlockId(2)];
        let result = PseudoId(4);
        func.add_pseudo(Pseudo::reg(result, 0));
        // Load x
        merge.add_insn(Instruction::load(result, x_sym, 0, int_id, 32));
        merge.add_insn(Instruction::ret(Some(result)));

        func.entry = BasicBlockId(0);
        func.blocks = vec![entry, then_bb, else_bb, merge];
        func.rebuild_block_idx();
        func
    }

    #[test]
    fn test_analyze_variable() {
        let types = TypeTable::new(&Target::host());
        let func = make_simple_if_cfg(&types);

        let info = analyze_variable(&func, &types, "x").unwrap();
        assert_eq!(info.store_count, 2); // One in entry, one in then
        assert_eq!(info.def_blocks.len(), 2);
        assert!(!info.addr_taken);
    }

    #[test]
    fn test_ssa_convert_creates_phi() {
        let types = TypeTable::new(&Target::host());
        let mut func = make_simple_if_cfg(&types);
        ssa_convert(&mut func, &types);

        // After SSA conversion, merge block should have a phi node
        let merge = func.get_block(BasicBlockId(3)).unwrap();

        let has_phi = merge.insns.iter().any(|i| i.op == Opcode::Phi);
        assert!(has_phi, "Merge block should have a phi node");
    }

    #[test]
    fn test_domtree_built() {
        let types = TypeTable::new(&Target::host());
        let mut func = make_simple_if_cfg(&types);
        domtree_build(&mut func);

        // Entry should be the root (no idom)
        let entry = func.get_block(BasicBlockId(0)).unwrap();
        assert!(entry.idom.is_none());

        // Then, else, and merge should all have entry as idom
        let then_bb = func.get_block(BasicBlockId(1)).unwrap();
        assert_eq!(then_bb.idom, Some(BasicBlockId(0)));

        let else_bb = func.get_block(BasicBlockId(2)).unwrap();
        assert_eq!(else_bb.idom, Some(BasicBlockId(0)));

        let merge = func.get_block(BasicBlockId(3)).unwrap();
        assert_eq!(merge.idom, Some(BasicBlockId(0)));
    }

    // ========================================================================
    // Bug fix regression test: pseudo ID tracking from instructions
    // Verifies that SSA conversion correctly finds max pseudo ID from
    // instruction operands, not just func.pseudos
    // ========================================================================

    #[test]
    fn test_max_pseudo_id_from_instructions() {
        // Regression test: pseudo IDs allocated but not in func.pseudos are tracked
        // This tests the fix in SsaConverter::new() that scans instruction operands
        let types = TypeTable::new(&Target::host());
        let int_id = types.int_id;
        let mut func = Function::new("test", int_id);

        // Only add ONE pseudo to func.pseudos with ID 0
        let x_sym = PseudoId(0);
        func.add_pseudo(Pseudo::sym(x_sym, "x".to_string()));
        func.add_local(
            "x",
            x_sym,
            int_id,
            false,
            false,
            Some(BasicBlockId(0)),
            None,
        );

        // Create an instruction that uses a HIGHER pseudo ID (say, 100)
        // that is NOT in func.pseudos. This simulates what the linearizer does
        // when it allocates pseudos via alloc_pseudo() without adding to func.pseudos
        let high_id = PseudoId(100);
        // Note: we intentionally DON'T add this to func.pseudos

        // Build minimal CFG
        let mut entry = BasicBlock::new(BasicBlockId(0));
        entry.add_insn(Instruction::new(Opcode::Entry));
        // Store using the high pseudo ID as source (simulating linearizer output)
        entry.add_insn(Instruction::store(high_id, x_sym, 0, int_id, 32));
        entry.add_insn(Instruction::ret(None));

        func.entry = BasicBlockId(0);
        func.blocks = vec![entry];
        func.rebuild_block_idx();

        // This should NOT panic - the SSA converter should find max ID from instructions
        ssa_convert(&mut func, &types);

        // The converter should have found PseudoId(100) from scanning instructions
        // and used next_pseudo_id >= 101 for any new IDs
        // (We can't easily verify the internal counter, but the lack of panic
        // indicates the fix is working)
    }

    // ========================================================================
    // Regression test: phi insertion in goto-dispatch CFG
    //
    // The bug: insert_phi_nodes() had a filter that skipped phi node insertion
    // at IDF blocks not dominated by the variable's declaration block. This
    // was too aggressive for function-scope variables used in goto-dispatch
    // patterns (like CPython's ceval.c), where a variable declared at function
    // scope is modified in case handlers and read back at a dispatch label
    // connected by goto back-edges.
    //
    // The fix: remove the decl_block dominance filter from insert_phi_nodes().
    // ========================================================================

    fn make_goto_dispatch_cfg(types: &TypeTable) -> Function {
        // Create a CFG mimicking CPython's ceval.c goto-dispatch pattern:
        //
        //   int x = 0;
        //   int opcode = get_opcode(); // simulated
        //   goto dispatch;
        //
        // dispatch:
        //   switch(opcode) {
        //     case 0: goto handler0;
        //     case 1: goto handler1;
        //     default: goto done;
        //   }
        //
        // handler0:
        //   x = 10;
        //   opcode = 1; // next opcode
        //   goto dispatch;
        //
        // handler1:
        //   x = 20;
        //   goto done;
        //
        // done:
        //   return x;
        //
        //       entry(0)
        //         |
        //         v
        //  +-> dispatch(1) --+--+
        //  |    |    |       |  |
        //  |    v    |       |  v
        //  | handler0(2)     | handler1(3)
        //  |    |            |  |
        //  +----+            |  v
        //                    +-> done(4)

        let int_id = types.int_id;
        let mut func = Function::new("test_dispatch", int_id);

        // Symbol pseudo for local variable 'x' (declared at function scope = entry block)
        let x_sym = PseudoId(0);
        func.add_pseudo(Pseudo::sym(x_sym, "x".to_string()));
        func.add_local(
            "x",
            x_sym,
            int_id,
            false,
            false,
            Some(BasicBlockId(0)), // declared in entry
            None,
        );

        // Value pseudos
        let val0 = PseudoId(1);
        func.add_pseudo(Pseudo::val(val0, 0));
        let val10 = PseudoId(2);
        func.add_pseudo(Pseudo::val(val10, 10));
        let val20 = PseudoId(3);
        func.add_pseudo(Pseudo::val(val20, 20));
        let val1 = PseudoId(4);
        func.add_pseudo(Pseudo::val(val1, 1));

        // Pseudo for switch value (simulated opcode)
        let opcode_sym = PseudoId(5);
        func.add_pseudo(Pseudo::sym(opcode_sym, "opcode".to_string()));
        func.add_local(
            "opcode",
            opcode_sym,
            int_id,
            false,
            false,
            Some(BasicBlockId(0)),
            None,
        );

        // Reg pseudos for loads
        let load_result = PseudoId(6);
        func.add_pseudo(Pseudo::reg(load_result, 0));
        let load_x_done = PseudoId(7);
        func.add_pseudo(Pseudo::reg(load_x_done, 1));

        // entry(0): x = 0; opcode = 0; goto dispatch
        let mut entry = BasicBlock::new(BasicBlockId(0));
        entry.children = vec![BasicBlockId(1)];
        entry.add_insn(Instruction::new(Opcode::Entry));
        entry.add_insn(Instruction::store(val0, x_sym, 0, int_id, 32));
        entry.add_insn(Instruction::store(val0, opcode_sym, 0, int_id, 32));
        entry.add_insn(Instruction::br(BasicBlockId(1)));

        // dispatch(1): load opcode; switch(opcode) { 0->handler0, 1->handler1, default->done }
        let mut dispatch = BasicBlock::new(BasicBlockId(1));
        dispatch.parents = vec![BasicBlockId(0), BasicBlockId(2)]; // entry + handler0 back-edge
        dispatch.children = vec![BasicBlockId(2), BasicBlockId(3), BasicBlockId(4)];
        dispatch.add_insn(Instruction::load(load_result, opcode_sym, 0, int_id, 32));
        dispatch.add_insn(Instruction::switch_insn(
            load_result,
            vec![
                (0, BasicBlockId(2)), // case 0 -> handler0
                (1, BasicBlockId(3)), // case 1 -> handler1
            ],
            Some(BasicBlockId(4)), // default -> done
            32,
        ));

        // handler0(2): x = 10; opcode = 1; goto dispatch (back-edge)
        let mut handler0 = BasicBlock::new(BasicBlockId(2));
        handler0.parents = vec![BasicBlockId(1)];
        handler0.children = vec![BasicBlockId(1)]; // back-edge to dispatch
        handler0.add_insn(Instruction::store(val10, x_sym, 0, int_id, 32));
        handler0.add_insn(Instruction::store(val1, opcode_sym, 0, int_id, 32));
        handler0.add_insn(Instruction::br(BasicBlockId(1)));

        // handler1(3): x = 20; goto done
        let mut handler1 = BasicBlock::new(BasicBlockId(3));
        handler1.parents = vec![BasicBlockId(1)];
        handler1.children = vec![BasicBlockId(4)];
        handler1.add_insn(Instruction::store(val20, x_sym, 0, int_id, 32));
        handler1.add_insn(Instruction::br(BasicBlockId(4)));

        // done(4): return x
        let mut done = BasicBlock::new(BasicBlockId(4));
        done.parents = vec![BasicBlockId(1), BasicBlockId(3)];
        done.add_insn(Instruction::load(load_x_done, x_sym, 0, int_id, 32));
        done.add_insn(Instruction::ret(Some(load_x_done)));

        func.entry = BasicBlockId(0);
        func.blocks = vec![entry, dispatch, handler0, handler1, done];
        func.rebuild_block_idx();
        func
    }

    #[test]
    fn test_goto_dispatch_phi_insertion() {
        // Regression test for the phi insertion bug.
        // In a goto-dispatch CFG, variable 'x' is:
        //   - defined in entry(0), handler0(2), handler1(3)
        //   - read in done(4)
        //   - the dispatch(1) block is a merge point on the back-edge from handler0
        //
        // The IDF of def_blocks {0, 2, 3} should include dispatch(1) and done(4).
        // The old buggy code would skip dispatch(1) if it wasn't dominated by decl_block.
        let types = TypeTable::new(&Target::host());
        let mut func = make_goto_dispatch_cfg(&types);

        ssa_convert(&mut func, &types);

        // dispatch(1) must have a phi for 'x' because it's a merge point
        // where the value of x from entry (x=0) meets the value from handler0 (x=10)
        let dispatch = func.get_block(BasicBlockId(1)).unwrap();
        let dispatch_has_phi_x = dispatch.phi_map.contains_key("x");
        assert!(
            dispatch_has_phi_x,
            "dispatch block (bb1) must have a phi node for 'x' - \
             this is the goto-dispatch pattern regression"
        );

        // done(4) should also have a phi for 'x' because it merges values
        // from dispatch(1) (default case) and handler1(3)
        let done = func.get_block(BasicBlockId(4)).unwrap();
        let done_has_phi_x = done.phi_map.contains_key("x");
        assert!(
            done_has_phi_x,
            "done block (bb4) must have a phi node for 'x'"
        );

        // Verify the phi in dispatch has the correct predecessors (entry and handler0)
        let phi_idx = dispatch.phi_map["x"];
        let phi_insn = &dispatch.insns[phi_idx];
        assert_eq!(phi_insn.op, Opcode::Phi);
        assert_eq!(
            phi_insn.phi_list.len(),
            2,
            "dispatch phi for 'x' should have 2 operands (from entry and handler0)"
        );

        // Check predecessor block IDs in the phi_list
        let phi_preds: Vec<BasicBlockId> = phi_insn.phi_list.iter().map(|(bb, _)| *bb).collect();
        assert!(
            phi_preds.contains(&BasicBlockId(0)),
            "dispatch phi should have entry(0) as predecessor"
        );
        assert!(
            phi_preds.contains(&BasicBlockId(2)),
            "dispatch phi should have handler0(2) as predecessor"
        );
    }

    #[test]
    fn test_goto_dispatch_loads_replaced() {
        // Verify that after SSA conversion, loads of 'x' are replaced
        // with SSA values (Copy instructions), not left as memory loads.
        let types = TypeTable::new(&Target::host());
        let mut func = make_goto_dispatch_cfg(&types);

        ssa_convert(&mut func, &types);

        // The load of x in done(4) should have been converted to a Copy
        let done = func.get_block(BasicBlockId(4)).unwrap();
        let has_load = done.insns.iter().any(|i| i.op == Opcode::Load);
        assert!(
            !has_load,
            "done block should not have any Load instructions after SSA conversion \
             (they should be converted to Copy)"
        );

        let has_copy = done.insns.iter().any(|i| i.op == Opcode::Copy);
        assert!(
            has_copy,
            "done block should have a Copy instruction (the promoted load of x)"
        );
    }

    // ========================================================================
    // PhiSource tests: verify SSA conversion emits PhiSource instructions
    // ========================================================================

    #[test]
    fn test_phisource_in_predecessors() {
        // After SSA conversion, predecessor blocks should have PhiSource instructions
        let types = TypeTable::new(&Target::host());
        let mut func = make_simple_if_cfg(&types);

        ssa_convert(&mut func, &types);

        // merge(3) has phi for 'x' with predecessors then(1) and else(2)
        let merge = func.get_block(BasicBlockId(3)).unwrap();
        assert!(merge.phi_map.contains_key("x"));
        let phi_idx = merge.phi_map["x"];
        let phi_insn = &merge.insns[phi_idx];
        assert_eq!(phi_insn.op, Opcode::Phi);

        // Check that predecessor blocks have PhiSource instructions
        let then_bb = func.get_block(BasicBlockId(1)).unwrap();
        let then_has_phisrc = then_bb.insns.iter().any(|i| i.op == Opcode::PhiSource);
        assert!(
            then_has_phisrc,
            "then block should have PhiSource instruction"
        );

        // else(2) should also have PhiSource (for the reaching def from entry)
        let else_bb = func.get_block(BasicBlockId(2)).unwrap();
        let else_has_phisrc = else_bb.insns.iter().any(|i| i.op == Opcode::PhiSource);
        assert!(
            else_has_phisrc,
            "else block should have PhiSource instruction"
        );
    }

    #[test]
    fn test_phisource_has_backpointer() {
        // Each PhiSource should have a back-pointer to its owning Phi
        let types = TypeTable::new(&Target::host());
        let mut func = make_simple_if_cfg(&types);

        ssa_convert(&mut func, &types);

        let merge = func.get_block(BasicBlockId(3)).unwrap();
        let phi_idx = merge.phi_map["x"];
        let phi_target = merge.insns[phi_idx].target.unwrap();

        // Find PhiSource instructions in predecessor blocks
        for bb in &func.blocks {
            for insn in &bb.insns {
                if insn.op == Opcode::PhiSource {
                    // PhiSource must have a back-pointer in phi_list
                    assert!(
                        !insn.phi_list.is_empty(),
                        "PhiSource must have a back-pointer"
                    );
                    let (phi_bb, phi_pseudo) = insn.phi_list[0];
                    // Back-pointer should reference the merge block's phi
                    assert_eq!(phi_bb, BasicBlockId(3));
                    assert_eq!(phi_pseudo, phi_target);
                }
            }
        }
    }

    #[test]
    fn test_phisource_targets_match_phi_list() {
        // The phi_list entries on the Phi should reference PhiSource targets
        let types = TypeTable::new(&Target::host());
        let mut func = make_goto_dispatch_cfg(&types);

        ssa_convert(&mut func, &types);

        let dispatch = func.get_block(BasicBlockId(1)).unwrap();
        if let Some(&phi_idx) = dispatch.phi_map.get("x") {
            let phi_insn = &dispatch.insns[phi_idx];

            for (pred_bb, phisrc_pseudo) in &phi_insn.phi_list {
                // Find the PhiSource instruction in the predecessor block
                let pred = func.get_block(*pred_bb).unwrap();
                let phisrc = pred
                    .insns
                    .iter()
                    .find(|i| i.op == Opcode::PhiSource && i.target == Some(*phisrc_pseudo));
                assert!(
                    phisrc.is_some(),
                    "PhiSource for pseudo {:?} not found in predecessor block {:?}",
                    phisrc_pseudo,
                    pred_bb
                );
            }
        }
    }

    #[test]
    fn test_goto_dispatch_phisource_instructions() {
        // Verify PhiSource instructions exist in predecessor blocks of the dispatch phi
        let types = TypeTable::new(&Target::host());
        let mut func = make_goto_dispatch_cfg(&types);

        ssa_convert(&mut func, &types);

        // entry(0) should have PhiSource before its terminator
        let entry = func.get_block(BasicBlockId(0)).unwrap();
        let entry_phisrcs: Vec<_> = entry
            .insns
            .iter()
            .filter(|i| i.op == Opcode::PhiSource)
            .collect();
        assert!(
            !entry_phisrcs.is_empty(),
            "entry block should have PhiSource instructions"
        );

        // handler0(2) should have PhiSource for the back-edge
        let handler0 = func.get_block(BasicBlockId(2)).unwrap();
        let handler0_phisrcs: Vec<_> = handler0
            .insns
            .iter()
            .filter(|i| i.op == Opcode::PhiSource)
            .collect();
        assert!(
            !handler0_phisrcs.is_empty(),
            "handler0 block should have PhiSource instructions"
        );
    }
}
