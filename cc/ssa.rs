//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// SSA Conversion for pcc C99 compiler
//
// Based on sparse's ssa.c:
// - Converts memory-based local variables to SSA form
// - Inserts phi nodes at dominance frontiers
// - Renames variables to complete SSA construction
//

use crate::dominate::{compute_dominance_frontiers, domtree_build, idf_compute};
use crate::ir::{
    BasicBlockId, Function, InsnRef, Instruction, Opcode, Pseudo, PseudoId, PseudoKind,
};
use crate::types::{TypeId, TypeTable};
use std::collections::{HashMap, HashSet};

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
        let max_pseudo_id = func.pseudos.iter().map(|p| p.id.0).max().unwrap_or(0);

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
            to_rename: HashSet::new(),
            all_phis: Vec::new(),
            dead_stores: Vec::new(),
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
    /// Block where this variable was declared (for scope-aware phi placement)
    decl_block: Option<BasicBlockId>,
}

/// Analyze a variable to determine if it can be promoted to SSA.
fn analyze_variable(func: &Function, types: &TypeTable, var_name: &str) -> Option<VarInfo> {
    let local = func.get_local(var_name)?;
    let sym_id = local.sym;
    let typ = local.typ;
    let decl_block = local.decl_block;

    // Check basic promotability - only scalar types can be promoted
    if local.is_volatile || !types.is_scalar(typ) {
        return None;
    }

    let mut info = VarInfo {
        typ,
        size: types.size_bits(typ),
        decl_block,
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

    // Insert phi node at each IDF block, but only if it's dominated by the declaration block.
    // This ensures that variables declared inside inner scopes don't get phi nodes
    // at outer loop headers where they're not in scope.
    for bb_id in idf {
        // Filter: only insert phi if the IDF block is dominated by the declaration block
        if let Some(decl_bb) = var_info.decl_block {
            if !converter.func.dominates(decl_bb, bb_id) {
                // This IDF block is not dominated by the declaration block,
                // so the variable is not in scope here - skip phi insertion
                continue;
            }
        }
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
            stacks: HashMap::new(),
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

        // For each predecessor, find the reaching definition
        for pred_id in preds {
            // Find the definition in the predecessor
            let val = lookup_var_in_pred(converter.func, pred_id, &var_name).unwrap_or_else(|| {
                // Create undef pseudo
                let id = PseudoId(converter.next_pseudo_id);
                converter.next_pseudo_id += 1;
                let pseudo = Pseudo::undef(id);
                converter.func.add_pseudo(pseudo);
                id
            });

            // Add to phi_list
            if let Some(bb) = converter.func.get_block_mut(bb_id) {
                if let Some(phi_insn) = bb.insns.get_mut(phi_idx) {
                    phi_insn.phi_list.push((pred_id, val));
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
                if let Some(pseudo) = func.pseudos.iter().find(|p| p.id == addr) {
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
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::BasicBlock;

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
            Some(BasicBlockId(0)),
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
        func
    }

    #[test]
    fn test_analyze_variable() {
        let types = TypeTable::new();
        let func = make_simple_if_cfg(&types);

        let info = analyze_variable(&func, &types, "x").unwrap();
        assert_eq!(info.store_count, 2); // One in entry, one in then
        assert_eq!(info.def_blocks.len(), 2);
        assert!(!info.addr_taken);
    }

    #[test]
    fn test_ssa_convert_creates_phi() {
        let types = TypeTable::new();
        let mut func = make_simple_if_cfg(&types);
        ssa_convert(&mut func, &types);

        // After SSA conversion, merge block should have a phi node
        let merge = func.get_block(BasicBlockId(3)).unwrap();

        let has_phi = merge.insns.iter().any(|i| i.op == Opcode::Phi);
        assert!(has_phi, "Merge block should have a phi node");
    }

    #[test]
    fn test_domtree_built() {
        let types = TypeTable::new();
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
}
