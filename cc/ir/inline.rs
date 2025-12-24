//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Function inlining pass for pcc C99 compiler
//
// This pass inlines small functions at their call sites. The inlining
// is performed at the IR level so that subsequent optimization passes
// (InstCombine, DCE) can work on the inlined code.
//
// The pass:
// 1. Analyzes all functions for inlineability
// 2. Identifies call sites eligible for inlining
// 3. Clones callee function bodies with remapped pseudos and basic blocks
// 4. Replaces call instructions with the inlined code
// 5. Removes dead static functions that were fully inlined
//

use super::{
    BasicBlock, BasicBlockId, Function, Instruction, Module, Opcode, Pseudo, PseudoId, PseudoKind,
};
use std::collections::{HashMap, HashSet};

// ============================================================================
// Inlining Constants
// ============================================================================

/// Maximum iterations for the inlining pass (handles nested inlining)
const MAX_INLINE_ITERATIONS: usize = 3;

/// Functions with this many instructions or fewer are always inlined
const ALWAYS_INLINE_SIZE: usize = 10;

/// Functions with inline hint and this many instructions or fewer are inlined
const HINT_INLINE_SIZE: usize = 50;

/// Maximum function size to inline at -O2 or higher
const MAX_INLINE_SIZE: usize = 100;

/// Maximum growth in caller size before we stop inlining
const MAX_CALLER_GROWTH: usize = 1000;

// ============================================================================
// Inline Candidate Analysis
// ============================================================================

/// Metadata about a function's suitability for inlining
#[derive(Debug, Clone, Default)]
pub struct InlineCandidate {
    /// Estimated instruction count (for heuristics)
    pub estimated_size: usize,
    /// Whether function is marked with `inline` keyword
    pub has_inline_hint: bool,
    /// Whether function is recursive (calls itself)
    pub is_recursive: bool,
    /// Whether function uses va_args (should not inline)
    pub uses_varargs: bool,
    /// Whether function uses alloca (dynamic stack)
    pub uses_alloca: bool,
    /// Number of times this function is called in the module
    pub call_count: usize,
}

/// Analyze all functions in a module for inlineability
pub fn analyze_all_functions(module: &Module) -> HashMap<String, InlineCandidate> {
    let mut candidates = HashMap::new();

    for func in &module.functions {
        let candidate = analyze_function(func, module);
        candidates.insert(func.name.clone(), candidate);
    }

    candidates
}

/// Analyze a single function for inlineability
fn analyze_function(func: &Function, module: &Module) -> InlineCandidate {
    let mut candidate = InlineCandidate {
        has_inline_hint: func.is_inline,
        ..Default::default()
    };

    // Count instructions and check for disqualifying patterns
    for bb in &func.blocks {
        candidate.estimated_size += bb.insns.len();

        for insn in &bb.insns {
            match insn.op {
                // Varargs: cannot inline
                Opcode::VaStart | Opcode::VaArg | Opcode::VaEnd | Opcode::VaCopy => {
                    candidate.uses_varargs = true;
                }
                // Alloca: dynamic stack complicates inlining
                Opcode::Alloca => {
                    candidate.uses_alloca = true;
                }
                // Check for recursion
                Opcode::Call => {
                    if let Some(callee) = &insn.func_name {
                        if callee == &func.name {
                            candidate.is_recursive = true;
                        }
                    }
                }
                _ => {}
            }
        }
    }

    // Count call sites in module
    candidate.call_count = count_calls_to_function(module, &func.name);

    candidate
}

/// Count how many times a function is called in the module
fn count_calls_to_function(module: &Module, func_name: &str) -> usize {
    let mut count = 0;
    for func in &module.functions {
        for bb in &func.blocks {
            for insn in &bb.insns {
                if insn.op == Opcode::Call {
                    if let Some(callee) = &insn.func_name {
                        if callee == func_name {
                            count += 1;
                        }
                    }
                }
            }
        }
    }
    count
}

// ============================================================================
// Inlining Decision Heuristics
// ============================================================================

/// Determine whether to inline a function at a specific call site
fn should_inline(candidate: &InlineCandidate, opt_level: u32, caller_size: usize) -> bool {
    // Never inline if disqualifying conditions
    if candidate.uses_varargs {
        return false;
    }
    if candidate.is_recursive {
        return false;
    }
    if candidate.uses_alloca {
        return false;
    }

    // At -O0, don't inline
    if opt_level == 0 {
        return false;
    }

    // Very small functions: always inline
    if candidate.estimated_size <= ALWAYS_INLINE_SIZE {
        return true;
    }

    // Functions with inline hint: higher threshold
    if candidate.has_inline_hint && candidate.estimated_size <= HINT_INLINE_SIZE {
        return true;
    }

    // At -O2 or higher, be more aggressive
    if opt_level >= 2 {
        // Single call site: inline up to max size
        if candidate.call_count == 1 && candidate.estimated_size <= MAX_INLINE_SIZE {
            return true;
        }

        // Multiple call sites: be more conservative
        if candidate.estimated_size <= ALWAYS_INLINE_SIZE * 2 {
            return true;
        }
    }

    // Check caller growth limit
    let growth = candidate.estimated_size * candidate.call_count;
    if growth + caller_size > MAX_CALLER_GROWTH {
        return false;
    }

    false
}

// ============================================================================
// Inline Context and Remapping
// ============================================================================

/// Context for cloning a function body into caller
struct InlineContext {
    /// Map from callee PseudoId to caller PseudoId
    pseudo_map: HashMap<PseudoId, PseudoId>,
    /// Map from callee BasicBlockId to caller BasicBlockId
    bb_map: HashMap<BasicBlockId, BasicBlockId>,
    /// Next pseudo ID available in the caller
    next_pseudo_id: u32,
    /// Next basic block ID available in the caller
    next_bb_id: u32,
    /// Arguments passed at the call site (replace Arg(n) with these)
    call_args: Vec<PseudoId>,
    /// Block to jump to after inlined function returns
    return_continuation_bb: BasicBlockId,
    /// Pseudo to store return value (if any)
    return_target: Option<PseudoId>,
    /// Unique ID for this inlining (for name mangling)
    inline_id: u32,
    /// Callee function name (for name mangling)
    callee_name: String,
    /// Set of local variable names from callee (these need to be renamed)
    callee_locals: HashSet<String>,
}

/// Global counter for unique inline IDs
static mut INLINE_COUNTER: u32 = 0;

fn next_inline_id() -> u32 {
    // SAFETY: This is single-threaded compilation
    unsafe {
        let id = INLINE_COUNTER;
        INLINE_COUNTER += 1;
        id
    }
}

impl InlineContext {
    /// Create a new inline context
    fn new(
        caller: &Function,
        callee: &Function,
        call_args: Vec<PseudoId>,
        return_continuation_bb: BasicBlockId,
        return_target: Option<PseudoId>,
    ) -> Self {
        // Find max pseudo ID and BB ID in caller
        let max_pseudo = caller.pseudos.iter().map(|p| p.id.0).max().unwrap_or(0);
        let max_bb = caller.blocks.iter().map(|b| b.id.0).max().unwrap_or(0);

        // Collect callee's local variable names - these need to be renamed
        let callee_locals: HashSet<String> = callee.locals.keys().cloned().collect();

        Self {
            pseudo_map: HashMap::new(),
            bb_map: HashMap::new(),
            next_pseudo_id: max_pseudo + 1,
            next_bb_id: max_bb + 1,
            call_args,
            return_continuation_bb,
            return_target,
            inline_id: next_inline_id(),
            callee_name: callee.name.clone(),
            callee_locals,
        }
    }

    /// Allocate a new pseudo ID
    fn alloc_pseudo_id(&mut self) -> PseudoId {
        let id = PseudoId(self.next_pseudo_id);
        self.next_pseudo_id += 1;
        id
    }

    /// Allocate a new basic block ID
    fn alloc_bb_id(&mut self) -> BasicBlockId {
        let id = BasicBlockId(self.next_bb_id);
        self.next_bb_id += 1;
        id
    }

    /// Remap a basic block ID from callee to caller space
    fn remap_bb(&mut self, callee_bb: BasicBlockId) -> BasicBlockId {
        if let Some(&mapped) = self.bb_map.get(&callee_bb) {
            return mapped;
        }

        let new_id = self.alloc_bb_id();
        self.bb_map.insert(callee_bb, new_id);
        new_id
    }

    /// Remap a pseudo from callee to caller space
    fn remap_pseudo(&mut self, callee_id: PseudoId, callee_pseudos: &[Pseudo]) -> PseudoId {
        // Check if already mapped
        if let Some(&mapped) = self.pseudo_map.get(&callee_id) {
            return mapped;
        }

        // Find the pseudo in callee
        let callee_pseudo = callee_pseudos.iter().find(|p| p.id == callee_id);

        let new_id = match callee_pseudo.map(|p| &p.kind) {
            // Arguments: substitute with call site arguments
            Some(PseudoKind::Arg(n)) => {
                let arg_idx = *n as usize;
                if arg_idx < self.call_args.len() {
                    let mapped = self.call_args[arg_idx];
                    self.pseudo_map.insert(callee_id, mapped);
                    return mapped;
                } else {
                    // Missing argument - shouldn't happen in valid code
                    self.alloc_pseudo_id()
                }
            }
            // For all other kinds, allocate new ID
            _ => self.alloc_pseudo_id(),
        };

        self.pseudo_map.insert(callee_id, new_id);
        new_id
    }

    /// Create a remapped copy of a pseudo
    fn clone_pseudo(&mut self, callee_pseudo: &Pseudo, callee_pseudos: &[Pseudo]) -> Pseudo {
        let new_id = self.remap_pseudo(callee_pseudo.id, callee_pseudos);

        let new_kind = match &callee_pseudo.kind {
            PseudoKind::Arg(n) => {
                // Arguments were substituted, so this shouldn't be reached
                // But if it is, treat as void
                let arg_idx = *n as usize;
                if arg_idx < self.call_args.len() {
                    // Already mapped to call arg, return that pseudo's kind
                    PseudoKind::Void
                } else {
                    PseudoKind::Undef
                }
            }
            PseudoKind::Sym(name) => {
                // Only mangle local variable names - keep global symbols unchanged
                if self.callee_locals.contains(name) {
                    let new_name =
                        format!("{}_inline{}_{}", self.callee_name, self.inline_id, name);
                    PseudoKind::Sym(new_name)
                } else {
                    // Global symbol - keep original name
                    PseudoKind::Sym(name.clone())
                }
            }
            PseudoKind::Reg(nr) => PseudoKind::Reg(*nr),
            PseudoKind::Phi(nr) => PseudoKind::Phi(*nr),
            PseudoKind::Val(v) => PseudoKind::Val(*v),
            PseudoKind::FVal(v) => PseudoKind::FVal(*v),
            PseudoKind::Void => PseudoKind::Void,
            PseudoKind::Undef => PseudoKind::Undef,
        };

        let new_name = callee_pseudo
            .name
            .as_ref()
            .map(|n| format!("{}_inline{}_{}", self.callee_name, self.inline_id, n));

        Pseudo {
            id: new_id,
            kind: new_kind,
            name: new_name,
        }
    }
}

// ============================================================================
// Instruction Cloning
// ============================================================================

/// Clone an instruction with remapped pseudos and basic blocks
fn clone_instruction(
    ctx: &mut InlineContext,
    insn: &Instruction,
    callee_pseudos: &[Pseudo],
) -> Vec<Instruction> {
    match insn.op {
        // Entry instruction: skip (caller already has its own)
        Opcode::Entry => vec![],

        // Return instruction: convert to copy + branch to continuation
        Opcode::Ret => {
            let mut result = Vec::new();

            // If returning a value, copy to return target
            if let Some(ret_val) = insn.src.first() {
                if let Some(target) = ctx.return_target {
                    let remapped_val = ctx.remap_pseudo(*ret_val, callee_pseudos);
                    let mut copy_insn = Instruction::new(Opcode::Copy);
                    copy_insn.target = Some(target);
                    copy_insn.src = vec![remapped_val];
                    copy_insn.typ = insn.typ;
                    copy_insn.size = insn.size;
                    result.push(copy_insn);
                }
            }

            // Branch to continuation
            let mut br_insn = Instruction::new(Opcode::Br);
            br_insn.bb_true = Some(ctx.return_continuation_bb);
            result.push(br_insn);

            result
        }

        // Branch instruction: remap target
        Opcode::Br => {
            let mut new_insn = Instruction::new(Opcode::Br);
            new_insn.bb_true = insn.bb_true.map(|bb| ctx.remap_bb(bb));
            vec![new_insn]
        }

        // Conditional branch: remap both targets
        Opcode::Cbr => {
            let mut new_insn = Instruction::new(Opcode::Cbr);
            new_insn.src = insn
                .src
                .iter()
                .map(|s| ctx.remap_pseudo(*s, callee_pseudos))
                .collect();
            new_insn.bb_true = insn.bb_true.map(|bb| ctx.remap_bb(bb));
            new_insn.bb_false = insn.bb_false.map(|bb| ctx.remap_bb(bb));
            vec![new_insn]
        }

        // Switch: remap all targets
        Opcode::Switch => {
            let mut new_insn = Instruction::new(Opcode::Switch);
            new_insn.target = insn.target.map(|t| ctx.remap_pseudo(t, callee_pseudos));
            new_insn.switch_cases = insn
                .switch_cases
                .iter()
                .map(|(val, bb)| (*val, ctx.remap_bb(*bb)))
                .collect();
            new_insn.switch_default = insn.switch_default.map(|bb| ctx.remap_bb(bb));
            vec![new_insn]
        }

        // Phi nodes: remap all sources and block references
        Opcode::Phi => {
            let mut new_insn = Instruction::new(Opcode::Phi);
            new_insn.target = insn.target.map(|t| ctx.remap_pseudo(t, callee_pseudos));
            new_insn.typ = insn.typ;
            new_insn.size = insn.size;
            new_insn.phi_list = insn
                .phi_list
                .iter()
                .map(|(bb, pseudo)| (ctx.remap_bb(*bb), ctx.remap_pseudo(*pseudo, callee_pseudos)))
                .collect();
            vec![new_insn]
        }

        // Call instructions: remap arguments but keep function name
        Opcode::Call => {
            let mut new_insn = insn.clone();
            new_insn.target = insn.target.map(|t| ctx.remap_pseudo(t, callee_pseudos));
            new_insn.src = insn
                .src
                .iter()
                .map(|s| ctx.remap_pseudo(*s, callee_pseudos))
                .collect();
            // For indirect calls, also remap the function pointer pseudo
            new_insn.indirect_target = insn
                .indirect_target
                .map(|t| ctx.remap_pseudo(t, callee_pseudos));
            // Keep func_name and other call metadata unchanged
            vec![new_insn]
        }

        // All other instructions: remap target and sources
        _ => {
            let mut new_insn = insn.clone();

            // Remap target
            if let Some(target) = new_insn.target {
                new_insn.target = Some(ctx.remap_pseudo(target, callee_pseudos));
            }

            // Remap sources
            new_insn.src = new_insn
                .src
                .iter()
                .map(|s| ctx.remap_pseudo(*s, callee_pseudos))
                .collect();

            // Remap branch targets if present (shouldn't be for non-control-flow)
            if let Some(bb) = new_insn.bb_true {
                new_insn.bb_true = Some(ctx.remap_bb(bb));
            }
            if let Some(bb) = new_insn.bb_false {
                new_insn.bb_false = Some(ctx.remap_bb(bb));
            }

            vec![new_insn]
        }
    }
}

// ============================================================================
// Call Site Inlining
// ============================================================================

/// Inline a specific call site
/// Returns true if inlining was performed
fn inline_call_site(
    caller: &mut Function,
    call_bb_idx: usize,
    call_insn_idx: usize,
    callee: &Function,
) -> bool {
    // Get the call instruction
    let call_bb = &caller.blocks[call_bb_idx];
    let call_insn = &call_bb.insns[call_insn_idx];

    if call_insn.op != Opcode::Call {
        return false;
    }

    // Extract call info before borrowing mutably
    let call_args = call_insn.src.clone();
    let return_target = call_insn.target;
    let call_bb_id = call_bb.id;

    // Initialize inline context (continuation_bb_id will be allocated after callee BBs)
    let mut ctx = InlineContext::new(
        caller,
        callee,
        call_args,
        BasicBlockId(0), // Placeholder, will be set after allocating callee BBs
        return_target,
    );

    // Pre-allocate BB mappings for all callee blocks
    for bb in &callee.blocks {
        ctx.remap_bb(bb.id);
    }

    // Now allocate continuation block ID (after all callee BB IDs are allocated)
    let continuation_bb_id = ctx.alloc_bb_id();
    ctx.return_continuation_bb = continuation_bb_id;

    // Clone all basic blocks from callee
    let mut inlined_blocks: Vec<BasicBlock> = Vec::new();
    let mut inlined_pseudos: Vec<Pseudo> = Vec::new();

    for callee_bb in &callee.blocks {
        let new_bb_id = ctx.bb_map[&callee_bb.id];
        let mut new_bb = BasicBlock::new(new_bb_id);

        // Copy parents and children (will be remapped later)
        new_bb.parents = callee_bb.parents.clone();
        new_bb.children = callee_bb.children.clone();

        // Clone instructions
        for insn in &callee_bb.insns {
            let cloned = clone_instruction(&mut ctx, insn, &callee.pseudos);
            for new_insn in cloned {
                new_bb.insns.push(new_insn);
            }
        }

        // Set label for debugging
        new_bb.label = Some(format!(
            "{}_inline{}_bb{}",
            callee.name, ctx.inline_id, callee_bb.id.0
        ));

        inlined_blocks.push(new_bb);
    }

    // Clone pseudos that need to exist in caller
    for callee_pseudo in &callee.pseudos {
        // Skip arguments (they're substituted with call args)
        if matches!(callee_pseudo.kind, PseudoKind::Arg(_)) {
            continue;
        }

        // Check if this pseudo was used (has a mapping)
        if ctx.pseudo_map.contains_key(&callee_pseudo.id) {
            let new_pseudo = ctx.clone_pseudo(callee_pseudo, &callee.pseudos);
            // Only add if not already substituted to an existing pseudo
            if !caller.pseudos.iter().any(|p| p.id == new_pseudo.id) {
                inlined_pseudos.push(new_pseudo);
            }
        }
    }

    // Get inlined entry block ID
    let inlined_entry = ctx.bb_map[&callee.entry];

    // Split the caller's block at the call site
    let call_bb = &mut caller.blocks[call_bb_idx];

    // Move instructions after the call to continuation block
    let after_call_insns: Vec<Instruction> = call_bb.insns.drain((call_insn_idx + 1)..).collect();

    // Replace call with branch to inlined entry
    call_bb.insns[call_insn_idx] = {
        let mut br = Instruction::new(Opcode::Br);
        br.bb_true = Some(inlined_entry);
        br
    };

    // Create continuation block with instructions after the call
    let mut continuation_bb = BasicBlock::new(continuation_bb_id);
    continuation_bb.insns = after_call_insns;
    continuation_bb.label = Some(format!("inline{}_cont", ctx.inline_id));

    // Update CFG: set children of call block to just the inlined entry
    let old_children = caller.blocks[call_bb_idx].children.clone();
    caller.blocks[call_bb_idx].children = vec![inlined_entry];

    // Update CFG: continuation block's children are the old children
    continuation_bb.children = old_children.clone();

    // Update CFG: continuation block's parents will be set below
    continuation_bb.parents = Vec::new();

    // Add all inlined blocks to caller
    for mut block in inlined_blocks {
        // Update parent/child references
        block.parents = block
            .parents
            .iter()
            .filter_map(|&p| ctx.bb_map.get(&p).copied())
            .collect();
        block.children = block
            .children
            .iter()
            .filter_map(|&c| ctx.bb_map.get(&c).copied())
            .collect();

        // Entry block of inlined function: add call block as parent
        if ctx.bb_map.get(&callee.entry) == Some(&block.id) {
            block.parents.push(call_bb_id);
        }

        // Blocks with Ret (now Br to continuation): update children
        if block
            .insns
            .last()
            .map(|i| i.op == Opcode::Br && i.bb_true == Some(continuation_bb_id))
            .unwrap_or(false)
        {
            block.children = vec![continuation_bb_id];
            continuation_bb.parents.push(block.id);
        }

        caller.blocks.push(block);
    }

    // Add continuation block
    caller.blocks.push(continuation_bb);

    // Add cloned pseudos to caller
    caller.pseudos.extend(inlined_pseudos);

    // Add callee's local variables to caller's locals with mangled names
    // This is necessary so that regalloc treats these as stack-allocated locals
    // rather than global symbols
    for (local_name, local_var) in &callee.locals {
        // Create the mangled name (same pattern as clone_pseudo)
        let new_name = format!("{}_inline{}_{}", callee.name, ctx.inline_id, local_name);

        // Look up the remapped pseudo ID
        if let Some(&new_sym) = ctx.pseudo_map.get(&local_var.sym) {
            // Remap decl_block if present
            let new_decl_block = local_var
                .decl_block
                .and_then(|bb| ctx.bb_map.get(&bb).copied());

            caller.locals.insert(
                new_name,
                super::LocalVar {
                    sym: new_sym,
                    typ: local_var.typ,
                    is_volatile: local_var.is_volatile,
                    decl_block: new_decl_block,
                },
            );
        }
    }

    // Update old children's parent references to point to continuation
    for &child_id in &old_children {
        if let Some(child) = caller.blocks.iter_mut().find(|b| b.id == child_id) {
            for parent in &mut child.parents {
                if *parent == call_bb_id {
                    *parent = continuation_bb_id;
                }
            }
        }
    }

    true
}

// ============================================================================
// Main Inlining Pass
// ============================================================================

/// Run the inlining pass on a module
pub fn run(module: &mut Module, opt_level: u32) -> bool {
    if opt_level == 0 {
        return false;
    }

    let mut any_changed = false;

    // Iterate to handle nested inlining
    for _iteration in 0..MAX_INLINE_ITERATIONS {
        // Re-analyze functions each iteration
        let candidates = analyze_all_functions(module);

        let mut changed_this_iteration = false;

        // Process each function
        for func_idx in 0..module.functions.len() {
            let caller_size: usize = module.functions[func_idx]
                .blocks
                .iter()
                .map(|b| b.insns.len())
                .sum();

            // Find all call sites in this function that should be inlined
            // Process in reverse order to avoid invalidating indices
            let mut call_sites: Vec<(usize, usize, String)> = Vec::new();

            for (bb_idx, bb) in module.functions[func_idx].blocks.iter().enumerate() {
                for (insn_idx, insn) in bb.insns.iter().enumerate() {
                    if insn.op == Opcode::Call {
                        if let Some(callee_name) = &insn.func_name {
                            if let Some(candidate) = candidates.get(callee_name) {
                                if should_inline(candidate, opt_level, caller_size) {
                                    // Don't inline recursive calls
                                    if callee_name != &module.functions[func_idx].name {
                                        call_sites.push((bb_idx, insn_idx, callee_name.clone()));
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Process call sites in reverse order
            for (bb_idx, insn_idx, callee_name) in call_sites.into_iter().rev() {
                // Find callee function
                let callee = module
                    .functions
                    .iter()
                    .find(|f| f.name == callee_name)
                    .cloned();

                if let Some(callee) = callee {
                    if inline_call_site(&mut module.functions[func_idx], bb_idx, insn_idx, &callee)
                    {
                        changed_this_iteration = true;
                    }
                }
            }
        }

        if changed_this_iteration {
            any_changed = true;
        } else {
            break;
        }
    }

    // Remove dead static functions that were fully inlined
    if any_changed {
        remove_dead_functions(module);
    }

    any_changed
}

/// Remove functions that are static and have no callers
fn remove_dead_functions(module: &mut Module) {
    // Collect all function names for lookup
    let func_names: HashSet<String> = module.functions.iter().map(|f| f.name.clone()).collect();

    // Count calls to each function and track address-taken functions
    let mut call_counts: HashMap<String, usize> = HashMap::new();
    let mut address_taken: HashSet<String> = HashSet::new();

    for func in &module.functions {
        for bb in &func.blocks {
            for insn in &bb.insns {
                if insn.op == Opcode::Call {
                    if let Some(name) = &insn.func_name {
                        *call_counts.entry(name.clone()).or_insert(0) += 1;
                    }
                } else if insn.op == Opcode::SymAddr {
                    // Check if this takes the address of a function
                    if !insn.src.is_empty() {
                        let src_id = insn.src[0];
                        if let Some(pseudo) = func.pseudos.iter().find(|p| p.id == src_id) {
                            if let PseudoKind::Sym(ref name) = pseudo.kind {
                                // Check if this symbol is a function name
                                if func_names.contains(name) {
                                    address_taken.insert(name.clone());
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Remove static functions with no callers and no address taken (except main)
    module.functions.retain(|f| {
        f.name == "main"
            || !f.is_static
            || call_counts.get(&f.name).copied().unwrap_or(0) > 0
            || address_taken.contains(&f.name)
    });
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::TypeTable;

    fn make_simple_func(name: &str, is_inline: bool) -> Function {
        let types = TypeTable::new(64);
        let mut func = Function::new(name, types.int_id);
        func.is_inline = is_inline;
        func.is_static = is_inline; // inline implies static for our purposes

        // Add simple function: entry + ret 42
        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.insns.push(Instruction::new(Opcode::Entry));

        // Return constant
        let mut ret = Instruction::new(Opcode::Ret);
        ret.src = vec![PseudoId(0)];
        bb.insns.push(ret);

        func.blocks.push(bb);
        func.entry = BasicBlockId(0);

        // Add constant pseudo
        func.pseudos.push(Pseudo {
            id: PseudoId(0),
            kind: PseudoKind::Val(42),
            name: None,
        });

        func
    }

    #[test]
    fn test_analyze_simple_function() {
        let func = make_simple_func("test", true);
        let mut module = Module::new();
        module.functions.push(func);

        let candidate = analyze_function(&module.functions[0], &module);

        assert!(candidate.has_inline_hint);
        assert!(!candidate.uses_varargs);
        assert!(!candidate.is_recursive);
        assert_eq!(candidate.estimated_size, 2); // entry + ret
        assert_eq!(candidate.call_count, 0);
    }

    #[test]
    fn test_should_inline_small_function() {
        let candidate = InlineCandidate {
            estimated_size: 5,
            has_inline_hint: false,
            is_recursive: false,
            uses_varargs: false,
            uses_alloca: false,
            call_count: 1,
        };

        // Small function should always inline at -O1
        assert!(should_inline(&candidate, 1, 100));
    }

    #[test]
    fn test_should_not_inline_varargs() {
        let candidate = InlineCandidate {
            estimated_size: 5,
            has_inline_hint: true,
            is_recursive: false,
            uses_varargs: true,
            uses_alloca: false,
            call_count: 1,
        };

        // Varargs functions should never inline
        assert!(!should_inline(&candidate, 2, 100));
    }

    #[test]
    fn test_should_not_inline_recursive() {
        let candidate = InlineCandidate {
            estimated_size: 5,
            has_inline_hint: true,
            is_recursive: true,
            uses_varargs: false,
            uses_alloca: false,
            call_count: 1,
        };

        // Recursive functions should not inline
        assert!(!should_inline(&candidate, 2, 100));
    }

    #[test]
    fn test_should_not_inline_at_o0() {
        let candidate = InlineCandidate {
            estimated_size: 5,
            has_inline_hint: true,
            is_recursive: false,
            uses_varargs: false,
            uses_alloca: false,
            call_count: 1,
        };

        // Should not inline at -O0
        assert!(!should_inline(&candidate, 0, 100));
    }

    #[test]
    fn test_inline_hint_threshold() {
        let candidate = InlineCandidate {
            estimated_size: 30,
            has_inline_hint: true,
            is_recursive: false,
            uses_varargs: false,
            uses_alloca: false,
            call_count: 1,
        };

        // 30 instructions with inline hint should inline
        assert!(should_inline(&candidate, 1, 100));

        // Without hint, 30 instructions is too large
        let candidate_no_hint = InlineCandidate {
            has_inline_hint: false,
            ..candidate
        };
        assert!(!should_inline(&candidate_no_hint, 1, 100));
    }
}
