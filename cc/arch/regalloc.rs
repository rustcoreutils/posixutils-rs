//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Common register allocator utilities shared between architectures
//

use crate::abi::{Abi, ArgClass};
use crate::ir::{BasicBlockId, Function, Instruction, Opcode, PseudoId, PseudoKind};
use crate::types::{TypeKind, TypeTable};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::hash::Hash;

// ============================================================================
// LocationMap — single owner for PseudoId → Loc bindings (M2)
// ============================================================================
//
// Every PseudoId has at most one current Loc, set by the regalloc and
// observed by the codegen. Bug-class C in the regalloc findings was
// "two disagreeing location computations" — codegen deriving an alternate
// Loc from `PseudoKind` instead of asking the allocator. Routing all
// accesses through this newtype makes that pattern syntactically visible:
//   * `get(p)` returns the single current binding.
//   * `set(p, loc)` is the only way to write — call sites stand out in
//     review (the allocator owns most writes; codegen needs the seam
//     for intrinsic results that land in fixed ABI registers).
// The inner HashMap stays private so no caller can stash a stale lookup.
//
// `Loc` differs per architecture, so the map is generic. Each backend
// uses its own `LocationMap<Loc>` instantiation.

#[derive(Debug, Clone, Default)]
pub struct LocationMap<L: Clone> {
    inner: HashMap<PseudoId, L>,
}

impl<L: Clone> LocationMap<L> {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    /// Look up the current binding, if any.
    pub fn get(&self, pseudo: PseudoId) -> Option<L> {
        self.inner.get(&pseudo).cloned()
    }

    /// Look up the current binding by reference (no clone). Use this
    /// only when a fast existence check is enough — most callers want
    /// `get` so the borrow doesn't outlive the immediate match.
    pub fn get_ref(&self, pseudo: PseudoId) -> Option<&L> {
        self.inner.get(&pseudo)
    }

    /// Insert or overwrite a binding. Codegen uses this for the post-
    /// allocator updates that pin an intrinsic's result pseudo to a
    /// fixed ABI register (the only legitimate write-after-allocate).
    pub fn set(&mut self, pseudo: PseudoId, loc: L) {
        self.inner.insert(pseudo, loc);
    }
}

impl<L: Clone> From<HashMap<PseudoId, L>> for LocationMap<L> {
    fn from(inner: HashMap<PseudoId, L>) -> Self {
        Self { inner }
    }
}

const DEFAULT_INTERVAL_CAPACITY: usize = 64;
const DEFAULT_CONSTRAINT_CAPACITY: usize = 16;
const DEFAULT_CALL_POS_CAPACITY: usize = 16;
const DEFAULT_SMALL_VEC_CAPACITY: usize = 8;

// ============================================================================
// Common Types
// ============================================================================

/// Live interval for a pseudo-register
#[derive(Debug, Clone)]
pub struct LiveInterval {
    pub pseudo: PseudoId,
    pub start: usize,
    pub end: usize,
    /// True if this interval spans a loop back edge (used within a loop)
    pub in_loop: bool,
}

/// A stack slot freed by an expired live interval, available for reuse.
#[derive(Debug, Clone)]
pub struct FreeSlot {
    pub offset: i32,
    pub alignment: i32,
    /// The pseudo that previously owned this slot (used for interference checks)
    pub owner: PseudoId,
}

/// A point in the function where register constraints apply.
/// Used during allocation to avoid assigning pseudos to registers
/// that would be clobbered at this point.
/// Generic over register type R.
#[derive(Debug, Clone)]
pub struct ConstraintPoint<R> {
    /// Instruction position in the function
    pub position: usize,
    /// Registers clobbered at this point
    pub clobbers: Vec<R>,
    /// Pseudos that ARE the operands of the constrained instruction
    /// (these should NOT be evicted - they're the actual operands)
    pub involved_pseudos: Vec<PseudoId>,
}

// ============================================================================
// Common Functions
// ============================================================================

/// Expire old intervals from the active list, returning freed registers to the free list.
/// Generic over register type R (works with both GP and FP register types).
pub fn expire_intervals<R: Copy>(
    active: &mut Vec<(LiveInterval, R)>,
    free_regs: &mut Vec<R>,
    point: usize,
) {
    let mut to_remove = Vec::with_capacity(DEFAULT_SMALL_VEC_CAPACITY);
    for (i, (interval, reg)) in active.iter().enumerate() {
        if interval.end < point {
            free_regs.push(*reg);
            to_remove.push(i);
        }
    }
    for i in to_remove.into_iter().rev() {
        active.remove(i);
    }
}

/// Expire stack intervals whose live range ended before `point`,
/// returning their slots to the free list.
pub fn expire_stack_intervals(
    active_stack: &mut Vec<(LiveInterval, i32, i32)>,
    free_slots: &mut BTreeMap<i32, Vec<FreeSlot>>,
    point: usize,
) {
    active_stack.retain(|(interval, offset, size)| {
        if interval.end < point {
            let alignment = if *size >= 16 { 16 } else { 8 };
            free_slots.entry(*size).or_default().push(FreeSlot {
                offset: *offset,
                alignment,
                owner: interval.pseudo,
            });
            false
        } else {
            true
        }
    });
}

/// Find all positions of call instructions in a function.
/// Used by spill_args_across_calls to identify where arguments may be clobbered.
/// Includes Call, Longjmp, and Setjmp opcodes since they all invoke external functions
/// and clobber caller-saved registers.
pub fn find_call_positions(func: &Function) -> Vec<usize> {
    let mut call_positions = Vec::with_capacity(DEFAULT_CALL_POS_CAPACITY);
    let mut pos = 0usize;
    for block in &func.blocks {
        for insn in &block.insns {
            if matches!(insn.op, Opcode::Call | Opcode::Longjmp | Opcode::Setjmp) {
                call_positions.push(pos);
            }
            pos += 1;
        }
    }
    call_positions
}

/// Check if a live interval crosses any call position.
/// Note: We use <= for the end check because values used as call arguments
/// (where interval.end == call_pos) need to survive until after argument
/// setup, which clobbers caller-saved registers before the actual call.
pub fn interval_crosses_call(interval: &LiveInterval, call_positions: &[usize]) -> bool {
    call_positions
        .iter()
        .any(|&call_pos| interval.start <= call_pos && call_pos <= interval.end)
}

/// Find registers that would conflict with this interval due to constraints.
///
/// A constraint point models an instruction that clobbers one or more
/// physical registers (e.g. `idivl` clobbers RAX and RDX). For any live
/// interval that overlaps such a point, we must keep that pseudo out of the
/// clobbered registers — otherwise its value would be silently destroyed.
///
/// There is one exception: pseudos that are direct *operands* of the
/// constraining instruction (`involved_pseudos`) MAY occupy a clobbered
/// register, because the constraining instruction itself reads or writes
/// those registers as part of its semantics. The classic case is the
/// integer dividend, which must be in RAX/EAX for `idiv` to execute.
///
/// Crucially, that "operand exemption" only holds when the operand's value
/// dies at the constraint point (`interval.end == cp.position`). When the
/// interval extends *past* the clobber (`interval.end > cp.position`), the
/// operand's value is still needed afterward, but the clobbering
/// instruction will have destroyed it. In that case the allocator must
/// pick a NON-clobbered register; codegen will materialize the value into
/// the operand-required register (e.g. RAX) with a move just before the
/// constraint, preserving the original pseudo's contents for later uses.
///
/// Without this distinction, `Copy`/CSE/SCCP-class passes that extend a
/// pseudo's live range across a `mods.32`/`idivl` boundary silently
/// miscompile (see git history: prior copyprop attempts hung do-while-
/// continue tests by allocating the dividend to RAX with a use after the
/// idiv).
///
/// Generic over register type R.
pub fn find_conflicting_registers<R: Copy + Eq + Hash>(
    interval: &LiveInterval,
    constraint_points: &[ConstraintPoint<R>],
) -> HashSet<R> {
    let mut conflicts = HashSet::with_capacity(DEFAULT_SMALL_VEC_CAPACITY);

    for cp in constraint_points {
        // Use <= on both ends so an interval that exactly meets the
        // constraint point still counts as overlapping.
        if interval.start <= cp.position && cp.position <= interval.end {
            let is_involved = cp.involved_pseudos.contains(&interval.pseudo);
            let dies_at_point = interval.end == cp.position;
            // The operand exemption only applies when the value is
            // consumed AT the constraint point. If it must survive past
            // the clobber, it has to live in a non-clobbered register.
            if !is_involved || !dies_at_point {
                for &reg in &cp.clobbers {
                    conflicts.insert(reg);
                }
            }
        }
    }

    conflicts
}

/// Result of liveness analysis: intervals, constraint points, and per-block liveness sets.
pub struct LivenessResult<R> {
    pub intervals: Vec<LiveInterval>,
    pub constraint_points: Vec<ConstraintPoint<R>>,
    /// Per-block live-in sets (indexed by block index)
    pub live_in: Vec<HashSet<PseudoId>>,
    /// Per-block live-out sets (indexed by block index)
    pub live_out: Vec<HashSet<PseudoId>>,
}

/// Check whether two pseudos interfere (are simultaneously live in any block).
/// Uses block-level live_in/live_out sets from the dataflow fixpoint, which are
/// correct for all CFG shapes including computed gotos.
pub fn pseudos_interfere(
    live_in: &[HashSet<PseudoId>],
    live_out: &[HashSet<PseudoId>],
    a: PseudoId,
    b: PseudoId,
) -> bool {
    for (li, lo) in live_in.iter().zip(live_out.iter()) {
        let a_live = li.contains(&a) || lo.contains(&a);
        let b_live = li.contains(&b) || lo.contains(&b);
        if a_live && b_live {
            return true;
        }
    }
    false
}

/// Compute live intervals and constraint points for a function.
///
/// This is the shared core of live interval computation used by both x86-64 and AArch64.
/// The `get_constraint_info` callback allows architecture-specific constraint handling.
///
/// # Arguments
/// * `func` - The function to analyze
/// * `get_constraint_info` - Callback that returns constraint info for an instruction:
///   - Returns `Some((clobbers, involved_pseudos))` if the instruction has register constraints
///   - Returns `None` if no constraints apply
///
/// The callback signature: `Fn(&Instruction) -> Option<(Vec<R>, Vec<PseudoId>)>`
pub fn compute_live_intervals<R, F>(func: &Function, get_constraint_info: F) -> LivenessResult<R>
where
    R: Clone,
    F: Fn(&Instruction) -> Option<(Vec<R>, Vec<PseudoId>)>,
{
    let num_blocks = func.blocks.len();
    let mut constraint_points: Vec<ConstraintPoint<R>> =
        Vec::with_capacity(DEFAULT_CONSTRAINT_CAPACITY);

    // Phase A: Assign linear positions and build block position maps
    let mut block_start_pos: Vec<usize> = Vec::with_capacity(num_blocks);
    let mut block_end_pos: Vec<usize> = Vec::with_capacity(num_blocks);
    let mut bb_id_to_idx: HashMap<BasicBlockId, usize> = HashMap::with_capacity(num_blocks);
    let mut pos = 0usize;
    for (idx, block) in func.blocks.iter().enumerate() {
        bb_id_to_idx.insert(block.id, idx);
        let block_start = pos;
        block_start_pos.push(block_start);
        for insn in &block.insns {
            if let Some((clobbers, involved_pseudos)) = get_constraint_info(insn) {
                constraint_points.push(ConstraintPoint {
                    position: pos,
                    clobbers,
                    involved_pseudos,
                });
            }
            pos += 1;
        }
        block_end_pos.push(if pos > block_start {
            pos - 1
        } else {
            block_start
        });
    }

    // Collect argument pseudo IDs (implicitly defined at function entry)
    let arg_pseudos: Vec<PseudoId> = func
        .pseudos
        .iter()
        .filter(|p| matches!(p.kind, PseudoKind::Arg(_)))
        .map(|p| p.id)
        .collect();

    // Phase B: per-block first/last use positions, def-block map, and the
    // set of pseudos written in each block (used to bound liveness via
    // Phase C's Boissinot Up_and_Mark traversal). Replaces the legacy
    // gen/kill + backward dataflow fixpoint.
    let mut first_pos_map: Vec<HashMap<PseudoId, usize>> = vec![HashMap::new(); num_blocks];
    let mut last_pos_map: Vec<HashMap<PseudoId, usize>> = vec![HashMap::new(); num_blocks];
    let mut defined_in: Vec<HashSet<PseudoId>> = vec![HashSet::new(); num_blocks];
    // Per-pseudo set of defining blocks. The IR pcc hands to the
    // allocator is *not* strictly SSA: lower::eliminate_phi_nodes
    // converts each Phi into one Copy per predecessor edge, all
    // targeting the same pseudo. A single pseudo therefore has up to
    // `phi.phi_list.len()` defs spread across predecessor blocks.
    // Boissinot's Up_and_Mark must stop at ANY of those defs — tracking
    // only one would cause spurious live-in propagation past the
    // other defs and inflate the interference graph.
    //
    // Arg pseudos are defined implicitly at entry; Sym pseudos at
    // their declaration block; regular instruction targets at the
    // block they sit in. All cases push to the same per-pseudo def
    // set.
    let mut def_blocks: HashMap<PseudoId, HashSet<usize>> = HashMap::new();

    for &arg_pseudo in &arg_pseudos {
        def_blocks.entry(arg_pseudo).or_default().insert(0);
        defined_in[0].insert(arg_pseudo);
        first_pos_map[0].entry(arg_pseudo).or_insert(0);
        last_pos_map[0].entry(arg_pseudo).or_insert(0);
    }

    for local_var in func.locals.values() {
        if let Some(&block_idx) = local_var.decl_block.and_then(|id| bb_id_to_idx.get(&id)) {
            def_blocks
                .entry(local_var.sym)
                .or_default()
                .insert(block_idx);
            defined_in[block_idx].insert(local_var.sym);
            first_pos_map[block_idx]
                .entry(local_var.sym)
                .or_insert(block_start_pos[block_idx]);
            last_pos_map[block_idx]
                .entry(local_var.sym)
                .or_insert(block_start_pos[block_idx]);
        }
    }

    for (idx, block) in func.blocks.iter().enumerate() {
        for (ipos, insn) in (block_start_pos[idx]..).zip(block.insns.iter()) {
            for &src in &insn.src {
                first_pos_map[idx].entry(src).or_insert(ipos);
                last_pos_map[idx].insert(src, ipos);
            }
            if let Some(indirect) = insn.indirect_target {
                first_pos_map[idx].entry(indirect).or_insert(ipos);
                last_pos_map[idx].insert(indirect, ipos);
            }
            if insn.op == Opcode::Asm {
                if let Some(asm) = &insn.asm_data {
                    for input in &asm.inputs {
                        let p = input.pseudo;
                        first_pos_map[idx].entry(p).or_insert(ipos);
                        last_pos_map[idx].insert(p, ipos);
                    }
                    for output in &asm.outputs {
                        let p = output.pseudo;
                        def_blocks.entry(p).or_default().insert(idx);
                        defined_in[idx].insert(p);
                        first_pos_map[idx].entry(p).or_insert(ipos);
                        last_pos_map[idx].insert(p, ipos);
                    }
                }
            }
            if let Some(target) = insn.target {
                def_blocks.entry(target).or_default().insert(idx);
                defined_in[idx].insert(target);
                first_pos_map[idx].entry(target).or_insert(ipos);
                last_pos_map[idx].insert(target, ipos);
            }
        }
    }

    // Phase C: Boissinot 2011 Up_and_Mark. For each use site, BFS
    // upward through predecessor edges marking `live_in[B]` and
    // `live_out[parent]` until reaching the def block (the use was
    // downstream of the def, so the def block itself is not live-in
    // for this pseudo). Each (pseudo, block) pair is visited at most
    // once — no global fixpoint iteration.
    //
    // Pseudos with no recorded def site (constants, globals, or
    // implicit values that flow into the function from a context the
    // IR doesn't model) propagate all the way back to the entry block
    // and stop there because the entry block has no predecessors.
    // This matches the legacy fixpoint's behavior of treating "no kill"
    // as "live from entry".
    let mut live_in: Vec<HashSet<PseudoId>> = vec![HashSet::new(); num_blocks];
    let mut live_out: Vec<HashSet<PseudoId>> = vec![HashSet::new(); num_blocks];
    let mut worklist: Vec<usize> = Vec::with_capacity(num_blocks);

    let propagate_use = |use_block: usize,
                         pseudo: PseudoId,
                         live_in: &mut [HashSet<PseudoId>],
                         live_out: &mut [HashSet<PseudoId>],
                         worklist: &mut Vec<usize>| {
        let defs = def_blocks.get(&pseudo);
        worklist.clear();
        // Seed the use block unconditionally — we are AT a use site,
        // and the caller has already determined the pseudo isn't
        // defined earlier in this block (else `defined_so_far` would
        // have suppressed the call). The same-block redef case (e.g.
        // lower.rs's Copy-before-terminator producing a later def of
        // the same pseudo) still needs liveness to flow in from
        // predecessors to satisfy this use.
        if live_in[use_block].insert(pseudo) {
            for &parent_id in &func.blocks[use_block].parents {
                if let Some(&parent_idx) = bb_id_to_idx.get(&parent_id) {
                    live_out[parent_idx].insert(pseudo);
                    worklist.push(parent_idx);
                }
            }
        }
        while let Some(b) = worklist.pop() {
            // When traversing from a successor (i.e. not the original
            // use block), stop at any defining block — the def supplies
            // the value, no need to propagate past.
            if defs.is_some_and(|d| d.contains(&b)) {
                continue;
            }
            if !live_in[b].insert(pseudo) {
                continue;
            }
            for &parent_id in &func.blocks[b].parents {
                if let Some(&parent_idx) = bb_id_to_idx.get(&parent_id) {
                    live_out[parent_idx].insert(pseudo);
                    worklist.push(parent_idx);
                }
            }
        }
    };

    for (idx, block) in func.blocks.iter().enumerate() {
        // For uses appearing in the same block as the def of a pseudo,
        // we only invoke Up_and_Mark if the def hasn't run yet at that
        // point — otherwise the use is satisfied locally and the value
        // need not be live-in. We track defined-so-far separately from
        // `defined_in` (which is the whole-block summary) by walking
        // the block in program order.
        let mut defined_so_far: HashSet<PseudoId> = HashSet::new();
        // Args and Sym decls are considered defined at the start of
        // the block they belong to, before any instruction.
        if idx == 0 {
            for &arg in &arg_pseudos {
                defined_so_far.insert(arg);
            }
        }
        for local_var in func.locals.values() {
            if let Some(&block_idx) = local_var.decl_block.and_then(|id| bb_id_to_idx.get(&id)) {
                if block_idx == idx {
                    defined_so_far.insert(local_var.sym);
                }
            }
        }

        for insn in &block.insns {
            for &src in &insn.src {
                if !defined_so_far.contains(&src) {
                    propagate_use(idx, src, &mut live_in, &mut live_out, &mut worklist);
                }
            }
            if let Some(indirect) = insn.indirect_target {
                if !defined_so_far.contains(&indirect) {
                    propagate_use(idx, indirect, &mut live_in, &mut live_out, &mut worklist);
                }
            }
            if insn.op == Opcode::Asm {
                if let Some(asm) = &insn.asm_data {
                    for input in &asm.inputs {
                        if !defined_so_far.contains(&input.pseudo) {
                            propagate_use(
                                idx,
                                input.pseudo,
                                &mut live_in,
                                &mut live_out,
                                &mut worklist,
                            );
                        }
                    }
                    for output in &asm.outputs {
                        defined_so_far.insert(output.pseudo);
                    }
                }
            }
            if let Some(target) = insn.target {
                defined_so_far.insert(target);
            }
        }
    }

    // Phase C.1: Args occupy their calling-convention register from
    // function entry. The allocator needs to see them as live at the
    // entry-block start so it doesn't reuse the arg register for an
    // unrelated pseudo whose interval starts at 0. Force them into
    // live_in[0] regardless of where Boissinot terminated propagation.
    for &arg_pseudo in &arg_pseudos {
        live_in[0].insert(arg_pseudo);
    }

    // Phase D: Construct intervals from liveness
    let mut interval_start: HashMap<PseudoId, usize> =
        HashMap::with_capacity(DEFAULT_INTERVAL_CAPACITY);
    let mut interval_end: HashMap<PseudoId, usize> =
        HashMap::with_capacity(DEFAULT_INTERVAL_CAPACITY);

    for idx in 0..num_blocks {
        let mut referenced: HashSet<PseudoId> =
            HashSet::with_capacity(defined_in[idx].len() + live_in[idx].len());
        for &p in &defined_in[idx] {
            referenced.insert(p);
        }
        for &p in &live_in[idx] {
            referenced.insert(p);
        }
        for &p in &live_out[idx] {
            referenced.insert(p);
        }

        for &p in &referenced {
            let start = if live_in[idx].contains(&p) {
                block_start_pos[idx]
            } else {
                first_pos_map[idx]
                    .get(&p)
                    .copied()
                    .unwrap_or(block_start_pos[idx])
            };

            let end = if live_out[idx].contains(&p) {
                block_end_pos[idx]
            } else {
                last_pos_map[idx]
                    .get(&p)
                    .copied()
                    .unwrap_or(block_end_pos[idx])
            };

            interval_start
                .entry(p)
                .and_modify(|s| *s = (*s).min(start))
                .or_insert(start);
            interval_end
                .entry(p)
                .and_modify(|e| *e = (*e).max(end))
                .or_insert(end);
        }
    }

    // Phase E: Detect loop back-edges and mark loop-carried pseudos
    let mut loop_pseudos: HashSet<PseudoId> = HashSet::new();
    for (idx, block) in func.blocks.iter().enumerate() {
        for &child_id in &block.children {
            if let Some(&child_idx) = bb_id_to_idx.get(&child_id) {
                if block_start_pos[child_idx] < block_start_pos[idx] {
                    for &p in &live_out[idx] {
                        if live_in[child_idx].contains(&p) {
                            loop_pseudos.insert(p);
                        }
                    }
                }
            }
        }
    }

    // Phase F: Build sorted LiveInterval vec
    let mut result: Vec<LiveInterval> = interval_start
        .into_iter()
        .filter_map(|(pseudo, start)| {
            interval_end.get(&pseudo).map(|&end| LiveInterval {
                pseudo,
                start,
                end,
                in_loop: loop_pseudos.contains(&pseudo),
            })
        })
        .collect();

    result.sort_by_key(|i| (i.start, i.pseudo.0));
    LivenessResult {
        intervals: result,
        constraint_points,
        live_in,
        live_out,
    }
}

/// Identify Sym pseudos whose address is taken (SymAddr opcode).
/// These must have stable stack addresses and cannot participate in slot reuse.
pub fn identify_addr_taken_syms(func: &Function) -> HashSet<PseudoId> {
    let mut addr_taken = HashSet::new();
    for block in &func.blocks {
        for insn in &block.insns {
            if insn.op == Opcode::SymAddr {
                for &src in &insn.src {
                    addr_taken.insert(src);
                }
            }
        }
    }
    addr_taken
}

/// Identify pseudo-registers that should use floating-point registers.
/// This is a shared implementation used by both x86-64 and AArch64.
///
/// A pseudo is marked as FP if:
/// 1. It's an FVal constant
/// 2. It's the target of an FP arithmetic operation (FAdd, FSub, FMul, FDiv, FNeg)
/// 3. It's the target of an FP conversion (FCvtF, UCvtF, SCvtF)
/// 4. It has a float type (excluding FP comparisons which produce integer results)
///
/// Note: FP comparisons (FCmpOxx) produce integer results (0 or 1), so their
/// targets should NOT be in FP registers.
pub fn identify_fp_pseudos<F>(func: &Function, is_float_type: F) -> HashSet<PseudoId>
where
    F: Fn(crate::types::TypeId) -> bool,
{
    use crate::ir::Opcode;

    let mut fp_pseudos = HashSet::with_capacity(DEFAULT_SMALL_VEC_CAPACITY);

    // Mark FVal constants as FP
    for pseudo in &func.pseudos {
        if matches!(pseudo.kind, PseudoKind::FVal(_)) {
            fp_pseudos.insert(pseudo.id);
        }
    }

    // Scan instructions for FP operations
    for block in &func.blocks {
        for insn in &block.insns {
            // Check if this is an FP operation that produces an FP result
            let is_fp_producing_op = matches!(
                insn.op,
                Opcode::FAdd
                    | Opcode::FSub
                    | Opcode::FMul
                    | Opcode::FDiv
                    | Opcode::FNeg
                    | Opcode::FCvtF
                    | Opcode::UCvtF
                    | Opcode::SCvtF
            );

            if is_fp_producing_op {
                if let Some(target) = insn.target {
                    fp_pseudos.insert(target);
                }
            }

            // Also check type information (but exclude comparisons)
            let is_comparison = matches!(
                insn.op,
                Opcode::FCmpOEq
                    | Opcode::FCmpONe
                    | Opcode::FCmpOLt
                    | Opcode::FCmpOLe
                    | Opcode::FCmpOGt
                    | Opcode::FCmpOGe
            );

            if !is_comparison {
                if let Some(typ) = insn.typ {
                    if is_float_type(typ) {
                        if let Some(target) = insn.target {
                            fp_pseudos.insert(target);
                        }
                    }
                }
            }
        }
    }

    fp_pseudos
}

// ============================================================================
// Backend orchestration helpers (M4)
// ============================================================================
//
// Both backends contain a few short, structurally identical loops in their
// `RegAlloc::allocate` orchestrators — mostly differing only in stack-offset
// sign and which `Loc` variant they construct. These helpers centralize the
// shared shape and let each backend supply the architecture-specific bits as
// a closure.

/// Assign a fresh stack slot to every `Alloca` instruction's result pseudo.
///
/// Both backends bump their stack counter by 8 per Alloca and write a
/// `Loc::Stack(offset)` for the result. The stack-offset sign differs
/// (x86_64 stores `+stack_offset`, aarch64 stores `-stack_offset`), so
/// the caller passes a `mk_stack_loc` closure that converts an i32
/// counter into the backend's preferred `Loc::Stack` value.
pub fn assign_alloca_slots<L, F>(
    func: &Function,
    stack_offset: &mut i32,
    locations: &mut HashMap<PseudoId, L>,
    mk_stack_loc: F,
) where
    F: Fn(i32) -> L,
{
    for block in &func.blocks {
        for insn in &block.insns {
            if insn.op == Opcode::Alloca {
                if let Some(target) = insn.target {
                    *stack_offset += 8;
                    locations.insert(target, mk_stack_loc(*stack_offset));
                }
            }
        }
    }
}

/// Spill GP arguments that live in caller-saved registers and cross a
/// call instruction.
///
/// The GP path is identical between backends modulo the per-arch
/// callbacks: the caller supplies which registers are arg-passing,
/// how to extract a register from a `Loc`, how to construct the new
/// `Loc::Stack` for the spilled value, and how to record the spill
/// metadata for the prologue. The stack-offset sign convention is
/// folded into `mk_stack_loc` and `record_spill` so this helper
/// itself is sign-agnostic.
#[allow(clippy::too_many_arguments)]
pub fn spill_gp_args_across_calls<L, R, IsArg, ExtractReg, MkStackLoc, RecordSpill, PushFree>(
    intervals: &[LiveInterval],
    call_positions: &[usize],
    locations: &mut HashMap<PseudoId, L>,
    stack_offset: &mut i32,
    is_arg_reg: IsArg,
    extract_reg: ExtractReg,
    mk_stack_loc: MkStackLoc,
    mut record_spill: RecordSpill,
    mut push_free: PushFree,
) where
    L: Clone,
    R: Copy,
    IsArg: Fn(R) -> bool,
    ExtractReg: Fn(&L) -> Option<R>,
    MkStackLoc: Fn(i32) -> L,
    RecordSpill: FnMut(PseudoId, R, i32),
    PushFree: FnMut(R),
{
    for interval in intervals {
        let Some(loc) = locations.get(&interval.pseudo) else {
            continue;
        };
        let Some(reg) = extract_reg(loc) else {
            continue;
        };
        if !is_arg_reg(reg) {
            continue;
        }
        if !interval_crosses_call(interval, call_positions) {
            continue;
        }
        *stack_offset += 8;
        let to_offset = *stack_offset;
        record_spill(interval.pseudo, reg, to_offset);
        locations.insert(interval.pseudo, mk_stack_loc(to_offset));
        push_free(reg);
    }
}

/// Try to reuse a previously freed stack slot of the given size and alignment.
/// Uses block-level interference check to verify the candidate pseudo doesn't
/// overlap with the slot's previous owner.
/// Shared between x86_64 and aarch64 register allocators.
pub fn try_reuse_stack_slot(
    free_stack_slots: &mut BTreeMap<i32, Vec<FreeSlot>>,
    size: i32,
    alignment: i32,
    candidate: PseudoId,
    live_in: &[HashSet<PseudoId>],
    live_out: &[HashSet<PseudoId>],
) -> Option<i32> {
    if let Some(slots) = free_stack_slots.get_mut(&size) {
        if let Some(idx) = slots.iter().position(|s| {
            s.alignment >= alignment && !pseudos_interfere(live_in, live_out, candidate, s.owner)
        }) {
            let slot = slots.remove(idx);
            if slots.is_empty() {
                free_stack_slots.remove(&size);
            }
            return Some(slot.offset);
        }
    }
    None
}

// ============================================================================
// Allocator–ABI contract layer (M1)
// ============================================================================
//
// Both per-arch register allocators previously re-derived argument
// classification inline — repeating type-kind checks (`is_float`,
// `is_complex`, `is_long_double`, `is_int128`, struct-size thresholds)
// that the `cc/abi/*::classify_param` implementations already perform.
// Two sources of truth, two opportunities to drift.
//
// `AbiLowering` centralizes that contract:
//
//   * Build it once per function with the `Function` + `TypeTable`. It
//     pre-indexes the function's pseudos by `Arg(n)` for O(1) lookup
//     (the previous code did an O(P) inner scan per argument) and
//     detects the hidden sret pointer.
//
//   * For each parameter, `iter_args(abi)` yields an `AbiArg` carrying
//     the `ArgClass` from `abi.classify_param` plus a handful of type
//     tiebreakers (`is_complex`, `is_long_double`, `is_int128`) that
//     `ArgClass` alone cannot disambiguate — most notably, x86_64
//     `_Complex float`/`_Complex double` and a 2-eightbyte all-SSE
//     struct both classify as `Direct { classes: [Sse, Sse] }` but the
//     backend routes them differently.
//
// Backends still own the actual `Loc` decision because `Loc` is per-arch
// (each has its own `Reg`/`XmmReg`/`VReg`). They dispatch on `arg.class`
// and the `is_*` flags, eliminating the duplicated type-kind checks.

/// Per-argument context yielded by `AbiLowering::iter_args`.
///
/// Carries the ABI classification together with the type tiebreakers a
/// backend may need beyond `ArgClass` alone.
#[derive(Debug, Clone)]
pub struct AbiArg {
    /// The pseudo representing this argument in the function's IR.
    pub pseudo: PseudoId,
    /// Classification produced by `abi.classify_param(typ, types)`.
    pub class: ArgClass,
    /// True iff the type is `__int128` / `unsigned __int128`. Backends
    /// always allocate an aligned local stack slot for int128 even when
    /// the value arrives in a register pair.
    pub is_int128: bool,
}

/// Allocator-side adapter over `cc/abi/*::classify_param`.
///
/// Construct once per function being lowered, then iterate the function's
/// parameters with `iter_args(abi)`. The struct caches the sret detection
/// and a per-`Arg(n)` pseudo index so per-arg work is O(1).
pub struct AbiLowering<'a> {
    func: &'a Function,
    types: &'a TypeTable,
    /// PseudoId for each `Arg(n)`. `n` indexes the vector; absent
    /// positions are `None`. Vector length covers `0..=max_arg_index`.
    pub arg_pseudos: Vec<Option<PseudoId>>,
    /// Hidden sret pointer pseudo (named `__sret` with kind `Arg(0)`).
    /// When present, normal `Arg(k)` parameters shift by 1, so the
    /// `i`-th `func.params` entry maps to `Arg(i + 1)`.
    pub sret_pseudo: Option<PseudoId>,
    /// 1 when an sret pseudo is present, 0 otherwise — added to the
    /// param index to find the matching `Arg(n)`.
    pub arg_idx_offset: u32,
}

impl<'a> AbiLowering<'a> {
    /// Build an `AbiLowering` for `func`. Does not consult any `Abi`
    /// yet — the per-arg classification is produced lazily by
    /// `iter_args` so the same `AbiLowering` can be reused across
    /// alternative calling-convention overrides if ever needed.
    pub fn new(func: &'a Function, types: &'a TypeTable) -> Self {
        // Detect the hidden return pointer for large struct returns.
        // The linearizer emits it as `Arg(0)` with the literal name
        // `__sret`, shifting all normal-parameter `Arg(n)` indices by 1.
        let sret_pseudo = func
            .pseudos
            .iter()
            .find(|p| matches!(p.kind, PseudoKind::Arg(0)) && p.name.as_deref() == Some("__sret"))
            .map(|p| p.id);
        let arg_idx_offset: u32 = if sret_pseudo.is_some() { 1 } else { 0 };

        // Index pseudos by Arg(n). The previous backend code did an
        // O(P) scan per argument; this is O(P) once.
        let max_arg = func
            .pseudos
            .iter()
            .filter_map(|p| {
                if let PseudoKind::Arg(n) = p.kind {
                    Some(n as usize)
                } else {
                    None
                }
            })
            .max()
            .map(|n| n + 1)
            .unwrap_or(0);
        let mut arg_pseudos: Vec<Option<PseudoId>> = vec![None; max_arg];
        for p in &func.pseudos {
            if let PseudoKind::Arg(n) = p.kind {
                arg_pseudos[n as usize] = Some(p.id);
            }
        }

        Self {
            func,
            types,
            arg_pseudos,
            sret_pseudo,
            arg_idx_offset,
        }
    }

    /// Iterate the function's parameters in declaration order, yielding
    /// the per-arg context backends need. The sret pseudo (if any) is
    /// not yielded — callers handle it explicitly via `self.sret_pseudo`.
    pub fn iter_args<'b>(&'b self, abi: &'b dyn Abi) -> impl Iterator<Item = AbiArg> + 'b {
        self.func
            .params
            .iter()
            .enumerate()
            .filter_map(move |(i, (_name, typ))| {
                let arg_idx = (i as u32) + self.arg_idx_offset;
                let pseudo = self.arg_pseudos.get(arg_idx as usize).copied().flatten()?;
                let class = abi.classify_param(*typ, self.types);
                let kind = self.types.kind(*typ);
                Some(AbiArg {
                    pseudo,
                    class,
                    is_int128: kind == TypeKind::Int128,
                })
            })
    }
}
