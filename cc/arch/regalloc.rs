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

use crate::ir::{BasicBlockId, Function, Instruction, Opcode, PseudoId, PseudoKind};
use crate::types::{TypeId, TypeTable};
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::hash::Hash;

/// Grow a frame's locals area by one slot, refusing a total no `i32`
/// displacement can reach -- counting what the prologue will add to it.
///
/// [`TypeTable::MAX_STACK_OBJECT_BYTES`] bounds one object and
/// [`crate::abi::slot_bytes`] enforces that. Neither bounds their *sum*, and
/// `+=` on an `i32` wraps: a frame past two gigabytes came out negative and the
/// prologue subtracted nothing. Both backends' `alloc_stack_slot` is the one
/// place their locals area grows, so this is the one place the total can be
/// checked. A VLA or `alloca` extent never reaches it: the frame holds only an
/// eight-byte pointer for one, and the extent is subtracted from the stack
/// pointer at run time in a 64-bit register.
///
/// Admitting a frame here is a promise to everything downstream, so the check
/// is against what the frame *becomes*, not what it is so far:
///
/// - The slot's own rounding happens inside, in `i64`: the offset up to the
///   slot's alignment, and the size up to a multiple of it. Callers used to
///   round the size first, in `i32`, so `_Alignas(16) char a[2147483640]`
///   wrapped to a zero-byte slot before this check ever saw it.
/// - `frame_align` is the alignment the whole locals area is rounded to at
///   the end -- sixteen, or the over-aligned frame base's -- and that rounding
///   can add almost twice it (`stack_size`). It is reserved here.
/// - Everything else the prologue adds -- saved registers, the variadic save
///   area -- is inside [`TypeTable::FRAME_HEADROOM_BYTES`], which
///   `MAX_STACK_OBJECT_BYTES` already leaves below `i32::MAX`.
///
/// So every frame quantity computed later, in `i32`, stays in range without a
/// check of its own. Before this, an accepted frame near the limit wrapped in
/// `stack_size`, in the prologue total and in aarch64's frame zeroing -- which
/// then looped forever, pushing instructions until the compiler ran out of
/// memory.
pub fn grow_frame(
    offset: &mut i32,
    size: i32,
    alignment: i32,
    frame_align: i32,
    pos: crate::diag::Position,
) -> i32 {
    let align = i64::from(alignment.max(1));
    let mut want = i64::from(*offset);
    if alignment > 8 {
        want = (want + align - 1) & !(align - 1);
    }
    want += (i64::from(size.max(0)) + align - 1) & !(align - 1);
    let limit = TypeTable::MAX_STACK_OBJECT_BYTES as i64 - 2 * i64::from(frame_align.max(16));
    match i32::try_from(want) {
        Ok(n) if i64::from(n) <= limit => *offset = n,
        _ => {
            crate::diag::error_args(
                pos,
                "this function's stack frame needs {0} bytes, \
                 past the {1} bytes a frame can address",
                &[&want.to_string(), &limit.max(0).to_string()],
            );
            *offset = offset.saturating_add(8);
        }
    }
    *offset
}

/// Refuse an incoming stacked-argument area no `i32` displacement reaches.
///
/// Each parameter is inside the frame ceiling on its own; their sum is not,
/// and `IncomingOff::take` saturates rather than wrapping so this one check
/// at the end of a function's parameter layout sees it.
pub fn check_incoming_area(end: i32, pos: crate::diag::Position) {
    if end as usize > TypeTable::MAX_STACK_OBJECT_BYTES {
        crate::diag::error_args(
            pos,
            "this function's stacked parameters need more than the {0} bytes \
             a stack frame can address",
            &[&TypeTable::MAX_STACK_OBJECT_BYTES.to_string()],
        );
    }
}

// ============================================================================
// LocationMap — single owner for PseudoId → Loc bindings
// ============================================================================
//
// Every PseudoId has at most one current Loc, set by the regalloc and
// observed by the codegen. Routing all accesses through this newtype
// keeps codegen from deriving an alternate Loc from `PseudoKind`
// instead of asking the allocator:
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

// Common Types

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
    /// The live intervals of *every* pseudo that has ever owned
    /// this slot — current and prior owners. A candidate may
    /// reuse the slot only if its own interval is disjoint from
    /// EVERY entry. The "current owner only" approximation
    /// (checking just the most-recent occupant) is unsafe:
    /// suppose A owns the slot during [11,17], then frees it; B
    /// reuses during [81,130], also frees. If a candidate C with
    /// interval [6,42] now checks "do I overlap B?" it sees no
    /// conflict — but C overlaps A's old window [11,17] and would
    /// read garbage during that span. CPython's `double_round`
    /// miscompile (assertAlmostEqual segfault) hits this exact
    /// pattern.
    pub owner_intervals: Vec<LiveInterval>,
}

/// A stack slot currently assigned to a live pseudo, plus the full
/// history of past owners. Used by `expire_stack_intervals` and
/// `try_reuse_stack_slot` to preserve history across reuse cycles.
#[derive(Debug, Clone)]
pub struct ActiveSlot {
    /// The current owner's live interval — drives `expire_stack_intervals`.
    pub current: LiveInterval,
    /// Past owners (FIFO) whose intervals already ended but whose
    /// constraints must still be respected on the next reuse.
    pub past: Vec<LiveInterval>,
    pub offset: i32,
    pub size: i32,
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

impl<R> ConstraintPoint<R> {
    /// May `interval` sit in a register this point clobbers?
    ///
    /// Being an operand of the clobbering instruction is not enough on its
    /// own. `mulq` and `idivq` read an operand out of a fixed register and
    /// write their result back over it, so an operand whose value is still
    /// wanted *after* the instruction is destroyed by it. The exemption is
    /// only sound at the two ends of a live range:
    ///
    /// - the range starts here, so the pseudo *is* what the instruction
    ///   produces (the `%rdx` half of a `mulq`, the quotient in `%rax`);
    /// - the range ends here, so the value has been consumed and nothing
    ///   reads it again.
    ///
    /// A range that spans the point strictly is live on both sides, and the
    /// instruction overwrites it in between. That is what left a 128-bit
    /// product's cross term computed from the low half of its own multiply.
    pub fn operand_survives(&self, pseudo: PseudoId, start: usize, end: usize) -> bool {
        self.involved_pseudos.contains(&pseudo) && (start >= self.position || end <= self.position)
    }
}

/// The registers each candidate may not take because a constraint point
/// inside its live range clobbers them -- unless `exempt` says the interval
/// is one the point may clobber, which is where the backends differ.
///
/// An exemption belongs to an operand of the point, so `exempt` is asked only
/// about points whose `involved_pseudos` name the interval's pseudo; both
/// backends' rules already require that.
///
/// Each register is looked up once per interval: the points clobbering it
/// inside the interval are counted by binary search over their positions, and
/// the register is forbidden unless every one of them excuses the interval.
/// The work is intervals x registers, not intervals x points. Visiting every
/// point an interval spans was quadratic for a function with many labels and
/// gotos, whose merged live ranges each span most of the function.
pub fn constraint_clobbers<R: Copy + Ord>(
    constraint_points: &[ConstraintPoint<R>],
    intervals: &[LiveInterval],
    candidates: &std::collections::BTreeSet<PseudoId>,
    exempt: impl Fn(&ConstraintPoint<R>, &LiveInterval) -> bool,
) -> BTreeMap<PseudoId, std::collections::BTreeSet<R>> {
    // Positions of the points clobbering each register, one per point.
    let mut clobbered_at: BTreeMap<R, Vec<usize>> = BTreeMap::new();
    // The points each pseudo is an operand of, one entry per point.
    let mut operand_of: HashMap<PseudoId, Vec<&ConstraintPoint<R>>> = HashMap::new();
    for cp in constraint_points {
        let regs: std::collections::BTreeSet<R> = cp.clobbers.iter().copied().collect();
        for r in regs {
            clobbered_at.entry(r).or_default().push(cp.position);
        }
        let operands: std::collections::BTreeSet<PseudoId> =
            cp.involved_pseudos.iter().copied().collect();
        for p in operands {
            operand_of.entry(p).or_default().push(cp);
        }
    }
    for positions in clobbered_at.values_mut() {
        positions.sort_unstable();
    }

    let mut forbidden: BTreeMap<PseudoId, std::collections::BTreeSet<R>> = BTreeMap::new();
    for interval in intervals.iter().filter(|i| candidates.contains(&i.pseudo)) {
        // A point is inside the interval when start <= position <= end.
        let excused: Vec<&ConstraintPoint<R>> = operand_of
            .get(&interval.pseudo)
            .into_iter()
            .flatten()
            .copied()
            .filter(|cp| interval.start <= cp.position && cp.position <= interval.end)
            .filter(|cp| exempt(cp, interval))
            .collect();
        for (&r, positions) in &clobbered_at {
            let inside = positions.partition_point(|&q| q <= interval.end)
                - positions.partition_point(|&q| q < interval.start);
            let excused_here = excused.iter().filter(|cp| cp.clobbers.contains(&r)).count();
            if inside > excused_here {
                forbidden.entry(interval.pseudo).or_default().insert(r);
            }
        }
    }
    forbidden
}

/// The width each constant pseudo is defined at, from its `SetVal` -- the
/// first, if there are several.
///
/// Built once for the chordal pre-pass, which asked it per interval by
/// scanning the whole function; that made the pre-pass intervals x
/// instructions.
pub fn setval_sizes(func: &Function) -> HashMap<PseudoId, u32> {
    let mut sizes = HashMap::new();
    for insn in func.blocks.iter().flat_map(|b| &b.insns) {
        if insn.op == Opcode::SetVal {
            if let Some(target) = insn.target {
                sizes.entry(target).or_insert(insn.size);
            }
        }
    }
    sizes
}

// Common Functions

/// Release stack slots whose owning interval ended before `point` back
/// to the free-slot pool, where future `try_reuse_stack_slot` calls
/// can find them.
///
/// The chordal allocator calls it twice per bank: once in Phase 1
/// sweeping monotonically by interval start, then once with
/// `usize::MAX` just before Phase 3 spill commits to drain everything
/// so spilled pseudos can reuse any non-interfering slot.
///
/// Without this, every `alloc_stack_slot` request creates a fresh
/// slot. On x86_64 that's wasteful but harmless. On aarch64 it pushes
/// stp/ldp offsets past the architectural [-512, 504] immediate range
/// and the assembler rejects the output.
pub fn expire_stack_intervals(
    active_stack: &mut Vec<ActiveSlot>,
    free_slots: &mut BTreeMap<i32, Vec<FreeSlot>>,
    point: usize,
) {
    let mut to_free: Vec<ActiveSlot> = Vec::new();
    active_stack.retain(|slot| {
        if slot.current.end < point {
            to_free.push(slot.clone());
            false
        } else {
            true
        }
    });
    for slot in to_free {
        let alignment = if slot.size >= 16 { 16 } else { 8 };
        // The freed slot's history is `past ++ [current]`.
        let mut owners = slot.past;
        owners.push(slot.current);
        // Merge with any existing FreeSlot at the same offset (an
        // earlier owner that already freed and is waiting for reuse).
        let slots = free_slots.entry(slot.size).or_default();
        if let Some(existing) = slots.iter_mut().find(|s| s.offset == slot.offset) {
            existing.owner_intervals.extend(owners);
        } else {
            slots.push(FreeSlot {
                offset: slot.offset,
                alignment,
                owner_intervals: owners,
            });
        }
    }
}

/// Find all positions of call-like instructions in a function.
/// Used by spill_args_across_calls and the chordal allocator's cross-call
/// caller-saved forbidding to identify where caller-saved registers
/// will be clobbered.
///
/// `is_call_like` decides per-opcode. The shared core opcodes are
/// `Call`, `Longjmp`, `Setjmp`; backends extend that list with any
/// IR opcodes whose codegen lowering emits a libc call (e.g. `Memcpy`,
/// `Memmove` on both arches). Without this, chordal coloring will happily put
/// a live pseudo into a caller-saved register that the codegen helper's
/// embedded libc call silently overwrites — see `memory/MEMORY.md`
pub fn find_call_positions(func: &Function, is_call_like: impl Fn(Opcode) -> bool) -> Vec<usize> {
    let mut call_positions = Vec::with_capacity(DEFAULT_CALL_POS_CAPACITY);
    let mut pos = 0usize;
    for block in &func.blocks {
        for insn in &block.insns {
            if is_call_like(insn.op) {
                call_positions.push(pos);
            }
            pos += 1;
        }
    }
    call_positions
}

/// The constraint point of a `__builtin_setjmp`, which clobbers every
/// register in `regs`: control comes back to it from a `__builtin_longjmp`
/// with only the frame and stack pointers restored, so no value may be in a
/// register across it. The result alone is exempt -- both paths write it,
/// after they join -- and not the buffer operand, which a value read again
/// later would otherwise keep in a register the resumed path has lost.
pub fn builtin_setjmp_constraint<R: Copy>(
    insn: &Instruction,
    regs: &[R],
) -> (Vec<R>, Vec<PseudoId>) {
    (regs.to_vec(), insn.target.into_iter().collect())
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

/// Result of liveness analysis: intervals, constraint points, and per-block liveness sets.
pub struct LivenessResult<R> {
    pub intervals: Vec<LiveInterval>,
    pub constraint_points: Vec<ConstraintPoint<R>>,
    /// Per-block live-in sets (indexed by block index)
    pub live_in: Vec<HashSet<PseudoId>>,
    /// Per-block live-out sets (indexed by block index)
    pub live_out: Vec<HashSet<PseudoId>>,
}

/// Compute live intervals and constraint points for a function.
///
/// This is the shared core of live interval computation used by both x86-64 and AArch64.
/// The `get_constraint_info` callback allows architecture-specific constraint handling.
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
    // Phase C's Boissinot Up_and_Mark traversal).
    let mut first_pos_map: Vec<HashMap<PseudoId, usize>> = vec![HashMap::new(); num_blocks];
    let mut last_pos_map: Vec<HashMap<PseudoId, usize>> = vec![HashMap::new(); num_blocks];
    let mut defined_in: Vec<HashSet<PseudoId>> = vec![HashSet::new(); num_blocks];
    // Per-pseudo set of defining blocks. The IR c17 hands to the
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
            if let Some(indirect) = insn.extra().indirect_target {
                first_pos_map[idx].entry(indirect).or_insert(ipos);
                last_pos_map[idx].insert(indirect, ipos);
            }
            if insn.op == Opcode::Asm {
                if let Some(asm) = &insn.extra().asm_data {
                    for input in &asm.inputs {
                        let p = input.pseudo;
                        first_pos_map[idx].entry(p).or_insert(ipos);
                        last_pos_map[idx].insert(p, ipos);
                    }
                    for output in &asm.outputs {
                        let p = output.pseudo;
                        // A *memory* output reads its pseudo: the pseudo holds
                        // the address the assembly writes through, so it is a
                        // use like any input. Recording it as a def made the
                        // address computation look dead on entry and let the
                        // allocator hand the register to another operand of the
                        // same asm.
                        if !output.is_memory() {
                            def_blocks.entry(p).or_default().insert(idx);
                            defined_in[idx].insert(p);
                        }
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

    // A constant, or a global's symbol, is never held in a register across
    // blocks: both backends' pre-passes give it an immediate or a global
    // location whatever its interval. With no def it would otherwise be
    // propagated from every use back to the entry block -- one pseudo live
    // across every block before its use -- and a function of n `if (x > i)`
    // statements, one constant each, held n^2 set entries: gigabytes, and
    // minutes, for n in the tens of thousands. Such a pseudo gets an interval
    // spanning its uses and no liveness. One with a def (a 128-bit constant's
    // `SetVal`, whose stack slot must survive to its last use) is tracked
    // like any value.
    let needs_no_liveness = |p: PseudoId| -> bool {
        !def_blocks.contains_key(&p)
            && func.get_pseudo(p).is_some_and(|ps| match &ps.kind {
                PseudoKind::Val(_) | PseudoKind::FVal(_) => true,
                PseudoKind::Sym(_) => func.local_of(p).is_none(),
                _ => false,
            })
    };

    let propagate_use = |use_block: usize,
                         pseudo: PseudoId,
                         live_in: &mut [HashSet<PseudoId>],
                         live_out: &mut [HashSet<PseudoId>],
                         worklist: &mut Vec<usize>| {
        if needs_no_liveness(pseudo) {
            return;
        }
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
            if let Some(indirect) = insn.extra().indirect_target {
                if !defined_so_far.contains(&indirect) {
                    propagate_use(idx, indirect, &mut live_in, &mut live_out, &mut worklist);
                }
            }
            if insn.op == Opcode::Asm {
                if let Some(asm) = &insn.extra().asm_data {
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
                        // See above: a memory output is a use of the address.
                        if output.is_memory() {
                            if !defined_so_far.contains(&output.pseudo) {
                                propagate_use(
                                    idx,
                                    output.pseudo,
                                    &mut live_in,
                                    &mut live_out,
                                    &mut worklist,
                                );
                            }
                        } else {
                            defined_so_far.insert(output.pseudo);
                        }
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

    // Phase D.1: the pseudos Phase C left without liveness span their uses.
    for idx in 0..num_blocks {
        for (&p, &first) in &first_pos_map[idx] {
            if !needs_no_liveness(p) {
                continue;
            }
            let last = last_pos_map[idx].get(&p).copied().unwrap_or(first);
            interval_start
                .entry(p)
                .and_modify(|s| *s = (*s).min(first))
                .or_insert(first);
            interval_end
                .entry(p)
                .and_modify(|e| *e = (*e).max(last))
                .or_insert(last);
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

    // Phase F: Build sorted LiveInterval vec. A local's interval is its
    // lifetime's, which liveness alone cannot see; see `local_lifetimes`.
    let lifetimes = local_lifetimes(func, &block_start_pos, &block_end_pos, &bb_id_to_idx);
    let mut result: Vec<LiveInterval> = interval_start
        .into_iter()
        .filter_map(|(pseudo, start)| {
            interval_end.get(&pseudo).map(|&end| {
                let (start, end) = lifetimes.get(&pseudo).copied().unwrap_or((start, end));
                LiveInterval {
                    pseudo,
                    start,
                    end,
                    in_loop: loop_pseudos.contains(&pseudo),
                }
            })
        })
        .collect();

    extend_across_returns_twice(
        func,
        &mut result,
        block_end_pos.last().copied().unwrap_or(0),
    );
    result.sort_by_key(|i| (i.start, i.pseudo.0));
    LivenessResult {
        intervals: result,
        constraint_points,
        live_in,
        live_out,
    }
}

/// Stretch every value live across a `setjmp` to the end of the function.
///
/// A `longjmp` resumes at the setjmp from whatever call it is made under,
/// and the CFG has no edge for that: a value live across the setjmp is read
/// again on the second return, after code the liveness analysis believed it
/// was dead in -- the path that led to the `longjmp`. A slot or register
/// handed to another value there is overwritten before the value is read
/// back. Every call after the setjmp may be the one that jumps, so the value
/// must survive to the end. This is the pseudo-level counterpart of
/// `local_lifetimes`, which spans every local over the whole function.
///
/// The setjmp's own result starts there, and is written again on each
/// return, so it is not stretched.
fn extend_across_returns_twice(func: &Function, intervals: &mut [LiveInterval], last: usize) {
    let positions: Vec<usize> = func
        .blocks
        .iter()
        .flat_map(|b| &b.insns)
        .enumerate()
        .filter(|(_, insn)| insn.op == Opcode::Setjmp)
        .map(|(pos, _)| pos)
        .collect();
    if positions.is_empty() {
        return;
    }
    for interval in intervals.iter_mut() {
        if positions
            .iter()
            .any(|&p| interval.start < p && p < interval.end)
        {
            interval.end = interval.end.max(last);
        }
    }
}

/// The frame slot a local takes, as `(bytes, alignment)`: its own size and
/// alignment, each at least eight -- or the alignment it was declared with.
/// One rule for both allocators.
///
/// Eight, and not the object's own size, is a contract the back ends rely
/// on: they move a register-passed value or an aggregate's tail a whole
/// eightbyte at a time, as the calling conventions classify them, so every
/// slot is a whole number of eightbytes. Packing a three-byte struct, a
/// `_Float16` or a plain `char` parameter at its own size was measured to
/// break each of those paths.
pub fn local_slot(
    local: &crate::ir::LocalVar,
    types: &TypeTable,
    pos: crate::diag::Position,
) -> (i32, i32) {
    let bytes = crate::abi::slot_bytes(types.size_bytes(local.typ), pos, "an automatic object");
    let align = local
        .explicit_align
        .map(|a| a as i32)
        .unwrap_or_else(|| (types.alignment(local.typ) as i32).max(8));
    (bytes.max(8), align)
}

/// Each local's frame slot, as `(local, offset)`, with `new_slot(bytes,
/// alignment)` making a fresh one.
///
/// Locals are laid out in declaration order, as they always were: the order
/// decides which of them land near the frame base, and an inline-asm memory
/// operand can name a local in place only while its displacement fits the
/// addressing mode -- small locals declared beside a large array must stay
/// on the near side of it. A local takes an earlier one's slot when the two
/// agree in size and alignment and its interval -- its lifetime, see
/// `local_lifetimes` -- overlaps none the slot has held.
///
/// A function with a stack-protector canary (`guarded`) lays its arrays out
/// first, `char` arrays before the rest, as gcc does: the canary is the slot
/// before them, so an array that overruns reaches it without passing over a
/// scalar the function may still read before it returns.
pub fn place_locals(
    func: &Function,
    types: &TypeTable,
    pos: crate::diag::Position,
    intervals: &[LiveInterval],
    guarded: bool,
    mut new_slot: impl FnMut(i32, i32) -> i32,
) -> Vec<(PseudoId, i32)> {
    struct Shared {
        bytes: i32,
        align: i32,
        offset: i32,
        owners: Vec<(usize, usize)>,
    }
    let lifetime: HashMap<PseudoId, (usize, usize)> = intervals
        .iter()
        .map(|i| (i.pseudo, (i.start, i.end)))
        .collect();
    let mut locals: Vec<&crate::ir::LocalVar> = func
        .locals
        .values()
        .filter(|l| lifetime.contains_key(&l.sym))
        .collect();
    if guarded {
        locals.sort_by_key(|l| (crate::arch::stack_protect::placement(l.typ, types), l.sym.0));
    } else {
        locals.sort_by_key(|l| l.sym.0);
    }
    let mut slots: Vec<Shared> = Vec::new();
    let mut placed = Vec::with_capacity(locals.len());
    for local in locals {
        let (bytes, align) = local_slot(local, types, pos);
        let (start, end) = lifetime[&local.sym];
        let free = slots.iter_mut().find(|s| {
            s.bytes == bytes
                && s.align >= align
                && s.owners.iter().all(|&(os, oe)| oe < start || end < os)
        });
        let offset = match free {
            Some(s) => {
                s.owners.push((start, end));
                s.offset
            }
            None => {
                let offset = new_slot(bytes, align);
                slots.push(Shared {
                    bytes,
                    align,
                    offset,
                    owners: vec![(start, end)],
                });
                offset
            }
        };
        placed.push((local.sym, offset));
    }
    placed
}

/// The positions each local's object may hold a value at, as `[start, end]`:
/// the hull of every point where it may be inside its lifetime.
///
/// A local is a `Sym`, which no instruction defines, so ordinary liveness
/// finds each of its uses upward-exposed and carries it to function entry:
/// every local overlapped every other from position 0, and no frame slot was
/// ever free to share. What bounds an object is its lifetime instead. It is
/// "may be live" forward from any instruction that mentions it -- the first
/// mention of an object nothing has written is its start, and a jump past
/// its declaration reaches a mention all the same -- until a `LifetimeEnd`
/// for it, which the linearizer puts where control falls out of the block
/// that declared it. A path leaving that block any other way simply carries
/// the object further, the safe direction. A use after the end is undefined
/// behaviour (C17 6.2.4p2), so a pointer into the object, escaped or not,
/// needs no tracking of its own.
///
/// A parameter's local is written by the prologue before any instruction
/// mentions it, so it is live from entry. In a function that calls
/// `setjmp`, control can come back to a point the analysis cannot see, and
/// every local spans the whole function.
fn local_lifetimes(
    func: &Function,
    block_start_pos: &[usize],
    block_end_pos: &[usize],
    bb_id_to_idx: &HashMap<BasicBlockId, usize>,
) -> HashMap<PseudoId, (usize, usize)> {
    let locals: Vec<PseudoId> = func.locals.values().map(|l| l.sym).collect();
    let last = block_end_pos.last().copied().unwrap_or(0);
    let returns_twice = func
        .blocks
        .iter()
        .flat_map(|b| &b.insns)
        .any(|i| matches!(i.op, Opcode::Setjmp | Opcode::Longjmp));
    if returns_twice {
        return locals.into_iter().map(|l| (l, (0, last))).collect();
    }
    let index: HashMap<PseudoId, usize> = locals.iter().enumerate().map(|(i, &l)| (l, i)).collect();
    let words = locals.len().div_ceil(64);
    let set = |bits: &mut Vec<u64>, i: usize| bits[i / 64] |= 1 << (i % 64);
    let clear = |bits: &mut Vec<u64>, i: usize| bits[i / 64] &= !(1 << (i % 64));
    let has = |bits: &[u64], i: usize| bits[i / 64] & (1 << (i % 64)) != 0;

    // Each block's events in order: (position, local, starts) -- a mention
    // starts (or continues) a lifetime, a `LifetimeEnd` ends one.
    let mut events: Vec<Vec<(usize, usize, bool)>> = vec![Vec::new(); func.blocks.len()];
    for (b, block) in func.blocks.iter().enumerate() {
        for (pos, insn) in (block_start_pos[b]..).zip(&block.insns) {
            if insn.op == Opcode::LifetimeEnd {
                if let Some(&i) = insn.extra().lifetime_of.and_then(|l| index.get(&l)) {
                    events[b].push((pos, i, false));
                }
                continue;
            }
            for p in insn.mentioned() {
                if let Some(&i) = index.get(&p) {
                    events[b].push((pos, i, true));
                }
            }
        }
    }

    // Forward "may be inside its lifetime", to a fixed point.
    let mut live_in: Vec<Vec<u64>> = vec![vec![0; words]; func.blocks.len()];
    let mut live_out: Vec<Vec<u64>> = vec![vec![0; words]; func.blocks.len()];
    let params: HashSet<&str> = func.params.iter().map(|(n, _)| n.as_str()).collect();
    if let Some(&entry) = bb_id_to_idx.get(&func.entry) {
        for (name, local) in &func.locals {
            if params.contains(name.as_str()) {
                set(&mut live_in[entry], index[&local.sym]);
            }
        }
    }
    let mut work: VecDeque<usize> = (0..func.blocks.len()).collect();
    let mut queued = vec![true; func.blocks.len()];
    while let Some(b) = work.pop_front() {
        queued[b] = false;
        let mut out = live_in[b].clone();
        for &(_, i, starts) in &events[b] {
            if starts {
                set(&mut out, i);
            } else {
                clear(&mut out, i);
            }
        }
        if out == live_out[b] {
            continue;
        }
        live_out[b] = out;
        for child in &func.blocks[b].children {
            let Some(&c) = bb_id_to_idx.get(child) else {
                continue;
            };
            let mut changed = false;
            for (w, word) in live_in[c].iter_mut().enumerate() {
                let merged = *word | live_out[b][w];
                changed |= merged != *word;
                *word = merged;
            }
            if changed && !queued[c] {
                queued[c] = true;
                work.push_back(c);
            }
        }
    }

    // The hull: a block's start where the object is live in, its end where
    // live out, and every event in between.
    let mut hull: HashMap<PseudoId, (usize, usize)> = HashMap::new();
    let mut widen = |i: usize, pos: usize| {
        hull.entry(locals[i])
            .and_modify(|(s, e)| {
                *s = (*s).min(pos);
                *e = (*e).max(pos);
            })
            .or_insert((pos, pos));
    };
    for b in 0..func.blocks.len() {
        for i in 0..locals.len() {
            if has(&live_in[b], i) {
                widen(i, block_start_pos[b]);
            }
            if has(&live_out[b], i) {
                widen(i, block_end_pos[b]);
            }
        }
        for &(pos, i, _) in &events[b] {
            widen(i, pos);
        }
    }
    hull
}

/// Each pseudo's live interval, by pseudo -- the first, if several.
///
/// Built once per coloring pass. Finding one by scanning every interval,
/// once per spilled pseudo, made spilling intervals x spills: a function of
/// tens of thousands of locals spent its time there.
pub fn intervals_by_pseudo(intervals: &[LiveInterval]) -> HashMap<PseudoId, &LiveInterval> {
    let mut by_pseudo = HashMap::with_capacity(intervals.len());
    for interval in intervals {
        by_pseudo.entry(interval.pseudo).or_insert(interval);
    }
    by_pseudo
}

/// Every pseudo live out of some block: one whose value crosses a block
/// boundary.
///
/// Built once per function. Asking it per interval by scanning every block's
/// live-out set made the chordal pre-pass intervals x blocks.
pub fn live_out_anywhere(live_out: &[HashSet<PseudoId>]) -> HashSet<PseudoId> {
    live_out.iter().flatten().copied().collect()
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
/// Note: FP comparisons (`Opcode::is_float_comparison`) produce integer results (0 or 1), so their
/// targets should NOT be in FP registers.
/// Every argument pseudo with the declared type of the parameter it carries.
///
/// What an argument *is* comes from the parameter list, not from the
/// instructions that happen to use it. The classifications below used to be
/// inferred from uses alone, which worked only while every parameter was
/// copied into a typed pseudo at entry: once copy propagation let a
/// `__float128` argument flow straight into a call, nothing typed it, and it
/// got an 8-byte slot it was stored into with `movsd` and reloaded from with
/// `movups`.
pub fn arg_pseudo_types(func: &Function) -> Vec<(PseudoId, TypeId)> {
    let args = func.arg_types();
    func.pseudos
        .iter()
        .filter_map(|p| match p.kind {
            PseudoKind::Arg(n) => args.of(n).map(|t| (p.id, t)),
            _ => None,
        })
        .collect()
}

pub fn identify_fp_pseudos<F>(func: &Function, is_float_type: F) -> HashSet<PseudoId>
where
    F: Fn(TypeId) -> bool,
{
    use crate::ir::Opcode;

    let mut fp_pseudos = HashSet::with_capacity(DEFAULT_SMALL_VEC_CAPACITY);

    // Mark FVal constants as FP
    for pseudo in &func.pseudos {
        if matches!(pseudo.kind, PseudoKind::FVal(_)) {
            fp_pseudos.insert(pseudo.id);
        }
    }
    for (p, t) in arg_pseudo_types(func) {
        if is_float_type(t) {
            fp_pseudos.insert(p);
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
                    | Opcode::Simd(_)
            );

            if is_fp_producing_op {
                if let Some(target) = insn.target {
                    fp_pseudos.insert(target);
                }
            }

            // The result type decides the rest. A comparison's is an
            // integer whatever it compared, so its 0 or 1 lands in a general
            // register, where its emitter writes it.
            if let Some(typ) = insn.typ {
                if is_float_type(typ) {
                    if let Some(target) = insn.target {
                        fp_pseudos.insert(target);
                    }
                }
            }
        }
    }

    fp_pseudos
}

// ============================================================================
// Backend orchestration helpers
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

/// Does anything overwrite `reg` while `interval` is live -- a constraint
/// point that clobbers it and does not exempt the interval's pseudo?
///
/// The coloring pass asks the same question of the pseudos it places, through
/// [`constraint_clobbers`], which is where each backend's `exempt` rule comes
/// from; an ABI-pinned argument never reaches that pass, so
/// [`spill_gp_args_across`] asks it here.
pub fn clobbered_while_live<R: PartialEq>(
    interval: &LiveInterval,
    reg: R,
    constraint_points: &[ConstraintPoint<R>],
    exempt: impl Fn(&ConstraintPoint<R>, &LiveInterval) -> bool,
) -> bool {
    constraint_points.iter().any(|cp| {
        interval.start <= cp.position
            && cp.position <= interval.end
            && cp.clobbers.contains(&reg)
            && !exempt(cp, interval)
    })
}

/// Move an ABI-pinned GP argument out of its register when something
/// overwrites that register while the argument is live.
///
/// An argument arrives pre-colored in its ABI register and never goes through
/// coloring, so the forbidden-color machinery that keeps an ordinary value out
/// of a clobbered register does not apply to it. `overwritten` says whether its
/// register is destroyed within its interval: a call (every argument register
/// is caller-saved), or a constraint point that clobbers it
/// ([`clobbered_while_live`]) -- inline asm, an atomic's fixed scratch, a
/// thread-local sequence that calls a resolver or getter. One rule for both
/// backends: aarch64 once asked only about calls, so a thread-local access
/// under the descriptor or Mach-O TLV model, which returns its result in x0,
/// left every later use of an argument that arrived in x0 reading the
/// thread-local's address.
///
/// The caller supplies which registers are argument registers, how to extract
/// a register from a `Loc`, how to construct the spilled `Loc`, and how to
/// record the spill for the prologue. The stack-offset sign convention is
/// folded into `mk_stack_loc` and `record_spill`, so this helper is
/// sign-agnostic.
#[allow(clippy::too_many_arguments)]
pub fn spill_gp_args_across<L, R, IsArg, ExtractReg, MkStackLoc, RecordSpill, PushFree>(
    intervals: &[LiveInterval],
    overwritten: impl Fn(&LiveInterval, R) -> bool,
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
        if !overwritten(interval, reg) {
            continue;
        }
        *stack_offset += 8;
        let to_offset = *stack_offset;
        record_spill(interval.pseudo, reg, to_offset);
        locations.insert(interval.pseudo, mk_stack_loc(to_offset));
        push_free(reg);
    }
}

// SSA interference graphs are chordal (Pereira/Palsberg 2005, Hack
// 2005): a perfect elimination ordering exists and greedy coloring on
// that ordering achieves the chromatic number. The two-step recipe:
//   1. Maximum Cardinality Search (MCS) on the interference graph
//      yields a perfect elimination ordering.
//   2. Greedy color in MCS order: pick the lowest-index register not
//      used by any already-colored neighbor.
//
// c17's IR is not strictly SSA at the allocator boundary —
// `lower::eliminate_phi_nodes` introduces multi-def Copy patterns. The
// interference graph is still well-defined: vertex u interferes with
// vertex v iff u and v are simultaneously live somewhere in the
// function. It is built with a per-instruction backward walk (the
// textbook "for each def, add edges to currently-live" pattern) rather
// than a coarser block-level all-pairs, which would over-constrain
// coloring with spurious edges.
//
// Constraint pre-coloring (e.g. x86_64 `idiv` clobbers RAX/RDX) is
// expressed via per-pseudo `forbidden` colors passed to greedy_color.
// ABI-pinned arg pseudos arrive via `pre_colored`. Physical registers
// don't need to be modeled as graph vertices.
//
// BTreeMap / BTreeSet throughout: colouring walks these in iteration
// order and the colour a pseudo gets depends on that order, so a
// hash-ordered container would make register assignment differ between
// runs of the same compiler on the same input.

/// Per-pseudo set of interfering pseudos for one register bank.
#[derive(Debug, Default)]
pub struct InterferenceGraph {
    /// Adjacency map. `edges[p]` is the set of pseudos that interfere
    /// with `p` and belong to the same register bank.
    pub edges: BTreeMap<PseudoId, std::collections::BTreeSet<PseudoId>>,
    /// All vertices, including isolated ones (no edges).
    pub vertices: std::collections::BTreeSet<PseudoId>,
}

impl InterferenceGraph {
    pub fn new() -> Self {
        Self {
            edges: BTreeMap::new(),
            vertices: std::collections::BTreeSet::new(),
        }
    }

    pub fn add_vertex(&mut self, p: PseudoId) {
        self.vertices.insert(p);
        self.edges.entry(p).or_default();
    }

    pub fn add_edge(&mut self, a: PseudoId, b: PseudoId) {
        if a == b {
            return;
        }
        self.add_vertex(a);
        self.add_vertex(b);
        self.edges.entry(a).or_default().insert(b);
        self.edges.entry(b).or_default().insert(a);
    }

    pub fn neighbors(&self, p: PseudoId) -> impl Iterator<Item = PseudoId> + '_ {
        self.edges
            .get(&p)
            .into_iter()
            .flat_map(|s| s.iter().copied())
    }
}

/// Build the interference graph for pseudos in `candidates`.
///
/// Walks each basic block backward maintaining the current live set.
/// For each instruction, the new defs interfere with everything that
/// is live just after the instruction (i.e. before the backward step
/// removes them); the def-vs-live edges must be added BEFORE removing
/// the def from `live`, or edges between same-block adjacent def/use
/// pairs are silently dropped.
///
/// Additionally, each def interferes with the instruction's own
/// sources. Some codegen lowerings (notably x86_64 `cmov` for ternary
/// `(cond) ? a : b`) materialize the target into a register and then
/// read source values, which would clobber a source that shares a
/// register with the target. The def-vs-src edge enforces disjoint
/// registers in that case at the cost of an occasional extra move
/// when the codegen would have allowed sharing. Net effect on .text
/// size is small; without these edges, register allocation silently
/// corrupts cmov/conditional-store patterns.
pub fn build_interference_graph(
    candidates: &std::collections::BTreeSet<PseudoId>,
    func: &Function,
    live_out: &[HashSet<PseudoId>],
    add_def_src_edges: bool,
) -> InterferenceGraph {
    let mut graph = InterferenceGraph::new();
    for &c in candidates {
        graph.add_vertex(c);
    }
    for (idx, block) in func.blocks.iter().enumerate() {
        let mut live: std::collections::BTreeSet<PseudoId> = live_out[idx]
            .iter()
            .copied()
            .filter(|p| candidates.contains(p))
            .collect();
        for insn in block.insns.iter().rev() {
            // Collect defs of this instruction.
            let mut defs: Vec<PseudoId> = Vec::new();
            if let Some(target) = insn.target {
                if candidates.contains(&target) {
                    defs.push(target);
                }
            }
            if insn.op == Opcode::Asm {
                if let Some(asm) = &insn.extra().asm_data {
                    for output in &asm.outputs {
                        // A memory output defines nothing; it reads an address.
                        if !output.is_memory() && candidates.contains(&output.pseudo) {
                            defs.push(output.pseudo);
                        }
                    }
                }
            }
            // An early-clobber asm output is written before the template
            // has read its inputs, so it interferes with every one of them
            // -- a register input, and the address of a memory operand, which
            // the template also reads -- even an input that dies here, whose
            // register a plain output may take. A tied input names the
            // output's own pseudo and so shares with it by construction.
            if insn.op == Opcode::Asm {
                if let Some(asm) = &insn.extra().asm_data {
                    add_early_clobber_edges(&mut graph, asm, candidates);
                }
            }
            // Each def interferes with everything currently live AND
            // with the instruction's other defs AND with each src
            // (for the lowering-correctness reason described above).
            for &d in &defs {
                for &l in live.iter() {
                    if l != d {
                        graph.add_edge(d, l);
                    }
                }
                for &d2 in &defs {
                    if d != d2 {
                        graph.add_edge(d, d2);
                    }
                }
                if add_def_src_edges {
                    for &src in &insn.src {
                        if src != d && candidates.contains(&src) {
                            graph.add_edge(d, src);
                        }
                    }
                }
            }
            // Remove defs from live (they were born here; not live before).
            for &d in &defs {
                live.remove(&d);
            }
            // Add uses (srcs are live coming INTO this instruction).
            for &src in &insn.src {
                if candidates.contains(&src) {
                    live.insert(src);
                }
            }
            if let Some(indirect) = insn.extra().indirect_target {
                if candidates.contains(&indirect) {
                    live.insert(indirect);
                }
            }
            if insn.op == Opcode::Asm {
                if let Some(asm) = &insn.extra().asm_data {
                    for input in &asm.inputs {
                        if candidates.contains(&input.pseudo) {
                            live.insert(input.pseudo);
                        }
                    }
                    // A memory output is read, not written: its address must
                    // stay live across the asm.
                    for output in &asm.outputs {
                        if output.is_memory() && candidates.contains(&output.pseudo) {
                            live.insert(output.pseudo);
                        }
                    }
                }
            }
        }
    }
    graph
}

/// Edges from each early-clobber register output of one asm statement to
/// every pseudo the statement reads: its inputs, and the addresses of its
/// memory outputs.
fn add_early_clobber_edges(
    graph: &mut InterferenceGraph,
    asm: &crate::ir::AsmData,
    candidates: &std::collections::BTreeSet<PseudoId>,
) {
    let read = asm
        .inputs
        .iter()
        .chain(asm.outputs.iter().filter(|o| o.is_memory()))
        .map(|c| c.pseudo)
        .filter(|p| candidates.contains(p));
    let read: Vec<PseudoId> = read.collect();
    for out in &asm.outputs {
        if !out.is_early_clobber() || out.is_memory() || !candidates.contains(&out.pseudo) {
            continue;
        }
        for &p in &read {
            // `add_edge` ignores a self-edge: a tied input is the output.
            graph.add_edge(out.pseudo, p);
        }
    }
}

/// Walk `func` and collect every `Opcode::Copy` instruction's
/// `(target, src)` pair as a coalescing candidate. Skips Copies
/// without a target or with `src.len() != 1` (degenerate cases).
pub fn find_copy_coalesce_candidates(func: &Function) -> Vec<(PseudoId, PseudoId)> {
    let mut out = Vec::new();
    for block in &func.blocks {
        for insn in &block.insns {
            if insn.op != Opcode::Copy {
                continue;
            }
            if insn.src.len() != 1 {
                continue;
            }
            if let (Some(target), Some(&src)) = (insn.target, insn.src.first()) {
                out.push((target, src));
            }
        }
    }
    out
}

/// The pseudos an inline-asm template needs in registers: register-class
/// inputs and outputs, and the address of a memory operand that is a
/// run-time value rather than a named object.
///
/// gcc guarantees each such operand a register at the asm, spilling other
/// values to make room. Colored in ordinary order, an operand could be the
/// value the allocator chose to spill, and then codegen had to find it a
/// register at the last moment -- on x86-64 by borrowing one that held another
/// operand or a live value.
pub fn asm_register_operands(func: &Function) -> std::collections::BTreeSet<PseudoId> {
    let mut out = std::collections::BTreeSet::new();
    for insn in func.blocks.iter().flat_map(|b| &b.insns) {
        let Some(asm) = insn
            .extra()
            .asm_data
            .as_ref()
            .filter(|_| insn.op == Opcode::Asm)
        else {
            continue;
        };
        for c in asm.outputs.iter().chain(asm.inputs.iter()) {
            // A memory operand whose address is a run-time value needs that
            // address in a base register just as much, which is how gcc
            // treats it. One that names its object's `Sym` is addressed in
            // place and needs none.
            let wants = if c.is_memory() {
                !func
                    .get_pseudo(c.pseudo)
                    .is_some_and(|p| matches!(p.kind, crate::ir::PseudoKind::Sym(_)))
            } else {
                c.wants_register()
            };
            if wants {
                out.insert(c.pseudo);
            }
        }
    }
    out
}

/// `order` with the asm register operands moved to the front, each group
/// keeping its relative order, so greedy coloring places them first.
pub fn asm_operands_first(
    order: Vec<PseudoId>,
    asm_ops: &std::collections::BTreeSet<PseudoId>,
) -> Vec<PseudoId> {
    let (mut first, rest): (Vec<PseudoId>, Vec<PseudoId>) =
        order.into_iter().partition(|p| asm_ops.contains(p));
    first.extend(rest);
    first
}

/// Maximum Cardinality Search (MCS) ordering. The reverse of this
/// ordering is a perfect elimination ordering when the graph is
/// chordal; greedy coloring in MCS order yields an optimal coloring.
///
/// Algorithm: start with all weights at 0. Repeatedly pick an
/// unprocessed vertex with maximum weight (ties broken by smallest
/// pseudo id for determinism), add it to the ordering, increment
/// weight of each unprocessed neighbor.
///
/// The unprocessed vertices are kept ordered by `(weight, Reverse(id))`, so
/// the pick is the last entry rather than a scan of all of them: the scan made
/// ordering quadratic in the vertex count, minutes for a function of a few
/// thousand branches.
pub fn mcs_ordering(graph: &InterferenceGraph) -> Vec<PseudoId> {
    use std::cmp::Reverse;
    // Weight of each vertex not yet ordered; absent once it is.
    let mut weight: HashMap<PseudoId, usize> = graph.vertices.iter().map(|&v| (v, 0)).collect();
    let mut queue: std::collections::BTreeSet<(usize, Reverse<PseudoId>)> =
        graph.vertices.iter().map(|&v| (0, Reverse(v))).collect();
    let mut order: Vec<PseudoId> = Vec::with_capacity(graph.vertices.len());
    // Max weight; among equals the greatest `Reverse`, i.e. the smallest id.
    while let Some((_, Reverse(pick))) = queue.pop_last() {
        weight.remove(&pick);
        order.push(pick);
        for n in graph.neighbors(pick) {
            if let Some(w) = weight.get_mut(&n) {
                queue.remove(&(*w, Reverse(n)));
                *w += 1;
                queue.insert((*w, Reverse(n)));
            }
        }
    }
    order
}

/// Result of greedy coloring for one register bank.
pub struct ColoringResult<R> {
    pub colors: BTreeMap<PseudoId, R>,
    pub spilled: Vec<PseudoId>,
}

/// Greedy color pseudos in `order` using registers from `palette`.
///
/// * `pre_colored` — forced color assignments (e.g. ABI-pinned args).
///   These are honored unchanged.
/// * `forbidden` — per-vertex sets of colors that MUST NOT be assigned
///   (correctness constraint, e.g. x86_64 cross-call pseudos forbidden
///   from caller-saved, or pseudos live across `idiv` forbidden from
///   RAX/RDX).
/// * `preferred_palette` — biases the search order for each vertex
///   (e.g. prefer callee-saved for in-loop pseudos). Returns the
///   subset to try first; the full `palette` is searched as a
///   fallback.
///
/// Vertices that cannot be colored land in `spilled`.
pub fn greedy_color<R, PrefFn>(
    graph: &InterferenceGraph,
    order: &[PseudoId],
    palette: &[R],
    pre_colored: &BTreeMap<PseudoId, R>,
    forbidden: &BTreeMap<PseudoId, std::collections::BTreeSet<R>>,
    preferred_palette: PrefFn,
) -> ColoringResult<R>
where
    R: Copy + Eq + Ord + Hash,
    PrefFn: Fn(PseudoId) -> Option<Vec<R>>,
{
    let mut colors: BTreeMap<PseudoId, R> = pre_colored.clone();
    let mut spilled: Vec<PseudoId> = Vec::new();
    let empty: std::collections::BTreeSet<R> = std::collections::BTreeSet::new();
    for &v in order {
        if colors.contains_key(&v) {
            continue;
        }
        let used: std::collections::BTreeSet<R> = graph
            .neighbors(v)
            .filter_map(|n| colors.get(&n).copied())
            .collect();
        let forbid = forbidden.get(&v).unwrap_or(&empty);
        let prefer = preferred_palette(v);
        let pick = prefer
            .as_ref()
            .and_then(|p| {
                p.iter()
                    .find(|r| !used.contains(r) && !forbid.contains(r))
                    .copied()
            })
            .or_else(|| {
                palette
                    .iter()
                    .find(|r| !used.contains(r) && !forbid.contains(r))
                    .copied()
            });
        if let Some(r) = pick {
            colors.insert(v, r);
        } else {
            spilled.push(v);
        }
    }
    ColoringResult { colors, spilled }
}

/// For each pseudo, the sorted positions at which it is used (read).
/// Belady eviction uses this for "next-use distance" — when register
/// pressure forces a spill, the pseudo with the furthest next use is
/// the cheapest to evict (reloading later costs less than reloading
/// sooner).
pub fn compute_use_positions(func: &Function) -> BTreeMap<PseudoId, Vec<usize>> {
    let mut uses: BTreeMap<PseudoId, Vec<usize>> = BTreeMap::new();
    let mut pos = 0usize;
    for block in &func.blocks {
        for insn in &block.insns {
            for &src in &insn.src {
                uses.entry(src).or_default().push(pos);
            }
            if let Some(indirect) = insn.extra().indirect_target {
                uses.entry(indirect).or_default().push(pos);
            }
            if insn.op == Opcode::Asm {
                if let Some(asm) = &insn.extra().asm_data {
                    for input in &asm.inputs {
                        uses.entry(input.pseudo).or_default().push(pos);
                    }
                    // A memory output's address is used here too, so the
                    // next-use distance must count it.
                    for output in &asm.outputs {
                        if output.is_memory() {
                            uses.entry(output.pseudo).or_default().push(pos);
                        }
                    }
                }
            }
            pos += 1;
        }
    }
    uses
}

/// Distance from position `p` to the next use of `pseudo`, or
/// `usize::MAX` if there is no remaining use.
pub fn next_use_distance(
    uses: &BTreeMap<PseudoId, Vec<usize>>,
    pseudo: PseudoId,
    p: usize,
) -> usize {
    let Some(positions) = uses.get(&pseudo) else {
        return usize::MAX;
    };
    let idx = positions.partition_point(|&q| q <= p);
    if idx < positions.len() {
        positions[idx] - p
    } else {
        usize::MAX
    }
}

/// Try to reuse a previously freed stack slot of the given size and alignment.
///
/// Reuse is safe iff `candidate_interval` is disjoint from **every**
/// past owner of the slot. Two intervals `[a.start, a.end]` and
/// `[b.start, b.end]` are disjoint iff `a.end < b.start` or
/// `b.end < a.start`.
///
/// History matters: when slot S is freed by A and reused by B, the
/// slot's "current owner" is B but A's interval still constrains any
/// future candidate. The earlier implementation checked only the
/// most-recent owner — when A.lifetime [11,17] and B.lifetime
/// [81,130] were both past owners, a candidate C with [6,42] saw
/// only B (no conflict) but actually collided with A's [11,17].
/// CPython's `double_round` miscompile (the assertAlmostEqual
/// segfault) reproduced this exact pattern.
///
/// When a slot is reused, the existing `FreeSlot` entry is removed
/// from `free_stack_slots`; the *new* owner's interval is appended to
/// the entry when the slot is freed again by `expire_stack_intervals`.
/// This preserves the full ownership history across the reuse cycle.
/// To make the history preservation work, this function returns
/// (offset, history) so the caller can stash history into
/// `active_stack` and have it re-merged on expire.
pub fn try_reuse_stack_slot(
    free_stack_slots: &mut BTreeMap<i32, Vec<FreeSlot>>,
    size: i32,
    alignment: i32,
    candidate_interval: &LiveInterval,
) -> Option<(i32, Vec<LiveInterval>)> {
    if let Some(slots) = free_stack_slots.get_mut(&size) {
        if let Some(idx) = slots.iter().position(|s| {
            if s.alignment < alignment {
                return false;
            }
            s.owner_intervals.iter().all(|owner| {
                owner.end < candidate_interval.start || candidate_interval.end < owner.start
            })
        }) {
            let slot = slots.remove(idx);
            if slots.is_empty() {
                free_stack_slots.remove(&size);
            }
            return Some((slot.offset, slot.owner_intervals));
        }
    }
    None
}

/// A function's incoming-argument pseudos, indexed for the allocators.
///
/// Built once per function: it indexes the pseudos by `Arg(n)` for O(1)
/// lookup and detects the hidden sret pointer. How each argument is *passed*
/// is the backend's own layout -- on aarch64 `param_layout`, the one rule
/// `va_start` reads as well.
pub struct AbiLowering {
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

impl AbiLowering {
    /// Build an `AbiLowering` for `func`.
    pub fn new(func: &Function) -> Self {
        // Detect the hidden return pointer for large struct returns.
        // The linearizer emits it as `Arg(0)` with the literal name
        // `__sret`, shifting all normal-parameter `Arg(n)` indices by 1.
        let sret_pseudo = func.sret_arg();
        let arg_idx_offset: u32 = if sret_pseudo.is_some() { 1 } else { 0 };

        // Index pseudos by Arg(n): O(P) once, O(1) per argument.
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
            arg_pseudos,
            sret_pseudo,
            arg_idx_offset,
        }
    }

    /// The declared type of the parameter an `Arg(arg)` pseudo carries, or
    /// `None` for the hidden sret pointer, which is no declared parameter.
    pub fn param_type(&self, func: &Function, arg: u32) -> Option<TypeId> {
        crate::ir::ArgTypes::new(self.sret_pseudo, &func.params).of(arg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// One block: `a` used and ended, then `b` used and ended, a parameter's
    /// local `p` read at the end, and a `setjmp` when `returns_twice`.
    fn two_scopes(returns_twice: bool) -> Function {
        use crate::ir::{BasicBlock, Pseudo};
        let types = TypeTable::new(&crate::target::Target::host());
        let mut f = Function::new("f", types.int_id);
        f.add_param("p", types.int_id);
        for (id, name) in [(0, "a.0"), (1, "b.1"), (2, "p")] {
            f.add_pseudo(Pseudo::sym(PseudoId(id), name.into()));
            f.add_local(name, PseudoId(id), types.long_id, None, None);
        }
        for id in 3..7 {
            f.add_pseudo(Pseudo::reg(PseudoId(id), id));
        }
        f.next_pseudo = 8;
        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        if returns_twice {
            bb.add_insn(Instruction::new(Opcode::Setjmp));
        }
        bb.add_insn(Instruction::load(
            PseudoId(3),
            PseudoId(0),
            0,
            types.long_id,
            64,
        ));
        bb.add_insn(Instruction::lifetime_end(PseudoId(0)));
        bb.add_insn(Instruction::load(
            PseudoId(4),
            PseudoId(1),
            0,
            types.long_id,
            64,
        ));
        bb.add_insn(Instruction::lifetime_end(PseudoId(1)));
        bb.add_insn(Instruction::load(
            PseudoId(5),
            PseudoId(2),
            0,
            types.int_id,
            32,
        ));
        bb.add_insn(Instruction::ret(Some(PseudoId(5))));
        f.add_block(bb);
        f.entry = BasicBlockId(0);
        f
    }

    fn lifetime_of(f: &Function, p: u32) -> (usize, usize) {
        let r = compute_live_intervals(f, |_| None::<(Vec<()>, Vec<PseudoId>)>);
        let i = r
            .intervals
            .iter()
            .find(|i| i.pseudo == PseudoId(p))
            .unwrap();
        (i.start, i.end)
    }

    /// A local's interval is its lifetime: from its first mention to its
    /// `LifetimeEnd`, so two locals whose lifetimes do not overlap can share
    /// a slot -- and a parameter's local, which the prologue writes, is live
    /// from entry.
    #[test]
    fn a_local_is_live_from_its_first_mention_to_its_end() {
        let f = two_scopes(false);
        let (a, b, p) = (lifetime_of(&f, 0), lifetime_of(&f, 1), lifetime_of(&f, 2));
        assert_eq!(a, (1, 2), "a");
        assert_eq!(b, (3, 4), "b");
        assert!(a.1 < b.0, "disjoint: {a:?} {b:?}");
        assert_eq!(p.0, 0, "a parameter's local from entry: {p:?}");
    }

    /// `%3 = load p; %4 = setjmp; cbr %4, .L1, .L2; .L1: ret %3;
    /// .L2: %5 = load p; ret %5` -- `%3` is last read in `.L1`, laid out
    /// before `.L2`, which is where the `longjmp` comes from. Its interval
    /// reaches the end of the function, or `.L2` could take its slot and the
    /// second return read `%5`. `%5`, defined after the setjmp, and the
    /// setjmp's own result are not stretched.
    fn value_across_setjmp(kind: crate::parse::ast::JmpKind) -> Function {
        use crate::ir::{BasicBlock, Pseudo};
        let types = TypeTable::new(&crate::target::Target::host());
        let mut f = Function::new("f", types.int_id);
        f.add_param("p", types.int_id);
        f.add_pseudo(Pseudo::sym(PseudoId(2), "p".into()));
        f.add_local("p", PseudoId(2), types.int_id, None, None);
        for id in 3..6 {
            f.add_pseudo(Pseudo::reg(PseudoId(id), id));
        }
        f.next_pseudo = 6;
        let (b0, b1, b2) = (BasicBlockId(0), BasicBlockId(1), BasicBlockId(2));
        let mut entry = BasicBlock::new(b0);
        entry.add_insn(Instruction::new(Opcode::Entry));
        entry.add_insn(Instruction::load(
            PseudoId(3),
            PseudoId(2),
            0,
            types.int_id,
            32,
        ));
        let mut setjmp = Instruction::new(Opcode::Setjmp)
            .with_target(PseudoId(4))
            .with_type_and_size(types.int_id, 32);
        setjmp.extra_mut().jmp_kind = kind;
        entry.add_insn(setjmp);
        entry.add_insn(Instruction::cbr(PseudoId(4), b1, b2));
        entry.children = vec![b1, b2];
        let mut resumed = BasicBlock::new(b1);
        resumed.add_insn(Instruction::ret(Some(PseudoId(3))));
        resumed.parents = vec![b0];
        let mut direct = BasicBlock::new(b2);
        direct.add_insn(Instruction::load(
            PseudoId(5),
            PseudoId(2),
            0,
            types.int_id,
            32,
        ));
        direct.add_insn(Instruction::ret(Some(PseudoId(5))));
        direct.parents = vec![b0];
        for b in [entry, resumed, direct] {
            f.add_block(b);
        }
        f.entry = b0;
        f
    }

    #[test]
    fn a_value_live_across_setjmp_spans_to_the_end() {
        use crate::parse::ast::JmpKind;
        for kind in [JmpKind::Library, JmpKind::Builtin] {
            let f = value_across_setjmp(kind);
            let last = 6;
            assert_eq!(
                lifetime_of(&f, 3),
                (1, last),
                "{kind:?}: the value read on return"
            );
            assert_eq!(
                lifetime_of(&f, 4).0,
                2,
                "{kind:?}: the result starts at the setjmp"
            );
            assert!(
                lifetime_of(&f, 4).1 < last,
                "{kind:?}: the result is not stretched"
            );
            assert_eq!(
                lifetime_of(&f, 5),
                (5, 6),
                "{kind:?}: a value after the setjmp"
            );
        }
    }

    /// Where `setjmp` can bring control back unseen, every local spans the
    /// whole function and nothing shares a slot.
    #[test]
    fn under_setjmp_every_local_spans_the_function() {
        let f = two_scopes(true);
        let (a, b) = (lifetime_of(&f, 0), lifetime_of(&f, 1));
        assert_eq!(a, b);
        assert_eq!(a.0, 0);
    }

    /// With a hidden sret pointer, `Arg(0)` is that pointer and each declared
    /// parameter is one `Arg` further along; without one, `Arg(n)` is
    /// `params[n]`. Indexing `params` by the `Arg` number took the next
    /// parameter's type -- a `double _Complex` parameter of a function
    /// returning a large struct was not recognised as complex and lost its
    /// imaginary half.
    #[test]
    fn param_type_skips_the_hidden_return_pointer() {
        use crate::ir::Pseudo;
        let types = TypeTable::new(&crate::target::Target::host());
        let params = [types.int_id, types.double_id];
        for sret in [false, true] {
            let mut func = Function::new("f", types.void_id);
            let offset = u32::from(sret);
            if sret {
                func.add_pseudo(Pseudo::arg(PseudoId(0), 0).with_name("__sret"));
                func.sret = Some(PseudoId(0));
            }
            for (i, typ) in params.iter().enumerate() {
                func.add_param(format!("p{i}"), *typ);
                let n = i as u32 + offset;
                func.add_pseudo(Pseudo::arg(PseudoId(n), n));
            }
            let lowering = AbiLowering::new(&func);
            if sret {
                assert_eq!(lowering.param_type(&func, 0), None, "the sret pointer");
            }
            for (i, typ) in params.iter().enumerate() {
                assert_eq!(
                    lowering.param_type(&func, i as u32 + offset),
                    Some(*typ),
                    "parameter {i}, sret {sret}"
                );
            }
            assert_eq!(lowering.param_type(&func, 2 + offset), None);
        }
    }

    /// A slot's size is rounded up to its alignment inside the check, and the
    /// frame's final rounding is reserved: the largest admitted locals area
    /// leaves room for both, and one byte more is refused rather than wrapped.
    #[test]
    fn test_grow_frame_reserves_what_follows() {
        let pos = crate::diag::Position::default();
        let limit = TypeTable::MAX_STACK_OBJECT_BYTES as i32 - 2 * 16;
        let mut offset = 0;
        assert_eq!(grow_frame(&mut offset, limit, 8, 16, pos), limit);

        // An `_Alignas(16)` object whose size is not a multiple of 16 takes
        // the rounded size.
        let mut offset = 0;
        assert_eq!(grow_frame(&mut offset, 24, 16, 16, pos), 32);

        // Past the limit: refused, and the offset does not wrap.
        let mut offset = limit;
        let after = grow_frame(&mut offset, 16, 8, 16, pos);
        assert!(after > 0, "offset wrapped to {after}");
    }

    /// Maximum weight first, and the smallest id among equals: `4` is picked
    /// before `3` because ordering `2` gave it a neighbour, though `3` has the
    /// smaller id. The order is what greedy coloring depends on, so the
    /// ordered-set pick has to reproduce the scan it replaced exactly.
    #[test]
    fn test_mcs_ordering_weight_then_smallest_id() {
        let mut graph = InterferenceGraph::new();
        for v in 1..=4 {
            graph.add_vertex(PseudoId(v));
        }
        graph.add_edge(PseudoId(2), PseudoId(4));
        graph.add_edge(PseudoId(3), PseudoId(4));
        assert_eq!(
            mcs_ordering(&graph),
            [PseudoId(1), PseudoId(2), PseudoId(4), PseudoId(3)]
        );
    }

    fn interval(p: u32, start: usize, end: usize) -> LiveInterval {
        LiveInterval {
            pseudo: PseudoId(p),
            start,
            end,
            in_loop: false,
        }
    }

    /// A point clobbers every candidate live across it, ends inclusive, and
    /// nothing else: not an interval that ended before it, not one that
    /// starts after it, not a non-candidate, and not one `exempt` excuses.
    #[test]
    fn test_constraint_clobbers_sweep() {
        let intervals = [
            interval(1, 0, 10),
            interval(2, 5, 5),
            interval(3, 6, 30),
            interval(4, 0, 40),
            interval(5, 26, 40),
        ];
        let points = [
            ConstraintPoint {
                position: 25,
                clobbers: vec![2u8],
                involved_pseudos: vec![PseudoId(3)],
            },
            ConstraintPoint {
                position: 5,
                clobbers: vec![1u8],
                involved_pseudos: vec![],
            },
        ];
        let candidates: std::collections::BTreeSet<PseudoId> =
            [1, 2, 3, 5].into_iter().map(PseudoId).collect();
        let forbidden = constraint_clobbers(&points, &intervals, &candidates, |cp, i| {
            cp.involved_pseudos.contains(&i.pseudo)
        });
        let expect: BTreeMap<PseudoId, std::collections::BTreeSet<u8>> =
            [(PseudoId(1), [1u8].into()), (PseudoId(2), [1u8].into())].into();
        assert_eq!(forbidden, expect);
    }

    /// The answer of pairing every point with every interval it lies in.
    fn clobbers_by_pairing(
        points: &[ConstraintPoint<u8>],
        intervals: &[LiveInterval],
        candidates: &std::collections::BTreeSet<PseudoId>,
        exempt: impl Fn(&ConstraintPoint<u8>, &LiveInterval) -> bool,
    ) -> BTreeMap<PseudoId, std::collections::BTreeSet<u8>> {
        let mut forbidden: BTreeMap<PseudoId, std::collections::BTreeSet<u8>> = BTreeMap::new();
        for cp in points {
            for i in intervals.iter().filter(|i| candidates.contains(&i.pseudo)) {
                if i.start <= cp.position && cp.position <= i.end && !exempt(cp, i) {
                    forbidden
                        .entry(i.pseudo)
                        .or_default()
                        .extend(cp.clobbers.iter().copied());
                }
            }
        }
        forbidden.retain(|_, regs| !regs.is_empty());
        forbidden
    }

    /// Agrees with pairing on overlapping intervals, a pseudo with two
    /// intervals, points clobbering one register twice or none, points that
    /// name an operand twice, and both backends' exemption rules.
    #[test]
    fn test_constraint_clobbers_matches_pairing() {
        let mut seed = 0x2545_f491_4f6c_dd1du64;
        let mut next = |bound: usize| {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            (seed % bound as u64) as usize
        };
        for _ in 0..200 {
            let intervals: Vec<LiveInterval> = (0..12)
                .map(|_| {
                    let start = next(60);
                    interval(next(8) as u32, start, start + next(30))
                })
                .collect();
            let points: Vec<ConstraintPoint<u8>> = (0..10)
                .map(|_| ConstraintPoint {
                    position: next(90),
                    clobbers: (0..next(4)).map(|_| next(5) as u8).collect(),
                    involved_pseudos: (0..next(3)).map(|_| PseudoId(next(8) as u32)).collect(),
                })
                .collect();
            let candidates: std::collections::BTreeSet<PseudoId> =
                (0..8).filter(|p| p % 3 != 2).map(PseudoId).collect();
            let operand = |cp: &ConstraintPoint<u8>, i: &LiveInterval| {
                cp.involved_pseudos.contains(&i.pseudo)
            };
            let survives = |cp: &ConstraintPoint<u8>, i: &LiveInterval| {
                cp.operand_survives(i.pseudo, i.start, i.end)
            };
            assert_eq!(
                constraint_clobbers(&points, &intervals, &candidates, operand),
                clobbers_by_pairing(&points, &intervals, &candidates, operand)
            );
            assert_eq!(
                constraint_clobbers(&points, &intervals, &candidates, survives),
                clobbers_by_pairing(&points, &intervals, &candidates, survives)
            );
        }
    }

    /// The work does not grow with intervals x points. Every interval spans
    /// every point, as the merged live ranges of a function full of gotos do,
    /// and the exemption rule is asked only about the points each pseudo is an
    /// operand of: one per interval here, where pairing asked about all of
    /// them.
    #[test]
    fn test_constraint_clobbers_work_is_not_pairwise() {
        const N: usize = 4000;
        let intervals: Vec<LiveInterval> = (0..N).map(|p| interval(p as u32, 0, 2 * N)).collect();
        let points: Vec<ConstraintPoint<u8>> = (0..N)
            .map(|p| ConstraintPoint {
                position: 2 * p + 1,
                clobbers: vec![(p % 3) as u8],
                involved_pseudos: vec![PseudoId(p as u32)],
            })
            .collect();
        let candidates: std::collections::BTreeSet<PseudoId> =
            (0..N as u32).map(PseudoId).collect();
        let asked = std::cell::Cell::new(0usize);
        let forbidden = constraint_clobbers(&points, &intervals, &candidates, |cp, i| {
            asked.set(asked.get() + 1);
            cp.involved_pseudos.contains(&i.pseudo)
        });
        assert_eq!(asked.get(), N);
        // Every pseudo still meets the other points clobbering each register.
        assert!(forbidden.values().all(|regs| regs.len() == 3));
        assert_eq!(forbidden.len(), N);
    }
}
