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

use crate::arch::asm_constraints::AsmRegClass;
use crate::ir::bitset::BitSet;
use crate::ir::{BasicBlockId, Function, Instruction, Opcode, PseudoId, PseudoKind};
use crate::types::{TypeId, TypeTable};
use crate::units::Bits;
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::hash::Hash;

mod verify;
pub use verify::{
    check_assignment, verify_assignment, Assignment, Cell, Clobber, Conflict, Spared, Violation,
};

/// Grow a frame's locals area by one slot, refusing a total no `i32`
/// displacement can reach -- counting what the prologue will add to it.
///
/// [`TypeTable::MAX_STACK_OBJECT_BYTES`] bounds one object and
/// [`crate::abi::slot_bytes`] enforces that. Neither bounds their *sum*, and
/// `+=` on an `i32` wraps: a frame past two gigabytes came out negative and the
/// prologue subtracted nothing. Every slot of either backend's frame is laid
/// out through here, so this is the one place the total can be checked. A
/// VLA or `alloca` extent never reaches it: the frame holds only an
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

/// The size of each frame slot the allocator hands a pseudo, by pseudo.
///
/// A backend's `Loc::Stack` carries only an offset, and the slot's extent is
/// decided where it is made -- a spill's width, a local's object, a 128-bit
/// value's sixteen bytes -- so each site that binds a pseudo to a slot
/// records it here too. The allocation verifier (`check_assignment`)
/// compares slots by these extents: two pseudos whose bytes overlap may
/// not be live together.
pub type SlotBytes = HashMap<PseudoId, i32>;

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
    /// May `pseudo` sit in a register this point clobbers? `across` says
    /// whether it is live across the point ([`Crossing::across`]).
    ///
    /// Being an operand of the clobbering instruction is not enough on its
    /// own. `mulq` and `idivq` read an operand out of a fixed register and
    /// write their result back over it, so an operand whose value is still
    /// wanted *after* the instruction is destroyed by it. The exemption is
    /// only sound for an operand not live across the instruction:
    ///
    /// - it is defined here, so the pseudo *is* what the instruction
    ///   produces (the `%rdx` half of a `mulq`, the quotient in `%rax`);
    /// - it dies here, so the value has been consumed and nothing reads it
    ///   again.
    ///
    /// An operand live across the point is live on both sides, and the
    /// instruction overwrites it in between. That is what left a 128-bit
    /// product's cross term computed from the low half of its own multiply.
    pub fn operand_survives(&self, pseudo: PseudoId, across: bool) -> bool {
        self.involved_pseudos.contains(&pseudo) && !across
    }
}

/// The registers each candidate may not take because a constraint point it
/// touches clobbers them -- unless `exempt` says the pseudo is one the point
/// may clobber, which is where the backends differ.
///
/// A point touches what is live across it, what it reads and what it
/// defines ([`Crossing`]); `exempt` is told which of these the pseudo is
/// live across. Asked by exact liveness, not by an interval: a value whose
/// positions merely span a point -- dead on the path through it -- is free
/// to take the registers it clobbers.
///
/// Every point must have its [`Crossing`] in `crossings`. The work is the
/// points times what each touches.
pub fn constraint_clobbers<R: Copy + Ord>(
    constraint_points: &[ConstraintPoint<R>],
    crossings: &HashMap<usize, Crossing>,
    candidates: &std::collections::BTreeSet<PseudoId>,
    exempt: impl Fn(&ConstraintPoint<R>, PseudoId, bool) -> bool,
) -> BTreeMap<PseudoId, std::collections::BTreeSet<R>> {
    let mut forbidden: BTreeMap<PseudoId, std::collections::BTreeSet<R>> = BTreeMap::new();
    for cp in constraint_points {
        let crossing = &crossings[&cp.position];
        for (p, across) in crossing.touching() {
            if candidates.contains(&p) && !exempt(cp, p, across) {
                forbidden
                    .entry(p)
                    .or_default()
                    .extend(cp.clobbers.iter().copied());
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
pub fn setval_sizes(func: &Function) -> HashMap<PseudoId, Bits> {
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
    find_insn_positions(func, |insn| is_call_like(insn.op))
}

/// [`find_call_positions`] deciding per instruction: the positions of every
/// instruction `clobbers` answers true for.
pub fn find_insn_positions(func: &Function, clobbers: impl Fn(&Instruction) -> bool) -> Vec<usize> {
    let mut call_positions = Vec::with_capacity(DEFAULT_CALL_POS_CAPACITY);
    let mut pos = 0usize;
    for block in &func.blocks {
        for insn in &block.insns {
            if clobbers(insn) {
                call_positions.push(pos);
            }
            pos += 1;
        }
    }
    call_positions
}

/// The registers an inline asm's clobber list names that `parse` knows --
/// one bank's worth. Nothing for any other instruction.
pub fn asm_clobbered<R>(insn: &Instruction, parse: impl Fn(&str) -> Option<R>) -> Vec<R> {
    match insn.extra().asm_data.as_deref() {
        Some(asm) if insn.op == Opcode::Asm => {
            asm.clobbers.iter().filter_map(|c| parse(c)).collect()
        }
        _ => Vec::new(),
    }
}

/// The constraint point of an instruction that returns twice (see
/// [`Instruction::returns_twice`]), which clobbers every register in `regs`:
/// control comes back to it with only the frame and stack pointers restored
/// -- by a `__builtin_longjmp`, or by whatever a `returns_twice` function
/// does, which gcc assumes keeps nothing -- so no value may be in a register
/// across it. The result alone is exempt -- both paths write it, after they
/// join -- and not the buffer operand, which a value read again later would
/// otherwise keep in a register the resumed path has lost.
pub fn returns_twice_constraint<R: Copy>(
    insn: &Instruction,
    regs: &[R],
) -> (Vec<R>, Vec<PseudoId>) {
    (regs.to_vec(), insn.target.into_iter().collect())
}

/// What one instruction must preserve, reads and defines: the answer to
/// "does this value cross this call, or this constraint point", by exact
/// liveness rather than by the hull of a value's positions, which spans
/// every block laid out between its first and last -- live there or not.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Crossing {
    /// Live just after the instruction and not defined by it: what it must
    /// leave as it found it.
    pub across: Vec<PseudoId>,
    /// What it reads ([`DefsUses`]).
    pub uses: Vec<PseudoId>,
    /// What it defines.
    pub defs: Vec<PseudoId>,
    /// It is an inline asm, which may write the registers it clobbers after
    /// it has written its outputs.
    pub is_asm: bool,
}

impl Crossing {
    /// Every pseudo the instruction touches, once each, with whether it is
    /// live across it.
    pub fn touching(&self) -> impl Iterator<Item = (PseudoId, bool)> + '_ {
        let mut seen: std::collections::BTreeSet<PseudoId> = self.across.iter().copied().collect();
        let rest: Vec<PseudoId> = self
            .uses
            .iter()
            .chain(&self.defs)
            .copied()
            .filter(|&p| seen.insert(p))
            .collect();
        self.across
            .iter()
            .map(|&p| (p, true))
            .chain(rest.into_iter().map(|p| (p, false)))
    }

    /// What a call destroys in a caller-saved register: every value live
    /// across it, and every argument it reads -- see [`crossings`].
    pub fn call_crossers(&self) -> impl Iterator<Item = PseudoId> + '_ {
        self.across.iter().chain(&self.uses).copied()
    }

    /// What an instruction that destroys a whole register bank -- a call,
    /// or an asm whose clobber list names the bank's registers -- destroys
    /// there: [`Crossing::call_crossers`], and an asm's outputs too, which
    /// the template may have written before it writes a clobbered
    /// register. A call writes its result after everything it clobbers.
    pub fn bank_crossers(&self) -> impl Iterator<Item = PseudoId> + '_ {
        let defs: &[PseudoId] = if self.is_asm { &self.defs } else { &[] };
        self.call_crossers().chain(defs.iter().copied())
    }
}

/// Each instruction at `positions` (as [`compute_live_intervals`] numbers
/// them) with its [`Crossing`], by one backward walk over the pseudos that
/// have liveness: not a constant, nor a symbol, which live in no register.
///
/// A call's own result is not live across it: the call writes it after it
/// returns, into whatever register it was given. An argument is, for the
/// caller-saved registers, by [`Crossing::call_crossers`]: a call's lowering
/// stages its arguments through caller-saved registers before it reads them
/// all, so an argument there may be overwritten before it is read. A
/// backend that knows better for a particular call says so itself.
pub fn crossings(
    func: &Function,
    live_out: &[HashSet<PseudoId>],
    positions: &std::collections::BTreeSet<usize>,
) -> HashMap<usize, Crossing> {
    let tracked = |p: PseudoId| {
        !func.get_pseudo(p).is_some_and(|ps| {
            matches!(
                ps.kind,
                PseudoKind::Val(_) | PseudoKind::FVal(_) | PseudoKind::Sym(_)
            )
        })
    };
    let mut out = HashMap::with_capacity(positions.len());
    walk_live_after(func, live_out, tracked, |pos, insn, du, live| {
        if positions.contains(&pos) {
            out.insert(
                pos,
                Crossing {
                    across: live
                        .iter()
                        .copied()
                        .filter(|p| !du.defs.contains(p))
                        .collect(),
                    uses: du.uses.clone(),
                    defs: du.defs.clone(),
                    is_asm: insn.op == Opcode::Asm,
                },
            );
        }
    });
    out
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

    // Each block's declared locals, which its walk in Phase C starts from.
    let mut declared_in: Vec<Vec<PseudoId>> = vec![Vec::new(); num_blocks];
    for local_var in func.locals.values() {
        if let Some(&block_idx) = local_var.decl_block.and_then(|id| bb_id_to_idx.get(&id)) {
            declared_in[block_idx].push(local_var.sym);
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
    //
    // Nor is a frame local, which has its slot before coloring
    // (`place_locals`) and whose interval is its lifetime
    // (`local_lifetimes`), whatever liveness would say. Tracked, every local
    // whose declaring block an optimization had merged away -- or a copied
    // parameter, declared in no block -- was carried back to the entry from
    // every use: blocks x locals set entries, for nothing.
    let needs_no_liveness = |p: PseudoId| -> bool {
        func.get_pseudo(p).is_some_and(|ps| match &ps.kind {
            PseudoKind::Sym(_) if func.local_of(p).is_some() => true,
            PseudoKind::Val(_) | PseudoKind::FVal(_) | PseudoKind::Sym(_) => {
                !def_blocks.contains_key(&p)
            }
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
        defined_so_far.extend(declared_in[idx].iter().copied());

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
        &block_start_pos,
        &bb_id_to_idx,
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

/// Stretch every value live across an instruction that returns twice (see
/// [`Instruction::returns_twice`]) over every position the instruction
/// reaches: to the end of the function, and back to the first block it
/// reaches.
///
/// A `longjmp` resumes at the setjmp from whatever call it is made under,
/// and the CFG has no edge for that: a value live across the setjmp is read
/// again on the second return, after code the liveness analysis believed it
/// was dead in -- the path that led to the `longjmp`. A slot or register
/// handed to another value there is overwritten before the value is read
/// back. Every call the setjmp reaches may be the one that jumps, so the
/// value must survive all of them. This is the pseudo-level counterpart of
/// `local_lifetimes`, which spans every local over the whole function.
///
/// What the setjmp reaches is not only what is laid out after it: the
/// inliner sorts blocks breadth first, and a path from the setjmp's first
/// return can run through blocks laid out before it. Stretched only to the
/// end, the value's slot went to a spill in one of those -- and `vfork`, a
/// setjmp whose second return comes after the child has run on the same
/// stack, lost CPython's `vfork_tstate_save` that way.
///
/// The setjmp's own result starts there, and is written again on each
/// return, so it is not stretched.
fn extend_across_returns_twice(
    func: &Function,
    intervals: &mut [LiveInterval],
    block_start_pos: &[usize],
    bb_id_to_idx: &HashMap<BasicBlockId, usize>,
    last: usize,
) {
    // Each returns-twice instruction's position, and the first position of
    // anything it reaches.
    let mut points: Vec<(usize, usize)> = Vec::new();
    for (b, block) in func.blocks.iter().enumerate() {
        for (pos, insn) in (block_start_pos[b]..).zip(&block.insns) {
            if !insn.returns_twice() {
                continue;
            }
            let mut reached = vec![false; func.blocks.len()];
            let mut work: Vec<usize> = block
                .children
                .iter()
                .filter_map(|c| bb_id_to_idx.get(c).copied())
                .collect();
            let mut from = pos;
            while let Some(r) = work.pop() {
                if std::mem::replace(&mut reached[r], true) {
                    continue;
                }
                from = from.min(block_start_pos[r]);
                work.extend(
                    func.blocks[r]
                        .children
                        .iter()
                        .filter_map(|c| bb_id_to_idx.get(c).copied()),
                );
            }
            points.push((pos, from));
        }
    }
    for interval in intervals.iter_mut() {
        let (start, end) = (interval.start, interval.end);
        for &(p, from) in &points {
            if start < p && p < end {
                interval.start = interval.start.min(from);
                interval.end = interval.end.max(last);
            }
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
    let bytes = crate::abi::slot_bytes(types.byte_size(local.typ), pos, "an automatic object");
    let align = local
        .explicit_align
        .map(|a| a as i32)
        .unwrap_or_else(|| crate::abi::slot_align(types.align(local.typ)).max(8));
    (bytes.max(8), align)
}

/// Each local's frame slot, as `(local, offset, bytes)`, with
/// `new_slot(bytes, alignment)` making a fresh one.
///
/// Locals are laid out in declaration order, as they always were: the order
/// decides which of them land near the frame base, and an inline-asm memory
/// operand can name a local in place only while its displacement fits the
/// addressing mode -- small locals declared beside a large array must stay
/// on the near side of it. A local takes an earlier one's slot when the two
/// agree in size and alignment and its interval -- its lifetime, see
/// `local_lifetimes` -- overlaps none the slot has held.
///
/// Which of a function's locals [`place_locals`] lays out.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LocalSet {
    /// Every local, in declaration order: a function without a canary.
    All,
    /// A protected function's arrays and aggregates holding one.
    Arrays,
    /// A protected function's other locals.
    Rest,
}

impl LocalSet {
    /// The two sets a function with a canary (`guarded`) lays out, in order,
    /// or the one set of one without.
    pub fn of(guarded: bool) -> (Option<Self>, Self) {
        if guarded {
            (Some(Self::Arrays), Self::Rest)
        } else {
            (None, Self::All)
        }
    }
}

/// `set` says which locals: a function with a stack-protector canary lays
/// its arrays out first ([`LocalSet::Arrays`]), right after the canary and
/// before any other slot, `char` arrays before the rest, as gcc does -- so
/// an array that overruns reaches the canary without passing over a spilled
/// argument or a scalar the function may still read before it returns --
/// and the rest ([`LocalSet::Rest`]) where the others go.
pub fn place_locals(
    func: &Function,
    types: &TypeTable,
    pos: crate::diag::Position,
    intervals: &[LiveInterval],
    set: LocalSet,
    mut new_slot: impl FnMut(i32, i32) -> i32,
) -> Vec<(PseudoId, i32, i32)> {
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
    let phase = |l: &&crate::ir::LocalVar| crate::arch::stack_protect::placement(l.typ, types);
    match set {
        LocalSet::All => {}
        LocalSet::Arrays => locals.retain(|l| phase(l) < 2),
        LocalSet::Rest => locals.retain(|l| phase(l) == 2),
    }
    if set == LocalSet::All {
        locals.sort_by_key(|l| l.sym.0);
    } else {
        locals.sort_by_key(|l| (phase(l), l.sym.0));
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
        placed.push((local.sym, offset, bytes));
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
    let regions = LocalRegions::of(func, block_start_pos, bb_id_to_idx);
    let last = block_end_pos.last().copied().unwrap_or(0);
    if regions.everywhere {
        return regions.locals.into_iter().map(|l| (l, (0, last))).collect();
    }

    // The hull: a block's start where the object is live in, its end where
    // live out, and every event in between. Only the set bits are visited:
    // a block's sets hold the few locals live there, not every local.
    let mut hull: Vec<Option<(usize, usize)>> = vec![None; regions.locals.len()];
    let mut widen = |i: usize, pos: usize| {
        let (s, e) = hull[i].get_or_insert((pos, pos));
        *s = (*s).min(pos);
        *e = (*e).max(pos);
    };
    for b in 0..func.blocks.len() {
        for i in regions.live_in[b].iter() {
            widen(i, block_start_pos[b]);
        }
        for i in regions.live_out[b].iter() {
            widen(i, block_end_pos[b]);
        }
        for &(pos, i, _) in &regions.events[b] {
            widen(i, pos);
        }
    }
    hull.into_iter()
        .enumerate()
        .filter_map(|(i, span)| Some((regions.locals[i], span?)))
        .collect()
}

/// Where each local may be inside its lifetime, block by block: the
/// forward analysis [`local_lifetimes`] takes the hull of, and the
/// allocation verifier reads point by point.
pub(crate) struct LocalRegions {
    /// The locals, by index.
    pub(crate) locals: Vec<PseudoId>,
    /// Each block's events in order: `(position, local, starts)` -- a
    /// mention starts (or continues) a lifetime, a `LifetimeEnd` ends one.
    pub(crate) events: Vec<Vec<(usize, usize, bool)>>,
    /// The locals that may be inside their lifetime entering each block.
    pub(crate) live_in: Vec<BitSet>,
    /// ... and leaving it.
    pub(crate) live_out: Vec<BitSet>,
    /// Control can come back to a point the analysis cannot see -- a
    /// `setjmp` -- so every local is live everywhere, and the sets above
    /// are left empty.
    pub(crate) everywhere: bool,
}

impl LocalRegions {
    pub(crate) fn of(
        func: &Function,
        block_start_pos: &[usize],
        bb_id_to_idx: &HashMap<BasicBlockId, usize>,
    ) -> Self {
        let locals: Vec<PseudoId> = func.locals.values().map(|l| l.sym).collect();
        let n = func.blocks.len();
        if func.has_nonlocal_control() {
            return Self {
                live_in: vec![BitSet::new(locals.len()); n],
                live_out: vec![BitSet::new(locals.len()); n],
                locals,
                events: vec![Vec::new(); n],
                everywhere: true,
            };
        }
        let index: HashMap<PseudoId, usize> =
            locals.iter().enumerate().map(|(i, &l)| (l, i)).collect();

        let mut events: Vec<Vec<(usize, usize, bool)>> = vec![Vec::new(); n];
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
        let mut live_in = vec![BitSet::new(locals.len()); n];
        let mut live_out = vec![BitSet::new(locals.len()); n];
        let params: HashSet<&str> = func.params.iter().map(|(n, _)| n.as_str()).collect();
        if let Some(&entry) = bb_id_to_idx.get(&func.entry) {
            for (name, local) in &func.locals {
                if params.contains(name.as_str()) {
                    live_in[entry].insert(index[&local.sym]);
                }
            }
        }
        let mut work: VecDeque<usize> = (0..n).collect();
        let mut queued = vec![true; n];
        while let Some(b) = work.pop_front() {
            queued[b] = false;
            let mut out = live_in[b].clone();
            for &(_, i, starts) in &events[b] {
                if starts {
                    out.insert(i);
                } else {
                    out.remove(i);
                }
            }
            if out == live_out[b] {
                continue;
            }
            for child in &func.blocks[b].children {
                let Some(&c) = bb_id_to_idx.get(child) else {
                    continue;
                };
                if live_in[c].union_with(&out) && !queued[c] {
                    queued[c] = true;
                    work.push_back(c);
                }
            }
            live_out[b] = out;
        }
        Self {
            locals,
            events,
            live_in,
            live_out,
            everywhere: false,
        }
    }
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

/// Every `Alloca` instruction's result pseudo: each lives in a frame slot.
pub fn alloca_results(func: &Function) -> Vec<PseudoId> {
    func.blocks
        .iter()
        .flat_map(|b| &b.insns)
        .filter(|insn| insn.op == Opcode::Alloca)
        .filter_map(|insn| insn.target)
        .collect()
}

/// The ABI-pinned GP arguments that must leave their register for a frame
/// slot, because something overwrites that register while they are live --
/// each with the register it arrived in.
///
/// An argument arrives pre-colored in its ABI register and never goes through
/// coloring, so the forbidden-color machinery that keeps an ordinary value out
/// of a clobbered register does not apply to it. `overwritten` says whether its
/// register is destroyed while it is live: a call (every argument register is
/// caller-saved), or a constraint point that clobbers it
/// ([`constraint_clobbers`]) -- inline asm, an atomic's fixed scratch, a
/// thread-local sequence that calls a resolver or getter. One rule for both
/// backends: aarch64 once asked only about calls, so a thread-local access
/// under the descriptor or Mach-O TLV model, which returns its result in x0,
/// left every later use of an argument that arrived in x0 reading the
/// thread-local's address.
///
/// `reg_of` answers the argument register a pseudo's location is, if it is
/// one. The backend records each spill for its prologue and asks for the
/// slot ([`color_stack_slots`] places it with the others).
pub fn gp_args_overwritten<R: Copy>(
    intervals: &[LiveInterval],
    reg_of: impl Fn(PseudoId) -> Option<R>,
    overwritten: impl Fn(PseudoId, R) -> bool,
) -> Vec<(PseudoId, R)> {
    intervals
        .iter()
        .filter_map(|interval| {
            let reg = reg_of(interval.pseudo)?;
            overwritten(interval.pseudo, reg).then_some((interval.pseudo, reg))
        })
        .collect()
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

/// What one instruction defines and reads, among the pseudos `tracked`
/// admits: the one enumeration the backward liveness walks share --
/// [`build_interference_graph`] and the allocation verifier
/// ([`check_assignment`]) -- so the two cannot disagree about what an
/// instruction kills and what it revives.
///
/// The defs are the target and every register asm output. A *memory* asm
/// output defines nothing: its pseudo is the address the template writes
/// through, so it is a use, like every input, the sources and an indirect
/// call's target.
pub(crate) struct DefsUses {
    pub(crate) defs: Vec<PseudoId>,
    pub(crate) uses: Vec<PseudoId>,
}

/// The set a backward liveness walk keeps: what is live at the current
/// point. [`DefsUses::step`] moves it across one instruction.
pub(crate) trait LiveSet {
    /// `p` is defined here, so it is not live above this point.
    fn kill(&mut self, p: PseudoId);
    /// `p` is read here, so it is live above this point.
    fn revive(&mut self, p: PseudoId);
}

impl LiveSet for std::collections::BTreeSet<PseudoId> {
    fn kill(&mut self, p: PseudoId) {
        self.remove(&p);
    }
    fn revive(&mut self, p: PseudoId) {
        self.insert(p);
    }
}

impl DefsUses {
    pub(crate) fn of(insn: &Instruction, tracked: impl Fn(PseudoId) -> bool) -> Self {
        let asm = insn
            .extra()
            .asm_data
            .as_deref()
            .filter(|_| insn.op == Opcode::Asm);
        let mut defs: Vec<PseudoId> = insn.target.into_iter().filter(|&t| tracked(t)).collect();
        let mut uses: Vec<PseudoId> = insn.src.iter().copied().filter(|&s| tracked(s)).collect();
        uses.extend(insn.extra().indirect_target.filter(|&p| tracked(p)));
        if let Some(asm) = asm {
            for output in &asm.outputs {
                if !tracked(output.pseudo) {
                    continue;
                }
                if output.is_memory() {
                    uses.push(output.pseudo);
                } else {
                    defs.push(output.pseudo);
                }
            }
            uses.extend(asm.inputs.iter().map(|i| i.pseudo).filter(|&p| tracked(p)));
        }
        Self { defs, uses }
    }

    /// Move `live` from just after the instruction to just before it: its
    /// defs were born here, and its uses are live coming in.
    pub(crate) fn step(&self, live: &mut impl LiveSet) {
        for &d in &self.defs {
            live.kill(d);
        }
        for &u in &self.uses {
            live.revive(u);
        }
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
    let tracked = |p: PseudoId| candidates.contains(&p);
    for (idx, block) in func.blocks.iter().enumerate() {
        let mut live: std::collections::BTreeSet<PseudoId> = live_out[idx]
            .iter()
            .copied()
            .filter(|&p| tracked(p))
            .collect();
        for insn in block.insns.iter().rev() {
            let du = DefsUses::of(insn, tracked);
            // An early-clobber asm output, and a register output beside a
            // memory output, may not share with what the statement reads;
            // see `asm_must_differ`.
            if let Some(asm) = insn.extra().asm_data.as_deref() {
                if insn.op == Opcode::Asm {
                    for (out, read) in asm_must_differ(asm) {
                        if tracked(out) && tracked(read) {
                            // `add_edge` ignores a self-edge: a tied input
                            // is the output.
                            graph.add_edge(out, read);
                        }
                    }
                }
            }
            // Each def interferes with everything currently live AND
            // with the instruction's other defs AND with each src
            // (for the lowering-correctness reason described above).
            for &d in &du.defs {
                for &l in live.iter() {
                    if l != d {
                        graph.add_edge(d, l);
                    }
                }
                for &d2 in &du.defs {
                    if d != d2 {
                        graph.add_edge(d, d2);
                    }
                }
                if add_def_src_edges {
                    for &src in &insn.src {
                        if src != d && tracked(src) {
                            graph.add_edge(d, src);
                        }
                    }
                }
            }
            du.step(&mut live);
        }
    }
    graph
}

/// Make the arguments among `vertices` interfere with one another. The
/// prologue writes each argument that has left the register it arrived in --
/// to a slot or to another register -- at entry, where none has a def for
/// [`build_interference_graph`] to see, and one that is never read is
/// written all the same.
pub fn args_interfere(
    graph: &mut InterferenceGraph,
    func: &Function,
    vertices: &std::collections::BTreeSet<PseudoId>,
) {
    let args: Vec<PseudoId> = vertices
        .iter()
        .copied()
        .filter(|&p| {
            func.get_pseudo(p)
                .is_some_and(|ps| matches!(ps.kind, PseudoKind::Arg(_)))
        })
        .collect();
    for (n, &a) in args.iter().enumerate() {
        for &b in &args[n + 1..] {
            graph.add_edge(a, b);
        }
    }
}

/// Walk every block backward from its live-out set, handing `visit` each
/// instruction's linear position (blocks in order, as
/// [`compute_live_intervals`] numbers them), the instruction, what it
/// defines and reads among the pseudos `tracked` admits, and the set of
/// those live just after it.
///
/// The one per-instruction liveness walk the allocators' exact questions
/// share: what a stack slot may be shared with, what is live across an
/// instruction that returns twice, and what crosses a call or a constraint
/// point ([`crossings`]).
pub(crate) fn walk_live_after(
    func: &Function,
    live_out: &[HashSet<PseudoId>],
    tracked: impl Fn(PseudoId) -> bool,
    mut visit: impl FnMut(usize, &Instruction, &DefsUses, &std::collections::BTreeSet<PseudoId>),
) {
    let mut start = 0usize;
    for (idx, block) in func.blocks.iter().enumerate() {
        let mut live: std::collections::BTreeSet<PseudoId> = live_out[idx]
            .iter()
            .copied()
            .filter(|&p| tracked(p))
            .collect();
        for (i, insn) in block.insns.iter().enumerate().rev() {
            let du = DefsUses::of(insn, &tracked);
            visit(start + i, insn, &du, &live);
            du.step(&mut live);
        }
        start += block.insns.len();
    }
}

/// The bytes and alignment of a frame slot a pseudo lives in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SlotShape {
    pub bytes: i32,
    pub align: i32,
}

impl SlotShape {
    pub fn new(bytes: i32, align: i32) -> Self {
        Self { bytes, align }
    }
}

/// Where [`color_stack_slots`] puts each stack-homed pseudo.
#[derive(Debug, Default)]
pub struct StackColoring {
    /// Each slot's shape, by slot number.
    pub slots: Vec<SlotShape>,
    /// Each pseudo's slot number.
    pub slot_of: BTreeMap<PseudoId, usize>,
}

/// Give every pseudo in `homed` a frame slot of its shape, sharing a slot
/// between pseudos that are never live together.
///
/// The question is the one register coloring answers, asked of the frame:
/// two values may share a slot unless one is defined where the other is
/// live, by the same exact per-instruction liveness
/// ([`build_interference_graph`]). The hull of a value's positions will not
/// do: block order scatters a `switch` case's blocks among the other cases',
/// so a value's hull spans every case it is dead in, and no two cases would
/// share. A slot holds one shape: a pseudo takes the lowest-numbered slot of
/// its own shape that no neighbour holds, in maximum-cardinality order, or a
/// new one.
///
/// Beyond liveness, three things decide:
///
/// - A def never shares with a source of its own instruction (the def-src
///   edges): the back ends write a frame-resident target before they have
///   read every source.
/// - The arguments never share with one another: the prologue writes every
///   spilled argument at entry, where none has a def for the graph to see,
///   and one that is never read is written all the same.
/// - A pseudo live across an instruction that returns twice gets a slot of
///   its own. A `longjmp` resumes there from code the graph sees no path
///   back from: whatever that code defines, while the value is dead to the
///   graph, would land on the slot it is read from on the second return.
///   `extend_across_returns_twice` is the interval-side counterpart.
///
/// The order is deterministic: the graph's containers are ordered, and the
/// slot numbers follow the order pseudos are colored in.
pub fn color_stack_slots(
    func: &Function,
    live_out: &[HashSet<PseudoId>],
    homed: &BTreeMap<PseudoId, SlotShape>,
) -> StackColoring {
    let candidates: std::collections::BTreeSet<PseudoId> = homed.keys().copied().collect();
    let mut graph = build_interference_graph(&candidates, func, live_out, true);
    args_interfere(&mut graph, func, &candidates);
    let mut exclusive: std::collections::BTreeSet<PseudoId> = std::collections::BTreeSet::new();
    if func.has_nonlocal_control() {
        walk_live_after(
            func,
            live_out,
            |p| candidates.contains(&p),
            |_, insn, du, live| {
                if insn.returns_twice() {
                    exclusive.extend(live.iter().filter(|p| !du.defs.contains(p)));
                }
            },
        );
    }

    let mut coloring = StackColoring::default();
    // The shared slots of each shape, in slot-number order.
    let mut shared: BTreeMap<SlotShape, Vec<usize>> = BTreeMap::new();
    for v in mcs_ordering(&graph) {
        let shape = homed[&v];
        let reuse = if exclusive.contains(&v) {
            None
        } else {
            let taken: std::collections::BTreeSet<usize> = graph
                .neighbors(v)
                .filter_map(|n| coloring.slot_of.get(&n).copied())
                .collect();
            shared
                .get(&shape)
                .and_then(|slots| slots.iter().copied().find(|s| !taken.contains(s)))
        };
        let slot = reuse.unwrap_or_else(|| {
            let slot = coloring.slots.len();
            coloring.slots.push(shape);
            if !exclusive.contains(&v) {
                shared.entry(shape).or_default().push(slot);
            }
            slot
        });
        coloring.slot_of.insert(v, slot);
    }
    coloring
}

/// The pairs `(output, read)` of one asm statement's operands that may not
/// share a register even where `read` dies at the statement.
///
/// - An early-clobber register output is written before the template has
///   read its inputs, so it differs from every one of them -- a register
///   input, and the address of a memory operand, which the template also
///   reads -- even an input that dies here, whose register a plain output
///   may take. A tied input names the output's own pseudo and so shares
///   with it by construction.
/// - A register output differs from the address of each memory output. A
///   plain output may share a register with an input the template has
///   finished reading, but the address of a memory output is read for the
///   template's write through it, which may come after the register output
///   is written. gprofng's `__collector_subget_32` is
///   `movl %2, %0; negl %0; lock; xaddl %0, %1` with `"=r"(r), "=m"(*ptr)`:
///   given one register for `r` and `ptr`, the `negl` overwrote the address
///   and the `xaddl` wrote through the negated count.
pub(crate) fn asm_must_differ(asm: &crate::ir::AsmData) -> Vec<(PseudoId, PseudoId)> {
    let addresses: Vec<PseudoId> = asm
        .outputs
        .iter()
        .filter(|o| o.is_memory())
        .map(|o| o.pseudo)
        .collect();
    let mut pairs = Vec::new();
    for out in asm.outputs.iter().filter(|o| !o.is_memory()) {
        if out.is_early_clobber() {
            pairs.extend(asm.inputs.iter().map(|i| (out.pseudo, i.pseudo)));
        }
        pairs.extend(addresses.iter().map(|&a| (out.pseudo, a)));
    }
    pairs.retain(|&(out, read)| out != read);
    pairs.sort_unstable();
    pairs.dedup();
    pairs
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
///
/// An operand pinned to one register (x86-64 `"a"`, a register variable) is
/// not among them. The asm wants it in that register, not in one of its own:
/// codegen moves it there from wherever it lives, a stack slot included, and
/// the pinned register is kept from every other operand. Colored first, a
/// pinned operand that could not be precolored -- its register still held
/// another value -- took a free register ahead of the operands that need
/// one, and the Linux kernel's paravirt calls, which pin five operands and
/// clobber the rest of the caller-saved registers, ran out.
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
            // A tied input is in its output's register.
            let class = match c.matching_output {
                Some(i) if i < asm.outputs.len() => &asm.outputs[i].class,
                _ => &c.class,
            };
            if matches!(class.reg, Some(AsmRegClass::Pinned(_))) {
                continue;
            }
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
    use crate::units::Bits;

    /// The asm register operands are the ones the template needs in a
    /// register of their own: `"r"`, and a `"0"` tied to one. An operand
    /// pinned to its register -- `"=a"`, `"D"`, the `"0"` tied to `"=a"` --
    /// is moved there wherever it lives, and is left out.
    #[test]
    fn asm_register_operands_leave_out_the_pinned_ones() {
        use crate::ir::{AsmConstraint, AsmData, BasicBlock, Pseudo};
        use crate::target::Arch;
        let types = TypeTable::new(&crate::target::Target::host());
        let mut f = Function::new("f", types.int_id);
        for id in 0..6 {
            f.add_pseudo(Pseudo::reg(PseudoId(id), id));
        }
        f.next_pseudo = 6;
        let op = |p: u32, c: &str, tied: Option<usize>| AsmConstraint {
            matching_output: tied,
            ..AsmConstraint::new(PseudoId(p), c, Arch::X86_64, Bits::new(64))
        };
        let mut asm = Instruction::new(Opcode::Asm);
        asm.extra_mut().asm_data = Some(Box::new(AsmData {
            template: String::new(),
            outputs: vec![op(0, "=a", None), op(1, "=r", None)],
            inputs: vec![
                op(0, "0", Some(0)),
                op(1, "1", Some(1)),
                op(2, "D", None),
                op(3, "r", None),
            ],
            clobbers: Vec::new(),
            goto_labels: Vec::new(),
        }));
        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        bb.add_insn(asm);
        f.add_block(bb);
        f.entry = BasicBlockId(0);
        let got: Vec<u32> = asm_register_operands(&f).iter().map(|p| p.0).collect();
        assert_eq!(got, [1, 3]);
    }

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
            Bits::new(64),
        ));
        bb.add_insn(Instruction::lifetime_end(PseudoId(0)));
        bb.add_insn(Instruction::load(
            PseudoId(4),
            PseudoId(1),
            0,
            types.long_id,
            Bits::new(64),
        ));
        bb.add_insn(Instruction::lifetime_end(PseudoId(1)));
        bb.add_insn(Instruction::load(
            PseudoId(5),
            PseudoId(2),
            0,
            types.int_id,
            Bits::new(32),
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
    /// setjmp's own result are not stretched. `setjmp` is `twice`, a
    /// `__builtin_setjmp` or a call of a function that returns twice.
    fn value_across_setjmp(mut twice: Instruction) -> Function {
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
            Bits::new(32),
        ));
        twice.target = Some(PseudoId(4));
        entry.add_insn(twice.with_type_and_size(types.int_id, Bits::new(32)));
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
            Bits::new(32),
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
        let types = TypeTable::new(&crate::target::Target::host());
        let mut call = Instruction::call(None, "rt", vec![], vec![], types.int_id, Bits::new(32));
        call.extra_mut().returns_twice_call = true;
        for twice in [Instruction::new(Opcode::Setjmp), call] {
            let kind = twice.op;
            assert!(twice.returns_twice());
            let f = value_across_setjmp(twice);
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

    /// `.L0: br .L2; .L1: %5 = 1; ret %5; .L2: %3 = load p; %4 = setjmp;
    /// cbr %4, .L3, .L1; .L3: ret %3` -- the path from the setjmp's first
    /// return runs through `.L1`, laid out before the setjmp. `%3`, read on
    /// the second return, spans `.L1` too, or `%5` could take its slot.
    #[test]
    fn a_value_across_setjmp_spans_what_it_reaches_laid_out_before_it() {
        use crate::ir::{BasicBlock, Pseudo};
        let types = TypeTable::new(&crate::target::Target::host());
        let mut f = Function::new("f", types.int_id);
        f.add_param("p", types.int_id);
        f.add_pseudo(Pseudo::sym(PseudoId(2), "p".into()));
        f.add_local("p", PseudoId(2), types.int_id, None, None);
        for id in 3..6 {
            f.add_pseudo(Pseudo::reg(PseudoId(id), id));
        }
        f.add_pseudo(Pseudo::val(PseudoId(6), 1));
        f.next_pseudo = 7;
        let [b0, b1, b2, b3] = [0, 1, 2, 3].map(BasicBlockId);
        let mut entry = BasicBlock::new(b0);
        entry.add_insn(Instruction::new(Opcode::Entry));
        entry.add_insn(Instruction::br(b2));
        entry.children = vec![b2];
        let mut first = BasicBlock::new(b1);
        first.add_insn(Instruction::unop(
            Opcode::Copy,
            PseudoId(5),
            PseudoId(6),
            types.int_id,
            Bits::new(32),
        ));
        first.add_insn(Instruction::ret(Some(PseudoId(5))));
        first.parents = vec![b2];
        let mut set = BasicBlock::new(b2);
        set.add_insn(Instruction::load(
            PseudoId(3),
            PseudoId(2),
            0,
            types.int_id,
            Bits::new(32),
        ));
        let mut setjmp = Instruction::new(Opcode::Setjmp);
        setjmp.target = Some(PseudoId(4));
        set.add_insn(setjmp.with_type_and_size(types.int_id, Bits::new(32)));
        set.add_insn(Instruction::cbr(PseudoId(4), b3, b1));
        set.parents = vec![b0];
        set.children = vec![b3, b1];
        let mut second = BasicBlock::new(b3);
        second.add_insn(Instruction::ret(Some(PseudoId(3))));
        second.parents = vec![b2];
        for b in [entry, first, set, second] {
            f.add_block(b);
        }
        f.entry = b0;
        // .L1 is positions 2-3, the load 4, the setjmp 5.
        let (start, end) = lifetime_of(&f, 3);
        assert!(start <= 2, "{start}..{end} misses .L1");
        assert_eq!(end, 7);
        let (start5, end5) = lifetime_of(&f, 5);
        assert!(start <= start5 && end5 <= end, "%5 {start5}..{end5}");
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

    /// `homed` colored over `f`'s own liveness, as each pseudo's slot.
    fn stack_slots(f: &Function, homed: &[(u32, SlotShape)]) -> BTreeMap<u32, usize> {
        let r = compute_live_intervals(f, |_| None::<(Vec<()>, Vec<PseudoId>)>);
        let homed: BTreeMap<PseudoId, SlotShape> =
            homed.iter().map(|&(p, s)| (PseudoId(p), s)).collect();
        let coloring = color_stack_slots(f, &r.live_out, &homed);
        for (p, &n) in &coloring.slot_of {
            assert_eq!(coloring.slots[n], homed[p], "{p} in a slot of its shape");
        }
        coloring
            .slot_of
            .into_iter()
            .map(|(p, n)| (p.0, n))
            .collect()
    }

    const EIGHT: SlotShape = SlotShape { bytes: 8, align: 8 };

    /// One block: `%2 = %0 + %0; %3 = %2 + %2; %4 = %3 + %0; %5 = %3 + %4;
    /// ret %5`, with `%0` and `%1` the arguments.
    fn straight_line() -> Function {
        use crate::ir::{BasicBlock, Pseudo};
        let types = TypeTable::new(&crate::target::Target::host());
        let mut f = Function::new("f", types.long_id);
        for n in 0..2 {
            f.add_param(format!("p{n}"), types.long_id);
            f.add_pseudo(Pseudo::arg(PseudoId(n), n));
        }
        for id in 2..6 {
            f.add_pseudo(Pseudo::reg(PseudoId(id), id));
        }
        f.next_pseudo = 6;
        let add = |t: u32, a: u32, b: u32| {
            Instruction::binop(
                Opcode::Add,
                PseudoId(t),
                PseudoId(a),
                PseudoId(b),
                types.long_id,
                Bits::new(64),
            )
        };
        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        bb.add_insn(add(2, 0, 0));
        bb.add_insn(add(3, 2, 2));
        bb.add_insn(add(4, 3, 0));
        bb.add_insn(add(5, 3, 4));
        bb.add_insn(Instruction::ret(Some(PseudoId(5))));
        f.add_block(bb);
        f.entry = BasicBlockId(0);
        f
    }

    /// Two values share a slot when neither is defined while the other is
    /// live, and not otherwise: `%2` dies where `%3` is defined -- which also
    /// reads it, so even there they may not share -- while `%3` is live where
    /// `%4` is defined. `%2` and `%4` never meet.
    #[test]
    fn stack_slots_are_shared_by_liveness() {
        let f = straight_line();
        let slot = stack_slots(&f, &[(2, EIGHT), (3, EIGHT), (4, EIGHT)]);
        assert_ne!(slot[&2], slot[&3], "a def and its own source");
        assert_ne!(slot[&3], slot[&4], "live together");
        assert_eq!(slot[&2], slot[&4], "never live together");
    }

    /// A slot holds one shape: an eight-byte value and a sixteen-byte one
    /// never share, live together or not.
    #[test]
    fn stack_slots_hold_one_shape() {
        let f = straight_line();
        let slot = stack_slots(&f, &[(2, EIGHT), (4, SlotShape::new(16, 16))]);
        assert_ne!(slot[&2], slot[&4]);
    }

    /// The arguments are all written at entry: two in the frame never share,
    /// though the second is never read.
    #[test]
    fn stack_slots_of_arguments_are_distinct() {
        let f = straight_line();
        let slot = stack_slots(&f, &[(0, EIGHT), (1, EIGHT)]);
        assert_ne!(slot[&0], slot[&1]);
    }

    /// `.L0: cbr %0, .L1, .L2; .L1: %3 = %0 + %0; br .L3;
    /// .L2: %4 = %0 + %0; ret %4; .L3: ret %3` -- `%3`'s positions span
    /// `.L2`, laid out between its def and its use, but it is dead there: the
    /// two share a slot, as the cases of a `switch` do.
    #[test]
    fn stack_slots_follow_liveness_not_layout() {
        use crate::ir::{BasicBlock, Pseudo};
        let types = TypeTable::new(&crate::target::Target::host());
        let mut f = Function::new("f", types.long_id);
        f.add_param("p", types.long_id);
        f.add_pseudo(Pseudo::arg(PseudoId(0), 0));
        for id in 3..5 {
            f.add_pseudo(Pseudo::reg(PseudoId(id), id));
        }
        f.next_pseudo = 5;
        let [b0, b1, b2, b3] = [0, 1, 2, 3].map(BasicBlockId);
        let add = |t: u32| {
            Instruction::binop(
                Opcode::Add,
                PseudoId(t),
                PseudoId(0),
                PseudoId(0),
                types.long_id,
                Bits::new(64),
            )
        };
        let mut entry = BasicBlock::new(b0);
        entry.add_insn(Instruction::new(Opcode::Entry));
        entry.add_insn(Instruction::cbr(PseudoId(0), b1, b2));
        entry.children = vec![b1, b2];
        let mut def = BasicBlock::new(b1);
        def.add_insn(add(3));
        def.add_insn(Instruction::br(b3));
        def.parents = vec![b0];
        def.children = vec![b3];
        let mut other = BasicBlock::new(b2);
        other.add_insn(add(4));
        other.add_insn(Instruction::ret(Some(PseudoId(4))));
        other.parents = vec![b0];
        let mut uses = BasicBlock::new(b3);
        uses.add_insn(Instruction::ret(Some(PseudoId(3))));
        uses.parents = vec![b1];
        for b in [entry, def, other, uses] {
            f.add_block(b);
        }
        f.entry = b0;
        let slot = stack_slots(&f, &[(3, EIGHT), (4, EIGHT)]);
        assert_eq!(slot[&3], slot[&4]);
    }

    /// `%3` is live across a setjmp and read on its second return only; `%5`
    /// is defined on the first return's path, where `%3` is dead. Liveness
    /// would let them share, and the `longjmp` from that path would then
    /// return to `%5` in `%3`'s slot: a value live across an instruction
    /// that returns twice gets a slot of its own.
    #[test]
    fn stack_slot_across_setjmp_is_its_own() {
        let types = TypeTable::new(&crate::target::Target::host());
        let mut call = Instruction::call(None, "rt", vec![], vec![], types.int_id, Bits::new(32));
        call.extra_mut().returns_twice_call = true;
        for twice in [Instruction::new(Opcode::Setjmp), call] {
            let kind = twice.op;
            let f = value_across_setjmp(twice);
            let slot = stack_slots(&f, &[(3, EIGHT), (5, EIGHT)]);
            assert_ne!(slot[&3], slot[&5], "{kind:?}");
            // Without the setjmp the two would share.
            let mut plain = f.clone();
            plain.blocks[0].insns[2] =
                Instruction::load(PseudoId(4), PseudoId(2), 0, types.int_id, Bits::new(32));
            let slot = stack_slots(&plain, &[(3, EIGHT), (5, EIGHT)]);
            assert_eq!(slot[&3], slot[&5], "{kind:?} replaced by a load");
        }
    }

    /// `.L0: cbr %0, .L1, .L2; .L1: %3 = %0 + %0; br .L3;
    /// .L2: %4 = call g(%0); %5 = %4 + %0; ret %5; .L3: ret %3` -- the call
    /// at position 4. `%3`'s positions span the call, laid out between its
    /// def and its use, but it is dead there; `%0` is an argument of the
    /// call and live after it; `%4` is the call's result.
    fn call_between() -> (Function, usize) {
        use crate::ir::{BasicBlock, Pseudo};
        let types = TypeTable::new(&crate::target::Target::host());
        let mut f = Function::new("f", types.long_id);
        f.add_param("p", types.long_id);
        f.add_pseudo(Pseudo::arg(PseudoId(0), 0));
        for id in 3..6 {
            f.add_pseudo(Pseudo::reg(PseudoId(id), id));
        }
        f.next_pseudo = 6;
        let [b0, b1, b2, b3] = [0, 1, 2, 3].map(BasicBlockId);
        let add = |t: u32, a: u32| {
            Instruction::binop(
                Opcode::Add,
                PseudoId(t),
                PseudoId(a),
                PseudoId(0),
                types.long_id,
                Bits::new(64),
            )
        };
        let mut entry = BasicBlock::new(b0);
        entry.add_insn(Instruction::new(Opcode::Entry));
        entry.add_insn(Instruction::cbr(PseudoId(0), b1, b2));
        entry.children = vec![b1, b2];
        let mut def = BasicBlock::new(b1);
        def.add_insn(add(3, 0));
        def.add_insn(Instruction::br(b3));
        def.parents = vec![b0];
        def.children = vec![b3];
        let mut call = BasicBlock::new(b2);
        call.add_insn(Instruction::call(
            Some(PseudoId(4)),
            "g",
            vec![PseudoId(0)],
            vec![types.long_id],
            types.long_id,
            Bits::new(64),
        ));
        call.add_insn(add(5, 4));
        call.add_insn(Instruction::ret(Some(PseudoId(5))));
        call.parents = vec![b0];
        let mut uses = BasicBlock::new(b3);
        uses.add_insn(Instruction::ret(Some(PseudoId(3))));
        uses.parents = vec![b1];
        for b in [entry, def, call, uses] {
            f.add_block(b);
        }
        f.entry = b0;
        (f, 4)
    }

    /// What crosses an instruction is what is live just after it and not
    /// defined there: not a value whose positions merely span it, nor its
    /// own result.
    #[test]
    fn a_crossing_is_exact_liveness() {
        let (f, call) = call_between();
        let r = compute_live_intervals(&f, |_| None::<(Vec<()>, Vec<PseudoId>)>);
        let span = r
            .intervals
            .iter()
            .find(|i| i.pseudo == PseudoId(3))
            .unwrap();
        assert!(
            span.start < call && call < span.end,
            "{span:?} spans the call"
        );
        let at = crossings(&f, &r.live_out, &[call].into());
        let c = &at[&call];
        assert_eq!(c.across, [PseudoId(0)]);
        assert_eq!(c.uses, [PseudoId(0)]);
        assert_eq!(c.defs, [PseudoId(4)]);
        let crossers: Vec<PseudoId> = c.call_crossers().collect();
        assert!(!crossers.contains(&PseudoId(3)), "dead across the call");
        assert!(!crossers.contains(&PseudoId(4)), "the call's own result");
    }

    /// A constraint point forbids its clobbers to what it touches -- what is
    /// live across it, what it reads and what it defines -- unless `exempt`
    /// excuses it, and to nothing else: not to a value whose positions merely
    /// span it. x86-64's rule excuses an operand that is not live across
    /// the point.
    #[test]
    fn a_constraint_point_clobbers_what_it_touches() {
        let (f, call) = call_between();
        let r = compute_live_intervals(&f, |_| None::<(Vec<()>, Vec<PseudoId>)>);
        let at = crossings(&f, &r.live_out, &[call].into());
        let candidates: std::collections::BTreeSet<PseudoId> =
            [0, 3, 4, 5].into_iter().map(PseudoId).collect();
        let point = |involved: Vec<PseudoId>| ConstraintPoint {
            position: call,
            clobbers: vec![7u8],
            involved_pseudos: involved,
        };
        let forbidden = |cp: ConstraintPoint<u8>| {
            constraint_clobbers(&[cp], &at, &candidates, |cp, p, across| {
                cp.operand_survives(p, across)
            })
        };
        let seven: std::collections::BTreeSet<u8> = [7].into();
        let none = forbidden(point(Vec::new()));
        assert_eq!(
            none,
            [(PseudoId(0), seven.clone()), (PseudoId(4), seven.clone())].into()
        );
        // The result is excused; the argument, live across, is not.
        let operands = forbidden(point(vec![PseudoId(4), PseudoId(0)]));
        assert_eq!(operands, [(PseudoId(0), seven)].into());
    }
}
