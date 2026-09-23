//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// SCCP: sparse conditional constant propagation (Wegman & Zadeck).
//
// What this does that `instcombine` cannot: it propagates constants along
// *reachable paths only*. A branch whose condition is a known constant makes
// its untaken successor unreachable, so a phi at the merge no longer has to
// meet the value arriving from there -- which is how
//
//     if (0) { link_error(); }
//
// and, less obviously,
//
//     int x; if (1) x = 3; else x = 4; if (x != 3) boom();
//
// both collapse. `instcombine` sees each instruction in isolation and has no
// notion of an edge, so neither is within its reach.
//
// Memory-ordering contract: SCCP performs no code motion. Every value rewrite
// is an in-place substitution at a single instruction site, and the only
// structural change is dropping a CFG edge that has been *proved* not taken.
// The relative order of every `Load`, `Store`, `Asm`, `Call`, `Atomic*` and
// `Fence` is preserved exactly, so `Instruction::is_memory_barrier()` is
// satisfied by construction. Any future extension that starts moving memory
// must consult it before crossing.
//
// Dominance: not used. Wegman-Zadeck needs only executable-edge marking, so
// this pass never builds a dominator tree -- which also means it cannot hold
// a stale one. An extension that wants dominance must call
// `dominate::domtree_build` *fresh*, after the CFG edits here.
//
// This pass does not delete blocks. It removes the dead edge and leaves the
// deletion to the `dce::run` that follows it in `opt::optimize_function`;
// see `fold_terminator`.
//

use super::constfold::{eval_binop, eval_unop, get_cmp_info};
use super::{BasicBlockId, Function, Instruction, Opcode, PseudoId, PseudoKind};
use std::collections::{HashMap, HashSet, VecDeque};

/// The lattice, of height three.
///
/// Moves downward only -- `Top` to `Const` to `Bottom` -- which is the whole
/// termination argument: each cell changes at most twice.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Val {
    /// No information yet. In this version only unreached definitions hold it
    /// at fixpoint; see the note on `PseudoKind::Undef` in `seed`.
    Top,
    /// A known integer, held as the raw bit pattern exactly as
    /// `PseudoKind::Val` does. Reading it at a given width is `constfold`'s
    /// job, not this lattice's.
    Const(i128),
    /// Overdefined: could be anything.
    Bottom,
}

impl Val {
    fn meet(self, other: Val) -> Val {
        match (self, other) {
            (Val::Top, x) | (x, Val::Top) => x,
            (Val::Const(a), Val::Const(b)) if a == b => Val::Const(a),
            (Val::Bottom, _) | (_, Val::Bottom) => Val::Bottom,
            _ => Val::Bottom,
        }
    }
}

/// Where an instruction lives: its block's index and its index within it.
type Site = (usize, usize);

struct Solver {
    /// Lattice value per pseudo, indexed by `PseudoId.0`.
    vals: Vec<Val>,
    /// Blocks proved reachable.
    executable_block: HashSet<BasicBlockId>,
    /// CFG edges proved taken, as `(pred, succ)`.
    executable_edge: HashSet<(BasicBlockId, BasicBlockId)>,
    cfg_worklist: VecDeque<(BasicBlockId, BasicBlockId)>,
    ssa_worklist: VecDeque<PseudoId>,
    /// Instruction sites that read each pseudo.
    uses: HashMap<PseudoId, Vec<Site>>,
    /// Pseudos that must never be folded whatever the lattice says. See
    /// `seed` for the two reasons.
    unfoldable: HashSet<PseudoId>,
}

/// Run SCCP over `func`, returning whether anything changed.
pub fn run(func: &mut Function) -> bool {
    if func.blocks.is_empty() {
        return false;
    }
    let mut solver = Solver::new(func);
    solver.solve(func);
    solver.apply(func)
}

impl Solver {
    fn new(func: &Function) -> Self {
        let mut s = Solver {
            vals: vec![Val::Top; func.next_pseudo as usize + 1],
            executable_block: HashSet::new(),
            executable_edge: HashSet::new(),
            cfg_worklist: VecDeque::new(),
            ssa_worklist: VecDeque::new(),
            uses: HashMap::new(),
            unfoldable: HashSet::new(),
        };
        s.index(func);
        s.seed(func);
        s
    }

    fn get(&self, id: PseudoId) -> Val {
        self.vals.get(id.0 as usize).copied().unwrap_or(Val::Bottom)
    }

    /// Lower `id` to `v`, pushing its readers if it moved.
    fn set(&mut self, id: PseudoId, v: Val) {
        let idx = id.0 as usize;
        if idx >= self.vals.len() {
            return;
        }
        let merged = self.vals[idx].meet(v);
        if merged != self.vals[idx] {
            self.vals[idx] = merged;
            self.ssa_worklist.push_back(id);
        }
    }

    /// Build the use index.
    fn index(&mut self, func: &Function) {
        for (b, bb) in func.blocks.iter().enumerate() {
            for (i, insn) in bb.insns.iter().enumerate() {
                for u in insn.uses() {
                    self.uses.entry(u).or_default().push((b, i));
                }
            }
        }
    }

    /// Initial lattice values, and the entry edge.
    fn seed(&mut self, func: &Function) {
        for p in &func.pseudos {
            let idx = p.id.0 as usize;
            if idx >= self.vals.len() {
                continue;
            }
            self.vals[idx] = match &p.kind {
                PseudoKind::Val(v) => Val::Const(*v),

                // An argument has no defining instruction and can be
                // anything the caller passes.
                PseudoKind::Arg(_) => Val::Bottom,

                // A `Sym` names *storage*, not a value: a struct-returning
                // call writes through one as its `target`. Folding a use of
                // it would replace an address with a number.
                PseudoKind::Sym(_) => {
                    self.unfoldable.insert(p.id);
                    Val::Bottom
                }

                // `Undef` is seeded `Bottom` here, not `Top`.
                //
                // `Top` is what buys `int x; if (c) x = 5; use(x)`, and it is
                // also the one way this algorithm turns unsound: a value
                // still `Top` at fixpoint that feeds a conditional branch
                // marks *neither* successor executable, so both look
                // unreachable and their side effects are deleted. Recovering
                // from that needs an undef-resolve loop, which belongs in its
                // own change with its own tests.
                PseudoKind::Undef => Val::Bottom,

                // Float constants are not in this lattice. Materializing a
                // folded one needs a correctly sized `SetVal`, because both
                // allocators default an `FVal` with no defining `SetVal` to
                // 64 bits -- so a 32-bit float would be emitted at the wrong
                // width. Until that exists, floats stay overdefined.
                PseudoKind::FVal(_) => Val::Bottom,

                _ => Val::Top,
            };
        }

        // An inline-asm output is a second definition of its pseudo that
        // invariant I1 deliberately exempts, so nothing else in the compiler
        // will notice that the pseudo has two defs. Folding a use of one past
        // the asm's write is a wrong value in exactly the code where a wrong
        // value does the most damage.
        for bb in &func.blocks {
            for insn in &bb.insns {
                if let Some(ref asm) = insn.asm_data {
                    for out in &asm.outputs {
                        self.unfoldable.insert(out.pseudo);
                        let idx = out.pseudo.0 as usize;
                        if idx < self.vals.len() {
                            self.vals[idx] = Val::Bottom;
                        }
                    }
                }
            }
        }

        // A block whose address is taken (`&&label`) is reachable by a route
        // that has no CFG edge, so it must be seeded executable or its values
        // would be treated as unreached.
        let entry = func.entry;
        self.mark_block(func, entry);
        for bb in &func.blocks {
            if bb.addr_taken {
                self.mark_block(func, bb.id);
            }
        }
    }

    fn mark_block(&mut self, func: &Function, id: BasicBlockId) {
        if !self.executable_block.insert(id) {
            return;
        }
        let Some(idx) = func.block_index(id) else {
            return;
        };
        // Re-evaluate everything in the newly reachable block.
        for i in 0..func.blocks[idx].insns.len() {
            self.eval_site(func, (idx, i));
        }
        // Edges this pass cannot see from the terminator alone. The same
        // conservative default as the transfer function: what is not
        // understood is assumed to happen.
        //
        // `asm goto` is the one that bites. Its block ends in an ordinary
        // `Br` to the fallthrough, and the branch targets live in
        // `asm_data.goto_labels` -- so marking what the terminator names
        // misses them entirely, they look unreachable, and `dce` deletes the
        // arm the assembly jumps to.
        let has_asm_goto = func.blocks[idx].insns.iter().any(|i| {
            i.asm_data
                .as_ref()
                .is_some_and(|d| !d.goto_labels.is_empty())
        });
        let modelled_terminator = matches!(
            func.blocks[idx].insns.last().map(|i| i.op),
            Some(Opcode::Br) | Some(Opcode::Cbr) | Some(Opcode::Switch) | Some(Opcode::IndirectBr)
        );
        if has_asm_goto || !modelled_terminator {
            self.mark_all_successors(func, idx);
            for i in 0..func.blocks[idx].insns.len() {
                let labels: Vec<BasicBlockId> = func.blocks[idx].insns[i]
                    .asm_data
                    .as_ref()
                    .map(|d| d.goto_labels.iter().map(|(b, _)| *b).collect())
                    .unwrap_or_default();
                for target in labels {
                    self.mark_edge(func.blocks[idx].id, target);
                }
            }
        }
    }

    fn mark_edge(&mut self, from: BasicBlockId, to: BasicBlockId) {
        if self.executable_edge.insert((from, to)) {
            self.cfg_worklist.push_back((from, to));
        }
    }

    fn solve(&mut self, func: &Function) {
        loop {
            if let Some((_, to)) = self.cfg_worklist.pop_front() {
                // A newly executable edge changes what the target's phis meet
                // over, even when no value moved.
                self.mark_block(func, to);
                if let Some(idx) = func.block_index(to) {
                    for i in 0..func.blocks[idx].insns.len() {
                        if func.blocks[idx].insns[i].op == Opcode::Phi {
                            self.eval_site(func, (idx, i));
                        }
                    }
                }
                continue;
            }
            if let Some(id) = self.ssa_worklist.pop_front() {
                for site in self.uses.get(&id).cloned().unwrap_or_default() {
                    if self.executable_block.contains(&func.blocks[site.0].id) {
                        self.eval_site(func, site);
                    }
                }
                continue;
            }
            break;
        }
    }

    /// Evaluate one instruction: update its target's lattice value, and for a
    /// terminator, mark the successor edges it can take.
    fn eval_site(&mut self, func: &Function, (b, i): Site) {
        let insn = &func.blocks[b].insns[i];
        let block_id = func.blocks[b].id;

        match insn.op {
            Opcode::Br => {
                if let Some(t) = insn.bb_true {
                    self.mark_edge(block_id, t);
                }
                return;
            }
            Opcode::Cbr => {
                let (Some(t), Some(f)) = (insn.bb_true, insn.bb_false) else {
                    return;
                };
                match insn.src.first().map(|s| self.get(*s)) {
                    Some(Val::Const(v)) => match cbr_taken(v) {
                        Some(true) => self.mark_edge(block_id, t),
                        Some(false) => self.mark_edge(block_id, f),
                        // A width-ambiguous constant proves nothing.
                        None => {
                            self.mark_edge(block_id, t);
                            self.mark_edge(block_id, f);
                        }
                    },
                    // Still `Top` means not yet known; leave both unmarked
                    // and come back when it lowers.
                    Some(Val::Top) => {}
                    _ => {
                        self.mark_edge(block_id, t);
                        self.mark_edge(block_id, f);
                    }
                }
                return;
            }
            Opcode::Switch => {
                match insn.src.first().map(|s| self.get(*s)) {
                    Some(Val::Const(v)) => match switch_taken(insn, v) {
                        Some(target) => self.mark_edge(block_id, target),
                        None => self.mark_all_successors(func, b),
                    },
                    Some(Val::Top) => {}
                    _ => self.mark_all_successors(func, b),
                }
                return;
            }
            Opcode::IndirectBr => {
                self.mark_all_successors(func, b);
                return;
            }
            _ => {}
        }

        let Some(target) = insn.target else {
            return;
        };
        // A `Sym` target is storage being written, not a value being defined.
        if self.unfoldable.contains(&target) {
            return;
        }
        let v = self.transfer(insn, block_id);
        self.set(target, v);
    }

    fn mark_all_successors(&mut self, func: &Function, b: usize) {
        let block_id = func.blocks[b].id;
        for &succ in &func.blocks[b].children {
            self.mark_edge(block_id, succ);
        }
    }

    /// The lattice value an instruction produces.
    ///
    /// The default arm is `Bottom`, and that is the safety property of the
    /// whole pass: an unmodelled opcode answering `Top` would be a licence to
    /// prove any branch below it dead. Adding an opcode here means knowing
    /// its semantics exactly.
    fn transfer(&self, insn: &Instruction, block: BasicBlockId) -> Val {
        match insn.op {
            // A conduit. Never rewritten -- see `apply`.
            Opcode::Copy | Opcode::PhiSource => match insn.src.first() {
                Some(s) => self.get(*s),
                None => Val::Bottom,
            },

            // The value is on the target pseudo and was seeded there.
            Opcode::SetVal => self.get(insn.target.unwrap_or(PseudoId(0))),

            Opcode::Phi => {
                let mut acc = Val::Top;
                for (pred, incoming) in &insn.phi_list {
                    if self.executable_edge.contains(&(*pred, block)) {
                        acc = acc.meet(self.get(*incoming));
                    }
                }
                acc
            }

            Opcode::Select => {
                if insn.src.len() != 3 {
                    return Val::Bottom;
                }
                match self.get(insn.src[0]) {
                    Val::Const(c) => match cbr_taken(c) {
                        Some(true) => self.get(insn.src[1]),
                        Some(false) => self.get(insn.src[2]),
                        None => self.get(insn.src[1]).meet(self.get(insn.src[2])),
                    },
                    Val::Top => Val::Top,
                    Val::Bottom => self.get(insn.src[1]).meet(self.get(insn.src[2])),
                }
            }

            Opcode::Neg | Opcode::Not => match insn.src.first().map(|s| self.get(*s)) {
                Some(Val::Const(a)) => match eval_unop(insn, a) {
                    Some(v) => Val::Const(v),
                    None => Val::Bottom,
                },
                Some(Val::Top) => Val::Top,
                _ => Val::Bottom,
            },

            _ if is_modelled_binop(insn.op) => {
                if insn.src.len() != 2 {
                    return Val::Bottom;
                }
                let a = self.get(insn.src[0]);
                let b = self.get(insn.src[1]);
                match (a, b) {
                    (Val::Const(x), Val::Const(y)) => match eval_binop(insn, x, y) {
                        Some(v) => Val::Const(v),
                        // Undefined for these operands -- a zero divisor, an
                        // out-of-range shift count. Not a constant.
                        None => Val::Bottom,
                    },
                    // A comparison of a pseudo with itself is decided without
                    // knowing the value, exactly as `instcombine` does it.
                    _ if insn.src[0] == insn.src[1] => match get_cmp_info(insn.op) {
                        Some(info) => Val::Const(info.identity_result),
                        None => Val::Bottom,
                    },
                    (Val::Top, _) | (_, Val::Top) => Val::Top,
                    _ => Val::Bottom,
                }
            }

            // Everything else. See the doc comment above.
            //
            // `Lo64`/`Hi64`/`Pair64`/`AddC`/`AdcC`/`SubC`/`SbcC`/`UMulHi`
            // land here deliberately and must stay: `arch::mapping` runs
            // before the optimizer, so they are present, and neither is
            // ordinary arithmetic. `AdcC`/`SbcC` take their carry from a
            // *flag*; their third operand names the producing instruction so
            // the scheduler keeps the pair adjacent, and reading it as a
            // value would be reading something else entirely.
            _ => Val::Bottom,
        }
    }

    /// Rewrite what the solution proves, returning whether anything changed.
    fn apply(&mut self, func: &mut Function) -> bool {
        let mut changed = false;
        // One pseudo per distinct constant per run, so repeated folds do not
        // inflate the pseudo table.
        let mut minted: HashMap<i128, PseudoId> = HashMap::new();

        // Values first: a folded condition is what lets the terminator below
        // it fold in this same run.
        for b in 0..func.blocks.len() {
            if !self.executable_block.contains(&func.blocks[b].id) {
                continue;
            }
            for i in 0..func.blocks[b].insns.len() {
                changed |= self.fold_value(func, (b, i), &mut minted);
            }
        }

        for b in 0..func.blocks.len() {
            if !self.executable_block.contains(&func.blocks[b].id) {
                continue;
            }
            changed |= self.fold_terminator(func, b);
        }
        changed
    }

    /// Replace an instruction whose result is a known constant with a `Copy`
    /// of that constant.
    fn fold_value(
        &self,
        func: &mut Function,
        (b, i): Site,
        minted: &mut HashMap<i128, PseudoId>,
    ) -> bool {
        let insn = &func.blocks[b].insns[i];
        let Some(target) = insn.target else {
            return false;
        };
        if self.unfoldable.contains(&target) {
            return false;
        }
        // A `PhiSource` is how `lower::eliminate_phi_nodes` finds a phi's
        // incoming value: it scans for the opcode, not for `Phi.phi_list`.
        // Turning one into a `Copy` silently deletes that incoming value.
        // Propagate *through* it, never over it.
        if matches!(
            insn.op,
            Opcode::PhiSource | Opcode::SetVal | Opcode::Nop | Opcode::Entry
        ) {
            return false;
        }
        let Val::Const(v) = self.get(target) else {
            return false;
        };
        // Both allocators resolve a `Val` pseudo with no defining `SetVal` to
        // an immediate; x86-64's sixteen-byte-slot case keys on the
        // *`SetVal`'s* size, which a minted pseudo has none of, and aarch64
        // has no such case at all. Emitting one would mean inserting an
        // instruction, which this pass's in-place discipline does not do.
        if insn.size == 128 {
            return false;
        }
        // Already a copy of this very constant: rewriting would mint a fresh
        // pseudo, report a change, and do it again next iteration.
        if insn.op == Opcode::Copy && insn.src.len() == 1 && func.const_val(insn.src[0]) == Some(v)
        {
            return false;
        }

        let c = match minted.get(&v) {
            Some(id) => *id,
            None => {
                let id = func.create_const_pseudo(v);
                minted.insert(v, id);
                id
            }
        };
        let insn = &mut func.blocks[b].insns[i];
        insn.op = Opcode::Copy;
        insn.src = vec![c];
        // A folded phi keeps no incoming values; clearing the list is what
        // makes the now-unread `PhiSource` instructions dead, for the `dce`
        // run that follows this pass to collect.
        insn.phi_list.clear();
        true
    }

    /// Turn a conditional terminator whose outcome is known into a `Br`.
    fn fold_terminator(&self, func: &mut Function, b: usize) -> bool {
        let block_id = func.blocks[b].id;
        let Some(insn) = func.blocks[b].insns.last() else {
            return false;
        };
        let taken = match insn.op {
            Opcode::Cbr => {
                let (Some(t), Some(f)) = (insn.bb_true, insn.bb_false) else {
                    return false;
                };
                let Val::Const(v) = insn
                    .src
                    .first()
                    .map(|s| self.get(*s))
                    .unwrap_or(Val::Bottom)
                else {
                    return false;
                };
                match cbr_taken(v) {
                    Some(true) => t,
                    Some(false) => f,
                    None => return false,
                }
            }
            Opcode::Switch => {
                let Val::Const(v) = insn
                    .src
                    .first()
                    .map(|s| self.get(*s))
                    .unwrap_or(Val::Bottom)
                else {
                    return false;
                };
                match switch_taken(insn, v) {
                    Some(t) => t,
                    None => return false,
                }
            }
            _ => return false,
        };

        // The terminator's targets must be exactly what the CFG records, or
        // the edge bookkeeping below would be repairing something already
        // broken. Leave such a block alone.
        let targets = terminator_targets(insn);
        let recorded: HashSet<BasicBlockId> = func.blocks[b].children.iter().copied().collect();
        if targets != recorded {
            debug_assert!(
                false,
                "sccp: terminator targets disagree with the CFG in {block_id}"
            );
            return false;
        }

        // A `Cbr` with both arms on one block, or a `Switch` with two cases
        // landing together, keeps its edge: it is still a successor.
        let dropped: Vec<BasicBlockId> = targets.iter().copied().filter(|t| *t != taken).collect();

        let last = func.blocks[b].insns.len() - 1;
        let insn = &mut func.blocks[b].insns[last];
        if insn.op == Opcode::Br && insn.bb_true == Some(taken) {
            return false;
        }
        // Rewritten in place, never `kill()`ed: `kill` leaves `bb_false`,
        // `switch_cases` and `switch_default` untouched, and `validate`'s
        // branch-target invariant inspects those on every instruction
        // whatever its opcode -- so a killed `Cbr` still naming a block that
        // is then removed trips the validator far from here.
        insn.op = Opcode::Br;
        insn.bb_true = Some(taken);
        insn.bb_false = None;
        insn.src.clear();
        insn.switch_cases.clear();
        insn.switch_default = None;

        func.blocks[b].children.retain(|c| !dropped.contains(c));
        for d in dropped {
            if let Some(succ) = func.get_block_mut(d) {
                // `parents` matters even though `dce` does not bother with
                // it: `dce`'s untaken successors die, and `retain_edges`
                // repairs them on the way out. A successor dropped here
                // usually survives, reached from somewhere else, and
                // `dominate` builds immediate dominators from `parents`.
                succ.parents.retain(|p| *p != block_id);
                succ.remove_phi_predecessor(block_id);
            }
        }
        true
    }
}

/// Which way a conditional branch on the constant `v` goes, or `None` when
/// the constant does not say.
///
/// A `Cbr` carries no type and a width of zero, and a `Val` pseudo may hold
/// bits above its nominal width, so the raw `i128` cannot simply be compared
/// against zero. Both backends compute the condition's width as at least 32,
/// so a value with any of its low 32 bits set is nonzero at every width the
/// hardware will test; and zero is zero at every width. Anything else --
/// `1 << 32` viewed at 32 bits -- proves nothing and is left alone.
fn cbr_taken(v: i128) -> Option<bool> {
    if v == 0 {
        return Some(false);
    }
    if super::constfold::at_width(v, 32, false) != 0 {
        return Some(true);
    }
    None
}

/// The block a `Switch` on the constant `v` transfers to.
///
/// Mirrors the backends' lowering rather than C's semantics, because that is
/// what actually runs: the selector is moved into a register of the switch's
/// width, rounded up to 32, and each case is a machine compare at that width.
/// A machine compare does not have a signedness -- `case 3000000000u` and a
/// selector of `3000000000u` agree on all 32 bits whether either is read as
/// negative or not -- so both sides are taken as their low `w` bits and
/// compared there. Reading the selector *signed* instead made
/// `switch (3000000000u) { case 3000000000u: }` fall to its default.
///
/// A GNU range `lo ... hi` is lowered as `(v - lo) <= (hi - lo)` unsigned, at
/// the same width, so the subtraction wraps there too.
fn switch_taken(insn: &Instruction, v: i128) -> Option<BasicBlockId> {
    // The backend prefers the type's width when the instruction carries one.
    // Without a `TypeTable` that width is unknowable, so such a switch is
    // left alone. Every `Switch` the linearizer builds has `typ: None`.
    if insn.typ.is_some() {
        return None;
    }
    let w = if insn.size.max(32) > 32 { 64 } else { 32 };
    let mask = |x: i128| -> u128 { (x as u128) & (u128::MAX >> (128 - w)) };

    let sel = mask(v);
    for (lo, hi, target) in &insn.switch_cases {
        let low = mask(*lo as i128);
        let matched = if lo == hi {
            sel == low
        } else {
            let span = mask((*hi as i128).wrapping_sub(*lo as i128));
            mask(sel.wrapping_sub(low) as i128) <= span
        };
        if matched {
            return Some(*target);
        }
    }
    insn.switch_default
}

/// Every block a terminator can transfer to.
fn terminator_targets(insn: &Instruction) -> HashSet<BasicBlockId> {
    let mut t = HashSet::new();
    t.extend(insn.bb_true);
    t.extend(insn.bb_false);
    t.extend(insn.switch_cases.iter().map(|(_, _, b)| *b));
    t.extend(insn.switch_default);
    t
}

/// Binary opcodes `constfold::eval_binop` knows.
fn is_modelled_binop(op: Opcode) -> bool {
    matches!(
        op,
        Opcode::Add
            | Opcode::Sub
            | Opcode::Mul
            | Opcode::DivS
            | Opcode::DivU
            | Opcode::ModS
            | Opcode::ModU
            | Opcode::Shl
            | Opcode::Lsr
            | Opcode::Asr
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
            | Opcode::SetEq
            | Opcode::SetNe
            | Opcode::SetLt
            | Opcode::SetLe
            | Opcode::SetGt
            | Opcode::SetGe
            | Opcode::SetB
            | Opcode::SetBe
            | Opcode::SetA
            | Opcode::SetAe
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, Pseudo};
    use crate::target::Target;
    use crate::types::TypeTable;

    /// A diamond: `entry` branches on `cond` to `then`/`els`, both of which
    /// reach `merge`, where a `Phi` meets `then_val` and `else_val`.
    ///
    /// Built by hand rather than linearized because SCCP runs on IR that is
    /// already in SSA form, so the phi and its two `PhiSource` conduits have
    /// to be present exactly as `ssa_convert` leaves them.
    fn diamond(cond: Pseudo, then_val: i128, else_val: i128) -> Function {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("test", types.int_id);
        let int = types.int_id;

        let c = cond.id;
        func.add_pseudo(cond);
        func.add_pseudo(Pseudo::val(PseudoId(10), then_val));
        func.add_pseudo(Pseudo::val(PseudoId(11), else_val));
        func.add_pseudo(Pseudo::reg(PseudoId(12), 12)); // then's PhiSource
        func.add_pseudo(Pseudo::reg(PseudoId(13), 13)); // else's PhiSource
        func.add_pseudo(Pseudo::reg(PseudoId(14), 14)); // the phi
        func.next_pseudo = 20;

        let (entry, then, els, merge) = (
            BasicBlockId(0),
            BasicBlockId(1),
            BasicBlockId(2),
            BasicBlockId(3),
        );

        let mut b0 = BasicBlock::new(entry);
        b0.add_insn(Instruction::new(Opcode::Entry));
        b0.add_insn(Instruction::cbr(c, then, els));
        b0.children = vec![then, els];

        let mut b1 = BasicBlock::new(then);
        let mut ps1 = Instruction::phi_source(PseudoId(12), PseudoId(10), int, 32);
        ps1.phi_list = vec![(merge, PseudoId(14))];
        b1.add_insn(ps1);
        b1.add_insn(Instruction::br(merge));
        b1.parents = vec![entry];
        b1.children = vec![merge];

        let mut b2 = BasicBlock::new(els);
        let mut ps2 = Instruction::phi_source(PseudoId(13), PseudoId(11), int, 32);
        ps2.phi_list = vec![(merge, PseudoId(14))];
        b2.add_insn(ps2);
        b2.add_insn(Instruction::br(merge));
        b2.parents = vec![entry];
        b2.children = vec![merge];

        let mut b3 = BasicBlock::new(merge);
        let mut phi = Instruction::phi(PseudoId(14), int, 32);
        phi.phi_list = vec![(then, PseudoId(12)), (els, PseudoId(13))];
        b3.add_insn(phi);
        b3.add_insn(Instruction::ret(Some(PseudoId(14))));
        b3.parents = vec![then, els];

        for bb in [b0, b1, b2, b3] {
            func.add_block(bb);
        }
        func.entry = entry;
        func
    }

    fn terminator(func: &Function, b: usize) -> &Instruction {
        func.blocks[b].insns.last().unwrap()
    }

    // Branch folding

    #[test]
    fn sccp_folds_cbr_on_constant_true() {
        let mut func = diamond(Pseudo::val(PseudoId(1), 1), 5, 9);
        assert!(run(&mut func));
        let t = terminator(&func, 0);
        assert_eq!(t.op, Opcode::Br);
        assert_eq!(t.bb_true, Some(BasicBlockId(1)));
        assert_eq!(t.bb_false, None, "a folded Cbr must not keep bb_false");
        assert_eq!(func.blocks[0].children, vec![BasicBlockId(1)]);
        assert!(
            !func.blocks[2].parents.contains(&BasicBlockId(0)),
            "the dropped successor must lose its parent edge too"
        );
    }

    #[test]
    fn sccp_folds_cbr_on_constant_false() {
        let mut func = diamond(Pseudo::val(PseudoId(1), 0), 5, 9);
        assert!(run(&mut func));
        let t = terminator(&func, 0);
        assert_eq!(t.op, Opcode::Br);
        assert_eq!(t.bb_true, Some(BasicBlockId(2)));
        assert_eq!(func.blocks[0].children, vec![BasicBlockId(2)]);
    }

    #[test]
    fn sccp_does_not_fold_cbr_on_an_argument() {
        let mut func = diamond(Pseudo::arg(PseudoId(1), 0), 5, 9);
        assert!(!run(&mut func), "nothing is knowable here");
        assert_eq!(terminator(&func, 0).op, Opcode::Cbr);
    }

    /// A value whose low 32 bits are zero but which is not zero proves
    /// nothing: the branch reads it at a width this pass cannot ask for.
    #[test]
    fn sccp_does_not_fold_cbr_on_a_width_ambiguous_constant() {
        let mut func = diamond(Pseudo::val(PseudoId(1), 1i128 << 32), 5, 9);
        run(&mut func);
        assert_eq!(terminator(&func, 0).op, Opcode::Cbr);
    }

    // Phi

    /// The headline: the phi folds only because one incoming edge is proved
    /// dead. `instcombine` has no notion of an edge and cannot do this.
    #[test]
    fn sccp_phi_over_a_dead_edge_folds() {
        let mut func = diamond(Pseudo::val(PseudoId(1), 1), 5, 9);
        assert!(run(&mut func));
        let phi = &func.blocks[3].insns[0];
        assert_eq!(phi.op, Opcode::Copy);
        assert!(phi.phi_list.is_empty(), "a folded phi keeps no incoming");
        assert_eq!(func.const_val(phi.src[0]), Some(5));

        let mut other = diamond(Pseudo::val(PseudoId(1), 1), 5, 9);
        assert!(
            !crate::ir::instcombine::run(&mut other),
            "if instcombine can already do this the test proves nothing"
        );
    }

    #[test]
    fn sccp_phi_with_differing_constants_does_not_fold() {
        let mut func = diamond(Pseudo::arg(PseudoId(1), 0), 5, 9);
        run(&mut func);
        assert_eq!(func.blocks[3].insns[0].op, Opcode::Phi);
    }

    /// `lower::eliminate_phi_nodes` finds a phi's incoming value by scanning
    /// for the opcode, so rewriting one deletes that value silently.
    #[test]
    fn sccp_never_rewrites_a_phisource() {
        let mut func = diamond(Pseudo::val(PseudoId(1), 1), 5, 5);
        run(&mut func);
        assert_eq!(func.blocks[1].insns[0].op, Opcode::PhiSource);
    }

    /// SCCP leaves the orphaned `PhiSource` for the `dce` that follows it in
    /// `opt::optimize_function`. If anyone moves SCCP out of that loop, this
    /// is the coupling that breaks.
    #[test]
    fn sccp_leaves_the_dead_phisource_for_dce() {
        let mut func = diamond(Pseudo::val(PseudoId(1), 1), 5, 9);
        run(&mut func);
        crate::ir::dce::run(&mut func);

        // The untaken arm has no predecessor left, so `dce` deletes it.
        assert!(
            func.get_block(BasicBlockId(2)).is_none(),
            "the untaken arm should be unreachable and gone"
        );
        // And the surviving arm's conduit has no reader now that the phi is
        // a `Copy`, so it goes too. Neither happens without the `dce` run.
        assert!(
            func.blocks
                .iter()
                .flat_map(|b| &b.insns)
                .all(|i| i.op != Opcode::PhiSource),
            "no PhiSource should survive a folded phi"
        );
    }

    // Safety

    /// The default arm of the transfer function must be `Bottom`. `Top`
    /// there would make every opcode this pass does not model a licence to
    /// prove the branch below it dead -- and it would look like it was
    /// working, because it optimizes more.
    #[test]
    fn sccp_treats_unmodelled_opcodes_as_overdefined() {
        let types = TypeTable::new(&Target::host());
        for op in [
            Opcode::Load,
            Opcode::Call,
            Opcode::UMulHi,
            Opcode::AdcC,
            Opcode::Lo64,
            Opcode::FAdd,
            Opcode::Ctz32,
            Opcode::VaArg,
        ] {
            let mut func = Function::new("t", types.int_id);
            func.add_pseudo(Pseudo::val(PseudoId(1), 1));
            func.add_pseudo(Pseudo::reg(PseudoId(2), 2));
            func.next_pseudo = 5;

            let mut insn = Instruction::new(op);
            insn.target = Some(PseudoId(2));
            insn.src = vec![PseudoId(1), PseudoId(1)];
            insn.size = 32;

            let mut b0 = BasicBlock::new(BasicBlockId(0));
            b0.add_insn(Instruction::new(Opcode::Entry));
            b0.add_insn(insn);
            b0.add_insn(Instruction::cbr(
                PseudoId(2),
                BasicBlockId(1),
                BasicBlockId(2),
            ));
            b0.children = vec![BasicBlockId(1), BasicBlockId(2)];
            func.add_block(b0);
            for id in [BasicBlockId(1), BasicBlockId(2)] {
                let mut bb = BasicBlock::new(id);
                bb.add_insn(Instruction::ret(None));
                bb.parents = vec![BasicBlockId(0)];
                func.add_block(bb);
            }
            func.entry = BasicBlockId(0);

            run(&mut func);
            assert_eq!(
                terminator(&func, 0).op,
                Opcode::Cbr,
                "{op:?} must not be treated as knowable"
            );
        }
    }

    /// A 128-bit constant needs a correctly sized `SetVal` to get its stack
    /// slot; a minted pseudo has none, and the aarch64 allocator has no
    /// 128-bit immediate case at all.
    #[test]
    fn sccp_does_not_materialize_at_128_bits() {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("t", types.int_id);
        func.add_pseudo(Pseudo::val(PseudoId(1), 3));
        func.add_pseudo(Pseudo::val(PseudoId(2), 4));
        func.add_pseudo(Pseudo::reg(PseudoId(3), 3));
        func.next_pseudo = 8;

        let mut b0 = BasicBlock::new(BasicBlockId(0));
        b0.add_insn(Instruction::new(Opcode::Entry));
        b0.add_insn(Instruction::binop(
            Opcode::Add,
            PseudoId(3),
            PseudoId(1),
            PseudoId(2),
            types.int_id,
            128,
        ));
        b0.add_insn(Instruction::ret(Some(PseudoId(3))));
        func.add_block(b0);
        func.entry = BasicBlockId(0);

        run(&mut func);
        assert_eq!(func.blocks[0].insns[1].op, Opcode::Add);
    }

    /// A machine compare has no signedness, and the selector and the case
    /// label are the same C type. Reading the selector *signed* made
    /// `switch (3000000000u) { case 3000000000u: }` miss its own case.
    #[test]
    fn sccp_switch_matches_a_case_above_the_signed_range() {
        let taken = switch_case_for(3_000_000_000i64, 3_000_000_000i128);
        assert_eq!(taken, Some(BasicBlockId(7)), "the case must match itself");
    }

    #[test]
    fn sccp_switch_falls_to_default_when_nothing_matches() {
        assert_eq!(switch_case_for(5, 6), Some(BasicBlockId(9)));
    }

    #[test]
    fn sccp_switch_matches_a_gnu_range() {
        let mut insn = Instruction::new(Opcode::Switch);
        insn.size = 32;
        insn.switch_cases = vec![(30, 50, BasicBlockId(7))];
        insn.switch_default = Some(BasicBlockId(9));
        assert_eq!(switch_taken(&insn, 40), Some(BasicBlockId(7)));
        assert_eq!(switch_taken(&insn, 51), Some(BasicBlockId(9)));
        assert_eq!(switch_taken(&insn, 29), Some(BasicBlockId(9)));
    }

    /// A switch carrying a type has a width this pass cannot compute without
    /// a `TypeTable`, so it must decline rather than guess.
    #[test]
    fn sccp_switch_with_a_type_is_left_alone() {
        let types = TypeTable::new(&Target::host());
        let mut insn = Instruction::new(Opcode::Switch);
        insn.size = 32;
        insn.typ = Some(types.int_id);
        insn.switch_cases = vec![(5, 5, BasicBlockId(7))];
        insn.switch_default = Some(BasicBlockId(9));
        assert_eq!(switch_taken(&insn, 5), None);
    }

    fn switch_case_for(case: i64, selector: i128) -> Option<BasicBlockId> {
        let mut insn = Instruction::new(Opcode::Switch);
        insn.size = 32;
        insn.switch_cases = vec![(case, case, BasicBlockId(7))];
        insn.switch_default = Some(BasicBlockId(9));
        switch_taken(&insn, selector)
    }

    /// An `asm goto` block ends in an ordinary `Br` to its fallthrough; the
    /// branch targets live in `asm_data.goto_labels`. Marking only what the
    /// terminator names leaves them looking unreachable, and `dce` then
    /// deletes the arm the assembly jumps to.
    #[test]
    fn sccp_marks_asm_goto_targets_executable() {
        use crate::ir::AsmData;
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("t", types.int_id);
        func.next_pseudo = 4;

        let (entry, label, fall) = (BasicBlockId(0), BasicBlockId(1), BasicBlockId(2));
        let mut b0 = BasicBlock::new(entry);
        b0.add_insn(Instruction::new(Opcode::Entry));
        let mut asm = Instruction::new(Opcode::Asm);
        asm.asm_data = Some(Box::new(AsmData {
            template: String::new(),
            outputs: Vec::new(),
            inputs: Vec::new(),
            clobbers: Vec::new(),
            goto_labels: vec![(label, "done".to_string())],
        }));
        b0.add_insn(asm);
        b0.add_insn(Instruction::br(fall));
        b0.children = vec![fall, label];
        func.add_block(b0);

        for id in [label, fall] {
            let mut bb = BasicBlock::new(id);
            bb.add_insn(Instruction::ret(None));
            bb.parents = vec![entry];
            func.add_block(bb);
        }
        func.entry = entry;

        run(&mut func);
        crate::ir::dce::run(&mut func);
        assert!(
            func.get_block(label).is_some(),
            "the asm goto target must not be deleted as unreachable"
        );
    }

    // Well-formedness

    #[test]
    fn sccp_preserves_the_ir_invariants() {
        for cond in [
            Pseudo::val(PseudoId(1), 1),
            Pseudo::val(PseudoId(1), 0),
            Pseudo::arg(PseudoId(1), 0),
        ] {
            let mut func = diamond(cond, 5, 9);
            run(&mut func);
            crate::ir::dce::run(&mut func);
            assert!(
                crate::ir::validate::validate_function(&func).is_ok(),
                "{:?}",
                crate::ir::validate::validate_function(&func).err()
            );
        }
    }

    #[test]
    fn sccp_is_idempotent() {
        let mut func = diamond(Pseudo::val(PseudoId(1), 1), 5, 9);
        assert!(run(&mut func));
        assert!(
            !run(&mut func),
            "a second run must find nothing, or the fixpoint loop burns its budget"
        );
    }

    #[test]
    fn sccp_keeps_parents_and_children_consistent() {
        let mut func = diamond(Pseudo::val(PseudoId(1), 1), 5, 9);
        run(&mut func);
        for bb in &func.blocks {
            for child in &bb.children {
                let c = func.get_block(*child).expect("child must exist");
                assert!(
                    c.parents.contains(&bb.id),
                    "{} lists {} as a child but is not its parent",
                    bb.id,
                    child
                );
            }
        }
    }
}
