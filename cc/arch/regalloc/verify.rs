//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The allocation verifier: no two values in one place at once
//
// Both allocators end with every pseudo bound to a location. Nothing in the
// assembly says when two of those bindings collide -- a value read back from
// a register another value was written into is a silent miscompile -- so in
// a debug build each function's assignment is checked against exact liveness
// before code is generated from it.
//
// The liveness is the allocator's own: the per-block live-out sets of
// `compute_live_intervals`, walked backward through each block with
// `DefsUses`, the enumeration `build_interference_graph` walks with. What is
// checked, at every program point:
//
//   * A def lands nowhere a value live just after it is kept, nor where
//     another def of the same instruction lands.
//   * A def lands on no source of its own instruction unless the backend
//     reads every source before writing the def (`def_may_share_src`), and
//     an asm output lands on none of the operands gcc says it must not share
//     (`asm_must_differ`).
//   * No register an instruction overwrites -- a call's caller-saved set, a
//     constraint point's clobbers, an asm's clobber list -- holds a value the
//     instruction does not spare (`Clobber`).
//   * The arguments arrive in distinct places.
//   * A value live across a `setjmp` shares with nothing written at any point
//     the `setjmp` reaches: a `longjmp` from there resumes at it and reads it.
//   * A local's frame slot shares no byte with a value live, or another local
//     inside its lifetime, at any point inside the local's own lifetime --
//     the forward region `local_lifetimes` takes its hull of, read point by
//     point.
//   * Nothing is placed over what the function keeps for itself: the
//     canary's slot, the x87 scratch, the frame base register, a global
//     register variable, a codegen scratch register.
//
// Why a def is checked and not every pair at every point: two values live at
// one point were, walking back from it, either both live coming into the
// function, or one of them was defined while the other was live. So the defs,
// the entry and the joins of the lifetime analysis cover every point.
//
// What is legitimately shared, and passes:
//
//   * A `Copy` whose target lands where its source lives: both hold one
//     value. Only the copy's own source is excused -- Chaitin's rule -- so a
//     chain of coalesced copies would need value tracking this does not do.
//   * A source that dies at an instruction and the def it computes, where the
//     backend allows it (`def_may_share_src`).
//   * Constants and globals, which occupy nothing (the backend's `cells`
//     returns none for them).
//   * Values undefined coming into the function -- an `Undef`, a pseudo
//     defined on some paths only -- which may sit anywhere until defined.
//   * A phi's pseudo, defined once per incoming edge: each def is checked.

use super::{asm_must_differ, DefsUses, LiveSet, LocalRegions};
use crate::ir::{BasicBlockId, Function, Instruction, Opcode, PseudoId, PseudoKind};
use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;

/// A place a value occupies, as the verifier compares places.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Cell<U> {
    /// A register, or a part of one the backend tells apart.
    Reg(U),
    /// Bytes `[lo, hi)` of the function's own frame, in whatever coordinate
    /// the backend chooses, as long as it is one coordinate.
    Frame { lo: i64, hi: i64 },
    /// The incoming argument at this offset of the caller's frame.
    Incoming(i32),
}

/// Which pseudos a [`Clobber`] spares.
#[derive(Debug, Clone)]
pub enum Spared {
    /// Whatever the instruction reads or defines: only a value live across
    /// it is lost. A call, whose arguments are read before it and whose
    /// result is written after.
    Operands,
    /// These operands, while not live across: the instruction reads them
    /// before it writes the register, or writes them last. x86-64's rule
    /// (`ConstraintPoint::operand_survives`).
    Ends(Vec<PseudoId>),
    /// These operands, live across or not. aarch64's rule.
    Always(Vec<PseudoId>),
    /// Nothing, an operand included: the instruction writes the register
    /// while its operands are still wanted.
    Nothing,
}

/// Registers an instruction overwrites.
#[derive(Debug, Clone)]
pub struct Clobber<U> {
    /// The instruction, by linear position (blocks in order, as
    /// `compute_live_intervals` numbers them).
    pub position: usize,
    /// What it overwrites.
    pub regs: Vec<U>,
    /// Whose value survives it anyway.
    pub spared: Spared,
    /// What overwrites them, for the report.
    pub why: &'static str,
}

/// A function's assignment, and what the verifier needs to know of the
/// backend to judge it.
pub struct Assignment<'a, U, L> {
    pub func: &'a Function,
    /// Per-block live-out sets, from `compute_live_intervals`.
    pub live_out: &'a [HashSet<PseudoId>],
    /// Where each pseudo lives.
    pub locations: &'a HashMap<PseudoId, L>,
    /// The cells a location occupies: none for a constant or a global, one
    /// for most, several for a value spanning registers.
    pub cells: &'a dyn Fn(PseudoId, &L) -> Vec<Cell<U>>,
    /// Every register some instruction overwrites, besides its defs.
    pub clobbers: Vec<Clobber<U>>,
    /// What the function keeps for itself throughout, by name: frame slots
    /// such as the canary's, and registers no value may be handed -- the
    /// frame base, a global register variable, a codegen scratch.
    pub reserved: Vec<(&'static str, Cell<U>)>,
    /// May a def in this cell share it with a source of the same instruction
    /// that dies there? Only where the backend reads every source before it
    /// writes the def.
    pub def_may_share_src: &'a dyn Fn(&Instruction, &Cell<U>) -> bool,
}

/// What kind of collision a [`Violation`] is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Conflict {
    /// A def lands where a value live just after it is kept.
    LiveAtDef,
    /// A def lands on a source of its own instruction the backend may still
    /// read after writing it.
    Source,
    /// Two defs of one instruction land in one place.
    Defs,
    /// An asm output lands on an operand it may not share with.
    AsmOperands,
    /// An instruction overwrites a register holding a value it does not
    /// spare.
    Clobbered,
    /// Two arguments arrive in one place.
    Arguments,
    /// A value live across a `setjmp` shares with a def the `setjmp` reaches.
    AcrossSetjmp,
    /// A local inside its lifetime shares bytes with a live value or another
    /// local inside its own.
    Local,
    /// A value or a local is placed over something the function reserves.
    Reserved,
}

/// One collision the verifier found.
#[derive(Debug, Clone)]
pub struct Violation {
    pub conflict: Conflict,
    pub first: PseudoId,
    pub second: Option<PseudoId>,
    /// The two pseudos, their locations and the program point.
    pub message: String,
}

/// Check `a` and panic -- an internal compiler error -- listing every
/// collision found. The backends call this in a debug build only.
///
/// Once an error has been reported nothing is emitted, and the assignment
/// may be one the allocator gave up on: a frame past the ceiling, whose
/// slots `grow_frame` stops placing. So it is not checked.
pub fn verify_assignment<U, L>(a: &Assignment<'_, U, L>)
where
    U: Copy + Eq + Hash + Debug,
    L: Debug,
{
    if crate::diag::has_error() != 0 {
        return;
    }
    let found = check_assignment(a);
    if found.is_empty() {
        return;
    }
    let list = found
        .iter()
        .take(20)
        .map(|v| v.message.as_str())
        .collect::<Vec<_>>()
        .join("\n  ");
    panic!(
        "internal compiler error: register allocation of `{}` put two values \
         in one place ({} found):\n  {list}",
        a.func.name,
        found.len()
    );
}

/// Every collision in `a`'s assignment, the first point each was seen at.
pub fn check_assignment<U, L>(a: &Assignment<'_, U, L>) -> Vec<Violation>
where
    U: Copy + Eq + Hash + Debug,
    L: Debug,
{
    let mut v = Verifier::new(a);
    v.check_reserved();
    v.walk();
    v.violations
}

/// The distinct cells of one function, each with the cells it overlaps.
struct CellTable<U> {
    cells: Vec<Cell<U>>,
    index: HashMap<Cell<U>, usize>,
    /// For each cell, every cell sharing a register or a byte with it,
    /// itself included.
    overlaps: Vec<Vec<usize>>,
}

impl<U: Copy + Eq + Hash> CellTable<U> {
    fn new() -> Self {
        Self {
            cells: Vec::new(),
            index: HashMap::new(),
            overlaps: Vec::new(),
        }
    }

    fn intern(&mut self, c: Cell<U>) -> usize {
        let next = self.cells.len();
        *self.index.entry(c).or_insert_with(|| {
            self.cells.push(c);
            next
        })
    }

    /// Fill in `overlaps`. A register or an incoming slot overlaps only
    /// itself; frame ranges are swept in order of their low end, so the work
    /// is the cells plus the overlapping pairs.
    fn finish(&mut self) {
        self.overlaps = (0..self.cells.len()).map(|i| vec![i]).collect();
        let mut frames: Vec<(i64, i64, usize)> = self
            .cells
            .iter()
            .enumerate()
            .filter_map(|(i, c)| match *c {
                Cell::Frame { lo, hi } => Some((lo, hi, i)),
                _ => None,
            })
            .collect();
        frames.sort_unstable();
        for (n, &(_, hi, i)) in frames.iter().enumerate() {
            for &(lo2, _, j) in &frames[n + 1..] {
                if lo2 >= hi {
                    break;
                }
                self.overlaps[i].push(j);
                self.overlaps[j].push(i);
            }
        }
    }

    fn find(&self, c: &Cell<U>) -> Option<usize> {
        self.index.get(c).copied()
    }

    /// Whether two pseudos' cell lists share anything.
    fn any_overlap(&self, a: &[usize], b: &[usize]) -> bool {
        a.iter()
            .any(|&c| self.overlaps[c].iter().any(|o| b.contains(o)))
    }
}

/// The live set of the backward walk, and which live pseudo is in each cell.
struct Occupancy<'c> {
    cells_of: &'c HashMap<PseudoId, Vec<usize>>,
    live: HashSet<PseudoId>,
    occupants: Vec<Vec<PseudoId>>,
}

impl<'c> Occupancy<'c> {
    fn new(cells_of: &'c HashMap<PseudoId, Vec<usize>>, n: usize) -> Self {
        Self {
            cells_of,
            live: HashSet::new(),
            occupants: vec![Vec::new(); n],
        }
    }

    fn clear(&mut self) {
        for p in self.live.drain() {
            for &c in &self.cells_of[&p] {
                self.occupants[c].clear();
            }
        }
    }
}

impl LiveSet for Occupancy<'_> {
    fn kill(&mut self, p: PseudoId) {
        if self.live.remove(&p) {
            for &c in &self.cells_of[&p] {
                self.occupants[c].retain(|&q| q != p);
            }
        }
    }

    fn revive(&mut self, p: PseudoId) {
        if self.live.insert(p) {
            for &c in &self.cells_of[&p] {
                self.occupants[c].push(p);
            }
        }
    }
}

/// The pseudos live across a `setjmp` that one block must preserve: through
/// all of it, where a `setjmp` reaches the block, or after the instruction,
/// in the `setjmp`'s own block.
#[derive(Default)]
struct SetjmpCarry {
    whole: Vec<PseudoId>,
    after: Vec<(usize, Vec<PseudoId>)>,
}

/// The aliasing locals -- those whose slot shares a byte with something else
/// -- and what each shares with.
struct LocalAliases {
    regions: LocalRegions,
    /// By the local's index in `regions.locals`: the other locals and the
    /// pseudos whose cells overlap its slot. Only locals with a partner.
    partners: HashMap<usize, (Vec<usize>, Vec<PseudoId>)>,
}

impl LocalAliases {
    /// Every aliasing local, in order.
    fn aliasing(&self) -> Vec<usize> {
        let mut all: Vec<usize> = self.partners.keys().copied().collect();
        all.sort_unstable();
        all
    }
}

struct Verifier<'a, U, L> {
    a: &'a Assignment<'a, U, L>,
    table: CellTable<U>,
    /// The cells of each pseudo the walk tracks: every one with a place,
    /// except the locals.
    cells_of: HashMap<PseudoId, Vec<usize>>,
    /// Each local's slot.
    local_cell: HashMap<PseudoId, usize>,
    reserved: Vec<(&'static str, usize)>,
    block_start: Vec<usize>,
    bb_idx: HashMap<BasicBlockId, usize>,
    clobbers_at: HashMap<usize, Vec<usize>>,
    found: BTreeSet<(Conflict, PseudoId, Option<PseudoId>)>,
    violations: Vec<Violation>,
}

impl<'a, U, L> Verifier<'a, U, L>
where
    U: Copy + Eq + Hash + Debug,
    L: Debug,
{
    fn new(a: &'a Assignment<'a, U, L>) -> Self {
        let func = a.func;
        let mut table = CellTable::new();
        let mut cells_of = HashMap::new();
        let mut local_cell = HashMap::new();
        let mut placed: Vec<(&PseudoId, &L)> = a.locations.iter().collect();
        placed.sort_by_key(|(p, _)| **p);
        for (&p, loc) in placed {
            let cells: Vec<usize> = (a.cells)(p, loc)
                .into_iter()
                .map(|c| table.intern(c))
                .collect();
            if cells.is_empty() {
                continue;
            }
            if func.local_of(p).is_some() {
                // A local is one object, so one slot.
                local_cell.insert(p, cells[0]);
            } else {
                cells_of.insert(p, cells);
            }
        }
        let reserved = a
            .reserved
            .iter()
            .map(|&(name, c)| (name, table.intern(c)))
            .collect();
        table.finish();

        let mut block_start = Vec::with_capacity(func.blocks.len());
        let mut bb_idx = HashMap::with_capacity(func.blocks.len());
        let mut pos = 0;
        for (i, block) in func.blocks.iter().enumerate() {
            block_start.push(pos);
            bb_idx.insert(block.id, i);
            pos += block.insns.len();
        }
        let mut clobbers_at: HashMap<usize, Vec<usize>> = HashMap::new();
        for (i, c) in a.clobbers.iter().enumerate() {
            clobbers_at.entry(c.position).or_default().push(i);
        }
        Self {
            a,
            table,
            cells_of,
            local_cell,
            reserved,
            block_start,
            bb_idx,
            clobbers_at,
            found: BTreeSet::new(),
            violations: Vec::new(),
        }
    }

    fn describe(&self, p: PseudoId) -> String {
        match self.a.locations.get(&p) {
            Some(loc) => format!("{p} ({loc:?})"),
            None => format!("{p} (nowhere)"),
        }
    }

    fn point(&self, b: usize, i: usize) -> String {
        let block = &self.a.func.blocks[b];
        match block.insns.get(i) {
            Some(insn) => format!(
                "{:?} at {} #{i} (position {})",
                insn.op,
                block.id,
                self.block_start[b] + i
            ),
            None => format!("the start of {}", block.id),
        }
    }

    fn report(
        &mut self,
        conflict: Conflict,
        first: PseudoId,
        second: Option<PseudoId>,
        what: &str,
        at: String,
    ) {
        // One report per pair, at the first point it is seen.
        let key = match second {
            Some(s) => (conflict, first.min(s), Some(first.max(s))),
            None => (conflict, first, None),
        };
        if !self.found.insert(key) {
            return;
        }
        let pair = match second {
            Some(s) => format!("{} and {}", self.describe(first), self.describe(s)),
            None => self.describe(first),
        };
        self.violations.push(Violation {
            conflict,
            first,
            second,
            message: format!("{conflict:?}: {pair} {what}, at {at}"),
        });
    }

    /// Nothing is placed over what the function keeps for itself throughout:
    /// a frame slot such as the canary's, or a register no value is handed
    /// -- the frame base, a global register variable, a codegen scratch.
    /// Except an argument, which arrives where the ABI puts it.
    fn check_reserved(&mut self) {
        let mut hits = Vec::new();
        for &(name, rc) in &self.reserved {
            let in_register = matches!(self.table.cells[rc], Cell::Reg(_));
            let is_arg = |p: &PseudoId| {
                self.a
                    .func
                    .get_pseudo(*p)
                    .is_some_and(|ps| matches!(ps.kind, PseudoKind::Arg(_)))
            };
            let reserved_here =
                |cells: &[usize]| cells.iter().any(|c| self.table.overlaps[rc].contains(c));
            let mut owners: Vec<PseudoId> = self
                .cells_of
                .iter()
                .filter(|(p, cells)| reserved_here(cells) && !(in_register && is_arg(p)))
                .map(|(&p, _)| p)
                .chain(
                    self.local_cell
                        .iter()
                        .filter(|(_, &c)| reserved_here(&[c]))
                        .map(|(&p, _)| p),
                )
                .collect();
            owners.sort();
            hits.extend(owners.into_iter().map(|p| (p, name)));
        }
        for (p, name) in hits {
            let what = format!("is placed over {name}");
            self.report(
                Conflict::Reserved,
                p,
                None,
                &what,
                "every point".to_string(),
            );
        }
    }

    /// Which locals share bytes with anything, and with what. `None` when
    /// none does, which is the usual case: then no local is looked at again.
    fn local_aliases(&self) -> Option<LocalAliases> {
        let mut owners: HashMap<usize, Vec<PseudoId>> = HashMap::new();
        for (&p, cells) in &self.cells_of {
            for &c in cells {
                owners.entry(c).or_default().push(p);
            }
        }
        let regions = LocalRegions::of(self.a.func, &self.block_start, &self.bb_idx);
        let index: HashMap<PseudoId, usize> = regions
            .locals
            .iter()
            .enumerate()
            .map(|(i, &l)| (l, i))
            .collect();
        let mut by_cell: HashMap<usize, Vec<usize>> = HashMap::new();
        for (l, &c) in &self.local_cell {
            if let Some(&i) = index.get(l) {
                by_cell.entry(c).or_default().push(i);
            }
        }
        let mut partners = HashMap::new();
        for (l, &c) in &self.local_cell {
            let Some(&i) = index.get(l) else { continue };
            let mut locals = Vec::new();
            let mut pseudos = Vec::new();
            for &o in &self.table.overlaps[c] {
                locals.extend(by_cell.get(&o).into_iter().flatten().filter(|&&j| j != i));
                pseudos.extend(owners.get(&o).into_iter().flatten());
            }
            if locals.is_empty() && pseudos.is_empty() {
                continue;
            }
            locals.sort_unstable();
            locals.dedup();
            pseudos.sort();
            pseudos.dedup();
            partners.insert(i, (locals, pseudos));
        }
        if partners.is_empty() {
            return None;
        }
        Some(LocalAliases { regions, partners })
    }

    /// For each block, the pseudos live across a `setjmp` that must also be
    /// preserved there.
    fn across_setjmp(&self) -> Vec<SetjmpCarry> {
        let func = self.a.func;
        let n = func.blocks.len();
        let mut out: Vec<SetjmpCarry> = (0..n).map(|_| SetjmpCarry::default()).collect();
        let tracked = |p: PseudoId| self.cells_of.contains_key(&p);
        for (b, block) in func.blocks.iter().enumerate() {
            if !block.insns.iter().any(|i| i.returns_twice()) {
                continue;
            }
            let mut live: BTreeSet<PseudoId> = self.a.live_out[b]
                .iter()
                .copied()
                .filter(|&p| tracked(p))
                .collect();
            let mut found = Vec::new();
            for (i, insn) in block.insns.iter().enumerate().rev() {
                let du = DefsUses::of(insn, tracked);
                if insn.returns_twice() {
                    let across: Vec<PseudoId> = live
                        .iter()
                        .copied()
                        .filter(|p| !du.defs.contains(p))
                        .collect();
                    found.push((i, across));
                }
                du.step(&mut live);
            }
            // Every block a `setjmp` reaches, through any successor.
            let mut reached = vec![false; n];
            let mut work: Vec<usize> = block
                .children
                .iter()
                .filter_map(|c| self.bb_idx.get(c).copied())
                .collect();
            while let Some(r) = work.pop() {
                if std::mem::replace(&mut reached[r], true) {
                    continue;
                }
                work.extend(
                    func.blocks[r]
                        .children
                        .iter()
                        .filter_map(|c| self.bb_idx.get(c).copied()),
                );
            }
            for (i, across) in found {
                for (r, _) in reached.iter().enumerate().filter(|(_, &on)| on) {
                    out[r].whole.extend(across.iter().copied());
                }
                if !reached[b] {
                    out[b].after.push((i, across));
                }
            }
        }
        for carry in &mut out {
            carry.whole.sort();
            carry.whole.dedup();
        }
        out
    }

    /// For each instruction of block `b`, the aliasing locals inside their
    /// lifetime just after it.
    fn locals_after(&self, aliases: &LocalAliases, b: usize) -> Vec<Vec<usize>> {
        let n = self.a.func.blocks[b].insns.len();
        let aliasing = |i: &usize| aliases.partners.contains_key(i);
        if aliases.regions.everywhere {
            return vec![aliases.aliasing(); n];
        }
        let mut state: BTreeSet<usize> =
            aliases.regions.live_in[b].iter().filter(aliasing).collect();
        let events = &aliases.regions.events[b];
        let mut next = 0;
        let mut out = Vec::with_capacity(n);
        for i in 0..n {
            let pos = self.block_start[b] + i;
            while next < events.len() && events[next].0 == pos {
                let (_, l, starts) = events[next];
                if aliasing(&l) {
                    if starts {
                        state.insert(l);
                    } else {
                        state.remove(&l);
                    }
                }
                next += 1;
            }
            out.push(state.iter().copied().collect());
        }
        out
    }

    /// The aliasing locals inside their lifetime entering block `b`.
    fn locals_in(&self, aliases: &LocalAliases, b: usize) -> Vec<usize> {
        if aliases.regions.everywhere {
            return aliases.aliasing();
        }
        aliases.regions.live_in[b]
            .iter()
            .filter(|i| aliases.partners.contains_key(i))
            .collect()
    }

    fn walk(&mut self) {
        let func = self.a.func;
        let aliases = self.local_aliases();
        let setjmp = if func
            .blocks
            .iter()
            .flat_map(|b| &b.insns)
            .any(|i| i.returns_twice())
        {
            self.across_setjmp()
        } else {
            Vec::new()
        };
        let cells_of = std::mem::take(&mut self.cells_of);
        let mut occ = Occupancy::new(&cells_of, self.table.cells.len());
        let entry = self.bb_idx.get(&func.entry).copied();
        for (b, block) in func.blocks.iter().enumerate() {
            occ.clear();
            for &p in &self.a.live_out[b] {
                if cells_of.contains_key(&p) {
                    occ.revive(p);
                }
            }
            let locals_after = aliases.as_ref().map(|al| self.locals_after(al, b));
            for (i, insn) in block.insns.iter().enumerate().rev() {
                let du = DefsUses::of(insn, |p| cells_of.contains_key(&p));
                self.check_defs(&cells_of, &occ, b, i, insn, &du);
                self.check_clobbers(&cells_of, &occ, b, i, &du);
                if let Some(carry) = setjmp.get(b) {
                    self.check_setjmp(&cells_of, carry, b, i, insn, &du);
                }
                if let (Some(al), Some(after)) = (&aliases, &locals_after) {
                    let mut values: Vec<PseudoId> = occ.live.iter().copied().collect();
                    values.extend(&du.defs);
                    self.check_locals(al, &after[i], &values, b, i);
                }
                du.step(&mut occ);
            }
            // Coming into the block.
            if let Some(al) = &aliases {
                let values: Vec<PseudoId> = occ.live.iter().copied().collect();
                let inside = self.locals_in(al, b);
                self.check_locals(al, &inside, &values, b, usize::MAX);
            }
            if Some(b) == entry {
                self.check_arguments(&cells_of, &occ, b);
            }
        }
        self.cells_of = cells_of;
    }

    /// `d = copy q`, with `d` landing exactly where `q` lives: one value.
    fn is_identity_copy(
        cells_of: &HashMap<PseudoId, Vec<usize>>,
        insn: &Instruction,
        d: PseudoId,
        q: PseudoId,
    ) -> bool {
        insn.op == Opcode::Copy && insn.src.first() == Some(&q) && cells_of[&d] == cells_of[&q]
    }

    /// Each def against what is live after it, the other defs, its own
    /// instruction's sources, and an asm's operands.
    fn check_defs(
        &mut self,
        cells_of: &HashMap<PseudoId, Vec<usize>>,
        occ: &Occupancy<'_>,
        b: usize,
        i: usize,
        insn: &Instruction,
        du: &DefsUses,
    ) {
        let mut hits: Vec<(Conflict, PseudoId, PseudoId, &'static str)> = Vec::new();
        for &d in &du.defs {
            let cells = &cells_of[&d];
            for &c in cells {
                for &o in &self.table.overlaps[c] {
                    for &q in &occ.occupants[o] {
                        if q != d && !Self::is_identity_copy(cells_of, insn, d, q) {
                            hits.push((
                                Conflict::LiveAtDef,
                                d,
                                q,
                                "share a place: a def overwrites a value still live",
                            ));
                        }
                    }
                }
            }
            for &d2 in &du.defs {
                if d2 > d && self.table.any_overlap(cells, &cells_of[&d2]) {
                    hits.push((Conflict::Defs, d, d2, "are both written by one instruction"));
                }
            }
            for &s in &insn.src {
                if s == d || occ.live.contains(&s) {
                    continue;
                }
                let Some(src_cells) = cells_of.get(&s) else {
                    continue;
                };
                let shared = cells.iter().any(|&c| {
                    self.table.overlaps[c].iter().any(|o| src_cells.contains(o))
                        && !(self.a.def_may_share_src)(insn, &self.table.cells[c])
                });
                if shared {
                    hits.push((
                        Conflict::Source,
                        d,
                        s,
                        "share a place: the def is written before the source is read",
                    ));
                }
            }
        }
        if let Some(asm) = insn
            .extra()
            .asm_data
            .as_deref()
            .filter(|_| insn.op == Opcode::Asm)
        {
            for (out, read) in asm_must_differ(asm) {
                if let (Some(oc), Some(rc)) = (cells_of.get(&out), cells_of.get(&read)) {
                    if self.table.any_overlap(oc, rc) {
                        hits.push((
                            Conflict::AsmOperands,
                            out,
                            read,
                            "share a place: an asm output and an operand it is written over",
                        ));
                    }
                }
            }
        }
        for (conflict, x, y, what) in hits {
            let at = self.point(b, i);
            self.report(conflict, x, Some(y), what, at);
        }
    }

    /// The registers the instruction overwrites hold nothing it does not
    /// spare.
    fn check_clobbers(
        &mut self,
        cells_of: &HashMap<PseudoId, Vec<usize>>,
        occ: &Occupancy<'_>,
        b: usize,
        i: usize,
        du: &DefsUses,
    ) {
        let pos = self.block_start[b] + i;
        let Some(list) = self.clobbers_at.get(&pos) else {
            return;
        };
        let mut hits = Vec::new();
        for &k in list {
            let clobber = &self.a.clobbers[k];
            for &u in &clobber.regs {
                let Some(c) = self.table.find(&Cell::Reg(u)) else {
                    continue;
                };
                let mut touching: Vec<PseudoId> = occ.occupants[c].clone();
                touching.extend(
                    du.defs
                        .iter()
                        .chain(&du.uses)
                        .filter(|p| cells_of[p].contains(&c)),
                );
                touching.sort();
                touching.dedup();
                for p in touching {
                    let across = occ.live.contains(&p) && !du.defs.contains(&p);
                    let spared = match &clobber.spared {
                        Spared::Operands => !across,
                        Spared::Ends(list) => list.contains(&p) && !across,
                        Spared::Always(list) => list.contains(&p),
                        Spared::Nothing => false,
                    };
                    if !spared {
                        hits.push((p, u, clobber.why));
                    }
                }
            }
        }
        for (p, u, why) in hits {
            let what = format!("is in {u:?}, which {why} overwrites");
            let at = self.point(b, i);
            self.report(Conflict::Clobbered, p, None, &what, at);
        }
    }

    /// A def at a point a `setjmp` reaches lands nowhere a value live across
    /// that `setjmp` is kept.
    fn check_setjmp(
        &mut self,
        cells_of: &HashMap<PseudoId, Vec<usize>>,
        carry: &SetjmpCarry,
        b: usize,
        i: usize,
        insn: &Instruction,
        du: &DefsUses,
    ) {
        let here = carry.whole.iter().chain(
            carry
                .after
                .iter()
                .filter(|(at, _)| *at < i)
                .flat_map(|(_, values)| values),
        );
        let mut hits = Vec::new();
        for &r in here {
            for &d in &du.defs {
                if r != d
                    && !Self::is_identity_copy(cells_of, insn, d, r)
                    && self.table.any_overlap(&cells_of[&d], &cells_of[&r])
                {
                    hits.push((d, r));
                }
            }
        }
        for (d, r) in hits {
            let at = self.point(b, i);
            self.report(
                Conflict::AcrossSetjmp,
                d,
                Some(r),
                "share a place: the second is live across a setjmp this def follows",
                at,
            );
        }
    }

    /// Locals inside their lifetime against each other and against the
    /// values live at one point.
    fn check_locals(
        &mut self,
        aliases: &LocalAliases,
        inside: &[usize],
        values: &[PseudoId],
        b: usize,
        i: usize,
    ) {
        let mut hits = Vec::new();
        for &l in inside {
            let (locals, pseudos) = &aliases.partners[&l];
            let local = aliases.regions.locals[l];
            for m in locals.iter().filter(|m| inside.contains(m)) {
                let other = aliases.regions.locals[*m];
                hits.push((local.min(other), local.max(other)));
            }
            for q in pseudos.iter().filter(|q| values.contains(q)) {
                hits.push((local, *q));
            }
        }
        for (x, y) in hits {
            let at = self.point(b, i);
            self.report(
                Conflict::Local,
                x,
                Some(y),
                "share bytes inside the local's lifetime",
                at,
            );
        }
    }

    /// The arguments live coming into the function arrive in distinct
    /// places. Any other value live there is undefined on some path, and
    /// may sit anywhere until it is defined.
    fn check_arguments(
        &mut self,
        cells_of: &HashMap<PseudoId, Vec<usize>>,
        occ: &Occupancy<'_>,
        b: usize,
    ) {
        let is_arg = |p: &PseudoId| {
            self.a
                .func
                .get_pseudo(*p)
                .is_some_and(|ps| matches!(ps.kind, PseudoKind::Arg(_)))
        };
        let mut args: Vec<PseudoId> = occ.live.iter().copied().filter(is_arg).collect();
        args.sort();
        let mut hits = Vec::new();
        for (n, &x) in args.iter().enumerate() {
            for &y in &args[n + 1..] {
                if self.table.any_overlap(&cells_of[&x], &cells_of[&y]) {
                    hits.push((x, y));
                }
            }
        }
        for (x, y) in hits {
            let at = self.point(b, usize::MAX);
            self.report(Conflict::Arguments, x, Some(y), "arrive in one place", at);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arch::regalloc::compute_live_intervals;
    use crate::ir::{BasicBlock, Pseudo};
    use crate::types::TypeTable;
    use crate::units::Bits;

    /// A location in the tests' toy machine: a register, `bytes` of frame
    /// from `lo`, or a constant.
    #[derive(Debug, Clone, Copy)]
    enum Loc {
        R(u8),
        S(i32, i32),
        Imm,
    }

    fn cells(_: PseudoId, loc: &Loc) -> Vec<Cell<u8>> {
        match *loc {
            Loc::R(r) => vec![Cell::Reg(r)],
            Loc::S(lo, bytes) => vec![Cell::Frame {
                lo: i64::from(lo),
                hi: i64::from(lo + bytes),
            }],
            Loc::Imm => Vec::new(),
        }
    }

    /// `%9` is the constant 1; every other pseudo up to `%8` a register.
    const ONE: PseudoId = PseudoId(9);

    fn types() -> TypeTable {
        TypeTable::new(&crate::target::Target::host())
    }

    fn p(n: u32) -> PseudoId {
        PseudoId(n)
    }

    /// A function of one block running `insns`, with `syms` as locals.
    fn one_block(types: &TypeTable, syms: &[(u32, &str)], insns: Vec<Instruction>) -> Function {
        with_args(types, syms, &[], insns)
    }

    /// ... and `args` as its parameters, in order.
    fn with_args(
        types: &TypeTable,
        syms: &[(u32, &str)],
        args: &[u32],
        insns: Vec<Instruction>,
    ) -> Function {
        let mut f = Function::new("f", types.long_id);
        for id in 0..9 {
            if let Some(&(_, name)) = syms.iter().find(|(s, _)| *s == id) {
                f.add_pseudo(Pseudo::sym(p(id), name.into()));
                f.add_local(name, p(id), types.long_id, None, None);
            } else if let Some(n) = args.iter().position(|&a| a == id) {
                f.add_pseudo(Pseudo::arg(p(id), n as u32));
                f.add_param(format!("a{n}"), types.long_id);
            } else {
                f.add_pseudo(Pseudo::reg(p(id), id));
            }
        }
        f.add_pseudo(Pseudo::val(ONE, 1));
        f.next_pseudo = 10;
        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        for insn in insns {
            bb.add_insn(insn);
        }
        f.add_block(bb);
        f.entry = BasicBlockId(0);
        f
    }

    fn copy(types: &TypeTable, target: u32, src: PseudoId) -> Instruction {
        Instruction::unop(Opcode::Copy, p(target), src, types.long_id, Bits::new(64))
    }

    fn add(types: &TypeTable, target: u32, a: PseudoId, b: PseudoId) -> Instruction {
        Instruction::binop(Opcode::Add, p(target), a, b, types.long_id, Bits::new(64))
    }

    /// Check `func` with `placed`, `clobbers` and `reserved`; `share` says
    /// whether a def may land on a source that dies at its instruction.
    fn check(
        func: &Function,
        placed: &[(u32, Loc)],
        clobbers: Vec<Clobber<u8>>,
        reserved: Vec<(&'static str, Cell<u8>)>,
        share: bool,
    ) -> Vec<Violation> {
        let liveness = compute_live_intervals(func, |_| None::<(Vec<u8>, Vec<PseudoId>)>);
        let mut locations: HashMap<PseudoId, Loc> =
            placed.iter().map(|&(n, l)| (p(n), l)).collect();
        locations.insert(ONE, Loc::Imm);
        let def_may_share_src = |insn: &Instruction, _: &Cell<u8>| share || insn.op == Opcode::Copy;
        check_assignment(&Assignment {
            func,
            live_out: &liveness.live_out,
            locations: &locations,
            cells: &cells,
            clobbers,
            reserved,
            def_may_share_src: &def_may_share_src,
        })
    }

    fn conflicts(found: &[Violation]) -> Vec<(Conflict, PseudoId, Option<PseudoId>)> {
        found
            .iter()
            .map(|v| (v.conflict, v.first, v.second))
            .collect()
    }

    /// `%1 = 1; %2 = 1; %3 = %1 + %2`: `%2` is written while `%1` is still
    /// wanted.
    fn two_live(types: &TypeTable) -> Function {
        one_block(
            types,
            &[],
            vec![
                copy(types, 1, ONE),
                copy(types, 2, ONE),
                add(types, 3, p(1), p(2)),
                Instruction::ret(Some(p(3))),
            ],
        )
    }

    #[test]
    fn two_live_values_in_one_register_are_caught() {
        let types = types();
        let f = two_live(&types);
        let placed = [(1, Loc::R(0)), (2, Loc::R(0)), (3, Loc::R(1))];
        let found = check(&f, &placed, vec![], vec![], true);
        assert_eq!(
            conflicts(&found),
            [(Conflict::LiveAtDef, p(2), Some(p(1)))],
            "{found:?}"
        );
        // The report names the function's pseudos, their places and the
        // instruction.
        let message = &found[0].message;
        assert!(message.contains("%2 (R(0))"), "{message}");
        assert!(message.contains("%1 (R(0))"), "{message}");
        assert!(message.contains("Copy at .L0 #2"), "{message}");

        let placed = [(1, Loc::R(0)), (2, Loc::R(1)), (3, Loc::R(0))];
        let found = check(&f, &placed, vec![], vec![], true);
        assert!(found.is_empty(), "{found:?}");
    }

    #[test]
    fn overlapping_stack_bytes_are_caught() {
        let types = types();
        let f = two_live(&types);
        // Eight bytes from -16 and eight from -12 share four.
        let placed = [(1, Loc::S(-16, 8)), (2, Loc::S(-12, 8)), (3, Loc::R(0))];
        let found = check(&f, &placed, vec![], vec![], true);
        assert_eq!(
            conflicts(&found),
            [(Conflict::LiveAtDef, p(2), Some(p(1)))],
            "{found:?}"
        );
        // A sixteen-byte slot covers an eight-byte one inside it.
        let placed = [(1, Loc::S(-32, 16)), (2, Loc::S(-24, 8)), (3, Loc::R(0))];
        let found = check(&f, &placed, vec![], vec![], true);
        assert_eq!(found.len(), 1, "{found:?}");
        // Adjacent slots share nothing.
        let placed = [(1, Loc::S(-16, 8)), (2, Loc::S(-8, 8)), (3, Loc::R(0))];
        let found = check(&f, &placed, vec![], vec![], true);
        assert!(found.is_empty(), "{found:?}");
    }

    /// `%2 = copy %1` may land where `%1` lives with both read later: one
    /// value. The same placement for any other instruction may not -- even
    /// one where the backend reads its sources first.
    #[test]
    fn an_identity_copy_is_accepted() {
        let types = types();
        let placed = [(1, Loc::R(0)), (2, Loc::R(0)), (3, Loc::R(1))];
        let f = one_block(
            &types,
            &[],
            vec![
                copy(&types, 1, ONE),
                copy(&types, 2, p(1)),
                add(&types, 3, p(1), p(2)),
                Instruction::ret(Some(p(3))),
            ],
        );
        let found = check(&f, &placed, vec![], vec![], false);
        assert!(found.is_empty(), "{found:?}");

        let f = one_block(
            &types,
            &[],
            vec![
                copy(&types, 1, ONE),
                add(&types, 2, p(1), ONE),
                add(&types, 3, p(1), p(2)),
                Instruction::ret(Some(p(3))),
            ],
        );
        let found = check(&f, &placed, vec![], vec![], true);
        assert_eq!(
            conflicts(&found),
            [(Conflict::LiveAtDef, p(2), Some(p(1)))],
            "{found:?}"
        );
    }

    /// `%2 = %1 + 1` with `%1` dying there: the def may take `%1`'s place
    /// -- unless the backend writes the def before it reads its sources.
    #[test]
    fn a_value_dying_into_its_def_is_accepted() {
        let types = types();
        let f = one_block(
            &types,
            &[],
            vec![
                copy(&types, 1, ONE),
                add(&types, 2, p(1), ONE),
                Instruction::ret(Some(p(2))),
            ],
        );
        let placed = [(1, Loc::R(3)), (2, Loc::R(3))];
        let found = check(&f, &placed, vec![], vec![], true);
        assert!(found.is_empty(), "{found:?}");

        let found = check(&f, &placed, vec![], vec![], false);
        assert_eq!(
            conflicts(&found),
            [(Conflict::Source, p(2), Some(p(1)))],
            "{found:?}"
        );
    }

    /// A call overwrites a register holding a value live across it, but not
    /// one holding its own argument or its result; an instruction that
    /// writes a register while its operands are still wanted spares neither.
    #[test]
    fn a_clobbered_register_holds_only_what_is_spared() {
        let types = types();
        let f = one_block(
            &types,
            &[],
            vec![
                copy(&types, 1, ONE),
                copy(&types, 2, ONE),
                Instruction::call(
                    Some(p(3)),
                    "g",
                    vec![p(2)],
                    vec![types.long_id],
                    types.long_id,
                    Bits::new(64),
                ),
                add(&types, 4, p(1), p(3)),
                Instruction::ret(Some(p(4))),
            ],
        );
        let at_call = |regs: Vec<u8>, spared: Spared| Clobber {
            position: 3,
            regs,
            spared,
            why: "the call",
        };
        let placed = [
            (1, Loc::R(5)),
            (2, Loc::R(0)),
            (3, Loc::R(0)),
            (4, Loc::R(0)),
        ];
        let found = check(
            &f,
            &placed,
            vec![at_call(vec![0], Spared::Operands)],
            vec![],
            true,
        );
        assert!(found.is_empty(), "{found:?}");

        let found = check(
            &f,
            &placed,
            vec![at_call(vec![0, 5], Spared::Operands)],
            vec![],
            true,
        );
        assert_eq!(
            conflicts(&found),
            [(Conflict::Clobbered, p(1), None)],
            "{found:?}"
        );
        assert!(found[0].message.contains("which the call overwrites"));

        let found = check(
            &f,
            &placed,
            vec![at_call(vec![0], Spared::Nothing)],
            vec![],
            true,
        );
        assert_eq!(
            conflicts(&found),
            [
                (Conflict::Clobbered, p(2), None),
                (Conflict::Clobbered, p(3), None)
            ],
            "{found:?}"
        );
        // An operand spared at the ends of its range only is lost when it is
        // live across.
        let f = one_block(
            &types,
            &[],
            vec![
                copy(&types, 1, ONE),
                add(&types, 2, p(1), ONE),
                add(&types, 3, p(1), p(2)),
                Instruction::ret(Some(p(3))),
            ],
        );
        let at_add = |spared: Spared| Clobber {
            position: 2,
            regs: vec![0],
            spared,
            why: "the add",
        };
        let placed = [(1, Loc::R(0)), (2, Loc::R(1)), (3, Loc::R(0))];
        let found = check(
            &f,
            &placed,
            vec![at_add(Spared::Always(vec![p(1)]))],
            vec![],
            true,
        );
        assert!(found.is_empty(), "{found:?}");
        let found = check(
            &f,
            &placed,
            vec![at_add(Spared::Ends(vec![p(1)]))],
            vec![],
            true,
        );
        assert_eq!(
            conflicts(&found),
            [(Conflict::Clobbered, p(1), None)],
            "{found:?}"
        );
    }

    /// Locals `a` (`%5`) and `b` (`%6`) share a slot: fine while `a`'s
    /// lifetime ends before `b`'s begins, and not once `a` is read again
    /// inside `b`'s. Nor may a spill slot share a local's bytes inside its
    /// lifetime.
    #[test]
    fn locals_share_a_slot_only_outside_each_others_lifetimes() {
        let types = types();
        let scopes = |reread: bool| {
            let load = |t: u32, from: u32| {
                Instruction::load(p(t), p(from), 0, types.long_id, Bits::new(64))
            };
            let mut insns = vec![load(1, 5), Instruction::lifetime_end(p(5)), load(2, 6)];
            if reread {
                insns.push(load(3, 5));
            }
            insns.push(Instruction::lifetime_end(p(6)));
            insns.push(Instruction::ret(Some(p(2))));
            one_block(&types, &[(5, "a.5"), (6, "b.6")], insns)
        };
        let placed = [
            (1, Loc::R(0)),
            (2, Loc::R(0)),
            (3, Loc::R(1)),
            (5, Loc::S(-8, 8)),
            (6, Loc::S(-8, 8)),
        ];
        let found = check(&scopes(false), &placed, vec![], vec![], true);
        assert!(found.is_empty(), "{found:?}");
        let found = check(&scopes(true), &placed, vec![], vec![], true);
        assert_eq!(
            conflicts(&found),
            [(Conflict::Local, p(5), Some(p(6)))],
            "{found:?}"
        );

        let placed = [
            (1, Loc::S(-8, 8)),
            (2, Loc::R(0)),
            (5, Loc::S(-16, 8)),
            (6, Loc::S(-8, 8)),
        ];
        let found = check(&scopes(false), &placed, vec![], vec![], true);
        assert!(found.is_empty(), "{found:?}");
        let placed = [
            (1, Loc::R(0)),
            (2, Loc::S(-8, 8)),
            (5, Loc::S(-16, 8)),
            (6, Loc::S(-8, 8)),
        ];
        let found = check(&scopes(false), &placed, vec![], vec![], true);
        assert_eq!(
            conflicts(&found),
            [(Conflict::Local, p(6), Some(p(2)))],
            "{found:?}"
        );
    }

    #[test]
    fn a_slot_over_a_reserved_one_is_caught() {
        let types = types();
        let f = two_live(&types);
        let canary = ("the canary's slot", Cell::Frame { lo: -8, hi: 0 });
        let placed = [(1, Loc::R(1)), (2, Loc::S(-24, 24)), (3, Loc::R(0))];
        let found = check(&f, &placed, vec![], vec![canary], true);
        assert_eq!(
            conflicts(&found),
            [(Conflict::Reserved, p(2), None)],
            "{found:?}"
        );
    }

    /// `%1` is live across a `setjmp`, and read when it returns the second
    /// time -- after a `longjmp` from anywhere the setjmp reaches, which the
    /// liveness of the straight-line code cannot see. A def after the
    /// setjmp may not take its place, though `%1` looks dead there.
    #[test]
    fn a_value_across_setjmp_keeps_its_place_wherever_setjmp_reaches() {
        let types = types();
        let mut setjmp = Instruction::new(Opcode::Setjmp);
        setjmp.target = Some(p(2));
        let (b0, b1, b2) = (BasicBlockId(0), BasicBlockId(1), BasicBlockId(2));
        let mut f = one_block(
            &types,
            &[],
            vec![
                copy(&types, 1, ONE),
                setjmp.with_type_and_size(types.int_id, Bits::new(32)),
                Instruction::cbr(p(2), b1, b2),
            ],
        );
        f.blocks[0].children = vec![b1, b2];
        let mut resumed = BasicBlock::new(b1);
        resumed.add_insn(Instruction::ret(Some(p(1))));
        resumed.parents = vec![b0];
        let mut direct = BasicBlock::new(b2);
        direct.add_insn(copy(&types, 3, ONE));
        direct.add_insn(Instruction::ret(Some(p(3))));
        direct.parents = vec![b0];
        f.add_block(resumed);
        f.add_block(direct);

        let placed = [(1, Loc::S(-8, 8)), (2, Loc::R(0)), (3, Loc::S(-16, 8))];
        let found = check(&f, &placed, vec![], vec![], true);
        assert!(found.is_empty(), "{found:?}");
        let placed = [(1, Loc::S(-8, 8)), (2, Loc::R(0)), (3, Loc::S(-8, 8))];
        let found = check(&f, &placed, vec![], vec![], true);
        assert_eq!(
            conflicts(&found),
            [(Conflict::AcrossSetjmp, p(3), Some(p(1)))],
            "{found:?}"
        );
    }

    /// Two arguments read later arrive in two places.
    #[test]
    fn arguments_arrive_in_distinct_places() {
        let types = types();
        let f = with_args(
            &types,
            &[],
            &[1, 2],
            vec![add(&types, 3, p(1), p(2)), Instruction::ret(Some(p(3)))],
        );
        let found = check(
            &f,
            &[(1, Loc::R(0)), (2, Loc::R(0)), (3, Loc::R(0))],
            vec![],
            vec![],
            true,
        );
        assert_eq!(
            conflicts(&found),
            [(Conflict::Arguments, p(1), Some(p(2)))],
            "{found:?}"
        );
    }
}
