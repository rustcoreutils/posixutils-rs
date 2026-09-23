//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Deleting a store nothing can observe.
//
// Two reasons a store is dead, and they need different evidence:
//
// **A later store covers it.** Every byte it wrote is written again before
// anything reads any of them. This is a within-block question and is decided
// by a forward walk.
//
// **Nothing reads it before the function returns.** The object is a local of
// this function that never escaped, so when the frame goes the value goes
// with it. This is a whole-CFG question and is decided by a backward one.
//
// This pass deletes and never reorders, so it owes nothing to the ordering
// contract that `Instruction::is_memory_barrier` answers; what it owes is
// the *access* question -- which addresses -- and that is `memloc`.
//
// Three refusals carry the weight.
//
// **A read of any overlapping byte revives the store**, not just a read of
// exactly the same bytes. A bit-field update is a load of the storage unit,
// a mask, and a store back; killing the first store of that pair because the
// second covers it is correct, but killing a store whose bytes the next
// instruction *reads* is a miscompile, and the two are one byte apart.
//
// **A partial overwrite is not a kill.** A one-byte store inside a four-byte
// one leaves three bytes live. The entry is dropped rather than killed.
//
// **The dead-at-exit claim rests on three conditions, asserted at the single
// place that seeds it**: the base is a `Local` -- a block-scope `static` is a
// `Global` here, which is the trap -- it never escaped, and `EscapeInfo` did
// not give up, so no `longjmp` can resume inside this frame and read it.
//

use super::escape::EscapeInfo;
use super::memloc::{may_alias, AddrMap, MemBase, MemLoc, ModuleInfo};
use super::{BasicBlockId, Function, Instruction, Opcode, PseudoId};
use crate::types::{TypeModifiers, TypeTable};
use std::collections::{HashMap, HashSet};

/// How many sweeps the dead-at-exit fixed point may take.
const MAX_SWEEPS: usize = 64;

/// Delete stores nothing can observe. Returns whether anything changed.
pub(crate) fn run(func: &mut Function, types: &TypeTable, mi: &ModuleInfo) -> bool {
    if func.blocks.is_empty() {
        return false;
    }
    let esc = EscapeInfo::analyze(func);
    if esc.gave_up() {
        return false;
    }
    let am = AddrMap::build(func);
    let dead_at_end = dead_locals_at_block_end(func, types, &esc, &am, mi);

    let mut kills: Vec<(usize, usize)> = Vec::new();
    for b in 0..func.blocks.len() {
        scan_block(func, types, &esc, &am, mi, b, &dead_at_end, &mut kills);
    }

    for (b, i) in &kills {
        func.blocks[*b].insns[*i].kill();
    }
    !kills.is_empty()
}

/// One store still waiting to be read.
struct Pending {
    loc: MemLoc,
    index: usize,
}

/// Collect the dead stores in one block.
#[allow(clippy::too_many_arguments)]
fn scan_block(
    func: &Function,
    types: &TypeTable,
    esc: &EscapeInfo,
    am: &AddrMap,
    mi: &ModuleInfo,
    b: usize,
    dead_at_end: &HashMap<BasicBlockId, HashSet<PseudoId>>,
    kills: &mut Vec<(usize, usize)>,
) {
    let mut pending: Vec<Pending> = Vec::new();

    for (i, insn) in func.blocks[b].insns.iter().enumerate() {
        if insn.op == Opcode::Nop {
            continue;
        }

        // Anything that may read one of the pending locations makes it live
        // again. This runs before the store rule below, because a
        // read-modify-write reads the bytes it is about to write.
        pending.retain(|p| !may_read(func, esc, am, mi, insn, &p.loc));

        if insn.op != Opcode::Store {
            continue;
        }
        let loc = am.location_of(func, insn);
        if !deletable(func, types, mi, &loc) {
            // An untrackable store is still a write: drop whatever it may
            // have touched rather than pretending it did not happen.
            pending.retain(|p| !may_alias(&loc, &p.loc, mi));
            continue;
        }

        let mut kept: Vec<Pending> = Vec::new();
        for p in pending.drain(..) {
            if covers(&loc, &p.loc) {
                // Every byte written again before anything read any of it.
                kills.push((b, p.index));
            } else if !may_alias(&loc, &p.loc, mi) {
                kept.push(p);
            }
            // Otherwise the overlap is partial: some bytes are still live,
            // so the entry is dropped without being killed.
        }
        pending = kept;
        pending.push(Pending { loc, index: i });
    }

    // Whatever is still pending at the end of the block is dead if the whole
    // object is dead on every path out.
    let Some(dead) = dead_at_end.get(&func.blocks[b].id) else {
        return;
    };
    for p in pending {
        if let MemBase::Local(sym) = p.loc.base {
            if dead.contains(&sym) {
                kills.push((b, p.index));
            }
        }
    }
}

/// Could `insn` read any byte of `loc`?
fn may_read(
    func: &Function,
    esc: &EscapeInfo,
    am: &AddrMap,
    mi: &ModuleInfo,
    insn: &Instruction,
    loc: &MemLoc,
) -> bool {
    match insn.op {
        // Nothing here reaches memory.
        Opcode::Nop
        | Opcode::Entry
        | Opcode::Phi
        | Opcode::PhiSource
        | Opcode::Copy
        | Opcode::SetVal
        | Opcode::SymAddr
        | Opcode::Select
        | Opcode::Br
        | Opcode::Cbr
        | Opcode::Switch
        | Opcode::IndirectBr
        | Opcode::Unreachable => false,

        Opcode::Load => may_alias(&am.location_of(func, insn), loc, mi),

        // A store *writes*; the bytes it does not cover stay as they were,
        // so it reads nothing. The covering rule above is what uses it.
        Opcode::Store => false,

        // `memcpy` reads its source and writes its destination, and
        // `memmove` may do both to overlapping ranges. Neither extent is
        // `insn.size`, which is the pointer's width.
        Opcode::Memcpy | Opcode::Memmove => insn
            .src
            .iter()
            .take(2)
            .any(|a| may_alias(&am.resolve(func, *a, 0, 0, None), loc, mi)),
        // `memset` writes a constant; it reads nothing.
        Opcode::Memset => false,

        // A callee reads what it can reach. A local whose address never left
        // this function is not that, and a `pure` callee reads but a `const`
        // one does not -- except that reading is exactly what is being asked
        // about, so only the escape question helps here.
        Opcode::Call => esc.is_captured(&loc.base),

        // A `Ret` hands the object to the caller, which is a read by any
        // other name -- an aggregate return carries an address rather than a
        // value. But the operand of a scalar return is a *value*, and
        // resolving one as an address answers `Unknown`, which aliases
        // everything and made every `return 0;` revive every pending store.
        //
        // The escape question settles it instead: a local whose address
        // reaches a `Ret` has escaped by that very fact, so a local that did
        // not escape cannot be what is being returned.
        Opcode::Ret => esc.is_captured(&loc.base),

        _ if !insn.op.may_access_memory() => false,

        // `Asm`, `Alloca`, `StackSave`/`StackRestore`, the `Va*` family,
        // every atomic, and anything unlisted. A `"memory"` clobber can name
        // a frame slot without naming an operand.
        _ => true,
    }
}

/// May a store to this location be deleted at all?
fn deletable(func: &Function, types: &TypeTable, mi: &ModuleInfo, loc: &MemLoc) -> bool {
    if loc.offset.is_none() || loc.size == 0 {
        return false;
    }
    // Volatility and atomicity are properties of the object, so both ends
    // are checked -- a volatile store is observable and stays.
    if let Some(t) = loc.typ {
        let m = types.modifiers(t);
        if m.contains(TypeModifiers::VOLATILE) || m.contains(TypeModifiers::ATOMIC) {
            return false;
        }
    }
    match &loc.base {
        MemBase::Unknown => false,
        MemBase::Local(p) => match func.local_of(*p) {
            Some(l) => !l.is_volatile && !l.is_atomic,
            None => false,
        },
        MemBase::Global(n) => {
            let g = mi.global(n);
            !g.is_volatile && !g.is_thread_local
        }
    }
}

/// Does `later` write every byte `earlier` wrote?
fn covers(later: &MemLoc, earlier: &MemLoc) -> bool {
    if later.base != earlier.base || later.base == MemBase::Unknown {
        return false;
    }
    let (Some(lo), Some(eo)) = (later.offset, earlier.offset) else {
        return false;
    };
    if later.size == 0 || earlier.size == 0 {
        return false;
    }
    let (lb, eb) = (
        later.size.div_ceil(8) as i64,
        earlier.size.div_ceil(8) as i64,
    );
    let (Some(lend), Some(eend)) = (lo.checked_add(lb), eo.checked_add(eb)) else {
        return false;
    };
    lo <= eo && eend <= lend
}

/// Which locals are never read again, at the end of each block.
///
/// A backward "not read on any path from here" analysis, keyed by the
/// object rather than by byte range: an entry survives only while *no* byte
/// of the object is read, which is coarse and safe.
///
/// **This is the single place the dead-at-exit claim is made**, and the
/// candidate set is where its three conditions live: the base is a `Local`
/// of this function, it never escaped, and `EscapeInfo` did not give up. A
/// block-scope `static` is a `Global` here and so is never a candidate,
/// which is the trap this guards.
fn dead_locals_at_block_end(
    func: &Function,
    types: &TypeTable,
    esc: &EscapeInfo,
    am: &AddrMap,
    mi: &ModuleInfo,
) -> HashMap<BasicBlockId, HashSet<PseudoId>> {
    let mut candidates: HashSet<PseudoId> = HashSet::new();
    for p in &func.pseudos {
        let Some(local) = func.local_of(p.id) else {
            continue;
        };
        if local.is_volatile || local.is_atomic {
            continue;
        }
        if esc.is_captured(&MemBase::Local(p.id)) {
            continue;
        }
        let m = types.modifiers(local.typ);
        if m.contains(TypeModifiers::VOLATILE) || m.contains(TypeModifiers::ATOMIC) {
            continue;
        }
        candidates.insert(p.id);
    }

    let mut out: HashMap<BasicBlockId, HashSet<PseudoId>> = HashMap::new();
    if candidates.is_empty() {
        for bb in &func.blocks {
            out.insert(bb.id, HashSet::new());
        }
        return out;
    }

    // What each block reads, computed once.
    let mut reads: HashMap<BasicBlockId, HashSet<PseudoId>> = HashMap::new();
    for bb in &func.blocks {
        let mut r = HashSet::new();
        for insn in &bb.insns {
            for &c in &candidates {
                if r.contains(&c) {
                    continue;
                }
                let whole = MemLoc {
                    base: MemBase::Local(c),
                    offset: Some(0),
                    size: 0,
                    typ: None,
                };
                if may_read(func, esc, am, mi, insn, &whole) {
                    r.insert(c);
                }
            }
        }
        reads.insert(bb.id, r);
    }

    // Successors, as `loadfwd` derives them: from `children`, which `dce`
    // maintains, never from `parents`, which it does not.
    let mut succs: HashMap<BasicBlockId, Vec<BasicBlockId>> = HashMap::new();
    for bb in &func.blocks {
        let mut s = bb.children.clone();
        for insn in &bb.insns {
            if let Some(ref asm) = insn.asm_data {
                for (t, _) in &asm.goto_labels {
                    if !s.contains(t) {
                        s.push(*t);
                    }
                }
            }
        }
        succs.insert(bb.id, s);
    }

    // Start everything dead and remove: the greatest fixed point is the
    // right one here, so a loop that never reads a local keeps it dead.
    let mut dead_in: HashMap<BasicBlockId, HashSet<PseudoId>> = HashMap::new();
    for bb in &func.blocks {
        dead_in.insert(bb.id, candidates.clone());
    }

    for _ in 0..MAX_SWEEPS {
        let mut moved = false;
        for bb in func.blocks.iter().rev() {
            let succ = succs.get(&bb.id).map(Vec::as_slice).unwrap_or(&[]);
            // No successors is an exit: the frame is gone, so everything a
            // non-escaping local held is unobservable.
            let mut dead_out = candidates.clone();
            for s in succ {
                if let Some(d) = dead_in.get(s) {
                    dead_out.retain(|c| d.contains(c));
                }
            }
            let mut d_in = dead_out.clone();
            if let Some(r) = reads.get(&bb.id) {
                d_in.retain(|c| !r.contains(c));
            }
            out.insert(bb.id, dead_out);
            if dead_in.get(&bb.id) != Some(&d_in) {
                dead_in.insert(bb.id, d_in);
                moved = true;
            }
        }
        if !moved {
            break;
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, Module, Pseudo};
    use crate::target::Target;

    fn host_types() -> TypeTable {
        TypeTable::new(&Target::host())
    }

    struct Build {
        f: Function,
        types: TypeTable,
    }

    impl Build {
        fn new() -> Build {
            let types = host_types();
            let i32t = types.int_id;
            let mut f = Function::new("f", types.void_id);
            f.add_pseudo(Pseudo::sym(PseudoId(0), "a.0".into()));
            f.add_pseudo(Pseudo::sym(PseudoId(1), "b.0".into()));
            f.add_pseudo(Pseudo::val(PseudoId(5), 7));
            f.add_pseudo(Pseudo::val(PseudoId(6), 9));
            f.add_local("a.0", PseudoId(0), i32t, false, false, None, None);
            f.add_local("b.0", PseudoId(1), i32t, false, false, None, None);
            f.next_pseudo = 60;
            Build { f, types }
        }

        fn block(&mut self, id: u32, insns: Vec<Instruction>, children: Vec<u32>) -> &mut Build {
            let mut bb = BasicBlock::new(BasicBlockId(id));
            for i in insns {
                bb.add_insn(i);
            }
            bb.children = children.into_iter().map(BasicBlockId).collect();
            self.f.blocks.push(bb);
            self
        }

        fn run(&mut self) -> bool {
            self.f.entry = self.f.blocks[0].id;
            self.f.rebuild_block_idx();
            let mi = ModuleInfo::build(&Module::default(), &self.types);
            super::run(&mut self.f, &self.types, &mi)
        }

        fn op(&self, b: usize, i: usize) -> Opcode {
            self.f.blocks[b].insns[i].op
        }
    }

    fn entry() -> Instruction {
        Instruction::new(Opcode::Entry)
    }

    fn br(to: u32) -> Instruction {
        let mut i = Instruction::new(Opcode::Br);
        i.bb_true = Some(BasicBlockId(to));
        i
    }

    fn cbr(cond: PseudoId, t: u32, e: u32) -> Instruction {
        let mut i = Instruction::new(Opcode::Cbr).with_src(cond);
        i.bb_true = Some(BasicBlockId(t));
        i.bb_false = Some(BasicBlockId(e));
        i
    }

    /// A store written again before anything reads it is dead.
    #[test]
    fn dse_a_covered_store_is_dead() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.block(
            0,
            vec![
                entry(),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, 32),
                Instruction::store(PseudoId(6), PseudoId(0), 0, i32t, 32),
                Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, 32),
                Instruction::new(Opcode::Ret).with_src(PseudoId(20)),
            ],
            vec![],
        );
        assert!(b.run());
        assert_eq!(b.op(0, 1), Opcode::Nop, "the first store is overwritten");
        assert_eq!(b.op(0, 2), Opcode::Store);
    }

    /// **The read-modify-write shape.** A read of the bytes between the two
    /// stores revives the first one, and killing it is a miscompile.
    #[test]
    fn dse_a_read_between_two_stores_revives_the_first() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.block(
            0,
            vec![
                entry(),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, 32),
                Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, 32),
                Instruction::store(PseudoId(6), PseudoId(0), 0, i32t, 32),
                Instruction::load(PseudoId(21), PseudoId(0), 0, i32t, 32),
                Instruction::new(Opcode::Ret).with_src(PseudoId(21)),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 1), Opcode::Store);
    }

    /// A read of an *overlapping* byte, not the same bytes, is still a read.
    #[test]
    fn dse_an_overlapping_read_revives_the_store() {
        let mut b = Build::new();
        let (i32t, i8t) = (b.types.int_id, b.types.char_id);
        b.block(
            0,
            vec![
                entry(),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, 32),
                // one byte inside the four just written
                Instruction::load(PseudoId(20), PseudoId(0), 1, i8t, 8),
                Instruction::store(PseudoId(6), PseudoId(0), 0, i32t, 32),
                Instruction::load(PseudoId(21), PseudoId(0), 0, i32t, 32),
                Instruction::new(Opcode::Ret).with_src(PseudoId(21)),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 1), Opcode::Store);
    }

    /// A partial overwrite leaves bytes live, so it is not a kill.
    #[test]
    fn dse_a_partial_overwrite_is_not_a_kill() {
        let mut b = Build::new();
        let (i32t, i8t) = (b.types.int_id, b.types.char_id);
        b.block(
            0,
            vec![
                entry(),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, 32),
                Instruction::store(PseudoId(6), PseudoId(0), 1, i8t, 8),
                Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, 32),
                Instruction::new(Opcode::Ret).with_src(PseudoId(20)),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 1), Opcode::Store);
    }

    /// A call cannot read a local whose address never left the function, so
    /// tracking survives across it -- which is the case worth having.
    #[test]
    fn dse_a_call_does_not_revive_a_non_escaping_local() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        let void = b.types.void_id;
        b.block(
            0,
            vec![
                entry(),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, 32),
                Instruction::call(None, "opaque", vec![], vec![], void, 0),
                Instruction::store(PseudoId(6), PseudoId(0), 0, i32t, 32),
                Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, 32),
                Instruction::new(Opcode::Ret).with_src(PseudoId(20)),
            ],
            vec![],
        );
        assert!(b.run());
        assert_eq!(b.op(0, 1), Opcode::Nop);
    }

    /// A global is visible to any callee, so a call does revive one.
    #[test]
    fn dse_a_call_revives_a_global() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        let void = b.types.void_id;
        b.f.add_pseudo(Pseudo::sym(PseudoId(2), "g".into()));
        b.block(
            0,
            vec![
                entry(),
                Instruction::store(PseudoId(5), PseudoId(2), 0, i32t, 32),
                Instruction::call(None, "opaque", vec![], vec![], void, 0),
                Instruction::store(PseudoId(6), PseudoId(2), 0, i32t, 32),
                Instruction::new(Opcode::Ret),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 1), Opcode::Store);
    }

    /// A store to a non-escaping local that nothing reads before the frame
    /// goes is dead, whatever block it is in.
    #[test]
    fn dse_a_store_dead_at_exit_is_deleted() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.block(
            0,
            vec![
                entry(),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, 32),
                br(1),
            ],
            vec![1],
        );
        b.block(1, vec![Instruction::new(Opcode::Ret)], vec![]);
        assert!(b.run());
        assert_eq!(b.op(0, 1), Opcode::Nop);
    }

    /// ...but not when a later block reads it.
    #[test]
    fn dse_a_store_read_in_a_later_block_survives() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.block(
            0,
            vec![
                entry(),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, 32),
                br(1),
            ],
            vec![1],
        );
        b.block(
            1,
            vec![
                Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, 32),
                Instruction::new(Opcode::Ret).with_src(PseudoId(20)),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 1), Opcode::Store);
    }

    /// A read on *one* arm of a diamond is enough to keep the store.
    #[test]
    fn dse_a_read_on_one_arm_keeps_the_store() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.f.add_pseudo(Pseudo::val(PseudoId(7), 1));
        b.block(
            0,
            vec![
                entry(),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, 32),
                cbr(PseudoId(7), 1, 2),
            ],
            vec![1, 2],
        );
        b.block(
            1,
            vec![
                Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, 32),
                br(3),
            ],
            vec![3],
        );
        b.block(2, vec![br(3)], vec![3]);
        b.block(3, vec![Instruction::new(Opcode::Ret)], vec![]);
        assert!(!b.run());
        assert_eq!(b.op(0, 1), Opcode::Store);
    }

    /// A read reached only through a back edge still counts.
    #[test]
    fn dse_a_read_across_a_back_edge_keeps_the_store() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.f.add_pseudo(Pseudo::val(PseudoId(7), 1));
        b.block(0, vec![entry(), br(1)], vec![1]);
        b.block(
            1,
            vec![
                Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, 32),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, 32),
                cbr(PseudoId(7), 1, 2),
            ],
            vec![1, 2],
        );
        b.block(2, vec![Instruction::new(Opcode::Ret)], vec![]);
        assert!(
            !b.run(),
            "the load at the top of the latch reads what the store wrote"
        );
        assert_eq!(b.op(1, 1), Opcode::Store);
    }

    /// A `volatile` local is written as many times as the program says.
    #[test]
    fn dse_a_volatile_local_is_never_deleted() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.f.add_local("a.0", PseudoId(0), i32t, true, false, None, None);
        b.block(
            0,
            vec![
                entry(),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, 32),
                Instruction::store(PseudoId(6), PseudoId(0), 0, i32t, 32),
                Instruction::new(Opcode::Ret),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 1), Opcode::Store);
    }

    /// A store to a local whose address left the function is observable
    /// after the frame goes.
    #[test]
    fn dse_an_escaped_local_is_not_dead_at_exit() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        let void = b.types.void_id;
        b.block(
            0,
            vec![
                entry(),
                Instruction::sym_addr(PseudoId(10), PseudoId(0), b.types.long_id),
                Instruction::call(
                    None,
                    "keep",
                    vec![PseudoId(10)],
                    vec![b.types.long_id],
                    void,
                    0,
                ),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, 32),
                Instruction::new(Opcode::Ret),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 3), Opcode::Store);
    }

    /// A global outlives the frame, so "dead at exit" never applies to one.
    #[test]
    fn dse_a_global_is_not_dead_at_exit() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.f.add_pseudo(Pseudo::sym(PseudoId(2), "g".into()));
        b.block(
            0,
            vec![
                entry(),
                Instruction::store(PseudoId(5), PseudoId(2), 0, i32t, 32),
                Instruction::new(Opcode::Ret),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 1), Opcode::Store);
    }

    /// `setjmp` can resume inside this frame, so no local is dead at exit
    /// and the pass declines the function.
    #[test]
    fn dse_declines_a_function_with_setjmp() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.block(
            0,
            vec![
                entry(),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, 32),
                Instruction::new(Opcode::Setjmp),
                Instruction::new(Opcode::Ret),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 1), Opcode::Store);
    }

    /// Two locals are two objects: a store to one neither covers nor
    /// revives a store to the other.
    #[test]
    fn dse_distinct_locals_do_not_interfere() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.block(
            0,
            vec![
                entry(),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, 32),
                Instruction::store(PseudoId(6), PseudoId(1), 0, i32t, 32),
                Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, 32),
                Instruction::load(PseudoId(21), PseudoId(1), 0, i32t, 32),
                Instruction::new(Opcode::Ret).with_src(PseudoId(20)),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 1), Opcode::Store);
        assert_eq!(b.op(0, 2), Opcode::Store);
    }

    /// `covers` is the byte-range question, and both ends of it matter.
    #[test]
    fn dse_covers_is_containment() {
        let at = |off: i64, bits: u32| MemLoc {
            base: MemBase::Local(PseudoId(0)),
            offset: Some(off),
            size: bits,
            typ: None,
        };
        assert!(covers(&at(0, 32), &at(0, 32)));
        assert!(covers(&at(0, 32), &at(1, 8)));
        assert!(!covers(&at(1, 8), &at(0, 32)));
        assert!(!covers(&at(0, 32), &at(4, 8)));
        assert!(
            !covers(&at(0, 0), &at(0, 8)),
            "an unknown extent covers nothing"
        );

        let elsewhere = MemLoc {
            base: MemBase::Local(PseudoId(1)),
            offset: Some(0),
            size: 32,
            typ: None,
        };
        assert!(!covers(&at(0, 32), &elsewhere));
        assert!(!covers(&MemLoc::unknown(), &MemLoc::unknown()));
    }
}
