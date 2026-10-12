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

use super::bitset::BitSet;
use super::escape::EscapeInfo;
use super::memloc::{is_ordinary_object, may_alias, Access, AddrMap, MemBase, MemLoc, ModuleInfo};
use super::{BasicBlockId, Function, Opcode, PseudoId};
use crate::types::TypeTable;
use crate::units::Bits;
use std::collections::HashMap;

/// How many sweeps the dead-at-exit fixed point may take before it gives up.
///
/// Exhausting this is *not* a licence to use what it has: the analysis
/// starts with everything dead and only removes, so an unfinished run is an
/// over-approximation of "dead", and acting on one deletes stores that are
/// read. Running out means answering "nothing is dead at exit", which is
/// what `dead_locals_at_block_end` does.
const MAX_SWEEPS: usize = 64;

/// Delete stores nothing can observe. Returns whether anything changed.
pub(crate) fn run(func: &mut Function, types: &TypeTable, mi: &ModuleInfo) -> bool {
    if func.blocks.is_empty() {
        return false;
    }
    let esc = EscapeInfo::analyze(func, types);
    if esc.gave_up() {
        return false;
    }
    let am = AddrMap::build(func);
    let ctx = DseCtx {
        func,
        types,
        esc: &esc,
        am: &am,
        mi,
    };
    let dead_at_end = ctx.dead_locals_at_block_end();

    let mut kills: Vec<(usize, usize)> = Vec::new();
    for b in 0..func.blocks.len() {
        ctx.scan_block(b, &dead_at_end, &mut kills);
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

/// What the pass knows about the function it is scanning.
#[derive(Clone, Copy)]
struct DseCtx<'a> {
    func: &'a Function,
    types: &'a TypeTable,
    esc: &'a EscapeInfo,
    am: &'a AddrMap,
    mi: &'a ModuleInfo,
}

impl DseCtx<'_> {
    /// Collect the dead stores in one block.
    fn scan_block(&self, b: usize, dead_at_end: &DeadAtEnd, kills: &mut Vec<(usize, usize)>) {
        let DseCtx {
            func,
            types,
            am,
            mi,
            ..
        } = *self;
        let mut pending: Vec<Pending> = Vec::new();

        for (i, insn) in func.blocks[b].insns.iter().enumerate() {
            if insn.op == Opcode::Nop {
                continue;
            }

            // Anything that may read one of the pending locations makes it live
            // again. This runs before the store rule below, because a
            // read-modify-write reads the bytes it is about to write.
            if !pending.is_empty() {
                let access = mi.access(am, func, insn);
                pending.retain(|p| !self.may_read(&access, &p.loc));
            }

            if insn.op != Opcode::Store {
                continue;
            }
            let loc = am.location_of(func, insn);
            // A volatile store is observable and stays, even when a later store
            // overwrites every byte of it. `is_ordinary_object` answers for a
            // named object; the marker also answers for `*p` where `p` is a
            // `volatile int *`.
            if insn.is_volatile_access() || !is_ordinary_object(func, types, mi, &loc) {
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
        for p in pending {
            if let MemBase::Local(sym) = p.loc.base {
                if dead_at_end.contains(b, sym) {
                    kills.push((b, p.index));
                }
            }
        }
    }

    /// Could an instruction that touches `access` read any byte of `loc`?
    ///
    /// What `AddrMap::access` says it reads, against `loc`. A `Ret` reads
    /// nothing there, which is right for both kinds of object this pass
    /// tracks: a local that did not escape cannot be what is returned -- an
    /// address reaching a `Ret` is an escape -- and a store to anything else
    /// is never dead at exit, so no answer about a `Ret`, the last
    /// instruction of its block, could kill one.
    fn may_read(&self, access: &Access, loc: &MemLoc) -> bool {
        access.may_read(loc, self.mi, self.esc)
    }
}

/// Does `later` write every byte `earlier` wrote?
fn covers(later: &MemLoc, earlier: &MemLoc) -> bool {
    if later.base != earlier.base {
        return false;
    }
    match (later.byte_extent(), earlier.byte_extent()) {
        (Some((lo, lend)), Some((eo, eend))) => lo <= eo && eend <= lend,
        _ => false,
    }
}

/// Which locals are never read again at the end of each block, as
/// [`DseCtx::dead_locals_at_block_end`] found them.
struct DeadAtEnd {
    /// The candidates, numbered for the bit sets.
    index: HashMap<PseudoId, usize>,
    /// By block position.
    by_block: Vec<BitSet>,
}

impl DeadAtEnd {
    /// Is `sym` dead at the end of the block at position `b`?
    fn contains(&self, b: usize, sym: PseudoId) -> bool {
        self.index
            .get(&sym)
            .is_some_and(|&i| self.by_block[b].contains(i))
    }
}

impl DseCtx<'_> {
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
    ///
    /// The sets are bits over the candidates, and an access pinned to one
    /// object is asked about that object alone: a hash set per block, and
    /// every read asked about every candidate, made this blocks x locals per
    /// sweep, which a function with hundreds of calls spliced in pays in
    /// seconds.
    fn dead_locals_at_block_end(&self) -> DeadAtEnd {
        let DseCtx {
            func,
            types,
            esc,
            am,
            mi,
        } = *self;
        let mut candidates: Vec<PseudoId> = Vec::new();
        for p in &func.pseudos {
            let Some(local) = func.local_of(p.id) else {
                continue;
            };
            if !local.is_ordinary(types) || esc.is_captured(&MemBase::Local(p.id)) {
                continue;
            }
            candidates.push(p.id);
        }
        let index: HashMap<PseudoId, usize> = candidates
            .iter()
            .enumerate()
            .map(|(i, &c)| (c, i))
            .collect();
        let n = candidates.len();
        let nothing = |index| DeadAtEnd {
            index,
            by_block: vec![BitSet::new(n); func.blocks.len()],
        };
        if n == 0 {
            return nothing(index);
        }

        // What each block reads, computed once, and each instruction's
        // footprint once rather than once per candidate.
        let wholes: Vec<MemLoc> = candidates
            .iter()
            .map(|&c| MemLoc {
                base: MemBase::Local(c),
                offset: Some(0),
                size: Bits::new(0),
                typ: None,
            })
            .collect();
        let mut reads: Vec<BitSet> = Vec::with_capacity(func.blocks.len());
        for bb in &func.blocks {
            let mut r = BitSet::new(n);
            for insn in &bb.insns {
                let access = mi.access(am, func, insn);
                for f in &access.reads {
                    let mut ask = |i: usize| {
                        if !r.contains(i) && f.may_touch(&wholes[i], mi, esc) {
                            r.insert(i);
                        }
                    };
                    match f.pinned_base() {
                        Some(MemBase::Local(p)) => {
                            if let Some(&i) = index.get(p) {
                                ask(i);
                            }
                        }
                        Some(_) => {}
                        None => (0..n).for_each(ask),
                    }
                }
            }
            reads.push(r);
        }

        // Start everything dead and remove: the greatest fixed point is the
        // right one here, so a loop that never reads a local keeps it dead.
        let position: HashMap<BasicBlockId, usize> = func
            .blocks
            .iter()
            .enumerate()
            .map(|(i, bb)| (bb.id, i))
            .collect();
        let all = BitSet::full(n);
        let mut dead_in = vec![all.clone(); func.blocks.len()];
        let mut out = vec![BitSet::new(n); func.blocks.len()];

        for _ in 0..MAX_SWEEPS {
            let mut moved = false;
            for (b, bb) in func.blocks.iter().enumerate().rev() {
                // No successors is an exit: the frame is gone, so everything a
                // non-escaping local held is unobservable.
                let mut dead_out = all.clone();
                for s in &bb.children {
                    if let Some(&s) = position.get(s) {
                        dead_out.intersect_with(&dead_in[s]);
                    }
                }
                let mut d_in = dead_out.clone();
                d_in.subtract(&reads[b]);
                out[b] = dead_out;
                if dead_in[b] != d_in {
                    dead_in[b] = d_in;
                    moved = true;
                }
            }
            if !moved {
                return DeadAtEnd {
                    index,
                    by_block: out,
                };
            }
        }
        // An unfinished greatest fixed point still says "dead" for things
        // that are read. Answer nothing rather than something wrong.
        nothing(index)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, FenceScope, Instruction, MemoryOrder, Module, Pseudo};
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
            f.add_local("a.0", PseudoId(0), i32t, None, None);
            f.add_local("b.0", PseudoId(1), i32t, None, None);
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
            self.f.rebuild_parents();
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
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, Bits::new(32)),
                Instruction::store(PseudoId(6), PseudoId(0), 0, i32t, Bits::new(32)),
                Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, Bits::new(32)),
                Instruction::new(Opcode::Ret).with_src(PseudoId(20)),
            ],
            vec![],
        );
        assert!(b.run());
        assert_eq!(b.op(0, 1), Opcode::Nop, "the first store is overwritten");
        assert_eq!(b.op(0, 2), Opcode::Store);
    }

    /// A signal fence between two stores revives the first: a handler that
    /// runs at the fence may read it, though the fence emits nothing.
    #[test]
    fn dse_a_signal_fence_between_two_stores_revives_the_first() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        let mut fence = Instruction::new(Opcode::Fence).with_memory_order(MemoryOrder::SeqCst);
        fence.extra_mut().fence_scope = FenceScope::Signal;
        b.block(
            0,
            vec![
                entry(),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, Bits::new(32)),
                fence,
                Instruction::store(PseudoId(6), PseudoId(0), 0, i32t, Bits::new(32)),
                Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, Bits::new(32)),
                Instruction::new(Opcode::Ret).with_src(PseudoId(20)),
            ],
            vec![],
        );
        b.run();
        assert_eq!(
            b.op(0, 1),
            Opcode::Store,
            "the fence orders the first store"
        );
        assert_eq!(b.op(0, 2), Opcode::Fence);
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
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, Bits::new(32)),
                Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, Bits::new(32)),
                Instruction::store(PseudoId(6), PseudoId(0), 0, i32t, Bits::new(32)),
                Instruction::load(PseudoId(21), PseudoId(0), 0, i32t, Bits::new(32)),
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
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, Bits::new(32)),
                // one byte inside the four just written
                Instruction::load(PseudoId(20), PseudoId(0), 1, i8t, Bits::new(8)),
                Instruction::store(PseudoId(6), PseudoId(0), 0, i32t, Bits::new(32)),
                Instruction::load(PseudoId(21), PseudoId(0), 0, i32t, Bits::new(32)),
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
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, Bits::new(32)),
                Instruction::store(PseudoId(6), PseudoId(0), 1, i8t, Bits::new(8)),
                Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, Bits::new(32)),
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
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, Bits::new(32)),
                Instruction::call(None, "opaque", vec![], vec![], void, Bits::new(0)),
                Instruction::store(PseudoId(6), PseudoId(0), 0, i32t, Bits::new(32)),
                Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, Bits::new(32)),
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
                Instruction::store(PseudoId(5), PseudoId(2), 0, i32t, Bits::new(32)),
                Instruction::call(None, "opaque", vec![], vec![], void, Bits::new(0)),
                Instruction::store(PseudoId(6), PseudoId(2), 0, i32t, Bits::new(32)),
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
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, Bits::new(32)),
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
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, Bits::new(32)),
                br(1),
            ],
            vec![1],
        );
        b.block(
            1,
            vec![
                Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, Bits::new(32)),
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
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, Bits::new(32)),
                cbr(PseudoId(7), 1, 2),
            ],
            vec![1, 2],
        );
        b.block(
            1,
            vec![
                Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, Bits::new(32)),
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
                Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, Bits::new(32)),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, Bits::new(32)),
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
        let vol_i32 = b.types.intern(crate::types::Type::with_modifiers(
            crate::types::TypeKind::Int,
            crate::types::TypeModifiers::VOLATILE,
        ));
        b.f.add_local("a.0", PseudoId(0), vol_i32, None, None);
        b.block(
            0,
            vec![
                entry(),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, Bits::new(32)),
                Instruction::store(PseudoId(6), PseudoId(0), 0, i32t, Bits::new(32)),
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
                    Bits::new(0),
                ),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, Bits::new(32)),
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
                Instruction::store(PseudoId(5), PseudoId(2), 0, i32t, Bits::new(32)),
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
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, Bits::new(32)),
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
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, Bits::new(32)),
                Instruction::store(PseudoId(6), PseudoId(1), 0, i32t, Bits::new(32)),
                Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, Bits::new(32)),
                Instruction::load(PseudoId(21), PseudoId(1), 0, i32t, Bits::new(32)),
                Instruction::new(Opcode::Ret).with_src(PseudoId(20)),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 1), Opcode::Store);
        assert_eq!(b.op(0, 2), Opcode::Store);
    }

    /// A block op from `src` to `dest`, `%5` bytes long.
    fn block_op(op: Opcode, dest: PseudoId, src: PseudoId, types: &TypeTable) -> Instruction {
        Instruction::new(op)
            .with_src3(dest, src, PseudoId(5))
            .with_type_and_size(types.void_ptr_id, Bits::new(64))
    }

    /// A local whose address went only to a block op has not escaped, so a
    /// store to it that nothing reads before the frame goes is dead -- a
    /// `memset` reads nothing.
    #[test]
    fn dse_a_local_only_a_block_op_wrote_is_dead_at_exit() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        let set = block_op(Opcode::Memset, PseudoId(0), PseudoId(6), &b.types);
        b.block(
            0,
            vec![
                entry(),
                Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, Bits::new(32)),
                set,
                Instruction::new(Opcode::Ret),
            ],
            vec![],
        );
        assert!(b.run());
        assert_eq!(b.op(0, 1), Opcode::Nop);
    }

    /// A block op that reads the local -- as `memcpy`'s source, or its
    /// destination, which counts as read too -- revives a store to it, and
    /// so does one whose pointer nothing is known about.
    #[test]
    fn dse_a_block_op_that_may_read_revives_the_store() {
        for (op, dest, src) in [
            (Opcode::Memcpy, PseudoId(1), PseudoId(0)),
            (Opcode::Memmove, PseudoId(1), PseudoId(0)),
            (Opcode::Memcpy, PseudoId(0), PseudoId(1)),
            (Opcode::Memcpy, PseudoId(1), PseudoId(30)),
        ] {
            let mut b = Build::new();
            let i32t = b.types.int_id;
            b.f.add_pseudo(Pseudo::arg(PseudoId(30), 0));
            let copy = block_op(op, dest, src, &b.types);
            b.block(
                0,
                vec![
                    entry(),
                    Instruction::store(PseudoId(5), PseudoId(0), 0, i32t, Bits::new(32)),
                    copy,
                    Instruction::store(PseudoId(6), PseudoId(0), 0, i32t, Bits::new(32)),
                    Instruction::load(PseudoId(20), PseudoId(0), 0, i32t, Bits::new(32)),
                    Instruction::new(Opcode::Ret).with_src(PseudoId(20)),
                ],
                vec![],
            );
            b.run();
            assert_eq!(b.op(0, 1), Opcode::Store, "{op:?} {dest:?} <- {src:?}");
        }
    }

    /// `covers` is the byte-range question, and both ends of it matter.
    #[test]
    fn dse_covers_is_containment() {
        let at = |off: i64, bits: u32| MemLoc {
            base: MemBase::Local(PseudoId(0)),
            offset: Some(off),
            size: Bits::new(bits),
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
            size: Bits::new(32),
            typ: None,
        };
        assert!(!covers(&at(0, 32), &elsewhere));
        assert!(!covers(&MemLoc::unknown(), &MemLoc::unknown()));
    }
}
