//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Replacing a load with a value that is already available: from an earlier
// store to the same location (store-to-load forwarding), or from an earlier
// load of it (redundant load elimination).
//
// One algorithm, because the question is one question -- *what is known to
// be in this location here?* -- and only the instruction that answers it
// differs.
//
// This is the first pass in the compiler to remove a memory operation, so
// what it declines matters more than what it does. Three rules carry that
// weight.
//
// **The dominator chain is not every path.** A definition that dominates the
// load is guaranteed to have run, but a side path that rejoins in between
// can have written the location since. `if (c) a[0] = 1; x = a[0];` forwards
// the wrong value unless every block that can be traversed between the two
// is scanned. That region is the *intersection* of what the definition's
// block reaches going forwards and what the load's block reaches going
// backwards, and it has to be both: a backward walk alone climbs past the
// definition -- fatally so when the two share a block, where it enumerates
// the whole function above them -- and a forward walk alone runs off down
// every path that never arrives.
//
// **The load's own block can be in that region.** When a back edge reaches
// it, a clobber *after* the load runs *before* the next execution of it:
//
// ```c
// a[0] = 0;
// loop: x = a[0];      // the store dominates this
//       a[0] = x + 1;  // ...and this runs before the next iteration
//       goto loop;
// ```
//
// Scanning only `[0, load)` of that block misses the store entirely and the
// loop reads 0 forever. When the backward walk reaches the load's own block,
// all of it is scanned.
//
// **Volatility is not on the instruction.** A `Load` says nothing about
// whether it is volatile; that lives on the `LocalVar` or on the
// `GlobalDef`, so both the base and the access type are checked, and a name
// this translation unit does not define is assumed to be everything.
//

use super::constfold::unambiguous_at;
use super::dominate::{domtree_build, DomTree};
use super::escape::EscapeInfo;
use super::memloc::{is_same_access, may_alias, AddrMap, MemBase, MemLoc, ModuleInfo};
use super::{BasicBlockId, Function, Instruction, Opcode, PseudoId};
use crate::types::{TypeId, TypeModifiers, TypeTable};
use std::collections::{HashMap, HashSet};

/// How far up the dominator chain one load will look.
const MAX_DOM_LEVELS: usize = 64;

/// How many instructions one load will scan before giving up, so a large
/// function cannot make this quadratic.
const MAX_SCAN_INSNS: usize = 4096;

/// How many blocks one reachability walk will visit.
const MAX_REGION_BLOCKS: usize = 4096;

/// Replace loads whose value is already available. Returns whether anything
/// changed.
pub(crate) fn run(func: &mut Function, types: &TypeTable, mi: &ModuleInfo) -> bool {
    if func.blocks.is_empty() {
        return false;
    }
    let esc = EscapeInfo::analyze(func);
    if esc.gave_up() {
        return false;
    }
    let am = AddrMap::build(func);
    let dom = domtree_build(func);
    let preds = build_preds(func);
    let succs = invert(&preds);

    // Collected under `&Function` -- resolving an address needs the pseudo
    // table -- then applied under `&mut`, as `constglobal` does.
    let mut sites: Vec<(usize, usize, Available)> = Vec::new();
    for (b, bb) in func.blocks.iter().enumerate() {
        for (i, insn) in bb.insns.iter().enumerate() {
            if insn.op != Opcode::Load {
                continue;
            }
            if let Some(a) =
                available_value(func, types, mi, &esc, &am, &dom, &preds, &succs, (b, i))
            {
                sites.push((b, i, a));
            }
        }
    }

    let mut changed = false;
    for (b, i, avail) in sites {
        let insn = &func.blocks[b].insns[i];
        let (target, typ, size, pos) = (insn.target, insn.typ, insn.size, insn.pos);
        let Some(target) = target else { continue };
        let (op, src_size, src_typ) = match avail.narrow {
            Some(n) => (Opcode::Trunc, n.from, n.typ),
            None => (Opcode::Copy, 0, None),
        };
        func.blocks[b].insns[i] = Instruction {
            op,
            target: Some(target),
            src: vec![avail.value],
            typ,
            size,
            pos,
            src_size,
            src_typ,
            ..Default::default()
        };
        changed = true;
    }
    changed
}

/// The pseudo already holding what the load at `site` would read.
#[allow(clippy::too_many_arguments)]
fn available_value(
    func: &Function,
    types: &TypeTable,
    mi: &ModuleInfo,
    esc: &EscapeInfo,
    am: &AddrMap,
    dom: &DomTree,
    preds: &HashMap<BasicBlockId, Vec<BasicBlockId>>,
    succs: &HashMap<BasicBlockId, Vec<BasicBlockId>>,
    (bl, il): (usize, usize),
) -> Option<Available> {
    let load = &func.blocks[bl].insns[il];
    let loc = am.location_of(func, load);
    if !forwardable(func, types, mi, &loc) {
        return None;
    }

    // Walk up the dominator chain for the nearest definition of this
    // location, stopping at the first thing that may write it.
    let mut block = func.blocks[bl].id;
    let mut start = il;
    let mut scanned = 0usize;
    for _ in 0..MAX_DOM_LEVELS {
        let bi = func.block_index(block)?;
        for i in (0..start).rev() {
            scanned += 1;
            if scanned > MAX_SCAN_INSNS {
                return None;
            }
            let insn = &func.blocks[bi].insns[i];
            if insn.op == Opcode::Nop {
                continue;
            }
            match candidate(func, types, mi, esc, am, insn, &loc) {
                Some(Candidate::Value(avail)) => {
                    return no_clobber_between(
                        func,
                        types,
                        mi,
                        esc,
                        am,
                        preds,
                        succs,
                        (bi, i),
                        (bl, il),
                        &loc,
                    )
                    .then_some(avail);
                }
                Some(Candidate::Clobber) => return None,
                None => {}
            }
        }
        let idom = dom.idom(block)?;
        block = idom;
        start = func.blocks[func.block_index(block)?].insns.len();
    }
    None
}

/// A value the load can be rewritten to read, and how it has to be read.
#[derive(Clone, Copy)]
struct Available {
    value: PseudoId,
    /// `None` when the pseudo already holds exactly the accessed bytes.
    narrow: Option<Narrowing>,
}

/// The width and type a forwarded value has to be truncated *from*.
#[derive(Clone, Copy)]
struct Narrowing {
    from: u32,
    typ: Option<TypeId>,
}

enum Candidate {
    /// This instruction leaves the wanted value in a pseudo, reached either
    /// directly or through a narrowing.
    Value(Available),
    /// This instruction may write the location, so nothing before it counts.
    Clobber,
}

/// How the value a store wrote reaches a load of the same bytes.
///
/// **A store narrows; the pseudo it stored does not.** `store.8` writes eight
/// bits of whatever the register holds, and a load of those bytes gets
/// exactly those eight -- but the value pseudo still holds the full width it
/// was computed at. Handing it over unchanged is the same mistake as "the
/// store will handle it", and it is what made `s.k = -1; mask = s.k;` come
/// back `0xFFFF` from an eight-bit bitfield.
///
/// `None` refuses the forward; otherwise the answer says how to read the
/// pseudo.
fn narrowing(func: &Function, am: &AddrMap, v: PseudoId, size: u32) -> Option<Available> {
    // A constant is width-polymorphic: it materializes at whatever width its
    // reader asks for, so it needs no narrowing -- but only where it means
    // one thing at that width, which is exactly what `unambiguous_at` asks.
    if let Some(c) = func.const_val(v) {
        return unambiguous_at(c, size.max(1)).then_some(Available {
            value: v,
            narrow: None,
        });
    }
    let (from, typ) = am.def_width(func, v)?;
    Some(Available {
        value: v,
        narrow: (from > size).then_some(Narrowing { from, typ }),
    })
}

fn candidate(
    func: &Function,
    types: &TypeTable,
    mi: &ModuleInfo,
    esc: &EscapeInfo,
    am: &AddrMap,
    insn: &Instruction,
    loc: &MemLoc,
) -> Option<Candidate> {
    match insn.op {
        Opcode::Store => {
            let s = am.location_of(func, insn);
            if is_same_access(&s, loc, types) {
                if let Some(v) = insn.src.get(1).copied() {
                    // A store this pass cannot forward is still a store: it
                    // has to stop the search, or the walk would run past it
                    // to an older value.
                    return Some(match narrowing(func, am, v, loc.size) {
                        Some(a) => Candidate::Value(a),
                        None => Candidate::Clobber,
                    });
                }
            }
            may_alias(&s, loc, mi).then_some(Candidate::Clobber)
        }
        Opcode::Load => {
            let s = am.location_of(func, insn);
            if is_same_access(&s, loc, types) {
                // A load's target holds exactly the bytes that were read, so
                // this one needs no narrowing.
                return insn.target.map(|t| {
                    Candidate::Value(Available {
                        value: t,
                        narrow: None,
                    })
                });
            }
            None
        }
        _ if writes(func, mi, esc, am, insn, loc) => Some(Candidate::Clobber),
        _ => None,
    }
}

/// Could `insn` write `loc`?
///
/// An allowlist read the safe way round: an opcode this does not recognize
/// is assumed to write, so a new one is conservative by default. That is the
/// same discipline `ifconv::is_speculatable` uses, and the reason
/// `Instruction::is_memory_barrier` is not enough on its own -- it answers
/// ordering, and omits `Store`, the mem intrinsics, the `Va*` family,
/// `Alloca` and `StackSave` entirely.
fn writes(
    func: &Function,
    mi: &ModuleInfo,
    esc: &EscapeInfo,
    am: &AddrMap,
    insn: &Instruction,
    loc: &MemLoc,
) -> bool {
    // A `Sym` target *is* storage, so an instruction that targets one writes
    // the object it names -- a struct-returning call writes its receiving
    // local this way, with no `Store` anywhere. The extent is left unknown
    // because `insn.size` describes a register, not the aggregate.
    if let Some(t) = insn.target {
        if matches!(
            func.get_pseudo(t).map(|p| &p.kind),
            Some(super::PseudoKind::Sym(_))
        ) && may_alias(&am.resolve(func, t, 0, 0, None), loc, mi)
        {
            return true;
        }
    }

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
        | Opcode::Ret
        | Opcode::Unreachable
        | Opcode::Load => false,

        Opcode::Store => {
            let s = am.location_of(func, insn);
            may_alias(&s, loc, mi)
        }

        // The extent is in the operands; `insn.size` on these is the
        // pointer's width, not the access's.
        Opcode::Memset | Opcode::Memcpy | Opcode::Memmove => {
            let dst = insn
                .src
                .first()
                .map(|a| am.resolve(func, *a, 0, 0, None))
                .unwrap_or_else(MemLoc::unknown);
            may_alias(&dst, loc, mi)
        }

        // **The rule that closes `pure-1`**: a callee cannot write a local
        // whose address never left this function, whatever it does. That
        // needs nothing at all from the callee.
        //
        // What the callee's effect adds is the *global* case, which escape
        // analysis can say nothing about: a `pure` or `const` function
        // writes no memory the caller can observe, so a global survives
        // across it too.
        Opcode::Call => {
            mi.call_effect(insn.func_name.as_deref()).may_write() && esc.is_captured(&loc.base)
        }

        _ if !insn.op.may_access_memory() => false,

        // `Asm`, `Fence`, `Alloca`, `StackSave`/`StackRestore`, the `Va*`
        // family, every atomic, and anything unlisted. A `"memory"` clobber
        // can name a frame slot without naming an operand -- `asm("movl $1,
        // -8(%rbp)")` is legal and reaches a local no analysis saw -- so
        // being blunt here costs nothing and removes the class.
        _ => true,
    }
}

/// May this location be forwarded from at all?
fn forwardable(func: &Function, types: &TypeTable, mi: &ModuleInfo, loc: &MemLoc) -> bool {
    if loc.offset.is_none() || loc.size == 0 {
        return false;
    }
    // Volatility and atomicity are properties of the object, not of the
    // instruction, so both ends are checked.
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

/// Is every path from the definition to the load free of a write to `loc`?
#[allow(clippy::too_many_arguments)]
fn no_clobber_between(
    func: &Function,
    _types: &TypeTable,
    mi: &ModuleInfo,
    esc: &EscapeInfo,
    am: &AddrMap,
    preds: &HashMap<BasicBlockId, Vec<BasicBlockId>>,
    succs: &HashMap<BasicBlockId, Vec<BasicBlockId>>,
    (bc, ic): (usize, usize),
    (bl, il): (usize, usize),
    loc: &MemLoc,
) -> bool {
    let def_block = func.blocks[bc].id;
    let load_block = func.blocks[bl].id;

    // Every block that can be entered on the way from the definition to the
    // load: forward-reachable from the definition's block *and*
    // backward-reachable from the load's.
    //
    // The intersection is what makes this right, not either half. A backward
    // walk alone climbs past the definition -- fatally so when the two share
    // a block, where it enumerates the whole function above them although
    // none of it runs between the two points. A forward walk alone runs off
    // down every path that never reaches the load.
    let Some(fwd) = reachable(succs, def_block) else {
        return false;
    };
    let Some(back) = reachable(preds, load_block) else {
        return false;
    };
    let region: HashSet<BasicBlockId> = fwd.intersection(&back).copied().collect();

    let scan = |bi: usize, from: usize, to: usize| -> bool {
        for i in from..to {
            let insn = &func.blocks[bi].insns[i];
            if insn.op == Opcode::Nop {
                continue;
            }
            if writes(func, mi, esc, am, insn, loc) {
                return false;
            }
        }
        true
    };

    // A back edge can put the load's own block in the region, and then the
    // instructions *after* the load run before the next execution of it.
    let load_block_wraps = region.contains(&load_block);

    if bc == bl {
        let to = if load_block_wraps {
            func.blocks[bl].insns.len()
        } else {
            il
        };
        let from = if load_block_wraps { 0 } else { ic + 1 };
        if !scan(bl, from, to) {
            return false;
        }
    } else {
        if !scan(bc, ic + 1, func.blocks[bc].insns.len()) {
            return false;
        }
        let (from, to) = if load_block_wraps {
            (0, func.blocks[bl].insns.len())
        } else {
            (0, il)
        };
        if !scan(bl, from, to) {
            return false;
        }
    }

    for b in &region {
        if *b == load_block {
            continue;
        }
        let Some(bi) = func.block_index(*b) else {
            return false;
        };
        if !scan(bi, 0, func.blocks[bi].insns.len()) {
            return false;
        }
    }
    true
}

/// Successors, as the exact inverse of the predecessor map, so the two can
/// never disagree about an edge.
fn invert(
    preds: &HashMap<BasicBlockId, Vec<BasicBlockId>>,
) -> HashMap<BasicBlockId, Vec<BasicBlockId>> {
    // Each predecessor list is duplicate-free and each block has one, so a
    // (p, b) pair arrives once: no duplicate can form. The search that used to
    // guard against one made a block of many successors -- a big `switch` --
    // quadratic in them.
    let mut succs: HashMap<BasicBlockId, Vec<BasicBlockId>> = HashMap::new();
    for (b, ps) in preds {
        for p in ps {
            succs.entry(*p).or_default().push(*b);
        }
    }
    succs
}

/// Every block with an edge path of length at least one from `seed`.
///
/// Length *at least one* is deliberate: `seed` itself is in the result only
/// when it lies on a cycle, which is exactly the question `load_block_wraps`
/// asks.
fn reachable(
    edges: &HashMap<BasicBlockId, Vec<BasicBlockId>>,
    seed: BasicBlockId,
) -> Option<HashSet<BasicBlockId>> {
    let mut seen: HashSet<BasicBlockId> = HashSet::new();
    let mut work: Vec<BasicBlockId> = edges.get(&seed).cloned().unwrap_or_default();
    let mut budget = MAX_REGION_BLOCKS;
    while let Some(b) = work.pop() {
        if budget == 0 {
            return None;
        }
        budget -= 1;
        if !seen.insert(b) {
            continue;
        }
        work.extend(edges.get(&b).cloned().unwrap_or_default());
    }
    Some(seen)
}

/// Predecessors from `children`, which `dce` maintains, rather than from
/// `parents`, which it does not.
///
/// Blocks are visited one at a time, so a repeated edge from `bb` -- a
/// conditional branch whose arms meet, an `asm goto` label that is also a
/// successor -- can only duplicate the entry `bb` itself pushed last. Checking
/// that one entry keeps the lists duplicate-free in the same order; searching
/// the whole list made a join of many predecessors quadratic in them.
fn build_preds(func: &Function) -> HashMap<BasicBlockId, Vec<BasicBlockId>> {
    let mut preds: HashMap<BasicBlockId, Vec<BasicBlockId>> = HashMap::new();
    let mut add = |to: BasicBlockId, from: BasicBlockId| {
        let e = preds.entry(to).or_default();
        if e.last() != Some(&from) {
            e.push(from);
        }
    };
    for bb in &func.blocks {
        for c in &bb.children {
            add(*c, bb.id);
        }
        for insn in &bb.insns {
            if let Some(ref asm) = insn.asm_data {
                for (t, _) in &asm.goto_labels {
                    add(*t, bb.id);
                }
            }
        }
    }
    preds
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::memloc::ModuleInfo;
    use crate::ir::{BasicBlock, Module, Pseudo, PseudoKind};
    use crate::target::Target;

    fn host_types() -> TypeTable {
        TypeTable::new(&Target::host())
    }

    fn module_info(types: &TypeTable) -> ModuleInfo {
        ModuleInfo::build(&Module::default(), types)
    }

    /// A function with one 32-bit local `@a.0`, addressed through
    /// `%10 = symaddr %0`, and blocks supplied by the caller.
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
            f.add_pseudo(Pseudo::val(PseudoId(5), 7));
            f.add_pseudo(Pseudo::val(PseudoId(6), 9));
            f.add_local("a.0", PseudoId(0), i32t, false, false, None, None);
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
            let mi = module_info(&self.types);
            super::run(&mut self.f, &self.types, &mi)
        }

        /// The opcode at `(block, index)` after the pass.
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

    fn call(name: &str, types: &TypeTable) -> Instruction {
        Instruction::call(None, name, vec![], vec![], types.void_id, 0)
    }

    /// A store and a load of the same bytes in one block, with a call to an
    /// entirely unknown function in between: **the rule that closes
    /// `pure-1`**. The callee cannot write a local whose address never left
    /// the function, whatever it does.
    #[test]
    fn loadfwd_forwards_across_a_call_to_a_non_escaping_local() {
        let mut b = Build::new();
        let (i32t, c) = (b.types.int_id, call("unknown", &b.types));
        b.block(
            0,
            vec![
                entry(),
                Instruction::sym_addr(PseudoId(10), PseudoId(0), i32t),
                Instruction::store(PseudoId(5), PseudoId(10), 0, i32t, 32),
                c,
                Instruction::load(PseudoId(20), PseudoId(10), 0, i32t, 32),
            ],
            vec![],
        );
        assert!(b.run());
        assert_eq!(b.op(0, 4), Opcode::Copy);
        assert_eq!(b.f.blocks[0].insns[4].src, vec![PseudoId(5)]);
    }

    /// The same shape on a *global*, which any externally-linked callee can
    /// name. Nothing may be forwarded across the call.
    #[test]
    fn loadfwd_refuses_across_a_call_to_a_global() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.f.add_pseudo(Pseudo::sym(PseudoId(1), "g".into()));
        let c = call("unknown", &b.types);
        b.block(
            0,
            vec![
                entry(),
                Instruction::store(PseudoId(5), PseudoId(1), 0, i32t, 32),
                c,
                Instruction::load(PseudoId(20), PseudoId(1), 0, i32t, 32),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 3), Opcode::Load);
    }

    /// **Risk 2, the back-edge clobber.** The store dominates the load, and
    /// the clobber sits *after* the load on the latch, so it runs before the
    /// load's next execution. `for(;;){ s = a[0]; a[0] = 1; }` reads 1 on
    /// every iteration but the first.
    #[test]
    fn loadfwd_refuses_a_clobber_on_the_back_edge() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.block(
            0,
            vec![
                entry(),
                Instruction::sym_addr(PseudoId(10), PseudoId(0), i32t),
                Instruction::store(PseudoId(5), PseudoId(10), 0, i32t, 32),
                br(1),
            ],
            vec![1],
        );
        b.block(
            1,
            vec![
                Instruction::load(PseudoId(20), PseudoId(10), 0, i32t, 32),
                // The clobber, after the load, on the latch.
                Instruction::store(PseudoId(6), PseudoId(10), 0, i32t, 32),
                br(1),
            ],
            vec![1],
        );
        assert!(!b.run());
        assert_eq!(b.op(1, 0), Opcode::Load);
    }

    /// **Risk 3, the dominator chain is not every path.** The store
    /// dominates the load, but one arm of the diamond writes the same bytes.
    #[test]
    fn loadfwd_refuses_a_clobber_on_one_arm_of_a_diamond() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.f.add_pseudo(Pseudo::val(PseudoId(7), 1));
        b.block(
            0,
            vec![
                entry(),
                Instruction::sym_addr(PseudoId(10), PseudoId(0), i32t),
                Instruction::store(PseudoId(5), PseudoId(10), 0, i32t, 32),
                cbr(PseudoId(7), 1, 2),
            ],
            vec![1, 2],
        );
        b.block(
            1,
            vec![
                Instruction::store(PseudoId(6), PseudoId(10), 0, i32t, 32),
                br(3),
            ],
            vec![3],
        );
        b.block(2, vec![br(3)], vec![3]);
        b.block(
            3,
            vec![Instruction::load(PseudoId(20), PseudoId(10), 0, i32t, 32)],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(3, 0), Opcode::Load);
    }

    /// The same diamond with *no* clobber on either arm forwards, which is
    /// what keeps the refusal above from being vacuous.
    #[test]
    fn loadfwd_forwards_through_a_clean_diamond() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.f.add_pseudo(Pseudo::val(PseudoId(7), 1));
        b.block(
            0,
            vec![
                entry(),
                Instruction::sym_addr(PseudoId(10), PseudoId(0), i32t),
                Instruction::store(PseudoId(5), PseudoId(10), 0, i32t, 32),
                cbr(PseudoId(7), 1, 2),
            ],
            vec![1, 2],
        );
        b.block(1, vec![br(3)], vec![3]);
        b.block(2, vec![br(3)], vec![3]);
        b.block(
            3,
            vec![Instruction::load(PseudoId(20), PseudoId(10), 0, i32t, 32)],
            vec![],
        );
        assert!(b.run());
        assert_eq!(b.op(3, 0), Opcode::Copy);
    }

    /// An `asm` can name a frame slot without naming an operand, so it
    /// clobbers everything.
    #[test]
    fn loadfwd_refuses_across_inline_asm() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        let mut asm = Instruction::new(Opcode::Asm);
        asm.asm_data = Some(Box::new(crate::ir::AsmData {
            template: String::new(),
            outputs: vec![],
            inputs: vec![],
            clobbers: vec!["memory".into()],
            goto_labels: vec![],
        }));
        b.block(
            0,
            vec![
                entry(),
                Instruction::sym_addr(PseudoId(10), PseudoId(0), i32t),
                Instruction::store(PseudoId(5), PseudoId(10), 0, i32t, 32),
                asm,
                Instruction::load(PseudoId(20), PseudoId(10), 0, i32t, 32),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 4), Opcode::Load);
    }

    /// A `volatile` local is read as many times as it is written, and the
    /// instruction does not say so -- the property is on the object.
    #[test]
    fn loadfwd_refuses_a_volatile_local() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.f.add_local("a.0", PseudoId(0), i32t, true, false, None, None);
        b.block(
            0,
            vec![
                entry(),
                Instruction::sym_addr(PseudoId(10), PseudoId(0), i32t),
                Instruction::store(PseudoId(5), PseudoId(10), 0, i32t, 32),
                Instruction::load(PseudoId(20), PseudoId(10), 0, i32t, 32),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 3), Opcode::Load);
    }

    /// A load of bytes an earlier *load* already read is the redundant-load
    /// case, and it is the same algorithm.
    #[test]
    fn loadfwd_eliminates_a_redundant_load() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.block(
            0,
            vec![
                entry(),
                Instruction::sym_addr(PseudoId(10), PseudoId(0), i32t),
                Instruction::load(PseudoId(20), PseudoId(10), 0, i32t, 32),
                Instruction::load(PseudoId(21), PseudoId(10), 0, i32t, 32),
            ],
            vec![],
        );
        assert!(b.run());
        assert_eq!(b.op(0, 3), Opcode::Copy);
        assert_eq!(b.f.blocks[0].insns[3].src, vec![PseudoId(20)]);
    }

    /// **A store narrows; the pseudo it stored does not.** An eight-bit
    /// store of a value computed at thirty-two must reach the load through a
    /// truncation, or `s.k = -1; mask = s.k;` comes back `0xFFFF` from an
    /// eight-bit field.
    #[test]
    fn loadfwd_narrows_a_wider_value_to_the_access() {
        let mut b = Build::new();
        let (i32t, i8t) = (b.types.int_id, b.types.char_id);
        b.block(
            0,
            vec![
                entry(),
                Instruction::sym_addr(PseudoId(10), PseudoId(0), i32t),
                // %30 is computed at 32 bits ...
                Instruction::binop(
                    Opcode::Add,
                    PseudoId(30),
                    PseudoId(5),
                    PseudoId(6),
                    i32t,
                    32,
                ),
                // ... and only its low 8 bits are stored.
                Instruction::store(PseudoId(30), PseudoId(10), 0, i8t, 8),
                Instruction::load(PseudoId(20), PseudoId(10), 0, i8t, 8),
            ],
            vec![],
        );
        assert!(b.run());
        let fwd = &b.f.blocks[0].insns[4];
        assert_eq!(
            fwd.op,
            Opcode::Trunc,
            "the store narrowed; the value did not"
        );
        assert_eq!(fwd.src, vec![PseudoId(30)]);
        assert_eq!(fwd.src_size, 32);
        assert_eq!(fwd.size, 8);
    }

    /// A value already at the access width needs no truncation.
    #[test]
    fn loadfwd_does_not_narrow_a_value_already_at_the_access_width() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.block(
            0,
            vec![
                entry(),
                Instruction::sym_addr(PseudoId(10), PseudoId(0), i32t),
                Instruction::binop(
                    Opcode::Add,
                    PseudoId(30),
                    PseudoId(5),
                    PseudoId(6),
                    i32t,
                    32,
                ),
                Instruction::store(PseudoId(30), PseudoId(10), 0, i32t, 32),
                Instruction::load(PseudoId(20), PseudoId(10), 0, i32t, 32),
            ],
            vec![],
        );
        assert!(b.run());
        assert_eq!(b.op(0, 4), Opcode::Copy);
    }

    /// A store of a constant that means two things at the access width is
    /// refused outright: `-1` stored as eight bits is `255` read back, and
    /// nothing in the IR says which the reader wants.
    #[test]
    fn loadfwd_refuses_an_ambiguous_narrow_constant() {
        let mut b = Build::new();
        let (i32t, i8t) = (b.types.int_id, b.types.char_id);
        b.f.add_pseudo(Pseudo::val(PseudoId(8), -1));
        b.block(
            0,
            vec![
                entry(),
                Instruction::sym_addr(PseudoId(10), PseudoId(0), i32t),
                Instruction::store(PseudoId(8), PseudoId(10), 0, i8t, 8),
                Instruction::load(PseudoId(20), PseudoId(10), 0, i8t, 8),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 3), Opcode::Load);
    }

    /// Disjoint bytes of one object are not the same access, and the
    /// intervening store must not be mistaken for the wanted one.
    #[test]
    fn loadfwd_distinguishes_disjoint_offsets() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.block(
            0,
            vec![
                entry(),
                Instruction::sym_addr(PseudoId(10), PseudoId(0), i32t),
                Instruction::store(PseudoId(5), PseudoId(10), 0, i32t, 32),
                Instruction::store(PseudoId(6), PseudoId(10), 4, i32t, 32),
                Instruction::load(PseudoId(20), PseudoId(10), 0, i32t, 32),
            ],
            vec![],
        );
        assert!(b.run());
        assert_eq!(
            b.f.blocks[0].insns[4].src,
            vec![PseudoId(5)],
            "the store at offset 4 does not supply offset 0"
        );
    }

    /// An overlapping store of a *different* extent stops the search: it
    /// wrote some of the bytes, and this pass cannot say which.
    #[test]
    fn loadfwd_refuses_a_partial_overwrite() {
        let mut b = Build::new();
        let (i32t, i8t) = (b.types.int_id, b.types.char_id);
        b.block(
            0,
            vec![
                entry(),
                Instruction::sym_addr(PseudoId(10), PseudoId(0), i32t),
                Instruction::store(PseudoId(5), PseudoId(10), 0, i32t, 32),
                // one byte inside the four just written
                Instruction::store(PseudoId(6), PseudoId(10), 1, i8t, 8),
                Instruction::load(PseudoId(20), PseudoId(10), 0, i32t, 32),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 4), Opcode::Load);
    }

    /// `setjmp` makes every ordering claim void, so the pass declines the
    /// whole function.
    #[test]
    fn loadfwd_declines_a_function_with_setjmp() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        b.block(
            0,
            vec![
                entry(),
                Instruction::sym_addr(PseudoId(10), PseudoId(0), i32t),
                Instruction::store(PseudoId(5), PseudoId(10), 0, i32t, 32),
                Instruction::new(Opcode::Setjmp),
                Instruction::load(PseudoId(20), PseudoId(10), 0, i32t, 32),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 4), Opcode::Load);
    }

    /// Predecessors come from `children`, which `dce` maintains, and never
    /// from `parents`, which it does not.
    #[test]
    fn loadfwd_derives_predecessors_from_children() {
        let mut b = Build::new();
        b.block(0, vec![entry(), br(1)], vec![1]);
        b.block(1, vec![br(1)], vec![1]);
        b.f.entry = BasicBlockId(0);
        // A stale `parents` must not be consulted.
        b.f.blocks[1].parents = vec![BasicBlockId(9)];
        let preds = build_preds(&b.f);
        assert_eq!(
            preds.get(&BasicBlockId(1)).map(Vec::as_slice),
            Some(&[BasicBlockId(0), BasicBlockId(1)][..])
        );
    }

    /// Successors are the exact inverse of predecessors, so the two can
    /// never disagree about an edge -- which is what makes the region an
    /// intersection rather than a guess.
    #[test]
    fn loadfwd_reachability_is_at_least_one_edge() {
        let mut preds: HashMap<BasicBlockId, Vec<BasicBlockId>> = HashMap::new();
        preds.insert(BasicBlockId(1), vec![BasicBlockId(0)]);
        preds.insert(BasicBlockId(2), vec![BasicBlockId(1)]);
        let succs = invert(&preds);
        // A block not on a cycle is not reachable from itself.
        assert_eq!(
            reachable(&succs, BasicBlockId(0)),
            Some([BasicBlockId(1), BasicBlockId(2)].into_iter().collect())
        );
        // With a back edge it is.
        preds
            .entry(BasicBlockId(1))
            .or_default()
            .push(BasicBlockId(2));
        let succs = invert(&preds);
        let r = reachable(&succs, BasicBlockId(1)).unwrap();
        assert!(r.contains(&BasicBlockId(1)), "a cycle reaches its own head");
    }

    /// A `Sym` target *is* storage: a struct-returning call writes its
    /// receiving local with no `Store` anywhere, and missing that forwards a
    /// value the call replaced.
    #[test]
    fn loadfwd_a_sym_target_writes_its_object() {
        let mut b = Build::new();
        let i32t = b.types.int_id;
        assert!(matches!(
            b.f.get_pseudo(PseudoId(0)).map(|p| &p.kind),
            Some(PseudoKind::Sym(_))
        ));
        let mut c = Instruction::call(
            Some(PseudoId(0)),
            "makes_a_struct",
            vec![],
            vec![],
            i32t,
            64,
        );
        c.target = Some(PseudoId(0));
        b.block(
            0,
            vec![
                entry(),
                Instruction::sym_addr(PseudoId(10), PseudoId(0), i32t),
                Instruction::store(PseudoId(5), PseudoId(10), 0, i32t, 32),
                c,
                Instruction::load(PseudoId(20), PseudoId(10), 0, i32t, 32),
            ],
            vec![],
        );
        assert!(!b.run());
        assert_eq!(b.op(0, 4), Opcode::Load);
    }
}
