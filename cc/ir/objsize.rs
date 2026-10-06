//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__builtin_object_size`, answered once the optimizer has found the object
//
// The parser answers the builtin from the expression as written --
// `__builtin_object_size(buf + 4, 0)` -- and defers the rest as an
// `ObjectSize` placeholder: a pointer variable, a choice between two
// addresses, a parameter of a function about to be inlined. gcc answers those
// after inlining and propagation (its `objsz` pass), and `_FORTIFY_SOURCE`
// depends on it: glibc's `memcpy` is `__builtin___memcpy_chk(d, s, n,
// __builtin_object_size(d, 0))`, and the check is dropped at compile time
// only where the size is a constant (`libcall_fold::fortify`).
//
// The answer walks the pointer back to the objects it may point into, the way
// `memloc` resolves an address: through copies, `SymAddr`, and constant
// displacements, to a local, a named object of this unit, a string literal or
// an `alloca` of a constant size. Unlike `memloc`, it also goes through a
// `Select` and a phi, whose arms each reach an object of their own: the
// maximum remaining (bit 1 of the type clear) is the largest of the arms'
// answers, the minimum the smallest -- `p = c ? &a.x[5] : &a.y[4]` leaves 15
// bytes at most. A displacement outside the object leaves nothing, as in gcc.
//
// A phi met again on its own cycle -- a pointer stepped through a loop -- adds
// nothing new where the step cannot raise the answer: none at all, or a
// forward step for a maximum (the bytes left only shrink). A backward step
// raises the maximum without bound, so that answer is unknown; a forward one
// lowers the minimum without bound, so that answer is 0.
//
// What is not seen through is unknown, and an unknown arm makes the whole
// answer unknown: `(size_t)-1` for a maximum, 0 for a minimum. That answer is
// not given until the optimizer has converged (`Settle::Everything`), since
// what is unknown now -- a load not yet forwarded, an arm not yet proved dead
// -- may be known after another round.
//
// Bit 0 asks about the closest surrounding subobject, which the IR does not
// record: `&s.buf[2]` is an address 2 bytes into `s`. What is left of the
// whole object is never less than what is left of the subobject, so it stands
// as the subobject's maximum -- a check that passes where gcc's would have
// fired, never the reverse. It is no minimum, so type 3 gets the unknown
// answer. The parser answers either type exactly wherever the expression
// itself names the subobject.
//

use super::memloc::AddrMap;
use super::propagate::fold_target_to_const;
use super::{Function, Module, Opcode, PseudoId, PseudoKind, Site};
use crate::parse::ast::ObjectSizeType;
use crate::token::lexer::payload_bytes;
use crate::types::TypeTable;
use std::collections::HashMap;

/// How many definitions one answer follows before it gives up.
const MAX_WALK: usize = 64;

/// The size of every named object of a module whose size is known: what a
/// pointer to its start may reach.
pub struct ObjectSizes {
    named: HashMap<String, u64>,
}

impl ObjectSizes {
    /// The sizes of `module`'s objects: each global and static of a complete
    /// type, and each string literal.
    ///
    /// A weak definition may be replaced at link time by one of another
    /// size, and an object whose type ends in a flexible array member, or an
    /// array of unknown bound, may be larger than its type: neither is
    /// listed, so a pointer into one is a pointer into an unknown object.
    pub fn build(module: &Module, types: &TypeTable) -> ObjectSizes {
        let mut named = HashMap::new();
        for g in &module.globals {
            if g.symbol_attrs.weak || types.has_unbounded_tail(g.typ) {
                continue;
            }
            let size = types.size_bytes(g.typ) as u64;
            if size > 0 {
                named.insert(g.name.clone(), size);
            }
        }
        for (label, payload) in &module.strings {
            named.insert(label.clone(), payload_bytes(payload).count() as u64 + 1);
        }
        ObjectSizes { named }
    }

    /// The size of the named object `name`, when it is listed.
    fn named(&self, name: &str) -> Option<u64> {
        self.named.get(name).copied()
    }
}

/// Which `ObjectSize` placeholders a run answers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Settle {
    /// Only those whose object is known: an unknown one may become known
    /// on the optimizer's next round.
    Known,
    /// Every one, the unknown ones with [`ObjectSizeType::unknown`]: the
    /// optimizer has converged, and nothing more will be learnt.
    Everything,
}

/// What one arm of a walk reached.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Reach {
    /// An object with this many bytes left past the pointer.
    Bytes(u64),
    /// Something the walk cannot see into.
    Unknown,
    /// Nothing: the arm comes back to a phi already being walked, and can
    /// only carry what the other arms bring.
    Cycle,
}

/// The IR constant for a size: a 64-bit `size_t` is kept sign-extended, as
/// every 64-bit constant is, so `(size_t)-1` is -1.
pub(crate) fn size_constant(size: u64) -> i128 {
    i128::from(size as i64)
}

/// Answer the `ObjectSize` placeholders in `func` that `settle` says to.
/// Answers whether anything changed.
pub fn run(func: &mut Function, types: &TypeTable, sizes: &ObjectSizes, settle: Settle) -> bool {
    let answers = answers(func, types, sizes, settle);
    let mut minted = HashMap::new();
    let mut changed = false;
    for (site, size) in answers {
        changed |= fold_target_to_const(func, site, size_constant(size), &mut minted);
    }
    changed
}

/// Each placeholder `settle` says to answer, and its answer.
fn answers(
    func: &Function,
    types: &TypeTable,
    sizes: &ObjectSizes,
    settle: Settle,
) -> Vec<(Site, u64)> {
    let mut found = Vec::new();
    let mut am = None;
    for (b, bb) in func.blocks.iter().enumerate() {
        for (i, insn) in bb.insns.iter().enumerate() {
            let Opcode::ObjectSize(otype) = insn.op else {
                continue;
            };
            let Some(&ptr) = insn.src.first() else {
                continue;
            };
            let am = am.get_or_insert_with(|| AddrMap::build(func));
            let reach = Walker::new(func, types, am, sizes, otype).reach(ptr);
            if let Some(size) = answer(otype, reach, settle) {
                found.push(((b, i), size));
            }
        }
    }
    found
}

/// What a placeholder of type `otype` whose pointer reached `reach` answers,
/// if `settle` says to answer it now.
fn answer(otype: ObjectSizeType, reach: Reach, settle: Settle) -> Option<u64> {
    match reach {
        // The whole object is no minimum for a subobject: see the header.
        Reach::Bytes(n) if !(otype.subobject && otype.minimum) => Some(n),
        _ => (settle == Settle::Everything).then(|| otype.unknown()),
    }
}

/// One walk from a pointer back to the objects it may point into.
struct Walker<'a> {
    func: &'a Function,
    am: &'a AddrMap,
    sizes: &'a ObjectSizes,
    types: &'a TypeTable,
    /// Whether the arms' answers are joined by their minimum.
    minimum: bool,
    /// The width of an address: what pointer arithmetic is done at.
    ptr_bits: u32,
    /// Definitions left to follow.
    budget: usize,
    /// The phis being walked, and the displacement each was entered at.
    open: HashMap<PseudoId, i64>,
}

impl<'a> Walker<'a> {
    fn new(
        func: &'a Function,
        types: &'a TypeTable,
        am: &'a AddrMap,
        sizes: &'a ObjectSizes,
        otype: ObjectSizeType,
    ) -> Self {
        Walker {
            func,
            am,
            sizes,
            types,
            minimum: otype.minimum,
            ptr_bits: types.size_bits(types.void_ptr_id),
            budget: MAX_WALK,
            open: HashMap::new(),
        }
    }

    /// What `ptr` reaches, all arms joined.
    fn reach(mut self, ptr: PseudoId) -> Reach {
        match self.walk(ptr, 0) {
            Reach::Cycle => Reach::Unknown,
            reach => reach,
        }
    }

    /// What `p` advanced `off` bytes reaches.
    fn walk(&mut self, p: PseudoId, off: i64) -> Reach {
        let Some(budget) = self.budget.checked_sub(1) else {
            return Reach::Unknown;
        };
        self.budget = budget;
        if let Some(size) = self.object_size(p) {
            return Reach::Bytes(remaining(size, off));
        }
        let Some(d) = self.am.def(self.func, p) else {
            return Reach::Unknown;
        };
        match (d.op, d.src.as_slice()) {
            // `SymAddr(s)` is the address of `s`; a thread-local's object is
            // the same size in every thread.
            (Opcode::Copy | Opcode::PhiSource | Opcode::SymAddr | Opcode::TlsAddr, &[src]) => {
                self.walk(src, off)
            }
            (Opcode::Add, &[a, b]) if d.size == self.ptr_bits => {
                match (self.displacement(b), self.displacement(a)) {
                    (Some(k), _) => self.step(a, off, k),
                    (None, Some(k)) => self.step(b, off, k),
                    (None, None) => Reach::Unknown,
                }
            }
            (Opcode::Sub, &[a, b]) if d.size == self.ptr_bits => {
                match self.displacement(b).and_then(i64::checked_neg) {
                    Some(k) => self.step(a, off, k),
                    None => Reach::Unknown,
                }
            }
            (Opcode::Select, &[_, a, b]) => self.join(&[a, b], off),
            (Opcode::Phi, _) => self.phi(p, off),
            (Opcode::Alloca, &[n]) => match self.am.const_operand(n, self.ptr_bits) {
                Some(n) => {
                    u64::try_from(n).map_or(Reach::Unknown, |n| Reach::Bytes(remaining(n, off)))
                }
                None => Reach::Unknown,
            },
            _ => Reach::Unknown,
        }
    }

    /// The size of the object `p` is the storage of: a local, or a named
    /// object whose size is known.
    fn object_size(&self, p: PseudoId) -> Option<u64> {
        let PseudoKind::Sym(name) = &self.func.get_pseudo(p)?.kind else {
            return None;
        };
        let size = match self.func.local_of(p) {
            Some(local) => self.types.size_bytes(local.typ) as u64,
            None => self.sizes.named(name)?,
        };
        (size > 0).then_some(size)
    }

    /// The constant `p` adds to an address, if it is one.
    fn displacement(&self, p: PseudoId) -> Option<i64> {
        i64::try_from(self.am.const_operand(p, self.ptr_bits)?).ok()
    }

    /// What `p` reaches, `k` bytes further on than `off`.
    fn step(&mut self, p: PseudoId, off: i64, k: i64) -> Reach {
        match off.checked_add(k) {
            Some(off) => self.walk(p, off),
            None => Reach::Unknown,
        }
    }

    /// What the phi `p` reaches, advanced `off` bytes: its arms joined, or,
    /// met again on its own cycle, what the step around the cycle allows.
    fn phi(&mut self, p: PseudoId, off: i64) -> Reach {
        if let Some(&entered) = self.open.get(&p) {
            return match (off.cmp(&entered), self.minimum) {
                (std::cmp::Ordering::Equal, _) => Reach::Cycle,
                // Forward: fewer bytes each time round.
                (std::cmp::Ordering::Greater, false) => Reach::Cycle,
                (std::cmp::Ordering::Greater, true) => Reach::Bytes(0),
                // Backward: more bytes each time round.
                (std::cmp::Ordering::Less, false) => Reach::Unknown,
                (std::cmp::Ordering::Less, true) => Reach::Cycle,
            };
        }
        let Some(d) = self.am.def(self.func, p) else {
            return Reach::Unknown;
        };
        let arms: Vec<PseudoId> = d.phi_list.iter().map(|&(_, v)| v).collect();
        self.open.insert(p, off);
        let joined = self.join(&arms, off);
        self.open.remove(&p);
        joined
    }

    /// The arms' answers, each advanced `off` bytes, joined: an unknown arm
    /// makes the answer unknown, and an arm that only cycles adds nothing.
    fn join(&mut self, arms: &[PseudoId], off: i64) -> Reach {
        let mut acc = Reach::Cycle;
        for &arm in arms {
            acc = match (acc, self.walk(arm, off)) {
                (Reach::Unknown, _) | (_, Reach::Unknown) => return Reach::Unknown,
                (Reach::Cycle, r) | (r, Reach::Cycle) => r,
                (Reach::Bytes(a), Reach::Bytes(b)) if self.minimum => Reach::Bytes(a.min(b)),
                (Reach::Bytes(a), Reach::Bytes(b)) => Reach::Bytes(a.max(b)),
            };
        }
        acc
    }
}

/// What is left of an object of `size` bytes past `off`: nothing outside it.
fn remaining(size: u64, off: i64) -> u64 {
    u64::try_from(off).map_or(0, |off| size.saturating_sub(off))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Instruction, Pseudo};
    use crate::target::Target;
    use crate::types::Type;

    /// A function under construction, with the module-wide sizes its
    /// globals have.
    struct Fx {
        types: TypeTable,
        func: Function,
        sizes: ObjectSizes,
    }

    impl Fx {
        fn new() -> Fx {
            let types = TypeTable::new(&Target::new(
                crate::target::Arch::X86_64,
                crate::target::Os::Linux,
            ));
            let mut func = Function::new("f", types.ulong_id);
            func.next_pseudo = 1;
            let mut b = BasicBlock::new(BasicBlockId(0));
            b.add_insn(Instruction::new(Opcode::Entry));
            func.add_block(b);
            func.entry = BasicBlockId(0);
            Fx {
                types,
                func,
                sizes: ObjectSizes {
                    named: HashMap::new(),
                },
            }
        }

        fn fresh(&mut self) -> PseudoId {
            let p = self.func.alloc_pseudo();
            self.func.add_pseudo(Pseudo::reg(p, p.0));
            p
        }

        fn push(&mut self, block: usize, insn: Instruction) {
            self.func.blocks[block].insns.push(insn);
        }

        fn ptr(&self) -> crate::types::TypeId {
            self.types.char_ptr_id
        }

        /// The address of a new local `char[size]`.
        fn local(&mut self, name: &str, size: usize) -> PseudoId {
            let arr = self.types.intern(Type::array(self.types.char_id, size));
            let sym = self.func.alloc_pseudo();
            self.func.add_pseudo(Pseudo::sym(sym, name.to_string()));
            self.func.add_local(name, sym, arr, None, None);
            let p = self.fresh();
            let ptr = self.ptr();
            self.push(0, Instruction::sym_addr(p, sym, ptr));
            p
        }

        /// The address of the named object `name` of `size` bytes.
        fn global(&mut self, name: &str, size: u64) -> PseudoId {
            self.sizes.named.insert(name.to_string(), size);
            let sym = self.func.alloc_pseudo();
            self.func.add_pseudo(Pseudo::sym(sym, name.to_string()));
            let p = self.fresh();
            let ptr = self.ptr();
            self.push(0, Instruction::sym_addr(p, sym, ptr));
            p
        }

        fn konst(&mut self, v: i128) -> PseudoId {
            self.func.create_const_pseudo(v)
        }

        /// A value the function cannot know: a parameter.
        fn unknown(&mut self) -> PseudoId {
            let p = self.func.alloc_pseudo();
            self.func.add_pseudo(Pseudo::arg(p, 0));
            p
        }

        fn binop(&mut self, op: Opcode, a: PseudoId, b: PseudoId) -> PseudoId {
            let t = self.fresh();
            let ptr = self.ptr();
            self.push(0, Instruction::binop(op, t, a, b, ptr, 64));
            t
        }

        fn select(&mut self, a: PseudoId, b: PseudoId) -> PseudoId {
            let (c, t) = (self.unknown(), self.fresh());
            let ptr = self.ptr();
            self.push(0, Instruction::select(t, c, a, b, ptr, 64));
            t
        }

        /// `ObjectSize(p)` of `otype`; returns its target.
        fn object_size(&mut self, p: PseudoId, otype: u64) -> PseudoId {
            let t = self.fresh();
            let ulong = self.types.ulong_id;
            let otype = ObjectSizeType::from_bits(otype);
            self.push(
                0,
                Instruction::new(Opcode::ObjectSize(otype))
                    .with_target(t)
                    .with_src(p)
                    .with_type_and_size(ulong, 64),
            );
            t
        }

        /// What the placeholder targeting `t` answers under `settle`, as an
        /// unsigned size; `None` if it is left alone.
        fn answer(&mut self, t: PseudoId, settle: Settle) -> Option<u64> {
            run(&mut self.func, &self.types, &self.sizes, settle);
            let insn = self
                .func
                .blocks
                .iter()
                .flat_map(|b| &b.insns)
                .find(|i| i.target == Some(t))
                .unwrap();
            match insn.op {
                Opcode::Copy => Some(self.func.const_val(insn.src[0]).unwrap() as u64),
                _ => None,
            }
        }

        /// The size type 0 gives `p`, and the size type 2 gives it.
        fn max_min(&mut self, p: PseudoId) -> (Option<u64>, Option<u64>) {
            let (max, min) = (self.object_size(p, 0), self.object_size(p, 2));
            (
                self.answer(max, Settle::Known),
                self.answer(min, Settle::Known),
            )
        }
    }

    /// A local and a named object, at their start and at a constant
    /// displacement in either direction; past the end leaves nothing.
    #[test]
    fn a_known_object_at_a_constant_displacement() {
        let mut fx = Fx::new();
        let buf = fx.local("buf", 32);
        assert_eq!(fx.max_min(buf), (Some(32), Some(32)));
        let k = fx.konst(4);
        let p = fx.binop(Opcode::Add, buf, k);
        assert_eq!(fx.max_min(p), (Some(28), Some(28)));
        let k8 = fx.konst(8);
        let q = fx.binop(Opcode::Add, buf, k8);
        let back = fx.binop(Opcode::Sub, q, k);
        assert_eq!(fx.max_min(back), (Some(28), Some(28)));
        let k40 = fx.konst(40);
        let past = fx.binop(Opcode::Add, buf, k40);
        assert_eq!(fx.max_min(past), (Some(0), Some(0)));
        let g = fx.global("g", 20);
        let g7 = fx.binop(Opcode::Add, k, g);
        assert_eq!(fx.max_min(g7), (Some(16), Some(16)));
    }

    /// A choice between two objects is the larger for a maximum and the
    /// smaller for a minimum: `c ? &a.b1[5] : &a.b2[4]` in a 20-byte `a`.
    #[test]
    fn a_choice_joins_its_arms() {
        let mut fx = Fx::new();
        let a = fx.local("a", 20);
        let (k5, k14) = (fx.konst(5), fx.konst(14));
        let b1 = fx.binop(Opcode::Add, a, k5);
        let b2 = fx.binop(Opcode::Add, a, k14);
        let r = fx.select(b1, b2);
        assert_eq!(fx.max_min(r), (Some(15), Some(6)));
        // Type 1 stands on the whole object's maximum; type 3 has no
        // minimum to give until the end.
        let t1 = fx.object_size(r, 1);
        assert_eq!(fx.answer(t1, Settle::Known), Some(15));
        let t3 = fx.object_size(r, 3);
        assert_eq!(fx.answer(t3, Settle::Known), None);
        assert_eq!(fx.answer(t3, Settle::Everything), Some(0));
    }

    /// An `alloca` of a constant size is an object of that size.
    #[test]
    fn an_alloca_of_a_constant_size() {
        let mut fx = Fx::new();
        let (n, t) = (fx.konst(4), fx.fresh());
        let ptr = fx.ptr();
        fx.push(
            0,
            Instruction::new(Opcode::Alloca)
                .with_target(t)
                .with_src(n)
                .with_type_and_size(ptr, 64),
        );
        let a = fx.local("a", 20);
        let k = fx.konst(17);
        let tail = fx.binop(Opcode::Add, a, k);
        let r = fx.select(t, tail);
        assert_eq!(fx.max_min(r), (Some(4), Some(3)));
    }

    /// An unknown pointer, or an unknown displacement, is answered only
    /// once everything is settled -- and then with the unknown answers --
    /// and an unknown arm makes a choice unknown.
    #[test]
    fn an_unknown_object_waits_for_the_end() {
        let mut fx = Fx::new();
        let u = fx.unknown();
        let buf = fx.local("buf", 32);
        let i = fx.unknown();
        let at_i = fx.binop(Opcode::Add, buf, i);
        let either = fx.select(u, buf);
        for p in [u, at_i, either] {
            assert_eq!(fx.max_min(p), (None, None));
            let (max, min) = (fx.object_size(p, 0), fx.object_size(p, 2));
            assert_eq!(fx.answer(max, Settle::Everything), Some(u64::MAX));
            assert_eq!(fx.answer(min, Settle::Everything), Some(0));
        }
    }

    /// Adds a block with `p = phi(entry: init, loop: next)`, where `next`
    /// is `p` stepped by `step`, and answers type 0 and type 2 of `p`.
    fn loop_answers(step: i128) -> (Option<u64>, Option<u64>) {
        let mut fx = Fx::new();
        let buf = fx.local("buf", 32);
        let k16 = fx.konst(16);
        let init = fx.binop(Opcode::Add, buf, k16);
        let (p, from_entry, from_loop) = (fx.fresh(), fx.fresh(), fx.fresh());
        let ptr = fx.ptr();
        let (entry, body) = (BasicBlockId(0), BasicBlockId(1));
        let mut src = Instruction::phi_source(from_entry, init, ptr, 64);
        src.phi_list = vec![(body, p)];
        fx.push(0, src);
        fx.func.add_block(BasicBlock::new(body));
        let mut phi = Instruction::new(Opcode::Phi)
            .with_target(p)
            .with_type_and_size(ptr, 64);
        phi.phi_list = vec![(entry, from_entry), (body, from_loop)];
        fx.push(1, phi);
        let k = fx.konst(step);
        let next = fx.fresh();
        fx.push(1, Instruction::binop(Opcode::Add, next, p, k, ptr, 64));
        let mut back = Instruction::phi_source(from_loop, next, ptr, 64);
        back.phi_list = vec![(body, p)];
        fx.push(1, back);
        fx.max_min(p)
    }

    /// A pointer stepped forward through a loop has at most what it started
    /// with and at least nothing; stepped backward, its maximum has no
    /// bound and its minimum is where it started; not stepped at all, it is
    /// where it started.
    #[test]
    fn a_pointer_stepped_through_a_loop() {
        assert_eq!(loop_answers(1), (Some(16), Some(0)));
        assert_eq!(loop_answers(-1), (None, Some(16)));
        assert_eq!(loop_answers(0), (Some(16), Some(16)));
    }

    /// A weak global, and one whose type ends in storage with no bound, may
    /// be larger than this unit says: neither has a size.
    #[test]
    fn named_sizes_skip_weak_and_unbounded_objects() {
        use crate::ir::{GlobalDef, Initializer};
        let mut types = TypeTable::new(&Target::new(
            crate::target::Arch::X86_64,
            crate::target::Os::Linux,
        ));
        let arr = types.intern(Type::array(types.char_id, 8));
        let open = types.intern(Type::array_of(
            types.char_id,
            crate::types::ArrayExtent::Unknown,
        ));
        let mut module = Module::default();
        module
            .globals
            .push(GlobalDef::new("plain", arr, Initializer::Int(0)));
        let mut weak = GlobalDef::new("weak", arr, Initializer::Int(0));
        weak.symbol_attrs.weak = true;
        module.globals.push(weak);
        module
            .globals
            .push(GlobalDef::new("open", open, Initializer::Int(0)));
        module.strings.push(("lit".to_string(), "abc".to_string()));
        let sizes = ObjectSizes::build(&module, &types);
        assert_eq!(sizes.named("plain"), Some(8));
        assert_eq!(sizes.named("weak"), None);
        assert_eq!(sizes.named("open"), None);
        assert_eq!(sizes.named("lit"), Some(4));
    }
}
