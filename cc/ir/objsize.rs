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
// `__builtin_dynamic_object_size` is answered by the same walk, which for it
// also sees sizes known only at run time (`ObjectSizeForm::Dynamic`): an
// object whose size is an argument of the call that returned it -- `malloc(n)`,
// `calloc(n, m)`, `realloc(p, n)`, a function declared `alloc_size` --
// `strdup(s)`, whose size is `strlen(s) + 1`, called just before it, a
// `strndup`, an `alloca` or VLA of a run-time size, a parameter that
// `access(mode, ref, size)` gives a size, and a run-time displacement into
// any object, a fixed-size one included. Its answer is computed where the
// builtin is, from values that are all defined before it: what is left past
// the displacement, or nothing when the displacement is past the end or
// before the start (sizes saturate, as in gcc); a `Select` of the arms'
// answers for a `Select`; the largest or smallest of a phi's arms. A size
// built from a value that does not dominate the builtin -- one defined on a
// phi's incoming edge only -- is unknown, and so is one a phi's arm reaches
// that is not defined before the phi's block: round a loop it holds this
// time's value, the arm the last time's. A run-time answer is given only once
// the optimizer has converged, and the passes then fold what they can of it.
//
// gcc answers it at `-O0` too, for an object of a fixed size at a run-time
// displacement, and for nothing else (`Scope::Unoptimized`).
//

use super::build::Builder;
use super::dominate::{domtree_build, DomTree};
use super::libcall_fold::{self, FoldCtx};
use super::memloc::AddrMap;
use super::propagate::fold_target_to_const;
use super::{Function, Instruction, Module, Opcode, PseudoId, PseudoKind, Site};
use crate::parse::ast::{AllocSize, LibFn, ObjectSizeForm, ObjectSizeType};
use crate::token::lexer::payload_bytes;
use crate::types::{TypeId, TypeKind, TypeTable};
use crate::units::Bits;
use std::cell::OnceCell;
use std::collections::HashMap;

/// How many definitions one answer follows before it gives up.
const MAX_WALK: usize = 64;

/// The size of every named object of a module whose size is known: what a
/// pointer to its start may reach. And how big an object each function
/// declared `alloc_size` returns.
pub struct ObjectSizes {
    named: HashMap<String, u64>,
    allocators: HashMap<String, AllocSize>,
}

impl ObjectSizes {
    /// The sizes of `module`'s objects: each global and static of a complete
    /// type, and each string literal; and the `alloc_size` of each function
    /// defined or declared with one.
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
        let defined = module.functions.iter().map(|f| (&f.name, &f.symbol_attrs));
        let allocators = defined
            .chain(&module.declared_symbol_attrs)
            .filter_map(|(name, attrs)| Some((name.clone(), attrs.alloc_size?)))
            .collect();
        ObjectSizes { named, allocators }
    }

    /// The size of the named object `name`, when it is listed.
    fn named(&self, name: &str) -> Option<u64> {
        self.named.get(name).copied()
    }
}

/// Which `ObjectSize` placeholders a run answers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Settle {
    /// Only those whose object is known, with a constant: an unknown one
    /// may become known on the optimizer's next round.
    Known,
    /// Every one, the unknown ones with [`ObjectSizeType::unknown`], and the
    /// dynamic form's run-time sizes: the optimizer has converged, and
    /// nothing more will be learnt.
    Everything,
}

/// Which run-time sizes a run may answer the dynamic form with.
#[derive(Clone, Copy)]
pub enum Scope<'a> {
    /// Not optimizing: only an object of a fixed size at a run-time
    /// displacement, which gcc answers at `-O0`.
    Unoptimized,
    /// Optimizing: every size the walk finds. A `strdup`'s is computed by
    /// a call to `strlen`, made through `fold`.
    Optimizing(&'a FoldCtx<'a>),
}

/// A run-time integer a size is computed from, and its type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Operand {
    value: PseudoId,
    typ: TypeId,
}

/// How many bytes an object has from its start.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Extent {
    Bytes(u64),
    /// `count` elements of `elem` bytes.
    Elements {
        count: Operand,
        elem: u64,
    },
    /// The product of two run-time values: `calloc(n, m)`.
    Product(Operand, Operand),
    /// `strdup(s)`, whose result `call` is: `strlen(s) + 1`, where `strlen`
    /// is called just before it.
    Duplicate {
        call: PseudoId,
        s: Operand,
    },
    /// `strndup(s, n)`'s most: `n + 1`, unless that wraps.
    OneMore(Operand),
}

/// A displacement from an object's start: a constant, and the run-time
/// values added to or subtracted from it.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Offset {
    bytes: i64,
    run: Vec<Term>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Term {
    Plus(PseudoId),
    Minus(PseudoId),
}

/// How many bytes are left past a pointer.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Size {
    Bytes(u64),
    /// What is left of an object of the extent past the offset, one of them
    /// known only at run time.
    Left(Extent, Offset),
    /// `cond ? a : b`, as the pointer was chosen.
    Choice(PseudoId, Box<Size>, Box<Size>),
    /// The largest of the arms, or for a minimum the smallest.
    Join(Vec<Size>),
}

impl Size {
    /// What is left of an object of `extent` bytes past `off`: a constant
    /// when both are.
    fn left(extent: Extent, off: Offset) -> Size {
        match extent {
            Extent::Bytes(n) if off.run.is_empty() => Size::Bytes(remaining(n, off.bytes)),
            extent => Size::Left(extent, off),
        }
    }

    /// The run-time values the answer is computed from, each of which has
    /// to be defined before the placeholder -- but the first `skip`
    /// run-time terms of each displacement, which a phi's arms were walked
    /// past.
    fn leaves(&self, skip: usize, out: &mut Vec<PseudoId>) {
        match self {
            Size::Bytes(_) => {}
            Size::Left(extent, off) => {
                match extent {
                    Extent::Bytes(_) => {}
                    Extent::Elements { count, .. } | Extent::OneMore(count) => {
                        out.push(count.value)
                    }
                    Extent::Product(a, b) => out.extend([a.value, b.value]),
                    // `strlen` is called where the `strdup` is.
                    Extent::Duplicate { call, .. } => out.push(*call),
                }
                let terms = off.run.iter().skip(skip);
                out.extend(terms.map(|&(Term::Plus(v) | Term::Minus(v))| v));
            }
            Size::Choice(cond, a, b) => {
                out.push(*cond);
                a.leaves(skip, out);
                b.leaves(skip, out);
            }
            Size::Join(arms) => arms.iter().for_each(|a| a.leaves(skip, out)),
        }
    }
}

/// What one arm of a walk reached.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Reach {
    /// An object with this much left past the pointer.
    Size(Size),
    /// Something the walk cannot see into.
    Unknown,
    /// Nothing: the arm comes back to a phi already being walked, and can
    /// only carry what the other arms bring.
    Cycle,
}

/// What a placeholder is answered with.
enum Answer {
    Constant(u64),
    /// The dynamic form's size, computed where the placeholder is.
    Computed(Size),
}

/// The IR constant for a size: a 64-bit `size_t` is kept sign-extended, as
/// every 64-bit constant is, so `(size_t)-1` is -1.
pub(crate) fn size_constant(size: u64) -> i128 {
    i128::from(size as i64)
}

/// Answer the `ObjectSize` placeholders in `func` that `settle` says to.
/// Answers whether anything changed.
pub fn run(
    func: &mut Function,
    types: &TypeTable,
    sizes: &ObjectSizes,
    scope: Scope,
    settle: Settle,
) -> bool {
    let answers = answers(func, types, sizes, scope, settle);
    let mut minted = HashMap::new();
    let mut changed = false;
    let mut computed = Vec::new();
    for (site, answer) in answers {
        match answer {
            Answer::Constant(size) => {
                changed |= fold_target_to_const(func, site, size_constant(size), &mut minted);
            }
            Answer::Computed(size) => computed.push((site, size)),
        }
    }
    if !computed.is_empty() {
        let fold = match scope {
            Scope::Optimizing(fold) => Some(fold),
            Scope::Unoptimized => None,
        };
        Computer::new(func, types, fold).run(computed);
        changed = true;
    }
    changed
}

/// Each placeholder `settle` says to answer, and its answer.
fn answers(
    func: &Function,
    types: &TypeTable,
    sizes: &ObjectSizes,
    scope: Scope,
    settle: Settle,
) -> Vec<(Site, Answer)> {
    let mut found = Vec::new();
    let mut am = None;
    let dom = OnceCell::new();
    for (b, bb) in func.blocks.iter().enumerate() {
        for (i, insn) in bb.insns.iter().enumerate() {
            let Opcode::ObjectSize(query) = insn.op else {
                continue;
            };
            let Some(&ptr) = insn.src.first() else {
                continue;
            };
            let am = am.get_or_insert_with(|| AddrMap::build(func));
            let ctx = WalkCtx {
                func,
                types,
                am,
                sizes,
                scope,
                dom: &dom,
            };
            let mut reach = Walker::new(&ctx, query.otype(), query.form()).reach(ptr);
            if let Reach::Size(size) = &reach {
                if !ctx.computable_at(size, (b, i)) {
                    reach = Reach::Unknown;
                }
            }
            if let Some(answer) = answer(query.otype(), query.form(), reach, settle) {
                found.push(((b, i), answer));
            }
        }
    }
    found
}

/// What a placeholder of type `otype` whose pointer reached `reach` answers,
/// if `settle` says to answer it now.
fn answer(
    otype: ObjectSizeType,
    form: ObjectSizeForm,
    reach: Reach,
    settle: Settle,
) -> Option<Answer> {
    let Reach::Size(size) = reach else {
        return (settle == Settle::Everything).then(|| Answer::Constant(otype.unknown()));
    };
    // The whole object is no minimum for a subobject: see the header. But
    // the start of an object the program allocated at run time is no
    // subobject address the IR could have lost.
    let allocation_start = matches!(&size, Size::Left(_, off) if *off == Offset::default());
    let whole_object_answers = !(otype.subobject && otype.minimum) || allocation_start;
    match size {
        Size::Bytes(n) if whole_object_answers => Some(Answer::Constant(n)),
        size if whole_object_answers
            && form == ObjectSizeForm::Dynamic
            && settle == Settle::Everything =>
        {
            Some(Answer::Computed(size))
        }
        _ => (settle == Settle::Everything).then(|| Answer::Constant(otype.unknown())),
    }
}

/// What every walk in a function shares.
struct WalkCtx<'a, 's> {
    func: &'a Function,
    types: &'a TypeTable,
    am: &'a AddrMap,
    sizes: &'a ObjectSizes,
    scope: Scope<'s>,
    /// The function's dominators, built when a run-time size asks.
    dom: &'a OnceCell<DomTree>,
}

impl WalkCtx<'_, '_> {
    /// Whether every value `size` is computed from is defined before the
    /// placeholder at `site`, and, unoptimized, whether it is what gcc
    /// answers there: one object of a fixed size.
    fn computable_at(&self, size: &Size, (b, i): Site) -> bool {
        match (self.scope, size) {
            (_, Size::Bytes(_)) => return true,
            (Scope::Unoptimized, Size::Choice(..) | Size::Join(..)) => return false,
            _ => {}
        }
        let mut leaves = Vec::new();
        size.leaves(0, &mut leaves);
        leaves.iter().all(|&v| match self.am.def_site(v) {
            Some((db, di)) if db == b => di < i,
            Some((db, _)) => self.dominates(db, b),
            None => self.held_from_entry(v),
        })
    }

    /// Whether every value the arms of a phi in block `b` reach -- `size`,
    /// but the first `skip` run-time terms of each displacement, added past
    /// the phi -- is defined before the block. One defined in it or after
    /// it holds the value of the latest time round a loop, while the arm
    /// that reached it may be of the time before.
    fn defined_before_block(&self, size: &Size, skip: usize, b: usize) -> bool {
        let mut leaves = Vec::new();
        size.leaves(skip, &mut leaves);
        leaves.iter().all(|&v| match self.am.def_site(v) {
            Some((db, _)) => db != b && self.dominates(db, b),
            None => self.held_from_entry(v),
        })
    }

    /// Whether block `a` dominates block `b`, by their indices.
    fn dominates(&self, a: usize, b: usize) -> bool {
        let dom = self.dom.get_or_init(|| domtree_build(self.func));
        dom.dominates(self.func.blocks[a].id, self.func.blocks[b].id)
    }

    /// Whether `v`, which no instruction defines, holds its value from the
    /// function's start: an argument or a constant.
    fn held_from_entry(&self, v: PseudoId) -> bool {
        self.func.const_val(v).is_some()
            || matches!(
                self.func.get_pseudo(v).map(|p| &p.kind),
                Some(PseudoKind::Arg(_))
            )
    }
}

/// One walk from a pointer back to the objects it may point into.
struct Walker<'w, 'a, 's> {
    ctx: &'w WalkCtx<'a, 's>,
    /// Whether the arms' answers are joined by their minimum.
    minimum: bool,
    form: ObjectSizeForm,
    /// The width of an address: what pointer arithmetic is done at.
    ptr_bits: Bits,
    /// Definitions left to follow.
    budget: usize,
    /// The phis being walked, and the displacement each was entered at.
    open: HashMap<PseudoId, Offset>,
}

impl<'w, 'a, 's> Walker<'w, 'a, 's> {
    fn new(ctx: &'w WalkCtx<'a, 's>, otype: ObjectSizeType, form: ObjectSizeForm) -> Self {
        Walker {
            ctx,
            minimum: otype.minimum,
            form,
            ptr_bits: ctx.types.value_bits(ctx.types.void_ptr_id),
            budget: MAX_WALK,
            open: HashMap::new(),
        }
    }

    /// What `ptr` reaches, all arms joined.
    fn reach(mut self, ptr: PseudoId) -> Reach {
        match self.walk(ptr, Offset::default()) {
            Reach::Cycle => Reach::Unknown,
            reach => reach,
        }
    }

    /// What `p` advanced `off` bytes reaches.
    fn walk(&mut self, p: PseudoId, off: Offset) -> Reach {
        let Some(budget) = self.budget.checked_sub(1) else {
            return Reach::Unknown;
        };
        self.budget = budget;
        if let Some(size) = self.object_size(p) {
            return Reach::Size(Size::left(Extent::Bytes(size), off));
        }
        let func = self.ctx.func;
        let Some(d) = self.ctx.am.def(func, p) else {
            return self.parameter(p, off);
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
                    (None, None) => self.step_run(a, b, off),
                }
            }
            (Opcode::Sub, &[a, b]) if d.size == self.ptr_bits => {
                match self.displacement(b).and_then(i64::checked_neg) {
                    Some(k) => self.step(a, off, k),
                    None if self.form == ObjectSizeForm::Dynamic => {
                        let mut off = off;
                        off.run.push(Term::Minus(b));
                        self.walk(a, off)
                    }
                    None => Reach::Unknown,
                }
            }
            (Opcode::Select, &[c, a, b]) => self.choose(c, a, b, off),
            (Opcode::Phi, _) => self.phi(p, off),
            (Opcode::Alloca, &[n]) => {
                let n = Operand {
                    value: n,
                    typ: self.ctx.types.ulong_id,
                };
                self.allocated(self.elements(n, 1), off)
            }
            (Opcode::Call, _) => self.call(d, off),
            _ => Reach::Unknown,
        }
    }

    /// The size of the object `p` is the storage of: a local, or a named
    /// object whose size is known.
    fn object_size(&self, p: PseudoId) -> Option<u64> {
        let func = self.ctx.func;
        let PseudoKind::Sym(name) = &func.get_pseudo(p)?.kind else {
            return None;
        };
        let size = match func.local_of(p) {
            Some(local) => self.ctx.types.size_bytes(local.typ) as u64,
            None => self.ctx.sizes.named(name)?,
        };
        (size > 0).then_some(size)
    }

    /// The constant `p` adds to an address, if it is one.
    fn displacement(&self, p: PseudoId) -> Option<i64> {
        i64::try_from(self.ctx.am.const_operand(p, self.ptr_bits.get())?).ok()
    }

    /// What `p` reaches, `k` bytes further on than `off`.
    fn step(&mut self, p: PseudoId, off: Offset, k: i64) -> Reach {
        match off.bytes.checked_add(k) {
            Some(bytes) => self.walk(p, Offset { bytes, ..off }),
            None => Reach::Unknown,
        }
    }

    /// What `a + b` reaches, neither a constant, for the dynamic form: one
    /// is the pointer and the other a displacement known at run time. The
    /// IR does not say which, so `a` is taken for the pointer unless it
    /// reaches nothing.
    fn step_run(&mut self, a: PseudoId, b: PseudoId, off: Offset) -> Reach {
        if self.form == ObjectSizeForm::Static {
            return Reach::Unknown;
        }
        let mut past_b = off.clone();
        past_b.run.push(Term::Plus(b));
        match self.walk(a, past_b) {
            Reach::Unknown => {
                let mut past_a = off;
                past_a.run.push(Term::Plus(a));
                self.walk(b, past_a)
            }
            reach => reach,
        }
    }

    /// What the phi `p` reaches, advanced `off` bytes: its arms joined, or,
    /// met again on its own cycle, what the step around the cycle allows.
    fn phi(&mut self, p: PseudoId, off: Offset) -> Reach {
        if let Some(entered) = self.open.get(&p) {
            // A run-time step around the cycle may go either way.
            if entered.run != off.run {
                return Reach::Unknown;
            }
            return match (off.bytes.cmp(&entered.bytes), self.minimum) {
                (std::cmp::Ordering::Equal, _) => Reach::Cycle,
                // Forward: fewer bytes each time round.
                (std::cmp::Ordering::Greater, false) => Reach::Cycle,
                (std::cmp::Ordering::Greater, true) => Reach::Size(Size::Bytes(0)),
                // Backward: more bytes each time round.
                (std::cmp::Ordering::Less, false) => Reach::Unknown,
                (std::cmp::Ordering::Less, true) => Reach::Cycle,
            };
        }
        let (Some(d), Some((block, _))) =
            (self.ctx.am.def(self.ctx.func, p), self.ctx.am.def_site(p))
        else {
            return Reach::Unknown;
        };
        let arms: Vec<PseudoId> = d.phi_list.iter().map(|&(_, v)| v).collect();
        let past_the_phi = off.run.len();
        self.open.insert(p, off.clone());
        let joined = self.join(&arms, off);
        self.open.remove(&p);
        match joined {
            Reach::Size(size) if !self.ctx.defined_before_block(&size, past_the_phi, block) => {
                Reach::Unknown
            }
            joined => joined,
        }
    }

    /// The arms' answers, each advanced `off` bytes, joined: an unknown arm
    /// makes the answer unknown, and an arm that only cycles adds nothing.
    fn join(&mut self, arms: &[PseudoId], off: Offset) -> Reach {
        let mut acc = Reach::Cycle;
        for &arm in arms {
            acc = match (acc, self.walk(arm, off.clone())) {
                (Reach::Unknown, _) | (_, Reach::Unknown) => return Reach::Unknown,
                (Reach::Cycle, r) | (r, Reach::Cycle) => r,
                (Reach::Size(a), Reach::Size(b)) => Reach::Size(self.joined(a, b)),
            };
        }
        acc
    }

    /// The larger of two answers, or for a minimum the smaller.
    fn joined(&self, a: Size, b: Size) -> Size {
        match (a, b) {
            (Size::Bytes(a), Size::Bytes(b)) if self.minimum => Size::Bytes(a.min(b)),
            (Size::Bytes(a), Size::Bytes(b)) => Size::Bytes(a.max(b)),
            (Size::Join(mut arms), b) => {
                arms.push(b);
                Size::Join(arms)
            }
            (a, b) => Size::Join(vec![a, b]),
        }
    }

    /// What `cond ? a : b` reaches: for the dynamic form, the answer of the
    /// arm chosen, where either is known only at run time; otherwise the
    /// arms joined.
    fn choose(&mut self, cond: PseudoId, a: PseudoId, b: PseudoId, off: Offset) -> Reach {
        let reach_a = self.walk(a, off.clone());
        match (reach_a, self.walk(b, off)) {
            (Reach::Unknown, _) | (_, Reach::Unknown) => Reach::Unknown,
            (Reach::Cycle, r) | (r, Reach::Cycle) => r,
            (Reach::Size(a @ Size::Bytes(_)), Reach::Size(b @ Size::Bytes(_))) => {
                Reach::Size(self.joined(a, b))
            }
            (Reach::Size(a), Reach::Size(b)) => {
                Reach::Size(Size::Choice(cond, Box::new(a), Box::new(b)))
            }
        }
    }

    /// `count` elements of `elem` bytes: a constant when `count` is.
    fn elements(&self, count: Operand, elem: u64) -> Option<Extent> {
        match self
            .ctx
            .am
            .const_operand(count.value, self.ctx.types.value_bits(count.typ).get())
        {
            Some(n) => u64::try_from(n).ok()?.checked_mul(elem).map(Extent::Bytes),
            None => Some(Extent::Elements { count, elem }),
        }
    }

    /// What is left past `off` of an object the program allocated, of
    /// `extent` bytes when that is known: a constant at any time, a
    /// run-time size only for the dynamic form, optimizing.
    fn allocated(&self, extent: Option<Extent>, off: Offset) -> Reach {
        match extent {
            Some(extent @ Extent::Bytes(_)) => Reach::Size(Size::left(extent, off)),
            Some(extent) if self.run_time_sizes() => Reach::Size(Size::left(extent, off)),
            _ => Reach::Unknown,
        }
    }

    /// Whether this walk may answer with the size of an object only known
    /// at run time.
    fn run_time_sizes(&self) -> bool {
        self.form == ObjectSizeForm::Dynamic && matches!(self.ctx.scope, Scope::Optimizing(_))
    }

    /// What the result of the call `d` reaches, advanced `off` bytes: an
    /// allocation function's object, for the dynamic form.
    fn call(&self, d: &Instruction, off: Offset) -> Reach {
        let extra = d.extra();
        if !self.run_time_sizes() || extra.indirect_target.is_some() {
            return Reach::Unknown;
        }
        let operand = |i: usize| {
            let (&value, &typ) = (d.src.get(i)?, extra.arg_types.get(i)?);
            self.ctx
                .types
                .is_integer(typ)
                .then_some(Operand { value, typ })
        };
        let extent = match extra.known {
            Some(LibFn::Strdup) => self.duplicate(d),
            Some(LibFn::Strndup) if self.minimum => Some(Extent::Bytes(1)),
            Some(LibFn::Strndup) => operand(1).map(Extent::OneMore),
            known => {
                let alloc = known.and_then(LibFn::alloc_size).or_else(|| {
                    let name = extra.func_name.as_deref()?;
                    self.ctx.sizes.allocators.get(name).copied()
                });
                alloc.and_then(|a| match a.count {
                    None => self.elements(operand(a.size)?, 1),
                    Some(count) => self.product(operand(a.size)?, operand(count)?),
                })
            }
        };
        self.allocated(extent, off)
    }

    /// The product of two run-time values: a constant when both are.
    fn product(&self, a: Operand, b: Operand) -> Option<Extent> {
        match self.elements(b, 1)? {
            Extent::Bytes(n) => self.elements(a, n),
            _ => Some(Extent::Product(a, b)),
        }
    }

    /// `strdup(s)`'s extent, when `strlen` can be called to compute it.
    fn duplicate(&self, d: &Instruction) -> Option<Extent> {
        let Scope::Optimizing(fold) = self.ctx.scope else {
            return None;
        };
        if !libcall_fold::can_call(fold, self.ctx.func, LibFn::Strlen) {
            return None;
        }
        Some(Extent::Duplicate {
            call: d.target?,
            s: Operand {
                value: *d.src.first()?,
                typ: *d.extra().arg_types.first()?,
            },
        })
    }

    /// What the parameter `p` reaches, advanced `off` bytes, when the
    /// function's `access` attribute gives the object it points to a size.
    fn parameter(&self, p: PseudoId, off: Offset) -> Reach {
        let func = self.ctx.func;
        let Some(PseudoKind::Arg(arg)) = func.get_pseudo(p).map(|p| &p.kind) else {
            return Reach::Unknown;
        };
        let args = func.arg_types();
        let Some(param) = (0..func.params.len()).find(|&i| args.arg_of_param(i) == *arg) else {
            return Reach::Unknown;
        };
        let Some(access) = func.symbol_attrs.access.iter().find(|a| a.ptr == param) else {
            return Reach::Unknown;
        };
        let types = self.ctx.types;
        let elem = types
            .base_type(func.params[param].1)
            .map(|t| match types.size_bytes(t) {
                // `void`: bytes.
                0 if types.kind(t) == TypeKind::Void => 1,
                n => n as u64,
            })
            .filter(|&n| n > 0);
        let count = func.params.get(access.size).and_then(|&(_, typ)| {
            let value = func
                .arg_pseudos()
                .get(&args.arg_of_param(access.size))
                .map(|p| p.id)?;
            types.is_integer(typ).then_some(Operand { value, typ })
        });
        match (elem, count) {
            (Some(elem), Some(count)) => self.allocated(self.elements(count, elem), off),
            _ => Reach::Unknown,
        }
    }
}

/// What is left of an object of `size` bytes past `off`: nothing outside it.
fn remaining(size: u64, off: i64) -> u64 {
    u64::try_from(off).map_or(0, |off| size.saturating_sub(off))
}

/// The instructions that compute the dynamic form's run-time answers.
struct Computer<'a, 's> {
    func: &'a mut Function,
    types: &'a TypeTable,
    /// How a `strdup`'s size is computed; `None` unoptimized, where no
    /// answer needs a call.
    fold: Option<&'s FoldCtx<'s>>,
    /// The `strlen` called before each `strdup` an answer reads, by the
    /// `strdup`'s result.
    lengths: HashMap<PseudoId, PseudoId>,
    /// The instructions to insert before an instruction.
    inserts: Vec<(Site, Vec<Instruction>)>,
}

impl<'a, 's> Computer<'a, 's> {
    fn new(func: &'a mut Function, types: &'a TypeTable, fold: Option<&'s FoldCtx<'s>>) -> Self {
        Computer {
            func,
            types,
            fold,
            lengths: HashMap::new(),
            inserts: Vec::new(),
        }
    }

    /// Answer each placeholder with its size, computed just before it.
    fn run(mut self, answers: Vec<(Site, Size)>) {
        let am = AddrMap::build(self.func);
        for ((b, i), size) in answers {
            let Opcode::ObjectSize(query) = self.func.blocks[b].insns[i].op else {
                continue;
            };
            let pos = self.func.blocks[b].insns[i].pos;
            let mut out = Vec::new();
            let result = self.size(&am, &mut out, pos, query.otype().minimum, &size);
            let insn = &mut self.func.blocks[b].insns[i];
            insn.op = Opcode::Copy;
            insn.src = vec![result];
            self.inserts.push(((b, i), out));
        }
        // From the last, so that the sites still to come keep their index.
        self.inserts
            .sort_by_key(|&(site, _)| std::cmp::Reverse(site));
        for ((b, i), insns) in self.inserts {
            self.func.blocks[b].insns.splice(i..i, insns);
        }
    }

    /// The instructions computing `size`, appended to `out`; answers the
    /// pseudo holding it.
    fn size(
        &mut self,
        am: &AddrMap,
        out: &mut Vec<Instruction>,
        pos: Option<crate::token::lexer::Position>,
        minimum: bool,
        size: &Size,
    ) -> PseudoId {
        let size_t = self.types.ulong_id;
        match size {
            Size::Bytes(n) => {
                Builder::new(self.func, self.types, pos, out).constant(size_constant(*n), size_t)
            }
            Size::Left(extent, off) => {
                let extent = self.extent(am, out, pos, extent);
                if *off == Offset::default() {
                    return extent;
                }
                let mut b = Builder::new(self.func, self.types, pos, out);
                let at = offset(&mut b, off);
                // Past the end, or before the start, wraps to above it.
                let fits = b.compare(Opcode::SetBe, at, extent, size_t);
                let rest = b.binop(Opcode::Sub, extent, at, size_t);
                let none = b.constant(0, size_t);
                b.select(fits, rest, none, size_t)
            }
            Size::Choice(cond, x, y) => {
                let x = self.size(am, out, pos, minimum, x);
                let y = self.size(am, out, pos, minimum, y);
                Builder::new(self.func, self.types, pos, out).select(*cond, x, y, size_t)
            }
            Size::Join(arms) => {
                let mut acc = None;
                for arm in arms {
                    let v = self.size(am, out, pos, minimum, arm);
                    acc = Some(match acc {
                        None => v,
                        Some(acc) => {
                            let mut b = Builder::new(self.func, self.types, pos, out);
                            let better = if minimum { Opcode::SetB } else { Opcode::SetA };
                            let take = b.compare(better, v, acc, size_t);
                            b.select(take, v, acc, size_t)
                        }
                    });
                }
                acc.expect("a join has arms")
            }
        }
    }

    /// The instructions computing `extent`, appended to `out`.
    fn extent(
        &mut self,
        am: &AddrMap,
        out: &mut Vec<Instruction>,
        pos: Option<crate::token::lexer::Position>,
        extent: &Extent,
    ) -> PseudoId {
        let size_t = self.types.ulong_id;
        if let Extent::Duplicate { call, s } = extent {
            let len = self.length_before(am, *call, *s);
            let mut b = Builder::new(self.func, self.types, pos, out);
            let one = b.constant(1, size_t);
            return b.binop(Opcode::Add, len, one, size_t);
        }
        let mut b = Builder::new(self.func, self.types, pos, out);
        match *extent {
            Extent::Bytes(n) => b.constant(size_constant(n), size_t),
            Extent::Elements { count, elem } => {
                let count = widened(&mut b, count);
                if elem == 1 {
                    return count;
                }
                let elem = b.constant(size_constant(elem), size_t);
                b.binop(Opcode::Mul, count, elem, size_t)
            }
            Extent::Product(x, y) => {
                let x = widened(&mut b, x);
                let y = widened(&mut b, y);
                b.binop(Opcode::Mul, x, y, size_t)
            }
            Extent::OneMore(n) => {
                let n = widened(&mut b, n);
                let most = b.constant(-1, size_t);
                let below = b.compare(Opcode::SetB, n, most, size_t);
                let one = b.constant(1, size_t);
                let more = b.binop(Opcode::Add, n, one, size_t);
                b.select(below, more, most, size_t)
            }
            Extent::Duplicate { .. } => unreachable!("computed above"),
        }
    }

    /// `strlen(s)`, called just before the `strdup(s)` whose result is
    /// `call`: what it copied.
    fn length_before(&mut self, am: &AddrMap, call: PseudoId, s: Operand) -> PseudoId {
        if let Some(&len) = self.lengths.get(&call) {
            return len;
        }
        let fold = self.fold.expect("only an optimizing walk reaches a strdup");
        let site = am.def_site(call).expect("the walk reached the call");
        let pos = self.func.blocks[site.0].insns[site.1].pos;
        let mut out = Vec::new();
        let mut b = Builder::new(self.func, self.types, pos, &mut out);
        let len = libcall_fold::call_library(
            &mut b,
            fold,
            LibFn::Strlen,
            vec![libcall_fold::Operand::Value(s.value, s.typ)],
        );
        self.inserts.push((site, out));
        self.lengths.insert(call, len);
        len
    }
}

/// `off` as a `size_t`, its run-time terms added in.
fn offset(b: &mut Builder, off: &Offset) -> PseudoId {
    let size_t = b.types.ulong_id;
    let mut at = b.constant(i128::from(off.bytes), size_t);
    for &term in &off.run {
        at = match term {
            Term::Plus(v) => b.binop(Opcode::Add, at, v, size_t),
            Term::Minus(v) => b.binop(Opcode::Sub, at, v, size_t),
        };
    }
    at
}

/// `v` as a `size_t`, extended as its type says if it is narrower.
fn widened(b: &mut Builder, v: Operand) -> PseudoId {
    let size_t = b.types.ulong_id;
    if b.types.value_bits(v.typ) >= b.types.value_bits(size_t) {
        return v.value;
    }
    let op = if b.types.is_unsigned(v.typ) {
        Opcode::Zext
    } else {
        Opcode::Sext
    };
    b.convert(op, v.value, v.typ, size_t)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, ObjectSizeQuery, Pseudo};
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
                    allocators: HashMap::new(),
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
            self.push(0, Instruction::binop(op, t, a, b, ptr, Bits::new(64)));
            t
        }

        fn select(&mut self, a: PseudoId, b: PseudoId) -> PseudoId {
            let (c, t) = (self.unknown(), self.fresh());
            let ptr = self.ptr();
            self.push(0, Instruction::select(t, c, a, b, ptr, Bits::new(64)));
            t
        }

        /// `ObjectSize(p)` of `otype`; returns its target.
        fn object_size(&mut self, p: PseudoId, otype: u64) -> PseudoId {
            let t = self.fresh();
            let ulong = self.types.ulong_id;
            let otype = ObjectSizeType::from_bits(otype);
            self.push(
                0,
                Instruction::new(Opcode::ObjectSize(ObjectSizeQuery::new(
                    otype,
                    ObjectSizeForm::Static,
                )))
                .with_target(t)
                .with_src(p)
                .with_type_and_size(ulong, Bits::new(64)),
            );
            t
        }

        /// `__builtin_dynamic_object_size(p)` of `otype`; returns its target.
        fn dynamic(&mut self, p: PseudoId, otype: u64) -> PseudoId {
            let t = self.fresh();
            let ulong = self.types.ulong_id;
            let otype = ObjectSizeType::from_bits(otype);
            self.push(
                0,
                Instruction::new(Opcode::ObjectSize(ObjectSizeQuery::new(
                    otype,
                    ObjectSizeForm::Dynamic,
                )))
                .with_target(t)
                .with_src(p)
                .with_type_and_size(ulong, Bits::new(64)),
            );
            t
        }

        /// A call to `name`, known to be the library's `known`, returning a
        /// pointer; its target.
        fn call(&mut self, name: &str, known: Option<LibFn>, args: &[PseudoId]) -> PseudoId {
            let t = self.fresh();
            let (ptr, ulong) = (self.ptr(), self.types.ulong_id);
            let mut insn = Instruction::call(
                Some(t),
                name,
                args.to_vec(),
                vec![ulong; args.len()],
                ptr,
                Bits::new(64),
            );
            insn.extra_mut().known = known;
            self.push(0, insn);
            t
        }

        /// Run the pass as the optimizer runs it, or as an unoptimized
        /// compile does.
        fn run(&mut self, settle: Settle, optimizing: bool) {
            if !optimizing {
                run(
                    &mut self.func,
                    &self.types,
                    &self.sizes,
                    Scope::Unoptimized,
                    settle,
                );
                return;
            }
            // A fold context over an empty module, whose library functions
            // are called by their own names.
            let types = &self.types;
            let mut module = Module::default();
            let bytes = crate::ir::strdata::ConstBytes::build(&module, types);
            let mi = crate::ir::memloc::ModuleInfo::build(&module, types);
            let callees: HashMap<&'static str, String> = crate::ir::FOLD_CALLEES
                .iter()
                .map(|&c| (c, c.to_string()))
                .collect();
            let target = Target::new(crate::target::Arch::X86_64, crate::target::Os::Linux);
            let (_, strings, _) = module.split_for_rewrite();
            let literals = crate::ir::libcall_fold::NewLiterals::new(strings);
            let fold = FoldCtx {
                types,
                target: &target,
                mi: &mi,
                bytes: &bytes,
                callees: &callees,
                literals: &literals,
            };
            let scope = Scope::Optimizing(&fold);
            run(&mut self.func, types, &self.sizes, scope, settle);
        }

        /// The value `p` holds once the function has run with each value
        /// in `env`: the instructions defining it evaluated, as unsigned
        /// 64-bit arithmetic.
        fn value(&self, p: PseudoId, env: &HashMap<PseudoId, u64>) -> u64 {
            if let Some(&v) = env.get(&p) {
                return v;
            }
            if let Some(c) = self.func.const_val(p) {
                return c as u64;
            }
            let insn = self.def_of(p);
            let v = |i: usize| self.value(insn.src[i], env);
            match insn.op {
                Opcode::Copy => v(0),
                Opcode::Add => v(0).wrapping_add(v(1)),
                Opcode::Sub => v(0).wrapping_sub(v(1)),
                Opcode::Mul => v(0).wrapping_mul(v(1)),
                Opcode::SetB => u64::from(v(0) < v(1)),
                Opcode::SetBe => u64::from(v(0) <= v(1)),
                Opcode::SetA => u64::from(v(0) > v(1)),
                Opcode::Select if v(0) != 0 => v(1),
                Opcode::Select => v(2),
                op => panic!("cannot evaluate {op:?}"),
            }
        }

        /// What the placeholder targeting `t` computes, run with `env`, once
        /// everything is settled; `None` if it is not answered.
        fn computed(
            &mut self,
            t: PseudoId,
            optimizing: bool,
            env: &[(PseudoId, u64)],
        ) -> Option<u64> {
            self.run(Settle::Everything, optimizing);
            let answered = self.def_of(t).op == Opcode::Copy;
            answered.then(|| self.value(t, &env.iter().copied().collect()))
        }

        /// The instruction defining `t`.
        fn def_of(&self, t: PseudoId) -> &Instruction {
            self.func
                .blocks
                .iter()
                .flat_map(|b| &b.insns)
                .find(|i| i.target == Some(t))
                .unwrap_or_else(|| panic!("nothing defines {t:?}"))
        }

        /// What the placeholder targeting `t` answers under `settle`, as an
        /// unsigned size; `None` if it is left alone.
        fn answer(&mut self, t: PseudoId, settle: Settle) -> Option<u64> {
            self.run(settle, true);
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
                .with_type_and_size(ptr, Bits::new(64)),
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
        let mut src = Instruction::phi_source(from_entry, init, ptr, Bits::new(64));
        src.phi_list = vec![(body, p)];
        fx.push(0, src);
        fx.func.add_block(BasicBlock::new(body));
        let mut phi = Instruction::new(Opcode::Phi)
            .with_target(p)
            .with_type_and_size(ptr, Bits::new(64));
        phi.phi_list = vec![(entry, from_entry), (body, from_loop)];
        fx.push(1, phi);
        let k = fx.konst(step);
        let next = fx.fresh();
        fx.push(
            1,
            Instruction::binop(Opcode::Add, next, p, k, ptr, Bits::new(64)),
        );
        let mut back = Instruction::phi_source(from_loop, next, ptr, Bits::new(64));
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

    /// The dynamic form of a fixed-size object at a run-time displacement
    /// is what is left past it -- nothing past the end or before the start
    /// -- even unoptimized, as gcc answers `fixed + m` at `-O0`; the static
    /// form of it is unknown.
    #[test]
    fn a_run_time_displacement_into_a_fixed_object() {
        for m in [3, 17, 20, u64::MAX] {
            let mut fx = Fx::new();
            let fixed = fx.local("fixed", 17);
            let at = fx.unknown();
            let p = fx.binop(Opcode::Add, fixed, at);
            let (max, min) = (fx.dynamic(p, 0), fx.dynamic(p, 2));
            let left = 17u64.saturating_sub(m);
            assert_eq!(fx.computed(max, false, &[(at, m)]), Some(left), "{m}");
            assert_eq!(fx.computed(min, false, &[(at, m)]), Some(left), "{m}");
        }
        let mut fx = Fx::new();
        let fixed = fx.local("fixed", 17);
        let at = fx.unknown();
        let p = fx.binop(Opcode::Add, fixed, at);
        let k = fx.konst(4);
        let back = fx.binop(Opcode::Sub, p, k);
        let t = fx.dynamic(back, 0);
        assert_eq!(fx.computed(t, false, &[(at, 6)]), Some(15));
        let t = fx.object_size(p, 0);
        assert_eq!(fx.answer(t, Settle::Everything), Some(u64::MAX));
    }

    /// An allocation is as big as its size argument, or the product of
    /// two: gcc's own knowledge of `malloc` and `calloc`, or a function's
    /// `alloc_size`. Only the dynamic form answers, only optimizing, and
    /// only once everything is settled.
    #[test]
    fn an_allocation_is_as_big_as_its_size_argument() {
        let mut fx = Fx::new();
        let (n, m) = (fx.unknown(), fx.unknown());
        let p = fx.call("malloc", Some(LibFn::Malloc), &[n]);
        let k3 = fx.konst(3);
        let p3 = fx.binop(Opcode::Add, p, k3);
        let q = fx.call("calloc", Some(LibFn::Calloc), &[n, m]);
        let k4 = fx.konst(4);
        let q4 = fx.binop(Opcode::Add, q, k4);
        fx.sizes.allocators.insert(
            "my_alloc".to_string(),
            AllocSize {
                size: 1,
                count: Some(0),
            },
        );
        let r = fx.call("my_alloc", None, &[m, n]);
        let env = [(n, 7), (m, 3)];
        let t = fx.dynamic(p3, 0);
        assert_eq!(fx.answer(t, Settle::Known), None);
        assert_eq!(fx.computed(t, true, &env), Some(4));
        let t = fx.dynamic(q4, 2);
        assert_eq!(fx.computed(t, true, &env), Some(17));
        let t = fx.dynamic(r, 1);
        assert_eq!(fx.computed(t, true, &env), Some(21));
        let t = fx.dynamic(p3, 0);
        assert_eq!(fx.computed(t, false, &env), Some(u64::MAX));
        let t = fx.object_size(p3, 0);
        assert_eq!(fx.answer(t, Settle::Everything), Some(u64::MAX));
    }

    /// An allocation of a constant size is answered at once by the dynamic
    /// form, and stays unknown to the static one.
    #[test]
    fn a_constant_allocation_is_answered_at_once() {
        let mut fx = Fx::new();
        let n = fx.konst(10);
        let p = fx.call("malloc", Some(LibFn::Malloc), &[n]);
        let t = fx.dynamic(p, 0);
        assert_eq!(fx.answer(t, Settle::Known), Some(10));
        let t = fx.object_size(p, 0);
        assert_eq!(fx.answer(t, Settle::Everything), Some(u64::MAX));
    }

    /// A `Select` between objects of run-time and fixed sizes is a select of
    /// their sizes; an `alloca` of a run-time size is that size.
    #[test]
    fn a_choice_selects_its_arms_sizes() {
        let mut fx = Fx::new();
        let n = fx.unknown();
        let a = fx.fresh();
        let ptr = fx.ptr();
        fx.push(
            0,
            Instruction::new(Opcode::Alloca)
                .with_target(a)
                .with_src(n)
                .with_type_and_size(ptr, Bits::new(64)),
        );
        let local = fx.local("local", 20);
        let k = fx.konst(5);
        let tail = fx.binop(Opcode::Add, local, k);
        let c = fx.select(a, tail);
        let cond = fx.def_of(c).src[0];
        let t = fx.dynamic(c, 0);
        assert_eq!(fx.computed(t, true, &[(n, 9), (cond, 1)]), Some(9));
        assert_eq!(fx.value(t, &[(n, 9), (cond, 0)].into_iter().collect()), 15);
    }

    /// `strdup(s)` is `strlen(s) + 1`, with `strlen` called just before the
    /// `strdup`; unoptimized it is unknown.
    #[test]
    fn a_strdup_is_one_more_than_strlen_called_before_it() {
        let mut fx = Fx::new();
        let s = fx.unknown();
        let d = fx.call("strdup", Some(LibFn::Strdup), &[s]);
        let t = fx.dynamic(d, 0);
        fx.run(Settle::Everything, true);
        let insns = &fx.func.blocks[0].insns;
        let at = |name: &str| {
            insns
                .iter()
                .position(|i| i.extra().func_name.as_deref() == Some(name))
                .unwrap()
        };
        let (strlen, strdup) = (at("strlen"), at("strdup"));
        assert_eq!(strlen + 1, strdup);
        assert_eq!(insns[strlen].src, vec![s]);
        let len = insns[strlen].target.unwrap();
        assert_eq!(fx.value(t, &[(len, 5)].into_iter().collect()), 6);

        let mut fx = Fx::new();
        let s = fx.unknown();
        let d = fx.call("strdup", Some(LibFn::Strdup), &[s]);
        let t = fx.dynamic(d, 0);
        assert_eq!(fx.computed(t, false, &[]), Some(u64::MAX));
    }

    /// A parameter `access` gives a size points to that many elements of
    /// its pointee type; without the attribute it is unknown.
    #[test]
    fn an_access_parameter_is_its_size_parameter() {
        for attributed in [true, false] {
            let mut fx = Fx::new();
            let int_ptr = fx.types.pointer_to(fx.types.int_id);
            let ulong = fx.types.ulong_id;
            fx.func.params = vec![("p".to_string(), int_ptr), ("n".to_string(), ulong)];
            if attributed {
                fx.func.symbol_attrs.access =
                    vec![crate::parse::ast::AccessSize { ptr: 0, size: 1 }];
            }
            let (p, n) = (fx.func.alloc_pseudo(), fx.func.alloc_pseudo());
            fx.func.add_pseudo(Pseudo::arg(p, 0));
            fx.func.add_pseudo(Pseudo::arg(n, 1));
            let t = fx.dynamic(p, 0);
            let want = if attributed { 28 } else { u64::MAX };
            assert_eq!(fx.computed(t, true, &[(n, 7)]), Some(want));
        }
    }

    /// A size computed on one incoming edge of a phi does not reach the
    /// builtin after it, so the answer is unknown; one computed before the
    /// branch does, and the arms are joined.
    #[test]
    fn a_size_defined_on_one_incoming_edge_is_unknown() {
        for before_the_branch in [true, false] {
            let mut fx = Fx::new();
            let ptr = fx.ptr();
            let x = fx.unknown();
            let k = fx.konst(1);
            let ids = [1, 2, 3].map(BasicBlockId);
            for id in ids {
                fx.func.add_block(BasicBlock::new(id));
            }
            let n = fx.fresh();
            let at = if before_the_branch { 0 } else { 1 };
            let ulong = fx.types.ulong_id;
            fx.push(
                at,
                Instruction::binop(Opcode::Add, n, x, k, ulong, Bits::new(64)),
            );
            let c = fx.unknown();
            fx.push(0, Instruction::cbr(c, ids[0], ids[1]));
            let (p, from1, from2) = (fx.fresh(), fx.fresh(), fx.fresh());
            for (block, from, size) in [(1, from1, n), (2, from2, x)] {
                let a = fx.fresh();
                let call = Instruction::call(
                    Some(a),
                    "malloc",
                    vec![size],
                    vec![ulong],
                    ptr,
                    Bits::new(64),
                );
                let mut call = call;
                call.extra_mut().known = Some(LibFn::Malloc);
                fx.push(block, call);
                let mut src = Instruction::phi_source(from, a, ptr, Bits::new(64));
                src.phi_list = vec![(ids[2], p)];
                fx.push(block, src);
                fx.push(block, Instruction::br(ids[2]));
            }
            let mut phi = Instruction::new(Opcode::Phi)
                .with_target(p)
                .with_type_and_size(ptr, Bits::new(64));
            phi.phi_list = vec![(ids[0], from1), (ids[1], from2)];
            fx.push(3, phi);
            fx.func.blocks[0].children = vec![ids[0], ids[1]];
            fx.func.blocks[1].children = vec![ids[2]];
            fx.func.blocks[2].children = vec![ids[2]];
            for b in 1..3 {
                fx.func.blocks[b].parents = vec![BasicBlockId(0)];
            }
            fx.func.blocks[3].parents = vec![ids[0], ids[1]];
            let t = fx.fresh();
            fx.push(
                3,
                Instruction::new(Opcode::ObjectSize(ObjectSizeQuery::new(
                    ObjectSizeType::from_bits(0),
                    ObjectSizeForm::Dynamic,
                )))
                .with_target(t)
                .with_src(p)
                .with_type_and_size(ulong, Bits::new(64)),
            );
            let want = if before_the_branch { 8 } else { u64::MAX };
            assert_eq!(fx.computed(t, true, &[(x, 7)]), Some(want));
        }
    }

    /// `p = phi(entry: malloc(x), loop: malloc(k))` with `k` computed in the
    /// loop after the phi: `k` is defined before the builtin, but holds this
    /// time round's size while `p` holds the last time's object, so the
    /// answer is unknown. A size from before the loop is answered.
    #[test]
    fn a_size_from_a_later_time_round_a_loop_is_unknown() {
        for in_the_loop in [true, false] {
            let mut fx = Fx::new();
            let (ptr, ulong) = (fx.ptr(), fx.types.ulong_id);
            let (x, y) = (fx.unknown(), fx.unknown());
            let (entry, body, exit) = (BasicBlockId(0), BasicBlockId(1), BasicBlockId(2));
            fx.func.add_block(BasicBlock::new(body));
            fx.func.add_block(BasicBlock::new(exit));
            let first = fx.call("malloc", Some(LibFn::Malloc), &[x]);
            let (p, from_entry, from_loop) = (fx.fresh(), fx.fresh(), fx.fresh());
            let mut src = Instruction::phi_source(from_entry, first, ptr, Bits::new(64));
            src.phi_list = vec![(body, p)];
            fx.push(0, src);
            fx.push(0, Instruction::br(body));
            let mut phi = Instruction::new(Opcode::Phi)
                .with_target(p)
                .with_type_and_size(ptr, Bits::new(64));
            phi.phi_list = vec![(entry, from_entry), (body, from_loop)];
            fx.push(1, phi);
            let size = if in_the_loop {
                let (k, one) = (fx.fresh(), fx.konst(1));
                fx.push(
                    1,
                    Instruction::binop(Opcode::Add, k, y, one, ulong, Bits::new(64)),
                );
                k
            } else {
                x
            };
            let next = fx.fresh();
            let mut call = Instruction::call(
                Some(next),
                "malloc",
                vec![size],
                vec![ulong],
                ptr,
                Bits::new(64),
            );
            call.extra_mut().known = Some(LibFn::Malloc);
            fx.push(1, call);
            let t = fx.fresh();
            fx.push(
                1,
                Instruction::new(Opcode::ObjectSize(ObjectSizeQuery::new(
                    ObjectSizeType::from_bits(0),
                    ObjectSizeForm::Dynamic,
                )))
                .with_target(t)
                .with_src(p)
                .with_type_and_size(ulong, Bits::new(64)),
            );
            let mut back = Instruction::phi_source(from_loop, next, ptr, Bits::new(64));
            back.phi_list = vec![(body, p)];
            fx.push(1, back);
            let c = fx.unknown();
            fx.push(1, Instruction::cbr(c, body, exit));
            fx.func.blocks[0].children = vec![body];
            fx.func.blocks[1].children = vec![body, exit];
            fx.func.blocks[1].parents = vec![entry, body];
            fx.func.blocks[2].parents = vec![body];
            let want = if in_the_loop { u64::MAX } else { 7 };
            assert_eq!(fx.computed(t, true, &[(x, 7), (y, 9)]), Some(want));
        }
    }

    /// Type 3 of the start of an allocated object is its size; anywhere
    /// else in it, the subobject the IR has lost may be smaller.
    #[test]
    fn type_3_of_an_allocation_start() {
        let mut fx = Fx::new();
        let n = fx.unknown();
        let p = fx.call("malloc", Some(LibFn::Malloc), &[n]);
        let k = fx.konst(1);
        let p1 = fx.binop(Opcode::Add, p, k);
        let t = fx.dynamic(p, 3);
        assert_eq!(fx.computed(t, true, &[(n, 7)]), Some(7));
        let t = fx.dynamic(p1, 3);
        assert_eq!(fx.computed(t, true, &[(n, 7)]), Some(0));
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
