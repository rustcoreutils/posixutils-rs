//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// What a call does to memory, from what the programmer promised and from
// what the callee's body actually does.
//
// The lattice is `ast::MemEffect` and the join is "the dirtier of the two",
// so a function is only as clean as the dirtiest thing it does. Two rules
// decide whether this is worth anything.
//
// **A declared effect is a promise and is never lowered.** For most of what
// a program calls, the prototype is all there is -- glibc's `__pure__
// strlen` is the only thing that says `strlen` writes nothing. An attribute
// that in-TU analysis could overrule would buy nothing exactly where it is
// written.
//
// **A local the callee never let escape contributes nothing.** Without that,
// nothing is ever inferred clean: before SSA promotion every parameter and
// every local is a stack slot, and `static int f(int a) { return i + a; }`
// stores `a` to its own frame before reading it back. A write the caller
// cannot observe is not a write.
//
// **Effects refine what a call may have *written*; they never delete one.**
// An inferred-pure function that does not terminate is still pure by this
// lattice, and deleting a dead call to it would turn a hang into a
// completion. `Const` versus `Pure` is the distinction a pass that wanted to
// *merge* two calls would need, and no pass here does.
//

use super::escape::EscapeInfo;
use super::memloc::{AddrMap, Footprint};
use super::{Function, Instruction, Module, Opcode};
use crate::parse::ast::MemEffect;
use crate::types::TypeTable;
use std::collections::{HashMap, VecDeque};

/// Every function's effect, by name.
pub(crate) struct EffectTable {
    effects: HashMap<String, MemEffect>,
}

impl EffectTable {
    /// What a call to `name` may do.
    ///
    /// A name with no entry is `Unknown`, which is the answer for everything
    /// this translation unit neither defines nor has a promise about.
    pub(crate) fn of(&self, name: &str) -> MemEffect {
        self.effects
            .get(name)
            .copied()
            .unwrap_or(MemEffect::Unknown)
    }

    pub(crate) fn build(module: &Module, types: &TypeTable) -> EffectTable {
        let mut effects: HashMap<String, MemEffect> = HashMap::new();

        // Promises first, from prototypes this unit only declares.
        for (name, e) in &module.declared_fn_effects {
            effects.insert(name.clone(), *e);
        }

        // Which functions the fixed point is allowed to move, and where each
        // one starts.
        let mut inferable: Vec<Summary> = Vec::new();
        for f in &module.functions {
            if f.declared_effect != MemEffect::Unknown {
                // A promise outranks anything a body could show, and pins
                // the cell.
                effects.insert(f.name.clone(), f.declared_effect);
                continue;
            }
            if !is_inferable(f) {
                effects.insert(f.name.clone(), MemEffect::Unknown);
                continue;
            }
            // Optimistic, which is what makes recursion converge: a cycle
            // with nothing dirty in it stays clean.
            effects.insert(f.name.clone(), MemEffect::Const);
            inferable.push(summarize(f, types));
        }

        solve(&inferable, &mut effects);

        if std::env::var_os("C17_DBG_EFFECTS").is_some() {
            let mut v: Vec<_> = effects.iter().collect();
            v.sort();
            eprintln!("DBG effects {:?}", v);
        }
        EffectTable { effects }
    }
}

/// What one function's body says, with its calls left as names.
///
/// Summarized once. The body scan is the expensive part -- it rebuilds the
/// escape and address maps -- and nothing in it depends on what the fixed
/// point currently believes, so re-running it per sweep was both slow and
/// the reason the sweep count had to be capped at all.
struct Summary {
    name: String,
    /// Everything the body does that is not a call.
    local: MemEffect,
    /// Every name it calls directly.
    callees: Vec<String>,
}

fn summarize(f: &Function, types: &TypeTable) -> Summary {
    let esc = EscapeInfo::analyze(f, types);
    if esc.gave_up() {
        return Summary {
            name: f.name.clone(),
            local: MemEffect::Unknown,
            callees: Vec::new(),
        };
    }
    let am = AddrMap::build(f);
    let mut local = MemEffect::Const;
    let mut callees: Vec<String> = Vec::new();

    for bb in &f.blocks {
        for insn in &bb.insns {
            if insn.op == Opcode::Call {
                match insn.extra().func_name.as_deref() {
                    Some(n) => {
                        if !callees.iter().any(|c| c == n) {
                            callees.push(n.to_string());
                        }
                    }
                    // An indirect call names no callee at all.
                    None => local = MemEffect::Unknown,
                }
                continue;
            }
            local = local.join(insn_effect(f, &esc, &am, insn));
        }
    }
    Summary {
        name: f.name.clone(),
        local,
        callees,
    }
}

/// Raise each function to the join of its own body and its callees, to a
/// fixed point.
///
/// A worklist rather than a sweep, and the difference is not only speed. A
/// sweep in `module.functions` order propagates dirtiness one caller per
/// pass, so a capped sweep count silently *kept the optimistic seed* for
/// anything deeper than the cap -- a chain of seventeen static functions
/// whose last one wrote a global came out `Const` at the top, and a store
/// across a call to it was forwarded. This needs no cap: each cell can rise
/// at most twice, over a three-point lattice, so the queue drains.
fn solve(inferable: &[Summary], effects: &mut HashMap<String, MemEffect>) {
    // Who has to be re-examined when a name gets dirtier.
    // Callers are visited in order, so a repeat can only be the entry this
    // caller pushed last.
    let mut callers: HashMap<&str, Vec<usize>> = HashMap::new();
    for (i, s) in inferable.iter().enumerate() {
        for c in &s.callees {
            let e = callers.entry(c.as_str()).or_default();
            if e.last() != Some(&i) {
                e.push(i);
            }
        }
    }

    let mut work: VecDeque<usize> = (0..inferable.len()).collect();
    let mut queued: Vec<bool> = vec![true; inferable.len()];
    while let Some(i) = work.pop_front() {
        queued[i] = false;
        let s = &inferable[i];
        let mut e = s.local;
        for c in &s.callees {
            e = e.join(
                effects
                    .get(c.as_str())
                    .copied()
                    .unwrap_or(MemEffect::Unknown),
            );
            if e == MemEffect::Unknown {
                break;
            }
        }
        let cur = effects.get(&s.name).copied().unwrap_or(MemEffect::Unknown);
        let next = cur.join(e);
        if next == cur {
            continue;
        }
        effects.insert(s.name.clone(), next);
        for &c in callers.get(s.name.as_str()).into_iter().flatten() {
            if !queued[c] {
                queued[c] = true;
                work.push_back(c);
            }
        }
    }
}

/// May this function's body be analyzed at all?
///
/// The linkage test is the substantive one. A definition another object can
/// replace is not evidence about what will actually run: a `weak` definition
/// exists to be replaced, and an exported one can be interposed at load
/// time. Only a function with internal linkage is certain to be the one
/// called, so only those are inferred -- anything else must say what it does
/// with an attribute.
fn is_inferable(f: &Function) -> bool {
    f.is_static && !f.symbol_attrs.weak && !f.blocks.is_empty()
}

/// What one instruction that is not a call does to observable memory: the
/// join over everything `AddrMap::access` says it touches.
///
/// Touching a local this function never let out is invisible to the caller,
/// and before SSA promotion that is most of what a body does -- through
/// loads and stores, or a `memset` or `memcpy` of a buffer of its own.
fn insn_effect(f: &Function, esc: &EscapeInfo, am: &AddrMap, insn: &Instruction) -> MemEffect {
    // Reading or writing a volatile object is itself observable (C17
    // 5.1.2.3p6), whatever the object: a function that does it cannot be
    // called fewer times, or in a different order, than written, and neither
    // `Const` nor `Pure` may say otherwise.
    if insn.is_volatile_access() {
        return MemEffect::Unknown;
    }
    // Calls are `Summary::callees`, resolved by the fixed point, and never
    // reach here; a callee nothing is known about is the honest answer.
    let access = am.access(f, insn, |_| MemEffect::Unknown);
    if access.writes.iter().any(|w| !w.is_private(esc)) {
        return MemEffect::Unknown;
    }
    access
        .reads
        .iter()
        .filter(|r| !r.is_private(esc))
        .map(read_effect)
        .fold(MemEffect::Const, MemEffect::join)
}

/// What reading `r`, which a caller can see, makes a function.
///
/// Only an exact access is known not to be volatile, by the instruction's
/// marker, which `insn_effect` has already asked. A block operation reads
/// bytes of no stated type, and they may be a volatile object's, whose read
/// is observable.
fn read_effect(r: &Footprint) -> MemEffect {
    match r {
        Footprint::At(_) => MemEffect::Pure,
        Footprint::Object(_) | Footprint::Escaped | Footprint::Anything => MemEffect::Unknown,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Instruction, Pseudo, PseudoId};
    use crate::target::Target;
    use crate::types::TypeTable;

    fn types() -> TypeTable {
        TypeTable::new(&Target::host())
    }

    /// A function with one block, whose body the caller fills in.
    fn func(
        name: &str,
        is_static: bool,
        build: impl FnOnce(&mut Function, &mut BasicBlock),
    ) -> Function {
        let t = types();
        let mut f = Function::new(name, t.void_id);
        f.is_static = is_static;
        f.next_pseudo = 40;
        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        build(&mut f, &mut bb);
        f.blocks.push(bb);
        f.entry = BasicBlockId(0);
        f.rebuild_block_idx();
        f
    }

    fn module(fns: Vec<Function>) -> Module {
        let mut m = Module::default();
        for f in fns {
            m.add_function(f);
        }
        m
    }

    /// A body that touches nothing is `Const`.
    #[test]
    fn effects_an_empty_body_is_const() {
        let t = types();
        let f = func("f", true, |_, bb| {
            bb.add_insn(Instruction::new(Opcode::Ret).with_type_and_size(t.void_id, 0));
        });
        let table = EffectTable::build(&module(vec![f]), &types());
        assert_eq!(table.of("f"), MemEffect::Const);
    }

    /// Reading a global is `Pure`; writing one is `Unknown`.
    #[test]
    fn effects_a_global_read_is_pure_a_write_is_not() {
        let t = types();
        let reader = func("r", true, |f, bb| {
            f.add_pseudo(Pseudo::sym(PseudoId(0), "g".into()));
            bb.add_insn(Instruction::load(PseudoId(1), PseudoId(0), 0, t.int_id, 32));
        });
        let writer = func("w", true, |f, bb| {
            f.add_pseudo(Pseudo::sym(PseudoId(0), "g".into()));
            f.add_pseudo(Pseudo::val(PseudoId(1), 7));
            bb.add_insn(Instruction::store(
                PseudoId(1),
                PseudoId(0),
                0,
                t.int_id,
                32,
            ));
        });
        let table = EffectTable::build(&module(vec![reader, writer]), &types());
        assert_eq!(table.of("r"), MemEffect::Pure);
        assert_eq!(table.of("w"), MemEffect::Unknown);
    }

    /// **The rule that makes inference worth having.** Before SSA promotion
    /// a parameter is a stack slot, so a body that only reads its own
    /// arguments still stores and loads. Those are invisible to the caller.
    #[test]
    fn effects_a_non_escaping_local_contributes_nothing() {
        let t = types();
        let f = func("f", true, |f, bb| {
            f.add_pseudo(Pseudo::sym(PseudoId(0), "a.0".into()));
            f.add_pseudo(Pseudo::val(PseudoId(1), 7));
            f.add_local("a.0", PseudoId(0), t.int_id, None, None);
            bb.add_insn(Instruction::store(
                PseudoId(1),
                PseudoId(0),
                0,
                t.int_id,
                32,
            ));
            bb.add_insn(Instruction::load(PseudoId(2), PseudoId(0), 0, t.int_id, 32));
        });
        let table = EffectTable::build(&module(vec![f]), &types());
        assert_eq!(table.of("f"), MemEffect::Const);
    }

    /// Reading a volatile object is observable, so a function whose body is
    /// only that read is neither `Pure` nor `Const`: two calls of it are two
    /// reads, and neither may be merged with the other or deleted. The same
    /// read unmarked is the control -- an ordinary global read is `Pure`.
    #[test]
    fn effects_a_volatile_access_is_never_pure() {
        let t = types();
        let body = |volatile: bool| {
            move |f: &mut Function, bb: &mut BasicBlock| {
                f.add_pseudo(Pseudo::sym(PseudoId(0), "g".into()));
                bb.add_insn(
                    Instruction::load(PseudoId(2), PseudoId(0), 0, t.int_id, 32)
                        .with_volatile(volatile),
                );
            }
        };
        let vol = EffectTable::build(&module(vec![func("f", true, body(true))]), &types());
        assert_eq!(vol.of("f"), MemEffect::Unknown);
        let plain = EffectTable::build(&module(vec![func("f", true, body(false))]), &types());
        assert_eq!(plain.of("f"), MemEffect::Pure);

        // A volatile local is no more private than a volatile global.
        let local = func("f", true, |f, bb| {
            f.add_pseudo(Pseudo::sym(PseudoId(0), "a.0".into()));
            f.add_local("a.0", PseudoId(0), t.int_id, None, None);
            bb.add_insn(
                Instruction::load(PseudoId(2), PseudoId(0), 0, t.int_id, 32).with_volatile(true),
            );
        });
        let table = EffectTable::build(&module(vec![local]), &types());
        assert_eq!(table.of("f"), MemEffect::Unknown);
    }

    /// A local whose address was handed to a callee is no longer private.
    #[test]
    fn effects_an_escaped_local_is_a_write() {
        let t = types();
        let f = func("f", true, |f, bb| {
            f.add_pseudo(Pseudo::sym(PseudoId(0), "a.0".into()));
            f.add_pseudo(Pseudo::val(PseudoId(1), 7));
            f.add_local("a.0", PseudoId(0), t.int_id, None, None);
            bb.add_insn(Instruction::sym_addr(PseudoId(3), PseudoId(0), t.long_id));
            bb.add_insn(Instruction::call(
                None,
                "opaque",
                vec![PseudoId(3)],
                vec![t.long_id],
                t.void_id,
                0,
            ));
            bb.add_insn(Instruction::store(
                PseudoId(1),
                PseudoId(0),
                0,
                t.int_id,
                32,
            ));
        });
        let table = EffectTable::build(&module(vec![f]), &types());
        assert_eq!(table.of("f"), MemEffect::Unknown);
    }

    /// A block operation, as the linearizer makes `memset(dest, 0, n)` and
    /// `memcpy(dest, src, n)`.
    fn block_op(op: Opcode, dest: PseudoId, src: PseudoId) -> Instruction {
        Instruction::new(op)
            .with_src3(dest, src, PseudoId(9))
            .with_type_and_size(types().void_ptr_id, 64)
    }

    /// The effect of a static function with two local buffers
    /// `@a.0` (`%10`) and `@b.0` (`%11`), a global `g` (`%12`), a pointer
    /// argument (`%13`) and the instructions `body` adds.
    fn effect_of(body: impl FnOnce(&mut Function, &mut BasicBlock)) -> MemEffect {
        let t = types();
        let f = func("f", true, |f, bb| {
            f.add_pseudo(Pseudo::sym(PseudoId(0), "a.0".into()));
            f.add_pseudo(Pseudo::sym(PseudoId(1), "b.0".into()));
            f.add_pseudo(Pseudo::sym(PseudoId(12), "g".into()));
            f.add_pseudo(Pseudo::arg(PseudoId(13), 0));
            f.add_pseudo(Pseudo::val(PseudoId(9), 200));
            f.add_local("a.0", PseudoId(0), t.long_id, None, None);
            f.add_local("b.0", PseudoId(1), t.long_id, None, None);
            bb.add_insn(Instruction::sym_addr(
                PseudoId(10),
                PseudoId(0),
                t.void_ptr_id,
            ));
            bb.add_insn(Instruction::sym_addr(
                PseudoId(11),
                PseudoId(1),
                t.void_ptr_id,
            ));
            body(f, bb);
        });
        EffectTable::build(&module(vec![f]), &types()).of("f")
    }

    /// **The precision block ops add.** A `memset` or `memcpy` that touches
    /// only the function's own buffers is as invisible to a caller as the
    /// stores it stands for.
    #[test]
    fn effects_a_block_op_on_private_buffers_contributes_nothing() {
        let e = effect_of(|_, bb| {
            bb.add_insn(block_op(Opcode::Memset, PseudoId(10), PseudoId(9)));
        });
        assert_eq!(e, MemEffect::Const, "memset of a private buffer");
        for op in [Opcode::Memcpy, Opcode::Memmove] {
            let e = effect_of(|_, bb| {
                bb.add_insn(block_op(op, PseudoId(10), PseudoId(11)));
            });
            assert_eq!(e, MemEffect::Const, "{op:?} between private buffers");
        }
        // The result is a pointer into the buffer, and reading through it
        // is still private.
        let e = effect_of(|_, bb| {
            let t = types();
            bb.add_insn(
                block_op(Opcode::Memset, PseudoId(10), PseudoId(9)).with_target(PseudoId(20)),
            );
            bb.add_insn(Instruction::load(
                PseudoId(21),
                PseudoId(20),
                0,
                t.int_id,
                32,
            ));
        });
        assert_eq!(e, MemEffect::Pure, "a load through an unresolved pointer");
    }

    /// A function body under construction, for a table of them.
    type Body = Box<dyn FnOnce(&mut Function, &mut BasicBlock)>;

    /// The conservative answers stay conservative: a block op that writes a
    /// global, writes through a pointer it was given, reads anything a caller
    /// can see, or touches a buffer whose address escaped is a write -- and
    /// so is everything the table does not model.
    #[test]
    fn effects_a_block_op_on_visible_memory_is_unknown() {
        let t = types();
        let cases: Vec<(&str, Body)> = vec![
            (
                "memset of a global",
                Box::new(|_, bb| bb.add_insn(block_op(Opcode::Memset, PseudoId(12), PseudoId(9)))),
            ),
            (
                "memset through an argument",
                Box::new(|_, bb| bb.add_insn(block_op(Opcode::Memset, PseudoId(13), PseudoId(9)))),
            ),
            (
                "memcpy into an argument",
                Box::new(|_, bb| bb.add_insn(block_op(Opcode::Memcpy, PseudoId(13), PseudoId(10)))),
            ),
            (
                "memcpy out of a global",
                Box::new(|_, bb| bb.add_insn(block_op(Opcode::Memcpy, PseudoId(10), PseudoId(12)))),
            ),
            (
                "memcpy out of an argument",
                Box::new(|_, bb| bb.add_insn(block_op(Opcode::Memcpy, PseudoId(10), PseudoId(13)))),
            ),
            (
                "memset of an escaped buffer",
                Box::new(move |_, bb| {
                    bb.add_insn(block_op(Opcode::Memset, PseudoId(10), PseudoId(9)));
                    bb.add_insn(Instruction::call(
                        None,
                        "keep",
                        vec![PseudoId(10)],
                        vec![t.void_ptr_id],
                        t.void_id,
                        0,
                    ));
                }),
            ),
            (
                "inline asm",
                Box::new(|_, bb| bb.add_insn(Instruction::new(Opcode::Asm))),
            ),
            (
                "an atomic on a private buffer",
                Box::new(move |_, bb| {
                    bb.add_insn(
                        Instruction::new(Opcode::AtomicLoad)
                            .with_target(PseudoId(20))
                            .with_src(PseudoId(10))
                            .with_type_and_size(t.int_id, 32),
                    )
                }),
            ),
            (
                "a volatile store to a private buffer",
                Box::new(move |_, bb| {
                    bb.add_insn(
                        Instruction::store(PseudoId(9), PseudoId(10), 0, t.int_id, 32)
                            .with_volatile(true),
                    )
                }),
            ),
        ];
        for (what, body) in cases {
            assert_eq!(effect_of(body), MemEffect::Unknown, "{what}");
        }
        // A plain load of the global beside them is only a read.
        let e = effect_of(|_, bb| {
            bb.add_insn(Instruction::load(
                PseudoId(20),
                PseudoId(12),
                0,
                t.int_id,
                32,
            ));
        });
        assert_eq!(e, MemEffect::Pure);
    }

    /// A caller is as dirty as its callee, and the sweep propagates it.
    #[test]
    fn effects_propagate_along_the_call_graph() {
        let t = types();
        let callee = func("dirty", true, |f, bb| {
            f.add_pseudo(Pseudo::sym(PseudoId(0), "g".into()));
            f.add_pseudo(Pseudo::val(PseudoId(1), 7));
            bb.add_insn(Instruction::store(
                PseudoId(1),
                PseudoId(0),
                0,
                t.int_id,
                32,
            ));
        });
        let caller = func("caller", true, |_, bb| {
            bb.add_insn(Instruction::call(
                None,
                "dirty",
                vec![],
                vec![],
                t.void_id,
                0,
            ));
        });
        let clean = func("clean", true, |_, bb| {
            bb.add_insn(Instruction::new(Opcode::Ret).with_type_and_size(t.void_id, 0));
        });
        let clean_caller = func("clean_caller", true, |_, bb| {
            bb.add_insn(Instruction::call(
                None,
                "clean",
                vec![],
                vec![],
                t.void_id,
                0,
            ));
        });
        let table =
            EffectTable::build(&module(vec![callee, caller, clean, clean_caller]), &types());
        assert_eq!(table.of("caller"), MemEffect::Unknown);
        assert_eq!(table.of("clean_caller"), MemEffect::Const);
    }

    /// Recursion converges on the optimistic seed rather than diverging.
    #[test]
    fn effects_recursion_converges() {
        let t = types();
        let a = func("a", true, |_, bb| {
            bb.add_insn(Instruction::call(None, "b", vec![], vec![], t.void_id, 0));
        });
        let b = func("b", true, |_, bb| {
            bb.add_insn(Instruction::call(None, "a", vec![], vec![], t.void_id, 0));
        });
        let table = EffectTable::build(&module(vec![a, b]), &types());
        assert_eq!(table.of("a"), MemEffect::Const);
        assert_eq!(table.of("b"), MemEffect::Const);
    }

    /// A definition another object can replace is not evidence about what
    /// will run, so external and weak definitions are never inferred.
    #[test]
    fn effects_only_internal_linkage_is_inferred() {
        let t = types();
        let mut external = func("ext", false, |_, bb| {
            bb.add_insn(Instruction::new(Opcode::Ret).with_type_and_size(t.void_id, 0));
        });
        external.is_static = false;
        let mut weak = func("wk", true, |_, bb| {
            bb.add_insn(Instruction::new(Opcode::Ret).with_type_and_size(t.void_id, 0));
        });
        weak.symbol_attrs.weak = true;
        let table = EffectTable::build(&module(vec![external, weak]), &types());
        assert_eq!(table.of("ext"), MemEffect::Unknown);
        assert_eq!(table.of("wk"), MemEffect::Unknown);
    }

    /// A promise outranks the body, in both directions: a dirty body does
    /// not lower a declared effect, and a declared effect on an otherwise
    /// un-inferable function still counts.
    #[test]
    fn effects_a_declared_promise_is_not_lowered() {
        let t = types();
        let mut f = func("f", false, |f, bb| {
            f.add_pseudo(Pseudo::sym(PseudoId(0), "g".into()));
            f.add_pseudo(Pseudo::val(PseudoId(1), 7));
            bb.add_insn(Instruction::store(
                PseudoId(1),
                PseudoId(0),
                0,
                t.int_id,
                32,
            ));
        });
        f.declared_effect = MemEffect::Pure;
        let table = EffectTable::build(&module(vec![f]), &types());
        assert_eq!(table.of("f"), MemEffect::Pure);
    }

    /// A prototype this unit only declares carries its promise.
    #[test]
    fn effects_a_declared_prototype_is_recorded() {
        let mut m = Module::default();
        m.declared_fn_effects
            .insert("strlen".into(), MemEffect::Pure);
        let table = EffectTable::build(&m, &types());
        assert_eq!(table.of("strlen"), MemEffect::Pure);
        assert_eq!(table.of("never_heard_of_it"), MemEffect::Unknown);
    }

    /// The lattice itself: the join is the dirtier of the two, and only
    /// `Unknown` may have written anything.
    #[test]
    fn effects_lattice_joins_upward() {
        use MemEffect::*;
        assert_eq!(Const.join(Pure), Pure);
        assert_eq!(Pure.join(Const), Pure);
        assert_eq!(Pure.join(Unknown), Unknown);
        assert_eq!(Const.join(Const), Const);
        assert!(!Const.may_write());
        assert!(!Pure.may_write());
        assert!(Unknown.may_write());
    }
}
