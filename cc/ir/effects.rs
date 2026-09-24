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
use super::memloc::{AddrMap, MemBase};
use super::{Function, Module, Opcode};
use crate::parse::ast::MemEffect;
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

    pub(crate) fn build(module: &Module) -> EffectTable {
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
            inferable.push(summarize(f));
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

fn summarize(f: &Function) -> Summary {
    let esc = EscapeInfo::analyze(f);
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
                match insn.func_name.as_deref() {
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
    let mut callers: HashMap<&str, Vec<usize>> = HashMap::new();
    for (i, s) in inferable.iter().enumerate() {
        for c in &s.callees {
            let e = callers.entry(c.as_str()).or_default();
            if !e.contains(&i) {
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

/// What one instruction that is not a call does to observable memory.
fn insn_effect(
    f: &Function,
    esc: &EscapeInfo,
    am: &AddrMap,
    insn: &super::Instruction,
) -> MemEffect {
    match insn.op {
        // Touching a local this function never let out is invisible to the
        // caller, and before SSA promotion that is most of what a body does.
        Opcode::Load | Opcode::Store => {
            let loc = am.location_of(f, insn);
            if matches!(&loc.base, MemBase::Local(p) if !esc.is_captured(&MemBase::Local(*p))) {
                MemEffect::Const
            } else if insn.op == Opcode::Load {
                MemEffect::Pure
            } else {
                MemEffect::Unknown
            }
        }

        // Calls are `Summary::callees`, resolved by the fixed point.
        Opcode::Call => MemEffect::Const,

        _ if !insn.op.may_access_memory() => MemEffect::Const,

        // `Asm`, the mem intrinsics, the `Va*` family, the atomics: all
        // reach memory in ways this does not model.
        _ => MemEffect::Unknown,
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
        let table = EffectTable::build(&module(vec![f]));
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
        let table = EffectTable::build(&module(vec![reader, writer]));
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
            f.add_local("a.0", PseudoId(0), t.int_id, false, false, None, None);
            bb.add_insn(Instruction::store(
                PseudoId(1),
                PseudoId(0),
                0,
                t.int_id,
                32,
            ));
            bb.add_insn(Instruction::load(PseudoId(2), PseudoId(0), 0, t.int_id, 32));
        });
        let table = EffectTable::build(&module(vec![f]));
        assert_eq!(table.of("f"), MemEffect::Const);
    }

    /// A local whose address was handed to a callee is no longer private.
    #[test]
    fn effects_an_escaped_local_is_a_write() {
        let t = types();
        let f = func("f", true, |f, bb| {
            f.add_pseudo(Pseudo::sym(PseudoId(0), "a.0".into()));
            f.add_pseudo(Pseudo::val(PseudoId(1), 7));
            f.add_local("a.0", PseudoId(0), t.int_id, false, false, None, None);
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
        let table = EffectTable::build(&module(vec![f]));
        assert_eq!(table.of("f"), MemEffect::Unknown);
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
        let table = EffectTable::build(&module(vec![callee, caller, clean, clean_caller]));
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
        let table = EffectTable::build(&module(vec![a, b]));
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
        let table = EffectTable::build(&module(vec![external, weak]));
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
        let table = EffectTable::build(&module(vec![f]));
        assert_eq!(table.of("f"), MemEffect::Pure);
    }

    /// A prototype this unit only declares carries its promise.
    #[test]
    fn effects_a_declared_prototype_is_recorded() {
        let mut m = Module::default();
        m.declared_fn_effects
            .insert("strlen".into(), MemEffect::Pure);
        let table = EffectTable::build(&m);
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
