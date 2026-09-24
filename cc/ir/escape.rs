//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Which of this function's locals could a callee, an inline asm, or another
// thread reach?
//
// The answer matters because of what it licenses: **a call cannot write a
// local whose address never left the function**, whatever the callee does.
// That is stronger than any purity attribute and needs no interprocedural
// information at all, and it is what lets a store be forwarded across a call
// to an entirely unknown function.
//
// `ssa::analyze_variable` already scans for an address being taken, and this
// generalizes it. The two want different answers, and the difference is
// exactly `SymAddr`: for SSA promotion any materialized address disqualifies
// a variable, because a promoted variable has no address at all. For escape
// it does not -- `symaddr` is simply how an array local is addressed, and a
// pointer that never leaves the function has not escaped. So this follows
// the address through the pseudos that *hold* it, and asks what is finally
// done with it.
//

use super::{Function, Instruction, Opcode, PseudoId, PseudoKind};
use std::collections::HashSet;

/// Which locals are reachable from outside this function.
pub(crate) struct EscapeInfo {
    escaped: HashSet<PseudoId>,
    /// Something defeats the analysis for the whole function.
    give_up: bool,
}

impl EscapeInfo {
    /// Can a callee, an asm, or another thread reach this base?
    pub(crate) fn is_captured(&self, base: &super::memloc::MemBase) -> bool {
        match base {
            super::memloc::MemBase::Unknown => true,
            // Any externally-linked callee can name a global.
            super::memloc::MemBase::Global(_) => true,
            super::memloc::MemBase::Local(p) => self.give_up || self.escaped.contains(p),
        }
    }

    pub(crate) fn gave_up(&self) -> bool {
        self.give_up
    }

    pub(crate) fn analyze(func: &Function) -> EscapeInfo {
        // `longjmp` resumes at a point the CFG does not model, so every
        // ordering claim a memory pass makes is void -- and C17 7.13.2.1p3
        // already makes a non-`volatile` local modified after `setjmp`
        // indeterminate. `&&label` is the same problem by another route.
        let give_up = func.takes_label_addr
            || func.blocks.iter().any(|bb| {
                bb.insns
                    .iter()
                    .any(|i| matches!(i.op, Opcode::Setjmp | Opcode::Longjmp))
            });

        let mut escaped = HashSet::new();
        if give_up {
            return EscapeInfo { escaped, give_up };
        }

        for p in &func.pseudos {
            if !matches!(p.kind, PseudoKind::Sym(_)) {
                continue;
            }
            let Some(local) = func.local_of(p.id) else {
                continue;
            };
            // Not an escape, but off limits all the same, and one set is
            // simpler to reason about than two.
            if local.is_volatile || local.is_atomic || escapes(func, p.id) {
                escaped.insert(p.id);
            }
        }
        EscapeInfo { escaped, give_up }
    }
}

/// Does the address of `sym` leave this function's control?
///
/// A transitive closure over the pseudos that hold the address. The default
/// arm is the point: anything not recognized as an access or as another way
/// of holding the address counts as an escape, so an opcode this does not
/// model fails closed.
fn escapes(func: &Function, sym: PseudoId) -> bool {
    let mut holders: HashSet<PseudoId> = HashSet::new();
    holders.insert(sym);
    let mut work = vec![sym];

    while let Some(p) = work.pop() {
        for bb in &func.blocks {
            for insn in &bb.insns {
                if insn.op == Opcode::Nop || !insn.mentions(p) {
                    continue;
                }
                // `mentions` answers "names this pseudo", and a target is a
                // name. But an instruction that only *defines* a *value* is
                // where that value came from, and that is already accounted
                // for: it is in the holder set precisely because a recognized
                // rule produced it. Only a *use* can let an address out, and
                // SSA invariant I1 makes "defines and does not use"
                // unambiguous.
                //
                // A `Sym` is the exception, and the reason this is not a
                // blanket skip: a `Sym` names storage rather than a value, so
                // targeting one is a *write to the object*, not a definition.
                // A struct-returning call targets the receiving local
                // directly, which means its address was handed to the callee.
                let defines_value = insn.target == Some(p)
                    && !insn.src.contains(&p)
                    && !matches!(
                        func.get_pseudo(p).map(|x| &x.kind),
                        Some(PseudoKind::Sym(_))
                    );
                if defines_value {
                    continue;
                }
                match holder_effect(insn, p) {
                    Effect::Access => {}
                    Effect::Holds(t) => {
                        if holders.insert(t) {
                            work.push(t);
                        }
                    }
                    Effect::Escapes => return true,
                }
            }
        }
    }
    false
}

enum Effect {
    /// Reads or writes through the address, which is what an address is for.
    Access,
    /// Produces another pseudo naming the same address.
    Holds(PseudoId),
    /// The address reaches somewhere this function cannot account for.
    Escapes,
}

fn holder_effect(insn: &Instruction, p: PseudoId) -> Effect {
    match insn.op {
        // Reading through the address is an access.
        Opcode::Load if insn.src.first() == Some(&p) => Effect::Access,
        // Writing *through* it is an access; writing *it* is an escape, and
        // the order of these two arms is what distinguishes them.
        Opcode::Store if insn.src.get(1) == Some(&p) => Effect::Escapes,
        Opcode::Store if insn.src.first() == Some(&p) => Effect::Access,

        // Another name for the same address.
        Opcode::SymAddr | Opcode::Copy if insn.src.first() == Some(&p) => match insn.target {
            Some(t) => Effect::Holds(t),
            None => Effect::Escapes,
        },
        // Pointer arithmetic still names the object.
        Opcode::Add | Opcode::Sub if insn.src.contains(&p) => match insn.target {
            Some(t) => Effect::Holds(t),
            None => Effect::Escapes,
        },

        // Everything else: passed to a call, returned, an asm operand, an
        // indirect-call target, an operand of a comparison, an incoming phi
        // value. A comparison leaks one bit rather than a write and a phi
        // produces an address this analysis would call unknown anyway, but
        // both are kept as escapes: failing closed is the same choice
        // `ssa::analyze_variable` makes, and for the same reason.
        _ => Effect::Escapes,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::memloc::MemBase;
    use crate::ir::{BasicBlock, BasicBlockId, Instruction, Pseudo};
    use crate::target::Target;
    use crate::types::{TypeId, TypeTable};

    fn host_types() -> TypeTable {
        TypeTable::new(&Target::host())
    }

    /// A function with one local `@a.0` reached through `symaddr`, plus
    /// whatever `tail` does with the address.
    fn with_local(tail: impl FnOnce(TypeId, &mut BasicBlock)) -> Function {
        let types = host_types();
        let i64t = types.long_id;
        let mut f = Function::new("f", types.void_id);
        f.add_pseudo(Pseudo::sym(PseudoId(0), "a.0".into()));
        f.add_pseudo(Pseudo::val(PseudoId(5), 0));
        f.add_local("a.0", PseudoId(0), i64t, false, false, None, None);
        f.next_pseudo = 40;

        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        bb.add_insn(Instruction::sym_addr(PseudoId(10), PseudoId(0), i64t));
        bb.add_insn(
            Instruction::new(Opcode::Copy)
                .with_target(PseudoId(11))
                .with_src(PseudoId(10))
                .with_type_and_size(i64t, 64),
        );
        tail(i64t, &mut bb);
        f.blocks.push(bb);
        f.entry = BasicBlockId(0);
        f
    }

    fn captured(f: &Function) -> bool {
        EscapeInfo::analyze(f).is_captured(&MemBase::Local(PseudoId(0)))
    }

    /// The whole point: a local reached only through `symaddr`, a `Copy` and
    /// loads and stores has not escaped, however many names its address has.
    ///
    /// This is the case `ssa::analyze_variable` answers the other way, and
    /// getting it wrong makes every array local look captured.
    #[test]
    fn escape_addressing_a_local_is_not_escaping() {
        let f = with_local(|t, bb| {
            bb.add_insn(Instruction::store(PseudoId(5), PseudoId(11), 0, t, 64));
            bb.add_insn(Instruction::load(PseudoId(12), PseudoId(11), 0, t, 64));
        });
        assert!(!captured(&f));
    }

    /// Pointer arithmetic still names the object, and the walk has to follow
    /// it -- but only as another holder, not as an escape.
    #[test]
    fn escape_pointer_arithmetic_still_names_the_object() {
        let f = with_local(|t, bb| {
            bb.add_insn(Instruction::binop(
                Opcode::Add,
                PseudoId(12),
                PseudoId(11),
                PseudoId(5),
                t,
                64,
            ));
            bb.add_insn(Instruction::load(PseudoId(13), PseudoId(12), 0, t, 64));
        });
        assert!(!captured(&f));
    }

    /// Handing the address to a call is the escape that matters.
    #[test]
    fn escape_passing_the_address_to_a_call_escapes() {
        let types = host_types();
        let f = with_local(|t, bb| {
            bb.add_insn(Instruction::call(
                None,
                "g",
                vec![PseudoId(11)],
                vec![t],
                types.void_id,
                0,
            ));
        });
        assert!(captured(&f));
    }

    /// Storing the address *itself* into memory escapes; storing *through*
    /// it does not. The two are one opcode apart, and the order of the arms
    /// is the only thing that distinguishes them.
    #[test]
    fn escape_storing_the_address_escapes_storing_through_it_does_not() {
        let through = with_local(|t, bb| {
            bb.add_insn(Instruction::store(PseudoId(5), PseudoId(11), 0, t, 64));
        });
        assert!(!captured(&through));

        let itself = with_local(|t, bb| {
            // store *the address* into some other location.
            bb.add_insn(Instruction::store(PseudoId(11), PseudoId(5), 0, t, 64));
        });
        assert!(captured(&itself));
    }

    /// An opcode the walk does not model fails closed.
    #[test]
    fn escape_an_unmodelled_use_fails_closed() {
        let f = with_local(|t, bb| {
            bb.add_insn(
                Instruction::new(Opcode::Ret)
                    .with_src(PseudoId(11))
                    .with_type_and_size(t, 64),
            );
        });
        assert!(captured(&f));
    }

    /// `longjmp` resumes at a point the CFG does not model, so every
    /// ordering claim is void and the analysis gives up wholesale.
    #[test]
    fn escape_setjmp_gives_up_on_the_whole_function() {
        let f = with_local(|t, bb| {
            bb.add_insn(Instruction::store(PseudoId(5), PseudoId(11), 0, t, 64));
            bb.add_insn(Instruction::new(Opcode::Setjmp));
        });
        let info = EscapeInfo::analyze(&f);
        assert!(info.gave_up());
        assert!(info.is_captured(&MemBase::Local(PseudoId(0))));
    }

    /// `&&label` is the same problem by another route.
    #[test]
    fn escape_label_address_gives_up() {
        let mut f = with_local(|t, bb| {
            bb.add_insn(Instruction::store(PseudoId(5), PseudoId(11), 0, t, 64));
        });
        f.takes_label_addr = true;
        assert!(EscapeInfo::analyze(&f).gave_up());
    }

    /// A `volatile` local has not escaped, but it is off limits all the
    /// same, and one set is simpler to reason about than two.
    #[test]
    fn escape_volatile_local_is_off_limits() {
        let types = host_types();
        let mut f = with_local(|t, bb| {
            bb.add_insn(Instruction::store(PseudoId(5), PseudoId(11), 0, t, 64));
        });
        f.add_local("a.0", PseudoId(0), types.long_id, true, false, None, None);
        assert!(captured(&f));
    }

    /// A global is reachable by any externally-linked callee, whatever this
    /// function does with it.
    #[test]
    fn escape_a_global_is_always_captured() {
        let f = with_local(|t, bb| {
            bb.add_insn(Instruction::store(PseudoId(5), PseudoId(11), 0, t, 64));
        });
        let info = EscapeInfo::analyze(&f);
        assert!(info.is_captured(&MemBase::Global("g".into())));
        assert!(info.is_captured(&MemBase::Unknown));
    }
}
