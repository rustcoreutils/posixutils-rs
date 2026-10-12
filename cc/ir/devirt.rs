//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A call through a pointer that holds one known function becomes a call of
// that function: `void (*p)(void) = f; p();` is `f();`, as gcc makes it from
// -O1.
//
// The pointer is known once the passes before have done their work: the
// local promoted and the copies followed, so that the call's target is
// defined by the `SymAddr` of the function -- or by a load of a `const`
// global, or a member of one, initialized with that address (`static void
// (*const p)(void) = f;`, a `const` table of operations). The
// call is then direct, which is what the backends emit cheaper (`call f`, no
// load of the address), what the `error` and `warning` attributes report on
// (`opt::report_attribute_calls`), and what the interprocedural facts about
// `f` reach.
//
// A call to any symbol may be made direct, a weak or interposable one
// included: calling `f` and calling through `&f` reach the same definition,
// whichever the linker or the dynamic loader binds the name to, and a weak
// undefined `f` is a call of address zero both ways. gcc folds them all. Only
// a function whose type takes the call's arguments is called directly: a
// call through a pointer cast to another function type keeps its pointer,
// so nothing downstream matches the arguments against parameters they are
// not.
//
// It runs twice, as gcc's does: before inlining, on the addresses the
// function itself takes, so that the inliner sees the direct call -- an
// `always_inline` function called through a pointer is inlined, as gcc
// inlines it, and must be, when it has no body of its own -- and in the
// optimizer's loop, where the loads of `const` globals are resolved too. A
// function with no out-of-line body is never made the target there, which
// only the inliner could have resolved.
//

use super::constglobal::KnownGlobals;
use super::memloc::{AddrMap, MemBase};
use super::{Function, Instruction, Opcode, PseudoId, PseudoKind};
use crate::types::{TypeKind, TypeTable};
use crate::units::ByteSize;
use std::collections::{HashMap, HashSet};

/// Make each indirect call of `func` whose target is one known function a
/// direct call of it, but for a function of `bodiless`, which has no
/// out-of-line body to call. `globals`, when given, resolves the loads of
/// `const` globals. Returns whether any changed.
pub fn run(
    func: &mut Function,
    types: &TypeTable,
    globals: Option<&KnownGlobals>,
    bodiless: &HashSet<String>,
) -> bool {
    if !func
        .blocks
        .iter()
        .flat_map(|b| &b.insns)
        .any(|i| i.op == Opcode::Call && i.extra().indirect_target.is_some())
    {
        return false;
    }
    let known = known_functions(func, types, globals);
    if known.is_empty() {
        return false;
    }
    let mut changed = false;
    for block in &mut func.blocks {
        for insn in &mut block.insns {
            if insn.op != Opcode::Call {
                continue;
            }
            let Some(target) = insn.extra().indirect_target else {
                continue;
            };
            let Some((name, params)) = known.get(&target) else {
                continue;
            };
            if bodiless.contains(name) {
                continue;
            }
            if !takes_the_arguments(*params, insn) {
                continue;
            }
            let extra = insn.extra_mut();
            extra.func_name = Some(name.clone());
            extra.indirect_target = None;
            changed = true;
        }
    }
    changed
}

/// What a function's type says of the arguments it takes: the number of
/// named parameters, and whether more may follow (variadic or no
/// prototype).
#[derive(Clone, Copy)]
struct Params {
    count: usize,
    open: bool,
}

/// Whether a function taking `params` takes the arguments `call` passes.
fn takes_the_arguments(params: Params, call: &Instruction) -> bool {
    let args = call.src.len();
    if params.open {
        args >= params.count
    } else {
        args == params.count
    }
}

/// Every pseudo of `func` that holds the address of one function: the
/// `SymAddr` of a function symbol, a load of a known global's address of
/// one, and copies of either -- with the function's name and what its type
/// takes.
fn known_functions(
    func: &Function,
    types: &TypeTable,
    globals: Option<&KnownGlobals>,
) -> HashMap<PseudoId, (String, Params)> {
    let mut known = HashMap::new();
    let mut copies = Vec::new();
    let am = AddrMap::build(func);
    for insn in func.blocks.iter().flat_map(|b| &b.insns) {
        let Some(target) = insn.target else { continue };
        match insn.op {
            Opcode::SymAddr => {
                let Some(&sym) = insn.src.first() else {
                    continue;
                };
                let Some(PseudoKind::Sym(name)) = func.get_pseudo(sym).map(|p| &p.kind) else {
                    continue;
                };
                let Some(params) = insn.typ.and_then(|t| function_params(types, t)) else {
                    continue;
                };
                known.insert(target, (name.clone(), params));
            }
            // A `volatile` access may read something else than the
            // initializer, whatever the object is declared as.
            Opcode::Load if !insn.is_volatile_access() && insn.src.len() == 1 => {
                let Some(globals) = globals else { continue };
                let Some(params) = insn.typ.and_then(|t| function_params(types, t)) else {
                    continue;
                };
                let loc = am.resolve(func, insn.src[0], insn.offset, insn.size, insn.typ);
                let (MemBase::Global(global), Some(offset)) = (&loc.base, loc.offset) else {
                    continue;
                };
                let (Ok(offset), Some(width)) = (u64::try_from(offset), insn.size.byte_size())
                else {
                    continue;
                };
                if let Some(name) = globals.address_at(global, ByteSize::new(offset), width) {
                    known.insert(target, (name.to_string(), params));
                }
            }
            Opcode::Copy => {
                if let Some(&src) = insn.src.first() {
                    copies.push((target, src));
                }
            }
            _ => {}
        }
    }
    // Copies of copies: until nothing new is learned. SSA form gives each
    // pseudo one definition, so the answer does not depend on the order.
    loop {
        let mut grew = false;
        for &(to, from) in &copies {
            if known.contains_key(&to) {
                continue;
            }
            if let Some(f) = known.get(&from).cloned() {
                known.insert(to, f);
                grew = true;
            }
        }
        if !grew {
            break;
        }
    }
    known
}

/// What the function `ptr` points to takes, when `ptr` is a pointer to a
/// function.
fn function_params(types: &TypeTable, ptr: crate::types::TypeId) -> Option<Params> {
    if types.kind(ptr) != TypeKind::Pointer {
        return None;
    }
    let pointee = types.base_type(ptr)?;
    if types.kind(pointee) != TypeKind::Function {
        return None;
    }
    let t = types.get(pointee);
    Some(match &t.params {
        Some(params) => Params {
            count: params.len(),
            open: t.variadic,
        },
        None => Params {
            count: 0,
            open: true,
        },
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Pseudo};
    use crate::target::Target;

    /// `%2 = symaddr @<name>` of a function of `params` (`None`: no
    /// prototype), copied to `%3`, and a call through `%3` with `args`
    /// arguments.
    fn call_through(name: &str, params: Option<usize>, args: usize) -> (Function, TypeTable) {
        let mut types = TypeTable::new(&Target::host());
        let (void, int) = (types.void_id, types.int_id);
        let fn_type = types.intern(match params {
            Some(n) => crate::types::Type::function(void, vec![int; n], false, false),
            None => crate::types::Type::function_no_prototype(void, false),
        });
        let ptr = types.intern(crate::types::Type::pointer(fn_type));
        let mut func = Function::new("caller", types.void_id);
        func.add_pseudo(Pseudo::sym(PseudoId(1), name.to_string()));
        func.add_pseudo(Pseudo::val(PseudoId(4), 7));
        func.next_pseudo = 5;
        let mut b = BasicBlock::new(BasicBlockId(0));
        b.add_insn(Instruction::new(Opcode::Entry));
        b.add_insn(Instruction::sym_addr(PseudoId(2), PseudoId(1), ptr));
        b.add_insn(
            Instruction::new(Opcode::Copy)
                .with_target(PseudoId(3))
                .with_src(PseudoId(2))
                .with_type_and_size(ptr, crate::units::Bits::new(64)),
        );
        let int = types.int_id;
        b.add_insn(Instruction::call_indirect(
            None,
            PseudoId(3),
            vec![PseudoId(4); args],
            vec![int; args],
            types.void_id,
            crate::units::Bits::ZERO,
        ));
        b.add_insn(Instruction::ret(None));
        func.add_block(b);
        func.entry = BasicBlockId(0);
        (func, types)
    }

    /// The known globals of a module that has none.
    fn no_globals(types: &TypeTable) -> KnownGlobals {
        KnownGlobals::collect(&crate::ir::Module::default(), types)
    }

    fn the_call(func: &Function) -> &Instruction {
        func.blocks[0]
            .insns
            .iter()
            .find(|i| i.op == Opcode::Call)
            .expect("a call")
    }

    #[test]
    fn a_call_through_a_known_function_is_direct() {
        let (mut func, types) = call_through("f", Some(1), 1);
        assert!(run(
            &mut func,
            &types,
            Some(&no_globals(&types)),
            &HashSet::new()
        ));
        let call = the_call(&func);
        assert_eq!(call.extra().func_name.as_deref(), Some("f"));
        assert_eq!(call.extra().indirect_target, None);
        assert_eq!(call.local_callee(), Some("f"));
        // Nothing left to do.
        assert!(!run(
            &mut func,
            &types,
            Some(&no_globals(&types)),
            &HashSet::new()
        ));
    }

    #[test]
    fn a_call_the_function_type_does_not_take_keeps_its_pointer() {
        for (params, args) in [(Some(0), 1), (Some(2), 1)] {
            let (mut func, types) = call_through("f", params, args);
            assert!(
                !run(
                    &mut func,
                    &types,
                    Some(&no_globals(&types)),
                    &HashSet::new()
                ),
                "{params:?} {args}"
            );
            assert!(the_call(&func).extra().indirect_target.is_some());
        }
        // No prototype takes anything.
        let (mut func, types) = call_through("f", None, 3);
        assert!(run(
            &mut func,
            &types,
            Some(&no_globals(&types)),
            &HashSet::new()
        ));
    }
}
