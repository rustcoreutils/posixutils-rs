//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Propagating a `const` global's initializer into the loads that read it.
//
// This is the only pass that reads memory, and it needs neither alias
// information nor escape analysis, because C says the answer outright:
// modifying an object defined with a `const`-qualified type is undefined
// behaviour (C17 6.7.3p6). A pointer to one may escape anywhere at all --
// the program still may not store through it, so the initializer is the
// value for the whole run.
//
// Its output is an ordinary constant, so everything the rest of the
// optimizer already does to constants follows for free: `(int) one != 1`
// closes because `instcombine` folds the conversion and the comparison
// once the `Load` has become a `SetVal`.
//
// What it declines is as load-bearing as what it folds, and each refusal
// below names the thing that could otherwise change the value underneath
// it: an `extern` declaration whose definition is in another translation
// unit, a tentative definition that may be merged with one, a weak
// definition that exists to be replaced, and `volatile`, which is a
// standing instruction that the value can change for reasons the program
// cannot see.
//

use super::{ConstValue, Function, Initializer, Instruction, Module, Opcode, PseudoKind};
use crate::types::{TypeId, TypeModifiers, TypeTable};
use std::collections::HashMap;

/// A global whose value is known for the whole run.
struct KnownGlobal {
    value: ConstValue,
    typ: TypeId,
    size: u32,
}

/// Replace every load of a `const` global with its initializer.
/// Returns whether anything changed.
pub fn run(module: &mut Module, types: &TypeTable) -> bool {
    let known = collect(module, types);
    if known.is_empty() {
        return false;
    }
    let mut changed = false;
    for func in &mut module.functions {
        changed |= propagate(func, types, &known);
    }
    changed
}

/// The globals this may fold, by name.
fn collect(module: &Module, types: &TypeTable) -> HashMap<String, KnownGlobal> {
    let mut known = HashMap::new();
    for g in &module.globals {
        if !qualifies(g, types) {
            continue;
        }
        let value = match &g.init {
            Initializer::Int(v) => ConstValue::Int(*v),
            Initializer::Float(v) | Initializer::Float128(v) => ConstValue::Float(*v),
            // Everything else is an aggregate, a string or absent. An
            // aggregate is foldable in principle, one member at a time, but
            // it needs the load's offset matched against the member list
            // rather than a whole-object compare.
            _ => continue,
        };
        known.insert(
            g.name.clone(),
            KnownGlobal {
                value,
                typ: g.typ,
                size: types.size_bits(g.typ),
            },
        );
    }
    known
}

/// Is this global's initializer the value for the whole run?
fn qualifies(g: &super::GlobalDef, types: &TypeTable) -> bool {
    // Not `const`: an ordinary store may change it.
    if !g.is_const {
        return false;
    }
    // `volatile` says the value can change for reasons not in the program,
    // which is exactly the assumption being made here.
    if types.modifiers(g.typ).contains(TypeModifiers::VOLATILE) {
        return false;
    }
    // A weak definition exists to be replaced at link time, and the
    // replacement's initializer is not this one. gcc folds these anyway;
    // declining costs a fold and cannot be wrong.
    if g.symbol_attrs.weak {
        return false;
    }
    // Thread-local storage is per-thread, and this pass has no notion of
    // which thread's copy a load reads.
    if g.is_thread_local {
        return false;
    }
    // A tentative definition (`const int t;`) may be merged with a real
    // definition in another translation unit, whose initializer is not the
    // implicit zero. gcc declines this one too.
    if matches!(g.init, Initializer::None) {
        return false;
    }
    true
}

/// Rewrite the loads in one function.
fn propagate(func: &mut Function, types: &TypeTable, known: &HashMap<String, KnownGlobal>) -> bool {
    // Collected first: the rewrite needs `&mut func` for the pseudo, and the
    // scan needs the pseudo table to resolve each `Sym`.
    let mut sites: Vec<(usize, usize, ConstValue)> = Vec::new();
    for (b, bb) in func.blocks.iter().enumerate() {
        for (i, insn) in bb.insns.iter().enumerate() {
            if let Some(v) = foldable_load(func, types, known, insn) {
                sites.push((b, i, v));
            }
        }
    }

    let mut changed = false;
    for (b, i, value) in sites {
        let Some(target) = func.blocks[b].insns[i].target else {
            continue;
        };
        if !func.make_const(target, value) {
            continue;
        }
        let insn = &mut func.blocks[b].insns[i];
        *insn = Instruction {
            op: Opcode::SetVal,
            target: Some(target),
            src: Vec::new(),
            typ: insn.typ,
            size: insn.size,
            ..Default::default()
        };
        changed = true;
    }
    changed
}

/// The constant `insn` reads, if it is a load this may fold.
fn foldable_load(
    func: &Function,
    types: &TypeTable,
    known: &HashMap<String, KnownGlobal>,
    insn: &Instruction,
) -> Option<ConstValue> {
    if insn.op != Opcode::Load || insn.src.len() != 1 || insn.offset != 0 {
        return None;
    }
    let PseudoKind::Sym(name) = &func.get_pseudo(insn.src[0])?.kind else {
        return None;
    };
    let g = known.get(name.as_str())?;

    // The whole object, at its own width. A narrower load is reading part of
    // it, and the initializer is not that part.
    if insn.size != g.size {
        return None;
    }

    // Both sides must agree on which register file the value lives in, or a
    // type pun -- `*(long *)&one` -- would be answered with the bits read the
    // wrong way round. Comparing the *formats* rather than testing one side
    // catches it in both directions, and distinguishes the two 128-bit float
    // formats that a width cannot.
    if types.fp_format(g.typ) != insn.typ.and_then(|t| types.fp_format(t)) {
        return None;
    }

    Some(g.value)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, GlobalDef, Pseudo, PseudoId};
    use crate::target::Target;

    /// A module with one global and a function that loads it whole.
    fn module_loading(global: GlobalDef, types: &TypeTable, load_typ: TypeId) -> Module {
        let size = types.size_bits(global.typ);
        let mut func = Function::new("t", types.int_id);
        func.add_pseudo(Pseudo::sym(PseudoId(0), global.name.clone()));
        func.next_pseudo = 4;

        let mut b0 = BasicBlock::new(BasicBlockId(0));
        b0.add_insn(Instruction::new(Opcode::Entry));
        b0.add_insn(
            Instruction::new(Opcode::Load)
                .with_target(PseudoId(1))
                .with_src(PseudoId(0))
                .with_type_and_size(load_typ, size),
        );
        b0.add_insn(Instruction::ret(Some(PseudoId(1))));
        func.add_block(b0);
        func.entry = BasicBlockId(0);

        let mut module = Module::default();
        module.globals.push(global);
        module.functions.push(func);
        module
    }

    /// The load's opcode after the pass, and the value its target holds.
    fn fold(module: &mut Module, types: &TypeTable) -> (Opcode, Option<ConstValue>) {
        run(module, types);
        let func = &module.functions[0];
        let insn = &func.blocks[0].insns[1];
        let value = func.get_pseudo(PseudoId(1)).and_then(|p| match p.kind {
            PseudoKind::Val(v) => Some(ConstValue::Int(v)),
            PseudoKind::FVal(v) => Some(ConstValue::Float(v)),
            _ => None,
        });
        (insn.op, value)
    }

    fn const_global(name: &str, typ: TypeId, init: Initializer) -> GlobalDef {
        let mut g = GlobalDef::new(name, typ, init);
        g.is_const = true;
        g
    }

    #[test]
    fn a_const_global_load_becomes_its_initializer() {
        let types = TypeTable::new(&Target::host());
        let g = const_global("k", types.int_id, Initializer::Int(5));
        let mut m = module_loading(g, &types, types.int_id);
        assert_eq!(
            fold(&mut m, &types),
            (Opcode::SetVal, Some(ConstValue::Int(5)))
        );
    }

    /// The target becomes the constant and its instruction becomes the
    /// `SetVal` that gives it a width -- the shape a literal is linearized
    /// into, and the only one both allocators resolve.
    #[test]
    fn a_folded_float_global_keeps_the_loads_width() {
        let types = TypeTable::new(&Target::host());
        let g = const_global(
            "f",
            types.float_id,
            Initializer::Float(crate::float::FloatVal::from_f64(2.5)),
        );
        let mut m = module_loading(g, &types, types.float_id);
        let (op, value) = fold(&mut m, &types);
        assert_eq!(op, Opcode::SetVal);
        assert_eq!(
            value,
            Some(ConstValue::Float(crate::float::FloatVal::from_f64(2.5)))
        );
        assert_eq!(m.functions[0].blocks[0].insns[1].size, 32);
    }

    /// Each of these could have its value changed underneath the fold.
    #[test]
    fn the_globals_that_may_change_are_not_folded() {
        let mut types = TypeTable::new(&Target::host());
        let volatile_int = types.intern(crate::types::Type::with_modifiers(
            crate::types::TypeKind::Int,
            TypeModifiers::CONST | TypeModifiers::VOLATILE,
        ));
        let types = types;

        let mut not_const = GlobalDef::new("a", types.int_id, Initializer::Int(5));
        not_const.is_const = false;

        let mut weak = const_global("b", types.int_id, Initializer::Int(5));
        weak.symbol_attrs.weak = true;

        let mut tls = const_global("c", types.int_id, Initializer::Int(5));
        tls.is_thread_local = true;

        let volatile = const_global("d", volatile_int, Initializer::Int(5));
        // A tentative definition may be merged with a real one elsewhere.
        let tentative = const_global("e", types.int_id, Initializer::None);

        for (g, why) in [
            (not_const, "not const"),
            (weak, "weak: replaceable at link time"),
            (tls, "thread-local"),
            (volatile, "volatile"),
            (tentative, "tentative definition"),
        ] {
            let load_typ = g.typ;
            let mut m = module_loading(g, &types, load_typ);
            assert_eq!(fold(&mut m, &types).0, Opcode::Load, "{why}");
        }
    }

    /// `*(long *)&one` reads the same bytes as a different kind of value, and
    /// the initializer is not that value.
    #[test]
    fn a_type_punned_load_is_not_folded() {
        let types = TypeTable::new(&Target::host());

        // A `double` global read as an integer of the same width.
        let g = const_global(
            "one",
            types.double_id,
            Initializer::Float(crate::float::FloatVal::from_f64(1.0)),
        );
        let mut m = module_loading(g, &types, types.long_id);
        assert_eq!(fold(&mut m, &types).0, Opcode::Load, "float read as int");

        // And the other way round.
        let g = const_global("k", types.long_id, Initializer::Int(1));
        let mut m = module_loading(g, &types, types.double_id);
        assert_eq!(fold(&mut m, &types).0, Opcode::Load, "int read as float");
    }

    /// A load narrower than the object is reading part of it.
    #[test]
    fn a_partial_load_is_not_folded() {
        let types = TypeTable::new(&Target::host());
        let g = const_global("k", types.long_id, Initializer::Int(0x1_0000_0005));
        let mut m = module_loading(g, &types, types.long_id);
        // Narrow the load to half the object.
        m.functions[0].blocks[0].insns[1].size = 32;
        m.functions[0].blocks[0].insns[1].typ = Some(types.int_id);
        assert_eq!(fold(&mut m, &types).0, Opcode::Load);
    }
}
