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

use super::memloc::{AddrMap, MemBase};
use super::{ConstValue, Function, Initializer, Instruction, Module, Opcode};
use crate::types::{TypeId, TypeTable};
use std::collections::HashMap;

/// A global whose value is known for the whole run.
struct KnownGlobal {
    value: ConstValue,
    typ: TypeId,
    size: u32,
}

/// The globals whose value is known for the whole run, by name: a fact about
/// the module, gathered once and read by every function.
pub struct KnownGlobals(HashMap<String, KnownGlobal>);

/// Replace every load of a `const` global in `func` with its initializer.
/// Returns whether anything changed.
///
/// A load is matched to its global by the address it reads, resolved the way
/// every memory pass resolves one, so a load through `&k` folds too. That is
/// why this runs in the optimizer's loop rather than once ahead of it: an
/// address resolves only once `instcombine` has folded the arithmetic that
/// forms it.
pub fn run(func: &mut Function, types: &TypeTable, known: &KnownGlobals) -> bool {
    !known.0.is_empty() && propagate(func, types, &known.0)
}

impl KnownGlobals {
    pub fn collect(module: &Module, types: &TypeTable) -> KnownGlobals {
        KnownGlobals(collect(module, types))
    }
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
pub(crate) fn qualifies(g: &super::GlobalDef, types: &TypeTable) -> bool {
    // Not `const`: an ordinary store may change it.
    if !g.is_const {
        return false;
    }
    // `volatile` says the value can change for reasons not in the program,
    // which is exactly the assumption being made here.
    //
    // `contains_volatile`, not the top-level modifier: a `const struct` with a
    // `volatile` member is one of these objects too, and asking only what was
    // written on the struct let it through the gate. Every access is checked
    // again below, so this was not reachable as a wrong fold -- but the object
    // and its members are one question and get one spelling of it.
    if types.contains_volatile(g.typ) {
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
    let am = AddrMap::build(func);
    let mut sites: Vec<(usize, usize, ConstValue)> = Vec::new();
    for (b, bb) in func.blocks.iter().enumerate() {
        for (i, insn) in bb.insns.iter().enumerate() {
            if let Some(v) = foldable_load(func, &am, types, known, insn) {
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
    am: &AddrMap,
    types: &TypeTable,
    known: &HashMap<String, KnownGlobal>,
    insn: &Instruction,
) -> Option<ConstValue> {
    if insn.op != Opcode::Load || insn.src.len() != 1 {
        return None;
    }
    // The access itself is observable, whatever the object's initializer says
    // it holds: `const volatile int t = 0;` -- a hardware status word, a
    // linker-set value -- must still be read. `qualifies` declines a global
    // written `volatile`, but the qualifier can also be on the *access*, as in
    // `*(volatile const int *)&t`, and only the instruction knows that.
    if insn.is_volatile_access() {
        return None;
    }
    // The start of the object: a member or an element is not the
    // initializer, which is the whole object's value.
    let loc = am.resolve(func, insn.src[0], insn.offset, insn.size, insn.typ);
    let (MemBase::Global(name), Some(0)) = (&loc.base, loc.offset) else {
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
    use crate::ir::{BasicBlock, BasicBlockId, GlobalDef, Pseudo, PseudoId, PseudoKind};
    use crate::target::Target;
    use crate::types::TypeModifiers;

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
        let known = KnownGlobals::collect(module, types);
        run(&mut module.functions[0], types, &known);
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

    /// `module_loading`, with the load reading through `&global` at `offset`.
    fn module_loading_through_address(global: GlobalDef, types: &TypeTable, offset: i64) -> Module {
        let mut m = module_loading(global, types, types.int_id);
        let b0 = &mut m.functions[0].blocks[0];
        b0.insns[1].src = vec![PseudoId(2)];
        b0.insns[1].offset = offset;
        b0.insns.insert(
            1,
            Instruction::sym_addr(PseudoId(2), PseudoId(0), types.int_id),
        );
        m
    }

    /// The address a load reads is resolved the way every memory pass
    /// resolves one: through `&k` is still `k`.
    #[test]
    fn a_load_through_the_globals_address_folds() {
        let types = TypeTable::new(&Target::host());
        let g = const_global("k", types.int_id, Initializer::Int(5));
        let mut m = module_loading_through_address(g, &types, 0);
        let known = KnownGlobals::collect(&m, &types);
        assert!(run(&mut m.functions[0], &types, &known));
        assert_eq!(m.functions[0].blocks[0].insns[2].op, Opcode::SetVal);
    }

    /// Four bytes in is not the object's start, whose value the initializer
    /// is.
    #[test]
    fn a_load_past_the_globals_start_is_left_alone() {
        let types = TypeTable::new(&Target::host());
        let g = const_global("k", types.long_id, Initializer::Int(5));
        let mut m = module_loading_through_address(g, &types, 4);
        let known = KnownGlobals::collect(&m, &types);
        assert!(!run(&mut m.functions[0], &types, &known));
        assert_eq!(m.functions[0].blocks[0].insns[2].op, Opcode::Load);
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
