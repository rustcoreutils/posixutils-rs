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
// Unlike `loadfwd`, which forwards a value the function itself stored and so
// needs alias and escape analysis, this needs neither, because C says the
// answer outright:
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
// A byte of an object whose bytes are all known for the whole run -- a
// string literal (C17 6.4.5p7) or a `const` `char` array -- folds the same
// way and for the same reason: `"hi"[0]` is `'h'`, as gcc folds it, and
// that is what makes `__builtin_constant_p("hi"[0])` 1.
//
// What it declines is as load-bearing as what it folds, and each refusal
// below names the thing that could otherwise change the value underneath
// it: an `extern` declaration whose definition is in another translation
// unit, a tentative definition that may be merged with one, a weak
// definition that exists to be replaced, and `volatile`, which is a
// standing instruction that the value can change for reasons the program
// cannot see.
//

use super::constfold::at_width;
use super::memloc::{same_register_file, AddrMap, GlobalFacts, MemBase, MemLoc};
use super::propagate;
use super::strdata::ConstBytes;
use super::{ConstValue, Function, Initializer, Instruction, Module, Opcode};
use crate::types::{TypeId, TypeKind, TypeTable};
use std::collections::HashMap;

/// A global whose value is known for the whole run.
struct KnownGlobal {
    value: ConstValue,
    typ: TypeId,
    size: u32,
}

/// The globals whose value is known for the whole run, by name: a fact about
/// the module, gathered once and read by every function.
pub struct KnownGlobals {
    /// The scalars, whose initializer is the value of the whole object.
    scalars: HashMap<String, KnownGlobal>,
    /// The objects whose every byte is known.
    bytes: ConstBytes,
}

/// Replace every load of a `const` global in `func` with its initializer,
/// and every byte load of a known byte object with the byte.
/// Returns whether anything changed.
///
/// A load is matched to its global by the address it reads, resolved the way
/// every memory pass resolves one, so a load through `&k` folds too. That is
/// why this runs in the optimizer's loop rather than once ahead of it: an
/// address resolves only once `instcombine` has folded the arithmetic that
/// forms it.
pub fn run(func: &mut Function, types: &TypeTable, known: &KnownGlobals) -> bool {
    !(known.scalars.is_empty() && known.bytes.is_empty()) && propagate(func, types, known)
}

impl KnownGlobals {
    pub fn collect(module: &Module, types: &TypeTable) -> KnownGlobals {
        KnownGlobals {
            scalars: collect(module, types),
            bytes: ConstBytes::build(module, types),
        }
    }

    /// The objects whose every byte is known, which `libcall_fold` reads
    /// strings out of.
    pub(crate) fn bytes(&self) -> &ConstBytes {
        &self.bytes
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
    // `volatile` -- anywhere inside, so a `const struct` with a `volatile`
    // member too -- says the value can change for reasons not in the
    // program, which is exactly the assumption being made here. A weak
    // definition's replacement has another initializer (gcc folds these
    // anyway; declining costs a fold and cannot be wrong). And this pass has
    // no notion of which thread's copy of a thread-local a load reads.
    if !GlobalFacts::of(g, types).is_plain() {
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
fn propagate(func: &mut Function, types: &TypeTable, known: &KnownGlobals) -> bool {
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
        changed |= propagate::fold_target_to_setval(func, (b, i), value);
    }
    changed
}

/// The constant `insn` reads, if it is a load this may fold.
fn foldable_load(
    func: &Function,
    am: &AddrMap,
    types: &TypeTable,
    known: &KnownGlobals,
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
    let loc = am.resolve(func, insn.src[0], insn.offset, insn.size, insn.typ);
    scalar_load(types, &known.scalars, insn, &loc)
        .or_else(|| byte_load(types, &known.bytes, insn, &loc))
}

/// The initializer of the scalar `insn` loads whole from `loc`.
fn scalar_load(
    types: &TypeTable,
    scalars: &HashMap<String, KnownGlobal>,
    insn: &Instruction,
    loc: &MemLoc,
) -> Option<ConstValue> {
    // The start of the object: a member or an element is not the
    // initializer, which is the whole object's value.
    let (MemBase::Global(name), Some(0)) = (&loc.base, loc.offset) else {
        return None;
    };
    let g = scalars.get(name.as_str())?;

    // The whole object, at its own width. A narrower load is reading part of
    // it, and the initializer is not that part.
    if insn.size != g.size {
        return None;
    }

    // A type pun -- `*(long *)&one` -- would be answered with the bits read
    // the wrong way round.
    if !same_register_file(types, Some(g.typ), insn.typ) {
        return None;
    }

    Some(g.value)
}

/// The byte `insn` loads from `loc`, a known byte object, read as the
/// integer type the load has.
///
/// One byte, so that no byte order is involved; a wider load out of a
/// string is a type pun this leaves alone. A `_Bool` is not an integer any
/// byte is a value of.
fn byte_load(
    types: &TypeTable,
    bytes: &ConstBytes,
    insn: &Instruction,
    loc: &MemLoc,
) -> Option<ConstValue> {
    let typ = insn.typ?;
    if insn.size != 8 || !types.is_integer(typ) || types.kind(typ) == TypeKind::Bool {
        return None;
    }
    let MemBase::Global(name) = &loc.base else {
        return None;
    };
    let at = usize::try_from(loc.offset?).ok()?;
    let byte = *bytes.object(name)?.get(at)?;
    Some(ConstValue::Int(at_width(
        i128::from(byte),
        8,
        !types.is_unsigned(typ),
    )))
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

    /// A module whose function loads one `typ` of `size` bits, `offset`
    /// bytes into the string literal `.LC0`, which holds `bytes`.
    fn module_loading_a_string(
        types: &TypeTable,
        bytes: &str,
        typ: TypeId,
        size: u32,
        offset: i64,
    ) -> Module {
        let mut func = Function::new("t", types.int_id);
        func.add_pseudo(Pseudo::sym(PseudoId(0), ".LC0".to_string()));
        func.next_pseudo = 4;
        let mut b0 = BasicBlock::new(BasicBlockId(0));
        b0.add_insn(Instruction::new(Opcode::Entry));
        b0.add_insn(Instruction::sym_addr(
            PseudoId(2),
            PseudoId(0),
            types.char_id,
        ));
        let mut load = Instruction::new(Opcode::Load)
            .with_target(PseudoId(1))
            .with_src(PseudoId(2))
            .with_type_and_size(typ, size);
        load.offset = offset;
        b0.add_insn(load);
        b0.add_insn(Instruction::ret(Some(PseudoId(1))));
        func.add_block(b0);
        func.entry = BasicBlockId(0);
        let mut module = Module::default();
        module.strings.push((".LC0".to_string(), bytes.to_string()));
        module.functions.push(func);
        module
    }

    /// The value a byte load out of `.LC0` folded to, or `None`.
    fn string_byte(
        types: &TypeTable,
        bytes: &str,
        typ: TypeId,
        size: u32,
        at: i64,
    ) -> Option<i128> {
        let mut m = module_loading_a_string(types, bytes, typ, size, at);
        let known = KnownGlobals::collect(&m, types);
        run(&mut m.functions[0], types, &known);
        let func = &m.functions[0];
        (func.blocks[0].insns[2].op == Opcode::SetVal)
            .then(|| func.const_val(PseudoId(1)))
            .flatten()
    }

    /// A byte of a string literal is the byte: `"hi"[0]` is `'h'`, read as
    /// the load's type -- and the terminator is a byte of it too.
    #[test]
    fn a_byte_of_a_string_literal_folds() {
        for target in [
            Target::new(crate::target::Arch::X86_64, crate::target::Os::Linux),
            Target::new(crate::target::Arch::Aarch64, crate::target::Os::Linux),
        ] {
            let types = TypeTable::new(&target);
            let (sc, uc) = (types.schar_id, types.uchar_id);
            assert_eq!(string_byte(&types, "hi", sc, 8, 0), Some(i128::from(b'h')));
            assert_eq!(string_byte(&types, "hi", sc, 8, 1), Some(i128::from(b'i')));
            assert_eq!(string_byte(&types, "hi", sc, 8, 2), Some(0));
            // `\xff` as a signed and as an unsigned `char`.
            assert_eq!(string_byte(&types, "\u{ff}", sc, 8, 0), Some(-1));
            assert_eq!(string_byte(&types, "\u{ff}", uc, 8, 0), Some(255));
        }
    }

    /// Past the object, a wider load (a pun whose answer would need a byte
    /// order) and a `_Bool` are all left alone.
    #[test]
    fn a_string_load_that_is_not_one_known_byte_is_left_alone() {
        let types = TypeTable::new(&Target::new(
            crate::target::Arch::X86_64,
            crate::target::Os::Linux,
        ));
        assert_eq!(string_byte(&types, "hi", types.schar_id, 8, 3), None);
        assert_eq!(string_byte(&types, "hi", types.schar_id, 8, -1), None);
        assert_eq!(string_byte(&types, "hi", types.short_id, 16, 0), None);
        assert_eq!(string_byte(&types, "hi", types.bool_id, 8, 0), None);
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
