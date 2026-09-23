//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// What an address is the address *of*, and whether two of them can be the
// same storage.
//
// Until now nothing in this compiler could answer that, and every pass said
// so in a comment: a `Call` is a memory barrier because "c17 has no
// escape/alias analysis". This is that analysis, and the answers it gives
// are deliberately coarse -- a base, a constant byte offset and a width --
// because that is the shape the IR actually produces and the shape a
// forwarding pass needs.
//
// Two things decide correctness here.
//
// **A local is keyed by pseudo, a global by name.** The linearizer emits one
// `Sym` per *reference*, so `static int s` inside `f` is `@f.s.0` reached
// through several distinct pseudos in a single function, and comparing
// addresses by pseudo identity would say two accesses to one object cannot
// alias. A local's `Sym` is unique by construction, and a name is not: a
// parameter can share a name with a global, and a block-scope `extern`
// carries the global's.
//
// **`SymAddr(s)` and `s` are the same address.** A scalar local is loaded
// straight off its `Sym`; an array, a struct or a `va_list` decays through
// `SymAddr` first. Treating those as different is why nothing today can see
// that `i.0` and `symaddr i.0` name one object.
//

use super::{Function, Instruction, Opcode, PseudoId, PseudoKind};
use crate::types::{TypeId, TypeTable};
use std::collections::HashMap;

/// How far the address walk follows a chain before giving up.
const MAX_ADDR_DEPTH: usize = 16;

/// What an address is the address of.
#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum MemBase {
    /// A block-scope local of *this* function, by its `Sym` pseudo.
    Local(PseudoId),
    /// A named object with a linker symbol: a global, a file- or block-scope
    /// static, a string literal, a function.
    Global(String),
    /// A pointer parameter, a loaded pointer, an `alloca`, a phi of
    /// addresses, arithmetic by an unknown amount.
    Unknown,
}

/// One access: a base, a byte displacement, a width and a type.
#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct MemLoc {
    pub(crate) base: MemBase,
    /// `None` when the displacement is not a compile-time constant.
    pub(crate) offset: Option<i64>,
    /// Access width in **bits**, as `Instruction::size` carries it. Zero
    /// means an unknown extent -- a `Memcpy`, a `VaStart`.
    pub(crate) size: u32,
    pub(crate) typ: Option<TypeId>,
}

impl MemLoc {
    /// An access that could be anywhere.
    pub(crate) fn unknown() -> MemLoc {
        MemLoc {
            base: MemBase::Unknown,
            offset: None,
            size: 0,
            typ: None,
        }
    }

    fn is_known(&self) -> bool {
        self.base != MemBase::Unknown
    }
}

/// Every pseudo's unique defining instruction.
///
/// Sound as a whole-function map with no dominance query, for the same
/// reason `facts::ConstMap` is: SSA invariant I1 makes each target's
/// definition unique, so "the instruction that defines %n" is a fact about
/// the function. Unsound after `lower::lower_module`, which deliberately
/// creates multi-def copies -- hence a value built per pass run, not a field
/// on `Function`.
pub(crate) struct AddrMap {
    defs: HashMap<PseudoId, (usize, usize)>,
}

impl AddrMap {
    pub(crate) fn build(func: &Function) -> AddrMap {
        let mut defs = HashMap::new();
        for (b, bb) in func.blocks.iter().enumerate() {
            for (i, insn) in bb.insns.iter().enumerate() {
                if insn.op == Opcode::Nop {
                    continue;
                }
                if let Some(t) = insn.target {
                    defs.insert(t, (b, i));
                }
            }
        }
        // An inline-asm output is the one second definition invariant I1
        // exempts, so for those pseudos "the instruction that defines %n" is
        // not a fact about the function. Leaving them out of the map makes
        // the walk stop there and answer `Unknown`, which is the safe end.
        for bb in &func.blocks {
            for insn in &bb.insns {
                if let Some(ref asm) = insn.asm_data {
                    for out in &asm.outputs {
                        defs.remove(&out.pseudo);
                    }
                }
            }
        }
        AddrMap { defs }
    }

    fn def<'f>(&self, func: &'f Function, id: PseudoId) -> Option<&'f Instruction> {
        let (b, i) = *self.defs.get(&id)?;
        func.blocks.get(b)?.insns.get(i)
    }

    /// The width and type the instruction defining `id` produces.
    ///
    /// `None` when nothing in this function defines it -- an argument, a
    /// `Sym`, a constant, an inline-asm output -- which is the answer "this
    /// pseudo's width is not a fact of this function".
    pub(crate) fn def_width(&self, func: &Function, id: PseudoId) -> Option<(u32, Option<TypeId>)> {
        self.def(func, id).map(|d| (d.size, d.typ))
    }

    /// The constant an address-arithmetic operand carries, if any.
    fn const_operand(&self, func: &Function, id: PseudoId) -> Option<i128> {
        let mut cur = id;
        for _ in 0..MAX_ADDR_DEPTH {
            if let Some(v) = func.const_val(cur) {
                return Some(v);
            }
            match self.def(func, cur) {
                Some(d) if d.op == Opcode::Copy && d.src.len() == 1 => cur = d.src[0],
                Some(d) if d.op == Opcode::SetVal => return func.const_val(d.target?),
                _ => return None,
            }
        }
        None
    }

    /// The location `insn` accesses, for a `Load` or `Store`.
    pub(crate) fn location_of(&self, func: &Function, insn: &Instruction) -> MemLoc {
        let Some(&addr) = insn.src.first() else {
            return MemLoc::unknown();
        };
        self.resolve(func, addr, insn.offset, insn.size, insn.typ)
    }

    /// Resolve an address pseudo to a base and a constant displacement.
    pub(crate) fn resolve(
        &self,
        func: &Function,
        addr: PseudoId,
        offset: i64,
        size: u32,
        typ: Option<TypeId>,
    ) -> MemLoc {
        let mut cur = addr;
        let mut off: i64 = offset;

        for _ in 0..MAX_ADDR_DEPTH {
            // A `Sym` is storage, and is where the walk ends.
            if let Some(PseudoKind::Sym(name)) = func.get_pseudo(cur).map(|p| &p.kind) {
                let base = if func.local_of(cur).is_some() {
                    MemBase::Local(cur)
                } else {
                    MemBase::Global(name.clone())
                };
                return MemLoc {
                    base,
                    offset: Some(off),
                    size,
                    typ,
                };
            }
            let Some(d) = self.def(func, cur) else {
                return MemLoc::unknown();
            };
            match d.op {
                // `SymAddr(s)` *is* the address of `s`.
                Opcode::Copy | Opcode::SymAddr if d.src.len() == 1 => cur = d.src[0],
                Opcode::Add if d.src.len() == 2 => {
                    let (a, b) = (d.src[0], d.src[1]);
                    if let Some(c) = self.const_operand(func, b) {
                        let Some(n) = add_offset(off, c) else {
                            return MemLoc::unknown();
                        };
                        off = n;
                        cur = a;
                    } else if let Some(c) = self.const_operand(func, a) {
                        let Some(n) = add_offset(off, c) else {
                            return MemLoc::unknown();
                        };
                        off = n;
                        cur = b;
                    } else {
                        return MemLoc::unknown();
                    }
                }
                Opcode::Sub if d.src.len() == 2 => {
                    let Some(c) = self.const_operand(func, d.src[1]) else {
                        return MemLoc::unknown();
                    };
                    let Some(n) = c.checked_neg().and_then(|n| add_offset(off, n)) else {
                        return MemLoc::unknown();
                    };
                    off = n;
                    cur = d.src[0];
                }
                // A thread-local's address is per-thread, and `ir::tls` may
                // expand it into a call.
                _ => return MemLoc::unknown(),
            }
        }
        MemLoc::unknown()
    }
}

/// `off + c`, or `None` on overflow -- two distinct offsets must never wrap
/// onto one another, or `may_alias` would call overlapping accesses disjoint.
fn add_offset(off: i64, c: i128) -> Option<i64> {
    let c: i64 = c.try_into().ok()?;
    off.checked_add(c)
}

/// Facts about a named object that a per-function pass cannot reach.
#[derive(Clone, Copy, Debug)]
pub(crate) struct GlobalFacts {
    pub(crate) is_volatile: bool,
    pub(crate) is_weak: bool,
    pub(crate) is_thread_local: bool,
}

impl GlobalFacts {
    /// What is assumed about a name this translation unit does not define.
    ///
    /// Everything. `extern volatile int x;` never reaches `module.globals`,
    /// so a name that is absent must be treated as though it had every
    /// property that would forbid an optimization.
    fn unknown() -> GlobalFacts {
        GlobalFacts {
            is_volatile: true,
            is_weak: true,
            is_thread_local: true,
        }
    }
}

/// Module-wide facts the per-function memory passes need.
pub(crate) struct ModuleInfo {
    globals: HashMap<String, GlobalFacts>,
    effects: super::effects::EffectTable,
}

impl ModuleInfo {
    pub(crate) fn build(module: &super::Module, types: &TypeTable) -> ModuleInfo {
        let mut globals = HashMap::new();
        for g in &module.globals {
            globals.insert(
                g.name.clone(),
                GlobalFacts {
                    is_volatile: types
                        .modifiers(g.typ)
                        .contains(crate::types::TypeModifiers::VOLATILE),
                    is_weak: g.symbol_attrs.weak,
                    is_thread_local: g.is_thread_local,
                },
            );
        }
        ModuleInfo {
            globals,
            effects: super::effects::EffectTable::build(module),
        }
    }

    pub(crate) fn global(&self, name: &str) -> GlobalFacts {
        self.globals
            .get(name)
            .copied()
            .unwrap_or_else(GlobalFacts::unknown)
    }

    /// What a call to `name` may do to memory the caller can observe.
    pub(crate) fn call_effect(&self, name: Option<&str>) -> crate::parse::ast::MemEffect {
        match name {
            Some(n) => self.effects.of(n),
            // An indirect call names no callee.
            None => crate::parse::ast::MemEffect::Unknown,
        }
    }
}

/// Can `a` and `b` be the same storage?
///
/// Answers `true` whenever it cannot prove otherwise, which is the only safe
/// direction: a wrong `false` makes a pass forward a stale value across a
/// write that really did happen.
pub(crate) fn may_alias(a: &MemLoc, b: &MemLoc, mi: &ModuleInfo) -> bool {
    if !a.is_known() || !b.is_known() {
        return true;
    }
    match (&a.base, &b.base) {
        // Two locals are two objects. C provides no way to overlap them.
        (MemBase::Local(p), MemBase::Local(q)) if p != q => false,
        (MemBase::Local(_), MemBase::Global(_)) | (MemBase::Global(_), MemBase::Local(_)) => false,
        (MemBase::Global(n), MemBase::Global(m)) if n != m => {
            // A weak definition exists to be replaced, and the replacement
            // may sit at another symbol's address.
            mi.global(n).is_weak || mi.global(m).is_weak
        }
        // Same base: disjoint only when both extents are known and do not
        // overlap.
        _ => {
            let (Some(ao), Some(bo)) = (a.offset, b.offset) else {
                return true;
            };
            if a.size == 0 || b.size == 0 {
                return true;
            }
            let (abytes, bbytes) = (a.size.div_ceil(8) as i64, b.size.div_ceil(8) as i64);
            let (Some(aend), Some(bend)) = (ao.checked_add(abytes), bo.checked_add(bbytes)) else {
                return true;
            };
            ao < bend && bo < aend
        }
    }
}

/// Are these the same access -- the same bytes, read as the same kind of
/// value?
///
/// The type test is `constglobal`'s pun guard: forwarding a `double` store
/// into a `long` load hands over bits in the wrong register file. Comparing
/// the *formats* catches it in both directions and distinguishes the two
/// 128-bit float formats that a width cannot, while `int` against `long` at
/// one width passes, which is right -- both are general registers.
pub(crate) fn is_same_access(a: &MemLoc, b: &MemLoc, types: &TypeTable) -> bool {
    a.is_known()
        && a.base == b.base
        && a.offset.is_some()
        && a.offset == b.offset
        && a.size == b.size
        && a.size != 0
        && types.fp_format(a.typ.unwrap_or(types.int_id))
            == types.fp_format(b.typ.unwrap_or(types.int_id))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Pseudo};
    use crate::target::Target;

    fn host_types() -> TypeTable {
        TypeTable::new(&Target::host())
    }

    /// One block, two locals, and the address shapes the linearizer emits.
    ///
    /// ```text
    /// %10 = symaddr %0(@a)      ; array local, addressed through symaddr
    /// %11 = setval $8
    /// %12 = add %10, %11
    /// store.32 %12, %20         ; a + 8
    /// %13 = load.32 %0(@a)      ; a + 0, straight off the Sym
    /// %14 = load.32 %1(@b)      ; a different local
    /// %15 = load.32 %2(@g)      ; a global
    /// ```
    fn two_locals() -> (Function, TypeTable) {
        let types = host_types();
        let i32t = types.int_id;
        let mut f = Function::new("f", types.void_id);
        f.add_pseudo(Pseudo::sym(PseudoId(0), "a.0".into()));
        f.add_pseudo(Pseudo::sym(PseudoId(1), "b.0".into()));
        f.add_pseudo(Pseudo::sym(PseudoId(2), "g".into()));
        f.add_pseudo(Pseudo::val(PseudoId(11), 8));
        f.add_pseudo(Pseudo::val(PseudoId(20), 7));
        f.add_local("a.0", PseudoId(0), i32t, false, false, None, None);
        f.add_local("b.0", PseudoId(1), i32t, false, false, None, None);
        f.next_pseudo = 40;

        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        bb.add_insn(Instruction::sym_addr(PseudoId(10), PseudoId(0), i32t));
        bb.add_insn(Instruction::binop(
            Opcode::Add,
            PseudoId(12),
            PseudoId(10),
            PseudoId(11),
            i32t,
            64,
        ));
        bb.add_insn(Instruction::store(PseudoId(20), PseudoId(12), 0, i32t, 32));
        bb.add_insn(Instruction::load(PseudoId(13), PseudoId(0), 0, i32t, 32));
        bb.add_insn(Instruction::load(PseudoId(14), PseudoId(1), 0, i32t, 32));
        bb.add_insn(Instruction::load(PseudoId(15), PseudoId(2), 0, i32t, 32));
        f.blocks.push(bb);
        f.entry = BasicBlockId(0);
        (f, types)
    }

    fn insn_at(f: &Function, i: usize) -> &Instruction {
        &f.blocks[0].insns[i]
    }

    fn empty_module_info() -> ModuleInfo {
        ModuleInfo::build(&super::super::Module::default(), &host_types())
    }

    /// `symaddr s` and `s` name one address, and the constant `Add` under it
    /// is a displacement rather than an unknown.
    #[test]
    fn memloc_symaddr_and_sym_are_one_address() {
        let (f, _types) = two_locals();
        let am = AddrMap::build(&f);
        let store = am.location_of(&f, insn_at(&f, 3));
        let load = am.location_of(&f, insn_at(&f, 4));
        assert_eq!(store.base, MemBase::Local(PseudoId(0)));
        assert_eq!(load.base, MemBase::Local(PseudoId(0)));
        assert_eq!(store.offset, Some(8));
        assert_eq!(load.offset, Some(0));
    }

    /// A local without an entry in `locals` is a global, keyed by its name.
    #[test]
    fn memloc_global_is_keyed_by_name() {
        let (f, _types) = two_locals();
        let am = AddrMap::build(&f);
        let g = am.location_of(&f, insn_at(&f, 6));
        assert_eq!(g.base, MemBase::Global("g".into()));
    }

    /// Two locals are two objects; one local at two disjoint offsets is two
    /// disjoint accesses; overlapping offsets alias.
    #[test]
    fn memloc_may_alias_separates_objects_and_extents() {
        let (f, _types) = two_locals();
        let am = AddrMap::build(&f);
        let mi = empty_module_info();
        let a8 = am.location_of(&f, insn_at(&f, 3));
        let a0 = am.location_of(&f, insn_at(&f, 4));
        let b0 = am.location_of(&f, insn_at(&f, 5));
        let g0 = am.location_of(&f, insn_at(&f, 6));

        assert!(!may_alias(&a0, &b0, &mi), "two locals are two objects");
        assert!(!may_alias(&a0, &g0, &mi), "a local is never a global");
        assert!(!may_alias(&a0, &a8, &mi), "0..4 and 8..12 are disjoint");
        assert!(may_alias(&a0, &a0, &mi));

        // An overlap by one byte still aliases.
        let mut a3 = a0.clone();
        a3.offset = Some(3);
        assert!(may_alias(&a0, &a3, &mi));
    }

    /// An unknown base aliases everything, and so does an unknown extent --
    /// the direction a wrong answer must never take.
    #[test]
    fn memloc_unknown_aliases_everything() {
        let (f, _types) = two_locals();
        let am = AddrMap::build(&f);
        let mi = empty_module_info();
        let a0 = am.location_of(&f, insn_at(&f, 4));
        assert!(may_alias(&a0, &MemLoc::unknown(), &mi));
        assert!(may_alias(&MemLoc::unknown(), &a0, &mi));

        let mut sizeless = a0.clone();
        sizeless.size = 0;
        assert!(may_alias(&a0, &sizeless, &mi));

        let mut no_offset = a0.clone();
        no_offset.offset = None;
        assert!(may_alias(&a0, &no_offset, &mi));
    }

    /// A weak definition exists to be replaced, and the replacement may sit
    /// at another symbol's address -- so two *differently named* globals can
    /// still be one object.
    #[test]
    fn memloc_weak_globals_may_alias() {
        let strong = GlobalFacts {
            is_volatile: false,
            is_weak: false,
            is_thread_local: false,
        };
        let weak = GlobalFacts {
            is_weak: true,
            ..strong
        };
        let mut globals = HashMap::new();
        globals.insert("g".to_string(), strong);
        globals.insert("h".to_string(), strong);
        globals.insert("w".to_string(), weak);
        let mi = ModuleInfo {
            globals,
            effects: super::super::effects::EffectTable::build(&super::super::Module::default()),
        };

        let at = |n: &str| MemLoc {
            base: MemBase::Global(n.into()),
            offset: Some(0),
            size: 32,
            typ: None,
        };
        assert!(!may_alias(&at("g"), &at("h"), &mi));
        assert!(may_alias(&at("g"), &at("w"), &mi));
    }

    /// A name this translation unit does not define is assumed to be
    /// everything that would forbid an optimization.
    #[test]
    fn memloc_unknown_global_is_assumed_hostile() {
        let mi = empty_module_info();
        let g = mi.global("nowhere");
        assert!(g.is_volatile && g.is_weak && g.is_thread_local);
    }

    /// An offset that overflows must not wrap onto another one, or
    /// `may_alias` would call overlapping accesses disjoint.
    #[test]
    fn memloc_offset_overflow_is_unknown() {
        assert_eq!(add_offset(1, 2), Some(3));
        assert_eq!(add_offset(i64::MAX, 1), None);
        assert_eq!(add_offset(0, i128::from(i64::MAX) + 1), None);
    }

    /// Same bytes read as the same kind of value. A `double` and a `long` at
    /// one width are *not* the same access: they live in different register
    /// files.
    #[test]
    fn memloc_same_access_rejects_a_pun() {
        let types = host_types();
        let at = |t: TypeId| MemLoc {
            base: MemBase::Local(PseudoId(0)),
            offset: Some(0),
            size: 64,
            typ: Some(t),
        };
        assert!(is_same_access(
            &at(types.long_id),
            &at(types.long_id),
            &types
        ));
        assert!(!is_same_access(
            &at(types.long_id),
            &at(types.double_id),
            &types
        ));

        // Different offsets, or a zero extent, are never the same access.
        let mut shifted = at(types.long_id);
        shifted.offset = Some(1);
        assert!(!is_same_access(&at(types.long_id), &shifted, &types));
        let mut sizeless = at(types.long_id);
        sizeless.size = 0;
        assert!(!is_same_access(&sizeless, &sizeless, &types));
    }

    /// An inline-asm output is the one second definition invariant I1
    /// exempts, so the walk must stop at it rather than follow the tied
    /// operand's `Copy` past the asm's write.
    #[test]
    fn memloc_asm_output_is_not_a_definition() {
        let types = host_types();
        let i64t = types.long_id;
        let mut f = Function::new("f", types.void_id);
        f.add_pseudo(Pseudo::sym(PseudoId(0), "a.0".into()));
        f.add_local("a.0", PseudoId(0), i64t, false, false, None, None);
        f.next_pseudo = 40;

        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        // %10 = copy %0 -- the tied-operand setup, before the asm.
        bb.add_insn(
            Instruction::new(Opcode::Copy)
                .with_target(PseudoId(10))
                .with_src(PseudoId(0))
                .with_type_and_size(i64t, 64),
        );
        let mut asm = Instruction::new(Opcode::Asm);
        asm.asm_data = Some(Box::new(crate::ir::AsmData {
            template: String::new(),
            outputs: vec![crate::ir::AsmConstraint {
                pseudo: PseudoId(10),
                name: None,
                matching_output: None,
                constraint: "=r".into(),
                size: 64,
            }],
            inputs: vec![],
            clobbers: vec![],
            goto_labels: vec![],
        }));
        bb.add_insn(asm);
        bb.add_insn(Instruction::load(PseudoId(11), PseudoId(10), 0, i64t, 64));
        f.blocks.push(bb);
        f.entry = BasicBlockId(0);

        let am = AddrMap::build(&f);
        let loc = am.location_of(&f, &f.blocks[0].insns[3]);
        assert_eq!(
            loc.base,
            MemBase::Unknown,
            "the asm redefines %10, so its pre-asm Copy says nothing about it"
        );
    }
}
