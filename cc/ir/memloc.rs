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

use super::escape::EscapeInfo;
use super::facts::ConstMap;
use super::{Function, GlobalDef, Instruction, Opcode, PseudoId, PseudoKind};
use crate::parse::ast::MemEffect;
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

    /// The bytes accessed, as `[start, end)` from the base, when the base,
    /// the displacement and the width are all known. A width that is not a
    /// whole number of bytes still touches the byte it ends in.
    pub(crate) fn byte_extent(&self) -> Option<(i64, i64)> {
        if !self.is_known() || self.size == 0 {
            return None;
        }
        let start = self.offset?;
        Some((start, start.checked_add(self.size.div_ceil(8) as i64)?))
    }
}

/// Every pseudo's unique defining instruction, and the constant each one
/// carries.
///
/// Sound as a whole-function map with no dominance query, for the same
/// reason `facts::ConstMap` is: SSA invariant I1 makes each target's
/// definition unique, so "the instruction that defines %n" is a fact about
/// the function. Unsound after `lower::lower_module`, which deliberately
/// creates multi-def copies -- hence a value built per pass run, not a field
/// on `Function`.
pub(crate) struct AddrMap {
    defs: HashMap<PseudoId, (usize, usize)>,
    consts: ConstMap,
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
        // An inline-asm output has no one defining instruction (see
        // `Function::asm_defined_pseudos`). Leaving them out of the map makes
        // the walk stop there and answer `Unknown`, which is the safe end.
        for out in func.asm_defined_pseudos() {
            defs.remove(&out);
        }
        AddrMap {
            defs,
            consts: ConstMap::new(func),
        }
    }

    /// The constants this function's pseudos carry, through their copies.
    pub(crate) fn consts(&self) -> &ConstMap {
        &self.consts
    }

    /// The one instruction defining `id`, if this function has it.
    pub(crate) fn def<'f>(&self, func: &'f Function, id: PseudoId) -> Option<&'f Instruction> {
        let (b, i) = *self.defs.get(&id)?;
        func.blocks.get(b)?.insns.get(i)
    }

    /// The width and type of the value `id` holds: what its defining
    /// instruction produces, or for an incoming scalar argument, the type
    /// its parameter is passed as.
    ///
    /// A parameter is a local like any other, whose slot the entry block
    /// fills from an `Arg`. Asking only the defining instruction made every
    /// parameter's value width-less -- no instruction defines an `Arg` -- so
    /// nothing stored into a parameter's slot could be forwarded, while the
    /// same shape on a block-scope local was.
    ///
    /// `None` when neither answers -- a `Sym`, a constant, an inline-asm
    /// output, an aggregate argument -- which is the answer "this pseudo's
    /// width is not a fact of this function".
    pub(crate) fn value_width(
        &self,
        func: &Function,
        types: &TypeTable,
        id: PseudoId,
    ) -> Option<(u32, Option<TypeId>)> {
        if let Some(d) = self.def(func, id) {
            return Some((d.size, d.typ));
        }
        func.arg_value_type(id, types)
            .map(|t| (types.size_bits(t), Some(t)))
    }

    /// The constant a pseudo carries, if any, read as the `width`-bit
    /// operand of the instruction consuming it: an address-arithmetic
    /// operand, or the value of a store.
    ///
    /// `ConstMap::get_at`, sign-extended: address arithmetic is modular at
    /// pointer width, so `p + 0xff..f8` is `p - 8`, and a store's bytes are
    /// the low `width` bits whichever extension holds them.
    pub(crate) fn const_operand(&self, id: PseudoId, width: u32) -> Option<i128> {
        self.consts.get_at(id, width, true)
    }

    /// The location `insn` accesses, for a `Load` or `Store`.
    pub(crate) fn location_of(&self, func: &Function, insn: &Instruction) -> MemLoc {
        let Some(&addr) = insn.src.first() else {
            return MemLoc::unknown();
        };
        self.resolve(func, addr, insn.offset, insn.size, insn.typ)
    }

    /// The whole object `p` points into: its base and how far in `p` is,
    /// with the extent left unknown.
    pub(crate) fn object_of(&self, func: &Function, p: PseudoId) -> MemLoc {
        self.resolve(func, p, 0, 0, None)
    }

    /// What memory `insn` may read and may write.
    ///
    /// **The one table of what an opcode touches.** Every memory pass asks
    /// its question of this answer -- `loadfwd` whether a write may reach a
    /// location, `dse` whether a read may, `effects` whether anything
    /// touched is visible to a caller -- so the passes cannot disagree about
    /// an opcode.
    ///
    /// Fail-closed at `Opcode::may_access_memory`: an opcode outside it
    /// touches nothing, and one inside it that is not named here may touch
    /// anything, so a new memory opcode is conservative by default.
    /// `Instruction::is_memory_barrier` is not this question -- it answers
    /// ordering, and omits `Store`, the block operations, the `Va*` family,
    /// `Alloca` and `StackSave` entirely.
    ///
    /// `callee` says what a `Call` may do; it is asked of nothing else.
    pub(crate) fn access(
        &self,
        func: &Function,
        insn: &Instruction,
        callee: impl FnOnce(&Instruction) -> MemEffect,
    ) -> Access {
        let mut access = self.access_by_opcode(func, insn, callee);
        // A `Sym` target *is* storage, so an instruction that targets one
        // writes the object it names -- a call returning a struct in
        // registers writes its receiving local this way, with no `Store`
        // anywhere. The extent is left unknown because `insn.size` describes
        // a register, not the aggregate.
        if let Some(t) = insn.target {
            if matches!(
                func.get_pseudo(t).map(|p| &p.kind),
                Some(PseudoKind::Sym(_))
            ) {
                access
                    .writes
                    .push(Footprint::Object(self.object_of(func, t)));
            }
        }
        access
    }

    fn access_by_opcode(
        &self,
        func: &Function,
        insn: &Instruction,
        callee: impl FnOnce(&Instruction) -> MemEffect,
    ) -> Access {
        let object = |slot: usize| match insn.src.get(slot) {
            Some(&p) => Footprint::Object(self.object_of(func, p)),
            None => Footprint::Anything,
        };
        match insn.op {
            Opcode::Load => Access {
                reads: vec![Footprint::At(self.location_of(func, insn))],
                writes: Vec::new(),
            },
            // A store reads nothing: the bytes it does not cover stay as
            // they were.
            Opcode::Store => Access {
                reads: Vec::new(),
                writes: vec![Footprint::At(self.location_of(func, insn))],
            },

            // The extent is in the operands; `insn.size` on these is the
            // pointer's width, not the access's. `memset` writes a constant
            // and reads nothing.
            Opcode::Memset => Access {
                reads: Vec::new(),
                writes: vec![object(0)],
            },
            // The destination is counted as read as well as written. Nothing
            // requires it -- an overlapping source is the source's own
            // footprint -- but it is the conservative side, and narrowing it
            // would change which stores `dse` keeps.
            Opcode::Memcpy | Opcode::Memmove => Access {
                reads: vec![object(0), object(1)],
                writes: vec![object(0)],
            },

            // A callee reaches what has escaped, and nothing else: **a call
            // cannot touch a local whose address never left this function**,
            // whatever it does. What its effect adds is that a `pure` or
            // `const` callee writes none of it. Its reads are not narrowed
            // by the effect: no pass needs a `const` callee to read nothing.
            Opcode::Call => Access {
                reads: vec![Footprint::Escaped],
                writes: if callee(insn).may_write() {
                    vec![Footprint::Escaped]
                } else {
                    Vec::new()
                },
            },

            _ if !insn.op.may_access_memory() => Access::default(),

            // `Asm`, `Fence`, `Alloca`, `StackSave`/`StackRestore`, the `Va*`
            // family, `Setjmp`/`Longjmp`, every atomic, and any other memory
            // opcode. A `"memory"` clobber can name a frame slot without
            // naming an operand -- `asm("movl $1, -8(%rbp)")` is legal and
            // reaches a local no analysis saw -- so being blunt here costs
            // nothing and removes the class.
            _ => Access {
                reads: vec![Footprint::Anything],
                writes: vec![Footprint::Anything],
            },
        }
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
                    if let Some(c) = self.const_operand(b, d.size) {
                        let Some(n) = add_offset(off, c) else {
                            return MemLoc::unknown();
                        };
                        off = n;
                        cur = a;
                    } else if let Some(c) = self.const_operand(a, d.size) {
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
                    let Some(c) = self.const_operand(d.src[1], d.size) else {
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

/// The operand slots `op` dereferences in place: memory it reads or writes
/// *through* the pointer there, which it does not keep.
///
/// `AddrMap::access` resolves exactly these slots, and escape analysis
/// counts a use in one of them as an access rather than an escape -- so a
/// pointer an instruction is given anywhere else (the value a `Store`
/// writes, the byte a `Memset` fills with, a length) still escapes.
pub(crate) fn accessed_slots(op: Opcode) -> &'static [usize] {
    match op {
        Opcode::Load | Opcode::Store | Opcode::Memset => &[0],
        Opcode::Memcpy | Opcode::Memmove => &[0, 1],
        _ => &[],
    }
}

/// Somewhere an instruction may read or write.
#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum Footprint {
    /// Exactly these bytes: a `Load` or `Store`, whose own width and
    /// displacement are the access's.
    At(MemLoc),
    /// Somewhere inside the object this location is in, extent unknown: a
    /// block operation, or a `Sym` an instruction targets.
    Object(MemLoc),
    /// Whatever a callee can reach: every global, every unknown address,
    /// and every local whose address left this function.
    Escaped,
    /// Any memory at all, including a local whose address never left.
    Anything,
}

impl Footprint {
    /// May this footprint include any byte of `loc`?
    pub(crate) fn may_touch(&self, loc: &MemLoc, mi: &ModuleInfo, esc: &EscapeInfo) -> bool {
        match self {
            Footprint::At(m) | Footprint::Object(m) => may_alias(m, loc, mi),
            Footprint::Escaped => esc.is_captured(&loc.base),
            Footprint::Anything => true,
        }
    }

    /// Is everything this footprint may include in a local of this function
    /// that nothing outside it can reach?
    pub(crate) fn is_private(&self, esc: &EscapeInfo) -> bool {
        match self {
            Footprint::At(m) | Footprint::Object(m) => {
                matches!(m.base, MemBase::Local(_)) && !esc.is_captured(&m.base)
            }
            Footprint::Escaped | Footprint::Anything => false,
        }
    }
}

/// What one instruction may read and may write.
#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub(crate) struct Access {
    pub(crate) reads: Vec<Footprint>,
    pub(crate) writes: Vec<Footprint>,
}

impl Access {
    /// May this instruction read any byte of `loc`?
    pub(crate) fn may_read(&self, loc: &MemLoc, mi: &ModuleInfo, esc: &EscapeInfo) -> bool {
        self.reads.iter().any(|f| f.may_touch(loc, mi, esc))
    }

    /// May this instruction write any byte of `loc`?
    pub(crate) fn may_write(&self, loc: &MemLoc, mi: &ModuleInfo, esc: &EscapeInfo) -> bool {
        self.writes.iter().any(|f| f.may_touch(loc, mi, esc))
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
    /// What `g`'s definition says. The one place a pass reads these three
    /// flags off a `GlobalDef`.
    pub(crate) fn of(g: &GlobalDef, types: &TypeTable) -> GlobalFacts {
        GlobalFacts {
            // Anywhere inside, not only on the object itself: a
            // `struct { volatile int v; ... }` is not volatile-qualified, but
            // every access to it still has to happen as written (C17 6.7.3p7).
            is_volatile: types.contains_volatile(g.typ),
            // A weak definition exists to be replaced at link time.
            is_weak: g.symbol_attrs.weak,
            // Thread-local storage is per-thread.
            is_thread_local: g.is_thread_local,
        }
    }

    /// Whether every access to this object happens in this program, to one
    /// copy of it, defined here: none of the three flags is set.
    pub(crate) fn is_plain(&self) -> bool {
        !self.is_volatile && !self.is_weak && !self.is_thread_local
    }

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
        let globals = module
            .globals
            .iter()
            .map(|g| (g.name.clone(), GlobalFacts::of(g, types)))
            .collect();
        ModuleInfo {
            globals,
            effects: super::effects::EffectTable::build(module, types),
        }
    }

    pub(crate) fn global(&self, name: &str) -> GlobalFacts {
        self.globals
            .get(name)
            .copied()
            .unwrap_or_else(GlobalFacts::unknown)
    }

    /// What the call `call` may do to memory the caller can observe.
    ///
    /// A call the parser tagged as a library function that only reads
    /// (`LibFn::only_reads`) writes nothing whatever its callee's body says:
    /// its definition in this unit, if any, defines a reserved name, which
    /// is undefined (C17 7.1.3p2).
    pub(crate) fn call_effect(&self, call: &Instruction) -> MemEffect {
        let effect = match call.extra().func_name.as_deref() {
            Some(n) => self.effects.of(n),
            // An indirect call names no callee.
            None => MemEffect::Unknown,
        };
        match call.extra().known {
            Some(f) if f.only_reads() => effect.min(MemEffect::Pure),
            _ => effect,
        }
    }

    /// `AddrMap::access`, with each call's effect from this module.
    pub(crate) fn access(&self, am: &AddrMap, func: &Function, insn: &Instruction) -> Access {
        am.access(func, insn, |call| self.call_effect(call))
    }
}

/// Is the storage `loc` names an ordinary object -- one whose accesses a pass
/// may forward from, merge or delete?
///
/// The one place the question is answered for `loadfwd` and `dse` alike. It
/// is a property of the *object*: an access carries its own answer in
/// `Instruction::is_volatile_access`, which a caller asks as well, but a
/// location must also be refused when the object it lies in is volatile or
/// atomic, or thread-local, or not certainly this translation unit's.
///
/// Volatility is `contains_volatile`, not the top-level qualifier: a
/// `struct { volatile int v; }` read or written whole is a volatile access,
/// though nothing was written on the struct itself (C17 6.7.3p7).
pub(crate) fn is_ordinary_object(
    func: &Function,
    types: &TypeTable,
    mi: &ModuleInfo,
    loc: &MemLoc,
) -> bool {
    if loc.offset.is_none() || loc.size == 0 {
        return false;
    }
    if let Some(t) = loc.typ {
        if types.contains_volatile(t) || types.is_atomic(t) {
            return false;
        }
    }
    match &loc.base {
        MemBase::Unknown => false,
        MemBase::Local(p) => func.local_of(*p).is_some_and(|l| l.is_ordinary(types)),
        MemBase::Global(n) => {
            let g = mi.global(n);
            !g.is_volatile && !g.is_thread_local
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
        _ => match (a.byte_extent(), b.byte_extent()) {
            (Some((ao, aend)), Some((bo, bend))) => ao < bend && bo < aend,
            _ => true,
        },
    }
}

/// Do values of types `a` and `b` live in the same register file -- may one
/// be handed over as the other without a conversion?
///
/// The pun guard: answering a `long` load with a `double` store's value, or
/// with a `double` initializer, hands over bits in the wrong register file.
/// Comparing the *formats* catches it in both directions and distinguishes
/// the two 128-bit float formats that a width cannot, while `int` against
/// `long` passes, which is right -- both are general registers.
///
/// A missing type has no float format, so it counts as a general register:
/// only a floating type names the other file.
pub(crate) fn same_register_file(types: &TypeTable, a: Option<TypeId>, b: Option<TypeId>) -> bool {
    let format = |t: Option<TypeId>| t.and_then(|t| types.fp_format(t));
    format(a) == format(b)
}

/// Are these the same access -- the same bytes, read as the same kind of
/// value (`same_register_file`)?
pub(crate) fn is_same_access(a: &MemLoc, b: &MemLoc, types: &TypeTable) -> bool {
    a.is_known()
        && a.base == b.base
        && a.offset.is_some()
        && a.offset == b.offset
        && a.size == b.size
        && a.size != 0
        && same_register_file(types, a.typ, b.typ)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Pseudo};
    use crate::target::Target;

    /// The extent every same-base comparison reads: a width ending part
    /// way into a byte still touches that byte, and an extent that cannot be
    /// stated is `None` rather than empty.
    #[test]
    fn byte_extent_counts_every_byte_touched() {
        let at = |offset, size| MemLoc {
            base: MemBase::Local(PseudoId(0)),
            offset,
            size,
            typ: None,
        };
        assert_eq!(at(Some(4), 32).byte_extent(), Some((4, 8)));
        assert_eq!(at(Some(4), 3).byte_extent(), Some((4, 5)));
        assert_eq!(at(Some(4), 0).byte_extent(), None, "unknown width");
        assert_eq!(at(None, 32).byte_extent(), None, "unknown offset");
        assert_eq!(at(Some(i64::MAX), 8).byte_extent(), None, "overflow");
        let anywhere = MemLoc {
            offset: Some(0),
            size: 8,
            ..MemLoc::unknown()
        };
        assert_eq!(anywhere.byte_extent(), None, "unknown base");
    }

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
        f.add_local("a.0", PseudoId(0), i32t, None, None);
        f.add_local("b.0", PseudoId(1), i32t, None, None);
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
            effects: super::super::effects::EffectTable::build(
                &super::super::Module::default(),
                &host_types(),
            ),
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

    /// A global whose *member* is `volatile` is volatile storage.
    ///
    /// `GlobalFacts::is_volatile` was `modifiers(g.typ)`, which reports what
    /// was written on the struct -- nothing, for
    /// `struct S { volatile int v; }` -- so `is_ordinary_object`
    /// treated `s` as ordinary and forwarded a load of it across a second
    /// copy. The plain struct beside it is the control.
    #[test]
    fn memloc_a_volatile_member_makes_the_global_volatile() {
        let mut types = host_types();
        let vol_int = types.intern(crate::types::Type::with_modifiers(
            crate::types::TypeKind::Int,
            crate::types::TypeModifiers::VOLATILE,
        ));
        let member = |typ| crate::types::StructMember {
            name: crate::strings::StringId::EMPTY,
            typ,
            offset: 0,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            align: crate::types::MemberAlign::NATURAL,
        };
        let composite = |members| crate::types::CompositeType {
            tag: None,
            members,
            enum_constants: Vec::new(),
            size: 4,
            align: 4,
            member_align: 4,
            is_complete: true,
            transparent: false,
            anon_id: None,
            tag_type: None,
        };
        let vol_struct = types.intern(crate::types::Type::struct_type(composite(vec![member(
            vol_int,
        )])));
        let plain_struct = types.intern(crate::types::Type::struct_type(composite(vec![member(
            types.int_id,
        )])));

        let mut module = super::super::Module::default();
        module.add_global("v", vol_struct, super::super::Initializer::None);
        module.add_global("p", plain_struct, super::super::Initializer::None);
        let mi = ModuleInfo::build(&module, &types);
        assert!(mi.global("v").is_volatile, "a volatile member carries");
        assert!(!mi.global("p").is_volatile, "and a plain struct does not");
    }

    /// `__attribute__((alias))` gives one object two names, so an access
    /// through the alias must overlap every access through the target:
    /// forwarding `b[i] = 1` past `a[i] = 2` into a load of `b[i]` was
    /// gcc.c-torture `alias-2`. The alias owns no `GlobalDef`, so it is
    /// judged as a name this unit does not define.
    #[test]
    fn memloc_alias_may_alias_its_target() {
        let types = host_types();
        let mut module = super::super::Module::default();
        module.add_global("a", types.int_id, super::super::Initializer::None);
        module.add_global("c", types.int_id, super::super::Initializer::None);
        module.aliases.push(super::super::SymbolAlias {
            name: "b".to_string(),
            target: "a".to_string(),
            is_static: false,
            weak: false,
            visibility: None,
        });
        let mi = ModuleInfo::build(&module, &types);
        let at = |n: &str| MemLoc {
            base: MemBase::Global(n.into()),
            offset: Some(0),
            size: 32,
            typ: None,
        };
        assert!(may_alias(&at("a"), &at("b"), &mi));
        assert!(!may_alias(&at("a"), &at("c"), &mi));
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

    /// A call the parser tagged as a library function that only reads
    /// writes nothing, whatever its name's entry says; an untagged call to
    /// the same name, or a tagged one that writes, keeps its entry's effect.
    #[test]
    fn memloc_a_reading_library_call_writes_nothing() {
        use crate::parse::ast::LibFn;
        let types = host_types();
        let mi = empty_module_info();
        let call = |known: Option<LibFn>| {
            let mut c = Instruction::call(None, "f", vec![], vec![], types.int_id, 32);
            c.extra_mut().known = known;
            c
        };
        assert_eq!(mi.call_effect(&call(None)), MemEffect::Unknown);
        assert_eq!(mi.call_effect(&call(Some(LibFn::Strlen))), MemEffect::Pure);
        assert_eq!(mi.call_effect(&call(Some(LibFn::Memcmp))), MemEffect::Pure);
        assert_eq!(
            mi.call_effect(&call(Some(LibFn::Strcpy))),
            MemEffect::Unknown
        );
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
        f.add_local("a.0", PseudoId(0), i64t, None, None);
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
        asm.extra_mut().asm_data = Some(Box::new(crate::ir::AsmData {
            template: String::new(),
            outputs: vec![crate::ir::AsmConstraint::new(
                PseudoId(10),
                "=r",
                crate::target::Arch::X86_64,
                64,
            )],
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

    /// The pun guard compares float formats, so a missing type is a general
    /// register: it matches an integer and not a float.
    #[test]
    fn memloc_same_register_file_reads_float_formats() {
        let types = host_types();
        let (int, long, double) = (types.int_id, types.long_id, types.double_id);
        assert!(same_register_file(&types, Some(int), Some(long)));
        assert!(!same_register_file(&types, Some(long), Some(double)));
        assert!(!same_register_file(
            &types,
            Some(types.float_id),
            Some(double)
        ));
        assert!(same_register_file(&types, None, Some(int)));
        assert!(!same_register_file(&types, None, Some(double)));
        assert!(same_register_file(&types, None, None));
    }

    /// `GlobalFacts::of` reads the three flags off the definition, and a
    /// global is plain only when none is set.
    #[test]
    fn memloc_global_facts_of_a_definition() {
        let types = host_types();
        let int = types.int_id;
        let mut module = super::super::Module::default();
        module.add_global("g", int, super::super::Initializer::None);
        let plain = GlobalFacts::of(&module.globals[0], &types);
        assert!(plain.is_plain());

        let mut weak = module.globals[0].clone();
        weak.symbol_attrs.weak = true;
        let facts = GlobalFacts::of(&weak, &types);
        assert!(facts.is_weak && !facts.is_plain());

        let mut tls = module.globals[0].clone();
        tls.is_thread_local = true;
        let facts = GlobalFacts::of(&tls, &types);
        assert!(facts.is_thread_local && !facts.is_plain());

        assert!(!GlobalFacts::unknown().is_plain());
    }

    /// `two_locals` with one more instruction appended, and what `access`
    /// says of it with every call `callee`.
    fn access_of(insn: Instruction, callee: MemEffect) -> (Access, Function) {
        let (mut f, _types) = two_locals();
        f.blocks[0].add_insn(insn);
        let am = AddrMap::build(&f);
        let last = f.blocks[0].insns.last().unwrap();
        (am.access(&f, last, |_| callee), f)
    }

    fn local_object(p: u32) -> Footprint {
        Footprint::Object(MemLoc {
            base: MemBase::Local(PseudoId(p)),
            offset: Some(0),
            size: 0,
            typ: None,
        })
    }

    fn block_op(op: Opcode, dest: PseudoId, src: PseudoId) -> Instruction {
        Instruction::new(op)
            .with_src3(dest, src, PseudoId(11))
            .with_type_and_size(host_types().void_ptr_id, 64)
    }

    /// The opcode table: a load and a store touch exactly their bytes; a
    /// block operation the objects its operands point into -- `memset`
    /// writing one, `memcpy` and `memmove` reading both and writing the
    /// first.
    #[test]
    fn memloc_access_of_loads_stores_and_block_ops() {
        let (f, _types) = two_locals();
        let am = AddrMap::build(&f);
        let none = |_: &Instruction| MemEffect::Unknown;
        let store = am.access(&f, insn_at(&f, 3), none);
        assert!(store.reads.is_empty());
        assert_eq!(
            store.writes,
            vec![Footprint::At(am.location_of(&f, insn_at(&f, 3)))]
        );
        let load = am.access(&f, insn_at(&f, 4), none);
        assert_eq!(
            load.reads,
            vec![Footprint::At(am.location_of(&f, insn_at(&f, 4)))]
        );
        assert!(load.writes.is_empty());

        // memset(&a, 8, 8): writes a, reads nothing.
        let (set, _) = access_of(
            block_op(Opcode::Memset, PseudoId(10), PseudoId(11)),
            MemEffect::Unknown,
        );
        assert!(set.reads.is_empty());
        assert_eq!(set.writes, vec![local_object(0)]);

        // memcpy(&a, b, 8) and memmove: write a, read both.
        for op in [Opcode::Memcpy, Opcode::Memmove] {
            let (copy, _) = access_of(block_op(op, PseudoId(10), PseudoId(1)), MemEffect::Unknown);
            assert_eq!(copy.reads, vec![local_object(0), local_object(1)], "{op:?}");
            assert_eq!(copy.writes, vec![local_object(0)], "{op:?}");
        }

        // A block op with its destination 8 bytes in is still in `a`, at an
        // unknown extent.
        let (into, _) = access_of(
            block_op(Opcode::Memset, PseudoId(12), PseudoId(11)),
            MemEffect::Unknown,
        );
        let Footprint::Object(m) = &into.writes[0] else {
            panic!("an object footprint");
        };
        assert_eq!(
            (m.base.clone(), m.offset, m.size),
            (MemBase::Local(PseudoId(0)), Some(8), 0)
        );
    }

    /// A call reaches what has escaped: it reads it always, and writes it
    /// unless its effect says it writes nothing. A `Sym` target is a write to
    /// that object whatever the callee is.
    #[test]
    fn memloc_access_of_a_call() {
        let types = host_types();
        let call = |target| Instruction::call(target, "f", vec![], vec![], types.int_id, 32);
        let (dirty, _) = access_of(call(None), MemEffect::Unknown);
        assert_eq!(dirty.reads, vec![Footprint::Escaped]);
        assert_eq!(dirty.writes, vec![Footprint::Escaped]);
        for clean in [MemEffect::Pure, MemEffect::Const] {
            let (a, _) = access_of(call(None), clean);
            assert_eq!(a.reads, vec![Footprint::Escaped], "{clean:?}");
            assert!(a.writes.is_empty(), "{clean:?}");
        }
        let (sret, _) = access_of(call(Some(PseudoId(1))), MemEffect::Const);
        assert_eq!(sret.writes, vec![local_object(1)]);
    }

    /// What the table does not model may touch anything -- an asm, a fence,
    /// an atomic, the `Va*` family, `Alloca` -- and an opcode that reaches no
    /// memory, or a `Ret`, touches nothing.
    #[test]
    fn memloc_access_fails_closed() {
        let anything = Access {
            reads: vec![Footprint::Anything],
            writes: vec![Footprint::Anything],
        };
        for op in [
            Opcode::Asm,
            Opcode::Fence,
            Opcode::AtomicLoad,
            Opcode::AtomicStore,
            Opcode::AtomicFetchAdd,
            Opcode::VaStart,
            Opcode::VaCopy,
            Opcode::Alloca,
            Opcode::StackRestore,
            Opcode::Setjmp,
        ] {
            let insn = Instruction::new(op).with_src(PseudoId(10));
            assert_eq!(access_of(insn, MemEffect::Unknown).0, anything, "{op:?}");
        }
        for op in [Opcode::Ret, Opcode::Add, Opcode::Copy, Opcode::SymAddr] {
            let insn = Instruction::new(op).with_src(PseudoId(10));
            assert_eq!(
                access_of(insn, MemEffect::Unknown).0,
                Access::default(),
                "{op:?}"
            );
        }
    }

    /// `accessed_slots` names exactly the operands `access` resolves, so
    /// escape analysis and the memory passes agree about which pointer an
    /// instruction dereferences.
    #[test]
    fn memloc_accessed_slots_are_what_access_resolves() {
        let (f, _types) = two_locals();
        let am = AddrMap::build(&f);
        let insns = [
            Instruction::load(PseudoId(30), PseudoId(1), 0, host_types().int_id, 32),
            Instruction::store(PseudoId(11), PseudoId(1), 0, host_types().int_id, 32),
            block_op(Opcode::Memset, PseudoId(1), PseudoId(11)),
            block_op(Opcode::Memcpy, PseudoId(1), PseudoId(0)),
            block_op(Opcode::Memmove, PseudoId(1), PseudoId(0)),
        ];
        for insn in &insns {
            let access = am.access(&f, insn, |_| MemEffect::Unknown);
            let mut touched: Vec<MemBase> = access
                .reads
                .iter()
                .chain(&access.writes)
                .map(|fp| match fp {
                    Footprint::At(m) | Footprint::Object(m) => m.base.clone(),
                    other => panic!("{other:?}"),
                })
                .collect();
            touched.sort_by_key(|b| format!("{b:?}"));
            touched.dedup();
            let mut slots: Vec<MemBase> = accessed_slots(insn.op)
                .iter()
                .map(|&s| am.object_of(&f, insn.src[s]).base)
                .collect();
            slots.sort_by_key(|b| format!("{b:?}"));
            assert_eq!(touched, slots, "{:?}", insn.op);
        }
        assert!(accessed_slots(Opcode::Call).is_empty());
        assert!(accessed_slots(Opcode::Asm).is_empty());
    }

    /// A footprint against a location: an exact or object footprint by
    /// `may_alias`; what a callee reaches only what has escaped; and only a
    /// non-escaping local is private.
    #[test]
    fn memloc_footprint_touches_and_privacy() {
        let (mut f, types) = two_locals();
        // `b` escapes: its address goes to a call.
        f.blocks[0].add_insn(Instruction::sym_addr(
            PseudoId(16),
            PseudoId(1),
            types.long_id,
        ));
        f.blocks[0].add_insn(Instruction::call(
            None,
            "g",
            vec![PseudoId(16)],
            vec![types.long_id],
            types.void_id,
            0,
        ));
        let esc = EscapeInfo::analyze(&f, &types);
        let mi = empty_module_info();
        let at = |base: MemBase| MemLoc {
            base,
            offset: Some(0),
            size: 32,
            typ: None,
        };
        let (a, b, g) = (
            at(MemBase::Local(PseudoId(0))),
            at(MemBase::Local(PseudoId(1))),
            at(MemBase::Global("g".into())),
        );

        assert!(local_object(0).may_touch(&a, &mi, &esc));
        assert!(!local_object(0).may_touch(&b, &mi, &esc));
        assert!(!Footprint::Escaped.may_touch(&a, &mi, &esc), "a never left");
        assert!(Footprint::Escaped.may_touch(&b, &mi, &esc));
        assert!(Footprint::Escaped.may_touch(&g, &mi, &esc));
        assert!(Footprint::Anything.may_touch(&a, &mi, &esc));

        assert!(local_object(0).is_private(&esc));
        assert!(Footprint::At(a.clone()).is_private(&esc));
        assert!(!local_object(1).is_private(&esc), "b escaped");
        assert!(!Footprint::At(g).is_private(&esc));
        assert!(!Footprint::Object(MemLoc::unknown()).is_private(&esc));
        assert!(!Footprint::Escaped.is_private(&esc));
        assert!(!Footprint::Anything.is_private(&esc));
    }

    /// `const_operand` answers through `ConstMap`: through a `Copy` or a
    /// `Trunc`, sign-extended at the consumer's width, and not through a
    /// chain narrower than that width.
    #[test]
    fn memloc_const_operand_reads_at_the_consumer_width() {
        let types = host_types();
        let (i8t, i32t, i64t) = (types.char_id, types.int_id, types.long_id);
        let mut f = Function::new("f", types.void_id);
        f.add_pseudo(Pseudo::val(PseudoId(1), -8));
        f.add_pseudo(Pseudo::val(PseudoId(2), 0xffff_fff8));
        f.add_pseudo(Pseudo::val(PseudoId(3), 300));
        f.next_pseudo = 40;

        let insn = |op, target, src, typ, size| {
            Instruction::new(op)
                .with_target(PseudoId(target))
                .with_src(PseudoId(src))
                .with_type_and_size(typ, size)
        };
        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        bb.add_insn(insn(Opcode::Copy, 10, 1, i64t, 64));
        bb.add_insn(insn(Opcode::Copy, 11, 10, i64t, 64));
        bb.add_insn(insn(Opcode::Copy, 12, 2, i32t, 32));
        bb.add_insn(insn(Opcode::Trunc, 13, 3, i8t, 8));
        f.blocks.push(bb);
        f.entry = BasicBlockId(0);

        let am = AddrMap::build(&f);
        assert_eq!(am.const_operand(PseudoId(1), 64), Some(-8));
        assert_eq!(am.const_operand(PseudoId(11), 64), Some(-8), "copies");
        assert_eq!(am.const_operand(PseudoId(12), 32), Some(-8), "at width");
        assert_eq!(am.const_operand(PseudoId(12), 64), None, "narrower chain");
        assert_eq!(am.const_operand(PseudoId(13), 8), Some(44), "a trunc");
        assert_eq!(
            am.const_operand(PseudoId(10), 64),
            am.consts().get_at(PseudoId(10), 64, true)
        );
    }
}
