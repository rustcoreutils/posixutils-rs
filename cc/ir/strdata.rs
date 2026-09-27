//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The bytes of the objects whose contents are known for the whole run, and
// the strings a pointer into one of them reads
//
// Two kinds of object qualify. A string literal: modifying one is undefined
// (C17 6.4.5p7). And a `char` array defined `const` in this translation unit
// with an initializer, by the rule `constglobal` applies to a scalar -- a
// store to it is undefined too (C17 6.7.3p6), unless it is weak, `volatile`,
// thread-local or tentative. A local array is neither, however it was
// initialized: its bytes are ordinary stores, which the library calls that
// read it cannot see past.
//
// A pointer reads a known string when `AddrMap` resolves it to one of these
// objects at a constant offset inside it. `string_len` goes further, for
// `strlen` alone: through a `Select` or a phi whose every arm has one length,
// and through `p + i` for an `i` it does not know, when the object is one
// string that fills it -- then the length is the whole string's less `i`.
//

use super::constglobal;
use super::memloc::{AddrMap, MemBase};
use super::{Function, GlobalDef, Initializer, Module, Opcode, PseudoId};
use crate::token::lexer::payload_bytes;
use crate::types::{TypeKind, TypeModifiers, TypeTable};
use std::collections::{HashMap, HashSet};

/// How many definitions `string_len` follows before it gives up.
const MAX_WALK: usize = 64;

/// The bytes of every object whose contents are known for the whole run,
/// by symbol.
pub(crate) struct ConstBytes {
    objects: HashMap<String, Vec<u8>>,
}

impl ConstBytes {
    pub(crate) fn build(module: &Module, types: &TypeTable) -> ConstBytes {
        let mut objects = HashMap::new();
        for (label, payload) in &module.strings {
            let mut bytes: Vec<u8> = payload_bytes(payload).collect();
            bytes.push(0);
            objects.insert(label.clone(), bytes);
        }
        for g in &module.globals {
            if let Some(bytes) = const_char_array(g, types) {
                objects.insert(g.name.clone(), bytes);
            }
        }
        ConstBytes { objects }
    }
}

/// The bytes of `g`, when it is a `char` array whose initializer holds for
/// the whole run: the initializer, zero-filled to the array's size.
fn const_char_array(g: &GlobalDef, types: &TypeTable) -> Option<Vec<u8>> {
    if !constglobal::qualifies(g, types) || types.kind(g.typ) != TypeKind::Array {
        return None;
    }
    let elem = types.base_type(g.typ)?;
    // `qualifies` asks the array; a `volatile` element is as volatile.
    if types.size_bytes(elem) != 1 || types.modifiers(elem).contains(TypeModifiers::VOLATILE) {
        return None;
    }
    let mut bytes = vec![0u8; types.size_bytes(g.typ)];
    match &g.init {
        // `char s[3] = "abc"` keeps no terminator: the string is cut to the
        // array, never the array stretched to the string.
        Initializer::String(s) => {
            for (b, v) in bytes.iter_mut().zip(payload_bytes(s)) {
                *b = v;
            }
        }
        Initializer::Array {
            elem_size: 1,
            elements,
            ..
        } => {
            for (at, init) in elements {
                let Initializer::Int(v) = init else {
                    return None;
                };
                // The byte of the value, whichever `char` it initializes.
                *bytes.get_mut(*at)? = *v as u8;
            }
        }
        _ => return None,
    }
    (!bytes.is_empty()).then_some(bytes)
}

/// The bytes a pointer into a known object reads: from it to the object's
/// end.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct StrRef<'a> {
    rest: &'a [u8],
}

impl<'a> StrRef<'a> {
    /// The C string here, without its terminator; `None` if the object ends
    /// first.
    pub(crate) fn c_str(self) -> Option<&'a [u8]> {
        let len = self.rest.iter().position(|&b| b == 0)?;
        Some(&self.rest[..len])
    }

    /// Every byte from here to the end of the object.
    pub(crate) fn bytes(self) -> &'a [u8] {
        self.rest
    }
}

/// The length of the string a pointer reads, as far as it is known.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Len {
    /// This many bytes.
    Const(u64),
    /// `len - var`: the pointer is `var` bytes into a string of `len`.
    MinusOffset { len: u64, var: PseudoId },
}

/// What one arm of a walk found.
enum Walk {
    Len(Len),
    /// Nothing: the arm comes back around to a phi already being asked
    /// about, so it can only carry a value one of the other arms brings.
    Cycle,
}

/// Reads the known strings `func`'s pointers point at.
pub(crate) struct StrReader<'a> {
    data: &'a ConstBytes,
    func: &'a Function,
    am: &'a AddrMap,
}

impl<'a> StrReader<'a> {
    pub(crate) fn new(data: &'a ConstBytes, func: &'a Function, am: &'a AddrMap) -> Self {
        StrReader { data, func, am }
    }

    /// The object `p` points into, and how far into it, when it is known and
    /// `p` is inside it.
    fn object_at(&self, p: PseudoId) -> Option<(&'a [u8], usize)> {
        let loc = self.am.resolve(self.func, p, 0, 0, None);
        let MemBase::Global(name) = loc.base else {
            return None;
        };
        let bytes = self.data.objects.get(&name)?;
        let at = usize::try_from(loc.offset?).ok()?;
        (at < bytes.len()).then_some((bytes.as_slice(), at))
    }

    /// What `p` points at, when it is inside a known object.
    pub(crate) fn string_at(&self, p: PseudoId) -> Option<StrRef<'a>> {
        let (bytes, at) = self.object_at(p)?;
        Some(StrRef { rest: &bytes[at..] })
    }

    /// The length of the string `p` points at, when every value `p` can
    /// have gives the same one.
    pub(crate) fn string_len(&self, p: PseudoId) -> Option<Len> {
        let mut budget = MAX_WALK;
        match self.walk(p, &mut HashSet::new(), &mut budget)? {
            Walk::Len(len) => Some(len),
            Walk::Cycle => None,
        }
    }

    fn walk(&self, p: PseudoId, open: &mut HashSet<PseudoId>, budget: &mut usize) -> Option<Walk> {
        *budget = budget.checked_sub(1)?;
        if let Some(s) = self.string_at(p) {
            return Some(Walk::Len(Len::Const(s.c_str()?.len() as u64)));
        }
        // An address with a place -- a local, or outside the object -- is
        // not a string this knows, whatever its arithmetic looks like.
        if self.am.resolve(self.func, p, 0, 0, None).base != MemBase::Unknown {
            return None;
        }
        let def = self.am.def(self.func, p)?;
        match (def.op, def.src.as_slice()) {
            (Opcode::Copy | Opcode::PhiSource, &[src]) => self.walk(src, open, budget),
            (Opcode::Select, &[_, a, b]) => self.join(&[a, b], open, budget),
            (Opcode::Phi, _) => {
                if !open.insert(p) {
                    return Some(Walk::Cycle);
                }
                let arms: Vec<PseudoId> = def.phi_list.iter().map(|&(_, v)| v).collect();
                let joined = self.join(&arms, open, budget);
                open.remove(&p);
                joined
            }
            (Opcode::Add, &[a, b]) if def.size == 64 => {
                self.minus_offset(a, b).or_else(|| self.minus_offset(b, a))
            }
            _ => None,
        }
    }

    /// The one length every arm gives, ignoring the arms that only cycle.
    fn join(
        &self,
        arms: &[PseudoId],
        open: &mut HashSet<PseudoId>,
        budget: &mut usize,
    ) -> Option<Walk> {
        let mut found: Option<Len> = None;
        for &arm in arms {
            match self.walk(arm, open, budget)? {
                Walk::Cycle => {}
                Walk::Len(len) if found.is_none_or(|f| f == len) => found = Some(len),
                Walk::Len(_) => return None,
            }
        }
        Some(found.map_or(Walk::Cycle, Walk::Len))
    }

    /// `base + var`, when `base` points into a known object that holds one
    /// string ending exactly at its last byte: whatever `var` is, if the
    /// sum is still inside the string the length is the rest less `var`,
    /// and if it is not the program is undefined. An embedded NUL makes the
    /// answer depend on which side of it `var` lands, so it refuses.
    fn minus_offset(&self, base: PseudoId, var: PseudoId) -> Option<Walk> {
        let rest = self.string_at(base)?.bytes();
        let len = rest.len() - 1;
        (rest.iter().position(|&b| b == 0) == Some(len)).then_some(Walk::Len(Len::MinusOffset {
            len: len as u64,
            var,
        }))
    }
}

/// A module of one function, one block, built an instruction at a time, for
/// the tests here and in `libcall_fold`. Nothing about the block is a real
/// control flow graph: the passes under test read definitions, not paths.
#[cfg(test)]
pub(crate) mod fixture {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Instruction, Pseudo};
    use crate::target::Target;
    use crate::types::{Type, TypeId};

    pub(crate) struct Fixture {
        pub(crate) module: Module,
        pub(crate) types: TypeTable,
    }

    impl Fixture {
        pub(crate) fn new() -> Self {
            let types = TypeTable::new(&Target::host());
            let mut module = Module::default();
            let mut f = Function::new("f", types.int_id);
            f.add_block(BasicBlock::new(BasicBlockId(0)));
            module.functions.push(f);
            Fixture { module, types }
        }

        pub(crate) fn func(&mut self) -> &mut Function {
            &mut self.module.functions[0]
        }

        pub(crate) fn push(&mut self, insn: Instruction) {
            self.func().blocks[0].insns.push(insn);
        }

        /// A string literal with these bytes, and its label.
        pub(crate) fn literal(&mut self, text: &str) -> String {
            self.module.add_string(text.to_string())
        }

        /// A `const char name[size]` initialized by `init`, adjusted by
        /// `tweak`.
        pub(crate) fn const_array(
            &mut self,
            name: &str,
            size: usize,
            init: Initializer,
            tweak: impl FnOnce(&mut GlobalDef),
        ) {
            let arr = self.types.intern(Type::array(self.types.char_id, size));
            let mut g = GlobalDef::new(name, arr, init);
            g.is_const = true;
            tweak(&mut g);
            self.module.globals.push(g);
        }

        /// The address of the symbol `name`.
        pub(crate) fn addr(&mut self, name: &str) -> PseudoId {
            let f = self.func();
            let sym = f.alloc_pseudo();
            f.add_pseudo(Pseudo::sym(sym, name.to_string()));
            let p = f.alloc_pseudo();
            let ptr = self.types.char_ptr_id;
            self.push(Instruction::sym_addr(p, sym, ptr));
            p
        }

        pub(crate) fn konst(&mut self, v: i128) -> PseudoId {
            self.func().create_const_pseudo(v)
        }

        /// A value nothing here defines: an argument, as far as any pass
        /// can tell.
        pub(crate) fn unknown(&mut self) -> PseudoId {
            let f = self.func();
            let p = f.alloc_pseudo();
            f.add_pseudo(Pseudo::arg(p, 0));
            p
        }

        pub(crate) fn op(&mut self, op: Opcode, a: PseudoId, b: PseudoId) -> PseudoId {
            let t = self.func().alloc_pseudo();
            let ulong = self.types.ulong_id;
            self.push(Instruction::binop(op, t, a, b, ulong, 64));
            t
        }

        pub(crate) fn select(&mut self, a: PseudoId, b: PseudoId) -> PseudoId {
            let cond = self.unknown();
            let t = self.func().alloc_pseudo();
            let ptr = self.types.char_ptr_id;
            self.push(Instruction::select(t, cond, a, b, ptr, 64));
            t
        }

        /// `target = phi(arms...)`, each arm through its `PhiSource`.
        pub(crate) fn phi_into(&mut self, target: PseudoId, arms: &[PseudoId]) {
            let ptr = self.types.char_ptr_id;
            let mut phi = Instruction::phi(target, ptr, 64);
            for &arm in arms {
                let src = self.func().alloc_pseudo();
                self.push(Instruction::phi_source(src, arm, ptr, 64));
                phi.phi_list.push((BasicBlockId(0), src));
            }
            self.push(phi);
        }

        pub(crate) fn fresh(&mut self) -> PseudoId {
            self.func().alloc_pseudo()
        }

        /// A call of `ret` to `known`, as the linearizer makes one.
        pub(crate) fn call(
            &mut self,
            known: crate::parse::ast::LibFn,
            name: &str,
            args: &[PseudoId],
            ret: TypeId,
        ) -> PseudoId {
            let t = self.func().alloc_pseudo();
            let arg_types = vec![self.types.ulong_id; args.len()];
            let size = self.types.size_bits(ret);
            let mut call = Instruction::call(Some(t), name, args.to_vec(), arg_types, ret, size);
            call.known = Some(known);
            self.push(call);
            t
        }

        /// What `string_len` answers for `p`.
        pub(crate) fn len(&self, p: PseudoId) -> Option<Len> {
            let bytes = ConstBytes::build(&self.module, &self.types);
            let f = &self.module.functions[0];
            let am = AddrMap::build(f);
            StrReader::new(&bytes, f, &am).string_len(p)
        }

        /// The C string `string_at` finds at `p`.
        pub(crate) fn c_str(&self, p: PseudoId) -> Option<Vec<u8>> {
            let bytes = ConstBytes::build(&self.module, &self.types);
            let f = &self.module.functions[0];
            let am = AddrMap::build(f);
            let s = StrReader::new(&bytes, f, &am).string_at(p)?;
            s.c_str().map(<[u8]>::to_vec)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::fixture::Fixture;
    use super::*;

    #[test]
    fn a_literal_reads_as_its_bytes_at_any_offset_inside_it() {
        let mut fx = Fixture::new();
        let lc = fx.literal("hello");
        let p = fx.addr(&lc);
        let four = fx.konst(4);
        let q = fx.op(Opcode::Add, p, four);
        let five = fx.konst(5);
        let end = fx.op(Opcode::Add, p, five);
        let six = fx.konst(6);
        let past = fx.op(Opcode::Add, p, six);
        assert_eq!(fx.c_str(p).as_deref(), Some(&b"hello"[..]));
        assert_eq!(fx.len(p), Some(Len::Const(5)));
        assert_eq!(fx.len(q), Some(Len::Const(1)));
        // The terminator is inside the object; one past it is not.
        assert_eq!(fx.len(end), Some(Len::Const(0)));
        assert_eq!(fx.len(past), None);
    }

    /// A `const` array is its initializer zero-filled to its size, and cut
    /// to it: `char s[3] = "abc"` has no terminator to find.
    #[test]
    fn a_const_array_is_its_initializer_to_its_size() {
        let mut fx = Fixture::new();
        fx.const_array("padded", 8, Initializer::String("ab".into()), |_| {});
        fx.const_array("cut", 3, Initializer::String("abc".into()), |_| {});
        let elements = vec![
            (0, Initializer::Int(b'x'.into())),
            (1, Initializer::Int(-1)),
        ];
        fx.const_array(
            "listed",
            4,
            Initializer::Array {
                elem_size: 1,
                total_size: 4,
                elements,
            },
            |_| {},
        );
        let padded = fx.addr("padded");
        let cut = fx.addr("cut");
        let listed = fx.addr("listed");
        assert_eq!(fx.len(padded), Some(Len::Const(2)));
        assert_eq!(fx.len(cut), None);
        assert_eq!(fx.c_str(listed).as_deref(), Some(&b"x\xff"[..]));
    }

    /// An object whose value may change reads as nothing: not `const`,
    /// `volatile`, weak, thread-local, or a tentative definition.
    #[test]
    fn an_object_that_may_change_is_not_read() {
        let mut fx = Fixture::new();
        let s = || Initializer::String("abc".into());
        fx.const_array("plain", 32, s(), |g| g.is_const = false);
        fx.const_array("weak", 4, s(), |g| g.symbol_attrs.weak = true);
        fx.const_array("tls", 4, s(), |g| g.is_thread_local = true);
        fx.const_array("tentative", 4, Initializer::None, |_| {});
        let vchar = fx.types.intern(crate::types::Type::with_modifiers(
            crate::types::TypeKind::Char,
            TypeModifiers::VOLATILE | TypeModifiers::CONST,
        ));
        let varr = fx.types.intern(crate::types::Type::array(vchar, 4));
        fx.const_array("volatile", 4, s(), |g| g.typ = varr);
        for name in ["plain", "weak", "tls", "tentative", "volatile"] {
            let p = fx.addr(name);
            assert_eq!(fx.len(p), None, "{name}");
        }
    }

    /// `p + i` of a string that fills its object is the whole length less
    /// `i`; an embedded terminator makes it depend on `i`, and refuses.
    #[test]
    fn a_variable_offset_subtracts_only_from_a_string_filling_its_object() {
        let mut fx = Fixture::new();
        let whole = fx.literal("abcd");
        let split = fx.literal("ab\0cd");
        let i = fx.unknown();
        let w = fx.addr(&whole);
        let two = fx.konst(2);
        let w2 = fx.op(Opcode::Add, w, two);
        let wi = fx.op(Opcode::Add, w2, i);
        let s = fx.addr(&split);
        let si = fx.op(Opcode::Add, s, i);
        assert_eq!(fx.len(wi), Some(Len::MinusOffset { len: 2, var: i }));
        assert_eq!(fx.len(si), None);
    }

    /// Every arm of a `Select` or a phi must agree; an arm that only comes
    /// back to the phi brings nothing new, so a loop over strings of one
    /// length has that length.
    #[test]
    fn arms_must_agree_and_a_loop_adds_nothing() {
        let mut fx = Fixture::new();
        let (foo, bar, four) = (fx.literal("foo"), fx.literal("bar"), fx.literal("four"));
        let (a, b, c) = (fx.addr(&foo), fx.addr(&bar), fx.addr(&four));
        let same = fx.select(a, b);
        let differ = fx.select(a, c);
        assert_eq!(fx.len(same), Some(Len::Const(3)));
        assert_eq!(fx.len(differ), None);

        // p = phi(a, q); q = cond ? p : b
        let p = fx.fresh();
        let q = fx.select(p, b);
        fx.phi_into(p, &[a, q]);
        assert_eq!(fx.len(p), Some(Len::Const(3)));

        // r = phi(a, s); s = cond ? r : c -- a loop that can bring "four".
        let r = fx.fresh();
        let s = fx.select(r, c);
        fx.phi_into(r, &[a, s]);
        assert_eq!(fx.len(r), None);
    }
}
