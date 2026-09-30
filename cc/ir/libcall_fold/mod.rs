//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Library calls whose result their arguments decide
//
// C17 7.1.4p1 lets an implementation compute a library function in place.
// A call the parser recognised (`Instruction::known`) is folded here once
// the optimizer has made its arguments as constant as they will get:
// `strlen("abc")` is 3, `strchr(s, 'o')` of a known `s` is `s + 4`, and
// `strcmp(p, "")` is the first byte of `p`. It runs inside the optimizer's
// loop, after `sccp` and `instcombine`, so an argument only they prove
// constant -- `n = 2; ++n` -- is seen, and the constants a fold leaves feed
// them on the next round.
//
// Each family of functions is a module of its own, which answers what one
// call folds to as a [`Folded`] without touching the function; this module
// finds the calls and writes the answers in. `strings` has the functions
// that read strings and compute a value; `copies` has `strcpy` and its kin,
// which write one, and fold to a `memcpy` of a known length; `stdio` has
// `printf` and its kin, whose result must be unused, and which become calls
// that print the same bytes; `complex` has libgcc's `__mul?c3` and
// `__div?c3`, which a floating complex `*` and `/` call, and whose result is
// written where the call would have written it. `memory` is the one family
// that rewrites an IR operation rather than a call: a `Memmove` whose source
// and destination cannot overlap becomes a `Memcpy`, in place.
//

mod complex;
mod copies;
mod memory;
mod stdio;
mod strings;

use super::build::Builder;
use super::facts::ConstMap;
use super::loadfwd::MemOracle;
use super::memloc::{AddrMap, MemBase, ModuleInfo};
use super::strdata::{ConstBytes, LocalBytes, StrReader};
use super::{string_label, Function, Instruction, Opcode, PseudoId};
use crate::abi::CallingConv;
use crate::float::FloatVal;
use crate::parse::ast::{CalleeBinding, LibFn};
use crate::target::Target;
use crate::token::lexer::bytes_payload;
use crate::types::{TypeId, TypeKind, TypeTable};
use std::cell::{OnceCell, RefCell};
use std::collections::{HashMap, HashSet};

/// What the folds need that is the same for every function in a module.
pub struct FoldCtx<'a> {
    pub(crate) types: &'a TypeTable,
    pub(crate) target: &'a Target,
    /// What the memory walk needs about the module's globals and callees.
    pub(crate) mi: &'a ModuleInfo,
    /// The objects whose bytes are known.
    pub(crate) bytes: &'a ConstBytes,
    /// `Module::library_symbols`: whom a call a fold makes calls.
    pub(crate) callees: &'a HashMap<&'static str, String>,
    /// The string literals the folds add.
    pub(crate) literals: &'a NewLiterals<'a>,
}

/// The string literals a fold adds to the module: `printf("hi\n")` is
/// `puts("hi")`, and the program may have no literal "hi" of its own. A
/// literal already in the module, or already added, is reused.
pub struct NewLiterals<'a> {
    /// `Module::strings`, as it was before the optimizer ran.
    existing: &'a [(String, String)],
    added: RefCell<Vec<(String, String)>>,
}

impl<'a> NewLiterals<'a> {
    pub fn new(existing: &'a [(String, String)]) -> Self {
        NewLiterals {
            existing,
            added: RefCell::new(Vec::new()),
        }
    }

    /// The label of the literal holding `bytes` and a terminator.
    fn label(&self, bytes: &[u8]) -> String {
        let payload = bytes_payload(bytes);
        let mut added = self.added.borrow_mut();
        let mut found = self.existing.iter().chain(added.iter());
        if let Some((label, _)) = found.find(|(_, c)| *c == payload) {
            return label.clone();
        }
        let label = string_label(self.existing.len() + added.len());
        added.push((label.clone(), payload));
        label
    }

    /// The literals added, to append to `Module::strings` in order.
    pub fn into_added(self) -> Vec<(String, String)> {
        self.added.into_inner()
    }
}

/// What a call folds to, in terms of its operands.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Folded {
    /// This integer, at the call's type.
    Int(i128),
    /// A null pointer.
    Null,
    /// The pointer `p` advanced this many bytes: `strchr` finding its
    /// character in a string `p` points at.
    Offset(PseudoId, i64),
    /// `len - var`, at the call's type.
    LenMinus { len: i64, var: PseudoId },
    /// The smaller of `n` and `len`, compared unsigned.
    AtMost { n: PseudoId, len: i64 },
    /// The first byte minus the second, each as an `unsigned char`: what
    /// the comparison functions answer when the first byte decides.
    ByteDiff(Byte, Byte),
    /// A call to another library function that computes the same thing.
    Call(NewCall),
    /// Bytes written in place of the call, and what it answers.
    Write(copies::Write),
    /// For a call whose result is unused: nothing, or a call to another
    /// library function that does the same thing.
    Discard(Option<NewCall>),
    /// A complex result, written where the call returns it.
    Complex(FloatVal, FloatVal),
}

/// One byte of a comparison.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Byte {
    /// The byte this pointer points at, read when the call would have run.
    At(PseudoId),
    Known(u8),
}

/// A call to the library function `func`, named by its C name `name`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct NewCall {
    pub(crate) func: LibFn,
    pub(crate) name: &'static str,
    pub(crate) args: Vec<Operand>,
}

/// An argument of a call a fold makes, and the type it is passed as.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Operand {
    Value(PseudoId, TypeId),
    Int(i128, TypeId),
    /// The address of a string literal of these bytes, and a terminator,
    /// passed as a `const char *`.
    Literal(Vec<u8>),
}

/// What a fold may ask about the function its call is in.
pub(crate) struct Facts<'a> {
    pub(crate) types: &'a TypeTable,
    pub(crate) target: &'a Target,
    consts: &'a ConstMap,
    am: &'a AddrMap,
    func: &'a Function,
    pub(crate) strings: StrReader<'a>,
    /// Every pseudo some instruction reads, built on first asking and shared
    /// by every call in the function.
    used: &'a OnceCell<HashSet<PseudoId>>,
}

impl Facts<'_> {
    /// Whether nothing reads `insn`'s result.
    pub(crate) fn result_unused(&self, insn: &Instruction) -> bool {
        let Some(t) = insn.target else {
            return true;
        };
        let used = self.used.get_or_init(|| {
            let insns = self.func.blocks.iter().flat_map(|bb| &bb.insns);
            insns.flat_map(Instruction::uses).collect()
        });
        !used.contains(&t)
    }

    /// The float constant `p` holds, of the format `typ` is in.
    pub(crate) fn float(&self, p: PseudoId, typ: TypeId) -> Option<FloatVal> {
        let fmt = self.types.fp_format(typ)?;
        let v = self.consts.fget(p, self.types.size_bits(typ))?;
        Some(v.round_to_format(fmt))
    }

    /// The constant `p` holds, read as an unsigned `bits`-bit value.
    pub(crate) fn unsigned(&self, p: PseudoId, bits: u32) -> Option<u128> {
        self.consts.get_at(p, bits, false).map(|v| v as u128)
    }

    /// Whether `a` and `b` are the same pointer: one value copied, or two
    /// addresses of the same place in the same object.
    pub(crate) fn same_pointer(&self, a: PseudoId, b: PseudoId) -> bool {
        if self.consts.root(a, 64) == self.consts.root(b, 64) {
            return true;
        }
        let loc = |p| self.am.resolve(self.func, p, 0, 0, None);
        let (la, lb) = (loc(a), loc(b));
        la.base != MemBase::Unknown
            && la.offset.is_some()
            && (la.base, la.offset) == (lb.base, lb.offset)
    }
}

/// Fold every call in `func` whose result its arguments decide. Answers
/// whether anything changed.
pub fn run(func: &mut Function, ctx: &FoldCtx) -> bool {
    let moved = memory::run(func, ctx);
    let sites = collect(func, ctx);
    if sites.is_empty() {
        return moved;
    }
    apply(func, ctx, sites);
    true
}

/// Where a call folds, and to what.
type Site = ((usize, usize), Folded);

fn collect(func: &Function, ctx: &FoldCtx) -> Vec<Site> {
    let am = AddrMap::build(func);
    let consts = ConstMap::new(func);
    let oracle = MemOracle::new(func, ctx.types, ctx.mi, &am);
    let used = OnceCell::new();
    let mut sites = Vec::new();
    for (b, bb) in func.blocks.iter().enumerate() {
        for (i, insn) in bb.insns.iter().enumerate() {
            let Some(known) = insn.extra().known.filter(|_| is_direct_call(insn)) else {
                continue;
            };
            // A local array's bytes are what the stores before this call
            // left in it.
            let locals = LocalBytes {
                oracle: &oracle,
                types: ctx.types,
                site: (b, i),
                little_endian: ctx.target.little_endian(),
            };
            let facts = Facts {
                types: ctx.types,
                target: ctx.target,
                consts: &consts,
                am: &am,
                func,
                strings: StrReader::new(ctx.bytes, func, &am).with_locals(locals),
                used: &used,
            };
            let Some(folded) = fold(known, insn, &facts) else {
                continue;
            };
            if calls_itself(func, ctx, &folded) || calls_unavailable(ctx, &folded) {
                continue;
            }
            sites.push(((b, i), folded));
        }
    }
    sites
}

/// Whether `insn` is a call by name, with nothing appended at inlining.
fn is_direct_call(insn: &Instruction) -> bool {
    insn.op == Opcode::Call
        && insn.extra().indirect_target.is_none()
        && !insn.extra().ends_with_va_arg_pack
}

/// What the call `insn` to `known` folds to, by the family it is in.
fn fold(known: LibFn, insn: &Instruction, facts: &Facts) -> Option<Folded> {
    use LibFn as L;
    match known {
        L::Strlen
        | L::Strnlen
        | L::Strcmp
        | L::Strncmp
        | L::Memcmp
        | L::Strchr
        | L::Strrchr
        | L::Memchr
        | L::Strstr
        | L::Strpbrk
        | L::Strcspn => strings::fold(known, insn, facts),
        L::Strcpy | L::Stpcpy | L::Strncpy | L::Strcat | L::Strncat | L::Sprintf => {
            copies::fold(known, insn, facts)
        }
        L::Printf
        | L::PrintfUnlocked
        | L::Vprintf
        | L::PrintfChk
        | L::VprintfChk
        | L::Fprintf
        | L::FprintfUnlocked
        | L::Vfprintf
        | L::FprintfChk
        | L::VfprintfChk
        | L::Fputs
        | L::FputsUnlocked => stdio::fold(known, insn, facts),
        L::MulComplex | L::DivComplex => complex::fold(known, insn, facts),
        _ => None,
    }
}

/// Whether `folded` calls the very function it is in: `strchr` written in
/// terms of `strpbrk` must not become a call to itself.
fn calls_itself(func: &Function, ctx: &FoldCtx, folded: &Folded) -> bool {
    calls_made(folded)
        .iter()
        .any(|&name| is_the_function(func, ctx, name))
}

/// The library functions `folded` will call, by their C names.
fn calls_made(folded: &Folded) -> &[&'static str] {
    match folded {
        Folded::Call(call) | Folded::Discard(Some(call)) => std::slice::from_ref(&call.name),
        Folded::Write(write) => write.calls(),
        _ => &[],
    }
}

/// Whether `folded` would call a library function this unit has taken away
/// by binding its name to something that is not a function.
///
/// `Module::library_symbols` holds every [`crate::ir::FOLD_CALLEES`] name the
/// program left alone, so a name missing from it is one no fold may reach
/// for -- calling it would jump into the program's own object.
fn calls_unavailable(ctx: &FoldCtx, folded: &Folded) -> bool {
    calls_made(folded)
        .iter()
        .any(|name| !ctx.callees.contains_key(name))
}

/// Whether `func` is the library function C calls `name`, by that name or
/// by the one this unit gives it.
fn is_the_function(func: &Function, ctx: &FoldCtx, name: &str) -> bool {
    func.name == name || ctx.callees.get(name) == Some(&func.name)
}

/// Whether an argument of type `t` is a pointer.
///
/// A variadic argument's type is not in the prototype, so a fold that means
/// to read through one has to ask: `%s` takes a pointer, and
/// `sprintf(d, "%s", 42)` is not a copy from address 42.
pub(super) fn is_pointer(types: &TypeTable, t: TypeId) -> bool {
    types.kind(t) == TypeKind::Pointer
}

/// The assembler name of the library function C calls `name`, for a call a
/// fold makes.
fn callee_symbol<'a>(ctx: &'a FoldCtx, name: &'a str) -> &'a str {
    ctx.callees.get(name).map_or(name, String::as_str)
}

/// Replace each call in `sites` with what it folds to.
fn apply(func: &mut Function, ctx: &FoldCtx, sites: Vec<Site>) {
    let mut by_block: HashMap<usize, HashMap<usize, Folded>> = HashMap::new();
    for ((b, i), folded) in sites {
        by_block.entry(b).or_default().insert(i, folded);
    }
    for (b, mut folds) in by_block {
        let insns = std::mem::take(&mut func.blocks[b].insns);
        let mut out = Vec::with_capacity(insns.len());
        for (i, insn) in insns.into_iter().enumerate() {
            match folds.remove(&i) {
                Some(folded) => {
                    let mut builder = Builder::new(func, ctx.types, insn.pos, &mut out);
                    materialize(&mut builder, ctx, &insn, folded);
                }
                None => out.push(insn),
            }
        }
        func.blocks[b].insns = out;
    }
}

/// The instructions that give `call`'s result the value `folded` says.
///
/// A call whose result is unused leaves nothing but what it writes, or
/// (`Discard`) the call that does what it did.
fn materialize(b: &mut Builder, ctx: &FoldCtx, call: &Instruction, folded: Folded) {
    if let Folded::Write(write) = folded {
        let result = call
            .target
            .zip(call.typ)
            .map(|(t, typ)| (t, typ, call.size));
        copies::materialize(b, ctx, write, result);
        return;
    }
    if let Folded::Complex(re, im) = folded {
        complex::materialize(b, call, (re, im));
        return;
    }
    if let Folded::Discard(new) = folded {
        if let Some(new) = new {
            let ret = new.func.return_type(b.types);
            make_call(b, ctx, None, ret, new);
        }
        return;
    }
    let (Some(target), Some(typ)) = (call.target, call.typ) else {
        return;
    };
    let size = call.size;
    let value = match folded {
        Folded::Int(v) => b.constant(v, typ, size),
        Folded::Null => b.constant(0, typ, size),
        Folded::Offset(p, k) => offset(b, p, k, typ, size),
        Folded::LenMinus { len, var } => {
            let len = b.constant(i128::from(len), typ, size);
            b.binop(Opcode::Sub, len, var, typ, size)
        }
        Folded::AtMost { n, len } => {
            let len = b.constant(i128::from(len), typ, size);
            let below = b.compare(Opcode::SetB, n, len, typ, size);
            b.select(below, n, len, typ, size)
        }
        Folded::ByteDiff(x, y) => {
            let x = byte_value(b, x, typ, size);
            let y = byte_value(b, y, typ, size);
            b.binop(Opcode::Sub, x, y, typ, size)
        }
        Folded::Call(new) => {
            make_call(b, ctx, Some(target), typ, new);
            return;
        }
        Folded::Write(_) | Folded::Discard(_) | Folded::Complex(..) => {
            unreachable!("materialized above")
        }
    };
    b.copy_into(target, value, typ, size);
}

/// The pointer `p` advanced `k` bytes, of the pointer type `typ`.
fn offset(b: &mut Builder, p: PseudoId, k: i64, typ: TypeId, size: u32) -> PseudoId {
    if k == 0 {
        return p;
    }
    let ulong = b.types.ulong_id;
    let k = b.constant(i128::from(k), ulong, 64);
    b.binop(Opcode::Add, p, k, typ, size)
}

/// `byte` at the comparison's result type (`int`): loaded as an `unsigned
/// char` and widened, or the constant.
fn byte_value(b: &mut Builder, byte: Byte, typ: TypeId, size: u32) -> PseudoId {
    match byte {
        Byte::Known(v) => b.constant(i128::from(v), typ, size),
        Byte::At(p) => {
            let uchar = b.types.uchar_id;
            let v = b.load(p, 0, uchar, 8);
            b.convert(Opcode::Zext, v, (uchar, 8), (typ, size))
        }
    }
}

/// The call `new`, returning `ret` into `target`.
fn make_call(b: &mut Builder, ctx: &FoldCtx, target: Option<PseudoId>, ret: TypeId, new: NewCall) {
    let mut args = Vec::with_capacity(new.args.len());
    let mut arg_types = Vec::with_capacity(new.args.len());
    for arg in new.args {
        let (v, t) = match arg {
            Operand::Value(p, t) => (p, t),
            Operand::Int(v, t) => (b.constant(v, t, b.types.size_bits(t)), t),
            Operand::Literal(bytes) => {
                let t = b.types.const_char_ptr_id;
                (b.sym_addr(ctx.literals.label(&bytes), t), t)
            }
        };
        args.push(v);
        arg_types.push(t);
    }
    let name = callee_symbol(ctx, new.name);
    let mut insn = Instruction::call_with_abi(
        target,
        name,
        args,
        arg_types,
        ret,
        CallingConv::C,
        b.types,
        ctx.target,
    );
    insn.extra_mut().callee_binding = CalleeBinding::Library;
    insn.extra_mut().known = Some(new.func);
    b.push(insn);
}

#[cfg(test)]
pub(super) mod tests {
    use super::*;
    use crate::ir::strdata::fixture::Fixture;

    /// Run `f` with a fold context for `fx`, whose callees are renamed as
    /// `callees` says, and hand back what it answers and the literals the
    /// folds added.
    ///
    /// The map starts out as the linearizer builds it: every
    /// [`crate::ir::FOLD_CALLEES`] name, called by itself, since that is what
    /// a program that leaves those names alone produces. The case where one
    /// is *missing* -- the program bound the name to an object, so no fold
    /// may call it -- is driven end to end from
    /// `cc/tests/builtins/stdio_fold.rs`, which compiles the C and reads the
    /// calls back out of the assembly.
    fn with_ctx<R>(
        fx: &Fixture,
        target: &Target,
        renames: &[(&'static str, &str)],
        f: impl FnOnce(&FoldCtx) -> R,
    ) -> (R, Vec<(String, String)>) {
        let bytes = ConstBytes::build(&fx.module, &fx.types);
        let mi = ModuleInfo::build(&fx.module, &fx.types);
        let mut callees: HashMap<&'static str, String> = crate::ir::FOLD_CALLEES
            .iter()
            .map(|&c| (c, c.to_string()))
            .collect();
        callees.extend(renames.iter().map(|&(c, a)| (c, a.to_string())));
        let literals = NewLiterals::new(&fx.module.strings);
        let answer = f(&FoldCtx {
            types: &fx.types,
            target,
            mi: &mi,
            bytes: &bytes,
            callees: &callees,
            literals: &literals,
        });
        (answer, literals.into_added())
    }

    /// What each known call in `fx` that folds folds to, by its index in
    /// the block.
    pub(in crate::ir::libcall_fold) fn folds(fx: &Fixture) -> Vec<(usize, Folded)> {
        let (folds, _) = with_ctx(fx, &Target::host(), &[], |ctx| {
            collect(&fx.module.functions[0], ctx)
                .into_iter()
                .map(|((_, i), folded)| (i, folded))
                .collect()
        });
        folds
    }

    /// Fold `fx`'s function with `callees`, add the literals the folds made
    /// to its module as `optimize_module` does, and hand back its
    /// instructions.
    pub(in crate::ir::libcall_fold) fn run_on(
        fx: &mut Fixture,
        callees: &[(&'static str, &str)],
    ) -> Vec<Instruction> {
        run_with(fx, &Target::host(), callees)
    }

    /// Fold `fx`'s function for `target`, and hand back its instructions.
    pub(in crate::ir::libcall_fold) fn run_for(
        fx: &mut Fixture,
        target: &Target,
    ) -> Vec<Instruction> {
        run_with(fx, target, &[])
    }

    fn run_with(
        fx: &mut Fixture,
        target: &Target,
        callees: &[(&'static str, &str)],
    ) -> Vec<Instruction> {
        let mut func = std::mem::take(&mut fx.module.functions[0]);
        let (_, added) = with_ctx(fx, target, callees, |ctx| run(&mut func, ctx));
        fx.module.strings.extend(added);
        let insns = func.blocks[0].insns.clone();
        fx.module.functions[0] = func;
        insns
    }

    /// The instruction defining `p`.
    pub(in crate::ir::libcall_fold) fn def(insns: &[Instruction], p: PseudoId) -> &Instruction {
        insns
            .iter()
            .find(|i| i.target == Some(p))
            .unwrap_or_else(|| panic!("nothing defines {p}"))
    }

    pub(in crate::ir::libcall_fold) fn ops(insns: &[Instruction]) -> Vec<Opcode> {
        insns.iter().map(|i| i.op).collect()
    }

    /// A folded call becomes a copy into its own result, of a constant for a
    /// known length, and nothing is called.
    #[test]
    fn a_folded_call_defines_its_result_and_is_gone() {
        let mut fx = Fixture::new();
        let lc = fx.literal("abc");
        let p = fx.addr(&lc);
        let ulong = fx.types.ulong_id;
        let r = fx.call(LibFn::Strlen, "strlen", &[p], ulong);
        let insns = run_on(&mut fx, &[]);
        assert!(!ops(&insns).contains(&Opcode::Call));
        let copy = def(&insns, r);
        assert_eq!(
            (copy.op, copy.typ, copy.size),
            (Opcode::Copy, Some(ulong), 64)
        );
        let set = def(&insns, copy.src[0]);
        assert_eq!(set.op, Opcode::SetVal);
        assert_eq!(fx.module.functions[0].const_val(copy.src[0]), Some(3));
    }

    /// A first-byte comparison loads each unknown byte as an `unsigned
    /// char` and widens it before subtracting.
    #[test]
    fn a_byte_difference_reads_unsigned_bytes() {
        let mut fx = Fixture::new();
        let (u, v, one) = (fx.unknown(), fx.unknown(), fx.konst(1));
        let int = fx.types.int_id;
        let r = fx.call(LibFn::Strncmp, "strncmp", &[u, v, one], int);
        let insns = run_on(&mut fx, &[]);
        let copy = def(&insns, r);
        let sub = def(&insns, copy.src[0]);
        assert_eq!((sub.op, sub.size), (Opcode::Sub, 32));
        for (side, from) in sub.src.iter().zip([u, v]) {
            let zext = def(&insns, *side);
            assert_eq!((zext.op, zext.src_size, zext.size), (Opcode::Zext, 8, 32));
            let load = def(&insns, zext.src[0]);
            assert_eq!((load.op, load.src[0], load.size), (Opcode::Load, from, 8));
            assert_eq!(load.typ, Some(fx.types.uchar_id));
        }
    }

    /// `strnlen` of an unknown bound is a `Select` on an unsigned compare.
    #[test]
    fn an_unknown_bound_selects_the_smaller() {
        let mut fx = Fixture::new();
        let lc = fx.literal("abc");
        let p = fx.addr(&lc);
        let n = fx.unknown();
        let ulong = fx.types.ulong_id;
        let r = fx.call(LibFn::Strnlen, "strnlen", &[p, n], ulong);
        let insns = run_on(&mut fx, &[]);
        let select = def(&insns, def(&insns, r).src[0]);
        assert_eq!(select.op, Opcode::Select);
        assert_eq!(select.src[1], n);
        let below = def(&insns, select.src[0]);
        assert_eq!((below.op, below.src[0]), (Opcode::SetB, n));
    }

    /// A call a fold makes goes to the program's name for the function,
    /// is tagged, and carries its ABI.
    #[test]
    fn a_made_call_uses_the_programs_name_for_the_function() {
        let mut fx = Fixture::new();
        let lc = fx.literal("o");
        let needle = fx.addr(&lc);
        let hay = fx.unknown();
        let ptr = fx.types.char_ptr_id;
        let r = fx.call(LibFn::Strstr, "strstr", &[hay, needle], ptr);
        let insns = run_on(&mut fx, &[("strchr", "my_strchr")]);
        let call = def(&insns, r);
        assert_eq!(call.op, Opcode::Call);
        assert_eq!(call.extra().func_name.as_deref(), Some("my_strchr"));
        assert_eq!(call.extra().known, Some(LibFn::Strchr));
        assert_eq!(call.extra().callee_binding, CalleeBinding::Library);
        assert!(call.extra().abi_info.is_some());
        assert_eq!(call.src[0], hay);
        assert_eq!(
            fx.module.functions[0].const_val(call.src[1]),
            Some(i128::from(b'o'))
        );
    }

    /// Inside the function it would call, a fold that makes a call does
    /// not: `strchr` written with `strstr` must not call itself.
    #[test]
    fn a_fold_never_makes_a_function_call_itself() {
        let mut fx = Fixture::new();
        fx.func().name = "strchr".to_string();
        let lc = fx.literal("o");
        let needle = fx.addr(&lc);
        let hay = fx.unknown();
        let ptr = fx.types.char_ptr_id;
        fx.call(LibFn::Strstr, "strstr", &[hay, needle], ptr);
        assert!(folds(&fx).is_empty());
        let insns = run_on(&mut fx, &[]);
        assert_eq!(
            insns.iter().filter(|i| i.op == Opcode::Call).count(),
            1,
            "the strstr call stays"
        );
    }

    /// A call not known to the parser is never folded, whatever it is
    /// named.
    #[test]
    fn an_unknown_call_is_left_alone() {
        let mut fx = Fixture::new();
        let lc = fx.literal("abc");
        let p = fx.addr(&lc);
        let ulong = fx.types.ulong_id;
        fx.call(LibFn::Strlen, "strlen", &[p], ulong);
        fx.func().blocks[0]
            .insns
            .last_mut()
            .unwrap()
            .extra_mut()
            .known = None;
        assert!(folds(&fx).is_empty());
    }
}
