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
use super::loadfwd::MemOracle;
use super::memexpand::BlockOp;
use super::memloc::{AddrMap, MemBase, ModuleInfo};
use super::strdata::{ConstBytes, LocalBytes, StrReader};
use super::{Function, Instruction, Opcode, PseudoId, Site, StringPool};
use crate::abi::CallingConv;
use crate::float::FloatVal;
use crate::parse::ast::{CalleeBinding, LibFamily, LibFn};
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
    pool: RefCell<StringPool<'a>>,
}

impl<'a> NewLiterals<'a> {
    pub fn new(pool: StringPool<'a>) -> Self {
        NewLiterals {
            pool: RefCell::new(pool),
        }
    }

    /// The label of the literal holding `bytes` and a terminator.
    fn label(&self, bytes: &[u8]) -> String {
        self.pool.borrow_mut().add(bytes_payload(bytes))
    }
}

/// What a call folds to, in terms of its operands.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Folded {
    /// A value computed in place of the call.
    Value(ValueFold),
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

/// A call's result, computed at the call's type from its operands.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ValueFold {
    /// This integer.
    Int(i128),
    /// A null pointer.
    Null,
    /// The pointer `p` advanced this many bytes: `strchr` finding its
    /// character in a string `p` points at.
    Offset(PseudoId, i64),
    /// `len - var`.
    LenMinus { len: i64, var: PseudoId },
    /// The smaller of `n` and `len`, compared unsigned.
    AtMost { n: PseudoId, len: i64 },
    /// The first byte minus the second, each as an `unsigned char`: what
    /// the comparison functions answer when the first byte decides.
    ByteDiff(Byte, Byte),
}

/// One byte of a comparison.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Byte {
    /// The byte this pointer points at, read when the call would have run.
    At(PseudoId),
    Known(u8),
}

/// A call to the library function `func`, by its C name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct NewCall {
    pub(crate) func: LibFn,
    pub(crate) args: Vec<Operand>,
}

/// A library function a fold calls.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Callee {
    /// One the optimizer knows, so that the call may fold in turn.
    Known(LibFn),
    /// The function a block memory operation calls.
    Block(BlockOp),
}

impl Callee {
    /// The function's C name: a key of `Module::library_symbols`.
    pub(crate) fn c_name(self) -> &'static str {
        match self {
            Callee::Known(f) => f.c_name().expect("a fold calls only a function C names"),
            Callee::Block(op) => op.c_name(),
        }
    }
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

/// What a fold may ask about the call it folds and the function it is in.
pub(crate) struct CallSite<'a> {
    pub(crate) types: &'a TypeTable,
    pub(crate) target: &'a Target,
    am: &'a AddrMap,
    func: &'a Function,
    pub(crate) strings: StrReader<'a>,
    /// Every pseudo some instruction reads, built on first asking and shared
    /// by every call in the function.
    used: &'a OnceCell<HashSet<PseudoId>>,
}

impl<'a> CallSite<'a> {
    /// The C string `p` points at, when it is known.
    pub(crate) fn c_str(&self, p: PseudoId) -> Option<&'a [u8]> {
        self.strings.string_at(p)?.c_str()
    }

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
        let v = self.am.consts().fget(p, self.types.size_bits(typ))?;
        Some(v.round_to_format(fmt))
    }

    /// The constant `p` holds, read as an unsigned `bits`-bit value.
    pub(crate) fn unsigned(&self, p: PseudoId, bits: u32) -> Option<u128> {
        self.am.consts().get_at(p, bits, false).map(|v| v as u128)
    }

    /// The constant `p` holds, read as a `size_t`.
    pub(crate) fn size_t(&self, p: PseudoId) -> Option<u128> {
        self.unsigned(p, self.types.size_bits(self.types.ulong_id))
    }

    /// Whether `a` and `b` are the same pointer: one value copied, or two
    /// addresses of the same place in the same object.
    pub(crate) fn same_pointer(&self, a: PseudoId, b: PseudoId) -> bool {
        let bits = self.types.size_bits(self.types.void_ptr_id);
        let consts = self.am.consts();
        if consts.root(a, bits) == consts.root(b, bits) {
            return true;
        }
        let loc = |p| self.am.object_of(self.func, p);
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
type FoldSite = (Site, Folded);

fn collect(func: &Function, ctx: &FoldCtx) -> Vec<FoldSite> {
    let am = AddrMap::build(func);
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
            let facts = CallSite {
                types: ctx.types,
                target: ctx.target,
                am: &am,
                func,
                strings: StrReader::new(ctx.bytes, func, &am, ctx.types).with_locals(locals),
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
fn fold(known: LibFn, insn: &Instruction, facts: &CallSite) -> Option<Folded> {
    match known.family() {
        LibFamily::StringQuery => strings::fold(known, insn, facts),
        LibFamily::StringWrite => copies::fold(known, insn, facts),
        LibFamily::Output => stdio::fold(known, insn, facts),
        LibFamily::ComplexArith => complex::fold(known, insn, facts),
    }
}

/// Whether `folded` calls the very function it is in: `strchr` written in
/// terms of `strpbrk` must not become a call to itself.
fn calls_itself(func: &Function, ctx: &FoldCtx, folded: &Folded) -> bool {
    calls_made(folded)
        .iter()
        .any(|callee| is_the_function(func, ctx, callee.c_name()))
}

/// The library functions `folded` will call.
fn calls_made(folded: &Folded) -> Vec<Callee> {
    match folded {
        Folded::Call(call) | Folded::Discard(Some(call)) => vec![Callee::Known(call.func)],
        Folded::Write(write) => write.calls().to_vec(),
        Folded::Value(_) | Folded::Discard(None) | Folded::Complex(..) => Vec::new(),
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
        .any(|callee| !ctx.callees.contains_key(callee.c_name()))
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

/// The assembler name of `callee`, for a call a fold makes.
fn callee_symbol<'a>(ctx: &'a FoldCtx, callee: Callee) -> &'a str {
    let name = callee.c_name();
    ctx.callees.get(name).map_or(name, String::as_str)
}

/// Replace each call in `sites` with what it folds to.
fn apply(func: &mut Function, ctx: &FoldCtx, sites: Vec<FoldSite>) {
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
    let result = call.target.zip(call.typ);
    match folded {
        Folded::Value(value) => {
            if let Some((target, typ)) = result {
                let v = compute(b, value, typ);
                b.copy_into(target, v, typ);
            }
        }
        Folded::Call(new) => {
            if let Some((target, typ)) = result {
                make_call(b, ctx, Some(target), typ, new);
            }
        }
        Folded::Write(write) => copies::materialize(b, ctx, write, result),
        Folded::Discard(new) => {
            if let Some(new) = new {
                let ret = new.func.return_type(b.types);
                make_call(b, ctx, None, ret, new);
            }
        }
        Folded::Complex(re, im) => complex::materialize(b, call, (re, im)),
    }
}

/// The instructions that compute `value`, of the call's type `typ`.
fn compute(b: &mut Builder, value: ValueFold, typ: TypeId) -> PseudoId {
    match value {
        ValueFold::Int(v) => b.constant(v, typ),
        ValueFold::Null => b.constant(0, typ),
        ValueFold::Offset(p, k) => offset(b, p, k, typ),
        ValueFold::LenMinus { len, var } => {
            let len = b.constant(i128::from(len), typ);
            b.binop(Opcode::Sub, len, var, typ)
        }
        ValueFold::AtMost { n, len } => {
            let len = b.constant(i128::from(len), typ);
            let below = b.compare(Opcode::SetB, n, len, typ);
            b.select(below, n, len, typ)
        }
        ValueFold::ByteDiff(x, y) => {
            let x = byte_value(b, x, typ);
            let y = byte_value(b, y, typ);
            b.binop(Opcode::Sub, x, y, typ)
        }
    }
}

/// The pointer `p` advanced `k` bytes, of the pointer type `typ`.
fn offset(b: &mut Builder, p: PseudoId, k: i64, typ: TypeId) -> PseudoId {
    if k == 0 {
        return p;
    }
    let k = b.constant(i128::from(k), b.types.ulong_id);
    b.binop(Opcode::Add, p, k, typ)
}

/// `byte` at the comparison's result type (`int`): loaded as an `unsigned
/// char` and widened, or the constant.
fn byte_value(b: &mut Builder, byte: Byte, typ: TypeId) -> PseudoId {
    match byte {
        Byte::Known(v) => b.constant(i128::from(v), typ),
        Byte::At(p) => {
            let uchar = b.types.uchar_id;
            let v = b.load(p, 0, uchar);
            b.convert(Opcode::Zext, v, uchar, typ)
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
            Operand::Int(v, t) => (b.constant(v, t), t),
            Operand::Literal(bytes) => {
                let t = b.types.const_char_ptr_id;
                (b.sym_addr(ctx.literals.label(&bytes), t), t)
            }
        };
        args.push(v);
        arg_types.push(t);
    }
    let name = callee_symbol(ctx, Callee::Known(new.func));
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
    /// `renames` says, on `fx`'s functions, and hand back what it answers.
    /// The literals the folds make are added to `fx`'s module.
    ///
    /// The map starts out as the linearizer builds it: every
    /// [`crate::ir::FOLD_CALLEES`] name, called by itself, since that is what
    /// a program that leaves those names alone produces. The case where one
    /// is *missing* -- the program bound the name to an object, so no fold
    /// may call it -- is driven end to end from
    /// `cc/tests/builtins/stdio_fold.rs`, which compiles the C and reads the
    /// calls back out of the assembly.
    fn with_ctx<R>(
        fx: &mut Fixture,
        target: &Target,
        renames: &[(&'static str, &str)],
        f: impl FnOnce(&FoldCtx, &mut Vec<Function>) -> R,
    ) -> R {
        let bytes = ConstBytes::build(&fx.module, &fx.types);
        let mi = ModuleInfo::build(&fx.module, &fx.types);
        let mut callees: HashMap<&'static str, String> = crate::ir::FOLD_CALLEES
            .iter()
            .map(|&c| (c, c.to_string()))
            .collect();
        callees.extend(renames.iter().map(|&(c, a)| (c, a.to_string())));
        let (functions, strings, _) = fx.module.split_for_rewrite();
        let literals = NewLiterals::new(strings);
        let ctx = FoldCtx {
            types: &fx.types,
            target,
            mi: &mi,
            bytes: &bytes,
            callees: &callees,
            literals: &literals,
        };
        f(&ctx, functions)
    }

    /// What each known call in `fx` that folds folds to, by its index in
    /// the block.
    pub(in crate::ir::libcall_fold) fn folds(fx: &mut Fixture) -> Vec<(usize, Folded)> {
        with_ctx(fx, &Target::host(), &[], |ctx, functions| {
            collect(&functions[0], ctx)
                .into_iter()
                .map(|((_, i), folded)| (i, folded))
                .collect()
        })
    }

    /// Fold `fx`'s function with `callees`, the literals the folds made added
    /// to its module as `optimize_module` adds them, and hand back its
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
        with_ctx(fx, target, callees, |ctx, functions| {
            run(&mut functions[0], ctx);
            functions[0].blocks[0].insns.clone()
        })
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
        assert!(folds(&mut fx).is_empty());
        let insns = run_on(&mut fx, &[]);
        assert_eq!(
            insns.iter().filter(|i| i.op == Opcode::Call).count(),
            1,
            "the strstr call stays"
        );
    }

    /// Every function a fold calls is one the linearizer looks up a name
    /// for: one missing from [`crate::ir::FOLD_CALLEES`] would never be
    /// available, and its fold would never happen.
    #[test]
    fn every_callee_is_looked_up() {
        use LibFn as L;
        let known = [
            L::Strlen,
            L::Strchr,
            L::Strcpy,
            L::Puts,
            L::Putchar,
            L::Fputs,
            L::Fputc,
            L::Fwrite,
        ]
        .map(Callee::Known);
        let block = [BlockOp::Copy, BlockOp::Set].map(Callee::Block);
        let mut names: Vec<_> = known.iter().chain(&block).map(|c| c.c_name()).collect();
        let mut listed = crate::ir::FOLD_CALLEES.to_vec();
        names.sort_unstable();
        listed.sort_unstable();
        assert_eq!(names, listed);
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
        assert!(folds(&mut fx).is_empty());
    }
}
