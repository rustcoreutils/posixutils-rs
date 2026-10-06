//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The `_chk` functions, where the write is known to fit
//
// `_FORTIFY_SOURCE` makes glibc's headers call `memcpy(d, s, n)` as
// `__builtin___memcpy_chk(d, s, n, __builtin_object_size(d, 0))`, and the
// library's `__memcpy_chk` aborts when `n` exceeds the size. gcc decides the
// check at compile time where it can (`gimple_fold_builtin_*_chk`), and so
// does this: a `_chk` call is the plain function when
//
// - the size is `(size_t)-1`, an object nothing is known about, so there is
//   nothing to check against; or
// - what the call writes is known, and fits: a constant length no greater
//   than the size, or the largest of the constants a length may be (`n = c ?
//   8 : 4`); the length of a known string, or the longest of several, and its
//   terminator; for `strcat`, a known string after one the destination is
//   known to hold, and for `strcat` and `strncat`, nothing at all; for
//   `sprintf` and
//   `vsprintf`, a format with no directive, or `sprintf`'s `"%s"` of a known
//   string, and its terminator.
//
// Otherwise the check stays, and a write that provably overflows aborts at
// run time, as in gcc. gcc also warns about that one at compile time
// (`-Wstringop-overflow`), which c17 does not have.
//
// A check that stays, of a string whose length is known, is the check of a
// copy of that many bytes and the terminator, as gcc's strlen pass makes it:
// `__strcpy_chk(d, "abc", 2)` is `__memcpy_chk(d, "abc", 4, 2)`.
//
// A call whose result nobody reads is first the `_chk` function that
// answers nothing more, as in gcc: `mempcpy` is `memcpy`, `stpcpy` is
// `strcpy`, `stpncpy` is `strncpy`, each still checked, and decided in turn.
//
// The `printf` forms take a flag as well, which glibc sets at
// `_FORTIFY_SOURCE=2` to have `%n` in a writable format refused; gcc drops
// such a check only where the format is known and has no directive but
// `"%s"`, since then there is no `%n` to refuse.
//
// One rule serves every function, driven by a table that says, for each,
// which argument is the size, which says what is written, and what the
// plain call is: a block operation for the `mem` functions, as the
// linearizer makes one, and otherwise the plain library function, the call
// rewritten in place without the size and flag arguments. A plain call the
// optimizer knows (`strcpy`, `sprintf`) folds again on the next round, so a
// fortified `strcpy(d, "abc")` ends up as four bytes stored, as in gcc.
//

use super::{callee_symbol, is_pointer, CallSite, Callee, FoldCtx, Folded, NewCall, Operand};
use crate::ir::build::Builder;
use crate::ir::memexpand::BlockOp;
use crate::ir::memloc::AddrMap;
use crate::ir::strdata::Len;
use crate::ir::{Function, Instruction, Opcode, PseudoId};
use crate::parse::ast::{CalleeBinding, LibFn};
use std::collections::HashSet;

/// How many definitions the walk for a length's largest value follows.
const MAX_WALK: usize = 64;

/// What a `_chk` function writes into its destination.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Writes {
    /// As many bytes as argument `n` says: `memcpy`, `strncpy`, and the
    /// bound of `snprintf`.
    Count(usize),
    /// The string argument `n` points at, terminator and all.
    String(usize),
    /// The string argument `src` points at, after the destination's own;
    /// `strncat` takes at most argument `bound` of its characters. Nothing,
    /// for an empty string or a bound of 0. Otherwise known only for
    /// `strcat`, where the destination holds a string of known length (gcc's
    /// strlen pass): both strings and the terminator.
    Append { src: usize, bound: Option<usize> },
    /// What the format prints ([`Printf::fmt`]); `va_list` when the
    /// arguments come in one rather than after the format.
    Format { va_list: bool },
}

/// Where a `printf` form's flag and format are.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Printf {
    flag: usize,
    fmt: usize,
}

/// The unchecked form of a `_chk` function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Plain {
    /// The block operation, on the call's first three arguments.
    Block(BlockOp),
    /// `mempcpy`: a copy, answering the end of what it wrote.
    CopyToEnd,
    /// This library function, of the call's arguments but the size and
    /// the flag.
    Call(Callee),
}

/// One `_chk` function: what it is, what it writes, where its checking
/// arguments are, and what it is without them.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Fortified {
    chk: LibFn,
    writes: Writes,
    /// The argument holding the size of the destination.
    size: usize,
    /// For the `printf` forms, where the flag and the format are.
    printf: Option<Printf>,
    plain: Plain,
    /// The `_chk` function doing the same where the result is unused: one
    /// that answers the destination rather than the end of what it wrote.
    unused: Option<LibFn>,
    /// The `_chk` function copying a counted length, which does the same
    /// for a string whose length is known: `(d, s, length, size)`.
    counted: Option<LibFn>,
}

/// Every `_chk` function folded here, by glibc's prototypes.
#[rustfmt::skip]
static FORTIFIED: [Fortified; 14] = {
    use LibFn as L;
    use Writes::*;
    const fn row(chk: LibFn, writes: Writes, size: usize, printf: Option<Printf>, plain: Plain) -> Fortified {
        Fortified { chk, writes, size, printf, plain, unused: None, counted: None }
    }
    const fn or_unused(row: Fortified, unused: LibFn) -> Fortified {
        Fortified { unused: Some(unused), ..row }
    }
    const fn or_counted(row: Fortified, counted: LibFn) -> Fortified {
        Fortified { counted: Some(counted), ..row }
    }
    const fn printf(flag: usize, fmt: usize) -> Option<Printf> {
        Some(Printf { flag, fmt })
    }
    const fn call(f: LibFn) -> Plain {
        Plain::Call(Callee::Known(f))
    }
    const fn named(name: &'static str) -> Plain {
        Plain::Call(Callee::Named(name))
    }
    [
        //   `_chk` function  writes                            size  flag, format   plain
        row(L::MemcpyChk,    Count(2),                         3,    None,          Plain::Block(BlockOp::Copy)),
        or_unused(
        row(L::MempcpyChk,   Count(2),                         3,    None,          Plain::CopyToEnd),
            L::MemcpyChk),
        row(L::MemmoveChk,   Count(2),                         3,    None,          Plain::Block(BlockOp::Move)),
        row(L::MemsetChk,    Count(2),                         3,    None,          Plain::Block(BlockOp::Set)),
        or_counted(
        row(L::StrcpyChk,    String(1),                        2,    None,          call(L::Strcpy)),
            L::MemcpyChk),
        or_unused(
        row(L::StpcpyChk,    String(1),                        2,    None,          call(L::Stpcpy)),
            L::StrcpyChk),
        row(L::StrncpyChk,   Count(2),                         3,    None,          call(L::Strncpy)),
        or_unused(
        row(L::StpncpyChk,   Count(2),                         3,    None,          named("stpncpy")),
            L::StrncpyChk),
        row(L::StrcatChk,    Append { src: 1, bound: None },   2,    None,          call(L::Strcat)),
        row(L::StrncatChk,   Append { src: 1, bound: Some(2) },3,    None,          call(L::Strncat)),
        row(L::SprintfChk,   Format { va_list: false },        2,    printf(1, 3),  call(L::Sprintf)),
        row(L::SnprintfChk,  Count(1),                         3,    printf(2, 4),  named("snprintf")),
        row(L::VsprintfChk,  Format { va_list: true },         2,    printf(1, 3),  named("vsprintf")),
        row(L::VsnprintfChk, Count(1),                         3,    printf(2, 4),  named("vsnprintf")),
    ]
};

/// The row for `f`.
fn row(f: LibFn) -> Option<&'static Fortified> {
    FORTIFIED.iter().find(|r| r.chk == f)
}

/// The plain functions a fold here may call, which the linearizer looks up
/// a name for ([`crate::ir::FOLD_CALLEES`]).
#[cfg(test)]
pub(super) fn callees() -> Vec<Callee> {
    let checked = FORTIFIED.iter().flat_map(|r| [r.unused, r.counted]);
    let checked = checked.flatten().map(Callee::Known);
    FORTIFIED
        .iter()
        .map(Fortified::callee)
        .chain(checked)
        .collect()
}

impl Fortified {
    /// Every argument position the row names.
    fn positions(&self) -> Vec<usize> {
        let mut at = vec![self.size];
        if let Some(Printf { flag, fmt }) = self.printf {
            at.extend([flag, fmt]);
        }
        match self.writes {
            Writes::Count(n) | Writes::String(n) => at.push(n),
            Writes::Append { src, bound } => at.extend([Some(src), bound].into_iter().flatten()),
            Writes::Format { .. } => {}
        }
        at
    }

    /// The function this calls in place of the `_chk` one.
    pub(super) fn callee(&self) -> Callee {
        match self.plain {
            Plain::Block(op) => Callee::Block(op),
            Plain::CopyToEnd => Callee::Block(BlockOp::Copy),
            Plain::Call(callee) => callee,
        }
    }
}

/// What the call `insn` to the `_chk` function `f` folds to: the plain
/// function, where the write needs no check.
pub(super) fn fold(f: LibFn, insn: &Instruction, facts: &CallSite) -> Option<Folded> {
    let row = row(f)?;
    let args = insn.src.as_slice();
    // The format's `%s` argument is judged by its type, and the two lists
    // are read in parallel.
    if args.len() != insn.extra().arg_types.len()
        || row.positions().iter().any(|&i| i >= args.len())
    {
        return None;
    }
    if let Some(unused) = row.unused.filter(|_| facts.result_unused(insn)) {
        return Some(Folded::Discard(Some(same_call(unused, insn))));
    }
    let size = facts.size_t(*args.get(row.size)?)?;
    if !flag_allows(row, insn, facts) {
        return None;
    }
    let unknown_object = size == u128::from(u64::MAX);
    if unknown_object || fits(row, insn, facts, size) {
        return Some(Folded::Unchecked(row));
    }
    counted_copy(row, insn, facts)
}

/// The `_chk` copy of a counted length in place of `insn`, a check that
/// stays of a string whose length is known.
fn counted_copy(row: &Fortified, insn: &Instruction, facts: &CallSite) -> Option<Folded> {
    let (Some(counted), Writes::String(s)) = (row.counted, row.writes) else {
        return None;
    };
    let Len::Const(len) = facts.strings.string_len(insn.src[s])? else {
        return None;
    };
    let types = &insn.extra().arg_types;
    let ulong = facts.types.ulong_id;
    let call = NewCall {
        func: counted,
        args: vec![
            Operand::Value(insn.src[0], types[0]),
            Operand::Value(insn.src[s], types[s]),
            Operand::Int(i128::from(len) + 1, ulong),
            Operand::Value(insn.src[row.size], types[row.size]),
        ],
    };
    Some(if facts.result_unused(insn) {
        Folded::Discard(Some(call))
    } else {
        Folded::Call(call)
    })
}

/// A call to `f` of `insn`'s own arguments, passed as it passes them.
fn same_call(f: LibFn, insn: &Instruction) -> NewCall {
    let types = &insn.extra().arg_types;
    NewCall {
        func: f,
        args: insn
            .src
            .iter()
            .zip(types)
            .map(|(&p, &t)| Operand::Value(p, t))
            .collect(),
    }
}

/// Whether the `printf` forms' flag lets the check go: 0, or a format with
/// no directive but `"%s"`.
fn flag_allows(row: &Fortified, insn: &Instruction, facts: &CallSite) -> bool {
    let Some(Printf { flag, fmt }) = row.printf else {
        return true;
    };
    facts.unsigned(insn.src[flag], 32) == Some(0)
        || facts
            .c_str(insn.src[fmt])
            .is_some_and(|text| !text.contains(&b'%') || text == b"%s")
}

/// Whether what the call `insn` writes is known to be no more than `size`
/// bytes.
fn fits(row: &Fortified, insn: &Instruction, facts: &CallSite, size: u128) -> bool {
    let args = insn.src.as_slice();
    match row.writes {
        Writes::Count(n) => largest(facts, args[n]).is_some_and(|n| n <= size),
        Writes::String(s) => facts
            .strings
            .longest_len(args[s])
            .is_some_and(|len| u128::from(len) < size),
        Writes::Append { src, bound } => {
            facts.c_str(args[src]) == Some(b"")
                || bound.is_some_and(|b| facts.size_t(args[b]) == Some(0))
                || bound.is_none() && appended(facts, args[0], args[src]).is_some_and(|n| n < size)
        }
        Writes::Format { va_list } => row
            .printf
            .and_then(|p| printed(facts, insn, p.fmt, va_list))
            .is_some_and(|len| u128::from(len) < size),
    }
}

/// How long the string at `dest` is after `src`'s is appended to it, when
/// `dest` holds a string of known length and `src` one of a known length or
/// one of several.
fn appended(facts: &CallSite, dest: PseudoId, src: PseudoId) -> Option<u128> {
    let Len::Const(have) = facts.strings.string_len(dest)? else {
        return None;
    };
    let added = facts.strings.longest_len(src)?;
    Some(u128::from(have) + u128::from(added))
}

/// How many characters the format at argument `fmt` of `insn` prints, when
/// gcc knows: a format with no directive and, for `sprintf`, no argument
/// after it; or `sprintf`'s `"%s"` of a known string.
fn printed(facts: &CallSite, insn: &Instruction, fmt: usize, va_list: bool) -> Option<u64> {
    let text = facts.c_str(insn.src[fmt])?;
    let rest = &insn.src[fmt + 1..];
    if !text.contains(&b'%') {
        return (va_list || rest.is_empty()).then_some(text.len() as u64);
    }
    match rest {
        // A variadic argument's type is not in the prototype: `%s` of an
        // integer is no string to measure.
        &[s] if !va_list
            && text == b"%s"
            && is_pointer(facts.types, insn.extra().arg_types[fmt + 1]) =>
        {
            match facts.strings.string_len(s)? {
                Len::Const(len) => Some(len),
                Len::MinusOffset { .. } => None,
            }
        }
        _ => None,
    }
}

/// The largest value the `size_t` `p` can have, when every value it can
/// have is a constant: one, or a choice among several, of `size_t` or
/// widened to it -- `n = c ? 8 : 4` chooses between two `int`s (gcc's
/// `get_maxval_strlen` of an integer).
fn largest(facts: &CallSite, p: PseudoId) -> Option<u128> {
    let mut walk = Largest {
        func: facts.func,
        am: facts.am,
        open: HashSet::new(),
        budget: MAX_WALK,
    };
    let bits = facts.types.size_bits(facts.types.ulong_id);
    walk.of(p, bits, false).flatten()
}

/// One walk for [`largest`].
struct Largest<'a> {
    func: &'a Function,
    am: &'a AddrMap,
    /// The phis being walked.
    open: HashSet<PseudoId>,
    budget: usize,
}

impl Largest<'_> {
    /// The largest value of `p`, read as a `bits`-bit integer, `signed` or
    /// not: `Some(None)` for an arm that only comes back around to a phi
    /// being walked, `None` for one that is not known. A negative value
    /// widened to a `size_t` is enormous, and is not known either.
    fn of(&mut self, p: PseudoId, bits: u32, signed: bool) -> Option<Option<u128>> {
        self.budget = self.budget.checked_sub(1)?;
        if let Some(v) = self.am.consts().get_at(p, bits, signed) {
            return u128::try_from(v).ok().map(Some);
        }
        let d = self.am.def(self.func, p)?;
        match (d.op, d.src.as_slice()) {
            (Opcode::Copy | Opcode::PhiSource, &[src]) => self.of(src, bits, signed),
            (Opcode::Zext, &[src]) => self.of(src, d.operand_width(), false),
            (Opcode::Sext, &[src]) => self.of(src, d.operand_width(), true),
            (Opcode::Select, &[_, a, b]) => self.join(&[a, b], bits, signed),
            (Opcode::Phi, _) => {
                if !self.open.insert(p) {
                    return Some(None);
                }
                let arms: Vec<PseudoId> = d.phi_list.iter().map(|&(_, v)| v).collect();
                let joined = self.join(&arms, bits, signed);
                self.open.remove(&p);
                joined
            }
            _ => None,
        }
    }

    fn join(&mut self, arms: &[PseudoId], bits: u32, signed: bool) -> Option<Option<u128>> {
        let mut found = None;
        for &arm in arms {
            if let Some(v) = self.of(arm, bits, signed)? {
                found = Some(found.map_or(v, |f: u128| f.max(v)));
            }
        }
        Some(found)
    }
}

/// The plain call `row` says, in place of the `_chk` call `call`.
pub(super) fn materialize(b: &mut Builder, ctx: &FoldCtx, call: &Instruction, row: &Fortified) {
    match row.plain {
        Plain::Block(op) => {
            let target = call.target.unwrap_or_else(|| b.func.alloc_pseudo());
            block(b, ctx, call, op, target);
        }
        // The copy answers its destination, and the call the end.
        Plain::CopyToEnd => {
            let copied = b.func.alloc_pseudo();
            block(b, ctx, call, BlockOp::Copy, copied);
            if let (Some(target), Some(typ)) = (call.target, call.typ) {
                let end = b.binop(Opcode::Add, call.src[0], call.src[2], typ);
                b.copy_into(target, end, typ);
            }
        }
        Plain::Call(callee) => b.push(unchecked_call(ctx, call, row, callee)),
    }
}

/// The block operation `op` of `call`'s first three arguments into
/// `target`, as the linearizer makes `memcpy`.
fn block(b: &mut Builder, ctx: &FoldCtx, call: &Instruction, op: BlockOp, target: PseudoId) {
    let void_ptr = b.types.void_ptr_id;
    b.push(
        Instruction::new(op.opcode())
            .with_func(callee_symbol(ctx, Callee::Block(op)))
            .with_target(target)
            .with_src3(call.src[0], call.src[1], call.src[2])
            .with_type_and_size(void_ptr, b.types.size_bits(void_ptr)),
    );
}

/// `call` made to `callee` instead, without the arguments `row` checks with.
fn unchecked_call(
    ctx: &FoldCtx,
    call: &Instruction,
    row: &Fortified,
    callee: Callee,
) -> Instruction {
    let flag = row.printf.map(|p| p.flag);
    let dropped: Vec<usize> = [Some(row.size), flag].into_iter().flatten().collect();
    let keep = |i: &usize| !dropped.contains(i);
    let mut insn = call.clone();
    insn.src = keep_args(&call.src, keep);
    let extra = insn.extra_mut();
    extra.arg_types = keep_args(&call.extra().arg_types, keep);
    if let Some(abi) = extra.abi_info.as_mut() {
        abi.params = keep_args(&abi.params, keep);
    }
    // The variadic arguments start as many places earlier as there were
    // dropped arguments before them.
    extra.variadic_arg_start = call
        .extra()
        .variadic_arg_start
        .map(|start| start - dropped.iter().filter(|&&i| i < start).count());
    extra.func_name = Some(callee_symbol(ctx, callee).to_string());
    extra.callee_binding = CalleeBinding::Library;
    extra.known = match callee {
        Callee::Known(f) => Some(f),
        Callee::Block(_) | Callee::Named(_) => None,
    };
    insn
}

/// The items of `items` whose index `keep` admits, in order.
fn keep_args<T: Clone>(items: &[T], keep: impl Fn(&usize) -> bool) -> Vec<T> {
    items
        .iter()
        .enumerate()
        .filter(|(i, _)| keep(i))
        .map(|(_, v)| v.clone())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::super::tests::{def, folds, run_on};
    use super::*;
    use crate::ir::strdata::fixture::Fixture;
    use crate::types::TypeId;

    /// An argument of a `_chk` call in a test, and the type it is passed as.
    #[derive(Clone, Copy)]
    enum Arg {
        /// A pointer the call cannot see into.
        Ptr,
        /// The address of a string literal.
        Str(&'static str),
        /// A `size_t` constant.
        Size(u64),
        /// A `size_t` that is one of these two constants.
        Either(u64, u64),
        /// A `size_t` that is one of these two `int` constants, widened.
        EitherInt(i128, i128),
        /// An `int` constant: a flag.
        Int(i128),
        /// A `size_t` the call cannot know.
        Unknown,
    }

    /// The `_chk` call to `f` of `args`; returns its result, which nothing
    /// reads unless `used`.
    fn call(fx: &mut Fixture, f: LibFn, args: &[Arg], used: bool) -> PseudoId {
        let (ptr, ulong, int) = (fx.types.char_ptr_id, fx.types.ulong_id, fx.types.int_id);
        let typed: Vec<(PseudoId, TypeId)> = args
            .iter()
            .map(|&arg| match arg {
                Arg::Ptr => (fx.unknown(), ptr),
                Arg::Str(text) => {
                    let label = fx.literal(text);
                    (fx.addr(&label), ptr)
                }
                Arg::Size(v) => (fx.konst(i128::from(v as i64)), ulong),
                Arg::Either(a, b) => {
                    let (a, b) = (fx.konst(i128::from(a)), fx.konst(i128::from(b)));
                    (fx.select(a, b), ulong)
                }
                Arg::EitherInt(a, b) => {
                    let (a, b) = (fx.konst(a), fx.konst(b));
                    let narrow = fx.select(a, b);
                    let wide = fx.fresh();
                    let mut sext = Instruction::unop(Opcode::Sext, wide, narrow, ulong, 64);
                    sext.src_size = 32;
                    sext.src_typ = Some(int);
                    fx.push(sext);
                    (wide, ulong)
                }
                Arg::Int(v) => (fx.konst(v), int),
                Arg::Unknown => (fx.unknown(), ulong),
            })
            .collect();
        let ret = f.return_type(&fx.types);
        let r = fx.call_typed(f, f.c_name().unwrap(), &typed, ret);
        if used {
            fx.push(Instruction::ret(Some(r)));
        }
        r
    }

    /// What the call to `f` of `args`, its result read, folds to.
    fn fold1(f: LibFn, args: &[Arg]) -> Option<Folded> {
        let mut fx = Fixture::new();
        call(&mut fx, f, args, true);
        folds(&mut fx).into_iter().map(|(_, folded)| folded).next()
    }

    /// Whether the call to `f` of `args` becomes the plain function.
    fn unchecked(f: LibFn, args: &[Arg]) -> bool {
        matches!(fold1(f, args), Some(Folded::Unchecked(row)) if row.chk == f)
    }

    /// The arguments of a call to `f` whose destination is unknown, the
    /// size `size`, the flag 0 and any format "x": what each argument slot
    /// holds by the table's row.
    fn args_with_size(f: LibFn, size: u64) -> Vec<Arg> {
        let row = row(f).unwrap();
        let count = match f {
            LibFn::SprintfChk => 4,
            LibFn::VsnprintfChk => 6,
            LibFn::SnprintfChk | LibFn::VsprintfChk => 5,
            _ => row.size + 1,
        };
        (0..count)
            .map(|i| match row.printf {
                _ if i == row.size => Arg::Size(size),
                Some(p) if i == p.flag => Arg::Int(0),
                Some(p) if i == p.fmt => Arg::Str("x"),
                _ if i == 0 => Arg::Ptr,
                _ => Arg::Unknown,
            })
            .collect()
    }

    /// Every `_chk` function of an unknown object, `(size_t)-1`, is the
    /// plain one, whatever it writes: there is nothing to check against.
    #[test]
    fn an_unknown_object_needs_no_check() {
        for row in &FORTIFIED {
            assert!(
                unchecked(row.chk, &args_with_size(row.chk, u64::MAX)),
                "{:?}",
                row.chk
            );
            // A size of 0 holds nothing, and nothing written is known to be
            // nothing but the empty append below.
            if !matches!(row.writes, Writes::Append { .. }) {
                assert!(
                    !unchecked(row.chk, &args_with_size(row.chk, 0)),
                    "{:?}",
                    row.chk
                );
            }
        }
    }

    /// A count fits when it, or the largest of the constants it may be, is
    /// no more than the size; an unknown count is checked.
    #[test]
    fn a_count_fits_when_its_largest_value_does() {
        use Arg::*;
        let f = LibFn::MemcpyChk;
        assert!(unchecked(f, &[Ptr, Ptr, Size(6), Size(32)]));
        assert!(unchecked(f, &[Ptr, Ptr, Size(6), Size(6)]));
        assert!(!unchecked(f, &[Ptr, Ptr, Size(7), Size(6)]));
        assert!(!unchecked(f, &[Ptr, Ptr, Unknown, Size(32)]));
        assert!(unchecked(f, &[Ptr, Ptr, Either(4, 8), Size(8)]));
        assert!(!unchecked(f, &[Ptr, Ptr, Either(4, 8), Size(7)]));
        // `n = c ? 8 : 4` of two `int`s, widened; a negative one widens to
        // an enormous `size_t`.
        assert!(unchecked(f, &[Ptr, Ptr, EitherInt(4, 8), Size(8)]));
        assert!(!unchecked(f, &[Ptr, Ptr, EitherInt(4, 8), Size(7)]));
        assert!(!unchecked(f, &[Ptr, Ptr, EitherInt(4, -1), Size(64)]));
        // An unknown size is no constant: the check stays.
        assert!(!unchecked(f, &[Ptr, Ptr, Size(1), Unknown]));
        // `snprintf` writes at most its bound.
        let f = LibFn::SnprintfChk;
        assert!(unchecked(
            f,
            &[Ptr, Either(8, 4), Int(0), Size(8), Str("%d"), Unknown]
        ));
        assert!(!unchecked(
            f,
            &[Ptr, Size(9), Int(0), Size(8), Str("%d"), Unknown]
        ));
    }

    /// A string fits with its terminator; of several, the longest must.
    #[test]
    fn a_string_fits_with_its_terminator() {
        use Arg::*;
        let f = LibFn::StrcpyChk;
        assert!(unchecked(f, &[Ptr, Str("abc"), Size(4)]));
        assert!(!unchecked(f, &[Ptr, Str("abc"), Size(3)]));
        assert!(!unchecked(f, &[Ptr, Ptr, Size(32)]));
        let mut fx = Fixture::new();
        let (e, gh) = (fx.literal("e"), fx.literal("gh"));
        let (e, gh) = (fx.addr(&e), fx.addr(&gh));
        let src = fx.select(e, gh);
        let (d, ptr) = (fx.unknown(), fx.types.char_ptr_id);
        for (size, fits) in [(3, true), (2, false)] {
            let size = fx.konst(size);
            let ulong = fx.types.ulong_id;
            let r = fx.call_typed(
                LibFn::StrcpyChk,
                "__strcpy_chk",
                &[(d, ptr), (src, ptr), (size, ulong)],
                ptr,
            );
            fx.push(Instruction::ret(Some(r)));
            let at = fx.func().blocks[0].insns.len() - 2;
            let folded = folds(&mut fx).into_iter().any(|(i, _)| i == at);
            assert_eq!(folded, fits);
        }
    }

    /// An append to an unknown string is unknown in length but where it
    /// appends nothing: an empty string, or a bound of 0.
    #[test]
    fn an_append_to_an_unknown_string_fits_only_when_empty() {
        use Arg::*;
        assert!(unchecked(LibFn::StrcatChk, &[Ptr, Str(""), Size(1)]));
        assert!(!unchecked(LibFn::StrcatChk, &[Ptr, Str("a"), Size(32)]));
        let f = LibFn::StrncatChk;
        assert!(unchecked(f, &[Ptr, Str("abc"), Size(0), Size(1)]));
        assert!(!unchecked(f, &[Ptr, Str("abc"), Size(2), Size(32)]));
    }

    /// `strcat` onto a local array known to hold "ab" writes both strings
    /// and the terminator; `strncat`, as in gcc, is not decided so.
    #[test]
    fn strcat_onto_a_known_string_fits_with_both() {
        for (f, size, fits) in [
            (LibFn::StrcatChk, 6, true),
            (LibFn::StrcatChk, 5, false),
            (LibFn::StrncatChk, 64, false),
        ] {
            let mut fx = Fixture::new();
            let d = fx.local_array("d", 8);
            for (at, byte) in [(0, b'a'), (1, b'b'), (2, 0)] {
                let v = fx.konst(i128::from(byte));
                fx.store_byte(d, at, v);
            }
            let lit = fx.literal("xyz");
            let src = fx.addr(&lit);
            let (ptr, ulong) = (fx.types.char_ptr_id, fx.types.ulong_id);
            let size = fx.konst(size);
            let mut args = vec![(d, ptr), (src, ptr)];
            if f == LibFn::StrncatChk {
                let n = fx.konst(8);
                args.push((n, ulong));
            }
            args.push((size, ulong));
            let r = fx.call_typed(f, f.c_name().unwrap(), &args, ptr);
            fx.push(Instruction::ret(Some(r)));
            let folded = folds(&mut fx)
                .into_iter()
                .any(|(_, folded)| folded == Folded::Unchecked(row(f).unwrap()));
            assert_eq!(folded, fits, "{f:?} into {size:?}");
        }
    }

    /// `sprintf` prints a known length for a format with no directive and
    /// nothing after it, and for `"%s"` of a known string; that and its
    /// terminator must fit. `vsprintf` prints the first only.
    #[test]
    fn a_format_fits_when_its_output_is_known() {
        use Arg::*;
        let f = LibFn::SprintfChk;
        assert!(unchecked(f, &[Ptr, Int(0), Size(4), Str("foo")]));
        assert!(!unchecked(f, &[Ptr, Int(0), Size(3), Str("foo")]));
        assert!(unchecked(f, &[Ptr, Int(0), Size(4), Str("%s"), Str("bar")]));
        assert!(!unchecked(f, &[Ptr, Int(0), Size(32), Str("%s"), Ptr]));
        assert!(!unchecked(f, &[Ptr, Int(0), Size(32), Str("%d"), Int(1)]));
        // `"%s"` of an integer is no string.
        assert!(!unchecked(f, &[Ptr, Int(0), Size(32), Str("%s"), Int(1)]));
        // An argument after a format with no directive: gcc leaves it.
        assert!(!unchecked(f, &[Ptr, Int(0), Size(32), Str("foo"), Int(1)]));
        let f = LibFn::VsprintfChk;
        assert!(unchecked(f, &[Ptr, Int(0), Size(4), Str("foo"), Ptr]));
        assert!(!unchecked(f, &[Ptr, Int(0), Size(32), Str("%s"), Ptr]));
    }

    /// A flag other than 0 keeps the check, even of an unknown object,
    /// unless the format has no directive but `"%s"`.
    #[test]
    fn a_flag_keeps_the_check_of_a_directive() {
        use Arg::*;
        let f = LibFn::SprintfChk;
        let unknown = u64::MAX;
        assert!(!unchecked(
            f,
            &[Ptr, Int(1), Size(unknown), Str("%d"), Int(1)]
        ));
        assert!(!unchecked(
            f,
            &[Ptr, Unknown, Size(unknown), Str("%d"), Int(1)]
        ));
        assert!(unchecked(f, &[Ptr, Int(1), Size(unknown), Str("%s"), Ptr]));
        assert!(unchecked(f, &[Ptr, Int(1), Size(4), Str("foo")]));
        assert!(!unchecked(f, &[Ptr, Int(1), Size(unknown), Ptr]));
    }

    /// A result nobody reads makes `mempcpy`, `stpcpy` and `stpncpy` the
    /// `_chk` forms that answer the destination, of the same arguments;
    /// read, they are decided as themselves.
    #[test]
    fn an_unused_result_drops_the_end_pointer_form() {
        use Arg::*;
        let cases = [
            (
                LibFn::MempcpyChk,
                vec![Ptr, Ptr, Unknown, Size(8)],
                LibFn::MemcpyChk,
            ),
            (LibFn::StpcpyChk, vec![Ptr, Ptr, Size(8)], LibFn::StrcpyChk),
            (
                LibFn::StpncpyChk,
                vec![Ptr, Ptr, Size(4), Size(8)],
                LibFn::StrncpyChk,
            ),
        ];
        for (f, args, to) in cases {
            let mut fx = Fixture::new();
            call(&mut fx, f, &args, false);
            let folded = folds(&mut fx).into_iter().map(|(_, folded)| folded).next();
            let Some(Folded::Discard(Some(new))) = folded else {
                panic!("{f:?} unused: {folded:?}");
            };
            assert_eq!(new.func, to);
            assert_eq!(new.args.len(), args.len());
            assert!(
                fold1(f, &args).is_none_or(|folded| folded == Folded::Unchecked(row(f).unwrap()))
            );
        }
    }

    /// A check that stays, of a string of known length, is the check of a
    /// copy of its bytes and the terminator, against the same size; of a
    /// string not known, or one of several, it stays as it is.
    #[test]
    fn a_known_string_that_overflows_is_a_counted_copy() {
        use Arg::*;
        let Some(Folded::Call(call)) = fold1(LibFn::StrcpyChk, &[Ptr, Str("abc"), Size(2)]) else {
            panic!("a counted copy");
        };
        assert_eq!(call.func, LibFn::MemcpyChk);
        assert!(matches!(call.args[2], Operand::Int(4, _)));
        assert!(matches!(call.args[3], Operand::Value(..)));
        assert_eq!(fold1(LibFn::StrcpyChk, &[Ptr, Ptr, Size(2)]), None);
        // `stpcpy` answers the end, which `memcpy` does not.
        assert_eq!(fold1(LibFn::StpcpyChk, &[Ptr, Str("abc"), Size(2)]), None);
    }

    /// `memcpy` is a `Memcpy` into the call's own result, and `mempcpy` a
    /// `Memcpy` whose result is the end.
    #[test]
    fn a_block_function_becomes_its_operation() {
        use Arg::*;
        let mut fx = Fixture::new();
        let r = call(
            &mut fx,
            LibFn::MemcpyChk,
            &[Ptr, Ptr, Size(4), Size(8)],
            true,
        );
        let insns = run_on(&mut fx, &[]);
        let copy = def(&insns, r);
        assert_eq!((copy.op, copy.src.len()), (Opcode::Memcpy, 3));
        assert_eq!(copy.extra().func_name.as_deref(), Some("memcpy"));

        let mut fx = Fixture::new();
        let r = call(
            &mut fx,
            LibFn::MempcpyChk,
            &[Ptr, Ptr, Size(4), Size(8)],
            true,
        );
        let insns = run_on(&mut fx, &[]);
        assert!(insns.iter().any(|i| i.op == Opcode::Memcpy));
        let end = def(&insns, def(&insns, r).src[0]);
        assert_eq!(end.op, Opcode::Add);
        let copy = insns.iter().find(|i| i.op == Opcode::Memcpy).unwrap();
        assert_eq!(end.src, vec![copy.src[0], copy.src[2]]);
    }

    /// A plain call is the call without its size and flag, by the
    /// program's name for the function, tagged so that it may fold in turn,
    /// with its variadic arguments starting as many places earlier.
    #[test]
    fn a_plain_call_drops_the_checking_arguments() {
        use Arg::*;
        let mut fx = Fixture::new();
        let r = call(
            &mut fx,
            LibFn::SprintfChk,
            &[Ptr, Int(0), Size(u64::MAX), Str("%d"), Int(7)],
            true,
        );
        let at = fx.func().blocks[0].insns.len() - 2;
        let chk = fx.func().blocks[0].insns[at].clone();
        fx.func().blocks[0].insns[at].extra_mut().variadic_arg_start = Some(4);
        let insns = run_on(&mut fx, &[("sprintf", "my_sprintf")]);
        let plain = def(&insns, r);
        assert_eq!(plain.op, Opcode::Call);
        assert_eq!(plain.extra().func_name.as_deref(), Some("my_sprintf"));
        assert_eq!(plain.extra().known, Some(LibFn::Sprintf));
        assert_eq!(plain.extra().callee_binding, CalleeBinding::Library);
        assert_eq!(plain.src, vec![chk.src[0], chk.src[3], chk.src[4]]);
        let types = &chk.extra().arg_types;
        assert_eq!(plain.extra().arg_types, vec![types[0], types[3], types[4]]);
        assert_eq!(plain.extra().variadic_arg_start, Some(2));
    }

    /// The table's argument positions are glibc's: each names an argument
    /// the prototype has.
    #[test]
    fn every_row_names_arguments_its_function_has() {
        for row in &FORTIFIED {
            let params = row.chk.param_count();
            assert!(row.positions().iter().all(|&i| i < params), "{:?}", row.chk);
        }
    }
}
