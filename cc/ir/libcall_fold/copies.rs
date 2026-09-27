//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The <string.h> and <stdio.h> functions that write a string
//
// `strcpy`, `stpcpy`, `strncpy`, `strcat`, `strncat` and `sprintf` each copy
// a source string's bytes into a destination. When the source's length is
// known (`strdata`: a string whose bytes are known, or a choice among
// strings of one length), how many bytes are written is known too, so the
// call is a `memcpy` of that many -- which `memexpand` turns into loads and
// stores when it is short, and whose loads `loadfwd` then answers from the
// string. The bytes are copied from the source pointer the program passed,
// never from the compiler's copy of them, so a choice among several strings
// needs no answer about which one it is.
//
// A call whose result nobody reads and whose source is unknown still gets
// simpler: `stpcpy` and `sprintf(d, "%s", s)` are then `strcpy`.
//

use super::{callee_symbol, make_call, offset, Facts, FoldCtx, Folded, NewCall, Operand};
use crate::ir::build::Builder;
use crate::ir::memexpand::INLINE_LIMIT_BYTES;
use crate::ir::strdata::Len;
use crate::ir::{Instruction, Opcode, PseudoId};
use crate::parse::ast::LibFn;
use crate::types::TypeId;

/// What a call that writes a string does instead.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Write {
    /// `n` bytes of `src` copied to `to`, then `zeros` zero bytes after them.
    Copy {
        to: Place,
        src: PseudoId,
        n: u64,
        zeros: u64,
        answer: Answer,
    },
    /// `strcpy(dest, src)`, for a call whose result nobody reads.
    Strcpy { dest: PseudoId, src: PseudoId },
}

/// Where a copy's bytes go.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Place {
    /// At the pointer.
    At(PseudoId),
    /// Over the terminator of the string the pointer points at: `strcat`.
    End(PseudoId),
}

/// What a call that was copied in place answers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Answer {
    /// The pointer advanced this many bytes.
    Ptr(PseudoId, i64),
    /// This count of characters: `sprintf`.
    Int(i128),
}

impl Write {
    /// The library functions this calls, by their C names.
    pub(super) fn calls(&self) -> &'static [&'static str] {
        match self {
            Write::Copy {
                to: Place::End(_), ..
            } => &["strlen", "memcpy"],
            Write::Copy { zeros: 0, .. } => &["memcpy"],
            Write::Copy { .. } => &["memcpy", "memset"],
            Write::Strcpy { .. } => &["strcpy"],
        }
    }
}

/// What the call `insn` to `f`, one of the functions that write a string,
/// folds to.
pub(super) fn fold(f: LibFn, insn: &Instruction, facts: &Facts) -> Option<Folded> {
    match (f, insn.src.as_slice()) {
        (LibFn::Strcpy, &[d, s]) => {
            let len = known_len(facts, s)?;
            Some(copy_string(Place::At(d), s, len, Answer::Ptr(d, 0)))
        }
        (LibFn::Stpcpy, &[d, s]) => stpcpy(facts, insn, d, s),
        (LibFn::Strncpy, &[d, s, n]) => strncpy(facts, d, s, n),
        (LibFn::Strcat, &[d, s]) => strcat(facts, d, s),
        (LibFn::Strncat, &[d, s, n]) => strncat(facts, d, s, n),
        (LibFn::Sprintf, &[d, fmt, ref rest @ ..]) => sprintf(facts, insn, d, fmt, rest),
        _ => None,
    }
}

/// The length of the string `p` points at, when it is a constant.
fn known_len(facts: &Facts, p: PseudoId) -> Option<u64> {
    match facts.strings.string_len(p)? {
        Len::Const(len) => Some(len),
        Len::MinusOffset { .. } => None,
    }
}

/// The string of `len` characters at `src`, terminator and all, copied to
/// `to`.
fn copy_string(to: Place, src: PseudoId, len: u64, answer: Answer) -> Folded {
    Folded::Write(Write::Copy {
        to,
        src,
        n: len + 1,
        zeros: 0,
        answer,
    })
}

/// `stpcpy(d, s)` answers the terminator it wrote: `d` plus the length.
fn stpcpy(facts: &Facts, insn: &Instruction, d: PseudoId, s: PseudoId) -> Option<Folded> {
    match known_len(facts, s) {
        Some(len) => Some(copy_string(
            Place::At(d),
            s,
            len,
            Answer::Ptr(d, i64::try_from(len).ok()?),
        )),
        None => unused_strcpy(facts, insn, d, s),
    }
}

/// `strcpy(d, s)` in place of a call that copies the same bytes, when
/// nothing reads what it answers.
fn unused_strcpy(facts: &Facts, insn: &Instruction, d: PseudoId, s: PseudoId) -> Option<Folded> {
    facts
        .result_unused(insn)
        .then_some(Folded::Write(Write::Strcpy { dest: d, src: s }))
}

/// `strncpy(d, s, n)` writes exactly `n` bytes: the string, cut short or
/// padded out with zeros (C17 7.24.2.4p3). The padding is written only when
/// all of it is short enough to be stores.
fn strncpy(facts: &Facts, d: PseudoId, s: PseudoId, n: PseudoId) -> Option<Folded> {
    let n = u64::try_from(facts.unsigned(n, 64)?).ok()?;
    if n == 0 {
        return Some(Folded::Offset(d, 0));
    }
    let len = known_len(facts, s)?;
    let copied = n.min(len + 1);
    let zeros = n - copied;
    if zeros > INLINE_LIMIT_BYTES as u64 {
        return None;
    }
    Some(Folded::Write(Write::Copy {
        to: Place::At(d),
        src: s,
        n: copied,
        zeros,
        answer: Answer::Ptr(d, 0),
    }))
}

/// `strcat(d, s)`: `s` copied over the terminator of `d`, and `d` answered.
fn strcat(facts: &Facts, d: PseudoId, s: PseudoId) -> Option<Folded> {
    let len = known_len(facts, s)?;
    Some(append(d, s, len))
}

/// `s`, `len` characters long, appended to `d`: nothing at all for an empty
/// `s`.
fn append(d: PseudoId, s: PseudoId, len: u64) -> Folded {
    if len == 0 {
        return Folded::Offset(d, 0);
    }
    copy_string(Place::End(d), s, len, Answer::Ptr(d, 0))
}

/// `strncat(d, s, n)` appends at most `n` characters and then a terminator
/// (C17 7.24.3.2p2), so a bound of 0 or an empty `s` appends nothing, and a
/// bound of at least the length is `strcat`.
fn strncat(facts: &Facts, d: PseudoId, s: PseudoId, n: PseudoId) -> Option<Folded> {
    let n = facts.unsigned(n, 64);
    if n == Some(0) {
        return Some(Folded::Offset(d, 0));
    }
    let len = known_len(facts, s)?;
    (len == 0 || n.is_some_and(|n| n >= u128::from(len))).then(|| append(d, s, len))
}

/// `sprintf(d, fmt)` of a format with no conversion, and `sprintf(d, "%s",
/// s)`: each writes a string and answers its length.
fn sprintf(
    facts: &Facts,
    insn: &Instruction,
    d: PseudoId,
    fmt: PseudoId,
    rest: &[PseudoId],
) -> Option<Folded> {
    let text = facts.strings.string_at(fmt)?.c_str()?;
    let s = match (text, rest) {
        (b"%s", &[s]) => s,
        (_, []) if !text.contains(&b'%') => fmt,
        _ => return None,
    };
    match known_len(facts, s) {
        Some(len) => Some(copy_string(
            Place::At(d),
            s,
            len,
            Answer::Int(i128::from(len)),
        )),
        None => unused_strcpy(facts, insn, d, s),
    }
}

/// The instructions that do what `write` says, and the call's result,
/// `result` -- its pseudo, type and width -- given what it answers.
pub(super) fn materialize(
    b: &mut Builder,
    ctx: &FoldCtx,
    write: Write,
    result: Option<(PseudoId, TypeId, u32)>,
) {
    let char_ptr = b.types.char_ptr_id;
    let answer = match write {
        Write::Strcpy { dest, src } => {
            let call = NewCall {
                func: LibFn::Strcpy,
                name: "strcpy",
                args: vec![
                    Operand::Value(dest, char_ptr),
                    Operand::Value(src, b.types.const_char_ptr_id),
                ],
            };
            make_call(b, ctx, None, char_ptr, call);
            return;
        }
        Write::Copy {
            to,
            src,
            n,
            zeros,
            answer,
        } => {
            let at = place(b, ctx, to);
            block(b, ctx, Opcode::Memcpy, at, src, n);
            if zeros > 0 {
                let end = offset(b, at, n as i64, char_ptr, 64);
                let int = b.types.int_id;
                let zero = b.constant(0, int, 32);
                block(b, ctx, Opcode::Memset, end, zero, zeros);
            }
            answer
        }
    };
    let Some((target, typ, size)) = result else {
        return;
    };
    let value = match answer {
        Answer::Ptr(p, k) => offset(b, p, k, typ, size),
        Answer::Int(v) => b.constant(v, typ, size),
    };
    b.copy_into(target, value, typ, size);
}

/// The address `to` names: for the end of a string, a call to `strlen`,
/// itself tagged so that it may fold in turn.
fn place(b: &mut Builder, ctx: &FoldCtx, to: Place) -> PseudoId {
    match to {
        Place::At(p) => p,
        Place::End(p) => {
            let ulong = b.types.ulong_id;
            let len = b.func.alloc_pseudo();
            let call = NewCall {
                func: LibFn::Strlen,
                name: "strlen",
                args: vec![Operand::Value(p, b.types.const_char_ptr_id)],
            };
            make_call(b, ctx, Some(len), ulong, call);
            b.binop(Opcode::Add, p, len, b.types.char_ptr_id, 64)
        }
    }
}

/// `op` -- `Memcpy` or `Memset` -- of `n` bytes at `dest` from `second`, a
/// call to the program's own name for the function.
fn block(b: &mut Builder, ctx: &FoldCtx, op: Opcode, dest: PseudoId, second: PseudoId, n: u64) {
    let ulong = b.types.ulong_id;
    let n = b.constant(i128::from(n), ulong, 64);
    let name = match op {
        Opcode::Memcpy => "memcpy",
        _ => "memset",
    };
    let callee = callee_symbol(ctx, name);
    let void_ptr = b.types.void_ptr_id;
    let result = b.func.alloc_pseudo();
    b.push(
        Instruction::new(op)
            .with_func(callee)
            .with_target(result)
            .with_src3(dest, second, n)
            .with_type_and_size(void_ptr, 64),
    );
}

#[cfg(test)]
mod tests {
    use super::super::tests::{def, folds, ops, run_on};
    use super::*;
    use crate::ir::strdata::fixture::Fixture;

    /// What a call to `f` of `args`, returning `ret`, added to `fx`, folds
    /// to.
    fn fold1(fx: &mut Fixture, f: LibFn, args: &[PseudoId], ret: TypeId) -> Option<Folded> {
        fx.call(f, "callee", args, ret);
        let at = fx.func().blocks[0].insns.len() - 1;
        folds(fx)
            .into_iter()
            .find_map(|(i, folded)| (i == at).then_some(folded))
    }

    fn copy(to: Place, src: PseudoId, n: u64, zeros: u64, answer: Answer) -> Option<Folded> {
        Some(Folded::Write(Write::Copy {
            to,
            src,
            n,
            zeros,
            answer,
        }))
    }

    /// A known length is a copy of it and the terminator; an unknown one
    /// is left alone, and so is a length that depends on an offset.
    #[test]
    fn strcpy_and_stpcpy_of_a_known_length() {
        let mut fx = Fixture::new();
        let lc = fx.literal("abcde");
        let s = fx.addr(&lc);
        let (d, u, i) = (fx.unknown(), fx.unknown(), fx.unknown());
        let ptr = fx.types.char_ptr_id;
        let at = Place::At(d);
        assert_eq!(
            fold1(&mut fx, LibFn::Strcpy, &[d, s], ptr),
            copy(at, s, 6, 0, Answer::Ptr(d, 0))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Stpcpy, &[d, s], ptr),
            copy(at, s, 6, 0, Answer::Ptr(d, 5))
        );
        assert_eq!(fold1(&mut fx, LibFn::Strcpy, &[d, u], ptr), None);
        let si = fx.op(Opcode::Add, s, i);
        assert_eq!(fold1(&mut fx, LibFn::Strcpy, &[d, si], ptr), None);
    }

    /// Every string a choice can pick has the one length, so the choice
    /// itself is copied from.
    #[test]
    fn a_choice_of_strings_of_one_length_is_copied_from() {
        let mut fx = Fixture::new();
        let (a, b, c) = (fx.literal("bar"), fx.literal("foo"), fx.literal("quux"));
        let (a, b, c) = (fx.addr(&a), fx.addr(&b), fx.addr(&c));
        let d = fx.unknown();
        let ptr = fx.types.char_ptr_id;
        let same = fx.select(a, b);
        assert_eq!(
            fold1(&mut fx, LibFn::Strcpy, &[d, same], ptr),
            copy(Place::At(d), same, 4, 0, Answer::Ptr(d, 0))
        );
        let differ = fx.select(a, c);
        assert_eq!(fold1(&mut fx, LibFn::Strcpy, &[d, differ], ptr), None);
    }

    /// `stpcpy` of an unknown string is `strcpy` only when nothing reads
    /// its answer.
    #[test]
    fn stpcpy_of_an_unknown_string_is_strcpy_when_unused() {
        let mut fx = Fixture::new();
        let (d, u) = (fx.unknown(), fx.unknown());
        let ptr = fx.types.char_ptr_id;
        assert_eq!(
            fold1(&mut fx, LibFn::Stpcpy, &[d, u], ptr),
            Some(Folded::Write(Write::Strcpy { dest: d, src: u }))
        );
        let r = fx.call(LibFn::Stpcpy, "stpcpy", &[d, u], ptr);
        fx.op(Opcode::Add, r, d);
        let at = fx.func().blocks[0].insns.len() - 2;
        assert!(folds(&fx).iter().all(|&(i, _)| i != at));
    }

    /// `strncpy` copies at most the string and its terminator, pads with
    /// zeros past it, and of no bytes writes nothing.
    #[test]
    fn strncpy_cuts_short_or_pads() {
        let mut fx = Fixture::new();
        let lc = fx.literal("hello");
        let s = fx.addr(&lc);
        let (d, u) = (fx.unknown(), fx.unknown());
        let (zero, four, six, ten, far) = (
            fx.konst(0),
            fx.konst(4),
            fx.konst(6),
            fx.konst(10),
            fx.konst(6 + INLINE_LIMIT_BYTES as i128 + 1),
        );
        let ptr = fx.types.char_ptr_id;
        let at = Place::At(d);
        let ans = Answer::Ptr(d, 0);
        assert_eq!(
            fold1(&mut fx, LibFn::Strncpy, &[d, u, zero], ptr),
            Some(Folded::Offset(d, 0))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Strncpy, &[d, s, four], ptr),
            copy(at, s, 4, 0, ans)
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Strncpy, &[d, s, six], ptr),
            copy(at, s, 6, 0, ans)
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Strncpy, &[d, s, ten], ptr),
            copy(at, s, 6, 4, ans)
        );
        assert_eq!(fold1(&mut fx, LibFn::Strncpy, &[d, s, far], ptr), None);
        assert_eq!(fold1(&mut fx, LibFn::Strncpy, &[d, s, u], ptr), None);
        assert_eq!(fold1(&mut fx, LibFn::Strncpy, &[d, u, four], ptr), None);
    }

    #[test]
    fn strcat_and_strncat_append_at_the_end() {
        let mut fx = Fixture::new();
        let (foo, empty) = (fx.literal("foo"), fx.literal(""));
        let (s, e) = (fx.addr(&foo), fx.addr(&empty));
        let (d, u, n) = (fx.unknown(), fx.unknown(), fx.unknown());
        let (zero, two, three, big) = (fx.konst(0), fx.konst(2), fx.konst(3), fx.konst(100));
        let ptr = fx.types.char_ptr_id;
        let end = copy(Place::End(d), s, 4, 0, Answer::Ptr(d, 0));
        let nothing = Some(Folded::Offset(d, 0));
        assert_eq!(fold1(&mut fx, LibFn::Strcat, &[d, s], ptr), end);
        assert_eq!(fold1(&mut fx, LibFn::Strcat, &[d, e], ptr), nothing);
        assert_eq!(fold1(&mut fx, LibFn::Strcat, &[d, u], ptr), None);
        assert_eq!(fold1(&mut fx, LibFn::Strncat, &[d, s, three], ptr), end);
        assert_eq!(fold1(&mut fx, LibFn::Strncat, &[d, s, big], ptr), end);
        assert_eq!(fold1(&mut fx, LibFn::Strncat, &[d, u, zero], ptr), nothing);
        assert_eq!(fold1(&mut fx, LibFn::Strncat, &[d, e, n], ptr), nothing);
        assert_eq!(fold1(&mut fx, LibFn::Strncat, &[d, s, two], ptr), None);
        assert_eq!(fold1(&mut fx, LibFn::Strncat, &[d, s, n], ptr), None);
    }

    #[test]
    fn sprintf_of_a_plain_format_or_one_string() {
        let mut fx = Fixture::new();
        let (foo, pct_s, pct_d, pct) = (
            fx.literal("foo"),
            fx.literal("%s"),
            fx.literal("%d"),
            fx.literal("100%%"),
        );
        let (foo, pct_s, pct_d, pct) = (
            fx.addr(&foo),
            fx.addr(&pct_s),
            fx.addr(&pct_d),
            fx.addr(&pct),
        );
        let (d, u) = (fx.unknown(), fx.unknown());
        let int = fx.types.int_id;
        let at = Place::At(d);
        assert_eq!(
            fold1(&mut fx, LibFn::Sprintf, &[d, foo], int),
            copy(at, foo, 4, 0, Answer::Int(3))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Sprintf, &[d, pct_s, foo], int),
            copy(at, foo, 4, 0, Answer::Int(3))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Sprintf, &[d, pct_s, u], int),
            Some(Folded::Write(Write::Strcpy { dest: d, src: u }))
        );
        // A format with extra arguments, a conversion other than one `%s`,
        // or an escaped `%` is left to the library.
        assert_eq!(fold1(&mut fx, LibFn::Sprintf, &[d, foo, u], int), None);
        assert_eq!(fold1(&mut fx, LibFn::Sprintf, &[d, pct_d, u], int), None);
        assert_eq!(fold1(&mut fx, LibFn::Sprintf, &[d, pct], int), None);
        assert_eq!(fold1(&mut fx, LibFn::Sprintf, &[d, u], int), None);
    }

    /// `strcpy` becomes a `memcpy` of the length and the terminator, and
    /// the call's own result is the destination.
    #[test]
    fn a_copy_is_a_memcpy_and_answers_the_destination() {
        let mut fx = Fixture::new();
        let lc = fx.literal("abc");
        let s = fx.addr(&lc);
        let d = fx.unknown();
        let ptr = fx.types.char_ptr_id;
        let r = fx.call(LibFn::Strcpy, "strcpy", &[d, s], ptr);
        let insns = run_on(&mut fx, &[("memcpy", "my_memcpy")]);
        assert!(!ops(&insns).contains(&Opcode::Call));
        let memcpy = insns.iter().find(|i| i.op == Opcode::Memcpy).unwrap();
        assert_eq!(memcpy.library_callee(), "my_memcpy");
        assert_eq!(&memcpy.src[..2], &[d, s]);
        assert_eq!(fx.module.functions[0].const_val(memcpy.src[2]), Some(4));
        let copy = def(&insns, r);
        assert_eq!((copy.op, copy.src[0]), (Opcode::Copy, d));
    }

    /// `strcat` finds the end with a tagged `strlen`, and copies there.
    #[test]
    fn strcat_copies_to_the_end_strlen_finds() {
        let mut fx = Fixture::new();
        let lc = fx.literal("abc");
        let s = fx.addr(&lc);
        let d = fx.unknown();
        let ptr = fx.types.char_ptr_id;
        fx.call(LibFn::Strcat, "strcat", &[d, s], ptr);
        let insns = run_on(&mut fx, &[]);
        let strlen = insns.iter().find(|i| i.op == Opcode::Call).unwrap();
        assert_eq!(
            (strlen.func_name.as_deref(), strlen.known, &strlen.src[..]),
            (Some("strlen"), Some(LibFn::Strlen), &[d][..])
        );
        assert!(strlen.abi_info.is_some());
        let memcpy = insns.iter().find(|i| i.op == Opcode::Memcpy).unwrap();
        let end = def(&insns, memcpy.src[0]);
        assert_eq!(
            (end.op, &end.src[..]),
            (Opcode::Add, &[d, strlen.target.unwrap()][..])
        );
    }

    /// `strncpy` pads with a `memset` of zero after the string.
    #[test]
    fn strncpy_pads_with_a_memset() {
        let mut fx = Fixture::new();
        let lc = fx.literal("ab");
        let s = fx.addr(&lc);
        let d = fx.unknown();
        let eight = fx.konst(8);
        let ptr = fx.types.char_ptr_id;
        fx.call(LibFn::Strncpy, "strncpy", &[d, s, eight], ptr);
        let insns = run_on(&mut fx, &[]);
        let func = &fx.module.functions[0];
        let memset = insns.iter().find(|i| i.op == Opcode::Memset).unwrap();
        assert_eq!(func.const_val(memset.src[1]), Some(0));
        assert_eq!(func.const_val(memset.src[2]), Some(5));
        let end = def(&insns, memset.src[0]);
        assert_eq!((end.op, end.src[0]), (Opcode::Add, d));
        assert_eq!(func.const_val(end.src[1]), Some(3));
    }

    /// `sprintf` answers the count, and one whose `%s` is unknown and
    /// unread becomes a `strcpy` with no result.
    #[test]
    fn sprintf_answers_the_count_or_becomes_strcpy() {
        let mut fx = Fixture::new();
        let lc = fx.literal("foo");
        let fmt = fx.addr(&lc);
        let d = fx.unknown();
        let int = fx.types.int_id;
        let r = fx.call(LibFn::Sprintf, "sprintf", &[d, fmt], int);
        let insns = run_on(&mut fx, &[]);
        let copy = def(&insns, r);
        assert_eq!(fx.module.functions[0].const_val(copy.src[0]), Some(3));

        let mut fx = Fixture::new();
        let lc = fx.literal("%s");
        let fmt = fx.addr(&lc);
        let (d, u) = (fx.unknown(), fx.unknown());
        fx.call(LibFn::Sprintf, "sprintf", &[d, fmt, u], int);
        let insns = run_on(&mut fx, &[("strcpy", "my_strcpy")]);
        let call = insns.iter().find(|i| i.op == Opcode::Call).unwrap();
        assert_eq!(
            (
                call.func_name.as_deref(),
                call.known,
                call.target,
                &call.src[..]
            ),
            (Some("my_strcpy"), Some(LibFn::Strcpy), None, &[d, u][..])
        );
        assert_eq!(call.typ, Some(fx.types.char_ptr_id));
    }

    /// Inside `strlen`, `strcat` does not make a call to `strlen`; inside
    /// `strcpy`, `stpcpy` does not become `strcpy`.
    #[test]
    fn a_write_never_calls_the_function_it_is_in() {
        for (inside, f) in [("strlen", LibFn::Strcat), ("strcpy", LibFn::Stpcpy)] {
            let mut fx = Fixture::new();
            fx.func().name = inside.to_string();
            let lc = fx.literal("abc");
            let s = fx.addr(&lc);
            let (d, u) = (fx.unknown(), fx.unknown());
            let ptr = fx.types.char_ptr_id;
            let src = if f == LibFn::Strcat { s } else { u };
            fx.call(f, "callee", &[d, src], ptr);
            assert!(folds(&fx).is_empty(), "{inside}");
        }
    }
}
