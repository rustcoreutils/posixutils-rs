//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The <string.h> functions that read strings and compute a value
//
// Each answers from the bytes of the strings its pointers read, where those
// are known (`strdata`), and otherwise from what its constant arguments
// alone decide: a length of zero compares equal, and an empty string
// compares as the other side's first byte. A pointer result is the argument
// it points into, advanced, so the answer is an address in the very object
// the program passed.
//

use super::{Byte, CallSite, Folded, NewCall, Operand, ValueFold};
use crate::ir::strdata::{Len, StrRef};
use crate::ir::{Instruction, PseudoId};
use crate::parse::ast::LibFn;

/// What the call `insn` to `f`, one of the string functions, folds to.
pub(super) fn fold(f: LibFn, insn: &Instruction, facts: &CallSite) -> Option<Folded> {
    match (f, insn.src.as_slice()) {
        (LibFn::Strlen, &[s]) => strlen(facts, s),
        (LibFn::Strnlen, &[s, n]) => strnlen(facts, s, n),
        (LibFn::Strcmp, &[a, b]) => compare(facts, a, b, Limit::Terminator),
        (LibFn::Strncmp, &[a, b, n]) => {
            let n = facts.size_t(n)?;
            compare(facts, a, b, Limit::Bytes(n, Stop::AtTerminator))
        }
        (LibFn::Memcmp, &[a, b, n]) => {
            let n = facts.size_t(n)?;
            compare(facts, a, b, Limit::Bytes(n, Stop::Never))
        }
        (LibFn::Strchr, &[s, c]) => find_char(facts, s, c, Direction::First),
        (LibFn::Strrchr, &[s, c]) => find_char(facts, s, c, Direction::Last),
        (LibFn::Memchr, &[s, c, n]) => memchr(facts, s, c, n),
        (LibFn::Strstr, &[h, n]) => strstr(facts, h, n),
        (LibFn::Strpbrk, &[s, set]) => strpbrk(facts, s, set),
        (LibFn::Strcspn, &[s, reject]) => strcspn(facts, s, reject),
        _ => None,
    }
}

/// An offset into a string, as the pointer arithmetic it becomes.
fn offset(p: PseudoId, at: usize) -> Option<Folded> {
    Some(Folded::Value(ValueFold::Offset(p, i64::try_from(at).ok()?)))
}

fn strlen(facts: &CallSite, s: PseudoId) -> Option<Folded> {
    Some(match facts.strings.string_len(s)? {
        Len::Const(len) => Folded::Value(ValueFold::Int(i128::from(len))),
        Len::MinusOffset { len, var } => Folded::Value(ValueFold::LenMinus {
            len: i64::try_from(len).ok()?,
            var,
        }),
    })
}

/// `strnlen(s, n)`: the smaller of the two, which needs no read of `s`
/// past its terminator or past `n` bytes.
fn strnlen(facts: &CallSite, s: PseudoId, n: PseudoId) -> Option<Folded> {
    let n_const = facts.size_t(n);
    if n_const == Some(0) {
        return Some(Folded::Value(ValueFold::Int(0)));
    }
    let Len::Const(len) = facts.strings.string_len(s)? else {
        return None;
    };
    Some(match n_const {
        Some(n) => Folded::Value(ValueFold::Int(i128::from(len).min(n as i128))),
        None => Folded::Value(ValueFold::AtMost {
            n,
            len: i64::try_from(len).ok()?,
        }),
    })
}

/// How far a comparison reads.
#[derive(Clone, Copy)]
enum Limit {
    /// To the first terminator: `strcmp`.
    Terminator,
    /// At most this many bytes, stopping at a terminator or not.
    Bytes(u128, Stop),
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Stop {
    AtTerminator,
    Never,
}

impl Limit {
    fn allows(self, i: usize) -> bool {
        match self {
            Limit::Terminator => true,
            Limit::Bytes(n, _) => (i as u128) < n,
        }
    }

    fn stops_at_terminator(self) -> bool {
        !matches!(self, Limit::Bytes(_, Stop::Never))
    }
}

/// `strcmp`, `strncmp` or `memcmp` of `a` and `b`.
fn compare(facts: &CallSite, a: PseudoId, b: PseudoId, limit: Limit) -> Option<Folded> {
    if !limit.allows(0) || facts.same_pointer(a, b) {
        return Some(Folded::Value(ValueFold::Int(0)));
    }
    let (sa, sb) = (facts.strings.string_at(a), facts.strings.string_at(b));
    if let (Some(sa), Some(sb)) = (sa, sb) {
        if let Some(v) = compare_known(sa, sb, limit) {
            return Some(Folded::Value(ValueFold::Int(v)));
        }
    }
    let first = |s: Option<StrRef>, p| match s.and_then(|s| s.bytes().first().copied()) {
        Some(v) => Byte::Known(v),
        None => Byte::At(p),
    };
    let (x, y) = (first(sa, a), first(sb, b));
    // An empty string ends the comparison at the first byte, and so does a
    // limit of one: either way the first bytes' difference is the answer.
    let decided_by_first = !limit.allows(1)
        || (limit.stops_at_terminator() && (x == Byte::Known(0) || y == Byte::Known(0)));
    decided_by_first.then_some(Folded::Value(ValueFold::ByteDiff(x, y)))
}

/// The comparison of two known byte strings, as the difference of the
/// first unequal bytes read as `unsigned char`; `None` if it would read
/// past the end of either object.
fn compare_known(a: StrRef, b: StrRef, limit: Limit) -> Option<i128> {
    let mut i = 0;
    while limit.allows(i) {
        let (x, y) = (*a.bytes().get(i)?, *b.bytes().get(i)?);
        if x != y {
            return Some(i128::from(x) - i128::from(y));
        }
        if x == 0 && limit.stops_at_terminator() {
            break;
        }
        i += 1;
    }
    Some(0)
}

#[derive(Clone, Copy)]
enum Direction {
    First,
    Last,
}

/// `strchr` or `strrchr` of a known string: the character converted to
/// `char` (C17 7.24.5.2p2), and the terminator found where it is. The last
/// terminator of any string is its first, so `strrchr(s, 0)` is
/// `strchr(s, 0)`, which need not scan backwards.
fn find_char(facts: &CallSite, s: PseudoId, c: PseudoId, dir: Direction) -> Option<Folded> {
    let c = facts.unsigned(c, 8)? as u8;
    let Some(text) = facts.c_str(s) else {
        return (c == 0 && matches!(dir, Direction::Last)).then(|| strchr_call(facts, s, 0));
    };
    if c == 0 {
        return offset(s, text.len());
    }
    let at = match dir {
        Direction::First => text.iter().position(|&b| b == c),
        Direction::Last => text.iter().rposition(|&b| b == c),
    };
    match at {
        Some(at) => offset(s, at),
        None => Some(Folded::Value(ValueFold::Null)),
    }
}

/// `memchr(s, c, n)` over a known object: found within the first `n`
/// bytes, or not there when all `n` are inside the object.
fn memchr(facts: &CallSite, s: PseudoId, c: PseudoId, n: PseudoId) -> Option<Folded> {
    let n = facts.size_t(n)?;
    if n == 0 {
        return Some(Folded::Value(ValueFold::Null));
    }
    let c = facts.unsigned(c, 8)? as u8;
    let bytes = facts.strings.string_at(s)?.bytes();
    let window = usize::try_from(n).map_or(bytes.len(), |n| n.min(bytes.len()));
    match bytes[..window].iter().position(|&b| b == c) {
        Some(at) => offset(s, at),
        None if n <= bytes.len() as u128 => Some(Folded::Value(ValueFold::Null)),
        None => None,
    }
}

/// `strchr(s, c)`, the call a one-character search becomes.
fn strchr_call(facts: &CallSite, s: PseudoId, c: u8) -> Folded {
    let types = facts.types;
    Folded::Call(NewCall {
        func: LibFn::Strchr,
        args: vec![
            Operand::Value(s, types.const_char_ptr_id),
            Operand::Int(i128::from(c), types.int_id),
        ],
    })
}

fn strstr(facts: &CallSite, h: PseudoId, n: PseudoId) -> Option<Folded> {
    let needle = facts.c_str(n)?;
    if needle.is_empty() {
        return offset(h, 0);
    }
    match facts.c_str(h) {
        Some(hay) => match hay.windows(needle.len()).position(|w| w == needle) {
            Some(at) => offset(h, at),
            None => Some(Folded::Value(ValueFold::Null)),
        },
        None => (needle.len() == 1).then(|| strchr_call(facts, h, needle[0])),
    }
}

fn strpbrk(facts: &CallSite, s: PseudoId, set: PseudoId) -> Option<Folded> {
    let set = facts.c_str(set)?;
    if set.is_empty() {
        return Some(Folded::Value(ValueFold::Null));
    }
    match facts.c_str(s) {
        Some(text) => match text.iter().position(|b| set.contains(b)) {
            Some(at) => offset(s, at),
            None => Some(Folded::Value(ValueFold::Null)),
        },
        None => (set.len() == 1).then(|| strchr_call(facts, s, set[0])),
    }
}

fn strcspn(facts: &CallSite, s: PseudoId, reject: PseudoId) -> Option<Folded> {
    let text = facts.c_str(s);
    if text.is_some_and(<[u8]>::is_empty) {
        return Some(Folded::Value(ValueFold::Int(0)));
    }
    let reject = facts.c_str(reject)?;
    match text {
        Some(text) => {
            let span = text.iter().take_while(|b| !reject.contains(b)).count();
            Some(Folded::Value(ValueFold::Int(span as i128)))
        }
        None if reject.is_empty() => Some(Folded::Call(NewCall {
            func: LibFn::Strlen,
            args: vec![Operand::Value(s, facts.types.const_char_ptr_id)],
        })),
        None => None,
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::folds;
    use super::*;
    use crate::ir::strdata::fixture::Fixture;
    use crate::ir::Opcode;

    /// What a call to `f` of `args`, added to `fx`, folds to.
    fn fold1(fx: &mut Fixture, f: LibFn, args: &[PseudoId]) -> Option<Folded> {
        let ret = fx.types.int_id;
        fx.call(f, "callee", args, ret);
        let at = fx.func().blocks[0].insns.len() - 1;
        folds(fx)
            .into_iter()
            .find_map(|(i, folded)| (i == at).then_some(folded))
    }

    #[test]
    fn strlen_of_a_known_string_and_of_an_unknown_offset_into_one() {
        let mut fx = Fixture::new();
        let lc = fx.literal("hello");
        let p = fx.addr(&lc);
        assert_eq!(
            fold1(&mut fx, LibFn::Strlen, &[p]),
            Some(Folded::Value(ValueFold::Int(5)))
        );
        let i = fx.unknown();
        let q = fx.op(Opcode::Add, p, i);
        assert_eq!(
            fold1(&mut fx, LibFn::Strlen, &[q]),
            Some(Folded::Value(ValueFold::LenMinus { len: 5, var: i }))
        );
        let u = fx.unknown();
        assert_eq!(fold1(&mut fx, LibFn::Strlen, &[u]), None);
    }

    /// A local array's length is what the stores before *this* call left:
    /// a store after one call is seen by the next and not by it.
    #[test]
    fn strlen_of_a_local_array_reads_it_at_the_call() {
        let mut fx = Fixture::new();
        let s = fx.local_array("s.0", 4);
        let (a, z) = (fx.konst(b'a'.into()), fx.konst(0));
        fx.store_byte(s, 0, a);
        fx.store_byte(s, 1, z);
        assert_eq!(
            fold1(&mut fx, LibFn::Strlen, &[s]),
            Some(Folded::Value(ValueFold::Int(1)))
        );
        fx.store_byte(s, 1, a);
        fx.store_byte(s, 2, z);
        assert_eq!(
            fold1(&mut fx, LibFn::Strlen, &[s]),
            Some(Folded::Value(ValueFold::Int(2)))
        );
        let n = fx.unknown();
        assert_eq!(
            fold1(&mut fx, LibFn::Strnlen, &[s, n]),
            Some(Folded::Value(ValueFold::AtMost { n, len: 2 }))
        );
    }

    #[test]
    fn strnlen_is_the_smaller_even_of_an_unknown_bound() {
        let mut fx = Fixture::new();
        let lc = fx.literal("123");
        let p = fx.addr(&lc);
        let (two, nine, zero) = (fx.konst(2), fx.konst(9), fx.konst(0));
        let n = fx.unknown();
        let u = fx.unknown();
        assert_eq!(
            fold1(&mut fx, LibFn::Strnlen, &[p, two]),
            Some(Folded::Value(ValueFold::Int(2)))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Strnlen, &[p, nine]),
            Some(Folded::Value(ValueFold::Int(3)))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Strnlen, &[p, n]),
            Some(Folded::Value(ValueFold::AtMost { n, len: 3 }))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Strnlen, &[u, zero]),
            Some(Folded::Value(ValueFold::Int(0)))
        );
        assert_eq!(fold1(&mut fx, LibFn::Strnlen, &[u, two]), None);
    }

    /// Two known strings compare to the difference of their first unequal
    /// bytes, as `unsigned char`; one pointer compares equal to itself; an
    /// empty string, or a length of one, leaves the first bytes to decide.
    #[test]
    fn comparisons_fold_where_the_first_bytes_or_the_strings_decide() {
        let mut fx = Fixture::new();
        let (hw, he, hi) = (
            fx.literal("hello"),
            fx.literal("help"),
            fx.literal("\u{ff}"),
        );
        let (a, b, c) = (fx.addr(&hw), fx.addr(&he), fx.addr(&hi));
        let empty = fx.literal("");
        let e = fx.addr(&empty);
        let u = fx.unknown();
        let v = fx.unknown();
        let (zero, one, two, three) = (fx.konst(0), fx.konst(1), fx.konst(2), fx.konst(3));
        let int = |v: u8, w: u8| Some(Folded::Value(ValueFold::Int(i128::from(v) - i128::from(w))));
        assert_eq!(fold1(&mut fx, LibFn::Strcmp, &[a, b]), int(b'l', b'p'));
        // A payload is one `char` per source byte: "\u{ff}" is the byte 0xff.
        assert_eq!(fold1(&mut fx, LibFn::Strcmp, &[c, a]), int(0xff, b'h'));
        assert_eq!(
            fold1(&mut fx, LibFn::Strncmp, &[a, b, three]),
            Some(Folded::Value(ValueFold::Int(0)))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Strcmp, &[u, u]),
            Some(Folded::Value(ValueFold::Int(0)))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Strcmp, &[u, e]),
            Some(Folded::Value(ValueFold::ByteDiff(
                Byte::At(u),
                Byte::Known(0)
            )))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Strncmp, &[e, u, two]),
            Some(Folded::Value(ValueFold::ByteDiff(
                Byte::Known(0),
                Byte::At(u)
            )))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Strncmp, &[u, v, zero]),
            Some(Folded::Value(ValueFold::Int(0)))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Strncmp, &[u, a, one]),
            Some(Folded::Value(ValueFold::ByteDiff(
                Byte::At(u),
                Byte::Known(b'h')
            )))
        );
        assert_eq!(fold1(&mut fx, LibFn::Strncmp, &[u, v, two]), None);
        assert_eq!(fold1(&mut fx, LibFn::Strcmp, &[u, a]), None);
        // memcmp does not stop at a terminator, so "" decides nothing.
        assert_eq!(fold1(&mut fx, LibFn::Memcmp, &[u, e, two]), None);
        assert_eq!(
            fold1(&mut fx, LibFn::Memcmp, &[u, v, one]),
            Some(Folded::Value(ValueFold::ByteDiff(Byte::At(u), Byte::At(v))))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Memcmp, &[a, b, three]),
            Some(Folded::Value(ValueFold::Int(0)))
        );
    }

    /// A comparison that would read past the end of a known object is left
    /// to run time, unless a difference decides it before it gets there.
    #[test]
    fn a_comparison_never_reads_past_a_known_object() {
        let mut fx = Fixture::new();
        let (ab, ac) = (fx.literal("ab"), fx.literal("ac"));
        let (a, b) = (fx.addr(&ab), fx.addr(&ac));
        let (four, one) = (fx.konst(4), fx.konst(1));
        let a1 = fx.op(Opcode::Add, a, one);
        let b1 = fx.op(Opcode::Add, b, one);
        // "ab\0" against "c\0": decided at the first byte.
        assert_eq!(
            fold1(&mut fx, LibFn::Memcmp, &[a, b1, four]),
            Some(Folded::Value(ValueFold::Int(
                i128::from(b'a') - i128::from(b'c')
            )))
        );
        // "b\0" against "c\0": decided at the first byte too.
        assert_eq!(
            fold1(&mut fx, LibFn::Memcmp, &[a1, b1, four]),
            Some(Folded::Value(ValueFold::Int(-1)))
        );
        // "b\0" against "b\0", four bytes: equal as far as both go, and
        // then past the end.
        assert_eq!(
            fold1(&mut fx, LibFn::Memcmp, &[a1, a1, four]),
            Some(Folded::Value(ValueFold::Int(0)))
        );
        let aa = fx.literal("xb");
        let x = fx.addr(&aa);
        let x1 = fx.op(Opcode::Add, x, one);
        assert_eq!(fold1(&mut fx, LibFn::Memcmp, &[a1, x1, four]), None);
    }

    #[test]
    fn character_searches_answer_offsets_into_the_argument() {
        let mut fx = Fixture::new();
        let lc = fx.literal("hello world");
        let p = fx.addr(&lc);
        let (o, x, nul) = (fx.konst(b'o'.into()), fx.konst(b'x'.into()), fx.konst(0));
        // The `int` is converted to `char`: 0x16f is 'o'.
        let wide_o = fx.konst(0x16f);
        let (five, eleven, twelve) = (fx.konst(5), fx.konst(11), fx.konst(12));
        let u = fx.unknown();
        let at = |k| Some(Folded::Value(ValueFold::Offset(p, k)));
        assert_eq!(fold1(&mut fx, LibFn::Strchr, &[p, o]), at(4));
        assert_eq!(fold1(&mut fx, LibFn::Strchr, &[p, wide_o]), at(4));
        assert_eq!(fold1(&mut fx, LibFn::Strrchr, &[p, o]), at(7));
        assert_eq!(
            fold1(&mut fx, LibFn::Strchr, &[p, x]),
            Some(Folded::Value(ValueFold::Null))
        );
        assert_eq!(fold1(&mut fx, LibFn::Strchr, &[p, nul]), at(11));
        assert_eq!(fold1(&mut fx, LibFn::Strchr, &[u, o]), None);
        let Some(Folded::Call(call)) = fold1(&mut fx, LibFn::Strrchr, &[u, nul]) else {
            panic!("strrchr(s, 0) should become strchr(s, 0)");
        };
        assert_eq!(call.func, LibFn::Strchr);
        assert_eq!(
            call.args,
            vec![
                Operand::Value(u, fx.types.const_char_ptr_id),
                Operand::Int(0, fx.types.int_id)
            ]
        );

        assert_eq!(fold1(&mut fx, LibFn::Memchr, &[p, o, five]), at(4));
        assert_eq!(
            fold1(&mut fx, LibFn::Memchr, &[p, x, eleven]),
            Some(Folded::Value(ValueFold::Null))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Memchr, &[p, nul, eleven]),
            Some(Folded::Value(ValueFold::Null))
        );
        assert_eq!(fold1(&mut fx, LibFn::Memchr, &[p, nul, twelve]), at(11));
        // Not found, and the search would run past the object.
        let far = fx.konst(13);
        assert_eq!(fold1(&mut fx, LibFn::Memchr, &[p, x, far]), None);
    }

    #[test]
    fn substring_and_span_searches() {
        let mut fx = Fixture::new();
        let hw = fx.literal("hello world");
        let empty = fx.literal("");
        let w = fx.literal("world");
        let o = fx.literal("o");
        let lo = fx.literal("lo");
        let (h, e, wp) = (fx.addr(&hw), fx.addr(&empty), fx.addr(&w));
        let (op, lp) = (fx.addr(&o), fx.addr(&lo));
        let u = fx.unknown();
        let becomes = |folded: Option<Folded>, f: LibFn| matches!(folded, Some(Folded::Call(NewCall { func, .. })) if func == f);
        assert_eq!(
            fold1(&mut fx, LibFn::Strstr, &[u, e]),
            Some(Folded::Value(ValueFold::Offset(u, 0)))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Strstr, &[h, wp]),
            Some(Folded::Value(ValueFold::Offset(h, 6)))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Strstr, &[wp, h]),
            Some(Folded::Value(ValueFold::Null))
        );
        assert!(becomes(
            fold1(&mut fx, LibFn::Strstr, &[u, op]),
            LibFn::Strchr
        ));
        assert_eq!(fold1(&mut fx, LibFn::Strstr, &[u, wp]), None);
        assert_eq!(
            fold1(&mut fx, LibFn::Strpbrk, &[u, e]),
            Some(Folded::Value(ValueFold::Null))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Strpbrk, &[h, wp]),
            Some(Folded::Value(ValueFold::Offset(h, 2)))
        );
        assert!(becomes(
            fold1(&mut fx, LibFn::Strpbrk, &[u, op]),
            LibFn::Strchr
        ));
        assert_eq!(
            fold1(&mut fx, LibFn::Strcspn, &[h, lp]),
            Some(Folded::Value(ValueFold::Int(2)))
        );
        assert_eq!(
            fold1(&mut fx, LibFn::Strcspn, &[e, u]),
            Some(Folded::Value(ValueFold::Int(0)))
        );
        assert!(becomes(
            fold1(&mut fx, LibFn::Strcspn, &[u, e]),
            LibFn::Strlen
        ));
        assert_eq!(fold1(&mut fx, LibFn::Strcspn, &[u, lp]), None);
    }
}
