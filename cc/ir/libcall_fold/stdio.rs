//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The <stdio.h> output functions, where what they print is known
//
// `printf("hello\n")` writes the bytes `puts("hello")` writes, and C17
// 7.1.4p1 lets the one be the other. Only a call whose result is unused is
// rewritten: the count `printf` answers is not the one `puts` answers. A
// call that prints nothing is deleted, as gcc deletes it, although C17
// 7.21.2p4 has even an empty write orient the stream; every other becomes a
// call that prints the same bytes to the same stream -- never nothing at
// all, since writing can fail. These are gcc's rewrites, and gcc.c-torture
// pins which are made and which are not:
// - `printf` of a format without `%`: "" is nothing, one character is
//   `putchar`, and text ending in a newline is `puts` of the rest. Other
//   text stays a `printf`: only `fputs` could print it, and there is no
//   `stdout` to hand it. `printf("%s", s)` of a known `s` is the same;
//   `printf("%s\n", s)` is `puts(s)` and `printf("%c", c)` is `putchar(c)`.
// - `fprintf(fp, ...)` of a format without `%` is `fputs(fmt, fp)`, of
//   `"%s"` is `fputs`, and of `"%c"` is `fputc`.
// - `fputs(s, fp)` of a known length: 0 is nothing, one known character is
//   `fputc`, and any other is `fwrite(s, 1, len, fp)`.
// A format without `%` must come with no argument, and one with a
// directive with exactly one. The `v` forms fold only a format without `%`,
// and the `__*_chk` forms follow their plain ones. The `_unlocked` forms are
// only ever dropped: no C library has `puts_unlocked`, and macOS has no
// `fwrite_unlocked`, so there is nothing to call instead.
//

use super::{is_pointer, CallSite, Folded, NewCall, Operand};
use crate::ir::strdata::Len;
use crate::ir::{Instruction, PseudoId};
use crate::parse::ast::LibFn;
use crate::types::{TypeId, TypeKind, TypeTable};

/// An argument of the call, and the type it was passed as.
type Arg = (PseudoId, TypeId);

/// What the call `insn` to `f`, one of the output functions, folds to.
pub(super) fn fold(f: LibFn, insn: &Instruction, facts: &CallSite) -> Option<Folded> {
    if !facts.result_unused(insn) || insn.src.len() != insn.extra().arg_types.len() {
        return None;
    }
    let args: Vec<Arg> = insn
        .src
        .iter()
        .copied()
        .zip(insn.extra().arg_types.iter().copied())
        .collect();
    match (f, args.as_slice()) {
        (LibFn::Fputs, &[(s, _), fp]) => fputs(facts, s, fp, Locking::Locked),
        (LibFn::FputsUnlocked, &[(s, _), fp]) => fputs(facts, s, fp, Locking::Unlocked),
        _ => Print::of(f, &args)?.fold(facts),
    }
}

/// Where a call prints.
#[derive(Clone, Copy)]
enum Stream {
    Stdout,
    File(Arg),
}

/// What follows the format.
#[derive(Clone, Copy)]
enum Rest<'a> {
    /// The arguments, of `printf`.
    Args(&'a [Arg]),
    /// A `va_list`, of `vprintf`.
    VaList,
}

/// Whether a call is to an `_unlocked` form, which is only ever dropped.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Locking {
    Locked,
    Unlocked,
}

impl Locking {
    /// `new` in place of the call, where this form may call another.
    fn call(self, new: NewCall) -> Option<Folded> {
        (self == Locking::Locked).then_some(Folded::Discard(Some(new)))
    }
}

/// A call to one of the `printf` family, by what it prints and where.
struct Print<'a> {
    stream: Stream,
    fmt: PseudoId,
    rest: Rest<'a>,
    locking: Locking,
}

impl<'a> Print<'a> {
    /// The call to `f` of `args`, taken apart.
    fn of(f: LibFn, args: &'a [Arg]) -> Option<Self> {
        use LibFn as L;
        use Locking::{Locked, Unlocked};
        // Whether it takes a stream, where its format is, whether a
        // `va_list` follows it, and its locking.
        let (on_file, fmt_at, va, locking) = match f {
            L::Printf => (false, 0, false, Locked),
            L::PrintfUnlocked => (false, 0, false, Unlocked),
            L::Vprintf => (false, 0, true, Locked),
            L::PrintfChk => (false, 1, false, Locked),
            L::VprintfChk => (false, 1, true, Locked),
            L::Fprintf => (true, 1, false, Locked),
            L::FprintfUnlocked => (true, 1, false, Unlocked),
            L::Vfprintf => (true, 1, true, Locked),
            L::FprintfChk => (true, 2, false, Locked),
            L::VfprintfChk => (true, 2, true, Locked),
            _ => return None,
        };
        let stream = if on_file {
            Stream::File(*args.first()?)
        } else {
            Stream::Stdout
        };
        let rest = args.get(fmt_at + 1..)?;
        Some(Print {
            stream,
            fmt: args.get(fmt_at)?.0,
            rest: if va { Rest::VaList } else { Rest::Args(rest) },
            locking,
        })
    }

    fn fold(&self, facts: &CallSite) -> Option<Folded> {
        let types = facts.types;
        if writes_nothing(facts, self.fmt) && !matches!(self.rest, Rest::Args([_, ..])) {
            return Some(NOTHING);
        }
        let fmt = facts.c_str(self.fmt)?;
        if !fmt.contains(&b'%') {
            return match self.rest {
                Rest::Args([_, ..]) => None,
                _ => self.text(types, fmt, self.fmt),
            };
        }
        let Rest::Args(&[arg]) = self.rest else {
            return None;
        };
        let (p, t) = arg;
        match (fmt, self.stream) {
            (b"%s", _) if is_pointer(types, t) && writes_nothing(facts, p) => Some(NOTHING),
            (b"%s", Stream::Stdout) if is_pointer(types, t) => self.text(types, facts.c_str(p)?, p),
            (b"%s", Stream::File(fp)) if is_pointer(types, t) => {
                self.locking.call(fputs_call(Operand::Value(p, t), fp))
            }
            (b"%s\n", Stream::Stdout) if is_pointer(types, t) => {
                self.locking.call(puts_call(Operand::Value(p, t)))
            }
            (b"%c", stream) if is_int(types, t) => {
                self.locking.call(put_char(Operand::Value(p, t), stream))
            }
            _ => None,
        }
    }

    /// Printing the known `text`, which `p` points at, and which is not
    /// empty: that is `NOTHING`, and `fold` answers it first.
    fn text(&self, types: &TypeTable, text: &[u8], p: PseudoId) -> Option<Folded> {
        let new = match (self.stream, text) {
            (Stream::File(fp), _) => fputs_call(Operand::Value(p, types.const_char_ptr_id), fp),
            (Stream::Stdout, &[c]) => put_char(char_operand(types, c), Stream::Stdout),
            (Stream::Stdout, [line @ .., b'\n']) => puts_call(Operand::Literal(line.to_vec())),
            (Stream::Stdout, _) => return None,
        };
        self.locking.call(new)
    }
}

/// `fputs(s, fp)`, for an `s` whose length is known.
fn fputs(facts: &CallSite, s: PseudoId, fp: Arg, locking: Locking) -> Option<Folded> {
    let Len::Const(len) = facts.strings.string_len(s)? else {
        return None;
    };
    if len == 0 {
        return Some(NOTHING);
    }
    let types = facts.types;
    if let Some(&[c]) = facts.c_str(s) {
        return locking.call(put_char(char_operand(types, c), Stream::File(fp)));
    }
    let size_t = LibFn::Fwrite.return_type(types);
    locking.call(NewCall {
        func: LibFn::Fwrite,
        args: vec![
            Operand::Value(s, types.const_void_ptr_id),
            Operand::Int(1, size_t),
            Operand::Int(i128::from(len), size_t),
            Operand::Value(fp.0, fp.1),
        ],
    })
}

/// What a write of nothing folds to: nothing. gcc deletes the call, of every
/// form here, `_unlocked` and `__*_chk` alike, and so does c17, although
/// C17 7.21.2p4 has the first input or output function applied to a stream
/// orient it whether or not it transfers a byte: `fwide(fp, 0)` after a
/// deleted `fputs("", fp)` answers 0 where -O0 answers a byte orientation.
/// Its arguments were evaluated before the call, and stay.
const NOTHING: Folded = Folded::Discard(None);

/// Whether `p` is known to point at the empty string, whichever of the
/// strings it may be.
fn writes_nothing(facts: &CallSite, p: PseudoId) -> bool {
    facts.strings.string_len(p) == Some(Len::Const(0))
}

/// The character `c`, as the `int` `putchar` takes.
fn char_operand(types: &TypeTable, c: u8) -> Operand {
    Operand::Int(i128::from(c), types.int_id)
}

/// `putchar(c)`, or `fputc(c, fp)`.
fn put_char(c: Operand, stream: Stream) -> NewCall {
    match stream {
        Stream::Stdout => NewCall {
            func: LibFn::Putchar,
            args: vec![c],
        },
        Stream::File((fp, t)) => NewCall {
            func: LibFn::Fputc,
            args: vec![c, Operand::Value(fp, t)],
        },
    }
}

fn puts_call(s: Operand) -> NewCall {
    NewCall {
        func: LibFn::Puts,
        args: vec![s],
    }
}

fn fputs_call(s: Operand, (fp, t): Arg) -> NewCall {
    NewCall {
        func: LibFn::Fputs,
        args: vec![s, Operand::Value(fp, t)],
    }
}

/// Whether an argument of type `t` is what `%c` takes: an `int`, which a
/// narrower one has been promoted to.
fn is_int(types: &TypeTable, t: TypeId) -> bool {
    types.kind(t) == TypeKind::Int && !types.is_unsigned(t)
}

#[cfg(test)]
mod tests {
    use super::super::tests::{def, folds, run_on};
    use super::*;
    use crate::ir::strdata::fixture::Fixture;
    use crate::ir::{Opcode, PseudoKind};

    /// A call to `f` of `args`, each passed as its type, as the linearizer
    /// makes one.
    fn call(fx: &mut Fixture, f: LibFn, args: &[Arg]) -> PseudoId {
        let int = fx.types.int_id;
        fx.call_typed(f, "callee", args, int)
    }

    /// What a call to `f` of `args`, its result unused, folds to.
    fn fold1(fx: &mut Fixture, f: LibFn, args: &[Arg]) -> Option<Folded> {
        call(fx, f, args);
        let at = fx.func().blocks[0].insns.len() - 1;
        folds(fx)
            .into_iter()
            .find_map(|(i, folded)| (i == at).then_some(folded))
    }

    /// A pointer to a literal of `text`, as a `const char *` argument.
    fn lit(fx: &mut Fixture, text: &str) -> Arg {
        let lc = fx.literal(text);
        (fx.addr(&lc), fx.types.const_char_ptr_id)
    }

    /// An argument nothing here knows, of the type `typ` picks.
    fn unknown(fx: &mut Fixture, typ: impl Fn(&TypeTable) -> TypeId) -> Arg {
        (fx.unknown(), typ(&fx.types))
    }

    /// A `FILE *` nothing here knows.
    fn stream(fx: &mut Fixture) -> Arg {
        unknown(fx, |t| t.void_ptr_id)
    }

    fn int(fx: &Fixture, v: u8) -> Operand {
        Operand::Int(i128::from(v), fx.types.int_id)
    }

    fn val((p, t): Arg) -> Operand {
        Operand::Value(p, t)
    }

    fn made(func: LibFn, args: Vec<Operand>) -> Option<Folded> {
        Some(Folded::Discard(Some(NewCall { func, args })))
    }

    fn puts_literal(text: &[u8]) -> Option<Folded> {
        made(LibFn::Puts, vec![Operand::Literal(text.to_vec())])
    }

    #[test]
    fn printf_of_text_is_nothing_putchar_or_puts() {
        let mut fx = Fixture::new();
        // An empty write is deleted, as gcc deletes it; see `NOTHING`.
        let empty = lit(&mut fx, "");
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[empty]), Some(NOTHING));
        let a = lit(&mut fx, "a");
        let want = made(LibFn::Putchar, vec![int(&fx, b'a')]);
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[a]), want);
        let line = lit(&mut fx, "hi\n");
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[line]), puts_literal(b"hi"));
        // A lone newline is one character.
        let nl = lit(&mut fx, "\n");
        let want = made(LibFn::Putchar, vec![int(&fx, b'\n')]);
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[nl]), want);
    }

    /// Text that does not end in a newline could only be `fputs` to
    /// `stdout`, which there is no way to name.
    #[test]
    fn printf_of_other_text_stays() {
        let mut fx = Fixture::new();
        let hello = lit(&mut fx, "hello");
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[hello]), None);
        let inner = lit(&mut fx, "a\nb");
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[inner]), None);
    }

    #[test]
    fn printf_of_a_directive_and_its_one_argument() {
        let mut fx = Fixture::new();
        let s_nl = lit(&mut fx, "%s\n");
        let s = unknown(&mut fx, |t| t.char_ptr_id);
        let want = made(LibFn::Puts, vec![val(s)]);
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[s_nl, s]), want);
        let pc = lit(&mut fx, "%c");
        let c = unknown(&mut fx, |t| t.int_id);
        let want = made(LibFn::Putchar, vec![val(c)]);
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[pc, c]), want);
        // `%s` of a known string prints it as text, `%` and all.
        let ps = lit(&mut fx, "%s");
        let text = lit(&mut fx, "hello\n");
        assert_eq!(
            fold1(&mut fx, LibFn::Printf, &[ps, text]),
            puts_literal(b"hello")
        );
        let empty = lit(&mut fx, "");
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[ps, empty]), Some(NOTHING));
        let pct_nl = lit(&mut fx, "%d\n");
        assert_eq!(
            fold1(&mut fx, LibFn::Printf, &[ps, pct_nl]),
            puts_literal(b"%d")
        );
    }

    #[test]
    fn printf_refuses_what_does_not_match_exactly() {
        let mut fx = Fixture::new();
        let (ps, pc, pd) = (lit(&mut fx, "%s"), lit(&mut fx, "%c"), lit(&mut fx, "%d\n"));
        let s = unknown(&mut fx, |t| t.char_ptr_id);
        let x = unknown(&mut fx, |t| t.int_id);
        let l = unknown(&mut fx, |t| t.long_id);
        let u = unknown(&mut fx, |t| t.uint_id);
        // `%s` of an unknown string could only be `fputs` to `stdout`.
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[ps, s]), None);
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[pd, x]), None);
        // `%c` takes an `int`.
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[pc, l]), None);
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[pc, u]), None);
        // `%s` takes a pointer.
        let s_nl = lit(&mut fx, "%s\n");
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[s_nl, x]), None);
        // A directive with no argument, or with two.
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[s_nl]), None);
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[s_nl, s, s]), None);
        // Text with an argument it does not print.
        let text = lit(&mut fx, "a");
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[text, x]), None);
        // `%%` is a directive too.
        let pct = lit(&mut fx, "%%\n");
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[pct]), None);
        // An unknown format.
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[s]), None);
    }

    #[test]
    fn a_used_result_is_never_folded() {
        let mut fx = Fixture::new();
        let empty = lit(&mut fx, "");
        let r = call(&mut fx, LibFn::Printf, &[empty]);
        fx.push(Instruction::ret(Some(r)));
        assert!(folds(&mut fx).is_empty());
        let fp = stream(&mut fx);
        let r = call(&mut fx, LibFn::Fputs, &[empty, fp]);
        fx.push(Instruction::ret(Some(r)));
        assert!(folds(&mut fx).is_empty());
    }

    #[test]
    fn a_call_with_no_result_is_unused() {
        let mut fx = Fixture::new();
        let a = lit(&mut fx, "a");
        call(&mut fx, LibFn::Printf, &[a]);
        fx.func().blocks[0].insns.last_mut().unwrap().target = None;
        assert_eq!(folds(&mut fx).len(), 1);
    }

    #[test]
    fn the_unlocked_forms_only_drop() {
        let mut fx = Fixture::new();
        let (empty, a, nl, ps) = (
            lit(&mut fx, ""),
            lit(&mut fx, "a"),
            lit(&mut fx, "hi\n"),
            lit(&mut fx, "%s"),
        );
        let fp = stream(&mut fx);
        // An empty write is deleted, as gcc deletes it; see `NOTHING`.
        let none = Some(NOTHING);
        assert_eq!(fold1(&mut fx, LibFn::PrintfUnlocked, &[empty]), none);
        assert_eq!(fold1(&mut fx, LibFn::PrintfUnlocked, &[ps, empty]), none);
        assert_eq!(fold1(&mut fx, LibFn::PrintfUnlocked, &[a]), None);
        assert_eq!(fold1(&mut fx, LibFn::PrintfUnlocked, &[nl]), None);
        assert_eq!(fold1(&mut fx, LibFn::FprintfUnlocked, &[fp, empty]), none);
        assert_eq!(
            fold1(&mut fx, LibFn::FprintfUnlocked, &[fp, ps, empty]),
            none
        );
        assert_eq!(fold1(&mut fx, LibFn::FprintfUnlocked, &[fp, a]), None);
        assert_eq!(fold1(&mut fx, LibFn::FprintfUnlocked, &[fp, ps, a]), None);
        assert_eq!(fold1(&mut fx, LibFn::FputsUnlocked, &[empty, fp]), none);
        assert_eq!(fold1(&mut fx, LibFn::FputsUnlocked, &[a, fp]), None);
        assert_eq!(fold1(&mut fx, LibFn::FputsUnlocked, &[nl, fp]), None);
    }

    /// The `va_list` forms fold a format without `%` only; the `_chk`
    /// forms read the format after their flag.
    #[test]
    fn the_va_list_and_chk_forms() {
        let mut fx = Fixture::new();
        let (nl, ps, pd) = (
            lit(&mut fx, "hi\n"),
            lit(&mut fx, "%s"),
            lit(&mut fx, "%d\n"),
        );
        let ap = unknown(&mut fx, |t| t.va_list_id);
        let flag = unknown(&mut fx, |t| t.int_id);
        let fp = stream(&mut fx);
        assert_eq!(
            fold1(&mut fx, LibFn::Vprintf, &[nl, ap]),
            puts_literal(b"hi")
        );
        assert_eq!(fold1(&mut fx, LibFn::Vprintf, &[ps, ap]), None);
        let want = puts_literal(b"hi");
        assert_eq!(fold1(&mut fx, LibFn::VprintfChk, &[flag, nl, ap]), want);
        assert_eq!(fold1(&mut fx, LibFn::VprintfChk, &[flag, ps, ap]), None);
        assert_eq!(fold1(&mut fx, LibFn::PrintfChk, &[flag, nl]), want);
        let s = unknown(&mut fx, |t| t.char_ptr_id);
        assert_eq!(fold1(&mut fx, LibFn::PrintfChk, &[flag, pd, s]), None);
        let want = made(LibFn::Fputs, vec![val(nl), val(fp)]);
        assert_eq!(fold1(&mut fx, LibFn::Vfprintf, &[fp, nl, ap]), want);
        assert_eq!(
            fold1(&mut fx, LibFn::VfprintfChk, &[fp, flag, nl, ap]),
            want
        );
        assert_eq!(fold1(&mut fx, LibFn::FprintfChk, &[fp, flag, nl]), want);
        assert_eq!(fold1(&mut fx, LibFn::Vfprintf, &[fp, ps, ap]), None);
        // An empty format writes nothing, in every one of them.
        let empty = lit(&mut fx, "");
        let none = Some(NOTHING);
        assert_eq!(fold1(&mut fx, LibFn::Vprintf, &[empty, ap]), none);
        assert_eq!(fold1(&mut fx, LibFn::VprintfChk, &[flag, empty, ap]), none);
        assert_eq!(fold1(&mut fx, LibFn::PrintfChk, &[flag, empty]), none);
        assert_eq!(fold1(&mut fx, LibFn::PrintfChk, &[flag, ps, empty]), none);
        assert_eq!(fold1(&mut fx, LibFn::Vfprintf, &[fp, empty, ap]), none);
        let args = [fp, flag, empty, ap];
        assert_eq!(fold1(&mut fx, LibFn::VfprintfChk, &args), none);
        assert_eq!(fold1(&mut fx, LibFn::FprintfChk, &[fp, flag, empty]), none);
        let args = [fp, flag, ps, empty];
        assert_eq!(fold1(&mut fx, LibFn::FprintfChk, &args), none);
    }

    #[test]
    fn fprintf_is_fputs_or_fputc_to_its_own_stream() {
        let mut fx = Fixture::new();
        let (empty, hello, ps, pc) = (
            lit(&mut fx, ""),
            lit(&mut fx, "hello"),
            lit(&mut fx, "%s"),
            lit(&mut fx, "%c"),
        );
        let fp = stream(&mut fx);
        let s = unknown(&mut fx, |t| t.char_ptr_id);
        let c = unknown(&mut fx, |t| t.int_id);
        // Writing nothing is nothing, as in gcc.
        assert_eq!(fold1(&mut fx, LibFn::Fprintf, &[fp, empty]), Some(NOTHING));
        let none = Some(NOTHING);
        assert_eq!(fold1(&mut fx, LibFn::Fprintf, &[fp, ps, empty]), none);
        let want = made(LibFn::Fputs, vec![val(hello), val(fp)]);
        assert_eq!(fold1(&mut fx, LibFn::Fprintf, &[fp, hello]), want);
        let want = made(LibFn::Fputs, vec![val(s), val(fp)]);
        assert_eq!(fold1(&mut fx, LibFn::Fprintf, &[fp, ps, s]), want);
        let want = made(LibFn::Fputc, vec![val(c), val(fp)]);
        assert_eq!(fold1(&mut fx, LibFn::Fprintf, &[fp, pc, c]), want);
        let pd = lit(&mut fx, "%d");
        assert_eq!(fold1(&mut fx, LibFn::Fprintf, &[fp, pd, c]), None);
        assert_eq!(fold1(&mut fx, LibFn::Fprintf, &[fp, hello, c]), None);
    }

    #[test]
    fn fputs_of_a_known_length() {
        let mut fx = Fixture::new();
        let (empty, nl, hello) = (lit(&mut fx, ""), lit(&mut fx, "\n"), lit(&mut fx, "hello"));
        let fp = stream(&mut fx);
        // Writes nothing, and is deleted, as gcc deletes it.
        assert_eq!(fold1(&mut fx, LibFn::Fputs, &[empty, fp]), Some(NOTHING));
        let want = made(LibFn::Fputc, vec![int(&fx, b'\n'), val(fp)]);
        assert_eq!(fold1(&mut fx, LibFn::Fputs, &[nl, fp]), want);
        let want = fwrite(&fx, hello.0, 5, fp);
        assert_eq!(fold1(&mut fx, LibFn::Fputs, &[hello, fp]), want);
        let s = unknown(&mut fx, |t| t.char_ptr_id);
        assert_eq!(fold1(&mut fx, LibFn::Fputs, &[s, fp]), None);
    }

    fn fwrite(fx: &Fixture, s: PseudoId, len: i128, fp: Arg) -> Option<Folded> {
        let ulong = fx.types.ulong_id;
        let args = vec![
            Operand::Value(s, fx.types.const_void_ptr_id),
            Operand::Int(1, ulong),
            Operand::Int(len, ulong),
            val(fp),
        ];
        made(LibFn::Fwrite, args)
    }

    /// `fputs(i ? "f" : "x", fp)`: one length but no one character, so
    /// `fwrite` of the selected pointer; arms of two lengths stay `fputs`.
    #[test]
    fn fputs_of_a_select_of_one_length() {
        let mut fx = Fixture::new();
        let (f, x) = (lit(&mut fx, "f"), lit(&mut fx, "x"));
        let ptr = fx.types.const_char_ptr_id;
        let fp = stream(&mut fx);
        let sel = fx.select(f.0, x.0);
        let want = fwrite(&fx, sel, 1, fp);
        assert_eq!(fold1(&mut fx, LibFn::Fputs, &[(sel, ptr), fp]), want);
        let (ab, cde) = (lit(&mut fx, "ab"), lit(&mut fx, "cde"));
        let differ = fx.select(ab.0, cde.0);
        assert_eq!(fold1(&mut fx, LibFn::Fputs, &[(differ, ptr), fp]), None);
    }

    /// `printf(i ? "" : "")` and the like write nothing whichever string
    /// they print, and go, as in gcc; a format with an argument it does not
    /// print stays, as in gcc.
    #[test]
    fn a_choice_of_empty_strings_writes_nothing() {
        let mut fx = Fixture::new();
        let (e1, e2, ps) = (lit(&mut fx, ""), lit(&mut fx, ""), lit(&mut fx, "%s"));
        let ptr = fx.types.const_char_ptr_id;
        let fp = stream(&mut fx);
        let none = Some(NOTHING);
        let sel = (fx.select(e1.0, e2.0), ptr);
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[sel]), none);
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[ps, sel]), none);
        assert_eq!(fold1(&mut fx, LibFn::Fprintf, &[fp, ps, sel]), none);
        assert_eq!(fold1(&mut fx, LibFn::Fputs, &[sel, fp]), none);
        let x = unknown(&mut fx, |t| t.int_id);
        assert_eq!(fold1(&mut fx, LibFn::Printf, &[e1, x]), None);
        assert_eq!(fold1(&mut fx, LibFn::Fprintf, &[fp, e1, x]), None);
    }

    fn calls(insns: &[Instruction]) -> Vec<&Instruction> {
        insns.iter().filter(|i| i.op == Opcode::Call).collect()
    }

    /// The symbol whose address `call` passes first.
    fn first_arg_symbol(fx: &Fixture, insns: &[Instruction], call: &Instruction) -> String {
        let addr = def(insns, call.src[0]);
        assert_eq!(addr.op, Opcode::SymAddr);
        match &fx.module.functions[0].get_pseudo(addr.src[0]).unwrap().kind {
            PseudoKind::Sym(name) => name.clone(),
            other => panic!("not a symbol: {other:?}"),
        }
    }

    /// `printf("hi\n")` is `puts` of a new literal "hi", defining no result,
    /// and a rewritten call beside it leaves its own result behind.
    #[test]
    fn a_made_puts_passes_a_new_literal() {
        let mut fx = Fixture::new();
        // `printf("a")` becomes `putchar`, so its result pseudo goes too.
        let (a, nl) = (lit(&mut fx, "a"), lit(&mut fx, "hi\n"));
        let dropped = call(&mut fx, LibFn::Printf, &[a]);
        let r = call(&mut fx, LibFn::Printf, &[nl]);
        let base = fx.module.strings.len();
        let insns = run_on(&mut fx, &[]);
        let label = format!(".LC{base}");
        assert_eq!(
            fx.module.strings[base..],
            [(label.clone(), "hi".to_string())]
        );
        let calls = calls(&insns);
        assert_eq!(calls.len(), 2, "putchar and puts");
        let puts = calls
            .iter()
            .copied()
            .find(|c| c.extra().func_name.as_deref() == Some("puts"))
            .expect("the newline form becomes puts");
        assert_eq!(puts.extra().func_name.as_deref(), Some("puts"));
        assert_eq!(puts.extra().known, Some(LibFn::Puts));
        assert!(puts.extra().abi_info.is_some());
        assert_eq!(puts.target, None);
        assert!(!insns
            .iter()
            .any(|i| [Some(r), Some(dropped)].contains(&i.target)));
        assert_eq!(first_arg_symbol(&fx, &insns, puts), label);
    }

    /// A literal the module already has, or one a fold already added, is
    /// reused.
    #[test]
    fn a_made_literal_is_shared() {
        let mut fx = Fixture::new();
        let hi = fx.literal("hi");
        let (a, b, c) = (
            lit(&mut fx, "hi\n"),
            lit(&mut fx, "yo\n"),
            lit(&mut fx, "%s"),
        );
        call(&mut fx, LibFn::Printf, &[a]);
        call(&mut fx, LibFn::Printf, &[b]);
        call(&mut fx, LibFn::Printf, &[c, b]);
        let base = fx.module.strings.len();
        let insns = run_on(&mut fx, &[]);
        let added = &fx.module.strings[base..];
        assert_eq!(added.len(), 1, "only \"yo\" is new: {added:?}");
        let yo = added[0].0.clone();
        let calls = calls(&insns);
        assert_eq!(first_arg_symbol(&fx, &insns, calls[0]), hi);
        assert_eq!(first_arg_symbol(&fx, &insns, calls[1]), yo);
        assert_eq!(first_arg_symbol(&fx, &insns, calls[2]), yo);
    }

    /// `fwrite` returns `size_t`, whatever `fputs` returned.
    #[test]
    fn a_made_fwrite_returns_size_t() {
        let mut fx = Fixture::new();
        let hello = lit(&mut fx, "hello");
        let fp = stream(&mut fx);
        call(&mut fx, LibFn::Fputs, &[hello, fp]);
        let insns = run_on(&mut fx, &[]);
        let calls = calls(&insns);
        assert_eq!(calls.len(), 1);
        assert_eq!(calls[0].extra().func_name.as_deref(), Some("fwrite"));
        assert_eq!((calls[0].typ, calls[0].size), (Some(fx.types.ulong_id), 64));
        assert_eq!(calls[0].src[3], fp.0);
    }
}
