//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Options whose argument is optional and attached.
//!
//! `admin -i[name]`, `-t[name]`, `-y[comment]`, `delta -y[comment]`,
//! `-m[mrlist]` and `prs -r[SID]` take an option-argument that may be left
//! out and, when given, is part of the same word as the option letter:
//! `-ifile`, never `-i file`. The word after a bare `-i` is an operand.
//!
//! clap cannot say "an attached value only", so these options are taken out
//! of the command line here, and the rest goes to clap. Each utility had its
//! own way of doing it: `admin` a scan, `delta` an optional value that
//! swallowed the next word (`delta -y s.file` made a comment of the file and
//! had no operand left), `prs` a rewrite of the word `-r` alone.
//!
//! The words are bytes: a comment or a file name need not be UTF-8.

use std::ffi::{OsStr, OsString};
use std::os::unix::ffi::{OsStrExt, OsStringExt};

/// Which letters of a utility's options take what.
#[derive(Debug, Clone, Copy)]
pub struct Letters<'a> {
    /// Letters whose option-argument is optional and attached: taken out of
    /// the command line here.
    pub attached: &'a [u8],
    /// Letters whose option-argument is required, attached or the next
    /// word: left for clap, but the argument must be passed over unread.
    pub with_argument: &'a [u8],
}

impl Letters<'static> {
    /// The letters of the utility `utility`.
    ///
    /// One table for the utilities themselves and for the `sccs` front end,
    /// which must not read a command line differently from the utility it
    /// hands it to: taking the word after an option for its argument, or
    /// not, decides which words are files.
    pub fn of(utility: &str) -> Self {
        let (attached, with_argument): (&[u8], &[u8]) = match utility {
            "admin" => (b"ity", b"rmfdae"),
            "delta" => (b"ym", b"rg"),
            "get" => (b"", b"rcix"),
            "prs" => (b"r", b"cd"),
            "rmdel" | "unget" => (b"", b"r"),
            "val" => (b"", b"mry"),
            _ => (b"", b""),
        };
        Letters {
            attached,
            with_argument,
        }
    }

    /// Whether the option `letter`, the last of its word, has the next word
    /// for its argument.
    pub fn takes_next_word(&self, letter: u8) -> bool {
        self.with_argument.contains(&letter)
    }

    /// Whether the option `letter` ends the group of letters it is in: what
    /// follows it in the word, if anything, is its argument.
    pub fn ends_group(&self, letter: u8) -> bool {
        self.with_argument.contains(&letter) || self.attached.contains(&letter)
    }
}

/// The attached options found on a command line, in order.
#[derive(Debug, Default, PartialEq, Eq)]
pub struct Attached(Vec<(u8, Option<OsString>)>);

impl Attached {
    /// The option `letter`, as last given: `None` if it was not given,
    /// `Some(None)` given bare, `Some(Some(value))` with its attached value.
    pub fn get(&self, letter: u8) -> Option<Option<&OsStr>> {
        self.0
            .iter()
            .rev()
            .find(|(l, _)| *l == letter)
            .map(|(_, value)| value.as_deref())
    }

    /// Whether the option `letter` was given at all.
    pub fn has(&self, letter: u8) -> bool {
        self.get(letter).is_some()
    }
}

/// Split a command line into its attached options and the words left for
/// clap. `argv` begins with the utility's name.
///
/// The words are read as the Utility Syntax Guidelines read them (XBD 12.2),
/// so that only an option is taken for one:
/// - in a group of letters (`-niin.txt`), an attached letter takes the rest
///   of the word as its argument, and the flags before it stay for clap;
/// - a letter that takes a required argument ends its group, and the
///   argument -- the rest of the word, or the whole next word -- is passed
///   over unread, so `-m -yMR` is an MR and not a comment;
/// - after `--` every word is an operand.
pub fn scan<I>(argv: I, letters: Letters<'_>) -> (Attached, Vec<OsString>)
where
    I: IntoIterator<Item = OsString>,
{
    let mut found = Attached::default();
    let mut rest = Vec::new();
    let mut words = argv.into_iter();

    // argv[0] is the utility's name.
    rest.extend(words.next());

    while let Some(word) = words.next() {
        let bytes = word.as_bytes();
        if bytes == b"--" {
            rest.push(word);
            rest.extend(words);
            break;
        }
        // An operand, a lone "-", or a long option: not a group of letters.
        if !bytes.starts_with(b"-") || bytes == b"-" || bytes.starts_with(b"--") {
            rest.push(word);
            continue;
        }

        // What becomes of the word: it goes to clap whole, unless an
        // attached letter ends it, which leaves clap only the flags grouped
        // before it.
        let mut for_clap = Some(bytes);
        let mut argument_word = None;
        for (at, &letter) in bytes.iter().enumerate().skip(1) {
            if letters.with_argument.contains(&letter) {
                // Its argument is the rest of the word, or the next word,
                // which is passed over unread.
                if at + 1 == bytes.len() {
                    argument_word = words.next();
                }
                break;
            }
            if letters.attached.contains(&letter) {
                let value = &bytes[at + 1..];
                let value = (!value.is_empty()).then(|| OsString::from_vec(value.to_vec()));
                found.0.push((letter, value));
                for_clap = (at > 1).then(|| &bytes[..at]);
                break;
            }
            // A flag, or a letter for clap to refuse.
        }
        rest.extend(for_clap.map(|w| OsString::from_vec(w.to_vec())));
        rest.extend(argument_word);
    }

    (found, rest)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(words: &[&[u8]]) -> (Attached, Vec<Vec<u8>>) {
        let argv = std::iter::once(&b"admin"[..])
            .chain(words.iter().copied())
            .map(|w| OsString::from_vec(w.to_vec()));
        let (found, rest) = scan(argv, Letters::of("admin"));
        (
            found,
            rest.into_iter().skip(1).map(|w| w.into_vec()).collect(),
        )
    }

    fn value(found: &Attached, letter: u8) -> Option<Option<&[u8]>> {
        found.get(letter).map(|v| v.map(|v| v.as_bytes()))
    }

    #[test]
    fn an_attached_value_is_the_rest_of_its_word() {
        let (found, rest) = run(&[b"-ifile", b"s.f"]);
        assert_eq!(value(&found, b'i'), Some(Some(&b"file"[..])));
        assert_eq!(rest, [b"s.f"]);
    }

    #[test]
    fn a_bare_option_does_not_take_the_next_word() {
        let (found, rest) = run(&[b"-i", b"s.f"]);
        assert_eq!(value(&found, b'i'), Some(None));
        assert_eq!(rest, [b"s.f"]);
        assert!(found.has(b'i') && !found.has(b't'));
    }

    #[test]
    fn flags_grouped_before_it_stay_for_clap() {
        let (found, rest) = run(&[b"-nhiin.txt", b"-zy", b"s.f"]);
        assert_eq!(value(&found, b'i'), Some(Some(&b"in.txt"[..])));
        assert_eq!(value(&found, b'y'), Some(None));
        assert_eq!(rest, [&b"-nh"[..], b"-z", b"s.f"]);
    }

    #[test]
    fn a_required_argument_is_passed_over_unread() {
        // -m's argument is the next word, which looks like -y; -a's is the
        // rest of its own word, which holds a 't'.
        let (found, rest) = run(&[b"-m", b"-yMR", b"-atim", b"-nfi", b"s.f"]);
        assert_eq!(found, Attached::default());
        assert_eq!(rest, [&b"-m"[..], b"-yMR", b"-atim", b"-nfi", b"s.f"]);
    }

    #[test]
    fn operands_and_what_follows_a_double_dash_are_left_alone() {
        let (found, rest) = run(&[b"s.f", b"-", b"--long", b"-yone", b"--", b"-ytwo"]);
        assert_eq!(value(&found, b'y'), Some(Some(&b"one"[..])));
        assert_eq!(rest, [&b"s.f"[..], b"-", b"--long", b"--", b"-ytwo"]);
    }

    #[test]
    fn the_last_of_a_repeated_option_stands() {
        let (found, _) = run(&[b"-yone", b"-y", b"-ytwo"]);
        assert_eq!(value(&found, b'y'), Some(Some(&b"two"[..])));
        let (found, _) = run(&[b"-yone", b"-y"]);
        assert_eq!(value(&found, b'y'), Some(None));
    }

    #[test]
    fn words_are_bytes() {
        let (found, rest) = run(&[b"-ycaf\xe9", b"-i\xff\xfe", b"s.\xe9"]);
        assert_eq!(value(&found, b'y'), Some(Some(&b"caf\xe9"[..])));
        assert_eq!(value(&found, b'i'), Some(Some(&b"\xff\xfe"[..])));
        assert_eq!(rest, [&b"s.\xe9"[..]]);
    }
}
