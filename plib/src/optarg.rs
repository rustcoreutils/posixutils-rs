//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A short option whose argument is optional, which clap cannot parse.
//!
//! GNU getopt gives such an option (`sed -i[SUFFIX]`, `date -I[FMT]`) an
//! argument only when it is attached: `-i.bak` has one, `-i .bak` does not.
//! clap reads the rest of the word as more flags instead, so the option is
//! spelled in its long form, `--in-place[=SUFFIX]`, before clap sees it.

use std::ffi::OsString;

/// One option of the command whose argument the rewrite must step over.
pub enum TakesArgument<'a> {
    /// A short option letter: its argument is the rest of its word, or the
    /// next word when the letter ends the word.
    Short(char),
    /// A long option name, without the `--`: its argument is after `=`, or
    /// the next word.
    Long(&'a str),
}

/// Spell each `-<letter>[ARG]` in `argv` as `--<long>[=ARG]`.
///
/// The argument is the rest of the word, never the next word, and the letter
/// may end a cluster of flags: `-ni~` is `-n` and `--in-place=~`. The
/// argument of each option in `others`, attached or the next word, is left
/// alone, as is everything after `--`.
pub fn spell_optional_argument(
    argv: Vec<OsString>,
    letter: char,
    long: &str,
    others: &[TakesArgument],
) -> Vec<OsString> {
    let short_takes = |c: char| {
        others
            .iter()
            .any(|o| matches!(o, TakesArgument::Short(s) if *s == c))
    };
    let long_takes = |name: &str| {
        others
            .iter()
            .any(|o| matches!(o, TakesArgument::Long(l) if *l == name))
    };
    let mut out = Vec::with_capacity(argv.len());
    let mut words = argv.into_iter();
    out.extend(words.next());
    let mut option_argument_next = false;
    let mut operands_only = false;
    for word in words {
        if option_argument_next || operands_only {
            option_argument_next = false;
            out.push(word);
            continue;
        }
        let Some(text) = word.to_str() else {
            out.push(word);
            continue;
        };
        if text == "--" {
            operands_only = true;
            out.push(word);
            continue;
        }
        let Some(cluster) = text.strip_prefix('-').filter(|c| !c.is_empty()) else {
            out.push(word);
            continue;
        };
        if let Some(name) = cluster.strip_prefix('-') {
            option_argument_next = long_takes(name);
            out.push(word);
            continue;
        }
        let mut rewritten = false;
        for (pos, c) in cluster.char_indices() {
            if c == letter {
                let (flags, argument) = (&cluster[..pos], &cluster[pos + c.len_utf8()..]);
                if !flags.is_empty() {
                    out.push(OsString::from(format!("-{flags}")));
                }
                out.push(OsString::from(if argument.is_empty() {
                    format!("--{long}")
                } else {
                    format!("--{long}={argument}")
                }));
                rewritten = true;
                break;
            }
            if short_takes(c) {
                option_argument_next = pos + c.len_utf8() == cluster.len();
                break;
            }
            // A flag, or a letter clap will refuse.
        }
        if !rewritten {
            out.push(word);
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn spell(words: &[&str]) -> Vec<String> {
        let argv = std::iter::once("prog")
            .chain(words.iter().copied())
            .map(OsString::from)
            .collect();
        let others = [TakesArgument::Short('e'), TakesArgument::Long("expr")];
        spell_optional_argument(argv, 'i', "in-place", &others)
            .into_iter()
            .skip(1)
            .map(|w| w.into_string().unwrap())
            .collect()
    }

    #[test]
    fn attached_argument_only() {
        assert_eq!(spell(&["-i"]), ["--in-place"]);
        assert_eq!(spell(&["-i.bak"]), ["--in-place=.bak"]);
        assert_eq!(spell(&["-i", ".bak"]), ["--in-place", ".bak"]);
    }

    #[test]
    fn letter_ends_a_cluster_of_flags() {
        assert_eq!(spell(&["-ni~"]), ["-n", "--in-place=~"]);
        assert_eq!(spell(&["-nsi"]), ["-ns", "--in-place"]);
    }

    #[test]
    fn option_arguments_are_left_alone() {
        assert_eq!(spell(&["-ei"]), ["-ei"]);
        assert_eq!(spell(&["-e", "-i"]), ["-e", "-i"]);
        assert_eq!(spell(&["-ne", "-ix"]), ["-ne", "-ix"]);
        assert_eq!(spell(&["--expr", "-i"]), ["--expr", "-i"]);
        assert_eq!(spell(&["--expr=x", "-i"]), ["--expr=x", "--in-place"]);
        assert_eq!(spell(&["--", "-i"]), ["--", "-i"]);
        assert_eq!(spell(&["-", "-i"]), ["-", "--in-place"]);
    }
}
