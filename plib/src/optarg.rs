//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Short-option spellings clap cannot parse, rewritten before clap sees them.
//!
//! GNU getopt gives an option whose argument is optional (`sed -i[SUFFIX]`,
//! `date -I[FMT]`) an argument only when it is attached: `-i.bak` has one,
//! `-i .bak` does not.  clap reads the rest of the word as more flags
//! instead, so the option is spelled in its long form,
//! `--in-place[=SUFFIX]` (see [`spell_optional_argument`]).  GNU grep's
//! `-NUM` is another such spelling.
//!
//! Every rewrite walks the command line the same way, in
//! [`rewrite_short_clusters`]: an option-argument is not an option, and
//! nothing after `--` is.  Which options take an argument is read from the
//! command's clap definition ([`OptionArguments::of`]).

use std::ffi::OsString;

/// The options of a command whose argument may be the next word.
pub struct OptionArguments {
    short: Vec<char>,
    long: Vec<String>,
}

impl OptionArguments {
    /// The options of `command` that clap gives a required argument, which is
    /// the next word when it is not attached: every short and long name, and
    /// alias, of each.  An option whose argument is optional or must follow
    /// `=` never takes the next word, and is left out.
    pub fn of(mut command: clap::Command) -> OptionArguments {
        command.build();
        let mut short = Vec::new();
        let mut long = Vec::new();
        for arg in command.get_arguments() {
            let required_argument = arg
                .get_num_args()
                .is_some_and(|n| n.takes_values() && n.min_values() > 0);
            if arg.is_positional() || !required_argument || arg.is_require_equals_set() {
                continue;
            }
            short.extend(arg.get_short());
            short.extend(arg.get_all_short_aliases().unwrap_or_default());
            long.extend(arg.get_long().map(String::from));
            let aliases = arg.get_all_aliases().unwrap_or_default();
            long.extend(aliases.into_iter().map(String::from));
        }
        OptionArguments { short, long }
    }

    fn short_takes(&self, letter: char) -> bool {
        self.short.contains(&letter)
    }

    fn long_takes(&self, name: &str) -> bool {
        self.long.iter().any(|l| l == name)
    }

    /// Where `cluster`, a word without its leading `-`, splits: at its first
    /// letter that takes an argument, or at its end.
    fn split_cluster<'c>(&self, cluster: &'c str) -> (&'c str, &'c str) {
        let at = cluster
            .char_indices()
            .find(|&(_, c)| self.short_takes(c))
            .map_or(cluster.len(), |(i, _)| i);
        cluster.split_at(at)
    }

    /// Whether the word after `word`, an option word, is its argument: a long
    /// option that takes one without `=`, or a cluster that ends with a
    /// letter that takes one.
    fn next_is_argument(&self, word: &str) -> bool {
        if let Some(name) = word.strip_prefix("--") {
            return self.long_takes(name);
        }
        match word.strip_prefix('-') {
            Some(cluster) => {
                let (_, rest) = self.split_cluster(cluster);
                rest.chars().count() == 1
            }
            None => false,
        }
    }
}

/// Rewrite the short-option words of `argv`, as `rewrite` says.
///
/// Each word that is a cluster of short options, `-XYZ`, is split at its
/// first letter that takes an argument (see [`OptionArguments`]): `flags`
/// is the letters before it, `rest` is that letter and what follows it,
/// which is its argument.  `rewrite(flags, rest)` gives the words to put in
/// the cluster's place, or `None` to keep it.  An option-argument is left
/// alone, whether attached or the next word, as is everything after `--`;
/// whether the next word is an option-argument is read from the last word
/// kept or put in the cluster's place.
pub fn rewrite_short_clusters(
    argv: Vec<OsString>,
    options: &OptionArguments,
    mut rewrite: impl FnMut(&str, &str) -> Option<Vec<OsString>>,
) -> Vec<OsString> {
    let mut out = Vec::with_capacity(argv.len());
    let mut words = argv.into_iter();
    out.extend(words.next());
    let mut option_argument_next = false;
    while let Some(word) = words.next() {
        let Some(text) = word.to_str().filter(|_| !option_argument_next) else {
            option_argument_next = false;
            out.push(word);
            continue;
        };
        if text == "--" {
            out.push(word);
            out.extend(words);
            break;
        }
        let replacement = text
            .strip_prefix('-')
            .filter(|cluster| !cluster.is_empty() && !cluster.starts_with('-'))
            .and_then(|cluster| {
                let (flags, rest) = options.split_cluster(cluster);
                rewrite(flags, rest)
            });
        let words_out = replacement.unwrap_or_else(|| vec![word]);
        option_argument_next = words_out
            .last()
            .and_then(|w| w.to_str())
            .is_some_and(|w| options.next_is_argument(w));
        out.extend(words_out);
    }
    out
}

/// Spell each `-<letter>[ARG]` in `argv` as `--<long>[=ARG]`.
///
/// The argument is the rest of the word, never the next word, and the letter
/// may end a cluster of flags: `-ni~` is `-n` and `--in-place=~`.  The
/// argument of each option in `options`, attached or the next word, is left
/// alone, as is everything after `--`.
pub fn spell_optional_argument(
    argv: Vec<OsString>,
    letter: char,
    long: &str,
    options: &OptionArguments,
) -> Vec<OsString> {
    rewrite_short_clusters(argv, options, |flags, rest| {
        let pos = flags.find(letter)?;
        let before = &flags[..pos];
        let argument = format!("{}{rest}", &flags[pos + letter.len_utf8()..]);
        let mut words = Vec::with_capacity(2);
        if !before.is_empty() {
            words.push(OsString::from(format!("-{before}")));
        }
        words.push(OsString::from(if argument.is_empty() {
            format!("--{long}")
        } else {
            format!("--{long}={argument}")
        }));
        Some(words)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::{Arg, ArgAction, Command};

    /// A command like sed's: `-e`/`--expr` take an argument, `-n` and `-s`
    /// are flags, and `-i`/`--in-place` takes one only after `=`.
    fn options() -> OptionArguments {
        OptionArguments::of(
            Command::new("prog")
                .arg(Arg::new("expr").short('e').long("expr"))
                .arg(Arg::new("n").short('n').action(ArgAction::SetTrue))
                .arg(Arg::new("s").short('s').action(ArgAction::SetTrue))
                .arg(
                    Arg::new("in-place")
                        .short('i')
                        .long("in-place")
                        .num_args(0..=1)
                        .require_equals(true),
                )
                .arg(Arg::new("file").num_args(0..)),
        )
    }

    fn spell(words: &[&str]) -> Vec<String> {
        let argv = std::iter::once("prog")
            .chain(words.iter().copied())
            .map(OsString::from)
            .collect();
        spell_optional_argument(argv, 'i', "in-place", &options())
            .into_iter()
            .skip(1)
            .map(|w| w.into_string().unwrap())
            .collect()
    }

    #[test]
    fn options_read_from_clap() {
        let options = options();
        assert_eq!(options.short, ['e']);
        assert_eq!(options.long, ["expr"]);
    }

    #[test]
    fn attached_argument_only() {
        assert_eq!(spell(&["-i"]), ["--in-place"]);
        assert_eq!(spell(&["-i.bak"]), ["--in-place=.bak"]);
        assert_eq!(spell(&["-i", ".bak"]), ["--in-place", ".bak"]);
        assert_eq!(spell(&["-ie", "-i"]), ["--in-place=e", "--in-place"]);
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
