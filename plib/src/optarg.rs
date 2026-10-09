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
//!
//! An attached option-argument that begins with '=' is another: clap reads
//! `-d=VALUE` as its own spelling of `-d VALUE` and drops the '=' that
//! `cut -d=` means (see [`keep_leading_equals`]).  Every utility parses its
//! command line through [`parse`], [`try_parse`], [`args_os`] or
//! [`keep_leading_equals`] to keep it.

use clap::{Command, CommandFactory, Parser};
use std::ffi::{OsStr, OsString};

/// The options of a command whose argument may be the next word.
pub struct OptionArguments {
    short: Vec<char>,
    long: Vec<String>,
    /// Short options whose argument is optional, and so only ever attached.
    short_optional: Vec<char>,
    /// Whether the first operand ends the options: every word after it
    /// belongs to a utility the command runs (`xargs`, `env`, `time`).
    operand_ends_options: bool,
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
        let mut short_optional = Vec::new();
        for arg in command.get_arguments() {
            let takes = arg.get_num_args().filter(|n| n.takes_values());
            let required_argument = takes.is_some_and(|n| n.min_values() > 0);
            if arg.is_positional() || arg.is_require_equals_set() {
                continue;
            }
            if !required_argument {
                if takes.is_some() {
                    short_optional.extend(arg.get_short());
                }
                continue;
            }
            short.extend(arg.get_short());
            short.extend(arg.get_all_short_aliases().unwrap_or_default());
            long.extend(arg.get_long().map(String::from));
            let aliases = arg.get_all_aliases().unwrap_or_default();
            long.extend(aliases.into_iter().map(String::from));
        }
        let operand_ends_options = command.is_allow_external_subcommands_set()
            || command.is_trailing_var_arg_set()
            || command
                .get_positionals()
                .any(|a| a.is_trailing_var_arg_set() || a.is_last_set());
        OptionArguments {
            short,
            long,
            short_optional,
            operand_ends_options,
        }
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
/// alone, whether attached or the next word, as is everything after `--`
/// and, for a command that runs a utility, everything after the first
/// operand; whether the next word is an option-argument is read from the
/// last word kept or put in the cluster's place.
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
        let operand = !text.starts_with('-') || text == "-";
        if text == "--" || (operand && options.operand_ends_options) {
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

/// Parse the command line as `T`, keeping an attached option-argument that
/// begins with '=' whole (see [`keep_leading_equals`]).
pub fn parse<T: Parser>() -> T {
    T::parse_from(args_os::<T>())
}

/// [`parse`], returning clap's error instead of exiting with it.
pub fn try_parse<T: Parser>() -> Result<T, clap::Error> {
    T::try_parse_from(args_os::<T>())
}

/// The command line, with [`keep_leading_equals`] applied for `T`.
pub fn args_os<T: CommandFactory>() -> Vec<OsString> {
    keep_leading_equals::<T>(std::env::args_os())
}

/// Keep an attached option-argument that begins with '=' whole.
///
/// An option-argument attached to its option letter is everything after the
/// letter (XBD 12.1), so `cut -d=` has the delimiter "=" and `paste -d=x` the
/// list "=x".  clap reads `-d=VALUE` as its own spelling of `-d VALUE` and
/// drops the '='.  Each such word of `argv` gets its '=' doubled, `-d==` and
/// `-d==x`, which clap reads back as the argument that was written.
///
/// Which letters take an argument comes from `T`'s clap definition.  It is
/// built only when some word could need the rewrite, so a command line
/// without one costs a single scan of its words.
pub fn keep_leading_equals<T: CommandFactory>(
    argv: impl IntoIterator<Item = impl Into<OsString>>,
) -> Vec<OsString> {
    keep_leading_equals_with(argv.into_iter().map(Into::into).collect(), T::command)
}

/// [`keep_leading_equals`] for a command built by `command`, for a utility
/// that assembles its clap definition by hand.
pub fn keep_leading_equals_with(
    argv: Vec<OsString>,
    command: impl FnOnce() -> Command,
) -> Vec<OsString> {
    if !argv.iter().skip(1).any(|w| may_attach_equals(w)) {
        return argv;
    }
    let options = OptionArguments::of(command());
    rewrite_short_clusters(argv, &options, |flags, rest| {
        // A letter whose argument is optional takes the rest of the word,
        // and splits nothing: find it among the flags.
        let (flags, rest) = match flags.find(|c| options.short_optional.contains(&c)) {
            Some(at) => flags.split_at(at),
            None => (flags, rest),
        };
        let mut chars = rest.chars();
        let letter = chars.next()?;
        let argument = chars.as_str();
        argument
            .starts_with('=')
            .then(|| vec![OsString::from(format!("-{flags}{letter}={argument}"))])
    })
}

/// Whether `word` is an option cluster with a '=' after its first letter.
fn may_attach_equals(word: &OsStr) -> bool {
    let bytes = word.as_encoded_bytes();
    bytes.len() > 2 && bytes[0] == b'-' && bytes[1] != b'-' && bytes[2..].contains(&b'=')
}

#[cfg(test)]
mod equals_tests {
    use super::*;
    use clap::{Arg, ArgAction};

    fn command() -> Command {
        Command::new("prog")
            .arg(Arg::new("d").short('d').long("delimiter"))
            .arg(Arg::new("n").short('n').action(ArgAction::SetTrue))
            .arg(Arg::new("s").short('s').num_args(0..=1))
            .arg(Arg::new("files").action(ArgAction::Append))
    }

    fn keep(cmd: Command, words: &[&str]) -> Vec<String> {
        let argv = std::iter::once("prog")
            .chain(words.iter().copied())
            .map(OsString::from)
            .collect();
        keep_leading_equals_with(argv, || cmd)
            .into_iter()
            .skip(1)
            .map(|w| w.into_string().unwrap())
            .collect()
    }

    #[test]
    fn attached_argument_keeps_its_equals() {
        assert_eq!(keep(command(), &["-d="]), ["-d=="]);
        assert_eq!(keep(command(), &["-d=x"]), ["-d==x"]);
        assert_eq!(keep(command(), &["-nd="]), ["-nd=="]);
        assert_eq!(keep(command(), &["f", "-d="]), ["f", "-d=="]);
        assert_eq!(keep(command(), &["-ns=x"]), ["-ns==x"]);
    }

    #[test]
    fn clap_reads_back_the_argument_written() {
        for (word, id, value) in [
            ("-d=", "d", "="),
            ("-d=x", "d", "=x"),
            ("-nd==", "d", "=="),
            ("-s=", "s", "="),
        ] {
            let argv = vec![OsString::from("prog"), OsString::from(word)];
            let m = command()
                .try_get_matches_from(keep_leading_equals_with(argv, command))
                .unwrap();
            assert_eq!(m.get_one::<String>(id).unwrap(), value, "{word}");
        }
    }

    #[test]
    fn other_words_are_left_alone() {
        for words in [
            &["-dx="][..],
            &["-d", "-d="],
            &["--delimiter", "-d="],
            &["--delimiter=-d="],
            &["--", "-d="],
            &["-sx="],
        ] {
            assert_eq!(keep(command(), words), words);
        }
        assert_eq!(
            keep(command(), &["--delimiter=x", "-d="]),
            ["--delimiter=x", "-d=="]
        );
    }

    #[test]
    fn trailing_utility_arguments_are_left_alone() {
        let cmd = Command::new("prog").arg(Arg::new("d").short('d')).arg(
            Arg::new("utility")
                .action(ArgAction::Append)
                .trailing_var_arg(true)
                .allow_hyphen_values(true),
        );
        assert_eq!(
            keep(cmd.clone(), &["-d=", "cut", "-d="]),
            ["-d==", "cut", "-d="]
        );
        assert_eq!(keep(cmd, &["-", "-d="]), ["-", "-d="]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::{Arg, ArgAction};

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
