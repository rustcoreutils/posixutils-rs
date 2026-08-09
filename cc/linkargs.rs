//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Order-sensitive link-line reconstruction.
//
// c17 departs from XBD 12.2 to say the order of -L, -l, -R and the pathname
// operands is significant (c17.md 87866-87867):
//
//     "A library shall be searched when its name is encountered"
//
// and EXAMPLE 3 relies on it:
//
//     c17 -L /a/b/c main.o a.c -l Q b.c -l p
//
// Here `-l Q` must be searched after main.o and a.o but before b.o, because a
// definition pulled from libQ can only satisfy references seen so far. An
// argument parser that collects each flag into its own list (as clap does)
// loses the interleaving entirely: it can only emit every object, then every
// -L, then every -l.
//
// So the argument vector is rescanned here to recover one ordered stream.
// Scanning happens *after* `preprocess_args()` has normalized the vector, so
// this only has to understand the separated spellings (`-L dir`, `-l lib`) —
// the attached forms (`-L.`, `-lz`) are split there, and that stays the single
// place that knows the raw GCC-compatible spellings.

/// One item on the link line, in the order it appeared.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LinkArg {
    /// `-L dir` — a library search path.
    LibPath(String),
    /// `-l library` — a library to search at this point.
    Library(String),
    /// `-R dir` — a runtime (dynamic linker) search path.
    RunPath(String),
    /// A pathname operand: a source, object, archive, or shared library.
    Operand(String),
}

/// Options that consume the argument that follows them, in the vector as
/// normalized by `preprocess_args()`.
///
/// This table must track the `Args` struct: an option added there that takes a
/// separate value and is missing here would have its value mistaken for a
/// pathname operand, putting a stray entry on the link line.
///
/// `--dump-ir` is deliberately absent. It is optional-valued, so its presence
/// is ambiguous here — and it suppresses the link phase anyway, so a
/// misclassification cannot reach a link line.
const VALUE_OPTIONS: &[&str] = &[
    "-D",
    "-U",
    "-I",
    "-o",
    "-W",
    "-B",
    "--target",
    "--rtlib",
    "--dump-ir-func",
    "--c17-fno-builtin-func",
    "--c17-linker-flag",
    "--c17-unsupported-mflag",
];

/// Recover the ordered link line from a normalized argument vector.
///
/// `argv` includes `argv[0]`, which is skipped. Everything after a `--`
/// terminator is an operand.
pub fn scan<I>(argv: I) -> Vec<LinkArg>
where
    I: IntoIterator<Item = String>,
{
    let mut out = Vec::new();
    let mut it = argv.into_iter();
    let _ = it.next(); // argv[0]
    let mut operands_only = false;

    while let Some(arg) = it.next() {
        if operands_only {
            out.push(LinkArg::Operand(arg));
            continue;
        }
        if arg == "--" {
            operands_only = true;
            continue;
        }

        match arg.as_str() {
            "-L" => {
                if let Some(v) = it.next() {
                    out.push(LinkArg::LibPath(v));
                }
            }
            "-l" => {
                if let Some(v) = it.next() {
                    out.push(LinkArg::Library(v));
                }
            }
            "-R" => {
                if let Some(v) = it.next() {
                    out.push(LinkArg::RunPath(v));
                }
            }
            _ if VALUE_OPTIONS.contains(&arg.as_str()) => {
                let _ = it.next();
            }
            // A bare `-` is the stdin operand, not an option.
            _ if arg.starts_with('-') && arg != "-" => {}
            _ => out.push(LinkArg::Operand(arg)),
        }
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn argv(items: &[&str]) -> Vec<String> {
        std::iter::once("c17")
            .chain(items.iter().copied())
            .map(String::from)
            .collect()
    }

    #[test]
    fn spec_example_3_interleaving_is_preserved() {
        // c17 -L /a/b/c main.o a.c -l Q b.c -l p
        // (as normalized by preprocess_args: -L and -l already separated)
        assert_eq!(
            scan(argv(&[
                "-L", "/a/b/c", "main.o", "a.c", "-l", "Q", "b.c", "-l", "p"
            ])),
            vec![
                LinkArg::LibPath("/a/b/c".into()),
                LinkArg::Operand("main.o".into()),
                LinkArg::Operand("a.c".into()),
                LinkArg::Library("Q".into()),
                LinkArg::Operand("b.c".into()),
                LinkArg::Library("p".into()),
            ]
        );
    }

    #[test]
    fn option_values_are_not_mistaken_for_operands() {
        assert_eq!(
            scan(argv(&[
                "-o", "out", "-D", "FOO=1", "-I", "/inc", "-U", "BAR", "-W", "all", "prog.c",
            ])),
            vec![LinkArg::Operand("prog.c".into())]
        );
    }

    #[test]
    fn valueless_options_are_skipped() {
        assert_eq!(
            scan(argv(&["-c", "-g", "-O2", "--shared", "x.c"])),
            vec![LinkArg::Operand("x.c".into())]
        );
    }

    #[test]
    fn runpath_is_recorded_in_place() {
        assert_eq!(
            scan(argv(&["a.o", "-R", "/opt/lib", "-l", "m"])),
            vec![
                LinkArg::Operand("a.o".into()),
                LinkArg::RunPath("/opt/lib".into()),
                LinkArg::Library("m".into()),
            ]
        );
    }

    #[test]
    fn double_dash_makes_everything_an_operand() {
        assert_eq!(
            scan(argv(&["-c", "--", "-weird.c", "b.c"])),
            vec![
                LinkArg::Operand("-weird.c".into()),
                LinkArg::Operand("b.c".into()),
            ]
        );
    }

    #[test]
    fn bare_dash_is_an_operand() {
        assert_eq!(
            scan(argv(&["-", "-l", "m"])),
            vec![LinkArg::Operand("-".into()), LinkArg::Library("m".into())]
        );
    }

    #[test]
    fn a_trailing_option_missing_its_value_is_not_a_panic() {
        assert_eq!(
            scan(argv(&["a.c", "-l"])),
            vec![LinkArg::Operand("a.c".into())]
        );
    }
}
