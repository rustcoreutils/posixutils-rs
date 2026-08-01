//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Order-sensitive -D / -U command-line handling.
//
// `c17` specifies that "-D has lower precedence than -U. That is, if name is
// used in both a -U and a -D option, name shall be undefined regardless of the
// order of the options."
//
// `cflow` and `cxref` deliberately depart from that. Both say they conform to
// XBD 12.2 "except that the order of the -D, -I, and -U options ... is
// significant", so for those two utilities the *last* option mentioning a name
// wins:
//
//     -D FOO=1 -U FOO -D FOO=2      =>  FOO is 2
//     -D FOO=1 -U FOO               =>  FOO is undefined
//
// An argument parser that collects each flag into its own list (as clap does)
// loses the interleaving needed to tell those apart, so the raw argument vector
// is rescanned here.
//
// Only -D and -U need replaying: -I order matters solely among the -I options
// themselves, which any in-order collection already preserves, and interleaving
// -I with -D/-U carries no meaning.

/// A preprocessor-affecting option, in the order it appeared on the command line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PpArg {
    /// `-D name[=value]`
    Define(String),
    /// `-U name`
    Undef(String),
}

/// Extract `-D` / `-U` options from a raw argument vector, preserving order.
///
/// Accepts both the attached (`-DFOO=1`) and separated (`-D FOO=1`) spellings.
/// Scanning stops at `--`, after which everything is an operand.
pub fn scan<I>(argv: I) -> Vec<PpArg>
where
    I: IntoIterator<Item = String>,
{
    let mut out = Vec::new();
    let mut it = argv.into_iter();
    // Skip argv[0].
    let _ = it.next();

    while let Some(arg) = it.next() {
        if arg == "--" {
            break;
        }
        for (flag, ctor) in [
            ("-D", PpArg::Define as fn(String) -> PpArg),
            ("-U", PpArg::Undef as fn(String) -> PpArg),
        ] {
            if arg == flag {
                if let Some(value) = it.next() {
                    out.push(ctor(value));
                }
                break;
            } else if let Some(rest) = arg.strip_prefix(flag) {
                if !rest.is_empty() {
                    out.push(ctor(rest.to_string()));
                    break;
                }
            }
        }
    }
    out
}

/// Replay ordered `-D`/`-U` options into the final macro state.
///
/// Returns `(defines, undefines)` where the two lists are disjoint, so a
/// consumer that applies all defines and then all undefines (the usual shape)
/// still produces the order-sensitive result.
pub fn resolve(args: &[PpArg]) -> (Vec<String>, Vec<String>) {
    // Last option mentioning a name wins.
    let mut order: Vec<String> = Vec::new();
    let mut state: std::collections::BTreeMap<String, Option<String>> = Default::default();

    for arg in args {
        let (name, value) = match arg {
            PpArg::Define(spec) => {
                let name = spec.split('=').next().unwrap_or(spec).to_string();
                (name, Some(spec.clone()))
            }
            PpArg::Undef(name) => (name.clone(), None),
        };
        if !state.contains_key(&name) {
            order.push(name.clone());
        }
        state.insert(name, value);
    }

    let mut defines = Vec::new();
    let mut undefines = Vec::new();
    for name in order {
        match state.get(&name).and_then(|v| v.clone()) {
            Some(spec) => defines.push(spec),
            None => undefines.push(name),
        }
    }
    (defines, undefines)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn argv(items: &[&str]) -> Vec<String> {
        std::iter::once("prog")
            .chain(items.iter().copied())
            .map(String::from)
            .collect()
    }

    #[test]
    fn scans_attached_and_separated_forms() {
        assert_eq!(
            scan(argv(&["-DFOO=1", "-U", "BAR", "-D", "BAZ", "-UQUX"])),
            vec![
                PpArg::Define("FOO=1".into()),
                PpArg::Undef("BAR".into()),
                PpArg::Define("BAZ".into()),
                PpArg::Undef("QUX".into()),
            ]
        );
    }

    #[test]
    fn ignores_unrelated_options_and_operands() {
        assert_eq!(
            scan(argv(&["-r", "-I", "/inc", "file.c", "-DX"])),
            vec![PpArg::Define("X".into())]
        );
    }

    #[test]
    fn stops_at_double_dash() {
        assert_eq!(
            scan(argv(&["-DA", "--", "-DB"])),
            vec![PpArg::Define("A".into())]
        );
    }

    #[test]
    fn last_option_for_a_name_wins() {
        // -D then -U then -D: defined, with the final value.
        let (d, u) = resolve(&scan(argv(&["-DFOO=1", "-UFOO", "-DFOO=2"])));
        assert_eq!(d, vec!["FOO=2".to_string()]);
        assert!(u.is_empty());

        // -D then -U: undefined.
        let (d, u) = resolve(&scan(argv(&["-DFOO=1", "-UFOO"])));
        assert!(d.is_empty());
        assert_eq!(u, vec!["FOO".to_string()]);

        // -U then -D: defined.
        let (d, u) = resolve(&scan(argv(&["-UFOO", "-DFOO=1"])));
        assert_eq!(d, vec!["FOO=1".to_string()]);
        assert!(u.is_empty());
    }

    #[test]
    fn distinct_names_are_independent_and_ordered() {
        let (d, u) = resolve(&scan(argv(&["-DA=1", "-UB", "-DC=3"])));
        assert_eq!(d, vec!["A=1".to_string(), "C=3".to_string()]);
        assert_eq!(u, vec!["B".to_string()]);
    }

    #[test]
    fn define_without_value_is_preserved_verbatim() {
        // The preprocessor supplies the default value of 1.
        let (d, _) = resolve(&scan(argv(&["-DFOO"])));
        assert_eq!(d, vec!["FOO".to_string()]);
    }
}
