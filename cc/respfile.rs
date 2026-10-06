//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `@file` response files, as gcc reads them.
//
// CMake, Ninja and libtool write a long command line to a file and pass
// `@file` instead, so the argument vector every other part of the driver sees
// is the one after expansion. The rules are libiberty's `expandargv` and
// `buildargv`, which gcc uses:
//
// - arguments are separated by whitespace;
// - single and double quotes group, and a backslash takes the next character
//   literally, inside quotes as well as outside them;
// - an `@file` inside a response file is expanded in turn, by a path relative
//   to the working directory;
// - an `@file` that cannot be read stays as written, an ordinary argument.
//
// gcc stops a runaway after 2000 expansions; this refuses the cycle itself,
// which is the only way a finite set of files can expand forever.

use std::path::PathBuf;

/// Expand every `@file` in `argv`, leaving `argv[0]` alone.
///
/// The error names a response file that includes itself.
pub fn expand(argv: Vec<String>) -> Result<Vec<String>, String> {
    let mut it = argv.into_iter();
    let mut out: Vec<String> = it.next().into_iter().collect();
    expand_into(it, &mut Vec::new(), &mut out)?;
    Ok(out)
}

/// Append `args` to `out`, expanding each readable `@file`. `open` holds the
/// response files being expanded around this point, outermost first.
fn expand_into(
    args: impl IntoIterator<Item = String>,
    open: &mut Vec<PathBuf>,
    out: &mut Vec<String>,
) -> Result<(), String> {
    for arg in args {
        let Some(name) = arg.strip_prefix('@') else {
            out.push(arg);
            continue;
        };
        let Ok(bytes) = std::fs::read(name) else {
            out.push(arg);
            continue;
        };
        let key = std::fs::canonicalize(name).unwrap_or_else(|_| PathBuf::from(name));
        if open.contains(&key) {
            return Err(format!("response file includes itself: {name}"));
        }
        open.push(key);
        expand_into(split(&String::from_utf8_lossy(&bytes)), open, out)?;
        open.pop();
    }
    Ok(())
}

/// Split the text of a response file into arguments.
pub fn split(text: &str) -> Vec<String> {
    let mut args = Vec::new();
    let mut chars = text.chars().peekable();
    loop {
        while chars.next_if(|c| is_space(*c)).is_some() {}
        if chars.peek().is_none() {
            return args;
        }
        let mut arg = String::new();
        let (mut squote, mut dquote) = (false, false);
        while let Some(c) = chars.next() {
            match c {
                '\\' => arg.extend(chars.next()),
                '\'' if !dquote => squote = !squote,
                '"' if !squote => dquote = !dquote,
                c if is_space(c) && !squote && !dquote => break,
                c => arg.push(c),
            }
        }
        args.push(arg);
    }
}

/// libiberty's `ISSPACE`: the C locale's white-space characters.
fn is_space(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\r' | '\x0b' | '\x0c')
}

#[cfg(test)]
mod tests {
    use super::*;

    fn words(text: &str) -> Vec<String> {
        split(text)
    }

    #[test]
    fn split_on_whitespace() {
        assert_eq!(
            words(" -c\tfoo.c\n\r\x0b\x0c-o  foo.o \n"),
            ["-c", "foo.c", "-o", "foo.o"]
        );
        assert!(words("").is_empty());
        assert!(words(" \n\t ").is_empty());
    }

    #[test]
    fn split_quotes_group_and_vanish() {
        assert_eq!(words("'-DA=two words' x"), ["-DA=two words", "x"]);
        assert_eq!(words("\"-DB=a b\""), ["-DB=a b"]);
        assert_eq!(words("pre'mid dle'post"), ["premid dlepost"]);
        // Each kind of quote is literal inside the other.
        assert_eq!(words("\"it's\" 'say \"hi\"'"), ["it's", "say \"hi\""]);
        // An empty pair is an empty argument.
        assert_eq!(words("a '' b"), ["a", "", "b"]);
        // An unterminated quote runs to the end of the file.
        assert_eq!(words("'a b"), ["a b"]);
    }

    #[test]
    fn split_backslash_escapes_everywhere() {
        assert_eq!(words("a\\ b c"), ["a b", "c"]);
        assert_eq!(words("\"a\\\"b\""), ["a\"b"]);
        // libiberty escapes inside single quotes as well.
        assert_eq!(words("'a\\'b'"), ["a'b"]);
        assert_eq!(words("C:\\\\dir"), ["C:\\dir"]);
        // A trailing backslash escapes nothing and is dropped.
        assert_eq!(words("x\\"), ["x"]);
    }

    fn scratch() -> plib::tmp::TempDir {
        plib::tmp::Builder::new()
            .prefix("c17_respfile_")
            .tempdir()
            .expect("tempdir")
    }

    fn argv(items: &[&str]) -> Vec<String> {
        std::iter::once("c17")
            .chain(items.iter().copied())
            .map(String::from)
            .collect()
    }

    #[test]
    fn expand_in_place_and_nested() {
        let dir = scratch();
        let inner = dir.path().join("inner");
        let outer = dir.path().join("outer");
        std::fs::write(&inner, "-DC=3").unwrap();
        std::fs::write(&outer, format!("-DA=1 @{} -DB=2", inner.display())).unwrap();
        let got = expand(argv(&["-c", &format!("@{}", outer.display()), "x.c"])).unwrap();
        assert_eq!(got, argv(&["-c", "-DA=1", "-DC=3", "-DB=2", "x.c"]));

        // The same file twice, side by side, is not a cycle.
        let twice = format!("@{}", inner.display());
        let got = expand(argv(&[&twice, &twice])).unwrap();
        assert_eq!(got, argv(&["-DC=3", "-DC=3"]));

        // An empty response file contributes nothing.
        let empty = dir.path().join("empty");
        std::fs::write(&empty, "\n").unwrap();
        let got = expand(argv(&[&format!("@{}", empty.display()), "x.c"])).unwrap();
        assert_eq!(got, argv(&["x.c"]));
    }

    #[test]
    fn expand_leaves_unreadable_files_literal() {
        let dir = scratch();
        let missing = format!("@{}", dir.path().join("missing").display());
        let directory = format!("@{}", dir.path().display());
        let args = argv(&[&missing, &directory, "@", "a@b"]);
        assert_eq!(expand(args.clone()).unwrap(), args);
        // argv[0] is never a response file.
        let f = dir.path().join("f");
        std::fs::write(&f, "-c").unwrap();
        let zero = vec![format!("@{}", f.display())];
        assert_eq!(expand(zero.clone()).unwrap(), zero);
    }

    #[test]
    fn expand_refuses_a_cycle() {
        let dir = scratch();
        let a = dir.path().join("a");
        let b = dir.path().join("b");
        std::fs::write(&a, format!("-DA @{}", b.display())).unwrap();
        std::fs::write(&b, format!("-DB @{}", a.display())).unwrap();
        let err = expand(argv(&[&format!("@{}", a.display())])).unwrap_err();
        assert!(err.contains("response file includes itself"), "{err}");
    }
}
