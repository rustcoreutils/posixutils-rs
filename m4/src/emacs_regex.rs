//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The regular expressions of `regexp` and `patsubst`.
//!
//! GNU m4 compiles them with GNU regex in its default syntax, which is GNU
//! Emacs's: `\(` `\)` group, `\|` alternates, `*` `+` `?` repeat, and the
//! plain `(` `)` `|` `{` `}` are literal.  `^` and `$` anchor only where they
//! can (a branch's start or end) and are literal elsewhere, as are `*` `+`
//! `?` with nothing to repeat.  Anchors also match at a <newline> inside the
//! text.  A bracket expression takes its contents literally, backslash
//! included, and has no character classes.  `\w` `\W` `\b` `\B` `\<` `\>`
//! `\`` `\'` are the word and buffer operators; any other escaped character
//! stands for itself.
//!
//! [`translate`] rewrites such a pattern into the syntax of the `regex`
//! crate, matching bytes.  One difference remains: GNU regex picks the
//! longest of the matches starting at the leftmost position, `regex` the
//! first alternative that matches, so `\(a\|ab\)` matches `a`, not `ab`, in
//! `ab`.  Back-references (`\1` in a pattern) are not supported; autoconf
//! uses neither.

use std::fmt::Write;

/// Translate the GNU Emacs-syntax `pattern` into a `regex` crate pattern
/// over bytes, or say why it cannot be.
pub fn translate(pattern: &[u8]) -> Result<String, String> {
    let mut out = String::from("(?m-u)");
    // Where the last repeatable atom begins in `out`, if the previous item
    // was one: a repetition operator applies to it, and is a literal
    // character when there is none.
    let mut atom_start: Option<usize> = None;
    // Whether the last item was a repetition operator. GNU regex lets one
    // repeat another (`a**`), which `regex` refuses unless grouped.
    let mut repeated = false;
    let mut branch_start = true;
    // Open `\(` groups, which GNU regex requires to balance.
    let mut depth = 0usize;
    let mut i = 0;
    while i < pattern.len() {
        let c = pattern[i];
        i += 1;
        let at_branch_start = std::mem::replace(&mut branch_start, false);
        match c {
            b'^' if at_branch_start => {
                out.push('^');
                atom_start = None;
                repeated = false;
                // `^*` repeats nothing: GNU makes that `*` literal.
                continue;
            }
            b'$' if at_branch_end(pattern, i) => {
                out.push('$');
                atom_start = None;
                repeated = false;
                continue;
            }
            b'*' | b'+' | b'?' if atom_start.is_some() => {
                let start = atom_start.expect("checked");
                if repeated {
                    out.insert_str(start, "(?:");
                    out.push(')');
                }
                out.push(c as char);
                repeated = true;
                continue;
            }
            b'[' => {
                let start = out.len();
                i = bracket(pattern, i, &mut out)?;
                atom_start = Some(start);
            }
            b'.' => {
                atom_start = Some(out.len());
                out.push('.');
            }
            b'\\' => {
                let Some(&e) = pattern.get(i) else {
                    return Err(String::from("Trailing backslash"));
                };
                i += 1;
                let start = out.len();
                match e {
                    b'(' => {
                        depth += 1;
                        out.push('(');
                        branch_start = true;
                        atom_start = None;
                        repeated = false;
                        continue;
                    }
                    b')' => {
                        if depth == 0 {
                            return Err(String::from("Unmatched ) or \\)"));
                        }
                        depth -= 1;
                        out.push(')');
                        // The group just closed is what a following `*`
                        // repeats.
                        atom_start = Some(group_start(&out));
                    }
                    b'|' => {
                        out.push('|');
                        branch_start = true;
                        atom_start = None;
                        repeated = false;
                        continue;
                    }
                    b'1'..=b'9' => {
                        return Err(String::from("back-references are not supported"));
                    }
                    b'w' => {
                        out.push_str("[0-9A-Za-z_]");
                        atom_start = Some(start);
                    }
                    b'W' => {
                        out.push_str("[^0-9A-Za-z_]");
                        atom_start = Some(start);
                    }
                    b'b' | b'B' | b'<' | b'>' | b'`' | b'\'' => {
                        out.push_str(match e {
                            b'b' => r"\b",
                            b'B' => r"\B",
                            b'<' => r"\b{start}",
                            b'>' => r"\b{end}",
                            b'`' => r"\A",
                            _ => r"\z",
                        });
                        atom_start = None;
                    }
                    other => {
                        literal(other, &mut out);
                        atom_start = Some(start);
                    }
                }
            }
            other => {
                atom_start = Some(out.len());
                literal(other, &mut out);
            }
        }
        repeated = false;
    }
    if depth > 0 {
        return Err(String::from("Unmatched ( or \\("));
    }
    Ok(out)
}

/// Whether a `$` just consumed, with `i` the index after it, ends a branch:
/// the end of the pattern, or just before `\)` or `\|`.
fn at_branch_end(pattern: &[u8], i: usize) -> bool {
    matches!(pattern.get(i..), Some([]) | Some([b'\\', b')' | b'|', ..]))
}

/// Where the group whose `)` ends `out` begins.
fn group_start(out: &str) -> usize {
    let bytes = out.as_bytes();
    let mut depth = 0usize;
    let mut j = bytes.len();
    while j > 0 {
        j -= 1;
        // Every literal parenthesis was written as `\x28` or `\x29`, and a
        // bracket's as a hex escape too, so a bare one is a group's.
        match bytes[j] {
            b')' => depth += 1,
            b'(' => {
                depth -= 1;
                if depth == 0 {
                    return j;
                }
            }
            _ => {}
        }
    }
    0
}

/// Write the byte `c` as a literal.
fn literal(c: u8, out: &mut String) {
    if c.is_ascii_alphanumeric() {
        out.push(c as char);
    } else {
        write!(out, r"\x{c:02X}").expect("writing to a String");
    }
}

/// Translate the bracket expression whose `[` is just before `pattern[i]`,
/// returning the index after its `]`.
fn bracket(pattern: &[u8], mut i: usize, out: &mut String) -> Result<usize, String> {
    let unmatched = || String::from("Unmatched [ or [^");
    let negated = pattern.get(i) == Some(&b'^');
    if negated {
        i += 1;
    }
    let mut ranges: Vec<(u8, u8)> = Vec::new();
    let mut first = true;
    loop {
        let &c = pattern.get(i).ok_or_else(unmatched)?;
        if c == b']' && !first {
            i += 1;
            break;
        }
        first = false;
        i += 1;
        // `a-z`, unless the `-` is the last thing before `]`.
        match (pattern.get(i), pattern.get(i + 1)) {
            (Some(b'-'), Some(&hi)) if hi != b']' => {
                if hi < c {
                    return Err(String::from("Invalid range end"));
                }
                ranges.push((c, hi));
                i += 2;
            }
            _ => ranges.push((c, c)),
        }
    }
    out.push('[');
    if negated {
        out.push('^');
    }
    for (lo, hi) in ranges {
        write!(out, r"\x{lo:02X}").expect("writing to a String");
        if hi != lo {
            write!(out, r"-\x{hi:02X}").expect("writing to a String");
        }
    }
    out.push(']');
    Ok(i)
}

#[cfg(test)]
mod tests {
    use super::translate;

    fn find(pattern: &str, text: &str) -> Option<(usize, usize)> {
        let re = regex::bytes::Regex::new(&translate(pattern.as_bytes()).unwrap()).unwrap();
        re.find(text.as_bytes()).map(|m| (m.start(), m.end()))
    }

    #[test]
    fn groups_and_alternation_are_backslashed() {
        assert_eq!(find(r"\(ab\)*c", "ababc"), Some((0, 5)));
        assert_eq!(find(r"x\|y", "ay"), Some((1, 2)));
        assert_eq!(find("(a|b)", "(a|b)"), Some((0, 5)));
        assert_eq!(find("a{2}", "a{2}"), Some((0, 4)));
    }

    #[test]
    fn anchors_are_literal_where_they_cannot_anchor() {
        assert_eq!(find("a^b", "a^b"), Some((0, 3)));
        assert_eq!(find("a$b", "a$b"), Some((0, 3)));
        assert_eq!(find(r"^a\|^b", "b"), Some((0, 1)));
        assert_eq!(find(r"\(a$\)", "ba"), Some((1, 2)));
        // Anchors match at an embedded <newline>.
        assert_eq!(find("^b", "a\nb"), Some((2, 3)));
    }

    #[test]
    fn repetition_with_nothing_to_repeat_is_literal() {
        assert_eq!(find("*a", "x*a"), Some((1, 3)));
        assert_eq!(find(r"\(*a\)", "*a"), Some((0, 2)));
        assert_eq!(find("^*", "*"), Some((0, 1)));
        assert_eq!(find("a**", "aaa"), Some((0, 3)));
    }

    #[test]
    fn brackets_take_backslash_literally() {
        assert_eq!(find(r"[\\]", "a\\b"), Some((1, 2)));
        assert_eq!(find(r"[^\\]", "\\b"), Some((1, 2)));
        assert_eq!(find("[]a]", "]"), Some((0, 1)));
        assert_eq!(find("[$-]", "-"), Some((0, 1)));
        assert_eq!(find("[a-c]+", "xabcd"), Some((1, 4)));
        // A negated list matches a <newline>, `.` does not.
        assert_eq!(find("[^a]", "\n"), Some((0, 1)));
        assert_eq!(find(".", "\n"), None);
    }

    #[test]
    fn word_operators() {
        assert_eq!(find(r"\bdn[l]\b", "x dnl y"), Some((2, 5)));
        assert_eq!(find(r"\bdn[l]\b", "xdnl"), None);
        assert_eq!(find(r"\<a", "ba a"), Some((3, 4)));
        assert_eq!(find(r"\w+", "  ab_9 "), Some((2, 6)));
    }

    #[test]
    fn errors() {
        assert!(translate(br"a\").is_err());
        assert!(translate(b"[a").is_err());
        assert!(translate(br"\(a\)\1").is_err());
    }
}
