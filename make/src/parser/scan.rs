//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Pure lexical helpers for the makefile scanner.
//!
//! Nothing here holds state or touches the filesystem, so each piece is
//! testable on its own.

/// The three parts of a rule line: `target...: [prerequisite...][; command]`.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct RuleLine {
    pub targets: Vec<String>,
    pub prerequisites: Vec<String>,
    /// A command given inline after a `<semicolon>`, if any.
    pub inline: Option<String>,
}

/// Drop a trailing comment. POSIX 105551: a `#` and everything after it, up to
/// the newline, is a comment.
///
/// Must not be applied to command lines — POSIX 105629 hands those to the shell
/// verbatim, where `#` is meaningful (`@echo '#!/bin/sh'`).
pub(crate) fn strip_comment(line: &str) -> &str {
    match line.find('#') {
        Some(i) => &line[..i],
        None => line,
    }
}

/// Split on runs of `<blank>`. Deliberately not `split_whitespace`, which also
/// splits on vertical tab, form feed and carriage return; POSIX says `<blank>`,
/// which is `<space>` and `<tab>`.
pub(crate) fn split_words(s: &str) -> Vec<String> {
    s.split([' ', '\t'])
        .filter(|w| !w.is_empty())
        .map(String::from)
        .collect()
}

/// Split the prerequisite side on a `<semicolon>`. POSIX 105644: text after the
/// semicolon is a command line.
fn split_inline_command(rhs: &str) -> (&str, Option<String>) {
    match rhs.find(';') {
        Some(i) => (&rhs[..i], Some(rhs[i + 1..].trim_start().to_string())),
        None => (rhs, None),
    }
}

/// Parse `target [target...]: [prerequisite...][; command]` (POSIX 105639).
///
/// Every character that is not a `<blank>`, `:`, `;` or `#` is an ordinary name
/// character — including `/` (paths), `(` and `)` (archive members such as
/// `lib.a(member.o)`), `~`, `%`, `"` and `=`.
pub(crate) fn split_rule_line(line: &str) -> Result<RuleLine, String> {
    let colon = line.find(':').ok_or("expected ':'")?;
    let (lhs, rhs) = (&line[..colon], &line[colon + 1..]);

    // `target:: prerequisites` is the GNU double-colon rule: several rules for
    // one target, each with its own commands, all of which run. POSIX has no
    // such construct and we do not implement it. Say so: taking the first `:`
    // made the second one a prerequisite literally named `:`, and the makefile
    // failed with `no target ':'` (audit #86).
    if rhs.starts_with(':') {
        return Err("double-colon rules are not supported".to_string());
    }

    // A semicolon left of the colon would otherwise yield a target literally
    // named `a;b`, silently.
    if lhs.contains(';') {
        return Err("';' before ':'".to_string());
    }

    let targets = split_words(lhs);
    if targets.is_empty() {
        return Err("missing target before ':'".to_string());
    }

    let (prereq_text, inline) = split_inline_command(rhs);
    Ok(RuleLine {
        targets,
        prerequisites: split_words(prereq_text),
        inline,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rule(line: &str) -> RuleLine {
        split_rule_line(line).expect("should parse")
    }

    #[test]
    fn strips_a_trailing_comment() {
        assert_eq!(strip_comment("comment: # this is a comment"), "comment: ");
        assert_eq!(strip_comment("no comment here"), "no comment here");
        assert_eq!(strip_comment("# whole line"), "");
    }

    #[test]
    fn splits_on_blanks_only() {
        assert_eq!(split_words("a  b\tc"), vec!["a", "b", "c"]);
        assert_eq!(split_words("   "), Vec::<String>::new());
        // <carriage-return> is not a <blank> and stays part of the word.
        assert_eq!(split_words("a\rb"), vec!["a\rb"]);
    }

    // Audit #27: a rule may name more than one target.
    #[test]
    fn accepts_multiple_targets() {
        assert_eq!(rule("a b: c").targets, vec!["a", "b"]);
    }

    // Audit #86: GNU's double-colon rule. Splitting on the first `:` made the
    // second one a prerequisite literally named `:`, which failed later with
    // `no target ':'` -- a diagnostic about the wrong thing entirely.
    #[test]
    fn rejects_a_double_colon_rule() {
        let err = split_rule_line("all:: a").expect_err("must be rejected");
        assert!(err.contains("double-colon"), "err: {err}");
    }

    // A `:` on the prerequisite side is not the double-colon form.
    #[test]
    fn accepts_a_colon_in_a_prerequisite() {
        assert_eq!(rule("all: a:b").prerequisites, vec!["a:b"]);
    }

    // Audit #26: `/` is an ordinary name character, in both positions.
    #[test]
    fn accepts_slashes_in_names() {
        let r = rule("sub/dir/out: sub/dir/in.c");
        assert_eq!(r.targets, vec!["sub/dir/out"]);
        assert_eq!(r.prerequisites, vec!["sub/dir/in.c"]);
    }

    // POSIX 105946: a name with parentheses is an archive member and must
    // survive intact for the archive-member mtime lookup.
    #[test]
    fn keeps_archive_member_targets_intact() {
        assert_eq!(
            rule("lib.a(member.o): member.c").targets,
            vec!["lib.a(member.o)"]
        );
    }

    #[test]
    fn takes_an_inline_command() {
        let r = rule("rule: dep ; echo hi");
        assert_eq!(r.prerequisites, vec!["dep"]);
        assert_eq!(r.inline.as_deref(), Some("echo hi"));
    }

    // POSIX 105911-105915: `target: ;` is the empty rule -- it has a command,
    // and that command is empty.
    #[test]
    fn empty_rule_has_one_empty_command() {
        let r = rule("rule: ;");
        assert!(r.prerequisites.is_empty());
        assert_eq!(r.inline.as_deref(), Some(""));
    }

    #[test]
    fn inference_targets_parse() {
        assert_eq!(rule(".c.o:").targets, vec![".c.o"]);
        assert!(rule(".txt.out:  ").prerequisites.is_empty());
    }

    #[test]
    fn rejects_a_semicolon_before_the_colon() {
        assert!(split_rule_line("a;b: c").is_err());
    }

    #[test]
    fn rejects_a_line_without_a_colon() {
        assert!(split_rule_line("nocolon").is_err());
    }

    #[test]
    fn rejects_an_empty_target_list() {
        assert!(split_rule_line(": c").is_err());
    }
}
