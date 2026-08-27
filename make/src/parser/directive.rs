//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Makefile control directives: conditionals and multi-line macro definitions.
//!
//! Not POSIX -- the standard has no conditionals -- but they are the most
//! common construct in real makefiles by a wide margin, so a make that cannot
//! read them cannot read the field.
//!
//! Recognizing a directive is separated from acting on one: everything here is
//! pure, and the reader in `preprocessor` supplies the macro table.

/// A control directive, as written. Conditions are unexpanded at this point.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Directive {
    /// `ifeq`/`ifneq` with its raw argument text; `equal` distinguishes them.
    IfCompare {
        equal: bool,
        args: String,
    },
    /// `ifdef`/`ifndef` with the macro name; `defined` distinguishes them.
    IfDefined {
        defined: bool,
        name: String,
    },
    /// `else`, optionally carrying a further condition (`else ifeq (...)`).
    Else(Option<Box<Directive>>),
    EndIf,
    /// `define NAME` — begins a multi-line macro body.
    Define(String),
    /// `endef` — ends one.
    EndDef,
}

/// Split a directive line into its keyword and the rest.
fn split_keyword(line: &str) -> Option<(&str, &str)> {
    let trimmed = line.trim_start_matches([' ']);
    // A <tab>-indented line is a command line, never a directive.
    if line.starts_with('\t') {
        return None;
    }
    let end = trimmed.find([' ', '\t']).unwrap_or(trimmed.len());
    let (keyword, rest) = trimmed.split_at(end);
    Some((keyword, rest.trim()))
}

/// `define NAME`, `define NAME =`, `define NAME :=` — the trailing operator is
/// a GNU form; the name is what matters.
fn define_name(rest: &str) -> String {
    rest.split(['=', ':', '?', '+', '!'])
        .next()
        .unwrap_or(rest)
        .trim()
        .to_string()
}

/// Recognize a control directive, or `None` for an ordinary line.
pub(crate) fn parse_directive(line: &str) -> Option<Directive> {
    let (keyword, rest) = split_keyword(line)?;
    match keyword {
        "ifeq" => Some(Directive::IfCompare {
            equal: true,
            args: rest.to_string(),
        }),
        "ifneq" => Some(Directive::IfCompare {
            equal: false,
            args: rest.to_string(),
        }),
        "ifdef" => Some(Directive::IfDefined {
            defined: true,
            name: rest.to_string(),
        }),
        "ifndef" => Some(Directive::IfDefined {
            defined: false,
            name: rest.to_string(),
        }),
        "else" if rest.is_empty() => Some(Directive::Else(None)),
        // `else ifeq (...)` chains a further condition onto the else branch.
        "else" => Some(Directive::Else(parse_directive(rest).map(Box::new))),
        "endif" => Some(Directive::EndIf),
        "define" => Some(Directive::Define(define_name(rest))),
        "endef" => Some(Directive::EndDef),
        _ => None,
    }
}

/// Take a parenthesized or quoted argument starting at `s[0]`, returning the
/// contents and the rest of the input.
fn take_argument(s: &str) -> Option<(String, &str)> {
    let s = s.trim_start();
    let mut chars = s.char_indices();
    let (_, open) = chars.next()?;
    let close = match open {
        '"' => '"',
        '\'' => '\'',
        _ => return None,
    };
    let end = s[1..].find(close)? + 1;
    Some((s[1..end].to_string(), &s[end + 1..]))
}

/// Split the parenthesized form `(arg1,arg2)` on its top-level comma, so that a
/// comma inside a nested `$(...)` reference does not split the arguments.
fn split_parenthesized(s: &str) -> Option<(String, String)> {
    let inner = s.strip_prefix('(')?.strip_suffix(')')?;
    let mut depth = 0usize;
    for (i, c) in inner.char_indices() {
        match c {
            '(' | '{' => depth += 1,
            ')' | '}' => depth = depth.saturating_sub(1),
            ',' if depth == 0 => {
                return Some((
                    inner[..i].trim().to_string(),
                    inner[i + 1..].trim().to_string(),
                ))
            }
            _ => {}
        }
    }
    None
}

/// Split an `ifeq`/`ifneq` argument list. Both the `(a,b)` and the quoted
/// `"a" "b"` forms are accepted, as GNU make does.
pub(crate) fn split_condition_args(args: &str) -> Option<(String, String)> {
    let args = args.trim();
    if args.starts_with('(') {
        return split_parenthesized(args);
    }
    let (first, rest) = take_argument(args)?;
    let (second, _) = take_argument(rest)?;
    Some((first, second))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recognizes_conditionals() {
        assert_eq!(
            parse_directive("ifeq ($(A),b)"),
            Some(Directive::IfCompare {
                equal: true,
                args: "($(A),b)".to_string()
            })
        );
        assert_eq!(
            parse_directive("ifneq ($(A),b)"),
            Some(Directive::IfCompare {
                equal: false,
                args: "($(A),b)".to_string()
            })
        );
        assert_eq!(parse_directive("endif"), Some(Directive::EndIf));
        assert_eq!(parse_directive("else"), Some(Directive::Else(None)));
    }

    #[test]
    fn recognizes_ifdef_forms() {
        assert_eq!(
            parse_directive("ifdef FOO"),
            Some(Directive::IfDefined {
                defined: true,
                name: "FOO".to_string()
            })
        );
        assert_eq!(
            parse_directive("ifndef FOO"),
            Some(Directive::IfDefined {
                defined: false,
                name: "FOO".to_string()
            })
        );
    }

    #[test]
    fn recognizes_else_if() {
        let d = parse_directive("else ifeq ($(A),b)");
        assert!(matches!(d, Some(Directive::Else(Some(_)))));
    }

    #[test]
    fn leading_spaces_are_allowed_but_a_tab_means_recipe() {
        assert_eq!(parse_directive("   endif"), Some(Directive::EndIf));
        assert_eq!(parse_directive("\tendif"), None);
    }

    #[test]
    fn ordinary_lines_are_not_directives() {
        assert_eq!(parse_directive("all: dep"), None);
        assert_eq!(parse_directive("ifeqx foo"), None);
        assert_eq!(parse_directive(""), None);
    }

    #[test]
    fn recognizes_define_forms() {
        assert_eq!(
            parse_directive("define BODY"),
            Some(Directive::Define("BODY".to_string()))
        );
        assert_eq!(
            parse_directive("define BODY ="),
            Some(Directive::Define("BODY".to_string()))
        );
        assert_eq!(parse_directive("endef"), Some(Directive::EndDef));
    }

    #[test]
    fn splits_parenthesized_arguments() {
        assert_eq!(
            split_condition_args("(a,b)"),
            Some(("a".to_string(), "b".to_string()))
        );
        assert_eq!(
            split_condition_args("( a , b )"),
            Some(("a".to_string(), "b".to_string()))
        );
        assert_eq!(
            split_condition_args("(,)"),
            Some((String::new(), String::new()))
        );
    }

    // A comma inside a nested reference must not split the arguments.
    #[test]
    fn respects_nesting_when_splitting() {
        assert_eq!(
            split_condition_args("($(subst a,b,$(X)),y)"),
            Some(("$(subst a,b,$(X))".to_string(), "y".to_string()))
        );
    }

    #[test]
    fn splits_quoted_arguments() {
        assert_eq!(
            split_condition_args("\"a\" \"b\""),
            Some(("a".to_string(), "b".to_string()))
        );
        assert_eq!(
            split_condition_args("'a' 'b'"),
            Some(("a".to_string(), "b".to_string()))
        );
    }
}
