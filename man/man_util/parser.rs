//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::mdoc_macro::*;
use super::parse::mdoc::NestingTooDeep;

static BLOCK_PARTIAL_IMPLICIT: &[&str] = &[
    "Aq", "Bq", "Brq", "D1", "Dl", "Dq", "En", "Op", "Pq", "Ql", "Qq", "Sq", "Vt",
];

#[allow(unreachable_patterns)]
fn does_start_with_macro(word: &str) -> bool {
    matches!(
        word,
        "Bd" | "Bf"
            | "Bk"
            | "Bl"
            | "Ed"
            | "Ef"
            | "Ek"
            | "El"
            | "It"
            | "Nd"
            | "Nm"
            | "Sh"
            | "Ss"
            | "Ac"
            | "Ao"
            | "Bc"
            | "Bo"
            | "Brc"
            | "Bro"
            | "Dc"
            | "Do"
            | "Ec"
            | "Eo"
            | "Fc"
            | "Fo"
            | "Oc"
            | "Oo"
            | "Pc"
            | "Po"
            | "Qc"
            | "Qo"
            | "Re"
            | "Rs"
            | "Sc"
            | "So"
            | "Xc"
            | "Xo"
            | "Aq"
            | "Bq"
            | "Brq"
            | "D1"
            | "Dl"
            | "Dq"
            | "En"
            | "Op"
            | "Pq"
            | "Ql"
            | "Qq"
            | "Sq"
            | "Vt"
            | "Ta"
            | "%A"
            | "%B"
            | "%C"
            | "%D"
            | "%I"
            | "%J"
            | "%N"
            | "%O"
            | "%P"
            | "%Q"
            | "%R"
            | "%T"
            | "%U"
            | "%V"
            | "Ad"
            | "An"
            | "Ap"
            | "Ar"
            | "At"
            | "Bsx"
            | "Bt"
            | "Bx"
            | "Cd"
            | "Cm"
            | "Db"
            | "Dd"
            | "Dt"
            | "Dv"
            | "Dx"
            | "Em"
            | "Er"
            | "Es"
            | "Ev"
            | "Ex"
            | "Fa"
            | "Fd"
            | "Fl"
            | "Fn"
            | "Fr"
            | "Ft"
            | "Fx"
            | "Hf"
            | "Ic"
            | "In"
            | "Lb"
            | "Li"
            | "Lk"
            | "Lp"
            | "Ms"
            | "Mt"
            | "Nm"
            | "No"
            | "Ns"
            | "Nx"
            | "Os"
            | "Ot"
            | "Ox"
            | "Pa"
            | "Pf"
            | "Pp"
            | "Rv"
            | "Sm"
            | "St"
            | "Sx"
            | "Sy"
            | "Tg"
            | "Tn"
            | "Ud"
            | "Ux"
            | "Va"
            | "Vt"
            | "Xr"
    )
}

/// Whether `line` is a control line invoking the macro `name`.
///
/// The macro name must be followed by whitespace or end the line, so prose that
/// merely mentions `.Ed` is not mistaken for a block end and `.Blah` is not
/// mistaken for `.Bl`.
fn is_macro_line(line: &str, name: &str) -> bool {
    let Some(rest) = line.trim_start().strip_prefix('.') else {
        return false;
    };
    let Some(rest) = rest.trim_start().strip_prefix(name) else {
        return false;
    };
    rest.is_empty() || rest.starts_with(char::is_whitespace)
}

pub fn prepare_document(text: &str) -> String {
    let mut is_bd_literal_block = false;
    let mut bl_depth: i32 = 0;

    text.lines()
        .map(|l| {
            // Track list nesting so a `.It` that is not inside any `.Bl` (stray)
            // can be rendered as plain text instead of being silently dropped by
            // the grammar (which only recognizes `.It` within a list).
            let trimmed = l.trim_start();
            if is_macro_line(trimmed, "Bl") {
                bl_depth += 1;
            } else if is_macro_line(trimmed, "El") {
                bl_depth = (bl_depth - 1).max(0);
            }
            let stray_it = bl_depth == 0 && is_macro_line(trimmed, "It");
            let source = if stray_it {
                trimmed.strip_prefix(".It").unwrap_or(trimmed).trim()
            } else {
                l
            };

            let line = if !stray_it && source.contains(".It") {
                source.replace('\t', " Ta ").replace("    ", " Ta ")
            } else {
                source.to_string()
            };

            // Match the control line, not a substring of it: prose mentioning
            // `.Ed` used to end literal mode part-way through a block, and any
            // line merely naming `.Bd -literal` used to start one.
            if is_macro_line(&line, "Bd")
                && (line.contains("-literal") || line.contains("-unfilled"))
            {
                is_bd_literal_block = true;
            }

            // A section heading closes an unterminated display, matching what
            // the parser's own frame stack does.
            if is_bd_literal_block && (is_macro_line(&line, "Ed") || is_macro_line(&line, "Sh")) {
                is_bd_literal_block = false;
            }

            let transformed_line = if is_bd_literal_block {
                let mut leading_spaces = if line.is_empty() { 1 } else { 0 };
                let mut index = 0;
                for (i, ch) in line.char_indices() {
                    if !ch.is_whitespace() {
                        break;
                    }
                    leading_spaces += if ch == '\t' { 4 } else { 1 };
                    index = i + ch.len_utf8();
                }

                format!("{}{}", "\\^".repeat(leading_spaces), &line[index..])
            } else {
                line.clone()
            };

            let mut processed_line = if let Some(first_word) = line.split_whitespace().next() {
                if does_start_with_macro(first_word) {
                    format!("\\&{}", transformed_line)
                } else {
                    transformed_line
                }
            } else {
                transformed_line
            };

            let count_partials = processed_line
                .split_whitespace()
                .filter(|word| BLOCK_PARTIAL_IMPLICIT.contains(word))
                .count();

            if count_partials > 0 {
                processed_line.push_str(&"\n".repeat(count_partials));
            }

            processed_line
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Strip a single *matching* pair of surrounding double quotes (unless the
/// closing one is escaped with `\&`).
///
/// The leading and trailing quotes used to be stripped independently, so a
/// string that merely ended in a quote lost it: `#include "myheader.h"` came
/// out as `#include "myheader.h`, in prose and inside `.Bd -literal` alike,
/// which is exactly where code samples live.
pub fn trim_quotes(s: String) -> String {
    if s.len() >= 2 && s.starts_with('"') && s.ends_with('"') && !s.ends_with("\\&\"") {
        return s[1..s.len() - 1].to_string();
    }

    s
}

/// Mdoc files parser (a thin wrapper over the hand-written parser).
pub struct MdocParser;

/// Stores macro parameters and subnodes
#[derive(Debug, Clone, PartialEq)]
pub struct MacroNode {
    /// Macro type
    pub mdoc_macro: Macro,
    /// Sub nodes of current node
    pub nodes: Vec<Element>,
}

/// Mdoc language units
#[derive(Debug, Clone, PartialEq)]
pub enum Element {
    /// Text node
    Text(String),
    /// Macro node
    Macro(MacroNode),
    /// "End of input" marker
    Eoi,
}

impl From<Element> for String {
    fn from(element: Element) -> Self {
        match element {
            Element::Text(text) => text,
            Element::Macro(macro_node) => format!("{:?}", macro_node),
            Element::Eoi => "EOI".to_string(),
        }
    }
}

impl From<String> for Element {
    fn from(value: String) -> Self {
        Element::Text(value)
    }
}

/// Stores full mdoc AST
#[derive(Debug, Clone, PartialEq)]
pub struct MdocDocument {
    pub elements: Vec<Element>,
}

impl MdocParser {
    /// Parse a full mdoc document into the AST (delegated to the hand-written
    /// parser; pest has been removed).
    ///
    /// The only failure is a document nested past the parser's depth cap. It is
    /// reported rather than truncated, because the AST is walked recursively
    /// when it is cloned, formatted and dropped, and overflowing that walk
    /// aborts the process.
    pub fn parse_mdoc(input: &str) -> Result<MdocDocument, NestingTooDeep> {
        crate::man_util::parse::mdoc::parse_mdoc_v2(input)
    }
}

#[cfg(test)]
mod tests {
    use super::{is_macro_line, prepare_document, trim_quotes};

    #[test]
    fn trim_quotes_needs_a_matching_pair() {
        // The two quotes used to be stripped independently, so any line that
        // merely ended in one lost it.
        assert_eq!(trim_quotes("\"quoted\"".into()), "quoted");
        assert_eq!(
            trim_quotes("#include \"myheader.h\"".into()),
            "#include \"myheader.h\""
        );
        assert_eq!(trim_quotes("\"unclosed".into()), "\"unclosed");
        assert_eq!(trim_quotes("unopened\"".into()), "unopened\"");
        assert_eq!(trim_quotes("\"".into()), "\"");
        assert_eq!(trim_quotes(String::new()), "");
    }

    #[test]
    fn is_macro_line_matches_the_whole_name() {
        assert!(is_macro_line(".Ed", "Ed"));
        assert!(is_macro_line(".Bl -tag", "Bl"));
        assert!(is_macro_line("  .El", "El"));
        // A longer macro whose name merely starts with the one asked for.
        assert!(!is_macro_line(".Edx", "Ed"));
        assert!(!is_macro_line(".Blah", "Bl"));
        // Prose, not a control line.
        assert!(!is_macro_line("See .Ed for details", "Ed"));
    }

    #[test]
    fn prose_mentioning_ed_does_not_end_literal_mode() {
        // Literal tracking tested `line.contains(".Ed")`, so a sentence naming
        // the macro ended the block and the rest of it lost its indentation.
        let out = prepare_document(".Bd -literal\n    a\nSee .Ed for details\n    b\n.Ed\n");
        assert_eq!(
            out.matches("\\^").count(),
            8,
            "both indented lines keep their escaped leading spaces: {out:?}"
        );
    }
}
