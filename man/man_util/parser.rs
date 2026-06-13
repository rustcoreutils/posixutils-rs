//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::mdoc_macro::*;

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

pub fn prepare_document(text: &str) -> String {
    let mut is_bd_literal_block = false;
    let mut bl_depth: i32 = 0;

    text.lines()
        .map(|l| {
            // Track list nesting so a `.It` that is not inside any `.Bl` (stray)
            // can be rendered as plain text instead of being silently dropped by
            // the grammar (which only recognizes `.It` within a list).
            let trimmed = l.trim_start();
            if trimmed.starts_with(".Bl") {
                bl_depth += 1;
            } else if trimmed.starts_with(".El") {
                bl_depth = (bl_depth - 1).max(0);
            }
            let stray_it = bl_depth == 0 && trimmed.starts_with(".It");
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

            if line.contains(".Bd") && (line.contains("-literal") || line.contains("-unfilled")) {
                is_bd_literal_block = true;
            }

            if is_bd_literal_block && line.contains(".Ed") {
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

/// Strip a single pair of surrounding double quotes (unless escaped with `\&`).
pub fn trim_quotes(mut s: String) -> String {
    if !s.starts_with("\\&\"") {
        if let Some(stripped) = s.strip_prefix("\"") {
            s = stripped.to_string();
        }
    }
    if !s.ends_with("\\&\"") {
        if let Some(stripped) = s.strip_suffix("\"") {
            s = stripped.to_string();
        }
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
    /// parser; pest has been removed). The parser is total — it never fails.
    pub fn parse_mdoc(input: &str) -> MdocDocument {
        crate::man_util::parse::mdoc::parse_mdoc_v2(input)
    }
}
