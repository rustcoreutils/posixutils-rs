//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Hand-written recursive-descent mdoc parser (grown to parity with pest).
//!
//! Implicit blocks (`.Sh`/`.Ss`/`.Nd`) scope following content until a closing
//! macro, modeled with an explicit frame stack — no PEG backtracking. Macro
//! arguments are tokenized per word into `Text` nodes; a plain text line becomes
//! a single `Text` node, matching the pest AST.

use crate::man_util::mdoc_macro::Macro;
use crate::man_util::parser::{prepare_document, trim_quotes, Element, MacroNode, MdocDocument};

/// A partially-built block: its macro (None = document root) and its children.
struct Frame {
    mac: Option<Macro>,
    nodes: Vec<Element>,
}

/// Parse an mdoc document into the AST. Coverage is being grown to parity with
/// the pest parser; unimplemented macros currently degrade to text.
pub fn parse_mdoc_v2(input: &str) -> MdocDocument {
    let prepared = prepare_document(input);
    let mut p = Parser {
        stack: vec![Frame {
            mac: None,
            nodes: Vec::new(),
        }],
    };

    for line in prepared.lines() {
        p.line(line);
    }

    MdocDocument {
        elements: p.finish(),
    }
}

struct Parser {
    stack: Vec<Frame>,
}

impl Parser {
    /// Append a finished element to the innermost open block.
    fn push(&mut self, el: Element) {
        self.stack.last_mut().unwrap().nodes.push(el);
    }

    /// Open a new implicit block, after closing the blocks it terminates.
    fn open_block(&mut self, mac: Macro, closes: &[fn(&Macro) -> bool]) {
        while self.stack.len() > 1 {
            let top_is_closable = self
                .stack
                .last()
                .unwrap()
                .mac
                .as_ref()
                .map(|m| closes.iter().any(|f| f(m)))
                .unwrap_or(false);
            if !top_is_closable {
                break;
            }
            self.close_top();
        }
        self.stack.push(Frame {
            mac: Some(mac),
            nodes: Vec::new(),
        });
    }

    /// Pop the innermost block and attach it to its parent.
    fn close_top(&mut self) {
        let frame = self.stack.pop().unwrap();
        let node = Element::Macro(MacroNode {
            mdoc_macro: frame.mac.unwrap(),
            nodes: frame.nodes,
        });
        self.stack.last_mut().unwrap().nodes.push(node);
    }

    /// Close every open block and return the document elements.
    fn finish(&mut self) -> Vec<Element> {
        while self.stack.len() > 1 {
            self.close_top();
        }
        std::mem::take(&mut self.stack[0].nodes)
    }

    fn line(&mut self, line: &str) {
        // A control line begins with the control char '.'.
        let is_control = line.starts_with('.');
        if !is_control {
            self.text_line(line);
            return;
        }

        let body = line[1..].trim_start();
        // Comment control line.
        if body.starts_with("\\\"") || body.is_empty() {
            return;
        }
        let (name, rest) = split_first(body);
        self.control(name, rest);
    }

    fn text_line(&mut self, line: &str) {
        if line.trim().is_empty() {
            return;
        }
        self.push(Element::Text(trim_quotes(line.to_string())));
    }

    fn control(&mut self, name: &str, rest: &str) {
        match name {
            "Sh" => {
                self.open_block(
                    Macro::Sh {
                        title: tokenize(rest).join(" "),
                    },
                    &[is_sh, is_ss, is_nd],
                );
            }
            "Ss" => {
                self.open_block(
                    Macro::Ss {
                        title: tokenize(rest).join(" "),
                    },
                    &[is_ss, is_nd],
                );
            }
            "Nd" => {
                self.open_block(Macro::Nd, &[is_nd]);
                for word in tokenize(rest) {
                    self.push(Element::Text(word));
                }
            }
            "Pp" | "Lp" => {
                self.push(Element::Macro(MacroNode {
                    mdoc_macro: macro_for_argless(name),
                    nodes: Vec::new(),
                }));
            }
            _ => {
                if let Some(mac) = simple_inline(name) {
                    // Inline macros that take only text arguments, one Text node
                    // per word.
                    let nodes = tokenize(rest).into_iter().map(Element::Text).collect();
                    self.push(Element::Macro(MacroNode {
                        mdoc_macro: mac,
                        nodes,
                    }));
                } else {
                    // Not yet implemented: degrade to text so the v2 path stays
                    // usable. (Production defaults to pest until v2 is complete.)
                    let mut text = String::from(".");
                    text.push_str(name);
                    if !rest.is_empty() {
                        text.push(' ');
                        text.push_str(rest);
                    }
                    self.push(Element::Text(text));
                }
            }
        }
    }
}

/// Inline macros that take only text arguments and map to a unit `Macro`
/// variant (each argument word becomes a `Text` child).
fn simple_inline(name: &str) -> Option<Macro> {
    Some(match name {
        "Ad" => Macro::Ad,
        "Ar" => Macro::Ar,
        "Cd" => Macro::Cd,
        "Cm" => Macro::Cm,
        "Dv" => Macro::Dv,
        "Em" => Macro::Em,
        "Er" => Macro::Er,
        "Ev" => Macro::Ev,
        "Fa" => Macro::Fa,
        "Fl" => Macro::Fl,
        "Ft" => Macro::Ft,
        "Ic" => Macro::Ic,
        "Li" => Macro::Li,
        "Ms" => Macro::Ms,
        "No" => Macro::No,
        "Pa" => Macro::Pa,
        "Sx" => Macro::Sx,
        "Sy" => Macro::Sy,
        "Tn" => Macro::Tn,
        "Va" => Macro::Va,
        _ => return None,
    })
}

fn macro_for_argless(name: &str) -> Macro {
    match name {
        "Pp" => Macro::Pp,
        "Lp" => Macro::Lp,
        _ => unreachable!(),
    }
}

fn is_sh(m: &Macro) -> bool {
    matches!(m, Macro::Sh { .. })
}
fn is_ss(m: &Macro) -> bool {
    matches!(m, Macro::Ss { .. })
}
fn is_nd(m: &Macro) -> bool {
    matches!(m, Macro::Nd)
}

/// Split a string into its first whitespace-delimited token and the remainder.
fn split_first(s: &str) -> (&str, &str) {
    let s = s.trim_start();
    match s.find(char::is_whitespace) {
        Some(i) => (&s[..i], s[i..].trim_start()),
        None => (s, ""),
    }
}

/// Tokenize macro arguments per word, honoring double quotes, returning each
/// argument with surrounding quotes stripped.
fn tokenize(s: &str) -> Vec<String> {
    let mut args = Vec::new();
    let mut cur = String::new();
    let mut in_quote = false;
    let mut has = false;
    for ch in s.chars() {
        match ch {
            '"' => {
                in_quote = !in_quote;
                has = true;
            }
            c if c.is_whitespace() && !in_quote => {
                if has {
                    args.push(std::mem::take(&mut cur));
                    has = false;
                }
            }
            c => {
                cur.push(c);
                has = true;
            }
        }
    }
    if has {
        args.push(cur);
    }
    args
}

#[cfg(test)]
mod tests {
    use super::parse_mdoc_v2;
    use crate::man_util::parser::MdocParser;

    /// Assert the hand-written parser produces the same AST as pest.
    fn parity(input: &str) {
        let pest = MdocParser::parse_mdoc(input).unwrap();
        let v2 = parse_mdoc_v2(input);
        assert_eq!(pest.elements, v2.elements, "input: {input:?}");
    }

    #[test]
    fn sh_without_body() {
        parity(".Sh SECTION");
    }

    #[test]
    fn sh_title_line() {
        parity(".Sh TITLE LINE\nLine 1\n");
    }

    #[test]
    fn sh_multiple_chapters() {
        parity(".Sh SECTION 1\nLine 1\n.Sh SECTION 2\nLine 2\n");
    }

    #[test]
    fn sh_name_with_nd() {
        parity(".Sh NAME\nLine 1\n.Nd short description");
    }

    #[test]
    fn ss_within_sh() {
        parity(".Sh A\ntext a\n.Ss B\ntext b\n.Sh C\ntext c\n");
    }

    #[test]
    fn pp_paragraph() {
        parity(".Sh A\nline\n.Pp\nmore\n");
    }

    #[test]
    fn inline_flag_and_arg() {
        parity(".Fl x\n");
        parity(".Ar file\n");
        parity(".Fl\n");
    }

    #[test]
    fn inline_multiple_text_args() {
        parity(".Cm one two three\n");
        parity(".Sy bold words\n");
    }

    #[test]
    fn inline_in_section() {
        parity(".Sh OPTIONS\n.Fl v\n.Ar path\n.Em note\n");
    }

    #[test]
    fn inline_assorted_macros() {
        for input in [
            ".Pa /etc/passwd\n",
            ".Va errno\n",
            ".Dv NULL\n",
            ".Ic command\n",
            ".Li literal\n",
            ".Ms alpha\n",
            ".No normal\n",
        ] {
            parity(input);
        }
    }
}
