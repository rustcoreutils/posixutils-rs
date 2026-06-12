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

use crate::man_util::mdoc_macro::types::{AnType, BdType, BfType, BlType, OffsetType, SmMode};
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
    /// Append a finished element to the innermost open block. Content directly
    /// inside a `.Bl` (i.e. not within an `.It`) is `bl_skip` — dropped.
    fn push(&mut self, el: Element) {
        let top = self.stack.last_mut().unwrap();
        if matches!(top.mac, Some(Macro::Bl { .. })) {
            return;
        }
        top.nodes.push(el);
    }

    /// Close any open `.It` item back to the enclosing `.Bl`.
    fn close_open_items(&mut self) {
        while matches!(self.stack.last().unwrap().mac, Some(Macro::It { .. })) {
            self.close_top();
        }
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

    /// Close the nearest explicit block matching `is_match` (and any frames
    /// nested inside it), for `.Ed`/`.Ef`/`.Ek`.
    fn close_explicit(&mut self, is_match: fn(&Macro) -> bool) {
        while self.stack.len() > 1 {
            let matched = self
                .stack
                .last()
                .unwrap()
                .mac
                .as_ref()
                .map(is_match)
                .unwrap_or(false);
            self.close_top();
            if matched {
                break;
            }
        }
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
            // The Sh/Ss title is the raw remainder (trailing space trimmed).
            "Sh" => self.open_block(
                Macro::Sh {
                    title: rest.trim_end().to_string(),
                },
                &[is_sh, is_ss, is_nd],
            ),
            "Ss" => self.open_block(
                Macro::Ss {
                    title: rest.trim_end().to_string(),
                },
                &[is_ss, is_nd],
            ),
            "Nd" => {
                self.open_block(Macro::Nd, &[is_nd]);
                for word in tokenize(rest) {
                    self.push(Element::Text(word));
                }
            }
            "Pp" | "Lp" | "Bt" | "Ud" => self.push(Element::Macro(MacroNode {
                mdoc_macro: macro_for_argless(name),
                nodes: Vec::new(),
            })),
            "Dd" => {
                // The whole date line is a single Text node.
                let nodes = if rest.is_empty() {
                    Vec::new()
                } else {
                    vec![Element::Text(rest.trim_end().to_string())]
                };
                self.push(Element::Macro(MacroNode {
                    mdoc_macro: Macro::Dd,
                    nodes,
                }));
            }
            "Dt" => self.push(parse_dt(rest)),
            // Block-full-explicit displays close on .Ed/.Ef/.Ek.
            "Bd" => self.open_block(parse_bd(rest), &[]),
            "Bf" => self.open_block(
                Macro::Bf(bf_type(tokenize(rest).first().map(|s| s.as_str()))),
                &[],
            ),
            "Bk" => self.open_block(Macro::Bk, &[]),
            "Ed" => self.close_explicit(is_bd),
            "Ef" => self.close_explicit(is_bf),
            "Ek" => self.close_explicit(is_bk),
            "Bl" => self.open_block(parse_bl(rest), &[]),
            "It" => {
                self.close_open_items();
                let head = parse_inline_seq(tokenize(rest));
                self.stack.push(Frame {
                    mac: Some(Macro::It { head }),
                    nodes: Vec::new(),
                });
            }
            "El" => {
                self.close_open_items();
                self.close_explicit(is_bl);
            }
            "Sm" => {
                // Optional on/off spacing mode; the first arg is consumed either
                // way (matching pest), the rest become text nodes.
                let mut it = tokenize(rest).into_iter();
                let mode = match it.next().as_deref() {
                    Some("on") => Some(SmMode::On),
                    Some("off") => Some(SmMode::Off),
                    _ => None,
                };
                self.push(Element::Macro(MacroNode {
                    mdoc_macro: Macro::Sm(mode),
                    nodes: it.map(Element::Text).collect(),
                }));
            }
            "Fd" => {
                let mut it = tokenize(rest).into_iter();
                let directive = it.next().unwrap_or_default();
                self.push(Element::Macro(MacroNode {
                    mdoc_macro: Macro::Fd {
                        directive,
                        arguments: it.collect(),
                    },
                    nodes: Vec::new(),
                }));
            }
            "Os" => self.push(Element::Macro(MacroNode {
                mdoc_macro: Macro::Os,
                nodes: tokenize(rest).into_iter().map(Element::Text).collect(),
            })),
            _ if is_callable(name) => {
                let mut tokens = vec![name.to_string()];
                tokens.extend(tokenize(rest));
                for el in parse_inline_seq(tokens) {
                    self.push(el);
                }
            }
            // Not yet implemented: degrade to text so the v2 path stays usable.
            // (Production defaults to pest until v2 is complete.)
            _ => {
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

/// Inline macros that take only text arguments and map to a unit `Macro`
/// variant (each argument word becomes a `Text` child).
fn simple_inline(name: &str) -> Option<Macro> {
    Some(match name {
        "Ad" => Macro::Ad,
        "Ap" => Macro::Ap,
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
        "Mt" => Macro::Mt,
        "No" => Macro::No,
        "Ns" => Macro::Ns,
        "Pa" => Macro::Pa,
        "Sx" => Macro::Sx,
        "Sy" => Macro::Sy,
        "Tn" => Macro::Tn,
        "Va" => Macro::Va,
        _ => return None,
    })
}

/// Parse a sequence of inline elements from a token stream (a control line's
/// words). A *leaf* macro (Fl/Ar/Xr/Nm/…) consumes following text words as its
/// arguments until the next callable token; a *container* partial-implicit macro
/// (Op/Aq/Bq/…) greedily wraps the rest of the line as its children. Bare words
/// become Text nodes.
fn parse_inline_seq(tokens: Vec<String>) -> Vec<Element> {
    let mut out = Vec::new();
    let mut toks = tokens.into_iter().peekable();
    while let Some(tok) = toks.next() {
        if tok == "Ta" {
            // Column separator (its own macro node).
            out.push(Element::Macro(MacroNode {
                mdoc_macro: Macro::Ta,
                nodes: Vec::new(),
            }));
            continue;
        }
        if tok == "An" {
            // .An -split / -nosplit consume only the flag; otherwise the rest of
            // the line is the author name.
            match toks.peek().map(|s| s.as_str()) {
                Some("-split") => {
                    toks.next();
                    out.push(an_node(AnType::Split, Vec::new()));
                }
                Some("-nosplit") => {
                    toks.next();
                    out.push(an_node(AnType::NoSplit, Vec::new()));
                }
                _ => {
                    let rest: Vec<String> = toks.by_ref().collect();
                    out.push(an_node(AnType::Name, parse_inline_seq(rest)));
                    return out;
                }
            }
            continue;
        }
        if tok == "Fn" {
            // .Fn funcname [args…] — funcname is the first word, the rest are
            // (possibly nested) argument nodes.
            let funcname = toks.next().unwrap_or_default();
            let rest: Vec<String> = toks.by_ref().collect();
            out.push(Element::Macro(MacroNode {
                mdoc_macro: Macro::Fn { funcname },
                nodes: parse_inline_seq(rest),
            }));
            return out;
        }
        if let Some(mac) = container_macro(&tok) {
            let rest: Vec<String> = toks.collect();
            out.push(Element::Macro(MacroNode {
                mdoc_macro: mac,
                nodes: parse_inline_seq(rest),
            }));
            return out;
        } else if is_leaf(&tok) {
            let mut args = Vec::new();
            while let Some(t) = toks.peek() {
                if is_callable(t) {
                    break;
                }
                args.push(toks.next().unwrap());
            }
            out.push(make_leaf(&tok, args));
        } else {
            out.push(Element::Text(tok));
        }
    }
    out
}

/// Build a leaf inline macro node (simple text-arg macro, or Xr/Nm).
fn make_leaf(name: &str, args: Vec<String>) -> Element {
    let node = if let Some(mac) = simple_inline(name) {
        MacroNode {
            mdoc_macro: mac,
            nodes: args.into_iter().map(Element::Text).collect(),
        }
    } else if name == "Xr" {
        let mut it = args.into_iter();
        let xr_name = it.next().unwrap_or_default();
        let section = it.next().unwrap_or_default();
        MacroNode {
            mdoc_macro: Macro::Xr {
                name: xr_name,
                section,
            },
            nodes: it.map(Element::Text).collect(),
        }
    } else if name == "Lk" {
        // Lk: the first argument is the URI, the rest are link-text nodes.
        let mut it = args.into_iter();
        let uri = it.next().unwrap_or_default();
        MacroNode {
            mdoc_macro: Macro::Lk { uri },
            nodes: it.map(Element::Text).collect(),
        }
    } else {
        // Nm: first arg is the name if purely alphanumeric, else a text node.
        let mut nm_name = None;
        let mut nodes = Vec::new();
        let mut it = args.into_iter();
        if let Some(first) = it.next() {
            if first.chars().all(|c| c.is_alphanumeric()) {
                nm_name = Some(first);
            } else {
                nodes.push(Element::Text(first));
            }
        }
        nodes.extend(it.map(Element::Text));
        MacroNode {
            mdoc_macro: Macro::Nm { name: nm_name },
            nodes,
        }
    };
    Element::Macro(node)
}

/// Parse a `.Dt title section [arch]` line. With a single argument it is the
/// section (no title), matching pest's optional-title rule.
fn parse_dt(rest: &str) -> Element {
    let args = tokenize(rest);
    let (title, section) = match args.len() {
        0 => (None, String::new()),
        1 => (None, args[0].clone()),
        _ => (Some(args[0].clone()), args[1].clone()),
    };
    let arch = args.get(2).map(|a| a.trim().to_string());
    Element::Macro(MacroNode {
        mdoc_macro: Macro::Dt {
            title,
            section,
            arch,
        },
        nodes: Vec::new(),
    })
}

/// Parse a `.Bd -type [-offset X] [-compact]` display-block opener.
fn parse_bd(rest: &str) -> Macro {
    let toks = tokenize(rest);
    let mut it = toks.iter();
    let block_type = match it.next().map(|s| s.as_str()) {
        Some("-centered") => BdType::Centered,
        Some("-literal") => BdType::Literal,
        Some("-ragged") => BdType::Ragged,
        Some("-unfilled") => BdType::Unfilled,
        _ => BdType::Filled,
    };
    let mut offset = None;
    let mut compact = false;
    while let Some(t) = it.next() {
        match t.as_str() {
            "-offset" => offset = it.next().map(|v| offset_type(v)),
            "-compact" => compact = true,
            _ => {}
        }
    }
    Macro::Bd {
        block_type,
        offset,
        compact,
    }
}

fn offset_type(s: &str) -> OffsetType {
    match s {
        "indent-two" => OffsetType::IndentTwo,
        "indent" => OffsetType::Indent,
        "left" => OffsetType::Left,
        "right" => OffsetType::Right,
        "center" => OffsetType::Center,
        _ => OffsetType::Indent,
    }
}

fn bf_type(s: Option<&str>) -> BfType {
    match s {
        Some("-literal") | Some("Li") => BfType::Literal,
        Some("-symbolic") | Some("Sy") => BfType::Symbolic,
        _ => BfType::Emphasis,
    }
}

fn is_bd(m: &Macro) -> bool {
    matches!(m, Macro::Bd { .. })
}
fn is_bf(m: &Macro) -> bool {
    matches!(m, Macro::Bf(_))
}
fn is_bk(m: &Macro) -> bool {
    matches!(m, Macro::Bk)
}

/// Partial-implicit container macros: each wraps the remainder of the line.
fn container_macro(name: &str) -> Option<Macro> {
    Some(match name {
        "Aq" => Macro::Aq,
        "Bq" => Macro::Bq,
        "Brq" => Macro::Brq,
        "D1" => Macro::D1,
        "Dl" => Macro::Dl,
        "Dq" => Macro::Dq,
        "En" => Macro::En,
        "Op" => Macro::Op,
        "Pq" => Macro::Pq,
        "Ql" => Macro::Ql,
        "Qq" => Macro::Qq,
        "Sq" => Macro::Sq,
        "Vt" => Macro::Vt,
        _ => return None,
    })
}

/// A leaf inline macro (consumes following text words as arguments).
fn is_leaf(name: &str) -> bool {
    simple_inline(name).is_some() || matches!(name, "Xr" | "Nm" | "Lk")
}

/// Whether `name` is a callable inline macro the v2 parser handles (used both to
/// dispatch and as the chaining boundary).
fn is_callable(name: &str) -> bool {
    is_leaf(name) || container_macro(name).is_some() || matches!(name, "An" | "Fn" | "Ta")
}

/// Parse a `.Bl -type [-width w] [-offset o] [-compact] [columns…]` list opener.
fn parse_bl(rest: &str) -> Macro {
    let toks = tokenize(rest);
    let mut it = toks.iter();
    let list_type = match it.next().map(|s| s.as_str()) {
        Some("-bullet") => BlType::Bullet,
        Some("-column") => BlType::Column,
        Some("-dash") | Some("-hyphen") => BlType::Dash,
        Some("-diag") => BlType::Diag,
        Some("-enum") => BlType::Enum,
        Some("-hang") => BlType::Hang,
        Some("-inset") => BlType::Inset,
        Some("-ohang") => BlType::Ohang,
        Some("-tag") => BlType::Tag,
        _ => BlType::Item,
    };
    let mut width = None;
    let mut offset = None;
    let mut columns = Vec::new();
    let (mut seen_w, mut seen_o, mut compact) = (false, false, false);
    while let Some(t) = it.next() {
        match t.as_str() {
            "-width" if !seen_w => {
                seen_w = true;
                width = parse_width(it.next());
            }
            "-offset" if !seen_o => {
                seen_o = true;
                offset = it.next().map(|s| offset_type(s));
            }
            "-compact" if !compact => compact = true,
            _ => columns.push(t.clone()),
        }
    }
    Macro::Bl {
        list_type,
        width,
        offset,
        compact,
        columns,
    }
}

/// Parse a `.Bl -width` value: a leading number, a known macro width, else the
/// argument's character length.
fn parse_width(w: Option<&String>) -> Option<u8> {
    let s = w?;
    if s.chars().next()?.is_ascii_digit() {
        let digits: String = s.chars().take_while(|c| c.is_ascii_digit()).collect();
        digits.parse::<u8>().ok()
    } else {
        match s.as_str() {
            "Er" => Some(19),
            "Ds" => Some(8),
            "Ev" => Some(17),
            "Fl" => Some(12),
            _ => u8::try_from(s.len()).ok(),
        }
    }
}

fn is_bl(m: &Macro) -> bool {
    matches!(m, Macro::Bl { .. })
}

/// Build an `.An` author node.
fn an_node(author_name_type: AnType, nodes: Vec<Element>) -> Element {
    Element::Macro(MacroNode {
        mdoc_macro: Macro::An { author_name_type },
        nodes,
    })
}

fn macro_for_argless(name: &str) -> Macro {
    match name {
        "Pp" => Macro::Pp,
        "Lp" => Macro::Lp,
        "Bt" => Macro::Bt,
        "Ud" => Macro::Ud,
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
    fn xr_with_and_without_section() {
        parity(".Xr cat 1\n");
        parity(".Xr printf\n");
    }

    #[test]
    fn nm_name_and_text() {
        parity(".Nm cat\n");
        parity(".Nm \\-x\n");
        parity(".Sh NAME\n.Nm grep\n");
    }

    #[test]
    fn inline_chaining_siblings() {
        parity(".Nm foo Ar bar\n");
        parity(".Fl v Ar file\n");
        parity(".Xr cat 1 Ar end\n");
    }

    #[test]
    fn document_prologue() {
        parity(".Dd July 4, 2024\n.Dt PROGNAME 1\n.Os\n");
        parity(".Dt FOO 8 i386\n");
        parity(".Dd $Mdocdate$\n");
        parity(".Os FreeBSD 14.0\n");
    }

    #[test]
    fn full_prologue_with_name() {
        parity(".Dd June 1, 2024\n.Dt CAT 1\n.Os\n.Sh NAME\n.Nm cat\n.Nd concatenate\n");
    }

    #[test]
    fn lists() {
        parity(".Bl -bullet\n.It\nfirst\n.It\nsecond\n.El\n");
        parity(".Bl -tag -width Ds\n.It item one\nbody one\n.It item two\nbody two\n.El\n");
        parity(".Bl -enum -compact\n.It\na\n.It\nb\n.El\n");
        parity(".Bl -dash -offset indent\n.It\nx\n.El\n");
    }

    #[test]
    fn list_with_flag_heads() {
        parity(
            ".Sh OPTIONS\n.Bl -tag -width Fl\n.It Fl v\nverbose\n.It Fl o Ar file\noutput\n.El\n",
        );
    }

    #[test]
    fn list_stray_content_skipped() {
        parity(".Bl -bullet\nstray text\n.It\nitem\n.El\n");
    }

    #[test]
    fn display_blocks() {
        parity(".Bd -literal\ncode line\n.Ed\n");
        parity(".Bd -filled -offset indent\ntext\n.Ed\n");
        parity(".Bd -ragged -offset indent-two -compact\nx\n.Ed\n");
        parity(".Bf -symbolic\nbold\n.Ef\n");
        parity(".Bk -words\nkept\n.Ek\n");
        parity(".Sh A\n.Bd -literal\nx\n.Ed\nafter\n");
    }

    #[test]
    fn special_field_macros() {
        parity(".An -split\n");
        parity(".An -nosplit\n");
        parity(".An John Doe\n");
        parity(".Fn main int argc\n");
        parity(".Sm off\n");
        parity(".Sm on\n");
        parity(".Sm\n");
        parity(".Fd #include <stdio.h>\n");
    }

    #[test]
    fn link_and_spacing_macros() {
        parity(".Mt user@example.com\n");
        parity(".Lk https://example.com label\n");
        parity(".Ap\n");
        parity(".Sh A\n.Ar x Ns y\n");
    }

    #[test]
    fn argless_macros() {
        parity(".Pp\n");
        parity(".Bt\n");
        parity(".Ud\n");
    }

    #[test]
    fn partial_implicit_containers() {
        parity(".Op Fl x\n");
        parity(".Aq address\n");
        parity(".Bq text\n");
        parity(".Op\n");
    }

    #[test]
    fn partial_implicit_nesting() {
        parity(".Op Fl x Ar file\n");
        parity(".Op Aq Fl v\n");
        parity(".Dq quoted words\n");
    }

    #[test]
    fn inline_then_container() {
        parity(".Fl a Op Fl b\n");
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
