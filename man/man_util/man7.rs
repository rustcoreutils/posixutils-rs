//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A minimal renderer for legacy `man(7)` (roff `man` macro) pages.
//!
//! The main engine in `formatter.rs` handles `mdoc(7)` only; most Linux pages
//! are `man(7)` (`.TH`/`.SH`/`.B`/`.TP`/…), which that engine renders as an
//! empty page. This module covers the common `man(7)` macro subset so those
//! pages display. It deliberately does *not* implement roff programmability
//! (`.if`/`.ie`/`.de`/`.nr`/`.ds`/`.so`).

use crate::man_util::formatter::{apply_styling, display_width, STYLE_BOLD, STYLE_RESET, STYLE_UL};
use crate::FormattingSettings;

/// Returns true if `content` looks like a `man(7)` page (its first macro is
/// `.TH`/`.SH`), as opposed to an `mdoc(7)` page (which starts with `.Dd`/`.Dt`).
pub fn is_man7(content: &str) -> bool {
    for line in content.lines() {
        let t = line.trim_start();
        if t.is_empty() || t.starts_with(".\\\"") || t.starts_with("'\\\"") {
            continue;
        }
        if let Some(rest) = t.strip_prefix('.') {
            let macro_name = rest.split_whitespace().next().unwrap_or("");
            return matches!(macro_name, "TH" | "SH");
        }
        // Leading text before any macro: not a recognizable man(7) page.
        return false;
    }
    false
}

/// Render a `man(7)` page to terminal bytes.
pub fn format_man7(content: &str, settings: &FormattingSettings) -> Vec<u8> {
    Man7Formatter::new(settings).render(content)
}

/// Did the page render any real content (beyond header/footer)? Used to reject
/// an unsupported/empty page with a non-zero exit rather than displaying a bare
/// header.
pub fn produced_body(content: &str, settings: &FormattingSettings) -> bool {
    let mut f = Man7Formatter::new(settings);
    f.process(content);
    f.flush_fill();
    f.has_body
}

/// Map a manual section number to its conventional volume title (used to center
/// the page header, matching the mdoc renderer's headers).
fn volume_title(section: &str) -> &'static str {
    match section.chars().next() {
        Some('1') => "General Commands Manual",
        Some('2') => "System Calls Manual",
        Some('3') => "Library Functions Manual",
        Some('4') => "Device Drivers Manual",
        Some('5') => "File Formats Manual",
        Some('6') => "Games Manual",
        Some('7') => "Miscellaneous Information Manual",
        Some('8') => "System Manager's Manual",
        Some('9') => "Kernel Developer's Manual",
        _ => "Manual",
    }
}

/// Compose a three-part header/footer line: `left` flush-left, `center`
/// centered, `right` flush-right, within `width` columns.
fn three_part(left: &str, center: &str, right: &str, width: usize) -> String {
    let mut line = String::from(left);
    let center_start = width.saturating_sub(center.chars().count()) / 2;
    while line.chars().count() < center_start {
        line.push(' ');
    }
    line.push_str(center);
    let right_start = width.saturating_sub(right.chars().count());
    while line.chars().count() < right_start {
        line.push(' ');
    }
    line.push_str(right);
    line
}

/// Tokenize a macro line into the macro name and its arguments, honoring double
/// quotes (so `.SH "EXIT STATUS"` is one argument).
fn macro_args(rest: &str) -> Vec<String> {
    let mut args = Vec::new();
    let mut cur = String::new();
    let mut in_quote = false;
    let mut has_token = false;
    for ch in rest.chars() {
        match ch {
            '"' => {
                in_quote = !in_quote;
                has_token = true;
            }
            c if c.is_whitespace() && !in_quote => {
                if has_token {
                    args.push(std::mem::take(&mut cur));
                    has_token = false;
                }
            }
            c => {
                cur.push(c);
                has_token = true;
            }
        }
    }
    if has_token {
        args.push(cur);
    }
    args
}

struct Man7Formatter {
    width: usize,
    indent: usize,
    styling: bool,
    /// Output lines accumulated so far.
    lines: Vec<String>,
    /// Words pending in the current filled paragraph.
    pending: Vec<String>,
    /// Current left indent (base indent + `.RS` levels + tag indent).
    cur_indent: usize,
    /// Stack of `.RS` indents for `.RE`.
    rs_stack: Vec<usize>,
    /// No-fill mode (`.nf`); lines are emitted verbatim.
    no_fill: bool,
    /// Header text (from `.TH`).
    header: String,
    /// Footer text (from `.TH`).
    footer: String,
    /// When a `.TP` tag is pending, the body indent to switch to once the tag
    /// (the next content-producing line, text or font macro) is emitted.
    tp_body_indent: Option<usize>,
    /// True if any body content was produced (for the empty-render check).
    has_body: bool,
}

impl Man7Formatter {
    fn new(settings: &FormattingSettings) -> Self {
        Man7Formatter {
            width: settings.width.max(1),
            indent: settings.indent,
            styling: settings.styling,
            lines: Vec::new(),
            pending: Vec::new(),
            cur_indent: settings.indent,
            rs_stack: Vec::new(),
            no_fill: false,
            header: String::new(),
            footer: String::new(),
            tp_body_indent: None,
            has_body: false,
        }
    }

    fn render(mut self, content: &str) -> Vec<u8> {
        self.process(content);
        self.flush_fill();

        let mut out = Vec::new();
        if !self.header.is_empty() {
            out.push(self.header.clone());
            out.push(String::new());
        }
        out.extend(self.lines.clone());
        if !self.footer.is_empty() {
            out.push(String::new());
            out.push(self.footer.clone());
        }

        let joined = collapse_blank_lines(&out);
        apply_styling(&joined, self.styling).into_bytes()
    }

    fn process(&mut self, content: &str) {
        for raw in content.lines() {
            // A control line starts with '.' or '\''.
            let is_control = raw.starts_with('.') || raw.starts_with('\'');
            if !is_control {
                self.text_line(raw);
                continue;
            }
            let rest = &raw[1..];
            let trimmed = rest.trim_start();
            if trimmed.starts_with("\\\"") || trimmed.is_empty() {
                continue; // comment or bare control char
            }
            let name = trimmed.split_whitespace().next().unwrap_or("");
            let arg_str = trimmed[name.len()..].trim_start();
            self.control_line(name, arg_str);
        }
    }

    fn control_line(&mut self, name: &str, args: &str) {
        match name {
            "TH" => self.do_th(args),
            "SH" => self.do_heading(args, false),
            "SS" => self.do_heading(args, true),
            "PP" | "LP" | "P" | "sp" | "Sp" => {
                self.flush_fill();
                self.lines.push(String::new());
            }
            "br" => self.flush_fill(),
            "nf" => {
                self.flush_fill();
                self.no_fill = true;
            }
            "fi" => {
                self.flush_fill();
                self.no_fill = false;
            }
            "RS" => {
                self.flush_fill();
                self.rs_stack.push(self.cur_indent);
                let extra = macro_args(args)
                    .first()
                    .and_then(|n| n.parse::<usize>().ok())
                    .unwrap_or(self.indent);
                self.cur_indent += extra;
            }
            "RE" => {
                self.flush_fill();
                if let Some(prev) = self.rs_stack.pop() {
                    self.cur_indent = prev;
                }
            }
            "TP" => {
                self.flush_fill();
                let extra = macro_args(args)
                    .first()
                    .and_then(|n| n.parse::<usize>().ok())
                    .unwrap_or(self.indent);
                self.tp_body_indent = Some(self.cur_indent + extra);
            }
            "IP" => self.do_ip(args),
            "HP" => {
                self.flush_fill();
                self.lines.push(String::new());
            }
            "B" | "I" | "SM" | "SB" => {
                let marker = if name == "I" { STYLE_UL } else { STYLE_BOLD };
                let text = if name == "SM" {
                    self.format_words(&macro_args(args), None)
                } else {
                    self.format_words(&macro_args(args), Some(marker))
                };
                self.emit_inline(text);
            }
            "BR" | "RB" | "BI" | "IB" | "IR" | "RI" => {
                let text = self.format_alternating(name, &macro_args(args));
                self.emit_inline(text);
            }
            // Recognized-but-ignored or unknown macros are skipped rather than
            // dumped as literal text.
            _ => {}
        }
    }

    fn do_th(&mut self, args: &str) {
        let a = macro_args(args);
        let title = a.first().cloned().unwrap_or_default();
        let section = a.get(1).cloned().unwrap_or_default();
        let date = a.get(2).cloned().unwrap_or_default();
        let source = a.get(3).cloned().unwrap_or_default();
        let manual = a
            .get(4)
            .cloned()
            .unwrap_or_else(|| volume_title(&section).to_string());

        let left = if section.is_empty() {
            title.clone()
        } else {
            format!("{title}({section})")
        };
        self.header = three_part(&left, &manual, &left, self.width);
        self.footer = three_part(&source, &date, &source, self.width);
    }

    fn do_heading(&mut self, args: &str, sub: bool) {
        self.flush_fill();
        // Reset indentation at each new section.
        self.cur_indent = self.indent;
        self.rs_stack.clear();
        self.tp_body_indent = None;
        let title = macro_args(args).join(" ");
        let title = self.resolve(&title);
        self.lines.push(String::new());
        let indent = if sub { self.indent / 2 } else { 0 };
        self.lines.push(format!(
            "{}{}",
            " ".repeat(indent),
            wrap_marker(&title.to_string(), STYLE_BOLD, self.styling)
        ));
        self.has_body = true;
    }

    fn do_ip(&mut self, args: &str) {
        self.flush_fill();
        let a = macro_args(args);
        let tag = a.first().cloned();
        let extra = a
            .get(1)
            .and_then(|n| n.parse::<usize>().ok())
            .unwrap_or(self.indent);
        let body_indent = self.cur_indent + extra;
        if let Some(tag) = tag {
            let tag = self.resolve(&tag);
            self.lines
                .push(format!("{}{}", " ".repeat(self.cur_indent), tag));
        }
        self.cur_indent = body_indent;
    }

    /// A plain text line: in fill mode add its words to the paragraph, else emit
    /// it verbatim at the current indent.
    fn text_line(&mut self, raw: &str) {
        if let Some(body_indent) = self.tp_body_indent.take() {
            // The line after `.TP` is the tag; the following text is the body.
            let tag = self.resolve(raw.trim());
            self.lines
                .push(format!("{}{}", " ".repeat(self.cur_indent), tag));
            self.cur_indent = body_indent;
            self.has_body = true;
            return;
        }

        if self.no_fill {
            let resolved = self.resolve(raw);
            self.lines
                .push(format!("{}{}", " ".repeat(self.cur_indent), resolved));
            if !resolved.trim().is_empty() {
                self.has_body = true;
            }
            return;
        }

        if raw.trim().is_empty() {
            self.flush_fill();
            self.lines.push(String::new());
            return;
        }

        for word in raw.split_whitespace() {
            self.pending.push(self.resolve(word));
        }
        self.has_body = true;
    }

    fn emit_inline(&mut self, text: String) {
        // A `.TP` tag may itself be a font macro (e.g. `.B 0`); capture it as the
        // tag line and switch to the body indent.
        if let Some(body_indent) = self.tp_body_indent.take() {
            self.lines
                .push(format!("{}{}", " ".repeat(self.cur_indent), text));
            self.cur_indent = body_indent;
            self.has_body = true;
            return;
        }

        if self.no_fill {
            self.flush_fill();
            self.lines
                .push(format!("{}{}", " ".repeat(self.cur_indent), text));
        } else {
            self.pending.push(text);
        }
        self.has_body = true;
    }

    /// Wrap and emit the pending paragraph words at the current indent.
    fn flush_fill(&mut self) {
        if self.pending.is_empty() {
            return;
        }
        let words = std::mem::take(&mut self.pending);
        let indent = " ".repeat(self.cur_indent);
        let max = self.width.saturating_sub(self.cur_indent).max(1);
        let mut line = String::new();
        for word in words {
            let wlen = display_width(&word);
            if !line.is_empty() && display_width(&line) + 1 + wlen > max {
                self.lines.push(format!("{indent}{line}"));
                line = String::new();
            }
            if line.is_empty() {
                line = word;
            } else {
                line.push(' ');
                line.push_str(&word);
            }
        }
        if !line.is_empty() {
            self.lines.push(format!("{indent}{line}"));
        }
    }

    /// Join `args` into one styled token (optionally wrapped in `marker`).
    fn format_words(&self, args: &[String], marker: Option<char>) -> String {
        let text = args
            .iter()
            .map(|a| self.resolve(a))
            .collect::<Vec<_>>()
            .join(" ");
        match marker {
            Some(m) => wrap_marker(&text, m, self.styling),
            None => text,
        }
    }

    /// Alternating-font macros (`.BR`, `.RI`, …): each argument takes the next
    /// font in the pair, joined without spaces.
    fn format_alternating(&self, name: &str, args: &[String]) -> String {
        let fonts: [Option<char>; 2] = match name {
            "BR" => [Some(STYLE_BOLD), None],
            "RB" => [None, Some(STYLE_BOLD)],
            "BI" => [Some(STYLE_BOLD), Some(STYLE_UL)],
            "IB" => [Some(STYLE_UL), Some(STYLE_BOLD)],
            "IR" => [Some(STYLE_UL), None],
            "RI" => [None, Some(STYLE_UL)],
            _ => [None, None],
        };
        let mut out = String::new();
        for (i, arg) in args.iter().enumerate() {
            let text = self.resolve(arg);
            match fonts[i % 2] {
                Some(m) => out.push_str(&wrap_marker(&text, m, self.styling)),
                None => out.push_str(&text),
            }
        }
        out
    }

    /// Resolve roff escapes (`\fB…`, `\-`, special chars) using the shared mdoc
    /// escape machinery.
    fn resolve(&self, s: &str) -> String {
        crate::man_util::formatter::replace_escapes(s)
    }
}

fn wrap_marker(text: &str, marker: char, styling: bool) -> String {
    if styling {
        format!("{marker}{text}{STYLE_RESET}")
    } else {
        text.to_string()
    }
}

/// Join lines, collapsing runs of more than one blank line into a single blank.
fn collapse_blank_lines(lines: &[String]) -> String {
    let mut out: Vec<&str> = Vec::with_capacity(lines.len());
    let mut prev_blank = false;
    for line in lines {
        let blank = line.trim().is_empty();
        if blank && prev_blank {
            continue;
        }
        out.push(line);
        prev_blank = blank;
    }
    out.join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    const SETTINGS: FormattingSettings = FormattingSettings {
        width: 78,
        indent: 6,
        styling: false,
    };

    fn render(content: &str) -> String {
        String::from_utf8(format_man7(content, &SETTINGS)).unwrap()
    }

    #[test]
    fn detects_man7_vs_mdoc() {
        assert!(is_man7(".\\\" comment\n.TH T 1\n.SH NAME\n"));
        assert!(is_man7(".SH NAME\n"));
        assert!(!is_man7(".Dd today\n.Dt T 1\n")); // mdoc
        assert!(!is_man7("just text\n"));
        assert!(!is_man7(""));
    }

    #[test]
    fn renders_sections_and_body() {
        let out = render(
            ".TH CAT 1 2024 \"util 1.0\"\n\
             .SH NAME\n\
             cat \\- concatenate\n\
             .SH DESCRIPTION\n\
             Write files to standard output.\n",
        );
        assert!(out.contains("CAT(1)"), "header: {out}");
        assert!(out.contains("NAME"), "{out}");
        assert!(out.contains("cat - concatenate"), "{out}");
        assert!(out.contains("DESCRIPTION"), "{out}");
        assert!(out.contains("Write files to standard output."), "{out}");
    }

    #[test]
    fn tp_tag_macro_orders_correctly() {
        // The tag is itself a font macro; it must appear before its body.
        let out = render(".TH T 1\n.SH OPTIONS\n.TP\n.B \\-n\nNumber lines.\n");
        let n_pos = out.find("-n").expect("tag present");
        let body_pos = out.find("Number lines.").expect("body present");
        assert!(n_pos < body_pos, "tag must precede body:\n{out}");
    }

    #[test]
    fn alternating_fonts_join_without_spaces() {
        // `.BR cat (1)` -> "cat(1)" (no space between alternating args).
        let out = render(".TH T 1\n.SH SEE ALSO\n.BR cat (1)\n");
        assert!(out.contains("cat(1)"), "{out}");
    }

    #[test]
    fn empty_man7_has_no_body() {
        assert!(!produced_body(".TH T 1\n", &SETTINGS));
        assert!(produced_body(".TH T 1\n.SH NAME\nx\n", &SETTINGS));
    }

    #[test]
    fn styling_emits_overstrike() {
        let styled = FormattingSettings {
            styling: true,
            ..SETTINGS
        };
        let out = String::from_utf8(format_man7(".TH T 1\n.SH D\n.B bold\n", &styled)).unwrap();
        assert!(out.contains("b\u{8}bo\u{8}ol\u{8}ld\u{8}d"), "{out:?}");
    }
}
