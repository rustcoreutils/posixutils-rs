//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A renderer for legacy `man(7)` (roff `man` macro) pages.
//!
//! The main engine in `formatter.rs` handles `mdoc(7)` only; most Linux pages
//! are `man(7)` (`.TH`/`.SH`/`.B`/`.TP`/…), which that engine renders as an
//! empty page. This module covers the common `man(7)` macro subset so those
//! pages display. It drives the shared [`Term`] backend, so fill/wrap/indent
//! and emphasis behave identically to the mdoc renderer.
//!
//! It deliberately does *not* implement roff programmability
//! (`.if`/`.ie`/`.de`/`.nr`/`.ds`/`.so`).

use crate::man_util::formatter::replace_escapes;
use crate::man_util::term::style::{STYLE_BOLD, STYLE_RESET, STYLE_UL};
use crate::man_util::term::Term;
use crate::FormattingSettings;

/// Returns true if `content` is a `man(7)` page rather than an `mdoc(7)` one.
///
/// The decision is made by whichever language-identifying macro appears first —
/// `.TH`/`.SH`/`.SS` for man(7), `.Dd`/`.Dt`/`.Os`/`.Sh` for mdoc(7) — which is
/// how mandoc decides. Everything else is skipped.
///
/// Looking only at the *first* macro is not enough: a page may open with any
/// number of plain roff requests, and `.nh`, `.pc`, `.ig`, `.ds` and `.nr`
/// preambles are all common. Such a page was classified as mdoc, parsed by a
/// parser that knows none of its macros, and its raw roff source printed to the
/// terminal with exit status 0 — which happened for 43 of the 300 pages in
/// `/usr/share/man/man1` on a stock system, including every docker page.
pub fn is_man7(content: &str) -> bool {
    for line in content.lines() {
        let t = line.trim_start();
        let rest = match t.strip_prefix('.').or_else(|| t.strip_prefix('\'')) {
            Some(r) => r.trim_start(),
            None => continue,
        };
        match rest.split_whitespace().next().unwrap_or("") {
            "TH" | "SH" | "SS" => return true,
            "Dd" | "Dt" | "Os" | "Sh" => return false,
            _ => {}
        }
    }
    false
}

/// Render a `man(7)` page to terminal bytes, or `None` if the page produced no
/// real content (beyond header/footer) — used to reject an unsupported/empty
/// page with a non-zero exit rather than displaying a bare header.
pub fn format_man7(content: &str, settings: &FormattingSettings) -> Option<Vec<u8>> {
    Man7Formatter::new(settings).render(content)
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
    /// Base indentation step (also the default `.RS`/`.TP`/`.IP` amount).
    indent: usize,
    /// Whether to emit bold/underline emphasis.
    styling: bool,
    /// The shared terminal backend that owns fill/wrap/indent/assembly.
    term: Term,
    /// No-fill mode (`.nf`); lines are emitted verbatim.
    no_fill: bool,
    /// When a `.TP` tag is pending, the body indent to switch to once the tag
    /// (the next content-producing line, text or font macro) is emitted.
    tp_body_indent: Option<usize>,
    /// The margin that paragraphs start at: the base indent, plus any enclosing
    /// `.RS`. `.TP`/`.IP` hang their tag here and indent their body relative to
    /// it, and `.PP` returns to it.
    ///
    /// Without this, each tag's body indent was computed from the *current*
    /// margin — which for a run of `.TP` entries is the previous entry's body
    /// indent, so every option in a GNU OPTIONS section sat further right than
    /// the last. Real pages have dozens: `ls.1` rendered a 396-column line into
    /// a 78-column terminal.
    para_base: usize,
    /// `para_base` values saved by `.RS`, restored by `.RE`.
    ///
    /// It needs its own stack rather than being read back from the terminal
    /// margin: `.RE` restores the margin that was current when `.RS` ran, and
    /// inside a `.TP` body that is the tag's body indent, not the paragraph
    /// margin. Deriving it from the margin therefore ratcheted the base up by
    /// one step per `.TP` containing an `.RS`/`.RE` pair — the shape used
    /// throughout screen(1), whose deepest line ended up 880 columns in.
    base_stack: Vec<usize>,
}

impl Man7Formatter {
    fn new(settings: &FormattingSettings) -> Self {
        let mut f = Man7Formatter {
            indent: settings.indent,
            styling: settings.styling,
            term: Term::new(settings.width, settings.styling),
            no_fill: false,
            tp_body_indent: None,
            para_base: settings.indent,
            base_stack: Vec::new(),
        };
        // Body text starts at the base indent (sections reset to it).
        f.term.set_offset(settings.indent);
        f
    }

    fn render(mut self, content: &str) -> Option<Vec<u8>> {
        self.process(content);
        if self.term.has_body() {
            Some(self.term.finish())
        } else {
            None
        }
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
            // A paragraph break ends any hanging-tag body and returns to the
            // paragraph margin.
            "PP" | "LP" | "P" | "sp" | "Sp" => {
                self.term.blank_line();
                self.tp_body_indent = None;
                self.term.set_offset(self.para_base);
            }
            "br" => self.term.break_line(),
            "nf" => {
                self.term.break_line();
                self.no_fill = true;
            }
            "fi" => {
                self.term.break_line();
                self.no_fill = false;
            }
            "RS" => {
                let extra = macro_args(args)
                    .first()
                    .and_then(|n| n.parse::<usize>().ok())
                    .unwrap_or(self.indent);
                self.term.push_indent(extra);
                self.base_stack.push(self.para_base);
                self.para_base = self.term.offset();
            }
            "RE" => {
                self.term.pop_indent();
                self.para_base = self.base_stack.pop().unwrap_or(self.indent);
            }
            "TP" => {
                let extra = macro_args(args)
                    .first()
                    .and_then(|n| n.parse::<usize>().ok())
                    .unwrap_or(self.indent);
                // Relative to the paragraph margin, not to wherever the previous
                // tag's body left the margin.
                self.term.set_offset(self.para_base);
                self.tp_body_indent = Some(self.para_base + extra);
            }
            "IP" => self.do_ip(args),
            "HP" => self.term.blank_line(),
            "B" | "I" | "SM" | "SB" => {
                let marker = if name == "I" { STYLE_UL } else { STYLE_BOLD };
                let marker = (name != "SM").then_some(marker);
                // Each argument becomes its own filled word. Joining them into a
                // single styled run made the whole thing unbreakable, so a
                // one-line `.B` synopsis ran off the terminal — cifsiostat(1) has
                // a 100-column one.
                let words: Vec<String> = macro_args(args)
                    .iter()
                    .map(|a| self.style_word(a, marker))
                    .collect();
                self.emit_inline_words(words);
            }
            "BR" | "RB" | "BI" | "IB" | "IR" | "RI" => {
                // Alternating macros concatenate their arguments with no
                // separator, so the result is genuinely one word.
                let text = self.format_alternating(name, &macro_args(args));
                self.emit_inline_words(vec![text]);
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
        self.term.set_header(&left, &manual, &left);
        self.term.set_footer(&source, &date, &source);
    }

    fn do_heading(&mut self, args: &str, sub: bool) {
        // Reset indentation at each new section.
        self.tp_body_indent = None;
        self.para_base = self.indent;
        self.base_stack.clear();
        let title = macro_args(args).join(" ");
        let title = self.resolve(&title);
        self.term.reset_indent(self.indent);
        self.term.blank_line();
        let indent = if sub { self.indent / 2 } else { 0 };
        self.term
            .emit(indent, wrap_marker(&title, STYLE_BOLD, self.styling));
    }

    fn do_ip(&mut self, args: &str) {
        let a = macro_args(args);
        let tag = a.first().cloned();
        let extra = a
            .get(1)
            .and_then(|n| n.parse::<usize>().ok())
            .unwrap_or(self.indent);
        // As for `.TP`: relative to the paragraph margin, and filled rather than
        // emitted verbatim. pod2man writes `.IP` tags that list every option a
        // paragraph covers; openssl-s_client(1) has one 490 columns long.
        let body_indent = self.para_base + extra;
        if let Some(tag) = tag {
            let tag = self.resolve(&tag);
            self.term.set_offset(self.para_base);
            for word in tag.split_whitespace() {
                self.term.word(word);
            }
            self.term.break_line();
        }
        self.term.set_offset(body_indent);
    }

    /// A plain text line: in fill mode add its words to the paragraph, else emit
    /// it verbatim at the current indent.
    fn text_line(&mut self, raw: &str) {
        if let Some(body_indent) = self.tp_body_indent.take() {
            // The line after `.TP` is the tag; the following text is the body.
            // Fill it rather than emitting it verbatim: groff fills a plain-text
            // tag, and pages do use long ones — nvidia-smi(1) has a `.TP` whose
            // tag is a full 490-column paragraph, which ran straight off the
            // terminal.
            for word in raw.split_whitespace() {
                self.term.word(self.resolve(word));
            }
            self.term.break_line();
            self.term.set_offset(body_indent);
            return;
        }

        if self.no_fill {
            let resolved = self.resolve(raw);
            let off = self.term.offset();
            self.term.emit(off, resolved);
            return;
        }

        if raw.trim().is_empty() {
            self.term.blank_line();
            return;
        }

        for word in raw.split_whitespace() {
            self.term.word(self.resolve(word));
        }
    }

    fn emit_inline_words(&mut self, words: Vec<String>) {
        // A `.TP` tag may itself be a font macro (e.g. `.B 0`); capture it as the
        // tag line and switch to the body indent.
        if let Some(body_indent) = self.tp_body_indent.take() {
            self.term.set_offset(self.para_base);
            for w in words {
                self.term.word(w);
            }
            self.term.break_line();
            self.term.set_offset(body_indent);
            return;
        }

        if self.no_fill {
            let off = self.term.offset();
            self.term.emit(off, words.join(" "));
        } else {
            for w in words {
                self.term.word(w);
            }
        }
    }

    /// Resolve one argument's escapes and wrap it in `marker`, if any.
    fn style_word(&self, arg: &str, marker: Option<char>) -> String {
        let text = self.resolve(arg);
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
        replace_escapes(s)
    }
}

fn wrap_marker(text: &str, marker: char, styling: bool) -> String {
    if styling {
        format!("{marker}{text}{STYLE_RESET}")
    } else {
        text.to_string()
    }
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
        String::from_utf8(format_man7(content, &SETTINGS).unwrap()).unwrap()
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
        assert!(format_man7(".TH T 1\n", &SETTINGS).is_none());
        assert!(format_man7(".TH T 1\n.SH NAME\nx\n", &SETTINGS).is_some());
    }

    #[test]
    fn styling_emits_overstrike() {
        let styled = FormattingSettings {
            styling: true,
            ..SETTINGS
        };
        let out =
            String::from_utf8(format_man7(".TH T 1\n.SH D\n.B bold\n", &styled).unwrap()).unwrap();
        assert!(out.contains("b\u{8}bo\u{8}ol\u{8}ld\u{8}d"), "{out:?}");
    }

    /// Longest rendered line, ignoring the header/footer (which are exactly the
    /// terminal width by construction) and nroff overstrike (a display
    /// attribute, not a column).
    fn longest_body_line(out: &str) -> usize {
        let plain: String = {
            let mut r = String::new();
            let mut it = out.chars().peekable();
            while let Some(c) = it.next() {
                if it.peek() == Some(&'\u{8}') {
                    it.next();
                    continue;
                }
                r.push(c);
            }
            r
        };
        plain
            .lines()
            .skip(2)
            .filter(|l| !l.contains("Manual"))
            .map(|l| l.chars().count())
            .max()
            .unwrap_or(0)
    }

    #[test]
    fn preamble_before_th_is_still_man7() {
        // Looking only at the *first* macro misclassified any page with a roff
        // preamble as mdoc; the mdoc parser knows none of these macros, so the
        // page's raw source was printed with exit status 0. This shape covers
        // 43 of the 300 pages in /usr/share/man/man1 on a stock system.
        assert!(is_man7(".nh\n.TH LS 1\n.SH NAME\n"));
        assert!(is_man7(".pc\n.TH APROPOS 1\n"));
        assert!(is_man7(".ds V 1.0\n.nr X 1\n.TH T 1\n"));
        assert!(is_man7("free text first\n.TH T 1\n"));
        // A preamble in front of mdoc still resolves to mdoc.
        assert!(!is_man7(".nh\n.Dd July 4, 2024\n.Dt T 1\n"));
    }

    #[test]
    fn consecutive_tp_tags_do_not_nest() {
        // Each `.TP` body indent was computed from the current margin, which for
        // a run of tags is the *previous* tag's body indent — so every entry in
        // an OPTIONS section sat one step further right than the last. Real
        // pages have dozens of them; ls(1) reached 396 columns at width 78.
        let mut src = String::from(".TH T 1\n.SH OPTIONS\n");
        for i in 0..40 {
            src.push_str(&format!(
                ".TP\n\\-\\-option{i}\nDescription of option {i}.\n"
            ));
        }
        let out = render(&src);
        assert!(
            longest_body_line(&out) <= SETTINGS.width,
            "line exceeds terminal width ({}):\n{}",
            longest_body_line(&out),
            out
        );
    }

    #[test]
    fn tp_containing_rs_re_does_not_ratchet_the_margin() {
        // `.RE` restores the margin that was current at `.RS` — inside a `.TP`
        // body, that is the tag's body indent, not the paragraph margin. Reading
        // the paragraph margin back from it therefore moved the base one step
        // right per tag. screen(1) uses this shape ~200 times and ended up 880
        // columns in.
        let mut src = String::from(".TH T 1\n.SH OPTIONS\n");
        for i in 0..40 {
            src.push_str(&format!(
                ".TP\n\\-\\-opt{i}\nBody.\n.RS\nNested paragraph.\n.RE\n"
            ));
        }
        let out = render(&src);
        assert!(
            longest_body_line(&out) <= SETTINGS.width,
            "line exceeds terminal width ({}):\n{}",
            longest_body_line(&out),
            out
        );
    }

    #[test]
    fn pp_returns_to_the_paragraph_margin() {
        let out = render(
            ".TH T 1\n.SH DESCRIPTION\n\
             .TP\n\\-x\nTagged body text.\n\
             .PP\nPlain paragraph after the tag.\n",
        );
        let base = " ".repeat(SETTINGS.indent);
        let para = out
            .lines()
            .find(|l| l.contains("Plain paragraph"))
            .expect("paragraph present");
        assert_eq!(
            para,
            format!("{base}Plain paragraph after the tag."),
            "`.PP` did not return to the paragraph margin:\n{out}"
        );
    }

    #[test]
    fn long_tag_is_filled_not_emitted_verbatim() {
        // groff fills a plain-text tag. pod2man writes `.IP` tags listing every
        // option a paragraph covers; openssl-s_client(1) has one 490 columns
        // long, and nvidia-smi(1) a `.TP` tag of similar length.
        let long: String = (0..40).map(|i| format!("word{i} ")).collect();
        let out = render(&format!(".TH T 1\n.SH OPTIONS\n.TP\n{long}\nBody.\n"));
        assert!(longest_body_line(&out) <= SETTINGS.width, "{out}");
        let out = render(&format!(".TH T 1\n.SH OPTIONS\n.IP \"{long}\" 4\nBody.\n"));
        assert!(longest_body_line(&out) <= SETTINGS.width, "{out}");
    }

    #[test]
    fn bold_macro_arguments_fill_individually() {
        // `.B` joined its arguments into one styled run, which the filler treats
        // as a single unbreakable word: cifsiostat(1) has a one-line `.B`
        // synopsis 100 columns wide.
        let long: String = (0..40).map(|i| format!("arg{i} ")).collect();
        let out = render(&format!(".TH T 1\n.SH SYNOPSIS\n.B {long}\n"));
        assert!(longest_body_line(&out) <= SETTINGS.width, "{out}");
    }

    #[test]
    fn wide_characters_wrap_at_the_terminal_width() {
        // A CJK line counted by scalars fits twice as many characters as the
        // terminal has cells, so it wrapped at 2x the width.
        let cjk: String = "日本語のマニュアルページ ".repeat(20);
        let out = render(&format!(".TH T 1\n.SH DESCRIPTION\n{cjk}\n"));
        assert!(
            longest_body_line_cells(&out) <= SETTINGS.width,
            "wide text exceeded the terminal width ({}):\n{}",
            longest_body_line_cells(&out),
            out
        );
    }

    /// As `longest_body_line`, but measuring terminal cells.
    ///
    /// Measured independently of `display_width` — the function under test — so
    /// that a renderer measuring scalars cannot agree with a check that also
    /// measures scalars.
    fn longest_body_line_cells(out: &str) -> usize {
        use unicode_width::UnicodeWidthStr;
        out.lines()
            .skip(2)
            .filter(|l| !l.contains("Manual"))
            .map(UnicodeWidthStr::width)
            .max()
            .unwrap_or(0)
    }
}
