//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The text `-E` writes: the preprocessed tokens, with the line markers and
// line breaks gcc's `-E` writes around them.
//

use std::io::{self, Write};

use super::lexer::{
    escape_c_string, indentation, show_token, spellings_merge, write_token, FileChange, LexerMode,
    Token, TokenType, TokenValue,
};
use super::preprocess::{marker_file_name, pragma_text, LayoutPragma};
use crate::diag::{self, Position};
use crate::strings::StringTable;

/// How the text is laid out.
#[derive(Debug, Clone, Copy)]
pub struct Layout {
    /// Line markers are written; `-P` says not.
    pub markers: bool,
    /// What the source is written in. Assembly gets the markers of a change
    /// of file but none of C's mid-line ones: gcc's `-lang-asm` puts no token
    /// back on its own line, and marks no change of system-ness between two
    /// tokens.
    pub language: LexerMode,
    /// The source is a `.i`, whose own line markers, reproduced from its
    /// text, say what it holds: as gcc's `-fpreprocessed`, none of the
    /// source's own name and pseudo-files are written before them.
    pub preprocessed: bool,
}

/// The macros in force before the source, which `-dD` writes first, each
/// where gcc says it came from.
#[derive(Debug, Clone, Copy, Default)]
pub struct InitialDefinitions<'a> {
    /// The compiler's own, from `<built-in>`.
    pub builtin: &'a [String],
    /// The command line's `-D` and `-U`, from `<command-line>`.
    pub cmdline: &'a [String],
}

/// Where a line marker places text: the file it names, the line, and
/// whether the file is a system header.
struct Place {
    file: String,
    line: u32,
    system: bool,
}

impl Place {
    /// Where `pos` is, as the reader of the output is told: after any
    /// `#line`, with a bundled header under gcc's name for compiler text.
    fn of(pos: Position) -> Place {
        let (name, line, _) = diag::effective_position(pos);
        Place {
            file: marker_file_name(&name).to_string(),
            line,
            system: diag::stream_is_system(pos.stream),
        }
    }

    /// Line 0 of one of gcc's pseudo-files, `<built-in>` or
    /// `<command-line>`.
    fn pseudo(file: &str) -> Place {
        Place {
            file: file.to_string(),
            line: 0,
            system: false,
        }
    }
}

/// gcc's `-E` writer (`c-ppoutput.cc`), state and all: where the output
/// stands, so that it knows when a gap in the source is a few blank lines and
/// when it takes a marker.
struct Printer<'w> {
    out: &'w mut dyn Write,
    layout: Layout,
    /// The source line the output line being written stands for.
    src_line: u32,
    /// The file the last marker named.
    src_file: String,
    /// Something has been written on the current output line.
    printed: bool,
    /// The last token written came from a system header.
    prev_was_system: bool,
}

/// Up to seven lines of gap are written as blank lines, as gcc writes them;
/// eight or more take a marker.
const MAX_BLANK_RUN: u32 = 8;

impl<'w> Printer<'w> {
    fn new(out: &'w mut dyn Write, layout: Layout) -> Self {
        Printer {
            out,
            layout,
            src_line: 0,
            src_file: String::new(),
            printed: false,
            prev_was_system: false,
        }
    }

    /// C's tokens are put back on their own lines; assembly's are not.
    fn adjusts_lines(&self) -> bool {
        self.layout.markers && self.layout.language == LexerMode::C
    }

    /// Whether `left` written directly before `right` would lex back as
    /// something else. In assembly only two words run together: `$` and `%`
    /// begin no identifier there, so `$1` and `%eax` are already two tokens.
    fn would_merge(&self, left: &str, right: &str) -> bool {
        match self.layout.language {
            LexerMode::C => spellings_merge(left, right),
            LexerMode::Assembly => {
                let word =
                    |c: Option<char>| c.is_some_and(|c| c.is_ascii_alphanumeric() || c == '_');
                word(left.chars().last()) && word(right.chars().next())
            }
        }
    }

    fn newline(&mut self) -> io::Result<()> {
        self.out.write_all(b"\n")
    }

    /// End the line in progress, and write a marker for `place` (gcc's
    /// `print_line`). `flags` is the marker's change of file: ` 1` entering,
    /// ` 2` returning, or nothing.
    fn print_line(&mut self, place: &Place, flags: &str) -> io::Result<()> {
        if self.printed {
            self.newline()?;
        }
        self.printed = false;
        if !self.layout.markers {
            return Ok(());
        }
        self.src_line = place.line;
        self.src_file.clone_from(&place.file);
        let system = if place.system { " 3 4" } else { "" };
        writeln!(
            self.out,
            "# {} \"{}\"{flags}{system}",
            place.line,
            escape_c_string(&place.file)
        )
    }

    /// Bring the output to `place`'s line (gcc's `maybe_print_line`): end the
    /// line in progress, then write blank lines up to it when it is a few
    /// lines on in the same file, and a marker otherwise. Whether a marker
    /// was written.
    fn maybe_print_line(&mut self, place: &Place) -> io::Result<bool> {
        if self.printed {
            self.newline()?;
            self.src_line += 1;
            self.printed = false;
        }
        if self.layout.markers
            && place.line >= self.src_line
            && place.line < self.src_line + MAX_BLANK_RUN
            && place.file == self.src_file
        {
            while place.line > self.src_line {
                self.newline()?;
                self.src_line += 1;
            }
            return Ok(false);
        }
        self.print_line(place, "")?;
        Ok(self.layout.markers)
    }

    /// Start a fresh output line for a token at `place` (gcc's
    /// `do_line_change`), indented by `spaces`. Whether a marker was written.
    fn line_change(&mut self, place: &Place, spaces: usize) -> io::Result<bool> {
        let marker = self.maybe_print_line(place)?;
        self.printed = true;
        self.out.write_all(&b" ".repeat(spaces))?;
        Ok(marker)
    }

    /// A line of text standing for a directive -- a `#pragma`, or a
    /// `#define` that `-dD` carries -- at `place`, on its own line.
    fn directive_line(&mut self, place: &Place, text: &str) -> io::Result<()> {
        self.maybe_print_line(place)?;
        writeln!(self.out, "{}", text.trim_end())?;
        self.printed = false;
        self.src_line += 1;
        Ok(())
    }

    /// What gcc writes before the source: the source's name, then each
    /// pseudo-file with what `-dD` says came from it.
    fn prologue(&mut self, main: &str, defs: InitialDefinitions) -> io::Result<()> {
        self.print_line(&Place::pseudo(main), "")?;
        for (file, lines) in [
            ("<built-in>", defs.builtin),
            ("<command-line>", defs.cmdline),
        ] {
            let place = Place::pseudo(file);
            self.print_line(&place, "")?;
            for line in lines {
                self.directive_line(&place, line)?;
            }
        }
        Ok(())
    }

    fn file_change(&mut self, token: &Token, change: FileChange) -> io::Result<()> {
        // gcc's `-P` writes nothing at all for a change of file, not even
        // the end of the line in progress.
        if !self.layout.markers {
            return Ok(());
        }
        match change {
            FileChange::Enter { stream } => {
                self.maybe_print_line(&Place::of(token.pos))?;
                self.print_line(&Place::of(Position::new(stream, 1, 1)), " 1")
            }
            FileChange::Return => self.print_line(&Place::of(token.pos), " 2"),
            FileChange::Rename => self.print_line(&Place::of(token.pos), ""),
        }
    }
}

/// The token last written, as the next one's spacing asks about it.
struct Written {
    /// Its spelling, which the next one must not lex back into.
    text: String,
    /// The stream it came from.
    stream: u16,
    /// The source line it ends on.
    end_line: u32,
    /// It is a comment `-C` kept.
    comment: bool,
}

/// The text of a token the parser was handed in place of a directive: a
/// `#pragma`, or under `-dD` a `#define` or `#undef`. `None` when the token
/// is no such thing.
fn directive_text(token: &Token) -> Option<String> {
    if token.typ != TokenType::Pragma {
        return None;
    }
    // `pack` and `scalar_storage_order` travel decoded, because the parser
    // acts on them; every other pragma travels as its own text. Dropping
    // the second kind is what made `c17 -E` keep one pragma line out of
    // five, so an `-E`/compile split silently meant something different
    // from compiling in one step.
    match LayoutPragma::from_token(token) {
        Some(action) => Some(action.to_pragma_text()),
        None => pragma_text(token),
    }
}

/// Write `tokens`, the preprocessed form of the source named `main`, as
/// gcc's `-E` writes it.
///
/// The markers are gcc's, one for one: the source and the pseudo-files
/// `<built-in>` and `<command-line>` at line 0 first; each change of file
/// with gcc's flags (1 entering, 2 returning, 3 4 a system header); a gap of
/// eight or more lines; and, in C, a token put back on its own line, or
/// written after a token of a header of the other kind, system or not.
///
/// A token is written in its column at the start of a line, and with a
/// space before it where the source had one or where the two would lex
/// back as one. A comment `-C` keeps may run over lines, which the count
/// follows.
pub fn write_preprocessed(
    out: &mut dyn Write,
    tokens: &[Token],
    strings: &StringTable,
    main: &str,
    defs: InitialDefinitions,
    layout: Layout,
) -> io::Result<()> {
    let mut p = Printer::new(out, layout);
    if !layout.preprocessed {
        p.prologue(main, defs)?;
    }
    // The last token written, for the space and the line break before the
    // next.
    let mut prev: Option<Written> = None;
    let mut spelling = Vec::new();

    for token in tokens {
        match (&token.typ, &token.value) {
            (TokenType::StreamBegin | TokenType::StreamEnd, _) => continue,
            (TokenType::FileChange, TokenValue::FileChange(change)) => {
                p.file_change(token, *change)?;
                prev = None;
                continue;
            }
            (TokenType::Pragma, _) => {
                if let Some(text) = directive_text(token) {
                    p.directive_line(&Place::of(token.pos), &text)?;
                    prev = None;
                }
                continue;
            }
            _ => {}
        }
        let text = show_token(token, strings);
        // The preprocessor's internal markers are not text.
        if text.starts_with("<ident") || text.starts_with("<special") {
            continue;
        }
        let place = Place::of(token.pos);
        // A token after a comment that started its line inherits the start
        // of line, though it may stand on the line the comment ends on.
        let beside_comment = prev
            .as_ref()
            .is_some_and(|w| w.comment && w.stream == token.pos.stream && w.end_line == place.line);
        let starts_line = token.pos.newline && !beside_comment;
        let mut marker = false;
        if starts_line {
            p.line_change(&place, indentation(token.pos))?;
        } else {
            // From another line than the one being written, without a
            // newline before it: after a macro invocation whose arguments ran
            // on, the first token of an expansion after one, or the text
            // after a `_Pragma`. gcc puts it back on its own line.
            let later = prev
                .as_ref()
                .is_some_and(|w| w.stream == token.pos.stream && place.line > p.src_line);
            let elsewhere = token.pos.whitespace && place.line != p.src_line;
            if p.adjusts_lines() && (later || elsewhere) {
                let spaces = indentation(token.pos).saturating_sub(1);
                marker = p.line_change(&place, spaces)?;
                p.out.write_all(b" ")?;
            } else if token.pos.whitespace
                || prev.as_ref().is_some_and(|w| p.would_merge(&w.text, &text))
            {
                p.out.write_all(b" ")?;
            }
        }
        if p.adjusts_lines() && !marker && p.prev_was_system != place.system {
            // gcc marks where text from a system header meets text from
            // elsewhere, so a reader can tell which tokens' warnings to keep.
            p.line_change(&place, indentation(token.pos).saturating_sub(1))?;
            p.prev_was_system = place.system;
        }
        // Byte for byte: a literal's payload holds one `char` per source
        // byte, so writing `text` would re-encode every byte >= 0x80 as two.
        spelling.clear();
        write_token(&mut spelling, token, strings);
        p.out.write_all(&spelling)?;
        p.printed = true;
        let comment = token.typ == TokenType::Comment;
        let lines = text.matches('\n').count() as u32;
        if comment {
            p.src_line += lines;
        }
        prev = Some(Written {
            text,
            stream: token.pos.stream,
            end_line: place.line + lines,
            comment,
        });
    }
    if p.printed {
        p.newline()?;
    }
    p.out.flush()
}

#[cfg(test)]
#[path = "test_ppoutput.rs"]
mod tests;
