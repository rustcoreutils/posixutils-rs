//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Program output, wrapped to bc's line width.
//!
//! POSIX (XCU bc): "Very large numbers shall be split across lines with 70
//! characters per line ... Lines that are continued shall end with a
//! `<backslash>`." The column is a property of the output stream, not of any
//! one value: a number printed after a string continues that string's line, so
//! wrapping cannot be done by each value in isolation.

use std::io::{self, Write};

/// Characters of content per line before a continuation. The backslash makes
/// the line 69 columns, within the 70 POSIX allows, and matches GNU bc
/// byte for byte.
const LINE_WIDTH: usize = 68;

pub struct OutputWriter<'a> {
    sink: &'a mut dyn Write,
    column: usize,
}

impl<'a> OutputWriter<'a> {
    pub fn new(sink: &'a mut dyn Write) -> Self {
        OutputWriter { sink, column: 0 }
    }

    /// Write text, breaking it across lines as bc does. A newline in the text
    /// ends the line and resets the column.
    pub fn write_text(&mut self, text: &str) -> io::Result<()> {
        for (i, line) in text.split('\n').enumerate() {
            if i > 0 {
                self.sink.write_all(b"\n")?;
                self.column = 0;
            }
            self.write_wrapped(line)?;
        }
        Ok(())
    }

    fn write_wrapped(&mut self, mut text: &str) -> io::Result<()> {
        while !text.is_empty() {
            if self.column >= LINE_WIDTH {
                self.sink.write_all(b"\\\n")?;
                self.column = 0;
            }
            let room = LINE_WIDTH - self.column;
            let end = match text.char_indices().nth(room) {
                Some((offset, _)) => offset,
                None => text.len(),
            };
            let chunk = &text[..end];
            self.sink.write_all(chunk.as_bytes())?;
            self.column += chunk.chars().count();
            text = &text[end..];
        }
        Ok(())
    }

    pub fn flush(&mut self) -> io::Result<()> {
        self.sink.flush()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn written(chunks: &[&str]) -> String {
        let mut buffer = Vec::new();
        {
            let mut out = OutputWriter::new(&mut buffer);
            for chunk in chunks {
                out.write_text(chunk).unwrap();
            }
        }
        String::from_utf8(buffer).unwrap()
    }

    #[test]
    fn short_output_is_not_wrapped() {
        assert_eq!(written(&["123\n"]), "123\n");
    }

    #[test]
    fn long_output_breaks_at_the_line_width() {
        let digits = "1".repeat(73);
        let text = written(&[&digits, "\n"]);
        let lines: Vec<&str> = text.split('\n').collect();
        assert_eq!(lines[0].len(), 69);
        assert!(lines[0].ends_with('\\'));
        assert_eq!(lines[1].len(), 5);
        assert_eq!(text.replace("\\\n", ""), format!("{}\n", digits));
    }

    #[test]
    fn exactly_the_line_width_is_not_continued() {
        let digits = "1".repeat(68);
        assert_eq!(written(&[&digits, "\n"]), format!("{}\n", digits));
    }

    #[test]
    fn the_column_carries_across_writes() {
        // A number printed after a string continues that string's line, so the
        // break falls two characters earlier.
        let text = written(&["ab", &"1".repeat(73), "\n"]);
        let first = text.split('\n').next().unwrap();
        assert_eq!(first.len(), 69);
        assert!(first.starts_with("ab1"));
        assert_eq!(text.replace("\\\n", ""), format!("ab{}\n", "1".repeat(73)));
    }

    #[test]
    fn a_newline_resets_the_column() {
        let text = written(&["ab\n", &"1".repeat(68), "\n"]);
        assert_eq!(text, format!("ab\n{}\n", "1".repeat(68)));
    }

    #[test]
    fn multibyte_text_is_measured_in_characters() {
        // Strings may hold any characters; the column counts them, not bytes.
        let text = written(&["é".repeat(70).as_str(), "\n"]);
        let first = text.split('\n').next().unwrap();
        assert_eq!(first.chars().count(), 69);
        assert_eq!(text.replace("\\\n", ""), format!("{}\n", "é".repeat(70)));
    }
}
