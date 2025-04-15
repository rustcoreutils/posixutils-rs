//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::{FORM_FEED, Line};
use std::collections::VecDeque;
use std::io::{self, BufRead, BufReader, Read};

/// Iterator that splits the stream on `\n` or form-feed.
pub struct LineBreakIterator {
    reader: BufReader<Box<dyn Read>>,
    buf: Vec<u8>,
    lines: VecDeque<Line>,
}

impl LineBreakIterator {
    pub fn new(stream: Box<dyn Read>) -> Self {
        Self {
            reader: io::BufReader::new(stream),
            buf: Vec::new(),
            lines: VecDeque::new(),
        }
    }

    fn split_on_line_breaks(&mut self) -> io::Result<usize> {
        // Both coreutils and uutils seem to assume UTF-8. coreutils in
        // particular, assume an encoding where ' ' is 1 byte.
        fn buffer_to_string(buffer: Vec<u8>) -> io::Result<String> {
            String::from_utf8(buffer).map_err(|e| io::Error::other(format!("{e}")))
        }

        self.buf.clear();
        let num_bytes_read = self.reader.read_until(b'\n', &mut self.buf)?;
        if self.buf.ends_with(b"\n") {
            self.buf.pop();

            if self.buf.ends_with(b"\r") {
                self.buf.pop();
            }
        }

        if self.buf.is_empty() && num_bytes_read > 0 {
            self.lines.push_back(Line {
                line: String::new(),
                ends_on_form_feed: false,
                is_padding: false,
            });
        } else {
            let mut cursor = io::Cursor::new(self.buf.as_slice());
            loop {
                let mut buffer = Vec::new();

                if cursor.read_until(FORM_FEED as u8, &mut buffer)? == 0 {
                    break;
                }

                if buffer.ends_with(&[FORM_FEED as u8]) {
                    buffer.pop();
                    self.lines.push_back(Line {
                        line: buffer_to_string(buffer)?,
                        ends_on_form_feed: true,
                        is_padding: false,
                    });
                } else {
                    self.lines.push_back(Line {
                        line: buffer_to_string(buffer)?,
                        ends_on_form_feed: false,
                        is_padding: false,
                    });
                }
            }
        }

        Ok(num_bytes_read)
    }
}

impl Iterator for LineBreakIterator {
    type Item = io::Result<Line>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(line) = self.lines.pop_front() {
                return Some(Ok(line));
            }

            match self.split_on_line_breaks() {
                Ok(bytes) => {
                    // EOF
                    if bytes == 0 {
                        return None;
                    }
                }
                Err(e) => return Some(Err(e)),
            }
        }
    }
}

#[test]
fn test_pr_line_break_iterator() {
    let data: &[(&str, Vec<&str>)] = &[
        ("a\nb\nc", vec!["a", "b", "c"]),
        ("a\nb\nc\n", vec!["a", "b", "c"]),
        ("\n\nc", vec!["", "", "c"]),
        ("a\u{c}b\u{c}c", vec!["a", "b", "c"]),
        ("a\u{c}b\u{c}c\u{c}", vec!["a", "b", "c"]),
        ("\u{c}\u{c}c", vec!["", "", "c"]),
    ];

    for (s, out) in data {
        let cursor = io::Cursor::new(s.as_bytes());
        let it = LineBreakIterator::new(Box::new(cursor));

        let res: Vec<_> = it.map(|line| line.unwrap().line).collect();

        assert_eq!(res.len(), out.len());
        for (a, b) in res.iter().zip(out.iter()) {
            assert_eq!(a, b);
        }
    }

    let data: &[(&str, Vec<(&str, bool)>)] = &[
        (
            "a\u{c}b\nc\n",
            vec![("a", true), ("b", false), ("c", false)],
        ),
        (
            "a\nb\u{c}c\n",
            vec![("a", false), ("b", true), ("c", false)],
        ),
        (
            "a\nb\nc\u{c}",
            vec![("a", false), ("b", false), ("c", true)],
        ),
    ];

    for (s, out) in data {
        let cursor = io::Cursor::new(s.as_bytes());
        let it = LineBreakIterator::new(Box::new(cursor));

        let res: Vec<_> = it
            .map(|line| {
                let line = line.unwrap();
                (line.line, line.ends_on_form_feed)
            })
            .collect();

        assert_eq!(res.len(), out.len());
        for (a, b) in res.iter().zip(out.iter()) {
            assert_eq!(a.0, b.0);
            assert_eq!(a.1, b.1);
        }
    }
}
