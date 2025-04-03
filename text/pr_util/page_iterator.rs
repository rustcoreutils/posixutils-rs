//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::Read;

use super::LineBreakIterator;
use crate::{Line, Page};

/// Iterator over whole pages.
pub struct PageIterator {
    line_iterator: LineBreakIterator,
    body_lines_per_page: usize,
}

impl PageIterator {
    pub fn new(stream: Box<dyn Read>, body_lines_per_page: usize) -> Self {
        Self {
            line_iterator: LineBreakIterator::new(stream),
            body_lines_per_page,
        }
    }
}

impl Iterator for PageIterator {
    type Item = Page;

    fn next(&mut self) -> Option<Self::Item> {
        let mut lines = Vec::new();

        // Builds and returns a `Page` with *exactly* `self.body_lines_per_page`
        // lines.
        loop {
            match self.line_iterator.next() {
                Some(line) => {
                    let mut ends_in_form_feed = false;
                    if let Ok(line) = &line {
                        ends_in_form_feed = line.ends_on_form_feed;
                    }
                    lines.push(line);
                    if lines.len() == self.body_lines_per_page || ends_in_form_feed {
                        return Some(Page {
                            lines,
                            num_nonpadding_lines: self.body_lines_per_page,
                        });
                    }
                }
                None => {
                    return if lines.is_empty() {
                        None
                    } else {
                        let num_nonpadding_lines = lines.len();
                        // Fill the remaining with blank lines
                        while lines.len() < self.body_lines_per_page {
                            lines.push(Ok(Line {
                                line: String::new(),
                                ends_on_form_feed: false,
                                is_padding: true,
                            }))
                        }
                        Some(Page {
                            lines,
                            num_nonpadding_lines,
                        })
                    };
                }
            }
        }
    }
}

#[test]
fn test_pr_line_break_iterator() {
    let body_lines_per_page = 56;
    let line_counts = [(55, 1), (56, 1), (57, 2)];
    for (i, num_pages) in line_counts {
        let s = "\n".repeat(i);
        let cursor = std::io::Cursor::new(s.into_bytes());
        let it = PageIterator::new(Box::new(cursor), body_lines_per_page);
        assert_eq!(it.count(), num_pages);
    }
}
