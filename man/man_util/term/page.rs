//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Page-level assembly helpers shared by the term backend: three-part
//! header/footer composition and blank-line collapsing. These operate on
//! terminal cells (character counts), not bytes, so multibyte titles align.

/// Compose a three-part line: `left` flush-left, `center` centered, `right`
/// flush-right, within `width` columns. Widths are counted in characters.
pub fn three_part(left: &str, center: &str, right: &str, width: usize) -> String {
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

/// Collapse runs of blank lines. A run longer than one blank collapses to a
/// `delimiter_size`-newline gap; a single blank stays a single blank. Whitespace-
/// only lines count as blank. (Ported verbatim from the mdoc formatter so both
/// renderers share one blank-line policy.)
pub fn remove_empty_lines(input: &str, delimiter_size: usize) -> String {
    let input = input
        .lines()
        .map(|line| {
            if line.chars().all(|ch| ch.is_whitespace()) {
                ""
            } else {
                line
            }
        })
        .collect::<Vec<_>>()
        .join("\n");
    let mut result = String::with_capacity(input.len());
    let mut iter = input.chars().peekable();
    let lines_delimiter_big = "\n".repeat(delimiter_size);
    let mut nl_count = 0;

    while let Some(current_char) = iter.next() {
        if current_char == '\n' {
            if iter.peek() != Some(&'\n') {
                let lines_delimiter = if nl_count > 1 {
                    &lines_delimiter_big.clone()
                } else {
                    "\n"
                };
                result.push_str(lines_delimiter);
                nl_count = 1;
            } else {
                nl_count += 1;
            }
        } else {
            result.push(current_char);
        }
    }

    result
}
