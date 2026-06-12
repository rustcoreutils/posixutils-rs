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

/// Join lines, collapsing runs of more than one blank line into a single blank.
pub fn collapse_blank_lines(lines: &[String]) -> String {
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
