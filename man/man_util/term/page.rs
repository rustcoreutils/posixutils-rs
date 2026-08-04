//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Page-level assembly helpers shared by the term backend: three-part
//! header/footer composition and blank-line collapsing. These operate on
//! terminal cells, not bytes or scalars, so multibyte and East Asian wide
//! titles align.

use super::style::display_width;

/// Compose a three-part line: `left` flush-left, `center` centered, `right`
/// flush-right, within `width` columns. Widths are counted in terminal cells.
pub fn three_part(left: &str, center: &str, right: &str, width: usize) -> String {
    // Track the width as the line is built rather than re-measuring it for every
    // space appended: `display_width` scans the whole string, so padding one
    // column at a time made this quadratic in the header's length.
    let mut line = String::from(left);
    let mut cells = display_width(left);

    let center_start = width.saturating_sub(display_width(center)) / 2;
    let pad = center_start.saturating_sub(cells);
    line.push_str(&" ".repeat(pad));
    line.push_str(center);
    cells += pad + display_width(center);

    let right_start = width.saturating_sub(display_width(right));
    line.push_str(&" ".repeat(right_start.saturating_sub(cells)));
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
                    lines_delimiter_big.as_str()
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

#[cfg(test)]
mod tests {
    use super::three_part;
    use crate::man_util::term::style::display_width;

    /// The original padding loop, kept as an oracle: the incremental version
    /// must agree with it on every input, including ones where a part is wider
    /// than the space allotted to it.
    fn reference(left: &str, center: &str, right: &str, width: usize) -> String {
        let mut line = String::from(left);
        let center_start = width.saturating_sub(display_width(center)) / 2;
        while display_width(&line) < center_start {
            line.push(' ');
        }
        line.push_str(center);
        let right_start = width.saturating_sub(display_width(right));
        while display_width(&line) < right_start {
            line.push(' ');
        }
        line.push_str(right);
        line
    }

    #[test]
    fn matches_the_padding_loop_it_replaced() {
        let parts = [
            "",
            "LS(1)",
            "General Commands Manual",
            "日本語のマニュアル",
            "e\u{301}cole",
            "a very long left part that overruns its own field",
        ];
        for l in parts {
            for c in parts {
                for r in parts {
                    for width in [0, 1, 20, 78, 80, 200] {
                        assert_eq!(
                            three_part(l, c, r, width),
                            reference(l, c, r, width),
                            "l={l:?} c={c:?} r={r:?} width={width}"
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn lays_out_a_normal_header() {
        let out = three_part("LS(1)", "General Commands Manual", "LS(1)", 78);
        assert_eq!(display_width(&out), 78);
        assert!(out.starts_with("LS(1)"));
        assert!(out.ends_with("LS(1)"));
    }
}
