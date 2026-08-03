//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Emphasis layer for the term backend: zero-width style markers, character-count
//! width measurement (ignoring style markers), and resolution into nroff
//! backspace-overstrike. The width count is per Unicode scalar, not true terminal
//! cells, so wide/combining characters are not specially weighted.

// Private, zero-width style markers inserted during formatting and resolved by
// `apply_styling` into nroff backspace-overstrike (or stripped). These control
// characters never occur in manual-page text.
pub(crate) const STYLE_BOLD: char = '\u{1}';
pub(crate) const STYLE_UL: char = '\u{2}';
pub(crate) const STYLE_RESET: char = '\u{3}';

fn is_style_marker(c: char) -> bool {
    matches!(c, STYLE_BOLD | STYLE_UL | STYLE_RESET)
}

/// Terminal-cell width of `s`, ignoring the zero-width style markers.
/// (Overstrike is applied only after wrapping, so no backspaces are present
/// here.)
///
/// This is real cell width, not a scalar count: East Asian wide and fullwidth
/// characters occupy two cells and combining marks occupy none, so a CJK line
/// counted by `chars().count()` wrapped at half the terminal width and a line
/// with combining accents wrapped early.
pub(crate) fn display_width(s: &str) -> usize {
    use unicode_width::UnicodeWidthChar;
    s.chars()
        .filter(|&c| !is_style_marker(c))
        .map(|c| c.width().unwrap_or(0))
        .sum()
}

/// Resolve the style markers in the fully formatted document. When `styling` is
/// on, characters inside a bold/underline span become nroff overstrike
/// (`c\bc` / `_\bc`); otherwise the markers are simply removed. The font state
/// persists across newlines so a span that wraps keeps its style.
pub(crate) fn apply_styling(content: &str, styling: bool) -> String {
    let mut out = String::with_capacity(content.len());
    let mut state = STYLE_RESET;
    for ch in content.chars() {
        match ch {
            STYLE_BOLD | STYLE_UL | STYLE_RESET => state = ch,
            _ if !styling || ch.is_whitespace() => out.push(ch),
            _ if state == STYLE_BOLD => {
                out.push(ch);
                out.push('\u{8}');
                out.push(ch);
            }
            _ if state == STYLE_UL => {
                out.push('_');
                out.push('\u{8}');
                out.push(ch);
            }
            _ => out.push(ch),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::display_width;

    #[test]
    fn wide_and_combining_characters_measure_in_cells() {
        // ASCII is one cell each.
        assert_eq!(display_width("hello"), 5);
        // East Asian wide and fullwidth forms occupy two cells; counting scalars
        // wrapped CJK text at half the terminal width.
        assert_eq!(display_width("日本語"), 6);
        assert_eq!(display_width("ＡＢ"), 4);
        // Combining marks occupy none; counting them wrapped accented text early.
        assert_eq!(display_width("e\u{301}"), 1);
        assert_eq!(display_width("a\u{300}\u{301}\u{302}"), 1);
        // Style markers are zero-width regardless.
        assert_eq!(display_width("\u{1}日\u{3}"), 2);
        // Mixed script.
        assert_eq!(display_width("ls 一覧"), 7);
    }
}
