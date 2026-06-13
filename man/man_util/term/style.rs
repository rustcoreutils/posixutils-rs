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

/// Display width of `s` as a Unicode scalar count, ignoring the zero-width style
/// markers. (Overstrike is applied only after wrapping, so no backspaces are
/// present here.) This is not true terminal-cell width — wide/combining
/// characters are each counted as one. For unstyled text it equals
/// `chars().count()`.
pub(crate) fn display_width(s: &str) -> usize {
    s.chars().filter(|&c| !is_style_marker(c)).count()
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
