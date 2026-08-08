//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! posixutils-i18n library
//!
//! This library provides functionality for internationalization utilities:
//! - gettext/ngettext: message catalog lookup
//! - msgfmt: .po to .mo compilation
//! - locale/localedef: locale handling

/// Byte-oriented helpers shared by the message-catalog parsers.
///
/// These read text in whatever codeset `LC_CTYPE` describes, so they work on
/// bytes rather than `str`. The POSIX <blank>s are single bytes in every codeset
/// the standard admits, so trimming needs no character scan.
pub mod bytes {
    /// The <blank>-trimmed span of `s`.
    pub fn trim(s: &[u8]) -> &[u8] {
        let Some(start) = s.iter().position(|b| !b.is_ascii_whitespace()) else {
            return &[];
        };
        let end = s.iter().rposition(|b| !b.is_ascii_whitespace()).unwrap();
        &s[start..=end]
    }

    /// The <blank>-trimmed tail of `s`.
    pub fn trim_end(s: &[u8]) -> &[u8] {
        match s.iter().rposition(|b| !b.is_ascii_whitespace()) {
            None => &[],
            Some(end) => &s[..=end],
        }
    }
}

pub mod gettext_lib;
pub mod locale_lib;
