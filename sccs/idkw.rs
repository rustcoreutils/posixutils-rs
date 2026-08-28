//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! SCCS identification keywords.
//!
//! `get` scanned a `&str` and `admin` scanned a `&[u8]` for the same `%X%`
//! pattern, with bodies identical modulo the input type. Scanning bytes serves
//! both: a keyword is ASCII, and an s-file body need not be UTF-8.

/// Whether `data` contains an identification keyword of the form `%X%`, where
/// X is an uppercase ASCII letter.
pub fn contains(data: &[u8]) -> bool {
    data.windows(3)
        .any(|w| w[0] == b'%' && w[1].is_ascii_uppercase() && w[2] == b'%')
}

/// Whether `line` contains an identification keyword.
pub fn in_line(line: &str) -> bool {
    contains(line.as_bytes())
}
