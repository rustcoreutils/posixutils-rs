//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::shstr::ShString;
use std::ffi::CStr;

pub fn strcoll(lhs: &CStr, rhs: &CStr) -> std::cmp::Ordering {
    // strings are valid, this is safe
    let ordering = unsafe { libc::strcoll(lhs.as_ptr(), rhs.as_ptr()) };
    ordering.cmp(&0)
}

/// Single-quotes `s` so the result is suitable for re-input to the shell, even
/// when `s` contains single-quote characters (each `'` becomes `'\''`). Used by
/// the re-inputtable output of `export -p`, `readonly -p`, `set`, `trap`, and
/// `alias`.
/// Operates on bytes and passes them through untouched, so that a value which
/// is not valid text still round-trips through `export -p` and friends.
pub fn shell_quote<S: AsRef<[u8]>>(s: S) -> ShString {
    let bytes = s.as_ref();
    let mut result = ShString::new();
    result.push_bytes(b"'");
    for &byte in bytes {
        if byte == b'\'' {
            result.push_bytes(b"'\\''");
        } else {
            result.push_bytes([byte]);
        }
    }
    result.push_bytes(b"'");
    result
}
