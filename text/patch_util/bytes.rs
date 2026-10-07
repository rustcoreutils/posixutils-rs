//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Patch text is bytes, not UTF-8.
//!
//! A patch, and the files it changes, may be in any encoding, and every byte
//! patch does not change must come out exactly as it went in. Internally each
//! byte is held as the `char` of the same value (ISO-8859-1), so the string
//! machinery works unchanged and every comparison is a byte comparison.
//! Convert only at the edges, through these helpers -- never with
//! `from_utf8`, `to_string_lossy` or `Path::display` on patch text.
//!
//! Consequences inside: `char::is_whitespace` and `str::trim` also match
//! 0x85 and 0xA0, which are UTF-8 continuation bytes here, so blank handling
//! must use the ASCII forms (`trim_ascii`, `split_ascii_whitespace`).

use std::ffi::OsString;
use std::path::{Path, PathBuf};

/// Bytes read from a patch or a file, as patch text.
pub fn decode(bytes: &[u8]) -> String {
    bytes.iter().map(|&b| char::from(b)).collect()
}

/// Patch text back to the bytes it came from.
pub fn encode(text: &str) -> Vec<u8> {
    text.chars()
        .map(|c| {
            debug_assert!(u32::from(c) <= 0xff, "patch text holds one byte per char");
            u32::from(c) as u8
        })
        .collect()
}

/// A command-line string (UTF-8) as patch text, so that what it inserts is
/// written back as the same UTF-8.
pub fn from_arg(arg: &str) -> String {
    decode(arg.as_bytes())
}

/// A file name taken from the patch, as a path naming the same bytes.
pub fn to_path(text: &str) -> PathBuf {
    let bytes = encode(text);
    #[cfg(unix)]
    {
        use std::os::unix::ffi::OsStringExt;
        PathBuf::from(OsString::from_vec(bytes))
    }
    #[cfg(not(unix))]
    {
        PathBuf::from(String::from_utf8_lossy(&bytes).into_owned())
    }
}

/// A path as patch text, for writing it into a reject file.
pub fn from_path(path: &Path) -> String {
    #[cfg(unix)]
    {
        use std::os::unix::ffi::OsStrExt;
        decode(path.as_os_str().as_bytes())
    }
    #[cfg(not(unix))]
    {
        decode(path.to_string_lossy().as_bytes())
    }
}

/// `path` with `suffix` appended to its last component.
pub fn with_suffix(path: &Path, suffix: &str) -> PathBuf {
    let mut name = OsString::from(path.as_os_str());
    name.push(suffix);
    PathBuf::from(name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip_every_byte() {
        let all: Vec<u8> = (0..=255).collect();
        assert_eq!(encode(&decode(&all)), all);
    }

    #[test]
    fn arg_round_trips_as_utf8() {
        assert_eq!(encode(&from_arg("caf\u{e9}")), "caf\u{e9}".as_bytes());
    }
}
