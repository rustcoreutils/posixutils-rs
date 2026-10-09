//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

// This module is shared between `cp`, `mv` and `rm` but is considered as three
// separated modules due to the project structure. The `#![allow(unused)]` is
// to remove warnings when, say, `rm` doesn't use all the the functions in this
// module (but is used in `cp` or `mv`).
#![allow(unused)]

mod change_ownership;
mod copy;
mod pinned;

use gettextrs::gettext;
use std::{ffi::CStr, io, os::unix::ffi::OsStrExt, path::Path};

/// What `-v` writes for each step of a copy (GNU's wording).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Verbose {
    /// `cp -v`: `'source' -> 'dest'` for each file copied or linked and each directory made.
    Copy,
    /// `mv -v` across filesystems: `created directory 'dest'` for each directory made and
    /// `copied 'source' -> 'dest'` for each file copied.
    Move,
}

/// `path` quoted as GNU coreutils quotes a file name in its messages (`quotearg`'s
/// shell-escape-always style), so the name can be pasted back into a shell.
pub fn quote(path: &Path) -> String {
    quote_bytes(path.as_os_str().as_bytes())
}

/// One character of a name being quoted.
enum Quoted<'a> {
    /// A printable character, in the current locale's encoding.
    Printable(&'a str),
    /// A byte that is not part of a printable character.
    Unprintable(u8),
}

/// Whether a printable character can stand in a double-quoted shell word exactly as in a C
/// string, so that a name holding `'` may be put in double quotes. `#` and `~` qualify only at
/// the start of the name, where the shell would otherwise read them specially.
fn quote_compatible(c: &str, at_start: bool) -> bool {
    match c.as_bytes() {
        [b'#' | b'~'] => at_start,
        [b] if b.is_ascii_alphanumeric() => true,
        [b' ' | b'%' | b'+' | b',' | b'-' | b'.' | b'/' | b':' | b']' | b'_' | b'@' | b'\''] => {
            true
        }
        [b] if b.is_ascii() => false,
        // A printable multibyte character.
        _ => true,
    }
}

/// `quote` for a name given as bytes.
pub fn quote_bytes(name: &[u8]) -> String {
    let mut chars = Vec::new();
    for slice in plib::locale::mb_char_slices(name) {
        let printable = match std::str::from_utf8(slice) {
            Ok(s) if slice.len() == 1 => (0x20..0x7f).contains(&slice[0]).then_some(s),
            Ok(s) => s
                .chars()
                .next()
                .filter(|c| plib::locale::isprint(*c))
                .map(|_| s),
            Err(_) => None,
        };
        match printable {
            Some(s) => chars.push(Quoted::Printable(s)),
            None => chars.extend(slice.iter().map(|b| Quoted::Unprintable(*b))),
        }
    }

    let has_single_quote = chars.iter().any(|c| matches!(c, Quoted::Printable("'")));
    let all_compatible = chars.iter().enumerate().all(|(i, c)| match c {
        Quoted::Printable(s) => quote_compatible(s, i == 0),
        Quoted::Unprintable(_) => false,
    });
    if has_single_quote && all_compatible {
        let mut out = String::from("\"");
        for c in &chars {
            if let Quoted::Printable(s) = c {
                out.push_str(s);
            }
        }
        out.push('"');
        return out;
    }

    // Single quotes; each `'` as `'\''`, and each run of unprintable bytes as `'$'...'` with C
    // escapes, the single quotes resuming after it.
    let mut out = String::from("'");
    let mut in_escape = false;
    for c in &chars {
        match c {
            Quoted::Unprintable(b) => {
                if !in_escape {
                    out.push_str("'$'");
                    in_escape = true;
                }
                match b {
                    0x07 => out.push_str("\\a"),
                    0x08 => out.push_str("\\b"),
                    0x09 => out.push_str("\\t"),
                    0x0a => out.push_str("\\n"),
                    0x0b => out.push_str("\\v"),
                    0x0c => out.push_str("\\f"),
                    0x0d => out.push_str("\\r"),
                    _ => out.push_str(&format!("\\{b:03o}")),
                }
            }
            Quoted::Printable("'") => {
                out.push_str("'\\''");
                in_escape = false;
            }
            Quoted::Printable(s) => {
                if in_escape {
                    out.push_str("''");
                    in_escape = false;
                }
                out.push_str(s);
            }
        }
    }
    out.push('\'');
    out
}

// cp and mv
pub use copy::{
    copy_file, copy_file_at, copy_files, copy_moved_file, finish_made_dir_mode,
    made_dir_open_error, open_made_dir, preserve_through_fd, CopyConfig, DerefMode, Destination,
    InodeMap, MadeTrust, MoveSource,
};

// mv
pub use pinned::{Anchor, CopiedSources, PinnedDir, PinnedDirs, PinnedEntry, SourceState};

// chgrp and chown
pub use change_ownership::{chown_traverse, ChangeOwnershipArgs};

/// Return the error message.
///
/// This is for compatibility with coreutils mv. `format!("{e}")` will append
/// the error code after the error message which we do not want.
pub fn error_string(e: &io::Error) -> String {
    let s = match e.raw_os_error() {
        // Like `format!("{e}")` except without the error code.
        //
        // `std` doesn't expose `sys::os::error_string` so this was copied from:
        // https://github.com/rust-lang/rust/blob/72f616273cbbacc06918ef50470d052d39d9b514/library/std/src/sys/pal/unix/os.rs#L124-L149
        Some(errno) => {
            let mut buf = [0; 128];

            unsafe {
                if libc::strerror_r(errno as _, buf.as_mut_ptr(), buf.len()) == 0 {
                    String::from_utf8_lossy(CStr::from_ptr(buf.as_ptr()).to_bytes()).to_string()
                } else {
                    // `std` just panics here
                    String::from("Unknown error")
                }
            }
        }
        None => format!("{e}"),
    };

    // Translate the error string
    gettext(s)
}
