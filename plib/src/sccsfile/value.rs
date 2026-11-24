//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The values a command writes into an s-file or a p-file.
//!
//! Both files are lines of fields. A value that holds the wrong byte -- a
//! newline in a comment, a blank in a login, a leading SOH in a line of text
//! -- does not corrupt its own record only: it cuts a line in two, or shifts
//! every field after it, and the file no longer reads back as what was
//! written. Checking for that at each place a value is written is how it was
//! missed at some of them.
//!
//! Each kind of value is a type here, and the only way to make one from
//! outside text is its `new`, which refuses what its record cannot hold. The
//! records that are written (`DeltaEntry`, `PfileEntry`, `NewFile`, the edit
//! methods) take these types, so an unchecked value has no way in. A value
//! read from an s-file is taken as it stands (`from_stored`): it came from
//! one line, and is never written back, since an s-file is edited by its
//! lines. A p-file is written whole from its entries, so its fields are
//! checked when read as well.
//!
//! All of them are bytes. What a login or a comment is in some character set
//! is the locale's business; the file stores what it was given.

use std::borrow::Borrow;
use std::fmt;
use std::ops::Deref;

use super::{SccsError, SccsResult, SOH};

/// Refuse `bytes` for a value of kind `what`.
fn refuse<T>(what: &str, bytes: &[u8], why: &str) -> SccsResult<T> {
    Err(SccsError::InvalidFormat(format!(
        "{what} {:?} {why}",
        String::from_utf8_lossy(bytes)
    )))
}

/// The impls every value type shares: it is its bytes, read-only.
macro_rules! stored_value {
    ($name:ident) => {
        impl $name {
            /// The bytes.
            pub fn as_bytes(&self) -> &[u8] {
                &self.0
            }
        }

        impl Deref for $name {
            type Target = [u8];
            fn deref(&self) -> &[u8] {
                &self.0
            }
        }

        impl AsRef<[u8]> for $name {
            fn as_ref(&self) -> &[u8] {
                &self.0
            }
        }

        impl Borrow<[u8]> for $name {
            fn borrow(&self) -> &[u8] {
                &self.0
            }
        }

        /// For diagnostics: the bytes as text, replacing what is not UTF-8.
        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                String::from_utf8_lossy(&self.0).fmt(f)
            }
        }
    };
    // A kind of value the s-file parser reads.
    ($name:ident, stored) => {
        stored_value!($name);

        impl $name {
            /// A value read from an s-file, taken as it stands. Only the
            /// parser has such values, so only this module's parent can
            /// make one unchecked.
            pub(super) fn from_stored(bytes: &[u8]) -> Self {
                $name(bytes.to_vec())
            }
        }
    };
}

/// A login name: the author of a delta, the holder of an edit, an entry of
/// the list of authorized users (where it may be a numeric group ID, or
/// either with a leading `!`).
///
/// It is one blank-separated field of the `^Ad` line and of a p-file line,
/// and a whole line of the user list. So it is not empty, holds no white
/// space, and does not begin with SOH, which would make a control line of it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Login(Vec<u8>);
stored_value!(Login, stored);

impl Login {
    pub fn new(bytes: &[u8]) -> SccsResult<Self> {
        if bytes.is_empty() {
            return refuse("login", bytes, "is empty");
        }
        if bytes.iter().any(u8::is_ascii_whitespace) {
            return refuse("login", bytes, "holds white space");
        }
        if bytes[0] == SOH {
            return refuse("login", bytes, "begins with a control character");
        }
        Ok(Login(bytes.to_vec()))
    }
}

/// The login name of the real user, which SCCS records as the author of a
/// delta and the holder of an edit.
///
/// It is the name `getpwuid(getuid())` gives (the real uid), as in
/// historical SCCS and CSSC. Only when that lookup finds nothing does the
/// `LOGNAME` or `USER` environment stand in, and `unknown` when neither is
/// set. The environment is the caller's to set to anything, so what comes
/// from it is checked like any other login: a name with a blank or a newline
/// in it would shift the fields of the delta's line or cut a p-file line in
/// two, and is refused.
pub fn real_login_name() -> SccsResult<Login> {
    let uid = unsafe { libc::getuid() };
    let passwd = crate::user::get_by_uid(uid).map(|user| user.name.into_encoded_bytes());
    let env = std::env::var_os("LOGNAME").or_else(|| std::env::var_os("USER"));
    login_from(
        passwd.as_deref(),
        env.as_ref().map(|name| name.as_encoded_bytes()),
    )
}

/// [`real_login_name`], given what the lookups found.
pub(super) fn login_from(passwd: Option<&[u8]>, env: Option<&[u8]>) -> SccsResult<Login> {
    match (passwd.filter(|name| !name.is_empty()), env) {
        (Some(name), _) | (None, Some(name)) => Login::new(name),
        (None, None) => Login::new(b"unknown"),
    }
}

/// One line of a delta's comment (`^Ac`). It is the rest of its line, so the
/// one thing it cannot hold is a newline; [`CommentLine::lines`] makes one
/// per line of a longer comment.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CommentLine(Vec<u8>);
stored_value!(CommentLine, stored);

impl CommentLine {
    pub fn new(bytes: &[u8]) -> SccsResult<Self> {
        if bytes.contains(&b'\n') {
            return refuse("comment line", bytes, "holds a newline");
        }
        Ok(CommentLine(bytes.to_vec()))
    }

    /// A comment as the lines the s-file stores, one record each. A final
    /// newline ends the last line rather than starting an empty one.
    pub fn lines(comment: &[u8]) -> Vec<CommentLine> {
        let comment = comment.strip_suffix(b"\n").unwrap_or(comment);
        comment
            .split(|&b| b == b'\n')
            .map(|line| CommentLine(line.to_vec()))
            .collect()
    }
}

/// A modification request number (`^Am`), one per line. MRs are given as a
/// blank-separated list, so one holds no white space, and it is not empty.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MrNumber(Vec<u8>);
stored_value!(MrNumber, stored);

impl MrNumber {
    pub fn new(bytes: &[u8]) -> SccsResult<Self> {
        if bytes.is_empty() {
            return refuse("MR number", bytes, "is empty");
        }
        if bytes.iter().any(u8::is_ascii_whitespace) {
            return refuse("MR number", bytes, "holds white space");
        }
        Ok(MrNumber(bytes.to_vec()))
    }

    /// The MRs of a blank-separated list.
    pub fn list(list: &[u8]) -> Vec<MrNumber> {
        list.split(u8::is_ascii_whitespace)
            .filter(|mr| !mr.is_empty())
            .map(|mr| MrNumber(mr.to_vec()))
            .collect()
    }
}

/// A line of text that stands by itself in the s-file, with no control
/// prefix: a line of the descriptive text. It holds no newline and does not
/// begin with SOH, which would make a control line of it (`^AT` ends the
/// descriptive text).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextLine(Vec<u8>);
stored_value!(TextLine);

impl TextLine {
    pub fn new(bytes: &[u8]) -> SccsResult<Self> {
        if bytes.contains(&b'\n') {
            return refuse("line", bytes, "holds a newline");
        }
        if bytes.first() == Some(&SOH) {
            return refuse("line", bytes, "begins with a control character");
        }
        Ok(TextLine(bytes.to_vec()))
    }
}

/// The list of deltas an edit includes or excludes (`get -i`, `-x`), as the
/// p-file records it: SIDs and ranges of SIDs, separated by commas.
///
/// It is the last field of a p-file line, attached to its `-i` or `-x`, so
/// it holds nothing but what a list is made of: digits, `.`, `-` and `,`.
/// A list given with blanks between its items is the same list with commas.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SidList(Vec<u8>);
stored_value!(SidList);

impl SidList {
    pub fn new(list: &[u8]) -> SccsResult<Self> {
        let items: Vec<&[u8]> = list
            .split(|b| b.is_ascii_whitespace() || *b == b',')
            .filter(|item| !item.is_empty())
            .collect();
        if items.is_empty() {
            return refuse("list of deltas", list, "is empty");
        }
        let well_formed = |item: &&[u8]| {
            item[0].is_ascii_digit() && item.iter().all(|b| b.is_ascii_digit() || b".-".contains(b))
        };
        if !items.iter().all(well_formed) {
            return refuse("list of deltas", list, "is not SIDs and ranges of SIDs");
        }
        Ok(SidList(items.join(&b","[..])))
    }

    /// The list as text: it is ASCII.
    pub fn as_str(&self) -> &str {
        std::str::from_utf8(&self.0).unwrap_or_default()
    }
}
