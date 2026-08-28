//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The protection an s-file carries, and who may edit it.
//!
//! POSIX 99077-99078: "SCCS file protection specified via the ceiling, floor,
//! and authorized user list stored in the SCCS file shall be enforced when the
//! -e option is used." The `l` flag adds locked releases, against which "get
//! -e ... fails" (84047-84048).
//!
//! Every one of these was parsed by the s-file reader, written back out
//! faithfully, and reported by `prs` -- and consulted by nobody. A release
//! lock or a user exclusion was accepted and then silently had no effect,
//! which is worse than not supporting it: the administrator believes the file
//! is protected.
//!
//! Retrieval without `-e` is deliberately not gated. The spec attaches these
//! checks to editing, and CSSC does the same.

use gettextrs::gettext;
use plib::sccsfile::SccsFile;

/// Why an edit was refused. The wording matches CSSC 1.4.1, which is what
/// existing scripts match against.
pub enum Refusal {
    /// The release is locked, or outside the ceiling/floor range.
    ReleaseLocked,
    /// The invoking user is not permitted to make deltas.
    NotAuthorized,
}

impl Refusal {
    /// The diagnostic to print, translated.
    ///
    /// Localized here rather than at each call site so a third caller cannot
    /// forget: these were the only user-visible strings in the crate reaching
    /// the terminal untranslated.
    pub fn message(&self) -> String {
        match self {
            Refusal::ReleaseLocked => gettext("Requested release is locked."),
            Refusal::NotAuthorized => gettext("You are not authorized to make deltas."),
        }
    }
}

/// Check whether `login` may create a delta in `release` of this file.
pub fn check_edit(sccs: &SccsFile, release: u16, login: &str) -> Result<(), Refusal> {
    if sccs
        .locked_releases()
        .is_some_and(|lock| lock.locks(release))
    {
        return Err(Refusal::ReleaseLocked);
    }

    // The ceiling and floor default to 9999 and 1 when their flags are absent,
    // so this comparison is always meaningful.
    if release > sccs.ceiling() || release < sccs.floor() {
        return Err(Refusal::ReleaseLocked);
    }

    if !is_authorized(&sccs.header.users, login) {
        return Err(Refusal::NotAuthorized);
    }

    Ok(())
}

/// Whether `login` appears on an authorized-user list.
///
/// "If the list of users is empty, then anyone may add deltas. If login or
/// group ID is preceded by a '!', the users so specified shall be denied
/// permission to make deltas" (POSIX 84088-84090). A denial outranks an
/// allowance, and a list that names only denials admits nobody -- which is
/// what CSSC does, and follows from the list being non-empty.
pub fn is_authorized(users: &[String], login: &str) -> bool {
    if users.is_empty() {
        return true;
    }

    let mut allowed = false;
    for entry in users {
        match entry.strip_prefix('!') {
            Some(denied) => {
                if entry_matches(denied, login) {
                    return false;
                }
            }
            None => {
                if entry_matches(entry, login) {
                    allowed = true;
                }
            }
        }
    }
    allowed
}

/// Whether one list entry covers `login`.
///
/// "A group ID shall be equivalent to specifying all login names common to
/// that group ID" (POSIX 84085-84086), so a numeric entry matches both the
/// group's supplementary member list and any user whose primary group it is.
fn entry_matches(entry: &str, login: &str) -> bool {
    if entry == login {
        return true;
    }

    let Ok(gid) = entry.parse::<u32>() else {
        return false;
    };

    if plib::user::get_by_name(login).is_some_and(|u| u.gid == gid) {
        return true;
    }

    plib::group::get_by_gid(gid).is_some_and(|g| g.members.iter().any(|m| m == login))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn list(entries: &[&str]) -> Vec<String> {
        entries.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn an_empty_list_admits_everyone() {
        assert!(is_authorized(&[], "anyone"));
    }

    #[test]
    fn a_named_user_is_admitted_and_others_are_not() {
        let users = list(&["alice"]);
        assert!(is_authorized(&users, "alice"));
        assert!(!is_authorized(&users, "bob"));
    }

    #[test]
    fn a_denial_outranks_an_allowance() {
        assert!(!is_authorized(&list(&["alice", "!alice"]), "alice"));
        assert!(!is_authorized(&list(&["!alice", "alice"]), "alice"));
    }

    /// A list holding only denials is still a list, so it admits nobody.
    #[test]
    fn a_deny_only_list_admits_nobody() {
        assert!(!is_authorized(&list(&["!alice"]), "bob"));
        assert!(!is_authorized(&list(&["!alice"]), "alice"));
    }
}
