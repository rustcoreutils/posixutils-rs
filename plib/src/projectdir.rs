//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `PROJECTDIR` resolution, shared by `make` and the `sccs` front end.
//!
//! Both utilities' XSI descriptions define the variable in the same words --
//! `make` at spec 105471-82, `sccs` at 113894-99 -- and both had, or needed, a
//! copy of the rule. One rule, one implementation: they drifted before the
//! shared version existed, with `sccs` missing the final relative-pathname
//! branch entirely.

use std::env;
use std::path::{Path, PathBuf};

/// Resolve a `PROJECTDIR` *value* to the directory to search.
///
/// Per spec 105474-78, in order:
///
/// - a value beginning with `/` "shall be considered an absolute pathname";
/// - otherwise the value "is treated as a user name and that user's initial
///   working directory shall be examined for a subdirectory `src` or `source`.
///   If such a directory is found, it shall be used";
/// - "Otherwise, the value is used as a relative pathname."
///
/// A null value yields `None`: spec 105479-80 makes an unset *or* null
/// `PROJECTDIR` mean "search the current directory", which is what the callers
/// already do when this returns nothing.
pub fn resolve(value: &str) -> Option<PathBuf> {
    if value.is_empty() {
        return None;
    }
    if value.starts_with('/') {
        return Some(PathBuf::from(value));
    }
    if let Some(home) = user_home_dir(value) {
        // Only `src`/`source`; the home directory itself is never the answer.
        for sub in ["src", "source"] {
            let candidate = home.join(sub);
            if candidate.is_dir() {
                return Some(candidate);
            }
        }
    }
    // The fall-through the spec spells out and `sccs` used to drop on the
    // floor, leaving root_dir at "." for every non-absolute value that did not
    // name a user with a src/source directory.
    Some(PathBuf::from(value))
}

/// Resolve the `PROJECTDIR` environment variable, or `None` when it is unset
/// or null.
pub fn from_env() -> Option<PathBuf> {
    resolve(&env::var("PROJECTDIR").ok()?)
}

/// The `SCCS` directory inside a resolved `PROJECTDIR`.
///
/// "In all of the following cases, the search for SCCS files is made in the
/// directory SCCS in the identified directory" (105473-74). Callers that want
/// a history file append `s.<name>` to this.
pub fn sccs_dir(resolved: &Path) -> PathBuf {
    resolved.join("SCCS")
}

/// Look up a user's home directory in the passwd database.
fn user_home_dir(name: &str) -> Option<PathBuf> {
    use std::ffi::{CStr, CString};
    let cname = CString::new(name).ok()?;
    // SAFETY: `cname` is a live NUL-terminated string; the returned `passwd`
    // points into libc's static storage, read before any further libc call.
    unsafe {
        let pw = libc::getpwnam(cname.as_ptr());
        if pw.is_null() || (*pw).pw_dir.is_null() {
            return None;
        }
        let dir = CStr::from_ptr((*pw).pw_dir).to_str().ok()?;
        Some(PathBuf::from(dir))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_null_value_means_the_current_directory() {
        assert_eq!(resolve(""), None);
    }

    #[test]
    fn a_leading_slash_is_an_absolute_pathname() {
        assert_eq!(resolve("/proj"), Some(PathBuf::from("/proj")));
        // Not probed against the passwd database, and not required to exist.
        assert_eq!(
            resolve("/no/such/dir/anywhere"),
            Some(PathBuf::from("/no/such/dir/anywhere"))
        );
    }

    #[test]
    fn a_name_that_is_no_user_falls_back_to_a_relative_pathname() {
        // The branch sccs dropped: a value that is neither absolute nor a user
        // with src/source is still a pathname, not a reason to give up.
        assert_eq!(
            resolve("zz_no_such_user_zz"),
            Some(PathBuf::from("zz_no_such_user_zz"))
        );
        assert_eq!(resolve("proj/sub"), Some(PathBuf::from("proj/sub")));
    }

    #[test]
    fn a_user_name_selects_src_or_source_when_present() {
        // `root` exists in the passwd database everywhere this runs, but its
        // home almost never has src/source -- so this pins the *fallback* for
        // a real user, which is the half that is testable without fixtures.
        if user_home_dir("root").is_some() {
            let got = resolve("root").expect("a non-null value always resolves");
            let plausible =
                got == Path::new("root") || got.ends_with("src") || got.ends_with("source");
            assert!(plausible, "unexpected resolution for a real user: {got:?}");
        }
    }

    #[test]
    fn sccs_files_are_sought_in_an_sccs_subdirectory() {
        assert_eq!(sccs_dir(Path::new("/proj")), PathBuf::from("/proj/SCCS"));
    }
}
