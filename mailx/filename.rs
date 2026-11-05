//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Resolving the filenames commands and command escapes are given.

use crate::util::{home, mbox_path, spool_path, system_mailbox};
use crate::variables::Variables;

/// Resolve the filename notations that are mailx's own, not the shell's.
///
/// `+folder` becomes the `folder` variable and a slash (spec 104705-104708);
/// `%` is the system mailbox, `%user` that user's, `&` the secondary mbox; a
/// leading `~` is the home directory. `#` needs the previously opened folder
/// and is resolved by `file`.
///
/// None of this involves the shell. That makes it the expansion for a name
/// taken from message content -- `Save`, `Copy`, `followup` and `Followup`
/// name their file after the author, and shell expansion would make a
/// `From:` header a command line -- and for a command-escape filename, which
/// POSIX names as "the file named by the pathname file" without the shell
/// word expansion of a command-mode filename.
pub fn expand_local_prefixes(name: &str, vars: &Variables) -> String {
    let name = name.trim();

    match name {
        "%" => return system_mailbox(),
        "&" => return mbox_path(vars),
        _ => {}
    }

    if let Some(user) = name.strip_prefix('%') {
        return spool_path(user);
    }
    if let Some(rest) = name.strip_prefix('+') {
        return in_folder(rest, vars).unwrap_or_else(|| name.to_string());
    }
    if name == "~" {
        return home();
    }
    if let Some(rest) = name.strip_prefix("~/") {
        return format!("{}/{}", home(), rest);
    }

    name.to_string()
}

/// Resolve a filename typed in command mode.
///
/// POSIX applies two transformations in sequence (spec 104704-104711): a
/// leading unquoted `+` becomes the `folder` variable and a slash, then shell
/// word expansion. mailx's own `%`, `%user`, and `&` notations are resolved
/// first; `#` needs the previous folder, so `cmd_file` handles it.
///
/// Only one pathname may result; a name that expands to several, or that the
/// shell cannot parse, is used as typed.
pub fn expand_typed_filename(name: &str, vars: &Variables) -> String {
    let expanded = expand_local_prefixes(name, vars);
    shell_expand(&expanded, vars).unwrap_or(expanded)
}

/// Apply shell word expansion to a filename, requiring a single result.
///
/// `printf '%s\n'` puts each resulting word on its own line, so a name that
/// expands to several pathnames can be recognized rather than silently
/// concatenated. Returns `None` when the name is left as typed.
fn shell_expand(name: &str, vars: &Variables) -> Option<String> {
    // The word is interpolated into a shell command, so this must only ever be
    // reached for a name the user typed; see `expand_local_prefixes`.
    // Nothing to expand: no shell metacharacter, no `~`, no variable.
    if !name.contains(['~', '$', '*', '?', '[', '`', '"', '\'', '\\']) {
        return None;
    }

    let out = crate::util::shell_output(&format!("printf '%s\\n' {}", name), vars).ok()?;
    let mut words = out.lines();
    let first = words.next()?;
    if words.next().is_some() {
        // More than one pathname where one is expected: effects are unspecified
        // (spec 104710-104711), so leave the name alone rather than guess.
        return None;
    }
    Some(first.to_string())
}

/// `name` in the directory `folder` names, relative to `HOME` unless it is
/// absolute; `None` when `folder` is unset or null.
pub fn in_folder(name: &str, vars: &Variables) -> Option<String> {
    let folder = vars.get_nonempty("folder")?;
    if folder.starts_with('/') {
        Some(format!("{}/{}", folder, name))
    } else {
        Some(format!("{}/{}/{}", home(), folder, name))
    }
}
