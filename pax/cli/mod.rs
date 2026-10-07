//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Compatibility command-line front-ends.
//!
//! `tar` and `cpio` are not separate implementations: each is a parser for a
//! historic command line that produces the same internal [`crate::Args`] pax
//! itself is driven by, with the archive format forced to the one that command
//! implies. Everything after argument parsing -- traversal, header codecs,
//! extraction -- is shared.
//!
//! Both parsers accept a deliberately limited subset of what GNU tar and GNU
//! cpio accept: enough that existing scripts keep working, without committing
//! to the whole GNU option surface. An option outside the subset is rejected
//! with a diagnostic naming it, never silently ignored, so a script that
//! depends on one fails loudly instead of quietly producing a wrong archive.

pub mod cpio;
pub mod tar;

use crate::error::{PaxError, PaxResult};
use crate::modes::write::NameList;
use crate::rawpath;
use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};

/// Which historic command line this invocation should be parsed as.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProgramMode {
    Pax,
    Tar,
    Cpio,
}

impl ProgramMode {
    /// Pick the parser from the name the binary was invoked under.
    ///
    /// The comparison is against the exact basename, not a suffix of the whole
    /// argument: a suffix test would make any path ending in `tar` -- including
    /// `ustar` or `mytar` -- silently switch parsers.
    pub fn detect() -> Self {
        let argv0 = std::env::args_os().next().unwrap_or_default();
        match Path::new(&argv0).file_name().and_then(|n| n.to_str()) {
            Some("tar") => ProgramMode::Tar,
            Some("cpio") => ProgramMode::Cpio,
            _ => ProgramMode::Pax,
        }
    }

    /// The name to use in diagnostics and usage messages.
    pub fn name(self) -> &'static str {
        match self {
            ProgramMode::Pax => "pax",
            ProgramMode::Tar => "tar",
            ProgramMode::Cpio => "cpio",
        }
    }
}

/// Build a usage diagnostic.
///
/// The program name is not repeated here: `main` already prefixes every error
/// it prints with it.
fn usage(prog: &'static str, msg: impl std::fmt::Display) -> PaxError {
    PaxError::Usage(msg.to_string(), Some(prog))
}

/// Report an option this front-end deliberately does not implement.
///
/// Silently ignoring one would let, say, `tar -cjf` write an uncompressed
/// archive under a `.bz2` name; naming the option makes the gap actionable.
fn unsupported(prog: &'static str, opt: &str, why: &str) -> PaxError {
    usage(prog, format!("unsupported option '{}' ({})", opt, why))
}

/// Reject an unrecognized option.
fn unknown(prog: &'static str, opt: &str) -> PaxError {
    usage(prog, format!("unrecognized option '{}'", opt))
}

/// Open a list of names in `path`, one per line (or per NUL when `nul`), to be
/// read as the names are needed.
///
/// `-` means standard input, matching tar's `-T -`. The file is opened now,
/// before any `-C` has changed the working directory, so a relative `path` is
/// resolved from where the command was invoked.
fn open_name_list(path: &OsStr, nul: bool) -> PaxResult<NameList> {
    let sep = if nul { b'\0' } else { b'\n' };
    if path == "-" {
        return Ok(NameList::stdin(sep));
    }
    let file = File::open(path)
        .map_err(|e| PaxError::Usage(format!("{}: {}", Path::new(path).display(), e), None))?;
    Ok(NameList::file(file, sep))
}

/// Read a whole list of names from `path`, for a list that is needed in full
/// before anything else happens (tar's `-X`). See `open_name_list`.
///
/// The names are kept as the bytes the list holds: a list naming `caf\351`
/// must not exclude some other file whose name is its lossy rendering.
fn read_name_list(path: &OsStr, nul: bool) -> PaxResult<Vec<OsString>> {
    open_name_list(path, nul)?
        .names()
        .map(|name| name.map(PathBuf::into_os_string))
        .collect()
}

/// A cursor over the command line, shared by both front-end parsers.
///
/// The two differ in which options exist and how the first operand is spelled,
/// but not in how an option-argument is found: it is either glued to the option
/// letter (`-fx.tar`), joined to the long name with `=`, or the next argument.
///
/// Arguments are kept as `OsString`: an operand or option-argument may be a
/// pathname, and a pathname is a byte string that need not be UTF-8.
struct ArgCursor {
    argv: Vec<OsString>,
    pos: usize,
    prog: &'static str,
}

impl ArgCursor {
    fn new(prog: &'static str, argv: Vec<OsString>, start: usize) -> Self {
        ArgCursor {
            argv,
            pos: start,
            prog,
        }
    }

    fn next(&mut self) -> Option<OsString> {
        let item = self.argv.get(self.pos).cloned();
        if item.is_some() {
            self.pos += 1;
        }
        item
    }

    /// Consume the argument of an option that requires one.
    ///
    /// `glued` is the value given in the same argument -- after the option
    /// letter, or after `=` -- and is the value even when empty: `--file=`
    /// names the empty string, it does not take the next argument. With none
    /// the value comes from the next argument.
    fn value(&mut self, opt: &str, glued: Option<OsString>) -> PaxResult<OsString> {
        if let Some(v) = glued {
            return Ok(v);
        }
        self.next()
            .ok_or_else(|| usage(self.prog, format!("option '{}' requires an argument", opt)))
    }

    /// Everything not yet consumed, as operands.
    fn rest(&mut self) -> Vec<OsString> {
        self.argv.split_off(self.pos.min(self.argv.len()))
    }
}

/// Parse a non-negative integer option-argument.
fn parse_number(prog: &'static str, opt: &str, value: &OsStr) -> PaxResult<u64> {
    value
        .to_str()
        .and_then(|v| v.parse::<u64>().ok())
        .ok_or_else(|| {
            usage(
                prog,
                format!("invalid number '{}' for '{}'", value.to_string_lossy(), opt),
            )
        })
}

/// A front-end's handler for a long option or a short-option cluster.
type OptionHandler<S> = fn(&[u8], &mut S, &mut ArgCursor) -> PaxResult<()>;

/// Walk the rest of the command line, handing each long option and each
/// short-option cluster to the front-end, and return the operands in order.
fn parse_options<S>(
    cur: &mut ArgCursor,
    st: &mut S,
    long: OptionHandler<S>,
    cluster: OptionHandler<S>,
) -> PaxResult<Vec<OsString>> {
    let mut operands = Vec::new();
    while let Some(arg) = cur.next() {
        match classify(&arg) {
            ArgKind::EndOfOptions => {
                operands.extend(cur.rest());
                break;
            }
            ArgKind::Long(name) => long(name, st, cur)?,
            ArgKind::Cluster(letters) => cluster(letters, st, cur)?,
            ArgKind::Operand => operands.push(arg),
        }
    }
    Ok(operands)
}

/// One command-line argument, classified the way both front-ends read it.
enum ArgKind<'a> {
    /// `--`: everything after it is an operand.
    EndOfOptions,
    /// `--name` or `--name=value`, with the dashes stripped.
    Long(&'a [u8]),
    /// `-xyz`, with the dash stripped.
    Cluster(&'a [u8]),
    /// Anything else, including a bare `-`.
    Operand,
}

fn classify(arg: &OsStr) -> ArgKind<'_> {
    let bytes = arg.as_bytes();
    if bytes == b"--" {
        ArgKind::EndOfOptions
    } else if let Some(long) = bytes.strip_prefix(b"--") {
        ArgKind::Long(long)
    } else if bytes.len() > 1 && bytes[0] == b'-' {
        ArgKind::Cluster(&bytes[1..])
    } else {
        ArgKind::Operand
    }
}

/// Split a long option into its name and any `=value` glued to it.
///
/// The value keeps its bytes. The name is text: every long option is ASCII, so
/// a name that is not UTF-8 is simply one no front-end knows, and is rendered
/// lossily only for the diagnostic.
fn split_long(long: &[u8]) -> (String, Option<OsString>) {
    let (name, value) = match long.iter().position(|&b| b == b'=') {
        Some(i) => (
            &long[..i],
            Some(OsStr::from_bytes(&long[i + 1..]).to_owned()),
        ),
        None => (long, None),
    };
    (String::from_utf8_lossy(name).into_owned(), value)
}

/// The value glued to an option letter in a cluster: the rest of the
/// argument, or none when the letter ends it (`-f x.tar`).
fn glued_value(rest: &[u8]) -> Option<OsString> {
    (!rest.is_empty()).then(|| OsStr::from_bytes(rest).to_owned())
}

/// The option letters of a `-xyz` cluster, each with the bytes that follow it.
///
/// A letter that is not one character of UTF-8 comes back as U+FFFD, which no
/// front-end recognizes, so it is reported as an unknown option. What follows
/// a letter is the glued value of an option that takes one.
fn cluster_letters(cluster: &[u8]) -> impl Iterator<Item = (char, &[u8])> {
    let mut i = 0;
    std::iter::from_fn(move || {
        if i >= cluster.len() {
            return None;
        }
        let len = rawpath::unit_len(&cluster[i..]);
        let c = std::str::from_utf8(&cluster[i..i + len])
            .ok()
            .and_then(|s| s.chars().next())
            .unwrap_or(char::REPLACEMENT_CHARACTER);
        i += len;
        Some((c, &cluster[i..]))
    })
}
