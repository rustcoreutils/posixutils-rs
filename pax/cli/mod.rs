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
use crate::modes;
use std::fs::File;
use std::io;
use std::path::Path;

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
fn usage(prog: &str, msg: impl std::fmt::Display) -> PaxError {
    PaxError::Usage(format!("{}\nTry '{} --help'.", msg, prog))
}

/// Report an option this front-end deliberately does not implement.
///
/// Silently ignoring one would let, say, `tar -cjf` write an uncompressed
/// archive under a `.bz2` name; naming the option makes the gap actionable.
fn unsupported(prog: &str, opt: &str, why: &str) -> PaxError {
    usage(prog, format!("unsupported option '{}' ({})", opt, why))
}

/// Reject an unrecognized option.
fn unknown(prog: &str, opt: &str) -> PaxError {
    usage(prog, format!("unrecognized option '{}'", opt))
}

/// Read a list of names from `path`, one per line (or per NUL when `nul`).
///
/// `-` means standard input, matching tar's `-T -`. The list is read before the
/// operation starts, and therefore before any `-C` has changed the working
/// directory, which is what makes the names in the file relative to where the
/// command was invoked.
fn read_name_list(path: &str, nul: bool) -> PaxResult<Vec<String>> {
    let sep = if nul { b'\0' } else { b'\n' };
    let names = if path == "-" {
        modes::write::read_file_list_sep(io::stdin(), sep)?
    } else {
        let file = File::open(path).map_err(|e| PaxError::Usage(format!("{}: {}", path, e)))?;
        modes::write::read_file_list_sep(file, sep)?
    };

    Ok(names
        .into_iter()
        .map(|p| p.to_string_lossy().into_owned())
        .collect())
}

/// A cursor over the command line, shared by both front-end parsers.
///
/// The two differ in which options exist and how the first operand is spelled,
/// but not in how an option-argument is found: it is either glued to the option
/// letter (`-fx.tar`), joined to the long name with `=`, or the next argument.
struct ArgCursor {
    argv: Vec<String>,
    pos: usize,
    prog: &'static str,
}

impl ArgCursor {
    fn new(prog: &'static str, argv: Vec<String>, start: usize) -> Self {
        ArgCursor {
            argv,
            pos: start,
            prog,
        }
    }

    fn next(&mut self) -> Option<String> {
        let item = self.argv.get(self.pos).cloned();
        if item.is_some() {
            self.pos += 1;
        }
        item
    }

    /// Consume the argument of an option that requires one.
    ///
    /// `glued` is whatever followed the option letter in the same argument;
    /// when it is empty the value comes from the next argument.
    fn value(&mut self, opt: &str, glued: Option<String>) -> PaxResult<String> {
        if let Some(v) = glued {
            if !v.is_empty() {
                return Ok(v);
            }
        }
        self.next()
            .ok_or_else(|| usage(self.prog, format!("option '{}' requires an argument", opt)))
    }

    /// Everything not yet consumed, as operands.
    fn rest(&mut self) -> Vec<String> {
        self.argv.split_off(self.pos.min(self.argv.len()))
    }
}

/// Parse a non-negative integer option-argument.
fn parse_number(prog: &str, opt: &str, value: &str) -> PaxResult<u64> {
    value
        .parse::<u64>()
        .map_err(|_| usage(prog, format!("invalid number '{}' for '{}'", value, opt)))
}
