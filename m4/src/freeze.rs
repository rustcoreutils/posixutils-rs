//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU m4's frozen state files, version 1: `--freeze-state` writes the macro
//! table, quotes, comments and diversions at the end of a run, and
//! `--reload-state` restores them at the start of another, in place of the
//! built-in table.  Every autom4te language reloads one; Debian ships them
//! frozen by GNU m4 1.4.19.
//!
//! The file is a sequence of directives, each a letter and comma-separated
//! decimal numbers on a line, most followed by that many bytes of strings
//! and a newline:
//!
//! ```text
//! # comment                 V1                version
//! Q<l>,<r>\n<lq><rq>\n      quotes            C<b>,<e>\n<bc><ec>\n  comments
//! T<n>,<t>\n<name><text>\n  pushdef text      F<n>,<b>\n<name><builtin>\n  pushdef builtin
//! D<d>,<s>\n<text>\n        diversion d holds text; the last `D<d>,0` makes d current
//! ```

use std::io::Write;
use std::path::Path;
use std::rc::Rc;

use crate::lexer::MacroName;
use crate::macros::{BuiltinMacro, MacroDefinition, MacroDefinitionImplementation};
use crate::state::State;

/// The only frozen file version there is.
const VERSION: u64 = 1;

/// The exit status for a frozen file of an unsupported version, GNU m4's.
pub const VERSION_MISMATCH_STATUS: i32 = 63;
/// Why a frozen file could not be read. The numbers are the line of the
/// file the bad directive starts on, which GNU m4 reports.
pub enum ReloadError {
    /// It could not be opened or read.
    Io(std::io::Error),
    /// Its version is not 1.
    Version(u64, usize),
    /// It is not well formed.
    Malformed(usize),
}

/// A cursor over the bytes of a frozen file.
struct Reader<'a> {
    bytes: &'a [u8],
    pos: usize,
    /// Where the directive being read starts.
    directive_start: usize,
}

impl<'a> Reader<'a> {
    fn next(&mut self) -> Option<u8> {
        let c = *self.bytes.get(self.pos)?;
        self.pos += 1;
        Some(c)
    }

    /// The line the directive being read starts on.
    fn line(&self) -> usize {
        1 + self.bytes[..self.directive_start]
            .iter()
            .filter(|&&c| c == b'\n')
            .count()
    }

    fn malformed(&self) -> ReloadError {
        ReloadError::Malformed(self.line())
    }

    /// A decimal number, possibly negative (a diversion), ended by `end`.
    fn number(&mut self, end: u8) -> Result<i64, ReloadError> {
        let negative = self.bytes.get(self.pos) == Some(&b'-');
        if negative {
            self.pos += 1;
        }
        let start = self.pos;
        let mut value: i64 = 0;
        while let Some(c) = self.bytes.get(self.pos).filter(|c| c.is_ascii_digit()) {
            value = value
                .checked_mul(10)
                .and_then(|v| v.checked_add(i64::from(c - b'0')))
                .ok_or_else(|| self.malformed())?;
            self.pos += 1;
        }
        if self.pos == start || self.next() != Some(end) {
            return Err(self.malformed());
        }
        Ok(if negative { -value } else { value })
    }

    /// A count of bytes: a non-negative number.
    fn count(&mut self, end: u8) -> Result<usize, ReloadError> {
        let n = self.number(end)?;
        usize::try_from(n).map_err(|_| self.malformed())
    }

    /// The next `len` bytes.
    fn take(&mut self, len: usize) -> Result<&'a [u8], ReloadError> {
        let end = self.pos.checked_add(len).ok_or_else(|| self.malformed())?;
        let text = self
            .bytes
            .get(self.pos..end)
            .ok_or_else(|| self.malformed())?;
        self.pos = end;
        Ok(text)
    }

    /// Two strings whose lengths are `<a>,<b>\n`, then the newline after
    /// them.
    fn two_strings(&mut self) -> Result<(&'a [u8], &'a [u8]), ReloadError> {
        let a = self.count(b',')?;
        let b = self.count(b'\n')?;
        let first = self.take(a)?;
        let second = self.take(b)?;
        self.end_of_line()?;
        Ok((first, second))
    }

    fn end_of_line(&mut self) -> Result<(), ReloadError> {
        match self.next() {
            Some(b'\n') => Ok(()),
            _ => Err(self.malformed()),
        }
    }
}

/// Replace the macro table, quotes, comments and diversions of `state` with
/// those of the frozen file at `path`. Built-in names the file asks for that
/// this m4 does not have are reported on `stderr` and left undefined.
pub fn reload(state: &mut State, path: &Path, stderr: &mut dyn Write) -> Result<(), ReloadError> {
    let (mut file, _) = state.open_input(path).map_err(ReloadError::Io)?;
    let mut bytes = Vec::new();
    std::io::Read::read_to_end(&mut file, &mut bytes).map_err(ReloadError::Io)?;
    let mut r = Reader {
        bytes: &bytes,
        pos: 0,
        directive_start: 0,
    };

    state.macro_definitions.clear();
    loop {
        r.directive_start = r.pos;
        let Some(directive) = r.next() else {
            break;
        };
        match directive {
            b'#' => while r.next().is_some_and(|c| c != b'\n') {},
            b'\n' => {}
            b'V' => {
                let version = r.number(b'\n')?;
                if version != VERSION as i64 {
                    return Err(ReloadError::Version(version.max(0) as u64, r.line()));
                }
            }
            b'Q' => {
                let (open, close) = r.two_strings()?;
                state.parse_config.quote_open_tag = open.to_vec();
                state.parse_config.quote_close_tag = close.to_vec();
            }
            b'C' => {
                let (open, close) = r.two_strings()?;
                state.parse_config.comment_enabled = !open.is_empty();
                state.parse_config.comment_open_tag = open.to_vec();
                state.parse_config.comment_close_tag = close.to_vec();
            }
            b'T' => {
                let (name, text) = r.two_strings()?;
                let name = MacroName(name.to_vec());
                let definition = MacroDefinition::new_user_defined(name.clone(), text.to_vec());
                state
                    .macro_definitions
                    .entry(name)
                    .or_default()
                    .push(Rc::new(definition));
            }
            b'F' => {
                let (name, builtin_name) = r.two_strings()?;
                let Some(builtin) = BuiltinMacro::from_name(builtin_name) else {
                    let _ = writeln!(
                        stderr,
                        "m4: builtin `{}' requested by frozen file is not supported",
                        String::from_utf8_lossy(builtin_name)
                    );
                    continue;
                };
                let name = MacroName(name.to_vec());
                let definition = MacroDefinition::new_builtin(name.clone(), builtin);
                state
                    .macro_definitions
                    .entry(name)
                    .or_default()
                    .push(Rc::new(definition));
            }
            b'D' => {
                let number = r.number(b',')?;
                let len = r.count(b'\n')?;
                let text = r.take(len)?;
                r.end_of_line()?;
                match usize::try_from(number) {
                    Ok(n) if n > 0 && len > 0 => state
                        .output
                        .output
                        .append_to_diversion(n, text)
                        .map_err(|_| r.malformed())?,
                    _ => {}
                }
                // The last D directive leaves its diversion current.
                state
                    .output
                    .output
                    .divert(number)
                    .map_err(|_| r.malformed())?;
            }
            _ => return Err(r.malformed()),
        }
    }
    Ok(())
}

/// Write the state of `state` to `path` as a frozen file.
pub fn freeze(state: &State, path: &Path) -> std::io::Result<()> {
    let mut out = Vec::new();
    out.extend_from_slice(b"# This is a frozen state file generated by posixutils m4\nV1\n");
    let config = &state.parse_config;
    if config.quote_open_tag != crate::lexer::DEFAULT_QUOTE_OPEN_TAG
        || config.quote_close_tag != crate::lexer::DEFAULT_QUOTE_CLOSE_TAG
    {
        two_strings(
            &mut out,
            b'Q',
            &config.quote_open_tag,
            &config.quote_close_tag,
        );
    }
    let (comment_open, comment_close): (&[u8], &[u8]) = if config.comment_enabled {
        (&config.comment_open_tag, &config.comment_close_tag)
    } else {
        (b"", b"")
    };
    if comment_open != crate::lexer::DEFAULT_COMMENT_OPEN_TAG
        || comment_close != crate::lexer::DEFAULT_COMMENT_CLOSE_TAG
    {
        two_strings(&mut out, b'C', comment_open, comment_close);
    }
    // Names sorted so that freezing the same state gives the same file;
    // each pushdef stack oldest first, so reloading rebuilds it in order.
    let mut names: Vec<&MacroName> = state.macro_definitions.keys().collect();
    names.sort_by(|a, b| a.0.cmp(&b.0));
    for name in names {
        for definition in &state.macro_definitions[name] {
            match &definition.implementation {
                MacroDefinitionImplementation::UserDefined(user) => {
                    two_strings(&mut out, b'T', &name.0, &user.definition)
                }
                implementation => {
                    let builtin = implementation
                        .builtin()
                        .expect("a definition that is not user-defined is a built-in");
                    two_strings(&mut out, b'F', &name.0, builtin.as_ref());
                }
            }
        }
    }
    for (n, text) in state.output.output.diversions() {
        out.extend_from_slice(format!("D{n},{}\n", text.len()).as_bytes());
        out.extend_from_slice(&text);
        out.push(b'\n');
    }
    out.extend_from_slice(format!("D{},0\n\n", state.output.output.divert_number()).as_bytes());
    out.extend_from_slice(b"# End of frozen state file\n");
    plib::io::write_atomic(path, &out)
}

/// Append the directive `<letter><a>,<b>\n<a bytes><b bytes>\n`.
fn two_strings(out: &mut Vec<u8>, letter: u8, a: &[u8], b: &[u8]) {
    out.push(letter);
    out.extend_from_slice(format!("{},{}\n", a.len(), b.len()).as_bytes());
    out.extend_from_slice(a);
    out.extend_from_slice(b);
    out.push(b'\n');
}
