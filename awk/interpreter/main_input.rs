//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::io::{FileStream, RecordReader, StdinRecordReader};
use super::value::{AwkValue, AwkValueRef, AwkValueVariant};
use super::{maybe_numeric_string, parse_assignment, GlobalEnv};
use crate::compiler::escape_string_contents;
use crate::program::SpecialVar;
use std::collections::HashMap;

/// The main input, which the rules and plain `getline` read: each file
/// operand in ARGV in turn, or standard input if there is none.  The
/// assignment operands between the files are performed when the input
/// reaches them.
pub(super) struct MainInput {
    /// The ARGV index of the next operand.
    next_operand: usize,
    /// The file being read.
    reader: Option<Box<dyn RecordReader>>,
    /// Whether a file operand has been opened; if none is, standard input
    /// is read.
    opened_a_file: bool,
    /// No input is left, or `exit` stopped the reading of it.
    finished: bool,
    /// The global index of each variable of the program, for assignment
    /// operands.
    program_globals: HashMap<String, u32>,
}

/// A pointer to the value of `globals[var]`.  The main input is read
/// between statements, when no reference to a global is live, so it is
/// safe to dereference there.
fn global(globals: &[AwkValueRef], var: SpecialVar) -> *mut AwkValue {
    globals[var as usize].get()
}

impl MainInput {
    pub(super) fn new(program_globals: HashMap<String, u32>) -> Self {
        Self {
            next_operand: 1,
            reader: None,
            opened_a_file: false,
            finished: false,
            program_globals,
        }
    }

    /// A main input with nothing to read.
    #[cfg(test)]
    pub(super) fn exhausted() -> Self {
        let mut input = Self::new(HashMap::new());
        input.finish();
        input
    }

    /// Reads the next record of the main input, going on to the next file
    /// at the end of one; `None` when the input is exhausted.
    pub(super) fn next_record(
        &mut self,
        globals: &[AwkValueRef],
        global_env: &mut GlobalEnv,
    ) -> Result<Option<String>, String> {
        loop {
            if let Some(reader) = &mut self.reader {
                if let Some(record) = reader.read_next_record(&global_env.rs)? {
                    return Ok(Some(record));
                }
                self.reader = None;
            }
            if self.finished || !self.open_next_file(globals, global_env)? {
                self.finished = true;
                return Ok(None);
            }
        }
    }

    /// Stops reading the current file (`nextfile`).
    pub(super) fn skip_file(&mut self) {
        self.reader = None;
    }

    /// Stops reading the input altogether (`exit`).
    pub(super) fn finish(&mut self) {
        self.reader = None;
        self.finished = true;
    }

    /// Opens the next file operand, performing the assignment operands
    /// before it; false if there is none left.
    fn open_next_file(
        &mut self,
        globals: &[AwkValueRef],
        global_env: &mut GlobalEnv,
    ) -> Result<bool, String> {
        loop {
            let argc = unsafe { &*global(globals, SpecialVar::Argc) }.scalar_as_f64();
            let operand = if (self.next_operand as f64) < argc {
                self.next_operand += 1;
                let argv = unsafe { &mut *global(globals, SpecialVar::Argv) }.as_array()?;
                // a deleted operand is skipped, and not created again
                match argv.get((self.next_operand - 1).to_string().as_str()) {
                    Some(value) => value.clone().scalar_to_string(&global_env.convfmt)?,
                    None => continue,
                }
            } else if self.opened_a_file {
                return Ok(false);
            } else {
                "-".into()
            };

            if operand.is_empty() {
                continue;
            }
            if let Some((var, value)) = parse_assignment(&operand) {
                if let Some(&index) = self.program_globals.get(var) {
                    let value = maybe_numeric_string(escape_string_contents(value)?);
                    unsafe { &mut *globals[index as usize].get() }.assign(value, global_env)?;
                }
                continue;
            }

            self.reader = Some(if operand.as_str() == "-" {
                Box::new(StdinRecordReader::default())
            } else {
                Box::new(FileStream::open(&operand)?)
            });
            self.opened_a_file = true;
            unsafe { &mut *global(globals, SpecialVar::Filename) }.value =
                AwkValueVariant::String(maybe_numeric_string(operand));
            unsafe { &mut *global(globals, SpecialVar::Fnr) }.assign(0.0, global_env)?;
            return Ok(true);
        }
    }
}
