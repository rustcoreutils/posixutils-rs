//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{BuiltinResult, BuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use crate::shstr::ShString;
use std::io::{self, LineWriter, Write};

/// `printf`.
///
/// Built in, as in dash and bash, so that a script does not fork a process
/// for every line it prints.  The format engine is `plib::printf`, shared
/// with the standalone utility so the two cannot drift apart; the
/// diagnostics, their text and the exit status are the utility's.
pub struct Printf;

/// Standard output of the built-in, as a `Write` sink for the engine.
///
/// A failed write is remembered by `OpenedFiles` and reported once by the
/// caller (`report_write_error`), so it is not an error here.
struct BuiltinStdout<'a>(&'a OpenedFiles);

impl Write for BuiltinStdout<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write_out(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl BuiltinUtility for Printf {
    fn exec(
        &self,
        args: &[ShString],
        _: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let opened_files = &*opened_files;
        let report = |msg: &str| opened_files.write_err(format!("printf: {msg}\n"));

        let Some((format, operands)) = plib::printf::split_operands(args) else {
            report(&plib::printf::usage());
            return Ok(plib::printf::USAGE_STATUS.into());
        };
        let operands: Vec<&[u8]> = operands.iter().map(|a| a.as_bytes()).collect();

        // Line-buffered, as the utility's standard output is: one write per
        // line rather than per conversion, and diagnostics interleave with
        // the output exactly as they do for the utility.
        let mut stdout = LineWriter::new(BuiltinStdout(opened_files));
        let mut diag = report;
        match plib::printf::printf(&mut stdout, format.as_bytes(), &operands, &mut diag) {
            Ok(had_error) => Ok(i32::from(had_error)),
            Err(err) => {
                report(&err.to_string());
                Ok(1)
            }
        }
    }
}
