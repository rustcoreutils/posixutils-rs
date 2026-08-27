//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
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
use gettextrs::gettext;
use plib::test_expr::{eval_posix_strict, EvalResult};

/// `test` and `[`.
///
/// Built in rather than forked: a conditional is the most frequently executed
/// command in a shell script, and `while [ ... ]` that spawns a process per
/// iteration makes every loop pay for a fork. The expression evaluator itself
/// lives in `plib::test_expr`, shared with the standalone utility so the two
/// cannot drift apart.
pub struct Test {
    /// `[` requires a closing `]`; `test` must not have one.
    pub requires_closing_bracket: bool,
}

impl BuiltinUtility for Test {
    fn exec(
        &self,
        args: &[ShString],
        _: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        // Operands keep their bytes: a path or a compared string need not be
        // text, and converting lossily made two distinct values compare equal
        // and file tests probe the wrong path.
        let mut args: Vec<Vec<u8>> = args.iter().map(|a| a.as_bytes().to_vec()).collect();
        if self.requires_closing_bracket {
            if args.last().map(Vec::as_slice) != Some(b"]".as_slice()) {
                // A usage error, not a false expression: POSIX distinguishes
                // them by exiting greater than 1.
                opened_files.write_err(format!("[: {}\n", gettext("missing closing bracket")));
                return Ok(2);
            }
            args.pop();
        }
        match eval_posix_strict(&args) {
            EvalResult::True => Ok(0),
            EvalResult::False => Ok(1),
            EvalResult::Error(message) => {
                // POSIX: a `test` usage error exits >1, distinguishing it from
                // an expression that is merely false.
                opened_files.write_err(format!("test: {message}\n"));
                Ok(2)
            }
        }
    }
}
