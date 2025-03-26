//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{skip_option_terminator, BuiltinResult, SpecialBuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

pub struct Eval;

impl SpecialBuiltinUtility for Eval {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let args = skip_option_terminator(args);
        let program = args.join(" ");

        std::mem::swap(&mut shell.opened_files, opened_files);
        let execution_result = shell.execute_program(&program);
        std::mem::swap(&mut shell.opened_files, opened_files);

        execution_result
            .map_err(|err| format!("eval: parsing error({}): {}", err.lineno, err.message).into())
    }
}
