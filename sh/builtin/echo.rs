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
use plib::echo::echo_output;

/// `echo`.
///
/// Built in, as in dash and bash, so that a script does not fork a process
/// for every line it prints.  The output is computed by `plib::echo`, shared
/// with the standalone utility so the two cannot drift apart.  A failed write
/// is reported by the caller (`report_write_error`).
pub struct Echo;

impl BuiltinUtility for Echo {
    fn exec(
        &self,
        args: &[ShString],
        _: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        opened_files.write_out(echo_output(args));
        Ok(0)
    }
}
