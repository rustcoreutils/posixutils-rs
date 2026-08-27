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

/// `true` — do nothing, successfully. POSIX XCU lists it as a utility, and
/// every shell builds it in: a loop that calls it should not fork.
pub struct True;

impl BuiltinUtility for True {
    fn exec(&self, _: &[ShString], _: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
        Ok(0)
    }
}

/// `false` — do nothing, unsuccessfully.
pub struct False;

impl BuiltinUtility for False {
    fn exec(&self, _: &[ShString], _: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
        Ok(1)
    }
}
