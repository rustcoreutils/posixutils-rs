//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::CStr;
use std::fmt::Display;

pub fn strcoll(lhs: &CStr, rhs: &CStr) -> std::cmp::Ordering {
    // strings are valid, this is safe
    let ordering = unsafe { libc::strcoll(lhs.as_ptr(), rhs.as_ptr()) };
    ordering.cmp(&0)
}
