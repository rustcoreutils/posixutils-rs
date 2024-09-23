//
// Copyright (c) 2024 fox0
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::{CStr, CString};

#[inline(always)]
pub unsafe fn to_cstring(s: *mut libc::c_char) -> CString {
    debug_assert!(!s.is_null());
    let s = CStr::from_ptr(s);
    let s = CString::from(s);
    s
}
