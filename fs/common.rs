//
// Copyright (c) 2024 fox0
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::{CStr, CString};

pub trait ToCString {
    unsafe fn to_cstring(&self) -> CString;
}

impl ToCString for *mut libc::c_char {
    #[inline(always)]
    unsafe fn to_cstring(&self) -> CString {
        debug_assert!(!self.is_null());
        let s = CStr::from_ptr(*self);
        let s = CString::from(s);
        s
    }
}

impl ToCString for &[libc::c_char] {
    #[inline(always)]
    unsafe fn to_cstring(&self) -> CString {
        let s = CStr::from_ptr(self.as_ptr());
        let s = CString::from(s);
        s
    }
}
