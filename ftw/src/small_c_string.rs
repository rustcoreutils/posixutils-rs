// SPDX-License-Identifier: MIT

//! Copied from [`std::sys::common::small_c_string`]

use std::ffi::{CStr, CString};
use std::mem::MaybeUninit;
use std::path::Path;
use std::{io, ptr, slice};

// Make sure to stay under 4096 so the compiler doesn't insert a probe frame:
// https://docs.rs/compiler_builtins/latest/compiler_builtins/probestack/index.html
#[cfg(not(target_os = "espidf"))]
const MAX_STACK_ALLOCATION: usize = 384;
#[cfg(target_os = "espidf")]
const MAX_STACK_ALLOCATION: usize = 32;

// const NUL_ERR: io::Error =
//     io::const_io_error!(io::ErrorKind::InvalidInput, "file name contained an unexpected NUL byte");

#[inline]
pub fn run_path_with_cstr<T>(path: &Path, f: &dyn Fn(&CStr) -> io::Result<T>) -> io::Result<T> {
    run_with_cstr(path.as_os_str().as_encoded_bytes(), f)
}

#[inline]
pub fn run_with_cstr<T>(bytes: &[u8], f: &dyn Fn(&CStr) -> io::Result<T>) -> io::Result<T> {
    // Dispatch and dyn erase the closure type to prevent mono bloat.
    // See https://github.com/rust-lang/rust/pull/121101.
    if bytes.len() >= MAX_STACK_ALLOCATION {
        run_with_cstr_allocating(bytes, f)
    } else {
        unsafe { run_with_cstr_stack(bytes, f) }
    }
}

/// # Safety
///
/// `bytes` must have a length less than `MAX_STACK_ALLOCATION`.
unsafe fn run_with_cstr_stack<T>(
    bytes: &[u8],
    f: &dyn Fn(&CStr) -> io::Result<T>,
) -> io::Result<T> {
    let mut buf = MaybeUninit::<[u8; MAX_STACK_ALLOCATION]>::uninit();
    let buf_ptr = buf.as_mut_ptr() as *mut u8;

    unsafe {
        ptr::copy_nonoverlapping(bytes.as_ptr(), buf_ptr, bytes.len());
        buf_ptr.add(bytes.len()).write(0);
    }

    match CStr::from_bytes_with_nul(unsafe { slice::from_raw_parts(buf_ptr, bytes.len() + 1) }) {
        Ok(s) => f(s),
        Err(_) => Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            "file name contained an unexpected NUL byte",
        )),
    }
}

#[cold]
#[inline(never)]
fn run_with_cstr_allocating<T>(bytes: &[u8], f: &dyn Fn(&CStr) -> io::Result<T>) -> io::Result<T> {
    match CString::new(bytes) {
        Ok(s) => f(&s),
        Err(_) => Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            "file name contained an unexpected NUL byte",
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run_path_with_cstr() {
        let file = "tests/not_exist";
        let result = run_path_with_cstr(file.as_ref(), &|p| {
            assert_eq!(p, c"tests/not_exist");
            Ok(())
        });
        assert!(result.is_ok());
    }

    // #[coverage(off)]
    #[test]
    fn test_run_path_with_cstr_nul() {
        let file = "tests\0not_exist";
        let result = run_path_with_cstr(file.as_ref(), &|_p| Ok(()));
        assert!(result.is_err());
        let result = result.unwrap_err();
        assert_eq!(result.kind(), std::io::ErrorKind::InvalidInput);
    }

    #[test]
    fn test_run_path_with_cstr_alloc() {
        let file = "e".repeat(MAX_STACK_ALLOCATION + 1);
        let result = run_path_with_cstr(file.as_ref(), &|p| {
            assert_eq!(p, CString::new(file.as_str()).unwrap().as_c_str());
            Ok(())
        });
        assert!(result.is_ok());
    }

    // #[coverage(off)]
    #[test]
    fn test_run_path_with_cstr_alloc_nul() {
        let file = "\0".repeat(MAX_STACK_ALLOCATION + 1);
        let result = run_path_with_cstr(file.as_ref(), &|_p| Ok(()));
        assert!(result.is_err());
        let result = result.unwrap_err();
        assert_eq!(result.kind(), std::io::ErrorKind::InvalidInput);
    }
}
