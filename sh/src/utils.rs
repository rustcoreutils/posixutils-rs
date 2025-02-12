use nix::libc;
use std::ffi::CStr;

pub fn strcoll(lhs: &CStr, rhs: &CStr) -> std::cmp::Ordering {
    // strings are valid, this is safe
    let ordering = unsafe { libc::strcoll(lhs.as_ptr(), rhs.as_ptr()) };
    if ordering < 0 {
        std::cmp::Ordering::Less
    } else if ordering == 0 {
        std::cmp::Ordering::Equal
    } else {
        std::cmp::Ordering::Greater
    }
}
