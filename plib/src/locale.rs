//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Locale-aware shims around libc's character / collation / time functions.
//!
//! `setlocale(LC_ALL, "")` must have been called for these to honor `LC_CTYPE`,
//! `LC_COLLATE`, `LC_TIME`, and `TZ` (see [`crate::diag::init_locale`]).
//!
//! Used by:
//!
//! - `strings` — [`isprint`] decides what counts as a printable byte under
//!   the current `LC_CTYPE`; replaces the previous `char::is_whitespace`
//!   heuristic (which incorrectly accepted `\n`).
//! - `ar` `-tv` — [`strftime`] formats archive member mtimes under `LC_TIME`
//!   and `TZ`, replacing chrono's locale-blind UTC formatter.
//! - `nm` — [`strcoll`] enables the default symbol-name sort to follow
//!   `LC_COLLATE` (consumed when nm's sort lands).

use std::ffi::CString;
use std::io;

// libc-rs doesn't surface `wint_t` for Linux or macOS targets (only for
// teeos / hurd), so we mirror the platform's underlying `wint_t` choice
// here. On glibc/musl: `unsigned int`. On Darwin: `int`. Both are 32-bit
// integers on x86_64/aarch64; the ABI for register-passing is identical,
// but we still match signedness for strict type correctness.
#[cfg(target_vendor = "apple")]
type WintT = libc::c_int;
#[cfg(not(target_vendor = "apple"))]
type WintT = libc::c_uint;

extern "C" {
    fn iswprint(c: WintT) -> libc::c_int;
}

/// True if `c` is printable under the current `LC_CTYPE`.
///
/// For single-byte values (`c <= 0xFF`) this calls libc `isprint(3)`. For
/// multi-byte / non-ASCII Unicode characters it calls libc `iswprint(3)`.
/// In the POSIX `C` locale: ASCII space and printable graph characters are
/// printable; control characters (including `\n`, `\r`, `\t`, NUL) are not.
pub fn isprint(c: char) -> bool {
    let code = c as u32;
    if code <= 0xFF {
        // SAFETY: libc::isprint is thread-safe and side-effect-free with
        // any int input; we pass a value in [0, 255].
        unsafe { libc::isprint(code as libc::c_int) != 0 }
    } else {
        // SAFETY: iswprint is thread-safe; `WintT` matches the platform's
        // wint_t (32-bit unsigned on glibc/musl, 32-bit signed on Darwin),
        // and every Unicode codepoint (max 0x10FFFF) fits losslessly in
        // both representations because the high bit is always clear.
        unsafe { iswprint(code as WintT) != 0 }
    }
}

/// Compare two strings using `LC_COLLATE`.
///
/// Returns `Less`, `Equal`, or `Greater` per libc `strcoll(3)`. If either
/// argument contains an interior NUL byte, falls back to byte-wise comparison
/// (since `strcoll` can't accept NULs).
pub fn strcoll(a: &str, b: &str) -> std::cmp::Ordering {
    let (ca, cb) = match (CString::new(a), CString::new(b)) {
        (Ok(ca), Ok(cb)) => (ca, cb),
        _ => return a.cmp(b),
    };
    // SAFETY: both CStrings own their bytes for the duration of the call.
    let cmp = unsafe { libc::strcoll(ca.as_ptr(), cb.as_ptr()) };
    cmp.cmp(&0)
}

/// Format a unix epoch timestamp using libc `strftime(3)`.
///
/// `fmt` is the strftime conversion string (e.g. `"%b %e %H:%M %Y"`). The
/// time is interpreted in the local timezone via `localtime_r(3)`, so `TZ`
/// (and `LC_TIME` for month/day names) take effect.
///
/// Returns an `io::Error` if `fmt` contains an interior NUL byte or if
/// `localtime_r` reports failure.
pub fn strftime(fmt: &str, epoch_secs: i64) -> io::Result<String> {
    let cfmt = CString::new(fmt).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;

    // SAFETY: tm is a POD struct that we initialize via localtime_r; the
    // returned pointer is either &raw mut tm or null, which we check.
    let mut tm: libc::tm = unsafe { std::mem::zeroed() };
    let t: libc::time_t = epoch_secs as libc::time_t;
    let ret = unsafe { libc::localtime_r(&t, &mut tm) };
    if ret.is_null() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "localtime_r failed",
        ));
    }

    // libc strftime writes up to `maxsize - 1` bytes plus a NUL; if the
    // output exactly fills the buffer it can return 0 and leave the buffer
    // contents unspecified. Start with a generous buffer; grow on 0 return.
    let mut buf = vec![0u8; 256];
    loop {
        // SAFETY: buf is sized appropriately; cfmt and tm are valid.
        let n = unsafe {
            libc::strftime(
                buf.as_mut_ptr() as *mut libc::c_char,
                buf.len(),
                cfmt.as_ptr(),
                &tm,
            )
        };
        if n > 0 {
            buf.truncate(n);
            return String::from_utf8(buf)
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e));
        }
        // n == 0 means the buffer was too small (or the format expanded to
        // 0 bytes, which we treat the same way — grow and retry).
        if buf.len() >= 65536 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "strftime: output exceeds 64 KiB",
            ));
        }
        buf.resize(buf.len() * 2, 0);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn isprint_ascii_printable() {
        assert!(isprint('a'));
        assert!(isprint('Z'));
        assert!(isprint(' '));
        assert!(isprint('5'));
        assert!(isprint('!'));
    }

    #[test]
    fn isprint_ascii_control() {
        assert!(!isprint('\n'));
        assert!(!isprint('\r'));
        assert!(!isprint('\t'));
        assert!(!isprint('\0'));
        assert!(!isprint('\x7f')); // DEL
    }

    #[test]
    fn strcoll_basic_ordering() {
        use std::cmp::Ordering;
        assert_eq!(strcoll("aa", "ab"), Ordering::Less);
        assert_eq!(strcoll("ab", "aa"), Ordering::Greater);
        assert_eq!(strcoll("foo", "foo"), Ordering::Equal);
    }

    #[test]
    fn strcoll_interior_nul_falls_back() {
        use std::cmp::Ordering;
        // CString::new rejects interior NUL; we should still return *some*
        // sensible ordering (byte-wise here) rather than panic.
        let a = "ab";
        let b = "a\0b";
        // Whatever ordering libc would have produced is replaced by byte-wise
        // ordering; just verify the call doesn't panic and returns something
        // consistent across runs.
        let r = strcoll(a, b);
        assert_eq!(r, a.cmp(b));
        // Sanity: b has a NUL in the middle which is byte 0; "ab" > "a\0b"
        // because byte 'b' > byte 0.
        assert_eq!(r, Ordering::Greater);
    }

    #[test]
    fn strftime_year_at_epoch() {
        let s = strftime("%Y", 0).unwrap();
        // Unix epoch is 1970 in any sane timezone; the year string must
        // contain "1970" or "1969" (in negative-UTC timezones the local
        // date wraps back). Either is acceptable.
        assert!(s == "1970" || s == "1969", "got {}", s);
    }

    #[test]
    fn strftime_nul_in_format_errors() {
        assert!(strftime("%Y\0%m", 0).is_err());
    }

    #[test]
    fn isprint_non_ascii_requires_setlocale() {
        // In the absence of an explicit setlocale("") call the process
        // typically runs in the "C" locale where iswprint rejects non-ASCII.
        // After calling setlocale, a UTF-8 locale should accept Cyrillic.
        // We don't assert the exact outcome (it depends on the env's LANG)
        // but we exercise the path so any panic surfaces.
        let _ = isprint('З');
        let _ = isprint('🦀');
    }
}
