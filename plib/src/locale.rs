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

/// Opaque `mbstate_t`. The `libc` crate exposes `mbstate_t` on Linux but not on
/// macOS, so we declare our own buffer large enough for any supported platform's
/// layout (glibc: 8 bytes; macOS/Darwin: 128 bytes) with 8-byte alignment. A
/// freshly zeroed value is the documented initial conversion state; `mbrtowc`
/// only touches the bytes its own ABI defines, so over-sizing is safe.
#[repr(C, align(8))]
#[derive(Clone, Copy)]
struct MbStateT([u8; 128]);

impl MbStateT {
    fn zeroed() -> Self {
        MbStateT([0u8; 128])
    }
}

extern "C" {
    fn iswprint(c: WintT) -> libc::c_int;
    fn towlower(c: WintT) -> WintT;
    fn towupper(c: WintT) -> WintT;
    // Wide-character classification + display-width functions. The `libc`
    // crate does not surface all of these on every target (notably macOS), so
    // they are declared directly to match the `iswprint` precedent above.
    fn iswblank(c: WintT) -> libc::c_int;
    fn iswspace(c: WintT) -> libc::c_int;
    fn iswalpha(c: WintT) -> libc::c_int;
    fn iswalnum(c: WintT) -> libc::c_int;
    fn iswdigit(c: WintT) -> libc::c_int;
    fn iswpunct(c: WintT) -> libc::c_int;
    fn iswcntrl(c: WintT) -> libc::c_int;
    fn iswgraph(c: WintT) -> libc::c_int;
    fn iswxdigit(c: WintT) -> libc::c_int;
    fn wcwidth(c: libc::wchar_t) -> libc::c_int;
    // `mbrtowc` and `mbstate_t` are not surfaced by the `libc` crate on all
    // targets (notably macOS), so both are declared directly.
    fn mbrtowc(
        pwc: *mut libc::wchar_t,
        s: *const libc::c_char,
        n: libc::size_t,
        ps: *mut MbStateT,
    ) -> libc::size_t;
}

/// Number of column positions occupied by `c` under the current `LC_CTYPE`,
/// per libc `wcwidth(3)`.
///
/// Returns `1` for ordinary single-width printable characters, `2` for
/// double-width (e.g. East Asian wide) characters, `0` for zero-width
/// (combining) characters, and `-1` for non-printable characters (control
/// characters, or any character not representable in the current locale).
///
/// `setlocale(LC_ALL, "")` must have been called for non-ASCII characters to be
/// measured correctly; in the default `C` locale only ASCII has a defined width
/// and other codepoints return `-1`. Callers that track screen columns (e.g.
/// `expand`, `fold`, `unexpand`, `pr`) handle control characters such as
/// `<tab>`, `<backspace>`, and `<carriage-return>` separately and only consult
/// this for ordinary characters.
pub fn wcwidth_char(c: char) -> i32 {
    // SAFETY: wcwidth is thread-safe and side-effect-free; every Unicode
    // codepoint (max 0x10FFFF) fits losslessly in wchar_t (32-bit on all
    // supported targets).
    unsafe { wcwidth(c as u32 as libc::wchar_t) }
}

/// Generate a locale-aware character-class predicate that dispatches a
/// single-byte value (`code <= 0xFF`) to the libc `isX(3)` function and any
/// wider Unicode character to the corresponding `iswX(3)` function. Mirrors the
/// byte-vs-wide split used by [`isprint`].
macro_rules! ctype_predicate {
    ($(#[$meta:meta])* $name:ident, $byte_fn:path, $wide_fn:ident) => {
        $(#[$meta])*
        pub fn $name(c: char) -> bool {
            let code = c as u32;
            if code <= 0xFF {
                // SAFETY: the libc ctype function is thread-safe and
                // side-effect-free; the argument is in [0, 255].
                unsafe { $byte_fn(code as libc::c_int) != 0 }
            } else {
                // SAFETY: the libc wide-ctype function is thread-safe; `WintT`
                // matches the platform's wint_t and every Unicode codepoint
                // fits losslessly (the high bit is always clear).
                unsafe { $wide_fn(code as WintT) != 0 }
            }
        }
    };
}

ctype_predicate!(
    /// True if `c` is a blank (`<space>` or `<tab>` in the POSIX locale) under
    /// the current `LC_CTYPE`, per libc `isblank(3)`.
    isblank, libc::isblank, iswblank
);
ctype_predicate!(
    /// True if `c` is whitespace under the current `LC_CTYPE`, per libc
    /// `isspace(3)` (space, tab, newline, vertical tab, form feed, carriage
    /// return in the POSIX locale).
    isspace, libc::isspace, iswspace
);
ctype_predicate!(
    /// True if `c` is alphabetic under the current `LC_CTYPE`, per libc
    /// `isalpha(3)`.
    isalpha, libc::isalpha, iswalpha
);
ctype_predicate!(
    /// True if `c` is alphanumeric under the current `LC_CTYPE`, per libc
    /// `isalnum(3)`.
    isalnum, libc::isalnum, iswalnum
);
ctype_predicate!(
    /// True if `c` is a decimal digit under the current `LC_CTYPE`, per libc
    /// `isdigit(3)`.
    isdigit, libc::isdigit, iswdigit
);
ctype_predicate!(
    /// True if `c` is punctuation under the current `LC_CTYPE`, per libc
    /// `ispunct(3)`.
    ispunct, libc::ispunct, iswpunct
);
ctype_predicate!(
    /// True if `c` is a control character under the current `LC_CTYPE`, per
    /// libc `iscntrl(3)`.
    iscntrl, libc::iscntrl, iswcntrl
);
ctype_predicate!(
    /// True if `c` has a visible glyph (printable and not `<space>`) under the
    /// current `LC_CTYPE`, per libc `isgraph(3)`.
    isgraph, libc::isgraph, iswgraph
);
ctype_predicate!(
    /// True if `c` is a hexadecimal digit under the current `LC_CTYPE`, per
    /// libc `isxdigit(3)`.
    isxdigit, libc::isxdigit, iswxdigit
);

/// Map `c` to lowercase under the current `LC_CTYPE`.
///
/// ASCII characters go through libc `tolower(3)`; all other characters go
/// through `towlower(3)`. (A non-ASCII codepoint such as `É` is multi-byte in a
/// UTF-8 locale, so the byte-oriented `tolower(3)` could not map it.) Characters
/// with no mapping are returned unchanged.
pub fn to_lower(c: char) -> char {
    let mapped = if c.is_ascii() {
        // SAFETY: tolower is thread-safe; the argument is in [0, 127].
        unsafe { libc::tolower(c as libc::c_int) as u32 }
    } else {
        // SAFETY: towlower is thread-safe; every Unicode codepoint fits in WintT.
        unsafe { towlower(c as u32 as WintT) as u32 }
    };
    char::from_u32(mapped).unwrap_or(c)
}

/// Map `c` to uppercase under the current `LC_CTYPE`. See [`to_lower`].
pub fn to_upper(c: char) -> char {
    let mapped = if c.is_ascii() {
        // SAFETY: toupper is thread-safe; the argument is in [0, 127].
        unsafe { libc::toupper(c as libc::c_int) as u32 }
    } else {
        // SAFETY: towupper is thread-safe; every Unicode codepoint fits in WintT.
        unsafe { towupper(c as u32 as WintT) as u32 }
    };
    char::from_u32(mapped).unwrap_or(c)
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
    // Reject out-of-range timestamps explicitly. On supported targets
    // (Linux/macOS x86_64+aarch64) `time_t` is i64 and this conversion
    // always succeeds (clippy::useless_conversion fires here), but on a
    // 32-bit `time_t` target it correctly surfaces the Y2038-era overflow
    // instead of truncating silently.
    #[allow(clippy::useless_conversion)]
    let t: libc::time_t = epoch_secs
        .try_into()
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "timestamp out of range"))?;
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

/// Return `true` if `response` is an affirmative answer under the current `LC_MESSAGES`.
///
/// Uses the locale's `YESEXPR` extended regular expression (via libc `nl_langinfo(3)`); if that is
/// unavailable, empty, or fails to compile, falls back to matching a leading `y`/`Y`. Intended for
/// the interactive prompts of `cp`/`mv`/`rm` and similar utilities, replacing a hardcoded `y`
/// check so the locale's affirmative responses are honored (POSIX `LC_MESSAGES`).
///
/// `setlocale(LC_ALL, "")` must have been called for a non-`C` `YESEXPR` to take effect.
pub fn is_affirmative(response: &str) -> bool {
    use std::ffi::CStr;
    // SAFETY: nl_langinfo returns a pointer to a static, locale-owned string (or a valid empty
    // string); the bytes are copied immediately before any further locale call.
    let pattern = unsafe {
        let p = libc::nl_langinfo(libc::YESEXPR);
        if p.is_null() {
            None
        } else {
            CStr::from_ptr(p).to_str().ok().map(str::to_owned)
        }
    };
    let pattern = pattern.filter(|s| !s.is_empty());
    match pattern {
        Some(pat) => match crate::regex::Regex::ere(&pat) {
            Ok(re) => re.is_match(response),
            Err(_) => response.starts_with(['y', 'Y']),
        },
        None => response.starts_with(['y', 'Y']),
    }
}

/// Split `bytes` into multibyte characters under the current `LC_CTYPE`, each
/// returned as the sub-slice of bytes comprising one character.
///
/// `setlocale(LC_ALL, "")` must have been called for non-ASCII multibyte
/// encodings (e.g. UTF-8) to be recognized; in the default `C` locale every
/// byte is its own character. Invalid or incomplete byte sequences fall back to
/// a single byte so the function is total and never loses data.
///
/// Used by `m4` for character- (not byte-) oriented `len`, `index`, `substr`,
/// and `translit`, per POSIX `LC_CTYPE`.
pub fn mb_char_slices(bytes: &[u8]) -> Vec<&[u8]> {
    let mut result = Vec::new();
    // An all-zero mbstate_t is the documented initial conversion state.
    let mut state = MbStateT::zeroed();
    let mut i = 0;
    while i < bytes.len() {
        let remaining = &bytes[i..];
        // SAFETY: the pointer/length describe a valid slice, and `state` is a
        // live mbstate_t owned by this call. A null first argument means "do
        // not store the wide character", only report the byte count.
        let n = unsafe {
            mbrtowc(
                std::ptr::null_mut(),
                remaining.as_ptr() as *const libc::c_char,
                remaining.len() as libc::size_t,
                &mut state,
            )
        };
        let consumed = if n == 0 {
            // A NUL wide character occupies one byte.
            1
        } else if n == usize::MAX || n == usize::MAX - 1 {
            // (size_t)-1 (invalid sequence) or (size_t)-2 (incomplete at end of
            // input): consume one byte and reset the conversion state.
            state = MbStateT::zeroed();
            1
        } else {
            n
        };
        result.push(&bytes[i..i + consumed]);
        i += consumed;
    }
    result
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
    fn to_lower_upper_ascii() {
        assert_eq!(to_lower('A'), 'a');
        assert_eq!(to_lower('Z'), 'z');
        assert_eq!(to_upper('a'), 'A');
        assert_eq!(to_upper('z'), 'Z');
        // Non-alphabetic characters are unchanged.
        assert_eq!(to_lower('5'), '5');
        assert_eq!(to_upper('!'), '!');
        assert_eq!(to_lower('a'), 'a');
    }

    #[test]
    fn is_affirmative_basic() {
        // In the default C locale YESEXPR is `^[yY]`; the fallback also matches a leading y/Y.
        assert!(is_affirmative("y"));
        assert!(is_affirmative("Y"));
        assert!(is_affirmative("yes"));
        assert!(!is_affirmative("n"));
        assert!(!is_affirmative("no"));
        assert!(!is_affirmative(""));
        // A leading non-y is not affirmative even if a y appears later.
        assert!(!is_affirmative("maybe y"));
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
    fn mb_char_slices_ascii_is_one_byte_each() {
        // ASCII is single-byte in every locale.
        let slices = mb_char_slices(b"abc");
        assert_eq!(slices, vec![b"a".as_slice(), b"b", b"c"]);
        assert_eq!(mb_char_slices(b"").len(), 0);
    }

    // Serializes tests that mutate the process-global locale via setlocale, so
    // they cannot interleave with each other when the test harness runs in
    // parallel.
    static LOCALE_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    #[test]
    fn mb_char_slices_utf8_after_setlocale() {
        // Recover from a poisoned lock (a prior test panicked while holding it):
        // the guarded data is just (), so the lock is still usable.
        let _guard = LOCALE_LOCK.lock().unwrap_or_else(|e| e.into_inner());

        // Save the exact current locale so it can be restored afterwards
        // (setlocale(_, NULL) returns it; the string must be copied immediately
        // as the next setlocale call may invalidate it).
        let saved = unsafe { libc::setlocale(libc::LC_ALL, std::ptr::null()) };
        let saved =
            (!saved.is_null()).then(|| unsafe { std::ffi::CStr::from_ptr(saved) }.to_owned());

        let utf8 = std::ffi::CString::new("C.UTF-8").unwrap();
        let ok = unsafe { libc::setlocale(libc::LC_ALL, utf8.as_ptr()) };
        // "é" is U+00E9 = 0xC3 0xA9 in UTF-8. Only assert when a UTF-8 locale is
        // actually available (mirrors isprint_non_ascii_requires_setlocale).
        let slices = mb_char_slices("é".as_bytes());
        let matched = slices == vec![&[0xC3u8, 0xA9u8][..]];

        // Restore the precise prior locale before asserting (so a failure does
        // not leak the C.UTF-8 locale into other tests).
        if let Some(saved) = saved {
            unsafe { libc::setlocale(libc::LC_ALL, saved.as_ptr()) };
        }
        if !ok.is_null() {
            assert!(
                matched,
                "expected é to be one 2-byte character, got {slices:?}"
            );
        }
    }

    #[test]
    fn ctype_predicates_ascii_c_locale() {
        // blank = space + tab only
        assert!(isblank(' '));
        assert!(isblank('\t'));
        assert!(!isblank('\n'));
        assert!(!isblank('a'));
        // space = the six standard whitespace chars
        assert!(isspace(' '));
        assert!(isspace('\t'));
        assert!(isspace('\n'));
        assert!(isspace('\r'));
        assert!(!isspace('a'));
        // alpha / alnum / digit
        assert!(isalpha('a'));
        assert!(isalpha('Z'));
        assert!(!isalpha('5'));
        assert!(!isalpha(' '));
        assert!(isalnum('a'));
        assert!(isalnum('5'));
        assert!(!isalnum('!'));
        assert!(isdigit('0'));
        assert!(isdigit('9'));
        assert!(!isdigit('a'));
        // punct / cntrl / graph
        assert!(ispunct('!'));
        assert!(ispunct(','));
        assert!(!ispunct('a'));
        assert!(!ispunct(' '));
        assert!(iscntrl('\n'));
        assert!(iscntrl('\0'));
        assert!(!iscntrl('a'));
        assert!(isgraph('a'));
        assert!(isgraph('!'));
        assert!(!isgraph(' ')); // space is printable but not graph
        assert!(!isgraph('\n'));
        // xdigit
        assert!(isxdigit('0'));
        assert!(isxdigit('9'));
        assert!(isxdigit('a'));
        assert!(isxdigit('F'));
        assert!(!isxdigit('g'));
    }

    #[test]
    fn wcwidth_ascii() {
        // Ordinary printable ASCII is one column wide.
        assert_eq!(wcwidth_char('a'), 1);
        assert_eq!(wcwidth_char(' '), 1);
        assert_eq!(wcwidth_char('~'), 1);
        // Control characters are non-printable: width -1.
        assert_eq!(wcwidth_char('\t'), -1);
        assert_eq!(wcwidth_char('\n'), -1);
    }

    #[test]
    fn wcwidth_wide_after_setlocale() {
        // Recover from a poisoned lock (guarded data is just ()).
        let _guard = LOCALE_LOCK.lock().unwrap_or_else(|e| e.into_inner());

        let saved = unsafe { libc::setlocale(libc::LC_ALL, std::ptr::null()) };
        let saved =
            (!saved.is_null()).then(|| unsafe { std::ffi::CStr::from_ptr(saved) }.to_owned());

        let utf8 = std::ffi::CString::new("C.UTF-8").unwrap();
        let ok = unsafe { libc::setlocale(libc::LC_ALL, utf8.as_ptr()) };
        // U+4E16 (世) is a double-width CJK ideograph in a UTF-8 locale.
        let w = wcwidth_char('世');

        if let Some(saved) = saved {
            unsafe { libc::setlocale(libc::LC_ALL, saved.as_ptr()) };
        }
        if !ok.is_null() {
            assert_eq!(w, 2, "expected 世 to be 2 columns wide in a UTF-8 locale");
        }
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
