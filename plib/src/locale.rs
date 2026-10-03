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
//!
//! # Windows
//!
//! The MSVC CRT cannot stand in for the Unix C library here: its `wchar_t` is
//! 16 bits, so a character outside the Basic Multilingual Plane is not one
//! wide character, and it has no `wcwidth`. On Windows the character functions
//! therefore answer for themselves, in one of two modes that
//! [`crate::diag::init_locale`] picks from `LC_CTYPE` as POSIX resolves it
//! (see [`CtypeMode`]); a program starts in the C locale's, as on Unix. Each
//! public function keeps its signature and its contract, so callers are
//! unchanged:
//!
//! - ASCII answers exactly as the POSIX locale does on Unix, in both modes.
//! - In the C locale (`LC_CTYPE` is `C` or `POSIX`), nothing above ASCII
//!   belongs to any class, has a case mapping or a width, and every byte is
//!   its own character -- one above ASCII undecodable -- as in glibc's C
//!   locale.
//! - Otherwise (`C.UTF-8`, or any other locale) above ASCII each predicate
//!   takes the closest Unicode property (each function's documentation names
//!   it), and byte input decodes as UTF-8, an invalid or incomplete sequence
//!   decoding as a single byte -- the same fallback the Unix functions
//!   document for a byte `mbrtowc(3)` rejects.
//!
//! Every public function splits ASCII from the rest the same way on both
//! platforms; only the private helpers that answer each half are per-platform.
//! [`strftime`] formats with [`crate::timefmt`]: `TZ` as the C runtime reads
//! it, and the POSIX locale's names, `LC_TIME` having no Windows meaning.

use std::ffi::CString;
#[cfg(unix)]
use std::io;
#[cfg(windows)]
use std::sync::atomic::{AtomicBool, Ordering};

/// How the Windows character functions read text: the `LC_CTYPE` category's
/// meaning there. Unix asks the C library, whose locale already says this.
#[cfg(windows)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum CtypeMode {
    /// The C (POSIX) locale: ASCII classification and case mapping only, and
    /// every byte one character.
    C,
    /// Unicode classification and case mapping, text decoded as UTF-8.
    Unicode,
}

/// Whether [`CtypeMode::Unicode`] is in effect. A program starts in the C
/// locale, as on Unix, until [`crate::diag::init_locale`] reads `LC_CTYPE`.
#[cfg(windows)]
static UNICODE_CTYPE: AtomicBool = AtomicBool::new(false);

/// Select how the character functions read text; [`crate::diag::init_locale`]
/// calls it once, and tests to exercise each mode.
#[cfg(windows)]
pub(crate) fn set_ctype_mode(mode: CtypeMode) {
    UNICODE_CTYPE.store(mode == CtypeMode::Unicode, Ordering::Relaxed);
}

/// The mode [`set_ctype_mode`] last selected.
#[cfg(windows)]
pub(crate) fn ctype_mode() -> CtypeMode {
    if UNICODE_CTYPE.load(Ordering::Relaxed) {
        CtypeMode::Unicode
    } else {
        CtypeMode::C
    }
}

// libc-rs doesn't surface `wint_t` for Linux or macOS targets (only for
// teeos / hurd), so we mirror the platform's underlying `wint_t` choice
// here. On glibc/musl: `unsigned int`. On Darwin: `int`. Both are 32-bit
// integers on x86_64/aarch64; the ABI for register-passing is identical,
// but we still match signedness for strict type correctness.
#[cfg(target_vendor = "apple")]
type WintT = libc::c_int;
#[cfg(all(unix, not(target_vendor = "apple")))]
type WintT = libc::c_uint;

/// Opaque `mbstate_t`. The `libc` crate exposes `mbstate_t` on Linux but not on
/// macOS, so we declare our own buffer large enough for any supported platform's
/// layout (glibc: 8 bytes; macOS/Darwin: 128 bytes) with 8-byte alignment. A
/// freshly zeroed value is the documented initial conversion state; `mbrtowc`
/// only touches the bytes its own ABI defines, so over-sizing is safe.
#[cfg(unix)]
#[repr(C, align(8))]
#[derive(Clone, Copy)]
struct MbStateT([u8; 128]);

#[cfg(unix)]
impl MbStateT {
    fn zeroed() -> Self {
        MbStateT([0u8; 128])
    }
}

#[cfg(unix)]
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
    fn iswlower(c: WintT) -> libc::c_int;
    fn iswupper(c: WintT) -> libc::c_int;
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
///
/// On Windows: NUL is 0 and any other control character -1, as `wcwidth`
/// answers, and in the C locale so is anything above ASCII; otherwise the
/// zero-width combining blocks, zero-width spaces and joiners,
/// and variation selectors are 0; East Asian Wide and Fullwidth characters are
/// 2; everything else is 1. Rust's standard library cannot tell a combining
/// mark, so one outside the dedicated combining blocks (e.g. a Devanagari
/// vowel sign) counts as 1 column.
pub fn wcwidth_char(c: char) -> i32 {
    #[cfg(unix)]
    {
        // SAFETY: wcwidth is thread-safe and side-effect-free; every Unicode
        // codepoint (max 0x10FFFF) fits losslessly in wchar_t (32-bit on all
        // supported Unix targets).
        unsafe { wcwidth(c as u32 as libc::wchar_t) }
    }
    #[cfg(windows)]
    {
        if c == '\0' {
            0
        } else if c.is_control() || (!c.is_ascii() && ctype_mode() == CtypeMode::C) {
            -1
        } else if in_ranges(c, ZERO_WIDTH) {
            0
        } else if in_ranges(c, DOUBLE_WIDTH) {
            2
        } else {
            1
        }
    }
}

/// Zero-width characters on Windows: the blocks that hold only combining marks
/// (Combining Diacritical Marks, its Extended and Supplement blocks, the
/// Combining Marks for Symbols, and Combining Half Marks), the zero-width
/// space, joiners and direction marks, and the variation selectors.
#[cfg(windows)]
const ZERO_WIDTH: &[(char, char)] = &[
    ('\u{0300}', '\u{036F}'),
    ('\u{1AB0}', '\u{1AFF}'),
    ('\u{1DC0}', '\u{1DFF}'),
    ('\u{200B}', '\u{200F}'),
    ('\u{20D0}', '\u{20FF}'),
    ('\u{FE00}', '\u{FE0F}'),
    ('\u{FE20}', '\u{FE2F}'),
];

/// Double-width characters on Windows: East Asian Wide and Fullwidth, as
/// Markus Kuhn's reference `wcwidth` gives them -- Hangul Jamo initials, the
/// angle brackets, CJK radicals through Yi (less U+303F, which is narrow),
/// Hangul syllables, CJK compatibility ideographs, the vertical, compatibility
/// and fullwidth forms, the supplementary ideographic planes -- plus the two
/// main pictographic emoji blocks.
#[cfg(windows)]
const DOUBLE_WIDTH: &[(char, char)] = &[
    ('\u{1100}', '\u{115F}'),
    ('\u{2329}', '\u{232A}'),
    ('\u{2E80}', '\u{303E}'),
    ('\u{3040}', '\u{A4CF}'),
    ('\u{AC00}', '\u{D7A3}'),
    ('\u{F900}', '\u{FAFF}'),
    ('\u{FE10}', '\u{FE19}'),
    ('\u{FE30}', '\u{FE6F}'),
    ('\u{FF00}', '\u{FF60}'),
    ('\u{FFE0}', '\u{FFE6}'),
    ('\u{1F300}', '\u{1F64F}'),
    ('\u{1F900}', '\u{1F9FF}'),
    ('\u{20000}', '\u{2FFFD}'),
    ('\u{30000}', '\u{3FFFD}'),
];

/// True if `c` falls in one of the inclusive `ranges`.
#[cfg(windows)]
fn in_ranges(c: char, ranges: &[(char, char)]) -> bool {
    ranges.iter().any(|&(lo, hi)| (lo..=hi).contains(&c))
}

/// Answer a character-class question for an ASCII character. On Unix this is
/// the libc `isX(3)` function under `LC_CTYPE`; on Windows it is the POSIX
/// locale's answer, given as a `fn(u8) -> bool`.
#[cfg(unix)]
macro_rules! ascii_class {
    ($c:expr, $byte_fn:path, $ascii:expr) => {
        // SAFETY: the libc ctype function is thread-safe and side-effect-free;
        // the argument is in [0, 127].
        unsafe { $byte_fn($c as libc::c_int) != 0 }
    };
}
#[cfg(windows)]
macro_rules! ascii_class {
    ($c:expr, $byte_fn:path, $ascii:expr) => {
        ($ascii)($c as u8)
    };
}

/// Answer a character-class question for a character above ASCII. On Unix
/// this is the libc `iswX(3)` function under `LC_CTYPE`; on Windows it is the
/// closest Unicode property, given as a `fn(char) -> bool`, and in the C
/// locale "no".
#[cfg(unix)]
macro_rules! wide_class {
    ($c:expr, $wide_fn:ident, $unicode:expr) => {
        // SAFETY: the libc wide-ctype function is thread-safe; `WintT` matches
        // the platform's wint_t (32-bit unsigned on glibc/musl, 32-bit signed
        // on Darwin) and every Unicode codepoint (max 0x10FFFF) fits losslessly
        // in both because the high bit is always clear.
        unsafe { $wide_fn($c as u32 as WintT) != 0 }
    };
}
#[cfg(windows)]
macro_rules! wide_class {
    ($c:expr, $wide_fn:ident, $unicode:expr) => {
        ctype_mode() == CtypeMode::Unicode && ($unicode)($c)
    };
}

/// Generate a locale-aware character-class predicate that sends an ASCII
/// character to `ascii_class!` and anything wider to `wide_class!`.
///
/// The split is at ASCII, not at `<= 0xFF`: U+00E9 is two bytes in UTF-8, so
/// the byte-oriented `isalpha(0xE9)` cannot classify it and answers "no". This
/// matches the `is_ascii()` split `to_lower`/`to_upper` use.
macro_rules! ctype_predicate {
    (
        $(#[$meta:meta])* $name:ident,
        unix: $byte_fn:path, $wide_fn:ident;
        windows: $ascii:expr, $unicode:expr $(;)?
    ) => {
        $(#[$meta])*
        pub fn $name(c: char) -> bool {
            if c.is_ascii() {
                ascii_class!(c, $byte_fn, $ascii)
            } else {
                wide_class!(c, $wide_fn, $unicode)
            }
        }
    };
}

ctype_predicate!(
    /// True if `c` is a blank (`<space>` or `<tab>` in the POSIX locale) under
    /// the current `LC_CTYPE`, per libc `isblank(3)`.
    ///
    /// On Windows, above ASCII: a Unicode space separator (category Zs) other
    /// than the no-break spaces U+00A0, U+2007 and U+202F, as glibc's UTF-8
    /// locales answer.
    isblank,
    unix: libc::isblank, iswblank;
    windows: |b: u8| b == b' ' || b == b'\t', unicode_blank
);
ctype_predicate!(
    /// True if `c` is whitespace under the current `LC_CTYPE`, per libc
    /// `isspace(3)` (space, tab, newline, vertical tab, form feed, carriage
    /// return in the POSIX locale).
    ///
    /// On Windows, above ASCII: Unicode `White_Space` (`char::is_whitespace`)
    /// other than the no-break spaces U+00A0, U+2007 and U+202F, as glibc's
    /// UTF-8 locales answer.
    isspace,
    unix: libc::isspace, iswspace;
    windows: |b: u8| matches!(b, b' ' | b'\t'..=b'\r'), unicode_space
);
ctype_predicate!(
    /// True if `c` is alphabetic under the current `LC_CTYPE`, per libc
    /// `isalpha(3)`.
    ///
    /// On Windows, above ASCII: Unicode `Alphabetic` or `Numeric`
    /// (`char::is_alphabetic`, `char::is_numeric`). Digits of other scripts
    /// count as alphabetic because the `digit` class is ASCII-only, and that
    /// keeps `alnum` the union of `alpha` and `digit`; glibc does the same.
    isalpha,
    unix: libc::isalpha, iswalpha;
    windows: |b: u8| b.is_ascii_alphabetic(), unicode_alpha
);
ctype_predicate!(
    /// True if `c` is alphanumeric under the current `LC_CTYPE`, per libc
    /// `isalnum(3)`.
    ///
    /// On Windows, above ASCII: the same as [`isalpha`], since [`isdigit`] is
    /// ASCII-only.
    isalnum,
    unix: libc::isalnum, iswalnum;
    windows: |b: u8| b.is_ascii_alphanumeric(), unicode_alpha
);
ctype_predicate!(
    /// True if `c` is a decimal digit under the current `LC_CTYPE`, per libc
    /// `isdigit(3)`.
    ///
    /// On Windows, false above ASCII: POSIX defines the `digit` class as
    /// `0`..`9` only.
    isdigit,
    unix: libc::isdigit, iswdigit;
    windows: |b: u8| b.is_ascii_digit(), |_: char| false
);
ctype_predicate!(
    /// True if `c` is punctuation under the current `LC_CTYPE`, per libc
    /// `ispunct(3)`.
    ///
    /// On Windows, above ASCII: [`isgraph`] and not [`isalnum`], which takes in
    /// symbols as well as punctuation, as POSIX defines the class and glibc
    /// answers.
    ispunct,
    unix: libc::ispunct, iswpunct;
    windows: |b: u8| b.is_ascii_punctuation(), unicode_punct
);
ctype_predicate!(
    /// True if `c` is a control character under the current `LC_CTYPE`, per
    /// libc `iscntrl(3)`.
    ///
    /// On Windows, above ASCII: Unicode category Cc (`char::is_control`), which
    /// is U+0080..U+009F.
    iscntrl,
    unix: libc::iscntrl, iswcntrl;
    windows: |b: u8| b.is_ascii_control(), char::is_control
);
ctype_predicate!(
    /// True if `c` has a visible glyph (printable and not `<space>`) under the
    /// current `LC_CTYPE`, per libc `isgraph(3)`.
    ///
    /// On Windows, above ASCII: [`isprint`] and not [`isspace`].
    isgraph,
    unix: libc::isgraph, iswgraph;
    windows: |b: u8| b.is_ascii_graphic(), unicode_graph
);
ctype_predicate!(
    /// True if `c` is a hexadecimal digit under the current `LC_CTYPE`, per
    /// libc `isxdigit(3)`.
    ///
    /// On Windows, false above ASCII: POSIX defines the `xdigit` class as
    /// `0`..`9`, `A`..`F` and `a`..`f` only.
    isxdigit,
    unix: libc::isxdigit, iswxdigit;
    windows: |b: u8| b.is_ascii_hexdigit(), |_: char| false
);
ctype_predicate!(
    /// True if `c` is a lowercase letter under the current `LC_CTYPE`, per libc
    /// `islower(3)`.
    ///
    /// On Windows, above ASCII: Unicode `Lowercase` (`char::is_lowercase`).
    islower,
    unix: libc::islower, iswlower;
    windows: |b: u8| b.is_ascii_lowercase(), char::is_lowercase
);
ctype_predicate!(
    /// True if `c` is an uppercase letter under the current `LC_CTYPE`, per libc
    /// `isupper(3)`.
    ///
    /// On Windows, above ASCII: Unicode `Uppercase` (`char::is_uppercase`).
    isupper,
    unix: libc::isupper, iswupper;
    windows: |b: u8| b.is_ascii_uppercase(), char::is_uppercase
);
ctype_predicate!(
    /// True if `c` is printable under the current `LC_CTYPE`.
    ///
    /// ASCII calls libc `isprint(3)`; anything wider calls libc `iswprint(3)`.
    /// In the POSIX `C` locale: ASCII space and printable graph characters are
    /// printable; control characters (including `\n`, `\r`, `\t`, NUL) are not.
    ///
    /// On Windows, above ASCII: anything that is not a control character
    /// (`!char::is_control`).
    isprint,
    unix: libc::isprint, iswprint;
    windows: |b: u8| matches!(b, b' '..=b'~'), |c: char| !c.is_control()
);

/// The no-break spaces, which are Unicode `White_Space` but not in glibc's
/// `space` or `blank` classes.
#[cfg(windows)]
fn is_no_break_space(c: char) -> bool {
    matches!(c, '\u{00A0}' | '\u{2007}' | '\u{202F}')
}

#[cfg(windows)]
fn unicode_space(c: char) -> bool {
    c.is_whitespace() && !is_no_break_space(c)
}

/// `unicode_space` less the line-breaking characters NEL, LINE SEPARATOR
/// and PARAGRAPH SEPARATOR, leaving the horizontal space separators.
#[cfg(windows)]
fn unicode_blank(c: char) -> bool {
    unicode_space(c) && !matches!(c, '\u{0085}' | '\u{2028}' | '\u{2029}')
}

#[cfg(windows)]
fn unicode_alpha(c: char) -> bool {
    c.is_alphabetic() || c.is_numeric()
}

#[cfg(windows)]
fn unicode_graph(c: char) -> bool {
    !c.is_control() && !unicode_space(c)
}

#[cfg(windows)]
fn unicode_punct(c: char) -> bool {
    unicode_graph(c) && !unicode_alpha(c)
}

/// Map `c` to lowercase under the current `LC_CTYPE`.
///
/// ASCII characters go through libc `tolower(3)`; all other characters go
/// through `towlower(3)`. (A non-ASCII codepoint such as `É` is multi-byte in a
/// UTF-8 locale, so the byte-oriented `tolower(3)` could not map it.) Characters
/// with no mapping are returned unchanged.
///
/// On Windows, above ASCII: unchanged in the C locale; otherwise
/// `char::to_lowercase` when it maps `c` to a single character; a character
/// whose lowercase is several characters (U+0130 is `i` plus a combining dot)
/// has no one-character mapping and is unchanged, as `towlower` leaves it.
pub fn to_lower(c: char) -> char {
    if c.is_ascii() {
        lower_ascii(c)
    } else {
        lower_wide(c)
    }
}

/// Map `c` to uppercase under the current `LC_CTYPE`. See [`to_lower`].
///
/// On Windows, above ASCII: unchanged in the C locale; otherwise
/// `char::to_uppercase` when it maps `c` to a single character, else
/// unchanged (U+00DF `ß` uppercases to `SS`, so it stays).
pub fn to_upper(c: char) -> char {
    if c.is_ascii() {
        upper_ascii(c)
    } else {
        upper_wide(c)
    }
}

#[cfg(unix)]
fn lower_ascii(c: char) -> char {
    // SAFETY: tolower is thread-safe; the argument is in [0, 127].
    let mapped = unsafe { libc::tolower(c as libc::c_int) as u32 };
    char::from_u32(mapped).unwrap_or(c)
}

#[cfg(unix)]
fn lower_wide(c: char) -> char {
    // SAFETY: towlower is thread-safe; every Unicode codepoint fits in WintT.
    let mapped = unsafe { towlower(c as u32 as WintT) as u32 };
    char::from_u32(mapped).unwrap_or(c)
}

#[cfg(unix)]
fn upper_ascii(c: char) -> char {
    // SAFETY: toupper is thread-safe; the argument is in [0, 127].
    let mapped = unsafe { libc::toupper(c as libc::c_int) as u32 };
    char::from_u32(mapped).unwrap_or(c)
}

#[cfg(unix)]
fn upper_wide(c: char) -> char {
    // SAFETY: towupper is thread-safe; every Unicode codepoint fits in WintT.
    let mapped = unsafe { towupper(c as u32 as WintT) as u32 };
    char::from_u32(mapped).unwrap_or(c)
}

#[cfg(windows)]
fn lower_ascii(c: char) -> char {
    c.to_ascii_lowercase()
}

#[cfg(windows)]
fn lower_wide(c: char) -> char {
    match ctype_mode() {
        CtypeMode::C => c,
        CtypeMode::Unicode => single_char(c.to_lowercase()).unwrap_or(c),
    }
}

#[cfg(windows)]
fn upper_ascii(c: char) -> char {
    c.to_ascii_uppercase()
}

#[cfg(windows)]
fn upper_wide(c: char) -> char {
    match ctype_mode() {
        CtypeMode::C => c,
        CtypeMode::Unicode => single_char(c.to_uppercase()).unwrap_or(c),
    }
}

/// The one character `mapping` yields, or `None` when it yields several.
#[cfg(windows)]
fn single_char(mut mapping: impl Iterator<Item = char>) -> Option<char> {
    match (mapping.next(), mapping.next()) {
        (Some(c), None) => Some(c),
        _ => None,
    }
}

/// The current locale's radix character (`LC_NUMERIC`'s decimal point).
///
/// Returns `"."` when the locale does not supply one, so callers can use the
/// result unconditionally. `localeconv(3)` is used rather than
/// `nl_langinfo(RADIXCHAR)` because the latter's `nl_item` constant is not
/// exposed for every platform this project targets.
///
/// Call after `setlocale(LC_ALL, "")` (see `plib::diag::init_locale`);
/// before that, the C locale's `"."` is reported. On Windows this is always
/// `"."`, the POSIX locale's radix character.
pub fn radix_char() -> String {
    #[cfg(unix)]
    if let Some(radix) = locale_radix_char() {
        return radix;
    }
    ".".to_string()
}

/// `localeconv(3)`'s decimal point; `None` when the locale supplies none or it
/// is empty or not UTF-8, and [`radix_char`] reports `"."` instead.
#[cfg(unix)]
fn locale_radix_char() -> Option<String> {
    // SAFETY: localeconv() returns a pointer to a static structure owned by
    // the C library, valid until the next setlocale()/localeconv() call. We
    // copy the string out immediately.
    unsafe {
        let lconv = libc::localeconv();
        if lconv.is_null() {
            return None;
        }
        let decimal_point = (*lconv).decimal_point;
        if decimal_point.is_null() {
            return None;
        }
        match std::ffi::CStr::from_ptr(decimal_point).to_str() {
            Ok("") | Err(_) => None,
            Ok(s) => Some(s.to_string()),
        }
    }
}

/// Compare two strings using `LC_COLLATE`.
///
/// Returns `Less`, `Equal`, or `Greater` per libc `strcoll(3)`. If either
/// argument contains an interior NUL byte, falls back to byte-wise comparison
/// (since `strcoll` can't accept NULs).
pub fn strcoll(a: &str, b: &str) -> std::cmp::Ordering {
    strcoll_bytes(a.as_bytes(), b.as_bytes())
}

/// Compare two byte strings using `strcoll(3)`, honoring `LC_COLLATE`.
///
/// POSIX operands are byte strings, so this is the form utilities that keep
/// their operands as bytes want; the `&str` entry point delegates here.
/// Falls back to a byte-wise comparison if either argument contains an
/// interior NUL, which `strcoll` cannot be given.
pub fn strcoll_bytes(a: &[u8], b: &[u8]) -> std::cmp::Ordering {
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
#[cfg(unix)]
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

/// Format a unix epoch timestamp in local time; see [`crate::timefmt`].
#[cfg(windows)]
pub fn strftime(fmt: &str, epoch_secs: i64) -> std::io::Result<String> {
    crate::timefmt::format_time(fmt, epoch_secs, false)
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
    #[cfg(unix)]
    if let Some(yesexpr) = locale_yesexpr() {
        return yesexpr.is_match(response);
    }
    response.starts_with(['y', 'Y'])
}

/// The current locale's `YESEXPR`, compiled; `None` when the locale supplies
/// none or it does not compile, and [`is_affirmative`] takes the POSIX
/// locale's leading `y`/`Y` instead. Windows has no `nl_langinfo`, and always
/// takes that rule.
#[cfg(unix)]
fn locale_yesexpr() -> Option<crate::regex::Regex> {
    use std::ffi::CStr;
    // SAFETY: nl_langinfo returns a pointer to a static, locale-owned string (or a valid empty
    // string); the bytes are copied immediately before any further locale call.
    let pattern = unsafe {
        let p = libc::nl_langinfo(libc::YESEXPR);
        if p.is_null() {
            return None;
        }
        CStr::from_ptr(p).to_str().ok()?.to_owned()
    };
    if pattern.is_empty() {
        return None;
    }
    crate::regex::Regex::ere(&pattern).ok()
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
///
/// On Windows every byte is its own character in the C locale; otherwise the
/// encoding is UTF-8, with the same one-byte fallback.
pub fn mb_char_slices(bytes: &[u8]) -> Vec<&[u8]> {
    let mut result = Vec::new();
    let mut char_len = mb_char_len_fn();
    let mut i = 0;
    while i < bytes.len() {
        let consumed = char_len(&bytes[i..]);
        result.push(&bytes[i..i + consumed]);
        i += consumed;
    }
    result
}

/// A function giving the byte length of the character that starts a non-empty
/// slice, between 1 and the slice's length, for [`mb_char_slices`]. On Unix it
/// owns the `mbrtowc` conversion state across calls.
#[cfg(unix)]
fn mb_char_len_fn() -> impl FnMut(&[u8]) -> usize {
    // An all-zero mbstate_t is the documented initial conversion state.
    let mut state = MbStateT::zeroed();
    move |remaining: &[u8]| {
        // SAFETY: the pointer/length describe a valid slice, and `state` is a
        // live mbstate_t owned by this closure. A null first argument means "do
        // not store the wide character", only report the byte count.
        let n = unsafe {
            mbrtowc(
                std::ptr::null_mut(),
                remaining.as_ptr() as *const libc::c_char,
                remaining.len() as libc::size_t,
                &mut state,
            )
        };
        if n == 0 {
            // A NUL wide character occupies one byte.
            1
        } else if n == usize::MAX || n == usize::MAX - 1 {
            // (size_t)-1 (invalid sequence) or (size_t)-2 (incomplete at end of
            // input): consume one byte and reset the conversion state.
            state = MbStateT::zeroed();
            1
        } else {
            n
        }
    }
}

#[cfg(windows)]
fn mb_char_len_fn() -> impl FnMut(&[u8]) -> usize {
    let mode = ctype_mode();
    move |remaining: &[u8]| match decode_char(remaining, mode) {
        Utf8Step::Char(_, n) => n,
        // An incomplete sequence at the end of the input, like an invalid one,
        // is one byte: there is no later input to complete it.
        Utf8Step::Invalid | Utf8Step::Incomplete => 1,
    }
}

/// What the bytes at the start of a slice decode to.
#[cfg(windows)]
#[derive(Debug, PartialEq)]
enum Utf8Step {
    /// A complete character and its length in bytes.
    Char(char, usize),
    /// The first byte begins no valid sequence; it stands alone.
    Invalid,
    /// The whole slice is a valid but unfinished sequence.
    Incomplete,
}

/// Decode the character that starts the non-empty slice `bytes` as `mode`
/// reads text: in the C locale one byte, undecodable above ASCII; otherwise
/// UTF-8.
#[cfg(windows)]
fn decode_char(bytes: &[u8], mode: CtypeMode) -> Utf8Step {
    match mode {
        CtypeMode::C if bytes[0].is_ascii() => Utf8Step::Char(bytes[0] as char, 1),
        CtypeMode::C => Utf8Step::Invalid,
        CtypeMode::Unicode => decode_utf8_char(bytes),
    }
}

/// Decode the character that starts the non-empty slice `bytes` as UTF-8.
#[cfg(windows)]
fn decode_utf8_char(bytes: &[u8]) -> Utf8Step {
    // No UTF-8 character is longer than four bytes.
    let window = &bytes[..bytes.len().min(4)];
    let valid_len = match std::str::from_utf8(window) {
        Ok(_) => window.len(),
        Err(e) if e.valid_up_to() > 0 => e.valid_up_to(),
        // Nothing valid, and no invalid byte either: the window ended inside
        // a sequence. Four bytes finish any sequence, so the window is the
        // whole slice and the slice is the unfinished prefix.
        Err(e) if e.error_len().is_none() => return Utf8Step::Incomplete,
        Err(_) => return Utf8Step::Invalid,
    };
    match std::str::from_utf8(&window[..valid_len])
        .ok()
        .and_then(|s| s.chars().next())
    {
        Some(c) => Utf8Step::Char(c, c.len_utf8()),
        None => Utf8Step::Invalid,
    }
}

/// Stateful incremental multibyte decoder for streaming input.
///
/// Feed successive byte chunks to [`MbDecoder::decode`]: complete characters are
/// returned, and a trailing incomplete multibyte sequence is absorbed into the
/// decoder's conversion state to be completed by a later chunk (no caller-side
/// carryover buffer is needed — feed each byte exactly once). At end of input
/// call [`MbDecoder::pending`] to learn how many bytes of an unfinished trailing
/// sequence remain. `setlocale(LC_ALL, "")` governs the encoding via
/// `LC_CTYPE`; in the `C` locale every byte is its own character.
///
/// Used by `wc` to count characters (`-m`) and split words (`-w`) correctly in
/// a multibyte locale without reading the whole input into memory.
///
/// On Windows every byte is one character in the C locale, a byte above ASCII
/// decoding as `None` as glibc's C locale has it; otherwise the encoding is
/// UTF-8. A sequence split across chunks is completed the same way; if the next chunk shows the retained bytes did not
/// begin a valid character after all, each of them decodes as `None`, one
/// character per byte.
pub struct MbDecoder {
    /// The `mbrtowc` conversion state, holding any unfinished sequence.
    #[cfg(unix)]
    state: MbStateT,
    /// The bytes of the unfinished sequence; `pending` of them are live.
    #[cfg(windows)]
    carry: [u8; 3],
    pending: usize,
}

impl Default for MbDecoder {
    fn default() -> Self {
        Self::new()
    }
}

impl MbDecoder {
    pub fn new() -> Self {
        MbDecoder {
            #[cfg(unix)]
            state: MbStateT::zeroed(),
            #[cfg(windows)]
            carry: [0; 3],
            pending: 0,
        }
    }

    /// Bytes of an incomplete trailing multibyte sequence not yet completed.
    /// After the final chunk, these count as one character each (matching the
    /// invalid-byte convention).
    pub fn pending(&self) -> usize {
        self.pending
    }

    /// Decode every complete character in `bytes`, returning each as the decoded
    /// `char` (an undecodable byte yields `None` and counts as one character).
    /// The whole chunk is consumed: a trailing incomplete sequence is retained
    /// in the decoder state and completed by the next chunk.
    #[cfg(windows)]
    pub fn decode(&mut self, bytes: &[u8]) -> Vec<Option<char>> {
        // Resume an unfinished sequence by decoding it together with this chunk.
        let joined;
        let input = if self.pending == 0 {
            bytes
        } else {
            joined = [&self.carry[..self.pending], bytes].concat();
            &joined[..]
        };
        self.pending = 0;

        let mode = ctype_mode();
        let mut chars = Vec::new();
        let mut i = 0;
        while i < input.len() {
            match decode_char(&input[i..], mode) {
                Utf8Step::Char(c, n) => {
                    chars.push(Some(c));
                    i += n;
                }
                Utf8Step::Invalid => {
                    chars.push(None);
                    i += 1;
                }
                Utf8Step::Incomplete => {
                    // At most three bytes: four would have finished it.
                    let rest = &input[i..];
                    self.carry[..rest.len()].copy_from_slice(rest);
                    self.pending = rest.len();
                    break;
                }
            }
        }
        chars
    }

    /// Decode every complete character in `bytes`, returning each as the decoded
    /// `char` (an undecodable byte yields `None` and counts as one character).
    /// The whole chunk is consumed: a trailing incomplete sequence is retained
    /// in the decoder state and completed by the next chunk.
    #[cfg(unix)]
    pub fn decode(&mut self, bytes: &[u8]) -> Vec<Option<char>> {
        let mut chars = Vec::new();
        let mut i = 0;
        while i < bytes.len() {
            let remaining = &bytes[i..];
            let mut wc: libc::wchar_t = 0;
            // SAFETY: the pointer/length describe a valid slice and `state` is a
            // live mbstate_t owned by this decoder.
            let n = unsafe {
                mbrtowc(
                    &mut wc,
                    remaining.as_ptr() as *const libc::c_char,
                    remaining.len() as libc::size_t,
                    &mut self.state,
                )
            };
            if n == 0 {
                // A NUL wide character occupies one byte.
                chars.push(Some('\0'));
                i += 1;
                self.pending = 0;
            } else if n == usize::MAX - 1 {
                // (size_t)-2: the remaining bytes form an incomplete but valid
                // prefix and are absorbed into `state`; record them as pending.
                self.pending += remaining.len();
                i = bytes.len();
            } else if n == usize::MAX {
                // (size_t)-1: invalid sequence — consume one byte, reset state.
                self.state = MbStateT::zeroed();
                chars.push(None);
                i += 1;
                self.pending = 0;
            } else {
                // A complete character; `n` bytes were consumed from this chunk
                // (it may also have used bytes retained from earlier chunks).
                chars.push(char::from_u32(wc as u32));
                i += n;
                self.pending = 0;
            }
        }
        chars
    }
}

#[cfg(test)]
mod tests {
    //! Tests whose answers are the same on every platform: ASCII, which every
    //! locale and the Windows implementation classify as the POSIX locale does,
    //! and collation of plain byte strings.

    use super::*;

    #[test]
    fn strcoll_bytes_agrees_with_the_str_form() {
        use std::cmp::Ordering;
        assert_eq!(strcoll_bytes(b"a", b"b"), strcoll("a", "b"));
        assert_eq!(strcoll_bytes(b"b", b"a"), strcoll("b", "a"));
        assert_eq!(strcoll_bytes(b"a", b"a"), Ordering::Equal);
        // Bytes that are not valid text still compare.
        assert_eq!(strcoll_bytes(b"a\xff", b"a\xff"), Ordering::Equal);
    }

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
    fn mb_char_slices_ascii_is_one_byte_each() {
        // ASCII is single-byte in every locale.
        let slices = mb_char_slices(b"abc");
        assert_eq!(slices, vec![b"a".as_slice(), b"b", b"c"]);
        assert_eq!(mb_char_slices(b"").len(), 0);
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

    /// Every ASCII character, every class, against the POSIX locale's
    /// definitions -- the libc answer on Unix, the Windows implementation's own.
    #[test]
    fn ascii_classes_match_the_posix_locale() {
        // ASCII answers the same in either Windows mode; on Unix a test may
        // change the locale, so hold it.
        #[cfg(unix)]
        let _guard = crate::locale_test_lock();
        for b in 0u8..=0x7F {
            let c = b as char;
            let print = (0x20..=0x7E).contains(&b);
            assert_eq!(isblank(c), b == b' ' || b == b'\t', "isblank({b:#x})");
            assert_eq!(
                isspace(c),
                b == b' ' || (0x09..=0x0D).contains(&b),
                "isspace({b:#x})"
            );
            assert_eq!(isalpha(c), b.is_ascii_alphabetic(), "isalpha({b:#x})");
            assert_eq!(isalnum(c), b.is_ascii_alphanumeric(), "isalnum({b:#x})");
            assert_eq!(isdigit(c), b.is_ascii_digit(), "isdigit({b:#x})");
            assert_eq!(isxdigit(c), b.is_ascii_hexdigit(), "isxdigit({b:#x})");
            assert_eq!(iscntrl(c), b < 0x20 || b == 0x7F, "iscntrl({b:#x})");
            assert_eq!(isprint(c), print, "isprint({b:#x})");
            assert_eq!(isgraph(c), print && b != b' ', "isgraph({b:#x})");
            assert_eq!(
                ispunct(c),
                print && b != b' ' && !b.is_ascii_alphanumeric(),
                "ispunct({b:#x})"
            );
            assert_eq!(islower(c), b.is_ascii_lowercase(), "islower({b:#x})");
            assert_eq!(isupper(c), b.is_ascii_uppercase(), "isupper({b:#x})");
            assert_eq!(to_lower(c), c.to_ascii_lowercase(), "to_lower({b:#x})");
            assert_eq!(to_upper(c), c.to_ascii_uppercase(), "to_upper({b:#x})");
            let width = match b {
                0 => 0,
                _ if print => 1,
                _ => -1,
            };
            assert_eq!(wcwidth_char(c), width, "wcwidth_char({b:#x})");
        }
    }

    #[test]
    fn radix_char_defaults_to_period() {
        // The process runs in the C locale (a test that switches it restores
        // it under the lock), whose radix character is the period.
        #[cfg(unix)]
        let _guard = crate::locale_test_lock();
        assert_eq!(radix_char(), ".");
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
    fn mb_decoder_ascii() {
        let mut d = MbDecoder::new();
        let chars = d.decode(b"abc");
        assert_eq!(chars, vec![Some('a'), Some('b'), Some('c')]);
        assert_eq!(d.pending(), 0);
    }
}

#[cfg(all(test, unix))]
mod unix_tests {
    //! Tests of the libc-backed implementation and its locale dependence.

    use super::*;

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
    fn mb_char_slices_utf8_after_setlocale() {
        let _guard = crate::locale_test_lock();

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
    fn wcwidth_wide_after_setlocale() {
        let _guard = crate::locale_test_lock();

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
    fn mb_decoder_split_sequence_across_chunks() {
        // A 2-byte character (é = 0xC3 0xA9) split across two chunks must be
        // counted once. Each byte is fed exactly once; the decoder's state
        // carries the partial sequence.
        let _guard = crate::locale_test_lock();
        let saved = unsafe { libc::setlocale(libc::LC_ALL, std::ptr::null()) };
        let saved =
            (!saved.is_null()).then(|| unsafe { std::ffi::CStr::from_ptr(saved) }.to_owned());
        let utf8 = std::ffi::CString::new("C.UTF-8").unwrap();
        let ok = unsafe { libc::setlocale(libc::LC_ALL, utf8.as_ptr()) };

        if !ok.is_null() {
            let mut d = MbDecoder::new();
            // First chunk ends mid-character: 'a' plus the lead byte of é.
            let c1 = d.decode(&[b'a', 0xC3]);
            assert_eq!(c1, vec![Some('a')]);
            assert_eq!(d.pending(), 1); // 0xC3 retained in the decoder state
                                        // Second chunk completes é and adds 'b'.
            let c2 = d.decode(&[0xA9, b'b']);
            assert_eq!(c2, vec![Some('é'), Some('b')]);
            assert_eq!(d.pending(), 0);
        }

        if let Some(saved) = saved {
            unsafe { libc::setlocale(libc::LC_ALL, saved.as_ptr()) };
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

    /// `islower`/`isupper` back tr's `[:lower:]`/`[:upper:]` classes and its
    /// case-conversion pairing, so they must agree with the byte-oriented
    /// `islower(3)`/`isupper(3)` for ASCII and with the wide functions above it.
    #[test]
    fn case_predicates_follow_the_locale() {
        // ASCII holds in every locale.
        assert!(islower('a') && !islower('A') && !islower('1'));
        assert!(isupper('A') && !isupper('a') && !isupper('1'));

        // Non-ASCII letters classify only where a suitable locale exists; the
        // C locale legitimately says no.
        let Some(loc) = crate::testing::utf8_locale() else {
            return;
        };
        let _ = loc; // the process locale is whatever the harness set
                     // Exercise the wide path for panics regardless of the answer.
        let _ = islower('\u{e9}');
        let _ = isupper('\u{c9}');
    }

    /// U+0080..U+00FF are single *bytes* in Latin-1 but multi-byte in UTF-8, so
    /// dispatching them to the byte-oriented `is*(3)` asked a question those
    /// functions cannot answer and got "no". Every predicate sent them there.
    #[test]
    fn predicates_classify_latin1_range_in_a_utf8_locale() {
        let Some(loc) = crate::testing::utf8_locale() else {
            return;
        };
        // This mutates the process-global locale, so it takes the lock like
        // every other test that does -- otherwise the lock excludes nothing and
        // a byte-oriented test running beside it sees UTF-8.
        let _guard = crate::locale_test_lock();
        let saved = unsafe { libc::setlocale(libc::LC_ALL, std::ptr::null()) };
        let saved =
            (!saved.is_null()).then(|| unsafe { std::ffi::CStr::from_ptr(saved) }.to_owned());

        // Set the process locale, then ask about U+00E9 (e-acute), which is a
        // letter in any UTF-8 locale but not in C.
        unsafe {
            let c = std::ffi::CString::new(loc).unwrap();
            libc::setlocale(libc::LC_ALL, c.as_ptr());
        }
        assert!(isalpha('\u{e9}'), "U+00E9 is a letter under LC_CTYPE");
        assert!(islower('\u{e9}'));
        assert!(isupper('\u{c9}'));
        assert!(isprint('\u{e9}'));

        // Restore what was actually there. Restoring a hard-coded "C" would
        // undo a caller's locale rather than this test's.
        if let Some(saved) = saved {
            unsafe { libc::setlocale(libc::LC_ALL, saved.as_ptr()) };
        }
    }
}

#[cfg(all(test, windows))]
mod windows_tests {
    //! Tests of the Windows implementation: in the Unicode mode, Unicode
    //! classification above ASCII and UTF-8 decoding with the one-byte
    //! fallback; in the C locale's, ASCII alone and one byte per character.

    use super::*;

    /// Holds the character functions in one mode for a test, under the locale
    /// lock, and restores the previous mode when dropped.
    struct ModeGuard {
        saved: CtypeMode,
        _lock: std::sync::MutexGuard<'static, ()>,
    }

    impl Drop for ModeGuard {
        fn drop(&mut self) {
            set_ctype_mode(self.saved);
        }
    }

    fn ctype(mode: CtypeMode) -> ModeGuard {
        let lock = crate::locale_test_lock();
        let saved = ctype_mode();
        set_ctype_mode(mode);
        ModeGuard { saved, _lock: lock }
    }

    #[test]
    fn a_program_starts_in_the_c_locale() {
        let _lock = crate::locale_test_lock();
        assert_eq!(ctype_mode(), CtypeMode::C);
    }

    #[test]
    fn c_locale_has_nothing_above_ascii() {
        let _mode = ctype(CtypeMode::C);
        for c in [
            'é', 'É', 'З', '世', '\u{80}', '\u{A0}', '\u{2003}', '¿', '\u{663}',
        ] {
            for (name, class) in [
                ("isalpha", isalpha as fn(char) -> bool),
                ("isalnum", isalnum),
                ("isblank", isblank),
                ("isspace", isspace),
                ("iscntrl", iscntrl),
                ("isdigit", isdigit),
                ("isgraph", isgraph),
                ("islower", islower),
                ("isprint", isprint),
                ("ispunct", ispunct),
                ("isupper", isupper),
                ("isxdigit", isxdigit),
            ] {
                assert!(!class(c), "{name}({c:?})");
            }
            assert_eq!(to_upper(c), c);
            assert_eq!(to_lower(c), c);
            assert_eq!(wcwidth_char(c), -1, "wcwidth_char({c:?})");
        }
        // ASCII is unchanged.
        assert!(isalpha('a') && isupper('A') && isprint(' '));
        assert_eq!(to_upper('a'), 'A');
        assert_eq!(wcwidth_char('a'), 1);
    }

    #[test]
    fn c_locale_reads_one_byte_per_character() {
        let _mode = ctype(CtypeMode::C);
        let input = "aé世".as_bytes();
        let slices = mb_char_slices(input);
        assert_eq!(slices.len(), input.len());
        assert!(slices.iter().all(|s| s.len() == 1));
        let mut d = MbDecoder::new();
        assert_eq!(
            d.decode(b"a\xC3\xA9\0"),
            vec![Some('a'), None, None, Some('\0')]
        );
        // No byte waits for another.
        assert_eq!(d.decode(&[0xE4]), vec![None]);
        assert_eq!(d.pending(), 0);
    }

    #[test]
    fn non_ascii_letters_and_case() {
        let _mode = ctype(CtypeMode::Unicode);
        for c in ['é', 'É', 'ß', 'З', 'ж', '世'] {
            assert!(isalpha(c) && isalnum(c), "{c} is alphabetic");
            assert!(isprint(c) && isgraph(c), "{c} is printable");
            assert!(!ispunct(c) && !isspace(c) && !iscntrl(c), "{c}");
            assert!(!isdigit(c) && !isxdigit(c), "{c}");
        }
        assert!(islower('é') && !isupper('é'));
        assert!(isupper('É') && !islower('É'));
        assert!(!islower('世') && !isupper('世'));
        assert_eq!(to_upper('é'), 'É');
        assert_eq!(to_lower('É'), 'é');
        assert_eq!(to_lower('З'), 'з');
        // No one-character mapping: unchanged.
        assert_eq!(to_upper('ß'), 'ß');
        assert_eq!(to_lower('\u{130}'), '\u{130}');
        assert_eq!(to_upper('世'), '世');
    }

    #[test]
    fn non_ascii_digits_are_alpha_not_digit() {
        let _mode = ctype(CtypeMode::Unicode);
        // ARABIC-INDIC DIGIT THREE and FULLWIDTH DIGIT ONE.
        for c in ['\u{663}', '\u{FF11}'] {
            assert!(!isdigit(c) && !isxdigit(c));
            assert!(isalpha(c) && isalnum(c));
        }
    }

    #[test]
    fn non_ascii_spaces_controls_and_punctuation() {
        let _mode = ctype(CtypeMode::Unicode);
        // EM SPACE and IDEOGRAPHIC SPACE: blank and space, not graph.
        for c in ['\u{2003}', '\u{3000}'] {
            assert!(isblank(c) && isspace(c) && isprint(c) && !isgraph(c));
        }
        // LINE SEPARATOR and NEL: space but not blank.
        assert!(isspace('\u{2028}') && !isblank('\u{2028}'));
        assert!(isspace('\u{85}') && !isblank('\u{85}'));
        // NO-BREAK SPACE: neither, and visible.
        assert!(!isspace('\u{A0}') && !isblank('\u{A0}') && isgraph('\u{A0}'));
        // C1 controls.
        assert!(iscntrl('\u{80}') && iscntrl('\u{9F}') && !isprint('\u{9F}'));
        // Punctuation and symbols.
        for c in ['¿', '«', '—', '€', '©'] {
            assert!(ispunct(c) && isgraph(c) && !isalnum(c), "{c}");
        }
    }

    #[test]
    fn wcwidth_unicode() {
        let _mode = ctype(CtypeMode::Unicode);
        assert_eq!(wcwidth_char('é'), 1);
        assert_eq!(wcwidth_char('З'), 1);
        assert_eq!(wcwidth_char('世'), 2);
        assert_eq!(wcwidth_char('한'), 2);
        assert_eq!(wcwidth_char('\u{FF21}'), 2); // FULLWIDTH LATIN CAPITAL A
        assert_eq!(wcwidth_char('\u{3000}'), 2); // IDEOGRAPHIC SPACE
        assert_eq!(wcwidth_char('\u{303F}'), 1); // IDEOGRAPHIC HALF FILL SPACE
        assert_eq!(wcwidth_char('🦀'), 2);
        assert_eq!(wcwidth_char('\u{20000}'), 2);
        assert_eq!(wcwidth_char('\u{301}'), 0); // COMBINING ACUTE ACCENT
        assert_eq!(wcwidth_char('\u{200B}'), 0); // ZERO WIDTH SPACE
        assert_eq!(wcwidth_char('\u{FE0F}'), 0); // VARIATION SELECTOR-16
        assert_eq!(wcwidth_char('\u{85}'), -1);
    }

    #[test]
    fn mb_char_slices_utf8() {
        let _mode = ctype(CtypeMode::Unicode);
        let s = "aé世🦀";
        let slices = mb_char_slices(s.as_bytes());
        let want: Vec<&[u8]> = vec![b"a", "é".as_bytes(), "世".as_bytes(), "🦀".as_bytes()];
        assert_eq!(slices, want);
    }

    #[test]
    fn mb_char_slices_invalid_bytes_stand_alone() {
        let _mode = ctype(CtypeMode::Unicode);
        // A stray continuation byte, an invalid lead, a lead cut off by ASCII,
        // a surrogate encoding, and a sequence cut off by the end of input.
        let input = b"\x80a\xFF\xC3b\xED\xA0\x80\xE4\xB8";
        let slices = mb_char_slices(input);
        let want: Vec<&[u8]> = vec![
            b"\x80", b"a", b"\xFF", b"\xC3", b"b", b"\xED", b"\xA0", b"\x80", b"\xE4", b"\xB8",
        ];
        assert_eq!(slices, want);
        assert_eq!(slices.concat(), input);
    }

    #[test]
    fn mb_decoder_split_sequences() {
        let _mode = ctype(CtypeMode::Unicode);
        let mut d = MbDecoder::new();
        // 世 is E4 B8 96: one byte per chunk.
        assert_eq!(d.decode(&[b'a', 0xE4]), vec![Some('a')]);
        assert_eq!(d.pending(), 1);
        assert!(d.decode(&[0xB8]).is_empty());
        assert_eq!(d.pending(), 2);
        assert_eq!(d.decode(&[0x96, b'b']), vec![Some('世'), Some('b')]);
        assert_eq!(d.pending(), 0);
        // 🦀 (F0 9F A6 80) split 2 + 2, and é split 1 + 1 within one call.
        assert!(d.decode(&[0xF0, 0x9F]).is_empty());
        assert_eq!(d.decode(&[0xA6, 0x80, 0xC3]), vec![Some('🦀')]);
        assert_eq!(d.decode(&[0xA9]), vec![Some('é')]);
        assert_eq!(d.pending(), 0);
        assert!(d.decode(b"").is_empty());
    }

    #[test]
    fn mb_decoder_invalid_bytes() {
        let _mode = ctype(CtypeMode::Unicode);
        let mut d = MbDecoder::new();
        assert_eq!(
            d.decode(b"\x80a\xFF\0"),
            vec![None, Some('a'), None, Some('\0')]
        );
        // A retained prefix that the next chunk breaks: each retained byte is
        // one undecodable character, and the breaking byte decodes normally.
        assert!(d.decode(&[0xE4, 0xB8]).is_empty());
        assert_eq!(d.pending(), 2);
        assert_eq!(d.decode(b"x"), vec![None, None, Some('x')]);
        assert_eq!(d.pending(), 0);
        // A prefix still unfinished at end of input stays pending.
        assert!(d.decode(&[0xC3]).is_empty());
        assert_eq!(d.pending(), 1);
    }

    #[test]
    fn decode_utf8_char_steps() {
        assert_eq!(decode_utf8_char(b"a"), Utf8Step::Char('a', 1));
        assert_eq!(decode_utf8_char("éx".as_bytes()), Utf8Step::Char('é', 2));
        assert_eq!(decode_utf8_char("🦀🦀".as_bytes()), Utf8Step::Char('🦀', 4));
        assert_eq!(decode_utf8_char(b"\xF0\x9F\xA6"), Utf8Step::Incomplete);
        assert_eq!(decode_utf8_char(b"\xF0\x9F\xA6x"), Utf8Step::Invalid);
        assert_eq!(decode_utf8_char(b"\xC0\x80"), Utf8Step::Invalid); // overlong
        assert_eq!(decode_utf8_char(b"\xED\xA0"), Utf8Step::Invalid); // surrogate
    }
}
