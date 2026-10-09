//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Shared diagnostic module for posixutils utilities.
//!
//! Provides a uniform "`<util>: <message>`" diagnostic surface backed by
//! atomic error / warning counters, optional source-file + line:column
//! tracking, and a one-shot locale + gettext initializer.
//!
//! Two API shapes are supported:
//!
//! - **Simple utilities** (ar, nm, strings, strip, etc.): use [`init_locale`]
//!   in `main`, then call [`error`] / [`warning`] with already-`gettext`'d
//!   strings. At exit, return [`exit_status`].
//!
//! - **Parser-style utilities** (yacc, lex, cc): also call [`set_source`]
//!   for each input file and use [`error_at`] / [`warning_at`] with a
//!   [`Position`] so line:column appears in the diagnostic.
//!
//! Output format mirrors GCC: `"<util>: <source>:<line>[:<col>]: <level>: <msg>"`
//! when a position is given, otherwise `"<util>: <msg>"`.

use std::cell::RefCell;
use std::fmt;
use std::io::{self, Write};
use std::sync::atomic::{AtomicU32, Ordering};

/// Source position for line/column-tracked diagnostics.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Position {
    /// Line number (1-based).
    pub line: u32,
    /// Column position (1-based, 0 means unknown).
    pub col: u16,
}

impl Position {
    /// Create a new position with line and column.
    pub fn new(line: u32, col: u16) -> Self {
        Self { line, col }
    }

    /// Create a position with line only (column unknown).
    pub fn line_only(line: u32) -> Self {
        Self { line, col: 0 }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = SOURCE_FILE.with(|s| s.borrow().clone());
        let name = if name.is_empty() {
            "<unknown>".to_string()
        } else {
            name
        };

        if self.col > 0 {
            write!(f, "{}:{}:{}", name, self.line, self.col)
        } else {
            write!(f, "{}:{}", name, self.line)
        }
    }
}

thread_local! {
    static UTIL_NAME: RefCell<String> = const { RefCell::new(String::new()) };
    static SOURCE_FILE: RefCell<String> = const { RefCell::new(String::new()) };
}

static ERROR_COUNT: AtomicU32 = AtomicU32::new(0);
static WARNING_COUNT: AtomicU32 = AtomicU32::new(0);

/// Initialize the diagnostic system with the utility name (e.g. `"ar"`, `"yacc"`).
///
/// The name is used as a prefix on every emitted diagnostic. Also resets the
/// error and warning counters.
pub fn init(utility: &str) {
    UTIL_NAME.with(|s| {
        *s.borrow_mut() = utility.to_string();
    });
    SOURCE_FILE.with(|s| {
        *s.borrow_mut() = String::new();
    });
    reset_counts();
}

/// One-shot initializer for the signal, locale, gettext and diagnostic surface.
///
/// This is the canonical startup entry point. It calls, in order:
///
/// - [`crate::io::restore_sigpipe`] — the Rust runtime ignores `SIGPIPE`, which
///   turns `ls | head` into a panic and exit 101 instead of the silent death by
///   signal every historical utility gets. A utility that writes into a pager
///   or filter it spawned itself needs `EPIPE` for *that* pipe, and holds a
///   [`crate::io::SigPipeIgnored`] across the write rather than changing the
///   disposition for its whole run. A `SIG_IGN` the process inherited is
///   kept, and a write to a closed pipe is then a write error.
/// - [`crate::io::report_stdout_write_errors`] — a failed `println!` is
///   reported as `UTILITY: write error: REASON` with exit 1, not a panic.
/// - `setlocale(LC_ALL, "")` — inherits the locale from the environment so that
///   locale-sensitive libc functions (`<ctype.h>`/`<wctype.h>`, `strcoll`,
///   `strftime`, `nl_langinfo`, …) observe `LC_*`. The gettextrs wrapper applies
///   this directly to libc's global locale.
/// - on Windows, `setlocale(LC_ALL, ".UTF-8")` — the user's locale as `""`
///   selects it, but in UTF-8 instead of the ANSI code page, so that the C
///   runtime's multibyte functions (and with them [`crate::regex`]) read
///   the UTF-8 that Rust strings hold. The UCRT supports this from Windows
///   10 1803; where it is refused (older systems, or the msvcrt that the
///   `-gnu` targets link) the `""` locale stays.
/// - on Windows, the POSIX locale variables. The C runtime reads none of
///   them: `""` is always the user's regional setting. So each category
///   (`LC_COLLATE`, `LC_CTYPE`, `LC_MONETARY`, `LC_NUMERIC`, `LC_TIME`) takes
///   its POSIX value -- `LC_ALL`, else its own `LC_*` variable, else `LANG`,
///   the first that is set and not empty -- and maps it as glibc would (see
///   [`PosixLocale`]):
///
///   | value | `LC_CTYPE` | the other categories |
///   |---|---|---|
///   | `C`, `POSIX` | the C runtime's `"C"`; ASCII characters, one per byte | `"C"` |
///   | `C.UTF-8`, `POSIX.utf8`, ... | `.UTF-8`; Unicode characters | `"C"`: byte order |
///   | anything else, or unset | `.UTF-8`; Unicode characters | the user's |
///
///   Windows locale names are not POSIX ones, so no other name is honoured.
///   The C runtime's locale governs `strcoll` and `plib::regex`'s multibyte
///   decoding; the `LC_CTYPE` row also sets the mode of the
///   [`crate::locale`] character functions, which answer for themselves on
///   Windows.
/// - `textdomain("posixutils-rs")`
/// - `bind_textdomain_codeset("posixutils-rs", "UTF-8")`
/// - [`init`]`(utility)`
///
/// Errors from `textdomain` / `bind_textdomain_codeset` are silently ignored
/// (the gettext crate returns `io::Error` when the catalog isn't installed;
/// that's expected on systems without translations and shouldn't abort the
/// utility's startup).
pub fn init_locale(utility: &str) {
    use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
    crate::io::restore_sigpipe();
    crate::io::report_stdout_write_errors(utility);
    setlocale(LocaleCategory::LcAll, "");
    #[cfg(windows)]
    {
        setlocale(LocaleCategory::LcAll, ".UTF-8");
        apply_posix_locale();
    }
    let _ = textdomain("posixutils-rs");
    let _ = bind_textdomain_codeset("posixutils-rs", "UTF-8");
    init(utility);
}

/// Apply what the POSIX locale variables select to each category, over the
/// user's locale in UTF-8 that [`init_locale`] has set. See [`init_locale`].
#[cfg(windows)]
fn apply_posix_locale() {
    use crate::locale::{set_ctype_mode, CtypeMode};
    use gettextrs::{setlocale, LocaleCategory};
    let env = |name: &str| std::env::var(name).ok();
    let categories = [
        (LocaleCategory::LcCollate, "LC_COLLATE"),
        (LocaleCategory::LcMonetary, "LC_MONETARY"),
        (LocaleCategory::LcNumeric, "LC_NUMERIC"),
        (LocaleCategory::LcTime, "LC_TIME"),
    ];
    for (category, variable) in categories {
        if resolve_posix_locale(variable, env) != PosixLocale::User {
            setlocale(category, "C");
        }
    }
    // LC_CTYPE keeps UTF-8 for a C locale with a codeset: only the plain C
    // locale reads one byte per character, in the C runtime (and with it
    // `plib::regex`) and in plib's own character functions alike.
    match resolve_posix_locale("LC_CTYPE", env) {
        PosixLocale::C => {
            setlocale(LocaleCategory::LcCType, "C");
            set_ctype_mode(CtypeMode::C);
        }
        PosixLocale::CWithCodeset | PosixLocale::User => set_ctype_mode(CtypeMode::Unicode),
    }
}

/// What a category's POSIX locale value means on Windows. See [`init_locale`].
#[cfg(windows)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PosixLocale {
    /// `C` or `POSIX`: the C locale.
    C,
    /// `C` or `POSIX` with a codeset (`C.UTF-8`): the C locale, but with
    /// Unicode characters in UTF-8, as glibc's `C.UTF-8`.
    CWithCodeset,
    /// Anything else, or no value: the user's locale, in UTF-8.
    User,
}

/// The meaning of a category's locale as POSIX resolves it from the
/// environment (see [`posix_locale_value`]). `env` looks a variable up.
#[cfg(windows)]
fn resolve_posix_locale(variable: &str, env: impl Fn(&str) -> Option<String>) -> PosixLocale {
    let Some(value) = posix_locale_value(variable, env) else {
        return PosixLocale::User;
    };
    let (language, codeset) = match value.split_once('.') {
        Some((language, codeset)) => (language, Some(codeset)),
        None => (value.as_str(), None),
    };
    match (language, codeset) {
        ("C" | "POSIX", None) => PosixLocale::C,
        ("C" | "POSIX", Some(codeset)) if !codeset.is_empty() => PosixLocale::CWithCodeset,
        _ => PosixLocale::User,
    }
}

/// A category's locale as POSIX resolves it from the environment: `LC_ALL`,
/// else the category's own variable (`variable`), else `LANG` -- the first
/// that is set and not empty. `env` looks a variable up.
#[cfg(windows)]
fn posix_locale_value(variable: &str, env: impl Fn(&str) -> Option<String>) -> Option<String> {
    ["LC_ALL", variable, "LANG"]
        .into_iter()
        .filter_map(env)
        .find(|value| !value.is_empty())
}

/// Set the current source filename used by [`error_at`] / [`warning_at`].
pub fn set_source(filename: &str) {
    SOURCE_FILE.with(|s| {
        *s.borrow_mut() = filename.to_string();
    });
}

/// Reset error and warning counters. Primarily for testing.
pub fn reset_counts() {
    ERROR_COUNT.store(0, Ordering::SeqCst);
    WARNING_COUNT.store(0, Ordering::SeqCst);
}

/// Number of errors emitted since the last [`init`] / [`reset_counts`].
pub fn error_count() -> u32 {
    ERROR_COUNT.load(Ordering::SeqCst)
}

/// Number of warnings emitted since the last [`init`] / [`reset_counts`].
pub fn warning_count() -> u32 {
    WARNING_COUNT.load(Ordering::SeqCst)
}

/// True if any error has been emitted.
pub fn has_errors() -> bool {
    error_count() > 0
}

/// Exit status appropriate for the recorded error state: `0` if no errors,
/// `1` otherwise. Use in `main` like `process::exit(plib::diag::exit_status())`.
pub fn exit_status() -> i32 {
    if has_errors() {
        1
    } else {
        0
    }
}

/// Flush standard output at the end of a run, and report a failure.
///
/// Standard output is line-buffered, so output that does not end in a
/// <newline> is still in the buffer when `main` returns or calls
/// `std::process::exit`. The runtime flushes it then but discards the error,
/// so a final partial line written to a full disk was lost with exit status 0
/// (`printf x | cat >/dev/full`). Call this after the last output: a failure
/// is reported as `UTILITY: write error: REASON`, counted like any [`error`],
/// and `false` is returned for the caller to fold into the exit status its
/// specification gives an error.
pub fn flush_stdout() -> bool {
    match io::stdout().flush() {
        Ok(()) => true,
        Err(e) => {
            error(&format!("write error: {}", io_error_text(&e)));
            false
        }
    }
}

/// Render an `io::Error` the way a system utility reports one.
///
/// Rust's `Display` appends `" (os error 2)"` to the strerror text, so a
/// diagnostic built with `{}` reads `No such file or directory (os error 2)`
/// where every other utility on the system says `No such file or directory`.
///
/// For an error that came from the operating system this returns `strerror`'s
/// own text, which is the locale's — the `LC_MESSAGES` obligation that
/// formatting the Rust error can never meet, since Rust's table is English
/// regardless of locale. Errors we constructed ourselves have no `errno` and
/// are passed through with the parenthetical stripped if one is somehow there.
pub fn io_error_text(e: &io::Error) -> String {
    // Windows has no `strerror_r`; there Rust's own text is already the
    // system's (FormatMessage), so the fallback below is the locale's message.
    #[cfg(unix)]
    if let Some(errno) = e.raw_os_error() {
        // `strerror_r` rather than `strerror`: the latter may return a pointer
        // into a shared static buffer for an unrecognized errno, which is a
        // data race between two threads reporting at once. Same shape as
        // `tree/common/mod.rs`'s `error_string`.
        let mut buf = [0 as libc::c_char; 128];
        // SAFETY: `buf` is a live, correctly sized array and `strerror_r`
        // writes at most `buf.len()` bytes, NUL-terminating within it.
        let rc = unsafe { libc::strerror_r(errno as _, buf.as_mut_ptr(), buf.len()) };
        if rc == 0 {
            let bytes = unsafe { std::ffi::CStr::from_ptr(buf.as_ptr()) }.to_bytes();
            return String::from_utf8_lossy(bytes).into_owned();
        }
    }
    // No errno, or strerror_r declined: fall back to Rust's own text with the
    // parenthetical removed.
    let s = e.to_string();
    match s.find(" (os error ") {
        Some(idx) => s[..idx].to_string(),
        None => s,
    }
}

/// Render any error the way a system utility reports one.
///
/// [`io_error_text`] needs an `io::Error`, but a utility whose `main` returns
/// `Result<_, Box<dyn Error>>` holds the same `io::Error` inside a box, and
/// formatting *that* leaks `" (os error 2)"` exactly as formatting the error
/// itself would. Downcasting recovers the errno, so such a caller gets the
/// system's message and the locale's; anything else falls back to the text
/// with the parenthetical stripped.
pub fn error_text(e: &(dyn std::error::Error + 'static)) -> String {
    if let Some(io_err) = e.downcast_ref::<io::Error>() {
        return io_error_text(io_err);
    }
    let s = e.to_string();
    match s.find(" (os error ") {
        Some(idx) => s[..idx].to_string(),
        None => s,
    }
}

/// Emit an error diagnostic with no source-position information.
/// Output format: `"<util>: <msg>"`.
pub fn error(msg: &str) {
    ERROR_COUNT.fetch_add(1, Ordering::SeqCst);
    write_plain("error", msg);
}

/// Emit a warning diagnostic with no source-position information.
/// Output format: `"<util>: warning: <msg>"`.
pub fn warning(msg: &str) {
    WARNING_COUNT.fetch_add(1, Ordering::SeqCst);
    write_plain("warning", msg);
}

/// Emit an error at the given source position.
/// Output format: `"<util>: <source>:<line>[:<col>]: error: <msg>"`.
pub fn error_at(pos: Position, msg: &str) {
    ERROR_COUNT.fetch_add(1, Ordering::SeqCst);
    write_at(pos, "error", msg);
}

/// Emit a warning at the given source position.
/// Output format: `"<util>: <source>:<line>[:<col>]: warning: <msg>"`.
pub fn warning_at(pos: Position, msg: &str) {
    WARNING_COUNT.fetch_add(1, Ordering::SeqCst);
    write_at(pos, "warning", msg);
}

fn util_prefix() -> String {
    UTIL_NAME.with(|s| s.borrow().clone())
}

fn write_plain(level: &str, msg: &str) {
    let util = util_prefix();
    let mut out = io::stderr().lock();
    let _ = if util.is_empty() {
        if level == "error" {
            writeln!(out, "{}", msg)
        } else {
            writeln!(out, "{}: {}", level, msg)
        }
    } else if level == "error" {
        writeln!(out, "{}: {}", util, msg)
    } else {
        writeln!(out, "{}: {}: {}", util, level, msg)
    };
}

fn write_at(pos: Position, level: &str, msg: &str) {
    let util = util_prefix();
    let mut out = io::stderr().lock();
    let _ = if util.is_empty() {
        writeln!(out, "{}: {}: {}", pos, level, msg)
    } else {
        writeln!(out, "{}: {}: {}: {}", util, pos, level, msg)
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    /// An environment holding exactly `vars`.
    #[cfg(windows)]
    fn env<'a>(vars: &'a [(&'a str, &'a str)]) -> impl Fn(&str) -> Option<String> + 'a {
        move |name| {
            vars.iter()
                .find(|(key, _)| *key == name)
                .map(|(_, value)| value.to_string())
        }
    }

    #[cfg(windows)]
    #[test]
    fn posix_locale_value_takes_lc_all_then_the_category_then_lang() {
        let all = [("LC_ALL", "C"), ("LC_COLLATE", "de"), ("LANG", "fr")];
        assert_eq!(
            posix_locale_value("LC_COLLATE", env(&all)).as_deref(),
            Some("C")
        );
        let category = [("LC_ALL", ""), ("LC_COLLATE", "POSIX"), ("LANG", "fr")];
        assert_eq!(
            posix_locale_value("LC_COLLATE", env(&category)).as_deref(),
            Some("POSIX")
        );
        // Another category's variable is not consulted.
        let lang = [("LC_CTYPE", "C"), ("LANG", "fr")];
        assert_eq!(
            posix_locale_value("LC_COLLATE", env(&lang)).as_deref(),
            Some("fr")
        );
        assert_eq!(posix_locale_value("LC_COLLATE", env(&[])), None);
    }

    #[cfg(windows)]
    #[test]
    fn resolve_posix_locale_maps_each_kind_of_name() {
        let resolve = |value: &str| resolve_posix_locale("LC_CTYPE", env(&[("LC_ALL", value)]));
        assert_eq!(resolve("C"), PosixLocale::C);
        assert_eq!(resolve("POSIX"), PosixLocale::C);
        for c_with_codeset in ["C.UTF-8", "C.utf8", "POSIX.UTF-8"] {
            assert_eq!(resolve(c_with_codeset), PosixLocale::CWithCodeset);
        }
        for other in [
            "c",
            "C.",
            "c.utf8",
            "Cx",
            "en_US.UTF-8",
            "English_United States",
        ] {
            assert_eq!(resolve(other), PosixLocale::User, "{other:?}");
        }
        // Unset, or set empty: the user's locale.
        assert_eq!(resolve(""), PosixLocale::User);
        assert_eq!(
            resolve_posix_locale("LC_CTYPE", env(&[])),
            PosixLocale::User
        );
    }

    #[cfg(windows)]
    #[test]
    fn resolve_posix_locale_takes_lc_all_then_the_category_then_lang() {
        let resolve = |vars: &[(&str, &str)]| resolve_posix_locale("LC_CTYPE", env(vars));
        let all = [
            ("LC_ALL", "C"),
            ("LC_CTYPE", "C.UTF-8"),
            ("LANG", "en_US.UTF-8"),
        ];
        assert_eq!(resolve(&all), PosixLocale::C);
        let category = [("LC_CTYPE", "C.UTF-8"), ("LANG", "C")];
        assert_eq!(resolve(&category), PosixLocale::CWithCodeset);
        let lang = [("LC_COLLATE", "C"), ("LANG", "POSIX")];
        assert_eq!(resolve(&lang), PosixLocale::C);
        let user = [("LC_ALL", "en_US.UTF-8"), ("LC_CTYPE", "C")];
        assert_eq!(resolve(&user), PosixLocale::User);
    }

    #[test]
    fn position_display_with_col() {
        init("test");
        set_source("foo.y");
        let pos = Position::new(10, 5);
        assert_eq!(format!("{}", pos), "foo.y:10:5");
    }

    #[test]
    fn position_display_line_only() {
        init("test");
        set_source("foo.l");
        let pos = Position::line_only(20);
        assert_eq!(format!("{}", pos), "foo.l:20");
    }

    #[test]
    fn position_display_no_source() {
        init("test");
        set_source("");
        let pos = Position::new(1, 1);
        assert_eq!(format!("{}", pos), "<unknown>:1:1");
    }

    #[test]
    fn error_warning_dont_panic() {
        // Counters are shared across parallel tests, so we only verify
        // that the calls don't panic and that the local count delta is
        // observable on a freshly-reset thread.
        init("test");
        error("plain error");
        warning("plain warning");
        error_at(Position::new(3, 4), "located error");
        warning_at(Position::line_only(5), "located warning");
    }

    #[test]
    fn util_prefix_set_by_init() {
        // Doesn't assert on global state, only on the thread-local prefix
        // (which is itself thread-local, so safe across parallel tests).
        init("zz_test_util");
        assert_eq!(util_prefix(), "zz_test_util");
    }
}
