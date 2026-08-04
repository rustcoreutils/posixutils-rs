//! A small `gettext` implementation for this workspace.
//!
//! `setlocale` calls libc so that locale-sensitive C functions behave correctly
//! — `strftime` month/day names under `LC_TIME`, `<ctype.h>` classification
//! under `LC_CTYPE`. `gettext` reads GNU `.mo` catalogs, honoring `NLSPATH`
//! (which POSIX lists for the XSI message-catalog interface) ahead of
//! `bindtextdomain`, `TEXTDOMAINDIR` and the system locale directories.
//!
//! When the `LC_MESSAGES` locale is `C`/`POSIX`, or no catalog for the domain
//! exists, `gettext` returns its argument — which is both what POSIX requires
//! of the `C` locale and, since no catalog for this workspace's domain is
//! installed by default, what every utility sees on an ordinary system.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Mutex, OnceLock};

mod catalog;

use catalog::Catalog;

/// Process-wide catalog state, mirroring the C library's.
#[derive(Default)]
struct Domains {
    /// The domain set by `textdomain`.
    current: Option<String>,
    /// Directories bound by `bindtextdomain`, per domain.
    bound: HashMap<String, PathBuf>,
    /// Catalogs already looked up, keyed by `(domain, locale)`. `None` records
    /// a completed search that found nothing, so a miss is not retried on every
    /// message.
    loaded: HashMap<(String, String), Option<Catalog>>,
}

fn domains() -> &'static Mutex<Domains> {
    static CELL: OnceLock<Mutex<Domains>> = OnceLock::new();
    CELL.get_or_init(|| Mutex::new(Domains::default()))
}

/// Lock the domain table, ignoring poisoning: a panic in another thread must
/// not turn every subsequent diagnostic into a second panic.
fn lock_domains() -> std::sync::MutexGuard<'static, Domains> {
    domains().lock().unwrap_or_else(|e| e.into_inner())
}

/// Set (or query) the program's locale via libc `setlocale(3)`.
///
/// Returns the resulting locale name, or `None` if the request could not be
/// honored.
pub fn setlocale<T: Into<Vec<u8>>>(category: LocaleCategory, locale: T) -> Option<Vec<u8>> {
    let c_locale = std::ffi::CString::new(locale).ok()?;
    // SAFETY: c_locale is a valid NUL-terminated string for the duration of the
    // call; setlocale returns either a pointer to a libc-owned C string or null.
    let ret = unsafe { libc::setlocale(category.to_libc(), c_locale.as_ptr()) };
    if ret.is_null() {
        return None;
    }
    // SAFETY: ret is a non-null pointer to a NUL-terminated string owned by libc.
    let name = unsafe { std::ffi::CStr::from_ptr(ret) };
    Some(name.to_bytes().to_vec())
}

pub fn bind_textdomain_codeset<T, U>(
    _domainname: T,
    _codeset: U,
) -> Result<Option<String>, std::io::Error>
where
    T: Into<Vec<u8>>,
    U: Into<String>,
{
    Ok(None)
}

/// Bind `domainname`'s catalogs to `dirname`, which is then searched ahead of
/// `TEXTDOMAINDIR` and the system locale directories (but behind `NLSPATH`).
pub fn bindtextdomain<T, U>(domainname: T, dirname: U) -> Result<std::path::PathBuf, std::io::Error>
where
    T: Into<Vec<u8>>,
    U: Into<std::path::PathBuf>,
{
    let dir = dirname.into();
    if let Ok(domain) = String::from_utf8(domainname.into()) {
        let mut d = lock_domains();
        d.bound.insert(domain.clone(), dir.clone());
        // A rebind invalidates anything already resolved for that domain.
        d.loaded.retain(|(dom, _), _| dom != &domain);
    }
    Ok(dir)
}

/// Set the domain `gettext` looks messages up in.
pub fn textdomain<T: Into<Vec<u8>>>(domainname: T) -> Result<Vec<u8>, std::io::Error> {
    let name = domainname.into();
    if let Ok(domain) = std::str::from_utf8(&name) {
        lock_domains().current = Some(domain.to_string());
    }
    Ok(name)
}

/// Translate `msgid` in the current domain, or return it unchanged when there
/// is no translation.
pub fn gettext<T: Into<String>>(msgid: T) -> String {
    let msgid = msgid.into();
    let locale = catalog::message_locale();
    if catalog::is_untranslated(&locale) {
        return msgid;
    }

    // Resolve what to look up, then release the lock: `catalog::load` walks the
    // filesystem, and holding a process-wide mutex across that would let one
    // slow or unreachable path (an automounted `NLSPATH` entry, say) stall every
    // other thread's diagnostics.
    let (domain, bound, cached) = {
        let d = lock_domains();
        // With no `textdomain` call there is nothing to look up in. C gettext
        // would use "messages"; searching for it here would let an unrelated
        // system catalog answer for a utility that never asked to be
        // translated, so decline instead.
        let domain = match d.current.clone() {
            Some(domain) => domain,
            None => return msgid,
        };
        let cached = d.loaded.contains_key(&(domain.clone(), locale.clone()));
        let bound = d.bound.get(&domain).cloned();
        (domain, bound, cached)
    };

    if !cached {
        let loaded = catalog::load(&domain, &locale, bound.as_deref());
        // Two threads may have missed the cache and both loaded; whichever
        // arrives first wins and the other's copy is dropped. Duplicated work,
        // once, is cheaper than serializing every caller behind the I/O.
        lock_domains()
            .loaded
            .entry((domain.clone(), locale.clone()))
            .or_insert(loaded);
    }

    let d = lock_domains();
    match d.loaded.get(&(domain, locale)).and_then(|c| c.as_ref()) {
        Some(cat) => cat.lookup(&msgid).map(str::to_string).unwrap_or(msgid),
        None => msgid,
    }
}

/// Format a diagnostic, translating it where that is possible.
///
/// The single-argument form is a plain message: it goes through [`gettext`] and
/// is translated. Because the argument is a translation msgid, it must be a
/// literal with no inline `{capture}` — those are substituted by `format!` at
/// compile time and would leave the catalog looking up a string that no
/// extractor ever saw.
///
/// The multi-argument form is **not** translated, and cannot be while it uses
/// `format!`: `format!` requires its template to be a compile-time literal, so
/// there is nowhere to put a string fetched at run time. Translating it needs a
/// runtime formatter with its own placeholder syntax — the shape C uses, where
/// the msgid keeps its `%d` and printf substitutes later. That is a workspace-
/// wide change to all 100-odd call sites, not a change to this macro.
#[macro_export]
macro_rules! gettext {
    ($fmt:expr) => {
        $crate::gettext($fmt)
    };
    ($fmt:expr, $($arg:tt)*) => {
        format!($fmt, $($arg)*)
    };
}

pub enum LocaleCategory {
    LcCType,
    LcNumeric,
    LcTime,
    LcCollate,
    LcMonetary,
    LcMessages,
    LcAll,
    LcPaper,
    LcName,
    LcAddress,
    LcTelephone,
    LcMeasurement,
    LcIdentification,
}

impl LocaleCategory {
    /// Map to the libc `LC_*` constant for `setlocale(3)`.
    ///
    /// The `LcPaper`..`LcIdentification` categories are glibc extensions; on
    /// platforms that lack them (e.g. macOS) they fall back to `LC_ALL`.
    fn to_libc(&self) -> libc::c_int {
        match self {
            LocaleCategory::LcCType => libc::LC_CTYPE,
            LocaleCategory::LcNumeric => libc::LC_NUMERIC,
            LocaleCategory::LcTime => libc::LC_TIME,
            LocaleCategory::LcCollate => libc::LC_COLLATE,
            LocaleCategory::LcMonetary => libc::LC_MONETARY,
            LocaleCategory::LcMessages => libc::LC_MESSAGES,
            LocaleCategory::LcAll => libc::LC_ALL,
            #[cfg(target_os = "linux")]
            LocaleCategory::LcPaper => libc::LC_PAPER,
            #[cfg(target_os = "linux")]
            LocaleCategory::LcName => libc::LC_NAME,
            #[cfg(target_os = "linux")]
            LocaleCategory::LcAddress => libc::LC_ADDRESS,
            #[cfg(target_os = "linux")]
            LocaleCategory::LcTelephone => libc::LC_TELEPHONE,
            #[cfg(target_os = "linux")]
            LocaleCategory::LcMeasurement => libc::LC_MEASUREMENT,
            #[cfg(target_os = "linux")]
            LocaleCategory::LcIdentification => libc::LC_IDENTIFICATION,
            #[cfg(not(target_os = "linux"))]
            _ => libc::LC_ALL,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn setlocale_invokes_libc() {
        // The "C" locale is always available; libc setlocale returns its name.
        assert_eq!(setlocale(LocaleCategory::LcAll, "C"), Some(b"C".to_vec()));

        // A nonexistent locale makes libc setlocale fail and return null, which
        // surfaces as None. The previous stub echoed the input back instead, so
        // this distinguishes the real call from the no-op.
        assert_eq!(
            setlocale(LocaleCategory::LcTime, "this_locale_does_not_exist"),
            None
        );
    }

    /// Build a minimal `.mo` image (mirrors `catalog::tests::build_mo`; the two
    /// modules are private to one another).
    fn build_mo(entries: &[(&str, &str)]) -> Vec<u8> {
        const HEADER: u32 = 28;
        const DESC: u32 = 8;
        let n = entries.len() as u32;
        let orig_tab = HEADER;
        let trans_tab = orig_tab + n * DESC;
        let strings_at = trans_tab + n * DESC;

        let mut blob: Vec<u8> = Vec::new();
        let mut descs = (Vec::new(), Vec::new());
        for (id, s) in entries {
            descs
                .0
                .push((id.len() as u32, strings_at + blob.len() as u32));
            blob.extend_from_slice(id.as_bytes());
            blob.push(0);
            descs
                .1
                .push((s.len() as u32, strings_at + blob.len() as u32));
            blob.extend_from_slice(s.as_bytes());
            blob.push(0);
        }

        let mut out = Vec::new();
        out.extend_from_slice(&0x9504_12deu32.to_le_bytes());
        for w in [0, n, orig_tab, trans_tab, 0, 0] {
            out.extend_from_slice(&w.to_le_bytes());
        }
        for (len, off) in descs.0.into_iter().chain(descs.1) {
            out.extend_from_slice(&len.to_le_bytes());
            out.extend_from_slice(&off.to_le_bytes());
        }
        out.extend_from_slice(&blob);
        out
    }

    /// `gettext` resolves a real catalog through `NLSPATH`, and falls back to
    /// the untranslated message when the locale is `C` or nothing is found.
    ///
    /// One test rather than several: it mutates process-global environment and
    /// catalog state, which cannot be done safely from concurrent tests.
    #[test]
    fn nlspath_catalog_is_found_and_used() {
        let dir = std::env::temp_dir().join(format!("gettextrs-nlspath-{}", std::process::id()));
        let _ = std::fs::create_dir_all(&dir);
        let mo = dir.join("testdomain.de_DE.mo");
        std::fs::write(&mo, build_mo(&[("Hello", "Hallo")])).expect("write catalog");

        // SAFETY: single-threaded test; no other thread reads the environment.
        unsafe {
            std::env::set_var("NLSPATH", dir.join("%N.%L.mo").to_str().unwrap());
            std::env::set_var("LC_ALL", "de_DE");
        }
        textdomain("testdomain").unwrap();

        assert_eq!(gettext("Hello"), "Hallo");
        // A msgid the catalog does not contain comes back unchanged.
        assert_eq!(gettext("Goodbye"), "Goodbye");

        // The C locale is untranslated, as POSIX requires.
        unsafe { std::env::set_var("LC_ALL", "C") };
        assert_eq!(gettext("Hello"), "Hello");

        // A locale with no catalog is untranslated too.
        unsafe { std::env::set_var("LC_ALL", "fr_FR") };
        assert_eq!(gettext("Hello"), "Hello");

        unsafe {
            std::env::remove_var("NLSPATH");
            std::env::remove_var("LC_ALL");
        }
        let _ = std::fs::remove_dir_all(&dir);
    }
}
