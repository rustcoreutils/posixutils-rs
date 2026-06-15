/// Set (or query) the program's locale via libc `setlocale(3)`.
///
/// Translation catalogs are not loaded in this workspace (`gettext` is an
/// identity passthrough), but the locale itself must still reach libc so that
/// locale-sensitive C functions behave correctly — e.g. `strftime` month/day
/// names under `LC_TIME`, and `<ctype.h>`/`<wctype.h>` classification under
/// `LC_CTYPE`. Returns the resulting locale name, or `None` if the request
/// could not be honored.
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

pub fn bindtextdomain<T, U>(
    _domainname: T,
    dirname: U,
) -> Result<std::path::PathBuf, std::io::Error>
where
    T: Into<Vec<u8>>,
    U: Into<std::path::PathBuf>,
{
    Ok(dirname.into())
}

pub fn textdomain<T: Into<Vec<u8>>>(domainname: T) -> Result<Vec<u8>, std::io::Error> {
    Ok(domainname.into())
}

pub fn gettext<T: Into<String>>(msgid: T) -> String {
    msgid.into()
}

#[macro_export]
macro_rules! gettext {
    ($fmt:expr) => {
        format!($fmt)
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
}
