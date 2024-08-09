pub fn setlocale<T: Into<Vec<u8>>>(_category: LocaleCategory, locale: T) -> Option<Vec<u8>> {
    Some(locale.into())
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

pub fn textdomain<T: Into<Vec<u8>>>(domainname: T) -> Result<Vec<u8>, std::io::Error> {
    Ok(domainname.into())
}

pub fn gettext<T: Into<String>>(msgid: T) -> String {
    return msgid.into();
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
