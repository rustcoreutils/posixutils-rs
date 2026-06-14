//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! locale - get locale-specific information
//!
//! The locale utility writes information about the current locale
//! environment, or all available locales, to standard output.

use clap::Parser;
use gettextrs::{
    bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory as GettextCategory,
};
use posixutils_i18n::locale_lib::env::LocaleSettings;
use posixutils_i18n::locale_lib::platform;
use posixutils_i18n::locale_lib::types::LocaleCategory;

/// locale - get locale-specific information
#[derive(Parser)]
#[command(
    version,
    about = gettext("locale - get locale-specific information"),
    disable_help_flag = true,
    disable_version_flag = true
)]
struct Args {
    #[arg(short = 'a', conflicts_with_all = ["charmaps", "category_names", "keyword_names", "names"], help = gettext("Write names of all available public locales"))]
    all_locales: bool,

    #[arg(short = 'm', conflicts_with_all = ["category_names", "keyword_names", "names"], help = gettext("Write names of available charmaps"))]
    charmaps: bool,

    #[arg(short = 'c', help = gettext("Write the names of selected categories"))]
    category_names: bool,

    #[arg(short = 'k', help = gettext("Write names and values of selected keywords"))]
    keyword_names: bool,

    #[arg(short, long, action = clap::ArgAction::HelpLong, help = gettext("Print help"))]
    help: Option<bool>,

    #[arg(short = 'V', long, action = clap::ArgAction::Version, help = gettext("Print version"))]
    version: Option<bool>,

    #[arg(help = gettext("Names of categories or keywords to query"))]
    names: Vec<String>,
}

fn main() {
    // Set up localization. Use libc::setlocale directly so the C global locale
    // (read by localeconv/nl_langinfo below) is actually switched to the
    // environment locale; the gettextrs wrapper does not reliably apply it here.
    unsafe {
        libc::setlocale(libc::LC_ALL, c"".as_ptr());
    }
    setlocale(GettextCategory::LcAll, "");
    if textdomain("posixutils-rs").is_err() {
        // Ignore error - translation may not be available
    }
    let _ = bind_textdomain_codeset("posixutils-rs", "UTF-8");

    let args = Args::parse();

    // Handle -a (list all locales)
    if args.all_locales {
        for locale in platform::list_available_locales() {
            println!("{}", locale);
        }
        return;
    }

    // Handle -m (list charmaps)
    if args.charmaps {
        for charmap in platform::list_available_charmaps() {
            println!("{}", charmap);
        }
        return;
    }

    // Get current locale settings
    let settings = LocaleSettings::from_env();

    // If no names specified, show all locale environment variables
    if args.names.is_empty() {
        print_all_settings(&settings);
        return;
    }

    // Query specific categories or keywords
    let mut had_error = false;
    for name in &args.names {
        if name == "charmap" {
            // Reserved operand: the codeset name of the current locale.
            print_charmap(args.keyword_names);
        } else if let Some(category) = LocaleCategory::from_name(name) {
            // It's a category name
            if args.category_names {
                println!("{}", category);
            }
            print_category_info(category, args.keyword_names);
        } else if !print_keyword_info(name, args.category_names, args.keyword_names) {
            // Unknown name: diagnosed on stderr and reflected in the exit status.
            had_error = true;
        }
    }

    if had_error {
        std::process::exit(1);
    }
}

/// Print the `charmap` reserved operand: the codeset of the current locale.
fn print_charmap(show_name: bool) {
    let codeset = locale_codeset();
    if show_name {
        print_keyword("charmap", codeset);
    } else {
        println!("{}", codeset);
    }
}

/// Print all locale settings
fn print_all_settings(settings: &LocaleSettings) {
    // Print LANG
    if let Some(ref lang) = settings.lang {
        println!("LANG={}", lang);
    } else {
        println!("LANG=");
    }

    // Print LC_CTYPE through LC_MESSAGES
    for category in LocaleCategory::all() {
        println!("{}={}", category, settings.display_value(*category));
    }

    // Print LC_ALL
    if let Some(ref lc_all) = settings.lc_all {
        println!("LC_ALL={}", lc_all);
    } else {
        println!("LC_ALL=");
    }
}

/// A keyword value, tagged by how POSIX formats it in the `-k` output.
enum KwVal {
    /// Scalar non-numeric value: `name="value"` with `-k`, `value` without.
    Str(String),
    /// Compound (list) value: `name="a;b;c"` with `-k`, `a;b;c` without.
    /// Each element is escaped independently; the `;` separators are not.
    List(Vec<String>),
    /// Numeric value, already formatted (e.g. `2`, `-1`, `3;3`): `name=value`
    /// with `-k` (unquoted), `value` without.
    Num(String),
}

/// Escape the characters POSIX requires to be escaped inside a quoted `-k`
/// value: `;`, `\`, `"`, and control characters, each preceded by the escape
/// character (the default backslash).
fn escape_value(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        if c == ';' || c == '\\' || c == '"' || c.is_control() {
            out.push('\\');
        }
        out.push(c);
    }
    out
}

impl KwVal {
    /// `-k` rendering: `name="value"` / `name="a;b"` / `name=numeric`.
    fn render_with_name(&self, name: &str) -> String {
        match self {
            KwVal::Str(s) => format!("{}=\"{}\"", name, escape_value(s)),
            KwVal::List(items) => {
                let joined = items
                    .iter()
                    .map(|s| escape_value(s))
                    .collect::<Vec<_>>()
                    .join(";");
                format!("{}=\"{}\"", name, joined)
            }
            KwVal::Num(s) => format!("{}={}", name, s),
        }
    }

    /// Plain (no `-k`) rendering: just the value.
    fn render_value(&self) -> String {
        match self {
            KwVal::Str(s) => s.clone(),
            KwVal::List(items) => items.join(";"),
            KwVal::Num(s) => s.clone(),
        }
    }
}

/// POSIX `LC_MONETARY` keywords, in output order.
const MONETARY_KEYWORDS: &[&str] = &[
    "int_curr_symbol",
    "currency_symbol",
    "mon_decimal_point",
    "mon_thousands_sep",
    "mon_grouping",
    "positive_sign",
    "negative_sign",
    "int_frac_digits",
    "frac_digits",
    "p_cs_precedes",
    "p_sep_by_space",
    "n_cs_precedes",
    "n_sep_by_space",
    "p_sign_posn",
    "n_sign_posn",
    "int_p_cs_precedes",
    "int_p_sep_by_space",
    "int_n_cs_precedes",
    "int_n_sep_by_space",
    "int_p_sign_posn",
    "int_n_sign_posn",
];

/// POSIX `LC_TIME` keywords this implementation exposes, in output order.
const TIME_KEYWORDS: &[&str] = &[
    "abday",
    "day",
    "abmon",
    "mon",
    "am_pm",
    "d_t_fmt",
    "d_fmt",
    "t_fmt",
    "t_fmt_ampm",
];

/// The keywords this implementation exposes for each category, in output order.
/// Per POSIX, no keyword values are written for `LC_CTYPE`/`LC_COLLATE`.
fn category_keywords(category: LocaleCategory) -> &'static [&'static str] {
    match category {
        LocaleCategory::LcNumeric => &["decimal_point", "thousands_sep", "grouping"],
        LocaleCategory::LcMonetary => MONETARY_KEYWORDS,
        LocaleCategory::LcTime => TIME_KEYWORDS,
        LocaleCategory::LcMessages => &["yesexpr", "noexpr"],
        LocaleCategory::LcCtype | LocaleCategory::LcCollate => &[],
    }
}

/// Look up the category and current value of a keyword, or `None` if this
/// implementation does not expose it.
fn keyword_value(keyword: &str) -> Option<(LocaleCategory, KwVal)> {
    use LocaleCategory::*;
    let entry = match keyword {
        "decimal_point" => (LcNumeric, KwVal::Str(get_decimal_point())),
        "thousands_sep" => (LcNumeric, KwVal::Str(get_thousands_sep())),
        "grouping" => (LcNumeric, numeric_grouping()),

        "d_t_fmt" => (LcTime, KwVal::Str(get_d_t_fmt())),
        "d_fmt" => (LcTime, KwVal::Str(get_d_fmt())),
        "t_fmt" => (LcTime, KwVal::Str(get_t_fmt())),
        "t_fmt_ampm" => (LcTime, KwVal::Str(get_t_fmt_ampm())),
        "abday" | "day" | "abmon" | "mon" | "am_pm" => (LcTime, KwVal::List(time_list(keyword))),

        "yesexpr" => (LcMessages, KwVal::Str(get_yesexpr())),
        "noexpr" => (LcMessages, KwVal::Str(get_noexpr())),

        name if MONETARY_KEYWORDS.contains(&name) => (LcMonetary, monetary_keyword(name)?),

        _ => return None,
    };
    Some(entry)
}

/// Print information about a specific category. Categories with no exposed
/// keywords (LC_CTYPE/LC_COLLATE) simply produce no keyword output, rather than
/// the malformed `CATEGORY=locale_name` line the previous code emitted.
fn print_category_info(category: LocaleCategory, show_keywords: bool) {
    for keyword in category_keywords(category) {
        let value = match keyword_value(keyword) {
            Some((_, v)) => v,
            None => continue,
        };
        if show_keywords {
            println!("{}", value.render_with_name(keyword));
        } else {
            // Item 4: without `-k`, write only the selected keyword values.
            println!("{}", value.render_value());
        }
    }
}

/// Print information about a specific keyword. Returns `false` (and diagnoses on
/// stderr) when the name is not a keyword this implementation recognizes.
fn print_keyword_info(keyword: &str, show_category: bool, show_name: bool) -> bool {
    let (category, value) = match keyword_value(keyword) {
        Some(entry) => entry,
        None => {
            eprintln!("locale: unknown name \"{}\"", keyword);
            return false;
        }
    };

    if show_category {
        println!("{}", category);
    }

    if show_name {
        println!("{}", value.render_with_name(keyword));
    } else {
        println!("{}", value.render_value());
    }
    true
}

/// Print a keyword=value pair (used for the `charmap` operand).
fn print_keyword(name: &str, value: String) {
    println!("{}=\"{}\"", name, escape_value(&value));
}

// Locale query functions using libc

fn get_decimal_point() -> String {
    // Try to get from libc
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    unsafe {
        let lconv = libc::localeconv();
        if !lconv.is_null() && !(*lconv).decimal_point.is_null() {
            if let Ok(s) = std::ffi::CStr::from_ptr((*lconv).decimal_point).to_str() {
                return s.to_string();
            }
        }
    }
    ".".to_string()
}

fn get_thousands_sep() -> String {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    unsafe {
        let lconv = libc::localeconv();
        if !lconv.is_null() && !(*lconv).thousands_sep.is_null() {
            if let Ok(s) = std::ffi::CStr::from_ptr((*lconv).thousands_sep).to_str() {
                return s.to_string();
            }
        }
    }
    "".to_string()
}

/// `LC_NUMERIC` `grouping` as a numeric value: the grouping sizes joined by
/// `;`, or `-1` when there is no grouping.
fn numeric_grouping() -> KwVal {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    unsafe {
        let lconv = libc::localeconv();
        if !lconv.is_null() {
            return KwVal::Num(grouping_str((*lconv).grouping));
        }
    }
    KwVal::Num("-1".to_string())
}

/// Read a NUL-terminated C string field; an empty string for NULL.
#[cfg(any(target_os = "linux", target_os = "macos"))]
unsafe fn cstr(ptr: *const libc::c_char) -> String {
    if ptr.is_null() {
        return String::new();
    }
    std::ffi::CStr::from_ptr(ptr)
        .to_str()
        .map(String::from)
        .unwrap_or_default()
}

/// Format a `localeconv` `char` field: the `CHAR_MAX` sentinel ("unspecified")
/// is rendered as `-1`, matching the reference `locale` utility.
#[cfg(any(target_os = "linux", target_os = "macos"))]
fn char_num(v: libc::c_char) -> String {
    if v == libc::c_char::MAX {
        "-1".to_string()
    } else {
        (v as i64).to_string()
    }
}

/// Format a grouping byte-string (`grouping`/`mon_grouping`): each size as a
/// number joined by `;`, stopping at NUL or the `CHAR_MAX` terminator; `-1`
/// when empty.
#[cfg(any(target_os = "linux", target_os = "macos"))]
unsafe fn grouping_str(ptr: *const libc::c_char) -> String {
    if ptr.is_null() {
        return "-1".to_string();
    }
    let mut sizes = Vec::new();
    let mut p = ptr;
    loop {
        let v = *p;
        if v == 0 || v == libc::c_char::MAX {
            break;
        }
        sizes.push((v as i64).to_string());
        p = p.add(1);
    }
    if sizes.is_empty() {
        "-1".to_string()
    } else {
        sizes.join(";")
    }
}

/// Value of an `LC_MONETARY` keyword from `localeconv`.
#[cfg(any(target_os = "linux", target_os = "macos"))]
fn monetary_keyword(name: &str) -> Option<KwVal> {
    unsafe {
        let lconv = libc::localeconv();
        if lconv.is_null() {
            return None;
        }
        let lc = &*lconv;
        let val = match name {
            "int_curr_symbol" => KwVal::Str(cstr(lc.int_curr_symbol)),
            "currency_symbol" => KwVal::Str(cstr(lc.currency_symbol)),
            "mon_decimal_point" => KwVal::Str(cstr(lc.mon_decimal_point)),
            "mon_thousands_sep" => KwVal::Str(cstr(lc.mon_thousands_sep)),
            "mon_grouping" => KwVal::Num(grouping_str(lc.mon_grouping)),
            "positive_sign" => KwVal::Str(cstr(lc.positive_sign)),
            "negative_sign" => KwVal::Str(cstr(lc.negative_sign)),
            "int_frac_digits" => KwVal::Num(char_num(lc.int_frac_digits)),
            "frac_digits" => KwVal::Num(char_num(lc.frac_digits)),
            "p_cs_precedes" => KwVal::Num(char_num(lc.p_cs_precedes)),
            "p_sep_by_space" => KwVal::Num(char_num(lc.p_sep_by_space)),
            "n_cs_precedes" => KwVal::Num(char_num(lc.n_cs_precedes)),
            "n_sep_by_space" => KwVal::Num(char_num(lc.n_sep_by_space)),
            "p_sign_posn" => KwVal::Num(char_num(lc.p_sign_posn)),
            "n_sign_posn" => KwVal::Num(char_num(lc.n_sign_posn)),
            "int_p_cs_precedes" => KwVal::Num(char_num(lc.int_p_cs_precedes)),
            "int_p_sep_by_space" => KwVal::Num(char_num(lc.int_p_sep_by_space)),
            "int_n_cs_precedes" => KwVal::Num(char_num(lc.int_n_cs_precedes)),
            "int_n_sep_by_space" => KwVal::Num(char_num(lc.int_n_sep_by_space)),
            "int_p_sign_posn" => KwVal::Num(char_num(lc.int_p_sign_posn)),
            "int_n_sign_posn" => KwVal::Num(char_num(lc.int_n_sign_posn)),
            _ => return None,
        };
        Some(val)
    }
}

#[cfg(not(any(target_os = "linux", target_os = "macos")))]
fn monetary_keyword(_name: &str) -> Option<KwVal> {
    None
}

/// Query `nl_langinfo(item)` for the active locale, returning `None` if it is
/// unavailable or empty. Used for the `LC_TIME`/`LC_MESSAGES` keywords and the
/// `charmap` operand, which `localeconv()` does not provide.
#[cfg(any(target_os = "linux", target_os = "macos"))]
fn nl_langinfo_string(item: libc::nl_item) -> Option<String> {
    unsafe {
        let ptr = libc::nl_langinfo(item);
        if ptr.is_null() {
            return None;
        }
        std::ffi::CStr::from_ptr(ptr)
            .to_str()
            .ok()
            .filter(|s| !s.is_empty())
            .map(|s| s.to_string())
    }
}

#[cfg(not(any(target_os = "linux", target_os = "macos")))]
fn nl_langinfo_string(_item: i32) -> Option<String> {
    None
}

/// Codeset of the current locale, for the `charmap` operand.
fn locale_codeset() -> String {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    {
        if let Some(s) = nl_langinfo_string(libc::CODESET) {
            return s;
        }
    }
    "ANSI_X3.4-1968".to_string()
}

fn get_d_t_fmt() -> String {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    if let Some(s) = nl_langinfo_string(libc::D_T_FMT) {
        return s;
    }
    "%a %b %e %H:%M:%S %Y".to_string()
}

fn get_d_fmt() -> String {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    if let Some(s) = nl_langinfo_string(libc::D_FMT) {
        return s;
    }
    "%m/%d/%y".to_string()
}

fn get_t_fmt() -> String {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    if let Some(s) = nl_langinfo_string(libc::T_FMT) {
        return s;
    }
    "%H:%M:%S".to_string()
}

fn get_yesexpr() -> String {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    if let Some(s) = nl_langinfo_string(libc::YESEXPR) {
        return s;
    }
    "^[yY]".to_string()
}

fn get_noexpr() -> String {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    if let Some(s) = nl_langinfo_string(libc::NOEXPR) {
        return s;
    }
    "^[nN]".to_string()
}

fn get_t_fmt_ampm() -> String {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    if let Some(s) = nl_langinfo_string(libc::T_FMT_AMPM) {
        return s;
    }
    "%I:%M:%S %p".to_string()
}

/// Compound `LC_TIME` list keywords (`abday`/`day`/`abmon`/`mon`/`am_pm`),
/// gathered from the individual `nl_langinfo` items.
#[cfg(any(target_os = "linux", target_os = "macos"))]
fn time_list(name: &str) -> Vec<String> {
    let items: &[libc::nl_item] = match name {
        "abday" => &[
            libc::ABDAY_1,
            libc::ABDAY_2,
            libc::ABDAY_3,
            libc::ABDAY_4,
            libc::ABDAY_5,
            libc::ABDAY_6,
            libc::ABDAY_7,
        ],
        "day" => &[
            libc::DAY_1,
            libc::DAY_2,
            libc::DAY_3,
            libc::DAY_4,
            libc::DAY_5,
            libc::DAY_6,
            libc::DAY_7,
        ],
        "abmon" => &[
            libc::ABMON_1,
            libc::ABMON_2,
            libc::ABMON_3,
            libc::ABMON_4,
            libc::ABMON_5,
            libc::ABMON_6,
            libc::ABMON_7,
            libc::ABMON_8,
            libc::ABMON_9,
            libc::ABMON_10,
            libc::ABMON_11,
            libc::ABMON_12,
        ],
        "mon" => &[
            libc::MON_1,
            libc::MON_2,
            libc::MON_3,
            libc::MON_4,
            libc::MON_5,
            libc::MON_6,
            libc::MON_7,
            libc::MON_8,
            libc::MON_9,
            libc::MON_10,
            libc::MON_11,
            libc::MON_12,
        ],
        "am_pm" => &[libc::AM_STR, libc::PM_STR],
        _ => &[],
    };
    items
        .iter()
        .map(|&item| nl_langinfo_string(item).unwrap_or_default())
        .collect()
}

#[cfg(not(any(target_os = "linux", target_os = "macos")))]
fn time_list(name: &str) -> Vec<String> {
    // C-locale fallbacks where nl_langinfo is unavailable.
    let csv = match name {
        "abday" => "Sun;Mon;Tue;Wed;Thu;Fri;Sat",
        "day" => "Sunday;Monday;Tuesday;Wednesday;Thursday;Friday;Saturday",
        "abmon" => "Jan;Feb;Mar;Apr;May;Jun;Jul;Aug;Sep;Oct;Nov;Dec",
        "mon" => {
            "January;February;March;April;May;June;July;August;September;October;November;December"
        }
        "am_pm" => "AM;PM",
        _ => "",
    };
    csv.split(';').map(String::from).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn escape_value_escapes_required_chars() {
        // ';', '\', '"', and control chars are escaped with a backslash.
        assert_eq!(escape_value("a;b"), "a\\;b");
        assert_eq!(escape_value("a\\b"), "a\\\\b");
        assert_eq!(escape_value("a\"b"), "a\\\"b");
        assert_eq!(escape_value("a\tb"), "a\\\tb");
        assert_eq!(escape_value("plain"), "plain");
    }

    #[test]
    fn kwval_numeric_is_unquoted() {
        assert_eq!(
            KwVal::Num("-1".into()).render_with_name("frac_digits"),
            "frac_digits=-1"
        );
        assert_eq!(KwVal::Num("2".into()).render_value(), "2");
    }

    #[test]
    fn kwval_string_is_quoted_and_escaped() {
        assert_eq!(
            KwVal::Str("a;b".into()).render_with_name("k"),
            "k=\"a\\;b\""
        );
        assert_eq!(KwVal::Str("x".into()).render_value(), "x");
    }

    #[test]
    fn kwval_list_escapes_elements_not_separators() {
        // Element with an embedded ';' is escaped; the joining ';' is not.
        let v = KwVal::List(vec!["a;b".into(), "c".into()]);
        assert_eq!(v.render_with_name("abday"), "abday=\"a\\;b;c\"");
        assert_eq!(v.render_value(), "a;b;c");
    }
}
