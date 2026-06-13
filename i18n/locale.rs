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
    #[arg(short = 'a', help = gettext("Write names of all available public locales"))]
    all_locales: bool,

    #[arg(short = 'm', help = gettext("Write names of available charmaps"))]
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

/// The keywords this implementation exposes for each category, in output order.
/// Per POSIX, no keyword values are written for `LC_CTYPE`/`LC_COLLATE`.
fn category_keywords(category: LocaleCategory) -> &'static [&'static str] {
    match category {
        LocaleCategory::LcNumeric => &["decimal_point", "thousands_sep", "grouping"],
        LocaleCategory::LcMonetary => &["int_curr_symbol", "currency_symbol", "mon_decimal_point"],
        LocaleCategory::LcTime => &["d_t_fmt", "d_fmt", "t_fmt"],
        LocaleCategory::LcMessages => &["yesexpr", "noexpr"],
        LocaleCategory::LcCtype | LocaleCategory::LcCollate => &[],
    }
}

/// Look up the category and current value of a keyword, or `None` if this
/// implementation does not expose it.
fn keyword_value(keyword: &str) -> Option<(LocaleCategory, String)> {
    use LocaleCategory::*;
    let entry = match keyword {
        "decimal_point" => (LcNumeric, get_decimal_point()),
        "thousands_sep" => (LcNumeric, get_thousands_sep()),
        "grouping" => (LcNumeric, get_grouping()),

        "int_curr_symbol" => (LcMonetary, get_int_curr_symbol()),
        "currency_symbol" => (LcMonetary, get_currency_symbol()),
        "mon_decimal_point" => (LcMonetary, get_mon_decimal_point()),

        "d_t_fmt" => (LcTime, get_d_t_fmt()),
        "d_fmt" => (LcTime, get_d_fmt()),
        "t_fmt" => (LcTime, get_t_fmt()),

        "yesexpr" => (LcMessages, get_yesexpr()),
        "noexpr" => (LcMessages, get_noexpr()),

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
            print_keyword(keyword, value);
        } else {
            // Item 4: without `-k`, write only the selected keyword values.
            println!("{}", value);
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
        print_keyword(keyword, value);
    } else {
        println!("{}", value);
    }
    true
}

/// Print a keyword=value pair (non-numeric keyword format).
fn print_keyword(name: &str, value: String) {
    println!("{}=\"{}\"", name, value);
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

fn get_grouping() -> String {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    unsafe {
        let lconv = libc::localeconv();
        if !lconv.is_null() && !(*lconv).grouping.is_null() {
            let mut result = Vec::new();
            let mut ptr = (*lconv).grouping;
            // CHAR_MAX (127 or 255) indicates no further grouping
            while *ptr != 0 && (*ptr as u8) < 127 {
                result.push((*ptr).to_string());
                ptr = ptr.add(1);
            }
            if !result.is_empty() {
                return result.join(";");
            }
        }
    }
    "-1".to_string()
}

fn get_currency_symbol() -> String {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    unsafe {
        let lconv = libc::localeconv();
        if !lconv.is_null() && !(*lconv).currency_symbol.is_null() {
            if let Ok(s) = std::ffi::CStr::from_ptr((*lconv).currency_symbol).to_str() {
                return s.to_string();
            }
        }
    }
    "".to_string()
}

fn get_int_curr_symbol() -> String {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    unsafe {
        let lconv = libc::localeconv();
        if !lconv.is_null() && !(*lconv).int_curr_symbol.is_null() {
            if let Ok(s) = std::ffi::CStr::from_ptr((*lconv).int_curr_symbol).to_str() {
                return s.to_string();
            }
        }
    }
    "".to_string()
}

fn get_mon_decimal_point() -> String {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    unsafe {
        let lconv = libc::localeconv();
        if !lconv.is_null() && !(*lconv).mon_decimal_point.is_null() {
            if let Ok(s) = std::ffi::CStr::from_ptr((*lconv).mon_decimal_point).to_str() {
                return s.to_string();
            }
        }
    }
    "".to_string()
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
