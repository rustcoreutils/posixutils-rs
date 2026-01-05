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
    LocaleCategory as GettextCategory, bind_textdomain_codeset, gettext, setlocale, textdomain,
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
    // Set up localization
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
    for name in &args.names {
        if let Some(category) = LocaleCategory::from_name(name) {
            // It's a category name
            if args.category_names {
                println!("{}", category);
            }
            print_category_info(&settings, category, args.keyword_names);
        } else {
            // It's a keyword name - find which category it belongs to
            print_keyword_info(name, args.category_names, args.keyword_names);
        }
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

/// Print information about a specific category
fn print_category_info(settings: &LocaleSettings, category: LocaleCategory, show_keywords: bool) {
    let locale = settings.effective(category);

    if show_keywords {
        // Print keyword=value pairs for this category
        match category {
            LocaleCategory::LcNumeric => {
                print_keyword("decimal_point", get_decimal_point());
                print_keyword("thousands_sep", get_thousands_sep());
                print_keyword("grouping", get_grouping());
            }
            LocaleCategory::LcMonetary => {
                print_keyword("currency_symbol", get_currency_symbol());
                print_keyword("int_curr_symbol", get_int_curr_symbol());
                print_keyword("mon_decimal_point", get_mon_decimal_point());
            }
            LocaleCategory::LcTime => {
                print_keyword("d_t_fmt", get_d_t_fmt());
                print_keyword("d_fmt", get_d_fmt());
                print_keyword("t_fmt", get_t_fmt());
            }
            LocaleCategory::LcMessages => {
                print_keyword("yesexpr", get_yesexpr());
                print_keyword("noexpr", get_noexpr());
            }
            _ => {
                // For other categories, just show the locale name
                println!("{}={}", category, locale);
            }
        }
    } else {
        println!("{}={}", category, locale);
    }
}

/// Print information about a specific keyword
fn print_keyword_info(keyword: &str, show_category: bool, show_name: bool) {
    let (category, value) = match keyword {
        // LC_NUMERIC keywords
        "decimal_point" => (Some(LocaleCategory::LcNumeric), get_decimal_point()),
        "thousands_sep" => (Some(LocaleCategory::LcNumeric), get_thousands_sep()),
        "grouping" => (Some(LocaleCategory::LcNumeric), get_grouping()),

        // LC_MONETARY keywords
        "currency_symbol" => (Some(LocaleCategory::LcMonetary), get_currency_symbol()),
        "int_curr_symbol" => (Some(LocaleCategory::LcMonetary), get_int_curr_symbol()),
        "mon_decimal_point" => (Some(LocaleCategory::LcMonetary), get_mon_decimal_point()),

        // LC_TIME keywords
        "d_t_fmt" => (Some(LocaleCategory::LcTime), get_d_t_fmt()),
        "d_fmt" => (Some(LocaleCategory::LcTime), get_d_fmt()),
        "t_fmt" => (Some(LocaleCategory::LcTime), get_t_fmt()),

        // LC_MESSAGES keywords
        "yesexpr" => (Some(LocaleCategory::LcMessages), get_yesexpr()),
        "noexpr" => (Some(LocaleCategory::LcMessages), get_noexpr()),

        _ => {
            eprintln!("locale: unknown keyword: {}", keyword);
            return;
        }
    };

    if show_category {
        if let Some(cat) = category {
            println!("{}", cat);
        }
    }

    if show_name {
        print_keyword(keyword, value);
    } else {
        println!("{}", value);
    }
}

/// Print a keyword=value pair
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

fn get_d_t_fmt() -> String {
    // Return typical format
    "%a %b %e %H:%M:%S %Y".to_string()
}

fn get_d_fmt() -> String {
    "%m/%d/%y".to_string()
}

fn get_t_fmt() -> String {
    "%H:%M:%S".to_string()
}

fn get_yesexpr() -> String {
    "^[yY]".to_string()
}

fn get_noexpr() -> String {
    "^[nN]".to_string()
}
