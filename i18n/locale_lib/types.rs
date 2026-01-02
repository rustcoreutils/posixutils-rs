//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Core data types for locale handling

use std::fmt;

/// All locale categories as defined by POSIX
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LocaleCategory {
    /// Character classification and case conversion
    LcCtype,
    /// Collation (sort) order
    LcCollate,
    /// Monetary formatting
    LcMonetary,
    /// Numeric formatting
    LcNumeric,
    /// Date and time formatting
    LcTime,
    /// Message formatting (yes/no responses)
    LcMessages,
}

impl LocaleCategory {
    /// Get all categories in order
    pub fn all() -> &'static [LocaleCategory] {
        &[
            LocaleCategory::LcCtype,
            LocaleCategory::LcCollate,
            LocaleCategory::LcMonetary,
            LocaleCategory::LcNumeric,
            LocaleCategory::LcTime,
            LocaleCategory::LcMessages,
        ]
    }

    /// Get the environment variable name for this category
    pub fn env_name(&self) -> &'static str {
        match self {
            LocaleCategory::LcCtype => "LC_CTYPE",
            LocaleCategory::LcCollate => "LC_COLLATE",
            LocaleCategory::LcMonetary => "LC_MONETARY",
            LocaleCategory::LcNumeric => "LC_NUMERIC",
            LocaleCategory::LcTime => "LC_TIME",
            LocaleCategory::LcMessages => "LC_MESSAGES",
        }
    }

    /// Parse a category name
    pub fn from_name(name: &str) -> Option<LocaleCategory> {
        match name.to_uppercase().as_str() {
            "LC_CTYPE" => Some(LocaleCategory::LcCtype),
            "LC_COLLATE" => Some(LocaleCategory::LcCollate),
            "LC_MONETARY" => Some(LocaleCategory::LcMonetary),
            "LC_NUMERIC" => Some(LocaleCategory::LcNumeric),
            "LC_TIME" => Some(LocaleCategory::LcTime),
            "LC_MESSAGES" => Some(LocaleCategory::LcMessages),
            _ => None,
        }
    }
}

impl fmt::Display for LocaleCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.env_name())
    }
}

/// Keywords for LC_NUMERIC category
pub const LC_NUMERIC_KEYWORDS: &[&str] = &["decimal_point", "thousands_sep", "grouping"];

/// Keywords for LC_TIME category
pub const LC_TIME_KEYWORDS: &[&str] = &[
    "abday",
    "day",
    "abmon",
    "mon",
    "d_t_fmt",
    "d_fmt",
    "t_fmt",
    "am_pm",
    "t_fmt_ampm",
    "era",
    "era_d_fmt",
    "era_t_fmt",
    "era_d_t_fmt",
    "alt_digits",
];

/// Keywords for LC_MONETARY category
pub const LC_MONETARY_KEYWORDS: &[&str] = &[
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
];

/// Keywords for LC_MESSAGES category
pub const LC_MESSAGES_KEYWORDS: &[&str] = &["yesexpr", "noexpr", "yesstr", "nostr"];

/// Keywords for LC_CTYPE category
pub const LC_CTYPE_KEYWORDS: &[&str] = &[
    "upper", "lower", "alpha", "digit", "space", "cntrl", "punct", "graph", "print", "xdigit",
    "blank", "toupper", "tolower",
];

/// Keywords for LC_COLLATE category
pub const LC_COLLATE_KEYWORDS: &[&str] = &[
    "collating-element",
    "collating-symbol",
    "order_start",
    "order_end",
];
