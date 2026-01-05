//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Environment variable handling for locales

use std::env;

use super::types::LocaleCategory;

/// Current locale settings from environment
#[derive(Debug, Clone, Default)]
pub struct LocaleSettings {
    /// LANG environment variable
    pub lang: Option<String>,
    /// LC_ALL environment variable (overrides all others)
    pub lc_all: Option<String>,
    /// LC_CTYPE environment variable
    pub lc_ctype: Option<String>,
    /// LC_COLLATE environment variable
    pub lc_collate: Option<String>,
    /// LC_MONETARY environment variable
    pub lc_monetary: Option<String>,
    /// LC_NUMERIC environment variable
    pub lc_numeric: Option<String>,
    /// LC_TIME environment variable
    pub lc_time: Option<String>,
    /// LC_MESSAGES environment variable
    pub lc_messages: Option<String>,
}

impl LocaleSettings {
    /// Read current locale settings from environment
    pub fn from_env() -> Self {
        LocaleSettings {
            lang: env::var("LANG").ok().filter(|s| !s.is_empty()),
            lc_all: env::var("LC_ALL").ok().filter(|s| !s.is_empty()),
            lc_ctype: env::var("LC_CTYPE").ok().filter(|s| !s.is_empty()),
            lc_collate: env::var("LC_COLLATE").ok().filter(|s| !s.is_empty()),
            lc_monetary: env::var("LC_MONETARY").ok().filter(|s| !s.is_empty()),
            lc_numeric: env::var("LC_NUMERIC").ok().filter(|s| !s.is_empty()),
            lc_time: env::var("LC_TIME").ok().filter(|s| !s.is_empty()),
            lc_messages: env::var("LC_MESSAGES").ok().filter(|s| !s.is_empty()),
        }
    }

    /// Get the effective locale for a specific category
    pub fn effective(&self, category: LocaleCategory) -> &str {
        // LC_ALL overrides everything
        if let Some(ref lc_all) = self.lc_all {
            return lc_all;
        }

        // Check category-specific variable
        let category_value = match category {
            LocaleCategory::LcCtype => &self.lc_ctype,
            LocaleCategory::LcCollate => &self.lc_collate,
            LocaleCategory::LcMonetary => &self.lc_monetary,
            LocaleCategory::LcNumeric => &self.lc_numeric,
            LocaleCategory::LcTime => &self.lc_time,
            LocaleCategory::LcMessages => &self.lc_messages,
        };

        if let Some(value) = category_value {
            return value;
        }

        // Fall back to LANG
        if let Some(ref lang) = self.lang {
            return lang;
        }

        // Default is POSIX/C
        "POSIX"
    }

    /// Get the value to display for a category (for locale command output)
    pub fn display_value(&self, category: LocaleCategory) -> String {
        // Check if LC_ALL is set
        if self.lc_all.is_some() {
            // Category value is implied from LC_ALL
            return format!("\"{}\"", self.effective(category));
        }

        // Check category-specific variable
        let category_value = match category {
            LocaleCategory::LcCtype => &self.lc_ctype,
            LocaleCategory::LcCollate => &self.lc_collate,
            LocaleCategory::LcMonetary => &self.lc_monetary,
            LocaleCategory::LcNumeric => &self.lc_numeric,
            LocaleCategory::LcTime => &self.lc_time,
            LocaleCategory::LcMessages => &self.lc_messages,
        };

        if let Some(value) = category_value {
            return value.clone();
        }

        // Implied from LANG
        format!("\"{}\"", self.effective(category))
    }
}
