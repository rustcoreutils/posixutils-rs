//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Message catalog lookup with locale and domain resolution
//!
//! This module provides functionality for finding and loading message catalogs
//! based on the current locale and text domain settings.

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use crate::gettext_lib::catalog::MessageCatalog;
use crate::gettext_lib::mo_file::MoFile;

/// Default locale search paths for Linux
#[cfg(target_os = "linux")]
const DEFAULT_LOCALE_PATHS: &[&str] = &[
    "/usr/share/locale",
    "/usr/local/share/locale",
    "/usr/lib/locale",
];

/// Default locale search paths for macOS
#[cfg(target_os = "macos")]
const DEFAULT_LOCALE_PATHS: &[&str] = &[
    "/usr/local/share/locale",
    "/opt/homebrew/share/locale",
    "/usr/share/locale",
];

/// Default locale search paths for other platforms
#[cfg(not(any(target_os = "linux", target_os = "macos")))]
const DEFAULT_LOCALE_PATHS: &[&str] = &["/usr/share/locale", "/usr/local/share/locale"];

/// Message lookup service
#[derive(Debug)]
pub struct MessageLookup {
    /// Custom search paths (takes precedence over defaults)
    search_paths: Vec<PathBuf>,
    /// Cached catalogs by (domain, locale)
    catalogs: HashMap<(String, String), MessageCatalog>,
}

impl MessageLookup {
    /// Create a new message lookup service
    pub fn new() -> Self {
        let mut search_paths = Vec::new();

        // TEXTDOMAINDIR takes precedence
        if let Ok(dir) = env::var("TEXTDOMAINDIR") {
            search_paths.push(PathBuf::from(dir));
        }

        // Add default paths
        for path in DEFAULT_LOCALE_PATHS {
            search_paths.push(PathBuf::from(path));
        }

        MessageLookup {
            search_paths,
            catalogs: HashMap::new(),
        }
    }

    /// Create a message lookup service with custom search paths
    pub fn with_paths(paths: Vec<PathBuf>) -> Self {
        MessageLookup {
            search_paths: paths,
            catalogs: HashMap::new(),
        }
    }

    /// Get the current locale from environment
    pub fn get_current_locale() -> String {
        // LC_ALL overrides everything
        if let Ok(locale) = env::var("LC_ALL") {
            if !locale.is_empty() {
                return locale;
            }
        }

        // LC_MESSAGES for message catalogs
        if let Ok(locale) = env::var("LC_MESSAGES") {
            if !locale.is_empty() {
                return locale;
            }
        }

        // LANG as fallback
        if let Ok(locale) = env::var("LANG") {
            if !locale.is_empty() {
                return locale;
            }
        }

        // Default to C locale
        "C".to_string()
    }

    /// Get the text domain from environment or default
    pub fn get_text_domain() -> String {
        env::var("TEXTDOMAIN").unwrap_or_else(|_| "messages".to_string())
    }

    /// Get locale variants for fallback lookup
    ///
    /// For "es_ES.UTF-8@valencia", returns:
    /// - "es_ES.UTF-8@valencia"
    /// - "es_ES@valencia"
    /// - "es_ES.UTF-8"
    /// - "es_ES"
    /// - "es@valencia"
    /// - "es"
    fn get_locale_variants(locale: &str) -> Vec<String> {
        let mut variants = Vec::new();

        // Parse locale: language[_territory][.codeset][@modifier]
        let (base, modifier) = if let Some(pos) = locale.find('@') {
            (&locale[..pos], Some(&locale[pos + 1..]))
        } else {
            (locale, None)
        };

        let (without_codeset, codeset) = if let Some(pos) = base.find('.') {
            (&base[..pos], Some(&base[pos + 1..]))
        } else {
            (base, None)
        };

        let (language, territory) = if let Some(pos) = without_codeset.find('_') {
            (&without_codeset[..pos], Some(&without_codeset[pos + 1..]))
        } else {
            (without_codeset, None)
        };

        // Full locale with everything
        variants.push(locale.to_string());

        // Without codeset but with modifier
        if let (Some(_), Some(m)) = (codeset, modifier) {
            variants.push(format!("{}@{}", without_codeset, m));
        }

        // Without modifier but with codeset
        if let (Some(_), Some(c)) = (modifier, codeset) {
            variants.push(format!("{}.{}", without_codeset, c));
        }

        // Without codeset and without modifier (but with territory)
        if codeset.is_some() || modifier.is_some() {
            variants.push(without_codeset.to_string());
        }

        // Language with modifier only
        if let (Some(_), Some(m)) = (territory, modifier) {
            variants.push(format!("{}@{}", language, m));
        }

        // Language only
        if territory.is_some() {
            variants.push(language.to_string());
        }

        // Remove duplicates while preserving order
        let mut seen = std::collections::HashSet::new();
        variants.retain(|v| seen.insert(v.clone()));

        variants
    }

    /// Find the .mo file path for a domain and locale
    fn find_mo_file(&self, domain: &str, locale: &str) -> Option<PathBuf> {
        let variants = Self::get_locale_variants(locale);

        for base_path in &self.search_paths {
            for variant in &variants {
                // Standard path: {base}/{locale}/LC_MESSAGES/{domain}.mo
                let path = base_path
                    .join(variant)
                    .join("LC_MESSAGES")
                    .join(format!("{}.mo", domain));

                if path.exists() {
                    return Some(path);
                }

                // Alternative path: {base}/{locale}/{domain}.mo
                let alt_path = base_path.join(variant).join(format!("{}.mo", domain));

                if alt_path.exists() {
                    return Some(alt_path);
                }
            }
        }

        None
    }

    /// Load a message catalog for a domain and locale
    fn load_catalog(&mut self, domain: &str, locale: &str) -> Option<&MessageCatalog> {
        let key = (domain.to_string(), locale.to_string());

        // Check if already loaded
        if self.catalogs.contains_key(&key) {
            return self.catalogs.get(&key);
        }

        // Find and load the .mo file
        if let Some(path) = self.find_mo_file(domain, locale) {
            if let Ok(mut file) = File::open(&path) {
                let mut data = Vec::new();
                if file.read_to_end(&mut data).is_ok() {
                    if let Ok(mo) = MoFile::read(&data) {
                        let catalog = MessageCatalog::from_mo_file(&mo);
                        self.catalogs.insert(key.clone(), catalog);
                        return self.catalogs.get(&key);
                    }
                }
            }
        }

        None
    }

    /// Look up a message
    pub fn gettext(&mut self, domain: &str, msgid: &str) -> Option<&str> {
        let locale = Self::get_current_locale();

        if let Some(catalog) = self.load_catalog(domain, &locale) {
            catalog.gettext(msgid)
        } else {
            None
        }
    }

    /// Look up a message with context
    pub fn pgettext(&mut self, domain: &str, msgctxt: &str, msgid: &str) -> Option<&str> {
        let locale = Self::get_current_locale();

        if let Some(catalog) = self.load_catalog(domain, &locale) {
            catalog.pgettext(msgctxt, msgid)
        } else {
            None
        }
    }

    /// Look up a plural message
    pub fn ngettext(
        &mut self,
        domain: &str,
        msgid: &str,
        msgid_plural: &str,
        n: u64,
    ) -> Option<&str> {
        let locale = Self::get_current_locale();

        if let Some(catalog) = self.load_catalog(domain, &locale) {
            catalog.ngettext(msgid, msgid_plural, n)
        } else {
            None
        }
    }

    /// Look up a plural message with context
    pub fn npgettext(
        &mut self,
        domain: &str,
        msgctxt: &str,
        msgid: &str,
        msgid_plural: &str,
        n: u64,
    ) -> Option<&str> {
        let locale = Self::get_current_locale();

        if let Some(catalog) = self.load_catalog(domain, &locale) {
            catalog.npgettext(msgctxt, msgid, msgid_plural, n)
        } else {
            None
        }
    }
}

impl Default for MessageLookup {
    fn default() -> Self {
        Self::new()
    }
}

/// Expand escape sequences in a string (for gettext -e flag)
pub fn expand_escapes(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.peek() {
                Some('n') => {
                    chars.next();
                    result.push('\n');
                }
                Some('t') => {
                    chars.next();
                    result.push('\t');
                }
                Some('r') => {
                    chars.next();
                    result.push('\r');
                }
                Some('\\') => {
                    chars.next();
                    result.push('\\');
                }
                Some('a') => {
                    chars.next();
                    result.push('\x07'); // bell
                }
                Some('b') => {
                    chars.next();
                    result.push('\x08'); // backspace
                }
                Some('f') => {
                    chars.next();
                    result.push('\x0c'); // form feed
                }
                Some('v') => {
                    chars.next();
                    result.push('\x0b'); // vertical tab
                }
                Some('0') => {
                    // Octal escape
                    chars.next();
                    let mut value = 0u8;
                    for _ in 0..3 {
                        match chars.peek() {
                            Some(d @ '0'..='7') => {
                                value = value * 8 + (*d as u8 - b'0');
                                chars.next();
                            }
                            _ => break,
                        }
                    }
                    if value != 0 {
                        result.push(value as char);
                    }
                }
                Some('x') => {
                    // Hex escape
                    chars.next();
                    let mut value = 0u8;
                    for _ in 0..2 {
                        match chars.peek() {
                            Some(d @ '0'..='9') => {
                                value = value * 16 + (*d as u8 - b'0');
                                chars.next();
                            }
                            Some(d @ 'a'..='f') => {
                                value = value * 16 + (*d as u8 - b'a' + 10);
                                chars.next();
                            }
                            Some(d @ 'A'..='F') => {
                                value = value * 16 + (*d as u8 - b'A' + 10);
                                chars.next();
                            }
                            _ => break,
                        }
                    }
                    if value != 0 {
                        result.push(value as char);
                    }
                }
                Some('c') => {
                    // \c suppresses further output (shell-style)
                    break;
                }
                _ => {
                    // Unknown escape - keep the backslash
                    result.push('\\');
                }
            }
        } else {
            result.push(c);
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_locale_variants() {
        let variants = MessageLookup::get_locale_variants("es_ES.UTF-8@valencia");
        assert!(variants.contains(&"es_ES.UTF-8@valencia".to_string()));
        assert!(variants.contains(&"es_ES@valencia".to_string()));
        assert!(variants.contains(&"es_ES.UTF-8".to_string()));
        assert!(variants.contains(&"es_ES".to_string()));
        assert!(variants.contains(&"es@valencia".to_string()));
        assert!(variants.contains(&"es".to_string()));
    }

    #[test]
    fn test_locale_variants_simple() {
        let variants = MessageLookup::get_locale_variants("en_US");
        assert!(variants.contains(&"en_US".to_string()));
        assert!(variants.contains(&"en".to_string()));
    }

    #[test]
    fn test_expand_escapes() {
        assert_eq!(expand_escapes("Hello\\nWorld"), "Hello\nWorld");
        assert_eq!(expand_escapes("Tab\\there"), "Tab\there");
        assert_eq!(expand_escapes("Back\\\\slash"), "Back\\slash");
        assert_eq!(expand_escapes("No escapes"), "No escapes");
    }

    #[test]
    fn test_expand_escapes_hex() {
        assert_eq!(expand_escapes("\\x41"), "A");
        assert_eq!(expand_escapes("\\x61"), "a");
    }

    #[test]
    fn test_expand_escapes_c() {
        assert_eq!(expand_escapes("Hello\\cWorld"), "Hello");
    }
}
