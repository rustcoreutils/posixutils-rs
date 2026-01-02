//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Message catalog for runtime message lookups
//!
//! This module provides the `MessageCatalog` type which is the runtime
//! representation of a message catalog, used for looking up translated
//! messages.

use std::collections::HashMap;

use crate::gettext_lib::mo_file::MoFile;
use crate::gettext_lib::plural::{parse_plural_forms, PluralExpr};

/// Runtime message catalog for lookups
#[derive(Debug, Clone)]
pub struct MessageCatalog {
    /// Character set
    pub charset: String,
    /// Number of plural forms
    pub nplurals: usize,
    /// Parsed plural expression
    pub plural_expr: Option<PluralExpr>,
    /// Map from msgid to msgstr (singular messages)
    pub messages: HashMap<String, String>,
    /// Map from (msgid, msgid_plural) to [msgstr[0], msgstr[1], ...] (plural messages)
    pub plural_messages: HashMap<(String, String), Vec<String>>,
}

impl MessageCatalog {
    /// Create a new empty message catalog
    pub fn new() -> Self {
        MessageCatalog {
            charset: "UTF-8".to_string(),
            nplurals: 2,
            plural_expr: None,
            messages: HashMap::new(),
            plural_messages: HashMap::new(),
        }
    }

    /// Create a message catalog from a MoFile
    pub fn from_mo_file(mo: &MoFile) -> Self {
        let mut catalog = MessageCatalog::new();

        // Set charset
        if let Some(ref cs) = mo.charset {
            catalog.charset = cs.clone();
        }

        // Parse plural forms
        if let Some(ref info) = mo.plural_info {
            catalog.nplurals = info.nplurals;
            if let Ok(expr) = PluralExpr::parse(&info.plural_expr) {
                catalog.plural_expr = Some(expr);
            }
        }

        // Import messages
        for (msgid, msgstr) in &mo.messages {
            // Check if this is a plural message (contains null separator)
            if msgid.contains('\0') {
                let parts: Vec<&str> = msgid.splitn(2, '\0').collect();
                if parts.len() == 2 {
                    let singular = parts[0].to_string();
                    let plural = parts[1].to_string();
                    let translations: Vec<String> =
                        msgstr.split('\0').map(|s| s.to_string()).collect();
                    catalog
                        .plural_messages
                        .insert((singular, plural), translations);
                }
            } else if msgstr.contains('\0') {
                // Single msgid but plural msgstr (less common format)
                let translations: Vec<String> = msgstr.split('\0').map(|s| s.to_string()).collect();
                if translations.len() > 1 {
                    // Treat as plural with same msgid for singular and plural
                    catalog
                        .plural_messages
                        .insert((msgid.clone(), msgid.clone()), translations);
                } else {
                    catalog.messages.insert(msgid.clone(), msgstr.clone());
                }
            } else {
                catalog.messages.insert(msgid.clone(), msgstr.clone());
            }
        }

        catalog
    }

    /// Look up a singular message
    pub fn gettext(&self, msgid: &str) -> Option<&str> {
        self.messages.get(msgid).map(|s| s.as_str())
    }

    /// Look up a message with context
    pub fn pgettext(&self, msgctxt: &str, msgid: &str) -> Option<&str> {
        // Context is encoded as "msgctxt\x04msgid" in the catalog
        let key = format!("{}\x04{}", msgctxt, msgid);
        self.messages.get(&key).map(|s| s.as_str())
    }

    /// Look up a plural message
    pub fn ngettext(&self, msgid: &str, msgid_plural: &str, n: u64) -> Option<&str> {
        let key = (msgid.to_string(), msgid_plural.to_string());

        if let Some(translations) = self.plural_messages.get(&key) {
            let index = self.get_plural_index(n);
            if index < translations.len() {
                return Some(&translations[index]);
            }
        }

        // Try just the msgid as a fallback (some catalogs store plural forms this way)
        if let Some(msgstr) = self.messages.get(msgid) {
            // If the message contains null separators, it's plural
            if msgstr.contains('\0') {
                let forms: Vec<&str> = msgstr.split('\0').collect();
                let index = self.get_plural_index(n);
                if index < forms.len() {
                    return Some(forms[index]);
                }
            }
        }

        None
    }

    /// Look up a plural message with context
    pub fn npgettext(
        &self,
        msgctxt: &str,
        msgid: &str,
        msgid_plural: &str,
        n: u64,
    ) -> Option<&str> {
        // Context is encoded as "msgctxt\x04msgid"
        let key_singular = format!("{}\x04{}", msgctxt, msgid);
        let key_plural = format!("{}\x04{}", msgctxt, msgid_plural);
        let key = (key_singular, key_plural);

        if let Some(translations) = self.plural_messages.get(&key) {
            let index = self.get_plural_index(n);
            if index < translations.len() {
                return Some(&translations[index]);
            }
        }

        None
    }

    /// Get the plural form index for a count
    fn get_plural_index(&self, n: u64) -> usize {
        if let Some(ref expr) = self.plural_expr {
            let index = expr.evaluate(n) as usize;
            // Clamp to valid range
            index.min(self.nplurals.saturating_sub(1))
        } else {
            // Default Germanic plural rule: n != 1
            if n == 1 {
                0
            } else {
                1.min(self.nplurals - 1)
            }
        }
    }

    /// Add a singular message
    pub fn add_message(&mut self, msgid: String, msgstr: String) {
        self.messages.insert(msgid, msgstr);
    }

    /// Add a plural message
    pub fn add_plural_message(
        &mut self,
        msgid: String,
        msgid_plural: String,
        translations: Vec<String>,
    ) {
        self.plural_messages
            .insert((msgid, msgid_plural), translations);
    }

    /// Set plural forms from a header string
    pub fn set_plural_forms(&mut self, header: &str) {
        if let Some((nplurals, expr)) = parse_plural_forms(header) {
            self.nplurals = nplurals;
            self.plural_expr = Some(expr);
        }
    }
}

impl Default for MessageCatalog {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_lookup() {
        let mut catalog = MessageCatalog::new();
        catalog.add_message("Hello".to_string(), "Hola".to_string());

        assert_eq!(catalog.gettext("Hello"), Some("Hola"));
        assert_eq!(catalog.gettext("NotFound"), None);
    }

    #[test]
    fn test_plural_lookup() {
        let mut catalog = MessageCatalog::new();
        catalog.set_plural_forms("nplurals=2; plural=(n != 1);");
        catalog.add_plural_message(
            "One item".to_string(),
            "%d items".to_string(),
            vec!["Un elemento".to_string(), "%d elementos".to_string()],
        );

        assert_eq!(
            catalog.ngettext("One item", "%d items", 1),
            Some("Un elemento")
        );
        assert_eq!(
            catalog.ngettext("One item", "%d items", 0),
            Some("%d elementos")
        );
        assert_eq!(
            catalog.ngettext("One item", "%d items", 5),
            Some("%d elementos")
        );
    }

    #[test]
    fn test_context_lookup() {
        let mut catalog = MessageCatalog::new();
        catalog.add_message("menu\x04File".to_string(), "Archivo".to_string());
        catalog.add_message("File".to_string(), "Fichero".to_string());

        assert_eq!(catalog.pgettext("menu", "File"), Some("Archivo"));
        assert_eq!(catalog.gettext("File"), Some("Fichero"));
    }

    #[test]
    fn test_polish_plural() {
        let mut catalog = MessageCatalog::new();
        catalog.set_plural_forms(
            "nplurals=3; plural=(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);",
        );
        catalog.add_plural_message(
            "file".to_string(),
            "files".to_string(),
            vec![
                "plik".to_string(),   // singular
                "pliki".to_string(),  // few
                "plików".to_string(), // many
            ],
        );

        assert_eq!(catalog.ngettext("file", "files", 1), Some("plik"));
        assert_eq!(catalog.ngettext("file", "files", 2), Some("pliki"));
        assert_eq!(catalog.ngettext("file", "files", 5), Some("plików"));
        assert_eq!(catalog.ngettext("file", "files", 22), Some("pliki"));
    }
}
