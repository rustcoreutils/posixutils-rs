//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Locating and reading GNU `.mo` message catalogs.
//!
//! This is the read-only half of the format: enough to answer `gettext`. The
//! `i18n` crate has a fuller implementation (writing, plural-form expressions,
//! contexts) for the `gettext`/`msgfmt`/`xgettext` utilities, but it cannot be
//! reused here — it depends on this crate, so the edge would be circular.

use std::collections::HashMap;
use std::env;
use std::path::{Path, PathBuf};

const MO_MAGIC_LE: u32 = 0x9504_12de;
const MO_MAGIC_BE: u32 = 0xde12_0495;
const MO_HEADER_SIZE: usize = 28;

/// Size of one `(length, offset)` string descriptor.
const DESCRIPTOR_SIZE: usize = 8;

#[cfg(not(target_os = "macos"))]
const DEFAULT_LOCALE_PATHS: &[&str] = &[
    "/usr/share/locale",
    "/usr/local/share/locale",
    "/usr/lib/locale",
];

#[cfg(target_os = "macos")]
const DEFAULT_LOCALE_PATHS: &[&str] = &[
    "/usr/local/share/locale",
    "/opt/homebrew/share/locale",
    "/usr/share/locale",
];

/// The message table of one `.mo` file.
pub struct Catalog {
    messages: HashMap<String, String>,
}

impl Catalog {
    pub fn lookup(&self, msgid: &str) -> Option<&str> {
        // An empty translation means "untranslated" — the empty msgid is the
        // catalog's own metadata header, and an empty msgstr is a msgid that
        // was extracted but never translated.
        self.messages
            .get(msgid)
            .map(String::as_str)
            .filter(|s| !s.is_empty())
    }

    /// Parse a `.mo` image. Returns `None` for anything not recognizable, which
    /// includes every form of truncation or out-of-range offset.
    pub fn parse(data: &[u8]) -> Option<Catalog> {
        if data.len() < MO_HEADER_SIZE {
            return None;
        }
        let le = match u32::from_le_bytes(data[0..4].try_into().ok()?) {
            MO_MAGIC_LE => true,
            MO_MAGIC_BE => false,
            _ => return None,
        };
        let word = |off: usize| -> Option<u32> {
            let bytes: [u8; 4] = data.get(off..off + 4)?.try_into().ok()?;
            Some(if le {
                u32::from_le_bytes(bytes)
            } else {
                u32::from_be_bytes(bytes)
            })
        };

        if word(4)? > 1 {
            return None; // unsupported revision
        }
        let nstrings = word(8)? as usize;
        let orig_tab = word(12)? as usize;
        let trans_tab = word(16)? as usize;

        // Both string tables must lie inside the file *before* anything is sized
        // from `nstrings`. It is a `u32` straight out of an untrusted header, so
        // reserving the message map against it first turned a 28-byte file
        // claiming four billion strings into a 420 GB allocation and an abort —
        // the file is far too small to hold the tables such a count implies.
        let table_bytes = nstrings.checked_mul(DESCRIPTOR_SIZE)?;
        if orig_tab.checked_add(table_bytes)? > data.len()
            || trans_tab.checked_add(table_bytes)? > data.len()
        {
            return None;
        }

        let mut messages = HashMap::with_capacity(nstrings);
        for i in 0..nstrings {
            let orig = read_string(data, orig_tab.checked_add(i * DESCRIPTOR_SIZE)?, &word)?;
            let trans = read_string(data, trans_tab.checked_add(i * DESCRIPTOR_SIZE)?, &word)?;
            messages.insert(orig, trans);
        }
        Some(Catalog { messages })
    }
}

/// Read the string described by the descriptor at `desc_off`.
fn read_string(
    data: &[u8],
    desc_off: usize,
    word: &impl Fn(usize) -> Option<u32>,
) -> Option<String> {
    let len = word(desc_off)? as usize;
    let off = word(desc_off + 4)? as usize;
    let bytes = data.get(off..off.checked_add(len)?)?;
    String::from_utf8(bytes.to_vec()).ok()
}

/// Locate and read the catalog for `domain` under `locale`.
///
/// Search order, following GNU gettext and XSI:
///
/// 1. `NLSPATH` templates — a colon-separated list of path templates with `%N`
///    (domain), `%L` (full locale), `%l` (language), `%t` (territory) and `%c`
///    (codeset) substitutions. POSIX lists this for the XSI message-catalog
///    interface, and it takes precedence over everything else.
/// 2. The directory bound to this domain by `bindtextdomain`.
/// 3. `TEXTDOMAINDIR`.
/// 4. The system locale directories.
pub fn load(domain: &str, locale: &str, bound_dir: Option<&Path>) -> Option<Catalog> {
    let variants = locale_variants(locale);

    if let Ok(nlspath) = env::var("NLSPATH") {
        for template in nlspath.split(':').filter(|t| !t.is_empty()) {
            for variant in &variants {
                let path = PathBuf::from(expand_nlspath(template, domain, variant));
                if let Some(cat) = try_read(&path) {
                    return Some(cat);
                }
            }
        }
    }

    let mut roots: Vec<PathBuf> = Vec::new();
    if let Some(dir) = bound_dir {
        roots.push(dir.to_path_buf());
    }
    if let Ok(dir) = env::var("TEXTDOMAINDIR") {
        if !dir.is_empty() {
            roots.push(PathBuf::from(dir));
        }
    }
    roots.extend(DEFAULT_LOCALE_PATHS.iter().map(PathBuf::from));

    for root in &roots {
        for variant in &variants {
            let path = root
                .join(variant)
                .join("LC_MESSAGES")
                .join(format!("{domain}.mo"));
            if let Some(cat) = try_read(&path) {
                return Some(cat);
            }
            let alt = root.join(variant).join(format!("{domain}.mo"));
            if let Some(cat) = try_read(&alt) {
                return Some(cat);
            }
        }
    }
    None
}

fn try_read(path: &Path) -> Option<Catalog> {
    let data = std::fs::read(path).ok()?;
    Catalog::parse(&data)
}

/// Expand an `NLSPATH` template's `%` substitutions.
fn expand_nlspath(template: &str, domain: &str, locale: &str) -> String {
    let (base, _modifier) = split_once_or(locale, '@');
    let (lang_terr, codeset) = split_once_or(base, '.');
    let (language, territory) = split_once_or(lang_terr, '_');

    let mut out = String::with_capacity(template.len());
    let mut chars = template.chars();
    while let Some(c) = chars.next() {
        if c != '%' {
            out.push(c);
            continue;
        }
        match chars.next() {
            Some('N') => out.push_str(domain),
            Some('L') => out.push_str(locale),
            Some('l') => out.push_str(language),
            Some('t') => out.push_str(territory.unwrap_or("")),
            Some('c') => out.push_str(codeset.unwrap_or("")),
            Some('%') => out.push('%'),
            Some(other) => {
                out.push('%');
                out.push(other);
            }
            None => out.push('%'),
        }
    }
    out
}

fn split_once_or(s: &str, sep: char) -> (&str, Option<&str>) {
    match s.split_once(sep) {
        Some((a, b)) => (a, Some(b)),
        None => (s, None),
    }
}

/// The locale names to try, most specific first: `ll_TT.codeset@modifier` down
/// to bare `ll`.
fn locale_variants(locale: &str) -> Vec<String> {
    let (base, modifier) = split_once_or(locale, '@');
    let (lang_terr, codeset) = split_once_or(base, '.');
    let (language, territory) = split_once_or(lang_terr, '_');

    let mut variants = vec![locale.to_string()];
    if let Some(m) = modifier {
        if codeset.is_some() {
            variants.push(format!("{lang_terr}@{m}"));
        }
    }
    if codeset.is_some() || modifier.is_some() {
        variants.push(lang_terr.to_string());
    }
    if let Some(m) = modifier {
        if territory.is_some() {
            variants.push(format!("{language}@{m}"));
        }
    }
    if territory.is_some() {
        variants.push(language.to_string());
    }
    let mut seen = std::collections::HashSet::new();
    variants.retain(|v| seen.insert(v.clone()));
    variants
}

/// The locale in effect for `LC_MESSAGES`, per POSIX precedence: `LC_ALL`, then
/// `LC_MESSAGES`, then `LANG`.
pub fn message_locale() -> String {
    for var in ["LC_ALL", "LC_MESSAGES", "LANG"] {
        if let Ok(v) = env::var(var) {
            if !v.is_empty() {
                return v;
            }
        }
    }
    "C".to_string()
}

/// Whether `locale` disables translation. POSIX requires the `C`/`POSIX` locale
/// to produce the untranslated messages.
pub fn is_untranslated(locale: &str) -> bool {
    let (base, _) = split_once_or(locale, '@');
    let (lang_terr, _) = split_once_or(base, '.');
    matches!(lang_terr, "" | "C" | "POSIX")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a minimal little-endian `.mo` image.
    fn build_mo(entries: &[(&str, &str)]) -> Vec<u8> {
        let n = entries.len() as u32;
        let orig_tab = MO_HEADER_SIZE as u32;
        let trans_tab = orig_tab + n * DESCRIPTOR_SIZE as u32;
        let strings_at = trans_tab + n * DESCRIPTOR_SIZE as u32;

        let mut blob: Vec<u8> = Vec::new();
        let mut origs = Vec::new();
        let mut transs = Vec::new();
        for (id, s) in entries {
            origs.push((id.len() as u32, strings_at + blob.len() as u32));
            blob.extend_from_slice(id.as_bytes());
            blob.push(0);
            transs.push((s.len() as u32, strings_at + blob.len() as u32));
            blob.extend_from_slice(s.as_bytes());
            blob.push(0);
        }

        let mut out = Vec::new();
        out.extend_from_slice(&MO_MAGIC_LE.to_le_bytes());
        out.extend_from_slice(&0u32.to_le_bytes()); // revision
        out.extend_from_slice(&n.to_le_bytes());
        out.extend_from_slice(&orig_tab.to_le_bytes());
        out.extend_from_slice(&trans_tab.to_le_bytes());
        out.extend_from_slice(&0u32.to_le_bytes()); // hash size
        out.extend_from_slice(&0u32.to_le_bytes()); // hash offset
        for (len, off) in origs.into_iter().chain(transs) {
            out.extend_from_slice(&len.to_le_bytes());
            out.extend_from_slice(&off.to_le_bytes());
        }
        out.extend_from_slice(&blob);
        out
    }

    #[test]
    fn parses_a_catalog_and_looks_messages_up() {
        let mo = build_mo(&[
            ("", "Content-Type: text/plain; charset=UTF-8\n"),
            ("Hello", "Hola"),
        ]);
        let cat = Catalog::parse(&mo).expect("valid catalog");
        assert_eq!(cat.lookup("Hello"), Some("Hola"));
        assert_eq!(cat.lookup("Goodbye"), None);
        // An empty translation is "untranslated", not an empty message.
        assert_eq!(
            Catalog::parse(&build_mo(&[("x", "")])).unwrap().lookup("x"),
            None
        );
    }

    #[test]
    fn malformed_catalogs_are_refused_not_trusted() {
        // A `.mo` file is untrusted input; none of these may panic.
        assert!(Catalog::parse(b"").is_none());
        assert!(Catalog::parse(b"not a catalog at all, just some bytes...").is_none());
        let mut truncated = build_mo(&[("Hello", "Hola")]);
        truncated.truncate(40);
        assert!(Catalog::parse(&truncated).is_none());
        // A string descriptor pointing past the end of the file.
        let mut bogus = build_mo(&[("Hello", "Hola")]);
        let off = MO_HEADER_SIZE + 4;
        bogus[off..off + 4].copy_from_slice(&u32::MAX.to_le_bytes());
        assert!(Catalog::parse(&bogus).is_none());
        // A string length that overflows when added to its offset.
        let mut huge = build_mo(&[("Hello", "Hola")]);
        huge[MO_HEADER_SIZE..MO_HEADER_SIZE + 4].copy_from_slice(&u32::MAX.to_le_bytes());
        assert!(Catalog::parse(&huge).is_none());
    }

    #[test]
    fn big_endian_catalogs_are_read() {
        const ENTRIES: usize = 1;
        let mut be = build_mo(&[("Hello", "Hola")]);
        // Byte-swap every 32-bit word of the header and of both string tables;
        // the string data itself is bytes, not words.
        let words = MO_HEADER_SIZE / 4 + 2 * ENTRIES * (DESCRIPTOR_SIZE / 4);
        for w in 0..words {
            be[w * 4..w * 4 + 4].reverse();
        }
        // Reversing word 0 already produced the big-endian magic: those bytes
        // read as a little-endian word are MO_MAGIC_BE, which is the marker.
        assert_eq!(
            u32::from_le_bytes(be[0..4].try_into().unwrap()),
            MO_MAGIC_BE
        );
        assert_eq!(Catalog::parse(&be).unwrap().lookup("Hello"), Some("Hola"));
    }

    #[test]
    fn nlspath_templates_expand() {
        assert_eq!(
            expand_nlspath("/cat/%L/%N.cat", "man", "de_DE.UTF-8@euro"),
            "/cat/de_DE.UTF-8@euro/man.cat"
        );
        assert_eq!(
            expand_nlspath("/n/%l/%t/%c/%N", "man", "de_DE.UTF-8"),
            "/n/de/DE/UTF-8/man"
        );
        // A locale with no territory or codeset leaves those empty.
        assert_eq!(expand_nlspath("%l-%t-%c", "d", "fr"), "fr--");
        // `%%` is a literal percent; an unknown escape is left alone.
        assert_eq!(expand_nlspath("100%%%z", "d", "fr"), "100%%z");
    }

    #[test]
    fn locale_variants_go_from_specific_to_general() {
        assert_eq!(
            locale_variants("de_DE.UTF-8@euro"),
            ["de_DE.UTF-8@euro", "de_DE@euro", "de_DE", "de@euro", "de"]
        );
        assert_eq!(locale_variants("fr_FR"), ["fr_FR", "fr"]);
        assert_eq!(locale_variants("C"), ["C"]);
    }

    #[test]
    fn c_and_posix_locales_disable_translation() {
        assert!(is_untranslated("C"));
        assert!(is_untranslated("POSIX"));
        assert!(is_untranslated("C.UTF-8"));
        assert!(is_untranslated(""));
        assert!(!is_untranslated("de_DE.UTF-8"));
        assert!(!is_untranslated("fr"));
    }

    #[test]
    fn a_huge_string_count_does_not_allocate() {
        // A 28-byte header claiming four billion strings. The count is used as
        // the message map's capacity, so trusting it reserves gigabytes — an
        // abort — before a single offset has been validated.
        let mut mo = Vec::new();
        mo.extend_from_slice(&MO_MAGIC_LE.to_le_bytes());
        for w in [
            0u32,
            u32::MAX,
            MO_HEADER_SIZE as u32,
            MO_HEADER_SIZE as u32,
            0,
            0,
        ] {
            mo.extend_from_slice(&w.to_le_bytes());
        }
        assert_eq!(mo.len(), MO_HEADER_SIZE);
        assert!(Catalog::parse(&mo).is_none());

        // The same shape at a size that would merely be very slow rather than
        // fatal, so the check is on the tables fitting rather than on a
        // magic threshold.
        let mut mo = Vec::new();
        mo.extend_from_slice(&MO_MAGIC_LE.to_le_bytes());
        for w in [
            0u32,
            100_000,
            MO_HEADER_SIZE as u32,
            MO_HEADER_SIZE as u32,
            0,
            0,
        ] {
            mo.extend_from_slice(&w.to_le_bytes());
        }
        assert!(Catalog::parse(&mo).is_none());
    }
}
