//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Platform-specific locale discovery

use std::collections::BTreeSet;
use std::fs;
use std::path::PathBuf;

/// Paths where compiled locales are stored
#[cfg(target_os = "linux")]
pub fn locale_paths() -> Vec<PathBuf> {
    vec![
        PathBuf::from("/usr/lib/locale"),
        PathBuf::from("/usr/share/locale"),
    ]
}

/// Paths where compiled locales are stored
#[cfg(target_os = "macos")]
pub fn locale_paths() -> Vec<PathBuf> {
    vec![
        PathBuf::from("/usr/share/locale"),
        PathBuf::from("/usr/local/share/locale"),
    ]
}

/// Paths where compiled locales are stored (fallback)
#[cfg(not(any(target_os = "linux", target_os = "macos")))]
pub fn locale_paths() -> Vec<PathBuf> {
    vec![
        PathBuf::from("/usr/share/locale"),
        PathBuf::from("/usr/local/share/locale"),
    ]
}

/// Paths where charmap files are stored
#[cfg(target_os = "linux")]
pub fn charmap_paths() -> Vec<PathBuf> {
    vec![PathBuf::from("/usr/share/i18n/charmaps")]
}

/// Paths where charmap files are stored
#[cfg(not(target_os = "linux"))]
pub fn charmap_paths() -> Vec<PathBuf> {
    vec![]
}

/// List all available locales
pub fn list_available_locales() -> Vec<String> {
    let mut locales = BTreeSet::new();

    // Always include POSIX and C
    locales.insert("POSIX".to_string());
    locales.insert("C".to_string());
    locales.insert("C.UTF-8".to_string());

    // Scan locale directories
    for base_path in locale_paths() {
        if let Ok(entries) = fs::read_dir(&base_path) {
            for entry in entries.flatten() {
                if let Ok(file_type) = entry.file_type() {
                    if file_type.is_dir() {
                        if let Some(name) = entry.file_name().to_str() {
                            // Skip common non-locale directories
                            if !name.starts_with('.') && name != "LC_MESSAGES" {
                                locales.insert(name.to_string());
                            }
                        }
                    }
                }
            }
        }

        // On Linux, also check for locale-archive
        #[cfg(target_os = "linux")]
        {
            let archive_path = base_path.join("locale-archive");
            if archive_path.exists() {
                // The locale-archive contains many locales; we'd need to parse it
                // For now, try to get locales from the system
                if let Ok(output) = std::process::Command::new("locale").arg("-a").output() {
                    if output.status.success() {
                        let stdout = String::from_utf8_lossy(&output.stdout);
                        for line in stdout.lines() {
                            let line = line.trim();
                            if !line.is_empty() {
                                locales.insert(line.to_string());
                            }
                        }
                    }
                }
            }
        }
    }

    locales.into_iter().collect()
}

/// List all available charmaps
pub fn list_available_charmaps() -> Vec<String> {
    let mut charmaps = BTreeSet::new();

    for base_path in charmap_paths() {
        if let Ok(entries) = fs::read_dir(&base_path) {
            for entry in entries.flatten() {
                if let Ok(file_type) = entry.file_type() {
                    if file_type.is_file() {
                        if let Some(name) = entry.file_name().to_str() {
                            // Remove .gz extension if present
                            let name = name.strip_suffix(".gz").unwrap_or(name);
                            charmaps.insert(name.to_string());
                        }
                    }
                }
            }
        }
    }

    // If no charmaps found, return some common ones
    if charmaps.is_empty() {
        charmaps.insert("UTF-8".to_string());
        charmaps.insert("ISO-8859-1".to_string());
        charmaps.insert("ASCII".to_string());
    }

    charmaps.into_iter().collect()
}
