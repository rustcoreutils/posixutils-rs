//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The archive symbol index, shared by ar and strip.

use object::{Object, ObjectSymbol, SymbolKind};

/// The names an archive's symbol index lists for one member: the symbols
/// it defines for other objects -- global or weak, defined or common.
/// A local symbol is not listed: no other object can resolve to it.
pub fn member_symbols(data: &[u8]) -> Vec<String> {
    let Ok(file) = object::read::File::parse(data) else {
        return Vec::new();
    };
    file.symbols()
        .filter(|s| s.is_global() && !s.is_undefined())
        .filter(|s| !matches!(s.kind(), SymbolKind::Section | SymbolKind::File))
        .filter_map(|s| s.name().ok().map(str::to_string))
        .collect()
}
