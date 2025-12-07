//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// String interning for pcc C99 compiler
//
// This module provides a global StringTable for interning strings across all
// compiler passes. Each unique string is stored once and referenced by a
// compact StringId (u32), providing:
// - Memory efficiency: each unique string stored once
// - O(1) equality comparison by ID
// - Reduced allocations throughout the compiler
//
// Design follows the pattern from:
// - TypeTable in types.rs (interned type references)
// - IdentTable in token/lexer.rs (lexer-local identifier interning)
// - https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html
//

use std::collections::HashMap;
use std::fmt;

// ============================================================================
// StringId - Unique identifier for interned strings
// ============================================================================

/// A unique identifier for an interned string
/// Similar to TypeId for types, this provides O(1) equality comparisons
/// and compact storage (4 bytes vs 16+ bytes for String on 64-bit)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct StringId(pub u32);

impl StringId {
    /// Empty string ID (always ID 0, pre-interned)
    pub const EMPTY: StringId = StringId(0);
}

impl fmt::Display for StringId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "S{}", self.0)
    }
}

// ============================================================================
// StringTable - Interned string storage
// ============================================================================

/// Default capacity for string table allocations (reduces reallocation overhead)
const DEFAULT_STRING_TABLE_CAPACITY: usize = 2048;

/// String interner - stores all strings and provides ID-based lookup
///
/// Pattern follows TypeTable and IdentTable:
/// - HashMap for deduplication (string -> id)
/// - Vec for reverse lookup (id -> string)
///
/// All strings in the compiler should be interned through this table:
/// - Identifier names (variables, functions, types)
/// - Struct/union/enum member names
/// - Macro names
/// - Label names
/// - etc.
pub struct StringTable {
    /// Lookup map for deduplication: string content -> StringId
    map: HashMap<String, StringId>,
    /// All interned strings indexed by StringId
    strings: Vec<String>,
}

impl StringTable {
    /// Create a new string table with default capacity and empty string pre-interned
    pub fn new() -> Self {
        Self::with_capacity(DEFAULT_STRING_TABLE_CAPACITY)
    }

    /// Create a new string table with specified capacity and empty string pre-interned
    pub fn with_capacity(capacity: usize) -> Self {
        let mut table = Self {
            map: HashMap::with_capacity(capacity),
            strings: Vec::with_capacity(capacity),
        };
        // Pre-intern empty string as ID 0
        let empty_id = table.intern_internal("");
        debug_assert_eq!(empty_id, StringId::EMPTY);
        table
    }

    /// Internal interning without pre-interned string check
    fn intern_internal(&mut self, s: &str) -> StringId {
        let id = StringId(self.strings.len() as u32);
        self.strings.push(s.to_string());
        self.map.insert(s.to_string(), id);
        id
    }

    /// Intern a string, returning its unique ID
    ///
    /// If the string has been interned before, returns the existing ID.
    /// Otherwise, stores the string and returns a new ID.
    ///
    /// # Example
    /// ```ignore
    /// let mut strings = StringTable::new();
    /// let id1 = strings.intern("foo");
    /// let id2 = strings.intern("foo");
    /// assert_eq!(id1, id2);  // Same string -> same ID
    /// ```
    pub fn intern(&mut self, s: &str) -> StringId {
        if let Some(&id) = self.map.get(s) {
            return id;
        }
        self.intern_internal(s)
    }

    /// Get the string for an ID
    ///
    /// # Panics
    /// Panics if the ID is invalid
    pub fn get(&self, id: StringId) -> &str {
        &self.strings[id.0 as usize]
    }

    /// Get the string for an ID, returning None for invalid IDs
    pub fn get_opt(&self, id: StringId) -> Option<&str> {
        self.strings.get(id.0 as usize).map(|s| s.as_str())
    }
}

impl Default for StringTable {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Debug for StringTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("StringTable")
            .field("len", &self.strings.len())
            .finish()
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_string_is_zero() {
        let table = StringTable::new();
        assert_eq!(table.get(StringId::EMPTY), "");
    }

    #[test]
    fn test_intern_returns_same_id() {
        let mut table = StringTable::new();
        let id1 = table.intern("foo");
        let id2 = table.intern("foo");
        assert_eq!(id1, id2);
    }

    #[test]
    fn test_different_strings_different_ids() {
        let mut table = StringTable::new();
        let id1 = table.intern("foo");
        let id2 = table.intern("bar");
        assert_ne!(id1, id2);
    }

    #[test]
    fn test_get_returns_string() {
        let mut table = StringTable::new();
        let id = table.intern("hello");
        assert_eq!(table.get(id), "hello");
    }

    #[test]
    fn test_string_id_display() {
        let id = StringId(42);
        assert_eq!(format!("{}", id), "S42");
    }

    #[test]
    fn test_string_id_empty_constant() {
        assert_eq!(StringId::EMPTY.0, 0);
    }

    #[test]
    fn test_many_strings() {
        let mut table = StringTable::new();
        for i in 0..1000 {
            let s = format!("string_{}", i);
            let id = table.intern(&s);
            assert_eq!(table.get(id), s);
        }
    }
}
