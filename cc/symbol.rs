//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Symbol table for pcc C99 compiler
// Based on sparse's scope-aware symbol management
//

use crate::strings::StringId;
use crate::types::TypeId;
use std::collections::HashMap;

// ============================================================================
// Symbol ID
// ============================================================================

/// Unique identifier for a symbol in the symbol table
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

// ============================================================================
// Namespace
// ============================================================================

/// C has multiple namespaces (C99 6.2.3)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Namespace {
    /// Ordinary identifiers: variables, functions, typedef names, enum constants
    Ordinary,
    /// Tags: struct, union, enum tags
    Tag,
}

// ============================================================================
// Symbol Kind
// ============================================================================

/// What kind of entity this symbol represents
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    /// A variable (local or global)
    Variable,
    /// A function
    Function,
    /// A function parameter
    Parameter,
    /// An enum constant
    EnumConstant,
    /// A struct/union/enum tag
    Tag,
    /// A typedef name
    Typedef,
}

// ============================================================================
// Symbol
// ============================================================================

/// A symbol in the symbol table
#[derive(Debug, Clone)]
pub struct Symbol {
    /// The symbol's name (interned StringId)
    pub name: StringId,

    /// What kind of symbol this is
    pub kind: SymbolKind,

    /// Which namespace this symbol belongs to
    pub namespace: Namespace,

    /// The type of this symbol (interned TypeId)
    pub typ: TypeId,

    /// Scope depth where this symbol was declared
    pub scope_depth: u32,

    /// Is this a definition (vs just a declaration)?
    pub defined: bool,

    /// Value for enum constants
    pub enum_value: Option<i64>,
}

impl Symbol {
    /// Create a new variable symbol
    pub fn variable(name: StringId, typ: TypeId, scope_depth: u32) -> Self {
        Self {
            name,
            kind: SymbolKind::Variable,
            namespace: Namespace::Ordinary,
            typ,
            scope_depth,
            defined: true,
            enum_value: None,
        }
    }

    /// Create a new function symbol
    pub fn function(name: StringId, typ: TypeId, scope_depth: u32) -> Self {
        Self {
            name,
            kind: SymbolKind::Function,
            namespace: Namespace::Ordinary,
            typ,
            scope_depth,
            defined: false, // Functions are declarations until we see the body
            enum_value: None,
        }
    }

    /// Create a new parameter symbol
    pub fn parameter(name: StringId, typ: TypeId, scope_depth: u32) -> Self {
        Self {
            name,
            kind: SymbolKind::Parameter,
            namespace: Namespace::Ordinary,
            typ,
            scope_depth,
            defined: true,
            enum_value: None,
        }
    }

    /// Create a new enum constant symbol (requires int_id from TypeTable)
    pub fn enum_constant(name: StringId, value: i64, int_id: TypeId, scope_depth: u32) -> Self {
        Self {
            name,
            kind: SymbolKind::EnumConstant,
            namespace: Namespace::Ordinary,
            typ: int_id,
            scope_depth,
            defined: true,
            enum_value: Some(value),
        }
    }

    /// Create a new tag symbol (struct/union/enum tag)
    pub fn tag(name: StringId, typ: TypeId, scope_depth: u32) -> Self {
        Self {
            name,
            kind: SymbolKind::Tag,
            namespace: Namespace::Tag,
            typ,
            scope_depth,
            defined: true,
            enum_value: None,
        }
    }

    /// Create a new typedef symbol
    pub fn typedef(name: StringId, typ: TypeId, scope_depth: u32) -> Self {
        Self {
            name,
            kind: SymbolKind::Typedef,
            namespace: Namespace::Ordinary,
            typ,
            scope_depth,
            defined: true,
            enum_value: None,
        }
    }

    /// Check if this is an enum constant
    pub fn is_enum_constant(&self) -> bool {
        self.kind == SymbolKind::EnumConstant
    }

    /// Check if this is a typedef
    pub fn is_typedef(&self) -> bool {
        self.kind == SymbolKind::Typedef
    }
}

// ============================================================================
// Scope
// ============================================================================

/// A scope in the symbol table
#[derive(Debug)]
struct Scope {
    /// Symbol IDs declared in this scope
    symbols: Vec<SymbolId>,
    /// Parent scope (None for global scope)
    parent: Option<u32>,
}

impl Scope {
    fn new(parent: Option<u32>) -> Self {
        Self {
            symbols: Vec::new(),
            parent,
        }
    }
}

// ============================================================================
// Symbol Table
// ============================================================================

/// Scope-aware symbol table
///
/// Implements C's scoping rules:
/// - File scope (global)
/// - Function scope (for labels)
/// - Block scope (compound statements)
/// - Function prototype scope (parameters in declarations)
pub struct SymbolTable {
    /// All symbols (indexed by SymbolId)
    symbols: Vec<Symbol>,

    /// All scopes (indexed by scope id)
    scopes: Vec<Scope>,

    /// Current scope id
    current_scope: u32,

    /// Current scope depth
    scope_depth: u32,

    /// Fast lookup: name -> list of symbol IDs with that name
    /// (most recent first, for shadowing)
    name_map: HashMap<(StringId, Namespace), Vec<SymbolId>>,
}

impl SymbolTable {
    /// Create a new symbol table with global scope
    pub fn new() -> Self {
        let mut table = Self {
            symbols: Vec::new(),
            scopes: Vec::new(),
            current_scope: 0,
            scope_depth: 0,
            name_map: HashMap::new(),
        };
        // Create the global scope
        table.scopes.push(Scope::new(None));
        table
    }

    /// Enter a new scope
    pub fn enter_scope(&mut self) {
        let new_scope_id = self.scopes.len() as u32;
        self.scopes.push(Scope::new(Some(self.current_scope)));
        self.current_scope = new_scope_id;
        self.scope_depth += 1;
    }

    /// Leave the current scope, returning to the parent
    pub fn leave_scope(&mut self) {
        if let Some(parent) = self.scopes[self.current_scope as usize].parent {
            // Remove symbols from name_map that were in this scope
            let scope = &self.scopes[self.current_scope as usize];
            for &sym_id in &scope.symbols {
                let sym = &self.symbols[sym_id.0 as usize];
                let key = (sym.name, sym.namespace);
                if let Some(ids) = self.name_map.get_mut(&key) {
                    // Remove this symbol from the list
                    ids.retain(|&id| id != sym_id);
                    if ids.is_empty() {
                        self.name_map.remove(&key);
                    }
                }
            }
            self.current_scope = parent;
            self.scope_depth -= 1;
        }
    }

    /// Get the current scope depth
    pub fn depth(&self) -> u32 {
        self.scope_depth
    }

    /// Declare a symbol in the current scope
    ///
    /// Returns the SymbolId on success, or an error if redefinition
    pub fn declare(&mut self, mut sym: Symbol) -> Result<SymbolId, SymbolError> {
        // Set scope depth
        sym.scope_depth = self.scope_depth;

        // Check for redefinition in the same scope
        let key = (sym.name, sym.namespace);
        if let Some(ids) = self.name_map.get(&key) {
            for &id in ids {
                let existing = &self.symbols[id.0 as usize];
                if existing.scope_depth == self.scope_depth {
                    // Same scope - check for redefinition
                    if existing.defined && sym.defined {
                        return Err(SymbolError::Redefinition(sym.name));
                    }
                }
            }
        }

        // Add the symbol
        let id = SymbolId(self.symbols.len() as u32);
        self.symbols.push(sym.clone());

        // Add to current scope
        self.scopes[self.current_scope as usize].symbols.push(id);

        // Add to name map (at front for shadowing)
        self.name_map.entry(key).or_default().insert(0, id);

        Ok(id)
    }

    /// Look up a symbol by name in the given namespace
    ///
    /// Searches from innermost scope outward
    pub fn lookup(&self, name: StringId, ns: Namespace) -> Option<&Symbol> {
        let key = (name, ns);
        self.name_map
            .get(&key)
            .and_then(|ids| ids.first().map(|id| &self.symbols[id.0 as usize]))
    }

    /// Look up a tag (struct/union/enum) by name
    pub fn lookup_tag(&self, name: StringId) -> Option<&Symbol> {
        self.lookup(name, Namespace::Tag)
    }

    /// Look up a typedef by name
    /// Returns the aliased TypeId if found
    pub fn lookup_typedef(&self, name: StringId) -> Option<TypeId> {
        self.lookup(name, Namespace::Ordinary).and_then(|s| {
            if s.is_typedef() {
                Some(s.typ) // TypeId is Copy
            } else {
                None
            }
        })
    }

    /// Get the value of an enum constant
    pub fn get_enum_value(&self, name: StringId) -> Option<i64> {
        self.lookup(name, Namespace::Ordinary).and_then(|s| {
            if s.is_enum_constant() {
                s.enum_value
            } else {
                None
            }
        })
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Errors
// ============================================================================

/// Symbol table errors
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolError {
    /// Attempted to redefine an existing symbol
    Redefinition(StringId),
}

impl std::fmt::Display for SymbolError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolError::Redefinition(name) => write!(f, "redefinition of '{}'", name),
        }
    }
}

impl std::error::Error for SymbolError {}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::strings::StringTable;
    use crate::types::{Type, TypeKind, TypeTable};

    #[test]
    fn test_declare_and_lookup() {
        let mut strings = StringTable::new();
        let types = TypeTable::new(64);
        let mut table = SymbolTable::new();

        let x_id = strings.intern("x");

        // Declare a variable
        let sym = Symbol::variable(x_id, types.int_id, 0);
        let _id = table.declare(sym).unwrap();

        // Look it up
        let found = table.lookup(x_id, Namespace::Ordinary).unwrap();
        assert_eq!(found.name, x_id);
        assert_eq!(found.kind, SymbolKind::Variable);
    }

    #[test]
    fn test_scopes() {
        let mut strings = StringTable::new();
        let types = TypeTable::new(64);
        let mut table = SymbolTable::new();

        let x_id = strings.intern("x");
        let y_id = strings.intern("y");

        // Declare x in global scope
        let sym1 = Symbol::variable(x_id, types.int_id, 0);
        table.declare(sym1).unwrap();

        // Enter a new scope
        table.enter_scope();

        // Declare y in inner scope
        let sym2 = Symbol::variable(y_id, types.char_id, 0);
        table.declare(sym2).unwrap();

        // Both should be visible
        assert!(table.lookup(x_id, Namespace::Ordinary).is_some());
        assert!(table.lookup(y_id, Namespace::Ordinary).is_some());

        // Leave scope
        table.leave_scope();

        // x should still be visible, y should not
        assert!(table.lookup(x_id, Namespace::Ordinary).is_some());
        assert!(table.lookup(y_id, Namespace::Ordinary).is_none());
    }

    #[test]
    fn test_shadowing() {
        let mut strings = StringTable::new();
        let types = TypeTable::new(64);
        let mut table = SymbolTable::new();

        let x_id = strings.intern("x");

        // Declare x as int in global scope
        let sym1 = Symbol::variable(x_id, types.int_id, 0);
        table.declare(sym1).unwrap();

        // Enter a new scope
        table.enter_scope();

        // Shadow x with char
        let sym2 = Symbol::variable(x_id, types.char_id, 0);
        table.declare(sym2).unwrap();

        // Should find the inner x (char)
        let found = table.lookup(x_id, Namespace::Ordinary).unwrap();
        assert_eq!(types.kind(found.typ), TypeKind::Char);

        // Leave scope
        table.leave_scope();

        // Should find the outer x (int)
        let found = table.lookup(x_id, Namespace::Ordinary).unwrap();
        assert_eq!(types.kind(found.typ), TypeKind::Int);
    }

    #[test]
    fn test_redefinition_error() {
        let mut strings = StringTable::new();
        let types = TypeTable::new(64);
        let mut table = SymbolTable::new();

        let x_id = strings.intern("x");

        // Declare x
        let sym1 = Symbol::variable(x_id, types.int_id, 0);
        table.declare(sym1).unwrap();

        // Try to redeclare x in the same scope
        let sym2 = Symbol::variable(x_id, types.char_id, 0);
        let result = table.declare(sym2);

        assert!(matches!(result, Err(SymbolError::Redefinition(_))));
    }

    #[test]
    fn test_scope_depth() {
        let mut table = SymbolTable::new();

        assert_eq!(table.depth(), 0);

        table.enter_scope();
        assert_eq!(table.depth(), 1);

        table.enter_scope();
        assert_eq!(table.depth(), 2);

        table.leave_scope();
        assert_eq!(table.depth(), 1);

        table.leave_scope();
        assert_eq!(table.depth(), 0);
    }

    #[test]
    fn test_function_symbol() {
        let mut strings = StringTable::new();
        let mut types = TypeTable::new(64);
        let mut table = SymbolTable::new();

        let foo_id = strings.intern("foo");

        // Declare a function
        let func_type = types.intern(Type::function(types.int_id, vec![types.int_id], false));
        let func = Symbol::function(foo_id, func_type, 0);
        table.declare(func).unwrap();

        let found = table.lookup(foo_id, Namespace::Ordinary).unwrap();
        assert_eq!(found.kind, SymbolKind::Function);
        assert!(!found.defined); // Not yet defined
    }
}
