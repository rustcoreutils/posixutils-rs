//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parser for pcc C99 compiler
// Recursive descent parser with Pratt-style precedence climbing
//

use super::ast::{
    AsmOperand, BinaryOp, BlockItem, Declaration, Designator, Expr, ExprKind, ExternalDecl,
    ForInit, FunctionDef, InitDeclarator, InitElement, OffsetOfPath, Parameter, Stmt,
    TranslationUnit, UnaryOp,
};
use crate::diag;
use crate::strings::StringId;
use crate::symbol::{Namespace, Symbol, SymbolId, SymbolTable};
use crate::token::lexer::{IdentTable, Position, SpecialToken, Token, TokenType, TokenValue};
use crate::types::{
    CompositeType, EnumConstant, StructMember, Type, TypeId, TypeKind, TypeModifiers, TypeTable,
};
use std::fmt;

const DEFAULT_MEMBER_CAPACITY: usize = 16;
const DEFAULT_ENUM_CAPACITY: usize = 16;
const DEFAULT_PARAM_CAPACITY: usize = 8;

// ============================================================================
// Parse Error
// ============================================================================

/// Parse error type
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub pos: Position,
}

impl ParseError {
    pub fn new(message: impl Into<String>, pos: Position) -> Self {
        Self {
            message: message.into(),
            pos,
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}: {}", self.pos.line, self.pos.col, self.message)
    }
}

impl std::error::Error for ParseError {}

pub type ParseResult<T> = Result<T, ParseError>;

/// Raw parameter info: (name, type) - name is None for unnamed params
type RawParam = (Option<StringId>, TypeId);

/// Result of parsing a declarator: (name, type, VLA expressions, raw function parameters)
type DeclaratorResult = (StringId, TypeId, Vec<Expr>, Option<Vec<RawParam>>);

/// Function parameter type info: (type IDs, is_variadic)
type FuncParamTypes = (Vec<TypeId>, bool);

// ============================================================================
// GCC __attribute__ Support
// ============================================================================

/// An argument to a GCC __attribute__
#[derive(Debug, Clone, PartialEq)]
pub enum AttributeArg {
    /// Identifier argument (e.g., `noreturn`, `__printf__`)
    Ident(String),
    /// String literal argument (e.g., `"default"`)
    String(String),
    /// Integer argument (e.g., `16` in `aligned(16)`)
    Int(i64),
    /// Nested arguments (e.g., `__format__(__printf__, 1, 2)`)
    Nested(Vec<AttributeArg>),
}

/// A single GCC __attribute__
#[derive(Debug, Clone)]
pub struct Attribute {
    /// Attribute name (e.g., `packed`, `aligned`, `visibility`)
    pub name: String,
    /// Arguments to the attribute (may be empty)
    pub args: Vec<AttributeArg>,
}

impl Attribute {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            args: Vec::new(),
        }
    }

    pub fn with_args(name: impl Into<String>, args: Vec<AttributeArg>) -> Self {
        Self {
            name: name.into(),
            args,
        }
    }
}

impl fmt::Display for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.args.is_empty() {
            write!(f, "(")?;
            for (i, arg) in self.args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                match arg {
                    AttributeArg::Ident(s) => write!(f, "{}", s)?,
                    AttributeArg::String(s) => write!(f, "\"{}\"", s)?,
                    AttributeArg::Int(n) => write!(f, "{}", n)?,
                    AttributeArg::Nested(args) => {
                        for (j, a) in args.iter().enumerate() {
                            if j > 0 {
                                write!(f, ", ")?;
                            }
                            match a {
                                AttributeArg::Ident(s) => write!(f, "{}", s)?,
                                AttributeArg::String(s) => write!(f, "\"{}\"", s)?,
                                AttributeArg::Int(n) => write!(f, "{}", n)?,
                                AttributeArg::Nested(_) => write!(f, "...")?,
                            }
                        }
                    }
                }
            }
            write!(f, ")")?;
        }
        Ok(())
    }
}

/// A list of GCC __attribute__ declarations
#[derive(Debug, Clone, Default)]
pub struct AttributeList {
    pub attrs: Vec<Attribute>,
}

impl AttributeList {
    pub fn new() -> Self {
        Self { attrs: Vec::new() }
    }

    pub fn push(&mut self, attr: Attribute) {
        self.attrs.push(attr);
    }

    /// Check if this attribute list contains a noreturn attribute
    /// (either "noreturn" or "__noreturn__")
    pub fn has_noreturn(&self) -> bool {
        self.attrs
            .iter()
            .any(|a| a.name == "noreturn" || a.name == "__noreturn__")
    }

    /// Check if this attribute list contains sysv_abi attribute
    pub fn has_sysv_abi(&self) -> bool {
        self.attrs
            .iter()
            .any(|a| a.name == "sysv_abi" || a.name == "__sysv_abi__")
    }

    /// Check if this attribute list contains ms_abi attribute
    pub fn has_ms_abi(&self) -> bool {
        self.attrs
            .iter()
            .any(|a| a.name == "ms_abi" || a.name == "__ms_abi__")
    }

    /// Get the calling convention from attributes, if any
    pub fn calling_conv(&self) -> Option<crate::abi::CallingConv> {
        if self.has_sysv_abi() {
            Some(crate::abi::CallingConv::SysV)
        } else if self.has_ms_abi() {
            Some(crate::abi::CallingConv::Win64)
        } else {
            None
        }
    }
}

impl fmt::Display for AttributeList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.attrs.is_empty() {
            return Ok(());
        }
        write!(f, "__attribute__((")?;
        for (i, attr) in self.attrs.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", attr)?;
        }
        write!(f, "))")
    }
}

// ============================================================================
// Parser
// ============================================================================

/// C expression parser using recursive descent with precedence climbing
///
/// The parser binds symbols to the symbol table during parsing. This means
/// that by the time parsing is complete, all declared symbols are in the
/// table with their types.
pub struct Parser<'a> {
    /// Token stream
    tokens: &'a [Token],
    /// Identifier table for looking up names
    pub(crate) idents: &'a IdentTable,
    /// Symbol table for binding declarations
    pub(crate) symbols: &'a mut SymbolTable,
    /// Type table for interning types
    pub(crate) types: &'a mut TypeTable,
    /// Current position in token stream
    pub(crate) pos: usize,
    /// Explicit alignment from _Alignas in current declaration
    /// Cleared after each declaration is parsed.
    pending_alignas: Option<u32>,
}

impl<'a> Parser<'a> {
    /// Create a new parser with a symbol table and type table
    pub fn new(
        tokens: &'a [Token],
        idents: &'a IdentTable,
        symbols: &'a mut SymbolTable,
        types: &'a mut TypeTable,
    ) -> Self {
        Self {
            tokens,
            idents,
            symbols,
            types,
            pos: 0,
            pending_alignas: None,
        }
    }

    // ========================================================================
    // Token Navigation
    // ========================================================================

    /// Get the current token
    pub(crate) fn current(&self) -> &Token {
        self.tokens
            .get(self.pos)
            .unwrap_or(&self.tokens[self.tokens.len() - 1])
    }

    /// Peek at the current token type
    pub(crate) fn peek(&self) -> TokenType {
        self.current().typ
    }

    /// Peek at the current token's special value (if it's a Special token)
    pub(crate) fn peek_special(&self) -> Option<u32> {
        let token = self.current();
        if token.typ == TokenType::Special {
            if let TokenValue::Special(v) = &token.value {
                return Some(*v);
            }
        }
        None
    }

    /// Check if current token is a specific special character
    pub(crate) fn is_special(&self, c: u8) -> bool {
        self.peek_special() == Some(c as u32)
    }

    /// Check if current token is a specific multi-char special token
    pub(crate) fn is_special_token(&self, tok: SpecialToken) -> bool {
        self.peek_special() == Some(tok as u32)
    }

    /// Get current position for error messages
    pub(crate) fn current_pos(&self) -> Position {
        self.current().pos
    }

    /// Advance to the next token
    pub(crate) fn advance(&mut self) {
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
    }

    /// Consume a token and advance, returning a clone
    pub(crate) fn consume(&mut self) -> Token {
        let token = self.current().clone();
        self.advance();
        token
    }

    /// Expect a specific special character, return error if not found
    pub(crate) fn expect_special(&mut self, c: u8) -> ParseResult<()> {
        if self.is_special(c) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::new(
                format!("expected '{}'", c as char),
                self.current_pos(),
            ))
        }
    }

    /// Get the identifier name from an Ident token value
    pub(crate) fn get_ident_name(&self, token: &Token) -> Option<String> {
        if let TokenValue::Ident(id) = &token.value {
            self.idents.get_opt(*id).map(|s| s.to_string())
        } else {
            None
        }
    }

    /// Get the StringId directly from an Ident token
    pub(crate) fn get_ident_id(&self, token: &Token) -> Option<StringId> {
        if let TokenValue::Ident(id) = &token.value {
            Some(*id)
        } else {
            None
        }
    }

    /// Get string value for a StringId
    #[inline]
    pub(crate) fn str(&self, id: StringId) -> &str {
        self.idents.get(id)
    }

    /// Check if current position (after consuming '(') indicates a grouped declarator.
    ///
    /// Grouped declarators include:
    /// - Pointer declarators: `(*name)` or `(*)`
    /// - Function type typedefs: `(name)` where name is not a type
    ///
    /// Must be called after advancing past '('. Saves/restores position internally
    /// for the function-type check.
    fn is_grouped_declarator(&mut self) -> bool {
        // Check for pointer: (*name) or (*name[...]) etc
        if self.is_special(b'*') {
            return true;
        }

        // Check for grouped declarator: (name...) where name is NOT a type
        // This handles cases like:
        //   (name)     - function type typedef
        //   (name[N])  - parenthesized array declarator
        //   (name(...)) - parenthesized function declarator
        // Following sparse's is_nested() logic: if identifier is not a type, it's grouped
        if self.peek() == TokenType::Ident {
            if let Some(name_id) = self.get_ident_id(self.current()) {
                let is_type = self.symbols.lookup_typedef(name_id).is_some()
                    || Self::is_type_keyword(self.str(name_id));
                // If not a type, this is a grouped declarator
                return !is_type;
            }
        }

        false
    }

    /// Resolve an incomplete struct/union type to its complete definition.
    ///
    /// When a struct is forward-declared (e.g., `struct foo;`) and later
    /// defined, the forward declaration creates an incomplete TypeId.
    /// Pointers to the forward-declared type still reference this incomplete
    /// TypeId even after the struct is fully defined with a new TypeId.
    ///
    /// This method looks up the complete definition in the symbol table
    /// using the struct's tag name, returning the complete TypeId if found.
    pub(crate) fn resolve_struct_type(&self, type_id: TypeId) -> TypeId {
        let typ = self.types.get(type_id);

        // Only try to resolve struct/union types
        if typ.kind != TypeKind::Struct && typ.kind != TypeKind::Union {
            return type_id;
        }

        // Check if this is an incomplete type with a tag
        if let Some(ref composite) = typ.composite {
            if composite.is_complete {
                // Already complete, no resolution needed
                return type_id;
            }
            if let Some(tag) = composite.tag {
                // Look up the tag in the symbol table to find the complete type
                if let Some(symbol) = self.symbols.lookup_tag(tag) {
                    // Return the complete type from the symbol table
                    return symbol.typ;
                }
            }
        }

        // Couldn't resolve, return original
        type_id
    }

    /// Intern a type, but for struct/union types with tags, check the symbol table
    /// first to reuse the existing TypeId. This ensures forward-declared types
    /// are properly linked when the type is later completed.
    ///
    /// Important: Storage class modifiers (static, extern, etc.) are preserved from
    /// the input type even when reusing an existing struct TypeId.
    fn intern_type_with_tag(&mut self, typ: &Type) -> TypeId {
        // For struct/union types with a tag, use the existing TypeId from symbol table
        if matches!(typ.kind, TypeKind::Struct | TypeKind::Union) {
            if let Some(ref composite) = typ.composite {
                if let Some(tag) = composite.tag {
                    if let Some(existing) = self.symbols.lookup_tag(tag) {
                        // Check if we need to preserve type qualifiers (not storage class)
                        // Storage class (TYPEDEF, EXTERN, STATIC, etc.) is a property of
                        // the declaration, not the type. TYPEDEF especially must NOT create
                        // a new TypeId, otherwise "typedef struct Foo Foo;" creates a different
                        // TypeId than the tag, and when "struct Foo { ... };" completes the tag,
                        // the typedef still points to the incomplete type.
                        let type_qualifier_mask = TypeModifiers::CONST
                            | TypeModifiers::VOLATILE
                            | TypeModifiers::RESTRICT
                            | TypeModifiers::ATOMIC;
                        let new_qualifiers = typ.modifiers & type_qualifier_mask;
                        if !new_qualifiers.is_empty() {
                            // Create a new type with the existing struct's data but new qualifiers
                            let mut existing_type = self.types.get(existing.typ).clone();
                            existing_type.modifiers |= new_qualifiers;
                            return self.types.intern(existing_type);
                        }
                        return existing.typ;
                    }
                }
            }
        }
        // For other types, just intern normally
        self.types.intern(typ.clone())
    }

    /// Skip StreamBegin tokens (but not StreamEnd - that marks EOF)
    pub fn skip_stream_tokens(&mut self) {
        while self.peek() == TokenType::StreamBegin {
            self.advance();
        }
    }

    /// Check if we're at end of input
    fn is_eof(&self) -> bool {
        matches!(self.peek(), TokenType::StreamEnd)
    }

    /// Check if current token is __attribute__ or __attribute
    fn is_attribute_keyword(&self) -> bool {
        if self.peek() != TokenType::Ident {
            return false;
        }
        if let Some(name) = self.get_ident_name(self.current()) {
            name == "__attribute__" || name == "__attribute"
        } else {
            false
        }
    }

    /// Parse a single attribute argument
    /// Returns None if not a recognizable argument
    fn parse_attribute_arg(&mut self) -> Option<AttributeArg> {
        match self.peek() {
            TokenType::Ident => {
                let name = self.get_ident_name(self.current())?;
                self.advance();

                // Check if this identifier has nested arguments
                if self.is_special(b'(') {
                    self.advance();
                    let mut nested = Vec::new();
                    while !self.is_special(b')') && !self.is_eof() {
                        if let Some(arg) = self.parse_attribute_arg() {
                            nested.push(arg);
                        }
                        if self.is_special(b',') {
                            self.advance();
                        } else if !self.is_special(b')') {
                            // Skip unknown tokens
                            self.advance();
                        }
                    }
                    if self.is_special(b')') {
                        self.advance();
                    }
                    // Return as nested with first element being the function name
                    let mut all_args = vec![AttributeArg::Ident(name)];
                    all_args.extend(nested);
                    Some(AttributeArg::Nested(all_args))
                } else {
                    Some(AttributeArg::Ident(name))
                }
            }
            TokenType::String => {
                if let TokenValue::String(s) = &self.current().value {
                    let s = s.clone();
                    self.advance();
                    Some(AttributeArg::String(s))
                } else {
                    None
                }
            }
            TokenType::Number => {
                if let TokenValue::Number(s) = &self.current().value {
                    // Parse the number string to i64
                    let n = s.parse::<i64>().unwrap_or(0);
                    self.advance();
                    Some(AttributeArg::Int(n))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Parse a single attribute: name or name(args)
    fn parse_single_attribute(&mut self) -> Option<Attribute> {
        if self.peek() != TokenType::Ident {
            return None;
        }

        let name = self.get_ident_name(self.current())?;
        self.advance();

        // Check for arguments
        if self.is_special(b'(') {
            self.advance();
            let mut args = Vec::new();

            while !self.is_special(b')') && !self.is_eof() {
                if let Some(arg) = self.parse_attribute_arg() {
                    args.push(arg);
                }
                if self.is_special(b',') {
                    self.advance();
                } else if !self.is_special(b')') {
                    // Skip unknown tokens
                    self.advance();
                }
            }

            if self.is_special(b')') {
                self.advance();
            }

            Some(Attribute::with_args(name, args))
        } else {
            Some(Attribute::new(name))
        }
    }

    /// Parse __attribute__((...)) declarations (GCC extension)
    ///
    /// Syntax: __attribute__((attr1, attr2(args), ...))
    /// Returns the parsed attributes. Currently a no-op for code generation,
    /// but attributes are captured for diagnostics.
    fn parse_attributes(&mut self) -> AttributeList {
        let mut result = AttributeList::new();

        while self.is_attribute_keyword() {
            self.advance(); // consume __attribute__

            // Expect first '('
            if !self.is_special(b'(') {
                return result;
            }
            self.advance();

            // Expect second '('
            if !self.is_special(b'(') {
                return result;
            }
            self.advance();

            // Parse comma-separated list of attributes
            while !self.is_special(b')') && !self.is_eof() {
                if let Some(attr) = self.parse_single_attribute() {
                    result.push(attr);
                }
                if self.is_special(b',') {
                    self.advance();
                } else if !self.is_special(b')') {
                    // Skip unknown tokens within attributes
                    self.advance();
                }
            }

            // Consume first ')'
            if self.is_special(b')') {
                self.advance();
            }

            // Consume second ')'
            if self.is_special(b')') {
                self.advance();
            }
        }

        result
    }

    /// Skip __attribute__((...)) declarations (GCC extension, no-op)
    /// Wrapper around parse_attributes that discards the result.
    fn skip_attributes(&mut self) {
        let _ = self.parse_attributes();
    }

    /// Check if current token is __asm or __asm__
    fn is_asm_keyword(&self) -> bool {
        if self.peek() != TokenType::Ident {
            return false;
        }
        if let Some(name) = self.get_ident_name(self.current()) {
            name == "__asm__" || name == "__asm" || name == "asm"
        } else {
            false
        }
    }

    /// Skip __asm("...") or __asm__("...") declarations (GCC extension for symbol aliasing)
    /// Used in declarations like: FILE *fopen(...) __asm("_fopen$DARWIN_EXTSN");
    fn skip_asm(&mut self) {
        while self.is_asm_keyword() {
            self.advance(); // consume __asm/__asm__

            // Expect '('
            if !self.is_special(b'(') {
                return;
            }
            self.advance(); // consume '('

            // Skip contents until matching ')'
            let mut depth = 1;
            while depth > 0 && !self.is_eof() {
                if self.is_special(b'(') {
                    depth += 1;
                } else if self.is_special(b')') {
                    depth -= 1;
                }
                self.advance();
            }
        }
    }

    /// Parse GCC extended inline assembly statement
    /// Format: __asm__ [volatile] [goto] ( "template" [: outputs [: inputs [: clobbers [: goto_labels]]]] );
    fn parse_asm_statement(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume __asm/__asm__

        // Parse optional qualifiers: 'volatile', '__volatile__', 'inline', '__inline__', 'goto'
        let mut is_volatile = false;
        let mut _is_goto = false;
        while self.peek() == TokenType::Ident {
            if let Some(name) = self.get_ident_name(self.current()) {
                match name.as_str() {
                    "volatile" | "__volatile__" => {
                        is_volatile = true;
                        self.advance();
                    }
                    "inline" | "__inline__" => {
                        // inline qualifier - just consume it (affects inlining decisions)
                        self.advance();
                    }
                    "goto" => {
                        // goto qualifier - indicates asm can jump to C labels
                        _is_goto = true;
                        self.advance();
                    }
                    _ => break,
                }
            } else {
                break;
            }
        }

        self.expect_special(b'(')?;

        // Parse template string (may be multiple concatenated strings)
        let template = self.parse_asm_string_literal()?;

        // Parse outputs (after first ':')
        let outputs = if self.is_special(b':') {
            self.advance();
            self.parse_asm_operands()?
        } else {
            vec![]
        };

        // Parse inputs (after second ':')
        let inputs = if self.is_special(b':') {
            self.advance();
            self.parse_asm_operands()?
        } else {
            vec![]
        };

        // Parse clobbers (after third ':')
        let clobbers = if self.is_special(b':') {
            self.advance();
            self.parse_asm_clobbers()?
        } else {
            vec![]
        };

        // Parse goto labels (after fourth ':')
        let goto_labels = if self.is_special(b':') {
            self.advance();
            self.parse_asm_goto_labels()?
        } else {
            vec![]
        };

        self.expect_special(b')')?;
        self.expect_special(b';')?;

        // Note: is_volatile is parsed but not yet used (Phase 2 feature)
        let _ = is_volatile;

        Ok(Stmt::Asm {
            template,
            outputs,
            inputs,
            clobbers,
            goto_labels,
        })
    }

    /// Parse an asm template string (handles string concatenation)
    fn parse_asm_string_literal(&mut self) -> ParseResult<String> {
        let mut result = String::new();

        if self.peek() != TokenType::String {
            return Err(ParseError::new(
                "expected string literal in asm template",
                self.current_pos(),
            ));
        }

        // Parse first string
        let token = self.consume();
        if let TokenValue::String(s) = &token.value {
            result.push_str(&Self::parse_string_literal(s));
        }

        // Handle string concatenation (adjacent string literals)
        while self.peek() == TokenType::String {
            let token = self.consume();
            if let TokenValue::String(s) = &token.value {
                result.push_str(&Self::parse_string_literal(s));
            }
        }

        Ok(result)
    }

    /// Parse asm operand list: [name] "constraint" (expr), ...
    fn parse_asm_operands(&mut self) -> ParseResult<Vec<AsmOperand>> {
        let mut operands = Vec::new();

        // Allow empty operand list
        if self.is_special(b':') || self.is_special(b')') {
            return Ok(operands);
        }

        loop {
            // Parse optional symbolic name: [name]
            let name = if self.is_special(b'[') {
                self.advance(); // consume '['
                let name = self.expect_identifier()?;
                self.expect_special(b']')?;
                Some(name)
            } else {
                None
            };

            // Parse constraint string
            if self.peek() != TokenType::String {
                return Err(ParseError::new(
                    "expected constraint string in asm operand",
                    self.current_pos(),
                ));
            }
            let constraint = self.parse_asm_string_literal()?;

            // Parse expression in parentheses
            self.expect_special(b'(')?;
            let expr = self.parse_expression()?;
            self.expect_special(b')')?;

            operands.push(AsmOperand {
                name,
                constraint,
                expr,
            });

            // Check for more operands
            if self.is_special(b',') {
                self.advance();
            } else {
                break;
            }
        }

        Ok(operands)
    }

    /// Parse asm clobber list: "clobber", ...
    fn parse_asm_clobbers(&mut self) -> ParseResult<Vec<String>> {
        let mut clobbers = Vec::new();

        // Allow empty clobber list
        if self.is_special(b':') || self.is_special(b')') {
            return Ok(clobbers);
        }

        loop {
            if self.peek() != TokenType::String {
                return Err(ParseError::new(
                    "expected clobber string in asm statement",
                    self.current_pos(),
                ));
            }
            let clobber = self.parse_asm_string_literal()?;
            clobbers.push(clobber);

            if self.is_special(b',') {
                self.advance();
            } else {
                break;
            }
        }

        Ok(clobbers)
    }

    /// Parse asm goto label list: label1, label2, ...
    fn parse_asm_goto_labels(&mut self) -> ParseResult<Vec<StringId>> {
        let mut labels = Vec::new();

        // Allow empty label list
        if self.is_special(b')') {
            return Ok(labels);
        }

        loop {
            if self.peek() != TokenType::Ident {
                return Err(ParseError::new(
                    "expected label identifier in asm goto",
                    self.current_pos(),
                ));
            }
            let token = self.consume();
            if let TokenValue::Ident(label_id) = token.value {
                labels.push(label_id);
            }

            if self.is_special(b',') {
                self.advance();
            } else {
                break;
            }
        }

        Ok(labels)
    }

    /// Skip both __attribute__ and __asm extensions
    fn skip_extensions(&mut self) {
        loop {
            if self.is_attribute_keyword() {
                self.skip_attributes();
            } else if self.is_asm_keyword() {
                self.skip_asm();
            } else {
                break;
            }
        }
    }
}

// ============================================================================
// Statement Parsing
// ============================================================================

impl Parser<'_> {
    /// Parse a statement
    pub fn parse_statement(&mut self) -> ParseResult<Stmt> {
        // Check for keywords
        if self.peek() == TokenType::Ident {
            if let Some(name) = self.get_ident_name(self.current()) {
                match name.as_str() {
                    "if" => return self.parse_if_stmt(),
                    "while" => return self.parse_while_stmt(),
                    "do" => return self.parse_do_while_stmt(),
                    "for" => return self.parse_for_stmt(),
                    "return" => return self.parse_return_stmt(),
                    "break" => {
                        self.advance();
                        self.expect_special(b';')?;
                        return Ok(Stmt::Break);
                    }
                    "continue" => {
                        self.advance();
                        self.expect_special(b';')?;
                        return Ok(Stmt::Continue);
                    }
                    "goto" => {
                        self.advance();
                        let label = self.expect_identifier()?;
                        self.expect_special(b';')?;
                        return Ok(Stmt::Goto(label));
                    }
                    "switch" => return self.parse_switch_stmt(),
                    "case" => return self.parse_case_label(),
                    "default" => return self.parse_default_label(),
                    // GCC extended inline assembly
                    "__asm__" | "__asm" | "asm" => {
                        return self.parse_asm_statement();
                    }
                    _ => {}
                }
            }
        }

        // Check for compound statement
        if self.is_special(b'{') {
            return self.parse_block_stmt();
        }

        // Check for empty statement
        if self.is_special(b';') {
            self.advance();
            return Ok(Stmt::Empty);
        }

        // Check for labeled statement
        if self.peek() == TokenType::Ident {
            // Save position for potential backtrack
            let saved_pos = self.pos;
            let name = self.expect_identifier()?;
            if self.is_special(b':') {
                self.advance();
                let stmt = self.parse_statement()?;
                return Ok(Stmt::Label {
                    name,
                    stmt: Box::new(stmt),
                });
            }
            // Not a label, backtrack
            self.pos = saved_pos;
        }

        // Expression statement
        let expr = self.parse_expression()?;
        self.expect_special(b';')?;
        Ok(Stmt::Expr(expr))
    }

    /// Parse an if statement
    fn parse_if_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'if'
        self.expect_special(b'(')?;
        let cond = self.parse_expression()?;
        self.expect_special(b')')?;
        let then_stmt = self.parse_statement()?;

        let else_stmt = if self.peek() == TokenType::Ident {
            if let Some(name) = self.get_ident_name(self.current()) {
                if name == "else" {
                    self.advance();
                    Some(Box::new(self.parse_statement()?))
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        Ok(Stmt::If {
            cond,
            then_stmt: Box::new(then_stmt),
            else_stmt,
        })
    }

    /// Parse a while statement
    fn parse_while_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'while'
        self.expect_special(b'(')?;
        let cond = self.parse_expression()?;
        self.expect_special(b')')?;
        let body = self.parse_statement()?;

        Ok(Stmt::While {
            cond,
            body: Box::new(body),
        })
    }

    /// Parse a do-while statement
    fn parse_do_while_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'do'
        let body = self.parse_statement()?;

        // Expect 'while'
        if self.peek() != TokenType::Ident {
            return Err(ParseError::new("expected 'while'", self.current_pos()));
        }
        if let Some(name) = self.get_ident_name(self.current()) {
            if name != "while" {
                return Err(ParseError::new("expected 'while'", self.current_pos()));
            }
        }
        self.advance();

        self.expect_special(b'(')?;
        let cond = self.parse_expression()?;
        self.expect_special(b')')?;
        self.expect_special(b';')?;

        Ok(Stmt::DoWhile {
            body: Box::new(body),
            cond,
        })
    }

    /// Parse a for statement
    ///
    /// C99 allows declarations in for-init: `for (int i = 0; i < n; i++)`
    /// These declarations are scoped to the for loop (including body).
    fn parse_for_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'for'
        self.expect_special(b'(')?;

        // Enter scope for for-loop declarations (C99)
        // This scope includes init declaration + body
        self.symbols.enter_scope();

        // Parse init (can be declaration or expression)
        let init = if self.is_special(b';') {
            self.advance();
            None
        } else if self.is_declaration_start() {
            // C99: declaration in for-init, bind to for-scope
            // Note: storage class specifiers (static, extern) are forbidden here
            let decl = self.parse_for_init_declaration_and_bind()?;
            // Declaration already consumed the semicolon
            Some(ForInit::Declaration(decl))
        } else {
            let expr = self.parse_expression()?;
            self.expect_special(b';')?;
            Some(ForInit::Expression(expr))
        };

        // Parse condition
        let cond = if self.is_special(b';') {
            self.advance();
            None
        } else {
            let expr = self.parse_expression()?;
            self.expect_special(b';')?;
            Some(expr)
        };

        // Parse post
        let post = if self.is_special(b')') {
            None
        } else {
            Some(self.parse_expression()?)
        };

        self.expect_special(b')')?;
        let body = self.parse_statement()?;

        // Leave for-scope
        self.symbols.leave_scope();

        Ok(Stmt::For {
            init,
            cond,
            post,
            body: Box::new(body),
        })
    }

    /// Parse a return statement
    fn parse_return_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'return'

        if self.is_special(b';') {
            self.advance();
            return Ok(Stmt::Return(None));
        }

        let expr = self.parse_expression()?;
        self.expect_special(b';')?;
        Ok(Stmt::Return(Some(expr)))
    }

    /// Parse a switch statement
    fn parse_switch_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'switch'
        self.expect_special(b'(')?;
        let expr = self.parse_expression()?;
        self.expect_special(b')')?;
        let body = self.parse_statement()?;
        Ok(Stmt::Switch {
            expr,
            body: Box::new(body),
        })
    }

    /// Parse a case label
    fn parse_case_label(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'case'
        let expr = self.parse_conditional_expr()?;
        self.expect_special(b':')?;
        Ok(Stmt::Case(expr))
    }

    /// Parse a default label
    fn parse_default_label(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'default'
        self.expect_special(b':')?;
        Ok(Stmt::Default)
    }

    /// Parse a compound statement (block) with its own scope
    ///
    /// Blocks create their own scope for local declarations. This enters a
    /// new scope, parses the block, binds any declarations, then leaves
    /// the scope.
    /// Parse block items (declarations and statements) until closing brace
    fn parse_block_items(&mut self) -> ParseResult<Vec<BlockItem>> {
        let mut items = Vec::new();
        while !self.is_special(b'}') && !self.is_eof() {
            if self.is_declaration_start() {
                let decl = self.parse_declaration_and_bind()?;
                items.push(BlockItem::Declaration(decl));
            } else {
                let stmt = self.parse_statement()?;
                items.push(BlockItem::Statement(stmt));
            }
        }
        Ok(items)
    }

    fn parse_block_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_special(b'{')?;

        // Enter block scope
        self.symbols.enter_scope();

        let items = self.parse_block_items()?;

        // Leave block scope
        self.symbols.leave_scope();

        self.expect_special(b'}')?;
        Ok(Stmt::Block(items))
    }

    /// Parse a compound statement without entering a new scope
    ///
    /// Used by function definitions where the scope is already entered
    /// by the function parsing code (to include parameters in scope).
    fn parse_block_stmt_no_scope(&mut self) -> ParseResult<Stmt> {
        self.expect_special(b'{')?;
        let items = self.parse_block_items()?;
        self.expect_special(b'}')?;
        Ok(Stmt::Block(items))
    }

    /// Parse a statement expression: ({ stmt; stmt; expr; })
    /// This is a GNU extension that allows a compound statement to be used as an expression.
    /// The value is the result of the last expression in the block.
    pub(crate) fn parse_stmt_expr(&mut self, paren_pos: Position) -> ParseResult<Expr> {
        self.expect_special(b'{')?;

        // Enter block scope for the statement expression
        self.symbols.enter_scope();

        let mut items = self.parse_block_items()?;

        self.expect_special(b'}')?;
        self.expect_special(b')')?;

        // Leave block scope
        self.symbols.leave_scope();

        // The result of a statement expression is the last expression statement.
        // If there are no statements or the last isn't an expression, result is void.
        let (stmts, result, result_type) = if items.is_empty() {
            // Empty statement expression: ({ }) has type void
            (
                Vec::new(),
                Expr::typed(ExprKind::IntLit(0), self.types.void_id, paren_pos),
                self.types.void_id,
            )
        } else {
            // Check if the last item is an expression statement
            let last = items.pop().unwrap();
            match last {
                BlockItem::Statement(Stmt::Expr(expr)) => {
                    let typ = expr.typ.unwrap_or(self.types.int_id);
                    (items, expr, typ)
                }
                _ => {
                    // Last item is not an expression statement (e.g. if, while, for)
                    // Following sparse: the type becomes void (evaluate.c handles this
                    // by returning NULL which becomes void_ctype)
                    items.push(last);
                    (
                        items,
                        Expr::typed(ExprKind::IntLit(0), self.types.void_id, paren_pos),
                        self.types.void_id,
                    )
                }
            }
        };

        Ok(Self::typed_expr(
            ExprKind::StmtExpr {
                stmts,
                result: Box::new(result),
            },
            result_type,
            paren_pos,
        ))
    }

    /// Check if current position starts a declaration
    fn is_declaration_start(&self) -> bool {
        if self.peek() != TokenType::Ident {
            return false;
        }

        if let Some(name_id) = self.get_ident_id(self.current()) {
            let name = self.str(name_id);
            // Check for type keywords first
            if matches!(
                name,
                "void"
                    | "char"
                    | "short"
                    | "int"
                    | "long"
                    | "float"
                    | "double"
                    | "_Float16"
                    | "_Float32"
                    | "_Float64"
                    | "_Complex"
                    | "_Atomic"
                    | "_Alignas"
                    | "signed"
                    | "unsigned"
                    | "const"
                    | "volatile"
                    | "static"
                    | "extern"
                    | "auto"
                    | "register"
                    | "typedef"
                    | "inline"
                    | "__inline"
                    | "__inline__"
                    | "_Noreturn"
                    | "__noreturn__"
                    | "struct"
                    | "union"
                    | "enum"
                    | "_Bool"
                    | "__attribute__"
                    | "__attribute"
                    | "__builtin_va_list"
                    | "typeof"
                    | "__typeof__"
                    | "__typeof"
                    | "_Thread_local"
                    | "__thread"
                    | "_Static_assert"
                    | "static_assert"
            ) {
                return true;
            }
            // Also check for typedef names
            self.symbols.lookup_typedef(name_id).is_some()
        } else {
            false
        }
    }

    /// Parse a declaration and bind to symbol table
    ///
    /// Used for testing declarations in isolation.
    #[cfg(test)]
    pub(crate) fn parse_declaration(&mut self) -> ParseResult<Declaration> {
        // Parse type specifiers
        let base_type = self.parse_type_specifier()?;
        // Skip __attribute__ between type and declarator (GCC extension)
        self.skip_extensions();
        let base_type_id = self.types.intern(base_type);

        // Parse declarators
        let mut declarators = Vec::new();

        // Check for struct/union/enum-only declaration (no declarators)
        // e.g., "struct point { int x; int y; };"
        if !self.is_special(b';') {
            loop {
                let (name, typ, vla_sizes, _func_params) = self.parse_declarator(base_type_id)?;
                // Skip GCC extensions like __asm("...") or __attribute__((...))
                self.skip_extensions();
                let init = if self.is_special(b'=') {
                    self.advance();
                    Some(self.parse_initializer()?)
                } else {
                    None
                };

                // Validate explicit alignment (C11 6.7.5: >= natural alignment)
                let validated_align = self.validated_explicit_align(typ)?;

                // Declare symbol in symbol table
                let symbol = self
                    .symbols
                    .declare(
                        crate::symbol::Symbol::variable(name, typ, self.symbols.depth())
                            .with_align(validated_align),
                    )
                    .expect("symbol declaration failed in test");

                declarators.push(InitDeclarator {
                    symbol,
                    typ,
                    storage_class: TypeModifiers::empty(),
                    init,
                    vla_sizes,
                    explicit_align: validated_align,
                });

                if self.is_special(b',') {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        // Clear pending alignment after declaration
        self.pending_alignas = None;
        self.expect_special(b';')?;

        Ok(Declaration { declarators })
    }

    /// Parse a declaration and bind variables to symbol table
    ///
    /// Binds each declared variable to the symbol table immediately during
    /// parsing, so the symbol is available for subsequent references.
    ///
    /// If `forbid_storage_class` is true, emits an error if the declaration
    /// contains storage class specifiers (static, extern). This is used for
    /// for-loop init declarations per C99 6.8.5.3.
    fn parse_declaration_and_bind(&mut self) -> ParseResult<Declaration> {
        self.parse_declaration_and_bind_impl(false)
    }

    /// Infer array size from initializer for incomplete array types.
    ///
    /// For declarations like `int arr[] = {1,2,3}` or `char arr[] = "hello"`,
    /// infers the array size from the initializer and returns a complete type.
    ///
    /// This handles C99 6.7.8 paragraph 22: "If an array of unknown size is initialized,
    /// its size is determined by the largest indexed element with an explicit initializer."
    fn infer_array_size_from_init(&mut self, typ: TypeId, init: &Expr) -> TypeId {
        if self.types.kind(typ) != TypeKind::Array {
            return typ;
        }

        let array_size = self.types.get(typ).array_size;
        // Check if array size is incomplete (0 or None)
        if array_size != Some(0) && array_size.is_some() {
            return typ;
        }

        let new_size = match &init.kind {
            ExprKind::InitList { elements } => Some(self.array_size_from_elements(elements)),
            ExprKind::StringLit(s) => {
                // For char array initialized with string literal,
                // size is string length + 1 for null terminator
                Some(s.len() + 1)
            }
            ExprKind::WideStringLit(s) => {
                // For wchar_t array initialized with wide string literal,
                // size is number of chars + 1 for null terminator
                Some(s.chars().count() + 1)
            }
            _ => None,
        };

        if let Some(size) = new_size {
            // Update type with actual size from initializer
            let elem_type = self.types.base_type(typ).unwrap_or(self.types.int_id);
            // Preserve modifiers (like const, static)
            let modifiers = self.types.modifiers(typ);
            let mut arr_type = Type::array(elem_type, size);
            arr_type.modifiers = modifiers;
            self.types.intern(arr_type)
        } else {
            typ
        }
    }

    pub(crate) fn array_size_from_elements(&self, elements: &[InitElement]) -> usize {
        let mut max_index: i64 = -1;
        let mut current_index: i64 = 0;

        for element in elements {
            let mut designator_index = None;
            for designator in &element.designators {
                if let Designator::Index(index) = designator {
                    designator_index = Some(*index);
                    break;
                }
            }

            let index = if let Some(explicit_index) = designator_index {
                current_index = explicit_index + 1;
                explicit_index
            } else {
                let idx = current_index;
                current_index += 1;
                idx
            };

            if index > max_index {
                max_index = index;
            }
        }

        if max_index < 0 {
            0
        } else {
            (max_index + 1) as usize
        }
    }

    /// Parse a for-init declaration and bind variables to symbol table
    ///
    /// Same as `parse_declaration_and_bind()` but rejects storage class specifiers.
    fn parse_for_init_declaration_and_bind(&mut self) -> ParseResult<Declaration> {
        self.parse_declaration_and_bind_impl(true)
    }

    /// Implementation of declaration parsing with optional storage class check
    fn parse_declaration_and_bind_impl(
        &mut self,
        forbid_storage_class: bool,
    ) -> ParseResult<Declaration> {
        // Check for _Static_assert first (C11)
        if self.is_static_assert() {
            self.parse_static_assert()?;
            // Return empty declaration - static_assert produces nothing
            return Ok(Declaration {
                declarators: vec![],
            });
        }

        // Parse type specifiers
        let base_type = self.parse_type_specifier()?;
        // Skip __attribute__ between type and declarator (GCC extension)
        self.skip_extensions();

        // Check for forbidden storage class specifiers in for-init context
        if forbid_storage_class {
            if base_type.modifiers.contains(TypeModifiers::STATIC) {
                return Err(ParseError::new(
                    "declaration of static variable in for loop initial declaration",
                    self.current_pos(),
                ));
            }
            if base_type.modifiers.contains(TypeModifiers::EXTERN) {
                return Err(ParseError::new(
                    "declaration of extern variable in for loop initial declaration",
                    self.current_pos(),
                ));
            }
            if base_type.modifiers.contains(TypeModifiers::THREAD_LOCAL) {
                return Err(ParseError::new(
                    "declaration of thread-local variable in for loop initial declaration",
                    self.current_pos(),
                ));
            }
        }

        // C11 6.7.1p2: _Thread_local shall not appear in a declaration with auto or register
        if base_type.modifiers.contains(TypeModifiers::THREAD_LOCAL) {
            if base_type.modifiers.contains(TypeModifiers::AUTO) {
                return Err(ParseError::new(
                    "_Thread_local cannot be combined with auto",
                    self.current_pos(),
                ));
            }
            if base_type.modifiers.contains(TypeModifiers::REGISTER) {
                return Err(ParseError::new(
                    "_Thread_local cannot be combined with register",
                    self.current_pos(),
                ));
            }
        }

        // Check modifiers from the specifier before interning (storage class is not part of type)
        let is_typedef = base_type.modifiers.contains(TypeModifiers::TYPEDEF);
        // For struct/union types with tags, use existing TypeId to preserve forward declarations
        let base_type_id = self.intern_type_with_tag(&base_type);

        // Parse declarators
        let mut declarators = Vec::new();

        // Check for struct/union/enum-only declaration (no declarators)
        // e.g., "struct point { int x; int y; };"
        if !self.is_special(b';') {
            loop {
                let (name, mut typ, vla_sizes, _func_params) =
                    self.parse_declarator(base_type_id)?;
                // Skip GCC extensions like __asm("...") or __attribute__((...))
                self.skip_extensions();

                // Check if we have a name (needed for symbol binding)
                let has_name = !self.str(name).is_empty();

                // Validate explicit alignment (C11 6.7.5: >= natural alignment)
                let validated_align = self.validated_explicit_align(typ)?;

                // Bind variable to symbol table BEFORE parsing initializer.
                // This ensures the variable is in scope for sizeof(*var) in initializers.
                // Per C99 6.2.1p7: "Any other identifier has scope that begins just
                // after the completion of its declarator."
                let mut symbol_id: Option<SymbolId> = None;
                if has_name && !is_typedef {
                    let sym = Symbol::variable(name, typ, self.symbols.depth())
                        .with_align(validated_align);
                    if let Ok(id) = self.symbols.declare(sym) {
                        symbol_id = Some(id);
                    }
                }

                let init = if self.is_special(b'=') {
                    if is_typedef {
                        return Err(ParseError::new(
                            "typedef cannot have initializer",
                            self.current_pos(),
                        ));
                    }
                    self.advance();
                    Some(self.parse_initializer()?)
                } else {
                    None
                };

                // For incomplete array types, infer size from initializer
                if let Some(ref init_expr) = init {
                    let old_type = typ;
                    typ = self.infer_array_size_from_init(typ, init_expr);

                    // If the type changed (array size was inferred), update the symbol's type
                    // This is needed because the symbol was already added before parsing the initializer
                    if typ != old_type {
                        if let Some(sym_id) = symbol_id {
                            self.symbols.get_mut(sym_id).typ = typ;
                        }
                    }
                }

                // Bind typedef to symbol table (after parsing initializer, which
                // is forbidden for typedefs anyway)
                if has_name && is_typedef {
                    let sym = Symbol::typedef(name, typ, self.symbols.depth());
                    if let Ok(id) = self.symbols.declare(sym) {
                        symbol_id = Some(id);
                    }
                }

                // Only add declarator if it has a symbol (named declaration)
                // Nameless declarators like "int;" are allowed but produce no binding
                if let Some(symbol) = symbol_id {
                    // Extract storage class specifiers from base_type modifiers
                    let storage_class_mask = TypeModifiers::EXTERN
                        | TypeModifiers::STATIC
                        | TypeModifiers::THREAD_LOCAL
                        | TypeModifiers::TYPEDEF
                        | TypeModifiers::AUTO
                        | TypeModifiers::REGISTER;
                    let storage_class = base_type.modifiers & storage_class_mask;
                    declarators.push(InitDeclarator {
                        symbol,
                        typ,
                        storage_class,
                        init,
                        vla_sizes,
                        explicit_align: validated_align,
                    });
                }

                if self.is_special(b',') {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        // Clear pending alignment after declaration
        self.pending_alignas = None;
        self.expect_special(b';')?;

        Ok(Declaration { declarators })
    }

    /// Parse a type specifier
    fn parse_type_specifier(&mut self) -> ParseResult<Type> {
        let mut modifiers = TypeModifiers::empty();
        let mut base_kind: Option<TypeKind> = None;
        // Track typedef type separately - we continue parsing after a typedef
        // to collect trailing qualifiers like "z_word_t const"
        let mut typedef_base: Option<TypeId> = None;

        // Skip any leading __attribute__
        self.skip_extensions();

        loop {
            if self.peek() != TokenType::Ident {
                break;
            }

            let name_id = match self.get_ident_id(self.current()) {
                Some(id) => id,
                None => break,
            };
            let name = self.str(name_id);

            match name {
                // Skip __attribute__ in the type specifier loop
                "__attribute__" | "__attribute" => {
                    self.skip_extensions();
                    continue;
                }
                "const" => {
                    self.advance();
                    modifiers |= TypeModifiers::CONST;
                }
                "volatile" => {
                    self.advance();
                    modifiers |= TypeModifiers::VOLATILE;
                }
                "static" => {
                    self.advance();
                    modifiers |= TypeModifiers::STATIC;
                }
                "extern" => {
                    self.advance();
                    modifiers |= TypeModifiers::EXTERN;
                }
                "register" => {
                    self.advance();
                    modifiers |= TypeModifiers::REGISTER;
                }
                "auto" => {
                    self.advance();
                    modifiers |= TypeModifiers::AUTO;
                }
                "typedef" => {
                    self.advance();
                    modifiers |= TypeModifiers::TYPEDEF;
                }
                "_Thread_local" | "__thread" => {
                    self.advance();
                    modifiers |= TypeModifiers::THREAD_LOCAL;
                }
                "inline" | "__inline" | "__inline__" => {
                    self.advance();
                    modifiers |= TypeModifiers::INLINE;
                }
                "_Noreturn" | "__noreturn__" => {
                    self.advance();
                    modifiers |= TypeModifiers::NORETURN;
                }
                "signed" => {
                    self.advance();
                    modifiers |= TypeModifiers::SIGNED;
                }
                "unsigned" => {
                    self.advance();
                    modifiers |= TypeModifiers::UNSIGNED;
                }
                "_Complex" => {
                    self.advance();
                    modifiers |= TypeModifiers::COMPLEX;
                }
                "_Atomic" => {
                    self.advance();
                    // _Atomic can be:
                    // 1. Type specifier: _Atomic(type-name)
                    // 2. Type qualifier: _Atomic (without parens)
                    if self.is_special(b'(') {
                        // Type specifier form: _Atomic(type-name)
                        self.advance(); // consume '('
                        if let Some(inner_type) = self.try_parse_type_name() {
                            self.expect_special(b')')?;
                            // Return the type with ATOMIC modifier
                            let inner = self.types.get(inner_type).clone();
                            return Ok(Type {
                                modifiers: modifiers | inner.modifiers | TypeModifiers::ATOMIC,
                                ..inner
                            });
                        } else {
                            return Err(ParseError::new(
                                "expected type-name in _Atomic(...)",
                                self.current_pos(),
                            ));
                        }
                    } else {
                        // Qualifier form: just _Atomic
                        modifiers |= TypeModifiers::ATOMIC;
                    }
                }
                "_Alignas" => {
                    // C11 alignment specifier: _Alignas(type-name) or _Alignas(constant-expression)
                    let alignas_pos = self.current_pos();
                    self.advance();
                    self.expect_special(b'(')?;
                    let align = if let Some(type_id) = self.try_parse_type_name() {
                        // _Alignas(type) - alignment of the type
                        self.types.alignment(type_id) as u32
                    } else {
                        // Parse as constant expression: _Alignas(16)
                        let expr = self.parse_expression()?;
                        self.eval_const_expr(&expr).unwrap_or(0) as u32
                    };
                    self.expect_special(b')')?;

                    // C11 6.7.5p6: _Alignas(0) has no effect
                    if align == 0 {
                        // No effect - don't update pending_alignas
                    } else {
                        // C11 6.7.5: alignment must be a positive power of 2
                        if !align.is_power_of_two() {
                            return Err(ParseError::new(
                                format!("_Alignas({}) must be a power of 2", align),
                                alignas_pos,
                            ));
                        }
                        // Multiple _Alignas can appear; the strictest (largest) wins (C11 6.7.5)
                        if let Some(existing) = self.pending_alignas {
                            self.pending_alignas = Some(existing.max(align));
                        } else {
                            self.pending_alignas = Some(align);
                        }
                    }
                }
                "short" => {
                    self.advance();
                    modifiers |= TypeModifiers::SHORT;
                    if base_kind.is_none() {
                        base_kind = Some(TypeKind::Short);
                    }
                }
                "long" => {
                    self.advance();
                    if modifiers.contains(TypeModifiers::LONG) {
                        modifiers |= TypeModifiers::LONGLONG;
                        base_kind = Some(TypeKind::LongLong);
                    } else {
                        modifiers |= TypeModifiers::LONG;
                        // long double case
                        if base_kind == Some(TypeKind::Double) {
                            base_kind = Some(TypeKind::LongDouble);
                        } else if base_kind.is_none() {
                            base_kind = Some(TypeKind::Long);
                        }
                    }
                }
                "void" => {
                    self.advance();
                    base_kind = Some(TypeKind::Void);
                }
                "char" => {
                    self.advance();
                    base_kind = Some(TypeKind::Char);
                }
                "int" => {
                    self.advance();
                    if base_kind.is_none()
                        || !matches!(
                            base_kind,
                            Some(TypeKind::Short) | Some(TypeKind::Long) | Some(TypeKind::LongLong)
                        )
                    {
                        base_kind = Some(TypeKind::Int);
                    }
                }
                "float" => {
                    self.advance();
                    base_kind = Some(TypeKind::Float);
                }
                "double" => {
                    self.advance();
                    // Handle long double
                    if modifiers.contains(TypeModifiers::LONG) {
                        base_kind = Some(TypeKind::LongDouble);
                    } else {
                        base_kind = Some(TypeKind::Double);
                    }
                }
                "_Float16" => {
                    self.advance();
                    base_kind = Some(TypeKind::Float16);
                }
                "_Float32" => {
                    // _Float32 is an alias for float (TS 18661-3 / C23)
                    self.advance();
                    base_kind = Some(TypeKind::Float);
                }
                "_Float64" => {
                    // _Float64 is an alias for double (TS 18661-3 / C23)
                    self.advance();
                    base_kind = Some(TypeKind::Double);
                }
                "_Bool" => {
                    self.advance();
                    base_kind = Some(TypeKind::Bool);
                }
                "__builtin_va_list" => {
                    self.advance();
                    base_kind = Some(TypeKind::VaList);
                }
                "typeof" | "__typeof__" | "__typeof" => {
                    self.advance(); // consume typeof
                    self.expect_special(b'(')?;

                    // typeof can take either a type name or an expression
                    // Try type name first
                    if let Some(typ) = self.try_parse_type_name() {
                        self.expect_special(b')')?;
                        // Return the type with any modifiers
                        let result_type = self.types.get(typ).clone();
                        return Ok(Type {
                            modifiers: modifiers | result_type.modifiers,
                            ..result_type
                        });
                    }

                    // Not a type name, try expression
                    let expr = self.parse_expression()?;
                    self.expect_special(b')')?;

                    // Get the type of the expression
                    let expr_type_id = expr.typ.unwrap_or(self.types.int_id);
                    let result_type = self.types.get(expr_type_id).clone();
                    return Ok(Type {
                        modifiers: modifiers | result_type.modifiers,
                        ..result_type
                    });
                }
                "enum" => {
                    let mut enum_type = self.parse_enum_specifier()?;
                    // Consume trailing qualifiers (e.g., "enum foo const")
                    let trailing_mods = self.consume_type_qualifiers();
                    // Apply any modifiers we collected
                    enum_type.modifiers |= modifiers | trailing_mods;
                    return Ok(enum_type);
                }
                "struct" => {
                    let mut struct_type = self.parse_struct_or_union_specifier(false)?;
                    // Consume trailing qualifiers (e.g., "struct foo const")
                    let trailing_mods = self.consume_type_qualifiers();
                    struct_type.modifiers |= modifiers | trailing_mods;
                    return Ok(struct_type);
                }
                "union" => {
                    let mut union_type = self.parse_struct_or_union_specifier(true)?;
                    // Consume trailing qualifiers (e.g., "union foo const")
                    let trailing_mods = self.consume_type_qualifiers();
                    union_type.modifiers |= modifiers | trailing_mods;
                    return Ok(union_type);
                }
                _ => {
                    // Check if it's a typedef name
                    // Only consume the typedef if we haven't already seen a base type or typedef
                    if base_kind.is_none() && typedef_base.is_none() {
                        if let Some(typedef_type_id) = self.symbols.lookup_typedef(name_id) {
                            self.advance();
                            // Save the typedef type and continue looping to collect trailing
                            // qualifiers (e.g., "z_word_t const" where const comes after typedef)
                            typedef_base = Some(typedef_type_id);
                            continue;
                        }
                    }
                    break;
                }
            }
        }

        // If we parsed a typedef, return that with any trailing modifiers applied
        if let Some(typedef_type_id) = typedef_base {
            let typedef_type = self.types.get(typedef_type_id);
            let mut result = typedef_type.clone();
            // Strip TYPEDEF modifier - we're using the typedef, not defining one
            result.modifiers &= !TypeModifiers::TYPEDEF;
            result.modifiers |= modifiers;
            return Ok(result);
        }

        let kind = base_kind.unwrap_or(TypeKind::Int);
        Ok(Type::with_modifiers(kind, modifiers))
    }

    /// Parse an enum specifier
    /// enum-specifier: 'enum' identifier? '{' enumerator-list? '}' | 'enum' identifier
    pub(crate) fn parse_enum_specifier(&mut self) -> ParseResult<Type> {
        self.advance(); // consume 'enum'

        // Optional tag name
        let tag = if self.peek() == TokenType::Ident && !self.is_special(b'{') {
            Some(self.expect_identifier()?)
        } else {
            None
        };

        // Check for definition vs forward reference
        if self.is_special(b'{') {
            self.advance(); // consume '{'

            let mut constants = Vec::with_capacity(DEFAULT_ENUM_CAPACITY);
            let mut next_value = 0i64;

            while !self.is_special(b'}') && !self.is_eof() {
                let name = self.expect_identifier()?;

                let value = if self.is_special(b'=') {
                    self.advance();
                    let expr = self.parse_conditional_expr()?;
                    // Evaluate constant expression
                    self.eval_const_expr(&expr).ok_or_else(|| {
                        ParseError::new("enum value must be constant", self.current_pos())
                    })?
                } else {
                    next_value
                };

                constants.push(EnumConstant { name, value });
                next_value = value + 1;

                // Register enum constant in symbol table (Ordinary namespace)
                let sym =
                    Symbol::enum_constant(name, value, self.types.int_id, self.symbols.depth());
                let _ = self.symbols.declare(sym);

                if self.is_special(b',') {
                    self.advance();
                    // Allow trailing comma before '}'
                    if self.is_special(b'}') {
                        break;
                    }
                } else {
                    break;
                }
            }

            // Empty enum definition is a GNU extension, warn with -Wpedantic
            if constants.is_empty() {
                diag::warning(
                    self.current_pos(),
                    "empty enum definition is a GNU extension",
                );
            }

            self.expect_special(b'}')?;

            let composite = CompositeType {
                tag,
                members: Vec::new(),
                enum_constants: constants,
                size: 4,
                align: 4,
                is_complete: true,
            };

            let enum_type = Type::enum_type(composite);

            // Register tag if present
            if let Some(tag_name) = tag {
                let enum_type_id = self.types.intern(enum_type.clone());
                let sym = Symbol::tag(tag_name, enum_type_id, self.symbols.depth());
                let _ = self.symbols.declare(sym);
            }

            Ok(enum_type)
        } else {
            // Forward reference - look up existing tag
            if let Some(tag_name) = tag {
                // Look up or create incomplete type
                if let Some(existing) = self.symbols.lookup_tag(tag_name) {
                    // Return a clone of the underlying type
                    Ok(self.types.get(existing.typ).clone())
                } else {
                    Ok(Type::incomplete_enum(tag_name))
                }
            } else {
                Err(ParseError::new(
                    "expected enum definition or tag name",
                    self.current_pos(),
                ))
            }
        }
    }

    /// Parse a struct or union specifier
    /// struct-or-union-specifier: ('struct'|'union') identifier? '{' struct-declaration-list? '}'
    ///                          | ('struct'|'union') identifier
    pub(crate) fn parse_struct_or_union_specifier(&mut self, is_union: bool) -> ParseResult<Type> {
        self.advance(); // consume 'struct' or 'union'

        // Skip any __attribute__ between 'struct' and tag name
        self.skip_extensions();

        // Optional tag name
        let tag = if self.peek() == TokenType::Ident && !self.is_special(b'{') {
            // Make sure it's not __attribute__
            if !self.is_attribute_keyword() {
                Some(self.expect_identifier()?)
            } else {
                self.skip_extensions();
                if self.peek() == TokenType::Ident && !self.is_special(b'{') {
                    Some(self.expect_identifier()?)
                } else {
                    None
                }
            }
        } else {
            None
        };

        // Skip any __attribute__ after tag name but before '{'
        self.skip_extensions();

        // Check for definition vs forward reference
        if self.is_special(b'{') {
            self.advance(); // consume '{'

            let mut members = Vec::with_capacity(DEFAULT_MEMBER_CAPACITY);

            while !self.is_special(b'}') && !self.is_eof() {
                // Check for _Static_assert in struct (C11 6.7.2.1p1)
                if self.is_static_assert() {
                    self.parse_static_assert()?;
                    continue;
                }

                // Parse member declaration
                let member_base_type = self.parse_type_specifier()?;
                let is_struct_or_union =
                    matches!(member_base_type.kind, TypeKind::Struct | TypeKind::Union);
                // For struct/union types with tags, use the existing TypeId from symbol table
                // to ensure forward-declared types are properly linked
                let member_base_type_id = self.intern_type_with_tag(&member_base_type);

                // Skip any __attribute__ after type specifier (before member name)
                self.skip_extensions();

                // C11 anonymous struct/union members: "struct { ... };" or "union { ... };"
                // These have no declarator name, just end with ';'
                if is_struct_or_union && self.is_special(b';') {
                    members.push(StructMember {
                        name: StringId::EMPTY,
                        typ: member_base_type_id,
                        offset: 0,
                        bit_offset: None,
                        bit_width: None,
                        storage_unit_size: None,
                        explicit_align: None, // anonymous members
                    });
                    self.advance(); // consume ';'
                    continue;
                }

                // Check for unnamed bitfield (starts with ':')
                if self.is_special(b':') {
                    // Unnamed bitfield: parse width only
                    self.advance(); // consume ':'
                    let width = self.parse_bitfield_width()?;

                    members.push(StructMember {
                        name: StringId::EMPTY,
                        typ: member_base_type_id,
                        offset: 0,
                        bit_offset: None,
                        bit_width: Some(width),
                        storage_unit_size: None,
                        explicit_align: None, // bitfields don't support _Alignas
                    });

                    self.expect_special(b';')?;
                    continue;
                }

                loop {
                    // Check for unnamed bitfield (can appear after ',' too)
                    // e.g., "int a : 1, : 2, b : 3;"
                    if self.is_special(b':') {
                        // Unnamed bitfield: parse width only
                        self.advance(); // consume ':'
                        let width = self.parse_bitfield_width()?;

                        members.push(StructMember {
                            name: StringId::EMPTY,
                            typ: member_base_type_id,
                            offset: 0,
                            bit_offset: None,
                            bit_width: Some(width),
                            storage_unit_size: None,
                            explicit_align: None, // bitfields don't support _Alignas
                        });

                        if self.is_special(b',') {
                            self.advance();
                            continue;
                        } else {
                            break;
                        }
                    }

                    // VLAs are not allowed in struct members
                    let (name, typ, vla_sizes, _func_params) =
                        self.parse_declarator(member_base_type_id)?;

                    // C99 6.7.5.2: VLAs cannot be members of structures or unions
                    if !vla_sizes.is_empty() {
                        return Err(ParseError::new(
                            "variable length arrays cannot be structure or union members"
                                .to_string(),
                            self.current_pos(),
                        ));
                    }

                    // Check for bitfield: name : width
                    let bit_width = if self.is_special(b':') {
                        self.advance(); // consume ':'
                        let width = self.parse_bitfield_width()?;
                        // Validate bitfield type and width (this is a named bitfield)
                        self.validate_bitfield(typ, width, true)?;
                        Some(width)
                    } else {
                        None
                    };

                    // Skip any __attribute__ after member declaration
                    self.skip_extensions();

                    // Capture any pending _Alignas from type specifier
                    let member_align = self.pending_alignas.take();

                    members.push(StructMember {
                        name,
                        typ,
                        offset: 0, // Computed later
                        bit_offset: None,
                        bit_width,
                        storage_unit_size: None,
                        explicit_align: member_align,
                    });

                    if self.is_special(b',') {
                        self.advance();
                    } else {
                        break;
                    }
                }

                self.expect_special(b';')?;
            }

            self.expect_special(b'}')?;

            // Skip any trailing __attribute__ (e.g., __attribute__((packed)))
            self.skip_extensions();

            // Compute layout
            let (size, align) = if is_union {
                self.types.compute_union_layout(&mut members)
            } else {
                self.types.compute_struct_layout(&mut members)
            };

            let composite = CompositeType {
                tag,
                members,
                enum_constants: Vec::new(),
                size,
                align,
                is_complete: true,
            };

            // Check if there's an existing forward declaration that we should complete
            if let Some(tag_name) = tag {
                if let Some(existing) = self.symbols.lookup_tag(tag_name) {
                    // Complete the existing forward-declared type in place
                    // This ensures all pointers to the incomplete type now see the complete type
                    let existing_typ = existing.typ;
                    let existing_type = self.types.get(existing_typ);
                    if existing_type
                        .composite
                        .as_ref()
                        .is_some_and(|c| !c.is_complete)
                    {
                        self.types.complete_struct(existing_typ, composite);
                        return Ok(self.types.get(existing_typ).clone());
                    }
                }
            }

            // No existing forward declaration - create new type
            let struct_type = if is_union {
                Type::union_type(composite)
            } else {
                Type::struct_type(composite)
            };

            // Register tag if present
            if let Some(tag_name) = tag {
                let typ_id = self.types.intern(struct_type.clone());
                let sym = Symbol::tag(tag_name, typ_id, self.symbols.depth());
                let _ = self.symbols.declare(sym);
            }

            Ok(struct_type)
        } else {
            // Forward reference
            if let Some(tag_name) = tag {
                // Look up existing tag
                if let Some(existing) = self.symbols.lookup_tag(tag_name) {
                    Ok(self.types.get(existing.typ).clone())
                } else {
                    // Create new incomplete type and register it in symbol table
                    // This ensures that when the type is completed later, we can update
                    // this same TypeId rather than creating a new one
                    let incomplete_type = if is_union {
                        Type::incomplete_union(tag_name)
                    } else {
                        Type::incomplete_struct(tag_name)
                    };
                    let typ_id = self.types.intern(incomplete_type.clone());
                    let sym = Symbol::tag(tag_name, typ_id, self.symbols.depth());
                    let _ = self.symbols.declare(sym);
                    Ok(incomplete_type)
                }
            } else {
                Err(ParseError::new(
                    "expected struct/union definition or tag name",
                    self.current_pos(),
                ))
            }
        }
    }

    /// Parse a declarator (name and type modifiers)
    ///
    /// C declarators are parsed "inside-out". For example, `int (*p)[3]`:
    /// - `int` is the base type
    /// - `(*p)` means p is a pointer
    /// - `[3]` after the parens means "to array of 3"
    ///   So p is "pointer to array of 3 ints"
    ///
    /// Returns: (name, type, VLA size expressions, function parameters if declarator is function)
    /// The function parameters include names for use in function definitions.
    fn parse_declarator(&mut self, base_type_id: TypeId) -> ParseResult<DeclaratorResult> {
        // Collect pointer modifiers (they bind tighter than array/function)
        let mut pointer_modifiers: Vec<TypeModifiers> = Vec::new();
        while self.is_special(b'*') {
            self.advance();
            let mut ptr_modifiers = TypeModifiers::empty();

            // Parse pointer qualifiers (const, volatile, restrict, _Atomic)
            while self.peek() == TokenType::Ident {
                if let Some(name) = self.get_ident_name(self.current()) {
                    match name.as_str() {
                        "const" => {
                            self.advance();
                            ptr_modifiers |= TypeModifiers::CONST;
                        }
                        "volatile" => {
                            self.advance();
                            ptr_modifiers |= TypeModifiers::VOLATILE;
                        }
                        "restrict" => {
                            self.advance();
                            ptr_modifiers |= TypeModifiers::RESTRICT;
                        }
                        "_Atomic" => {
                            self.advance();
                            ptr_modifiers |= TypeModifiers::ATOMIC;
                        }
                        _ => break,
                    }
                } else {
                    break;
                }
            }
            pointer_modifiers.push(ptr_modifiers);
        }

        // Check for parenthesized declarator: int (*p)[3]
        // The paren comes AFTER pointers, e.g. int *(*p)[3] = pointer to (pointer to array of 3 ints)
        let (name, inner_type_id, inner_func_params) = if self.is_special(b'(') {
            // Check if this looks like a function parameter list or a grouped declarator
            // A grouped declarator will have * or identifier immediately after (
            let saved_pos = self.pos;
            self.advance(); // consume '('

            // Check what follows - if it's *, it's likely a grouped declarator
            // If it's a type or ), it's likely function params
            let is_grouped = self.is_special(b'*') || self.peek() == TokenType::Ident;

            if is_grouped {
                // Parse nested declarator recursively
                // For int (*p)[3]: we're now at *p), base_type is int
                // We need to parse *p as the declarator, but we don't know the full type yet
                // because [3] comes after the )
                // So we use a placeholder and fix it up after

                // Parse the inner declarator with a placeholder type (void)
                // Note: We ignore any VLA expression from inner declarators - VLAs would be
                // in the outer array dimensions, not inner pointer/grouped declarators
                let (inner_name, inner_decl_type_id, _inner_vla, inner_func_params) =
                    self.parse_declarator(self.types.void_id)?;
                self.expect_special(b')')?;

                // Now parse any suffix modifiers (arrays, function params)
                // These apply to the base type, not the inner declarator
                (inner_name, Some(inner_decl_type_id), inner_func_params)
            } else {
                // Not a grouped declarator, restore position
                self.pos = saved_pos;
                (self.expect_identifier()?, None, None)
            }
        } else {
            // Get the name directly, or use empty for abstract declarators
            // Abstract declarators have no name: void (*)(int) - the * has no identifier
            if self.peek() == TokenType::Ident {
                (self.expect_identifier()?, None, None)
            } else if self.is_special(b')')
                || self.is_special(b'[')
                || self.is_special(b'(')
                || self.is_special(b',')
            {
                // No identifier - abstract declarator (e.g., void (*)(int), const char * restrict)
                (StringId::EMPTY, None, None)
            } else {
                (self.expect_identifier()?, None, None)
            }
        };

        // Handle array declarators - collect all dimensions first
        // Also track VLA expressions (non-constant size) for each dimension
        let mut dimensions: Vec<Option<usize>> = Vec::new();
        let mut vla_exprs: Vec<Expr> = Vec::new();
        while self.is_special(b'[') {
            self.advance();

            // Parse optional qualifiers and static (C99 6.7.5.3)
            // These are valid in function parameter array declarators
            while self.peek() == TokenType::Ident {
                if let Some(name) = self.get_ident_name(self.current()) {
                    match name.as_str() {
                        "static" | "const" | "volatile" | "restrict" => {
                            self.advance();
                        }
                        _ => break,
                    }
                } else {
                    break;
                }
            }

            // Check for [*] VLA unspecified size (C99 6.7.5.2)
            // This is used in function prototypes: void f(int n, int arr[*])
            let size = if self.is_special(b']') {
                None
            } else if self.is_special(b'*') {
                // Check if it's [*] (VLA star) or just a multiplication expression
                let saved_pos = self.pos;
                self.advance();
                if self.is_special(b']') {
                    // It's [*] - VLA with unspecified size
                    None
                } else {
                    // It's an expression starting with * (e.g., [*ptr])
                    self.pos = saved_pos;
                    let expr = self.parse_assignment_expr()?;
                    match self.eval_const_expr(&expr) {
                        Some(n) if n >= 0 => Some(n as usize),
                        Some(_) => None,
                        None => {
                            vla_exprs.push(expr);
                            None
                        }
                    }
                }
            } else {
                // Parse constant expression for array size (C99 6.7.5.2)
                let expr = self.parse_assignment_expr()?;
                // Evaluate as integer constant expression
                match self.eval_const_expr(&expr) {
                    Some(n) if n >= 0 => Some(n as usize),
                    Some(_) => None, // Negative size is invalid
                    None => {
                        // Non-constant (VLA) - save expression for VLA handling
                        vla_exprs.push(expr);
                        None
                    }
                }
            };
            self.expect_special(b']')?;
            dimensions.push(size);
        }

        // Handle function declarators: void (*fp)(int, char)
        // This parses the parameter list after a grouped declarator
        // We keep both the TypeIds (for building the type) and raw params (for function defs)
        let (func_params, full_func_params): (Option<FuncParamTypes>, Option<Vec<RawParam>>) =
            if self.is_special(b'(') {
                self.advance();
                let (raw_params, variadic) = self.parse_parameter_list()?;
                self.expect_special(b')')?;
                let type_ids: Vec<TypeId> = raw_params.iter().map(|(_, typ)| *typ).collect();
                (Some((type_ids, variadic)), Some(raw_params))
            } else {
                (None, None)
            };

        // Build the type from the base type
        let mut result_type_id = base_type_id;

        if let Some(inner_tid) = inner_type_id {
            // Grouped declarator: int (*p)[3] or void (*fp)(int) or int *(*q)[3]
            // Outer pointers (before parens) apply to the base type first
            // Then arrays/functions in suffix are applied
            // Finally we substitute into the inner declarator

            // Apply any outer pointers (before the parens) to base type FIRST
            // For struct node *(*fp)(int): base is struct node, outer * -> Pointer(struct node)
            // For int *(*q)[3]: base is int, outer * -> Pointer(int)
            // Note: Forward iteration is correct - qualifiers after each * apply to that pointer level
            for modifiers in pointer_modifiers.into_iter() {
                let ptr_type = Type {
                    kind: TypeKind::Pointer,
                    modifiers,
                    base: Some(result_type_id),
                    array_size: None,
                    params: None,
                    variadic: false,
                    noreturn: false,
                    composite: None,
                };
                result_type_id = self.types.intern(ptr_type);
            }

            // Apply function parameters to (possibly pointer-modified) base type
            // For struct node *(*fp)(int): result is Pointer(struct node)
            //   -> Function(Pointer(struct node), [int])
            if let Some((param_type_ids, variadic)) = func_params {
                let func_type = Type::function(result_type_id, param_type_ids, variadic, false);
                result_type_id = self.types.intern(func_type);
            }

            // Apply array dimensions to result type
            // For int *(*q)[3]: result is Pointer(int) -> Array(3, Pointer(int))
            for size in dimensions.into_iter().rev() {
                let arr_type = Type {
                    kind: TypeKind::Array,
                    modifiers: TypeModifiers::empty(),
                    base: Some(result_type_id),
                    array_size: size,
                    params: None,
                    variadic: false,
                    noreturn: false,
                    composite: None,
                };
                result_type_id = self.types.intern(arr_type);
            }

            // Substitute into inner declarator
            // For int (*p)[3]: inner_decl is Pointer(Void), result_type is Array(3, int)
            // -> Pointer(Array(3, int))
            // For struct node *(*fp)(int): inner_decl is Pointer(Void),
            //   result_type is Function(Pointer(struct node), [int])
            // -> Pointer(Function(Pointer(struct node), [int]))
            result_type_id = self.substitute_base_type(inner_tid, result_type_id);
        } else {
            // Simple declarator: char *arr[3]
            // Pointers bind tighter than arrays: *arr[3] = array of pointers

            // Apply pointer modifiers to base type first
            // Note: Forward iteration is correct - qualifiers after each * apply to that pointer level
            for modifiers in pointer_modifiers.into_iter() {
                let ptr_type = Type {
                    kind: TypeKind::Pointer,
                    modifiers,
                    base: Some(result_type_id),
                    array_size: None,
                    params: None,
                    variadic: false,
                    noreturn: false,
                    composite: None,
                };
                result_type_id = self.types.intern(ptr_type);
            }

            // Then apply array dimensions
            // For char *arr[3]: result_type is char*, suffix [3] -> Array(3, char*)
            for size in dimensions.into_iter().rev() {
                let arr_type = Type {
                    kind: TypeKind::Array,
                    modifiers: TypeModifiers::empty(),
                    base: Some(result_type_id),
                    array_size: size,
                    params: None,
                    variadic: false,
                    noreturn: false,
                    composite: None,
                };
                result_type_id = self.types.intern(arr_type);
            }

            // Apply function parameters if present (for function declarators)
            // For int get_op(int which): base is int, suffix (int) -> Function(int, [int])
            // This is needed for nested declarators like int (*get_op(int))(int, int)
            if let Some((param_type_ids, variadic)) = func_params {
                let func_type = Type::function(result_type_id, param_type_ids, variadic, false);
                result_type_id = self.types.intern(func_type);
            }
        }

        // Determine which function parameters to return:
        // - For grouped declarator with inner function params: return inner_func_params
        //   (e.g., int (*get_op(int which))(int) - inner has params)
        // - For grouped declarator without inner params: return full_func_params
        //   (e.g., int (name)(int lhs, int rhs) - parenthesized name followed by params)
        // - For simple declarator with function: return full_func_params
        let returned_func_params = if inner_func_params.is_some() {
            inner_func_params
        } else {
            full_func_params
        };

        // Propagate storage class modifiers from base type to derived type
        // For "extern int *p", the EXTERN should be on the pointer type, not just int
        let storage_class_mask = TypeModifiers::EXTERN
            | TypeModifiers::STATIC
            | TypeModifiers::TYPEDEF
            | TypeModifiers::REGISTER
            | TypeModifiers::AUTO
            | TypeModifiers::THREAD_LOCAL;
        let base_storage_class = self.types.modifiers(base_type_id) & storage_class_mask;
        if !base_storage_class.is_empty() && result_type_id != base_type_id {
            // Add storage class modifiers to the result type
            let mut result_type = self.types.get(result_type_id).clone();
            result_type.modifiers |= base_storage_class;
            result_type_id = self.types.intern(result_type);
        }

        Ok((name, result_type_id, vla_exprs, returned_func_params))
    }

    /// Substitute the actual base type into a declarator parsed with a placeholder
    /// For int (*p)[3]: inner_decl is Pointer(Void), actual_base is Array(3, int)
    /// Result should be Pointer(Array(3, int))
    fn substitute_base_type(&mut self, decl_type_id: TypeId, actual_base_id: TypeId) -> TypeId {
        let decl_type = self.types.get(decl_type_id);
        match decl_type.kind {
            TypeKind::Void => actual_base_id,
            TypeKind::Pointer => {
                let inner_base_id = decl_type.base.unwrap();
                let decl_modifiers = decl_type.modifiers;
                let new_base_id = self.substitute_base_type(inner_base_id, actual_base_id);
                let ptr_type = Type {
                    kind: TypeKind::Pointer,
                    modifiers: decl_modifiers,
                    base: Some(new_base_id),
                    array_size: None,
                    params: None,
                    variadic: false,
                    noreturn: false,
                    composite: None,
                };
                self.types.intern(ptr_type)
            }
            TypeKind::Array => {
                let inner_base_id = decl_type.base.unwrap();
                let decl_modifiers = decl_type.modifiers;
                let decl_array_size = decl_type.array_size;
                let new_base_id = self.substitute_base_type(inner_base_id, actual_base_id);
                let arr_type = Type {
                    kind: TypeKind::Array,
                    modifiers: decl_modifiers,
                    base: Some(new_base_id),
                    array_size: decl_array_size,
                    params: None,
                    variadic: false,
                    noreturn: false,
                    composite: None,
                };
                self.types.intern(arr_type)
            }
            TypeKind::Function => {
                // For function declarators like int (*get_op(int))(int, int)
                // The inner declarator is Function(Pointer(Void), [int])
                // We need to substitute Void with the actual return type
                let inner_base_id = decl_type.base.unwrap(); // return type (placeholder)
                let decl_params = decl_type.params.clone();
                let decl_variadic = decl_type.variadic;
                let decl_noreturn = decl_type.noreturn;
                let new_ret_id = self.substitute_base_type(inner_base_id, actual_base_id);
                let func_type = Type {
                    kind: TypeKind::Function,
                    modifiers: TypeModifiers::empty(),
                    base: Some(new_ret_id),
                    array_size: None,
                    params: decl_params,
                    variadic: decl_variadic,
                    noreturn: decl_noreturn,
                    composite: None,
                };
                self.types.intern(func_type)
            }
            _ => decl_type_id, // Other types don't need substitution
        }
    }

    /// Parse a function definition
    ///
    /// Binds the function to the symbol table at global scope, then enters
    /// a new scope for the function body and binds all parameters in that
    /// scope.
    #[cfg(test)]
    pub(crate) fn parse_function_def(&mut self) -> ParseResult<FunctionDef> {
        let func_pos = self.current_pos();
        // Parse return type
        let return_type = self.parse_type_specifier()?;
        let mut ret_type_id = self.types.intern(return_type);

        // Handle pointer in return type with qualifiers
        while self.is_special(b'*') {
            self.advance();
            let mut ptr_modifiers = TypeModifiers::empty();

            // Parse pointer qualifiers
            while self.peek() == TokenType::Ident {
                if let Some(name) = self.get_ident_name(self.current()) {
                    match name.as_str() {
                        "const" => {
                            self.advance();
                            ptr_modifiers |= TypeModifiers::CONST;
                        }
                        "volatile" => {
                            self.advance();
                            ptr_modifiers |= TypeModifiers::VOLATILE;
                        }
                        "restrict" => {
                            self.advance();
                            ptr_modifiers |= TypeModifiers::RESTRICT;
                        }
                        _ => break,
                    }
                } else {
                    break;
                }
            }

            let ptr_type = Type {
                kind: TypeKind::Pointer,
                modifiers: ptr_modifiers,
                base: Some(ret_type_id),
                array_size: None,
                params: None,
                variadic: false,
                composite: None,
                noreturn: false,
            };
            ret_type_id = self.types.intern(ptr_type);
        }

        // Parse function name
        let name = self.expect_identifier()?;

        // Parse parameter list
        self.expect_special(b'(')?;
        let (raw_params, variadic) = self.parse_parameter_list()?;
        self.expect_special(b')')?;

        // Build the function type
        let param_type_ids: Vec<TypeId> = raw_params.iter().map(|(_, typ)| *typ).collect();
        let func_type = Type::function(ret_type_id, param_type_ids, variadic, false);
        let func_type_id = self.types.intern(func_type);

        // Bind function to symbol table at current (global) scope
        let func_sym = Symbol::function(name, func_type_id, self.symbols.depth());
        let _ = self.symbols.declare(func_sym); // Ignore redefinition errors for now

        // Enter function scope for parameters and body
        self.symbols.enter_scope();

        // Bind parameters in function scope and create Parameter structs
        let mut params = Vec::with_capacity(raw_params.len());
        for (param_name, param_typ) in &raw_params {
            let symbol_id = if let Some(name) = param_name {
                let param_sym = Symbol::parameter(*name, *param_typ, self.symbols.depth());
                self.symbols.declare(param_sym).ok()
            } else {
                None
            };
            params.push(Parameter {
                symbol: symbol_id,
                typ: *param_typ,
            });
        }

        // Parse function body (block handles its own scope for locals)
        let body = self.parse_block_stmt_no_scope()?;

        // Leave function scope
        self.symbols.leave_scope();

        Ok(FunctionDef {
            return_type: ret_type_id,
            name,
            params,
            body,
            pos: func_pos,
            is_static: false, // Test function, storage class not parsed
            is_inline: false,
            calling_conv: crate::abi::CallingConv::default(),
        })
    }

    /// Parse a parameter list, returning raw parameter info (name and type)
    /// Parameters are declared in a temporary scope during parsing so that
    /// VLA sizes like `arr[n]` can reference earlier parameters like `n`.
    /// The scope is exited at the end; callers re-declare parameters as needed.
    pub(crate) fn parse_parameter_list(&mut self) -> ParseResult<(Vec<RawParam>, bool)> {
        let mut params: Vec<RawParam> = Vec::with_capacity(DEFAULT_PARAM_CAPACITY);
        let mut variadic = false;

        // Enter a temporary scope for parameter parsing (C99 6.9.1p9)
        // This allows VLA sizes to reference earlier parameters
        self.symbols.enter_scope();

        if self.is_special(b')') {
            self.symbols.leave_scope();
            return Ok((params, variadic));
        }

        // Check for (void)
        if self.peek() == TokenType::Ident {
            if let Some(name) = self.get_ident_name(self.current()) {
                if name == "void" {
                    let saved_pos = self.pos;
                    self.advance();
                    if self.is_special(b')') {
                        self.symbols.leave_scope();
                        return Ok((params, variadic));
                    }
                    // Not just void, backtrack
                    self.pos = saved_pos;
                }
            }
        }

        loop {
            // Check for ellipsis
            if self.is_special_token(SpecialToken::Ellipsis) {
                // ISO C requires at least one named parameter before '...'
                // GCC/Clang emit a warning with -Wstrict-prototypes
                if params.is_empty() {
                    diag::warning(
                        self.current_pos(),
                        "ISO C requires a named argument before '...'",
                    );
                }
                self.advance();
                variadic = true;
                break;
            }

            // Parse parameter type
            let param_type = self.parse_type_specifier()?;
            // For struct/union types with tags, use existing TypeId to preserve forward declarations
            let base_type_id = self.intern_type_with_tag(&param_type);

            // Use parse_declarator to handle all declarator forms including:
            // - Simple pointers: void *, int *
            // - Grouped declarators: void (*)(int), int (*)[10]
            // - Arrays: int arr[], int arr[10]
            // Note: parse_declarator returns (name, type, vla_sizes)
            let (param_name, mut typ_id, _vla_sizes, _func_params) =
                self.parse_declarator(base_type_id)?;

            // Skip any __attribute__ after parameter declarator
            self.skip_extensions();

            // C99 6.7.5.3: Array and function parameters are adjusted to pointers
            // - Array T[] becomes pointer to T
            // - Function type becomes pointer to function type
            let typ = self.types.get(typ_id);
            if typ.kind == TypeKind::Array {
                // Convert array to pointer to element type
                let element_type = typ.base.unwrap_or(self.types.void_id);
                let ptr_type = Type {
                    kind: TypeKind::Pointer,
                    modifiers: TypeModifiers::empty(),
                    base: Some(element_type),
                    array_size: None,
                    params: None,
                    variadic: false,
                    noreturn: false,
                    composite: None,
                };
                typ_id = self.types.intern(ptr_type);
            } else if typ.kind == TypeKind::Function {
                // Convert function type to pointer to function type
                // e.g., `int fn(int)` becomes `int (*)(int)`
                let ptr_type = Type {
                    kind: TypeKind::Pointer,
                    modifiers: TypeModifiers::empty(),
                    base: Some(typ_id),
                    array_size: None,
                    params: None,
                    variadic: false,
                    noreturn: false,
                    composite: None,
                };
                typ_id = self.types.intern(ptr_type);
            }

            let name_opt = if param_name == StringId::EMPTY {
                None
            } else {
                Some(param_name)
            };
            params.push((name_opt, typ_id));

            // Declare parameter in temporary scope so later params can reference it
            // (C99 6.9.1p9: parameters are in scope for VLA sizes)
            if let Some(name) = name_opt {
                let sym = Symbol::variable(name, typ_id, self.symbols.depth());
                let _ = self.symbols.declare(sym);
            }

            if self.is_special(b',') {
                self.advance();
            } else {
                break;
            }
        }

        // Leave temporary parameter scope
        self.symbols.leave_scope();

        Ok((params, variadic))
    }

    /// Parse a translation unit (top-level)
    pub fn parse_translation_unit(&mut self) -> ParseResult<TranslationUnit> {
        let mut tu = TranslationUnit::new();

        self.skip_stream_tokens();

        while !self.is_eof() {
            // Try to determine if this is a function definition or a declaration
            // Both start with type specifier + declarator
            let external_decl = self.parse_external_decl()?;
            tu.add(external_decl);
        }

        Ok(tu)
    }

    /// Check if current token is _Static_assert or static_assert
    fn is_static_assert(&self) -> bool {
        if self.peek() != TokenType::Ident {
            return false;
        }
        if let Some(name_id) = self.get_ident_id(self.current()) {
            let name = self.str(name_id);
            matches!(name, "_Static_assert" | "static_assert")
        } else {
            false
        }
    }

    /// Parse _Static_assert(constant-expression, string-literal);
    /// C11: _Static_assert(expr, msg)
    /// C23: static_assert(expr) or static_assert(expr, msg)
    fn parse_static_assert(&mut self) -> ParseResult<()> {
        let pos = self.current_pos();
        self.advance(); // consume _Static_assert / static_assert
        self.expect_special(b'(')?;

        // Parse constant expression
        let expr = self.parse_conditional_expr()?;

        // Evaluate the constant expression
        let value = self.eval_const_expr(&expr);

        // Check for optional message (C23 allows omitting it)
        let message = if self.is_special(b',') {
            self.advance(); // consume ','
                            // Expect string literal
            if self.peek() != TokenType::String {
                return Err(ParseError::new(
                    "expected string literal in _Static_assert",
                    self.current_pos(),
                ));
            }
            let msg = if let TokenValue::String(s) = &self.current().value {
                s.clone()
            } else {
                String::new()
            };
            self.advance(); // consume string
            msg
        } else {
            // C23: no message provided
            String::new()
        };

        self.expect_special(b')')?;
        self.expect_special(b';')?;

        // Check if assertion failed
        if let Some(v) = value {
            if v == 0 {
                // Assertion failed
                let msg = if message.is_empty() {
                    "static assertion failed".to_string()
                } else {
                    format!("static assertion failed: {}", message)
                };
                return Err(ParseError::new(msg, pos));
            }
        } else {
            // Could not evaluate at compile time
            return Err(ParseError::new(
                "_Static_assert expression is not a constant expression",
                pos,
            ));
        }

        Ok(())
    }

    /// Parse an external declaration (function definition or declaration)
    fn parse_external_decl(&mut self) -> ParseResult<ExternalDecl> {
        // Check for _Static_assert first (C11)
        if self.is_static_assert() {
            self.parse_static_assert()?;
            // Return empty declaration - static_assert produces nothing
            return Ok(ExternalDecl::Declaration(Declaration {
                declarators: vec![],
            }));
        }

        let decl_pos = self.current_pos();
        // Parse type specifier
        let base_type = self.parse_type_specifier()?;
        // Skip __attribute__ between type and declarator (GCC extension)
        self.skip_extensions();
        // Check modifiers before interning (storage class specifiers)
        let is_typedef = base_type.modifiers.contains(TypeModifiers::TYPEDEF);
        // Extract storage class specifiers (not stored in type system)
        let storage_class_mask = TypeModifiers::EXTERN
            | TypeModifiers::STATIC
            | TypeModifiers::THREAD_LOCAL
            | TypeModifiers::TYPEDEF
            | TypeModifiers::AUTO
            | TypeModifiers::REGISTER;
        let storage_class = base_type.modifiers & storage_class_mask;
        // For struct/union types with tags, use existing TypeId to preserve forward declarations
        let base_type_id = self.intern_type_with_tag(&base_type);

        // Check for standalone type definition (e.g., "enum Color { ... };")
        // This happens when a composite type is defined but no variables are declared
        if self.is_special(b';') {
            self.advance();
            // Return empty declaration - the type was already registered in parse_*_specifier
            return Ok(ExternalDecl::Declaration(Declaration {
                declarators: vec![],
            }));
        }

        // Check for grouped declarator: void (*fp)(int), int (*arr)[10], or typedef int (name)(params)
        if self.is_special(b'(') {
            let saved_pos = self.pos;
            self.advance(); // consume '('
            if self.is_grouped_declarator() {
                // This is a grouped declarator - use parse_declarator
                self.pos = saved_pos; // restore position before '('
                let (name, typ, vla_sizes, decl_func_params) =
                    self.parse_declarator(base_type_id)?;

                // C99 6.7.5.2: VLAs must have block scope
                if !vla_sizes.is_empty() {
                    return Err(ParseError::new(
                        "variable length arrays cannot have file scope".to_string(),
                        self.current_pos(),
                    ));
                }

                // Parse any __attribute__ after declarator
                let attrs = self.parse_attributes();
                let calling_conv = attrs.calling_conv().unwrap_or_default();

                // Check if this is a function definition (function type followed by '{')
                // This handles cases like: int (*get_op(int which))(int, int) { ... }
                if self.types.kind(typ) == TypeKind::Function && self.is_special(b'{') {
                    // Get the function's return type
                    // Storage class (static, inline) comes from base_type, not the function type
                    let func_type = self.types.get(typ);
                    let return_type = func_type.base.unwrap();
                    let _is_variadic = func_type.variadic;
                    let is_static = base_type.modifiers.contains(TypeModifiers::STATIC);
                    let is_inline = base_type.modifiers.contains(TypeModifiers::INLINE);

                    // Add function to symbol table
                    let func_sym = Symbol::function(name, typ, self.symbols.depth());
                    let _ = self.symbols.declare(func_sym);

                    // Get raw parameters - use decl_func_params which has names
                    let raw_params = decl_func_params.unwrap_or_default();

                    // Enter function scope for parameters
                    self.symbols.enter_scope();

                    // Bind parameters in function scope and create Parameter structs
                    let mut params = Vec::with_capacity(raw_params.len());
                    for (param_name_opt, param_typ) in &raw_params {
                        let symbol_id = if let Some(param_name) = param_name_opt {
                            let param_sym =
                                Symbol::parameter(*param_name, *param_typ, self.symbols.depth());
                            self.symbols.declare(param_sym).ok()
                        } else {
                            None
                        };
                        params.push(Parameter {
                            symbol: symbol_id,
                            typ: *param_typ,
                        });
                    }

                    // Parse body without creating another scope
                    let body = self.parse_block_stmt_no_scope()?;

                    // Leave function scope
                    self.symbols.leave_scope();

                    return Ok(ExternalDecl::FunctionDef(FunctionDef {
                        return_type,
                        name,
                        params,
                        body,
                        pos: decl_pos,
                        is_static,
                        is_inline,
                        calling_conv,
                    }));
                }

                // Handle initializer (for declarations, not function definitions)
                let init = if self.is_special(b'=') {
                    if is_typedef {
                        return Err(ParseError::new(
                            "typedef cannot have initializer",
                            self.current_pos(),
                        ));
                    }
                    self.advance();
                    Some(self.parse_initializer()?)
                } else {
                    None
                };

                self.expect_special(b';')?;

                // Validate explicit alignment (C11 6.7.5: >= natural alignment)
                let validated_align = self.validated_explicit_align(typ)?;

                // Add to symbol table and capture SymbolId
                // C allows multiple declarations of the same variable at file scope
                let symbol_id = if is_typedef {
                    let sym = Symbol::typedef(name, typ, self.symbols.depth());
                    self.symbols
                        .declare(sym)
                        .ok()
                        .or_else(|| self.symbols.lookup_id(name, Namespace::Ordinary))
                } else {
                    let var_sym = Symbol::variable(name, typ, self.symbols.depth())
                        .with_align(validated_align);
                    self.symbols
                        .declare(var_sym)
                        .ok()
                        .or_else(|| self.symbols.lookup_id(name, Namespace::Ordinary))
                };

                let symbol = symbol_id.expect("declaration must have symbol");
                return Ok(ExternalDecl::Declaration(Declaration {
                    declarators: vec![InitDeclarator {
                        symbol,
                        typ,
                        storage_class,
                        init,
                        vla_sizes: vec![],
                        explicit_align: validated_align,
                    }],
                }));
            }
            // Not a grouped declarator, restore position
            self.pos = saved_pos;
        }

        // Handle pointer with qualifiers (const, volatile, restrict)
        let mut typ_id = base_type_id;
        while self.is_special(b'*') {
            self.advance();
            let mut ptr_modifiers = TypeModifiers::empty();

            // Parse pointer qualifiers
            while self.peek() == TokenType::Ident {
                if let Some(name) = self.get_ident_name(self.current()) {
                    match name.as_str() {
                        "const" => {
                            self.advance();
                            ptr_modifiers |= TypeModifiers::CONST;
                        }
                        "volatile" => {
                            self.advance();
                            ptr_modifiers |= TypeModifiers::VOLATILE;
                        }
                        "restrict" => {
                            self.advance();
                            ptr_modifiers |= TypeModifiers::RESTRICT;
                        }
                        _ => break,
                    }
                } else {
                    break;
                }
            }

            let ptr_type = Type {
                kind: TypeKind::Pointer,
                modifiers: ptr_modifiers,
                base: Some(typ_id),
                array_size: None,
                params: None,
                variadic: false,
                noreturn: false,
                composite: None,
            };
            typ_id = self.types.intern(ptr_type);
        }

        // Propagate storage class modifiers from base type to derived pointer type
        // For "extern int *p", the EXTERN should be on the pointer type, not just int
        if typ_id != base_type_id {
            let storage_class_mask = TypeModifiers::EXTERN
                | TypeModifiers::STATIC
                | TypeModifiers::TYPEDEF
                | TypeModifiers::REGISTER
                | TypeModifiers::AUTO
                | TypeModifiers::THREAD_LOCAL;
            let base_storage_class = self.types.modifiers(base_type_id) & storage_class_mask;
            if !base_storage_class.is_empty() {
                let mut typ = self.types.get(typ_id).clone();
                typ.modifiers |= base_storage_class;
                typ_id = self.types.intern(typ);
            }
        }

        // Skip __attribute__ after pointer declarator (GCC extension)
        // Handles: void * __attribute__((malloc)) func(...)
        self.skip_extensions();

        // Check again for grouped declarator after pointer modifiers: char *(*fp)(int)
        // Also handles: char *(name)(params) for function type
        if self.is_special(b'(') {
            let saved_pos = self.pos;
            self.advance(); // consume '('
            if self.is_grouped_declarator() {
                // This is a grouped declarator - use parse_declarator
                self.pos = saved_pos; // restore position before '('
                let (name, full_typ, vla_sizes, decl_func_params) =
                    self.parse_declarator(typ_id)?;

                // C99 6.7.5.2: VLAs must have block scope
                if !vla_sizes.is_empty() {
                    return Err(ParseError::new(
                        "variable length arrays cannot have file scope".to_string(),
                        self.current_pos(),
                    ));
                }

                // Parse any __attribute__ after declarator
                let attrs = self.parse_attributes();
                let calling_conv = attrs.calling_conv().unwrap_or_default();

                // Check if this is a function definition (function type followed by '{')
                // This handles cases like: char *(*get_op(int which))(int, int) { ... }
                if self.types.kind(full_typ) == TypeKind::Function && self.is_special(b'{') {
                    // Get the function's return type
                    // Storage class (static, inline) comes from base_type, not the function type
                    let func_type = self.types.get(full_typ);
                    let return_type = func_type.base.unwrap();
                    let is_static = base_type.modifiers.contains(TypeModifiers::STATIC);
                    let is_inline = base_type.modifiers.contains(TypeModifiers::INLINE);

                    // Add function to symbol table
                    let func_sym = Symbol::function(name, full_typ, self.symbols.depth());
                    let _ = self.symbols.declare(func_sym);

                    // Get raw parameters - use decl_func_params which has names
                    let raw_params = decl_func_params.unwrap_or_default();

                    // Enter function scope for parameters
                    self.symbols.enter_scope();

                    // Bind parameters in function scope and create Parameter structs
                    let mut params = Vec::with_capacity(raw_params.len());
                    for (param_name_opt, param_typ) in &raw_params {
                        let symbol_id = if let Some(param_name) = param_name_opt {
                            let param_sym =
                                Symbol::parameter(*param_name, *param_typ, self.symbols.depth());
                            self.symbols.declare(param_sym).ok()
                        } else {
                            None
                        };
                        params.push(Parameter {
                            symbol: symbol_id,
                            typ: *param_typ,
                        });
                    }

                    // Parse body without creating another scope
                    let body = self.parse_block_stmt_no_scope()?;

                    // Leave function scope
                    self.symbols.leave_scope();

                    return Ok(ExternalDecl::FunctionDef(FunctionDef {
                        return_type,
                        name,
                        params,
                        body,
                        pos: decl_pos,
                        is_static,
                        is_inline,
                        calling_conv,
                    }));
                }

                // Handle initializer (for declarations, not function definitions)
                let init = if self.is_special(b'=') {
                    if is_typedef {
                        return Err(ParseError::new(
                            "typedef cannot have initializer",
                            self.current_pos(),
                        ));
                    }
                    self.advance();
                    Some(self.parse_initializer()?)
                } else {
                    None
                };

                self.expect_special(b';')?;

                // Validate explicit alignment (C11 6.7.5: >= natural alignment)
                let validated_align = self.validated_explicit_align(full_typ)?;

                // Add to symbol table and capture SymbolId
                // C allows multiple declarations of the same variable at file scope
                let symbol_id = if is_typedef {
                    let sym = Symbol::typedef(name, full_typ, self.symbols.depth());
                    self.symbols
                        .declare(sym)
                        .ok()
                        .or_else(|| self.symbols.lookup_id(name, Namespace::Ordinary))
                } else {
                    let var_sym = Symbol::variable(name, full_typ, self.symbols.depth())
                        .with_align(validated_align);
                    self.symbols
                        .declare(var_sym)
                        .ok()
                        .or_else(|| self.symbols.lookup_id(name, Namespace::Ordinary))
                };

                let symbol = symbol_id.expect("declaration must have symbol");
                return Ok(ExternalDecl::Declaration(Declaration {
                    declarators: vec![InitDeclarator {
                        symbol,
                        typ: full_typ,
                        storage_class,
                        init,
                        vla_sizes: vec![],
                        explicit_align: validated_align,
                    }],
                }));
            }
            // Not a grouped declarator, restore position
            self.pos = saved_pos;
        }

        // Parse name
        let name = self.expect_identifier()?;

        // Check for function definition vs declaration
        if self.is_special(b'(') {
            // Could be function definition or declaration
            self.advance();
            let (params, variadic) = self.parse_parameter_list()?;
            self.expect_special(b')')?;

            // Parse __attribute__ after parameter list (e.g., __attribute__((noreturn)))
            let attrs = self.parse_attributes();
            // noreturn can come from __attribute__((noreturn)) or _Noreturn keyword in base type
            let typ_from_table = self.types.get(typ_id);
            let is_noreturn =
                attrs.has_noreturn() || typ_from_table.modifiers.contains(TypeModifiers::NORETURN);
            // Extract calling convention from attributes
            let calling_conv = attrs.calling_conv().unwrap_or_default();

            if self.is_special(b'{') {
                // Function definition
                // Use storage_class extracted from original base_type at line 5926,
                // not from typ_id which may have lost storage class for tagged structs
                let is_static = storage_class.contains(TypeModifiers::STATIC);
                let is_inline = storage_class.contains(TypeModifiers::INLINE);

                // Add function to symbol table so it can be called by other functions
                let param_type_ids: Vec<TypeId> = params.iter().map(|(_, typ)| *typ).collect();
                let func_type =
                    Type::function(typ_id, param_type_ids.clone(), variadic, is_noreturn);
                let func_type_id = self.types.intern(func_type);
                let func_sym = Symbol::function(name, func_type_id, self.symbols.depth());
                let _ = self.symbols.declare(func_sym);

                // Enter function scope for parameters
                self.symbols.enter_scope();

                // Bind parameters in function scope and create Parameter structs
                let mut final_params = Vec::with_capacity(params.len());
                for (param_name_opt, param_typ) in &params {
                    let symbol_id = if let Some(param_name) = param_name_opt {
                        let param_sym =
                            Symbol::parameter(*param_name, *param_typ, self.symbols.depth());
                        self.symbols.declare(param_sym).ok()
                    } else {
                        None
                    };
                    final_params.push(Parameter {
                        symbol: symbol_id,
                        typ: *param_typ,
                    });
                }

                // Parse body without creating another scope
                let body = self.parse_block_stmt_no_scope()?;

                // Leave function scope
                self.symbols.leave_scope();

                return Ok(ExternalDecl::FunctionDef(FunctionDef {
                    return_type: typ_id,
                    name,
                    params: final_params,
                    body,
                    pos: decl_pos,
                    is_static,
                    is_inline,
                    calling_conv,
                }));
            } else {
                // Function declaration
                // Skip __asm("...") symbol aliasing which can appear after function declarator
                self.skip_extensions();
                self.expect_special(b';')?;
                let param_type_ids: Vec<TypeId> = params.iter().map(|(_, typ)| *typ).collect();
                let func_type = Type::function(typ_id, param_type_ids, variadic, is_noreturn);
                let func_type_id = self.types.intern(func_type);
                // Add to symbol table and capture SymbolId
                // C allows multiple declarations of the same function
                let symbol_id = if is_typedef {
                    // Function type typedef: typedef void my_func(void);
                    let sym = Symbol::typedef(name, func_type_id, self.symbols.depth());
                    self.symbols
                        .declare(sym)
                        .ok()
                        .or_else(|| self.symbols.lookup_id(name, Namespace::Ordinary))
                } else {
                    // Function declaration: add so the variadic flag is available when called
                    let func_sym = Symbol::function(name, func_type_id, self.symbols.depth());
                    self.symbols
                        .declare(func_sym)
                        .ok()
                        .or_else(|| self.symbols.lookup_id(name, Namespace::Ordinary))
                };
                let symbol = symbol_id.expect("function declaration must have symbol");
                return Ok(ExternalDecl::Declaration(Declaration {
                    declarators: vec![InitDeclarator {
                        symbol,
                        typ: func_type_id,
                        storage_class,
                        init: None,
                        vla_sizes: vec![],
                        explicit_align: None, // Functions don't have _Alignas
                    }],
                }));
            }
        }

        // Variable/typedef declaration
        let mut declarators = Vec::new();

        // Handle array - collect dimensions first, build type from right to left
        let mut var_type_id = typ_id;
        let mut dimensions: Vec<Option<usize>> = Vec::new();
        while self.is_special(b'[') {
            self.advance();
            let size = if self.is_special(b']') {
                None
            } else {
                // Parse constant expression for array size (C99 6.7.5.2)
                let arr_expr = self.parse_assignment_expr()?;
                // Evaluate as integer constant expression
                match self.eval_const_expr(&arr_expr) {
                    Some(n) if n >= 0 => Some(n as usize),
                    Some(_) => None, // Negative size is invalid
                    None => None,    // Non-constant (VLA) or invalid expression
                }
            };
            self.expect_special(b']')?;
            dimensions.push(size);
        }
        // Build type from right to left (innermost dimension first)
        for size in dimensions.into_iter().rev() {
            let arr_type = Type::array(var_type_id, size.unwrap_or(0));
            var_type_id = self.types.intern(arr_type);
        }

        // Propagate storage class modifiers from base type to derived array type
        // For "typedef int arr[10]", the TYPEDEF should be on the array type, not just int
        if var_type_id != typ_id {
            let storage_class_mask = TypeModifiers::EXTERN
                | TypeModifiers::STATIC
                | TypeModifiers::TYPEDEF
                | TypeModifiers::REGISTER
                | TypeModifiers::AUTO
                | TypeModifiers::THREAD_LOCAL;
            let base_storage_class = self.types.modifiers(typ_id) & storage_class_mask;
            if !base_storage_class.is_empty() {
                let mut var_type = self.types.get(var_type_id).clone();
                var_type.modifiers |= base_storage_class;
                var_type_id = self.types.intern(var_type);
            }
        }

        // Skip any __attribute__ after variable name/array declarator
        self.skip_extensions();

        // Validate explicit alignment (C11 6.7.5: >= natural alignment)
        let validated_align = self.validated_explicit_align(var_type_id)?;

        // Bind variable to symbol table BEFORE parsing initializer.
        // This ensures the variable is in scope for self-referential initializers.
        // Per C99 6.2.1p7: "Any other identifier has scope that begins just
        // after the completion of its declarator."
        // For typedefs, we bind AFTER (since typedef initializers are forbidden anyway).
        let mut symbol = if is_typedef {
            None // Will be bound after initializer parsing
        } else {
            // Add global variable to symbol table so it can be referenced in initializer
            let var_sym = Symbol::variable(name, var_type_id, self.symbols.depth())
                .with_align(validated_align);
            Some(match self.symbols.declare(var_sym) {
                Ok(id) => id,
                Err(_) => {
                    // Extern declaration of existing variable - reuse existing symbol
                    self.symbols
                        .lookup_id(name, Namespace::Ordinary)
                        .expect("redeclaration should find existing symbol")
                }
            })
        };

        // Handle initializer (not allowed for typedef)
        let init = if self.is_special(b'=') {
            if is_typedef {
                return Err(ParseError::new(
                    "typedef cannot have initializer",
                    self.current_pos(),
                ));
            }
            self.advance();
            Some(self.parse_initializer()?)
        } else {
            None
        };

        // For incomplete array types, infer size from initializer
        if let Some(ref init_expr) = init {
            let old_type = var_type_id;
            var_type_id = self.infer_array_size_from_init(var_type_id, init_expr);

            // If the type changed (array size was inferred), update the symbol's type
            // This is needed because the symbol was already added before parsing the initializer
            if var_type_id != old_type {
                if let Some(sym_id) = symbol {
                    self.symbols.get_mut(sym_id).typ = var_type_id;
                }
            }
        }

        // Bind typedef to symbol table (after parsing initializer, which is forbidden anyway)
        if is_typedef {
            let sym = Symbol::typedef(name, var_type_id, self.symbols.depth());
            symbol = Some(match self.symbols.declare(sym) {
                Ok(id) => id,
                Err(_) => self
                    .symbols
                    .lookup_id(name, Namespace::Ordinary)
                    .expect("redeclaration should find existing symbol"),
            });
        }

        let symbol = symbol.expect("symbol should be bound");
        declarators.push(InitDeclarator {
            symbol,
            typ: var_type_id,
            storage_class,
            init,
            vla_sizes: vec![],
            explicit_align: validated_align,
        });

        // Handle additional declarators
        while self.is_special(b',') {
            self.advance();
            let (decl_name, mut decl_type, vla_sizes, _decl_func_params) =
                self.parse_declarator(base_type_id)?;

            // C99 6.7.5.2: VLAs must have block scope
            if !vla_sizes.is_empty() {
                return Err(ParseError::new(
                    "variable length arrays cannot have file scope".to_string(),
                    self.current_pos(),
                ));
            }

            // Validate explicit alignment for this declarator's type (C11 6.7.5)
            let decl_validated_align = self.validated_explicit_align(decl_type)?;

            // Bind variable to symbol table BEFORE parsing initializer (C99 6.2.1p7)
            let mut decl_symbol = if is_typedef {
                None // Will be bound after initializer parsing
            } else {
                let var_sym = Symbol::variable(decl_name, decl_type, self.symbols.depth())
                    .with_align(decl_validated_align);
                Some(match self.symbols.declare(var_sym) {
                    Ok(id) => id,
                    Err(_) => self
                        .symbols
                        .lookup_id(decl_name, Namespace::Ordinary)
                        .expect("redeclaration should find existing symbol"),
                })
            };

            let decl_init = if self.is_special(b'=') {
                if is_typedef {
                    return Err(ParseError::new(
                        "typedef cannot have initializer",
                        self.current_pos(),
                    ));
                }
                self.advance();
                Some(self.parse_initializer()?)
            } else {
                None
            };

            // For incomplete array types, infer size from initializer
            if let Some(ref init_expr) = decl_init {
                let old_type = decl_type;
                decl_type = self.infer_array_size_from_init(decl_type, init_expr);

                // If the type changed (array size was inferred), update the symbol's type
                // This is needed because the symbol was already added before parsing the initializer
                if decl_type != old_type {
                    if let Some(sym_id) = decl_symbol {
                        self.symbols.get_mut(sym_id).typ = decl_type;
                    }
                }
            }

            // Bind typedef to symbol table (after parsing initializer, which is forbidden anyway)
            if is_typedef {
                let sym = Symbol::typedef(decl_name, decl_type, self.symbols.depth());
                decl_symbol = Some(match self.symbols.declare(sym) {
                    Ok(id) => id,
                    Err(_) => self
                        .symbols
                        .lookup_id(decl_name, Namespace::Ordinary)
                        .expect("redeclaration should find existing symbol"),
                });
            }

            declarators.push(InitDeclarator {
                symbol: decl_symbol.expect("symbol should be bound"),
                typ: decl_type,
                storage_class,
                init: decl_init,
                vla_sizes: vec![],
                explicit_align: decl_validated_align,
            });
        }

        self.expect_special(b';')?;

        Ok(ExternalDecl::Declaration(Declaration { declarators }))
    }

    /// Evaluate a constant expression (for array sizes, enum values, case labels, static initializers)
    ///
    /// C99 6.6 defines integer constant expressions as:
    /// - Integer/character literals
    /// - Enum constants
    /// - sizeof expressions
    /// - Casts to integer types
    /// - Unary/binary operators with constant operands
    /// - Conditional expressions with constant operands
    pub(crate) fn eval_const_expr(&self, expr: &Expr) -> Option<i64> {
        match &expr.kind {
            ExprKind::IntLit(val) => Some(*val),
            ExprKind::CharLit(c) => Some(*c as i64),
            // Float literals are constant, return truncated value
            ExprKind::FloatLit(val) => Some(*val as i64),

            ExprKind::Unary { op, operand } => {
                let val = self.eval_const_expr(operand)?;
                match op {
                    UnaryOp::Neg => Some(-val),
                    UnaryOp::Not => Some(if val == 0 { 1 } else { 0 }),
                    UnaryOp::BitNot => Some(!val),
                    _ => None,
                }
            }

            ExprKind::Binary { op, left, right } => {
                let lval = self.eval_const_expr(left)?;
                let rval = self.eval_const_expr(right)?;
                match op {
                    BinaryOp::Add => Some(lval.wrapping_add(rval)),
                    BinaryOp::Sub => Some(lval.wrapping_sub(rval)),
                    BinaryOp::Mul => Some(lval.wrapping_mul(rval)),
                    BinaryOp::Div => {
                        if rval != 0 {
                            Some(lval / rval)
                        } else {
                            None
                        }
                    }
                    BinaryOp::Mod => {
                        if rval != 0 {
                            Some(lval % rval)
                        } else {
                            None
                        }
                    }
                    BinaryOp::BitAnd => Some(lval & rval),
                    BinaryOp::BitOr => Some(lval | rval),
                    BinaryOp::BitXor => Some(lval ^ rval),
                    BinaryOp::Shl => Some(lval << (rval as u32)),
                    BinaryOp::Shr => Some(lval >> (rval as u32)),
                    BinaryOp::Lt => Some(if lval < rval { 1 } else { 0 }),
                    BinaryOp::Le => Some(if lval <= rval { 1 } else { 0 }),
                    BinaryOp::Gt => Some(if lval > rval { 1 } else { 0 }),
                    BinaryOp::Ge => Some(if lval >= rval { 1 } else { 0 }),
                    BinaryOp::Eq => Some(if lval == rval { 1 } else { 0 }),
                    BinaryOp::Ne => Some(if lval != rval { 1 } else { 0 }),
                    BinaryOp::LogAnd => Some(if lval != 0 && rval != 0 { 1 } else { 0 }),
                    BinaryOp::LogOr => Some(if lval != 0 || rval != 0 { 1 } else { 0 }),
                }
            }

            ExprKind::Ident(symbol_id) => {
                // Check for enum constant
                let sym = self.symbols.get(*symbol_id);
                if sym.is_enum_constant() {
                    sym.enum_value
                } else {
                    None
                }
            }

            ExprKind::FuncName => None,

            ExprKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond_val = self.eval_const_expr(cond)?;
                if cond_val != 0 {
                    self.eval_const_expr(then_expr)
                } else {
                    self.eval_const_expr(else_expr)
                }
            }

            // sizeof(type) - constant for complete types
            ExprKind::SizeofType(type_id) => {
                let size_bits = self.types.size_bits(*type_id);
                Some((size_bits / 8) as i64)
            }

            // sizeof(expr) - constant if expr type is complete
            ExprKind::SizeofExpr(inner_expr) => {
                if let Some(typ) = inner_expr.typ {
                    let size_bits = self.types.size_bits(typ);
                    Some((size_bits / 8) as i64)
                } else {
                    None
                }
            }

            // Cast to integer type - evaluate inner and truncate/extend as needed
            ExprKind::Cast { expr: inner, .. } => {
                // For integer constant expressions, we can evaluate the inner expression
                self.eval_const_expr(inner)
            }

            // __builtin_offsetof(type, member-designator) - compile-time constant
            ExprKind::OffsetOf { type_id, path } => {
                let mut offset: i64 = 0;
                let mut current_type = *type_id;

                for element in path {
                    match element {
                        OffsetOfPath::Field(field_id) => {
                            // Look up the field in the current struct type
                            // find_member handles recursive lookup through anonymous members
                            if let Some(member_info) =
                                self.types.find_member(current_type, *field_id)
                            {
                                offset += member_info.offset as i64;
                                current_type = member_info.typ;
                            } else {
                                return None; // Field not found
                            }
                        }
                        OffsetOfPath::Index(index) => {
                            // Array indexing: offset += index * sizeof(element)
                            if let Some(elem_type) = self.types.base_type(current_type) {
                                let elem_size = self.types.size_bytes(elem_type);
                                offset += *index * (elem_size as i64);
                                current_type = elem_type;
                            } else {
                                return None; // Not an array type
                            }
                        }
                    }
                }

                Some(offset)
            }

            _ => None,
        }
    }

    /// Parse a bitfield width (constant expression after ':')
    fn parse_bitfield_width(&mut self) -> ParseResult<u32> {
        let expr = self.parse_conditional_expr()?;
        match self.eval_const_expr(&expr) {
            Some(val) if val >= 0 => Ok(val as u32),
            Some(_) => Err(ParseError::new(
                "bitfield width must be non-negative",
                self.current_pos(),
            )),
            None => Err(ParseError::new(
                "bitfield width must be a constant expression",
                self.current_pos(),
            )),
        }
    }

    /// Validate a bitfield declaration
    ///
    /// `is_named` indicates if this bitfield has a name (unnamed bitfields are
    /// allowed to have zero width for alignment purposes).
    fn validate_bitfield(&self, typ_id: TypeId, width: u32, is_named: bool) -> ParseResult<()> {
        // Check allowed types: _Bool, int, unsigned int (and their signed/unsigned variants)
        // Also allow long long since GCC/Clang support it
        let kind = self.types.kind(typ_id);
        let valid_type = matches!(
            kind,
            TypeKind::Bool
                | TypeKind::Int
                | TypeKind::Char
                | TypeKind::Short
                | TypeKind::Long
                | TypeKind::LongLong
        );

        if !valid_type {
            return Err(ParseError::new(
                "bitfield must have integer type",
                self.current_pos(),
            ));
        }

        // C99: Zero-width bitfield with a name is an error
        // (zero-width unnamed bitfields are allowed for alignment)
        if width == 0 && is_named {
            return Err(ParseError::new(
                "named bit-field has zero width",
                self.current_pos(),
            ));
        }

        // Check that width doesn't exceed type size
        let max_width = self.types.size_bits(typ_id);
        if width > max_width {
            return Err(ParseError::new(
                format!("bitfield width {} exceeds type size {}", width, max_width),
                self.current_pos(),
            ));
        }

        // Warning: one-bit signed bitfield has dubious values
        // (can only hold -1 or 0 in 2's complement, or 0/-0 in other representations)
        if width == 1 && !self.types.is_unsigned(typ_id) && kind != TypeKind::Bool {
            diag::warning(
                self.current_pos(),
                "single-bit signed bit-field has dubious values",
            );
        }

        Ok(())
    }

    /// Validate explicit alignment against natural alignment (C11 6.7.5)
    ///
    /// Returns the validated explicit alignment, or error if alignment is weaker than natural.
    /// Returns None if no explicit alignment was specified.
    fn validated_explicit_align(&self, typ: TypeId) -> ParseResult<Option<u32>> {
        match self.pending_alignas {
            None => Ok(None),
            Some(explicit) => {
                let natural = self.types.alignment(typ) as u32;
                if explicit < natural {
                    Err(ParseError::new(
                        format!(
                            "_Alignas({}) cannot reduce alignment below natural alignment {}",
                            explicit, natural
                        ),
                        self.current_pos(),
                    ))
                } else {
                    Ok(Some(explicit))
                }
            }
        }
    }
}
