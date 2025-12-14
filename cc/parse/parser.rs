//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parser for pcc C99 compiler
// Based on sparse's expression.c recursive descent design
//

use super::ast::{
    AssignOp, BinaryOp, BlockItem, Declaration, Designator, Expr, ExprKind, ExternalDecl, ForInit,
    FunctionDef, InitDeclarator, InitElement, Parameter, Stmt, TranslationUnit, UnaryOp,
};
use crate::diag;
use crate::strings::StringId;
use crate::symbol::{Namespace, Symbol, SymbolTable};
use crate::token::lexer::{IdentTable, Position, SpecialToken, Token, TokenType, TokenValue};
use crate::types::{
    CompositeType, EnumConstant, StructMember, Type, TypeId, TypeKind, TypeModifiers, TypeTable,
};
use std::fmt;

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

/// Result of parsing a declarator: (name, type, VLA expressions, function parameters)
type DeclaratorResult = (StringId, TypeId, Vec<Expr>, Option<Vec<Parameter>>);

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
/// Following sparse's design, the parser binds symbols to the symbol table
/// during parsing. This means that by the time parsing is complete, all
/// declared symbols are in the table with their types.
pub struct Parser<'a> {
    /// Token stream
    tokens: &'a [Token],
    /// Identifier table for looking up names
    idents: &'a IdentTable,
    /// Symbol table for binding declarations (like sparse's bind_symbol)
    symbols: &'a mut SymbolTable,
    /// Type table for interning types
    types: &'a mut TypeTable,
    /// Current position in token stream
    pos: usize,
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
        }
    }

    // ========================================================================
    // Token Navigation
    // ========================================================================

    /// Get the current token
    fn current(&self) -> &Token {
        self.tokens
            .get(self.pos)
            .unwrap_or(&self.tokens[self.tokens.len() - 1])
    }

    /// Peek at the current token type
    fn peek(&self) -> TokenType {
        self.current().typ
    }

    /// Peek at the current token's special value (if it's a Special token)
    fn peek_special(&self) -> Option<u32> {
        let token = self.current();
        if token.typ == TokenType::Special {
            if let TokenValue::Special(v) = &token.value {
                return Some(*v);
            }
        }
        None
    }

    /// Check if current token is a specific special character
    fn is_special(&self, c: u8) -> bool {
        self.peek_special() == Some(c as u32)
    }

    /// Check if current token is a specific multi-char special token
    fn is_special_token(&self, tok: SpecialToken) -> bool {
        self.peek_special() == Some(tok as u32)
    }

    /// Get current position for error messages
    fn current_pos(&self) -> Position {
        self.current().pos
    }

    /// Advance to the next token
    fn advance(&mut self) {
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
    }

    /// Consume a token and advance, returning a clone
    fn consume(&mut self) -> Token {
        let token = self.current().clone();
        self.advance();
        token
    }

    /// Expect a specific special character, return error if not found
    fn expect_special(&mut self, c: u8) -> ParseResult<()> {
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
    fn get_ident_name(&self, token: &Token) -> Option<String> {
        if let TokenValue::Ident(id) = &token.value {
            self.idents.get_opt(*id).map(|s| s.to_string())
        } else {
            None
        }
    }

    /// Get the StringId directly from an Ident token
    fn get_ident_id(&self, token: &Token) -> Option<StringId> {
        if let TokenValue::Ident(id) = &token.value {
            Some(*id)
        } else {
            None
        }
    }

    /// Get string value for a StringId
    #[inline]
    fn str(&self, id: StringId) -> &str {
        self.idents.get(id)
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

    /// Skip GCC extended inline assembly statement
    /// Format: __asm__ [volatile] ( "template" [: outputs [: inputs [: clobbers]]] );
    fn skip_asm_statement(&mut self) {
        self.advance(); // consume __asm/__asm__

        // Skip optional 'volatile' or '__volatile__'
        if self.peek() == TokenType::Ident {
            if let Some(name) = self.get_ident_name(self.current()) {
                if name == "volatile" || name == "__volatile__" {
                    self.advance();
                }
            }
        }

        // Expect '('
        if !self.is_special(b'(') {
            return;
        }
        self.advance(); // consume '('

        // Skip contents until matching ')', handling nested parens
        let mut depth = 1;
        while depth > 0 && !self.is_eof() {
            if self.is_special(b'(') {
                depth += 1;
            } else if self.is_special(b')') {
                depth -= 1;
            }
            self.advance();
        }

        // Consume trailing semicolon
        if self.is_special(b';') {
            self.advance();
        }
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

    // ========================================================================
    // Expression Parsing - Precedence Chain
    //
    // From lowest to highest precedence:
    // 1. comma (left-to-right)
    // 2. assignment (right-to-left)
    // 3. ternary/conditional (right-to-left)
    // 4. logical-or (left-to-right)
    // 5. logical-and (left-to-right)
    // 6. bitwise-or (left-to-right)
    // 7. bitwise-xor (left-to-right)
    // 8. bitwise-and (left-to-right)
    // 9. equality (left-to-right)
    // 10. relational (left-to-right)
    // 11. shift (left-to-right)
    // 12. additive (left-to-right)
    // 13. multiplicative (left-to-right)
    // 14. unary (right-to-left)
    // 15. postfix (left-to-right)
    // 16. primary
    // ========================================================================

    /// Parse an expression (comma expression, lowest precedence)
    pub fn parse_expression(&mut self) -> ParseResult<Expr> {
        self.parse_comma_expr()
    }

    /// Parse a comma expression: expr, expr, ...
    /// Result type is the type of the rightmost expression
    fn parse_comma_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_assignment_expr()?;

        while self.is_special(b',') {
            self.advance();
            let right = self.parse_assignment_expr()?;
            // Comma expression type is the type of the rightmost expression
            let result_typ = right.typ;

            // Build comma expression
            let Expr { kind, typ, pos } = expr;
            expr = match kind {
                ExprKind::Comma(mut exprs) => {
                    exprs.push(right);
                    Expr {
                        kind: ExprKind::Comma(exprs),
                        typ: result_typ,
                        pos,
                    }
                }
                other => Expr {
                    kind: ExprKind::Comma(vec![
                        Expr {
                            kind: other,
                            typ,
                            pos,
                        },
                        right,
                    ]),
                    typ: result_typ,
                    pos,
                },
            };
        }

        Ok(expr)
    }

    /// Parse an assignment expression (right-to-left associative)
    fn parse_assignment_expr(&mut self) -> ParseResult<Expr> {
        // Parse left side (could be lvalue for assignment)
        let left = self.parse_conditional_expr()?;

        // Check for assignment operators
        let op = match self.peek_special() {
            Some(v) if v == b'=' as u32 => Some(AssignOp::Assign),
            Some(v) if v == SpecialToken::AddAssign as u32 => Some(AssignOp::AddAssign),
            Some(v) if v == SpecialToken::SubAssign as u32 => Some(AssignOp::SubAssign),
            Some(v) if v == SpecialToken::MulAssign as u32 => Some(AssignOp::MulAssign),
            Some(v) if v == SpecialToken::DivAssign as u32 => Some(AssignOp::DivAssign),
            Some(v) if v == SpecialToken::ModAssign as u32 => Some(AssignOp::ModAssign),
            Some(v) if v == SpecialToken::AndAssign as u32 => Some(AssignOp::AndAssign),
            Some(v) if v == SpecialToken::OrAssign as u32 => Some(AssignOp::OrAssign),
            Some(v) if v == SpecialToken::XorAssign as u32 => Some(AssignOp::XorAssign),
            Some(v) if v == SpecialToken::ShlAssign as u32 => Some(AssignOp::ShlAssign),
            Some(v) if v == SpecialToken::ShrAssign as u32 => Some(AssignOp::ShrAssign),
            _ => None,
        };

        if let Some(assign_op) = op {
            let assign_pos = self.current_pos();
            self.advance();

            // Check if target is const (assignment to const is an error)
            self.check_const_assignment(&left, assign_pos);

            // Right-to-left associativity: parse the right side as another assignment
            let right = self.parse_assignment_expr()?;
            // In C, assignment expression type is the type of the left operand
            let assign_type = left.typ.unwrap_or(self.types.int_id);
            Ok(Self::typed_expr(
                ExprKind::Assign {
                    op: assign_op,
                    target: Box::new(left),
                    value: Box::new(right),
                },
                assign_type,
                assign_pos,
            ))
        } else {
            Ok(left)
        }
    }

    /// Parse an initializer (C99 6.7.8)
    /// Can be either:
    /// - assignment-expression
    /// - { initializer-list }
    /// - { initializer-list , }
    fn parse_initializer(&mut self) -> ParseResult<Expr> {
        if self.is_special(b'{') {
            self.parse_initializer_list()
        } else {
            self.parse_assignment_expr()
        }
    }

    /// Parse a brace-enclosed initializer list
    fn parse_initializer_list(&mut self) -> ParseResult<Expr> {
        let list_pos = self.current_pos();
        self.expect_special(b'{')?;

        let mut elements = Vec::new();

        // Handle empty initializer list: {}
        if self.is_special(b'}') {
            self.advance();
            return Ok(Expr::new(ExprKind::InitList { elements }, list_pos));
        }

        loop {
            // Parse one initializer element (with optional designators)
            let element = self.parse_init_element()?;
            elements.push(element);

            // Check for comma or end
            if self.is_special(b',') {
                self.advance();
                // Trailing comma is allowed
                if self.is_special(b'}') {
                    break;
                }
            } else {
                break;
            }
        }

        self.expect_special(b'}')?;
        Ok(Expr::new(ExprKind::InitList { elements }, list_pos))
    }

    /// Parse a single element of an initializer list
    /// Can have designators: .field = value, [index] = value, or just value
    fn parse_init_element(&mut self) -> ParseResult<InitElement> {
        let mut designators = Vec::new();

        // Parse designator chain: .field, [index], can be chained like .x[0].y
        loop {
            if self.is_special(b'.') {
                // Field designator: .fieldname
                self.advance();
                let name = self.expect_identifier()?;
                designators.push(Designator::Field(name));
            } else if self.is_special(b'[') {
                // Array index designator: [constant-expression]
                self.advance();
                let index_expr = self.parse_conditional_expr()?;
                self.expect_special(b']')?;

                // Evaluate to constant
                let index = self.eval_const_expr(&index_expr).ok_or_else(|| {
                    ParseError::new(
                        "array designator index must be constant",
                        self.current_pos(),
                    )
                })?;
                designators.push(Designator::Index(index));
            } else {
                break;
            }
        }

        // If we had designators, expect '='
        if !designators.is_empty() {
            self.expect_special(b'=')?;
        }

        // Parse the initializer value (can be nested initializer list)
        let value = self.parse_initializer()?;

        Ok(InitElement {
            designators,
            value: Box::new(value),
        })
    }

    /// Parse a conditional (ternary) expression: cond ? then : else
    fn parse_conditional_expr(&mut self) -> ParseResult<Expr> {
        let cond = self.parse_logical_or_expr()?;

        if self.is_special(b'?') {
            self.advance();
            let then_expr = self.parse_expression()?;
            self.expect_special(b':')?;
            // Right-to-left: parse else as another conditional
            let else_expr = self.parse_conditional_expr()?;

            // The result type is the common type of then and else branches
            // For simplicity, use then_expr's type (proper impl would need type promotion)
            let typ = then_expr.typ.unwrap_or(self.types.int_id);

            let pos = cond.pos;
            Ok(Self::typed_expr(
                ExprKind::Conditional {
                    cond: Box::new(cond),
                    then_expr: Box::new(then_expr),
                    else_expr: Box::new(else_expr),
                },
                typ,
                pos,
            ))
        } else {
            Ok(cond)
        }
    }

    /// Parse logical-or: expr || expr
    fn parse_logical_or_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_logical_and_expr()?;

        while self.is_special_token(SpecialToken::LogicalOr) {
            self.advance();
            let right = self.parse_logical_and_expr()?;
            left = self.make_binary(BinaryOp::LogOr, left, right);
        }

        Ok(left)
    }

    /// Parse logical-and: expr && expr
    fn parse_logical_and_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_bitwise_or_expr()?;

        while self.is_special_token(SpecialToken::LogicalAnd) {
            self.advance();
            let right = self.parse_bitwise_or_expr()?;
            left = self.make_binary(BinaryOp::LogAnd, left, right);
        }

        Ok(left)
    }

    /// Parse bitwise-or: expr | expr
    fn parse_bitwise_or_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_bitwise_xor_expr()?;

        // | but not ||
        while self.is_special(b'|') && !self.is_special_token(SpecialToken::LogicalOr) {
            self.advance();
            let right = self.parse_bitwise_xor_expr()?;
            left = self.make_binary(BinaryOp::BitOr, left, right);
        }

        Ok(left)
    }

    /// Parse bitwise-xor: expr ^ expr
    fn parse_bitwise_xor_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_bitwise_and_expr()?;

        while self.is_special(b'^') && !self.is_special_token(SpecialToken::XorAssign) {
            self.advance();
            let right = self.parse_bitwise_and_expr()?;
            left = self.make_binary(BinaryOp::BitXor, left, right);
        }

        Ok(left)
    }

    /// Parse bitwise-and: expr & expr
    fn parse_bitwise_and_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_equality_expr()?;

        // & but not &&
        while self.is_special(b'&') && !self.is_special_token(SpecialToken::LogicalAnd) {
            self.advance();
            let right = self.parse_equality_expr()?;
            left = self.make_binary(BinaryOp::BitAnd, left, right);
        }

        Ok(left)
    }

    /// Parse equality: expr == expr, expr != expr
    fn parse_equality_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_relational_expr()?;

        loop {
            let op = if self.is_special_token(SpecialToken::Equal) {
                Some(BinaryOp::Eq)
            } else if self.is_special_token(SpecialToken::NotEqual) {
                Some(BinaryOp::Ne)
            } else {
                None
            };

            if let Some(binary_op) = op {
                self.advance();
                let right = self.parse_relational_expr()?;
                left = self.make_binary(binary_op, left, right);
            } else {
                break;
            }
        }

        Ok(left)
    }

    /// Parse relational: expr < expr, expr > expr, expr <= expr, expr >= expr
    fn parse_relational_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_shift_expr()?;

        loop {
            let op = if self.is_special_token(SpecialToken::Lte) {
                Some(BinaryOp::Le)
            } else if self.is_special_token(SpecialToken::Gte) {
                Some(BinaryOp::Ge)
            } else if self.is_special(b'<') && !self.is_special_token(SpecialToken::LeftShift) {
                Some(BinaryOp::Lt)
            } else if self.is_special(b'>') && !self.is_special_token(SpecialToken::RightShift) {
                Some(BinaryOp::Gt)
            } else {
                None
            };

            if let Some(binary_op) = op {
                self.advance();
                let right = self.parse_shift_expr()?;
                left = self.make_binary(binary_op, left, right);
            } else {
                break;
            }
        }

        Ok(left)
    }

    /// Parse shift: expr << expr, expr >> expr
    fn parse_shift_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_additive_expr()?;

        loop {
            let op = if self.is_special_token(SpecialToken::LeftShift) {
                Some(BinaryOp::Shl)
            } else if self.is_special_token(SpecialToken::RightShift) {
                Some(BinaryOp::Shr)
            } else {
                None
            };

            if let Some(binary_op) = op {
                self.advance();
                let right = self.parse_additive_expr()?;
                left = self.make_binary(binary_op, left, right);
            } else {
                break;
            }
        }

        Ok(left)
    }

    /// Parse additive: expr + expr, expr - expr
    fn parse_additive_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_multiplicative_expr()?;

        loop {
            let op = if self.is_special(b'+') && !self.is_special_token(SpecialToken::Increment) {
                Some(BinaryOp::Add)
            } else if self.is_special(b'-') && !self.is_special_token(SpecialToken::Decrement) {
                Some(BinaryOp::Sub)
            } else {
                None
            };

            if let Some(binary_op) = op {
                self.advance();
                let right = self.parse_multiplicative_expr()?;
                left = self.make_binary(binary_op, left, right);
            } else {
                break;
            }
        }

        Ok(left)
    }

    /// Parse multiplicative: expr * expr, expr / expr, expr % expr
    fn parse_multiplicative_expr(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_unary_expr()?;

        loop {
            let op = if self.is_special(b'*') && !self.is_special_token(SpecialToken::MulAssign) {
                Some(BinaryOp::Mul)
            } else if self.is_special(b'/') && !self.is_special_token(SpecialToken::DivAssign) {
                Some(BinaryOp::Div)
            } else if self.is_special(b'%') && !self.is_special_token(SpecialToken::ModAssign) {
                Some(BinaryOp::Mod)
            } else {
                None
            };

            if let Some(binary_op) = op {
                self.advance();
                let right = self.parse_unary_expr()?;
                left = self.make_binary(binary_op, left, right);
            } else {
                break;
            }
        }

        Ok(left)
    }

    /// Parse unary expression: ++x, --x, &x, *x, +x, -x, ~x, !x, sizeof
    fn parse_unary_expr(&mut self) -> ParseResult<Expr> {
        // Check for prefix operators
        if self.is_special_token(SpecialToken::Increment) {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            // Check for const modification
            self.check_const_assignment(&operand, op_pos);
            // PreInc has same type as operand
            let typ = operand.typ.unwrap_or(self.types.int_id);
            return Ok(Self::typed_expr(
                ExprKind::Unary {
                    op: UnaryOp::PreInc,
                    operand: Box::new(operand),
                },
                typ,
                op_pos,
            ));
        }

        if self.is_special_token(SpecialToken::Decrement) {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            // Check for const modification
            self.check_const_assignment(&operand, op_pos);
            // PreDec has same type as operand
            let typ = operand.typ.unwrap_or(self.types.int_id);
            return Ok(Self::typed_expr(
                ExprKind::Unary {
                    op: UnaryOp::PreDec,
                    operand: Box::new(operand),
                },
                typ,
                op_pos,
            ));
        }

        if self.is_special(b'&') && !self.is_special_token(SpecialToken::LogicalAnd) {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            // AddrOf produces pointer to operand's type
            let base_type = operand.typ.unwrap_or(self.types.int_id);
            let ptr_type = self.types.intern(Type::pointer(base_type));
            return Ok(Self::typed_expr(
                ExprKind::Unary {
                    op: UnaryOp::AddrOf,
                    operand: Box::new(operand),
                },
                ptr_type,
                op_pos,
            ));
        }

        if self.is_special(b'*') {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            // Deref produces the base type of the pointer
            let typ = operand
                .typ
                .and_then(|t| self.types.base_type(t))
                .unwrap_or(self.types.int_id);
            return Ok(Self::typed_expr(
                ExprKind::Unary {
                    op: UnaryOp::Deref,
                    operand: Box::new(operand),
                },
                typ,
                op_pos,
            ));
        }

        if self.is_special(b'+') && !self.is_special_token(SpecialToken::Increment) {
            self.advance();
            // Unary + is a no-op for numeric types, but we need to parse it
            return self.parse_unary_expr();
        }

        if self.is_special(b'-') && !self.is_special_token(SpecialToken::Decrement) {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            // Neg has same type as operand
            let typ = operand.typ.unwrap_or(self.types.int_id);
            return Ok(Self::typed_expr(
                ExprKind::Unary {
                    op: UnaryOp::Neg,
                    operand: Box::new(operand),
                },
                typ,
                op_pos,
            ));
        }

        if self.is_special(b'~') {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            // BitNot: C99 integer promotion - types smaller than int promote to int
            let op_typ = operand.typ.unwrap_or(self.types.int_id);
            // Apply integer promotion: _Bool, char, short -> int
            let typ = {
                let kind = self.types.kind(op_typ);
                if matches!(kind, TypeKind::Bool | TypeKind::Char | TypeKind::Short) {
                    self.types.int_id
                } else {
                    op_typ
                }
            };
            return Ok(Self::typed_expr(
                ExprKind::Unary {
                    op: UnaryOp::BitNot,
                    operand: Box::new(operand),
                },
                typ,
                op_pos,
            ));
        }

        if self.is_special(b'!') {
            let op_pos = self.current_pos();
            self.advance();
            let operand = self.parse_unary_expr()?;
            // Logical not always produces int (0 or 1)
            return Ok(Self::typed_expr(
                ExprKind::Unary {
                    op: UnaryOp::Not,
                    operand: Box::new(operand),
                },
                self.types.int_id,
                op_pos,
            ));
        }

        // sizeof
        if self.peek() == TokenType::Ident {
            if let Some(name) = self.get_ident_name(self.current()) {
                if name == "sizeof" {
                    self.advance();
                    return self.parse_sizeof();
                }
            }
        }

        // No unary operator, parse postfix
        self.parse_postfix_expr()
    }

    /// Parse sizeof expression
    fn parse_sizeof(&mut self) -> ParseResult<Expr> {
        let sizeof_pos = self.current_pos();
        // sizeof returns size_t, which is unsigned long in our implementation
        let size_t = self.types.ulong_id;

        if self.is_special(b'(') {
            // Could be sizeof(type) or sizeof(expr)
            // For now, try to detect if it's a type
            // This is a simplified check - full implementation needs type lookahead
            self.advance(); // consume '('

            // Try to parse as type first
            if let Some(typ) = self.try_parse_type_name() {
                self.expect_special(b')')?;
                return Ok(Expr::typed(ExprKind::SizeofType(typ), size_t, sizeof_pos));
            }

            // Not a type, parse as expression
            let expr = self.parse_expression()?;
            self.expect_special(b')')?;
            Ok(Expr::typed(
                ExprKind::SizeofExpr(Box::new(expr)),
                size_t,
                sizeof_pos,
            ))
        } else {
            // sizeof without parens - must be expression
            let expr = self.parse_unary_expr()?;
            Ok(Expr::typed(
                ExprKind::SizeofExpr(Box::new(expr)),
                size_t,
                sizeof_pos,
            ))
        }
    }

    /// Check if identifier is a type-starting keyword (for cast/sizeof disambiguation)
    fn is_type_keyword(name: &str) -> bool {
        matches!(
            name,
            "void"
                | "_Bool"
                | "_Complex"
                | "char"
                | "short"
                | "int"
                | "long"
                | "float"
                | "double"
                | "signed"
                | "unsigned"
                | "const"
                | "volatile"
                | "struct"
                | "union"
                | "enum"
                | "__builtin_va_list"
        )
    }

    /// Parse a type name (required, returns error if not a type)
    fn parse_type_name(&mut self) -> ParseResult<TypeId> {
        self.try_parse_type_name()
            .ok_or_else(|| ParseError::new("expected type name".to_string(), self.current_pos()))
    }

    /// Try to parse a type name for casts and sizeof
    /// Supports compound types like `unsigned char`, `long long`, pointers, etc.
    fn try_parse_type_name(&mut self) -> Option<TypeId> {
        if self.peek() != TokenType::Ident {
            return None;
        }

        // Check if this looks like a type name (keyword or typedef)
        let name_id = self.get_ident_id(self.current())?;
        let name = self.str(name_id);
        if !Self::is_type_keyword(name) && self.symbols.lookup_typedef(name_id).is_none() {
            // Not a type keyword and not a typedef
            return None;
        }

        // Parse type specifiers (similar to parse_type_specifier)
        let mut modifiers = TypeModifiers::empty();
        let mut base_kind: Option<TypeKind> = None;
        let mut parsed_something = false;

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
                "const" => {
                    self.advance();
                    modifiers |= TypeModifiers::CONST;
                    parsed_something = true;
                }
                "volatile" => {
                    self.advance();
                    modifiers |= TypeModifiers::VOLATILE;
                    parsed_something = true;
                }
                "signed" => {
                    self.advance();
                    modifiers |= TypeModifiers::SIGNED;
                    parsed_something = true;
                }
                "unsigned" => {
                    self.advance();
                    modifiers |= TypeModifiers::UNSIGNED;
                    parsed_something = true;
                }
                "_Complex" => {
                    self.advance();
                    modifiers |= TypeModifiers::COMPLEX;
                    parsed_something = true;
                }
                "short" => {
                    self.advance();
                    modifiers |= TypeModifiers::SHORT;
                    if base_kind.is_none() {
                        base_kind = Some(TypeKind::Short);
                    }
                    parsed_something = true;
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
                    parsed_something = true;
                }
                "void" => {
                    self.advance();
                    base_kind = Some(TypeKind::Void);
                    parsed_something = true;
                }
                "char" => {
                    self.advance();
                    base_kind = Some(TypeKind::Char);
                    parsed_something = true;
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
                    parsed_something = true;
                }
                "float" => {
                    self.advance();
                    base_kind = Some(TypeKind::Float);
                    parsed_something = true;
                }
                "double" => {
                    self.advance();
                    // Handle long double
                    if modifiers.contains(TypeModifiers::LONG) {
                        base_kind = Some(TypeKind::LongDouble);
                    } else {
                        base_kind = Some(TypeKind::Double);
                    }
                    parsed_something = true;
                }
                "_Bool" => {
                    self.advance();
                    base_kind = Some(TypeKind::Bool);
                    parsed_something = true;
                }
                "__builtin_va_list" => {
                    self.advance();
                    base_kind = Some(TypeKind::VaList);
                    parsed_something = true;
                }
                "struct" => {
                    self.advance(); // consume 'struct'
                                    // For struct tag reference, look up directly in symbol table
                    if let Some(tag_name) = self.get_ident_id(self.current()) {
                        if !self.is_special(b'{') {
                            // This is a tag reference (e.g., "struct Point*")
                            // Look up the existing tag to get its TypeId directly
                            self.advance(); // consume tag name
                            if let Some(existing) = self.symbols.lookup_tag(tag_name) {
                                let mut result_id = existing.typ;
                                // Handle pointer
                                while self.is_special(b'*') {
                                    self.advance();
                                    result_id = self.types.intern(Type::pointer(result_id));
                                }
                                // Handle array declarators
                                while self.is_special(b'[') {
                                    self.advance();
                                    if let Ok(size_expr) = self.parse_conditional_expr() {
                                        if let Some(size) = self.eval_const_expr(&size_expr) {
                                            result_id = self
                                                .types
                                                .intern(Type::array(result_id, size as usize));
                                        }
                                    }
                                    if !self.is_special(b']') {
                                        return None;
                                    }
                                    self.advance();
                                }
                                return Some(result_id);
                            }
                            // Tag not found - return incomplete struct type
                            let incomplete = Type::incomplete_struct(tag_name);
                            let mut result_id = self.types.intern(incomplete);
                            while self.is_special(b'*') {
                                self.advance();
                                result_id = self.types.intern(Type::pointer(result_id));
                            }
                            return Some(result_id);
                        }
                    }
                    // Fall back to full struct parsing for definitions
                    // (rewind position first since we consumed 'struct')
                    self.pos -= 1;
                    if let Ok(struct_type) = self.parse_struct_or_union_specifier(false) {
                        // Intern base struct type with modifiers
                        let mut typ = struct_type;
                        typ.modifiers |= modifiers;
                        let mut result_id = self.types.intern(typ);
                        // Handle pointer
                        while self.is_special(b'*') {
                            self.advance();
                            result_id = self.types.intern(Type::pointer(result_id));
                        }
                        // Handle array declarators
                        while self.is_special(b'[') {
                            self.advance();
                            if let Ok(size_expr) = self.parse_conditional_expr() {
                                if let Some(size) = self.eval_const_expr(&size_expr) {
                                    result_id =
                                        self.types.intern(Type::array(result_id, size as usize));
                                }
                            }
                            if !self.is_special(b']') {
                                return None;
                            }
                            self.advance();
                        }
                        return Some(result_id);
                    }
                    return None;
                }
                "union" => {
                    self.advance(); // consume 'union'
                                    // For union tag reference, look up directly in symbol table
                    if let Some(tag_name) = self.get_ident_id(self.current()) {
                        if !self.is_special(b'{') {
                            // This is a tag reference
                            self.advance(); // consume tag name
                            if let Some(existing) = self.symbols.lookup_tag(tag_name) {
                                let mut result_id = existing.typ;
                                while self.is_special(b'*') {
                                    self.advance();
                                    result_id = self.types.intern(Type::pointer(result_id));
                                }
                                while self.is_special(b'[') {
                                    self.advance();
                                    if let Ok(size_expr) = self.parse_conditional_expr() {
                                        if let Some(size) = self.eval_const_expr(&size_expr) {
                                            result_id = self
                                                .types
                                                .intern(Type::array(result_id, size as usize));
                                        }
                                    }
                                    if !self.is_special(b']') {
                                        return None;
                                    }
                                    self.advance();
                                }
                                return Some(result_id);
                            }
                            // Tag not found - return incomplete union type
                            let incomplete = Type::incomplete_union(tag_name);
                            let mut result_id = self.types.intern(incomplete);
                            while self.is_special(b'*') {
                                self.advance();
                                result_id = self.types.intern(Type::pointer(result_id));
                            }
                            return Some(result_id);
                        }
                    }
                    // Fall back to full union parsing for definitions
                    self.pos -= 1;
                    if let Ok(union_type) = self.parse_struct_or_union_specifier(true) {
                        let mut typ = union_type;
                        typ.modifiers |= modifiers;
                        let mut result_id = self.types.intern(typ);
                        while self.is_special(b'*') {
                            self.advance();
                            result_id = self.types.intern(Type::pointer(result_id));
                        }
                        // Handle array declarators
                        while self.is_special(b'[') {
                            self.advance();
                            if let Ok(size_expr) = self.parse_conditional_expr() {
                                if let Some(size) = self.eval_const_expr(&size_expr) {
                                    result_id =
                                        self.types.intern(Type::array(result_id, size as usize));
                                }
                            }
                            if !self.is_special(b']') {
                                return None;
                            }
                            self.advance();
                        }
                        return Some(result_id);
                    }
                    return None;
                }
                "enum" => {
                    if let Ok(enum_type) = self.parse_enum_specifier() {
                        let mut typ = enum_type;
                        typ.modifiers |= modifiers;
                        let mut result_id = self.types.intern(typ);
                        while self.is_special(b'*') {
                            self.advance();
                            result_id = self.types.intern(Type::pointer(result_id));
                        }
                        // Handle array declarators
                        while self.is_special(b'[') {
                            self.advance();
                            if let Ok(size_expr) = self.parse_conditional_expr() {
                                if let Some(size) = self.eval_const_expr(&size_expr) {
                                    result_id =
                                        self.types.intern(Type::array(result_id, size as usize));
                                }
                            }
                            if !self.is_special(b']') {
                                return None;
                            }
                            self.advance();
                        }
                        return Some(result_id);
                    }
                    return None;
                }
                _ => {
                    // Check if it's a typedef name
                    if base_kind.is_none() {
                        if let Some(typedef_type_id) = self.symbols.lookup_typedef(name_id) {
                            self.advance();
                            // For typedef, we already have a TypeId - just apply pointer/array modifiers
                            let mut result_id = typedef_type_id;
                            // Handle pointer declarators
                            while self.is_special(b'*') {
                                self.advance();
                                result_id = self.types.intern(Type::pointer(result_id));
                            }
                            // Handle array declarators
                            while self.is_special(b'[') {
                                self.advance();
                                if let Ok(size_expr) = self.parse_conditional_expr() {
                                    if let Some(size) = self.eval_const_expr(&size_expr) {
                                        result_id = self
                                            .types
                                            .intern(Type::array(result_id, size as usize));
                                    }
                                }
                                if !self.is_special(b']') {
                                    return None;
                                }
                                self.advance();
                            }
                            return Some(result_id);
                        }
                    }
                    break;
                }
            }
        }

        if !parsed_something {
            return None;
        }

        // If we only have modifiers like `unsigned` without a base type, default to int
        let kind = base_kind.unwrap_or(TypeKind::Int);
        let typ = Type::with_modifiers(kind, modifiers);
        let mut result_id = self.types.intern(typ);

        // Handle pointer declarators
        while self.is_special(b'*') {
            self.advance();
            result_id = self.types.intern(Type::pointer(result_id));
        }

        // Handle function pointer declarators: void (*)(int), int (*)(void), etc.
        // Syntax: return_type (*)(param_types)
        if self.is_special(b'(') {
            // Look ahead to check for (*) pattern indicating function pointer
            let saved_pos = self.pos;
            self.advance(); // consume '('
            if self.is_special(b'*') {
                self.advance(); // consume '*'
                if self.is_special(b')') {
                    self.advance(); // consume ')'
                                    // Now expect parameter list
                    if self.is_special(b'(') {
                        self.advance(); // consume '('
                                        // Parse parameter types (simplified - just skip them for now)
                                        // Full implementation would build proper function type
                        let mut param_depth = 1;
                        while param_depth > 0 && !self.is_eof() {
                            if self.is_special(b'(') {
                                param_depth += 1;
                            } else if self.is_special(b')') {
                                param_depth -= 1;
                            }
                            if param_depth > 0 {
                                self.advance();
                            }
                        }
                        self.advance(); // consume final ')'
                                        // Create function pointer type: pointer to function returning result_id
                                        // For now, create a generic function pointer (void -> result_id)
                        let fn_type = Type::function(result_id, vec![], false);
                        let fn_type_id = self.types.intern(fn_type);
                        result_id = self.types.intern(Type::pointer(fn_type_id));
                        return Some(result_id);
                    }
                }
            }
            // Not a function pointer, backtrack
            self.pos = saved_pos;
        }

        // Handle array declarators: int[10], char[20], int[], etc.
        while self.is_special(b'[') {
            self.advance();
            // Check for empty brackets [] (incomplete array type for compound literals)
            if self.is_special(b']') {
                // Empty brackets - create array with size 0 (size to be determined from initializer)
                result_id = self.types.intern(Type::array(result_id, 0));
            } else if let Ok(size_expr) = self.parse_conditional_expr() {
                if let Some(size) = self.eval_const_expr(&size_expr) {
                    result_id = self.types.intern(Type::array(result_id, size as usize));
                } else {
                    // Non-constant size (VLA in type name) - create array with size 0
                    result_id = self.types.intern(Type::array(result_id, 0));
                }
            }
            if !self.is_special(b']') {
                return None;
            }
            self.advance();
        }

        Some(result_id)
    }

    /// Parse postfix expression: x++, x--, x[i], x.member, x->member, x(args)
    fn parse_postfix_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_primary_expr()?;

        loop {
            // Preserve the position of the base expression for all postfix ops
            let base_pos = expr.pos;

            if self.is_special_token(SpecialToken::Increment) {
                let op_pos = self.current_pos();
                self.advance();
                // Check for const modification
                self.check_const_assignment(&expr, op_pos);
                // PostInc has same type as operand
                let typ = expr.typ.unwrap_or(self.types.int_id);
                expr = Self::typed_expr(ExprKind::PostInc(Box::new(expr)), typ, base_pos);
            } else if self.is_special_token(SpecialToken::Decrement) {
                let op_pos = self.current_pos();
                self.advance();
                // Check for const modification
                self.check_const_assignment(&expr, op_pos);
                // PostDec has same type as operand
                let typ = expr.typ.unwrap_or(self.types.int_id);
                expr = Self::typed_expr(ExprKind::PostDec(Box::new(expr)), typ, base_pos);
            } else if self.is_special(b'[') {
                // Array subscript
                self.advance();
                let index = self.parse_expression()?;
                self.expect_special(b']')?;
                // Get element type from array/pointer type
                let elem_type = expr
                    .typ
                    .and_then(|t| self.types.base_type(t))
                    .unwrap_or(self.types.int_id);
                expr = Self::typed_expr(
                    ExprKind::Index {
                        array: Box::new(expr),
                        index: Box::new(index),
                    },
                    elem_type,
                    base_pos,
                );
            } else if self.is_special(b'.') {
                // Member access
                self.advance();
                let member = self.expect_identifier()?;
                // Get member type from struct type
                let member_type = expr
                    .typ
                    .and_then(|t| self.types.find_member(t, member))
                    .map(|info| info.typ)
                    .unwrap_or(self.types.int_id);
                expr = Self::typed_expr(
                    ExprKind::Member {
                        expr: Box::new(expr),
                        member,
                    },
                    member_type,
                    base_pos,
                );
            } else if self.is_special_token(SpecialToken::Arrow) {
                // Pointer member access
                self.advance();
                let member = self.expect_identifier()?;
                // Get member type: dereference pointer to get struct, then find member
                let member_type = expr
                    .typ
                    .and_then(|t| self.types.base_type(t))
                    .and_then(|struct_type| self.types.find_member(struct_type, member))
                    .map(|info| info.typ)
                    .unwrap_or(self.types.int_id);
                expr = Self::typed_expr(
                    ExprKind::Arrow {
                        expr: Box::new(expr),
                        member,
                    },
                    member_type,
                    base_pos,
                );
            } else if self.is_special(b'(') {
                // Function call
                self.advance();
                let args = self.parse_argument_list()?;
                self.expect_special(b')')?;

                // Get the return type from the function type
                // The func expression should have type TypeKind::Function
                // and the return type is stored in base
                let return_type = expr
                    .typ
                    .and_then(|t| {
                        if self.types.kind(t) == TypeKind::Function {
                            self.types.base_type(t)
                        } else {
                            None
                        }
                    })
                    .unwrap_or(self.types.int_id); // Default to int

                expr = Self::typed_expr(
                    ExprKind::Call {
                        func: Box::new(expr),
                        args,
                    },
                    return_type,
                    base_pos,
                );
            } else {
                break;
            }
        }

        Ok(expr)
    }

    /// Parse function argument list
    fn parse_argument_list(&mut self) -> ParseResult<Vec<Expr>> {
        let mut args = Vec::new();

        if self.is_special(b')') {
            return Ok(args);
        }

        loop {
            // Parse assignment expression (not comma, as comma separates args)
            args.push(self.parse_assignment_expr()?);

            if self.is_special(b',') {
                self.advance();
            } else {
                break;
            }
        }

        Ok(args)
    }

    /// Expect and consume an identifier, returning its StringId
    fn expect_identifier(&mut self) -> ParseResult<StringId> {
        if self.peek() != TokenType::Ident {
            return Err(ParseError::new("expected identifier", self.current_pos()));
        }

        let id = self
            .get_ident_id(self.current())
            .ok_or_else(|| ParseError::new("invalid identifier", self.current_pos()))?;

        self.advance();
        Ok(id)
    }

    /// Check if an expression is const and report error if assigning to it
    fn check_const_assignment(&self, target: &Expr, pos: Position) {
        // Check for assignment through pointer to const first: *p where p is const T*
        if let ExprKind::Unary {
            op: UnaryOp::Deref,
            operand,
        } = &target.kind
        {
            if let Some(ptr_type_id) = operand.typ {
                if let Some(base_type_id) = self.types.base_type(ptr_type_id) {
                    if self
                        .types
                        .modifiers(base_type_id)
                        .contains(TypeModifiers::CONST)
                    {
                        diag::error(pos, "assignment of read-only location");
                        return; // Don't duplicate with the general const check
                    }
                }
            }
        }

        // Check if target type has CONST modifier (direct const variable)
        if let Some(typ_id) = target.typ {
            if self.types.modifiers(typ_id).contains(TypeModifiers::CONST) {
                // Get variable name if it's an identifier
                let var_name = match &target.kind {
                    ExprKind::Ident { name } => format!(" '{}'", name),
                    _ => String::new(),
                };
                diag::error(
                    pos,
                    &format!("assignment of read-only variable{}", var_name),
                );
            }
        }
    }

    /// Parse primary expression: literals, identifiers, parenthesized expressions
    /// Create a typed expression with position
    fn typed_expr(kind: ExprKind, typ: TypeId, pos: Position) -> Expr {
        Expr {
            kind,
            typ: Some(typ),
            pos,
        }
    }

    /// Create a typed binary expression, computing result type from operands
    fn make_binary(&mut self, op: BinaryOp, left: Expr, right: Expr) -> Expr {
        // Compute result type based on operator and operand types
        let left_type = left.typ.unwrap_or(self.types.int_id);
        let right_type = right.typ.unwrap_or(self.types.int_id);

        let result_type = match op {
            // Comparison and logical operators always return int
            BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::Le
            | BinaryOp::Ge
            | BinaryOp::LogAnd
            | BinaryOp::LogOr => self.types.int_id,

            // Arithmetic operators use usual arithmetic conversions
            // But Add/Sub with pointers/arrays need special handling
            BinaryOp::Add | BinaryOp::Sub => {
                let left_kind = self.types.kind(left_type);
                let right_kind = self.types.kind(right_type);
                let left_is_ptr_or_arr =
                    left_kind == TypeKind::Pointer || left_kind == TypeKind::Array;
                let right_is_ptr_or_arr =
                    right_kind == TypeKind::Pointer || right_kind == TypeKind::Array;

                if left_is_ptr_or_arr && self.types.is_integer(right_type) {
                    // ptr + int or arr + int -> pointer to element type
                    if left_kind == TypeKind::Array {
                        // Array decays to pointer
                        let elem_type =
                            self.types.base_type(left_type).unwrap_or(self.types.int_id);
                        self.types.intern(Type::pointer(elem_type))
                    } else {
                        left_type
                    }
                } else if self.types.is_integer(left_type)
                    && right_is_ptr_or_arr
                    && op == BinaryOp::Add
                {
                    // int + ptr or int + arr -> pointer to element type
                    if right_kind == TypeKind::Array {
                        let elem_type = self
                            .types
                            .base_type(right_type)
                            .unwrap_or(self.types.int_id);
                        self.types.intern(Type::pointer(elem_type))
                    } else {
                        right_type
                    }
                } else if left_is_ptr_or_arr && right_is_ptr_or_arr && op == BinaryOp::Sub {
                    // ptr - ptr -> ptrdiff_t (long)
                    self.types.long_id
                } else {
                    self.usual_arithmetic_conversions(left_type, right_type)
                }
            }
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                self.usual_arithmetic_conversions(left_type, right_type)
            }

            // Bitwise and shift operators use integer promotions
            BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::Shl
            | BinaryOp::Shr => self.usual_arithmetic_conversions(left_type, right_type),
        };

        let pos = left.pos;
        Self::typed_expr(
            ExprKind::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            },
            result_type,
            pos,
        )
    }

    /// Compute usual arithmetic conversions (C99 6.3.1.8)
    fn usual_arithmetic_conversions(&mut self, left: TypeId, right: TypeId) -> TypeId {
        // C99 6.3.1.8: Usual arithmetic conversions
        // For complex types: if either operand is complex, result is complex
        // The underlying type follows the same rules as real types

        let left_kind = self.types.kind(left);
        let right_kind = self.types.kind(right);
        let left_complex = self.types.is_complex(left);
        let right_complex = self.types.is_complex(right);
        let is_complex = left_complex || right_complex;

        // Determine the underlying floating-point type
        // 1. If either is long double, result is long double
        // 2. If either is double, result is double
        // 3. If either is float, result is float
        // 4. Otherwise, integer promotions apply

        if left_kind == TypeKind::LongDouble || right_kind == TypeKind::LongDouble {
            if is_complex {
                self.types.complex_longdouble_id
            } else {
                self.types.longdouble_id
            }
        } else if left_kind == TypeKind::Double || right_kind == TypeKind::Double {
            if is_complex {
                self.types.complex_double_id
            } else {
                self.types.double_id
            }
        } else if left_kind == TypeKind::Float || right_kind == TypeKind::Float {
            if is_complex {
                self.types.complex_float_id
            } else {
                self.types.float_id
            }
        } else if left_kind == TypeKind::LongLong || right_kind == TypeKind::LongLong {
            // If either is unsigned long long, result is unsigned long long
            if self.types.is_unsigned(left) || self.types.is_unsigned(right) {
                self.types.ulonglong_id
            } else {
                self.types.longlong_id
            }
        } else if left_kind == TypeKind::Long || right_kind == TypeKind::Long {
            // If either is unsigned long, result is unsigned long
            if self.types.is_unsigned(left) || self.types.is_unsigned(right) {
                self.types.ulong_id
            } else {
                self.types.long_id
            }
        } else if self.types.is_unsigned(left) || self.types.is_unsigned(right) {
            self.types.uint_id
        } else {
            self.types.int_id
        }
    }

    fn parse_primary_expr(&mut self) -> ParseResult<Expr> {
        match self.peek() {
            TokenType::Number => {
                let token = self.consume();
                if let TokenValue::Number(s) = &token.value {
                    // Parse the number literal (returns typed expression)
                    self.parse_number_literal(s, token.pos)
                } else {
                    Err(ParseError::new("invalid number token", token.pos))
                }
            }

            TokenType::Ident => {
                let token = self.consume();
                let token_pos = token.pos;
                if let TokenValue::Ident(id) = &token.value {
                    let name_id = *id;
                    let name_str = self.idents.get_opt(name_id).unwrap_or("");

                    // Check for varargs builtins that need special parsing
                    match name_str {
                        "__builtin_va_start" => {
                            // __builtin_va_start(ap, last_param)
                            self.expect_special(b'(')?;
                            let ap = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            // Second arg is a parameter name
                            let last_param = self.expect_identifier()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::VaStart {
                                    ap: Box::new(ap),
                                    last_param,
                                },
                                self.types.void_id,
                                token_pos,
                            ));
                        }
                        "__builtin_va_arg" => {
                            // __builtin_va_arg(ap, type)
                            self.expect_special(b'(')?;
                            let ap = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            // Second arg is a type
                            let arg_type = self.parse_type_name()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::VaArg {
                                    ap: Box::new(ap),
                                    arg_type,
                                },
                                arg_type,
                                token_pos,
                            ));
                        }
                        "__builtin_va_end" => {
                            // __builtin_va_end(ap)
                            self.expect_special(b'(')?;
                            let ap = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::VaEnd { ap: Box::new(ap) },
                                self.types.void_id,
                                token_pos,
                            ));
                        }
                        "__builtin_va_copy" => {
                            // __builtin_va_copy(dest, src)
                            self.expect_special(b'(')?;
                            let dest = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let src = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::VaCopy {
                                    dest: Box::new(dest),
                                    src: Box::new(src),
                                },
                                self.types.void_id,
                                token_pos,
                            ));
                        }
                        "__builtin_bswap16" => {
                            // __builtin_bswap16(x) - returns uint16_t
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Bswap16 { arg: Box::new(arg) },
                                self.types.ushort_id,
                                token_pos,
                            ));
                        }
                        "__builtin_bswap32" => {
                            // __builtin_bswap32(x) - returns uint32_t
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Bswap32 { arg: Box::new(arg) },
                                self.types.uint_id,
                                token_pos,
                            ));
                        }
                        "__builtin_bswap64" => {
                            // __builtin_bswap64(x) - returns uint64_t
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Bswap64 { arg: Box::new(arg) },
                                self.types.ulonglong_id,
                                token_pos,
                            ));
                        }
                        "__builtin_ctz" => {
                            // __builtin_ctz(x) - returns int, counts trailing zeros in unsigned int
                            // Result is undefined if x is 0
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Ctz { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_ctzl" => {
                            // __builtin_ctzl(x) - returns int, counts trailing zeros in unsigned long
                            // Result is undefined if x is 0
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Ctzl { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_ctzll" => {
                            // __builtin_ctzll(x) - returns int, counts trailing zeros in unsigned long long
                            // Result is undefined if x is 0
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Ctzll { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_clz" => {
                            // __builtin_clz(x) - returns int, counts leading zeros in unsigned int
                            // Result is undefined if x is 0
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Clz { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_clzl" => {
                            // __builtin_clzl(x) - returns int, counts leading zeros in unsigned long
                            // Result is undefined if x is 0
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Clzl { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_clzll" => {
                            // __builtin_clzll(x) - returns int, counts leading zeros in unsigned long long
                            // Result is undefined if x is 0
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Clzll { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_popcount" => {
                            // __builtin_popcount(x) - returns int, counts set bits in unsigned int
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Popcount { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_popcountl" => {
                            // __builtin_popcountl(x) - returns int, counts set bits in unsigned long
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Popcountl { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_popcountll" => {
                            // __builtin_popcountll(x) - returns int, counts set bits in unsigned long long
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Popcountll { arg: Box::new(arg) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_alloca" => {
                            // __builtin_alloca(size) - returns void*
                            self.expect_special(b'(')?;
                            let size = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Alloca {
                                    size: Box::new(size),
                                },
                                self.types.void_ptr_id,
                                token_pos,
                            ));
                        }
                        "__builtin_unreachable" => {
                            // __builtin_unreachable() - marks code as unreachable
                            // Takes no arguments, returns void
                            // Behavior is undefined if actually reached at runtime
                            self.expect_special(b'(')?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Unreachable,
                                self.types.void_id,
                                token_pos,
                            ));
                        }
                        "__builtin_constant_p" => {
                            // __builtin_constant_p(expr) - returns 1 if expr is a constant, 0 otherwise
                            // This is evaluated at compile time, not runtime
                            self.expect_special(b'(')?;
                            let arg = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            // Check if the argument is a constant expression
                            let is_constant = self.eval_const_expr(&arg).is_some();
                            return Ok(Self::typed_expr(
                                ExprKind::IntLit(if is_constant { 1 } else { 0 }),
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "__builtin_types_compatible_p" => {
                            // __builtin_types_compatible_p(type1, type2) - returns 1 if types are compatible
                            // This is evaluated at compile time, ignoring top-level qualifiers
                            self.expect_special(b'(')?;
                            let type1 = self.parse_type_name()?;
                            self.expect_special(b',')?;
                            let type2 = self.parse_type_name()?;
                            self.expect_special(b')')?;
                            // Check type compatibility (ignoring qualifiers)
                            let compatible = self.types.types_compatible(type1, type2);
                            return Ok(Self::typed_expr(
                                ExprKind::IntLit(if compatible { 1 } else { 0 }),
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "setjmp" | "_setjmp" => {
                            // setjmp(env) - saves execution context, returns int
                            // Returns 0 on direct call, non-zero when returning via longjmp
                            self.expect_special(b'(')?;
                            let env = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Setjmp { env: Box::new(env) },
                                self.types.int_id,
                                token_pos,
                            ));
                        }
                        "longjmp" | "_longjmp" => {
                            // longjmp(env, val) - restores execution context (never returns)
                            // Causes corresponding setjmp to return val (or 1 if val == 0)
                            self.expect_special(b'(')?;
                            let env = self.parse_assignment_expr()?;
                            self.expect_special(b',')?;
                            let val = self.parse_assignment_expr()?;
                            self.expect_special(b')')?;
                            return Ok(Self::typed_expr(
                                ExprKind::Longjmp {
                                    env: Box::new(env),
                                    val: Box::new(val),
                                },
                                self.types.void_id,
                                token_pos,
                            ));
                        }
                        _ => {}
                    }

                    // Look up symbol to get type (during parsing, symbol is in scope)
                    // C99 6.4.2.2: __func__ is a predefined identifier with type const char[]
                    if name_str == "__func__" {
                        // __func__ behaves like a string literal (const char[])
                        return Ok(Self::typed_expr(
                            ExprKind::Ident { name: name_id },
                            self.types.char_ptr_id,
                            token_pos,
                        ));
                    }

                    // Check if this is an enum constant - if so, return IntLit
                    if let Some(sym) = self.symbols.lookup(name_id, Namespace::Ordinary) {
                        if sym.is_enum_constant() {
                            if let Some(value) = sym.enum_value {
                                return Ok(Self::typed_expr(
                                    ExprKind::IntLit(value),
                                    self.types.int_id,
                                    token_pos,
                                ));
                            }
                        }
                        // Regular variable/function
                        Ok(Self::typed_expr(
                            ExprKind::Ident { name: name_id },
                            sym.typ,
                            token_pos,
                        ))
                    } else {
                        // Unknown identifier - default to int
                        Ok(Self::typed_expr(
                            ExprKind::Ident { name: name_id },
                            self.types.int_id,
                            token_pos,
                        ))
                    }
                } else {
                    Err(ParseError::new("invalid identifier token", token.pos))
                }
            }

            TokenType::Char => {
                let token = self.consume();
                let token_pos = token.pos;
                if let TokenValue::Char(s) = &token.value {
                    // Parse character literal - type is int (C promotes char to int)
                    let c = self.parse_char_literal(s);
                    Ok(Self::typed_expr(
                        ExprKind::CharLit(c),
                        self.types.int_id,
                        token_pos,
                    ))
                } else {
                    Err(ParseError::new("invalid char token", token.pos))
                }
            }

            TokenType::String => {
                let token = self.consume();
                let token_pos = token.pos;
                if let TokenValue::String(s) = &token.value {
                    // Parse string literal - convert escape sequences
                    let parsed = Self::parse_string_literal(s);
                    // String literal type is char*
                    Ok(Self::typed_expr(
                        ExprKind::StringLit(parsed),
                        self.types.char_ptr_id,
                        token_pos,
                    ))
                } else {
                    Err(ParseError::new("invalid string token", token.pos))
                }
            }

            TokenType::Special => {
                if self.is_special(b'(') {
                    // Parenthesized expression or cast
                    let paren_pos = self.current_pos();
                    self.advance();

                    // Try to detect cast (type) or compound literal (type){...}
                    if let Some(typ) = self.try_parse_type_name() {
                        self.expect_special(b')')?;

                        // Check for compound literal: (type){ ... }
                        if self.is_special(b'{') {
                            let init_list = self.parse_initializer_list()?;
                            let elements = match init_list.kind {
                                ExprKind::InitList { elements } => elements,
                                _ => unreachable!(),
                            };

                            // For incomplete array types (size 0), determine size from initializer
                            let final_typ = if self.types.kind(typ) == TypeKind::Array
                                && self.types.get(typ).array_size == Some(0)
                            {
                                // Array size should be determined from initializer element count
                                let elem_type =
                                    self.types.base_type(typ).unwrap_or(self.types.int_id);
                                self.types.intern(Type::array(elem_type, elements.len()))
                            } else {
                                typ
                            };

                            return Ok(Self::typed_expr(
                                ExprKind::CompoundLiteral {
                                    typ: final_typ,
                                    elements,
                                },
                                final_typ,
                                paren_pos,
                            ));
                        }

                        // Regular cast expression
                        let expr = self.parse_unary_expr()?;
                        return Ok(Self::typed_expr(
                            ExprKind::Cast {
                                cast_type: typ,
                                expr: Box::new(expr),
                            },
                            typ,
                            paren_pos,
                        ));
                    }

                    // Regular parenthesized expression
                    let expr = self.parse_expression()?;
                    self.expect_special(b')')?;
                    Ok(expr)
                } else {
                    Err(ParseError::new(
                        "unexpected token in expression".to_string(),
                        self.current_pos(),
                    ))
                }
            }

            _ => Err(ParseError::new(
                format!("unexpected token {:?}", self.peek()),
                self.current_pos(),
            )),
        }
    }

    /// Parse a number literal string into an expression
    fn parse_number_literal(&self, s: &str, pos: Position) -> ParseResult<Expr> {
        let s_lower = s.to_lowercase();

        // Check if it's a hex number (must check before suffix trimming)
        let is_hex = s_lower.starts_with("0x");

        // Check if it's a floating point number
        let is_float = s_lower.contains('.')
            || (s_lower.contains('e') && !is_hex)
            || (s_lower.contains('p') && is_hex);

        // Remove suffixes - but for hex numbers, don't strip a-f as they're digits
        let num_str = if is_hex {
            // For hex, only strip u/l suffixes (not f which is a hex digit)
            s_lower.trim_end_matches(['u', 'l'])
        } else {
            // For decimal/octal, strip u/l/f suffixes
            s_lower.trim_end_matches(['u', 'l', 'f'])
        };

        if is_float {
            // Float - type is double by default, float if 'f' suffix, long double if 'l' suffix
            // C99: floating-suffix is f, F, l, or L
            let is_float_suffix = s_lower.ends_with('f');
            let is_longdouble_suffix = s_lower.ends_with('l');
            let value: f64 = if is_hex {
                // Hex float parsing: 0x[hex-digits].[hex-digits]p[±exponent]
                // Value = significand × 2^exponent
                Self::parse_hex_float(num_str).map_err(|_| {
                    ParseError::new(format!("invalid hex float literal: {}", s), pos)
                })?
            } else {
                num_str
                    .parse()
                    .map_err(|_| ParseError::new(format!("invalid float literal: {}", s), pos))?
            };
            let typ = if is_float_suffix {
                self.types.float_id
            } else if is_longdouble_suffix {
                self.types.longdouble_id
            } else {
                self.types.double_id
            };
            Ok(Self::typed_expr(ExprKind::FloatLit(value), typ, pos))
        } else {
            // Integer - determine type from suffix
            // Check for long long first (ll, ull, llu) before checking for long (l, ul, lu)
            let is_longlong =
                s_lower.ends_with("ll") || s_lower.ends_with("ull") || s_lower.ends_with("llu");
            let is_long = !is_longlong
                && (s_lower.ends_with('l') || s_lower.ends_with("ul") || s_lower.ends_with("lu"));
            let is_unsigned = s_lower.contains('u');

            // Parse as u64 first to handle large unsigned values, then reinterpret as i64
            let value_u64: u64 = if is_hex {
                // Strip 0x or 0X prefix
                let hex_part = num_str
                    .strip_prefix("0x")
                    .or_else(|| num_str.strip_prefix("0X"))
                    .unwrap_or(num_str);
                u64::from_str_radix(hex_part, 16)
            } else if let Some(bin_part) = num_str.strip_prefix("0b") {
                u64::from_str_radix(bin_part, 2)
            } else if num_str.starts_with('0') && num_str.len() > 1 {
                u64::from_str_radix(num_str, 8)
            } else {
                num_str.parse()
            }
            .map_err(|_| ParseError::new(format!("invalid integer literal: {}", s), pos))?;

            // Reinterpret bits as i64 (preserves bit pattern for unsigned values)
            let value = value_u64 as i64;

            let typ = match (is_longlong, is_long, is_unsigned) {
                (true, _, false) => self.types.longlong_id,
                (true, _, true) => self.types.ulonglong_id,
                (false, true, false) => self.types.long_id,
                (false, true, true) => self.types.ulong_id,
                (false, false, false) => self.types.int_id,
                (false, false, true) => self.types.uint_id,
            };
            Ok(Self::typed_expr(ExprKind::IntLit(value), typ, pos))
        }
    }

    /// Parse a hexadecimal floating-point literal (C99 feature)
    /// Format: 0x[hex-mantissa]p[±exponent] where mantissa can have decimal point
    /// Value = significand × 2^exponent
    fn parse_hex_float(s: &str) -> Result<f64, ()> {
        // Strip 0x prefix
        let s = s
            .strip_prefix("0x")
            .or_else(|| s.strip_prefix("0X"))
            .ok_or(())?;

        // Find 'p' or 'P' separator for exponent
        let p_pos = s.find(['p', 'P']).ok_or(())?;
        let (mantissa_str, exp_str) = s.split_at(p_pos);
        let exp_str = &exp_str[1..]; // Skip the 'p'/'P'

        // Parse mantissa (may have decimal point)
        let (int_part, frac_part) = if let Some(dot_pos) = mantissa_str.find('.') {
            (&mantissa_str[..dot_pos], &mantissa_str[dot_pos + 1..])
        } else {
            (mantissa_str, "")
        };

        // Convert hex mantissa to f64
        let int_val = if int_part.is_empty() {
            0u64
        } else {
            u64::from_str_radix(int_part, 16).map_err(|_| ())?
        };

        let mut mantissa = int_val as f64;

        // Add fractional part: each hex digit after dot is worth 1/16, 1/256, etc.
        if !frac_part.is_empty() {
            let frac_val = u64::from_str_radix(frac_part, 16).map_err(|_| ())?;
            let frac_bits = frac_part.len() * 4; // 4 bits per hex digit
            mantissa += frac_val as f64 / (1u64 << frac_bits) as f64;
        }

        // Parse exponent (base 10 number representing power of 2)
        let exponent: i32 = exp_str.parse().map_err(|_| ())?;

        // Calculate final value: mantissa × 2^exponent
        Ok(mantissa * (2.0_f64).powi(exponent))
    }

    /// Parse an escape sequence starting at position i (after the backslash).
    /// Returns (unescaped_char, number_of_chars_consumed_after_backslash).
    fn parse_escape_sequence(chars: &[char], i: usize) -> (char, usize) {
        if i >= chars.len() {
            return ('\\', 0);
        }

        match chars[i] {
            'n' => ('\n', 1),
            't' => ('\t', 1),
            'r' => ('\r', 1),
            '\\' => ('\\', 1),
            '\'' => ('\'', 1),
            '"' => ('"', 1),
            'a' => ('\x07', 1), // bell
            'b' => ('\x08', 1), // backspace
            'f' => ('\x0C', 1), // form feed
            'v' => ('\x0B', 1), // vertical tab
            'x' => {
                // Hex escape \xHH - consume all hex digits
                let mut hex_chars = 0;
                while i + 1 + hex_chars < chars.len()
                    && chars[i + 1 + hex_chars].is_ascii_hexdigit()
                {
                    hex_chars += 1;
                }
                if hex_chars > 0 {
                    let hex: String = chars[i + 1..i + 1 + hex_chars].iter().collect();
                    // C allows arbitrary-length hex escapes, but only low 8 bits matter
                    let val = u64::from_str_radix(&hex, 16).unwrap_or(0) as u8;
                    (val as char, 1 + hex_chars)
                } else {
                    ('x', 1) // \x with no hex digits - just 'x'
                }
            }
            'u' => {
                // UCN \uXXXX - exactly 4 hex digits (C99 6.4.3)
                if i + 4 < chars.len() && chars[i + 1..i + 5].iter().all(|c| c.is_ascii_hexdigit())
                {
                    let hex: String = chars[i + 1..i + 5].iter().collect();
                    let val = u32::from_str_radix(&hex, 16).unwrap_or(0);
                    if let Some(c) = char::from_u32(val) {
                        (c, 5)
                    } else {
                        ('u', 1) // Invalid code point
                    }
                } else {
                    ('u', 1) // Not enough hex digits
                }
            }
            'U' => {
                // UCN \UXXXXXXXX - exactly 8 hex digits (C99 6.4.3)
                if i + 8 < chars.len() && chars[i + 1..i + 9].iter().all(|c| c.is_ascii_hexdigit())
                {
                    let hex: String = chars[i + 1..i + 9].iter().collect();
                    let val = u32::from_str_radix(&hex, 16).unwrap_or(0);
                    if let Some(c) = char::from_u32(val) {
                        (c, 9)
                    } else {
                        ('U', 1) // Invalid code point
                    }
                } else {
                    ('U', 1) // Not enough hex digits
                }
            }
            c if c.is_ascii_digit() && c != '8' && c != '9' => {
                // Octal escape \NNN (up to 3 digits)
                let mut oct_chars = 1;
                while oct_chars < 3
                    && i + oct_chars < chars.len()
                    && chars[i + oct_chars].is_ascii_digit()
                    && chars[i + oct_chars] != '8'
                    && chars[i + oct_chars] != '9'
                {
                    oct_chars += 1;
                }
                let oct: String = chars[i..i + oct_chars].iter().collect();
                let val = u8::from_str_radix(&oct, 8).unwrap_or(0);
                (val as char, oct_chars)
            }
            c => (c, 1), // Unknown escape - just return the character
        }
    }

    /// Parse a character literal string into a char
    fn parse_char_literal(&self, s: &str) -> char {
        if s.is_empty() {
            return '\0';
        }

        let chars: Vec<char> = s.chars().collect();
        if chars[0] == '\\' && chars.len() > 1 {
            let (c, _) = Self::parse_escape_sequence(&chars, 1);
            c
        } else {
            chars[0]
        }
    }

    /// Parse a string literal, converting escape sequences to their actual values.
    /// This implements C99 translation phase 5 for string literals.
    fn parse_string_literal(s: &str) -> String {
        let chars: Vec<char> = s.chars().collect();
        let mut result = String::new();
        let mut i = 0;

        while i < chars.len() {
            if chars[i] == '\\' && i + 1 < chars.len() {
                let (c, consumed) = Self::parse_escape_sequence(&chars, i + 1);
                result.push(c);
                i += 1 + consumed;
            } else {
                result.push(chars[i]);
                i += 1;
            }
        }

        result
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
                    // GCC extended inline assembly - skip for now
                    "__asm__" | "__asm" | "asm" => {
                        self.skip_asm_statement();
                        return Ok(Stmt::Empty);
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
            let decl = self.parse_declaration_and_bind()?;
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
    /// Like sparse, blocks create their own scope for local declarations.
    /// This enters a new scope, parses the block, binds any declarations,
    /// then leaves the scope.
    fn parse_block_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_special(b'{')?;

        // Enter block scope
        self.symbols.enter_scope();

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

        self.expect_special(b'}')?;
        Ok(Stmt::Block(items))
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
                    | "_Complex"
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
                    | "_Noreturn"
                    | "__noreturn__"
                    | "struct"
                    | "union"
                    | "enum"
                    | "_Bool"
                    | "__attribute__"
                    | "__attribute"
                    | "__builtin_va_list"
            ) {
                return true;
            }
            // Also check for typedef names
            self.symbols.lookup_typedef(name_id).is_some()
        } else {
            false
        }
    }

    /// Parse a declaration (without binding to symbol table)
    ///
    /// Used for testing declarations in isolation.
    #[cfg(test)]
    fn parse_declaration(&mut self) -> ParseResult<Declaration> {
        // Parse type specifiers
        let base_type = self.parse_type_specifier()?;
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

                declarators.push(InitDeclarator {
                    name,
                    typ,
                    init,
                    vla_sizes,
                });

                if self.is_special(b',') {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        self.expect_special(b';')?;

        Ok(Declaration { declarators })
    }

    /// Parse a declaration and bind variables to symbol table
    ///
    /// Following sparse's design, this binds each declared variable to the
    /// symbol table immediately during parsing. Like sparse's bind_symbol().
    fn parse_declaration_and_bind(&mut self) -> ParseResult<Declaration> {
        // Parse type specifiers
        let base_type = self.parse_type_specifier()?;
        // Check modifiers from the specifier before interning (storage class is not part of type)
        let is_typedef = base_type.modifiers.contains(TypeModifiers::TYPEDEF);
        // Intern the base type (without storage class specifiers for the actual type)
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

                // Bind to symbol table (like sparse's bind_symbol)
                // Note: StringId is Copy, check for empty by comparing to empty string
                let name_str = self.str(name);
                if !name_str.is_empty() {
                    if is_typedef {
                        // For typedef, the type being aliased is the declarator type
                        let sym = Symbol::typedef(name, typ, self.symbols.depth());
                        let _ = self.symbols.declare(sym);
                    } else {
                        let sym = Symbol::variable(name, typ, self.symbols.depth());
                        let _ = self.symbols.declare(sym);
                    }
                }

                declarators.push(InitDeclarator {
                    name,
                    typ,
                    init,
                    vla_sizes,
                });

                if self.is_special(b',') {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        self.expect_special(b';')?;

        Ok(Declaration { declarators })
    }

    /// Parse a type specifier
    fn parse_type_specifier(&mut self) -> ParseResult<Type> {
        let mut modifiers = TypeModifiers::empty();
        let mut base_kind: Option<TypeKind> = None;

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
                "inline" => {
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
                        if base_kind.is_none() {
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
                "enum" => {
                    let enum_type = self.parse_enum_specifier()?;
                    // Apply any modifiers we collected
                    return Ok(Type {
                        modifiers,
                        ..enum_type
                    });
                }
                "struct" => {
                    let struct_type = self.parse_struct_or_union_specifier(false)?;
                    return Ok(Type {
                        modifiers,
                        ..struct_type
                    });
                }
                "union" => {
                    let union_type = self.parse_struct_or_union_specifier(true)?;
                    return Ok(Type {
                        modifiers,
                        ..union_type
                    });
                }
                _ => {
                    // Check if it's a typedef name
                    // Only consume the typedef if we haven't already seen a base type
                    if base_kind.is_none() {
                        if let Some(typedef_type_id) = self.symbols.lookup_typedef(name_id) {
                            self.advance();
                            // Get the underlying type and merge in any modifiers we collected
                            let typedef_type = self.types.get(typedef_type_id);
                            let mut result = typedef_type.clone();
                            // Strip TYPEDEF modifier - we're using the typedef, not defining one
                            result.modifiers &= !TypeModifiers::TYPEDEF;
                            result.modifiers |= modifiers;
                            return Ok(result);
                        }
                    }
                    break;
                }
            }
        }

        let kind = base_kind.unwrap_or(TypeKind::Int);
        Ok(Type::with_modifiers(kind, modifiers))
    }

    /// Parse an enum specifier
    /// enum-specifier: 'enum' identifier? '{' enumerator-list? '}' | 'enum' identifier
    fn parse_enum_specifier(&mut self) -> ParseResult<Type> {
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

            let mut constants = Vec::new();
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
    fn parse_struct_or_union_specifier(&mut self, is_union: bool) -> ParseResult<Type> {
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

            let mut members = Vec::new();

            while !self.is_special(b'}') && !self.is_eof() {
                // Parse member declaration
                let member_base_type = self.parse_type_specifier()?;
                let member_base_type_id = self.types.intern(member_base_type);

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
                        // Validate bitfield type and width
                        self.validate_bitfield(typ, width)?;
                        Some(width)
                    } else {
                        None
                    };

                    members.push(StructMember {
                        name,
                        typ,
                        offset: 0, // Computed later
                        bit_offset: None,
                        bit_width,
                        storage_unit_size: None,
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
                } else if is_union {
                    Ok(Type::incomplete_union(tag_name))
                } else {
                    Ok(Type::incomplete_struct(tag_name))
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

            // Parse pointer qualifiers (const, volatile, restrict)
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
        // We keep both the TypeIds (for building the type) and full Parameters (for function defs)
        let (func_params, full_func_params): (Option<FuncParamTypes>, Option<Vec<Parameter>>) =
            if self.is_special(b'(') {
                self.advance();
                let (params, variadic) = self.parse_parameter_list()?;
                self.expect_special(b')')?;
                let type_ids: Vec<TypeId> = params.iter().map(|p| p.typ).collect();
                (Some((type_ids, variadic)), Some(params))
            } else {
                (None, None)
            };

        // Build the type from the base type
        let mut result_type_id = base_type_id;

        if let Some(inner_tid) = inner_type_id {
            // Grouped declarator: int (*p)[3] or void (*fp)(int)
            // Arrays/functions in suffix apply to the base type first
            // Then we substitute into the inner declarator

            // Apply function parameters to base type first (if present)
            // For void (*fp)(int): base is void, suffix (int) -> Function(void, [int])
            if let Some((param_type_ids, variadic)) = func_params {
                let func_type = Type::function(result_type_id, param_type_ids, variadic);
                result_type_id = self.types.intern(func_type);
            }

            // Apply array dimensions to base type
            // For int (*p)[3]: base is int, suffix [3] -> Array(3, int)
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

            // Apply any outer pointers (before the parens)
            for modifiers in pointer_modifiers.into_iter().rev() {
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

            // Substitute into inner declarator
            // For int (*p)[3]: inner_decl is Pointer(Void), result_type is Array(3, int)
            // -> Pointer(Array(3, int))
            // For void (*fp)(int): inner_decl is Pointer(Void), result_type is Function(void, [int])
            // -> Pointer(Function(void, [int]))
            result_type_id = self.substitute_base_type(inner_tid, result_type_id);
        } else {
            // Simple declarator: char *arr[3]
            // Pointers bind tighter than arrays: *arr[3] = array of pointers

            // Apply pointer modifiers to base type first
            for modifiers in pointer_modifiers.into_iter().rev() {
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
                let func_type = Type::function(result_type_id, param_type_ids, variadic);
                result_type_id = self.types.intern(func_type);
            }
        }

        // Determine which function parameters to return:
        // - For grouped declarator with inner function: return inner_func_params
        //   (those are the params of the outer function after type substitution)
        // - For simple declarator with function: return full_func_params
        let returned_func_params = if inner_type_id.is_some() {
            inner_func_params
        } else {
            full_func_params
        };

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
    /// Following sparse's design, this binds the function to the symbol table
    /// at global scope, then enters a new scope for the function body and
    /// binds all parameters in that scope.
    #[cfg(test)]
    fn parse_function_def(&mut self) -> ParseResult<FunctionDef> {
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
        let (params, variadic) = self.parse_parameter_list()?;
        self.expect_special(b')')?;

        // Build the function type
        let param_type_ids: Vec<TypeId> = params.iter().map(|p| p.typ).collect();
        let func_type = Type::function(ret_type_id, param_type_ids, variadic);
        let func_type_id = self.types.intern(func_type);

        // Bind function to symbol table at current (global) scope
        // Like sparse's bind_symbol() in parse.c
        let func_sym = Symbol::function(name, func_type_id, self.symbols.depth());
        let _ = self.symbols.declare(func_sym); // Ignore redefinition errors for now

        // Enter function scope for parameters and body
        self.symbols.enter_scope();

        // Bind parameters in function scope
        for param in &params {
            if let Some(param_name) = param.name {
                let param_sym = Symbol::parameter(param_name, param.typ, self.symbols.depth());
                let _ = self.symbols.declare(param_sym);
            }
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
        })
    }

    /// Parse a parameter list
    fn parse_parameter_list(&mut self) -> ParseResult<(Vec<Parameter>, bool)> {
        let mut params = Vec::new();
        let mut variadic = false;

        if self.is_special(b')') {
            return Ok((params, variadic));
        }

        // Check for (void)
        if self.peek() == TokenType::Ident {
            if let Some(name) = self.get_ident_name(self.current()) {
                if name == "void" {
                    let saved_pos = self.pos;
                    self.advance();
                    if self.is_special(b')') {
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
                self.advance();
                variadic = true;
                break;
            }

            // Parse parameter type
            let param_type = self.parse_type_specifier()?;
            let base_type_id = self.types.intern(param_type);

            // Use parse_declarator to handle all declarator forms including:
            // - Simple pointers: void *, int *
            // - Grouped declarators: void (*)(int), int (*)[10]
            // - Arrays: int arr[], int arr[10]
            // Note: parse_declarator returns (name, type, vla_sizes)
            let (param_name, mut typ_id, _vla_sizes, _func_params) =
                self.parse_declarator(base_type_id)?;

            // C99 6.7.5.3: Array parameters are adjusted to pointers
            // parse_declarator already gives us the array type, we need to convert
            // the outermost array dimension to a pointer
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
            }

            params.push(Parameter {
                name: if param_name == StringId::EMPTY {
                    None
                } else {
                    Some(param_name)
                },
                typ: typ_id,
            });

            if self.is_special(b',') {
                self.advance();
            } else {
                break;
            }
        }

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

    /// Parse an external declaration (function definition or declaration)
    fn parse_external_decl(&mut self) -> ParseResult<ExternalDecl> {
        let decl_pos = self.current_pos();
        // Parse type specifier
        let base_type = self.parse_type_specifier()?;
        // Check modifiers before interning (storage class specifiers)
        let is_typedef = base_type.modifiers.contains(TypeModifiers::TYPEDEF);
        let base_type_id = self.types.intern(base_type);

        // Check for standalone type definition (e.g., "enum Color { ... };")
        // This happens when a composite type is defined but no variables are declared
        if self.is_special(b';') {
            self.advance();
            // Return empty declaration - the type was already registered in parse_*_specifier
            return Ok(ExternalDecl::Declaration(Declaration {
                declarators: vec![],
            }));
        }

        // Check for grouped declarator: void (*fp)(int) or int (*arr)[10]
        // These are detected by '(' followed by '*'
        if self.is_special(b'(') {
            // Look ahead to see if this is a grouped declarator
            let saved_pos = self.pos;
            self.advance(); // consume '('
            if self.is_special(b'*') {
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

                // Skip any __attribute__ after declarator
                self.skip_extensions();

                // Check if this is a function definition (function type followed by '{')
                // This handles cases like: int (*get_op(int which))(int, int) { ... }
                if self.types.kind(typ) == TypeKind::Function && self.is_special(b'{') {
                    // Get the function's return type
                    let func_type = self.types.get(typ);
                    let return_type = func_type.base.unwrap();
                    let _is_variadic = func_type.variadic;

                    // Add function to symbol table
                    let func_sym = Symbol::function(name, typ, self.symbols.depth());
                    let _ = self.symbols.declare(func_sym);

                    // Get parameters - use decl_func_params which has names
                    let params = decl_func_params.unwrap_or_default();

                    // Enter function scope for parameters
                    self.symbols.enter_scope();

                    // Bind parameters in function scope
                    for param in &params {
                        if let Some(param_name) = param.name {
                            let param_sym =
                                Symbol::parameter(param_name, param.typ, self.symbols.depth());
                            let _ = self.symbols.declare(param_sym);
                        }
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

                // Add to symbol table
                if is_typedef {
                    let sym = Symbol::typedef(name, typ, self.symbols.depth());
                    let _ = self.symbols.declare(sym);
                } else {
                    let var_sym = Symbol::variable(name, typ, self.symbols.depth());
                    let _ = self.symbols.declare(var_sym);
                }

                return Ok(ExternalDecl::Declaration(Declaration {
                    declarators: vec![InitDeclarator {
                        name,
                        typ,
                        init,
                        vla_sizes: vec![],
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

        // Check again for grouped declarator after pointer modifiers: char *(*fp)(int)
        // At this point typ_id is char*, and we see (*fp)(int)
        if self.is_special(b'(') {
            let saved_pos = self.pos;
            self.advance(); // consume '('
            if self.is_special(b'*') {
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

                // Skip any __attribute__ after declarator
                self.skip_extensions();

                // Check if this is a function definition (function type followed by '{')
                // This handles cases like: char *(*get_op(int which))(int, int) { ... }
                if self.types.kind(full_typ) == TypeKind::Function && self.is_special(b'{') {
                    // Get the function's return type
                    let func_type = self.types.get(full_typ);
                    let return_type = func_type.base.unwrap();

                    // Add function to symbol table
                    let func_sym = Symbol::function(name, full_typ, self.symbols.depth());
                    let _ = self.symbols.declare(func_sym);

                    // Get parameters - use decl_func_params which has names
                    let params = decl_func_params.unwrap_or_default();

                    // Enter function scope for parameters
                    self.symbols.enter_scope();

                    // Bind parameters in function scope
                    for param in &params {
                        if let Some(param_name) = param.name {
                            let param_sym =
                                Symbol::parameter(param_name, param.typ, self.symbols.depth());
                            let _ = self.symbols.declare(param_sym);
                        }
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

                // Add to symbol table
                if is_typedef {
                    let sym = Symbol::typedef(name, full_typ, self.symbols.depth());
                    let _ = self.symbols.declare(sym);
                } else {
                    let var_sym = Symbol::variable(name, full_typ, self.symbols.depth());
                    let _ = self.symbols.declare(var_sym);
                }

                return Ok(ExternalDecl::Declaration(Declaration {
                    declarators: vec![InitDeclarator {
                        name,
                        typ: full_typ,
                        init,
                        vla_sizes: vec![],
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
            let base_type = self.types.get(typ_id);
            let is_noreturn =
                attrs.has_noreturn() || base_type.modifiers.contains(TypeModifiers::NORETURN);

            if self.is_special(b'{') {
                // Function definition
                // Add function to symbol table so it can be called by other functions
                let param_type_ids: Vec<TypeId> = params.iter().map(|p| p.typ).collect();
                let func_type = Type::function_with_attrs(
                    typ_id,
                    param_type_ids.clone(),
                    variadic,
                    is_noreturn,
                );
                let func_type_id = self.types.intern(func_type);
                let func_sym = Symbol::function(name, func_type_id, self.symbols.depth());
                let _ = self.symbols.declare(func_sym);

                // Enter function scope for parameters
                self.symbols.enter_scope();

                // Bind parameters in function scope
                for param in &params {
                    if let Some(param_name) = param.name {
                        let param_sym =
                            Symbol::parameter(param_name, param.typ, self.symbols.depth());
                        let _ = self.symbols.declare(param_sym);
                    }
                }

                // Parse body without creating another scope
                let body = self.parse_block_stmt_no_scope()?;

                // Leave function scope
                self.symbols.leave_scope();

                return Ok(ExternalDecl::FunctionDef(FunctionDef {
                    return_type: typ_id,
                    name,
                    params,
                    body,
                    pos: decl_pos,
                }));
            } else {
                // Function declaration
                // Skip __asm("...") symbol aliasing which can appear after function declarator
                self.skip_extensions();
                self.expect_special(b';')?;
                let param_type_ids: Vec<TypeId> = params.iter().map(|p| p.typ).collect();
                let func_type =
                    Type::function_with_attrs(typ_id, param_type_ids, variadic, is_noreturn);
                let func_type_id = self.types.intern(func_type);
                // Add function declaration to symbol table so the variadic flag
                // is available when the function is called
                let func_sym = Symbol::function(name, func_type_id, self.symbols.depth());
                let _ = self.symbols.declare(func_sym);
                return Ok(ExternalDecl::Declaration(Declaration {
                    declarators: vec![InitDeclarator {
                        name,
                        typ: func_type_id,
                        init: None,
                        vla_sizes: vec![],
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

        // Skip any __attribute__ after variable name/array declarator
        self.skip_extensions();

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

        // Add to symbol table
        if is_typedef {
            let sym = Symbol::typedef(name, var_type_id, self.symbols.depth());
            let _ = self.symbols.declare(sym);
        } else {
            // Add global variable to symbol table so it can be referenced by later code
            let var_sym = Symbol::variable(name, var_type_id, self.symbols.depth());
            let _ = self.symbols.declare(var_sym);
        }

        declarators.push(InitDeclarator {
            name,
            typ: var_type_id,
            init,
            vla_sizes: vec![],
        });

        // Handle additional declarators
        while self.is_special(b',') {
            self.advance();
            let (decl_name, decl_type, vla_sizes, _decl_func_params) =
                self.parse_declarator(base_type_id)?;

            // C99 6.7.5.2: VLAs must have block scope
            if !vla_sizes.is_empty() {
                return Err(ParseError::new(
                    "variable length arrays cannot have file scope".to_string(),
                    self.current_pos(),
                ));
            }

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
            // Add to symbol table
            if is_typedef {
                let sym = Symbol::typedef(decl_name, decl_type, self.symbols.depth());
                let _ = self.symbols.declare(sym);
            } else {
                let var_sym = Symbol::variable(decl_name, decl_type, self.symbols.depth());
                let _ = self.symbols.declare(var_sym);
            }
            declarators.push(InitDeclarator {
                name: decl_name,
                typ: decl_type,
                init: decl_init,
                vla_sizes: vec![],
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
    fn eval_const_expr(&self, expr: &Expr) -> Option<i64> {
        match &expr.kind {
            ExprKind::IntLit(val) => Some(*val),
            ExprKind::CharLit(c) => Some(*c as i64),

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

            ExprKind::Ident { name } => {
                // Check for enum constant
                self.symbols.get_enum_value(*name)
            }

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
    fn validate_bitfield(&self, typ_id: TypeId, width: u32) -> ParseResult<()> {
        // Check allowed types: _Bool, int, unsigned int (and their signed/unsigned variants)
        let kind = self.types.kind(typ_id);
        let valid_type = matches!(
            kind,
            TypeKind::Bool | TypeKind::Int | TypeKind::Char | TypeKind::Short | TypeKind::Long
        );

        if !valid_type {
            return Err(ParseError::new(
                "bitfield must have integer type",
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

        Ok(())
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::strings::StringTable;
    use crate::symbol::SymbolTable;
    use crate::token::lexer::Tokenizer;

    fn parse_expr(input: &str) -> ParseResult<(Expr, TypeTable, StringTable)> {
        let mut strings = StringTable::new();
        let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
        let tokens = tokenizer.tokenize();
        drop(tokenizer);
        let mut symbols = SymbolTable::new();
        let mut types = TypeTable::new(64);
        let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types);
        parser.skip_stream_tokens();
        let expr = parser.parse_expression()?;
        Ok((expr, types, strings))
    }

    /// Helper to compare a StringId with a string literal
    fn check_name(strings: &StringTable, id: StringId, expected: &str) {
        assert_eq!(strings.get(id), expected);
    }

    // ========================================================================
    // Literal tests
    // ========================================================================

    #[test]
    fn test_int_literal() {
        let (expr, _types, _strings) = parse_expr("42").unwrap();
        assert!(matches!(expr.kind, ExprKind::IntLit(42)));
    }

    #[test]
    fn test_hex_literal() {
        let (expr, _types, _strings) = parse_expr("0xFF").unwrap();
        assert!(matches!(expr.kind, ExprKind::IntLit(255)));
    }

    #[test]
    fn test_octal_literal() {
        let (expr, _types, _strings) = parse_expr("0777").unwrap();
        assert!(matches!(expr.kind, ExprKind::IntLit(511)));
    }

    #[test]
    fn test_float_literal() {
        let (expr, _types, _strings) = parse_expr("3.14").unwrap();
        match expr.kind {
            ExprKind::FloatLit(v) => assert!((v - 3.14).abs() < 0.001),
            _ => panic!("Expected FloatLit"),
        }
    }

    #[test]
    fn test_char_literal() {
        let (expr, _types, _strings) = parse_expr("'a'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('a')));
    }

    #[test]
    fn test_char_escape() {
        let (expr, _types, _strings) = parse_expr("'\\n'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('\n')));
    }

    #[test]
    fn test_string_literal() {
        let (expr, _types, _strings) = parse_expr("\"hello\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected StringLit"),
        }
    }

    // ========================================================================
    // Character literal escape sequence tests
    // ========================================================================

    #[test]
    fn test_char_escape_newline() {
        let (expr, _types, _strings) = parse_expr("'\\n'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('\n')));
    }

    #[test]
    fn test_char_escape_tab() {
        let (expr, _types, _strings) = parse_expr("'\\t'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('\t')));
    }

    #[test]
    fn test_char_escape_carriage_return() {
        let (expr, _types, _strings) = parse_expr("'\\r'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('\r')));
    }

    #[test]
    fn test_char_escape_backslash() {
        let (expr, _types, _strings) = parse_expr("'\\\\'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('\\')));
    }

    #[test]
    fn test_char_escape_single_quote() {
        let (expr, _types, _strings) = parse_expr("'\\''").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('\'')));
    }

    #[test]
    fn test_char_escape_double_quote() {
        let (expr, _types, _strings) = parse_expr("'\\\"'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('"')));
    }

    #[test]
    fn test_char_escape_bell() {
        let (expr, _types, _strings) = parse_expr("'\\a'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('\x07')));
    }

    #[test]
    fn test_char_escape_backspace() {
        let (expr, _types, _strings) = parse_expr("'\\b'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('\x08')));
    }

    #[test]
    fn test_char_escape_formfeed() {
        let (expr, _types, _strings) = parse_expr("'\\f'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('\x0C')));
    }

    #[test]
    fn test_char_escape_vertical_tab() {
        let (expr, _types, _strings) = parse_expr("'\\v'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('\x0B')));
    }

    #[test]
    fn test_char_escape_null() {
        let (expr, _types, _strings) = parse_expr("'\\0'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('\0')));
    }

    #[test]
    fn test_char_escape_hex() {
        let (expr, _types, _strings) = parse_expr("'\\x41'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('A')));
    }

    #[test]
    fn test_char_escape_hex_lowercase() {
        let (expr, _types, _strings) = parse_expr("'\\x0a'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('\n')));
    }

    #[test]
    fn test_char_escape_octal() {
        let (expr, _types, _strings) = parse_expr("'\\101'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('A'))); // octal 101 = 65 = 'A'
    }

    #[test]
    fn test_char_escape_octal_012() {
        let (expr, _types, _strings) = parse_expr("'\\012'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('\n'))); // octal 012 = 10 = '\n'
    }

    // ========================================================================
    // UCN (Universal Character Name) escape sequence tests - C99 6.4.3
    // ========================================================================

    #[test]
    fn test_char_escape_ucn_short() {
        // \u00E9 is 'é' (U+00E9)
        let (expr, _types, _strings) = parse_expr("'\\u00E9'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('é')));
    }

    #[test]
    fn test_char_escape_ucn_short_lowercase() {
        // \u00e9 is 'é' (U+00E9) - lowercase hex
        let (expr, _types, _strings) = parse_expr("'\\u00e9'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('é')));
    }

    #[test]
    fn test_char_escape_ucn_long() {
        // \U00000041 is 'A' (U+0041)
        let (expr, _types, _strings) = parse_expr("'\\U00000041'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('A')));
    }

    #[test]
    fn test_char_escape_ucn_long_emoji() {
        // \U0001F600 is '😀' (U+1F600)
        let (expr, _types, _strings) = parse_expr("'\\U0001F600'").unwrap();
        assert!(matches!(expr.kind, ExprKind::CharLit('😀')));
    }

    #[test]
    fn test_string_ucn() {
        // "caf\u00E9" should become "café"
        let (expr, _types, _strings) = parse_expr("\"caf\\u00E9\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, "café"),
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_string_ucn_long() {
        // Test long UCN in string
        let (expr, _types, _strings) = parse_expr("\"hello\\U0001F600world\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, "hello😀world"),
            _ => panic!("Expected StringLit"),
        }
    }

    // ========================================================================
    // String literal escape sequence tests
    // ========================================================================

    #[test]
    fn test_string_escape_newline() {
        let (expr, _types, _strings) = parse_expr("\"hello\\nworld\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, "hello\nworld"),
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_string_escape_tab() {
        let (expr, _types, _strings) = parse_expr("\"hello\\tworld\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, "hello\tworld"),
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_string_escape_carriage_return() {
        let (expr, _types, _strings) = parse_expr("\"hello\\rworld\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, "hello\rworld"),
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_string_escape_backslash() {
        let (expr, _types, _strings) = parse_expr("\"hello\\\\world\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, "hello\\world"),
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_string_escape_double_quote() {
        let (expr, _types, _strings) = parse_expr("\"hello\\\"world\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, "hello\"world"),
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_string_escape_bell() {
        let (expr, _types, _strings) = parse_expr("\"\\a\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, "\x07"),
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_string_escape_backspace() {
        let (expr, _types, _strings) = parse_expr("\"\\b\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, "\x08"),
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_string_escape_formfeed() {
        let (expr, _types, _strings) = parse_expr("\"\\f\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, "\x0C"),
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_string_escape_vertical_tab() {
        let (expr, _types, _strings) = parse_expr("\"\\v\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, "\x0B"),
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_string_escape_null() {
        let (expr, _types, _strings) = parse_expr("\"hello\\0world\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => {
                assert_eq!(s.len(), 11);
                assert_eq!(s.as_bytes()[5], 0);
            }
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_string_escape_hex() {
        let (expr, _types, _strings) = parse_expr("\"\\x41\\x42\\x43\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, "ABC"),
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_string_escape_octal() {
        let (expr, _types, _strings) = parse_expr("\"\\101\\102\\103\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, "ABC"), // octal 101,102,103 = A,B,C
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_string_escape_octal_012() {
        let (expr, _types, _strings) = parse_expr("\"line1\\012line2\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, "line1\nline2"), // octal 012 = newline
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_string_multiple_escapes() {
        let (expr, _types, _strings) = parse_expr("\"\\t\\n\\r\\\\\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, "\t\n\r\\"),
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_string_mixed_content() {
        let (expr, _types, _strings) = parse_expr("\"Name:\\tJohn\\nAge:\\t30\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, "Name:\tJohn\nAge:\t30"),
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_string_empty() {
        let (expr, _types, _strings) = parse_expr("\"\"").unwrap();
        match expr.kind {
            ExprKind::StringLit(s) => assert_eq!(s, ""),
            _ => panic!("Expected StringLit"),
        }
    }

    #[test]
    fn test_integer_literal_suffixes() {
        // Plain int
        let (expr, types, _strings) = parse_expr("42").unwrap();
        assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Int);
        assert!(!types.is_unsigned(expr.typ.unwrap()));

        // Unsigned int
        let (expr, types, _strings) = parse_expr("42U").unwrap();
        assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Int);
        assert!(types.is_unsigned(expr.typ.unwrap()));

        // Long
        let (expr, types, _strings) = parse_expr("42L").unwrap();
        assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Long);
        assert!(!types.is_unsigned(expr.typ.unwrap()));

        // Unsigned long (UL)
        let (expr, types, _strings) = parse_expr("42UL").unwrap();
        assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Long);
        assert!(types.is_unsigned(expr.typ.unwrap()));

        // Unsigned long (LU)
        let (expr, types, _strings) = parse_expr("42LU").unwrap();
        assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::Long);
        assert!(types.is_unsigned(expr.typ.unwrap()));

        // Long long
        let (expr, types, _strings) = parse_expr("42LL").unwrap();
        assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::LongLong);
        assert!(!types.is_unsigned(expr.typ.unwrap()));

        // Unsigned long long (ULL)
        let (expr, types, _strings) = parse_expr("42ULL").unwrap();
        assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::LongLong);
        assert!(types.is_unsigned(expr.typ.unwrap()));

        // Unsigned long long (LLU)
        let (expr, types, _strings) = parse_expr("42LLU").unwrap();
        assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::LongLong);
        assert!(types.is_unsigned(expr.typ.unwrap()));

        // Hex with suffix
        let (expr, types, _strings) = parse_expr("0xFFLL").unwrap();
        assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::LongLong);
        assert!(!types.is_unsigned(expr.typ.unwrap()));

        let (expr, types, _strings) = parse_expr("0xFFULL").unwrap();
        assert_eq!(types.kind(expr.typ.unwrap()), TypeKind::LongLong);
        assert!(types.is_unsigned(expr.typ.unwrap()));
    }

    #[test]
    fn test_identifier() {
        let (expr, _types, strings) = parse_expr("foo").unwrap();
        match expr.kind {
            ExprKind::Ident { name, .. } => check_name(&strings, name, "foo"),
            _ => panic!("Expected Ident"),
        }
    }

    // ========================================================================
    // Binary operator tests
    // ========================================================================

    #[test]
    fn test_addition() {
        let (expr, _types, _strings) = parse_expr("1 + 2").unwrap();
        match expr.kind {
            ExprKind::Binary { op, left, right } => {
                assert_eq!(op, BinaryOp::Add);
                assert!(matches!(left.kind, ExprKind::IntLit(1)));
                assert!(matches!(right.kind, ExprKind::IntLit(2)));
            }
            _ => panic!("Expected Binary"),
        }
    }

    #[test]
    fn test_subtraction() {
        let (expr, _types, _strings) = parse_expr("5 - 3").unwrap();
        match expr.kind {
            ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Sub),
            _ => panic!("Expected Binary"),
        }
    }

    #[test]
    fn test_multiplication() {
        let (expr, _types, _strings) = parse_expr("2 * 3").unwrap();
        match expr.kind {
            ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Mul),
            _ => panic!("Expected Binary"),
        }
    }

    #[test]
    fn test_division() {
        let (expr, _types, _strings) = parse_expr("10 / 2").unwrap();
        match expr.kind {
            ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Div),
            _ => panic!("Expected Binary"),
        }
    }

    #[test]
    fn test_modulo() {
        let (expr, _types, _strings) = parse_expr("10 % 3").unwrap();
        match expr.kind {
            ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Mod),
            _ => panic!("Expected Binary"),
        }
    }

    #[test]
    fn test_precedence_mul_add() {
        // 1 + 2 * 3 should be 1 + (2 * 3)
        let (expr, _types, _strings) = parse_expr("1 + 2 * 3").unwrap();
        match expr.kind {
            ExprKind::Binary { op, left, right } => {
                assert_eq!(op, BinaryOp::Add);
                assert!(matches!(left.kind, ExprKind::IntLit(1)));
                match right.kind {
                    ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Mul),
                    _ => panic!("Expected nested Binary"),
                }
            }
            _ => panic!("Expected Binary"),
        }
    }

    #[test]
    fn test_left_associativity() {
        // 1 - 2 - 3 should be (1 - 2) - 3
        let (expr, _types, _strings) = parse_expr("1 - 2 - 3").unwrap();
        match expr.kind {
            ExprKind::Binary { op, left, right } => {
                assert_eq!(op, BinaryOp::Sub);
                assert!(matches!(right.kind, ExprKind::IntLit(3)));
                match left.kind {
                    ExprKind::Binary { op, left, right } => {
                        assert_eq!(op, BinaryOp::Sub);
                        assert!(matches!(left.kind, ExprKind::IntLit(1)));
                        assert!(matches!(right.kind, ExprKind::IntLit(2)));
                    }
                    _ => panic!("Expected nested Binary"),
                }
            }
            _ => panic!("Expected Binary"),
        }
    }

    #[test]
    fn test_comparison_ops() {
        let (expr, _types, _strings) = parse_expr("a < b").unwrap();
        match expr.kind {
            ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Lt),
            _ => panic!("Expected Binary"),
        }

        let (expr, _types, _strings) = parse_expr("a > b").unwrap();
        match expr.kind {
            ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Gt),
            _ => panic!("Expected Binary"),
        }

        let (expr, _types, _strings) = parse_expr("a <= b").unwrap();
        match expr.kind {
            ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Le),
            _ => panic!("Expected Binary"),
        }

        let (expr, _types, _strings) = parse_expr("a >= b").unwrap();
        match expr.kind {
            ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Ge),
            _ => panic!("Expected Binary"),
        }
    }

    #[test]
    fn test_equality_ops() {
        let (expr, _types, _strings) = parse_expr("a == b").unwrap();
        match expr.kind {
            ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Eq),
            _ => panic!("Expected Binary"),
        }

        let (expr, _types, _strings) = parse_expr("a != b").unwrap();
        match expr.kind {
            ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Ne),
            _ => panic!("Expected Binary"),
        }
    }

    #[test]
    fn test_logical_ops() {
        let (expr, _types, _strings) = parse_expr("a && b").unwrap();
        match expr.kind {
            ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::LogAnd),
            _ => panic!("Expected Binary"),
        }

        let (expr, _types, _strings) = parse_expr("a || b").unwrap();
        match expr.kind {
            ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::LogOr),
            _ => panic!("Expected Binary"),
        }
    }

    #[test]
    fn test_bitwise_ops() {
        let (expr, _types, _strings) = parse_expr("a & b").unwrap();
        match expr.kind {
            ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::BitAnd),
            _ => panic!("Expected Binary"),
        }

        let (expr, _types, _strings) = parse_expr("a | b").unwrap();
        match expr.kind {
            ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::BitOr),
            _ => panic!("Expected Binary"),
        }

        let (expr, _types, _strings) = parse_expr("a ^ b").unwrap();
        match expr.kind {
            ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::BitXor),
            _ => panic!("Expected Binary"),
        }
    }

    #[test]
    fn test_shift_ops() {
        let (expr, _types, _strings) = parse_expr("a << b").unwrap();
        match expr.kind {
            ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Shl),
            _ => panic!("Expected Binary"),
        }

        let (expr, _types, _strings) = parse_expr("a >> b").unwrap();
        match expr.kind {
            ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Shr),
            _ => panic!("Expected Binary"),
        }
    }

    // ========================================================================
    // Unary operator tests
    // ========================================================================

    #[test]
    fn test_unary_neg() {
        let (expr, _types, strings) = parse_expr("-x").unwrap();
        match expr.kind {
            ExprKind::Unary { op, operand } => {
                assert_eq!(op, UnaryOp::Neg);
                match operand.kind {
                    ExprKind::Ident { name, .. } => check_name(&strings, name, "x"),
                    _ => panic!("Expected Ident"),
                }
            }
            _ => panic!("Expected Unary"),
        }
    }

    #[test]
    fn test_unary_not() {
        let (expr, _types, _strings) = parse_expr("!x").unwrap();
        match expr.kind {
            ExprKind::Unary { op, .. } => assert_eq!(op, UnaryOp::Not),
            _ => panic!("Expected Unary"),
        }
    }

    #[test]
    fn test_unary_bitnot() {
        let (expr, _types, _strings) = parse_expr("~x").unwrap();
        match expr.kind {
            ExprKind::Unary { op, .. } => assert_eq!(op, UnaryOp::BitNot),
            _ => panic!("Expected Unary"),
        }
    }

    #[test]
    fn test_unary_addr() {
        let (expr, _types, _strings) = parse_expr("&x").unwrap();
        match expr.kind {
            ExprKind::Unary { op, .. } => assert_eq!(op, UnaryOp::AddrOf),
            _ => panic!("Expected Unary"),
        }
    }

    #[test]
    fn test_unary_deref() {
        let (expr, _types, _strings) = parse_expr("*p").unwrap();
        match expr.kind {
            ExprKind::Unary { op, .. } => assert_eq!(op, UnaryOp::Deref),
            _ => panic!("Expected Unary"),
        }
    }

    #[test]
    fn test_pre_increment() {
        let (expr, _types, _strings) = parse_expr("++x").unwrap();
        match expr.kind {
            ExprKind::Unary { op, .. } => assert_eq!(op, UnaryOp::PreInc),
            _ => panic!("Expected Unary"),
        }
    }

    #[test]
    fn test_pre_decrement() {
        let (expr, _types, _strings) = parse_expr("--x").unwrap();
        match expr.kind {
            ExprKind::Unary { op, .. } => assert_eq!(op, UnaryOp::PreDec),
            _ => panic!("Expected Unary"),
        }
    }

    // ========================================================================
    // Postfix operator tests
    // ========================================================================

    #[test]
    fn test_post_increment() {
        let (expr, _types, _strings) = parse_expr("x++").unwrap();
        assert!(matches!(expr.kind, ExprKind::PostInc(_)));
    }

    #[test]
    fn test_post_decrement() {
        let (expr, _types, _strings) = parse_expr("x--").unwrap();
        assert!(matches!(expr.kind, ExprKind::PostDec(_)));
    }

    #[test]
    fn test_array_subscript() {
        let (expr, _types, strings) = parse_expr("arr[5]").unwrap();
        match expr.kind {
            ExprKind::Index { array, index } => {
                match array.kind {
                    ExprKind::Ident { name, .. } => check_name(&strings, name, "arr"),
                    _ => panic!("Expected Ident"),
                }
                assert!(matches!(index.kind, ExprKind::IntLit(5)));
            }
            _ => panic!("Expected Index"),
        }
    }

    #[test]
    fn test_member_access() {
        let (expr, _types, strings) = parse_expr("obj.field").unwrap();
        match expr.kind {
            ExprKind::Member { expr, member } => {
                match expr.kind {
                    ExprKind::Ident { name, .. } => check_name(&strings, name, "obj"),
                    _ => panic!("Expected Ident"),
                }
                check_name(&strings, member, "field");
            }
            _ => panic!("Expected Member"),
        }
    }

    #[test]
    fn test_arrow_access() {
        let (expr, _types, strings) = parse_expr("ptr->field").unwrap();
        match expr.kind {
            ExprKind::Arrow { expr, member } => {
                match expr.kind {
                    ExprKind::Ident { name, .. } => check_name(&strings, name, "ptr"),
                    _ => panic!("Expected Ident"),
                }
                check_name(&strings, member, "field");
            }
            _ => panic!("Expected Arrow"),
        }
    }

    #[test]
    fn test_function_call_no_args() {
        let (expr, _types, strings) = parse_expr("foo()").unwrap();
        match expr.kind {
            ExprKind::Call { func, args } => {
                match func.kind {
                    ExprKind::Ident { name, .. } => check_name(&strings, name, "foo"),
                    _ => panic!("Expected Ident"),
                }
                assert!(args.is_empty());
            }
            _ => panic!("Expected Call"),
        }
    }

    #[test]
    fn test_function_call_with_args() {
        let (expr, _types, strings) = parse_expr("foo(1, 2, 3)").unwrap();
        match expr.kind {
            ExprKind::Call { func, args } => {
                match func.kind {
                    ExprKind::Ident { name, .. } => check_name(&strings, name, "foo"),
                    _ => panic!("Expected Ident"),
                }
                assert_eq!(args.len(), 3);
            }
            _ => panic!("Expected Call"),
        }
    }

    #[test]
    fn test_chained_postfix() {
        // obj.arr[0]->next
        let (expr, _types, strings) = parse_expr("obj.arr[0]").unwrap();
        match expr.kind {
            ExprKind::Index { array, index } => {
                match array.kind {
                    ExprKind::Member { expr, member } => {
                        match expr.kind {
                            ExprKind::Ident { name, .. } => check_name(&strings, name, "obj"),
                            _ => panic!("Expected Ident"),
                        }
                        check_name(&strings, member, "arr");
                    }
                    _ => panic!("Expected Member"),
                }
                assert!(matches!(index.kind, ExprKind::IntLit(0)));
            }
            _ => panic!("Expected Index"),
        }
    }

    // ========================================================================
    // Assignment tests
    // ========================================================================

    #[test]
    fn test_simple_assignment() {
        let (expr, _types, strings) = parse_expr("x = 5").unwrap();
        match expr.kind {
            ExprKind::Assign { op, target, value } => {
                assert_eq!(op, AssignOp::Assign);
                match target.kind {
                    ExprKind::Ident { name, .. } => check_name(&strings, name, "x"),
                    _ => panic!("Expected Ident"),
                }
                assert!(matches!(value.kind, ExprKind::IntLit(5)));
            }
            _ => panic!("Expected Assign"),
        }
    }

    #[test]
    fn test_compound_assignments() {
        let (expr, _types, _strings) = parse_expr("x += 5").unwrap();
        match expr.kind {
            ExprKind::Assign { op, .. } => assert_eq!(op, AssignOp::AddAssign),
            _ => panic!("Expected Assign"),
        }

        let (expr, _types, _strings) = parse_expr("x -= 5").unwrap();
        match expr.kind {
            ExprKind::Assign { op, .. } => assert_eq!(op, AssignOp::SubAssign),
            _ => panic!("Expected Assign"),
        }

        let (expr, _types, _strings) = parse_expr("x *= 5").unwrap();
        match expr.kind {
            ExprKind::Assign { op, .. } => assert_eq!(op, AssignOp::MulAssign),
            _ => panic!("Expected Assign"),
        }
    }

    #[test]
    fn test_assignment_right_associativity() {
        // a = b = c should be a = (b = c)
        let (expr, _types, strings) = parse_expr("a = b = c").unwrap();
        match expr.kind {
            ExprKind::Assign { target, value, .. } => {
                match target.kind {
                    ExprKind::Ident { name, .. } => check_name(&strings, name, "a"),
                    _ => panic!("Expected Ident"),
                }
                match value.kind {
                    ExprKind::Assign { target, .. } => match target.kind {
                        ExprKind::Ident { name, .. } => check_name(&strings, name, "b"),
                        _ => panic!("Expected Ident"),
                    },
                    _ => panic!("Expected nested Assign"),
                }
            }
            _ => panic!("Expected Assign"),
        }
    }

    // ========================================================================
    // Ternary expression tests
    // ========================================================================

    #[test]
    fn test_ternary() {
        let (expr, _types, strings) = parse_expr("a ? b : c").unwrap();
        match expr.kind {
            ExprKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                match cond.kind {
                    ExprKind::Ident { name, .. } => check_name(&strings, name, "a"),
                    _ => panic!("Expected Ident"),
                }
                match then_expr.kind {
                    ExprKind::Ident { name, .. } => check_name(&strings, name, "b"),
                    _ => panic!("Expected Ident"),
                }
                match else_expr.kind {
                    ExprKind::Ident { name, .. } => check_name(&strings, name, "c"),
                    _ => panic!("Expected Ident"),
                }
            }
            _ => panic!("Expected Conditional"),
        }
    }

    #[test]
    fn test_nested_ternary() {
        // a ? b : c ? d : e should be a ? b : (c ? d : e)
        let (expr, _types, _strings) = parse_expr("a ? b : c ? d : e").unwrap();
        match expr.kind {
            ExprKind::Conditional { else_expr, .. } => {
                assert!(matches!(else_expr.kind, ExprKind::Conditional { .. }));
            }
            _ => panic!("Expected Conditional"),
        }
    }

    // ========================================================================
    // Comma expression tests
    // ========================================================================

    #[test]
    fn test_comma_expr() {
        let (expr, _types, _strings) = parse_expr("a, b, c").unwrap();
        match expr.kind {
            ExprKind::Comma(exprs) => assert_eq!(exprs.len(), 3),
            _ => panic!("Expected Comma"),
        }
    }

    // ========================================================================
    // sizeof tests
    // ========================================================================

    #[test]
    fn test_sizeof_expr() {
        let (expr, _types, _strings) = parse_expr("sizeof x").unwrap();
        assert!(matches!(expr.kind, ExprKind::SizeofExpr(_)));
    }

    #[test]
    fn test_sizeof_type() {
        let (expr, types, _strings) = parse_expr("sizeof(int)").unwrap();
        match expr.kind {
            ExprKind::SizeofType(typ) => assert_eq!(types.kind(typ), TypeKind::Int),
            _ => panic!("Expected SizeofType"),
        }
    }

    #[test]
    fn test_sizeof_paren_expr() {
        // sizeof(x) where x is not a type
        let (expr, _types, _strings) = parse_expr("sizeof(x)").unwrap();
        assert!(matches!(expr.kind, ExprKind::SizeofExpr(_)));
    }

    // ========================================================================
    // Cast tests
    // ========================================================================

    #[test]
    fn test_cast() {
        let (expr, types, strings) = parse_expr("(int)x").unwrap();
        match expr.kind {
            ExprKind::Cast { cast_type, expr } => {
                assert_eq!(types.kind(cast_type), TypeKind::Int);
                match expr.kind {
                    ExprKind::Ident { name, .. } => check_name(&strings, name, "x"),
                    _ => panic!("Expected Ident"),
                }
            }
            _ => panic!("Expected Cast"),
        }
    }

    #[test]
    fn test_cast_unsigned_char() {
        let (expr, types, _strings) = parse_expr("(unsigned char)x").unwrap();
        match expr.kind {
            ExprKind::Cast { cast_type, .. } => {
                assert_eq!(types.kind(cast_type), TypeKind::Char);
                assert!(types
                    .get(cast_type)
                    .modifiers
                    .contains(TypeModifiers::UNSIGNED));
            }
            _ => panic!("Expected Cast"),
        }
    }

    #[test]
    fn test_cast_signed_int() {
        let (expr, types, _strings) = parse_expr("(signed int)x").unwrap();
        match expr.kind {
            ExprKind::Cast { cast_type, .. } => {
                assert_eq!(types.kind(cast_type), TypeKind::Int);
                assert!(types
                    .get(cast_type)
                    .modifiers
                    .contains(TypeModifiers::SIGNED));
            }
            _ => panic!("Expected Cast"),
        }
    }

    #[test]
    fn test_cast_unsigned_long() {
        let (expr, types, _strings) = parse_expr("(unsigned long)x").unwrap();
        match expr.kind {
            ExprKind::Cast { cast_type, .. } => {
                assert_eq!(types.kind(cast_type), TypeKind::Long);
                assert!(types
                    .get(cast_type)
                    .modifiers
                    .contains(TypeModifiers::UNSIGNED));
            }
            _ => panic!("Expected Cast"),
        }
    }

    #[test]
    fn test_cast_long_long() {
        let (expr, types, _strings) = parse_expr("(long long)x").unwrap();
        match expr.kind {
            ExprKind::Cast { cast_type, .. } => {
                assert_eq!(types.kind(cast_type), TypeKind::LongLong);
            }
            _ => panic!("Expected Cast"),
        }
    }

    #[test]
    fn test_cast_unsigned_long_long() {
        let (expr, types, _strings) = parse_expr("(unsigned long long)x").unwrap();
        match expr.kind {
            ExprKind::Cast { cast_type, .. } => {
                assert_eq!(types.kind(cast_type), TypeKind::LongLong);
                assert!(types
                    .get(cast_type)
                    .modifiers
                    .contains(TypeModifiers::UNSIGNED));
            }
            _ => panic!("Expected Cast"),
        }
    }

    #[test]
    fn test_cast_pointer() {
        let (expr, types, _strings) = parse_expr("(int*)x").unwrap();
        match expr.kind {
            ExprKind::Cast { cast_type, .. } => {
                assert_eq!(types.kind(cast_type), TypeKind::Pointer);
                let base = types.base_type(cast_type).unwrap();
                assert_eq!(types.kind(base), TypeKind::Int);
            }
            _ => panic!("Expected Cast"),
        }
    }

    #[test]
    fn test_cast_void_pointer() {
        let (expr, types, _strings) = parse_expr("(void*)x").unwrap();
        match expr.kind {
            ExprKind::Cast { cast_type, .. } => {
                assert_eq!(types.kind(cast_type), TypeKind::Pointer);
                let base = types.base_type(cast_type).unwrap();
                assert_eq!(types.kind(base), TypeKind::Void);
            }
            _ => panic!("Expected Cast"),
        }
    }

    #[test]
    fn test_cast_unsigned_char_pointer() {
        let (expr, types, _strings) = parse_expr("(unsigned char*)x").unwrap();
        match expr.kind {
            ExprKind::Cast { cast_type, .. } => {
                assert_eq!(types.kind(cast_type), TypeKind::Pointer);
                let base = types.base_type(cast_type).unwrap();
                assert_eq!(types.kind(base), TypeKind::Char);
                assert!(types.get(base).modifiers.contains(TypeModifiers::UNSIGNED));
            }
            _ => panic!("Expected Cast"),
        }
    }

    #[test]
    fn test_cast_const_int() {
        let (expr, types, _strings) = parse_expr("(const int)x").unwrap();
        match expr.kind {
            ExprKind::Cast { cast_type, .. } => {
                assert_eq!(types.kind(cast_type), TypeKind::Int);
                assert!(types
                    .get(cast_type)
                    .modifiers
                    .contains(TypeModifiers::CONST));
            }
            _ => panic!("Expected Cast"),
        }
    }

    #[test]
    fn test_cast_double_pointer() {
        let (expr, types, _strings) = parse_expr("(int**)x").unwrap();
        match expr.kind {
            ExprKind::Cast { cast_type, .. } => {
                assert_eq!(types.kind(cast_type), TypeKind::Pointer);
                let base = types.base_type(cast_type).unwrap();
                assert_eq!(types.kind(base), TypeKind::Pointer);
                let innermost = types.base_type(base).unwrap();
                assert_eq!(types.kind(innermost), TypeKind::Int);
            }
            _ => panic!("Expected Cast"),
        }
    }

    #[test]
    fn test_sizeof_compound_type() {
        let (expr, types, _strings) = parse_expr("sizeof(unsigned long long)").unwrap();
        match expr.kind {
            ExprKind::SizeofType(typ) => {
                assert_eq!(types.kind(typ), TypeKind::LongLong);
                assert!(types.get(typ).modifiers.contains(TypeModifiers::UNSIGNED));
            }
            _ => panic!("Expected SizeofType"),
        }
    }

    #[test]
    fn test_sizeof_pointer_type() {
        let (expr, types, _strings) = parse_expr("sizeof(int*)").unwrap();
        match expr.kind {
            ExprKind::SizeofType(typ) => {
                assert_eq!(types.kind(typ), TypeKind::Pointer);
            }
            _ => panic!("Expected SizeofType"),
        }
    }

    // ========================================================================
    // Parentheses tests
    // ========================================================================

    #[test]
    fn test_parentheses() {
        let (expr, _types, _strings) = parse_expr("(1 + 2) * 3").unwrap();
        match expr.kind {
            ExprKind::Binary { op, left, .. } => {
                assert_eq!(op, BinaryOp::Mul);
                match left.kind {
                    ExprKind::Binary { op, .. } => assert_eq!(op, BinaryOp::Add),
                    _ => panic!("Expected Binary"),
                }
            }
            _ => panic!("Expected Binary"),
        }
    }

    // ========================================================================
    // Complex expression tests
    // ========================================================================

    #[test]
    fn test_complex_expr() {
        // x = a + b * c - d / e
        let (expr, _types, _strings) = parse_expr("x = a + b * c - d / e").unwrap();
        assert!(matches!(expr.kind, ExprKind::Assign { .. }));
    }

    #[test]
    fn test_function_call_complex() {
        // foo(a + b, c * d)
        let (expr, _types, _strings) = parse_expr("foo(a + b, c * d)").unwrap();
        match expr.kind {
            ExprKind::Call { args, .. } => {
                assert_eq!(args.len(), 2);
                assert!(matches!(
                    args[0].kind,
                    ExprKind::Binary {
                        op: BinaryOp::Add,
                        ..
                    }
                ));
                assert!(matches!(
                    args[1].kind,
                    ExprKind::Binary {
                        op: BinaryOp::Mul,
                        ..
                    }
                ));
            }
            _ => panic!("Expected Call"),
        }
    }

    #[test]
    fn test_pointer_arithmetic() {
        // *p++
        let (expr, _types, _strings) = parse_expr("*p++").unwrap();
        match expr.kind {
            ExprKind::Unary {
                op: UnaryOp::Deref,
                operand,
            } => {
                assert!(matches!(operand.kind, ExprKind::PostInc(_)));
            }
            _ => panic!("Expected Unary Deref"),
        }
    }

    // ========================================================================
    // Statement tests
    // ========================================================================

    fn parse_stmt(input: &str) -> ParseResult<(Stmt, StringTable)> {
        let mut strings = StringTable::new();
        let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
        let tokens = tokenizer.tokenize();
        drop(tokenizer);
        let mut symbols = SymbolTable::new();
        let mut types = TypeTable::new(64);
        let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types);
        parser.skip_stream_tokens();
        let stmt = parser.parse_statement()?;
        Ok((stmt, strings))
    }

    #[test]
    fn test_empty_stmt() {
        let (stmt, _strings) = parse_stmt(";").unwrap();
        assert!(matches!(stmt, Stmt::Empty));
    }

    #[test]
    fn test_expr_stmt() {
        let (stmt, _strings) = parse_stmt("x = 5;").unwrap();
        assert!(matches!(stmt, Stmt::Expr(_)));
    }

    #[test]
    fn test_if_stmt() {
        let (stmt, _strings) = parse_stmt("if (x) y = 1;").unwrap();
        match stmt {
            Stmt::If {
                cond,
                then_stmt,
                else_stmt,
            } => {
                assert!(matches!(cond.kind, ExprKind::Ident { .. }));
                assert!(matches!(*then_stmt, Stmt::Expr(_)));
                assert!(else_stmt.is_none());
            }
            _ => panic!("Expected If"),
        }
    }

    #[test]
    fn test_if_else_stmt() {
        let (stmt, _strings) = parse_stmt("if (x) y = 1; else y = 2;").unwrap();
        match stmt {
            Stmt::If { else_stmt, .. } => {
                assert!(else_stmt.is_some());
            }
            _ => panic!("Expected If"),
        }
    }

    #[test]
    fn test_while_stmt() {
        let (stmt, _strings) = parse_stmt("while (x) x--;").unwrap();
        match stmt {
            Stmt::While { cond, body } => {
                assert!(matches!(cond.kind, ExprKind::Ident { .. }));
                assert!(matches!(*body, Stmt::Expr(_)));
            }
            _ => panic!("Expected While"),
        }
    }

    #[test]
    fn test_do_while_stmt() {
        let (stmt, _strings) = parse_stmt("do x++; while (x < 10);").unwrap();
        match stmt {
            Stmt::DoWhile { body, cond } => {
                assert!(matches!(*body, Stmt::Expr(_)));
                assert!(matches!(
                    cond.kind,
                    ExprKind::Binary {
                        op: BinaryOp::Lt,
                        ..
                    }
                ));
            }
            _ => panic!("Expected DoWhile"),
        }
    }

    #[test]
    fn test_for_stmt_basic() {
        let (stmt, _strings) = parse_stmt("for (i = 0; i < 10; i++) x++;").unwrap();
        match stmt {
            Stmt::For {
                init,
                cond,
                post,
                body,
            } => {
                assert!(init.is_some());
                assert!(cond.is_some());
                assert!(post.is_some());
                assert!(matches!(*body, Stmt::Expr(_)));
            }
            _ => panic!("Expected For"),
        }
    }

    #[test]
    fn test_for_stmt_with_decl() {
        let (stmt, _strings) = parse_stmt("for (int i = 0; i < 10; i++) x++;").unwrap();
        match stmt {
            Stmt::For { init, .. } => {
                assert!(matches!(init, Some(ForInit::Declaration(_))));
            }
            _ => panic!("Expected For"),
        }
    }

    #[test]
    fn test_for_stmt_empty() {
        let (stmt, _strings) = parse_stmt("for (;;) ;").unwrap();
        match stmt {
            Stmt::For {
                init,
                cond,
                post,
                body,
            } => {
                assert!(init.is_none());
                assert!(cond.is_none());
                assert!(post.is_none());
                assert!(matches!(*body, Stmt::Empty));
            }
            _ => panic!("Expected For"),
        }
    }

    #[test]
    fn test_return_void() {
        let (stmt, _strings) = parse_stmt("return;").unwrap();
        match stmt {
            Stmt::Return(None) => {}
            _ => panic!("Expected Return(None)"),
        }
    }

    #[test]
    fn test_return_value() {
        let (stmt, _strings) = parse_stmt("return 42;").unwrap();
        match stmt {
            Stmt::Return(Some(ref e)) => {
                assert!(matches!(e.kind, ExprKind::IntLit(42)));
            }
            _ => panic!("Expected Return(Some(42))"),
        }
    }

    #[test]
    fn test_break_stmt() {
        let (stmt, _strings) = parse_stmt("break;").unwrap();
        assert!(matches!(stmt, Stmt::Break));
    }

    #[test]
    fn test_continue_stmt() {
        let (stmt, _strings) = parse_stmt("continue;").unwrap();
        assert!(matches!(stmt, Stmt::Continue));
    }

    #[test]
    fn test_goto_stmt() {
        let (stmt, strings) = parse_stmt("goto label;").unwrap();
        match stmt {
            Stmt::Goto(name) => check_name(&strings, name, "label"),
            _ => panic!("Expected Goto"),
        }
    }

    #[test]
    fn test_labeled_stmt() {
        let (stmt, strings) = parse_stmt("label: x = 1;").unwrap();
        match stmt {
            Stmt::Label { name, stmt } => {
                check_name(&strings, name, "label");
                assert!(matches!(*stmt, Stmt::Expr(_)));
            }
            _ => panic!("Expected Label"),
        }
    }

    #[test]
    fn test_block_stmt() {
        let (stmt, _strings) = parse_stmt("{ x = 1; y = 2; }").unwrap();
        match stmt {
            Stmt::Block(items) => {
                assert_eq!(items.len(), 2);
                assert!(matches!(items[0], BlockItem::Statement(Stmt::Expr(_))));
                assert!(matches!(items[1], BlockItem::Statement(Stmt::Expr(_))));
            }
            _ => panic!("Expected Block"),
        }
    }

    #[test]
    fn test_block_with_decl() {
        let (stmt, _strings) = parse_stmt("{ int x = 1; x++; }").unwrap();
        match stmt {
            Stmt::Block(items) => {
                assert_eq!(items.len(), 2);
                assert!(matches!(items[0], BlockItem::Declaration(_)));
                assert!(matches!(items[1], BlockItem::Statement(_)));
            }
            _ => panic!("Expected Block"),
        }
    }

    // ========================================================================
    // Declaration tests
    // ========================================================================

    fn parse_decl(input: &str) -> ParseResult<(Declaration, TypeTable, StringTable)> {
        let mut strings = StringTable::new();
        let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
        let tokens = tokenizer.tokenize();
        drop(tokenizer);
        let mut symbols = SymbolTable::new();
        let mut types = TypeTable::new(64);
        let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types);
        parser.skip_stream_tokens();
        let decl = parser.parse_declaration()?;
        Ok((decl, types, strings))
    }

    #[test]
    fn test_simple_decl() {
        let (decl, types, strings) = parse_decl("int x;").unwrap();
        assert_eq!(decl.declarators.len(), 1);
        check_name(&strings, decl.declarators[0].name, "x");
        assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Int);
    }

    #[test]
    fn test_decl_with_init() {
        let (decl, _types, _strings) = parse_decl("int x = 5;").unwrap();
        assert_eq!(decl.declarators.len(), 1);
        assert!(decl.declarators[0].init.is_some());
    }

    #[test]
    fn test_multiple_declarators() {
        let (decl, _types, strings) = parse_decl("int x, y, z;").unwrap();
        assert_eq!(decl.declarators.len(), 3);
        check_name(&strings, decl.declarators[0].name, "x");
        check_name(&strings, decl.declarators[1].name, "y");
        check_name(&strings, decl.declarators[2].name, "z");
    }

    #[test]
    fn test_pointer_decl() {
        let (decl, types, _strings) = parse_decl("int *p;").unwrap();
        assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
    }

    #[test]
    fn test_array_decl() {
        let (decl, types, _strings) = parse_decl("int arr[10];").unwrap();
        assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Array);
        assert_eq!(types.get(decl.declarators[0].typ).array_size, Some(10));
    }

    #[test]
    fn test_const_decl() {
        let (decl, types, _strings) = parse_decl("const int x = 5;").unwrap();
        assert!(types
            .get(decl.declarators[0].typ)
            .modifiers
            .contains(TypeModifiers::CONST));
    }

    #[test]
    fn test_unsigned_decl() {
        let (decl, types, _strings) = parse_decl("unsigned int x;").unwrap();
        assert!(types
            .get(decl.declarators[0].typ)
            .modifiers
            .contains(TypeModifiers::UNSIGNED));
    }

    #[test]
    fn test_long_long_decl() {
        let (decl, types, _strings) = parse_decl("long long x;").unwrap();
        assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::LongLong);
    }

    // ========================================================================
    // Function parsing tests
    // ========================================================================

    fn parse_func(input: &str) -> ParseResult<(FunctionDef, TypeTable, StringTable)> {
        let mut strings = StringTable::new();
        let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
        let tokens = tokenizer.tokenize();
        drop(tokenizer);
        let mut symbols = SymbolTable::new();
        let mut types = TypeTable::new(64);
        let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types);
        parser.skip_stream_tokens();
        let func = parser.parse_function_def()?;
        Ok((func, types, strings))
    }

    #[test]
    fn test_simple_function() {
        let (func, types, strings) = parse_func("int main() { return 0; }").unwrap();
        check_name(&strings, func.name, "main");
        assert_eq!(types.kind(func.return_type), TypeKind::Int);
        assert!(func.params.is_empty());
    }

    #[test]
    fn test_function_with_params() {
        let (func, _types, strings) =
            parse_func("int add(int a, int b) { return a + b; }").unwrap();
        check_name(&strings, func.name, "add");
        assert_eq!(func.params.len(), 2);
    }

    #[test]
    fn test_void_function() {
        let (func, types, _strings) = parse_func("void foo(void) { }").unwrap();
        assert_eq!(types.kind(func.return_type), TypeKind::Void);
        assert!(func.params.is_empty());
    }

    #[test]
    fn test_variadic_function() {
        // Variadic functions are parsed but variadic info is not tracked in FunctionDef
        let (func, _types, strings) =
            parse_func("int printf(char *fmt, ...) { return 0; }").unwrap();
        check_name(&strings, func.name, "printf");
    }

    #[test]
    fn test_pointer_return() {
        let (func, types, _strings) = parse_func("int *getptr() { return 0; }").unwrap();
        assert_eq!(types.kind(func.return_type), TypeKind::Pointer);
    }

    // ========================================================================
    // Translation unit tests
    // ========================================================================

    fn parse_tu(input: &str) -> ParseResult<(TranslationUnit, TypeTable, StringTable)> {
        let mut strings = StringTable::new();
        let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
        let tokens = tokenizer.tokenize();
        drop(tokenizer);
        let mut symbols = SymbolTable::new();
        let mut types = TypeTable::new(64);
        let mut parser = Parser::new(&tokens, &strings, &mut symbols, &mut types);
        let tu = parser.parse_translation_unit()?;
        Ok((tu, types, strings))
    }

    #[test]
    fn test_simple_program() {
        let (tu, _types, _strings) = parse_tu("int main() { return 0; }").unwrap();
        assert_eq!(tu.items.len(), 1);
        assert!(matches!(tu.items[0], ExternalDecl::FunctionDef(_)));
    }

    #[test]
    fn test_global_var() {
        let (tu, _types, _strings) = parse_tu("int x = 5;").unwrap();
        assert_eq!(tu.items.len(), 1);
        assert!(matches!(tu.items[0], ExternalDecl::Declaration(_)));
    }

    #[test]
    fn test_multiple_items() {
        let (tu, _types, _strings) = parse_tu("int x; int main() { return x; }").unwrap();
        assert_eq!(tu.items.len(), 2);
        assert!(matches!(tu.items[0], ExternalDecl::Declaration(_)));
        assert!(matches!(tu.items[1], ExternalDecl::FunctionDef(_)));
    }

    #[test]
    fn test_function_declaration() {
        let (tu, types, strings) = parse_tu("int foo(int x);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "foo");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_struct_only_declaration() {
        // Struct definition without a variable declarator
        let (tu, _types, _strings) = parse_tu("struct point { int x; int y; };").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                // No declarators for struct-only definition
                assert!(decl.declarators.is_empty());
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_struct_with_variable_declaration() {
        // Struct definition with a variable declarator
        let (tu, types, strings) = parse_tu("struct point { int x; int y; } p;").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                assert_eq!(decl.declarators.len(), 1);
                check_name(&strings, decl.declarators[0].name, "p");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Struct);
            }
            _ => panic!("Expected Declaration"),
        }
    }

    // ========================================================================
    // Typedef tests
    // ========================================================================

    #[test]
    fn test_typedef_basic() {
        // Basic typedef declaration
        let (tu, types, strings) = parse_tu("typedef int myint;").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                assert_eq!(decl.declarators.len(), 1);
                check_name(&strings, decl.declarators[0].name, "myint");
                // The type includes the TYPEDEF modifier
                assert!(types
                    .get(decl.declarators[0].typ)
                    .modifiers
                    .contains(TypeModifiers::TYPEDEF));
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_typedef_usage() {
        // Typedef declaration followed by usage
        let (tu, types, strings) = parse_tu("typedef int myint; myint x;").unwrap();
        assert_eq!(tu.items.len(), 2);

        // First item: typedef declaration
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "myint");
            }
            _ => panic!("Expected typedef Declaration"),
        }

        // Second item: variable using typedef
        match &tu.items[1] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "x");
                // The variable should have int type (resolved from typedef)
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Int);
            }
            _ => panic!("Expected variable Declaration"),
        }
    }

    #[test]
    fn test_typedef_pointer() {
        // Typedef for pointer type
        let (tu, types, strings) = parse_tu("typedef int *intptr; intptr p;").unwrap();
        assert_eq!(tu.items.len(), 2);

        // Variable should have pointer type
        match &tu.items[1] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "p");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
            }
            _ => panic!("Expected variable Declaration"),
        }
    }

    #[test]
    fn test_typedef_struct() {
        // Typedef for anonymous struct
        let (tu, types, strings) =
            parse_tu("typedef struct { int x; int y; } Point; Point p;").unwrap();
        assert_eq!(tu.items.len(), 2);

        // Variable should have struct type
        match &tu.items[1] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "p");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Struct);
            }
            _ => panic!("Expected variable Declaration"),
        }
    }

    #[test]
    fn test_typedef_chained() {
        // Chained typedef: typedef of typedef
        let (tu, types, strings) =
            parse_tu("typedef int myint; typedef myint myint2; myint2 x;").unwrap();
        assert_eq!(tu.items.len(), 3);

        // Final variable should resolve to int
        match &tu.items[2] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "x");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Int);
            }
            _ => panic!("Expected variable Declaration"),
        }
    }

    #[test]
    fn test_typedef_multiple() {
        // Multiple typedefs in one declaration
        let (tu, types, strings) = parse_tu("typedef int INT, *INTPTR;").unwrap();
        assert_eq!(tu.items.len(), 1);

        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                assert_eq!(decl.declarators.len(), 2);
                check_name(&strings, decl.declarators[0].name, "INT");
                check_name(&strings, decl.declarators[1].name, "INTPTR");
                // INTPTR should be a pointer type
                assert_eq!(types.kind(decl.declarators[1].typ), TypeKind::Pointer);
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_typedef_in_function() {
        // Typedef used in function parameter and return type
        let (tu, types, strings) =
            parse_tu("typedef int myint; myint add(myint a, myint b) { return a + b; }").unwrap();
        assert_eq!(tu.items.len(), 2);

        match &tu.items[1] {
            ExternalDecl::FunctionDef(func) => {
                check_name(&strings, func.name, "add");
                // Return type should resolve to int
                assert_eq!(types.kind(func.return_type), TypeKind::Int);
                // Parameters should also resolve to int
                assert_eq!(func.params.len(), 2);
                assert_eq!(types.kind(func.params[0].typ), TypeKind::Int);
                assert_eq!(types.kind(func.params[1].typ), TypeKind::Int);
            }
            _ => panic!("Expected FunctionDef"),
        }
    }

    #[test]
    fn test_typedef_local_variable() {
        // Typedef used as local variable type inside function body
        let (tu, _types, strings) =
            parse_tu("typedef int myint; int main(void) { myint x; x = 42; return 0; }").unwrap();
        assert_eq!(tu.items.len(), 2);

        match &tu.items[1] {
            ExternalDecl::FunctionDef(func) => {
                check_name(&strings, func.name, "main");
                // Check that the body parsed correctly
                match &func.body {
                    Stmt::Block(items) => {
                        assert!(items.len() >= 2, "Expected at least 2 block items");
                    }
                    _ => panic!("Expected Block statement"),
                }
            }
            _ => panic!("Expected FunctionDef"),
        }
    }

    // ========================================================================
    // Restrict qualifier tests
    // ========================================================================

    #[test]
    fn test_restrict_pointer_decl() {
        // Local variable with restrict qualifier
        let (tu, _types, _strings) =
            parse_tu("int main(void) { int * restrict p; return 0; }").unwrap();
        assert_eq!(tu.items.len(), 1);
        // Just verify it parses without error
    }

    #[test]
    fn test_restrict_function_param() {
        // Function with restrict-qualified pointer parameters
        let (tu, types, strings) =
            parse_tu("void copy(int * restrict dest, int * restrict src) { *dest = *src; }")
                .unwrap();
        assert_eq!(tu.items.len(), 1);

        match &tu.items[0] {
            ExternalDecl::FunctionDef(func) => {
                check_name(&strings, func.name, "copy");
                assert_eq!(func.params.len(), 2);
                // Both params should be restrict-qualified pointers
                assert_eq!(types.kind(func.params[0].typ), TypeKind::Pointer);
                assert!(types
                    .get(func.params[0].typ)
                    .modifiers
                    .contains(TypeModifiers::RESTRICT));
                assert_eq!(types.kind(func.params[1].typ), TypeKind::Pointer);
                assert!(types
                    .get(func.params[1].typ)
                    .modifiers
                    .contains(TypeModifiers::RESTRICT));
            }
            _ => panic!("Expected FunctionDef"),
        }
    }

    #[test]
    fn test_restrict_with_const() {
        // Pointer with both const and restrict qualifiers
        let (tu, _types, _strings) =
            parse_tu("int main(void) { int * const restrict p = 0; return 0; }").unwrap();
        assert_eq!(tu.items.len(), 1);
        // Just verify it parses without error - both qualifiers should be accepted
    }

    #[test]
    fn test_restrict_global_pointer() {
        // Global pointer with restrict qualifier
        let (tu, types, strings) = parse_tu("int * restrict global_ptr;").unwrap();
        assert_eq!(tu.items.len(), 1);

        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "global_ptr");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
                assert!(types
                    .get(decl.declarators[0].typ)
                    .modifiers
                    .contains(TypeModifiers::RESTRICT));
            }
            _ => panic!("Expected Declaration"),
        }
    }

    // ========================================================================
    // Volatile qualifier tests
    // ========================================================================

    #[test]
    fn test_volatile_basic() {
        // Basic volatile variable
        let (tu, types, strings) = parse_tu("volatile int x;").unwrap();
        assert_eq!(tu.items.len(), 1);

        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "x");
                assert!(types
                    .get(decl.declarators[0].typ)
                    .modifiers
                    .contains(TypeModifiers::VOLATILE));
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_volatile_pointer() {
        // Pointer to volatile int
        let (tu, types, strings) = parse_tu("volatile int *p;").unwrap();
        assert_eq!(tu.items.len(), 1);

        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "p");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
                // The base type should be volatile
                let base_id = types.base_type(decl.declarators[0].typ).unwrap();
                assert!(types
                    .get(base_id)
                    .modifiers
                    .contains(TypeModifiers::VOLATILE));
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_volatile_pointer_itself() {
        // Volatile pointer to int (pointer itself is volatile)
        let (tu, types, strings) = parse_tu("int * volatile p;").unwrap();
        assert_eq!(tu.items.len(), 1);

        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "p");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
                // The pointer type itself should be volatile
                assert!(types
                    .get(decl.declarators[0].typ)
                    .modifiers
                    .contains(TypeModifiers::VOLATILE));
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_volatile_const_combined() {
        // Both const and volatile
        let (tu, types, strings) = parse_tu("const volatile int x;").unwrap();
        assert_eq!(tu.items.len(), 1);

        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "x");
                assert!(types
                    .get(decl.declarators[0].typ)
                    .modifiers
                    .contains(TypeModifiers::VOLATILE));
                assert!(types
                    .get(decl.declarators[0].typ)
                    .modifiers
                    .contains(TypeModifiers::CONST));
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_volatile_function_param() {
        // Function with volatile pointer parameter
        let (tu, types, strings) = parse_tu("void foo(volatile int *p) { *p = 1; }").unwrap();
        assert_eq!(tu.items.len(), 1);

        match &tu.items[0] {
            ExternalDecl::FunctionDef(func) => {
                check_name(&strings, func.name, "foo");
                assert_eq!(func.params.len(), 1);
                // Parameter is pointer to volatile int
                assert_eq!(types.kind(func.params[0].typ), TypeKind::Pointer);
                let base_id = types.base_type(func.params[0].typ).unwrap();
                assert!(types
                    .get(base_id)
                    .modifiers
                    .contains(TypeModifiers::VOLATILE));
            }
            _ => panic!("Expected FunctionDef"),
        }
    }

    // ========================================================================
    // __attribute__ tests
    // ========================================================================

    #[test]
    fn test_attribute_on_function_declaration() {
        // Attribute on function declaration
        let (tu, _types, strings) = parse_tu("void foo(void) __attribute__((noreturn));").unwrap();
        assert_eq!(tu.items.len(), 1);

        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                assert_eq!(decl.declarators.len(), 1);
                check_name(&strings, decl.declarators[0].name, "foo");
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_attribute_on_struct() {
        // Attribute between struct keyword and name (with variable)
        let (tu, types, strings) =
            parse_tu("struct __attribute__((packed)) foo { int x; } s;").unwrap();
        assert_eq!(tu.items.len(), 1);

        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                assert_eq!(decl.declarators.len(), 1);
                check_name(&strings, decl.declarators[0].name, "s");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Struct);
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_attribute_after_struct() {
        // Attribute after struct closing brace (with variable)
        let (tu, types, strings) =
            parse_tu("struct foo { int x; } __attribute__((aligned(16))) s;").unwrap();
        assert_eq!(tu.items.len(), 1);

        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                assert_eq!(decl.declarators.len(), 1);
                check_name(&strings, decl.declarators[0].name, "s");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Struct);
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_attribute_on_struct_only() {
        // Attribute on struct-only definition (no variable)
        let (tu, _types, _strings) =
            parse_tu("struct __attribute__((packed)) foo { int x; };").unwrap();
        assert_eq!(tu.items.len(), 1);

        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                // No variable declared, just the struct definition
                assert_eq!(decl.declarators.len(), 0);
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_attribute_on_variable() {
        // Attribute on variable declaration
        let (tu, _types, strings) = parse_tu("int x __attribute__((aligned(8)));").unwrap();
        assert_eq!(tu.items.len(), 1);

        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                assert_eq!(decl.declarators.len(), 1);
                check_name(&strings, decl.declarators[0].name, "x");
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_attribute_multiple() {
        // Multiple attributes in one list
        let (tu, _types, strings) =
            parse_tu("void foo(void) __attribute__((noreturn, cold));").unwrap();
        assert_eq!(tu.items.len(), 1);

        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "foo");
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_attribute_with_args() {
        // Attribute with multiple arguments
        let (tu, _types, strings) = parse_tu(
            "void foo(const char *fmt, ...) __attribute__((__format__(__printf__, 1, 2)));",
        )
        .unwrap();
        assert_eq!(tu.items.len(), 1);

        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "foo");
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_attribute_before_declaration() {
        // Attribute before declaration
        let (tu, _types, strings) =
            parse_tu("__attribute__((visibility(\"default\"))) int exported_var;").unwrap();
        assert_eq!(tu.items.len(), 1);

        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "exported_var");
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_attribute_underscore_variant() {
        // __attribute variant (single underscore pair)
        let (tu, _types, strings) = parse_tu("void foo(void) __attribute((noreturn));").unwrap();
        assert_eq!(tu.items.len(), 1);

        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "foo");
            }
            _ => panic!("Expected Declaration"),
        }
    }

    // ========================================================================
    // Const enforcement tests
    // ========================================================================
    // Note: Error detection is tested via integration tests, not unit tests,
    // because the error counter is global state shared across parallel tests.
    // These tests verify that const parsing works and code with const violations
    // still produces a valid AST (parsing continues after reporting errors).

    #[test]
    fn test_const_assignment_parses() {
        // Assignment to const variable should still parse (errors are reported but parsing continues)
        let (tu, _types, _strings) =
            parse_tu("int main(void) { const int x = 42; x = 10; return 0; }").unwrap();
        assert_eq!(tu.items.len(), 1);
        // Verify we got a function definition
        assert!(matches!(tu.items[0], ExternalDecl::FunctionDef(_)));
    }

    #[test]
    fn test_const_pointer_deref_parses() {
        // Assignment through pointer to const should still parse
        let (tu, _types, _strings) =
            parse_tu("int main(void) { int v = 1; const int *p = &v; *p = 2; return 0; }").unwrap();
        assert_eq!(tu.items.len(), 1);
        assert!(matches!(tu.items[0], ExternalDecl::FunctionDef(_)));
    }

    #[test]
    fn test_const_usage_valid() {
        // Valid const usage - reading const values
        let (tu, _types, _strings) =
            parse_tu("int main(void) { const int x = 42; int y = x + 1; return y; }").unwrap();
        assert_eq!(tu.items.len(), 1);
        assert!(matches!(tu.items[0], ExternalDecl::FunctionDef(_)));
    }

    #[test]
    fn test_const_pointer_types() {
        // Different const pointer combinations
        let (tu, _types, _strings) = parse_tu(
            "int main(void) { int v = 1; const int *a = &v; int * const b = &v; const int * const c = &v; return 0; }",
        )
        .unwrap();
        assert_eq!(tu.items.len(), 1);
        assert!(matches!(tu.items[0], ExternalDecl::FunctionDef(_)));
    }

    // ========================================================================
    // Function declaration tests (prototypes)
    // ========================================================================

    #[test]
    fn test_function_decl_no_params() {
        let (tu, types, strings) = parse_tu("int foo(void);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                assert_eq!(decl.declarators.len(), 1);
                check_name(&strings, decl.declarators[0].name, "foo");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
                assert!(!types.is_variadic(decl.declarators[0].typ));
                // Check return type
                if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                    assert_eq!(types.kind(base_id), TypeKind::Int);
                }
                // Check params (void means empty)
                if let Some(params) = types.params(decl.declarators[0].typ) {
                    assert!(params.is_empty());
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_function_decl_one_param() {
        let (tu, types, strings) = parse_tu("int square(int x);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "square");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
                assert!(!types.is_variadic(decl.declarators[0].typ));
                if let Some(params) = types.params(decl.declarators[0].typ) {
                    assert_eq!(params.len(), 1);
                    assert_eq!(types.kind(params[0]), TypeKind::Int);
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_function_decl_multiple_params() {
        let (tu, types, strings) = parse_tu("int add(int a, int b, int c);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "add");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
                assert!(!types.is_variadic(decl.declarators[0].typ));
                if let Some(params) = types.params(decl.declarators[0].typ) {
                    assert_eq!(params.len(), 3);
                    for p in params {
                        assert_eq!(types.kind(*p), TypeKind::Int);
                    }
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_function_decl_void_return() {
        let (tu, types, strings) = parse_tu("void do_something(int x);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "do_something");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
                if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                    assert_eq!(types.kind(base_id), TypeKind::Void);
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_function_decl_pointer_return() {
        let (tu, types, strings) = parse_tu("char *get_string(void);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "get_string");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
                if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                    assert_eq!(types.kind(base_id), TypeKind::Pointer);
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_function_decl_pointer_param() {
        let (tu, types, strings) = parse_tu("void process(int *data, int count);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "process");
                if let Some(params) = types.params(decl.declarators[0].typ) {
                    assert_eq!(params.len(), 2);
                    assert_eq!(types.kind(params[0]), TypeKind::Pointer);
                    assert_eq!(types.kind(params[1]), TypeKind::Int);
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    // ========================================================================
    // Variadic function declaration tests
    // ========================================================================

    #[test]
    fn test_function_decl_variadic_printf() {
        // Classic printf prototype
        let (tu, types, strings) = parse_tu("int printf(const char *fmt, ...);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "printf");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
                // Should be marked as variadic
                assert!(
                    types.is_variadic(decl.declarators[0].typ),
                    "printf should be marked as variadic"
                );
                if let Some(params) = types.params(decl.declarators[0].typ) {
                    // Only the fixed parameter (fmt) should be in params
                    assert_eq!(params.len(), 1);
                    assert_eq!(types.kind(params[0]), TypeKind::Pointer);
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_function_decl_variadic_sprintf() {
        let (tu, types, strings) =
            parse_tu("int sprintf(char *buf, const char *fmt, ...);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "sprintf");
                assert!(
                    types.is_variadic(decl.declarators[0].typ),
                    "sprintf should be marked as variadic"
                );
                if let Some(params) = types.params(decl.declarators[0].typ) {
                    // Two fixed parameters: buf and fmt
                    assert_eq!(params.len(), 2);
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_function_decl_variadic_custom() {
        // Custom variadic function with int first param
        let (tu, types, strings) = parse_tu("int sum_ints(int count, ...);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "sum_ints");
                assert!(
                    types.is_variadic(decl.declarators[0].typ),
                    "sum_ints should be marked as variadic"
                );
                if let Some(params) = types.params(decl.declarators[0].typ) {
                    assert_eq!(params.len(), 1);
                    assert_eq!(types.kind(params[0]), TypeKind::Int);
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_function_decl_variadic_multiple_fixed() {
        // Variadic function with multiple fixed parameters
        let (tu, types, strings) =
            parse_tu("int variadic_func(int a, double b, char *c, ...);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "variadic_func");
                assert!(
                    types.is_variadic(decl.declarators[0].typ),
                    "variadic_func should be marked as variadic"
                );
                if let Some(params) = types.params(decl.declarators[0].typ) {
                    assert_eq!(params.len(), 3);
                    assert_eq!(types.kind(params[0]), TypeKind::Int);
                    assert_eq!(types.kind(params[1]), TypeKind::Double);
                    assert_eq!(types.kind(params[2]), TypeKind::Pointer);
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_function_decl_variadic_void_return() {
        // Variadic function with void return type
        let (tu, types, strings) = parse_tu("void log_message(const char *fmt, ...);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "log_message");
                assert!(
                    types.is_variadic(decl.declarators[0].typ),
                    "log_message should be marked as variadic"
                );
                if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                    assert_eq!(types.kind(base_id), TypeKind::Void);
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_function_decl_not_variadic() {
        // Make sure non-variadic functions are NOT marked as variadic
        let (tu, types, strings) = parse_tu("int regular_func(int a, int b);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "regular_func");
                assert!(
                    !types.is_variadic(decl.declarators[0].typ),
                    "regular_func should NOT be marked as variadic"
                );
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_variadic_function_definition() {
        // Variadic function definition (not just declaration)
        let (func, _types, strings) =
            parse_func("int my_printf(char *fmt, ...) { return 0; }").unwrap();
        check_name(&strings, func.name, "my_printf");
        // Note: FunctionDef doesn't directly expose variadic, but the function
        // body can use va_start etc. This test just ensures parsing succeeds.
        assert_eq!(func.params.len(), 1);
    }

    #[test]
    fn test_multiple_function_decls_mixed() {
        // Mix of variadic and non-variadic declarations
        let (tu, types, strings) = parse_tu(
            "int printf(const char *fmt, ...); int puts(const char *s); int sprintf(char *buf, const char *fmt, ...);",
        )
        .unwrap();
        assert_eq!(tu.items.len(), 3);

        // printf - variadic
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "printf");
                assert!(types.is_variadic(decl.declarators[0].typ));
            }
            _ => panic!("Expected Declaration"),
        }

        // puts - not variadic
        match &tu.items[1] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "puts");
                assert!(!types.is_variadic(decl.declarators[0].typ));
            }
            _ => panic!("Expected Declaration"),
        }

        // sprintf - variadic
        match &tu.items[2] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "sprintf");
                assert!(types.is_variadic(decl.declarators[0].typ));
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_function_decl_with_struct_param() {
        // Function declaration with struct parameter
        let (tu, types, strings) =
            parse_tu("struct point { int x; int y; }; void move_point(struct point p);").unwrap();
        assert_eq!(tu.items.len(), 2);
        match &tu.items[1] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "move_point");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
                assert!(!types.is_variadic(decl.declarators[0].typ));
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_function_decl_array_decay() {
        // Array parameters decay to pointers in function declarations
        let (tu, types, strings) = parse_tu("void process_array(int arr[]);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "process_array");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Function);
                // The array parameter should decay to pointer
                if let Some(params) = types.params(decl.declarators[0].typ) {
                    assert_eq!(params.len(), 1);
                    // Array params in function declarations become pointers
                    let p_kind = types.kind(params[0]);
                    assert!(p_kind == TypeKind::Pointer || p_kind == TypeKind::Array);
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    // ========================================================================
    // Function pointer tests
    // ========================================================================

    #[test]
    fn test_function_pointer_declaration() {
        // Basic function pointer: void (*fp)(int)
        let (tu, types, strings) = parse_tu("void (*fp)(int);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                assert_eq!(decl.declarators.len(), 1);
                check_name(&strings, decl.declarators[0].name, "fp");
                // fp should be a pointer to a function
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
                // The base type of the pointer should be a function
                if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                    assert_eq!(types.kind(base_id), TypeKind::Function);
                    // Function returns void
                    if let Some(ret_id) = types.base_type(base_id) {
                        assert_eq!(types.kind(ret_id), TypeKind::Void);
                    }
                    // Function takes one int parameter
                    if let Some(params) = types.params(base_id) {
                        assert_eq!(params.len(), 1);
                        assert_eq!(types.kind(params[0]), TypeKind::Int);
                    }
                } else {
                    panic!("Expected function pointer base type");
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_function_pointer_no_params() {
        // Function pointer with no parameters: int (*fp)(void)
        let (tu, types, strings) = parse_tu("int (*fp)(void);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "fp");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
                if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                    assert_eq!(types.kind(base_id), TypeKind::Function);
                    // (void) means no parameters
                    if let Some(params) = types.params(base_id) {
                        assert!(params.is_empty());
                    }
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_function_pointer_multiple_params() {
        // Function pointer with multiple parameters: int (*fp)(int, char, double)
        let (tu, types, strings) = parse_tu("int (*fp)(int, char, double);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "fp");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
                if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                    assert_eq!(types.kind(base_id), TypeKind::Function);
                    if let Some(params) = types.params(base_id) {
                        assert_eq!(params.len(), 3);
                        assert_eq!(types.kind(params[0]), TypeKind::Int);
                        assert_eq!(types.kind(params[1]), TypeKind::Char);
                        assert_eq!(types.kind(params[2]), TypeKind::Double);
                    }
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_function_pointer_returning_pointer() {
        // Function pointer returning a pointer: char *(*fp)(int)
        let (tu, types, strings) = parse_tu("char *(*fp)(int);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "fp");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
                if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                    assert_eq!(types.kind(base_id), TypeKind::Function);
                    // Return type is char*
                    if let Some(ret_id) = types.base_type(base_id) {
                        assert_eq!(types.kind(ret_id), TypeKind::Pointer);
                        if let Some(char_id) = types.base_type(ret_id) {
                            assert_eq!(types.kind(char_id), TypeKind::Char);
                        }
                    }
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_function_pointer_variadic() {
        // Variadic function pointer: int (*fp)(const char *, ...)
        let (tu, types, strings) = parse_tu("int (*fp)(const char *, ...);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                check_name(&strings, decl.declarators[0].name, "fp");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Pointer);
                if let Some(base_id) = types.base_type(decl.declarators[0].typ) {
                    assert_eq!(types.kind(base_id), TypeKind::Function);
                    assert!(types.is_variadic(base_id), "Function should be variadic");
                    if let Some(params) = types.params(base_id) {
                        assert_eq!(params.len(), 1);
                        assert_eq!(types.kind(params[0]), TypeKind::Pointer);
                    }
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_function_returning_function_pointer() {
        // Function returning function pointer: int (*get_op(int which))(int, int)
        // This declares get_op as a function taking int and returning a pointer to
        // a function (int, int) -> int
        let (tu, types, strings) = parse_tu("int (*get_op(int which))(int, int);").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                assert_eq!(decl.declarators.len(), 1);
                check_name(&strings, decl.declarators[0].name, "get_op");

                // get_op should be a Function type
                let get_op_type = decl.declarators[0].typ;
                assert_eq!(types.kind(get_op_type), TypeKind::Function);

                // get_op takes one int parameter
                if let Some(params) = types.params(get_op_type) {
                    assert_eq!(params.len(), 1);
                    assert_eq!(types.kind(params[0]), TypeKind::Int);
                } else {
                    panic!("Expected function parameters");
                }

                // Return type should be a pointer to a function
                if let Some(ret_id) = types.base_type(get_op_type) {
                    assert_eq!(types.kind(ret_id), TypeKind::Pointer);

                    // The pointer's base should be a function
                    if let Some(func_id) = types.base_type(ret_id) {
                        assert_eq!(types.kind(func_id), TypeKind::Function);

                        // The inner function returns int
                        if let Some(inner_ret_id) = types.base_type(func_id) {
                            assert_eq!(types.kind(inner_ret_id), TypeKind::Int);
                        } else {
                            panic!("Expected inner function return type");
                        }

                        // The inner function takes (int, int)
                        if let Some(inner_params) = types.params(func_id) {
                            assert_eq!(inner_params.len(), 2);
                            assert_eq!(types.kind(inner_params[0]), TypeKind::Int);
                            assert_eq!(types.kind(inner_params[1]), TypeKind::Int);
                        } else {
                            panic!("Expected inner function parameters");
                        }
                    } else {
                        panic!("Expected function pointer base type");
                    }
                } else {
                    panic!("Expected return type");
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    // ========================================================================
    // Bitfield tests
    // ========================================================================

    #[test]
    fn test_bitfield_basic() {
        // Basic bitfield parsing - include a variable declarator
        let (tu, types, strings) =
            parse_tu("struct flags { unsigned int a : 4; unsigned int b : 4; } f;").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                assert_eq!(decl.declarators.len(), 1);
                check_name(&strings, decl.declarators[0].name, "f");
                assert_eq!(types.kind(decl.declarators[0].typ), TypeKind::Struct);
                if let Some(composite) = types.composite(decl.declarators[0].typ) {
                    assert_eq!(composite.members.len(), 2);
                    // First bitfield
                    check_name(&strings, composite.members[0].name, "a");
                    assert_eq!(composite.members[0].bit_width, Some(4));
                    assert_eq!(composite.members[0].bit_offset, Some(0));
                    // Second bitfield
                    check_name(&strings, composite.members[1].name, "b");
                    assert_eq!(composite.members[1].bit_width, Some(4));
                    assert_eq!(composite.members[1].bit_offset, Some(4));
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_bitfield_unnamed() {
        // Unnamed bitfield for padding
        let (tu, types, strings) = parse_tu(
            "struct padded { unsigned int a : 4; unsigned int : 4; unsigned int b : 8; } p;",
        )
        .unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                assert_eq!(decl.declarators.len(), 1);
                if let Some(composite) = types.composite(decl.declarators[0].typ) {
                    assert_eq!(composite.members.len(), 3);
                    // First named bitfield
                    check_name(&strings, composite.members[0].name, "a");
                    assert_eq!(composite.members[0].bit_width, Some(4));
                    // Unnamed padding bitfield
                    check_name(&strings, composite.members[1].name, "");
                    assert_eq!(composite.members[1].bit_width, Some(4));
                    // Second named bitfield
                    check_name(&strings, composite.members[2].name, "b");
                    assert_eq!(composite.members[2].bit_width, Some(8));
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_bitfield_zero_width() {
        // Zero-width bitfield forces alignment
        let (tu, types, strings) = parse_tu(
            "struct aligned { unsigned int a : 4; unsigned int : 0; unsigned int b : 4; } x;",
        )
        .unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                assert_eq!(decl.declarators.len(), 1);
                if let Some(composite) = types.composite(decl.declarators[0].typ) {
                    assert_eq!(composite.members.len(), 3);
                    // After zero-width bitfield, b should start at new storage unit
                    check_name(&strings, composite.members[2].name, "b");
                    assert_eq!(composite.members[2].bit_width, Some(4));
                    // b should be at offset 0 within its storage unit
                    assert_eq!(composite.members[2].bit_offset, Some(0));
                    // b's byte offset should be different from a's
                    assert!(composite.members[2].offset > composite.members[0].offset);
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_bitfield_mixed_with_regular() {
        // Bitfield mixed with regular member
        let (tu, types, strings) =
            parse_tu("struct mixed { int x; unsigned int bits : 8; int y; } m;").unwrap();
        assert_eq!(tu.items.len(), 1);
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                assert_eq!(decl.declarators.len(), 1);
                if let Some(composite) = types.composite(decl.declarators[0].typ) {
                    assert_eq!(composite.members.len(), 3);
                    // x is regular member
                    check_name(&strings, composite.members[0].name, "x");
                    assert!(composite.members[0].bit_width.is_none());
                    // bits is a bitfield
                    check_name(&strings, composite.members[1].name, "bits");
                    assert_eq!(composite.members[1].bit_width, Some(8));
                    // y is regular member
                    check_name(&strings, composite.members[2].name, "y");
                    assert!(composite.members[2].bit_width.is_none());
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }

    #[test]
    fn test_bitfield_struct_size() {
        // Verify struct size calculation with bitfields
        let (tu, types, _strings) = parse_tu(
            "struct small { unsigned int a : 1; unsigned int b : 1; unsigned int c : 1; } s;",
        )
        .unwrap();
        match &tu.items[0] {
            ExternalDecl::Declaration(decl) => {
                assert_eq!(decl.declarators.len(), 1);
                if let Some(composite) = types.composite(decl.declarators[0].typ) {
                    // Three 1-bit fields should fit in one 4-byte int
                    assert_eq!(composite.size, 4);
                }
            }
            _ => panic!("Expected Declaration"),
        }
    }
}
