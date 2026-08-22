//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parser for c17 C17 compiler
// Recursive descent parser with Pratt-style precedence climbing
//

use super::ast::{AsmOperand, BinaryOp, BlockItem, Expr, ExprKind, ForInit, Stmt, UnaryOp};
use crate::constexpr::ConstScope;
use crate::diag;
use crate::strings::StringId;
use crate::symbol::{Namespace, Symbol, SymbolId, SymbolTable};
use crate::token::lexer::{
    payload_text, IdentTable, Position, SpecialToken, Token, TokenType, TokenValue,
};
use crate::token::preprocess::PackAction;
use crate::types::{Type, TypeId, TypeKind, TypeModifiers, TypeTable};
use gettextrs::gettext;
use std::collections::{BTreeMap, HashMap};
use std::fmt;

// Parse Error

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

/// Raw parameter info gathered while parsing a parameter list.
#[derive(Debug, Clone)]
pub(crate) struct RawParam {
    /// Parameter name; None for an unnamed parameter such as `void f(int)`.
    pub(crate) name: Option<StringId>,
    /// Parameter type, already adjusted from array/function to pointer.
    pub(crate) typ: TypeId,
    /// Run-time size expressions for a variably-modified element type; see
    /// [`Parameter::vm_dims`](crate::parse::ast::Parameter::vm_dims).
    pub(crate) vm_dims: Vec<Expr>,
    /// Symbol created while parsing the parameter list. `vm_dims` resolves
    /// against it, so the function scope re-declares this very symbol instead
    /// of a fresh one.
    pub(crate) symbol: Option<SymbolId>,
}

/// A function declarator's parameter list.
///
/// C17 6.7.6.3p14 makes `()` and `(void)` different types: an empty
/// *identifier* list supplies no information about the number or types of the
/// parameters, while `(void)` says there are none. A K&R identifier list --
/// `int f(a, b) int a, b;` -- is likewise no prototype. Both used to arrive as
/// a plain `Vec<RawParam>`, so "unknown" and "none" were indistinguishable:
/// a call to `int f(void)` went unchecked, and a call to a K&R definition was
/// checked when 6.5.2.2p1 forbids it.
pub(crate) struct ParameterList {
    pub params: Vec<RawParam>,
    pub variadic: bool,
    /// False for `()` and for an identifier list.
    pub prototyped: bool,
}

/// The `-Wno-<name>` group the unimplemented-attribute warnings belong to.
pub(crate) const ATTRIBUTE_WARNING: &str = "attributes";

/// Whether the declarator being parsed must name something.
///
/// C17 spells the two grammars separately -- `declarator` (6.7.6) always has
/// an identifier, `abstract-declarator` (6.7.7) never does -- and only the
/// caller knows which one it asked for. A parameter may be either, so it asks
/// for `Optional`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum DeclaratorName {
    /// A declaration: the identifier is what is being declared.
    Required,
    /// A type-name or a parameter, where the identifier may be absent.
    Optional,
}

// GCC __attribute__ Support

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

    pub fn has_sysv_abi(&self) -> bool {
        self.attrs
            .iter()
            .any(|a| a.name == "sysv_abi" || a.name == "__sysv_abi__")
    }

    pub fn has_ms_abi(&self) -> bool {
        self.attrs
            .iter()
            .any(|a| a.name == "ms_abi" || a.name == "__ms_abi__")
    }

    pub fn calling_conv(&self) -> Option<crate::abi::CallingConv> {
        if self.has_sysv_abi() {
            Some(crate::abi::CallingConv::SysV)
        } else if self.has_ms_abi() {
            Some(crate::abi::CallingConv::Win64)
        } else {
            None
        }
    }

    /// Whether `__attribute__((noinline))` is present.
    pub fn has_noinline(&self) -> bool {
        self.has_attr("noinline")
    }

    /// Whether `__attribute__((always_inline))` is present.
    pub fn has_always_inline(&self) -> bool {
        self.has_attr("always_inline")
    }

    /// `__attribute__((transparent_union))`, in either spelling.
    pub(super) fn has_transparent_union(&self) -> bool {
        self.has_attr("transparent_union")
    }

    /// Whether an attribute is present, in either spelling.
    fn has_attr(&self, name: &str) -> bool {
        let underscored = format!("__{name}__");
        self.attrs
            .iter()
            .any(|a| a.name == name || a.name == underscored)
    }

    /// Look up an attribute in both its plain and `__underscored__` spelling,
    /// returning its optional integer argument. The result distinguishes
    /// "absent" (`None`) from "present without a priority" (`Some(None)`).
    fn init_priority(&self, name: &str) -> Option<Option<u16>> {
        let underscored = format!("__{name}__");
        let attr = self
            .attrs
            .iter()
            .find(|a| a.name == name || a.name == underscored)?;
        match attr.args.first() {
            Some(AttributeArg::Int(n)) => Some(Some(*n as u16)),
            _ => Some(None),
        }
    }

    /// The `constructor` attribute and its optional priority.
    pub fn constructor_priority(&self) -> Option<Option<u16>> {
        self.init_priority("constructor")
    }

    /// The `destructor` attribute and its optional priority.
    pub fn destructor_priority(&self) -> Option<Option<u16>> {
        self.init_priority("destructor")
    }

    /// Collect the attributes that affect how a function is emitted.
    pub fn function_attrs(&self) -> crate::parse::ast::FunctionAttrs {
        crate::parse::ast::FunctionAttrs {
            symbol: self.symbol_attrs(),
            noinline: self.has_noinline(),
            always_inline: self.has_always_inline(),
            constructor: self.constructor_priority(),
            destructor: self.destructor_priority(),
            gnu_inline: self.has_attr("gnu_inline"),
            artificial: self.has_attr("artificial"),
        }
    }

    /// The `weak`, `used`, `section(...)` and `visibility(...)` requests in
    pub fn symbol_attrs(&self) -> crate::parse::ast::SymbolAttrs {
        let mut out = crate::parse::ast::SymbolAttrs::default();
        for attr in &self.attrs {
            let text = |a: &Attribute| match a.args.first() {
                Some(AttributeArg::String(s)) => Some(s.clone()),
                Some(AttributeArg::Ident(s)) => Some(s.clone()),
                _ => None,
            };
            match attr.name.trim_matches('_') {
                "weak" => out.weak = true,
                "used" => out.used = true,
                "section" => out.section = text(attr),
                "visibility" => out.visibility = text(attr),
                _ => {}
            }
        }
        out
    }

    pub fn get_alignment(&self) -> Option<u32> {
        for attr in &self.attrs {
            if attr.name == "aligned" || attr.name == "__aligned__" {
                if attr.args.is_empty() {
                    return Some(16); // GCC default: max useful alignment
                }
                if let Some(AttributeArg::Int(n)) = attr.args.first() {
                    return Some(*n as u32);
                }
            }
        }
        None
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

// Parser

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
    pub(super) pending_alignas: Option<u32>,
    /// The machine mode named by `__attribute__((mode(M)))` on the declaration
    /// being parsed, and where it was written. Held like `pending_alignas`
    /// because the attribute is seen while the declarator is being consumed and
    /// can only be applied once the type is final.
    pub(super) pending_mode: Option<(String, Position)>,

    /// The width in bytes named by `__attribute__((vector_size(N)))` on the
    /// declaration being parsed, and where it was written. Held like
    /// `pending_mode`, and applied at the same point, because it replaces the
    /// declared type.
    pending_vector_size: Option<(u64, Position)>,

    /// The alignment written by `__attribute__((aligned(N)))` in the list
    /// being parsed, recorded as it is seen.
    ///
    /// `apply_attribute_alignment` folds the same value into `pending_alignas`
    /// later, which is too late for `vector_size`: a vector aligns to its own
    /// width unless the source says otherwise, and the two are written in one
    /// list -- `__attribute__((vector_size(32), aligned(16)))`.
    pending_attr_align: Option<u32>,
    /// `__attribute__((transparent_union))` written *after* the declarator,
    /// which is how glibc spells it -- `typedef union { ... } __SOCKADDR_ARG
    /// __attribute__((__transparent_union__));`. Held for the same reason as
    /// `pending_mode`: the attribute is consumed while the declarator is, and
    /// only the finished type can carry it.
    pub(super) pending_transparent_union: Option<Position>,
    /// File-scope object definitions whose type was incomplete when parsed.
    /// Judged at end of translation unit -- see
    /// [`Self::check_deferred_incomplete_definitions`].
    pub(super) tentative_definitions: Vec<(TypeId, Position)>,
    /// Alignment from an attribute written *after* a declarator.
    ///
    /// Kept apart from `pending_alignas` because the two have different
    /// scope: `_Alignas`, and an attribute in the specifier position, belong
    /// to the whole declaration and reach every declarator, while
    /// `int a, b __attribute__((aligned(64)));` aligns only `b`. Sharing one
    /// slot over-aligned every declarator that followed an attributed one.
    pending_declarator_align: Option<u32>,
    /// `weak`, `used`, `section(...)`, `visibility(...)` seen on the
    /// declaration being parsed. Accumulated like `pending_alignas`, because
    /// an attribute may appear before the declarator, after it, or on the
    /// specifier, and the declarator is built once at the end.
    pub(super) pending_symbol_attrs: crate::parse::ast::SymbolAttrs,
    /// Emission-affecting function attributes seen so far in the declaration
    /// being parsed. Accumulated like `pending_alignas` because a
    /// `constructor` / `noinline` attribute may appear before the declaration
    /// specifiers, between the type and the declarator, or after the
    /// parameter list. Cleared at the start of each external declaration.
    pub(super) pending_fn_attrs: crate::parse::ast::FunctionAttrs,

    /// Set while parsing the body of a function that may use the forwarding
    /// builtins: one that is both variadic and `always_inline`.
    ///
    /// `__builtin_va_arg_pack()` names the caller's variadic arguments, so it
    /// needs both facts, and this is the last place either is visible --
    /// `ir::Function` records neither.
    pub(crate) in_forwarding_function: bool,
    /// Every function attribute seen for a given name anywhere in the
    /// translation unit.
    ///
    /// GCC applies `constructor` / `destructor` / `noinline` to the function
    /// however it was declared, so writing the attribute on a prototype and
    /// leaving the definition bare -- `static void f(void)
    /// __attribute__((constructor));` followed by `static void f(void) {}` --
    /// still makes `f` a constructor. Reading the definition's own attributes
    /// alone would silently drop it.
    pub(super) declared_fn_attrs: BTreeMap<StringId, crate::parse::ast::FunctionAttrs>,
    /// The GCC asm label seen in the declaration being parsed, awaiting the
    /// declarator it renames. Accumulated like `pending_fn_attrs` because
    /// `__asm__("...")` can appear before or after an `__attribute__` --
    /// glibc's `__REDIRECT_NTH` writes it before `__THROW`.
    pub(super) pending_asm_label: Option<String>,
    /// Every asm label seen for a given name, so that a label written on a
    /// prototype reaches the definition parsed later. GCC requires the label
    /// to appear on the first declaration, but it does not require the
    /// definition to repeat it.
    declared_asm_labels: BTreeMap<StringId, String>,
    /// Names for which some file-scope declaration carried `extern`.
    ///
    /// Kept beside the symbol table rather than only on the symbol because a
    /// definition declares a *fresh* symbol that shadows the earlier
    /// declaration's, and C99 6.7.4p6 asks about the whole translation unit.
    declared_extern_fns: std::collections::BTreeSet<StringId>,
    /// Names for which some file-scope declaration omitted `inline`.
    /// See [`crate::symbol::Symbol::has_non_inline_decl`].
    declared_non_inline_fns: std::collections::BTreeSet<StringId>,
    /// Set by `parse_type_specifier`: whether the specifier list actually
    /// named a type, rather than defaulting to `int`.
    ///
    /// C99 removed implicit int, but defaulting is still the right *recovery*
    /// — the declarator after it is usually fine — so the flag lets each
    /// caller decide whether a diagnostic belongs at its own position.
    /// `parse_type_specifier` has seven callers, and some of them (an abstract
    /// parameter declarator, a K&R identifier list) legitimately reach it with
    /// no specifier.
    pub(super) saw_explicit_type: bool,
    /// `#pragma pack` directives, and where they stood in the token stream.
    ///
    /// Sorted by index; `pack_cursor` is how far the parser has consumed
    /// them. A directive takes effect for every structure defined after it,
    /// so applying them lazily as the parse position passes each one gives
    /// exactly the right answer without a second traversal.
    pack_directives: Vec<(usize, PackAction)>,
    pack_cursor: usize,
    /// The alignment cap currently in force, and the `push`ed stack of caps.
    pack_current: Option<u32>,
    pack_stack: Vec<Option<u32>>,
    /// Typedef names that specify a variably modified type, and how many
    /// run-time extents each carries (C17 6.7.7).
    ///
    /// A use of such a name has to name the typedef's *already evaluated*
    /// extents rather than repeat its size expressions, since 6.7.7p3
    /// evaluates them at the typedef and not at each use.
    ///
    /// A `HashMap` rather than a `BTreeMap`: this is pure lookup, never
    /// iterated, so no iteration order can reach the output. See the container
    /// selection rule in `cc/CLAUDE.md`.
    pub(super) vm_typedefs: HashMap<SymbolId, u32>,
    /// The extents of the variably modified typedef named by the declaration
    /// specifiers just parsed, awaiting the declarators they apply to.
    ///
    /// Held like `pending_alignas`: the specifier list is parsed before the
    /// declarator list, and every declarator in the declaration shares it.
    pub(super) pending_vm_typedef_dims: Option<Vec<Expr>>,
}

impl<'a> Parser<'a> {
    /// Create a new parser with a symbol table and type table.
    ///
    /// `pack_directives` comes from `extract_pragma_directives`, which every
    /// caller must run over the preprocessed stream: it removes the pragma
    /// markers the preprocessor leaves behind as well as reporting them, and
    /// a stream still carrying them is not one this parser can read.
    pub fn new(
        tokens: &'a [Token],
        idents: &'a IdentTable,
        symbols: &'a mut SymbolTable,
        types: &'a mut TypeTable,
        pack_directives: Vec<(usize, PackAction)>,
    ) -> Self {
        Self {
            tokens,
            idents,
            symbols,
            types,
            pos: 0,
            pending_alignas: None,
            pending_mode: None,
            pending_vector_size: None,
            pending_attr_align: None,
            pending_transparent_union: None,
            tentative_definitions: Vec::new(),
            pending_declarator_align: None,
            pending_symbol_attrs: Default::default(),
            pending_fn_attrs: Default::default(),
            in_forwarding_function: false,
            declared_fn_attrs: BTreeMap::new(),
            pending_asm_label: None,
            declared_asm_labels: BTreeMap::new(),
            declared_extern_fns: std::collections::BTreeSet::new(),
            declared_non_inline_fns: std::collections::BTreeSet::new(),
            saw_explicit_type: true,
            pack_directives,
            pack_cursor: 0,
            vm_typedefs: HashMap::new(),
            pending_vm_typedef_dims: None,
            pack_current: None,
            pack_stack: Vec::new(),
        }
    }

    /// The alignment cap `#pragma pack` puts on a structure defined here.
    ///
    /// Applies every directive the parse position has now passed. `pop` on an
    /// empty stack is what gcc warns about and ignores; the alternative --
    /// treating it as a reset -- would silently change the layout of every
    /// structure after an unbalanced pragma.
    pub(super) fn current_pack(&mut self) -> Option<u32> {
        while self
            .pack_directives
            .get(self.pack_cursor)
            .is_some_and(|(idx, _)| *idx <= self.pos)
        {
            let (_, action) = self.pack_directives[self.pack_cursor];
            self.pack_cursor += 1;
            match action {
                PackAction::Set(n) => self.pack_current = n,
                PackAction::Push(n) => {
                    self.pack_stack.push(self.pack_current);
                    if n.is_some() {
                        self.pack_current = n;
                    }
                }
                PackAction::Pop => match self.pack_stack.pop() {
                    Some(prev) => self.pack_current = prev,
                    None => diag::warning(
                        self.current_pos(),
                        &gettext("'#pragma pack(pop)' with no matching push"),
                    ),
                },
            }
        }
        self.pack_current
    }

    // Token Navigation

    pub(crate) fn current(&self) -> &Token {
        self.tokens
            .get(self.pos)
            .unwrap_or(&self.tokens[self.tokens.len() - 1])
    }

    pub(crate) fn peek(&self) -> TokenType {
        self.current().typ
    }

    /// Whether the token *after* the current one is `(`.
    ///
    /// Used to tell a keyword being applied from the same word being used as
    /// an ordinary identifier.
    pub(super) fn next_token_is_open_paren(&self) -> bool {
        match self.tokens.get(self.pos + 1) {
            Some(t) => matches!(t.value, TokenValue::Special(v) if v == b'(' as u32),
            None => false,
        }
    }

    pub(crate) fn peek_special(&self) -> Option<u32> {
        let token = self.current();
        if token.typ == TokenType::Special {
            if let TokenValue::Special(v) = &token.value {
                return Some(*v);
            }
        }
        None
    }

    pub(crate) fn is_special(&self, c: u8) -> bool {
        self.peek_special() == Some(c as u32)
    }

    pub(crate) fn is_special_token(&self, tok: SpecialToken) -> bool {
        self.peek_special() == Some(tok as u32)
    }

    pub(crate) fn current_pos(&self) -> Position {
        self.current().pos
    }

    pub(crate) fn advance(&mut self) {
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
    }

    pub(crate) fn consume(&mut self) -> Token {
        let token = self.current().clone();
        self.advance();
        token
    }

    pub(crate) fn expect_special(&mut self, c: u8) -> ParseResult<()> {
        if self.is_special(c) {
            self.advance();
            Ok(())
        } else {
            let found = match &self.current().value {
                TokenValue::Ident(id) => {
                    format!("identifier '{}'", self.idents.get_opt(*id).unwrap_or("?"))
                }
                // A multi-character special has a discriminant above the ASCII
                // range -- `...` is 278 -- so rendering it as a `char` printed
                // a stray Latin letter. `show_special` spells all of them.
                TokenValue::Special(v) => {
                    format!("'{}'", crate::token::lexer::show_special(*v))
                }
                other => format!("{:?}", other),
            };
            Err(ParseError::new(
                format!("expected '{}', found {}", c as char, found),
                self.current_pos(),
            ))
        }
    }

    pub(crate) fn get_ident_name(&self, token: &Token) -> Option<String> {
        if let TokenValue::Ident(id) = &token.value {
            self.idents.get_opt(*id).map(|s| s.to_string())
        } else {
            None
        }
    }

    pub(crate) fn get_ident_id(&self, token: &Token) -> Option<StringId> {
        if let TokenValue::Ident(id) = &token.value {
            Some(*id)
        } else {
            None
        }
    }

    #[inline]
    pub(crate) fn str(&self, id: StringId) -> &str {
        self.idents.get(id)
    }

    /// Consume the identifier naming a declarator, rejecting keywords.
    ///
    /// Separate from [`Parser::expect_identifier`], which has eighteen callers
    /// covering labels, struct tags, member references and `goto` targets --
    /// all of which live in their own namespaces and may legitimately be
    /// spelled with a word that is reserved here. Only a *declarator* name is
    /// constrained, so only the declarator sites use this.
    pub(super) fn expect_declarator_name(&mut self) -> ParseResult<StringId> {
        if self.peek() == TokenType::Ident {
            if let Some(id) = self.get_ident_id(self.current()) {
                if crate::kw::has_tag(id, crate::kw::RESERVED_NAME) {
                    let pos = self.current_pos();
                    return Err(ParseError::new(
                        format!(
                            "'{}' is a keyword and cannot be used as a name",
                            self.str(id)
                        ),
                        pos,
                    ));
                }
            }
        }
        self.expect_identifier()
    }

    /// Check if current position (after consuming '(') indicates a grouped declarator.
    ///
    /// Grouped declarators include:
    /// - Pointer declarators: `(*name)` or `(*)`
    /// - Function type typedefs: `(name)` where name is not a type
    ///
    /// Must be called after advancing past '('. Saves/restores position internally
    /// for the function-type check.
    pub(super) fn is_grouped_declarator(&mut self) -> bool {
        // Check for pointer: (*name) or (*name[...]) etc
        if self.is_special(b'*') {
            return true;
        }

        // Another declarator: C17 6.7.6's direct-declarator is
        // `( declarator )` recursively, so `int ((q));` and `int (((*h)));`
        // are as legal as one level, and 5.2.4.1 requires 63 of them. This
        // costs no ambiguity: after the `(` of a declarator the only other
        // continuations are `)`, `...`, a declaration specifier, or a K&R
        // identifier, and a parameter list can never begin with `(`.
        if self.is_special(b'(') {
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
                    || crate::kw::has_tag(name_id, crate::kw::TYPE_KEYWORD);
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
    pub(super) fn intern_type_with_tag(&mut self, typ: &Type) -> TypeId {
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

    pub(crate) fn is_eof(&self) -> bool {
        matches!(self.peek(), TokenType::StreamEnd)
    }

    /// Check if current token is __attribute__ or __attribute
    pub(super) fn is_attribute_keyword(&self) -> bool {
        if self.peek() != TokenType::Ident {
            return false;
        }
        if let Some(id) = self.get_ident_id(self.current()) {
            crate::kw::has_tag(id, crate::kw::ATTR_KW)
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
                    let s = payload_text(s);
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
            // A negative argument. Without this the sign was skipped as an
            // unknown token and the magnitude parsed on its own, so
            // `vector_size(-16)` read as `vector_size(16)` and silently
            // produced a type the source never asked for.
            TokenType::Special if self.is_special(b'-') => {
                self.advance();
                if let TokenValue::Number(s) = &self.current().value {
                    let n = s.parse::<i64>().unwrap_or(0);
                    self.advance();
                    Some(AttributeArg::Int(-n))
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

        let pos = self.current_pos();
        let id = self.get_ident_id(self.current());
        let name = self.get_ident_name(self.current())?;
        self.advance();

        // Every attribute in the program passes through here exactly once, so
        // this is where an unrecognised one gets said out loud. It used to be
        // dropped in silence, which is survivable for an attribute that only
        // hints -- and is not for one that changes what the type *is*.
        let recognised = id.is_some_and(|id| crate::kw::has_tag(id, crate::kw::SUPPORTED_ATTR));
        if !recognised {
            if name.trim_matches('_') == "vector_size" {
                // Captured below, once the byte count is parsed, and applied
                // with the other type attributes. c17 gives it the *storage* a
                // vector has -- the right size and the right alignment -- and
                // not vector arithmetic, which is what glibc's <link.h> needs
                // and all it declares these types for. What cannot be done is
                // ignore it: the type would stay scalar and every operation on
                // it would silently compute on one element.
            } else if name.trim_matches('_') == "mode" {
                // Applied below, once the argument is parsed: a mode replaces
                // the declared type, and getting it wrong is not cosmetic --
                // glibc declares `register_t` with `__mode__(__word__)`, which
                // c17 sized 4 bytes against gcc's 8 while this was a warning.
            } else if diag::warning_group_enabled(ATTRIBUTE_WARNING) {
                diag::warning_args(pos, "'{0}' attribute directive ignored", &[&name]);
            }
        }

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

            if name.trim_matches('_') == "mode" {
                if let Some(AttributeArg::Ident(m)) = args.first() {
                    self.pending_mode = Some((m.trim_matches('_').to_string(), pos));
                }
            } else if name.trim_matches('_') == "vector_size" {
                match args.first() {
                    Some(AttributeArg::Int(n)) if *n > 0 => {
                        self.pending_vector_size = Some((*n as u64, pos));
                    }
                    _ => diag::error(pos, "'vector_size' requires a positive byte count"),
                }
            } else if name.trim_matches('_') == "aligned" {
                if let Some(AttributeArg::Int(n)) = args.first() {
                    if *n > 0 && (*n as u32).is_power_of_two() {
                        self.pending_attr_align = Some(*n as u32);
                    }
                }
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
    pub(super) fn parse_attributes(&mut self) -> AttributeList {
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

    /// Check if current token is a C11 nullability qualifier
    fn is_nullability_qualifier(&self) -> bool {
        self.peek() == TokenType::Ident
            && self
                .get_ident_id(self.current())
                .is_some_and(super::is_nullability_qualifier)
    }

    /// Check if current token is __asm or __asm__
    fn is_asm_keyword(&self) -> bool {
        if self.peek() != TokenType::Ident {
            return false;
        }
        if let Some(id) = self.get_ident_id(self.current()) {
            crate::kw::has_tag(id, crate::kw::ASM_KW)
        } else {
            false
        }
    }

    /// Parse `__asm("name")` / `__asm__("name")` on a declaration: a GCC asm
    /// label, which renames the symbol the declaration refers to.
    ///
    /// `extern int myfn(int) __asm__("realfn");` still declares `myfn` for the
    /// source to use, but every emitted reference names `realfn`. The label is
    /// left in [`Parser::pending_asm_label`] for the declarator to claim.
    ///
    /// The label is a string *sequence*, not a single literal: glibc spells it
    /// `__ASMNAME(cname)`, which expands to
    /// `__STRING(__USER_LABEL_PREFIX__) cname` — two adjacent literals, `""`
    /// and `"stpncpy"` on ELF. They concatenate as in any other C string
    /// context.
    fn parse_asm_label(&mut self) {
        while self.is_asm_keyword() {
            self.advance(); // consume __asm/__asm__

            // Expect '('
            if !self.is_special(b'(') {
                return;
            }
            self.advance(); // consume '('

            // Collect the string literals, and skip anything else so that a
            // shape we do not model still parses as it used to.
            let mut label = String::new();
            let mut depth = 1;
            while depth > 0 && !self.is_eof() {
                if self.is_special(b'(') {
                    depth += 1;
                } else if self.is_special(b')') {
                    depth -= 1;
                    if depth == 0 {
                        self.advance();
                        break;
                    }
                } else if depth == 1 {
                    if let TokenValue::String(s) = &self.current().value {
                        label.push_str(&payload_text(s));
                    }
                }
                self.advance();
            }

            // An empty label is not a rename. `__asm__("")` would ask for a
            // nameless symbol, which is not something GCC accepts either.
            if !label.is_empty() {
                self.pending_asm_label = Some(label);
            }
        }
    }

    /// Record, on whichever symbol is now bound to `name`, the facts about it
    /// that accumulate across every declaration in the translation unit.
    ///
    /// Two of them so far -- the GCC asm label and whether anything said
    /// `extern` -- and they share this helper because they share the problem
    /// that makes them awkward: a redeclaration, and in particular a
    /// definition, binds a *fresh* symbol that would not otherwise inherit
    /// what earlier declarations established, and the declaration that settles
    /// the question is allowed to come afterwards.
    ///
    /// Consuming the pending asm label means a declaration list gives each
    /// declarator only the label written on it: in `int a __asm__("x"), b;`
    /// only `a` is renamed.
    pub(super) fn settle_declaration_facts(
        &mut self,
        name: StringId,
        storage_class: TypeModifiers,
    ) {
        // C99 6.7.4p6 asks whether *any* declaration of this name says
        // `extern`, including ones not yet parsed, so accumulate rather than
        // overwrite. See `Symbol::has_extern_decl`.
        if storage_class.contains(TypeModifiers::EXTERN) {
            self.declared_extern_fns.insert(name);
        }
        if !storage_class.contains(TypeModifiers::INLINE) {
            self.declared_non_inline_fns.insert(name);
        }
        if let Some(id) = self.symbols.lookup_id(name, Namespace::Ordinary) {
            let sym = self.symbols.get_mut(id);
            sym.has_extern_decl |= self.declared_extern_fns.contains(&name);
            sym.has_non_inline_decl |= self.declared_non_inline_fns.contains(&name);
        }

        let label = match self.pending_asm_label.take() {
            Some(label) => {
                self.declared_asm_labels.insert(name, label.clone());
                Some(label)
            }
            None => self.declared_asm_labels.get(&name).cloned(),
        };
        if let (Some(label), Some(id)) = (label, self.symbols.lookup_id(name, Namespace::Ordinary))
        {
            self.symbols.get_mut(id).asm_label = Some(label);
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
            if let Some(name_id) = self.get_ident_id(self.current()) {
                match name_id {
                    crate::kw::VOLATILE | crate::kw::GNU_VOLATILE => {
                        is_volatile = true;
                        self.advance();
                    }
                    crate::kw::INLINE | crate::kw::GNU_INLINE => {
                        self.advance();
                    }
                    crate::kw::GOTO => {
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
            result.push_str(&crate::token::literal::literal_bytes(
                &crate::token::literal::parse_string_literal(s),
            ));
        }

        // Handle string concatenation (adjacent string literals)
        while self.peek() == TokenType::String {
            let token = self.consume();
            if let TokenValue::String(s) = &token.value {
                result.push_str(&crate::token::literal::literal_bytes(
                    &crate::token::literal::parse_string_literal(s),
                ));
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

    /// Accumulate the symbol-emission attributes from one attribute list.
    pub(super) fn merge_symbol_attrs(&mut self, attrs: &AttributeList) {
        let found = attrs.symbol_attrs();
        self.pending_symbol_attrs.weak |= found.weak;
        self.pending_symbol_attrs.used |= found.used;
        if found.section.is_some() {
            self.pending_symbol_attrs.section = found.section;
        }
        if found.visibility.is_some() {
            self.pending_symbol_attrs.visibility = found.visibility;
        }
    }

    /// `transparent_union` is a union attribute. gcc warns and ignores it
    /// anywhere else rather than rejecting, and so does c17 -- dropping it in
    /// silence would leave the program believing a rule was in force that was
    /// not.
    ///
    /// Shared by the two routes that can reach the mistake: on the
    /// struct-or-union specifier, and trailing after the declarator.
    pub(super) fn warn_transparent_union_ignored(&self, pos: Position) {
        if crate::diag::warning_group_enabled(ATTRIBUTE_WARNING) {
            diag::warning(
                pos,
                &gettext("'transparent_union' attribute ignored on a non-union type"),
            );
        }
    }

    /// Apply every type attribute held over from the declarator: the machine
    /// mode named by `mode(M)`, then `transparent_union`.
    ///
    /// Both are seen mid-declarator and can only land once the type is final,
    /// so every path that finishes a declarator calls this rather than
    /// remembering which attributes exist.
    pub(super) fn apply_pending_type_attrs(&mut self, typ: TypeId) -> TypeId {
        let typ = self.apply_pending_mode(typ);
        let typ = self.apply_pending_vector_size(typ);
        if let Some(pos) = self.pending_transparent_union.take() {
            if self.types.kind(typ) == TypeKind::Union {
                self.types.set_transparent_union(typ);
            } else {
                self.warn_transparent_union_ignored(pos);
            }
        }
        typ
    }

    /// Apply `__attribute__((vector_size(N)))` to a declared type.
    ///
    /// c17 gives such a type a vector's *storage* and not its arithmetic: it
    /// becomes an array of `N / sizeof(element)` elements, aligned to `N`.
    /// That is exactly the layout GCC gives it, so a struct or union holding
    /// one -- which is all glibc's `<link.h>` does with `La_x86_64_xmm` and
    /// its siblings -- lays out identically.
    ///
    /// What it deliberately does not get is element-wise `+`, `*` and the
    /// rest. Those need a real vector type in the IR and both backends. An
    /// array does not accept them, so the gap is a diagnostic at the point of
    /// use rather than arithmetic that silently runs on one element -- which
    /// is the failure the outright rejection was guarding against.
    fn apply_pending_vector_size(&mut self, typ: TypeId) -> TypeId {
        let Some((bytes, pos)) = self.pending_vector_size.take() else {
            return typ;
        };
        let elem_size = self.types.size_bytes(typ);
        if elem_size == 0 || !self.types.is_arithmetic(typ) {
            diag::error(pos, "'vector_size' requires an arithmetic element type");
            return typ;
        }
        // The same ceiling `derive_array_type` applies, and for the same
        // reason: this interns an array directly, so nothing else would catch
        // an absurd width. Without it `vector_size(4294967296)` quietly
        // produced a four-gigabyte type, and a width near `u64::MAX` made
        // `next_power_of_two` overflow -- a panic in a debug build.
        if bytes > TypeTable::MAX_OBJECT_BYTES as u64 {
            diag::error_args(
                pos,
                "'vector_size' of {0} exceeds the maximum object size of {1} bytes",
                &[&bytes.to_string(), &TypeTable::MAX_OBJECT_BYTES.to_string()],
            );
            return typ;
        }
        if bytes % elem_size as u64 != 0 {
            let named = self.types.format_type(typ, Some(self.idents));
            diag::error_args(
                pos,
                "'vector_size' of {0} is not a multiple of sizeof({1})",
                &[&bytes.to_string(), &named],
            );
            return typ;
        }
        let count = bytes / elem_size as u64;
        let mut vector = Type {
            kind: TypeKind::Array,
            base: Some(typ),
            array_size: Some(count as usize),
            ..Default::default()
        };
        // A vector aligns to its width rounded up to a power of two, capped
        // at sixteen -- which is what GCC does by default on both targets c17
        // supports. A 32-byte vector therefore aligns to 16, not 32; only
        // `-mavx2` raises the cap, and c17 does not model that. Measured
        // against gcc across widths 4 through 128 rather than assumed, because
        // "aligns to its own width" is the obvious rule and is wrong.
        //
        // An `aligned(n)` written alongside takes precedence, which is what
        // `<link.h>` does: `__vector_size__(32), __aligned__(16)`. It has to
        // be applied here rather than left to the later attribute pass, since
        // an explicit alignment may not reduce one already recorded.
        const MAX_VECTOR_ALIGN: u32 = 16;
        vector.explicit_align = Some(match self.pending_attr_align.take() {
            Some(written) => written,
            None => (bytes.next_power_of_two() as u32).min(MAX_VECTOR_ALIGN),
        });
        self.types.intern(vector)
    }

    /// Apply `__attribute__((mode(M)))` to a declared type.
    ///
    /// A machine mode names a width, and the attribute replaces the declared
    /// type with the one of that width in the same family -- keeping the
    /// declared signedness, so `typedef unsigned u8 __attribute__((mode(QI)));`
    /// is unsigned and the `int` spelling is signed. glibc's `register_t` is
    /// `__mode__(__word__)`, which is why leaving this unimplemented sized it
    /// 4 bytes against gcc's 8.
    ///
    /// An unrecognised mode -- `V4SF` and the other vector modes, which need
    /// vector types -- keeps the warning, because ignoring it would silently
    /// change what the program computes.
    fn apply_pending_mode(&mut self, typ: TypeId) -> TypeId {
        let Some((mode, pos)) = self.pending_mode.take() else {
            return typ;
        };
        let unsigned = self.types.is_unsigned(typ);
        let t = &self.types;
        let mapped = match mode.as_str() {
            // Integer modes, named for their width in bytes.
            "QI" => Some(if unsigned { t.uchar_id } else { t.schar_id }),
            "HI" => Some(if unsigned { t.ushort_id } else { t.short_id }),
            "SI" => Some(if unsigned { t.uint_id } else { t.int_id }),
            "DI" | "word" | "pointer" => Some(if unsigned { t.ulong_id } else { t.long_id }),
            "TI" => Some(if unsigned { t.uint128_id } else { t.int128_id }),
            // Floating modes. `XF` is the x87 extended format and `TF` IEEE
            // binary128 -- both sixteen bytes on x86-64 and *not*
            // interchangeable, so they map to their own types rather than to a
            // width.
            //
            // The binary128 modes are offered only where the type is. c17 does
            // not support `_Float128` on macOS and deliberately predefines no
            // `__FLT128_*` family there, so that `<float.h>` cannot advertise a
            // type whose every operation fails to link. Handing the same type
            // back through a mode attribute defeated that: `mode(TF)` on Apple
            // arm64 produced a type needing `__divtf3` and `__multf3`, which
            // that platform has no equivalent of. `has_float128` is the one
            // condition both places ask.
            "HF" => Some(t.float16_id),
            "SF" => Some(t.float_id),
            "DF" => Some(t.double_id),
            "XF" => Some(t.longdouble_id),
            "TF" if t.has_float128() => Some(t.float128_id),
            // Complex modes, named for the format of each half. glibc's
            // <bits/floatn.h> declares `__cfloat128` with `mode(TC)`, which was
            // 285 of the warnings a CPython build produced.
            "HC" => Some(t.complex_float16_id),
            "SC" => Some(t.complex_float_id),
            "DC" => Some(t.complex_double_id),
            "XC" => Some(t.complex_longdouble_id),
            "TC" if t.has_float128() => Some(t.complex_float128_id),
            _ => None,
        };
        match mapped {
            Some(m) => {
                // The declared type's qualifiers survive; only its width and
                // family change.
                let quals = self.types.modifiers(typ)
                    & (TypeModifiers::CONST | TypeModifiers::VOLATILE | TypeModifiers::ATOMIC);
                if quals.is_empty() {
                    m
                } else {
                    let mut q = self.types.get(m).clone();
                    q.modifiers |= quals;
                    self.types.intern(q)
                }
            }
            None => {
                if diag::warning_group_enabled(ATTRIBUTE_WARNING) {
                    diag::warning_args(
                        pos,
                        "'mode({0})' is not implemented; the declared type is used unchanged",
                        &[&mode],
                    );
                }
                typ
            }
        }
    }

    /// Apply alignment from __attribute__((aligned(N))) to pending_alignas.
    /// Merges max: multiple aligned attrs → strictest wins.
    fn apply_attribute_alignment(&mut self, attrs: &AttributeList) {
        if let Some(align) = attrs.get_alignment() {
            if align > 0 && align.is_power_of_two() {
                if let Some(existing) = self.pending_alignas {
                    self.pending_alignas = Some(existing.max(align));
                } else {
                    self.pending_alignas = Some(align);
                }
            }
        }
    }

    /// `skip_extensions` for the position just after a declarator, where an
    /// `aligned` attribute names that declarator alone rather than the whole
    /// declaration.
    pub(super) fn skip_extensions_after_declarator(&mut self) {
        self.skip_extensions_inner(true)
    }

    /// Parse __attribute__, __asm, and nullability extensions, wiring aligned() to pending_alignas
    pub(super) fn skip_extensions(&mut self) {
        self.skip_extensions_inner(false)
    }

    fn skip_extensions_inner(&mut self, declarator_scoped: bool) {
        loop {
            if self.is_attribute_keyword() {
                let attrs = self.parse_attributes();
                if declarator_scoped {
                    if let Some(align) = attrs.get_alignment() {
                        if align > 0 && align.is_power_of_two() {
                            self.pending_declarator_align = Some(
                                self.pending_declarator_align
                                    .map_or(align, |a| a.max(align)),
                            );
                        }
                    }
                } else {
                    self.apply_attribute_alignment(&attrs);
                }
                if attrs.has_transparent_union() {
                    self.pending_transparent_union = Some(self.current_pos());
                }
                self.merge_symbol_attrs(&attrs);
                let fn_attrs = attrs.function_attrs();
                self.pending_fn_attrs.merge(&fn_attrs);
            } else if self.is_asm_keyword() {
                self.parse_asm_label();
            } else if self.is_nullability_qualifier() {
                self.advance();
            } else {
                break;
            }
        }
    }
}

// Statement Parsing

impl Parser<'_> {
    pub fn parse_statement(&mut self) -> ParseResult<Stmt> {
        // Check for keywords
        if self.peek() == TokenType::Ident {
            if let Some(name_id) = self.get_ident_id(self.current()) {
                match name_id {
                    crate::kw::IF => return self.parse_if_stmt(),
                    crate::kw::WHILE => return self.parse_while_stmt(),
                    crate::kw::DO => return self.parse_do_while_stmt(),
                    crate::kw::FOR => return self.parse_for_stmt(),
                    crate::kw::RETURN => return self.parse_return_stmt(),
                    crate::kw::BREAK => {
                        let pos = self.current_pos();
                        self.advance();
                        self.expect_special(b';')?;
                        return Ok(Stmt::Break(pos));
                    }
                    crate::kw::CONTINUE => {
                        let pos = self.current_pos();
                        self.advance();
                        self.expect_special(b';')?;
                        return Ok(Stmt::Continue(pos));
                    }
                    crate::kw::GOTO => {
                        let pos = self.current_pos();
                        self.advance();
                        // GNU computed goto: `goto *expr;`
                        if self.is_special(b'*') {
                            self.advance();
                            let target = self.parse_expression()?;
                            self.expect_special(b';')?;
                            return Ok(Stmt::GotoIndirect { target, pos });
                        }
                        let name = self.expect_identifier()?;
                        self.expect_special(b';')?;
                        return Ok(Stmt::Goto { name, pos });
                    }
                    crate::kw::SWITCH => return self.parse_switch_stmt(),
                    crate::kw::CASE => return self.parse_case_label(),
                    crate::kw::DEFAULT => return self.parse_default_label(),
                    // GCC extended inline assembly
                    crate::kw::ASM | crate::kw::GNU_ASM | crate::kw::GNU_ASM2 => {
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
            let pos = self.current_pos();
            let name = self.expect_identifier()?;
            if self.is_special(b':') {
                self.advance();
                let stmt = self.parse_statement()?;
                return Ok(Stmt::Label {
                    name,
                    stmt: Box::new(stmt),
                    pos,
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

    fn parse_if_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'if'
        self.expect_special(b'(')?;
        let cond = self.parse_expression()?;
        self.expect_special(b')')?;
        let then_stmt = self.parse_statement()?;

        let else_stmt = if self.peek() == TokenType::Ident {
            if let Some(name_id) = self.get_ident_id(self.current()) {
                if name_id == crate::kw::ELSE {
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

    fn parse_switch_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'switch'
        self.expect_special(b'(')?;
        let expr = self.parse_expression()?;
        self.expect_special(b')')?;
        let body = self.parse_switch_body()?;
        Ok(Stmt::Switch {
            expr,
            body: Box::new(body),
        })
    }

    /// Parse a `switch` body, which C17 6.8.4 says is one statement.
    ///
    /// `case E : statement` is a single *labeled statement* in the grammar, but
    /// the AST flattens the label into a sibling marker -- `Stmt::Case` carries
    /// the value and not the statement it labels. That flattening is only sound
    /// inside a block, where the marker and its statement stay adjacent items of
    /// one list. Given a non-compound body there is room for exactly one
    /// statement, so the marker took the whole body and the labeled statement
    /// escaped the switch to become the *following* statement of the enclosing
    /// block -- reached unconditionally, whatever the controlling expression.
    /// `switch (x) case 1: return 2;` returned 2 for every `x`.
    ///
    /// So rebuild the block the flattening assumes. A body that opens a block,
    /// or that carries no label at all, is returned exactly as before; only the
    /// labeled non-compound form gains the wrapper.
    fn parse_switch_body(&mut self) -> ParseResult<Stmt> {
        if self.is_special(b'{') {
            return self.parse_statement();
        }

        let mut items = Vec::new();
        loop {
            let stmt = self.parse_statement()?;
            let is_label = matches!(stmt, Stmt::Case(..) | Stmt::Default(_));
            items.push(BlockItem::Statement(Box::new(stmt)));
            // A label prefixes a statement, so one more must follow it. Anything
            // else ends the body. The `}`/EOF guard keeps a body that is nothing
            // but a label -- which no conforming program contains -- from
            // running past the end of its enclosing block.
            if !is_label || self.is_special(b'}') || self.is_eof() {
                break;
            }
        }

        if items.len() == 1 {
            let BlockItem::Statement(only) = items.remove(0) else {
                unreachable!("parse_switch_body pushes only statements")
            };
            return Ok(*only);
        }
        Ok(Stmt::Block(items))
    }

    /// Parse a case label, including the GNU range form `case lo ... hi:`.
    ///
    /// GCC requires whitespace around the `...`: `case 1...9:` lexes as one
    /// pp-number and is rejected there too ("too many decimal points in
    /// number"), so only the spaced form is accepted here as well.
    fn parse_case_label(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'case'
        let expr = self.parse_conditional_expr()?;
        let high = if self.is_special_token(SpecialToken::Ellipsis) {
            self.advance();
            Some(self.parse_conditional_expr()?)
        } else {
            None
        };
        self.expect_special(b':')?;
        Ok(Stmt::Case(expr, high))
    }

    fn parse_default_label(&mut self) -> ParseResult<Stmt> {
        let pos = self.current_pos();
        self.advance(); // consume 'default'
        self.expect_special(b':')?;
        Ok(Stmt::Default(pos))
    }

    /// Parse block items (declarations and statements) until closing brace
    fn parse_block_items(&mut self) -> ParseResult<Vec<BlockItem>> {
        let mut items = Vec::new();
        while !self.is_special(b'}') && !self.is_eof() {
            if self.is_declaration_start() {
                let decl = self.parse_declaration_and_bind()?;
                items.push(BlockItem::Declaration(decl));
            } else {
                let stmt = self.parse_statement()?;
                items.push(BlockItem::Statement(Box::new(stmt)));
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
    pub(super) fn parse_block_stmt_no_scope(&mut self) -> ParseResult<Stmt> {
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
                BlockItem::Statement(stmt) if matches!(stmt.as_ref(), Stmt::Expr(_)) => {
                    let Stmt::Expr(expr) = *stmt else {
                        unreachable!()
                    };
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

    pub(super) fn is_declaration_start(&self) -> bool {
        if self.peek() != TokenType::Ident {
            return false;
        }

        if let Some(name_id) = self.get_ident_id(self.current()) {
            if crate::kw::has_tag(name_id, crate::kw::DECL_START) {
                return true;
            }
            // Also check for typedef names
            self.symbols.lookup_typedef(name_id).is_some()
        } else {
            false
        }
    }

    /// The `f64` value of a constant floating subexpression.
    ///
    /// Only reached from a comparison, whose result is an integer -- the
    /// arithmetic itself is folded at full width by the linearizer's
    /// `eval_const_float_expr` for anything that survives to code generation.
    /// `f64` is enough to decide an ordering that `i128` truncation was
    /// getting wrong.
    pub(crate) fn eval_const_f64(&self, scope: ConstScope, expr: &Expr) -> Option<f64> {
        match &expr.kind {
            ExprKind::FloatLit(v) => Some(v.to_f64()),
            ExprKind::IntLit(v) => Some(*v as f64),
            // `CharLit` is an `i64` whose signedness the lexer has already
            // resolved, so `'\x80'` is -128 where `char` is signed. Rounding
            // it through `u32` made that 4294967168.0.
            ExprKind::CharLit(c) => Some(*c as f64),
            ExprKind::Cast { expr: inner, .. } => {
                let v = self.eval_const_f64(scope, inner)?;
                // A cast to an integer type truncates before the comparison.
                match expr.typ {
                    Some(t) if self.types.is_integer(t) => Some(v.trunc()),
                    _ => Some(v),
                }
            }
            ExprKind::Unary {
                op: UnaryOp::Neg,
                operand,
            } => Some(-self.eval_const_f64(scope, operand)?),
            ExprKind::Binary { op, left, right } => {
                let l = self.eval_const_f64(scope, left)?;
                let r = self.eval_const_f64(scope, right)?;
                match op {
                    BinaryOp::Add => Some(l + r),
                    BinaryOp::Sub => Some(l - r),
                    BinaryOp::Mul => Some(l * r),
                    BinaryOp::Div if r != 0.0 => Some(l / r),
                    _ => None,
                }
            }
            // Anything else that is an integer constant expression converts to
            // one. `sizeof`, `_Alignof`, an enumerator and a conditional all
            // reach here, and each was answered "not a constant expression"
            // for want of an arm -- so `_Static_assert(sizeof(int) < 4.5, "")`
            // was rejected although both operands are perfectly constant.
            _ => crate::constexpr::eval(self, scope, expr).map(|v| v as f64),
        }
    }

    /// Evaluate an integer constant expression: array bounds, enumerators,
    /// `case` labels, bit-field widths, `_Static_assert`.
    ///
    /// The walk itself lives in [`crate::constexpr`], shared with the
    /// linearizer's static-initializer folding. The parser answers only
    /// [`ConstScope::Standard`]: it has no emitted globals to read a `const`
    /// object's value out of, and no context here would accept one anyway.
    pub(crate) fn eval_const_expr(&self, expr: &Expr) -> Option<i128> {
        crate::constexpr::eval(self, ConstScope::Standard, expr)
    }

    /// Build the symbol for a declared name, choosing its kind from its type.
    ///
    /// A declarator whose type is a function declares a *function*, whatever
    /// company it keeps -- `int f(int), g(int);` at file scope, or
    /// `void h(void) { int g(int); }` inside a block. Both of those binders
    /// took "not a function definition" to mean "variable", and `is_lvalue`
    /// asks the symbol's *kind* rather than its type, so a function bound as a
    /// variable was assignable: `g = 0;` compiled and stored through the
    /// function's own address.
    ///
    /// `_Alignas` does not apply to a function, so an alignment is dropped
    /// here rather than recorded against one.
    pub(super) fn declared_symbol(
        &self,
        name: StringId,
        typ: TypeId,
        align: Option<u32>,
    ) -> Symbol {
        if self.types.kind(typ) == TypeKind::Function {
            return Symbol::function(name, typ, self.symbols.depth());
        }
        Symbol::variable(name, typ, self.symbols.depth()).with_align(align)
    }

    /// Validate explicit alignment against natural alignment (C11 6.7.5)
    ///
    /// Returns the validated explicit alignment, or error if alignment is weaker than natural.
    /// Returns None if no explicit alignment was specified.
    /// Also propagates alignment from typedef's explicit_align.
    pub(super) fn validated_explicit_align(&mut self, typ: TypeId) -> ParseResult<Option<u32>> {
        // Combine pending_alignas (from _Alignas / __attribute__) with type's explicit_align
        let type_align = self.types.get(typ).explicit_align;
        // An attribute written after this declarator belongs to it alone, so
        // take it here and leave nothing behind for the next one.
        let declaration_align = match (self.pending_alignas, self.pending_declarator_align.take()) {
            (Some(a), Some(b)) => Some(a.max(b)),
            (a, b) => a.or(b),
        };
        let effective = match (declaration_align, type_align) {
            (Some(a), Some(b)) => Some(a.max(b)),
            (Some(a), None) => Some(a),
            (None, Some(b)) => Some(b),
            (None, None) => None,
        };

        match effective {
            None => Ok(None),
            Some(explicit) => {
                // For C11 _Alignas validation, don't reject typedef alignment that
                // exceeds the natural alignment of the underlying type (that's the point).
                // Only reject explicit _Alignas that reduces below natural.
                if declaration_align.is_some() {
                    let natural = self.types.natural_alignment(typ) as u32;
                    if explicit < natural {
                        return Err(ParseError::new(
                            format!(
                                "_Alignas({}) cannot reduce alignment below natural alignment {}",
                                explicit, natural
                            ),
                            self.current_pos(),
                        ));
                    }
                }
                Ok(Some(explicit))
            }
        }
    }
}

/// The parser's half of the shared C17 6.6 walk.
///
/// [`crate::constexpr`] owns the walk; what differs between the two hosts is
/// only what an identifier means and how a floating subexpression folds.
impl crate::constexpr::ConstEnv for Parser<'_> {
    fn types(&self) -> &TypeTable {
        self.types
    }

    /// An enumeration constant is the only identifier with a value in the
    /// parser: a `const` object's value lives in an emitted global, which does
    /// not exist yet here, and no parse-time context would accept one.
    fn ident_value(&self, sym: crate::symbol::SymbolId, _scope: ConstScope) -> Option<i128> {
        let symbol = self.symbols.get(sym);
        symbol.is_enum_constant().then_some(symbol.enum_value)?
    }

    fn struct_of(&self, typ: TypeId) -> TypeId {
        self.resolve_struct_type(typ)
    }

    fn float_value(&self, scope: ConstScope, expr: &Expr) -> Option<f64> {
        self.eval_const_f64(scope, expr)
    }
}
