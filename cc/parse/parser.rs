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

use super::ast::{
    AsmOperand, BinaryOp, BlockItem, Declaration, Designator, Expr, ExprKind, ExternalDecl,
    ForInit, FunctionDef, InitDeclarator, InitElement, Parameter, Stmt, TranslationUnit, UnaryOp,
};
use crate::constexpr::ConstScope;
use crate::diag;
use crate::strings::StringId;
use crate::symbol::{Namespace, Symbol, SymbolId, SymbolKind, SymbolTable};
use crate::token::lexer::{IdentTable, Position, SpecialToken, Token, TokenType, TokenValue};
use crate::token::preprocess::PackAction;
use crate::types::{
    CompositeType, EnumConstant, StructMember, Type, TypeId, TypeKind, TypeModifiers, TypeTable,
};
use gettextrs::gettext;
use std::collections::{BTreeMap, HashMap};
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

/// The type specifiers seen while parsing one declaration specifier list.
///
/// C17 6.7.2p2 does not let a declaration name a type by accumulation: the
/// specifiers must together be one of a fixed list of combinations. The
/// specifier loop only tracked the resulting kind, each keyword overwriting
/// the last, so an impossible combination named whichever type came last --
/// `float double x;` was a `double`, `void int y;` an object of type void with
/// a size of 4, and `long long long z;` a `long long`. All three were accepted
/// silently, which makes this a wrong program rather than the missing
/// diagnostic it was first filed as.
///
/// `short`, `long`, `signed` and `unsigned` are counted rather than recorded
/// as data types, because the valid combinations pair them with `int`, and
/// `long` with `double` -- they qualify a data type instead of naming one.
#[derive(Default)]
struct SpecifierTally {
    /// Data-type specifiers, in source order, under their canonical spelling.
    /// More than one is always a constraint violation.
    data_types: Vec<(&'static str, Position)>,
    short_count: u32,
    long_count: u32,
    signed_count: u32,
    unsigned_count: u32,
    /// Where the most recent `short`/`long`/`signed`/`unsigned` appeared, for
    /// the diagnostic that reports it against an incompatible data type.
    last_size: Option<(&'static str, Position)>,
    last_sign: Option<(&'static str, Position)>,
    /// Storage-class specifiers, in source order. 6.7.1p2 permits at most one
    /// -- `_Thread_local` excepted, which may accompany `static` or `extern`.
    storage_classes: Vec<(&'static str, Position)>,
}

impl SpecifierTally {
    /// Whether an *alias* type specifier appearing now is the name of what is
    /// being declared rather than a second data type.
    ///
    /// A C library may define one of these as a typedef of its own: glibc's
    /// <bits/floatn-common.h> has `typedef float _Float32;` whenever the
    /// compiler does not claim native support. Once a data type has been
    /// given, `_Float32` is the declarator -- the same rule `typeof` needs --
    /// and without it that typedef is two data types and an error.
    fn alias_is_declarator_name(&self) -> bool {
        !self.data_types.is_empty()
    }

    fn note_data_type(&mut self, name: &'static str, pos: Position) {
        self.data_types.push((name, pos));
    }

    fn note_size(&mut self, name: &'static str, pos: Position) {
        self.last_size = Some((name, pos));
        if name == "short" {
            self.short_count += 1;
        } else {
            self.long_count += 1;
        }
    }

    /// `_Thread_local` is deliberately not recorded: 6.7.1p2 lets it appear
    /// with `static` or `extern`, and gcc accepts both orders, so counting it
    /// would reject `static _Thread_local int x;`.
    fn note_storage_class(&mut self, name: &'static str, pos: Position) {
        self.storage_classes.push((name, pos));
    }

    fn note_sign(&mut self, name: &'static str, pos: Position) {
        self.last_sign = Some((name, pos));
        if name == "signed" {
            self.signed_count += 1;
        } else {
            self.unsigned_count += 1;
        }
    }

    /// Report every way this specifier list fails C17 6.7.2p2.
    ///
    /// Reporting rather than returning an error: a constraint violation needs a
    /// diagnostic (C17 5.1.1.3), and the parser recovers with the type it had
    /// already built, so one bad declaration does not cascade.
    fn check(&self) {
        if let Some((second, pos)) = self.storage_classes.get(1) {
            let first = self.storage_classes[0].0;
            if first == *second {
                diag::error_args(*pos, "duplicate '{0}'", &[second]);
            } else {
                diag::error_args(
                    *pos,
                    "multiple storage classes in declaration specifiers ('{0}' and '{1}')",
                    &[first, second],
                );
            }
            return;
        }

        if let Some((second, pos)) = self.data_types.get(1) {
            diag::error_args(
                *pos,
                "two or more data types in declaration specifiers ('{0}' and '{1}')",
                &[self.data_types[0].0, second],
            );
            return;
        }

        if self.signed_count > 0 && self.unsigned_count > 0 {
            if let Some((_, pos)) = self.last_sign {
                diag::error(
                    pos,
                    "both 'signed' and 'unsigned' in declaration specifiers",
                );
            }
        } else if self.signed_count > 1 || self.unsigned_count > 1 {
            if let Some((name, pos)) = self.last_sign {
                diag::error_args(pos, "duplicate '{0}'", &[name]);
            }
        }

        if self.short_count > 0 && self.long_count > 0 {
            if let Some((_, pos)) = self.last_size {
                diag::error(pos, "both 'short' and 'long' in declaration specifiers");
            }
        } else if self.short_count > 1 {
            if let Some((_, pos)) = self.last_size {
                diag::error(pos, "duplicate 'short'");
            }
        } else if self.long_count > 2 {
            if let Some((_, pos)) = self.last_size {
                diag::error(pos, "'long long long' is too long for c17");
            }
        }

        let data_type = self.data_types.first().map(|(name, _)| *name);

        // `short` and `long` pair with `int`; `long` alone also pairs with
        // `double`. Nothing else.
        let size_ok = match data_type {
            None | Some("int") => true,
            Some("double") => self.short_count == 0 && self.long_count == 1,
            _ => false,
        };
        if !size_ok {
            if let (Some((size, pos)), Some(data)) = (self.last_size, data_type) {
                diag::error_args(
                    pos,
                    "both '{0}' and '{1}' in declaration specifiers",
                    &[size, data],
                );
            }
        }

        // `signed` and `unsigned` pair with the integer types only.
        let sign_ok = matches!(
            data_type,
            None | Some("int") | Some("char") | Some("__int128")
        );
        if !sign_ok {
            if let (Some((sign, pos)), Some(data)) = (self.last_sign, data_type) {
                diag::error_args(
                    pos,
                    "both '{0}' and '{1}' in declaration specifiers",
                    &[sign, data],
                );
            }
        }
    }
}

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

/// Result of parsing a declarator: (name, type, VLA expressions, raw function parameters)
type DeclaratorResult = (StringId, TypeId, Vec<Expr>, Option<Vec<RawParam>>);

/// Function parameter type info: (type IDs, is_variadic)
/// The signature a function declarator's `( ... )` suffix contributes.
///
/// `prototyped` is false for `()` and for a K&R identifier list, which C17
/// 6.7.6.3p14 makes a different type from `(void)` rather than a shorter one.
struct FuncSignature {
    param_types: Vec<TypeId>,
    variadic: bool,
    prototyped: bool,
}

impl FuncSignature {
    /// The function type this signature makes of a return type.
    fn into_type(self, return_type: TypeId) -> Type {
        if self.prototyped {
            Type::function(return_type, self.param_types, self.variadic, false)
        } else {
            Type::function_no_prototype(return_type, false)
        }
    }
}

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

    /// Whether `__attribute__((noinline))` is present.
    pub fn has_noinline(&self) -> bool {
        self.has_attr("noinline")
    }

    /// Whether `__attribute__((always_inline))` is present.
    pub fn has_always_inline(&self) -> bool {
        self.has_attr("always_inline")
    }

    /// `__attribute__((transparent_union))`, in either spelling.
    fn has_transparent_union(&self) -> bool {
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

    /// Get alignment from __attribute__((aligned(N))) or __attribute__((aligned))
    /// Returns Some(alignment) if found, None otherwise.
    /// Per GCC: aligned with no args defaults to 16 ("max useful alignment").
    /// The `weak`, `used`, `section(...)` and `visibility(...)` requests in
    /// this list. All four were parsed and dropped before, while
    /// `__has_attribute` answered 1 for each.
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
    /// The machine mode named by `__attribute__((mode(M)))` on the declaration
    /// being parsed, and where it was written. Held like `pending_alignas`
    /// because the attribute is seen while the declarator is being consumed and
    /// can only be applied once the type is final.
    pending_mode: Option<(String, Position)>,

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
    pending_transparent_union: Option<Position>,
    /// File-scope object definitions whose type was incomplete when parsed.
    /// Judged at end of translation unit -- see
    /// [`Self::check_deferred_incomplete_definitions`].
    tentative_definitions: Vec<(TypeId, Position)>,
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
    pending_symbol_attrs: crate::parse::ast::SymbolAttrs,
    /// Emission-affecting function attributes seen so far in the declaration
    /// being parsed. Accumulated like `pending_alignas` because a
    /// `constructor` / `noinline` attribute may appear before the declaration
    /// specifiers, between the type and the declarator, or after the
    /// parameter list. Cleared at the start of each external declaration.
    pending_fn_attrs: crate::parse::ast::FunctionAttrs,
    /// Every function attribute seen for a given name anywhere in the
    /// translation unit.
    ///
    /// GCC applies `constructor` / `destructor` / `noinline` to the function
    /// however it was declared, so writing the attribute on a prototype and
    /// leaving the definition bare -- `static void f(void)
    /// __attribute__((constructor));` followed by `static void f(void) {}` --
    /// still makes `f` a constructor. Reading the definition's own attributes
    /// alone would silently drop it.
    declared_fn_attrs: BTreeMap<StringId, crate::parse::ast::FunctionAttrs>,
    /// The GCC asm label seen in the declaration being parsed, awaiting the
    /// declarator it renames. Accumulated like `pending_fn_attrs` because
    /// `__asm__("...")` can appear before or after an `__attribute__` --
    /// glibc's `__REDIRECT_NTH` writes it before `__THROW`.
    pending_asm_label: Option<String>,
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
    saw_explicit_type: bool,
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
    vm_typedefs: HashMap<SymbolId, u32>,
    /// The extents of the variably modified typedef named by the declaration
    /// specifiers just parsed, awaiting the declarators they apply to.
    ///
    /// Held like `pending_alignas`: the specifier list is parsed before the
    /// declarator list, and every declarator in the declaration shares it.
    pending_vm_typedef_dims: Option<Vec<Expr>>,
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
    fn current_pack(&mut self) -> Option<u32> {
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

    /// Whether the token *after* the current one is `(`.
    ///
    /// Used to tell a keyword being applied from the same word being used as
    /// an ordinary identifier.
    fn next_token_is_open_paren(&self) -> bool {
        match self.tokens.get(self.pos + 1) {
            Some(t) => matches!(t.value, TokenValue::Special(v) if v == b'(' as u32),
            None => false,
        }
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
            let found = match &self.current().value {
                TokenValue::Ident(id) => {
                    format!("identifier '{}'", self.idents.get_opt(*id).unwrap_or("?"))
                }
                TokenValue::Special(v) => format!("'{}'", char::from_u32(*v).unwrap_or('?')),
                other => format!("{:?}", other),
            };
            Err(ParseError::new(
                format!("expected '{}', found {}", c as char, found),
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

    /// Consume the identifier naming a declarator, rejecting keywords.
    ///
    /// Separate from [`Parser::expect_identifier`], which has eighteen callers
    /// covering labels, struct tags, member references and `goto` targets --
    /// all of which live in their own namespaces and may legitimately be
    /// spelled with a word that is reserved here. Only a *declarator* name is
    /// constrained, so only the declarator sites use this.
    fn expect_declarator_name(&mut self) -> ParseResult<StringId> {
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
    fn is_grouped_declarator(&mut self) -> bool {
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
    pub(crate) fn is_eof(&self) -> bool {
        matches!(self.peek(), TokenType::StreamEnd)
    }

    /// Check if current token is __attribute__ or __attribute
    fn is_attribute_keyword(&self) -> bool {
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
                        label.push_str(s);
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
    fn settle_declaration_facts(&mut self, name: StringId, storage_class: TypeModifiers) {
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
            result.push_str(&Self::literal_bytes(&Self::parse_string_literal(s)));
        }

        // Handle string concatenation (adjacent string literals)
        while self.peek() == TokenType::String {
            let token = self.consume();
            if let TokenValue::String(s) = &token.value {
                result.push_str(&Self::literal_bytes(&Self::parse_string_literal(s)));
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
    fn merge_symbol_attrs(&mut self, attrs: &AttributeList) {
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
    fn warn_transparent_union_ignored(&self, pos: Position) {
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
    fn apply_pending_type_attrs(&mut self, typ: TypeId) -> TypeId {
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
    fn skip_extensions_after_declarator(&mut self) {
        self.skip_extensions_inner(true)
    }

    /// Parse __attribute__, __asm, and nullability extensions, wiring aligned() to pending_alignas
    fn skip_extensions(&mut self) {
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

// ============================================================================
// Statement Parsing
// ============================================================================

impl Parser<'_> {
    /// Parse a statement
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

    /// Parse an if statement
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
            let is_label = matches!(stmt, Stmt::Case(_) | Stmt::Default(_));
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

    /// Parse a case label
    fn parse_case_label(&mut self) -> ParseResult<Stmt> {
        self.advance(); // consume 'case'
        let expr = self.parse_conditional_expr()?;
        self.expect_special(b':')?;
        Ok(Stmt::Case(expr))
    }

    /// Parse a default label
    fn parse_default_label(&mut self) -> ParseResult<Stmt> {
        let pos = self.current_pos();
        self.advance(); // consume 'default'
        self.expect_special(b':')?;
        Ok(Stmt::Default(pos))
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

    /// Check if current position starts a declaration
    fn is_declaration_start(&self) -> bool {
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
    /// The element count an array takes from a string-literal initializer:
    /// the characters plus the terminating null.
    ///
    /// One C byte per `char`, so count chars rather than bytes: `len()` is the
    /// UTF-8 encoded length, which over-counts every byte at or above 0x80 --
    /// `char a[] = "\x80";` came out as 3 bytes, not 2. `char16_t`/`char32_t`
    /// literals already carry code units, so their unit count is the element
    /// count; the *byte* size follows from the element type.
    pub(crate) fn string_initializer_len(&self, init: &Expr) -> Option<usize> {
        match &init.kind {
            ExprKind::StringLit(s) | ExprKind::WideStringLit(s) => Some(s.chars().count() + 1),
            ExprKind::Utf16StringLit(units) => Some(units.len() + 1),
            ExprKind::Utf32StringLit(units) => Some(units.len() + 1),
            _ => None,
        }
    }

    /// The string literal inside `{ ... }`, when that is what the braces hold.
    ///
    /// C17 6.7.9p14 lets the string literal initializing a character array be
    /// enclosed in braces, so `char b[] = {"hi"};` declares `char[3]` and
    /// copies the characters in. Counting the initializer list instead gives
    /// an array of one element holding a pointer's worth of nothing.
    ///
    /// Gated on the element type, because the same shape means something else
    /// one level up: `const char *p[] = {"aa", "bbb"}` is an array of two
    /// pointers, and `char names[3][4] = {"Sun"}` an array of arrays.
    pub(crate) fn braced_string_initializer<'a>(
        &self,
        elem_type: TypeId,
        elements: &'a [InitElement],
    ) -> Option<&'a Expr> {
        if !self.types.is_integer(elem_type) {
            return None;
        }
        let [only] = elements else { return None };
        if !only.designators.is_empty() {
            return None;
        }
        matches!(
            only.value.kind,
            ExprKind::StringLit(_)
                | ExprKind::WideStringLit(_)
                | ExprKind::Utf16StringLit(_)
                | ExprKind::Utf32StringLit(_)
        )
        .then_some(only.value.as_ref())
    }

    /// Report an initializer list with more elements than the object it
    /// initializes can hold (C17 6.7.9p2).
    ///
    /// Deliberately narrow, because the count is only unambiguous in the
    /// simple cases. A designator places an element anywhere, so a list
    /// containing one is left alone. Brace elision lets an aggregate member
    /// consume several consecutive elements -- `struct P p[2] = {1,2,3,4}`
    /// fills two two-field structs -- so only aggregates whose elements or
    /// members are all scalars are counted. A union takes one initializer,
    /// whatever it holds, and a flexible array member has no bound at all.
    ///
    /// Everything skipped is a missed warning rather than a wrong one.
    fn check_excess_initializers(&self, typ: TypeId, init: &Expr) {
        let ExprKind::InitList { elements } = &init.kind else {
            return;
        };
        if elements.iter().any(|e| !e.designators.is_empty()) {
            return;
        }

        match self.types.kind(typ) {
            TypeKind::Array => {
                let Some(elem) = self.types.base_type(typ) else {
                    return;
                };
                if !self.types.is_scalar(elem) {
                    return;
                }
                // An absent or zero size is an array whose bound came from
                // this very initializer, so it cannot overflow.
                let Some(capacity) = self.types.array_size(typ).filter(|&n| n > 0) else {
                    return;
                };
                if elements.len() > capacity {
                    diag::warning(init.pos, &gettext("excess elements in array initializer"));
                }
            }
            TypeKind::Struct => {
                let Some(comp) = self.types.composite(typ) else {
                    return;
                };
                if comp.members.is_empty()
                    || comp.members.iter().any(|m| !self.types.is_scalar(m.typ))
                {
                    return;
                }
                if elements.len() > comp.members.len() {
                    diag::warning(init.pos, &gettext("excess elements in struct initializer"));
                }
            }
            TypeKind::Union => {}
            _ => {
                // A scalar may be written in braces, but only the first value
                // initializes it (6.7.9p11).
                if elements.len() > 1 {
                    diag::warning(init.pos, &gettext("excess elements in scalar initializer"));
                }
            }
        }
    }

    fn infer_array_size_from_init(&mut self, typ: TypeId, init: &Expr) -> TypeId {
        if self.types.kind(typ) != TypeKind::Array {
            return typ;
        }

        let array_size = self.types.get(typ).array_size;
        // Check if array size is incomplete (0 or None)
        if array_size != Some(0) && array_size.is_some() {
            return typ;
        }

        // `{"hi"}` initializes the array with the string, not with one
        // element (C17 6.7.9p14), so look through the braces first.
        let elem_type = self.types.base_type(typ).unwrap_or(self.types.int_id);
        let init = match &init.kind {
            ExprKind::InitList { elements } => self
                .braced_string_initializer(elem_type, elements)
                .unwrap_or(init),
            _ => init,
        };

        let new_size = match &init.kind {
            ExprKind::InitList { elements } => {
                Some(self.array_size_from_elements(elements, elem_type))
            }
            _ => self.string_initializer_len(init),
        };

        if let Some(size) = new_size {
            // Update type with actual size from initializer
            // Preserve modifiers (like const, static)
            let modifiers = self.types.modifiers(typ);
            let mut arr_type = Type::array(elem_type, size);
            arr_type.modifiers = modifiers;
            self.types.intern(arr_type)
        } else {
            typ
        }
    }

    /// How many elements an incomplete array's initializer list implies.
    ///
    /// One list element is not one array element. When the array's element
    /// type is an aggregate and the initializer leaves its braces out, that
    /// one array element swallows as many list elements as it has scalar
    /// fields (C17 6.7.9p20) -- so `int a[][2] = {1,2,3,4}` names two rows,
    /// not four. Counting list elements instead sized the object at twice
    /// what the linearizer then filled.
    /// C17 6.7.6.2p1: an array's size expression shall have integer type.
    ///
    /// Without this the non-constant fallback took over and `int a[1.5];`
    /// became a variable length array at block scope -- accepted, and sized
    /// from a `double` -- where gcc says *"size of array has non-integer
    /// type"*. At file scope it read as a VLA rejection, which is a different
    /// reason and the wrong one.
    fn check_array_size_type(&self, expr: &Expr, pos: Position) -> ParseResult<()> {
        match expr.typ {
            Some(t) if !self.types.is_integer(t) => Err(ParseError::new(
                "size of array has non-integer type".to_string(),
                pos,
            )),
            _ => Ok(()),
        }
    }

    /// Derive an array type, refusing an extent the compiler cannot describe.
    ///
    /// `TypeTable::size_bits` answers in a `u32`, so an object wider than
    /// `u32::MAX` bits has no representable size. That used to saturate in
    /// silence: `char big[5000000000];` reported `sizeof` 536870911 with no
    /// diagnostic anywhere. The bound is enforced here, where the element
    /// type is known, so the outer dimension of a multi-dimensional array is
    /// measured against an inner one that is already within it.
    fn derive_array_type(
        &mut self,
        elem: TypeId,
        size: Option<usize>,
        pos: Position,
    ) -> Result<TypeId, ParseError> {
        if let Some(count) = size {
            let total = (count as u128) * (self.types.size_bytes(elem) as u128);
            if total > TypeTable::MAX_OBJECT_BYTES as u128 {
                return Err(ParseError::new(
                    format!(
                        "size of array exceeds the maximum object size of {} bytes",
                        TypeTable::MAX_OBJECT_BYTES
                    ),
                    pos,
                ));
            }
        }
        Ok(self.types.intern(Type {
            kind: TypeKind::Array,
            base: Some(elem),
            array_size: size,
            ..Default::default()
        }))
    }

    pub(crate) fn array_size_from_elements(
        &self,
        elements: &[InitElement],
        elem_type: TypeId,
    ) -> usize {
        let per_element = self.types.count_scalar_fields(elem_type).max(1);
        let mut max_index: i64 = -1;
        let mut current_index: i64 = 0;
        let mut idx = 0usize;

        while idx < elements.len() {
            let element = &elements[idx];
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
                let i = current_index;
                current_index += 1;
                i
            };

            if index > max_index {
                max_index = index;
            }

            // A brace-less aggregate element consumes several list elements
            // for this one slot. Stop early at a designator, which addresses
            // the enclosing array rather than continuing to fill this slot --
            // the same boundary `consume_brace_elision` observes.
            idx += 1;
            if designator_index.is_none()
                && crate::parse::ast::is_brace_elision_candidate(self.types, element, elem_type)
            {
                let mut taken = 1;
                while taken < per_element
                    && idx < elements.len()
                    && elements[idx].designators.is_empty()
                {
                    idx += 1;
                    taken += 1;
                }
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
        let decl_pos = self.current_pos();
        let base_type = self.parse_type_specifier()?;
        self.check_implicit_int(decl_pos);
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
                let decl_pos = self.current_pos();
                let (name, mut typ, mut vla_sizes, _func_params) =
                    self.parse_declarator(base_type_id, DeclaratorName::Required)?;
                // A variably modified typedef supplies the extents of the
                // levels it contributed. The declarator's own `[n]` levels, if
                // it wrote any, are innermost-of-the-outer and come first --
                // the same ordering `try_parse_type_name_vm` applies to a
                // type-name's declarator and specifier levels.
                if let Some(dims) = &self.pending_vm_typedef_dims {
                    vla_sizes.extend(dims.iter().cloned());
                }
                // Skip GCC extensions like __asm("...") or __attribute__((...))
                self.skip_extensions_after_declarator();

                // Check if we have a name (needed for symbol binding)
                let has_name = !self.str(name).is_empty();

                // Validate explicit alignment (C11 6.7.5: >= natural alignment)
                typ = self.apply_pending_type_attrs(typ);
                let validated_align = self.validated_explicit_align(typ)?;

                // Bind variable to symbol table BEFORE parsing initializer.
                // This ensures the variable is in scope for sizeof(*var) in initializers.
                // Per C99 6.2.1p7: "Any other identifier has scope that begins just
                // after the completion of its declarator."
                let mut symbol_id: Option<SymbolId> = None;
                if has_name && !is_typedef {
                    self.check_redeclaration(name, typ, decl_pos);
                    let sym = Symbol::variable(name, typ, self.symbols.depth())
                        .with_align(validated_align)
                        .with_variably_modified_array(!vla_sizes.is_empty());
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

                // 6.7p7: the object needs a size here, and unlike at file
                // scope nothing later can supply one -- a tag completed further
                // down the block is a different declaration. An `extern`
                // declaration defines nothing and is exempt, and so does a
                // `typedef`, which declares no object at all: without that,
                // `typedef struct Incomplete T;` at block scope was rejected
                // although it names a type nobody has asked to size. The
                // file-scope twin has had the guard all along.
                if !is_typedef
                    && !base_type.modifiers.contains(TypeModifiers::EXTERN)
                    && !self.types.is_composite_complete(typ)
                {
                    let named = self.types.format_type(typ, Some(self.idents));
                    diag::error_args(
                        self.current_pos(),
                        "storage size of an object of type '{0}' is not known",
                        &[&named],
                    );
                }

                // For incomplete array types, infer size from initializer
                if let Some(ref init_expr) = init {
                    // 6.7.9p5: an identifier declared `extern` at block scope
                    // has linkage, so it refers to a definition elsewhere and
                    // cannot carry one here. At *file* scope the same spelling
                    // is a definition with external linkage, which gcc only
                    // warns about -- hence the scope test.
                    if base_type.modifiers.contains(TypeModifiers::EXTERN) {
                        diag::error(
                            init_expr.pos,
                            &gettext("'extern' variable has an initializer"),
                        );
                    }
                    let old_type = typ;
                    typ = self.infer_array_size_from_init(typ, init_expr);
                    self.check_excess_initializers(typ, init_expr);
                    self.check_initializer_types(typ, init_expr);

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
                    // C17 6.7.7p3 admits a typedef of a variably modified type
                    // only at block scope, and this path is only ever reached
                    // from one -- `parse_block_items` and a `for`-init are its
                    // sole callers. The file-scope spelling is refused by the
                    // declarator itself, with "variable length arrays cannot
                    // have file scope", before it could arrive here.
                    // A mode replaces the type; alignment then attaches to
                    // whatever the type ended up being.
                    typ = self.apply_pending_type_attrs(typ);
                    // Apply __attribute__((aligned(N))) to typedef type
                    if let Some(align) = self.pending_alignas {
                        let mut aligned_type = self.types.get(typ).clone();
                        aligned_type.explicit_align =
                            Some(aligned_type.explicit_align.map_or(align, |e| e.max(align)));
                        typ = self.types.intern(aligned_type);
                    }
                    self.check_typedef_redefinition(name, typ, decl_pos);
                    let sym = Symbol::typedef(name, typ, self.symbols.depth());
                    if let Ok(id) = self.symbols.declare(sym) {
                        symbol_id = Some(id);
                        // Remember how many extents this name carries, so a
                        // use can name each of them. They cannot be recovered
                        // from the type: `int[n]`, `int[m]` and `int[]` all
                        // intern to one `TypeId`.
                        if !vla_sizes.is_empty() {
                            self.vm_typedefs.insert(id, vla_sizes.len() as u32);
                        }
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
                        symbol_attrs: std::mem::take(&mut self.pending_symbol_attrs),
                        symbol,
                        typ,
                        storage_class,
                        init,
                        vla_sizes,
                        explicit_align: validated_align,
                        pos: decl_pos,
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
        // Belongs to the declaration whose specifiers named the typedef, and
        // to no later one.
        self.pending_vm_typedef_dims = None;
        // A mode that no declarator consumed belongs to no later declaration:
        // leaving it set applied it to whatever came next.
        self.pending_mode = None;
        self.pending_transparent_union = None;
        self.expect_special(b';')?;

        Ok(Declaration { declarators })
    }

    /// Parse a type specifier
    /// Parse a type specifier, reporting whether one was actually present.
    ///
    /// See [`SpecifierTally`] for the C17 6.7.2p2 combination check this makes.
    ///
    /// The flag has to be written on *every* path out, including the early
    /// returns for struct/union/enum/typeof. Leaving a stale value behind is
    /// invisible until the next declaration inherits it: a K&R identifier list
    /// — whose parameters are `int` by C17 6.9.1p6, so no specifier appears —
    /// left it false, and the *following* `struct S s;` drew "type specifier
    /// missing". Returning it rather than assigning a field is what makes the
    /// compiler check the paths.
    fn parse_type_specifier_inner(&mut self) -> ParseResult<(Type, bool)> {
        let mut modifiers = TypeModifiers::empty();
        let mut base_kind: Option<TypeKind> = None;
        // Track typedef type separately - we continue parsing after a typedef
        // to collect trailing qualifiers like "z_word_t const"
        let mut typedef_base: Option<TypeId> = None;
        // C17 6.7.2p2 admits only a fixed list of specifier combinations. The
        // loop below merely overwrites `base_kind`, so without a tally
        // `float double x;` silently became a `double` and `void int y;` an
        // object of type void with a size -- accepted, and wrong, rather than
        // diagnosed. Recorded here and checked once the list is complete.
        let mut tally = SpecifierTally::default();

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
            match name_id {
                // Skip __attribute__ in the type specifier loop
                crate::kw::GNU_ATTRIBUTE | crate::kw::GNU_ATTRIBUTE2 => {
                    self.skip_extensions();
                    continue;
                }
                crate::kw::CONST => {
                    self.advance();
                    modifiers |= TypeModifiers::CONST;
                }
                crate::kw::VOLATILE => {
                    self.advance();
                    modifiers |= TypeModifiers::VOLATILE;
                }
                crate::kw::STATIC => {
                    tally.note_storage_class("static", self.current_pos());
                    self.advance();
                    modifiers |= TypeModifiers::STATIC;
                }
                crate::kw::EXTERN => {
                    tally.note_storage_class("extern", self.current_pos());
                    self.advance();
                    modifiers |= TypeModifiers::EXTERN;
                }
                crate::kw::REGISTER => {
                    tally.note_storage_class("register", self.current_pos());
                    self.advance();
                    modifiers |= TypeModifiers::REGISTER;
                }
                crate::kw::AUTO => {
                    tally.note_storage_class("auto", self.current_pos());
                    self.advance();
                    modifiers |= TypeModifiers::AUTO;
                }
                crate::kw::TYPEDEF => {
                    tally.note_storage_class("typedef", self.current_pos());
                    self.advance();
                    modifiers |= TypeModifiers::TYPEDEF;
                }
                crate::kw::THREAD_LOCAL | crate::kw::GNU_THREAD => {
                    self.advance();
                    modifiers |= TypeModifiers::THREAD_LOCAL;
                }
                crate::kw::INLINE | crate::kw::GNU_INLINE2 | crate::kw::GNU_INLINE => {
                    self.advance();
                    modifiers |= TypeModifiers::INLINE;
                }
                crate::kw::NORETURN | crate::kw::GNU_NORETURN => {
                    self.advance();
                    modifiers |= TypeModifiers::NORETURN;
                }
                crate::kw::SIGNED => {
                    tally.note_sign("signed", self.current_pos());
                    self.advance();
                    modifiers |= TypeModifiers::SIGNED;
                }
                crate::kw::UNSIGNED => {
                    tally.note_sign("unsigned", self.current_pos());
                    self.advance();
                    modifiers |= TypeModifiers::UNSIGNED;
                }
                crate::kw::COMPLEX => {
                    self.advance();
                    modifiers |= TypeModifiers::COMPLEX;
                }
                crate::kw::ATOMIC => {
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
                            // C17 6.7.2.4p3: the type name in `_Atomic(T)`
                            // shall not be an array or a function type.
                            let inner_kind = self.types.kind(inner_type);
                            if matches!(inner_kind, TypeKind::Array | TypeKind::Function) {
                                diag::error_args(
                                    self.current_pos(),
                                    "'_Atomic' cannot be applied to {0} type",
                                    &[if inner_kind == TypeKind::Array {
                                        "an array"
                                    } else {
                                        "a function"
                                    }],
                                );
                            }
                            let inner = self.types.get(inner_type).clone();
                            return Ok((
                                Type {
                                    modifiers: modifiers | inner.modifiers | TypeModifiers::ATOMIC,
                                    ..inner
                                },
                                true,
                            ));
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
                crate::kw::ALIGNAS => {
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
                crate::kw::SHORT => {
                    tally.note_size("short", self.current_pos());
                    self.advance();
                    modifiers |= TypeModifiers::SHORT;
                    // C17 6.7.2p2 lists the declaration specifiers as a set,
                    // so `int short` names what `short int` names. A tally
                    // that had already seen `int` must still take the size:
                    // testing only `is_none()` left the kind at `Int` and
                    // gave `int short` four bytes.
                    if base_kind.is_none() || base_kind == Some(TypeKind::Int) {
                        base_kind = Some(TypeKind::Short);
                    }
                }
                crate::kw::LONG => {
                    tally.note_size("long", self.current_pos());
                    self.advance();
                    if modifiers.contains(TypeModifiers::LONG) {
                        modifiers |= TypeModifiers::LONGLONG;
                        base_kind = Some(TypeKind::LongLong);
                    } else {
                        modifiers |= TypeModifiers::LONG;
                        // long double case
                        if base_kind == Some(TypeKind::Double) {
                            base_kind = Some(TypeKind::LongDouble);
                        } else if base_kind.is_none() || base_kind == Some(TypeKind::Int) {
                            // `int long` is `long int`; see the SHORT arm.
                            base_kind = Some(TypeKind::Long);
                        }
                    }
                }
                crate::kw::VOID => {
                    tally.note_data_type("void", self.current_pos());
                    self.advance();
                    base_kind = Some(TypeKind::Void);
                }
                crate::kw::CHAR => {
                    tally.note_data_type("char", self.current_pos());
                    self.advance();
                    base_kind = Some(TypeKind::Char);
                }
                crate::kw::INT => {
                    tally.note_data_type("int", self.current_pos());
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
                crate::kw::FLOAT => {
                    tally.note_data_type("float", self.current_pos());
                    self.advance();
                    base_kind = Some(TypeKind::Float);
                }
                crate::kw::DOUBLE => {
                    tally.note_data_type("double", self.current_pos());
                    self.advance();
                    // Handle long double
                    if modifiers.contains(TypeModifiers::LONG) {
                        base_kind = Some(TypeKind::LongDouble);
                    } else {
                        base_kind = Some(TypeKind::Double);
                    }
                }
                crate::kw::FLOAT16 => {
                    if tally.alias_is_declarator_name() {
                        break;
                    }
                    tally.note_data_type("_Float16", self.current_pos());
                    self.advance();
                    base_kind = Some(TypeKind::Float16);
                }
                crate::kw::FLOAT32 => {
                    // _Float32 is an alias for float (TS 18661-3 / C23)
                    if tally.alias_is_declarator_name() {
                        break;
                    }
                    tally.note_data_type("float", self.current_pos());
                    self.advance();
                    base_kind = Some(TypeKind::Float);
                }
                crate::kw::FLOAT64 => {
                    // _Float64 is an alias for double (TS 18661-3 / C23)
                    if tally.alias_is_declarator_name() {
                        break;
                    }
                    tally.note_data_type("double", self.current_pos());
                    self.advance();
                    base_kind = Some(TypeKind::Double);
                }
                crate::kw::FLOAT128 | crate::kw::FLOAT128_ALIAS => {
                    // IEEE binary128. `_Float128` is the C23/TS 18661-3
                    // spelling and `__float128` the GCC one; on a target whose
                    // long double is already binary128 glibc typedefs the
                    // former to `long double`, so it takes the same
                    // yield-to-the-declarator rule as the other aliases.
                    if tally.alias_is_declarator_name() {
                        break;
                    }
                    if !self.types.has_float128() {
                        return Err(ParseError::new(
                            "__float128 is not supported on this target",
                            self.current_pos(),
                        ));
                    }
                    tally.note_data_type("__float128", self.current_pos());
                    self.advance();
                    base_kind = Some(TypeKind::Float128);
                }
                crate::kw::BOOL => {
                    tally.note_data_type("_Bool", self.current_pos());
                    self.advance();
                    base_kind = Some(TypeKind::Bool);
                }
                crate::kw::INT128 => {
                    tally.note_data_type("__int128", self.current_pos());
                    self.advance();
                    base_kind = Some(TypeKind::Int128);
                }
                crate::kw::INT128_T => {
                    if tally.alias_is_declarator_name() {
                        break;
                    }
                    tally.note_data_type("__int128", self.current_pos());
                    self.advance();
                    base_kind = Some(TypeKind::Int128);
                }
                crate::kw::UINT128_T => {
                    if tally.alias_is_declarator_name() {
                        break;
                    }
                    tally.note_data_type("__int128", self.current_pos());
                    self.advance();
                    modifiers |= TypeModifiers::UNSIGNED;
                    base_kind = Some(TypeKind::Int128);
                }
                crate::kw::BUILTIN_VA_LIST => {
                    tally.note_data_type("__builtin_va_list", self.current_pos());
                    self.advance();
                    base_kind = Some(TypeKind::VaList);
                }
                crate::kw::TYPEOF | crate::kw::GNU_TYPEOF | crate::kw::GNU_TYPEOF2 => {
                    // `typeof` always takes a parenthesized operand, so
                    // without one this is not a type specifier -- it is the
                    // declarator's name. gcc accepts `int typeof;` in C17,
                    // because `typeof` is a GNU extension rather than a C17
                    // keyword; consuming it here and then demanding `(` made
                    // c17 reject the declaration outright.
                    if !self.next_token_is_open_paren() {
                        break;
                    }
                    self.advance(); // consume typeof
                    self.expect_special(b'(')?;

                    // typeof can take either a type name or an expression
                    // Try type name first
                    if let Some(typ) = self.try_parse_type_name() {
                        self.expect_special(b')')?;
                        // Return the type with any modifiers
                        let result_type = self.types.get(typ).clone();
                        return Ok((
                            Type {
                                modifiers: modifiers | result_type.modifiers,
                                ..result_type
                            },
                            true,
                        ));
                    }

                    // Not a type name, try expression
                    let expr = self.parse_expression()?;
                    self.expect_special(b')')?;

                    // Get the type of the expression
                    let expr_type_id = expr.typ.unwrap_or(self.types.int_id);
                    let result_type = self.types.get(expr_type_id).clone();
                    return Ok((
                        Type {
                            modifiers: modifiers | result_type.modifiers,
                            ..result_type
                        },
                        true,
                    ));
                }
                crate::kw::ENUM => {
                    tally.note_data_type("enum", self.current_pos());
                    tally.check();
                    let mut enum_type = self.parse_enum_specifier()?;
                    // Consume trailing qualifiers (e.g., "enum foo const")
                    let trailing_mods = self.consume_type_qualifiers();
                    // Apply any modifiers we collected
                    enum_type.modifiers |= modifiers | trailing_mods;
                    return Ok((enum_type, true));
                }
                crate::kw::STRUCT => {
                    tally.note_data_type("struct", self.current_pos());
                    tally.check();
                    let mut struct_type = self.parse_struct_or_union_specifier(false)?;
                    // Consume trailing qualifiers (e.g., "struct foo const")
                    let trailing_mods = self.consume_type_qualifiers();
                    struct_type.modifiers |= modifiers | trailing_mods;
                    return Ok((struct_type, true));
                }
                crate::kw::UNION => {
                    tally.note_data_type("union", self.current_pos());
                    tally.check();
                    let mut union_type = self.parse_struct_or_union_specifier(true)?;
                    // Consume trailing qualifiers (e.g., "union foo const")
                    let trailing_mods = self.consume_type_qualifiers();
                    union_type.modifiers |= modifiers | trailing_mods;
                    return Ok((union_type, true));
                }
                _ => {
                    // Check if it's a typedef name
                    // Only consume the typedef if we haven't already seen a base type or typedef
                    if base_kind.is_none() && typedef_base.is_none() {
                        if let Some((sym, typedef_type_id)) =
                            self.symbols.lookup_typedef_symbol(name_id)
                        {
                            self.advance();
                            // A variably modified typedef carries extents the
                            // `TypeId` cannot: hand the declarator list the
                            // names of the ones this typedef already evaluated
                            // (C17 6.7.7p3), not its size expressions.
                            self.pending_vm_typedef_dims = self.vm_typedef_extents(sym);
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

        tally.check();

        // If we parsed a typedef, return that with any trailing modifiers applied
        if let Some(typedef_type_id) = typedef_base {
            let typedef_type = self.types.get(typedef_type_id);
            let mut result = typedef_type.clone();
            // Strip TYPEDEF modifier - we're using the typedef, not defining one
            result.modifiers &= !TypeModifiers::TYPEDEF;
            result.modifiers |= modifiers;
            return Ok((result, true));
        }

        // `signed x;` and `unsigned x;` name a type without setting `base_kind`
        // — those two only ever set a modifier — so they are explicit even
        // though the kind defaults. `short`/`long` do set the kind.
        let explicit = base_kind.is_some()
            || modifiers.intersects(TypeModifiers::SIGNED | TypeModifiers::UNSIGNED);

        let kind = base_kind.unwrap_or(TypeKind::Int);
        Ok((Type::with_modifiers(kind, modifiers), explicit))
    }

    /// The extents of the variably modified typedef `sym`, as expressions that
    /// name what its declaration already evaluated.
    ///
    /// None for an ordinary typedef, which is nearly all of them.
    pub(crate) fn vm_typedef_extents(&self, sym: SymbolId) -> Option<Vec<Expr>> {
        let levels = *self.vm_typedefs.get(&sym)?;
        Some(
            (0..levels)
                .map(|level| Expr {
                    kind: ExprKind::VmTypedefExtent(sym, level),
                    typ: Some(self.types.ulong_id),
                    pos: self.current_pos(),
                })
                .collect(),
        )
    }

    /// Parse a type specifier and record whether one was present.
    fn parse_type_specifier(&mut self) -> ParseResult<Type> {
        let pos = self.current_pos();
        let (typ, explicit) = self.parse_type_specifier_inner()?;
        self.saw_explicit_type = explicit;

        // C17 6.7.3p3: the _Atomic *qualifier* shall not be applied to an array
        // or function type. The specifier form `_Atomic(T)` is checked where it
        // is parsed, but the qualifier form only sets a bit, so
        //
        //     typedef int A[4];   _Atomic A x;
        //     typedef int F(void); _Atomic F f;
        //
        // both slipped through -- the only way to reach the constraint, since
        // `_Atomic int a[4]` is an array *of* atomic ints and perfectly legal.
        if typ.modifiers.contains(TypeModifiers::ATOMIC) {
            let what = match typ.kind {
                TypeKind::Array => Some("an array"),
                TypeKind::Function => Some("a function"),
                _ => None,
            };
            if let Some(what) = what {
                diag::error_args(pos, "'_Atomic' cannot be applied to {0} type", &[what]);
            }
        }

        Ok(typ)
    }

    /// Drop the storage-class bits from a type, leaving only what it denotes.
    ///
    /// These describe the *declaration*, not the type, so two names for the
    /// same type can differ in them and still be compatible.
    fn strip_declaration_modifiers(&mut self, id: TypeId) -> TypeId {
        const DECL_ONLY: TypeModifiers = TypeModifiers::TYPEDEF
            .union(TypeModifiers::EXTERN)
            .union(TypeModifiers::STATIC)
            .union(TypeModifiers::AUTO)
            .union(TypeModifiers::REGISTER)
            .union(TypeModifiers::THREAD_LOCAL)
            .union(TypeModifiers::INLINE);

        let t = self.types.get(id);
        if !t.modifiers.intersects(DECL_ONLY) {
            return id;
        }
        let mut stripped = t.clone();
        stripped.modifiers = stripped.modifiers.difference(DECL_ONLY);
        self.types.intern(stripped)
    }

    /// `strip_declaration_modifiers`, reaching a function type's return type
    /// as well.
    ///
    /// `static`, `extern` and `inline` are recorded on the declaration's base
    /// type, which for a function declarator *is* the return type -- so
    /// `inline int hdr(int)` and `extern int hdr(int)` build function types
    /// whose returns are two different `int`s, and compatibility comparing
    /// bases by id calls them different. They print identically, which is how
    /// the diagnostic gave itself away: "conflicting types for 'hdr':
    /// 'int(int)' then 'int(int)'". `cc/audit.md` records the same family of
    /// bug reaching call compatibility once before.
    fn strip_declaration_modifiers_deep(&mut self, id: TypeId) -> TypeId {
        let id = self.strip_declaration_modifiers(id);
        if self.types.kind(id) != TypeKind::Function {
            return id;
        }
        let Some(ret) = self.types.base_type(id) else {
            return id;
        };
        let stripped_ret = self.strip_declaration_modifiers_deep(ret);
        if stripped_ret == ret {
            return id;
        }
        let mut func = self.types.get(id).clone();
        func.base = Some(stripped_ret);
        self.types.intern(func)
    }

    /// Diagnose a redeclaration whose type conflicts with the one already in
    /// scope (C17 6.7p4: all declarations of the same object or function shall
    /// specify compatible types).
    ///
    /// Nothing compared types before: `SymbolTable::declare` only rejects two
    /// *definitions* at one depth, and `Symbol::function` is never marked
    /// defined, so two function declarations never collided at all. `int x;
    /// double x;` therefore bound the second declarator to the first symbol
    /// and emitted `.comm x,4,4` -- a silent miscompile, since a `double`
    /// store through it runs off the object.
    ///
    /// The two guards are what keep legal code legal, and both are borrowed
    /// from `check_typedef_redefinition`: only a repeat *in the same scope* is
    /// a redeclaration, so shadowing survives; and the declaration-only
    /// modifiers come off first, so `extern int x;` followed by `int x = 5;`
    /// is one type, not two.
    fn check_redeclaration(&mut self, name: StringId, new_type: TypeId, pos: Position) {
        let Some(existing_id) = self.symbols.lookup_id(name, Namespace::Ordinary) else {
            return;
        };
        let existing = self.symbols.get(existing_id);
        // An enumerator shares the ordinary name space with a variable, so
        // declaring one over the other is 6.7p3 and gcc's own wording says
        // so. The reverse direction -- an enumerator over a variable -- is
        // caught where enumerators are bound. Without this arm `enum A { Z };
        // int Z;` compiled and the two names collided silently.
        if existing.kind == SymbolKind::EnumConstant && existing.scope_depth == self.symbols.depth()
        {
            let spelled = self.idents.get_opt(name).unwrap_or("").to_string();
            diag::error_args(
                pos,
                "'{0}' redeclared as a different kind of symbol",
                &[&spelled],
            );
            return;
        }
        // Typedefs have their own check; a tag is a different namespace.
        if !matches!(
            existing.kind,
            SymbolKind::Variable | SymbolKind::Function | SymbolKind::Parameter
        ) {
            return;
        }
        if existing.scope_depth != self.symbols.depth() {
            return;
        }
        let old_kind = existing.kind;
        let old_type = existing.typ;
        let old_type = self.strip_declaration_modifiers_deep(old_type);
        let new_type = self.strip_declaration_modifiers_deep(new_type);
        if self.redeclaration_compatible(old_type, new_type) {
            return;
        }

        let spelled = self.idents.get_opt(name).unwrap_or("").to_string();
        // gcc distinguishes these, and the distinction is the useful part: a
        // function becoming an object is a different mistake from a function
        // changing its signature.
        let old_is_func =
            self.types.kind(old_type) == TypeKind::Function || old_kind == SymbolKind::Function;
        let new_is_func = self.types.kind(new_type) == TypeKind::Function;
        if old_is_func != new_is_func {
            diag::error_args(
                pos,
                "'{0}' redeclared as a different kind of symbol",
                &[&spelled],
            );
            return;
        }
        diag::error_args(
            pos,
            "conflicting types for '{0}': '{1}' then '{2}'",
            &[
                &spelled,
                &self.types.format_type(old_type, Some(self.idents)),
                &self.types.format_type(new_type, Some(self.idents)),
            ],
        );
    }

    /// Are these two declarations of one name compatible (C17 6.2.7)?
    ///
    /// Beyond ordinary type compatibility, 6.2.7p2 pairs a declarator with no
    /// prototype against one that has a prototype: `int f(); int f(int);` is a
    /// composite type, not a conflict. That case is only expressible because
    /// the function type now records whether a prototype was supplied.
    fn redeclaration_compatible(&self, old: TypeId, new: TypeId) -> bool {
        if self.types.types_compatible(old, new) {
            return true;
        }
        let (o, n) = (self.types.get(old), self.types.get(new));

        // 6.2.7p3: an array of unknown size is compatible with a sized array
        // of the same element type -- the composite takes the known size. That
        // is how `extern int a[]; int a[3];` completes a declaration.
        //
        // "Unknown" is spelled two ways here, absent and zero, because the
        // declarator paths do not agree on which; that also means a genuine
        // `int a[0]` (the GNU zero-length array) is accepted against any size.
        // Under-diagnosing that is the safe direction.
        if o.kind == TypeKind::Array && n.kind == TypeKind::Array {
            let size_unknown = |sz: Option<usize>| matches!(sz, None | Some(0));
            if size_unknown(o.array_size) || size_unknown(n.array_size) {
                return match (o.base, n.base) {
                    (Some(a), Some(b)) => self.types.types_compatible(a, b),
                    _ => false,
                };
            }
        }

        if o.kind != TypeKind::Function || n.kind != TypeKind::Function {
            return false;
        }
        // Exactly one side lacks a prototype: compatible when the return types
        // agree. Checking the parameters against their promoted types as well
        // would be stricter than gcc, which accepts the pairing outright.
        if o.params.is_some() == n.params.is_some() {
            return false;
        }
        match (o.base, n.base) {
            (Some(a), Some(b)) => self.types.types_compatible(a, b),
            _ => false,
        }
    }

    /// Diagnose redefining a typedef name with an incompatible type.
    ///
    /// C11/C17 6.7p3 legalized redefining a typedef, but only to a *compatible*
    /// type. Every `declare()` caller discards `SymbolError::Redefinition` and
    /// reuses the existing symbol, so an incompatible redefinition silently
    /// kept the first type — strictly worse than C89, where any redefinition
    /// was flagged.
    fn check_typedef_redefinition(&mut self, name: StringId, new_type: TypeId, pos: Position) {
        let Some(existing_id) = self.symbols.lookup_id(name, Namespace::Ordinary) else {
            return;
        };
        let existing = self.symbols.get(existing_id);
        if !existing.is_typedef() {
            return;
        }
        // 6.7p3 governs a *repeat* declaration, which means the same scope.
        // `lookup_id` answers with the innermost visible binding from any
        // enclosing scope, so without this a block-scope `typedef double T;`
        // shadowing a file-scope `typedef int T;` — perfectly legal, and what
        // shadowing is for — was reported as an incompatible redefinition and
        // failed the translation unit.
        if existing.scope_depth != self.symbols.depth() {
            return;
        }
        let old_type = existing.typ;
        // Compare the types the two names denote, not how they were spelled.
        // A typedef's recorded type may still carry the TYPEDEF bit and the
        // storage class from its declaration, and glibc reaches most of these
        // names through a second typedef (`typedef __int16_t int16_t;`), so
        // comparing raw modifiers reports two identical `short`s as different.
        let old_type = self.strip_declaration_modifiers(old_type);
        let new_type = self.strip_declaration_modifiers(new_type);
        if self.types.types_compatible(old_type, new_type) {
            return;
        }
        let spelled = self.idents.get_opt(name).unwrap_or("").to_string();
        diag::error_args(
            pos,
            "typedef '{0}' redefined with an incompatible type ('{1}' then '{2}')",
            &[
                &spelled.to_string(),
                &self.types.get(old_type).to_string(),
                &self.types.get(new_type).to_string(),
            ],
        );
    }

    /// Diagnose a declaration that named no type (C99 removed implicit int;
    /// 6.7.2p2 makes "at least one type specifier shall be given" a
    /// constraint). Call after `parse_type_specifier` at a site where a type is
    /// genuinely required.
    fn check_implicit_int(&mut self, pos: Position) {
        if !self.saw_explicit_type {
            diag::error(
                pos,
                &gettext("type specifier missing; implicit 'int' was removed in C99"),
            );
            // Keep the defaulted `int` and carry on: the declarator that
            // follows is usually well-formed, and one diagnostic per
            // declaration reads better than a cascade.
            self.saw_explicit_type = true;
        }
    }

    /// The integer type an enumerated type is compatible with, and its size.
    ///
    /// C17 6.7.2.2p4 requires it to represent every member; the choice among
    /// the types that do is implementation-defined. Narrowest wins, signed
    /// before unsigned at each width, so an enum whose members all fit in
    /// `int` is exactly the four signed bytes it has always been -- the
    /// widening only happens where the alternative was a wrong value.
    fn enum_underlying_type(
        &mut self,
        constants: &[EnumConstant],
        pos: Position,
    ) -> (TypeId, usize) {
        let Some(min) = constants.iter().map(|c| c.value).min() else {
            return (self.types.int_id, 4);
        };
        let max = constants.iter().map(|c| c.value).max().unwrap_or(0);

        let fits = |lo: i128, hi: i128| min >= lo && max <= hi;
        // 6.7.2.2p4 leaves the choice to the implementation, requiring only a
        // type that represents every member. gcc's choice is unsigned whenever
        // no enumerator is negative, and it is observable -- `(enum E)-1 > 0`
        // is true there and was false here. Preferring `int` for a small
        // non-negative list also made an enum bit-field read back negative:
        // `enum E { A, B, C, D }; struct { enum E e:2; }` holding `D` gave -1
        // where gcc gives 3, because the field's signedness follows the type's.
        if min >= 0 {
            if fits(0, u32::MAX as i128) {
                (self.types.uint_id, 4)
            } else if fits(0, u64::MAX as i128) {
                (self.types.ulong_id, 8)
            } else {
                // Not unreachable, as this once claimed: nothing clamps an
                // enumerator to 64 bits and the folder computes in `i128`, so
                // `enum E { A = 1 << 64 };` reached here and **panicked the
                // compiler**. gcc warns and carries on, folding the shift to 0
                // because it truncates to the expression's type -- which c17's
                // folders do not yet do (see #C115). Until they do, say what is
                // wrong rather than aborting: a compiler diagnoses.
                diag::error(
                    pos,
                    &gettext("no integer type can represent all values of this enumeration"),
                );
                (self.types.ulong_id, 8)
            }
        } else if fits(i32::MIN as i128, i32::MAX as i128) {
            (self.types.int_id, 4)
        } else if fits(i64::MIN as i128, i64::MAX as i128) {
            (self.types.long_id, 8)
        } else {
            // A list spanning below i64::MIN and above i64::MAX has no
            // integer type that holds both ends. Say so rather than picking
            // one and truncating the other.
            diag::error(
                pos,
                &gettext("no integer type can represent all values of this enumeration"),
            );
            (self.types.long_id, 8)
        }
    }

    /// Parse an enum specifier
    /// enum-specifier: 'enum' identifier? '{' enumerator-list? '}' | 'enum' identifier
    pub(crate) fn parse_enum_specifier(&mut self) -> ParseResult<Type> {
        let enum_pos = self.current_pos();
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
            let mut next_value = 0i128;
            // The enumerators bind before the enum's own width is known --
            // `enum { A = 1, B = A + 1 }` reads A while the list is still
            // being parsed -- so they are declared with a provisional type
            // and re-typed once every value has been seen.
            let mut constant_syms: Vec<SymbolId> = Vec::new();

            while !self.is_special(b'}') && !self.is_eof() {
                let name = self.expect_identifier()?;

                let value = if self.is_special(b'=') {
                    self.advance();
                    let value_pos = self.current_pos();
                    let expr = self.parse_conditional_expr()?;
                    // Evaluate constant expression
                    let v = self.eval_const_expr(&expr).ok_or_else(|| {
                        ParseError::new("enum value must be constant", self.current_pos())
                    })?;
                    // C17 6.7.2.2p2 requires an enumerator to be
                    // representable as `int`, so exceeding it is a constraint
                    // violation and 5.1.1.3 requires it be diagnosed. gcc
                    // widens the enumerated type rather than rejecting, and
                    // so does c17 -- but not in silence, which is how
                    // `X = 5000000000` used to become 705032704.
                    if v < i32::MIN as i128 || v > i32::MAX as i128 {
                        diag::warning_args(
                            value_pos,
                            "enumerator value {0} is outside the range of 'int'",
                            &[&v.to_string()],
                        );
                    }
                    v
                } else {
                    next_value
                };

                constants.push(EnumConstant { name, value });
                next_value = value + 1;

                // Register enum constant in symbol table (Ordinary namespace)
                let sym =
                    Symbol::enum_constant(name, value, self.types.int_id, self.symbols.depth());
                // 6.7p3: an enumerator shares the ordinary name space, so a
                // repeat -- of another enumerator or of a variable -- is a
                // constraint violation. `declare` already detects it; the
                // `Err` was being dropped, so `enum A { X }; enum B { X };`
                // compiled and the second `X` silently kept the first's value.
                match self.symbols.declare(sym) {
                    Ok(id) => constant_syms.push(id),
                    Err(_) => {
                        let spelled = self.idents.get_opt(name).unwrap_or("").to_string();
                        let existing = self.symbols.lookup(name, Namespace::Ordinary);
                        let redeclared = existing.is_some_and(|e| !e.is_enum_constant());
                        if redeclared {
                            diag::error_args(
                                enum_pos,
                                "'{0}' redeclared as a different kind of symbol",
                                &[&spelled],
                            );
                        } else {
                            diag::error_args(
                                enum_pos,
                                "redeclaration of enumerator '{0}'",
                                &[&spelled],
                            );
                        }
                    }
                }

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
                    &gettext("empty enum definition is a GNU extension"),
                );
            }

            self.expect_special(b'}')?;

            // C17 6.7.2.2p4: the enumerated type is compatible with some
            // integer type capable of representing every member. Pick the
            // narrowest that is, preferring signed at each width, and give
            // the enumerators that type -- for an enum that fits in `int`,
            // which is nearly all of them, this is the `int` it always was.
            let (underlying, size) = self.enum_underlying_type(&constants, enum_pos);
            for &sym_id in &constant_syms {
                self.symbols.get_mut(sym_id).typ = underlying;
            }

            let composite = CompositeType {
                tag,
                members: Vec::new(),
                enum_constants: constants,
                size,
                align: size,
                is_complete: true,
                transparent: false,
            };

            let mut enum_type = Type::enum_type(composite);
            // C17 6.7.2.2p4: the enumerated type shall represent every member.
            // `enum_underlying_type` picks a type that does, but the enum's own
            // type carried no signedness, so an *object* of it was loaded and
            // compared as signed even where the underlying type is unsigned:
            // `enum E { BIG = 0x80000000u }; enum E e = BIG;` read back
            // -2147483648 and `e < 0` was true, while the constant `BIG` was
            // correct all along.
            if self.types.is_unsigned(underlying) {
                enum_type.modifiers |= TypeModifiers::UNSIGNED;
            }

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
        let specifier_pos = self.current_pos();
        self.advance(); // consume 'struct' or 'union'

        // Parse __attribute__ between 'struct' keyword and tag name
        // (e.g., struct __attribute__((packed)) tagname { ... })
        let early_attrs = self.parse_attributes();
        let mut is_packed = early_attrs
            .attrs
            .iter()
            .any(|a| a.name == "packed" || a.name == "__packed__");
        // `transparent_union` is collected at the same four positions as
        // `packed`, for the same reason: gcc accepts it at any of them.
        let mut is_transparent = early_attrs.has_transparent_union();
        // Track struct-level aligned attribute (max across all positions)
        let mut struct_align: Option<u32> = early_attrs.get_alignment();

        // Optional tag name
        let tag = if self.peek() == TokenType::Ident && !self.is_special(b'{') {
            if !self.is_attribute_keyword() {
                Some(self.expect_identifier()?)
            } else {
                let mid_attrs = self.parse_attributes();
                is_packed = is_packed
                    || mid_attrs
                        .attrs
                        .iter()
                        .any(|a| a.name == "packed" || a.name == "__packed__");
                is_transparent = is_transparent || mid_attrs.has_transparent_union();
                if let Some(a) = mid_attrs.get_alignment() {
                    struct_align = Some(struct_align.map_or(a, |e| e.max(a)));
                }
                if self.peek() == TokenType::Ident && !self.is_special(b'{') {
                    Some(self.expect_identifier()?)
                } else {
                    None
                }
            }
        } else {
            None
        };

        // Parse __attribute__ after tag name but before '{'
        let pre_attrs = self.parse_attributes();
        is_packed = is_packed
            || pre_attrs
                .attrs
                .iter()
                .any(|a| a.name == "packed" || a.name == "__packed__");
        is_transparent = is_transparent || pre_attrs.has_transparent_union();
        if let Some(a) = pre_attrs.get_alignment() {
            struct_align = Some(struct_align.map_or(a, |e| e.max(a)));
        }

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
                        access_bytes: None,
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
                    // An unnamed bit-field is still a bit-field: its type has
                    // to be one a bit-field may have, and its width has to fit.
                    // Neither unnamed site validated anything, so
                    // `struct { float : 3; }` was accepted.
                    self.validate_bitfield(member_base_type_id, width, false)?;

                    members.push(StructMember {
                        name: StringId::EMPTY,
                        typ: member_base_type_id,
                        offset: 0,
                        bit_offset: None,
                        bit_width: Some(width),
                        access_bytes: None,
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
                        self.validate_bitfield(member_base_type_id, width, false)?;

                        members.push(StructMember {
                            name: StringId::EMPTY,
                            typ: member_base_type_id,
                            offset: 0,
                            bit_offset: None,
                            bit_width: Some(width),
                            access_bytes: None,
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
                        self.parse_declarator(member_base_type_id, DeclaratorName::Required)?;

                    // C99 6.7.5.2: VLAs cannot be members of structures or unions
                    if !vla_sizes.is_empty() {
                        return Err(ParseError::new(
                            "variable length arrays cannot be structure or union members"
                                .to_string(),
                            self.current_pos(),
                        ));
                    }

                    // 6.7.2.1p9 says the same of a member that reached its
                    // variably modified type through a typedef, which is the
                    // only other way in -- the declarator wrote no `[n]`, so
                    // the check above sees nothing. Without this the member
                    // looked like a flexible array (its extent is absent) and
                    // drew a diagnostic about that instead. gcc's wording.
                    if self.pending_vm_typedef_dims.is_some() {
                        return Err(ParseError::new(
                            "a member of a structure or union cannot have a variably modified type"
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

                    // A member's type attributes are the member's. Nothing
                    // consumed them here, so `__attribute__((mode(M)))` on a
                    // member did two wrong things at once: it did not size the
                    // member, and it stayed pending, so the next declarator to
                    // consume one -- the enclosing declaration's -- took it
                    // instead. `struct Big { int arr[100]; int x
                    // __attribute__((mode(QI))); } b;` gave `sizeof b == 1`
                    // where gcc gives 404. The eight declarator sites all do
                    // this; the member loop was the gap.
                    let typ = self.apply_pending_type_attrs(typ);

                    // Capture any pending _Alignas from type specifier
                    let member_align = self.pending_alignas.take();

                    // C17 6.7.2.1p2: members share one name space, so a
                    // repeated name is a constraint violation. Unnamed members
                    // -- anonymous struct/union members and unnamed bitfields
                    // -- all carry the empty name and are not repeats of each
                    // other.
                    if name != StringId::EMPTY && members.iter().any(|m| m.name == name) {
                        let spelled = self.idents.get_opt(name).unwrap_or("").to_string();
                        diag::error_args(self.current_pos(), "duplicate member '{0}'", &[&spelled]);
                    }

                    members.push(StructMember {
                        name,
                        typ,
                        offset: 0, // Computed later
                        bit_offset: None,
                        bit_width,
                        access_bytes: None,
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

            // Parse trailing __attribute__ (e.g., __attribute__((packed)))
            let attrs = self.parse_attributes();
            is_packed = is_packed
                || attrs
                    .attrs
                    .iter()
                    .any(|a| a.name == "packed" || a.name == "__packed__");
            is_transparent = is_transparent || attrs.has_transparent_union();
            if let Some(a) = attrs.get_alignment() {
                struct_align = Some(struct_align.map_or(a, |e| e.max(a)));
            }

            self.check_flexible_array_members(&members, is_union);

            // Compute layout. `__attribute__((packed))` is a cap of 1; a
            // `#pragma pack(n)` in force is a cap of n. Where both apply the
            // tighter one wins, which is what gcc does.
            let pragma_cap = self.current_pack();
            let pack_cap = match (is_packed, pragma_cap) {
                (true, Some(n)) => Some(n.min(1)),
                (true, None) => Some(1),
                (false, cap) => cap,
            };
            let (size, mut align) = if is_union {
                self.types.compute_union_layout(&mut members, pack_cap)
            } else {
                self.types.compute_struct_layout(&mut members, pack_cap)
            };

            // Apply struct-level aligned attribute (raises alignment, never lowers)
            if let Some(sa) = struct_align {
                if sa as usize > align {
                    align = sa as usize;
                }
            }

            // Re-pad size to new alignment
            let size = if align > 1 {
                (size + align - 1) & !(align - 1)
            } else {
                size
            };

            // The same bound `derive_array_type` enforces: a member list can
            // reach it even when no single member does.
            if size > TypeTable::MAX_OBJECT_BYTES {
                return Err(ParseError::new(
                    format!(
                        "size of {} exceeds the maximum object size of {} bytes",
                        if is_union { "union" } else { "struct" },
                        TypeTable::MAX_OBJECT_BYTES
                    ),
                    specifier_pos,
                ));
            }

            if is_transparent && !is_union {
                self.warn_transparent_union_ignored(specifier_pos);
            }

            let composite = CompositeType {
                tag,
                members,
                enum_constants: Vec::new(),
                size,
                align,
                is_complete: true,
                transparent: is_transparent && is_union,
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

    /// Consume the type-qualifier run after a `*` in a declarator.
    ///
    /// C17 6.7.6.1 lets any type qualifier appear there, `_Atomic` included:
    /// `int *_Atomic p;` declares an atomic pointer to int. Three copies of
    /// this loop had drifted apart and only the one in `parse_declarator`
    /// listed `_Atomic`, so the same declaration parsed inside a function and
    /// failed at file scope -- where `_Atomic` fell through to the name
    /// position and was taken as the identifier instead.
    fn parse_pointer_qualifiers(&mut self) -> TypeModifiers {
        let mut modifiers = TypeModifiers::empty();
        while self.peek() == TokenType::Ident {
            let Some(name_id) = self.get_ident_id(self.current()) else {
                break;
            };
            match name_id {
                crate::kw::CONST => modifiers |= TypeModifiers::CONST,
                crate::kw::VOLATILE => modifiers |= TypeModifiers::VOLATILE,
                crate::kw::RESTRICT => modifiers |= TypeModifiers::RESTRICT,
                crate::kw::ATOMIC => modifiers |= TypeModifiers::ATOMIC,
                _ if super::is_nullability_qualifier(name_id) => {}
                _ => break,
            }
            self.advance();
        }
        modifiers
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
    pub(crate) fn parse_declarator(
        &mut self,
        base_type_id: TypeId,
        name: DeclaratorName,
    ) -> ParseResult<DeclaratorResult> {
        // Collect pointer modifiers (they bind tighter than array/function)
        let mut pointer_modifiers: Vec<TypeModifiers> = Vec::new();
        while self.is_special(b'*') {
            self.advance();
            let mut ptr_modifiers = TypeModifiers::empty();

            ptr_modifiers |= self.parse_pointer_qualifiers();
            pointer_modifiers.push(ptr_modifiers);
        }

        // Check for parenthesized declarator: int (*p)[3]
        // The paren comes AFTER pointers, e.g. int *(*p)[3] = pointer to (pointer to array of 3 ints)
        let (name, inner_type_id, inner_func_params) = if self.is_special(b'(') {
            // Check if this looks like a function parameter list or a grouped declarator
            // A grouped declarator will have * or identifier immediately after (
            let saved_pos = self.pos;
            self.advance(); // consume '('

            // One predicate decides this everywhere. The copy that used to
            // live here treated *any* identifier as a grouped declarator,
            // where `is_grouped_declarator` excludes typedef names and type
            // keywords -- so `void f(int (size_t));`, a function-type
            // parameter, was read as a declarator named `size_t`.
            let is_grouped = self.is_grouped_declarator();

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
                    self.parse_declarator(self.types.void_id, name)?;
                self.expect_special(b')')?;

                // Now parse any suffix modifiers (arrays, function params)
                // These apply to the base type, not the inner declarator
                (inner_name, Some(inner_decl_type_id), inner_func_params)
            } else {
                // The `(` opens a parameter list, so this declarator is
                // abstract: `int (size_t)` names a function type, and there is
                // no identifier to find. Rewind to the `(` and let the
                // function-suffix loop below consume the parameter list.
                //
                // This branch used to call `expect_declarator_name()` here,
                // which cannot succeed while positioned on `(`. It was
                // unreachable in practice because the old predicate claimed
                // every identifier -- including a typedef name or a type
                // keyword -- as a grouped declarator, so an abstract function
                // type reached the recursive branch and had its first
                // parameter's type name mistaken for the declarator's name.
                self.pos = saved_pos;
                (StringId::EMPTY, None, None)
            }
        } else if self.peek() == TokenType::Ident {
            (self.expect_declarator_name()?, None, None)
        } else if name == DeclaratorName::Optional {
            // An abstract declarator has no identifier by construction
            // (C17 6.7.7): `void (*)(int)`, or a parameter written as a bare
            // type. Whether that is allowed is the caller's question, not a
            // guess from the next token -- this used to be a whitelist of
            // `)`, `[`, `(`, `,`, so a type-name ending in anything else was
            // rejected outright. `_Generic(1, int: 11)` ends its type-name at
            // a `:`, which was not on the list.
            (StringId::EMPTY, None, None)
        } else {
            (self.expect_declarator_name()?, None, None)
        };

        // Handle array declarators - collect all dimensions first
        // Also track VLA expressions (non-constant size) for each dimension
        let mut dimensions: Vec<(Option<usize>, Position)> = Vec::new();
        let mut vla_exprs: Vec<Expr> = Vec::new();
        while self.is_special(b'[') {
            let dim_pos = self.current_pos();
            self.advance();

            // Parse optional qualifiers and static (C99 6.7.5.3)
            // These are valid in function parameter array declarators
            while self.peek() == TokenType::Ident {
                if let Some(name_id) = self.get_ident_id(self.current()) {
                    match name_id {
                        // C17 6.7.6.2: the array declarator of a parameter
                        // takes a type-qualifier list, which includes
                        // `_Atomic`, and optionally `static`.
                        crate::kw::STATIC
                        | crate::kw::CONST
                        | crate::kw::VOLATILE
                        | crate::kw::RESTRICT
                        | crate::kw::ATOMIC => {
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
                    let size_pos = self.current_pos();
                    let expr = self.parse_assignment_expr()?;
                    match self.eval_const_expr(&expr) {
                        Some(n) if n >= 0 => Some(n as usize),
                        Some(_) => {
                            return Err(ParseError::new(
                                "size of array is negative".to_string(),
                                size_pos,
                            ));
                        }
                        None => {
                            self.check_array_size_type(&expr, size_pos)?;
                            vla_exprs.push(expr);
                            None
                        }
                    }
                }
            } else {
                // Parse constant expression for array size (C99 6.7.5.2)
                let size_pos = self.current_pos();
                let expr = self.parse_assignment_expr()?;
                // Evaluate as integer constant expression
                match self.eval_const_expr(&expr) {
                    Some(n) if n >= 0 => Some(n as usize),
                    // C17 6.7.6.2p1: the size shall be greater than zero.
                    // This used to become `None`, i.e. an incomplete array,
                    // so `int a[-1]` was accepted and silently sized zero.
                    Some(_) => {
                        // gcc distinguishes the two, and the abstract case is
                        // the one a type-name reaches: `sizeof(char[-1])`
                        // says "unnamed", `char a[-1];` names `a`.
                        return Err(ParseError::new(
                            if name == StringId::EMPTY {
                                "size of unnamed array is negative".to_string()
                            } else {
                                format!("size of array '{}' is negative", self.idents.get(name))
                            },
                            size_pos,
                        ));
                    }
                    None => {
                        // Non-constant (VLA) - save expression for VLA handling
                        self.check_array_size_type(&expr, size_pos)?;
                        vla_exprs.push(expr);
                        None
                    }
                }
            };
            self.expect_special(b']')?;
            dimensions.push((size, dim_pos));
        }

        // Handle function declarators: void (*fp)(int, char)
        // This parses the parameter list after a grouped declarator
        // We keep both the TypeIds (for building the type) and raw params (for function defs)
        let (func_params, full_func_params): (Option<FuncSignature>, Option<Vec<RawParam>>) =
            if self.is_special(b'(') {
                self.advance();
                let list = self.parse_parameter_list()?;
                self.expect_special(b')')?;
                let param_types: Vec<TypeId> = list.params.iter().map(|p| p.typ).collect();
                (
                    Some(FuncSignature {
                        param_types,
                        variadic: list.variadic,
                        prototyped: list.prototyped,
                    }),
                    Some(list.params),
                )
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
                    ..Default::default()
                };
                result_type_id = self.types.intern(ptr_type);
            }

            // Apply function parameters to (possibly pointer-modified) base type
            // For struct node *(*fp)(int): result is Pointer(struct node)
            //   -> Function(Pointer(struct node), [int])
            if let Some(sig) = func_params {
                let func_type = sig.into_type(result_type_id);
                result_type_id = self.types.intern(func_type);
            }

            // Apply array dimensions to result type
            // For int *(*q)[3]: result is Pointer(int) -> Array(3, Pointer(int))
            for (size, pos) in dimensions.into_iter().rev() {
                result_type_id = self.derive_array_type(result_type_id, size, pos)?;
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
                    ..Default::default()
                };
                result_type_id = self.types.intern(ptr_type);
            }

            // Then apply array dimensions
            // For char *arr[3]: result_type is char*, suffix [3] -> Array(3, char*)
            for (size, pos) in dimensions.into_iter().rev() {
                result_type_id = self.derive_array_type(result_type_id, size, pos)?;
            }

            // Apply function parameters if present (for function declarators)
            // For int get_op(int which): base is int, suffix (int) -> Function(int, [int])
            // This is needed for nested declarators like int (*get_op(int))(int, int)
            if let Some(sig) = func_params {
                let func_type = sig.into_type(result_type_id);
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
                    ..Default::default()
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
                    ..Default::default()
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
                    base: Some(new_ret_id),
                    params: decl_params,
                    variadic: decl_variadic,
                    noreturn: decl_noreturn,
                    ..Default::default()
                };
                self.types.intern(func_type)
            }
            _ => decl_type_id, // Other types don't need substitution
        }
    }

    /// Parse a parameter list, returning raw parameter info (name and type)
    /// Parameters are declared in a temporary scope during parsing so that
    /// VLA sizes like `arr[n]` can reference earlier parameters like `n`.
    /// The scope is exited at the end; callers re-declare parameters as needed.
    pub(crate) fn parse_parameter_list(&mut self) -> ParseResult<ParameterList> {
        let mut params: Vec<RawParam> = Vec::with_capacity(DEFAULT_PARAM_CAPACITY);
        let mut variadic = false;
        let mut prototyped = true;

        // Enter a temporary scope for parameter parsing (C99 6.9.1p9)
        // This allows VLA sizes to reference earlier parameters
        self.symbols.enter_scope();

        // `()` -- an empty identifier list, which is not a prototype.
        if self.is_special(b')') {
            self.symbols.leave_scope();
            return Ok(ParameterList {
                params,
                variadic,
                prototyped: false,
            });
        }

        // Check for (void)
        if self.peek() == TokenType::Ident {
            if let Some(name_id) = self.get_ident_id(self.current()) {
                if name_id == crate::kw::VOID {
                    let saved_pos = self.pos;
                    self.advance();
                    if self.is_special(b')') {
                        self.symbols.leave_scope();
                        return Ok(ParameterList {
                            params,
                            variadic,
                            prototyped: true,
                        });
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
                        &gettext("ISO C requires a named argument before '...'"),
                    );
                }
                self.advance();
                variadic = true;
                break;
            }

            // Parse parameter type
            let param_type = self.parse_type_specifier()?;
            // An identifier list -- `int f(a, b) int a, b;` -- is not a
            // prototype (C17 6.7.6.3p14), and it is exactly the case where the
            // specifier parser supplied an implicit `int` without consuming an
            // identifier. The choice is all-or-nothing across the list, so the
            // first parameter settles it.
            if params.is_empty() && !variadic {
                prototyped = self.saw_explicit_type;
            }
            // For struct/union types with tags, use existing TypeId to preserve forward declarations
            let base_type_id = self.intern_type_with_tag(&param_type);

            // Use parse_declarator to handle all declarator forms including:
            // - Simple pointers: void *, int *
            // - Grouped declarators: void (*)(int), int (*)[10]
            // - Arrays: int arr[], int arr[10]
            // Note: parse_declarator returns (name, type, vla_sizes)
            let (param_name, mut typ_id, vla_sizes, _func_params) =
                self.parse_declarator(base_type_id, DeclaratorName::Optional)?;

            // Skip any __attribute__ after parameter declarator
            self.skip_extensions();

            // A parameter's type attributes are the parameter's, and are
            // applied before the array-to-pointer adjustment below so a mode
            // names the declared type rather than the adjusted one. Nothing
            // consumed them here, so `int x __attribute__((mode(QI)))` was
            // silently an `int`.
            typ_id = self.apply_pending_type_attrs(typ_id);

            // C99 6.7.5.3: Array and function parameters are adjusted to pointers
            // - Array T[] becomes pointer to T
            // - Function type becomes pointer to function type
            let typ = self.types.get(typ_id);
            if typ.kind == TypeKind::Array {
                // Convert array to pointer to element type
                let element_type = typ.base.unwrap_or(self.types.void_id);
                let ptr_type = Type {
                    kind: TypeKind::Pointer,
                    base: Some(element_type),
                    ..Default::default()
                };
                typ_id = self.types.intern(ptr_type);
            } else if typ.kind == TypeKind::Function {
                // Convert function type to pointer to function type
                // e.g., `int fn(int)` becomes `int (*)(int)`
                let ptr_type = Type {
                    kind: TypeKind::Pointer,
                    base: Some(typ_id),
                    ..Default::default()
                };
                typ_id = self.types.intern(ptr_type);
            }

            let name_opt = if param_name == StringId::EMPTY {
                None
            } else {
                Some(param_name)
            };

            // Keep the run-time dimensions of a variably-modified element
            // type. Every other declarator path already carries `vla_sizes`;
            // dropping them here is what left `int a[n][m]` indexing with a
            // row stride of zero.
            //
            // `vla_sizes` runs outermost-first over the whole declarator,
            // while only the element type matters after the array-to-pointer
            // adjustment. The outermost dimension is the one that may be
            // absent (`int a[][m]`), and C17 6.7.6.2p1 requires every later
            // dimension to be present, so the element type's variable
            // dimensions are exactly the trailing entries.
            let elem_typ = self.types.get(typ_id).base;
            let vm_dims = match elem_typ {
                Some(elem) => {
                    let want = self.types.unsized_array_levels(elem);
                    let skip = vla_sizes.len().saturating_sub(want);
                    vla_sizes[skip..].to_vec()
                }
                None => Vec::new(),
            };

            // C17 6.7.6.3p10: `void` may appear as a parameter only as the
            // unnamed sole item in the list -- and then it means the function
            // takes none. The literal `(void)` is recognised earlier, on the
            // token; this is the same thing reached through a typedef, as in
            // `typedef void V; int f(V);`, which must not become a
            // one-parameter prototype.
            if self.types.kind(typ_id) == TypeKind::Void {
                if name_opt.is_none() && params.is_empty() && self.is_special(b')') {
                    self.symbols.leave_scope();
                    return Ok(ParameterList {
                        params,
                        variadic,
                        prototyped: true,
                    });
                }
                diag::warning_args(
                    self.current_pos(),
                    "parameter {0} has void type",
                    &[&(params.len() + 1).to_string()],
                );
            }

            params.push(RawParam {
                name: name_opt,
                typ: typ_id,
                vm_dims,
                symbol: None,
            });

            // Declare parameter in temporary scope so later params can reference it
            // (C99 6.9.1p9: parameters are in scope for VLA sizes)
            if let Some(name) = name_opt {
                let sym = Symbol::parameter(name, typ_id, self.symbols.depth())
                    .with_variably_modified_array(!vla_sizes.is_empty());
                // 6.9.1p5: no two parameters may share a name. `declare`
                // reports it and the `Err` was dropped, leaving the second
                // parameter with no symbol at all -- so the function compiled
                // and every use of the name reached the first one.
                match self.symbols.declare(sym) {
                    Ok(sym_id) => {
                        if let Some(last) = params.last_mut() {
                            last.symbol = Some(sym_id);
                        }
                    }
                    Err(_) => {
                        let spelled = self.idents.get_opt(name).unwrap_or("").to_string();
                        diag::error_args(
                            self.current_pos(),
                            "redefinition of parameter '{0}'",
                            &[&spelled],
                        );
                    }
                }
            }

            if self.is_special(b',') {
                self.advance();
                // C17 6.7.6.3: a parameter-type-list is a comma-separated list
                // of parameter declarations, optionally followed by `, ...`.
                // Nothing else may follow the comma. Falling through here let
                // `parse_type_specifier` supply an implicit `int`, so
                // `void g(int, );` silently declared `void(int, int)` -- and
                // once call arity was checked, the *correct* call `g(1)`
                // became the one rejected. (C23 permits the trailing comma;
                // this compiler is C17.)
                if self.is_special(b')') {
                    return Err(ParseError::new(
                        "expected a declaration specifier or '...' after ','".to_string(),
                        self.current_pos(),
                    ));
                }
            } else {
                break;
            }
        }

        // Leave temporary parameter scope
        self.symbols.leave_scope();

        Ok(ParameterList {
            params,
            variadic,
            prototyped,
        })
    }

    /// Parse a translation unit (top-level)
    pub fn parse_translation_unit(&mut self) -> ParseResult<TranslationUnit> {
        let mut tu = TranslationUnit::default();

        self.skip_stream_tokens();

        while !self.is_eof() {
            // Try to determine if this is a function definition or a declaration
            // Both start with type specifier + declarator
            let external_decl = self.parse_external_decl()?;
            tu.add(external_decl);
        }

        self.check_deferred_incomplete_definitions();

        Ok(tu)
    }

    /// C17 6.7p7: an object's type must be complete where the object is
    /// *defined*. At file scope that cannot be judged where the declaration
    /// appears, because 6.9.2p3 lets a tentative definition be completed later
    /// in the translation unit -- `struct U; struct U u; struct U { int a; };`
    /// is legal, and forward-declare-then-complete is everywhere in CPython and
    /// glibc. So file-scope definitions are collected as they are parsed and
    /// judged here, when nothing more can complete them.
    fn check_deferred_incomplete_definitions(&mut self) {
        for (typ, pos) in std::mem::take(&mut self.tentative_definitions) {
            // Ask the *tag*, not the recorded id. A qualified spelling --
            // `volatile struct S` -- is interned as a fresh type carrying a
            // clone of the tag's composite data as it stood at the time
            // (`intern_type_with_tag`), and `complete_struct` only ever
            // mutates the tag's own entry. So the recorded id is a frozen
            // `is_complete: false` that completing the tag never updates, and
            // `struct S; volatile struct S vs; struct S { int a; };` was
            // rejected although the tag is complete.
            let typ = self.resolve_struct_type(typ);
            if self.types.is_composite_complete(typ) {
                continue;
            }
            let named = self.types.format_type(typ, Some(self.idents));
            diag::error_args(
                pos,
                "storage size of an object of type '{0}' is not known",
                &[&named],
            );
        }
    }

    /// Check if current token is _Static_assert or static_assert
    fn is_static_assert(&self) -> bool {
        if self.peek() != TokenType::Ident {
            return false;
        }
        if let Some(id) = self.get_ident_id(self.current()) {
            crate::kw::has_tag(id, crate::kw::ASSERT_KW)
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

    /// Fold the attributes pending on this declaration into what `name` has
    /// collected so far, and return the total.
    ///
    /// Called for declarations as well as definitions, so that an attribute on
    /// a prototype reaches the definition parsed later; see
    /// [`Parser::declared_fn_attrs`].
    fn accumulate_fn_attrs(&mut self, name: StringId) -> crate::parse::ast::FunctionAttrs {
        let pending = self.pending_fn_attrs.clone();
        let seen = self.declared_fn_attrs.entry(name).or_default();
        seen.merge(&pending);
        seen.clone()
    }

    /// Parse an external declaration (function definition or declaration)
    pub(crate) fn parse_external_decl(&mut self) -> ParseResult<ExternalDecl> {
        // Clear pending alignment from previous declaration
        self.pending_alignas = None;
        // A mode that no declarator consumed belongs to no later declaration:
        // leaving it set applied it to whatever came next.
        self.pending_mode = None;
        self.pending_transparent_union = None;
        self.pending_fn_attrs = Default::default();
        // And any asm label the previous declaration left behind.
        //
        // `skip_extensions` collects one wherever it runs, which is most
        // places a type specifier can appear, but only a file-scope declarator
        // claims one -- so a label written on a block-scope declaration, on a
        // struct definition, or in a shape this parser does not model at all
        // stays pending and is claimed by whatever is declared next. The
        // symptom is an unrelated global emitted under someone else's
        // assembler name.
        self.pending_asm_label = None;
        // Likewise the symbol attributes. A function *definition* takes its
        // attributes through `pending_fn_attrs` and leaves these behind, so the
        // next declaration to build an `InitDeclarator` claimed them: the
        // variable after a `section(...)` function was emitted into that
        // function's section, and the conflicting "ax"/"aw" flags made the
        // assembler reject the file outright.
        self.pending_symbol_attrs = Default::default();

        // Check for _Static_assert first (C11)
        if self.is_static_assert() {
            self.parse_static_assert()?;
            // Return empty declaration - static_assert produces nothing
            return Ok(ExternalDecl::Declaration(Declaration {
                declarators: vec![],
            }));
        }

        // A stray `;` at file scope is an empty declaration. C17 6.7p2 makes it
        // a constraint violation, but GCC and Clang accept it by default (they
        // warn only under -pedantic) and it is common in real source: any
        // function-like macro that expands to nothing and is invoked with a
        // trailing semicolon produces one. CPython's `_Py_DECLARE_STR()` is
        // exactly that. Consume it before reaching the type specifier, which
        // would otherwise report a spurious "type specifier missing".
        if self.is_special(b';') {
            self.advance();
            return Ok(ExternalDecl::Declaration(Declaration {
                declarators: vec![],
            }));
        }

        let decl_pos = self.current_pos();
        // Parse type specifier
        let base_type = self.parse_type_specifier()?;
        self.check_implicit_int(decl_pos);
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
            | TypeModifiers::REGISTER
            // `inline` is a function specifier rather than a storage class,
            // but it travels with them here: leaving it out made
            // `FunctionDef::is_inline` false for every ordinary definition,
            // and with it every consumer downstream.
            | TypeModifiers::INLINE;
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
                let (name, mut typ, vla_sizes, decl_func_params) =
                    self.parse_declarator(base_type_id, DeclaratorName::Required)?;

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
                let fn_attrs = attrs.function_attrs();
                self.pending_fn_attrs.merge(&fn_attrs);
                // Symbol attributes too. Only the function half was collected
                // here, so `weak` written after a function declarator was
                // dropped -- and a weak *declaration* is the whole point of
                // the attribute.
                self.merge_symbol_attrs(&attrs);
                let all_fn_attrs = if self.types.kind(typ) == TypeKind::Function {
                    self.accumulate_fn_attrs(name)
                } else {
                    Default::default()
                };

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
                    self.check_redeclaration(name, typ, decl_pos);
                    let func_sym = Symbol::function(name, typ, self.symbols.depth());
                    let _ = self.symbols.declare(func_sym);
                    // Definitions bind a fresh symbol, so this path needs the
                    // accumulated facts settled onto it exactly as the other
                    // two function-definition paths do -- without it, this
                    // definition's C99 6.7.4p6 inline classification is
                    // computed from declarations it never saw.
                    self.settle_declaration_facts(name, storage_class);

                    // Get raw parameters - use decl_func_params which has names
                    let raw_params = decl_func_params.unwrap_or_default();

                    // Enter function scope for parameters
                    self.symbols.enter_scope();

                    // Bind parameters in function scope and create Parameter structs
                    let mut params = Vec::with_capacity(raw_params.len());
                    for raw in &raw_params {
                        let symbol_id = raw.symbol.map(|id| self.symbols.redeclare(id, raw.typ));
                        params.push(Parameter {
                            symbol: symbol_id,
                            typ: raw.typ,
                            vm_dims: raw.vm_dims.clone(),
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
                        attrs: all_fn_attrs,
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

                self.skip_extensions();
                self.expect_special(b';')?;

                // Validate explicit alignment (C11 6.7.5: >= natural alignment)
                typ = self.apply_pending_type_attrs(typ);
                let validated_align = self.validated_explicit_align(typ)?;

                // Add to symbol table and capture SymbolId
                // C allows multiple declarations of the same variable at file scope
                let symbol_id = if is_typedef {
                    // A mode replaces the type; alignment then attaches to
                    // whatever the type ended up being.
                    typ = self.apply_pending_type_attrs(typ);
                    // Apply __attribute__((aligned(N))) to typedef type
                    if let Some(align) = self.pending_alignas {
                        let mut aligned_type = self.types.get(typ).clone();
                        aligned_type.explicit_align =
                            Some(aligned_type.explicit_align.map_or(align, |e| e.max(align)));
                        typ = self.types.intern(aligned_type);
                    }
                    self.check_typedef_redefinition(name, typ, decl_pos);
                    let sym = Symbol::typedef(name, typ, self.symbols.depth());
                    self.symbols
                        .declare(sym)
                        .ok()
                        .or_else(|| self.symbols.lookup_id(name, Namespace::Ordinary))
                } else {
                    self.check_redeclaration(name, typ, decl_pos);
                    let var_sym = Symbol::variable(name, typ, self.symbols.depth())
                        .with_align(validated_align);
                    self.symbols
                        .declare(var_sym)
                        .ok()
                        .or_else(|| self.symbols.lookup_id(name, Namespace::Ordinary))
                };

                let symbol = symbol_id.expect("declaration must have symbol");
                self.settle_declaration_facts(name, storage_class);
                return Ok(ExternalDecl::Declaration(Declaration {
                    declarators: vec![InitDeclarator {
                        symbol_attrs: std::mem::take(&mut self.pending_symbol_attrs),
                        symbol,
                        typ,
                        storage_class,
                        init,
                        vla_sizes: vec![],
                        explicit_align: validated_align,
                        pos: decl_pos,
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

            ptr_modifiers |= self.parse_pointer_qualifiers();

            let ptr_type = Type {
                kind: TypeKind::Pointer,
                modifiers: ptr_modifiers,
                base: Some(typ_id),
                ..Default::default()
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
                | TypeModifiers::THREAD_LOCAL
                // Without this a pointer-returning `inline` function loses the
                // bit twice over -- `memcpy` returning `void *` is exactly the
                // shape glibc uses.
                | TypeModifiers::INLINE;
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
                let (name, mut full_typ, vla_sizes, decl_func_params) =
                    self.parse_declarator(typ_id, DeclaratorName::Required)?;

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
                let fn_attrs = attrs.function_attrs();
                self.pending_fn_attrs.merge(&fn_attrs);
                // Symbol attributes too. Only the function half was collected
                // here, so `weak` written after a function declarator was
                // dropped -- and a weak *declaration* is the whole point of
                // the attribute.
                self.merge_symbol_attrs(&attrs);
                let all_fn_attrs = if self.types.kind(full_typ) == TypeKind::Function {
                    self.accumulate_fn_attrs(name)
                } else {
                    Default::default()
                };

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
                    self.check_redeclaration(name, full_typ, decl_pos);
                    let func_sym = Symbol::function(name, full_typ, self.symbols.depth());
                    let _ = self.symbols.declare(func_sym);
                    self.settle_declaration_facts(name, storage_class);

                    // Get raw parameters - use decl_func_params which has names
                    let raw_params = decl_func_params.unwrap_or_default();

                    // Enter function scope for parameters
                    self.symbols.enter_scope();

                    // Bind parameters in function scope and create Parameter structs
                    let mut params = Vec::with_capacity(raw_params.len());
                    for raw in &raw_params {
                        let symbol_id = raw.symbol.map(|id| self.symbols.redeclare(id, raw.typ));
                        params.push(Parameter {
                            symbol: symbol_id,
                            typ: raw.typ,
                            vm_dims: raw.vm_dims.clone(),
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
                        attrs: all_fn_attrs,
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

                self.skip_extensions();
                self.expect_special(b';')?;

                // Validate explicit alignment (C11 6.7.5: >= natural alignment)
                full_typ = self.apply_pending_type_attrs(full_typ);
                let validated_align = self.validated_explicit_align(full_typ)?;

                // Add to symbol table and capture SymbolId
                // C allows multiple declarations of the same variable at file scope
                let symbol_id = if is_typedef {
                    self.check_typedef_redefinition(name, full_typ, decl_pos);
                    let sym = Symbol::typedef(name, full_typ, self.symbols.depth());
                    self.symbols
                        .declare(sym)
                        .ok()
                        .or_else(|| self.symbols.lookup_id(name, Namespace::Ordinary))
                } else {
                    self.check_redeclaration(name, full_typ, decl_pos);
                    let var_sym = Symbol::variable(name, full_typ, self.symbols.depth())
                        .with_align(validated_align);
                    self.symbols
                        .declare(var_sym)
                        .ok()
                        .or_else(|| self.symbols.lookup_id(name, Namespace::Ordinary))
                };

                let symbol = symbol_id.expect("declaration must have symbol");
                self.settle_declaration_facts(name, storage_class);
                return Ok(ExternalDecl::Declaration(Declaration {
                    declarators: vec![InitDeclarator {
                        symbol_attrs: std::mem::take(&mut self.pending_symbol_attrs),
                        symbol,
                        typ: full_typ,
                        storage_class,
                        init,
                        vla_sizes: vec![],
                        explicit_align: validated_align,
                        pos: decl_pos,
                    }],
                }));
            }
            // Not a grouped declarator, restore position
            self.pos = saved_pos;
        }

        // Parse name
        let name = self.expect_declarator_name()?;

        // Check for function definition vs declaration
        if self.is_special(b'(') {
            // Could be function definition or declaration
            self.advance();
            let param_list = self.parse_parameter_list()?;
            let (variadic, prototyped) = (param_list.variadic, param_list.prototyped);
            let mut params = param_list.params;
            self.expect_special(b')')?;

            // Parse __attribute__ after parameter list (e.g., __attribute__((noreturn)))
            let attrs = self.parse_attributes();
            let fn_attrs = attrs.function_attrs();
            self.pending_fn_attrs.merge(&fn_attrs);
            self.merge_symbol_attrs(&attrs);
            let all_fn_attrs = self.accumulate_fn_attrs(name);
            // noreturn can come from __attribute__((noreturn)) or _Noreturn keyword in base type
            let typ_from_table = self.types.get(typ_id);
            let is_noreturn =
                attrs.has_noreturn() || typ_from_table.modifiers.contains(TypeModifiers::NORETURN);
            // Extract calling convention from attributes
            let calling_conv = attrs.calling_conv().unwrap_or_default();

            // K&R (old-style) parameter declarations: type declarations between ) and {
            // e.g., int add(a, b) int a; int b; { ... }
            // The parameter list already parsed bare names with implicit int.
            // Now parse explicit type declarations and update parameter types.
            if self.is_declaration_start() && !self.is_special(b'{') {
                while self.is_declaration_start() {
                    let knr_type = self.parse_type_specifier()?;
                    let knr_base_id = self.intern_type_with_tag(&knr_type);
                    loop {
                        let (decl_name, mut decl_typ, _vla, _fparams) =
                            self.parse_declarator(knr_base_id, DeclaratorName::Required)?;
                        // C99 6.7.5.3: array/function params adjusted to pointers
                        let typ = self.types.get(decl_typ);
                        if typ.kind == TypeKind::Array {
                            let elem = typ.base.unwrap_or(self.types.void_id);
                            decl_typ = self.types.intern(Type {
                                kind: TypeKind::Pointer,
                                base: Some(elem),
                                ..Default::default()
                            });
                        } else if typ.kind == TypeKind::Function {
                            decl_typ = self.types.intern(Type {
                                kind: TypeKind::Pointer,
                                base: Some(decl_typ),
                                ..Default::default()
                            });
                        }
                        // Update matching parameter type
                        if decl_name != StringId::EMPTY {
                            for param in &mut params {
                                if param.name == Some(decl_name) {
                                    param.typ = decl_typ;
                                }
                            }
                        }
                        if self.is_special(b',') {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    self.expect_special(b';')?;
                }
            }

            if self.is_special(b'{') {
                // Function definition
                // Use storage_class extracted from original base_type at line 5926,
                // not from typ_id which may have lost storage class for tagged structs
                let is_static = storage_class.contains(TypeModifiers::STATIC);
                let is_inline = storage_class.contains(TypeModifiers::INLINE);

                // Add function to symbol table so it can be called by other functions
                let param_type_ids: Vec<TypeId> = params.iter().map(|p| p.typ).collect();
                let func_type = if prototyped {
                    Type::function(typ_id, param_type_ids.clone(), variadic, is_noreturn)
                } else {
                    Type::function_no_prototype(typ_id, is_noreturn)
                };
                let func_type_id = self.types.intern(func_type);
                self.check_redeclaration(name, func_type_id, decl_pos);
                let func_sym = Symbol::function(name, func_type_id, self.symbols.depth());
                let _ = self.symbols.declare(func_sym);
                self.settle_declaration_facts(name, storage_class);

                // Enter function scope for parameters
                self.symbols.enter_scope();

                // Bind parameters in function scope and create Parameter structs
                let mut final_params = Vec::with_capacity(params.len());
                for raw in &params {
                    let symbol_id = raw.symbol.map(|id| self.symbols.redeclare(id, raw.typ));
                    final_params.push(Parameter {
                        symbol: symbol_id,
                        typ: raw.typ,
                        vm_dims: raw.vm_dims.clone(),
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
                    attrs: all_fn_attrs,
                }));
            } else {
                // Function declaration
                // Skip __asm("...") symbol aliasing which can appear after function declarator
                self.skip_extensions();
                self.expect_special(b';')?;
                let param_type_ids: Vec<TypeId> = params.iter().map(|p| p.typ).collect();
                let func_type = if prototyped {
                    Type::function(typ_id, param_type_ids, variadic, is_noreturn)
                } else {
                    Type::function_no_prototype(typ_id, is_noreturn)
                };
                let func_type_id = self.types.intern(func_type);
                // Add to symbol table and capture SymbolId
                // C allows multiple declarations of the same function
                let symbol_id = if is_typedef {
                    // Function type typedef: typedef void my_func(void);
                    self.check_typedef_redefinition(name, func_type_id, decl_pos);
                    let sym = Symbol::typedef(name, func_type_id, self.symbols.depth());
                    self.symbols
                        .declare(sym)
                        .ok()
                        .or_else(|| self.symbols.lookup_id(name, Namespace::Ordinary))
                } else {
                    // Function declaration: add so the variadic flag is available when called
                    self.check_redeclaration(name, func_type_id, decl_pos);
                    let func_sym = Symbol::function(name, func_type_id, self.symbols.depth());
                    self.symbols
                        .declare(func_sym)
                        .ok()
                        .or_else(|| self.symbols.lookup_id(name, Namespace::Ordinary))
                };
                let symbol = symbol_id.expect("function declaration must have symbol");
                self.settle_declaration_facts(name, storage_class);
                return Ok(ExternalDecl::Declaration(Declaration {
                    declarators: vec![InitDeclarator {
                        symbol_attrs: std::mem::take(&mut self.pending_symbol_attrs),
                        symbol,
                        typ: func_type_id,
                        storage_class,
                        init: None,
                        vla_sizes: vec![],
                        explicit_align: None, // Functions don't have _Alignas
                        pos: decl_pos,
                    }],
                }));
            }
        }

        // Variable/typedef declaration
        let mut declarators = Vec::new();

        // Handle array - collect dimensions first, build type from right to left
        //
        // This is the first declarator of a file-scope declaration, which has
        // its own dimension loop rather than going through `parse_declarator`.
        // That is why the three "variable length arrays cannot have file
        // scope" checks elsewhere in this function never saw a plain
        // `int bad[n];`: they guard the grouped-declarator, K&R and
        // second-declarator paths. Here every unusable size folded to
        // `unwrap_or(0)`, so the declaration was accepted and sized zero.
        let mut var_type_id = typ_id;
        let mut dimensions: Vec<(Option<usize>, Position)> = Vec::new();
        while self.is_special(b'[') {
            let dim_pos = self.current_pos();
            self.advance();
            let size = if self.is_special(b']') {
                // No size given at all: an incomplete type, which is a
                // tentative definition at file scope and legal.
                None
            } else {
                // Parse constant expression for array size (C99 6.7.5.2)
                let size_pos = self.current_pos();
                let arr_expr = self.parse_assignment_expr()?;
                // Evaluate as integer constant expression
                match self.eval_const_expr(&arr_expr) {
                    Some(n) if n >= 0 => Some(n as usize),
                    // C17 6.7.6.2p1: the size shall be greater than zero. Zero
                    // itself is a GNU extension gcc accepts, so only a
                    // negative size is refused here.
                    Some(_) => {
                        // gcc distinguishes the two, and the abstract case is
                        // the one a type-name reaches: `sizeof(char[-1])`
                        // says "unnamed", `char a[-1];` names `a`.
                        return Err(ParseError::new(
                            if name == StringId::EMPTY {
                                "size of unnamed array is negative".to_string()
                            } else {
                                format!("size of array '{}' is negative", self.idents.get(name))
                            },
                            size_pos,
                        ));
                    }
                    // A size expression that is not a constant makes the type
                    // variably modified, which 6.7.6.2p2 confines to block
                    // scope.
                    None => {
                        self.check_array_size_type(&arr_expr, size_pos)?;
                        return Err(ParseError::new(
                            "variable length arrays cannot have file scope".to_string(),
                            size_pos,
                        ));
                    }
                }
            };
            self.expect_special(b']')?;
            dimensions.push((size, dim_pos));
        }
        // Build type from right to left (innermost dimension first).
        //
        // An absent extent stays `None`, as `parse_declarator` records it.
        // Collapsing it to `Some(0)` here made `int a[];` indistinguishable
        // from the GNU zero-length `int a[0];`, so the two declarator paths
        // disagreed about what "incomplete" looks like and `sizeof a` could
        // not tell them apart.
        for (size, pos) in dimensions.into_iter().rev() {
            var_type_id = self.derive_array_type(var_type_id, size, pos)?;
        }

        // Propagate storage class modifiers from base type to derived array type
        // For "typedef int arr[10]", the TYPEDEF should be on the array type, not just int
        if var_type_id != typ_id {
            let storage_class_mask = TypeModifiers::EXTERN
                | TypeModifiers::STATIC
                | TypeModifiers::TYPEDEF
                | TypeModifiers::REGISTER
                | TypeModifiers::AUTO
                | TypeModifiers::THREAD_LOCAL
                // Without this a pointer-returning `inline` function loses the
                // bit twice over -- `memcpy` returning `void *` is exactly the
                // shape glibc uses.
                | TypeModifiers::INLINE;
            let base_storage_class = self.types.modifiers(typ_id) & storage_class_mask;
            if !base_storage_class.is_empty() {
                let mut var_type = self.types.get(var_type_id).clone();
                var_type.modifiers |= base_storage_class;
                var_type_id = self.types.intern(var_type);
            }
        }

        // Skip any __attribute__ after variable name/array declarator
        self.skip_extensions_after_declarator();

        // Validate explicit alignment (C11 6.7.5: >= natural alignment)
        var_type_id = self.apply_pending_type_attrs(var_type_id);
        let validated_align = self.validated_explicit_align(var_type_id)?;

        // 6.7p7 for a file-scope *definition*. Judged at end of translation
        // unit, since 6.9.2p3 lets a tentative definition be completed later.
        // `extern` and `typedef` define nothing, and a function is not an
        // object.
        if !is_typedef {
            // An array's *element* type has to be complete where the array is
            // declared, because the stride is what forms the type -- so this
            // holds even for `extern`, and even when the tag is completed
            // further down, both of which gcc rejects.
            let elem = self.types.array_element_deep(var_type_id);
            if elem != var_type_id {
                if !self.types.is_composite_complete(elem) {
                    let named = self.types.format_type(elem, Some(self.idents));
                    diag::error_args(
                        self.current_pos(),
                        "array type has incomplete element type '{0}'",
                        &[&named],
                    );
                }
            } else if !base_type.modifiers.contains(TypeModifiers::EXTERN)
                && !self.types.is_composite_complete(var_type_id)
            {
                // A *definition* needs a size. Only this half gets the
                // tentative-definition grace of 6.9.2p3.
                self.tentative_definitions
                    .push((var_type_id, self.current_pos()));
            }
        }

        // Bind variable to symbol table BEFORE parsing initializer.
        // This ensures the variable is in scope for self-referential initializers.
        // Per C99 6.2.1p7: "Any other identifier has scope that begins just
        // after the completion of its declarator."
        // For typedefs, we bind AFTER (since typedef initializers are forbidden anyway).
        let mut symbol = if is_typedef {
            None // Will be bound after initializer parsing
        } else {
            // Add global variable to symbol table so it can be referenced in initializer
            self.check_redeclaration(name, var_type_id, self.current_pos());
            let var_sym = Symbol::variable(name, var_type_id, self.symbols.depth())
                .with_align(validated_align);
            Some(match self.symbols.declare(var_sym) {
                Ok(id) => id,
                Err(_) => {
                    // Extern declaration of existing variable - reuse existing symbol
                    let existing = self
                        .symbols
                        .lookup_id(name, Namespace::Ordinary)
                        .expect("redeclaration should find existing symbol");
                    // 6.2.7p4: the composite of two compatible array types has
                    // whichever extent is known, so a later declaration
                    // completes an earlier `extern int a[];`. Keeping the first
                    // type left the object incomplete for good, and `sizeof a`
                    // after `int a[4];` was refused.
                    if self
                        .types
                        .unsized_array_levels(self.symbols.get(existing).typ)
                        > 0
                        && self.types.unsized_array_levels(var_type_id) == 0
                    {
                        self.symbols.get_mut(existing).typ = var_type_id;
                    }
                    existing
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
            // 6.7.9p5 again, at file scope: the declaration *is* a
            // definition with external linkage, so `extern` adds nothing
            // and gcc warns rather than rejecting.
            if base_type.modifiers.contains(TypeModifiers::EXTERN) {
                diag::warning(
                    init_expr.pos,
                    &gettext("'extern' variable has an initializer"),
                );
            }
            self.check_excess_initializers(var_type_id, init_expr);
            self.check_initializer_types(var_type_id, init_expr);

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
            var_type_id = self.apply_pending_type_attrs(var_type_id);
            // Apply __attribute__((aligned(N))) to typedef type
            if let Some(align) = self.pending_alignas {
                let mut aligned_type = self.types.get(var_type_id).clone();
                aligned_type.explicit_align =
                    Some(aligned_type.explicit_align.map_or(align, |e| e.max(align)));
                var_type_id = self.types.intern(aligned_type);
            }
            self.check_typedef_redefinition(name, var_type_id, self.current_pos());
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
        self.settle_declaration_facts(name, storage_class);
        declarators.push(InitDeclarator {
            symbol_attrs: std::mem::take(&mut self.pending_symbol_attrs),
            symbol,
            typ: var_type_id,
            storage_class,
            init,
            vla_sizes: vec![],
            explicit_align: validated_align,
            pos: decl_pos,
        });

        // Handle additional declarators
        while self.is_special(b',') {
            self.advance();
            let next_decl_pos = self.current_pos();
            let (decl_name, mut decl_type, vla_sizes, _decl_func_params) =
                self.parse_declarator(base_type_id, DeclaratorName::Required)?;

            // An attribute may follow any declarator, not just the first.
            // Without this the second one was a syntax error -- `int a, b
            // __attribute__((unused));` did not compile at file scope, while
            // the same line inside a function did, because the block-scope
            // loop has always parsed attributes after every declarator.
            self.skip_extensions_after_declarator();

            // C99 6.7.5.2: VLAs must have block scope
            if !vla_sizes.is_empty() {
                return Err(ParseError::new(
                    "variable length arrays cannot have file scope".to_string(),
                    self.current_pos(),
                ));
            }

            // Validate explicit alignment for this declarator's type (C11 6.7.5)
            decl_type = self.apply_pending_type_attrs(decl_type);
            let decl_validated_align = self.validated_explicit_align(decl_type)?;

            // Bind variable to symbol table BEFORE parsing initializer (C99 6.2.1p7)
            let mut decl_symbol = if is_typedef {
                None // Will be bound after initializer parsing
            } else {
                self.check_redeclaration(decl_name, decl_type, decl_pos);
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
                // 6.7.9p5 again, at file scope: the declaration *is* a
                // definition with external linkage, so `extern` adds nothing
                // and gcc warns rather than rejecting.
                if base_type.modifiers.contains(TypeModifiers::EXTERN) {
                    diag::warning(
                        init_expr.pos,
                        &gettext("'extern' variable has an initializer"),
                    );
                }
                self.check_excess_initializers(decl_type, init_expr);
                self.check_initializer_types(decl_type, init_expr);

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
                self.check_typedef_redefinition(decl_name, decl_type, decl_pos);
                let sym = Symbol::typedef(decl_name, decl_type, self.symbols.depth());
                decl_symbol = Some(match self.symbols.declare(sym) {
                    Ok(id) => id,
                    Err(_) => self
                        .symbols
                        .lookup_id(decl_name, Namespace::Ordinary)
                        .expect("redeclaration should find existing symbol"),
                });
            }

            self.settle_declaration_facts(decl_name, storage_class);
            declarators.push(InitDeclarator {
                symbol_attrs: std::mem::take(&mut self.pending_symbol_attrs),
                symbol: decl_symbol.expect("symbol should be bound"),
                typ: decl_type,
                storage_class,
                init: decl_init,
                vla_sizes: vec![],
                explicit_align: decl_validated_align,
                pos: next_decl_pos,
            });
        }

        self.expect_special(b';')?;

        // Clear pending alignment after declaration
        self.pending_alignas = None;
        // Belongs to the declaration whose specifiers named the typedef, and
        // to no later one.
        self.pending_vm_typedef_dims = None;
        // A mode that no declarator consumed belongs to no later declaration:
        // leaving it set applied it to whatever came next.
        self.pending_mode = None;
        self.pending_transparent_union = None;

        Ok(ExternalDecl::Declaration(Declaration { declarators }))
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
    /// C17 6.7.2.1p18: an array of unspecified size may appear only as the
    /// *last* member of a structure with more than one named member.
    ///
    /// Nothing in the tree recognised a flexible array member at all, so
    /// `struct S { int a[]; int b; }` compiled and sized the array zero. The
    /// distinction that matters is against the GNU zero-length array: `int
    /// a[]` has no extent while `char d[0]` has one that happens to be zero,
    /// and `unsized_array_levels` tells them apart. Mid-struct `char d[0]` is
    /// an ordinary member and is everywhere in system headers, so conflating
    /// the two would reject far more than this rejects.
    fn check_flexible_array_members(&self, members: &[StructMember], is_union: bool) {
        let is_flexible =
            |m: &StructMember| m.bit_width.is_none() && self.types.unsized_array_levels(m.typ) > 0;

        let Some(first) = members.iter().position(is_flexible) else {
            return;
        };
        let pos = self.current_pos();

        if is_union {
            diag::error(pos, &gettext("flexible array member in union"));
            return;
        }
        if first + 1 != members.len() {
            diag::error(pos, &gettext("flexible array member not at end of struct"));
            return;
        }
        // A named member has to precede it: the array is a tail on something,
        // and a struct that is nothing but a tail has no size to speak of.
        if members
            .iter()
            .take(first)
            .all(|m| m.name == StringId::EMPTY)
        {
            diag::error(
                pos,
                &gettext("flexible array member in a struct with no named members"),
            );
        }
    }

    fn validate_bitfield(&self, typ_id: TypeId, width: u32, is_named: bool) -> ParseResult<()> {
        // C17 6.7.2.1p5 allows `_Bool`, `signed int`, `unsigned int`, and "some
        // other implementation-defined type". gcc's set is every integer type,
        // enumerations included, and real headers lean on `enum E e : 2;`
        // heavily -- a hand-written list of `TypeKind`s omitted `Enum` and
        // rejected all of them. `is_integer` is that set, and already covers
        // every kind the list named.
        if !self.types.is_integer(typ_id) {
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

        // Nothing here carries more than 64 bits of bit-field: the value mask
        // is a `u64` and `bitfield_storage_type` has no arm for a 16-byte unit.
        // An `unsigned __int128 a:100` therefore read back a wrong value in a
        // release build and *panicked* the compiler in a debug one. gcc handles
        // it, so this is a divergence -- but a diagnostic naming the limit is
        // better than either of those, and `__int128` is a GNU extension.
        // Widths up to 64 of such a type keep working and keep agreeing with
        // gcc, so the cap is on the width rather than on the declared type.
        const MAX_BITFIELD_WIDTH: u32 = 64;
        if width > MAX_BITFIELD_WIDTH {
            return Err(ParseError::new(
                format!(
                    "bit-field width {} is not supported; c17 carries at most {} bits",
                    width, MAX_BITFIELD_WIDTH
                ),
                self.current_pos(),
            ));
        }

        // Warning: one-bit signed bitfield has dubious values
        // (can only hold -1 or 0 in 2's complement, or 0/-0 in other representations).
        // `_Bool` needs no exemption here: it is an unsigned type, which
        // `is_unsigned` now reports, so `_Bool f:1` is an ordinary flag.
        if width == 1 && !self.types.is_unsigned(typ_id) {
            diag::warning(
                self.current_pos(),
                &gettext("single-bit signed bit-field has dubious values"),
            );
        }

        Ok(())
    }

    /// Validate explicit alignment against natural alignment (C11 6.7.5)
    ///
    /// Returns the validated explicit alignment, or error if alignment is weaker than natural.
    /// Returns None if no explicit alignment was specified.
    /// Also propagates alignment from typedef's explicit_align.
    fn validated_explicit_align(&mut self, typ: TypeId) -> ParseResult<Option<u32>> {
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
