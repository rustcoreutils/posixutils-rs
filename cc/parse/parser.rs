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

use super::ast::Expr;
use crate::constexpr::ConstScope;
use crate::diag;
use crate::strings::StringId;
use crate::symbol::{Namespace, Symbol, SymbolId, SymbolTable};
use crate::token::lexer::{IdentTable, Position, SpecialToken, Token, TokenType, TokenValue};
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
    /// See [`Parameter::discarded_dims`](crate::parse::ast::Parameter::discarded_dims).
    pub(crate) discarded_dims: Vec<Expr>,
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
/// `int f(a, b) int a, b;` -- is likewise no prototype, so `prototyped`
/// records a distinction the parameter vector alone cannot: a call to
/// `int f(void)` is checked, a call to a K&R definition is not (6.5.2.2p1).
pub(crate) struct ParameterList {
    pub params: Vec<RawParam>,
    pub variadic: bool,
    /// False for `()` and for an identifier list.
    pub prototyped: bool,
}

/// Where a declarator is written, which settles what it may contain.
///
/// C17 spells the two grammars separately -- `declarator` (6.7.6) always has
/// an identifier, `abstract-declarator` (6.7.7) never does -- and 6.7.6.2p1
/// admits `static`, type qualifiers and `[*]` in an array declarator only in
/// a function parameter. Only the caller knows which of these it asked for.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum DeclaratorContext {
    /// An ordinary or member declaration: the identifier is what is being
    /// declared, and the array declarators are plain.
    Declaration,
    /// A parameter declaration in a K&R declaration list. Named, and a
    /// parameter, so its array declarator may carry `static` and qualifiers
    /// (6.7.6.3p7) -- but not in prototype scope, so no `[*]` (6.7.6.2p4).
    OldStyleParameter,
    /// A parameter in a prototype: the identifier may be absent, and the
    /// array declarator takes everything 6.7.6.2p1 allows.
    Parameter,
    /// A type-name: an abstract declarator with plain array declarators.
    TypeName,
}

impl DeclaratorContext {
    /// Whether the declarator must name what it declares.
    pub(crate) fn requires_name(self) -> bool {
        matches!(self, Self::Declaration | Self::OldStyleParameter)
    }

    /// Whether an array declarator may carry `static` or type qualifiers.
    pub(crate) fn is_parameter(self) -> bool {
        matches!(self, Self::OldStyleParameter | Self::Parameter)
    }

    /// Whether `[*]` may appear: only in function prototype scope.
    pub(crate) fn is_prototype_scope(self) -> bool {
        self == Self::Parameter
    }
}

/// A parsed declarator: what it names and the type it derives.
pub(crate) struct ParsedDeclarator {
    /// The declared identifier; `EMPTY` for an abstract declarator.
    pub(crate) name: StringId,
    /// Where the declarator starts, which is where its diagnostics point.
    pub(crate) pos: Position,
    /// Where the declared identifier is written; the declarator's start for
    /// an abstract declarator. gcc points a diagnostic about the declared
    /// type here.
    pub(crate) name_pos: Position,
    /// The derived type. A function declarator's *return* type carries the
    /// specifiers' storage class, as any other derived type does.
    pub(crate) typ: TypeId,
    /// The run-time extents of its variably modified array levels,
    /// outermost-first -- the grouped inner declarator's before the outer
    /// suffix's, since the inner one is the outer level.
    pub(crate) vla: Vec<Expr>,
    /// Where the first run-time extent is written, for the diagnostic that
    /// refuses one where it may not appear.
    pub(crate) vla_pos: Option<Position>,
    /// A function declarator's parameters, with their names, for a
    /// definition to bind.
    pub(crate) params: Option<Vec<RawParam>>,
}

// Parser

/// The function whose body is being parsed, as `return` and the variadic
/// builtins see it.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct EnclosingFunction {
    /// Declared with `...`: `__builtin_va_start` needs it to be.
    pub(crate) variadic: bool,
    /// Variadic and `always_inline`. `__builtin_va_arg_pack()` names the
    /// caller's variadic arguments, so it needs both: there are arguments,
    /// and a known caller to take them from.
    pub(crate) forwarding: bool,
    /// The last named parameter, which `__builtin_va_start` names.
    pub(crate) last_param: Option<StringId>,
    /// The function's calling convention, which decides which of
    /// `__builtin_va_start` and `__builtin_ms_va_start` it may use.
    pub(crate) conv: crate::abi::CallingConv,
    /// The declared return type, which a `return` converts its value to.
    /// `None` outside a function body.
    pub(crate) return_type: Option<TypeId>,
    /// The function's name as written, which `__func__` holds (C17
    /// 6.4.2.2p1) -- not an asm label it is emitted under. `None` outside a
    /// function body.
    pub(crate) name: Option<StringId>,
}

/// C expression parser using recursive descent with precedence climbing
///
/// The parser binds symbols to the symbol table during parsing. This means
/// that by the time parsing is complete, all declared symbols are in the
/// table with their types.
pub struct Parser<'a> {
    /// Token stream
    pub(super) tokens: &'a [Token],
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
    /// Where the `_Alignas` *keyword* was written, when that is what set
    /// `pending_alignas`.
    ///
    /// Kept apart from the value because C11 6.7.5p2 constrains the keyword and
    /// not the GNU `aligned` attribute, which shares the same slot: `aligned`
    /// is legal on a typedef and is the only way to align one, so a check that
    /// could not tell them apart would either miss the constraint or break
    /// every aligned typedef.
    pub(super) pending_alignas_kw: Option<Position>,
    /// The machine mode named by `__attribute__((mode(M)))` on the declaration
    /// being parsed, and where it was written. Held like `pending_alignas`
    /// because the attribute is seen while the declarator is being consumed and
    /// can only be applied once the type is final.
    pub(super) pending_mode: Option<(String, Position)>,

    /// The width in bytes named by `__attribute__((vector_size(N)))` on the
    /// declaration being parsed, and where it was written. Held like
    /// `pending_mode`, and applied at the same point, because it replaces the
    /// declared type.
    pub(super) pending_vector_size: Option<(u64, Position)>,

    /// The alignment written by `__attribute__((aligned(N)))` in the list
    /// being parsed, recorded as it is seen.
    ///
    /// `apply_attribute_alignment` folds the same value into `pending_alignas`
    /// later, which is too late for `vector_size`: a vector aligns to its own
    /// width unless the source says otherwise, and the two are written in one
    /// list -- `__attribute__((vector_size(32), aligned(16)))`.
    pub(super) pending_attr_align: Option<u32>,
    /// `__attribute__((transparent_union))` written *after* the declarator,
    /// which is how glibc spells it -- `typedef union { ... } __SOCKADDR_ARG
    /// __attribute__((__transparent_union__));`. Held for the same reason as
    /// `pending_mode`: the attribute is consumed while the declarator is, and
    /// only the finished type can carry it.
    pub(super) pending_transparent_union: Option<Position>,
    /// `__attribute__((packed))` written on the declaration being parsed.
    /// Only a struct or union member claims it, as half of its
    /// [`crate::types::MemberAlign`]; anywhere else gcc ignores it too, and
    /// the declaration's end clears it.
    pub(super) pending_packed: bool,
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
    pub(super) pending_declarator_align: Option<u32>,
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
    /// `__attribute__((ms_abi))` or `((sysv_abi))` awaiting the declarator
    /// whose function type it belongs to, with where it was written. A
    /// property of the type, so it is applied with the other type attributes
    /// (`apply_pending_type_attrs`) and taken there.
    pub(super) pending_calling_conv: Option<(crate::abi::CallingConv, Position)>,

    /// What the variadic builtins need to know of the function whose body
    /// is being parsed; the default outside any function. This is the last
    /// place it is visible -- `ir::Function` records none of it.
    pub(crate) enclosing_function: EnclosingFunction,
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
    pub(super) declared_asm_labels: BTreeMap<StringId, String>,
    /// Names for which some file-scope declaration carried `extern`.
    ///
    /// Kept beside the symbol table rather than only on the symbol because a
    /// definition declares a *fresh* symbol that shadows the earlier
    /// declaration's, and C99 6.7.4p6 asks about the whole translation unit.
    pub(super) declared_extern_fns: std::collections::BTreeSet<StringId>,
    /// Names for which some file-scope declaration omitted `inline`.
    /// See [`crate::symbol::Symbol::has_non_inline_decl`].
    pub(super) declared_non_inline_fns: std::collections::BTreeSet<StringId>,
    /// Names the translation unit has defined a function under so far, weak
    /// definitions aside; a definition displaces some library builtins (see
    /// `InlineLibraryFn::yields_to_a_definition`). Kept here for the same
    /// reason as `declared_extern_fns`: a later declaration binds a fresh
    /// symbol that knows nothing of the body.
    pub(super) defined_functions: std::collections::HashSet<StringId>,
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
    /// How a call to a library builtin is evaluated: the optimization level
    /// and `-f[no-]math-errno`. See [`Self::set_library_call_policy`].
    pub(super) library_call_policy: super::library_builtin::LibraryCallPolicy,
    /// How many `switch` bodies enclose the statement being parsed, which is
    /// all a `fallthrough` attribute statement needs to know to be valid.
    pub(super) switch_depth: u32,
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
            pending_packed: false,
            pending_alignas_kw: None,
            pending_mode: None,
            pending_vector_size: None,
            pending_attr_align: None,
            pending_transparent_union: None,
            tentative_definitions: Vec::new(),
            pending_declarator_align: None,
            pending_symbol_attrs: Default::default(),
            pending_fn_attrs: Default::default(),
            pending_calling_conv: None,
            enclosing_function: EnclosingFunction::default(),
            declared_fn_attrs: BTreeMap::new(),
            pending_asm_label: None,
            declared_asm_labels: BTreeMap::new(),
            declared_extern_fns: std::collections::BTreeSet::new(),
            declared_non_inline_fns: std::collections::BTreeSet::new(),
            defined_functions: std::collections::HashSet::new(),
            pack_directives,
            pack_cursor: 0,
            vm_typedefs: HashMap::new(),
            library_call_policy: Default::default(),
            switch_depth: 0,
            pack_current: None,
            pack_stack: Vec::new(),
        }
    }

    /// Evaluate library builtins as the command line says: whether the
    /// optimizer is on, and whether `errno` is to be set. The default is
    /// [`LibraryCallPolicy::default`](super::library_builtin::LibraryCallPolicy).
    pub fn set_library_call_policy(&mut self, policy: super::library_builtin::LibraryCallPolicy) {
        self.library_call_policy = policy;
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

    /// The interned name of the current token when it is an identifier.
    ///
    /// Keywords, typedef names and ordinary names are all identifiers to the
    /// lexer, so this answers for every one of them; the caller decides which
    /// ids it is looking for.
    pub(super) fn current_ident(&self) -> Option<StringId> {
        self.get_ident_id(self.current())
    }

    /// Whether the current token is the identifier `kw`.
    pub(super) fn is_keyword(&self, kw: StringId) -> bool {
        self.current_ident() == Some(kw)
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

    /// Whether the token *after* the current one is the special `c`.
    ///
    /// Used where one token of lookahead settles a form: an identifier
    /// followed by `:` inside an initializer list is GNU's obsolete field
    /// designator and cannot be anything else.
    pub(super) fn next_token_is_special(&self, c: u8) -> bool {
        match self.tokens.get(self.pos + 1) {
            Some(t) => matches!(t.value, TokenValue::Special(v) if v == c as u32),
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
        if let Some(id) = self.current_ident() {
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
        // An identifier that cannot start a declaration is the declarator's
        // name, so the parenthesis groups it rather than opening parameters.
        if self.peek() == TokenType::Ident {
            // "Does a declaration start here?" has one answer, and this asked
            // a narrower question than `is_declaration_start` did: it tested
            // `TYPE_KEYWORD` alone, so a parameter list beginning with an
            // attribute -- `void bar (int (__attribute__((mode(SI))) int f));`
            // -- was read as a parenthesized declarator instead.
            return !self.is_declaration_start();
        }

        false
    }

    /// Intern a type, answering a tagged struct, union or enum with the tag's
    /// own `TypeId` -- or a qualified copy of it -- rather than a new one.
    ///
    /// Which tag a type is comes from the type itself
    /// ([`CompositeType::tag_type`]), not from looking its tag's name up
    /// here: a typedef name or `typeof` names the tag that was visible where
    /// *it* was declared, and an inner scope may since have given the name to
    /// a different type. `typedef struct S TS;` used as `TS x;` in a block
    /// that defines its own `struct S` declared `x` with the inner type.
    ///
    /// Only the type qualifiers carry over. A storage class -- `typedef`
    /// above all -- is the declaration's, not the type's: keeping it would
    /// make `typedef struct Foo Foo;` a different `TypeId` from the tag.
    ///
    /// [`CompositeType::tag_type`]: crate::types::CompositeType::tag_type
    pub(super) fn intern_type_with_tag(&mut self, typ: &Type) -> TypeId {
        let Some(tag_type) = typ.composite.as_ref().and_then(|c| c.tag_type) else {
            return self.types.intern(typ.clone());
        };
        let type_qualifier_mask = TypeModifiers::CONST
            | TypeModifiers::VOLATILE
            | TypeModifiers::RESTRICT
            | TypeModifiers::ATOMIC;
        let qualifiers = typ.modifiers & type_qualifier_mask;
        if qualifiers.is_empty() {
            return tag_type;
        }
        let mut qualified = self.types.get(tag_type).clone();
        qualified.modifiers |= qualifiers;
        self.types.intern(qualified)
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
}

impl Parser<'_> {
    pub(super) fn is_declaration_start(&self) -> bool {
        let Some(name_id) = self.current_ident() else {
            return false;
        };
        if crate::kw::has_tag(name_id, crate::kw::DECL_START) {
            return true;
        }
        // Also check for typedef names
        self.symbols.lookup_typedef(name_id).is_some()
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

    /// The composite type of this declaration and a visible prior one of the
    /// same object (C17 6.2.7p4).
    ///
    /// Two declarations of an identifier *with linkage* describe one object,
    /// so an inner `extern char i[];` under an outer `extern char i[10];` is
    /// the same complete array -- `sizeof i` is 10, and gcc answers so. c17
    /// took the inner declaration's own incomplete type and refused the
    /// `sizeof` outright.
    ///
    /// Only the array-extent half of the composite is formed here, which is
    /// the half that changes an answer: a prototype against an unprototyped
    /// declarator is already handled by `redeclaration_compatible`.
    pub(super) fn composite_with_prior_declaration(
        &self,
        name: StringId,
        typ: TypeId,
        modifiers: TypeModifiers,
    ) -> TypeId {
        // Only a declaration with linkage names an object another declaration
        // could also name. A plain block-scope object is a different object.
        if !modifiers.contains(TypeModifiers::EXTERN) {
            return typ;
        }
        if self.types.kind(typ) != TypeKind::Array || self.types.get(typ).array_size.is_some() {
            return typ;
        }
        let Some(prior_id) = self.symbols.lookup_id(name, Namespace::Ordinary) else {
            return typ;
        };
        let prior = self.symbols.get(prior_id).typ;
        if self.types.kind(prior) != TypeKind::Array || self.types.get(prior).array_size.is_none() {
            return typ;
        }
        // The element types still have to agree, or these are not two
        // declarations of one object and the conflict belongs to
        // `check_redeclaration`.
        match (self.types.base_type(typ), self.types.base_type(prior)) {
            (Some(a), Some(b)) if self.types.types_compatible(a, b) => prior,
            _ => typ,
        }
    }

    /// Build the symbol for a declared name, choosing its kind from its type.
    ///
    /// A declarator whose type is a function declares a *function*, whatever
    /// company it keeps -- `int f(int), g(int);` at file scope, or
    /// `void h(void) { int g(int); }` inside a block. `is_lvalue` asks the
    /// symbol's *kind* rather than its type, so a function bound as a variable
    /// would be assignable. `_Alignas` does not apply to a function, so an
    /// alignment is dropped here rather than recorded against one.
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

    /// C11 6.7.5p2: an alignment specifier shall not appear in the declaration
    /// of a typedef, a bit-field, a function, a parameter, or an object with
    /// `register` storage.
    ///
    /// Only the `_Alignas` keyword is constrained. The GNU `aligned` attribute
    /// shares `pending_alignas` but is legal in several of these places -- on a
    /// typedef it is the only way to align one at all -- which is why the
    /// provenance is tracked separately and tested here.
    pub(super) fn reject_alignas_in(&mut self, context: &str) {
        if let Some(pos) = self.pending_alignas_kw.take() {
            crate::diag::error_args(pos, "_Alignas cannot be applied to {0}", &[context]);
            self.pending_alignas = None;
        }
    }

    /// Fold a declaration's alignment into the type a typedef names.
    ///
    /// C11 6.7.5 and the GNU `aligned` attribute both raise the alignment of
    /// the type itself, so every object later declared through the typedef
    /// inherits it.
    ///
    /// `declared_align` must be what [`Self::validated_explicit_align`]
    /// returned: that is the only value that has seen *both* channels an
    /// alignment can arrive on -- `pending_alignas` for a specifier-position
    /// spelling, and `pending_declarator_align` for one written after the
    /// declarator. All three typedef binding sites used to read
    /// `pending_alignas` directly, which silently dropped every trailing
    /// `__attribute__((aligned(N)))`.
    pub(super) fn align_typedef_type(
        &mut self,
        typ: TypeId,
        declared_align: Option<u32>,
    ) -> TypeId {
        let Some(align) = declared_align else {
            return typ;
        };
        let mut aligned = self.types.get(typ).clone();
        aligned.explicit_align = Some(aligned.explicit_align.map_or(align, |e| e.max(align)));
        self.types.intern(aligned)
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
                // C11 6.7.5p5 forbids the *keyword* from reducing alignment.
                // The GNU `aligned` attribute shares this slot and is not
                // constrained -- gcc accepts `__attribute__((aligned(2))) int`
                // on a typedef, a variable and a struct alike, and rejects
                // `_Alignas(2) int` -- so the check has to ask which one was
                // written. `pending_alignas_kw` records that and was simply
                // never consulted, which made every reduced `aligned` an
                // error under a message naming a keyword the source did not
                // contain.
                if self.pending_alignas_kw.is_some() {
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
/// only what an identifier means.
impl crate::constexpr::ConstEnv for Parser<'_> {
    /// Every constant expression the parser folds is one C requires, so it
    /// always answers.
    fn deferred_constant_p(&self, _scope: ConstScope) -> Option<i128> {
        Some(0)
    }

    fn types(&self) -> &TypeTable {
        self.types
    }

    /// An enumeration constant is the only identifier with a value in the
    /// parser: a `const` object's value lives in an emitted global, which does
    /// not exist yet here, and no parse-time context would accept one.
    /// No object has a value in the parser, for the reason
    /// [`Self::ident_value`] gives.
    fn subobject_value(&self, _expr: &Expr, _scope: ConstScope) -> Option<i128> {
        None
    }

    fn float_subobject_value(
        &self,
        _expr: &Expr,
        _scope: ConstScope,
    ) -> Option<crate::float::FloatVal> {
        None
    }

    fn ident_value(&self, sym: crate::symbol::SymbolId, _scope: ConstScope) -> Option<i128> {
        let symbol = self.symbols.get(sym);
        symbol.is_enum_constant().then_some(symbol.enum_value)?
    }

    /// No floating identifier has a value in the parser, for the reason
    /// [`Self::ident_value`] gives: a `const double` lives in a global that
    /// does not exist yet.
    fn float_ident_value(
        &self,
        _sym: crate::symbol::SymbolId,
        _scope: ConstScope,
    ) -> Option<crate::float::FloatVal> {
        None
    }
}
