//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GCC __attribute__ parsing: the attribute argument grammar, the
// AttributeList a declaration collects, and the pending attribute slots
// they feed while a declarator is built
//

use super::ast::ExprKind;
use super::parser::Parser;
use crate::diag;
use crate::symbol::Namespace;
use crate::target::ByteOrder;
use crate::token::lexer::{payload_text, Position, TokenType};
use crate::types::{TypeId, TypeKind, TypeModifiers};
use gettextrs::gettext;
use std::fmt;

/// The `-Wno-<name>` group the unimplemented-attribute warnings belong to.
pub(crate) const ATTRIBUTE_WARNING: &str = "attributes";

// GCC __attribute__ Support

/// An argument to a GCC __attribute__
#[derive(Debug, Clone, PartialEq)]
pub enum AttributeArg {
    /// A name the attribute reads as a name rather than a value: `printf` in
    /// `format(printf, 1, 2)`, `QI` in `mode(QI)`, `fn` in `cleanup(fn)`.
    /// Only ever the first argument of an attribute that is not
    /// integer-valued; see [`AttrArgs`].
    Ident(String),
    /// String literal argument (e.g., `"default"`), adjacent literals joined
    String(String),
    /// An integer constant expression, already folded: `16` in `aligned(16)`,
    /// but equally `0x40`, `16UL`, an enumerator or `sizeof(long double)`
    Int(i128),
}

/// The largest alignment `aligned` may request: gcc's object-file maximum,
/// 2^28 bytes.
pub(super) const MAX_ATTR_ALIGN: i128 = 1 << 28;

/// How an attribute's arguments are read.
///
/// gcc parses every argument of an attribute it knows as an
/// assignment-expression, keeping a leading bare identifier as a name for the
/// attributes that want one; what the attribute *is* decides whether that
/// name is allowed and what a value that is not an integer constant means.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum AttrArgs {
    /// Every argument is an integer constant expression.
    Integers(IntArgRole),
    /// A leading name, then strings and integer constants: `format(printf,
    /// 1, 2)`, `mode(QI)`, `section("x")`.
    General,
    /// `cleanup(fn)`: exactly one argument, the name of a function.
    Cleanup,
    /// An attribute c17 does not recognise, whose arguments are not read.
    Unknown,
}

impl AttrArgs {
    fn of(name: &str, recognised: bool) -> Self {
        match name.trim_matches('_') {
            "aligned" => AttrArgs::Integers(IntArgRole::Alignment),
            "vector_size" => AttrArgs::Integers(IntArgRole::VectorSize),
            "constructor" | "destructor" => AttrArgs::Integers(IntArgRole::Priority),
            "alloc_size"
            | "alloc_align"
            | "assume_aligned"
            | "warn_if_not_aligned"
            | "nonnull"
            | "nonnull_if_nonzero"
            | "sentinel"
            | "regparm" => AttrArgs::Integers(IntArgRole::Unused),
            "cleanup" if recognised => AttrArgs::Cleanup,
            _ if recognised => AttrArgs::General,
            _ => AttrArgs::Unknown,
        }
    }
}

/// What an integer-valued attribute's argument means to c17, which decides
/// both the values it accepts and how loudly a bad one is refused.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum IntArgRole {
    /// `aligned(N)`: a power of two, in bytes.
    Alignment,
    /// `vector_size(N)`: a positive width, in bytes.
    VectorSize,
    /// `constructor(P)` / `destructor(P)`: an init priority, 0 to 65535.
    Priority,
    /// `nonnull(1, 2)`, `alloc_size(1)`, `sentinel(0)`, ...: accepted and not
    /// acted on, so a bad argument drops the attribute with a warning, as gcc
    /// does.
    Unused,
}

/// What is wrong with an integer-valued attribute's arguments.
#[derive(Clone, Copy, Debug)]
enum IntArgFault {
    NotConstant,
    Count,
    Zero,
    OutOfRange(i128),
    TooLarge(i128),
}

/// An attribute whose arguments have already been diagnosed, and which is
/// dropped rather than applied with a value the source did not give it.
#[derive(Clone, Copy, Debug)]
struct Diagnosed;

impl IntArgRole {
    /// Report `fault` in `name`'s arguments, in gcc's words.
    fn report(self, name: &str, pos: Position, fault: IntArgFault) {
        let name = name.trim_matches('_');
        match (self, fault) {
            (_, IntArgFault::Count) => diag::error_args(
                pos,
                "wrong number of arguments specified for '{0}' attribute",
                &[name],
            ),
            (IntArgRole::Alignment, IntArgFault::NotConstant) => diag::error(
                pos,
                &gettext("requested alignment is not an integer constant"),
            ),
            // gcc warns and ignores `aligned(0)` rather than refusing it.
            (IntArgRole::Alignment, IntArgFault::Zero) => {
                if diag::warning_group_enabled(ATTRIBUTE_WARNING) {
                    diag::warning(
                        pos,
                        &gettext("requested alignment '0' is not a positive power of 2"),
                    );
                }
            }
            (IntArgRole::Alignment, IntArgFault::OutOfRange(n)) => diag::error_args(
                pos,
                "requested alignment '{0}' is not a positive power of 2",
                &[&n.to_string()],
            ),
            (IntArgRole::Alignment, IntArgFault::TooLarge(n)) => diag::error_args(
                pos,
                "requested alignment '{0}' exceeds object file maximum {1}",
                &[&n.to_string(), &MAX_ATTR_ALIGN.to_string()],
            ),
            (IntArgRole::VectorSize, IntArgFault::NotConstant) => diag::error(
                pos,
                &gettext("'vector_size' attribute argument is not an integer constant"),
            ),
            (IntArgRole::VectorSize, IntArgFault::Zero) => {
                diag::error(pos, &gettext("zero vector size"))
            }
            (IntArgRole::VectorSize, IntArgFault::OutOfRange(n) | IntArgFault::TooLarge(n)) => {
                diag::error_args(
                    pos,
                    "'vector_size' attribute argument value '{0}' is negative",
                    &[&n.to_string()],
                )
            }
            (IntArgRole::Priority, _) => diag::error_args(
                pos,
                "{0} priorities must be integers from 0 to 65535 inclusive",
                &[name],
            ),
            (IntArgRole::Unused, _) => {
                if diag::warning_group_enabled(ATTRIBUTE_WARNING) {
                    diag::warning_args(
                        pos,
                        "'{0}' attribute argument is not an integer constant",
                        &[name],
                    );
                }
            }
        }
    }
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
    pub fn with_args(name: impl Into<String>, args: Vec<AttributeArg>) -> Self {
        Self {
            name: name.into(),
            args,
        }
    }

    /// Whether this is attribute `name`, in its plain or `__name__` spelling.
    pub(super) fn is_named(&self, name: &str) -> bool {
        self.name == name
            || self
                .name
                .strip_prefix("__")
                .and_then(|n| n.strip_suffix("__"))
                == Some(name)
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
        self.has_attr("noreturn")
    }

    /// The calling convention this list names: `ms_abi` is Win64, and
    /// `sysv_abi` is the x86-64 default. `None` for neither -- or for both,
    /// which [`Parser::merge_calling_conv`] reports.
    ///
    /// Read only on x86-64: anywhere else the names are not attributes, and
    /// `parse_single_attribute` has already warned that they are ignored.
    pub fn calling_conv(&self) -> Option<crate::abi::CallingConv> {
        match (self.has_attr("ms_abi"), self.has_attr("sysv_abi")) {
            (true, false) => Some(crate::abi::CallingConv::Win64),
            (false, true) => Some(crate::abi::CallingConv::C),
            _ => None,
        }
    }

    /// Whether `__attribute__((noinline))` is present.
    pub fn has_noinline(&self) -> bool {
        self.has_attr("noinline")
    }

    /// The memory effect `__attribute__((pure))` or `((const))` promises.
    ///
    /// `const` is a keyword, so only the `__const__` spelling can appear
    /// bare; gcc accepts `__attribute__((const))` because an attribute name
    /// is matched as a token rather than as an identifier, and `has_attr`
    /// compares the text either way.
    pub fn mem_effect(&self) -> crate::parse::ast::MemEffect {
        use crate::parse::ast::MemEffect;
        if self.has_attr("const") {
            MemEffect::Const
        } else if self.has_attr("pure") {
            MemEffect::Pure
        } else {
            MemEffect::Unknown
        }
    }

    /// Whether `__attribute__((always_inline))` is present.
    pub fn has_always_inline(&self) -> bool {
        self.has_attr("always_inline")
    }

    /// `__attribute__((transparent_union))`, in either spelling.
    pub(super) fn has_transparent_union(&self) -> bool {
        self.has_attr("transparent_union")
    }

    /// `__attribute__((packed))`, in either spelling.
    pub(super) fn has_packed(&self) -> bool {
        self.has_attr("packed")
    }

    /// The byte order `__attribute__((scalar_storage_order("...")))` names,
    /// or `None` when the attribute is absent.
    ///
    /// An argument that names no order is gcc's error, reported here, and
    /// the attribute is then ignored.
    pub(super) fn storage_order(&self, pos: Position) -> Option<ByteOrder> {
        let attr = self.find("scalar_storage_order")?;
        match attr.args.as_slice() {
            [AttributeArg::String(s)] if s == "big-endian" => Some(ByteOrder::BigEndian),
            [AttributeArg::String(s)] if s == "little-endian" => Some(ByteOrder::LittleEndian),
            [_] => {
                diag::error(
                    pos,
                    "attribute 'scalar_storage_order' argument must be one of 'big-endian' or 'little-endian'",
                );
                None
            }
            _ => {
                diag::error_args(
                    pos,
                    "wrong number of arguments specified for '{0}' attribute",
                    &["scalar_storage_order"],
                );
                None
            }
        }
    }

    /// Whether an attribute is present, in either spelling.
    fn has_attr(&self, name: &str) -> bool {
        self.find(name).is_some()
    }

    /// The first attribute `name`, in either spelling.
    pub(super) fn find(&self, name: &str) -> Option<&Attribute> {
        self.attrs.iter().find(|a| a.is_named(name))
    }

    /// Look up an attribute in both its plain and `__underscored__` spelling,
    /// returning its optional integer argument. The result distinguishes
    /// "absent" (`None`) from "present without a priority" (`Some(None)`).
    fn init_priority(&self, name: &str) -> Option<Option<u16>> {
        let attr = self.find(name)?;
        match attr.args.first() {
            // In range: `check_integer_args` drops any other priority.
            Some(AttributeArg::Int(n)) => Some(u16::try_from(*n).ok()),
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

    /// Collect the attributes that affect how a function is emitted, for
    /// `target`.
    pub fn function_attrs(
        &self,
        target: &crate::target::Target,
    ) -> crate::parse::ast::FunctionAttrs {
        crate::parse::ast::FunctionAttrs {
            target: self.target_request(target),
            clones: self.target_clones(target),
            symbol: self.symbol_attrs(),
            noinline: self.has_noinline(),
            always_inline: self.has_always_inline(),
            constructor: self.constructor_priority(),
            destructor: self.destructor_priority(),
            gnu_inline: self.has_attr("gnu_inline"),
            artificial: self.has_attr("artificial"),
            effect: self.mem_effect(),
            align: self.get_alignment().filter(|n| n.is_power_of_two()),
            noreturn: self.has_noreturn(),
        }
    }

    /// The `weak`, `used`, `section(...)`, `visibility(...)`, `alias(...)` and
    /// `ifunc(...)` requests in this list.
    pub fn symbol_attrs(&self) -> crate::parse::ast::SymbolAttrs {
        use crate::parse::ast::{AliasAttr, AliasForm};
        let mut out = crate::parse::ast::SymbolAttrs::default();
        for attr in &self.attrs {
            let text = |a: &Attribute| match a.args.first() {
                Some(AttributeArg::String(s)) => Some(s.clone()),
                Some(AttributeArg::Ident(s)) => Some(s.clone()),
                _ => None,
            };
            let alias = |a: &Attribute, form| text(a).map(|target| AliasAttr { target, form });
            match attr.name.trim_matches('_') {
                "weak" => out.weak = true,
                "used" => out.used = true,
                "section" => out.section = text(attr),
                "visibility" => out.visibility = text(attr),
                "alias" => out.alias = alias(attr, AliasForm::Alias),
                "ifunc" => out.alias = alias(attr, AliasForm::Ifunc),
                _ => {}
            }
        }
        out
    }

    /// The alignment `aligned` asks for: a power of two, since
    /// `check_integer_args` drops any other with a diagnostic.
    pub fn get_alignment(&self) -> Option<u32> {
        for attr in &self.attrs {
            if attr.is_named("aligned") {
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

impl Parser<'_> {
    /// Check if current token is __attribute__ or __attribute
    pub(super) fn is_attribute_keyword(&self) -> bool {
        self.current_ident()
            .is_some_and(|id| crate::kw::has_tag(id, crate::kw::ATTR_KW))
    }

    /// Skip to the `,` or `)` that ends the attribute argument at hand,
    /// stepping over nested parentheses. Leaves that token current.
    fn skip_attribute_arg(&mut self) {
        let mut depth = 0usize;
        while !self.is_eof() {
            if self.is_special(b'(') {
                depth += 1;
            } else if self.is_special(b')') {
                if depth == 0 {
                    return;
                }
                depth -= 1;
            } else if self.is_special(b',') && depth == 0 {
                return;
            }
            self.advance();
        }
    }

    /// Whether the current token is a bare identifier standing alone as an
    /// argument: followed by the `,` or `)` that ends it.
    fn at_bare_attribute_name(&self) -> bool {
        self.peek() == TokenType::Ident
            && (self.next_token_is_special(b',') || self.next_token_is_special(b')'))
    }

    /// Read one argument of an attribute c17 recognises.
    ///
    /// gcc's rule: the first argument may be a bare name, and anything else is
    /// an assignment-expression. The expression is folded here, by the same
    /// evaluator array bounds and `case` labels use, so `aligned(0x40)`,
    /// `aligned(A)` for an enumerator and `vector_size(2 * sizeof(int))` all
    /// mean what they say. An integer-valued attribute takes no names at all:
    /// `aligned(A)` is the enumerator's value, never the word `A`.
    ///
    /// `Ok(None)` is an argument of a general attribute that is neither a
    /// name, a string nor a constant -- the declaration `copy(&f)` points at
    /// -- which no c17 consumer reads.
    fn parse_attribute_arg(
        &mut self,
        name: &str,
        grammar: AttrArgs,
        first: bool,
    ) -> Result<Option<AttributeArg>, Diagnosed> {
        if grammar == AttrArgs::Cleanup {
            return self.parse_cleanup_arg(first);
        }
        if grammar == AttrArgs::General && first && self.at_bare_attribute_name() {
            let ident = self.get_ident_name(self.current()).ok_or(Diagnosed)?;
            self.advance();
            return Ok(Some(AttributeArg::Ident(ident)));
        }
        let pos = self.current_pos();
        // The expression parser reports an undeclared name and stands `0` in
        // for it; judging the stand-in as well would add a second, wrong
        // complaint -- "requested alignment '0'" for `aligned(foo)`.
        let undeclared = self.at_bare_attribute_name()
            && self
                .current_ident()
                .is_some_and(|id| self.symbols.lookup_id(id, Namespace::Ordinary).is_none());
        let expr = match self.parse_assignment_expr() {
            Ok(expr) => expr,
            Err(e) => {
                diag::error(e.pos, &e.message);
                self.skip_attribute_arg();
                return Err(Diagnosed);
            }
        };
        if undeclared && matches!(expr.kind, ExprKind::IntLit(0)) {
            return Err(Diagnosed);
        }
        if let (AttrArgs::General, ExprKind::StringLit(bytes)) = (grammar, &expr.kind) {
            return Ok(Some(AttributeArg::String(payload_text(bytes))));
        }
        match (self.eval_const_expr(&expr), grammar) {
            (Some(n), _) => Ok(Some(AttributeArg::Int(n))),
            (None, AttrArgs::Integers(role)) => {
                role.report(name, pos, IntArgFault::NotConstant);
                Err(Diagnosed)
            }
            (None, _) => Ok(None),
        }
    }

    /// Read the parenthesised arguments of `name`, the `(` already consumed,
    /// through the closing `)`.
    ///
    /// `Err` when an argument was diagnosed: the attribute is then dropped
    /// rather than applied with a value the source did not give it.
    fn parse_attribute_args(
        &mut self,
        name: &str,
        grammar: AttrArgs,
    ) -> Result<Vec<AttributeArg>, Diagnosed> {
        let pos = self.current_pos();
        let mut args = Vec::new();
        let mut result = Ok(());
        let mut first = true;
        let mut count = 0;
        while !self.is_special(b')') && !self.is_eof() {
            count += 1;
            if grammar == AttrArgs::Unknown {
                // An attribute c17 does not know has a grammar c17 does not
                // know either -- clang's `availability(macos, introduced=10.4)`
                // is no expression list -- and is ignored, so its tokens are
                // stepped over rather than parsed.
                self.skip_attribute_arg();
            } else {
                match self.parse_attribute_arg(name, grammar, first) {
                    Ok(Some(arg)) => args.push(arg),
                    Ok(None) => {}
                    Err(d) => result = Err(d),
                }
            }
            first = false;
            if self.is_special(b',') {
                self.advance();
            } else if !self.is_special(b')') && !self.is_eof() {
                diag::error(
                    self.current_pos(),
                    "expected ',' or ')' in attribute arguments",
                );
                self.skip_attribute_arg();
                result = Err(Diagnosed);
            }
        }
        if self.is_special(b')') {
            self.advance();
        }
        if grammar == AttrArgs::Cleanup && count != 1 && result.is_ok() {
            Self::report_cleanup_arg_count(pos);
            result = Err(Diagnosed);
        }
        result.map(|()| args)
    }

    /// One argument of `cleanup`. Only the first is read, and it has to be a
    /// bare name; a second is the count error [`Self::parse_attribute_args`]
    /// reports once the list is read.
    fn parse_cleanup_arg(&mut self, first: bool) -> Result<Option<AttributeArg>, Diagnosed> {
        if first && self.at_bare_attribute_name() {
            let ident = self.get_ident_name(self.current()).ok_or(Diagnosed)?;
            self.advance();
            return Ok(Some(AttributeArg::Ident(ident)));
        }
        let pos = self.current_pos();
        self.skip_attribute_arg();
        if first {
            diag::error(pos, &gettext("cleanup argument not an identifier"));
            return Err(Diagnosed);
        }
        Ok(None)
    }

    /// `cleanup` takes exactly one argument, in gcc's words.
    fn report_cleanup_arg_count(pos: Position) {
        diag::error_args(
            pos,
            "wrong number of arguments specified for '{0}' attribute",
            &["cleanup"],
        );
    }

    /// Check that the name `cleanup(fn)` gives is a function: the name of a
    /// variable, even a pointer to one, is an error, as in gcc. `Err` drops
    /// the attribute.
    fn check_cleanup_function(
        &self,
        args: &[AttributeArg],
        pos: Position,
    ) -> Result<(), Diagnosed> {
        let [AttributeArg::Ident(name)] = args else {
            Self::report_cleanup_arg_count(pos);
            return Err(Diagnosed);
        };
        if self.cleanup_function(name).is_some() {
            return Ok(());
        }
        diag::error(pos, &gettext("cleanup argument not a function"));
        Err(Diagnosed)
    }

    /// The function a `cleanup(name)` names, if `name` is one.
    fn cleanup_function(&self, name: &str) -> Option<crate::symbol::SymbolId> {
        let id = self.idents.lookup(name)?;
        let symbol = self.symbols.lookup_id(id, Namespace::Ordinary)?;
        (self.symbols.get(symbol).kind == crate::symbol::SymbolKind::Function).then_some(symbol)
    }

    /// Check the arguments of an integer-valued attribute against what its
    /// role allows, diagnosing as gcc does. `Err` drops the attribute.
    fn check_integer_args(
        &self,
        name: &str,
        role: IntArgRole,
        args: &[AttributeArg],
        pos: Position,
    ) -> Result<(), Diagnosed> {
        let value = match args {
            [] => None,
            [AttributeArg::Int(n)] => Some(*n),
            _ if role == IntArgRole::Unused => return Ok(()),
            _ => {
                role.report(name, pos, IntArgFault::Count);
                return Err(Diagnosed);
            }
        };
        let fault = match (role, value) {
            // Bare `aligned` asks for the largest useful alignment.
            (IntArgRole::Alignment, None) => None,
            (IntArgRole::Alignment, Some(0)) => Some(IntArgFault::Zero),
            (IntArgRole::Alignment, Some(n)) if n < 0 || !(n as u128).is_power_of_two() => {
                Some(IntArgFault::OutOfRange(n))
            }
            (IntArgRole::Alignment, Some(n)) if n > MAX_ATTR_ALIGN => {
                Some(IntArgFault::TooLarge(n))
            }
            (IntArgRole::VectorSize, None) => Some(IntArgFault::Count),
            (IntArgRole::VectorSize, Some(0)) => Some(IntArgFault::Zero),
            (IntArgRole::VectorSize, Some(n)) if n < 0 => Some(IntArgFault::OutOfRange(n)),
            (IntArgRole::Priority, Some(n)) if !(0..=i128::from(u16::MAX)).contains(&n) => {
                Some(IntArgFault::OutOfRange(n))
            }
            _ => None,
        };
        match fault {
            Some(fault) => {
                role.report(name, pos, fault);
                Err(Diagnosed)
            }
            None => Ok(()),
        }
    }

    /// Parse a single attribute: name or name(args)
    ///
    /// `None` when there is no attribute here, or when its arguments were
    /// diagnosed and it is dropped.
    fn parse_single_attribute(&mut self) -> Option<Attribute> {
        if self.peek() != TokenType::Ident {
            return None;
        }

        let pos = self.current_pos();
        let id = self.current_ident();
        let name = self.get_ident_name(self.current())?;
        self.advance();

        // Every attribute in the program passes through here exactly once, so
        // this is where an unrecognised one gets said out loud: dropping one
        // in silence is survivable for an attribute that only hints, and is
        // not for one that changes what the type *is*.
        let arch = self.types.target().arch;
        let recognised = id.is_some_and(|id| crate::kw::attribute_supported(id, arch));
        if !recognised && diag::warning_group_enabled(ATTRIBUTE_WARNING) {
            diag::warning_args(pos, "'{0}' attribute directive ignored", &[&name]);
        }

        let grammar = AttrArgs::of(&name, recognised);
        let args = if self.is_special(b'(') {
            self.advance();
            self.parse_attribute_args(&name, grammar).ok()?
        } else {
            Vec::new()
        };
        match grammar {
            AttrArgs::Integers(role) => self.check_integer_args(&name, role, &args, pos).ok()?,
            AttrArgs::Cleanup => self.check_cleanup_function(&args, pos).ok()?,
            _ => {}
        }
        let target_attr = match name.trim_matches('_') {
            "target" => Some(false),
            "target_clones" => Some(true),
            _ => None,
        };
        if let (true, Some(clones)) = (recognised, target_attr) {
            let target = self.types.target();
            if !super::target_attr::check_target_args(clones, &args, pos, &target) {
                return None;
            }
        }

        // A mode or a vector width replaces the declared type, so each is
        // held here and applied with the other type attributes once the type
        // is final (`apply_pending_type_attrs`). Neither can be ignored: glibc
        // declares `register_t` with `__mode__(__word__)`, and a vector left
        // scalar would compute on one element. Only a recognised spelling is
        // applied: one warned about as ignored is not.
        match (recognised, name.trim_matches('_'), args.first()) {
            (true, "mode", Some(AttributeArg::Ident(m))) => {
                self.pending_mode = Some((m.trim_matches('_').to_string(), pos));
            }
            (true, "vector_size", Some(AttributeArg::Int(n))) => {
                self.pending_vector_size = Some((u64::try_from(*n).unwrap_or(u64::MAX), pos));
            }
            (true, "aligned", Some(AttributeArg::Int(n))) => {
                self.pending_attr_align = Some(*n as u32);
            }
            _ => {}
        }
        Some(Attribute::with_args(name, args))
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
        self.current_ident()
            .is_some_and(super::is_nullability_qualifier)
    }
    /// The memory effect pending for the declarator being built, consumed.
    ///
    /// Consumed, exactly as `pending_symbol_attrs` is taken, and for the same
    /// reason: `extern int p(void) __attribute__((pure)), q(void);` writes
    /// the attribute on `p`, and leaving it pending gave it to `q` as well.
    /// A callee wrongly believed to write nothing is a miscompile at the
    /// *call site*, so this fails safe -- a later declarator gets `Unknown`
    /// even where gcc would spread a declaration-level attribute across all
    /// of them, which costs precision and nothing else.
    /// The attributes a declaration's specifiers carry, to hand to each of its
    /// declarators in turn. See [`SpecifierAttrs`].
    pub(super) fn specifier_attrs(&self) -> SpecifierAttrs {
        SpecifierAttrs {
            fn_attrs: self.pending_fn_attrs.clone(),
            symbol_attrs: self.pending_symbol_attrs.clone(),
            calling_conv: self.pending_calling_conv,
        }
    }

    /// Start the next declarator of a list: it has the specifiers' attributes
    /// and none of the previous declarator's.
    pub(super) fn begin_declarator(&mut self, spec: &SpecifierAttrs) {
        self.pending_fn_attrs = spec.fn_attrs.clone();
        self.pending_symbol_attrs = spec.symbol_attrs.clone();
        self.pending_calling_conv = spec.calling_conv;
        self.pending_declarator_align = None;
    }

    pub(super) fn take_pending_fn_effect(&mut self) -> crate::parse::ast::MemEffect {
        std::mem::replace(
            &mut self.pending_fn_attrs.effect,
            crate::parse::ast::MemEffect::Unknown,
        )
    }

    /// Accumulate the symbol-emission attributes from one attribute list.
    ///
    /// `cleanup(fn)` is resolved to its function here, as the declaration is
    /// read: gcc looks the name up where the attribute is written, so
    /// `int c __attribute__((cleanup(c)))` names the function, not the
    /// variable being declared.
    pub(super) fn merge_symbol_attrs(&mut self, attrs: &AttributeList) {
        let mut symbol_attrs = attrs.symbol_attrs();
        symbol_attrs.cleanup = attrs.find("cleanup").and_then(|a| match a.args.first() {
            Some(AttributeArg::Ident(name)) => self.cleanup_function(name),
            _ => None,
        });
        self.pending_symbol_attrs.merge(&symbol_attrs);
    }

    /// `cleanup(fn)` on something that has no scope to leave -- a member, a
    /// parameter -- is dropped with gcc's warning.
    pub(super) fn drop_pending_cleanup(&mut self, pos: Position) {
        if self.pending_symbol_attrs.cleanup.take().is_some() {
            Self::warn_cleanup_ignored(pos);
        }
    }

    /// `ifunc("resolver")` on anything but a function is dropped with gcc's
    /// warning: only a function can be indirect, and the object is declared
    /// as written.
    pub(super) fn drop_ifunc(attrs: &mut crate::parse::ast::SymbolAttrs, pos: Position) {
        let form = attrs.alias.as_ref().map(|a| a.form);
        if form != Some(crate::parse::ast::AliasForm::Ifunc) {
            return;
        }
        attrs.alias = None;
        if diag::warning_group_enabled(ATTRIBUTE_WARNING) {
            diag::warning_args(pos, "'{0}' attribute ignored", &["ifunc"]);
        }
    }

    /// gcc's warning for a `cleanup(fn)` it ignores.
    pub(super) fn warn_cleanup_ignored(pos: Position) {
        if diag::warning_group_enabled(ATTRIBUTE_WARNING) {
            diag::warning_args(pos, "'{0}' attribute ignored", &["cleanup"]);
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
        let typ = self.apply_pending_calling_conv(typ);
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

    /// Hold the calling convention `attrs` names for the declarator being
    /// parsed, refusing a second one that disagrees with it -- in the same
    /// list or another -- as gcc does.
    fn merge_calling_conv(&mut self, attrs: &AttributeList, pos: Position) {
        if self.types.target().arch != crate::target::Arch::X86_64 {
            return;
        }
        let both = attrs.find("ms_abi").is_some() && attrs.find("sysv_abi").is_some();
        let conflict = match (self.pending_calling_conv, attrs.calling_conv()) {
            (Some((held, _)), Some(new)) => held != new,
            _ => false,
        };
        if both || conflict {
            diag::error(pos, "'ms_abi' and 'sysv_abi' attributes are not compatible");
            return;
        }
        if let Some(conv) = attrs.calling_conv() {
            self.pending_calling_conv = Some((conv, pos));
        }
    }

    /// Give the declared type the calling convention its declaration names.
    ///
    /// gcc's placement rule for an attribute that needs a function type: a
    /// function takes it, a pointer to a function hands it to the function it
    /// points at -- `__attribute__((ms_abi)) long (*fp)(long)` is a pointer
    /// to an `ms_abi` function -- and anything else, including a pointer to
    /// such a pointer, warns and ignores it. `sysv_abi` names the default,
    /// so it changes nothing but is placed by the same rule.
    fn apply_pending_calling_conv(&mut self, typ: TypeId) -> TypeId {
        let Some((conv, pos)) = self.pending_calling_conv.take() else {
            return typ;
        };
        match self.types.kind(typ) {
            TypeKind::Function => self.with_calling_conv(typ, conv),
            TypeKind::Pointer
                if self
                    .types
                    .base_type(typ)
                    .is_some_and(|b| self.types.kind(b) == TypeKind::Function) =>
            {
                let func = self.types.base_type(typ).expect("checked above");
                let func = self.with_calling_conv(func, conv);
                let mut ptr = self.types.get(typ).clone();
                ptr.base = Some(func);
                self.types.intern(ptr)
            }
            _ => {
                if diag::warning_group_enabled(ATTRIBUTE_WARNING) {
                    let name = match conv {
                        crate::abi::CallingConv::Win64 => "ms_abi",
                        crate::abi::CallingConv::C => "sysv_abi",
                    };
                    diag::warning_args(
                        pos,
                        "'{0}' attribute only applies to function types",
                        &[name],
                    );
                }
                typ
            }
        }
    }

    /// The function type `func` with calling convention `conv`.
    fn with_calling_conv(&mut self, func: TypeId, conv: crate::abi::CallingConv) -> TypeId {
        if self.types.get(func).conv == conv {
            return func;
        }
        let mut changed = self.types.get(func).clone();
        changed.conv = conv;
        self.types.intern(changed)
    }

    /// Apply `__attribute__((vector_size(N)))` to a declared type.
    ///
    /// The type becomes a vector of `N / sizeof(element)` elements, laid out
    /// as the array of them gcc lays it out as (see `TypeTable::vector_of`).
    pub(super) fn apply_pending_vector_size(&mut self, typ: TypeId) -> TypeId {
        let Some((bytes, pos)) = self.pending_vector_size.take() else {
            return typ;
        };
        let elem_size = self.types.size_bytes(typ);
        // A lane is a real number: no complex type, and no `_Bool`.
        let t = &*self.types;
        let lane = (t.is_integer(typ) || t.is_float(typ))
            && !t.is_complex(typ)
            && t.kind(typ) != TypeKind::Bool;
        if elem_size == 0 || !lane {
            diag::error(
                pos,
                &gettext("invalid vector type for attribute 'vector_size'"),
            );
            return typ;
        }
        // The same ceiling `derive_array_type` applies, and for the same
        // reason: this interns an array directly, so nothing else would catch
        // an absurd width. Without it `vector_size(4294967296)` quietly
        // produced a four-gigabyte type, and a width near `u64::MAX` made
        // `next_power_of_two` overflow -- a panic in a debug build.
        let max = self.types.max_object_bytes();
        if bytes > max as u64 {
            diag::error_args(
                pos,
                "'vector_size' attribute argument value '{0}' exceeds {1}, the maximum object size",
                &[&bytes.to_string(), &max.to_string()],
            );
            return typ;
        }
        if bytes % elem_size as u64 != 0 {
            diag::error(
                pos,
                &gettext("vector size not an integral multiple of component size"),
            );
            return typ;
        }
        // gcc refuses any other lane count at the declaration, so every
        // vector a convention has to pass is one of its widths.
        let count = bytes / elem_size as u64;
        if !count.is_power_of_two() {
            diag::error_args(
                pos,
                "number of vector components {0} not a power of two",
                &[&count.to_string()],
            );
            return typ;
        }
        // An `aligned(n)` written alongside has to be applied here rather
        // than left to the later attribute pass, since an explicit alignment
        // may not reduce one already recorded.
        let written = self.pending_attr_align.take();
        self.types.vector_of(typ, count as usize, written)
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
    pub(super) fn apply_pending_mode(&mut self, typ: TypeId) -> TypeId {
        let Some((mode, pos)) = self.pending_mode.take() else {
            return typ;
        };
        let unsigned = self.types.is_unsigned(typ);
        let t = &self.types;
        let mapped = match mode.as_str() {
            // Integer modes, named for their width in bytes.
            // `byte` is a byte, and `word`, `pointer` and `unwind_word` are
            // eight bytes on every target c17 has.
            "QI" | "byte" => Some(if unsigned { t.uchar_id } else { t.schar_id }),
            "HI" => Some(if unsigned { t.ushort_id } else { t.short_id }),
            "SI" => Some(if unsigned { t.uint_id } else { t.int_id }),
            "DI" | "word" | "pointer" | "unwind_word" => {
                Some(if unsigned { t.ulong_id } else { t.long_id })
            }
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
            self.raise_pending_alignas(align);
        }
    }

    /// Raise the declaration's pending alignment to `align`, ignoring a value
    /// that is not a power of two.
    pub(super) fn raise_pending_alignas(&mut self, align: u32) {
        if align > 0 && align.is_power_of_two() {
            self.pending_alignas = Some(self.pending_alignas.map_or(align, |e| e.max(align)));
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
                let pos = self.current_pos();
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
                // A struct or union's order is written in its specifier; on
                // a declaration or declarator gcc ignores it, and says so.
                if attrs.find("scalar_storage_order").is_some()
                    && diag::warning_group_enabled(ATTRIBUTE_WARNING)
                {
                    diag::warning_args(pos, "'{0}' attribute ignored", &["scalar_storage_order"]);
                }
                self.pending_packed |= attrs.has_packed();
                self.merge_symbol_attrs(&attrs);
                self.merge_calling_conv(&attrs, pos);
                let fn_attrs = attrs.function_attrs(&self.types.target());
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

/// The attributes written among a declaration's specifiers.
///
/// gcc applies those to every declarator in the list -- in
/// `__attribute__((aligned(32))) void f(void), g(void);` both functions are
/// aligned, and in `__attribute__((weak)) int a, b;` both objects are weak --
/// while an attribute written after a declarator belongs to that declarator
/// alone. The parser collects both kinds into the same pending slots, so the
/// specifiers' share is snapshotted before the first declarator and restored
/// for each one after it ([`Parser::begin_declarator`]). Without it a later
/// declarator either lost the specifiers' attributes or inherited the
/// previous declarator's.
#[derive(Clone, Default)]
pub(super) struct SpecifierAttrs {
    fn_attrs: crate::parse::ast::FunctionAttrs,
    symbol_attrs: crate::parse::ast::SymbolAttrs,
    calling_conv: Option<(crate::abi::CallingConv, Position)>,
}

/// Every slot an attribute list writes for the declaration being parsed.
///
/// A type-name -- in a cast, `sizeof`, `_Alignof`, `typeof`, a compound
/// literal -- can carry attributes of its own, and can sit in the middle of a
/// declaration: `int x = sizeof(long __attribute__((aligned(16))));`. Its
/// attributes go through the same parser and the same slots as a
/// declaration's, so the type-name takes the enclosing declaration's state
/// aside while it is parsed ([`Parser::take_pending_decl_attrs`]) and hands it
/// back afterwards. Otherwise the `aligned(16)` above would align `x`.
#[derive(Default)]
pub(super) struct PendingDeclAttrs {
    alignas: Option<u32>,
    alignas_kw: Option<Position>,
    declarator_align: Option<u32>,
    attr_align: Option<u32>,
    mode: Option<(String, Position)>,
    vector_size: Option<(u64, Position)>,
    transparent_union: Option<Position>,
    packed: bool,
    symbol_attrs: crate::parse::ast::SymbolAttrs,
    fn_attrs: crate::parse::ast::FunctionAttrs,
    calling_conv: Option<(crate::abi::CallingConv, Position)>,
    asm_label: Option<String>,
}

impl Parser<'_> {
    /// Take every pending declaration attribute, leaving the slots empty.
    /// See [`PendingDeclAttrs`].
    pub(super) fn take_pending_decl_attrs(&mut self) -> PendingDeclAttrs {
        use std::mem::take;
        PendingDeclAttrs {
            alignas: take(&mut self.pending_alignas),
            alignas_kw: take(&mut self.pending_alignas_kw),
            declarator_align: take(&mut self.pending_declarator_align),
            attr_align: take(&mut self.pending_attr_align),
            mode: take(&mut self.pending_mode),
            vector_size: take(&mut self.pending_vector_size),
            transparent_union: take(&mut self.pending_transparent_union),
            packed: take(&mut self.pending_packed),
            symbol_attrs: take(&mut self.pending_symbol_attrs),
            fn_attrs: take(&mut self.pending_fn_attrs),
            calling_conv: take(&mut self.pending_calling_conv),
            asm_label: take(&mut self.pending_asm_label),
        }
    }

    /// Put back what [`Self::take_pending_decl_attrs`] took, discarding
    /// whatever was collected in between.
    pub(super) fn restore_pending_decl_attrs(&mut self, saved: PendingDeclAttrs) {
        self.pending_alignas = saved.alignas;
        self.pending_alignas_kw = saved.alignas_kw;
        self.pending_declarator_align = saved.declarator_align;
        self.pending_attr_align = saved.attr_align;
        self.pending_mode = saved.mode;
        self.pending_vector_size = saved.vector_size;
        self.pending_transparent_union = saved.transparent_union;
        self.pending_packed = saved.packed;
        self.pending_symbol_attrs = saved.symbol_attrs;
        self.pending_fn_attrs = saved.fn_attrs;
        self.pending_calling_conv = saved.calling_conv;
        self.pending_asm_label = saved.asm_label;
    }
}
