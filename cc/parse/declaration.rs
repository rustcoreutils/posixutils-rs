//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Declaration parsing: declaration specifiers, type specifiers, initializer
// checking and redeclaration compatibility
//

use super::ast::{Declaration, Designator, Expr, ExprKind, InitDeclarator, InitElement};
use super::parser::{DeclaratorName, ParseError, ParseResult, Parser};
use crate::diag;
use crate::strings::StringId;
use crate::symbol::{Namespace, Symbol, SymbolId, SymbolKind};
use crate::token::lexer::{Position, TokenType};
use crate::types::{Type, TypeId, TypeKind, TypeModifiers, TypeTable};
use gettextrs::gettext;

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

impl Parser<'_> {
    /// Parse a declaration and bind variables to symbol table
    ///
    /// Binds each declared variable to the symbol table immediately during
    /// parsing, so the symbol is available for subsequent references.
    ///
    /// If `forbid_storage_class` is true, emits an error if the declaration
    /// contains storage class specifiers (static, extern). This is used for
    /// for-loop init declarations per C99 6.8.5.3.
    pub(super) fn parse_declaration_and_bind(&mut self) -> ParseResult<Declaration> {
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

    /// Report an array designator that addresses past the end of its array.
    ///
    /// Only the outermost list is walked, and only when the bound is known: an
    /// array sized *by* this initializer cannot overflow it, and a designator
    /// inside a nested list addresses a different object than `typ`.
    fn check_designator_bounds(&self, typ: TypeId, elements: &[InitElement]) {
        if self.types.kind(typ) != TypeKind::Array {
            return;
        }
        let Some(capacity) = self.types.array_size(typ).filter(|&n| n > 0) else {
            return;
        };
        for element in elements {
            let Some(designator) = element.designators.first() else {
                continue;
            };
            let end = match designator {
                Designator::Index(i) => *i,
                Designator::IndexRange(_, hi) => *hi,
                Designator::Field(_) => continue,
            };
            if end >= capacity as i64 {
                diag::error_args(
                    element.value.pos,
                    "array index in initializer exceeds array bounds ({0} >= {1})",
                    &[&end.to_string(), &capacity.to_string()],
                );
                return;
            }
        }
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
    pub(super) fn check_excess_initializers(&self, typ: TypeId, init: &Expr) {
        let ExprKind::InitList { elements } = &init.kind else {
            return;
        };
        // A designator names its own position, so the *count* of elements says
        // nothing -- but the position itself can still be out of range, and
        // nothing checked that anywhere: `int a[4] = {[10] = 7};` compiled and
        // wrote past the array. GCC rejects it. Ranges make it easy to write by
        // accident, so the bound is checked here where the array's size is
        // known; the element-count check below still stands aside.
        self.check_designator_bounds(typ, elements);
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

    pub(super) fn infer_array_size_from_init(&mut self, typ: TypeId, init: &Expr) -> TypeId {
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
    pub(super) fn check_array_size_type(&self, expr: &Expr, pos: Position) -> ParseResult<()> {
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
    pub(super) fn derive_array_type(
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
            let mut designator_high = None;
            for designator in &element.designators {
                match designator {
                    Designator::Index(index) => {
                        designator_index = Some(*index);
                        break;
                    }
                    Designator::IndexRange(lo, hi) => {
                        designator_index = Some(*lo);
                        designator_high = Some(*hi);
                        break;
                    }
                    Designator::Field(_) => {}
                }
            }

            let index = if let Some(explicit_index) = designator_index {
                // A range advances the cursor past its high endpoint and
                // extends the inferred bound to it: `int a[] = {[0 ... 3] = 1}`
                // is four elements. This is a second, independent copy of the
                // rule in `group_array_init_elements`; both have to know.
                let end = designator_high.unwrap_or(explicit_index);
                current_index = end + 1;
                if end > max_index {
                    max_index = end;
                }
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
    pub(super) fn parse_for_init_declaration_and_bind(&mut self) -> ParseResult<Declaration> {
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
        // A declaration that stops right here declares nothing, and that --
        // not a missing type specifier -- is what to report. `static;` used to
        // draw "type specifier missing", blaming the half that was absent
        // rather than the declarator that was. The `;` arms below do it.
        if !self.is_special(b';') {
            self.check_implicit_int(decl_pos);
        }
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
        if self.is_special(b';') {
            self.check_declares_something(decl_pos, &base_type);
        } else {
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
                    let sym = self
                        .declared_symbol(name, typ, validated_align)
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
    pub(super) fn parse_type_specifier(&mut self) -> ParseResult<Type> {
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
    pub(super) fn check_redeclaration(&mut self, name: StringId, new_type: TypeId, pos: Position) {
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
    pub(super) fn check_typedef_redefinition(
        &mut self,
        name: StringId,
        new_type: TypeId,
        pos: Position,
    ) {
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
    pub(super) fn check_implicit_int(&mut self, pos: Position) {
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

    /// The specifier a declaration led with, for the diagnostic below.
    ///
    /// Ordered so the one a reader would blame comes first: a storage class
    /// is more surprising in an empty declaration than a bare qualifier.
    fn leading_specifier_name(modifiers: TypeModifiers) -> Option<&'static str> {
        const SPELLINGS: &[(TypeModifiers, &str)] = &[
            (TypeModifiers::TYPEDEF, "typedef"),
            (TypeModifiers::EXTERN, "extern"),
            (TypeModifiers::STATIC, "static"),
            (TypeModifiers::REGISTER, "register"),
            (TypeModifiers::AUTO, "auto"),
            (TypeModifiers::THREAD_LOCAL, "_Thread_local"),
            (TypeModifiers::INLINE, "inline"),
            (TypeModifiers::CONST, "const"),
            (TypeModifiers::VOLATILE, "volatile"),
        ];
        SPELLINGS
            .iter()
            .find(|(m, _)| modifiers.contains(*m))
            .map(|(_, name)| *name)
    }

    /// Diagnose a declaration that stops at `;` having declared nothing.
    ///
    /// C17 6.7p2 requires a declaration to declare a declarator, a tag, or the
    /// members of an enumeration. `struct S;` and `enum E { A };` declare a
    /// tag and are the reason this arm exists at all; `int;`, `static;` and
    /// `int register;` declare nothing whatsoever and were accepted silently.
    ///
    /// Reported rather than warned: the constraint is violated, and a
    /// declaration that declares nothing is always a typo or a stray token.
    /// (gcc errors on `register`/`inline` here and warns on the rest; both are
    /// conforming, since 6.7p2 asks only for a diagnostic.)
    pub(super) fn check_declares_something(&mut self, pos: Position, base_type: &Type) {
        // A tag -- declared or defined -- is the thing this declaration form
        // exists to express, so it always counts.
        if matches!(
            base_type.kind,
            TypeKind::Struct | TypeKind::Union | TypeKind::Enum
        ) {
            return;
        }

        match Self::leading_specifier_name(base_type.modifiers) {
            Some(spec) => diag::error_args(pos, "'{0}' in empty declaration", &[spec]),
            None => diag::error(pos, &gettext("declaration declares nothing")),
        }
    }
}
