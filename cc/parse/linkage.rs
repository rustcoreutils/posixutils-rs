//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Linkage (C17 6.2.2), and the rules that follow from it: an identifier with
// no linkage is declared once per scope (6.7p3), one with linkage names one
// entity across every scope -- so every declaration of it must agree with
// the others about its linkage and its type (6.2.7p2) -- and that entity is
// defined at most once (6.9p3, p5).
//

use super::bind::DeclScope;
use super::declaration::Redeclared;
use super::parser::Parser;
use crate::diag;
use crate::strings::StringId;
use crate::symbol::{DeclaredAt, Linkage, Namespace, Symbol, SymbolId, SymbolKind};
use crate::token::lexer::Position;
use crate::types::{TypeId, TypeKind, TypeModifiers};

/// What the translation unit has said so far about one identifier with
/// linkage, in whichever scopes it said it.
#[derive(Clone, Copy)]
pub(crate) struct LinkedName {
    linkage: Linkage,
    /// The type the declarations so far agree on: the first one's, given
    /// its extent by any later one that has it, in whatever scope.
    typ: TypeId,
    /// Whether that extent came from a block-scope declaration made after a
    /// file-scope definition had left the array without one -- which the
    /// end of the unit then completes to one element regardless (gcc's
    /// "completed incompatibly with implicit initialization").
    completed_late: bool,
    /// Whether one of them was a definition.
    defined: bool,
    /// Whether that definition was a GNU inline-only one (`extern inline`
    /// under `gnu_inline` semantics), which emits nothing and so may be
    /// followed by the real one -- the one further definition allowed.
    inline_only: bool,
    /// Whether the declarations so far leave the name GNU `extern inline`:
    /// one said `extern inline` under GNU inline semantics, and no plain
    /// `inline` declaration or real definition has followed. gcc lets a
    /// `static` declaration take such a name over.
    gnu_extern_inline: bool,
    /// Whether one of them gave the name a visibility, by attribute or by
    /// `#pragma GCC visibility`, which a later pragma does not override.
    visibility_given: bool,
    /// The one a note about a later declaration points back at.
    pub(super) declared: DeclaredAt,
}

/// One declaration of an object or a function, as linkage sees it.
pub(super) struct Declared {
    pub(super) name: StringId,
    pub(super) typ: TypeId,
    pub(super) pos: Position,
    /// Where the declared identifier is written, which a note about a later
    /// declaration points at.
    pub(super) name_pos: Position,
    /// The storage-class specifiers and `inline`.
    pub(super) storage: TypeModifiers,
    pub(super) scope: DeclScope,
    /// Whether this declaration is a definition: an object with an
    /// initializer, or a function with a body. A tentative definition is not
    /// one (6.9.2p2).
    pub(super) defines: bool,
    /// For a function, whether its own specifiers say `extern inline` under
    /// GNU inline semantics ([`FunctionAttrs::gnu_inline_only`]). With a
    /// body and external linkage, it is the inline-only one.
    ///
    /// [`FunctionAttrs::gnu_inline_only`]: super::ast::FunctionAttrs::gnu_inline_only
    pub(super) gnu_extern_inline: bool,
}

impl Declared {
    /// Whether this is a GNU inline-only body under `linkage`. One with
    /// internal linkage -- `extern inline` after a `static` declaration -- is
    /// an ordinary static function, emitted as gcc emits it.
    pub(super) fn inline_only(&self, linkage: Linkage) -> bool {
        self.defines && self.gnu_extern_inline && linkage == Linkage::External
    }
}

impl Parser<'_> {
    /// The linkage `d` gives its identifier (6.2.2p3-p5), after reporting
    /// every way it contradicts an earlier declaration of it.
    pub(super) fn declare_linkage(&mut self, d: Declared) -> Linkage {
        let is_fn = self.types.kind(d.typ) == TypeKind::Function;
        let file = d.scope == DeclScope::File;
        let spelled = self.idents.get_opt(d.name).unwrap_or("").to_string();

        // The innermost visible declaration of the name, which settles the
        // linkage of an `extern` one (6.2.2p4) and, when it is in this very
        // scope, whether this is a forbidden repeat (6.7p3).
        let visible_id = self.symbols.lookup_id(d.name, Namespace::Ordinary);
        let visible = visible_id.map(|id| self.symbols.get(id)).filter(|s| {
            matches!(
                s.kind,
                SymbolKind::Variable | SymbolKind::Function | SymbolKind::Parameter
            )
        });
        let prior_linkage = visible.map(|s| s.linkage).filter(|&l| l != Linkage::None);
        let same_scope = visible
            .filter(|s| s.scope_depth == self.symbols.depth())
            .map(|s| s.linkage);

        let linkage = if d.storage.contains(TypeModifiers::STATIC) && is_fn && !file {
            // 6.7.1p7: a block-scope function declaration may have no
            // storage-class specifier other than `extern`.
            diag::error_args(
                d.pos,
                "invalid storage class for function '{0}'",
                &[&spelled],
            );
            prior_linkage.unwrap_or(Linkage::External)
        } else if d.storage.contains(TypeModifiers::STATIC) && file {
            Linkage::Internal
        } else if d.storage.contains(TypeModifiers::EXTERN) || is_fn {
            prior_linkage.unwrap_or(Linkage::External)
        } else if file {
            Linkage::External
        } else {
            Linkage::None
        };

        // 6.7p3: in one scope, only an identifier with linkage may be
        // declared again. gcc calls it a redefinition only when both are
        // definitions, and a parameter -- whose scope a function's body
        // shares -- another kind of symbol.
        let prior_defined = visible.is_some_and(|s| s.declared.definition);
        let prior_parameter = visible.is_some_and(|s| s.kind == SymbolKind::Parameter);
        let template = match (same_scope, linkage) {
            (Some(Linkage::None), _) if prior_parameter => {
                Some("'{0}' redeclared as different kind of symbol")
            }
            (Some(Linkage::None), Linkage::None) if d.defines && prior_defined => {
                Some("redefinition of '{0}'")
            }
            (Some(Linkage::None), Linkage::None) => Some("redeclaration of '{0}' with no linkage"),
            (Some(Linkage::None), _) => {
                Some("extern declaration of '{0}' follows declaration with no linkage")
            }
            (Some(_), Linkage::None) => {
                Some("declaration of '{0}' with no linkage follows extern declaration")
            }
            _ => None,
        };
        if let (Some(template), Some(prior)) = (template, visible_id) {
            let prior = self.prior_declaration(prior);
            diag::error_args(d.pos, template, &[&spelled]);
            self.note_previous(d.name, prior);
            return linkage;
        }
        if linkage == Linkage::None {
            return linkage;
        }

        let inline_only = d.inline_only(linkage);
        let declared = DeclaredAt::at(d.name_pos, d.defines);
        let first = LinkedName {
            linkage,
            typ: d.typ,
            completed_late: false,
            defined: d.defines,
            inline_only,
            gnu_extern_inline: d.gnu_extern_inline && linkage == Linkage::External,
            visibility_given: false,
            declared,
        };
        let Some(&prior) = self.linked_names.get(&d.name) else {
            self.linked_names.insert(d.name, first);
            return linkage;
        };

        // gcc's exception to 6.2.2p7: a `static` declaration silently takes
        // over a name the unit has so far declared GNU `extern inline`, and
        // the static one is the function -- of its calls, of `&f`, and of any
        // inline-only body before it (gcc.c-torture `compile/20021120-1`).
        if prior.linkage == Linkage::External
            && linkage == Linkage::Internal
            && prior.gnu_extern_inline
        {
            self.check_linked_type(&d, &prior, same_scope.is_some(), &spelled);
            self.linked_names.insert(d.name, first);
            return linkage;
        }

        // 6.2.2p7: one identifier, two linkages, is undefined; gcc rejects
        // it, in both directions.
        if prior.linkage != linkage {
            let msg = if linkage == Linkage::Internal {
                "static declaration of '{0}' follows non-static declaration"
            } else {
                "non-static declaration of '{0}' follows static declaration"
            };
            diag::error_args(d.pos, msg, &[&spelled]);
            self.note_previous(d.name, (prior.declared, prior.typ));
        }

        self.check_linked_type(&d, &prior, same_scope.is_some(), &spelled);

        // 6.9p3, p5: one definition. A GNU inline-only body defines nothing
        // here, so the real one may follow it -- and only that: gcc rejects
        // an inline-only body after the real one, and a second inline-only
        // body after the first.
        let redefined = d.defines && prior.defined && !(prior.inline_only && !inline_only);
        if redefined {
            diag::error_args(d.pos, "redefinition of '{0}'", &[&spelled]);
            self.note_previous(d.name, (prior.declared, prior.typ));
        }

        // 6.2.7p3: an extent, from any scope, completes the array.
        let completes = self.types.is_incomplete_array(prior.typ)
            && self.types.kind(d.typ) == TypeKind::Array
            && !self.types.is_incomplete_array(d.typ);
        let late = completes && !file && self.has_tentative_array(d.name);

        let prior = self.linked_names.get_mut(&d.name).expect("present");
        if completes {
            prior.typ = d.typ;
            prior.completed_late = late;
        }
        if d.defines && !inline_only {
            prior.inline_only = false;
        } else if d.defines && !prior.defined {
            prior.inline_only = true;
        }
        prior.defined |= d.defines;
        prior.declared = prior.declared.followed_by(declared);
        // A plain `inline` declaration promises the external definition, as
        // a real definition is one; either ends the GNU `extern inline` state.
        if d.gnu_extern_inline {
            prior.gnu_extern_inline = true;
        } else if d.storage.contains(TypeModifiers::INLINE) || d.defines {
            prior.gnu_extern_inline = false;
        }
        linkage
    }

    /// Give a declaration of `name` with `linkage` the visibility
    /// `#pragma GCC visibility` has pushed, in `slot` -- the declaration's
    /// own visibility attribute, if it has one.
    ///
    /// As in gcc, the pragma reaches every declaration with external
    /// linkage, an `extern` one too, which `-fvisibility` leaves alone; and
    /// it yields to a visibility this or any earlier declaration of the name
    /// has given it, by attribute or by the pragma. A function's is recorded
    /// with its other attributes too, for a definition outside the pragma
    /// to inherit.
    pub(super) fn settle_visibility(
        &mut self,
        name: StringId,
        linkage: Linkage,
        is_fn: bool,
        slot: &mut Option<String>,
    ) {
        if linkage != Linkage::External {
            return;
        }
        let pushed = self.current_pragma_visibility();
        let Some(linked) = self.linked_names.get_mut(&name) else {
            return;
        };
        if slot.is_none() && !linked.visibility_given {
            *slot = pushed.map(|how| how.spelling().to_string());
            if is_fn && slot.is_some() {
                let declared = self.declared_fn_attrs.entry(name).or_default();
                declared.symbol.visibility = slot.clone();
            }
        }
        linked.visibility_given |= slot.is_some();
    }

    /// 6.2.7p2: every declaration of one entity has a compatible type. One
    /// in the same scope was compared by `check_redeclaration`; this is
    /// every other -- a block-scope `extern` against the file's, or two
    /// blocks' against each other.
    fn check_linked_type(
        &mut self,
        d: &Declared,
        prior: &LinkedName,
        same_scope: bool,
        spelled: &str,
    ) {
        if same_scope {
            return;
        }
        let old = self.types.without_decl_specifiers(prior.typ);
        let new = self.types.without_decl_specifiers(d.typ);
        if !self.redeclaration_compatible(old, new, Redeclared::Declaration) {
            self.report_conflicting_types(d.name, spelled, d.pos, new, (prior.declared, old));
        }
    }

    /// gcc's "conflicting types for 'x'; have 'T'" about a declaration of
    /// type `new` at `pos`, with its note on the declaration before.
    pub(super) fn report_conflicting_types(
        &mut self,
        name: StringId,
        spelled: &str,
        pos: Position,
        new: TypeId,
        prior: (DeclaredAt, TypeId),
    ) {
        diag::error_args(
            pos,
            "conflicting types for '{0}'; have '{1}'",
            &[spelled, &self.types.format_type(new, Some(self.idents))],
        );
        self.note_previous(name, prior);
    }

    /// The declaration of `symbol`'s name a note points back at, and its
    /// type: for a name with linkage the one every scope's declarations
    /// have settled on, since a later declaration in the same scope binds
    /// no symbol of its own.
    pub(super) fn prior_declaration(&mut self, symbol: SymbolId) -> (DeclaredAt, TypeId) {
        let Symbol {
            name,
            linkage,
            declared,
            typ,
            ..
        } = *self.symbols.get(symbol);
        let declared = match linkage {
            Linkage::None => declared,
            _ => self
                .linked_names
                .get(&name)
                .map_or(declared, |l| l.declared),
        };
        (declared, self.types.without_decl_specifiers(typ))
    }

    /// gcc's note after a diagnostic about declaring `name` again: where it
    /// was declared before, with that declaration's type.
    pub(super) fn note_previous(&mut self, name: StringId, (prior, typ): (DeclaredAt, TypeId)) {
        let spelled = self.idents.get_opt(name).unwrap_or("");
        let shown = self.types.format_type(typ, Some(self.idents));
        let template = if prior.definition {
            "previous definition of '{0}' with type '{1}'"
        } else {
            "previous declaration of '{0}' with type '{1}'"
        };
        diag::note_args(prior.pos, template, &[spelled, &shown]);
    }

    /// Whether a file-scope definition of `name` was written as an array
    /// without its extent.
    fn has_tentative_array(&self, name: StringId) -> bool {
        self.tentative_arrays
            .iter()
            .any(|&(id, _)| self.symbols.get(id).name == name)
    }

    /// The type every declaration of `name` with linkage has built up --
    /// see [`LinkedName::typ`] -- and whether a block-scope declaration
    /// supplied its extent too late ([`LinkedName::completed_late`]).
    pub(super) fn linked_type(&self, name: StringId) -> Option<(TypeId, bool)> {
        self.linked_names
            .get(&name)
            .map(|l| (l.typ, l.completed_late))
    }
}
