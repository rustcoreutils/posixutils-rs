//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Where an indirect branch may land, which branch protection marks: x86-64's
// `endbr64` under `-fcf-protection=branch`, aarch64's `bti` under
// `-mbranch-protection=bti`. One rule for both targets, gcc's.
//

use crate::ir::Module;
use std::collections::HashSet;

/// The landing pads of a unit built for branch protection.
///
/// Each backend marks four kinds of place, each of which an indirect branch
/// reaches: the entries this plans ([`Self::entry`]) and the labels it plans
/// ([`Self::label`]); and, whenever protection is on ([`Self::active`]), the
/// return of a call that returns twice, an exception landing pad, and the
/// resume point of `__builtin_setjmp`, which the backends find as they emit
/// them.
#[derive(Debug, Default)]
pub struct LandingPads {
    active: bool,
    entries: HashSet<String>,
    labels: HashSet<String>,
}

impl LandingPads {
    /// Where an indirect branch may land in `module`, as gcc decides: the
    /// entry of every function but a static one only ever called directly --
    /// which gcc knows only when `optimizing`, so at -O0 every function is
    /// one -- and every label whose address is taken. A static function's
    /// address may be taken in code, in an initializer or by an alias; one
    /// that runs from `.init_array` or `.fini_array`, or that `used` keeps
    /// for whoever names it, is reached indirectly too. A function whose type
    /// is `nocf_check` asked for no landing pad. Nothing at all unless
    /// `protect`.
    pub fn plan(module: &Module, optimizing: bool, protect: bool) -> Self {
        if !protect {
            return Self::default();
        }
        let statics: HashSet<String> = module
            .functions
            .iter()
            .filter(|f| f.is_static)
            .map(|f| f.name.clone())
            .collect();
        let taken = module.address_taken(&statics);
        let entries = module
            .functions
            .iter()
            .filter(|f| {
                !f.nocf_check
                    && (!optimizing
                        || !f.is_static
                        || taken.contains(&f.name)
                        || f.symbol_attrs.used
                        || f.constructor.is_some()
                        || f.destructor.is_some())
            })
            .map(|f| f.name.clone())
            .collect();
        let labels: HashSet<String> = module
            .functions
            .iter()
            .filter(|f| f.takes_label_addr || f.saves_label_in_static)
            .flat_map(|f| f.blocks.iter().map(|bb| bb.id.label_symbol(&f.name)))
            .collect();
        Self {
            active: true,
            entries,
            labels: module.referenced_in_output(&labels),
        }
    }

    /// Whether the unit is built for branch protection at all.
    pub fn active(&self) -> bool {
        self.active
    }

    /// Whether function `name` starts with a landing pad.
    pub fn entry(&self, name: &str) -> bool {
        self.entries.contains(name)
    }

    /// Whether the block labelled `symbol` starts with one.
    pub fn label(&self, symbol: &str) -> bool {
        self.labels.contains(symbol)
    }
}
