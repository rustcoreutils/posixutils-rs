//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//! `-fstack-protector`: which functions get a canary.
//!
//! A protected function copies the guard value into a frame slot above
//! every local -- the first slot the register allocator hands out, so an
//! array overrunning upwards reaches it before anything the epilogue
//! reloads -- and compares the two before each return, calling
//! `__stack_chk_fail` when they differ. A path that ends in a noreturn call
//! has no return and no check, as in gcc. Where the guard lives and how
//! the comparison is spelt are each back end's; which functions are
//! protected is decided here, for both, by gcc's rules.
//!
//! The decision is made on the function as the back end receives it,
//! after inlining and the optimizer: an array inlined into a caller
//! protects the caller, and one the optimizer deleted protects nothing --
//! gcc decides at the same point, when it expands to RTL.

use crate::ir::{Function, Opcode, PseudoId};
use crate::parse::ast::StackProtectAttr;
use crate::target::StackProtector;
use crate::types::{TypeId, TypeKind, TypeTable};
use std::collections::HashSet;

/// The global guard and the failure function, as libc names them.
pub const GUARD_SYMBOL: &str = "__stack_chk_guard";
pub const FAIL_SYMBOL: &str = "__stack_chk_fail";

/// gcc's `--param ssp-buffer-size`: the size from which `-fstack-protector`
/// counts a `char` array.
const SSP_BUFFER_SIZE: usize = 8;

/// Does `func` get a canary under `level`?
pub fn protects(func: &Function, types: &TypeTable, level: StackProtector) -> bool {
    if level == StackProtector::Off {
        return false;
    }
    match func.stack_protect {
        StackProtectAttr::Exempt => return false,
        StackProtectAttr::Protect => return true,
        StackProtectAttr::Unspecified => {}
    }
    match level {
        StackProtector::Off | StackProtector::Explicit => false,
        StackProtector::All => true,
        StackProtector::Default => {
            allocates(func) || locals(func).any(|(_, t)| large_char_array(t, types))
        }
        StackProtector::Strong => {
            allocates(func)
                || locals(func)
                    .any(|(sym, t)| holds_array(t, types) || crate::ir::escape::escapes(func, sym))
        }
    }
}

/// Whether `func` allocates on the stack at run time: `alloca`, a VLA.
fn allocates(func: &Function) -> bool {
    func.blocks
        .iter()
        .flat_map(|b| &b.insns)
        .any(|i| i.op == Opcode::Alloca)
}

/// The objects gcc examines: the function's own automatic variables, not
/// its parameters, and only those still referenced -- one the optimizer has
/// deleted is not in the frame. Each as its symbol and type.
fn locals(func: &Function) -> impl Iterator<Item = (PseudoId, TypeId)> + '_ {
    let used: HashSet<PseudoId> = func
        .blocks
        .iter()
        .flat_map(|b| &b.insns)
        .filter(|i| i.op != Opcode::LifetimeEnd)
        .flat_map(|i| i.mentioned())
        .collect();
    func.locals
        .iter()
        .filter(move |(name, l)| {
            used.contains(&l.sym) && !func.params.iter().any(|(p, _)| p == *name)
        })
        .map(|(_, l)| (l.sym, l.typ))
}

/// gcc's `SPCT_HAS_LARGE_CHAR_ARRAY`: an array of `char`, `signed char` or
/// `unsigned char` of [`SSP_BUFFER_SIZE`] bytes or more, alone or as a
/// member at any depth. An array of anything else -- of arrays of `char`
/// included -- is not one.
fn large_char_array(t: TypeId, types: &TypeTable) -> bool {
    match types.kind(t) {
        TypeKind::Array => {
            types
                .base_type(t)
                .is_some_and(|e| types.kind(e) == TypeKind::Char)
                && types.size_bytes(t) >= SSP_BUFFER_SIZE
        }
        TypeKind::Struct | TypeKind::Union => types
            .composite(t)
            .is_some_and(|c| c.members.iter().any(|m| large_char_array(m.typ, types))),
        _ => false,
    }
}

/// Where a protected function lays out a local of type `t`, nearest the
/// canary first: gcc's phases, a `char` array of any size (not inside an
/// aggregate), then anything else holding an array, then the rest.
pub fn placement(t: TypeId, types: &TypeTable) -> u8 {
    let char_array = types.kind(t) == TypeKind::Array
        && types
            .base_type(t)
            .is_some_and(|e| types.kind(e) == TypeKind::Char);
    if char_array {
        0
    } else if holds_array(t, types) {
        1
    } else {
        2
    }
}

/// An array, or an aggregate with one among its members at any depth.
fn holds_array(t: TypeId, types: &TypeTable) -> bool {
    match types.kind(t) {
        TypeKind::Array => true,
        TypeKind::Struct | TypeKind::Union => types
            .composite(t)
            .is_some_and(|c| c.members.iter().any(|m| holds_array(m.typ, types))),
        _ => false,
    }
}
