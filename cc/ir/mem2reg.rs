//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// mem2reg — drop stack slots for promoted locals.
//
// Backend regalloc allocates a stack slot per Sym in `func.locals`, so a
// local whose Sym pseudo has no remaining references in the IR is removed
// here. Volatile, atomic and addr-taken locals are protected: their Sym is
// still referenced by a surviving Load/Store or SymAddr, so the retain
// check leaves them in place.

use super::{Function, Opcode, PseudoId};
use std::collections::HashSet;

/// Drop `func.locals` entries whose `Sym` pseudo has no remaining
/// users in the IR. Run after `ssa_convert` and before the IR is
/// handed off to optimization / lowering / codegen.
pub fn mem2reg(func: &mut Function) {
    let mut referenced: HashSet<PseudoId> = HashSet::new();
    for block in &func.blocks {
        for insn in &block.insns {
            // Nops carry stale operands from the in-place Store->Nop
            // rewrite ssa.rs performs; they are not real references.
            if insn.op == Opcode::Nop {
                continue;
            }
            for &src in &insn.src {
                referenced.insert(src);
            }
        }
    }
    func.locals
        .retain(|_, local| referenced.contains(&local.sym));
}
