//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// mem2reg — drop stack slots for promoted locals (M3).
//
// `ssa::ssa_convert` already does the heavy lifting: for each non-
// addr-taken, non-volatile, non-atomic scalar local, it inserts φ-nodes
// at the iterated dominance frontier, then converts every `Load` from
// the local into `Copy reaching_def` and every `Store` into `Nop`. The
// local's `Sym` pseudo no longer has any users — there is no `SymAddr`
// (those are the ones ssa.rs *won't* promote, by definition) and no
// surviving Load/Store reference.
//
// What ssa.rs does NOT do is delete the now-orphaned entries from
// `func.locals`. The backend regalloc still walks the locals map to
// allocate a stack slot per Sym, even though the slot is never read
// or written. M3 closes that gap by removing locals whose Sym pseudo
// has zero remaining references in the IR. Volatile/atomic/addr-taken
// locals are protected: their Sym is still referenced (by surviving
// Load/Store or SymAddr instructions), so the retain check leaves
// them in place.

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
