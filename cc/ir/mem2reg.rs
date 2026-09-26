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
            // Every role a pseudo can play, through the one enumeration of
            // them. Scanning `src` alone missed two ways a local's storage is
            // named. A call returning a struct in registers names the
            // receiving `__2reg_N` local's `Sym` as its *target*, and nothing
            // reads it when the result is discarded -- `one();` on its own
            // line. And an inline-asm memory operand names the object's `Sym`
            // in `asm_data` when it is addressed in place. Either way the
            // slot was dropped and the backend stored through an unset
            // register. A `target` that defines a value is never a local's
            // `Sym`, so counting targets costs nothing.
            referenced.extend(insn.mentioned());
        }
    }
    let mut dropped: HashSet<PseudoId> = HashSet::new();
    func.locals.retain(|_, local| {
        let keep = referenced.contains(&local.sym);
        if !keep {
            dropped.insert(local.sym);
        }
        keep
    });

    if dropped.is_empty() {
        return;
    }

    // Drop the orphaned `Sym` pseudos too. Nothing refers to them, but they
    // are still named: the inliner treats a `Sym` that is not in the callee's
    // locals as a *global* and clones it into the caller under its original,
    // unmangled name (`i`, `t`, ...), and `arch/*/regalloc.rs` resolves a
    // `Sym` by looking its name up in `func.locals`. Leaving a promoted
    // local's pseudo behind puts a plausible-looking global in both paths.
    func.pseudos.retain(|p| !dropped.contains(&p.id));
    func.rebuild_pseudo_idx();
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{AsmConstraint, AsmData, BasicBlock, BasicBlockId, Instruction, Pseudo};
    use crate::target::Target;
    use crate::types::TypeTable;

    /// A local named only by an inline-asm memory operand is still storage
    /// the program uses. Dropping it left the operand with no stack slot and
    /// the backend addressing it through an unset register.
    #[test]
    fn a_local_named_only_by_an_asm_operand_is_kept() {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("f", types.void_id);
        func.add_pseudo(Pseudo::sym(PseudoId(0), "x.0".into()));
        func.add_local("x.0", PseudoId(0), types.long_id, false, false, None, None);

        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        let mut asm = Instruction::new(Opcode::Asm);
        asm.asm_data = Some(Box::new(AsmData {
            template: String::new(),
            outputs: vec![AsmConstraint {
                pseudo: PseudoId(0),
                name: None,
                matching_output: None,
                constraint: "=m".into(),
                size: 64,
                offset: 0,
            }],
            inputs: vec![],
            clobbers: vec![],
            goto_labels: vec![],
        }));
        bb.add_insn(asm);
        bb.add_insn(Instruction::ret(None));
        func.blocks.push(bb);
        func.entry = BasicBlockId(0);

        mem2reg(&mut func);
        assert!(func.locals.contains_key("x.0"));
        assert!(func.get_pseudo(PseudoId(0)).is_some());
    }
}
