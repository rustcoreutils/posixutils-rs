//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Copy propagation: a use of `x` where `x = copy y` becomes a use of `y`.
//
// Promotion out of memory gives every read of a local its own `Copy`, and
// phi elimination, inlining and folding add more; nothing removed them, so
// each became a register-to-register move -- close to half of what a small
// function emitted. This rewrites the uses and leaves the copies, now unused,
// for `dce`.
//
// A copy is followed only when it changes nothing about the value:
//
// * Of the same width as its source's definition, and of the same register
//   class. A *narrowing* copy is not a no-op here, because the backends extend
//   a narrow copy when they perform it (`copy.8` sign- or zero-extends), and
//   they compare at no less than 32 bits. `x = copy.8 y` with `y` a 32-bit
//   value, then `x == 0` at 8 bits: substituting `y` compares all 32 of its
//   bits. So `ConstMap::root`, which follows any copy at least as wide as the
//   use, is the right question for "are these the same value" and the wrong
//   one for "may this operand be replaced".
// * Of a constant, only where the constant reads the same at the copy's
//   width however it is read (`constfold::unambiguous_at`); a float constant
//   is left alone.
//
// Safe in SSA form because every pseudo has one definition, so the copy's
// source holds the same value wherever the copy's result is used -- and the
// critical-edge splitting `lower` does before phi elimination is what keeps
// the copies that elimination introduces from overwriting a value a
// propagated use still needs (see `ir/cfg.rs`).
//

use super::constfold::unambiguous_at;
use super::{Function, Opcode, PseudoId, PseudoKind};
use crate::types::TypeTable;
use std::collections::HashMap;

/// What a pseudo's definition produces: its width, and whether it lives in
/// a floating-point register.
#[derive(Clone, Copy, PartialEq, Eq)]
struct Shape {
    width: u32,
    float: bool,
}

/// Rewrite every use of a no-op copy's result to its source. Returns whether
/// anything changed.
pub fn run(func: &mut Function, types: &TypeTable) -> bool {
    let forward = forwarding(func, types);
    if forward.is_empty() {
        return false;
    }
    let resolve = |mut p: PseudoId| {
        // Bounded by the chain's length: SSA has no copy cycles, but a
        // bound costs nothing and makes that an assumption rather than a hang.
        for _ in 0..=forward.len() {
            match forward.get(&p) {
                Some(&next) => p = next,
                None => break,
            }
        }
        p
    };

    let mut changed = false;
    for bb in &mut func.blocks {
        for insn in &mut bb.insns {
            // An `asm`'s operands name registers by constraint, and a `Phi`
            // names the `PhiSource`s feeding it rather than values.
            if matches!(insn.op, Opcode::Asm | Opcode::Phi | Opcode::Nop) {
                continue;
            }
            for s in &mut insn.src {
                let r = resolve(*s);
                if r != *s {
                    *s = r;
                    changed = true;
                }
            }
            if let Some(t) = insn.extra().indirect_target {
                let r = resolve(t);
                if r != t {
                    insn.extra_mut().indirect_target = Some(r);
                    changed = true;
                }
            }
        }
    }
    changed
}

/// Each copy whose result may be replaced by its source, mapped to that
/// source.
fn forwarding(func: &Function, types: &TypeTable) -> HashMap<PseudoId, PseudoId> {
    let mut shape: HashMap<PseudoId, Shape> = HashMap::new();
    let args = func.arg_types();
    for p in &func.pseudos {
        if let PseudoKind::Arg(n) = p.kind {
            let typ = if args.sret == Some(p.id) {
                Some(types.void_ptr_id)
            } else {
                args.of(n)
            };
            if let Some(t) = typ {
                shape.insert(
                    p.id,
                    Shape {
                        width: types.size_bits(t),
                        float: types.is_float(t),
                    },
                );
            }
        }
    }
    // An inline-asm output is a second definition of its pseudo, which the
    // SSA invariant exempts; such a pseudo is never a copy's safe source.
    let mut multiply_defined: Vec<PseudoId> = Vec::new();
    for insn in func.blocks.iter().flat_map(|b| &b.insns) {
        if let Some(asm) = &insn.extra().asm_data {
            multiply_defined.extend(asm.outputs.iter().map(|o| o.pseudo));
        }
        if let (Some(t), Some(typ)) = (insn.target, insn.typ) {
            shape.insert(
                t,
                Shape {
                    width: insn.size,
                    float: types.is_float(typ),
                },
            );
        }
    }
    for p in &multiply_defined {
        shape.remove(p);
    }

    let mut forward = HashMap::new();
    for insn in func.blocks.iter().flat_map(|b| &b.insns) {
        if insn.op != Opcode::Copy || insn.src.len() != 1 {
            continue;
        }
        let (Some(target), Some(typ)) = (insn.target, insn.typ) else {
            continue;
        };
        // A `Sym` target names storage, and uses of it are addresses, not
        // the value copied in.
        let register = matches!(
            func.get_pseudo(target).map(|p| &p.kind),
            Some(PseudoKind::Reg(_)) | None
        );
        if !register || multiply_defined.contains(&target) {
            continue;
        }
        let src = insn.src[0];
        let copy = Shape {
            width: insn.size,
            float: types.is_float(typ),
        };
        let same = match func.get_pseudo(src).map(|p| &p.kind) {
            Some(PseudoKind::Val(v)) => !copy.float && unambiguous_at(*v, copy.width.max(1)),
            Some(PseudoKind::Reg(_) | PseudoKind::Arg(_) | PseudoKind::Phi(_)) | None => {
                shape.get(&src) == Some(&copy)
            }
            _ => false,
        };
        if same {
            forward.insert(target, src);
        }
    }
    forward
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Instruction, Pseudo};
    use crate::target::Target;

    fn func_of(insns: Vec<Instruction>, pseudos: Vec<Pseudo>) -> Function {
        let types = TypeTable::new(&Target::host());
        let mut f = Function::new("f", types.int_id);
        for p in pseudos {
            f.add_pseudo(p);
        }
        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        for i in insns {
            bb.add_insn(i);
        }
        f.add_block(bb);
        f.entry = BasicBlockId(0);
        f
    }

    /// `%2 = copy.32 %1; %3 = add.32 %2, %2` reads `%1` directly.
    #[test]
    fn copyprop_forwards_an_exact_copy() {
        let types = TypeTable::new(&Target::host());
        let int = types.int_id;
        let mut f = func_of(
            vec![
                Instruction::binop(Opcode::Add, PseudoId(1), PseudoId(0), PseudoId(0), int, 32),
                Instruction::unop(Opcode::Copy, PseudoId(2), PseudoId(1), int, 32),
                Instruction::binop(Opcode::Add, PseudoId(3), PseudoId(2), PseudoId(2), int, 32),
            ],
            (0..4).map(|i| Pseudo::reg(PseudoId(i), i)).collect(),
        );
        assert!(run(&mut f, &types));
        assert_eq!(f.blocks[0].insns[3].src, vec![PseudoId(1), PseudoId(1)]);
        assert!(!run(&mut f, &types), "a second run has nothing left to do");
    }

    /// A narrowing copy is not a no-op: the backends extend it, and a use of
    /// the narrow value must keep reading the extended one.
    #[test]
    fn copyprop_keeps_a_narrowing_copy() {
        let types = TypeTable::new(&Target::host());
        let (int, ch) = (types.int_id, types.char_id);
        let mut f = func_of(
            vec![
                Instruction::binop(Opcode::Add, PseudoId(1), PseudoId(0), PseudoId(0), int, 32),
                Instruction::unop(Opcode::Copy, PseudoId(2), PseudoId(1), ch, 8),
                Instruction::compare(
                    Opcode::SetEq,
                    PseudoId(3),
                    (PseudoId(2), PseudoId(0)),
                    (ch, 8),
                    (int, 32),
                ),
            ],
            (0..4).map(|i| Pseudo::reg(PseudoId(i), i)).collect(),
        );
        assert!(!run(&mut f, &types));
        assert_eq!(f.blocks[0].insns[3].src[0], PseudoId(2));
    }

    /// A copy between register classes is a move between register files.
    #[test]
    fn copyprop_keeps_a_copy_that_changes_register_class() {
        let types = TypeTable::new(&Target::host());
        let (long, dbl) = (types.long_id, types.double_id);
        let mut f = func_of(
            vec![
                Instruction::binop(Opcode::Add, PseudoId(1), PseudoId(0), PseudoId(0), long, 64),
                Instruction::unop(Opcode::Copy, PseudoId(2), PseudoId(1), dbl, 64),
                Instruction::binop(Opcode::FAdd, PseudoId(3), PseudoId(2), PseudoId(2), dbl, 64),
            ],
            (0..4).map(|i| Pseudo::reg(PseudoId(i), i)).collect(),
        );
        assert!(!run(&mut f, &types));
    }

    /// A constant is forwarded where it reads the same at the copy's width,
    /// and not where it would change meaning: 200 in eight bits is -56 or
    /// 200, depending on who reads it.
    #[test]
    fn copyprop_forwards_only_an_unambiguous_constant() {
        let types = TypeTable::new(&Target::host());
        let ch = types.char_id;
        for (v, forwarded) in [(0, true), (100, true), (200, false)] {
            let mut f = func_of(
                vec![
                    Instruction::unop(Opcode::Copy, PseudoId(2), PseudoId(1), ch, 8),
                    Instruction::store(PseudoId(2), PseudoId(0), 0, ch, 8),
                ],
                vec![
                    Pseudo::reg(PseudoId(0), 0),
                    Pseudo::val(PseudoId(1), v),
                    Pseudo::reg(PseudoId(2), 2),
                ],
            );
            assert_eq!(run(&mut f, &types), forwarded, "{v}");
        }
    }
}
