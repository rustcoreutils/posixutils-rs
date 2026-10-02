//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Inline-asm operands that name something fixed: an object at a constant
// offset, or a constant written into the template.
//
// `AddrWalk` follows address arithmetic back to the object it names. The
// linearizer uses it for a memory operand, within the block it is building;
// `resolve_immediates` uses it over a whole function once optimization is
// done.
//
// An immediate-only operand (`"i"`, `"n"`, `"s"`, ...) must be a constant
// when gcc emits the statement, which is after optimization: a `static
// inline` helper passing its parameter to `"i"` is accepted at -O2 once the
// call is inlined with a literal, and rejected at -O0 or when the parameter
// stays a variable, with "impossible constraint in 'asm'". c17 decides at the
// same point. Before, it accepted any operand and substituted whatever
// register held it -- `"i"(&global)` gave `%rax` rather than `$global`.

use super::{AsmConstraint, Function, Instruction, Module, Opcode, PseudoId, PseudoKind};
use crate::arch::asm_constraints::{canonical_int, FloatImm};
use std::collections::{HashMap, HashSet};

/// Follows the address arithmetic of an operand back to the object it names.
/// It only reads: the arithmetic stays for whatever else uses it, and DCE
/// removes the rest.
pub(crate) struct AddrWalk<'w> {
    pub func: &'w Function,
    /// The instruction defining each pseudo the walk may follow.
    pub defs: &'w HashMap<PseudoId, &'w Instruction>,
}

impl AddrWalk<'_> {
    /// `p` as (object `Sym`, constant byte offset), for an object `admit`
    /// accepts, given the `Sym` and the `SymAddr` taking its address.
    pub fn object(
        &self,
        p: PseudoId,
        admit: &dyn Fn(PseudoId, &Instruction) -> bool,
    ) -> Option<(PseudoId, i64)> {
        let insn = *self.defs.get(&p)?;
        match insn.op {
            Opcode::SymAddr => {
                let sym = *insn.src.first()?;
                let is_sym = matches!(
                    self.func.get_pseudo(sym).map(|x| &x.kind),
                    Some(PseudoKind::Sym(_))
                );
                (is_sym && admit(sym, insn)).then_some((sym, 0))
            }
            Opcode::Copy => self.object(*insn.src.first()?, admit),
            Opcode::Add => {
                let (a, b) = (*insn.src.first()?, *insn.src.get(1)?);
                match self.object(a, admit) {
                    Some((sym, off)) => Some((sym, off.checked_add(self.constant(b)?)?)),
                    None => {
                        let (sym, off) = self.object(b, admit)?;
                        Some((sym, off.checked_add(self.constant(a)?)?))
                    }
                }
            }
            Opcode::Sub => {
                let (sym, off) = self.object(*insn.src.first()?, admit)?;
                Some((sym, off.checked_sub(self.constant(*insn.src.get(1)?)?)?))
            }
            _ => None,
        }
    }

    /// `p` as a constant, folding the widening and scaling a subscript is
    /// linearized into.
    pub fn constant(&self, p: PseudoId) -> Option<i64> {
        if let Some(PseudoKind::Val(v)) = self.func.get_pseudo(p).map(|x| &x.kind) {
            return i64::try_from(*v).ok();
        }
        let insn = *self.defs.get(&p)?;
        match insn.op {
            Opcode::Sext | Opcode::Zext => {
                let v = self.constant(*insn.src.first()?)?;
                let bits = insn.src_size;
                Some(if bits == 0 || bits >= 64 {
                    v
                } else if insn.op == Opcode::Sext {
                    (v << (64 - bits)) >> (64 - bits)
                } else {
                    v & ((1i64 << bits) - 1)
                })
            }
            Opcode::Mul => {
                let a = self.constant(*insn.src.first()?)?;
                a.checked_mul(self.constant(*insn.src.get(1)?)?)
            }
            Opcode::Copy => self.constant(*insn.src.first()?),
            _ => None,
        }
    }

    /// The pseudo a chain of copies starting at `p` reads from.
    fn copied_from(&self, mut p: PseudoId) -> PseudoId {
        while let Some(insn) = self.defs.get(&p).filter(|i| i.op == Opcode::Copy) {
            match insn.src.first() {
                Some(&src) => p = src,
                None => break,
            }
        }
        p
    }
}

/// The thread-local symbols of `module`: their addresses are no link-time
/// constant, so no immediate may name one.
pub fn thread_locals(module: &Module) -> HashSet<String> {
    module
        .globals
        .iter()
        .filter(|g| g.is_thread_local)
        .map(|g| g.name.clone())
        .chain(module.extern_tls_symbols.iter().cloned())
        .collect()
}

/// What an immediate-only operand turned out to be.
enum Immediate {
    /// An integer constant held by this pseudo.
    Int(PseudoId),
    /// A floating constant held by this pseudo.
    Float(PseudoId),
    /// A global's address plus a byte offset.
    Symbol(PseudoId, i64),
    /// An integer the walk folded, with no pseudo holding it yet.
    Folded(i64),
}

/// Rewrite every immediate-only inline-asm input of `func` to the constant it
/// is, and report each one that is not a constant of a kind its constraint
/// takes. Returns whether anything was rewritten, leaving the arithmetic that
/// fed it for DCE.
///
/// An integer or floating operand names its constant pseudo directly; a
/// symbolic one names the global's `Sym` with the offset on the operand, as a
/// memory operand does, so the backend writes `$sym+off`.
pub fn resolve_immediates(func: &mut Function, thread_locals: &HashSet<String>) -> bool {
    // An inline definition is never emitted, so nothing is substituted
    // into it; gcc never looks at it either.
    if !func.emit {
        return false;
    }
    let rewrites = immediate_rewrites(func, thread_locals);
    let changed = !rewrites.is_empty();
    for (b, i, input, imm) in rewrites {
        let (pseudo, offset) = match imm {
            Immediate::Int(p) | Immediate::Float(p) => (p, 0),
            Immediate::Symbol(sym, off) => (sym, off),
            Immediate::Folded(v) => (func.create_const_pseudo(v.into()), 0),
        };
        let insn = &mut func.blocks[b].insns[i];
        if let Some(asm) = insn.extra_mut().asm_data.as_mut() {
            asm.inputs[input].pseudo = pseudo;
            asm.inputs[input].offset = offset;
        }
    }
    changed
}

/// Each immediate-only input to rewrite, by block, instruction and input
/// index, reporting the ones that cannot be satisfied.
fn immediate_rewrites(
    func: &Function,
    thread_locals: &HashSet<String>,
) -> Vec<(usize, usize, usize, Immediate)> {
    let asms = || {
        func.blocks.iter().enumerate().flat_map(|(b, bb)| {
            bb.insns.iter().enumerate().filter_map(move |(i, insn)| {
                let asm = insn.extra().asm_data.as_deref()?;
                Some((b, i, insn, asm))
            })
        })
    };
    if !asms().any(|(.., asm)| asm.inputs.iter().any(|c| c.class.is_immediate_only())) {
        return Vec::new();
    }
    // The definition recorded for an asm output says nothing about the
    // value (see `Function::asm_defined_pseudos`): follow none of them.
    let redefined = func.asm_defined_pseudos();
    let defs: HashMap<PseudoId, &Instruction> = func
        .blocks
        .iter()
        .flat_map(|bb| &bb.insns)
        .filter_map(|insn| insn.target.map(|t| (t, insn)))
        .filter(|(t, _)| !redefined.contains(t))
        .collect();
    let walk = AddrWalk { func, defs: &defs };
    let global = |sym: PseudoId, _: &Instruction| {
        func.local_of(sym).is_none()
            && !matches!(
                func.get_pseudo(sym).map(|p| &p.kind),
                Some(PseudoKind::Sym(name)) if thread_locals.contains(name)
            )
    };

    let mut rewrites = Vec::new();
    for (b, i, insn, asm) in asms() {
        let mut number = asm.outputs.len();
        for (input, c) in asm.inputs.iter().enumerate() {
            if c.is_hidden_readwrite_input() {
                continue;
            }
            number += 1;
            if !c.class.is_immediate_only() || c.matching_output.is_some() {
                continue;
            }
            let imm = c.class.imm;
            let int = !imm.int.is_empty();
            let source = walk.copied_from(c.pseudo);
            let found = match func.get_pseudo(source).map(|p| &p.kind) {
                Some(PseudoKind::Val(_)) if int => Some(Immediate::Int(source)),
                Some(PseudoKind::FVal(_)) if imm.float != FloatImm::None => {
                    Some(Immediate::Float(source))
                }
                Some(PseudoKind::Val(_) | PseudoKind::FVal(_)) => None,
                // Already resolved: a global named by the operand itself.
                Some(PseudoKind::Sym(_)) if imm.symbol && global(source, insn) => {
                    Some(Immediate::Symbol(source, c.offset))
                }
                _ => match walk.object(c.pseudo, &global) {
                    Some((sym, off)) if imm.symbol => Some(Immediate::Symbol(sym, off)),
                    Some(_) => None,
                    None if int => walk.constant(c.pseudo).map(Immediate::Folded),
                    None => None,
                },
            };
            if let Some(Err(what)) = found.as_ref().map(|f| out_of_range(func, c, f)) {
                crate::diag::error(
                    insn.pos.unwrap_or_default(),
                    &format!(
                        "impossible constraint in 'asm': operand {} ({what}) is out of \
                         range for \"{}\"",
                        number - 1,
                        c.constraint
                    ),
                );
                continue;
            }
            match found {
                Some(Immediate::Int(p) | Immediate::Float(p)) if p == c.pseudo => {}
                Some(Immediate::Symbol(sym, off)) if sym == c.pseudo && off == c.offset => {}
                Some(found) => rewrites.push((b, i, input, found)),
                None => crate::diag::error(
                    insn.pos.unwrap_or_default(),
                    &format!(
                        "impossible constraint in 'asm': operand {} must be {} for \"{}\"",
                        number - 1,
                        imm.describe(),
                        c.constraint
                    ),
                ),
            }
        }
    }
    rewrites
}

/// A constant of the right kind may still be one no letter's range holds:
/// `"I"(32)` on x86-64, a non-zero `"Z"` on aarch64. `Err` names the value.
/// An integer is checked sign-extended from the operand's width, as gcc
/// checks its canonical constant.
fn out_of_range(func: &Function, c: &AsmConstraint, found: &Immediate) -> Result<(), String> {
    let kind = |p: PseudoId| func.get_pseudo(p).map(|p| &p.kind);
    let imm = c.class.imm;
    let int = match found {
        Immediate::Int(p) => match kind(*p) {
            Some(PseudoKind::Val(v)) => canonical_int(*v, c.size),
            _ => return Ok(()),
        },
        Immediate::Folded(v) => canonical_int((*v).into(), c.size),
        Immediate::Float(p) => {
            return match kind(*p) {
                Some(PseudoKind::FVal(v)) if !imm.float.admits(*v) => {
                    Err("the floating constant".to_string())
                }
                _ => Ok(()),
            }
        }
        Immediate::Symbol(..) => return Ok(()),
    };
    if imm.int.admits(int) {
        Ok(())
    } else {
        Err(int.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{AsmConstraint, AsmData, BasicBlock, BasicBlockId, Pseudo};
    use crate::target::Arch;
    use crate::types::TypeTable;

    /// `entry; <defs>; asm "" :: <constraint>(%operand); ret` over x86-64.
    fn func_with_asm(defs: Vec<Instruction>, constraint: &str, operand: PseudoId) -> Function {
        let types = TypeTable::new(&crate::target::Target::host());
        let mut func = Function::new("f", types.int_id);
        let mut entry = BasicBlock::new(BasicBlockId(0));
        entry.add_insn(Instruction::new(Opcode::Entry));
        for d in defs {
            entry.add_insn(d);
        }
        entry.add_insn(Instruction::asm(AsmData {
            template: String::new(),
            outputs: Vec::new(),
            inputs: vec![AsmConstraint::new(operand, constraint, Arch::X86_64, 64)],
            clobbers: Vec::new(),
            goto_labels: Vec::new(),
        }));
        entry.add_insn(Instruction::new(Opcode::Ret));
        func.add_block(entry);
        func
    }

    fn operand(func: &Function) -> (PseudoId, i64) {
        let asm = func.blocks[0]
            .insns
            .iter()
            .find_map(|i| i.extra().asm_data.as_deref())
            .unwrap();
        (asm.inputs[0].pseudo, asm.inputs[0].offset)
    }

    fn copy(target: u32, src: u32) -> Instruction {
        Instruction::new(Opcode::Copy)
            .with_target(PseudoId(target))
            .with_src(PseudoId(src))
            .with_size(32)
    }

    /// A constant reached through copies -- an inlined parameter -- is
    /// named directly.
    #[test]
    fn integer_through_copies() {
        let mut func = func_with_asm(vec![copy(1, 0), copy(2, 1)], "i", PseudoId(2));
        func.add_pseudo(Pseudo::val(PseudoId(0), 3));
        func.next_pseudo = 3;
        assert!(resolve_immediates(&mut func, &HashSet::new()));
        assert_eq!(operand(&func), (PseudoId(0), 0));
        // Nothing left to do.
        assert!(!resolve_immediates(&mut func, &HashSet::new()));
    }

    /// `&arr[1]` is the global's `Sym` with the offset on the operand.
    #[test]
    fn symbol_plus_offset() {
        let symaddr = Instruction::new(Opcode::SymAddr)
            .with_target(PseudoId(1))
            .with_src(PseudoId(0))
            .with_size(64);
        let add = Instruction::new(Opcode::Add)
            .with_target(PseudoId(3))
            .with_src2(PseudoId(1), PseudoId(2))
            .with_size(64);
        let mut func = func_with_asm(vec![symaddr, add], "i", PseudoId(3));
        func.add_pseudo(Pseudo::sym(PseudoId(0), "arr".to_string()));
        func.add_pseudo(Pseudo::val(PseudoId(2), 4));
        func.next_pseudo = 4;
        assert!(resolve_immediates(&mut func, &HashSet::new()));
        assert_eq!(operand(&func), (PseudoId(0), 4));
    }

    /// A constant of the right kind that no range the letter takes holds is
    /// out of range: `"I"(32)` on x86-64. The value is checked sign-extended
    /// from the operand's width, as gcc checks it.
    #[test]
    fn out_of_range_constants() {
        let check = |constraint: &str, value: i128, size: u32| {
            let types = TypeTable::new(&crate::target::Target::host());
            let mut func = Function::new("f", types.int_id);
            func.add_pseudo(Pseudo::val(PseudoId(0), value));
            let c = AsmConstraint::new(PseudoId(0), constraint, Arch::X86_64, size);
            out_of_range(&func, &c, &Immediate::Int(PseudoId(0)))
        };
        assert_eq!(check("I", 31, 32), Ok(()));
        assert_eq!(check("I", 32, 32), Err("32".to_string()));
        assert_eq!(check("IK", -128, 32), Ok(()));
        // 200 as an `unsigned char` is -56.
        assert_eq!(check("N", 200, 8), Err("-56".to_string()));
        assert_eq!(check("N", 200, 32), Ok(()));
        assert_eq!(check("e", 0xffff_ffff, 32), Ok(()));
        assert_eq!(check("e", 0xffff_ffff, 64), Err("4294967295".to_string()));
    }
}
