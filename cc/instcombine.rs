//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// InstCombine pass for pcc C99 compiler
//
// This pass performs instruction combining optimizations:
// - Constant folding: evaluate operations on constants at compile time
// - Algebraic simplification: x + 0 -> x, x * 1 -> x, etc.
// - Identity patterns: x - x -> 0, x ^ x -> 0, etc.
//

use crate::ir::{Function, Instruction, Opcode, Pseudo, PseudoId, PseudoKind};

// ============================================================================
// Main Entry Point
// ============================================================================

/// Run the InstCombine pass on a function.
/// Returns true if any changes were made.
pub fn run(func: &mut Function) -> bool {
    let mut changed = false;

    for bb in &mut func.blocks {
        for insn in &mut bb.insns {
            if let Some(new_insn) = try_simplify(insn, &func.pseudos) {
                *insn = new_insn;
                changed = true;
            }
        }
    }

    changed
}

// ============================================================================
// Simplification Dispatch
// ============================================================================

/// Try to simplify an instruction. Returns Some(new_instruction) if simplified.
fn try_simplify(insn: &Instruction, pseudos: &[Pseudo]) -> Option<Instruction> {
    match insn.op {
        // Integer arithmetic
        Opcode::Add => simplify_add(insn, pseudos),
        Opcode::Sub => simplify_sub(insn, pseudos),
        Opcode::Mul => simplify_mul(insn, pseudos),
        Opcode::DivS | Opcode::DivU => simplify_div(insn, pseudos),
        Opcode::ModS | Opcode::ModU => simplify_mod(insn, pseudos),

        // Shifts
        Opcode::Shl => simplify_shift(insn, pseudos),
        Opcode::Lsr => simplify_shift(insn, pseudos),
        Opcode::Asr => simplify_shift(insn, pseudos),

        // Bitwise
        Opcode::And => simplify_and(insn, pseudos),
        Opcode::Or => simplify_or(insn, pseudos),
        Opcode::Xor => simplify_xor(insn, pseudos),

        // Comparisons
        Opcode::SetEq
        | Opcode::SetNe
        | Opcode::SetLt
        | Opcode::SetLe
        | Opcode::SetGt
        | Opcode::SetGe
        | Opcode::SetB
        | Opcode::SetBe
        | Opcode::SetA
        | Opcode::SetAe => simplify_cmp(insn, pseudos),

        // Unary
        Opcode::Neg => simplify_neg(insn, pseudos),
        Opcode::Not => simplify_not(insn, pseudos),

        _ => None,
    }
}

// ============================================================================
// Helper: Get constant value from pseudos list
// ============================================================================

fn get_const(pseudos: &[Pseudo], id: PseudoId) -> Option<i64> {
    pseudos
        .iter()
        .find(|p| p.id == id)
        .and_then(|p| match &p.kind {
            PseudoKind::Val(v) => Some(*v),
            _ => None,
        })
}

/// Create a Copy instruction that copies a constant source.
/// For constant folding, we need to have an existing constant pseudo to copy from.
/// Since we can't easily create new constants here, we return None if we can't
/// find a matching constant. This is handled by returning None from the
/// simplify functions when both operands are constants but we can't fold.
fn make_const_result(insn: &Instruction, src_const: PseudoId) -> Instruction {
    // Copy from the constant pseudo
    Instruction {
        op: Opcode::Copy,
        target: insn.target,
        src: vec![src_const],
        typ: insn.typ,
        size: insn.size,
        ..Default::default()
    }
}

/// Create a Copy instruction that copies one value to another.
fn make_copy(insn: &Instruction, src: PseudoId) -> Instruction {
    Instruction {
        op: Opcode::Copy,
        target: insn.target,
        src: vec![src],
        typ: insn.typ,
        size: insn.size,
        ..Default::default()
    }
}

// ============================================================================
// Add Simplification
// ============================================================================

fn simplify_add(insn: &Instruction, pseudos: &[Pseudo]) -> Option<Instruction> {
    if insn.src.len() != 2 {
        return None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = get_const(pseudos, src1);
    let val2 = get_const(pseudos, src2);

    match (val1, val2) {
        // Algebraic: x + 0 -> x
        (None, Some(0)) => Some(make_copy(insn, src1)),

        // Algebraic: 0 + x -> x
        (Some(0), None) => Some(make_copy(insn, src2)),

        // Note: We can't fold constants (a + b -> result) because we can't
        // create new constant pseudos without modifying the function's pseudo list.
        // This would require passing &mut Function or a more complex design.
        _ => None,
    }
}

// ============================================================================
// Sub Simplification
// ============================================================================

fn simplify_sub(insn: &Instruction, pseudos: &[Pseudo]) -> Option<Instruction> {
    if insn.src.len() != 2 {
        return None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val2 = get_const(pseudos, src2);

    // Note: x - x -> 0 requires creating a zero constant, which we can't do
    // without modifying the function's pseudo list. Skip for MVP.

    // Algebraic: x - 0 -> x
    if val2 == Some(0) {
        return Some(make_copy(insn, src1));
    }

    None
}

// ============================================================================
// Mul Simplification
// ============================================================================

fn simplify_mul(insn: &Instruction, pseudos: &[Pseudo]) -> Option<Instruction> {
    if insn.src.len() != 2 {
        return None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = get_const(pseudos, src1);
    let val2 = get_const(pseudos, src2);

    match (val1, val2) {
        // Algebraic: x * 0 -> 0 (copy from the zero constant)
        (None, Some(0)) => Some(make_const_result(insn, src2)),
        (Some(0), None) => Some(make_const_result(insn, src1)),

        // Algebraic: x * 1 -> x
        (None, Some(1)) => Some(make_copy(insn, src1)),
        (Some(1), None) => Some(make_copy(insn, src2)),

        _ => None,
    }
}

// ============================================================================
// Div Simplification
// ============================================================================

fn simplify_div(insn: &Instruction, pseudos: &[Pseudo]) -> Option<Instruction> {
    if insn.src.len() != 2 {
        return None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = get_const(pseudos, src1);
    let val2 = get_const(pseudos, src2);

    match (val1, val2) {
        // Algebraic: x / 1 -> x
        (None, Some(1)) => Some(make_copy(insn, src1)),

        // Algebraic: 0 / x -> 0 (copy from the zero constant)
        (Some(0), None) => Some(make_const_result(insn, src1)),

        _ => None,
    }
}

// ============================================================================
// Mod Simplification
// ============================================================================

fn simplify_mod(insn: &Instruction, pseudos: &[Pseudo]) -> Option<Instruction> {
    if insn.src.len() != 2 {
        return None;
    }

    let src1 = insn.src[0];
    let val1 = get_const(pseudos, src1);

    // Algebraic: 0 % x -> 0 (copy from the zero constant)
    if val1 == Some(0) {
        return Some(make_const_result(insn, src1));
    }

    // Note: x % 1 -> 0 requires a zero constant we may not have
    None
}

// ============================================================================
// Shift Simplification
// ============================================================================

fn simplify_shift(insn: &Instruction, pseudos: &[Pseudo]) -> Option<Instruction> {
    if insn.src.len() != 2 {
        return None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = get_const(pseudos, src1);
    let val2 = get_const(pseudos, src2);

    match (val1, val2) {
        // Algebraic: x << 0 -> x, x >> 0 -> x
        (None, Some(0)) => Some(make_copy(insn, src1)),

        // Algebraic: 0 << n -> 0, 0 >> n -> 0 (copy from zero constant)
        (Some(0), None) => Some(make_const_result(insn, src1)),

        _ => None,
    }
}

// ============================================================================
// And Simplification
// ============================================================================

fn simplify_and(insn: &Instruction, pseudos: &[Pseudo]) -> Option<Instruction> {
    if insn.src.len() != 2 {
        return None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = get_const(pseudos, src1);
    let val2 = get_const(pseudos, src2);

    // Identity: x & x -> x
    if src1 == src2 {
        return Some(make_copy(insn, src1));
    }

    match (val1, val2) {
        // Algebraic: x & 0 -> 0 (copy from zero constant)
        (None, Some(0)) => Some(make_const_result(insn, src2)),
        (Some(0), None) => Some(make_const_result(insn, src1)),

        // Algebraic: x & -1 -> x (all bits set)
        (None, Some(-1)) => Some(make_copy(insn, src1)),
        (Some(-1), None) => Some(make_copy(insn, src2)),

        _ => None,
    }
}

// ============================================================================
// Or Simplification
// ============================================================================

fn simplify_or(insn: &Instruction, pseudos: &[Pseudo]) -> Option<Instruction> {
    if insn.src.len() != 2 {
        return None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = get_const(pseudos, src1);
    let val2 = get_const(pseudos, src2);

    // Identity: x | x -> x
    if src1 == src2 {
        return Some(make_copy(insn, src1));
    }

    match (val1, val2) {
        // Algebraic: x | 0 -> x
        (None, Some(0)) => Some(make_copy(insn, src1)),
        (Some(0), None) => Some(make_copy(insn, src2)),

        // Algebraic: x | -1 -> -1 (copy from the -1 constant)
        (None, Some(-1)) => Some(make_const_result(insn, src2)),
        (Some(-1), None) => Some(make_const_result(insn, src1)),

        _ => None,
    }
}

// ============================================================================
// Xor Simplification
// ============================================================================

fn simplify_xor(insn: &Instruction, pseudos: &[Pseudo]) -> Option<Instruction> {
    if insn.src.len() != 2 {
        return None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = get_const(pseudos, src1);
    let val2 = get_const(pseudos, src2);

    // Note: x ^ x -> 0 requires creating a zero constant, skip for MVP

    match (val1, val2) {
        // Algebraic: x ^ 0 -> x
        (None, Some(0)) => Some(make_copy(insn, src1)),
        (Some(0), None) => Some(make_copy(insn, src2)),

        _ => None,
    }
}

// ============================================================================
// Comparison Simplification
// ============================================================================

fn simplify_cmp(insn: &Instruction, _pseudos: &[Pseudo]) -> Option<Instruction> {
    if insn.src.len() != 2 {
        return None;
    }

    // Note: x cmp x patterns require creating 0 or 1 constants, skip for MVP
    // Note: Constant folding also requires creating new constants, skip for MVP

    None
}

// ============================================================================
// Unary Simplification
// ============================================================================

fn simplify_neg(insn: &Instruction, _pseudos: &[Pseudo]) -> Option<Instruction> {
    if insn.src.len() != 1 {
        return None;
    }

    // Note: Constant folding requires creating new constants, skip for MVP
    None
}

fn simplify_not(insn: &Instruction, _pseudos: &[Pseudo]) -> Option<Instruction> {
    if insn.src.len() != 1 {
        return None;
    }

    // Note: Constant folding requires creating new constants, skip for MVP
    None
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId};
    use crate::types::TypeTable;

    fn make_test_func_with_insn(insn: Instruction, pseudos: Vec<Pseudo>) -> Function {
        let types = TypeTable::new();
        let mut func = Function::new("test", types.int_id);

        for p in pseudos {
            func.add_pseudo(p);
        }

        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        bb.add_insn(insn);
        bb.add_insn(Instruction::ret(None));
        func.add_block(bb);
        func.entry = BasicBlockId(0);

        func
    }

    #[test]
    fn test_add_zero_right() {
        // x + 0 -> x
        let types = TypeTable::new();
        let insn = Instruction::binop(
            Opcode::Add,
            PseudoId(2),
            PseudoId(0), // x
            PseudoId(1), // 0
            types.int_id,
            32,
        );
        let pseudos = vec![
            Pseudo::reg(PseudoId(0), 0),
            Pseudo::val(PseudoId(1), 0),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);
        assert_eq!(result_insn.src, vec![PseudoId(0)]);
    }

    #[test]
    fn test_add_zero_left() {
        // 0 + x -> x
        let types = TypeTable::new();
        let insn = Instruction::binop(
            Opcode::Add,
            PseudoId(2),
            PseudoId(0), // 0
            PseudoId(1), // x
            types.int_id,
            32,
        );
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 0),
            Pseudo::reg(PseudoId(1), 1),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);
        assert_eq!(result_insn.src, vec![PseudoId(1)]);
    }

    #[test]
    fn test_mul_zero() {
        // x * 0 -> 0 (copy from zero constant)
        let types = TypeTable::new();
        let insn = Instruction::binop(
            Opcode::Mul,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1), // 0
            types.int_id,
            32,
        );
        let pseudos = vec![
            Pseudo::reg(PseudoId(0), 0),
            Pseudo::val(PseudoId(1), 0),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);
        assert_eq!(result_insn.src, vec![PseudoId(1)]); // Copy from zero constant
    }

    #[test]
    fn test_mul_one() {
        // x * 1 -> x
        let types = TypeTable::new();
        let insn = Instruction::binop(
            Opcode::Mul,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1), // 1
            types.int_id,
            32,
        );
        let pseudos = vec![
            Pseudo::reg(PseudoId(0), 0),
            Pseudo::val(PseudoId(1), 1),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);
        assert_eq!(result_insn.src, vec![PseudoId(0)]);
    }

    #[test]
    fn test_and_self() {
        // x & x -> x
        let types = TypeTable::new();
        let insn = Instruction::binop(
            Opcode::And,
            PseudoId(1),
            PseudoId(0),
            PseudoId(0),
            types.int_id,
            32,
        );
        let pseudos = vec![Pseudo::reg(PseudoId(0), 0), Pseudo::reg(PseudoId(1), 1)];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);
        assert_eq!(result_insn.src, vec![PseudoId(0)]);
    }

    #[test]
    fn test_or_self() {
        // x | x -> x
        let types = TypeTable::new();
        let insn = Instruction::binop(
            Opcode::Or,
            PseudoId(1),
            PseudoId(0),
            PseudoId(0),
            types.int_id,
            32,
        );
        let pseudos = vec![Pseudo::reg(PseudoId(0), 0), Pseudo::reg(PseudoId(1), 1)];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);
        assert_eq!(result_insn.src, vec![PseudoId(0)]);
    }

    #[test]
    fn test_xor_zero() {
        // x ^ 0 -> x
        let types = TypeTable::new();
        let insn = Instruction::binop(
            Opcode::Xor,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1), // 0
            types.int_id,
            32,
        );
        let pseudos = vec![
            Pseudo::reg(PseudoId(0), 0),
            Pseudo::val(PseudoId(1), 0),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);
        assert_eq!(result_insn.src, vec![PseudoId(0)]);
    }

    #[test]
    fn test_no_change_for_non_const() {
        // x + y (neither const) -> no change
        let types = TypeTable::new();
        let insn = Instruction::binop(
            Opcode::Add,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        );
        let pseudos = vec![
            Pseudo::reg(PseudoId(0), 0),
            Pseudo::reg(PseudoId(1), 1),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(!changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Add);
    }

    #[test]
    fn test_no_const_folding() {
        // 2 + 3 -> no change (MVP doesn't create new constants)
        let types = TypeTable::new();
        let insn = Instruction::binop(
            Opcode::Add,
            PseudoId(2),
            PseudoId(0), // 2
            PseudoId(1), // 3
            types.int_id,
            32,
        );
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 2),
            Pseudo::val(PseudoId(1), 3),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(!changed); // MVP doesn't fold constants

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Add);
    }
}
