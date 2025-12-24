//
// Copyright (c) 2025-2026 Jeff Garzik
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

use super::{Function, Instruction, Opcode, Pseudo, PseudoId, PseudoKind};
use crate::types::TypeId;

// ============================================================================
// Simplification Result
// ============================================================================

/// Result of trying to simplify an instruction
enum Simplification {
    /// No simplification possible
    None,
    /// Copy from an existing pseudo (algebraic identity)
    CopyFrom(PseudoId),
    /// Create a new constant with this value and copy from it
    FoldToConst(i64),
}

// ============================================================================
// Main Entry Point
// ============================================================================

/// Run the InstCombine pass on a function.
/// Returns true if any changes were made.
pub fn run(func: &mut Function) -> bool {
    let mut changed = false;

    // Collect all simplifications first (to avoid borrow conflicts)
    let mut simplifications: Vec<(usize, usize, Simplification)> = Vec::new();

    for (bb_idx, bb) in func.blocks.iter().enumerate() {
        for (insn_idx, insn) in bb.insns.iter().enumerate() {
            let result = try_simplify(insn, &func.pseudos);
            if !matches!(result, Simplification::None) {
                simplifications.push((bb_idx, insn_idx, result));
            }
        }
    }

    // Apply simplifications
    for (bb_idx, insn_idx, simplification) in simplifications {
        // Extract necessary data from the instruction before any mutation
        let (target, typ, size) = {
            let insn = &func.blocks[bb_idx].insns[insn_idx];
            (insn.target, insn.typ, insn.size)
        };

        let new_insn = match simplification {
            Simplification::None => continue,
            Simplification::CopyFrom(src) => make_copy_from_parts(target, typ, size, src),
            Simplification::FoldToConst(value) => {
                // Create a new constant pseudo
                let const_id = func.create_const_pseudo(value);
                make_copy_from_parts(target, typ, size, const_id)
            }
        };
        func.blocks[bb_idx].insns[insn_idx] = new_insn;
        changed = true;
    }

    changed
}

// ============================================================================
// Simplification Dispatch
// ============================================================================

/// Try to simplify an instruction. Returns the simplification to apply.
fn try_simplify(insn: &Instruction, pseudos: &[Pseudo]) -> Simplification {
    match insn.op {
        // Integer arithmetic
        Opcode::Add => simplify_add(insn, pseudos),
        Opcode::Sub => simplify_sub(insn, pseudos),
        Opcode::Mul => simplify_mul(insn, pseudos),
        Opcode::DivS | Opcode::DivU => simplify_div(insn, pseudos),
        Opcode::ModS | Opcode::ModU => simplify_mod(insn, pseudos),

        // Shifts
        Opcode::Shl => simplify_shl(insn, pseudos),
        Opcode::Lsr => simplify_lsr(insn, pseudos),
        Opcode::Asr => simplify_asr(insn, pseudos),

        // Bitwise (all handled by unified simplify_bitwise)
        Opcode::And | Opcode::Or | Opcode::Xor => simplify_bitwise(insn, pseudos),

        // Comparisons (all handled by unified simplify_comparison)
        Opcode::SetEq
        | Opcode::SetNe
        | Opcode::SetLt
        | Opcode::SetLe
        | Opcode::SetGt
        | Opcode::SetGe
        | Opcode::SetB
        | Opcode::SetBe
        | Opcode::SetA
        | Opcode::SetAe => simplify_comparison(insn, pseudos),

        // Unary
        Opcode::Neg => simplify_neg(insn, pseudos),
        Opcode::Not => simplify_not(insn, pseudos),

        _ => Simplification::None,
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

/// Create a Copy instruction from extracted parts.
fn make_copy_from_parts(
    target: Option<PseudoId>,
    typ: Option<TypeId>,
    size: u32,
    src: PseudoId,
) -> Instruction {
    Instruction {
        op: Opcode::Copy,
        target,
        src: vec![src],
        typ,
        size,
        ..Default::default()
    }
}

// ============================================================================
// Add Simplification
// ============================================================================

fn simplify_add(insn: &Instruction, pseudos: &[Pseudo]) -> Simplification {
    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = get_const(pseudos, src1);
    let val2 = get_const(pseudos, src2);

    match (val1, val2) {
        // Constant folding: a + b -> (a + b)
        (Some(a), Some(b)) => Simplification::FoldToConst(a.wrapping_add(b)),

        // Algebraic: x + 0 -> x
        (None, Some(0)) => Simplification::CopyFrom(src1),

        // Algebraic: 0 + x -> x
        (Some(0), None) => Simplification::CopyFrom(src2),

        _ => Simplification::None,
    }
}

// ============================================================================
// Sub Simplification
// ============================================================================

fn simplify_sub(insn: &Instruction, pseudos: &[Pseudo]) -> Simplification {
    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];

    // Identity: x - x -> 0
    if src1 == src2 {
        return Simplification::FoldToConst(0);
    }

    let val1 = get_const(pseudos, src1);
    let val2 = get_const(pseudos, src2);

    match (val1, val2) {
        // Constant folding: a - b -> (a - b)
        (Some(a), Some(b)) => Simplification::FoldToConst(a.wrapping_sub(b)),

        // Algebraic: x - 0 -> x
        (None, Some(0)) => Simplification::CopyFrom(src1),

        _ => Simplification::None,
    }
}

// ============================================================================
// Mul Simplification
// ============================================================================

fn simplify_mul(insn: &Instruction, pseudos: &[Pseudo]) -> Simplification {
    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = get_const(pseudos, src1);
    let val2 = get_const(pseudos, src2);

    match (val1, val2) {
        // Constant folding: a * b -> (a * b)
        (Some(a), Some(b)) => Simplification::FoldToConst(a.wrapping_mul(b)),

        // Algebraic: x * 0 -> 0
        (None, Some(0)) => Simplification::FoldToConst(0),
        (Some(0), None) => Simplification::FoldToConst(0),

        // Algebraic: x * 1 -> x
        (None, Some(1)) => Simplification::CopyFrom(src1),
        (Some(1), None) => Simplification::CopyFrom(src2),

        _ => Simplification::None,
    }
}

// ============================================================================
// Div Simplification
// ============================================================================

fn simplify_div(insn: &Instruction, pseudos: &[Pseudo]) -> Simplification {
    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = get_const(pseudos, src1);
    let val2 = get_const(pseudos, src2);

    match (val1, val2) {
        // Constant folding: a / b -> (a / b) (avoid div by zero)
        (Some(a), Some(b)) if b != 0 => {
            if insn.op == Opcode::DivS {
                Simplification::FoldToConst(a.wrapping_div(b))
            } else {
                Simplification::FoldToConst((a as u64).wrapping_div(b as u64) as i64)
            }
        }

        // Algebraic: x / 1 -> x
        (None, Some(1)) => Simplification::CopyFrom(src1),

        // Algebraic: 0 / x -> 0
        (Some(0), None) => Simplification::FoldToConst(0),

        _ => Simplification::None,
    }
}

// ============================================================================
// Mod Simplification
// ============================================================================

fn simplify_mod(insn: &Instruction, pseudos: &[Pseudo]) -> Simplification {
    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = get_const(pseudos, src1);
    let val2 = get_const(pseudos, src2);

    match (val1, val2) {
        // Constant folding: a % b -> (a % b) (avoid mod by zero)
        (Some(a), Some(b)) if b != 0 => {
            if insn.op == Opcode::ModS {
                Simplification::FoldToConst(a.wrapping_rem(b))
            } else {
                Simplification::FoldToConst((a as u64).wrapping_rem(b as u64) as i64)
            }
        }

        // Algebraic: 0 % x -> 0
        (Some(0), None) => Simplification::FoldToConst(0),

        // Algebraic: x % 1 -> 0
        (None, Some(1)) => Simplification::FoldToConst(0),

        _ => Simplification::None,
    }
}

// ============================================================================
// Shift Simplifications
// ============================================================================

fn simplify_shl(insn: &Instruction, pseudos: &[Pseudo]) -> Simplification {
    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = get_const(pseudos, src1);
    let val2 = get_const(pseudos, src2);

    match (val1, val2) {
        // Constant folding: a << b
        (Some(a), Some(b)) if (0..64).contains(&b) => {
            Simplification::FoldToConst(a.wrapping_shl(b as u32))
        }

        // Algebraic: x << 0 -> x
        (None, Some(0)) => Simplification::CopyFrom(src1),

        // Algebraic: 0 << n -> 0
        (Some(0), None) => Simplification::FoldToConst(0),

        _ => Simplification::None,
    }
}

fn simplify_lsr(insn: &Instruction, pseudos: &[Pseudo]) -> Simplification {
    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = get_const(pseudos, src1);
    let val2 = get_const(pseudos, src2);

    match (val1, val2) {
        // Constant folding: a >> b (logical/unsigned)
        (Some(a), Some(b)) if (0..64).contains(&b) => {
            Simplification::FoldToConst((a as u64).wrapping_shr(b as u32) as i64)
        }

        // Algebraic: x >> 0 -> x
        (None, Some(0)) => Simplification::CopyFrom(src1),

        // Algebraic: 0 >> n -> 0
        (Some(0), None) => Simplification::FoldToConst(0),

        _ => Simplification::None,
    }
}

fn simplify_asr(insn: &Instruction, pseudos: &[Pseudo]) -> Simplification {
    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = get_const(pseudos, src1);
    let val2 = get_const(pseudos, src2);

    match (val1, val2) {
        // Constant folding: a >> b (arithmetic/signed)
        (Some(a), Some(b)) if (0..64).contains(&b) => {
            Simplification::FoldToConst(a.wrapping_shr(b as u32))
        }

        // Algebraic: x >> 0 -> x
        (None, Some(0)) => Simplification::CopyFrom(src1),

        // Algebraic: 0 >> n -> 0
        (Some(0), None) => Simplification::FoldToConst(0),

        _ => Simplification::None,
    }
}

// ============================================================================
// And Simplification
// ============================================================================

/// Result when applying x op x
enum SelfOpResult {
    /// x op x copies src (e.g., x & x = x, x | x = x)
    CopySrc,
    /// x op x folds to a constant (e.g., x ^ x = 0)
    Const(i64),
}

/// Bitwise operation behavior for simplification
struct BitwiseInfo {
    /// Result when x op x
    self_result: SelfOpResult,
    /// Identity element: x op identity = x (e.g., x & -1 = x, x | 0 = x, x ^ 0 = x)
    identity: i64,
    /// Absorbing element: x op absorbing = absorbing (e.g., x & 0 = 0, x | -1 = -1)
    /// None for XOR (no absorbing element)
    absorbing: Option<i64>,
    /// Constant folding function
    fold: fn(i64, i64) -> i64,
}

/// Get bitwise operation info for the given opcode
fn get_bitwise_info(op: Opcode) -> Option<BitwiseInfo> {
    match op {
        Opcode::And => Some(BitwiseInfo {
            self_result: SelfOpResult::CopySrc,
            identity: -1,       // x & -1 = x (all bits set)
            absorbing: Some(0), // x & 0 = 0
            fold: |a, b| a & b,
        }),
        Opcode::Or => Some(BitwiseInfo {
            self_result: SelfOpResult::CopySrc,
            identity: 0,         // x | 0 = x
            absorbing: Some(-1), // x | -1 = -1
            fold: |a, b| a | b,
        }),
        Opcode::Xor => Some(BitwiseInfo {
            self_result: SelfOpResult::Const(0), // x ^ x = 0
            identity: 0,                         // x ^ 0 = x
            absorbing: None,                     // no absorbing element
            fold: |a, b| a ^ b,
        }),
        _ => None,
    }
}

/// Unified bitwise simplification for And/Or/Xor
fn simplify_bitwise(insn: &Instruction, pseudos: &[Pseudo]) -> Simplification {
    let info = match get_bitwise_info(insn.op) {
        Some(i) => i,
        None => return Simplification::None,
    };

    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];

    // x op x -> self_result
    if src1 == src2 {
        return match info.self_result {
            SelfOpResult::CopySrc => Simplification::CopyFrom(src1),
            SelfOpResult::Const(c) => Simplification::FoldToConst(c),
        };
    }

    let val1 = get_const(pseudos, src1);
    let val2 = get_const(pseudos, src2);

    match (val1, val2) {
        // Constant folding: a op b
        (Some(a), Some(b)) => Simplification::FoldToConst((info.fold)(a, b)),

        // Algebraic: x op identity -> x
        (None, Some(v)) if v == info.identity => Simplification::CopyFrom(src1),
        (Some(v), None) if v == info.identity => Simplification::CopyFrom(src2),

        // Algebraic: x op absorbing -> absorbing (if exists)
        (None, Some(v)) if info.absorbing == Some(v) => Simplification::FoldToConst(v),
        (Some(v), None) if info.absorbing == Some(v) => Simplification::FoldToConst(v),

        _ => Simplification::None,
    }
}

// ============================================================================
// Comparison Simplifications
// ============================================================================

/// Comparison behavior for identity (x op x) and constant folding
struct CmpInfo {
    /// Result when comparing x to itself (e.g., x == x -> 1, x < x -> 0)
    identity_result: i64,
    /// Constant comparison function
    compare: fn(i64, i64) -> bool,
}

/// Get comparison info for the given opcode
fn get_cmp_info(op: Opcode) -> Option<CmpInfo> {
    match op {
        Opcode::SetEq => Some(CmpInfo {
            identity_result: 1,
            compare: |a, b| a == b,
        }),
        Opcode::SetNe => Some(CmpInfo {
            identity_result: 0,
            compare: |a, b| a != b,
        }),
        Opcode::SetLt => Some(CmpInfo {
            identity_result: 0,
            compare: |a, b| a < b,
        }),
        Opcode::SetLe => Some(CmpInfo {
            identity_result: 1,
            compare: |a, b| a <= b,
        }),
        Opcode::SetGt => Some(CmpInfo {
            identity_result: 0,
            compare: |a, b| a > b,
        }),
        Opcode::SetGe => Some(CmpInfo {
            identity_result: 1,
            compare: |a, b| a >= b,
        }),
        Opcode::SetB => Some(CmpInfo {
            identity_result: 0,
            compare: |a, b| (a as u64) < (b as u64),
        }),
        Opcode::SetBe => Some(CmpInfo {
            identity_result: 1,
            compare: |a, b| (a as u64) <= (b as u64),
        }),
        Opcode::SetA => Some(CmpInfo {
            identity_result: 0,
            compare: |a, b| (a as u64) > (b as u64),
        }),
        Opcode::SetAe => Some(CmpInfo {
            identity_result: 1,
            compare: |a, b| (a as u64) >= (b as u64),
        }),
        _ => None,
    }
}

/// Unified comparison simplification for all SetXX opcodes
fn simplify_comparison(insn: &Instruction, pseudos: &[Pseudo]) -> Simplification {
    let info = match get_cmp_info(insn.op) {
        Some(i) => i,
        None => return Simplification::None,
    };

    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];

    // Identity: x op x -> identity_result
    if src1 == src2 {
        return Simplification::FoldToConst(info.identity_result);
    }

    // Constant folding
    let val1 = get_const(pseudos, src1);
    let val2 = get_const(pseudos, src2);

    if let (Some(a), Some(b)) = (val1, val2) {
        Simplification::FoldToConst(if (info.compare)(a, b) { 1 } else { 0 })
    } else {
        Simplification::None
    }
}

// ============================================================================
// Unary Simplifications
// ============================================================================

fn simplify_neg(insn: &Instruction, pseudos: &[Pseudo]) -> Simplification {
    if insn.src.len() != 1 {
        return Simplification::None;
    }

    let src = insn.src[0];
    if let Some(val) = get_const(pseudos, src) {
        Simplification::FoldToConst(val.wrapping_neg())
    } else {
        Simplification::None
    }
}

fn simplify_not(insn: &Instruction, pseudos: &[Pseudo]) -> Simplification {
    if insn.src.len() != 1 {
        return Simplification::None;
    }

    let src = insn.src[0];
    if let Some(val) = get_const(pseudos, src) {
        Simplification::FoldToConst(!val)
    } else {
        Simplification::None
    }
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
        let types = TypeTable::new(64);
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

    // ========================================================================
    // Algebraic Simplification Tests
    // ========================================================================

    #[test]
    fn test_add_zero_right() {
        // x + 0 -> x
        let types = TypeTable::new(64);
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
        let types = TypeTable::new(64);
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
    fn test_mul_one() {
        // x * 1 -> x
        let types = TypeTable::new(64);
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
        let types = TypeTable::new(64);
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
        let types = TypeTable::new(64);
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
        let types = TypeTable::new(64);
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
    fn test_xor_self() {
        // x ^ x -> 0
        let types = TypeTable::new(64);
        let insn = Instruction::binop(
            Opcode::Xor,
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
        // The new constant (0) was created and copied from
        assert!(func.pseudos.len() > 2);
    }

    #[test]
    fn test_no_change_for_non_const() {
        // x + y (neither const) -> no change
        let types = TypeTable::new(64);
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

    // ========================================================================
    // Constant Folding Tests
    // ========================================================================

    #[test]
    fn test_const_fold_add() {
        // 2 + 3 -> 5
        let types = TypeTable::new(64);
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
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);

        // Find the new constant pseudo that was created
        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(5));
    }

    #[test]
    fn test_const_fold_sub() {
        // 10 - 3 -> 7
        let types = TypeTable::new(64);
        let insn = Instruction::binop(
            Opcode::Sub,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        );
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 10),
            Pseudo::val(PseudoId(1), 3),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);

        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(7));
    }

    #[test]
    fn test_sub_self() {
        // x - x -> 0
        let types = TypeTable::new(64);
        let insn = Instruction::binop(
            Opcode::Sub,
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

        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(0));
    }

    #[test]
    fn test_const_fold_mul() {
        // 6 * 7 -> 42
        let types = TypeTable::new(64);
        let insn = Instruction::binop(
            Opcode::Mul,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        );
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 6),
            Pseudo::val(PseudoId(1), 7),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);

        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(42));
    }

    #[test]
    fn test_const_fold_mul_zero() {
        // x * 0 -> 0
        let types = TypeTable::new(64);
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

        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(0));
    }

    #[test]
    fn test_const_fold_div() {
        // 10 / 2 -> 5
        let types = TypeTable::new(64);
        let insn = Instruction::binop(
            Opcode::DivS,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        );
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 10),
            Pseudo::val(PseudoId(1), 2),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);

        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(5));
    }

    #[test]
    fn test_const_fold_mod() {
        // 10 % 3 -> 1
        let types = TypeTable::new(64);
        let insn = Instruction::binop(
            Opcode::ModS,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        );
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 10),
            Pseudo::val(PseudoId(1), 3),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);

        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(1));
    }

    #[test]
    fn test_const_fold_shift() {
        // 1 << 4 -> 16
        let types = TypeTable::new(64);
        let insn = Instruction::binop(
            Opcode::Shl,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        );
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 1),
            Pseudo::val(PseudoId(1), 4),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);

        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(16));
    }

    #[test]
    fn test_const_fold_and() {
        // 0xFF & 0x0F -> 0x0F
        let types = TypeTable::new(64);
        let insn = Instruction::binop(
            Opcode::And,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        );
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 0xFF),
            Pseudo::val(PseudoId(1), 0x0F),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);

        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(0x0F));
    }

    // ========================================================================
    // Comparison Folding Tests
    // ========================================================================

    #[test]
    fn test_const_fold_seteq_true() {
        // 5 == 5 -> 1
        let types = TypeTable::new(64);
        let insn = Instruction::binop(
            Opcode::SetEq,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        );
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 5),
            Pseudo::val(PseudoId(1), 5),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);

        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(1));
    }

    #[test]
    fn test_const_fold_seteq_false() {
        // 5 == 3 -> 0
        let types = TypeTable::new(64);
        let insn = Instruction::binop(
            Opcode::SetEq,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        );
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 5),
            Pseudo::val(PseudoId(1), 3),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);

        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(0));
    }

    #[test]
    fn test_const_fold_setlt() {
        // 3 < 5 -> 1
        let types = TypeTable::new(64);
        let insn = Instruction::binop(
            Opcode::SetLt,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
        );
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 3),
            Pseudo::val(PseudoId(1), 5),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);

        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(1));
    }

    #[test]
    fn test_identity_eq_self() {
        // x == x -> 1
        let types = TypeTable::new(64);
        let insn = Instruction::binop(
            Opcode::SetEq,
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

        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(1));
    }

    #[test]
    fn test_identity_lt_self() {
        // x < x -> 0
        let types = TypeTable::new(64);
        let insn = Instruction::binop(
            Opcode::SetLt,
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

        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(0));
    }

    // ========================================================================
    // Unary Folding Tests
    // ========================================================================

    #[test]
    fn test_const_fold_neg() {
        // -42 -> -42
        let types = TypeTable::new(64);
        let insn = Instruction::unop(Opcode::Neg, PseudoId(1), PseudoId(0), types.int_id, 32);
        let pseudos = vec![Pseudo::val(PseudoId(0), 42), Pseudo::reg(PseudoId(1), 1)];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);

        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(-42));
    }

    #[test]
    fn test_const_fold_not() {
        // ~0 -> -1
        let types = TypeTable::new(64);
        let insn = Instruction::unop(Opcode::Not, PseudoId(1), PseudoId(0), types.int_id, 32);
        let pseudos = vec![Pseudo::val(PseudoId(0), 0), Pseudo::reg(PseudoId(1), 1)];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func);
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);

        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(-1));
    }
}
