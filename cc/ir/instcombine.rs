//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// InstCombine: instruction-combining optimizations.
// - Constant folding: evaluate operations on constants at compile time
// - Algebraic simplification: x + 0 -> x, x * 1 -> x, etc.
// - Identity patterns: x - x -> 0, x ^ x -> 0, etc.
//
// Memory-ordering contract: InstCombine only rewrites pure
// arithmetic, bitwise, comparison, and unary opcodes (see the
// `try_simplify` dispatch). It never touches `Load`, `Store`, `Asm`,
// `Call`, `Atomic*`, `Fence`, or any other memory-touching op, and
// it never reorders surviving instructions — each rewrite is an
// in-place value substitution at a single instruction site. This
// preserves the relative order of every memory op, which is the
// contract that lets `Instruction::is_memory_barrier()` mean
// something. Any future extension that does start touching memory
// ops or moving instructions across blocks must consult
// `is_memory_barrier()` before crossing.
//

use super::{Function, Instruction, Opcode, PseudoId, PseudoKind};
use crate::types::TypeId;
use std::collections::HashMap;

// Constant Resolution

/// The constant visible at a pseudo, seeing through `Copy` chains.
///
/// `Function::const_val` answers from the pseudo's *kind*, and a `Copy`
/// target is an ordinary `Reg`. Folding therefore used to stop at the first
/// copy: `a = 2 + 3; b = a * 4;` folded `a` and then stalled, and the
/// fixed-point loop in `opt.rs` spun without finding anything more. Since
/// SSA promotion rewrites a promoted `Load` into a `Copy`, copies are the
/// normal shape of a value that came out of a local, and stopping at them
/// means not folding through a variable at all.
///
/// Sound as a function-wide map, with no dominance query, because SSA
/// invariant I1 (`ir/validate.rs`, `check_single_def`) makes each target's
/// definition unique — so "the Copy that defines %n" is a fact about the
/// function rather than about a program point. That is also why this lives
/// here and not on `Function`: after `ir::lower::lower_module`, phi
/// elimination deliberately creates multi-def copies, and the same walk
/// would be unsound there.
struct ConstMap {
    /// Every `PseudoKind::Val` pseudo's value.
    vals: HashMap<PseudoId, i128>,
    /// Copy target -> (source, the copy's operand width in bits).
    copies: HashMap<PseudoId, (PseudoId, u32)>,
}

impl ConstMap {
    fn new(func: &Function) -> Self {
        let mut vals = HashMap::new();
        for p in &func.pseudos {
            if let PseudoKind::Val(v) = &p.kind {
                // I1 should make this unique. If it is not, a release build
                // has no validator to say so, and folding the wrong one
                // silently is worse than not folding: poison the entry.
                if let Some(prev) = vals.insert(p.id, *v) {
                    if prev != *v {
                        vals.remove(&p.id);
                    }
                }
            }
        }

        let mut copies: HashMap<PseudoId, (PseudoId, u32)> = HashMap::new();
        let mut poisoned: Vec<PseudoId> = Vec::new();
        for bb in &func.blocks {
            for insn in &bb.insns {
                if insn.op != Opcode::Copy || insn.src.len() != 1 {
                    continue;
                }
                if let Some(target) = insn.target {
                    let entry = (insn.src[0], insn.size);
                    if let Some(prev) = copies.insert(target, entry) {
                        if prev != entry {
                            poisoned.push(target);
                        }
                    }
                }
            }
        }
        for id in poisoned {
            copies.remove(&id);
        }

        Self { vals, copies }
    }

    /// Record a simplification this run is about to apply.
    ///
    /// Every collected simplification is applied unconditionally, so this is
    /// a fact rather than a guess. It is what lets `a = 2+3; b = a*4;
    /// c = b+1;` fold all the way down in a single pass instead of needing
    /// one `opt.rs` iteration per level of expression depth.
    fn record(&mut self, target: PseudoId, size: u32, result: &Simplification) {
        match result {
            Simplification::FoldToConst(v) if unambiguous_at(*v, size.max(1)) => {
                self.vals.insert(target, *v);
            }
            Simplification::CopyFrom(src) => {
                self.copies.insert(target, (*src, size));
            }
            _ => {}
        }
    }

    /// The constant at `id`, following `Copy` chains to their source.
    ///
    /// `None` for anything that is not a compile-time integer constant: a
    /// `Phi`, a `Load` or `Call` result, an `Arg`, an `Undef`, a float, or a
    /// chain whose value does not fit the width it is copied at.
    fn get(&self, id: PseudoId) -> Option<i128> {
        let mut cur = id;
        // The narrowest width the value is observed through bounds what the
        // final answer is allowed to be.
        let mut narrowest: Option<u32> = None;

        // Bounded rather than visited-set: a cycle terminates instead of
        // hanging, without relying on I1 having actually been checked.
        for _ in 0..=self.copies.len() {
            if let Some(&v) = self.vals.get(&cur) {
                return match narrowest {
                    Some(w) if !unambiguous_at(v, w) => None,
                    _ => Some(v),
                };
            }
            let &(src, width) = self.copies.get(&cur)?;
            narrowest = Some(narrowest.map_or(width, |w: u32| w.min(width)));
            cur = src;
        }
        None
    }
}

// Simplification Result

/// Result of trying to simplify an instruction
enum Simplification {
    /// No simplification possible
    None,
    /// Copy from an existing pseudo (algebraic identity)
    CopyFrom(PseudoId),
    /// Create a new constant with this value and copy from it
    FoldToConst(i128),
}

// Main Entry Point

/// Run the InstCombine pass on a function.
/// Returns true if any changes were made.
pub fn run(func: &mut Function) -> bool {
    let mut changed = false;

    // Collect all simplifications first (to avoid borrow conflicts)
    let mut simplifications: Vec<(usize, usize, Simplification)> = Vec::new();
    let mut consts = ConstMap::new(func);

    for (bb_idx, bb) in func.blocks.iter().enumerate() {
        for (insn_idx, insn) in bb.insns.iter().enumerate() {
            let result = try_simplify(insn, &consts);
            if let Some(target) = insn.target {
                consts.record(target, insn.size, &result);
            }
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

// Simplification Dispatch

/// Try to simplify an instruction. Returns the simplification to apply.
fn try_simplify(insn: &Instruction, consts: &ConstMap) -> Simplification {
    match insn.op {
        // Integer arithmetic
        Opcode::Add => simplify_add(insn, consts),
        Opcode::Sub => simplify_sub(insn, consts),
        Opcode::Mul => simplify_mul(insn, consts),
        Opcode::DivS | Opcode::DivU => simplify_div(insn, consts),
        Opcode::ModS | Opcode::ModU => simplify_mod(insn, consts),

        // Shifts
        Opcode::Shl | Opcode::Lsr | Opcode::Asr => simplify_shift(insn, consts),

        // Bitwise (all handled by unified simplify_bitwise)
        Opcode::And | Opcode::Or | Opcode::Xor => simplify_bitwise(insn, consts),

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
        | Opcode::SetAe => simplify_comparison(insn, consts),

        // Unary
        Opcode::Neg => simplify_neg(insn, consts),
        Opcode::Not => simplify_not(insn, consts),

        _ => Simplification::None,
    }
}

/// Return FoldToConst(0).
fn fold_to_zero() -> Simplification {
    Simplification::FoldToConst(0)
}

/// Return FoldToConst for an absorbing/identity constant.
fn fold_to_const(value: i128) -> Simplification {
    Simplification::FoldToConst(value)
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

// Add Simplification

fn simplify_add(insn: &Instruction, consts: &ConstMap) -> Simplification {
    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = consts.get(src1);
    let val2 = consts.get(src2);

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

// Sub Simplification

fn simplify_sub(insn: &Instruction, consts: &ConstMap) -> Simplification {
    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];

    // Identity: x - x -> 0
    if src1 == src2 {
        return fold_to_zero();
    }

    let val1 = consts.get(src1);
    let val2 = consts.get(src2);

    match (val1, val2) {
        // Constant folding: a - b -> (a - b)
        (Some(a), Some(b)) => Simplification::FoldToConst(a.wrapping_sub(b)),

        // Algebraic: x - 0 -> x
        (None, Some(0)) => Simplification::CopyFrom(src1),

        _ => Simplification::None,
    }
}

// Mul Simplification

fn simplify_mul(insn: &Instruction, consts: &ConstMap) -> Simplification {
    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = consts.get(src1);
    let val2 = consts.get(src2);

    match (val1, val2) {
        // Constant folding: a * b -> (a * b)
        (Some(a), Some(b)) => Simplification::FoldToConst(a.wrapping_mul(b)),

        // Algebraic: x * 0 -> 0
        (None, Some(0)) => fold_to_zero(),
        (Some(0), None) => fold_to_zero(),

        // Algebraic: x * 1 -> x
        (None, Some(1)) => Simplification::CopyFrom(src1),
        (Some(1), None) => Simplification::CopyFrom(src2),

        _ => Simplification::None,
    }
}

// Div Simplification

fn simplify_div(insn: &Instruction, consts: &ConstMap) -> Simplification {
    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    // Read at the operand's own width, in the signedness the opcode implies --
    // the same discipline `simplify_shift` uses. Division is not congruent
    // modulo 2^n the way add/sub/mul are: it reads the whole value and its
    // sign, so `(int)0xFFFFFFFFu` arriving as 4294967295 rather than -1
    // answered 2147483647 where C says 0.
    let signed = insn.op == Opcode::DivS;
    let size = insn.size.max(1);
    let val1 = consts.get(src1).map(|v| at_width(v, size, signed));
    let val2 = consts.get(src2).map(|v| at_width(v, size, signed));

    match (val1, val2) {
        // Constant folding: a / b -> (a / b) (avoid div by zero)
        (Some(a), Some(b)) if b != 0 => {
            let folded = if signed {
                a.wrapping_div(b)
            } else {
                (a as u128).wrapping_div(b as u128) as i128
            };
            Simplification::FoldToConst(at_width(folded, size, signed))
        }

        // Algebraic: x / 1 -> x
        (None, Some(1)) => Simplification::CopyFrom(src1),

        // Algebraic: 0 / x -> 0
        (Some(0), None) => fold_to_zero(),

        _ => Simplification::None,
    }
}

// Mod Simplification

fn simplify_mod(insn: &Instruction, consts: &ConstMap) -> Simplification {
    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    // Same width and signedness discipline as `simplify_div`.
    let signed = insn.op == Opcode::ModS;
    let size = insn.size.max(1);
    let val1 = consts.get(src1).map(|v| at_width(v, size, signed));
    let val2 = consts.get(src2).map(|v| at_width(v, size, signed));

    match (val1, val2) {
        // Constant folding: a % b -> (a % b) (avoid mod by zero)
        (Some(a), Some(b)) if b != 0 => {
            let folded = if signed {
                a.wrapping_rem(b)
            } else {
                (a as u128).wrapping_rem(b as u128) as i128
            };
            Simplification::FoldToConst(at_width(folded, size, signed))
        }

        // Algebraic: 0 % x -> 0
        (Some(0), None) => fold_to_zero(),

        // Algebraic: x % 1 -> 0
        (None, Some(1)) => fold_to_zero(),

        _ => Simplification::None,
    }
}

// Shift Simplifications

/// Does `v` mean the same thing at `size` bits whether it is read as signed
/// or as unsigned?
///
/// An `i128` in this IR holds whatever bit pattern its constant was built
/// from, and nothing on the instruction says how to read it back: the same
/// 32 bits are -1 or 4294967295 depending on the consumer, and a `Set*`
/// cannot even ask, because its `size` is the width of its own `_Bool`/`int`
/// result rather than of its operands (`(_Bool)x` lowers to `setne.8` over
/// two 32-bit values).
///
/// That ambiguity is pre-existing and harmless while a folded constant only
/// ever reaches codegen, which truncates when it materializes an immediate.
/// Feeding one back into a *second* fold is what would make it visible --
/// `1u << 31` compared against `2147483648u`, or `0x40000000 * 4` divided by
/// two. Rather than guess a signedness this pass does not have the type
/// information to know, only values that read the same either way are
/// carried across a fold. The rest simply do not chain, which is exactly the
/// behaviour before copies were followed.
fn unambiguous_at(v: i128, size: u32) -> bool {
    at_width(v, size, true) == v && at_width(v, size, false) == v
}

/// A constant as it actually is at `size` bits.
///
/// `const_val` hands back an `i128` holding whatever bit pattern the constant
/// was built from, which for a narrower operand may be wider than the operand
/// itself: `(int)0xFFFFFFFFu` is stored as 4294967295, not -1. Every consumer
/// that only *emits* the value truncates and so never noticed, but folding
/// arithmetic on it does notice -- `Asr` on 4294967295 shifts in zeros and
/// answers 2147483647 where the operand is -1 and the answer is -1.
fn at_width(v: i128, size: u32, signed: bool) -> i128 {
    if size == 0 || size >= 128 {
        return v;
    }
    let pad = 128 - size;
    if signed {
        (v << pad) >> pad
    } else {
        (((v as u128) << pad) >> pad) as i128
    }
}

fn simplify_shift(insn: &Instruction, consts: &ConstMap) -> Simplification {
    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = consts.get(src1);
    let val2 = consts.get(src2);

    let max_shift = insn.size.max(1) as i128; // use operand width, not hardcoded 128

    match (val1, val2) {
        // Constant folding (shift amount must be in [0, type_width))
        (Some(a), Some(b)) if (0..max_shift).contains(&b) => {
            // Each shift is folded at the operand's own width, and the
            // operand is read with the signedness that shift implies: `Asr`
            // is the arithmetic one by definition, `Lsr` the logical one.
            let size = insn.size.max(1);
            let folded = match insn.op {
                // The result is truncated back, so an overflowing shift wraps
                // at the operand width rather than growing into the i128.
                Opcode::Shl => at_width(at_width(a, size, true).wrapping_shl(b as u32), size, true),
                Opcode::Lsr => at_width(a, size, false).wrapping_shr(b as u32),
                Opcode::Asr => at_width(a, size, true).wrapping_shr(b as u32),
                _ => return Simplification::None,
            };
            Simplification::FoldToConst(folded)
        }

        // Algebraic: x op 0 -> x
        (None, Some(0)) => Simplification::CopyFrom(src1),

        // Algebraic: 0 op n -> 0
        (Some(0), None) => fold_to_zero(),

        _ => Simplification::None,
    }
}

// And Simplification

/// Result when applying x op x
enum SelfOpResult {
    /// x op x copies src (e.g., x & x = x, x | x = x)
    CopySrc,
    /// x op x folds to a constant (e.g., x ^ x = 0)
    Const(i128),
}

/// Bitwise operation behavior for simplification
struct BitwiseInfo {
    /// Result when x op x
    self_result: SelfOpResult,
    /// Identity element: x op identity = x (e.g., x & -1 = x, x | 0 = x, x ^ 0 = x)
    identity: i128,
    /// Absorbing element: x op absorbing = absorbing (e.g., x & 0 = 0, x | -1 = -1)
    /// None for XOR (no absorbing element)
    absorbing: Option<i128>,
    /// Constant folding function
    fold: fn(i128, i128) -> i128,
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
fn simplify_bitwise(insn: &Instruction, consts: &ConstMap) -> Simplification {
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
            SelfOpResult::Const(c) => fold_to_const(c),
        };
    }

    let val1 = consts.get(src1);
    let val2 = consts.get(src2);

    match (val1, val2) {
        // Constant folding: a op b
        (Some(a), Some(b)) => Simplification::FoldToConst((info.fold)(a, b)),

        // Algebraic: x op identity -> x
        (None, Some(v)) if v == info.identity => Simplification::CopyFrom(src1),
        (Some(v), None) if v == info.identity => Simplification::CopyFrom(src2),

        // Algebraic: x op absorbing -> absorbing (if exists)
        (None, Some(v)) if info.absorbing == Some(v) => fold_to_const(v),
        (Some(v), None) if info.absorbing == Some(v) => fold_to_const(v),

        _ => Simplification::None,
    }
}

// Comparison Simplifications

/// Comparison behavior for identity (x op x) and constant folding
struct CmpInfo {
    /// Result when comparing x to itself (e.g., x == x -> 1, x < x -> 0)
    identity_result: i128,
    /// Constant comparison function
    compare: fn(i128, i128) -> bool,
    /// How to read the operands at their own width before comparing.
    ///
    /// The opcode is the only thing that carries this: `SetLt`/`Le`/`Gt`/`Ge`
    /// are the signed forms and `SetB`/`Be`/`A`/`Ae` the unsigned ones.
    /// `SetEq`/`SetNe` do not care which, as long as both sides are read the
    /// same way -- but they do care about the width.
    signed: bool,
}

/// Get comparison info for the given opcode
fn get_cmp_info(op: Opcode) -> Option<CmpInfo> {
    match op {
        Opcode::SetEq => Some(CmpInfo {
            identity_result: 1,
            compare: |a, b| a == b,
            signed: true,
        }),
        Opcode::SetNe => Some(CmpInfo {
            identity_result: 0,
            compare: |a, b| a != b,
            signed: true,
        }),
        Opcode::SetLt => Some(CmpInfo {
            identity_result: 0,
            compare: |a, b| a < b,
            signed: true,
        }),
        Opcode::SetLe => Some(CmpInfo {
            identity_result: 1,
            compare: |a, b| a <= b,
            signed: true,
        }),
        Opcode::SetGt => Some(CmpInfo {
            identity_result: 0,
            compare: |a, b| a > b,
            signed: true,
        }),
        Opcode::SetGe => Some(CmpInfo {
            identity_result: 1,
            compare: |a, b| a >= b,
            signed: true,
        }),
        Opcode::SetB => Some(CmpInfo {
            identity_result: 0,
            compare: |a, b| (a as u128) < (b as u128),
            signed: false,
        }),
        Opcode::SetBe => Some(CmpInfo {
            identity_result: 1,
            compare: |a, b| (a as u128) <= (b as u128),
            signed: false,
        }),
        Opcode::SetA => Some(CmpInfo {
            identity_result: 0,
            compare: |a, b| (a as u128) > (b as u128),
            signed: false,
        }),
        Opcode::SetAe => Some(CmpInfo {
            identity_result: 1,
            compare: |a, b| (a as u128) >= (b as u128),
            signed: false,
        }),
        _ => None,
    }
}

/// Unified comparison simplification for all SetXX opcodes
fn simplify_comparison(insn: &Instruction, consts: &ConstMap) -> Simplification {
    let info = match get_cmp_info(insn.op) {
        Some(i) => i,
        None => return Simplification::None,
    };

    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];

    // Identity: x op x -> identity_result (comparison result is always i32/i64, never i128)
    if src1 == src2 {
        return Simplification::FoldToConst(info.identity_result);
    }

    // Constant folding
    let val1 = consts.get(src1);
    let val2 = consts.get(src2);

    if let (Some(a), Some(b)) = (val1, val2) {
        // `insn.size` is the width of the *result* at one of the four Set*
        // construction sites -- a `_Bool` conversion lowers to `setne.8` over
        // two 32-bit operands -- and that site is the only one that records
        // the operand width, in `src_size`. Preferring `src_size` when it is
        // set is right at all four.
        let size = if insn.src_size != 0 {
            insn.src_size
        } else {
            insn.size.max(1)
        };
        let a = at_width(a, size, info.signed);
        let b = at_width(b, size, info.signed);
        return Simplification::FoldToConst(if (info.compare)(a, b) { 1 } else { 0 });
    }

    Simplification::None
}

// Unary Simplifications

fn simplify_neg(insn: &Instruction, consts: &ConstMap) -> Simplification {
    if insn.src.len() != 1 {
        return Simplification::None;
    }

    let src = insn.src[0];
    if let Some(val) = consts.get(src) {
        Simplification::FoldToConst(val.wrapping_neg())
    } else {
        Simplification::None
    }
}

fn simplify_not(insn: &Instruction, consts: &ConstMap) -> Simplification {
    if insn.src.len() != 1 {
        return Simplification::None;
    }

    let src = insn.src[0];
    if let Some(val) = consts.get(src) {
        Simplification::FoldToConst(!val)
    } else {
        Simplification::None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Pseudo, PseudoKind};
    use crate::target::Target;
    use crate::types::TypeTable;

    fn make_test_func_with_insn(insn: Instruction, pseudos: Vec<Pseudo>) -> Function {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("test", types.int_id);

        for p in &pseudos {
            func.add_pseudo(p.clone());
        }

        // Set next_pseudo to be after the highest pseudo ID
        let max_id = pseudos.iter().map(|p| p.id.0).max().unwrap_or(0);
        func.next_pseudo = max_id + 1;

        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        bb.add_insn(insn);
        bb.add_insn(Instruction::ret(None));
        func.add_block(bb);
        func.entry = BasicBlockId(0);

        func
    }

    // Algebraic Simplification Tests

    #[test]
    fn test_add_zero_right() {
        // x + 0 -> x
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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

    // Constant Folding Tests

    #[test]
    fn test_const_fold_add() {
        // 2 + 3 -> 5
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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

    // Comparison Folding Tests

    #[test]
    fn test_const_fold_seteq_true() {
        // 5 == 5 -> 1
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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

    // Unary Folding Tests

    #[test]
    fn test_const_fold_neg() {
        // -42 -> -42
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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

    /// Several instructions in one block, for chains that need more than a
    /// single site to show anything.
    fn make_test_func_with_insns(insns: Vec<Instruction>, pseudos: Vec<Pseudo>) -> Function {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("test", types.int_id);

        for p in &pseudos {
            func.add_pseudo(p.clone());
        }
        let max_id = pseudos.iter().map(|p| p.id.0).max().unwrap_or(0);
        func.next_pseudo = max_id + 1;

        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        for insn in insns {
            bb.add_insn(insn);
        }
        bb.add_insn(Instruction::ret(None));
        func.add_block(bb);
        func.entry = BasicBlockId(0);
        func
    }

    /// The instruction at `idx` (1-based past the Entry).
    fn insn_at(func: &Function, idx: usize) -> &Instruction {
        &func.blocks[0].insns[idx + 1]
    }

    fn int_type() -> TypeId {
        TypeTable::new(&Target::host()).int_id
    }

    // Constant folding through Copy
    //
    // `const_val` answers from the pseudo's kind, and a Copy target is an
    // ordinary Reg, so folding used to stop dead at the first copy. SSA
    // promotion turns every promoted Load into a Copy, so that was the
    // difference between folding through a local and not folding at all.

    #[test]
    fn test_const_fold_through_copy() {
        let int_id = int_type();
        // %1 = copy $0(5) ; %3 = add %1, $2(7)
        let insns = vec![
            Instruction::new(Opcode::Copy)
                .with_target(PseudoId(1))
                .with_src(PseudoId(0))
                .with_type_and_size(int_id, 32),
            Instruction::binop(
                Opcode::Add,
                PseudoId(3),
                PseudoId(1),
                PseudoId(2),
                int_id,
                32,
            ),
        ];
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 5),
            Pseudo::reg(PseudoId(1), 0),
            Pseudo::val(PseudoId(2), 7),
            Pseudo::reg(PseudoId(3), 1),
        ];
        let mut func = make_test_func_with_insns(insns, pseudos);
        assert!(run(&mut func));

        let add = insn_at(&func, 1);
        assert_eq!(add.op, Opcode::Copy, "the add should have folded");
        assert_eq!(func.const_val(add.src[0]), Some(12));
    }

    #[test]
    fn test_const_fold_through_copy_chain() {
        let int_id = int_type();
        let mut insns = vec![Instruction::new(Opcode::Copy)
            .with_target(PseudoId(1))
            .with_src(PseudoId(0))
            .with_type_and_size(int_id, 32)];
        // Two more links: %2 = copy %1 ; %3 = copy %2
        for i in 2..=3u32 {
            insns.push(
                Instruction::new(Opcode::Copy)
                    .with_target(PseudoId(i))
                    .with_src(PseudoId(i - 1))
                    .with_type_and_size(int_id, 32),
            );
        }
        insns.push(Instruction::binop(
            Opcode::Mul,
            PseudoId(5),
            PseudoId(3),
            PseudoId(4),
            int_id,
            32,
        ));
        let mut pseudos = vec![Pseudo::val(PseudoId(0), 5)];
        for i in 1..=3u32 {
            pseudos.push(Pseudo::reg(PseudoId(i), i));
        }
        pseudos.push(Pseudo::val(PseudoId(4), 4));
        pseudos.push(Pseudo::reg(PseudoId(5), 5));

        let mut func = make_test_func_with_insns(insns, pseudos);
        assert!(run(&mut func));
        let mul = insn_at(&func, 3);
        assert_eq!(mul.op, Opcode::Copy);
        assert_eq!(func.const_val(mul.src[0]), Some(20));
    }

    /// The whole chain must collapse in ONE pass. Before, each level needed
    /// its own iteration of the `opt.rs` fixed-point loop -- and since the
    /// first level never folded, the loop spun without progress.
    #[test]
    fn test_const_fold_multi_level_in_a_single_pass() {
        let int_id = int_type();
        // %2 = 2 + 3 ; %4 = %2 * 4 ; %6 = %4 + 1   -> 21
        let insns = vec![
            Instruction::binop(
                Opcode::Add,
                PseudoId(2),
                PseudoId(0),
                PseudoId(1),
                int_id,
                32,
            ),
            Instruction::binop(
                Opcode::Mul,
                PseudoId(4),
                PseudoId(2),
                PseudoId(3),
                int_id,
                32,
            ),
            Instruction::binop(
                Opcode::Add,
                PseudoId(6),
                PseudoId(4),
                PseudoId(5),
                int_id,
                32,
            ),
        ];
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 2),
            Pseudo::val(PseudoId(1), 3),
            Pseudo::reg(PseudoId(2), 0),
            Pseudo::val(PseudoId(3), 4),
            Pseudo::reg(PseudoId(4), 1),
            Pseudo::val(PseudoId(5), 1),
            Pseudo::reg(PseudoId(6), 2),
        ];
        let mut func = make_test_func_with_insns(insns, pseudos);
        assert!(run(&mut func), "one pass must fold the whole chain");

        let last = insn_at(&func, 2);
        assert_eq!(last.op, Opcode::Copy);
        assert_eq!(
            func.const_val(last.src[0]),
            Some(21),
            "2+3 then *4 then +1 is 21, in a single pass"
        );
    }

    /// The guard that keeps chaining from being a miscompile. `0x40000000 * 4`
    /// overflows `int`, and folds are computed on `i128` without being
    /// truncated, so the product is held as `0x1_0000_0000`. Chaining it into
    /// `y / 2` would answer INT_MIN where C -- and gcc -- say 0. The product
    /// does not read the same signed and unsigned at 32 bits, so it does not
    /// chain, and the division is left for run time.
    #[test]
    fn test_ambiguous_folded_value_does_not_chain() {
        let int_id = int_type();
        // %2 = 0x40000000 * 4 ; %4 = %2 / 2
        let insns = vec![
            Instruction::binop(
                Opcode::Mul,
                PseudoId(2),
                PseudoId(0),
                PseudoId(1),
                int_id,
                32,
            ),
            Instruction::binop(
                Opcode::DivS,
                PseudoId(4),
                PseudoId(2),
                PseudoId(3),
                int_id,
                32,
            ),
        ];
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 0x4000_0000),
            Pseudo::val(PseudoId(1), 4),
            Pseudo::reg(PseudoId(2), 0),
            Pseudo::val(PseudoId(3), 2),
            Pseudo::reg(PseudoId(4), 1),
        ];
        let mut func = make_test_func_with_insns(insns, pseudos);
        run(&mut func);

        assert_eq!(insn_at(&func, 0).op, Opcode::Copy, "the mul still folds");
        assert_eq!(
            insn_at(&func, 1).op,
            Opcode::DivS,
            "the div must NOT fold: the product is ambiguous at 32 bits"
        );
    }

    fn assert_no_fold_through(src: Pseudo, why: &str) {
        let int_id = int_type();
        let src_id = src.id;
        let insns = vec![
            Instruction::new(Opcode::Copy)
                .with_target(PseudoId(10))
                .with_src(src_id)
                .with_type_and_size(int_id, 32),
            Instruction::binop(
                Opcode::Add,
                PseudoId(12),
                PseudoId(10),
                PseudoId(11),
                int_id,
                32,
            ),
        ];
        let pseudos = vec![
            src,
            Pseudo::reg(PseudoId(10), 0),
            Pseudo::val(PseudoId(11), 7),
            Pseudo::reg(PseudoId(12), 1),
        ];
        let mut func = make_test_func_with_insns(insns, pseudos);
        run(&mut func);
        assert_eq!(insn_at(&func, 1).op, Opcode::Add, "{why}");
    }

    #[test]
    fn test_no_fold_through_copy_of_reg() {
        // Stands in for a Load or Call result.
        assert_no_fold_through(
            Pseudo::reg(PseudoId(0), 9),
            "a Reg source is not a constant",
        );
    }

    #[test]
    fn test_no_fold_through_copy_of_undef() {
        assert_no_fold_through(Pseudo::undef(PseudoId(0)), "undef is not a constant");
    }

    #[test]
    fn test_no_fold_through_copy_of_arg() {
        assert_no_fold_through(Pseudo::arg(PseudoId(0), 0), "an argument is not a constant");
    }

    #[test]
    fn test_no_fold_through_phi() {
        assert_no_fold_through(Pseudo::phi(PseudoId(0), 0), "a phi is not a constant");
    }

    /// A copy cycle is not legal SSA, but the validator that says so is
    /// `debug_assert!`-gated, so the walk must terminate on its own.
    #[test]
    fn test_copy_cycle_terminates() {
        let int_id = int_type();
        let insns = vec![
            Instruction::new(Opcode::Copy)
                .with_target(PseudoId(0))
                .with_src(PseudoId(1))
                .with_type_and_size(int_id, 32),
            Instruction::new(Opcode::Copy)
                .with_target(PseudoId(1))
                .with_src(PseudoId(0))
                .with_type_and_size(int_id, 32),
            Instruction::binop(
                Opcode::Add,
                PseudoId(3),
                PseudoId(0),
                PseudoId(2),
                int_id,
                32,
            ),
        ];
        let pseudos = vec![
            Pseudo::reg(PseudoId(0), 0),
            Pseudo::reg(PseudoId(1), 1),
            Pseudo::val(PseudoId(2), 7),
            Pseudo::reg(PseudoId(3), 2),
        ];
        let mut func = make_test_func_with_insns(insns, pseudos);
        run(&mut func);
        assert_eq!(
            insn_at(&func, 2).op,
            Opcode::Add,
            "a cycle yields no constant"
        );
    }

    /// Algebraic identities benefit from the same resolution.
    #[test]
    fn test_algebraic_identity_through_copy() {
        let int_id = int_type();
        // %1 = copy $0(0) ; %3 = add %2(x), %1  -> copy of x
        let insns = vec![
            Instruction::new(Opcode::Copy)
                .with_target(PseudoId(1))
                .with_src(PseudoId(0))
                .with_type_and_size(int_id, 32),
            Instruction::binop(
                Opcode::Add,
                PseudoId(3),
                PseudoId(2),
                PseudoId(1),
                int_id,
                32,
            ),
        ];
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 0),
            Pseudo::reg(PseudoId(1), 0),
            Pseudo::reg(PseudoId(2), 1),
            Pseudo::reg(PseudoId(3), 2),
        ];
        let mut func = make_test_func_with_insns(insns, pseudos);
        assert!(run(&mut func));
        let add = insn_at(&func, 1);
        assert_eq!(add.op, Opcode::Copy);
        assert_eq!(add.src, vec![PseudoId(2)], "x + 0 is x");
    }

    /// Following copies must not break the single-def invariant the map
    /// itself relies on.
    #[test]
    fn test_fold_through_copy_preserves_ssa() {
        let int_id = int_type();
        let insns = vec![
            Instruction::new(Opcode::Copy)
                .with_target(PseudoId(1))
                .with_src(PseudoId(0))
                .with_type_and_size(int_id, 32),
            Instruction::binop(
                Opcode::Add,
                PseudoId(3),
                PseudoId(1),
                PseudoId(2),
                int_id,
                32,
            ),
        ];
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 5),
            Pseudo::reg(PseudoId(1), 0),
            Pseudo::val(PseudoId(2), 7),
            Pseudo::reg(PseudoId(3), 1),
        ];
        let mut func = make_test_func_with_insns(insns, pseudos);
        run(&mut func);
        assert!(crate::ir::validate::validate_function(&func).is_ok());
    }

    /// Division reads the whole value and its sign, so the operand has to be
    /// read at its own width first. `(int)0xFFFFFFFFu` emits no conversion
    /// instruction, so the constant still holds 4294967295 where the opcode
    /// means -1.
    #[test]
    fn test_signed_div_reads_operands_at_their_width() {
        let int_id = int_type();
        let insns = vec![Instruction::binop(
            Opcode::DivS,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            int_id,
            32,
        )];
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 0xFFFF_FFFF),
            Pseudo::val(PseudoId(1), 2),
            Pseudo::reg(PseudoId(2), 0),
        ];
        let mut func = make_test_func_with_insns(insns, pseudos);
        assert!(run(&mut func));
        let d = insn_at(&func, 0);
        assert_eq!(d.op, Opcode::Copy);
        assert_eq!(func.const_val(d.src[0]), Some(0), "-1 / 2 is 0");
    }

    #[test]
    fn test_signed_mod_reads_operands_at_their_width() {
        let int_id = int_type();
        let insns = vec![Instruction::binop(
            Opcode::ModS,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            int_id,
            32,
        )];
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 0xFFFF_FFFF),
            Pseudo::val(PseudoId(1), 3),
            Pseudo::reg(PseudoId(2), 0),
        ];
        let mut func = make_test_func_with_insns(insns, pseudos);
        assert!(run(&mut func));
        let m = insn_at(&func, 0);
        assert_eq!(func.const_val(m.src[0]), Some(-1), "-1 % 3 is -1");
    }

    /// The unsigned opcodes must keep reading the same bits as unsigned.
    #[test]
    fn test_unsigned_div_stays_unsigned() {
        let uint_id = TypeTable::new(&Target::host()).uint_id;
        let insns = vec![Instruction::binop(
            Opcode::DivU,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            uint_id,
            32,
        )];
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 0xFFFF_FFFF),
            Pseudo::val(PseudoId(1), 2),
            Pseudo::reg(PseudoId(2), 0),
        ];
        let mut func = make_test_func_with_insns(insns, pseudos);
        assert!(run(&mut func));
        let d = insn_at(&func, 0);
        assert_eq!(
            func.const_val(d.src[0]),
            Some(2147483647),
            "0xFFFFFFFF / 2 unsigned is 2147483647"
        );
    }

    /// A signed ordering comparison folded on the raw bits answered backwards.
    #[test]
    fn test_signed_comparison_reads_operands_at_their_width() {
        let int_id = int_type();
        let insns = vec![Instruction::binop(
            Opcode::SetLt,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            int_id,
            32,
        )];
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 0xFFFF_FFFF),
            Pseudo::val(PseudoId(1), 0),
            Pseudo::reg(PseudoId(2), 0),
        ];
        let mut func = make_test_func_with_insns(insns, pseudos);
        assert!(run(&mut func));
        let c = insn_at(&func, 0);
        assert_eq!(func.const_val(c.src[0]), Some(1), "-1 < 0 is true");
    }

    /// `insn.size` on a Set* is the width of its own result. The `_Bool`
    /// conversion is the site that proves it: `setne.8` over two 32-bit
    /// operands, with the operand width recorded in `src_size`. Reading
    /// `size` there would compare only the low 8 bits, making `(_Bool)256`
    /// false.
    #[test]
    fn test_comparison_prefers_src_size_when_set() {
        let int_id = int_type();
        let bool_id = TypeTable::new(&Target::host()).bool_id;
        let mut insn = Instruction::binop(
            Opcode::SetNe,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            bool_id,
            8,
        );
        insn.src_size = 32;
        insn.src_typ = Some(int_id);
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 256),
            Pseudo::val(PseudoId(1), 0),
            Pseudo::reg(PseudoId(2), 0),
        ];
        let mut func = make_test_func_with_insns(vec![insn], pseudos);
        assert!(run(&mut func));
        let c = insn_at(&func, 0);
        assert_eq!(
            func.const_val(c.src[0]),
            Some(1),
            "256 != 0 is true; reading only the low 8 bits would say false"
        );
    }
}
