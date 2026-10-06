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

use super::constfold::{
    at_width, divmod_may_trap, eval_fbinop, eval_fcvt, eval_fcvtf, eval_fternop, eval_funop,
    eval_int, is_int_foldable, possible_against, CmpDomain, Outcomes,
};
use super::facts::{CmpFacts, ConstMap, Relation};
use super::propagate;
use super::{ConstValue, Function, Instruction, Opcode, PseudoId};
use crate::float::FloatVal;
use crate::types::{TypeId, TypeTable};
use std::collections::{HashMap, HashSet};

// Constant Resolution

/// Everything one run of this pass knows that is fixed for the whole
/// function, gathered so the dispatch keeps a stable arity as rules are
/// added.
struct RunFacts<'a> {
    cmps: CmpFacts,
    /// Pseudos whose value is never *less than* zero -- which is not the
    /// same as non-negative, and is deliberately the weaker fact: `fabs` of
    /// a NaN is a NaN, and a NaN is not less than zero either, because an
    /// unordered comparison is false. Stating it this way is what makes the
    /// rule below correct without a NaN test. `sqrt` gives the same fact: its
    /// root of `-0` is `-0`, and of any other negative number a NaN.
    never_lt_zero: HashSet<PseudoId>,
    types: &'a TypeTable,
}

impl<'a> RunFacts<'a> {
    pub(crate) fn new(func: &Function, consts: &ConstMap, types: &'a TypeTable) -> Self {
        let mut never_lt_zero = HashSet::new();
        for bb in &func.blocks {
            for insn in &bb.insns {
                if matches!(insn.op, Opcode::Fabs | Opcode::Sqrt) {
                    if let Some(target) = insn.target {
                        never_lt_zero.insert(target);
                    }
                }
            }
        }
        Self {
            cmps: CmpFacts::new(func, consts),
            never_lt_zero,
            types,
        }
    }

    /// The format `insn`'s float operands are in, or `None` when the
    /// instruction does not say -- a complex type, or no type at all. Folding
    /// at the wrong format is a wrong answer rather than an imprecise one, so
    /// not knowing means not folding.
    pub(crate) fn fp_format(&self, typ: Option<TypeId>) -> Option<crate::float::FpFormat> {
        typ.and_then(|t| self.types.fp_format(t))
    }
}

// Simplification Result

/// Result of trying to simplify an instruction
enum Simplification {
    /// Copy from an existing pseudo (algebraic identity)
    CopyFrom(PseudoId),
    /// Create a new constant with this value and copy from it
    FoldToConst(i128),
    /// Become this float constant.
    ///
    /// Not "copy from one": a float constant is a pseudo kind, so the fold
    /// converts the target itself and rewrites the instruction to the
    /// `SetVal` that gives it a width. See `propagate::fold_target_to_setval`.
    FoldToFloat(FloatVal),
}

// Main Entry Point

/// Run the InstCombine pass on a function.
/// Returns true if any changes were made.
pub fn run(func: &mut Function, types: &TypeTable) -> bool {
    let mut changed = false;

    // Collect all simplifications first (to avoid borrow conflicts)
    let mut simplifications: Vec<(usize, usize, Simplification)> = Vec::new();
    let mut consts = ConstMap::new(func);
    let facts = RunFacts::new(func, &consts, types);

    for (bb_idx, bb) in func.blocks.iter().enumerate() {
        for (insn_idx, insn) in bb.insns.iter().enumerate() {
            let Some(result) = try_simplify(insn, &consts, &facts) else {
                continue;
            };
            // A float fold converts the target pseudo itself, and only a
            // `Reg` may be converted, so a fold of any other target is not
            // collected at all.
            if matches!(result, Simplification::FoldToFloat(_))
                && !insn.target.is_some_and(|t| func.is_plain_temp(t))
            {
                continue;
            }
            if let Some(target) = insn.target {
                match &result {
                    Simplification::FoldToConst(v) => consts.record_int(target, insn.size, *v),
                    Simplification::CopyFrom(src) => consts.record_copy(target, insn.size, *src),
                    Simplification::FoldToFloat(v) => consts.record_float(target, *v),
                }
            }
            simplifications.push((bb_idx, insn_idx, result));
        }
    }

    // Apply simplifications. A constant goes through `propagate`, whose
    // guards may still decline the rewrite -- a 128-bit integer, for one.
    // What was recorded above stays true when they do: it is the value the
    // target holds, not the shape the instruction is rewritten to.
    let mut minted: HashMap<i128, PseudoId> = HashMap::new();
    for (bb_idx, insn_idx, simplification) in simplifications {
        let site = (bb_idx, insn_idx);
        changed |= match simplification {
            Simplification::CopyFrom(src) => {
                let insn = &func.blocks[bb_idx].insns[insn_idx];
                let copy = make_copy_from_parts(insn.target, insn.typ, insn.size, src);
                func.blocks[bb_idx].insns[insn_idx] = copy;
                true
            }
            Simplification::FoldToConst(value) => {
                propagate::fold_target_to_const(func, site, value, &mut minted)
            }
            Simplification::FoldToFloat(value) => {
                propagate::fold_target_to_setval(func, site, ConstValue::Float(value))
            }
        };
    }

    changed
}

// Simplification Dispatch

/// Try to simplify an instruction. Returns the simplification to apply, or
/// `None` when there is none.
fn try_simplify(insn: &Instruction, consts: &ConstMap, facts: &RunFacts) -> Option<Simplification> {
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
        Opcode::And | Opcode::Or | Opcode::Xor => {
            simplify_bitwise(insn, consts).or_else(|| decided(insn, consts, facts))
        }

        // Comparisons (all handled by unified simplify_comparison)
        op if op.is_int_comparison() => {
            simplify_comparison(insn, consts, facts).or_else(|| decided(insn, consts, facts))
        }

        // A short-circuit `&&`/`||` after if-conversion.
        Opcode::Select => simplify_select(insn, consts, facts),

        // Floating point
        Opcode::FAdd
        | Opcode::FSub
        | Opcode::FMul
        | Opcode::FDiv
        | Opcode::CopySign
        | Opcode::FMin
        | Opcode::FMax => simplify_fbinop(insn, consts, facts),
        Opcode::Fma => simplify_fternop(insn, consts, facts),
        Opcode::FNeg | Opcode::Fabs | Opcode::Sqrt | Opcode::RoundToIntegral(_) => {
            simplify_funop(insn, consts, facts)
        }
        Opcode::FCvtF => simplify_fcvtf(insn, consts, facts),
        op if op.is_float_comparison() => simplify_fcmp(insn, consts, facts),
        Opcode::FCvtS | Opcode::FCvtU | Opcode::Signbit => simplify_fcvt(insn, consts, facts),

        // Unary
        Opcode::Sext | Opcode::Zext | Opcode::Trunc => simplify_convert(insn, consts),
        // Every other integer operation `constfold` evaluates -- `-x`, `~x`,
        // a population count -- has no algebraic identity here.
        op if is_int_foldable(op) => simplify_of_consts(insn, consts),

        _ => None,
    }
}

/// Fold `insn` over known constants, one per operand, deferring to
/// `constfold` for the width and signedness rules. `None` there means the
/// operation is undefined for these operands -- a zero divisor, an
/// out-of-range shift count -- and the instruction is left alone.
fn fold_with(insn: &Instruction, ops: &[i128]) -> Option<Simplification> {
    eval_int(insn, ops).map(Simplification::FoldToConst)
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

fn simplify_add(insn: &Instruction, consts: &ConstMap) -> Option<Simplification> {
    if insn.src.len() != 2 {
        return None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = consts.get(src1);
    let val2 = consts.get(src2);

    match (val1, val2) {
        // Constant folding: a + b -> (a + b)
        (Some(a), Some(b)) => fold_with(insn, &[a, b]),

        // Algebraic: x + 0 -> x
        (None, Some(0)) => Some(Simplification::CopyFrom(src1)),

        // Algebraic: 0 + x -> x
        (Some(0), None) => Some(Simplification::CopyFrom(src2)),

        _ => None,
    }
}

// Sub Simplification

fn simplify_sub(insn: &Instruction, consts: &ConstMap) -> Option<Simplification> {
    if insn.src.len() != 2 {
        return None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];

    // Identity: x - x -> 0, by root: the two sides are usually distinct
    // copies of one value.
    if consts.same(src1, src2, insn.size.max(1)) {
        return Some(Simplification::FoldToConst(0));
    }

    let val1 = consts.get(src1);
    let val2 = consts.get(src2);

    match (val1, val2) {
        // Constant folding: a - b -> (a - b)
        (Some(a), Some(b)) => fold_with(insn, &[a, b]),

        // Algebraic: x - 0 -> x
        (None, Some(0)) => Some(Simplification::CopyFrom(src1)),

        _ => None,
    }
}

// Mul Simplification

fn simplify_mul(insn: &Instruction, consts: &ConstMap) -> Option<Simplification> {
    if insn.src.len() != 2 {
        return None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = consts.get(src1);
    let val2 = consts.get(src2);

    match (val1, val2) {
        // Constant folding: a * b -> (a * b)
        (Some(a), Some(b)) => fold_with(insn, &[a, b]),

        // Algebraic: x * 0 -> 0
        (None, Some(0)) => Some(Simplification::FoldToConst(0)),
        (Some(0), None) => Some(Simplification::FoldToConst(0)),

        // Algebraic: x * 1 -> x
        (None, Some(1)) => Some(Simplification::CopyFrom(src1)),
        (Some(1), None) => Some(Simplification::CopyFrom(src2)),

        _ => None,
    }
}

// Div Simplification

fn simplify_div(insn: &Instruction, consts: &ConstMap) -> Option<Simplification> {
    if insn.src.len() != 2 {
        return None;
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

    // A division that can trap is left to trap: `constfold::divmod_may_trap`
    // states the rule and both of the operand pairs it covers. This is the
    // whole of what stands between an unknown divisor and a folded answer --
    // `0 / x` used to fold to zero here without ever asking what `x` was, and
    // a divisor of -1 is only safe once the dividend is known not to be the
    // most negative value.
    if divmod_may_trap(insn.op, size, val1, val2) {
        return None;
    }

    match (val1, val2) {
        // Constant folding: a / b -> (a / b)
        (Some(a), Some(b)) => fold_with(insn, &[a, b]),

        // Algebraic: x / 1 -> x
        (None, Some(1)) => Some(Simplification::CopyFrom(src1)),

        _ => None,
    }
}

// Mod Simplification

fn simplify_mod(insn: &Instruction, consts: &ConstMap) -> Option<Simplification> {
    if insn.src.len() != 2 {
        return None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    // Same width and signedness discipline as `simplify_div`.
    let signed = insn.op == Opcode::ModS;
    let size = insn.size.max(1);
    let val1 = consts.get(src1).map(|v| at_width(v, size, signed));
    let val2 = consts.get(src2).map(|v| at_width(v, size, signed));

    // The remainder traps exactly where the division does -- on x86-64 it is
    // the same `idiv`, computing the same quotient -- so it asks the same
    // question. `0 % x` folded to zero here for an unknown `x`, and
    // `INT_MIN % -1` folded to zero through `fold_with`.
    if divmod_may_trap(insn.op, size, val1, val2) {
        return None;
    }

    match (val1, val2) {
        // Constant folding: a % b -> (a % b)
        (Some(a), Some(b)) => fold_with(insn, &[a, b]),

        // Algebraic: x % 1 -> 0
        (None, Some(1)) => Some(Simplification::FoldToConst(0)),

        _ => None,
    }
}

// Shift Simplifications

fn simplify_shift(insn: &Instruction, consts: &ConstMap) -> Option<Simplification> {
    if insn.src.len() != 2 {
        return None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let val1 = consts.get(src1);
    let val2 = consts.get(src2);

    // All-ones shifted arithmetically right is all-ones, whatever the count:
    // the sign bit fills every vacated position. `Lsr` is not this -- it
    // shifts in zeros -- and the operand has to be read *signed* at its own
    // width to be recognized at all, which is what `get` deliberately refuses
    // to guess.
    let size = insn.size.max(1);
    if insn.op == Opcode::Asr && consts.get_at(src1, size, true) == Some(-1) {
        return Some(Simplification::FoldToConst(at_width(-1, size, true)));
    }

    match (val1, val2) {
        // Constant folding (shift amount must be in [0, type_width))
        (Some(a), Some(b)) => fold_with(insn, &[a, b]),

        // Algebraic: x op 0 -> x
        (None, Some(0)) => Some(Simplification::CopyFrom(src1)),

        // Algebraic: 0 op n -> 0
        (Some(0), None) => Some(Simplification::FoldToConst(0)),

        _ => None,
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
}

/// Get bitwise operation info for the given opcode
fn get_bitwise_info(op: Opcode) -> Option<BitwiseInfo> {
    match op {
        Opcode::And => Some(BitwiseInfo {
            self_result: SelfOpResult::CopySrc,
            identity: -1,       // x & -1 = x (all bits set)
            absorbing: Some(0), // x & 0 = 0
        }),
        Opcode::Or => Some(BitwiseInfo {
            self_result: SelfOpResult::CopySrc,
            identity: 0,         // x | 0 = x
            absorbing: Some(-1), // x | -1 = -1
        }),
        Opcode::Xor => Some(BitwiseInfo {
            self_result: SelfOpResult::Const(0), // x ^ x = 0
            identity: 0,                         // x ^ 0 = x
            absorbing: None,                     // no absorbing element
        }),
        _ => None,
    }
}

/// Unified bitwise simplification for And/Or/Xor
fn simplify_bitwise(insn: &Instruction, consts: &ConstMap) -> Option<Simplification> {
    let info = get_bitwise_info(insn.op)?;

    if insn.src.len() != 2 {
        return None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];

    // x op x -> self_result, by root as above.
    if consts.same(src1, src2, insn.size.max(1)) {
        return match info.self_result {
            SelfOpResult::CopySrc => Some(Simplification::CopyFrom(src1)),
            SelfOpResult::Const(c) => Some(Simplification::FoldToConst(c)),
        };
    }

    let val1 = consts.get(src1);
    let val2 = consts.get(src2);

    match (val1, val2) {
        // Constant folding: a op b
        (Some(a), Some(b)) => fold_with(insn, &[a, b]),

        // Algebraic: x op identity -> x
        (None, Some(v)) if v == info.identity => Some(Simplification::CopyFrom(src1)),
        (Some(v), None) if v == info.identity => Some(Simplification::CopyFrom(src2)),

        // Algebraic: x op absorbing -> absorbing (if exists)
        (None, Some(v)) if info.absorbing == Some(v) => Some(Simplification::FoldToConst(v)),
        (Some(v), None) if info.absorbing == Some(v) => Some(Simplification::FoldToConst(v)),

        _ => None,
    }
}

// Comparison Simplifications

/// Unified comparison simplification for all SetXX opcodes
fn simplify_comparison(
    insn: &Instruction,
    consts: &ConstMap,
    facts: &RunFacts,
) -> Option<Simplification> {
    let Some((mask, domain @ CmpDomain::Int { signed })) = Outcomes::of_op(insn.op) else {
        return None;
    };

    if insn.src.len() != 2 {
        return None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let width = insn.operand_width();

    // `(a < b) != 0` is `a < b`. A comparison already yields 0 or 1, so the
    // boolification the front end wraps around every `&&`/`||` operand is a
    // no-op -- and an opaque one: it leaves the result of a comparison
    // *against zero*, which hides the operand pair the comparison was
    // actually about.
    if insn.op == Opcode::SetNe {
        for (bool_side, zero_side) in [(src1, src2), (src2, src1)] {
            if consts.get(zero_side) == Some(0)
                && facts.cmps.get_through(consts, bool_side, width).is_some()
            {
                return Some(Simplification::CopyFrom(bool_side));
            }
        }
    }

    // Identity: `x op x` is decided by the outcomes a value has against
    // itself.
    //
    // By root rather than by pseudo id: promotion out of memory gives every
    // use of a local its own `Copy`, so `x >> 0 != x` arrives as two distinct
    // pseudos naming one value.
    if consts.same(src1, src2, width) {
        if let Some(v) = mask.decide(domain.reflexive()) {
            return Some(Simplification::FoldToConst(i128::from(v)));
        }
    }

    // Constant folding. A comparison knows its operand width and signedness
    // from the opcode, so it can read a value `get` would refuse to guess at.
    let val1 = consts.get_at(src1, width, signed);
    let val2 = consts.get_at(src2, width, signed);

    if let (Some(a), Some(b)) = (val1, val2) {
        return fold_with(insn, &[a, b]);
    }

    None
}

/// A 0-or-1 value decided by the comparisons it combines, whatever their
/// operands are.
///
/// If-conversion turns `a && b` into `select(a, b, 0)` and `a || b` into
/// `select(a, 1, b)`; `!a` is `a == 0`; and `__builtin_isunordered` is an
/// `or` of two self-comparisons. When every comparison underneath compares
/// the *same* operand pair, the answer needs nothing about the operands:
/// `(x == y) && (x != y)` is never true, and `(x == y) || (x != y)` always
/// is. The operands may be written either way round -- `(x < y) && (y < x)`
/// is also never true. See `CmpFacts::relation` for the algebra, and for why
/// a float `(x >= y) || (x < y)` is not decided.
fn decided(insn: &Instruction, consts: &ConstMap, facts: &RunFacts) -> Option<Simplification> {
    let target = insn.target?;
    match facts.cmps.relation(consts, target, insn.size.max(1))? {
        Relation::Known(v) => Some(Simplification::FoldToConst(i128::from(v))),
        Relation::Holds(_) => None,
    }
}

/// `select(c, t, f)`, which is also the shape of a short-circuit `&&`/`||`
/// after if-conversion: see [`decided`].
fn simplify_select(
    insn: &Instruction,
    consts: &ConstMap,
    facts: &RunFacts,
) -> Option<Simplification> {
    if insn.src.len() != 3 {
        return None;
    }
    let (cond, t, f) = (insn.src[0], insn.src[1], insn.src[2]);

    // A constant condition needs no facts at all.
    if let Some(c) = consts.get(cond) {
        return Some(Simplification::CopyFrom(if c != 0 { t } else { f }));
    }
    // Both arms the same value, whatever the condition.
    let width = insn.size.max(1);
    if consts.same(t, f, width) {
        return Some(Simplification::CopyFrom(t));
    }

    decided(insn, consts, facts)
}

// Unary Simplifications

/// Fold an integer width conversion of a constant.
///
/// An extension's operand is read at the width the conversion says it was
/// stored in, which is not `insn.size` -- that is the destination. `get_at`
/// is the accessor that takes both, and it is right here for the same reason
/// it is right for a comparison: the opcode carries the signedness.
///
/// A truncation's operand is read at `size` instead: its source is wider
/// (validator I12), and the low `size` bits are all the result keeps.
fn simplify_convert(insn: &Instruction, consts: &ConstMap) -> Option<Simplification> {
    if insn.src.len() != 1 {
        return None;
    }
    let (width, signed) = match insn.op {
        Opcode::Trunc => (insn.size, true),
        _ => (insn.operand_width(), insn.op == Opcode::Sext),
    };
    fold_with(insn, &[consts.get_at(insn.src[0], width, signed)?])
}

// Floating-Point Simplification

/// Fold a float comparison whose answer does not depend on what is unknown
/// about its operands.
///
/// Decided from the set of outcomes -- less, equal, greater, unordered --
/// the two operands can possibly have ([`possible_fcmp_outcomes`]): the
/// comparison is 0 when none of them makes it true and 1 when all of them
/// do. That one rule covers two constants, a NaN on either side (only
/// *unordered* is possible, so every ordered predicate is 0 and `!=` is 1),
/// an infinity (`x > +Inf` is 0; `x <= +Inf` is not 1, since `x` may be a
/// NaN), a value compared with itself (`x < x` is 0; `x == x` is not 1), and
/// gcc's `fabs(x) < 0.0`.
///
/// Folding a signaling comparison (C's `<`) that a NaN may reach drops the
/// `FE_INVALID` it raises at run time, as gcc's folds do; DECISIONS.md
/// records why. A quiet one raises nothing to drop.
fn simplify_fcmp(
    insn: &Instruction,
    consts: &ConstMap,
    facts: &RunFacts,
) -> Option<Simplification> {
    if insn.src.len() != 2 {
        return None;
    }
    let Some((mask, CmpDomain::Float)) = Outcomes::of_op(insn.op) else {
        return None;
    };
    mask.decide(possible_fcmp_outcomes(insn, consts, facts))
        .map(|v| Simplification::FoldToConst(i128::from(v)))
}

/// Which outcomes comparing `insn`'s two float operands can have, from what
/// is known of each: whether it is a constant (and which), whether the two
/// are the same value, and whether one is never below zero.
fn possible_fcmp_outcomes(insn: &Instruction, consts: &ConstMap, facts: &RunFacts) -> Outcomes {
    let (lhs, rhs) = (insn.src[0], insn.src[1]);
    let width = insn.operand_width().max(1);
    if consts.same(lhs, rhs, width) {
        return CmpDomain::Float.reflexive();
    }
    // A constant is compared as the value it has in the operands' format. A
    // `FloatVal` carries a literal at 128 significand bits and rounds to the
    // target once on the way out, so comparing unrounded makes
    // `0.1 + 0.2 == 0.3` true, which in `double` it is not -- and makes
    // `1e39f` finite, which in `float` it is not. Not knowing the format
    // means not knowing the value.
    let fmt = facts.fp_format(insn.operand_type());
    let known = |id| Some(consts.fget(id, width)?.round_to_format(fmt?));
    let never_below = |id: PseudoId| facts.never_lt_zero.contains(&consts.root(id, width));
    match (known(lhs), known(rhs)) {
        (Some(a), Some(b)) => Outcomes::of_ordering(a.cmp_value(b)),
        (None, Some(c)) => possible_against(c, never_below(lhs)),
        (Some(c), None) => possible_against(c, never_below(rhs)).mirror(),
        (None, None) => CmpDomain::Float.all(),
    }
}

/// Fold float arithmetic over two constants.
fn simplify_fbinop(
    insn: &Instruction,
    consts: &ConstMap,
    facts: &RunFacts,
) -> Option<Simplification> {
    if insn.src.len() != 2 {
        return None;
    }
    let width = insn.size.max(1);
    let a = consts.fget(insn.src[0], width)?;
    let b = consts.fget(insn.src[1], width)?;
    let fmt = facts.fp_format(insn.typ)?;
    eval_fbinop(insn.op, fmt, a, b).map(Simplification::FoldToFloat)
}

/// Fold `Fma` of three constants.
fn simplify_fternop(
    insn: &Instruction,
    consts: &ConstMap,
    facts: &RunFacts,
) -> Option<Simplification> {
    let width = insn.size.max(1);
    let (Some(fmt), [a, b, c]) = (facts.fp_format(insn.typ), insn.src.as_slice()) else {
        return None;
    };
    let a = consts.fget(*a, width)?;
    let b = consts.fget(*b, width)?;
    let c = consts.fget(*c, width)?;
    eval_fternop(insn.op, fmt, a, b, c).map(Simplification::FoldToFloat)
}

/// Fold `FNeg` of a constant.
///
/// This is what makes a negative float literal a constant at all: `-1.5` is
/// parsed as a negation of `1.5`, exactly as `-1` is of `1`, and the integer
/// half has always folded here.
fn simplify_funop(
    insn: &Instruction,
    consts: &ConstMap,
    facts: &RunFacts,
) -> Option<Simplification> {
    if insn.src.len() != 1 {
        return None;
    }
    let a = consts.fget(insn.src[0], insn.size.max(1))?;
    let fmt = facts.fp_format(insn.typ)?;
    eval_funop(insn.op, fmt, a).map(Simplification::FoldToFloat)
}

/// Fold a float-to-float conversion of a constant, to `typ`'s format.
fn simplify_fcvtf(
    insn: &Instruction,
    consts: &ConstMap,
    facts: &RunFacts,
) -> Option<Simplification> {
    if insn.src.len() != 1 {
        return None;
    }
    let a = consts.fget(insn.src[0], insn.operand_width())?;
    let src_fmt = facts.fp_format(insn.operand_type())?;
    let dst_fmt = facts.fp_format(insn.typ)?;
    eval_fcvtf(insn.op, src_fmt, dst_fmt, a).map(Simplification::FoldToFloat)
}

/// Fold a float-to-integer conversion of a constant, or a `Signbit` of one,
/// which has the same shape.
///
/// The source format comes from `src_typ`, not `typ`: a conversion's `typ` is
/// the integer it produces, and the width it reads is the separate
/// `src_size` ([`Instruction::operand_width`]).
fn simplify_fcvt(
    insn: &Instruction,
    consts: &ConstMap,
    facts: &RunFacts,
) -> Option<Simplification> {
    if insn.src.len() != 1 {
        return None;
    }
    let a = consts.fget(insn.src[0], insn.operand_width())?;
    let fmt = facts.fp_format(insn.operand_type())?;
    eval_fcvt(insn.op, insn.size.max(1), fmt, a).map(Simplification::FoldToConst)
}

/// Fold an operation with no algebraic identities -- `-x`, `~x`, a
/// population count -- when every operand is a known constant.
fn simplify_of_consts(insn: &Instruction, consts: &ConstMap) -> Option<Simplification> {
    let mut ops = [0i128; 2];
    if insn.src.len() > ops.len() {
        return None;
    }
    for (slot, s) in ops.iter_mut().zip(&insn.src) {
        *slot = consts.get(*s)?;
    }
    fold_with(insn, &ops[..insn.src.len()])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::float::{FpFormat, NanKind};
    use crate::ir::constfold::bit_opcode;
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

        let changed = run(&mut func, &host_types());
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

        let changed = run(&mut func, &host_types());
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

        let changed = run(&mut func, &host_types());
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

        let changed = run(&mut func, &host_types());
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

        let changed = run(&mut func, &host_types());
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

        let changed = run(&mut func, &host_types());
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

        let changed = run(&mut func, &host_types());
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

        let changed = run(&mut func, &host_types());
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

        let changed = run(&mut func, &host_types());
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

        let changed = run(&mut func, &host_types());
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

        let changed = run(&mut func, &host_types());
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

        let changed = run(&mut func, &host_types());
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

        let changed = run(&mut func, &host_types());
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);

        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(0));
    }

    /// A 128-bit result proved constant is left alone, as `sccp` leaves it:
    /// a copy of a minted constant has no `SetVal` to give it its sixteen
    /// bytes. The 32-bit fold beside it still happens, and two folds to one
    /// value share one minted constant.
    #[test]
    fn a_128_bit_fold_is_declined_and_mints_are_shared() {
        let types = host_types();
        let mut func = make_test_func_with_insn(
            Instruction::binop(
                Opcode::Shl,
                PseudoId(2),
                PseudoId(0),
                PseudoId(1),
                types.uint128_id,
                128,
            ),
            vec![
                Pseudo::val(PseudoId(0), 0),
                Pseudo::reg(PseudoId(1), 1),
                Pseudo::reg(PseudoId(2), 2),
                Pseudo::reg(PseudoId(3), 3),
                Pseudo::reg(PseudoId(4), 4),
                Pseudo::reg(PseudoId(5), 5),
            ],
        );
        for t in [3, 4] {
            func.blocks[0].insns.insert(
                2,
                Instruction::binop(
                    Opcode::Mul,
                    PseudoId(t),
                    PseudoId(5),
                    PseudoId(0),
                    types.int_id,
                    32,
                ),
            );
        }
        let before = func.pseudos.len();

        assert!(run(&mut func, &types));
        let insns = &func.blocks[0].insns;
        assert_eq!(insns[1].op, Opcode::Shl, "the 128-bit shift stays");
        assert_eq!((insns[2].op, insns[3].op), (Opcode::Copy, Opcode::Copy));
        assert_eq!(insns[2].src, insns[3].src, "one minted zero");
        assert_eq!(func.pseudos.len(), before + 1);
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

        let changed = run(&mut func, &host_types());
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

        let changed = run(&mut func, &host_types());
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

        let changed = run(&mut func, &host_types());
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

        let changed = run(&mut func, &host_types());
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
        let insn = Instruction::compare(
            Opcode::SetEq,
            PseudoId(2),
            (PseudoId(0), PseudoId(1)),
            (types.int_id, 32),
            (int_type(), 32),
        );
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 5),
            Pseudo::val(PseudoId(1), 5),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func, &host_types());
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
        let insn = Instruction::compare(
            Opcode::SetEq,
            PseudoId(2),
            (PseudoId(0), PseudoId(1)),
            (types.int_id, 32),
            (int_type(), 32),
        );
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 5),
            Pseudo::val(PseudoId(1), 3),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func, &host_types());
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
        let insn = Instruction::compare(
            Opcode::SetLt,
            PseudoId(2),
            (PseudoId(0), PseudoId(1)),
            (types.int_id, 32),
            (int_type(), 32),
        );
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 3),
            Pseudo::val(PseudoId(1), 5),
            Pseudo::reg(PseudoId(2), 2),
        ];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func, &host_types());
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
        let insn = Instruction::compare(
            Opcode::SetEq,
            PseudoId(1),
            (PseudoId(0), PseudoId(0)),
            (types.int_id, 32),
            (int_type(), 32),
        );
        let pseudos = vec![Pseudo::reg(PseudoId(0), 0), Pseudo::reg(PseudoId(1), 1)];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func, &host_types());
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
        let insn = Instruction::compare(
            Opcode::SetLt,
            PseudoId(1),
            (PseudoId(0), PseudoId(0)),
            (types.int_id, 32),
            (int_type(), 32),
        );
        let pseudos = vec![Pseudo::reg(PseudoId(0), 0), Pseudo::reg(PseudoId(1), 1)];
        let mut func = make_test_func_with_insn(insn, pseudos);

        let changed = run(&mut func, &host_types());
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

        let changed = run(&mut func, &host_types());
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

        let changed = run(&mut func, &host_types());
        assert!(changed);

        let result_insn = &func.blocks[0].insns[1];
        assert_eq!(result_insn.op, Opcode::Copy);

        let new_const = func.get_pseudo(result_insn.src[0]).unwrap();
        assert_eq!(new_const.kind, PseudoKind::Val(-1));
    }

    /// A bit count of a constant folds to the count of the operand at the
    /// width the opcode names, and the copy that replaces it is an `int` --
    /// not the 64-bit operand width a `popcount64` or `ctz64` records in
    /// `src_size`.
    #[test]
    fn test_const_fold_bit_count() {
        let types = TypeTable::new(&Target::host());
        for (op, operand, count) in [
            (Opcode::Popcount64, 0xF0F0_F0F0_F0F0_F0F0_i128, 32),
            (Opcode::Popcount64, -1, 64),
            // Bits above the operand width are not counted.
            (Opcode::Popcount32, (1 << 40) | 7, 3),
            (Opcode::Popcount32, 0, 0),
            (Opcode::Ctz64, 1 << 40, 40),
            (Opcode::Ctz32, (1 << 40) | 8, 3),
            (Opcode::Clz64, 1, 63),
            (Opcode::Clz32, 1 << 40, 32),
        ] {
            let (_, size) = bit_opcode(op).unwrap();
            let mut insn = Instruction::unop(op, PseudoId(1), PseudoId(0), types.int_id, 32);
            insn.src_typ = Some(types.ulonglong_id);
            insn.src_size = size;
            let pseudos = vec![
                Pseudo::val(PseudoId(0), operand),
                Pseudo::reg(PseudoId(1), 1),
            ];
            let mut func = make_test_func_with_insn(insn, pseudos);

            assert!(run(&mut func, &host_types()), "{op:?} of {operand:#x}");
            let result_insn = &func.blocks[0].insns[1];
            assert_eq!(result_insn.op, Opcode::Copy);
            assert_eq!(result_insn.typ, Some(types.int_id));
            assert_eq!(result_insn.size, 32, "the count is an int");
            assert_eq!(func.const_val(result_insn.src[0]), Some(count));
        }
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

    /// The type table every test drives the pass with.
    fn host_types() -> TypeTable {
        TypeTable::new(&Target::host())
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
        assert!(run(&mut func, &host_types()));

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
        assert!(run(&mut func, &host_types()));
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
        assert!(
            run(&mut func, &host_types()),
            "one pass must fold the whole chain"
        );

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
        run(&mut func, &host_types());

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
        run(&mut func, &host_types());
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
        run(&mut func, &host_types());
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
        assert!(run(&mut func, &host_types()));
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
        run(&mut func, &host_types());
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
        assert!(run(&mut func, &host_types()));
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
        assert!(run(&mut func, &host_types()));
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
        assert!(run(&mut func, &host_types()));
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
        let insns = vec![Instruction::compare(
            Opcode::SetLt,
            PseudoId(2),
            (PseudoId(0), PseudoId(1)),
            (int_id, 32),
            (int_type(), 32),
        )];
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 0xFFFF_FFFF),
            Pseudo::val(PseudoId(1), 0),
            Pseudo::reg(PseudoId(2), 0),
        ];
        let mut func = make_test_func_with_insns(insns, pseudos);
        assert!(run(&mut func, &host_types()));
        let c = insn_at(&func, 0);
        assert_eq!(func.const_val(c.src[0]), Some(1), "-1 < 0 is true");
    }

    /// A comparison folds at its operands' width, not its result's. The
    /// `_Bool` conversion is the shape that tells them apart: `setne.8` over
    /// two 32-bit operands. Reading the result width would compare only the
    /// low 8 bits, making `(_Bool)256` false.
    #[test]
    fn test_comparison_folds_at_its_operand_width() {
        let int_id = int_type();
        let bool_id = TypeTable::new(&Target::host()).bool_id;
        let insn = Instruction::compare(
            Opcode::SetNe,
            PseudoId(2),
            (PseudoId(0), PseudoId(1)),
            (int_id, 32),
            (bool_id, 8),
        );
        let pseudos = vec![
            Pseudo::val(PseudoId(0), 256),
            Pseudo::val(PseudoId(1), 0),
            Pseudo::reg(PseudoId(2), 0),
        ];
        let mut func = make_test_func_with_insns(vec![insn], pseudos);
        assert!(run(&mut func, &host_types()));
        let c = insn_at(&func, 0);
        assert_eq!(
            func.const_val(c.src[0]),
            Some(1),
            "256 != 0 is true; reading only the low 8 bits would say false"
        );
    }
    /// Promotion out of memory gives every use of a local its own `Copy`, so
    /// `x >> 0 != x` reaches the comparison as two distinct pseudos naming one
    /// value. Comparing raw ids missed it; comparing roots does not.
    #[test]
    fn test_identity_holds_across_copies_of_one_value() {
        let types = TypeTable::new(&Target::host());
        let mut func = make_test_func_with_insns(
            vec![
                // %1 = copy %0 ; %2 = copy %0 ; %3 = setne %1, %2
                Instruction::unop(Opcode::Copy, PseudoId(1), PseudoId(0), types.int_id, 32),
                Instruction::unop(Opcode::Copy, PseudoId(2), PseudoId(0), types.int_id, 32),
                Instruction::compare(
                    Opcode::SetNe,
                    PseudoId(3),
                    (PseudoId(1), PseudoId(2)),
                    (types.int_id, 32),
                    (int_type(), 32),
                ),
            ],
            vec![
                Pseudo::arg(PseudoId(0), 0),
                Pseudo::reg(PseudoId(1), 1),
                Pseudo::reg(PseudoId(2), 2),
                Pseudo::reg(PseudoId(3), 3),
            ],
        );
        assert!(run(&mut func, &host_types()));
        let cmp = &func.blocks[0].insns[3];
        assert_eq!(cmp.op, Opcode::Copy);
        assert_eq!(func.const_val(cmp.src[0]), Some(0), "x != x is 0");
    }

    /// A copy narrower than the width being read carries only its own width,
    /// so it must not make two pseudos look like one value.
    #[test]
    fn test_root_does_not_follow_a_narrowing_copy() {
        let types = TypeTable::new(&Target::host());
        let func = make_test_func_with_insns(
            vec![
                // %1 = copy.8 %0 -- truncates; %2 = copy.32 %0 -- does not
                Instruction::unop(Opcode::Copy, PseudoId(1), PseudoId(0), types.int_id, 8),
                Instruction::unop(Opcode::Copy, PseudoId(2), PseudoId(0), types.int_id, 32),
            ],
            vec![
                Pseudo::arg(PseudoId(0), 0),
                Pseudo::reg(PseudoId(1), 1),
                Pseudo::reg(PseudoId(2), 2),
            ],
        );
        let consts = ConstMap::new(&func);
        assert!(
            !consts.same(PseudoId(1), PseudoId(2), 32),
            "an 8-bit copy is not the 32-bit value"
        );
        assert!(
            consts.same(PseudoId(1), PseudoId(2), 8),
            "at 8 bits they agree"
        );
    }

    /// All-ones shifted arithmetically right is all-ones for every count. The
    /// operand only reads as `-1` when read *signed* at its own width, which
    /// is what `get` refuses to guess and `get_at` is told.
    #[test]
    fn test_arithmetic_shift_of_all_ones_folds() {
        let types = TypeTable::new(&Target::host());
        let mut func = make_test_func_with_insns(
            vec![Instruction::binop(
                Opcode::Asr,
                PseudoId(2),
                PseudoId(0),
                PseudoId(1),
                types.int_id,
                32,
            )],
            vec![
                Pseudo::val(PseudoId(0), -1),
                Pseudo::arg(PseudoId(1), 0),
                Pseudo::reg(PseudoId(2), 2),
            ],
        );
        assert!(run(&mut func, &host_types()));
        let insn = &func.blocks[0].insns[1];
        assert_eq!(insn.op, Opcode::Copy);
        assert_eq!(func.const_val(insn.src[0]), Some(-1));
    }

    /// And a *logical* shift of all-ones is not: it shifts in zeros, so
    /// `0xFFFFFFFFu >> x` is only all-ones when `x` is zero.
    #[test]
    fn test_logical_shift_of_all_ones_does_not_fold() {
        let types = TypeTable::new(&Target::host());
        let mut func = make_test_func_with_insns(
            vec![Instruction::binop(
                Opcode::Lsr,
                PseudoId(2),
                PseudoId(0),
                PseudoId(1),
                types.int_id,
                32,
            )],
            vec![
                Pseudo::val(PseudoId(0), -1),
                Pseudo::arg(PseudoId(1), 0),
                Pseudo::reg(PseudoId(2), 2),
            ],
        );
        run(&mut func, &host_types());
        assert_eq!(func.blocks[0].insns[1].op, Opcode::Lsr);
    }

    /// `get_at` reads what `get` refuses to guess at, and still declines when
    /// the chain narrowed below the width asked for.
    #[test]
    fn test_get_at_reads_an_ambiguous_constant_at_a_stated_width() {
        let types = TypeTable::new(&Target::host());
        let func = make_test_func_with_insns(
            vec![Instruction::unop(
                Opcode::Copy,
                PseudoId(1),
                PseudoId(0),
                types.int_id,
                32,
            )],
            vec![Pseudo::val(PseudoId(0), -1), Pseudo::reg(PseudoId(1), 1)],
        );
        let consts = ConstMap::new(&func);
        assert_eq!(consts.get(PseudoId(1)), None, "ambiguous at 32 bits");
        assert_eq!(consts.get_at(PseudoId(1), 32, true), Some(-1));
        assert_eq!(consts.get_at(PseudoId(1), 32, false), Some(4294967295));
        assert_eq!(
            consts.get_at(PseudoId(1), 64, true),
            None,
            "the chain narrowed to 32; 64 was never carried"
        );
    }
    /// Two comparisons of one operand pair answer each other without knowing
    /// the operands: disjoint orderings can never both hold, and exhaustive
    /// ones always leave at least one holding.
    #[test]
    fn test_disjoint_relational_pair_folds_to_zero() {
        // select(x == y, x != y, 0)  ->  0
        assert_eq!(select_over_pair(Opcode::SetEq, Opcode::SetNe, 0), Some(0));
        // select(x < y, x > y, 0)    ->  0
        assert_eq!(select_over_pair(Opcode::SetLt, Opcode::SetGt, 0), Some(0));
    }

    #[test]
    fn test_exhaustive_relational_pair_folds_to_one() {
        // select(x == y, 1, x != y)  ->  1
        assert_eq!(select_over_pair(Opcode::SetEq, Opcode::SetNe, 1), Some(1));
        // select(x >= y, 1, x < y)   ->  1
        assert_eq!(select_over_pair(Opcode::SetGe, Opcode::SetLt, 1), Some(1));
    }

    /// Overlapping but not exhaustive: nothing is decided.
    #[test]
    fn test_overlapping_relational_pair_does_not_fold() {
        // select(x <= y, x >= y, 0): both hold when x == y.
        assert_eq!(select_over_pair(Opcode::SetLe, Opcode::SetGe, 0), None);
        // select(x < y, 1, x > y): neither holds when x == y.
        assert_eq!(select_over_pair(Opcode::SetLt, Opcode::SetGt, 1), None);
    }

    /// A signed and an unsigned comparison of one pair are not each other's
    /// complements, so their orderings must not be combined.
    #[test]
    fn test_signed_and_unsigned_are_not_combined() {
        // `x < y` signed with `x >= y` unsigned would look exhaustive.
        assert_eq!(select_over_pair(Opcode::SetLt, Opcode::SetAe, 1), None);
    }

    /// `(x < y) && (y < x)`: the same disjointness with the operands the
    /// other way round.
    #[test]
    fn test_mirrored_operands_are_recognized() {
        let types = TypeTable::new(&Target::host());
        let mut func = make_test_func_with_insns(
            vec![
                Instruction::compare(
                    Opcode::SetLt,
                    PseudoId(2),
                    (PseudoId(0), PseudoId(1)),
                    (types.int_id, 32),
                    (int_type(), 32),
                ),
                // operands swapped
                Instruction::compare(
                    Opcode::SetLt,
                    PseudoId(3),
                    (PseudoId(1), PseudoId(0)),
                    (types.int_id, 32),
                    (int_type(), 32),
                ),
                Instruction::select(
                    PseudoId(5),
                    PseudoId(2),
                    PseudoId(3),
                    PseudoId(4),
                    types.int_id,
                    32,
                ),
            ],
            vec![
                Pseudo::arg(PseudoId(0), 0),
                Pseudo::arg(PseudoId(1), 1),
                Pseudo::reg(PseudoId(2), 2),
                Pseudo::reg(PseudoId(3), 3),
                Pseudo::val(PseudoId(4), 0),
                Pseudo::reg(PseudoId(5), 5),
            ],
        );
        assert!(run(&mut func, &host_types()));
        let sel = &func.blocks[0].insns[3];
        assert_eq!(sel.op, Opcode::Copy);
        assert_eq!(func.const_val(sel.src[0]), Some(0));
    }

    /// `select(c, t, f)` with `c` and one arm comparing `%0` against `%1`,
    /// and the other arm the constant `other`. Returns the folded constant,
    /// or `None` when it did not fold.
    fn select_over_pair(cond_op: Opcode, arm_op: Opcode, other: i128) -> Option<i128> {
        select_over_typed_pair(cond_op, arm_op, other, false)
    }

    /// [`select_over_pair`] with both comparisons typed `double` when
    /// `float`, which is what a float comparison records as its operands.
    fn select_over_typed_pair(
        cond_op: Opcode,
        arm_op: Opcode,
        other: i128,
        float: bool,
    ) -> Option<i128> {
        let types = TypeTable::new(&Target::host());
        let (typ, size) = if float {
            (types.double_id, 64)
        } else {
            (types.int_id, 32)
        };
        // `other == 0` is `a && b`, so the comparison is the *true* arm;
        // `other == 1` is `a || b`, so it is the false arm.
        let (t, f) = if other == 0 {
            (PseudoId(3), PseudoId(4))
        } else {
            (PseudoId(4), PseudoId(3))
        };
        let mut func = make_test_func_with_insns(
            vec![
                Instruction::test_binary(
                    cond_op,
                    PseudoId(2),
                    (PseudoId(0), PseudoId(1)),
                    typ,
                    size,
                ),
                Instruction::test_binary(
                    arm_op,
                    PseudoId(3),
                    (PseudoId(0), PseudoId(1)),
                    typ,
                    size,
                ),
                Instruction::select(PseudoId(5), PseudoId(2), t, f, types.int_id, 32),
            ],
            vec![
                Pseudo::arg(PseudoId(0), 0),
                Pseudo::arg(PseudoId(1), 1),
                Pseudo::reg(PseudoId(2), 2),
                Pseudo::reg(PseudoId(3), 3),
                Pseudo::val(PseudoId(4), other),
                Pseudo::reg(PseudoId(5), 5),
            ],
        );
        run(&mut func, &host_types());
        let sel = &func.blocks[0].insns[3];
        if sel.op != Opcode::Copy {
            return None;
        }
        func.const_val(sel.src[0])
    }

    // Floating point
    //
    // Two things here would be silent if they went wrong. A float comparison
    // carries its *operand* type, so a folded one that keeps it puts an
    // integer result in an SSE register; and `FloatVal` holds more bits than
    // any target format, so an unrounded fold answers a question the program
    // did not ask.

    fn fval(id: u32, v: f64) -> Pseudo {
        Pseudo::fval(PseudoId(id), FloatVal::from_f64(v))
    }

    /// One `FCmp` over two float constants, folded; returns the constant.
    fn fold_fcmp(op: Opcode, a: f64, b: f64) -> Option<i128> {
        let types = TypeTable::new(&Target::host());
        let mut func = make_test_func_with_insns(
            vec![Instruction::test_binary(
                op,
                PseudoId(2),
                (PseudoId(0), PseudoId(1)),
                types.double_id,
                64,
            )],
            vec![fval(0, a), fval(1, b), Pseudo::reg(PseudoId(2), 2)],
        );
        run(&mut func, &host_types());
        let insn = insn_at(&func, 0);
        if insn.op != Opcode::Copy {
            return None;
        }
        func.const_val(insn.src[0])
    }

    #[test]
    fn float_comparisons_fold_over_constants() {
        for (op, a, b, want) in [
            (Opcode::FCmpOEq, 1.0, 1.0, 1),
            (Opcode::FCmpOEq, 1.0, 2.0, 0),
            (Opcode::FCmpONe, 1.0, 2.0, 1),
            (Opcode::FCmpOLt, 1.0, 2.0, 1),
            (Opcode::FCmpOLt, 2.0, 1.0, 0),
            (Opcode::FCmpOLe, 2.0, 2.0, 1),
            (Opcode::FCmpOGt, 2.0, 1.0, 1),
            (Opcode::FCmpOGe, 1.0, 2.0, 0),
            // C has one zero.
            (Opcode::FCmpOEq, 0.0, -0.0, 1),
        ] {
            assert_eq!(fold_fcmp(op, a, b), Some(want), "{op:?} {a} {b}");
        }
    }

    /// `FCmpONe` is the *unordered* form -- C's `!=`, true when either side
    /// is a NaN -- and every other arm is ordered. The opcode names do not
    /// say so, and both backends emit it this way.
    #[test]
    fn nan_is_unequal_to_itself_and_unordered_with_everything_else() {
        let nan = f64::NAN;
        assert_eq!(fold_fcmp(Opcode::FCmpONe, nan, nan), Some(1));
        assert_eq!(fold_fcmp(Opcode::FCmpONe, nan, 1.0), Some(1));
        for op in [
            Opcode::FCmpOEq,
            Opcode::FCmpOLt,
            Opcode::FCmpOLe,
            Opcode::FCmpOGt,
            Opcode::FCmpOGe,
        ] {
            assert_eq!(fold_fcmp(op, nan, 1.0), Some(0), "{op:?}");
            assert_eq!(fold_fcmp(op, 1.0, nan), Some(0), "{op:?}");
        }
    }

    /// The folded copy must carry the comparison's *result* type. Keeping the
    /// operand type made the backend return the constant in `%xmm0` for a
    /// function returning `int`.
    #[test]
    fn a_folded_float_comparison_is_typed_int_not_double() {
        let types = TypeTable::new(&Target::host());
        let mut func = make_test_func_with_insns(
            vec![Instruction::compare(
                Opcode::FCmpOLt,
                PseudoId(2),
                (PseudoId(0), PseudoId(1)),
                (types.double_id, 64),
                (int_type(), 32),
            )],
            vec![fval(0, 1.0), fval(1, 2.0), Pseudo::reg(PseudoId(2), 2)],
        );
        assert!(run(&mut func, &host_types()));
        let insn = insn_at(&func, 0);
        assert_eq!(insn.op, Opcode::Copy);
        assert_eq!(insn.typ, Some(types.int_id));
        assert_eq!(insn.size, types.size_bits(types.int_id));
    }

    /// `fabs(x) < 0.0` is false for every `x`, a NaN included, because an
    /// unordered `<` is false as well. The value itself is unknown. So is
    /// `sqrt(x) < 0.0`: the root of `-0` is `-0`, which is not below zero,
    /// and of anything else below zero a NaN.
    #[test]
    fn fabs_is_never_below_zero() {
        for (unary, op, fabs_first) in [
            (Opcode::Fabs, Opcode::FCmpOLt, true),
            (Opcode::Fabs, Opcode::FCmpOGt, false),
            (Opcode::Sqrt, Opcode::FCmpOLt, true),
            (Opcode::Sqrt, Opcode::FCmpOGt, false),
        ] {
            let types = TypeTable::new(&Target::host());
            let (l, r) = if fabs_first {
                (PseudoId(1), PseudoId(2))
            } else {
                (PseudoId(2), PseudoId(1))
            };
            let mut func = make_test_func_with_insns(
                vec![
                    Instruction::new(unary)
                        .with_target(PseudoId(1))
                        .with_src(PseudoId(0))
                        .with_type_and_size(types.double_id, 64),
                    Instruction::test_binary(op, PseudoId(3), (l, r), types.double_id, 64),
                ],
                vec![
                    Pseudo::arg(PseudoId(0), 0),
                    Pseudo::reg(PseudoId(1), 1),
                    fval(2, 0.0),
                    Pseudo::reg(PseudoId(3), 3),
                ],
            );
            assert!(run(&mut func, &host_types()), "{op:?}");
            let insn = insn_at(&func, 1);
            assert_eq!(insn.op, Opcode::Copy, "{op:?}");
            assert_eq!(func.const_val(insn.src[0]), Some(0), "{op:?}");
        }
    }

    /// The weaker neighbours must not fold: `fabs(x) <= 0.0` is true for a
    /// zero argument, and `fabs(x) >= 0.0` is false for a NaN one.
    #[test]
    fn the_non_strict_fabs_comparisons_do_not_fold() {
        for op in [Opcode::FCmpOLe, Opcode::FCmpOGe, Opcode::FCmpOEq] {
            let types = TypeTable::new(&Target::host());
            let mut func = make_test_func_with_insns(
                vec![
                    Instruction::new(Opcode::Fabs)
                        .with_target(PseudoId(1))
                        .with_src(PseudoId(0))
                        .with_type_and_size(types.double_id, 64),
                    Instruction::test_binary(
                        op,
                        PseudoId(3),
                        (PseudoId(1), PseudoId(2)),
                        types.double_id,
                        64,
                    ),
                ],
                vec![
                    Pseudo::arg(PseudoId(0), 0),
                    Pseudo::reg(PseudoId(1), 1),
                    fval(2, 0.0),
                    Pseudo::reg(PseudoId(3), 3),
                ],
            );
            run(&mut func, &host_types());
            assert_eq!(insn_at(&func, 1).op, op, "{op:?} must survive");
        }
    }

    /// One `FCmp` of the argument `x` against the constant `c`, written
    /// `x op c` or, when `const_first`, `c op x`, at `double` or at `float`.
    /// Returns the folded constant, or `None` when it did not fold.
    fn fcmp_vs_const(op: Opcode, c: f64, const_first: bool, float: bool) -> Option<i128> {
        let types = TypeTable::new(&Target::host());
        let (typ, size) = if float {
            (types.float_id, 32)
        } else {
            (types.double_id, 64)
        };
        let (l, r) = if const_first {
            (PseudoId(1), PseudoId(0))
        } else {
            (PseudoId(0), PseudoId(1))
        };
        let mut func = make_test_func_with_insns(
            vec![Instruction::test_binary(op, PseudoId(2), (l, r), typ, size)],
            vec![
                Pseudo::arg(PseudoId(0), 0),
                fval(1, c),
                Pseudo::reg(PseudoId(2), 2),
            ],
        );
        run(&mut func, &host_types());
        let insn = insn_at(&func, 0);
        if insn.op != Opcode::Copy {
            return None;
        }
        func.const_val(insn.src[0])
    }

    const FCMPS: [Opcode; 6] = [
        Opcode::FCmpOEq,
        Opcode::FCmpONe,
        Opcode::FCmpOLt,
        Opcode::FCmpOLe,
        Opcode::FCmpOGt,
        Opcode::FCmpOGe,
    ];

    /// Nothing is ordered with a NaN, so a comparison against a NaN constant
    /// is decided whatever the other side is: every ordered predicate is 0,
    /// and `!=` -- the unordered one -- is 1.
    #[test]
    fn a_comparison_with_a_nan_constant_folds_whatever_the_other_side() {
        for op in FCMPS {
            let want = i128::from(op == Opcode::FCmpONe);
            for const_first in [false, true] {
                for float in [false, true] {
                    assert_eq!(
                        fcmp_vs_const(op, f64::NAN, const_first, float),
                        Some(want),
                        "{op:?} const_first={const_first} float={float}"
                    );
                }
            }
        }
    }

    /// Nothing is above `+Inf` or below `-Inf`, a NaN included. But a NaN
    /// is not *at or below* `+Inf` either, so `x <= +Inf` must survive, and
    /// so must every predicate that some ordinary `x` makes true and another
    /// false.
    #[test]
    fn a_comparison_with_an_infinity_folds_only_the_impossible_side() {
        let inf = f64::INFINITY;
        // (op, constant, constant written first)
        for (op, c, const_first) in [
            (Opcode::FCmpOGt, inf, false),  // x > +Inf
            (Opcode::FCmpOLt, inf, true),   // +Inf < x
            (Opcode::FCmpOLt, -inf, false), // x < -Inf
            (Opcode::FCmpOGt, -inf, true),  // -Inf > x
        ] {
            assert_eq!(
                fcmp_vs_const(op, c, const_first, false),
                Some(0),
                "{op:?} {c} const_first={const_first}"
            );
        }
        for (op, c, const_first) in [
            (Opcode::FCmpOLe, inf, false), // false for a NaN, true otherwise
            (Opcode::FCmpOGe, -inf, false),
            (Opcode::FCmpOGe, inf, true),
            (Opcode::FCmpOLt, inf, false),
            (Opcode::FCmpOGe, inf, false), // x == +Inf
            (Opcode::FCmpOEq, inf, false),
            (Opcode::FCmpONe, inf, false),
            (Opcode::FCmpOGt, 1.0, false),
            (Opcode::FCmpOLt, f64::MAX, false),
        ] {
            assert_eq!(
                fcmp_vs_const(op, c, const_first, false),
                None,
                "{op:?} {c} const_first={const_first} must survive"
            );
        }
    }

    /// The constant is compared at the operands' format: `1e39` is finite as
    /// a `double` and `+Inf` as a `float`.
    #[test]
    fn an_infinity_is_recognized_after_rounding_to_the_format() {
        assert_eq!(fcmp_vs_const(Opcode::FCmpOGt, 1e39, false, true), Some(0));
        assert_eq!(fcmp_vs_const(Opcode::FCmpOGt, 1e39, false, false), None);
    }

    /// `x < x` is false for every `x`, NaN included; `x == x` is *not*
    /// always true, and `x != x` is how `isnan` is spelled.
    #[test]
    fn a_float_compared_with_itself_folds_only_the_strict_forms() {
        for op in FCMPS {
            let types = TypeTable::new(&Target::host());
            let mut func = make_test_func_with_insns(
                vec![
                    Instruction::new(Opcode::Copy)
                        .with_target(PseudoId(1))
                        .with_src(PseudoId(0))
                        .with_type_and_size(types.double_id, 64),
                    Instruction::test_binary(
                        op,
                        PseudoId(2),
                        (PseudoId(0), PseudoId(1)),
                        types.double_id,
                        64,
                    ),
                ],
                vec![
                    Pseudo::arg(PseudoId(0), 0),
                    Pseudo::reg(PseudoId(1), 1),
                    Pseudo::reg(PseudoId(2), 2),
                ],
            );
            run(&mut func, &host_types());
            let insn = insn_at(&func, 1);
            if matches!(op, Opcode::FCmpOLt | Opcode::FCmpOGt) {
                assert_eq!(insn.op, Opcode::Copy, "{op:?}");
                assert_eq!(func.const_val(insn.src[0]), Some(0), "{op:?}");
            } else {
                assert_eq!(insn.op, op, "{op:?} must survive");
            }
        }
    }

    /// `select(c, t, f)` as [`select_over_pair`] builds it, but over two
    /// `double` arguments.
    fn select_over_float_pair(cond_op: Opcode, arm_op: Opcode, other: i128) -> Option<i128> {
        select_over_typed_pair(cond_op, arm_op, other, true)
    }

    /// The IEEE forms of the relational-pair folds: a float comparison has a
    /// fourth outcome, *unordered*, so `(x >= y) || (x < y)` is false for a
    /// NaN and must not fold, while `(x == y) || (x != y)` does -- `!=` is
    /// the unordered form.
    #[test]
    fn float_relational_pairs_fold_only_when_a_nan_agrees() {
        let pairs = [
            (Opcode::FCmpOLt, Opcode::FCmpOGt, 0, Some(0)), // (x<y) && (x>y)
            (Opcode::FCmpOEq, Opcode::FCmpONe, 0, Some(0)), // (x==y) && (x!=y)
            (Opcode::FCmpOLe, Opcode::FCmpOGt, 0, Some(0)), // (x<=y) && (x>y)
            (Opcode::FCmpOEq, Opcode::FCmpONe, 1, Some(1)), // (x==y) || (x!=y)
            (Opcode::FCmpONe, Opcode::FCmpOEq, 1, Some(1)), // (x!=y) || (x==y)
            (Opcode::FCmpOGe, Opcode::FCmpOLt, 1, None),    // NaN: neither
            (Opcode::FCmpOLe, Opcode::FCmpOGt, 1, None),    // NaN: neither
            (Opcode::FCmpOLe, Opcode::FCmpOGe, 0, None),    // x == y: both
            (Opcode::FCmpONe, Opcode::FCmpOLt, 0, None),    // x < y: both
        ];
        for (c, a, other, want) in pairs {
            assert_eq!(
                select_over_float_pair(c, a, other),
                want,
                "{c:?} {a:?} other={other}"
            );
        }
    }

    /// An integer and a float comparison of one pair never combine: `x < y`
    /// as integers and `x >= y` as floats cover everything only if they read
    /// the same operands the same way, and they do not.
    #[test]
    fn integer_and_float_comparisons_are_not_combined() {
        assert_eq!(
            select_over_typed_pair(Opcode::SetLt, Opcode::FCmpOGe, 1, true),
            None
        );
        assert_eq!(
            select_over_typed_pair(Opcode::FCmpOLt, Opcode::SetGt, 0, true),
            None
        );
    }

    /// `%2 = x != x; %3 = y != y; %4 = or %2, %3` -- the shape
    /// `__builtin_isunordered(x, y)` lowers to, for arguments `%0` and `%1`.
    /// Pseudos `%2`..`%4` are used.
    fn isunordered(first: u32, second: u32, types: &TypeTable) -> Vec<Instruction> {
        let dbl = types.double_id;
        vec![
            Instruction::compare(
                Opcode::FCmpONe,
                PseudoId(2),
                (PseudoId(first), PseudoId(first)),
                (dbl, 64),
                (int_type(), 32),
            ),
            Instruction::compare(
                Opcode::FCmpONe,
                PseudoId(3),
                (PseudoId(second), PseudoId(second)),
                (dbl, 64),
                (int_type(), 32),
            ),
            Instruction::binop(
                Opcode::Or,
                PseudoId(4),
                PseudoId(2),
                PseudoId(3),
                types.int_id,
                32,
            ),
        ]
    }

    /// Run `tail` after `isunordered(%0, %1)`; `%10` is the constant 0 and
    /// `%11` the constant 1, and the last instruction is the one inspected.
    /// Returns what it folded to, or `None`.
    fn after_isunordered(order: (u32, u32), tail: Vec<Instruction>) -> Option<i128> {
        let types = TypeTable::new(&Target::host());
        let mut insns = isunordered(order.0, order.1, &types);
        insns.extend(tail);
        let last = insns.len() - 1;
        let mut pseudos = vec![Pseudo::arg(PseudoId(0), 0), Pseudo::arg(PseudoId(1), 1)];
        pseudos.extend((2..10).map(|i| Pseudo::reg(PseudoId(i), i)));
        pseudos.push(Pseudo::val(PseudoId(10), 0));
        pseudos.push(Pseudo::val(PseudoId(11), 1));
        let mut func = make_test_func_with_insns(insns, pseudos);
        run(&mut func, &host_types());
        let insn = insn_at(&func, last);
        if insn.op != Opcode::Copy {
            return None;
        }
        func.const_val(insn.src[0])
    }

    fn fcmp(op: Opcode, target: u32, l: u32, r: u32) -> Instruction {
        let types = TypeTable::new(&Target::host());
        Instruction::test_binary(
            op,
            PseudoId(target),
            (PseudoId(l), PseudoId(r)),
            types.double_id,
            64,
        )
    }

    fn select(target: u32, c: u32, t: u32, f: u32) -> Instruction {
        let types = TypeTable::new(&Target::host());
        Instruction::select(
            PseudoId(target),
            PseudoId(c),
            PseudoId(t),
            PseudoId(f),
            types.int_id,
            32,
        )
    }

    /// `isunordered(x, y) || x >= y || x < y` covers all four outcomes, and
    /// so does it with the operands of `isunordered` swapped and the
    /// relationals mirrored. Dropping any one of the three leaves a gap.
    #[test]
    fn isunordered_with_the_ordered_outcomes_is_always_true() {
        for order in [(0, 1), (1, 0)] {
            // isunordered || x >= y || y < x: less is missing.
            let gap = vec![
                fcmp(Opcode::FCmpOGe, 5, 0, 1),
                select(6, 4, 11, 5),
                fcmp(Opcode::FCmpOLt, 7, 1, 0),
                select(8, 6, 11, 7),
            ];
            assert_eq!(after_isunordered(order, gap), None, "{order:?}");
            // isunordered(y, x) || x <= y || y < x: compare-fp-3's test6.
            let mirrored = vec![
                fcmp(Opcode::FCmpOLe, 5, 0, 1),
                select(6, 4, 11, 5),
                fcmp(Opcode::FCmpOLt, 7, 1, 0),
                select(8, 6, 11, 7),
            ];
            assert_eq!(after_isunordered(order, mirrored), Some(1), "{order:?}");
            let full = vec![
                fcmp(Opcode::FCmpOGe, 5, 0, 1),
                select(6, 4, 11, 5),
                fcmp(Opcode::FCmpOLt, 7, 0, 1),
                select(8, 6, 11, 7),
            ];
            assert_eq!(after_isunordered(order, full), Some(1), "{order:?}");
            let no_unordered = vec![
                fcmp(Opcode::FCmpOGe, 5, 0, 1),
                fcmp(Opcode::FCmpOLt, 6, 0, 1),
                select(7, 5, 11, 6),
            ];
            assert_eq!(after_isunordered(order, no_unordered), None, "{order:?}");
        }
    }

    /// `isunordered(x, y) || !isunordered(x, y)`, with `!` lowered to
    /// `== 0` and the second call a separate pair of comparisons.
    #[test]
    fn isunordered_or_its_negation_is_always_true() {
        let tail = vec![
            fcmp(Opcode::FCmpONe, 5, 1, 1),
            fcmp(Opcode::FCmpONe, 6, 0, 0),
            Instruction::binop(
                Opcode::Or,
                PseudoId(7),
                PseudoId(5),
                PseudoId(6),
                host_types().int_id,
                32,
            ),
            Instruction::compare(
                Opcode::SetEq,
                PseudoId(8),
                (PseudoId(7), PseudoId(10)),
                (host_types().int_id, 32),
                (host_types().int_id, 32),
            ),
            select(9, 4, 11, 8),
        ];
        assert_eq!(after_isunordered((0, 1), tail), Some(1));
        // `isunordered(x, y) && x == y` is never true.
        let tail = vec![fcmp(Opcode::FCmpOEq, 5, 1, 0), select(6, 4, 5, 10)];
        assert_eq!(after_isunordered((0, 1), tail), Some(0));
        // `isunordered(x, y) || x == y` is not decided.
        let tail = vec![fcmp(Opcode::FCmpOEq, 5, 0, 1), select(6, 4, 11, 5)];
        assert_eq!(after_isunordered((0, 1), tail), None);
    }

    /// One NaN test is not the pair's: `x != x || x < y` is not
    /// `isunordered(x, y) || x < y`, since `y` alone may be the NaN.
    #[test]
    fn one_self_test_is_not_an_unordered_pair() {
        let tail = vec![
            fcmp(Opcode::FCmpONe, 5, 0, 0),
            fcmp(Opcode::FCmpOGe, 6, 0, 1),
            select(7, 5, 11, 6),
            fcmp(Opcode::FCmpOLt, 8, 0, 1),
            select(9, 7, 11, 8),
        ];
        assert_eq!(after_isunordered((0, 1), tail), None);
    }

    /// One `FCvtS`/`FCvtU` of a float constant, folded.
    fn fold_fcvt(op: Opcode, v: f64, dst_size: u32) -> Option<i128> {
        let types = TypeTable::new(&Target::host());
        let mut insn = Instruction::new(op)
            .with_target(PseudoId(1))
            .with_src(PseudoId(0))
            .with_type_and_size(types.int_id, dst_size);
        insn.src_typ = Some(types.double_id);
        insn.src_size = 64;
        let mut func =
            make_test_func_with_insns(vec![insn], vec![fval(0, v), Pseudo::reg(PseudoId(1), 1)]);
        run(&mut func, &host_types());
        let got = insn_at(&func, 0);
        if got.op != Opcode::Copy {
            return None;
        }
        func.const_val(got.src[0])
    }

    #[test]
    fn float_to_integer_conversions_fold() {
        assert_eq!(fold_fcvt(Opcode::FCvtS, 1.0, 32), Some(1));
        assert_eq!(fold_fcvt(Opcode::FCvtS, 1.9, 32), Some(1));
        assert_eq!(fold_fcvt(Opcode::FCvtS, -1.9, 32), Some(-1));
        assert_eq!(fold_fcvt(Opcode::FCvtU, 3.5, 32), Some(3));
    }

    /// Out of the destination's range the conversion is undefined, and it
    /// folds as gcc folds it: saturated, and a NaN to 0. The hardware's
    /// answer differs by target (x86-64 gives the minimum).
    #[test]
    fn an_out_of_range_conversion_saturates() {
        assert_eq!(fold_fcvt(Opcode::FCvtS, 3e9, 32), Some(i32::MAX.into()));
        assert_eq!(fold_fcvt(Opcode::FCvtS, -3e9, 32), Some(i32::MIN.into()));
        assert_eq!(fold_fcvt(Opcode::FCvtU, -1.0, 32), Some(0));
        assert_eq!(fold_fcvt(Opcode::FCvtS, f64::NAN, 32), Some(0));
        assert_eq!(
            fold_fcvt(Opcode::FCvtS, f64::INFINITY, 64),
            Some(i64::MAX.into())
        );
        assert_eq!(fold_fcvt(Opcode::FCvtU, 1e10, 32), Some(u32::MAX.into()));
        // The same value the 32-bit case saturated does fit 64 bits.
        assert_eq!(fold_fcvt(Opcode::FCvtS, 3e9, 64), Some(3_000_000_000));
    }

    /// An infinity reaching a float-to-integer conversion through a
    /// narrowing `FCvtF` folds as well: the narrowing raises nothing for it.
    #[test]
    fn an_infinity_narrowed_then_converted_folds() {
        let types = TypeTable::new(&Target::host());
        let (d, f) = ((types.double_id, 64), (types.float_id, 32));
        assert_eq!(fold_fcvtf(f64::NEG_INFINITY, d, f), Some(f64::NEG_INFINITY));
        assert!(fold_fcvtf(f64::NAN, d, f).is_some_and(f64::is_nan));
    }

    /// `Signbit` of a constant folds to 0 or 1 for every operand, a zero and
    /// a NaN included, and the copy is the `int` in `typ`/`size` -- not the
    /// `double` operand in `src_typ`/`src_size`.
    #[test]
    fn signbit_of_a_constant_folds() {
        let nan = f64::from_bits(0x7ff8_0000_0000_1234);
        for (v, want) in [
            (-1.5, 1),
            (1.5, 0),
            (-0.0, 1),
            (0.0, 0),
            (f64::NEG_INFINITY, 1),
            (nan, 0),
            (-nan, 1),
        ] {
            assert_eq!(fold_fcvt(Opcode::Signbit, v, 32), Some(want), "{v}");
        }
        let types = TypeTable::new(&Target::host());
        let mut insn = Instruction::new(Opcode::Signbit)
            .with_target(PseudoId(1))
            .with_src(PseudoId(0))
            .with_type_and_size(types.int_id, 32);
        insn.src_typ = Some(types.double_id);
        insn.src_size = 64;
        let mut func =
            make_test_func_with_insns(vec![insn], vec![fval(0, -2.0), Pseudo::reg(PseudoId(1), 1)]);
        assert!(run(&mut func, &host_types()));
        let got = insn_at(&func, 0);
        assert_eq!(got.op, Opcode::Copy);
        assert_eq!((got.typ, got.size), (Some(types.int_id), 32));
    }

    /// `CopySign` of constants folds at every width and for every pair: the
    /// sign of a zero, an infinity or a NaN is taken, and a NaN in the first
    /// operand keeps its payload.
    #[test]
    fn copysign_of_constants_folds() {
        let types = TypeTable::new(&Target::host());
        let widths = [
            (types.float_id, 32),
            (types.double_id, 64),
            (types.longdouble_id, types.size_bits(types.longdouble_id)),
        ];
        let nan = f64::from_bits(0x7ff8_0000_0000_1234);
        for (typ, size) in widths {
            for (x, y, want) in [
                (1.5, -0.0, -1.5),
                (-1.5, 0.0, 1.5),
                (2.0, -nan, -2.0),
                (-2.0, nan, 2.0),
                (f64::INFINITY, -1.0, f64::NEG_INFINITY),
                (-3.0, f64::INFINITY, 3.0),
            ] {
                let got = fold_float(Opcode::CopySign, typ, size, &[x, y]);
                assert_eq!(
                    got.map(|(v, sz)| (v.to_f64(), sz)),
                    Some((want, size)),
                    "copysign({x}, {y}) at {size}"
                );
            }
            let zero = fold_float(Opcode::CopySign, typ, size, &[0.0, -1.0]).map(|(v, _)| v);
            assert!(
                zero.is_some_and(|v| v.is_zero() && v.sign_bit()),
                "-0.0 at {size}"
            );
        }
        let got = fold_float(Opcode::CopySign, types.double_id, 64, &[nan, -1.0]);
        let (neg, exp, sig) = FloatVal::from_f64(nan).key();
        assert!(!neg);
        assert_eq!(
            got.map(|(v, _)| v.key()),
            Some((true, exp, sig)),
            "only the sign bit of a NaN changes"
        );
    }

    /// One float binary/unary op over constants; returns the folded value
    /// and the `SetVal` the instruction became.
    fn fold_float(op: Opcode, typ: TypeId, size: u32, args: &[f64]) -> Option<(FloatVal, u32)> {
        let args: Vec<FloatVal> = args.iter().map(|v| FloatVal::from_f64(*v)).collect();
        fold_float_vals(op, typ, size, &args)
    }

    /// [`fold_float`] over exact constants, for a value an `f64` cannot
    /// spell: a `float` or `long double` NaN with a payload.
    fn fold_float_vals(
        op: Opcode,
        typ: TypeId,
        size: u32,
        args: &[FloatVal],
    ) -> Option<(FloatVal, u32)> {
        let target = PseudoId(args.len() as u32);
        let mut pseudos: Vec<Pseudo> = args
            .iter()
            .enumerate()
            .map(|(i, v)| Pseudo::fval(PseudoId(i as u32), *v))
            .collect();
        pseudos.push(Pseudo::reg(target, target.0));
        let insn = match args.len() {
            2 => Instruction::test_binary(op, target, (PseudoId(0), PseudoId(1)), typ, size),
            n => {
                let mut insn = Instruction::new(op)
                    .with_target(target)
                    .with_type_and_size(typ, size);
                insn.src = (0..n as u32).map(PseudoId).collect();
                insn
            }
        };
        let mut func = make_test_func_with_insns(vec![insn], pseudos);
        run(&mut func, &host_types());
        let got = insn_at(&func, 0);
        if got.op != Opcode::SetVal {
            return None;
        }
        let size = got.size;
        match func.get_pseudo(target).map(|p| p.kind.clone()) {
            Some(PseudoKind::FVal(v)) => Some((v, size)),
            _ => None,
        }
    }

    /// The fold works the other way round from the integer one: the target
    /// pseudo *becomes* the constant, and its instruction becomes the
    /// `SetVal` that gives it a width.
    #[test]
    fn float_arithmetic_folds_into_a_sized_setval() {
        let types = TypeTable::new(&Target::host());
        let cases: &[(Opcode, &[f64], f64)] = &[
            (Opcode::FNeg, &[1.5], -1.5),
            (Opcode::FAdd, &[1.5, 2.25], 3.75),
            (Opcode::FSub, &[1.5, 0.5], 1.0),
            (Opcode::FMul, &[1.5, 2.0], 3.0),
            (Opcode::FDiv, &[3.0, 2.0], 1.5),
        ];
        for (op, args, want) in cases {
            let got = fold_float(*op, types.double_id, 64, args);
            assert_eq!(
                got.map(|(v, sz)| (v.to_f64(), sz)),
                Some((*want, 64)),
                "{op:?}"
            );
        }
    }

    /// The width comes from the instruction, not from a default. An `FVal`
    /// with no `SetVal` is resolved at 64 bits, so a folded `float` emitted
    /// without one is read out of the wrong number of bytes.
    #[test]
    fn a_folded_float_keeps_its_own_width() {
        let types = TypeTable::new(&Target::host());
        let got = fold_float(Opcode::FAdd, types.float_id, 32, &[1.5, 2.25]);
        assert_eq!(got.map(|(v, sz)| (v.to_f64(), sz)), Some((3.75, 32)));
    }

    /// Rounded at the format the program computes in, not at the 128 bits a
    /// literal is carried in: `0.1 + 0.2` is a `double` sum of two `double`s.
    #[test]
    fn float_arithmetic_rounds_at_the_operand_format() {
        let types = TypeTable::new(&Target::host());
        let wide = fold_float(Opcode::FAdd, types.double_id, 64, &[0.1, 0.2]);
        assert_eq!(wide.map(|(v, _)| v.to_f64()), Some(0.1f64 + 0.2f64));
        assert_ne!(wide.map(|(v, _)| v.to_f64()), Some(0.3f64));

        // The same sum in `float` is a different number again.
        let narrow = fold_float(Opcode::FAdd, types.float_id, 32, &[0.1, 0.2]);
        assert_eq!(
            narrow.map(|(v, _)| v.to_f64() as f32),
            Some(0.1f32 + 0.2f32)
        );
    }

    /// Anything that raises a floating-point exception is left to run time:
    /// C lets a program read the flag, and folding would take it away.
    #[test]
    fn arithmetic_that_raises_is_not_folded() {
        let types = TypeTable::new(&Target::host());
        let d = types.double_id;
        assert_eq!(fold_float(Opcode::FDiv, d, 64, &[1.0, 0.0]), None);
        assert_eq!(fold_float(Opcode::FAdd, d, 64, &[f64::NAN, 1.0]), None);
        assert_eq!(fold_float(Opcode::FMul, d, 64, &[f64::INFINITY, 2.0]), None);
        // Overflow to infinity raises too, so the sum must survive.
        assert_eq!(fold_float(Opcode::FMul, d, 64, &[1e300, 1e300]), None);
        // A sign flip computes nothing and raises nothing, so it folds for
        // every operand.
        assert!(fold_float(Opcode::FNeg, d, 64, &[f64::INFINITY]).is_some());
    }

    /// `Fabs` of a constant folds at every width and for every operand: it
    /// clears the sign and nothing else, so `-0.0` becomes `+0.0`, an
    /// infinity folds, and a NaN keeps its payload.
    #[test]
    fn sqrt_of_a_constant_folds_where_the_answer_is_its_own() {
        let types = host_types();
        let widths = [
            (types.float_id, 32),
            (types.double_id, 64),
            (types.longdouble_id, types.size_bits(types.longdouble_id)),
        ];
        for (typ, size) in widths {
            for (arg, want) in [(4.0, 2.0), (0.25, 0.5), (f64::INFINITY, f64::INFINITY)] {
                let got = fold_float(Opcode::Sqrt, typ, size, &[arg]);
                assert_eq!(got.map(|(v, sz)| (v.to_f64(), sz)), Some((want, size)));
            }
            let zero = fold_float(Opcode::Sqrt, typ, size, &[-0.0]).map(|(v, _)| v);
            assert!(
                zero.is_some_and(|v| v.is_zero() && v.sign_bit()),
                "-0 at {size}"
            );
            // A domain error's NaN is the target's own, and not folded.
            assert!(fold_float(Opcode::Sqrt, typ, size, &[-1.0]).is_none());
        }
        // Rounded at the operand's format: the float root of 2 is not the
        // double one.
        let got = fold_float(Opcode::Sqrt, types.float_id, 32, &[2.0]).map(|(v, _)| v);
        assert_eq!(
            got.map(|v| v.to_bits(FpFormat::Binary32)),
            Some(0x3fb5_04f3)
        );
        let got = fold_float(Opcode::Sqrt, types.double_id, 64, &[2.0]).map(|(v, _)| v);
        assert_eq!(got.map(|v| v.to_f64()), Some(2f64.sqrt()));
    }

    /// The roundings of a constant fold at the operand's format, keeping the
    /// sign of a zero result; `rint` and `nearbyint` only where the answer
    /// is the same in every rounding direction.
    #[test]
    fn rounding_a_constant_folds_where_the_direction_cannot_matter() {
        use crate::float::IntegralRounding::*;
        let types = host_types();
        for (typ, size) in [(types.float_id, 32), (types.double_id, 64)] {
            let fold = |how, arg: f64| {
                fold_float(Opcode::RoundToIntegral(how), typ, size, &[arg]).map(|(v, _)| v)
            };
            for (how, arg, want) in [
                (Floor, -0.5, -1.0),
                (Ceil, 2.25, 3.0),
                (Trunc, -2.75, -2.0),
                (Round, 2.5, 3.0),
                (Round, -0.5, -1.0),
                (Rint, 4.0, 4.0),
                (NearbyInt, -8.0, -8.0),
            ] {
                assert_eq!(
                    fold(how, arg).map(|v| v.to_f64()),
                    Some(want),
                    "{how:?}({arg})"
                );
            }
            let zero = fold(Ceil, -0.5).unwrap();
            assert!(
                zero.is_zero() && zero.sign_bit(),
                "ceil(-0.5) is -0 at {size}"
            );
            assert!(fold(Rint, 2.5).is_none(), "rint(2.5) is the direction's");
            assert!(fold(NearbyInt, -0.25).is_none());
        }
    }

    /// `fmin` and `fmax` of constants fold -- a quiet NaN operand to the
    /// other, the zeros to -0 and +0 -- and `fma` rounds once.
    #[test]
    fn min_max_and_fma_of_constants_fold() {
        let types = host_types();
        let d = types.double_id;
        let val = |op, args: &[f64]| fold_float(op, d, 64, args).map(|(v, _)| v.to_f64().to_bits());
        assert_eq!(val(Opcode::FMin, &[1.0, 2.0]), Some(1f64.to_bits()));
        assert_eq!(val(Opcode::FMax, &[1.0, 2.0]), Some(2f64.to_bits()));
        assert_eq!(val(Opcode::FMin, &[f64::NAN, 3.0]), Some(3f64.to_bits()));
        assert_eq!(val(Opcode::FMin, &[0.0, -0.0]), Some(1 << 63));
        assert_eq!(val(Opcode::FMax, &[-0.0, 0.0]), Some(0));
        let (a, b) = (1.0 + f64::EPSILON, 1.0 - f64::EPSILON / 2.0);
        assert_eq!(
            val(Opcode::Fma, &[a, b, -1.0]),
            Some(a.mul_add(b, -1.0).to_bits())
        );
        assert_ne!(
            a.mul_add(b, -1.0),
            a * b - 1.0,
            "the case needs one rounding"
        );
        assert_eq!(val(Opcode::Fma, &[1.0, 1.0, f64::INFINITY]), None);
        let got = fold_float(Opcode::Fma, types.float_id, 32, &[2.0, 3.0, 4.0]);
        assert_eq!(got.map(|(v, _)| v.to_f64()), Some(10.0));
    }

    #[test]
    fn fabs_of_a_constant_folds_to_its_magnitude() {
        let types = TypeTable::new(&Target::host());
        let widths = [
            (types.float_id, 32),
            (types.double_id, 64),
            (types.longdouble_id, types.size_bits(types.longdouble_id)),
        ];
        for (typ, size) in widths {
            for (arg, want) in [(-1.5, 1.5), (2.0, 2.0), (f64::NEG_INFINITY, f64::INFINITY)] {
                let got = fold_float(Opcode::Fabs, typ, size, &[arg]);
                assert_eq!(
                    got.map(|(v, sz)| (v.to_f64(), sz)),
                    Some((want, size)),
                    "{arg}"
                );
            }
            let zero = fold_float(Opcode::Fabs, typ, size, &[-0.0]).map(|(v, _)| v);
            assert!(zero.is_some_and(|v| v.is_positive_zero()), "-0.0 at {size}");
        }
        let nan = f64::from_bits(0xfff8_0000_0000_1234);
        let got = fold_float(Opcode::Fabs, types.double_id, 64, &[nan]);
        let (neg, exp, sig) = FloatVal::from_f64(nan).key();
        assert!(neg);
        assert_eq!(
            got.map(|(v, _)| v.key()),
            Some((false, exp, sig)),
            "only the sign bit of a NaN changes"
        );
        // And the bits the constant is emitted as say the same.
        assert_eq!(
            got.map(|(v, _)| v.to_bits(FpFormat::Binary64)),
            Some(0x7ff8_0000_0000_1234)
        );
    }

    /// `FNeg` of a NaN constant flips its sign and keeps everything else --
    /// payload and quiet bit alike -- at every width. This is the fold that
    /// made `-__builtin_nan("0x1234")` a positive NaN at -O2: the folded
    /// value was right and its emission, through `f64::NAN`, was not, so the
    /// assertion is on the emitted bits.
    #[test]
    fn fneg_of_a_nan_constant_keeps_its_payload() {
        let types = TypeTable::new(&Target::host());
        let ld = types.longdouble_id;
        let ld_fmt = types.fp_format(ld).expect("long double is floating");
        // (type, width, quiet -0x1234 bits, signalling -0x1234 bits)
        let cases = [
            (types.float_id, 32, 0xffc0_1234, 0xff80_1234),
            (
                types.double_id,
                64,
                0xfff8_0000_0000_1234,
                0xfff0_0000_0000_1234,
            ),
            match ld_fmt {
                FpFormat::X87Extended => (
                    ld,
                    types.size_bits(ld),
                    0xffff_c000_0000_0000_1234,
                    0xffff_8000_0000_0000_1234,
                ),
                FpFormat::Binary128 => (
                    ld,
                    types.size_bits(ld),
                    0xffff_8000_0000_0000_0000_0000_0000_1234,
                    0xffff_0000_0000_0000_0000_0000_0000_1234,
                ),
                _ => (
                    ld,
                    types.size_bits(ld),
                    0xfff8_0000_0000_1234,
                    0xfff0_0000_0000_1234,
                ),
            },
        ];
        for (typ, size, quiet, signalling) in cases {
            let fmt = types.fp_format(typ).expect("a floating type");
            for (kind, want) in [(NanKind::Quiet, quiet), (NanKind::Signalling, signalling)] {
                let nan = FloatVal::nan_with_payload(fmt, 0x1234, kind);
                let got = fold_float_vals(Opcode::FNeg, typ, size, &[nan]);
                assert_eq!(
                    got.map(|(v, _)| v.to_bits(fmt)),
                    Some(want),
                    "{kind:?} NaN at {fmt:?}"
                );
            }
        }
    }

    /// One `FCvtF` from `src` to `dst`, folded.
    fn fold_fcvtf(v: f64, src: (TypeId, u32), dst: (TypeId, u32)) -> Option<f64> {
        let mut insn = Instruction::new(Opcode::FCvtF)
            .with_target(PseudoId(1))
            .with_src(PseudoId(0))
            .with_type_and_size(dst.0, dst.1);
        insn.src_typ = Some(src.0);
        insn.src_size = src.1;
        let mut func =
            make_test_func_with_insns(vec![insn], vec![fval(0, v), Pseudo::reg(PseudoId(1), 1)]);
        run(&mut func, &host_types());
        match func.get_pseudo(PseudoId(1)).map(|p| p.kind.clone()) {
            Some(PseudoKind::FVal(f)) => Some(f.to_f64()),
            _ => None,
        }
    }

    /// A conversion between float formats folds, rounding to the
    /// destination -- and refuses when the narrowing overflows.
    #[test]
    fn float_to_float_conversions_fold() {
        let types = TypeTable::new(&Target::host());
        let (d, f) = ((types.double_id, 64), (types.float_id, 32));
        assert_eq!(fold_fcvtf(1.5, d, f), Some(1.5));
        // Rounded to `float`, which cannot hold this exactly.
        assert_eq!(fold_fcvtf(0.1, d, f), Some(0.1f32 as f64));
        // Overflows on the way to `float`.
        assert_eq!(fold_fcvtf(1e300, d, f), None);
    }

    /// A conversion rounds at its *source* format before its destination.
    ///
    /// The operand is a literal carried at 128 significand bits, so it is
    /// not yet the value its own type holds. Going straight to the
    /// destination skips a rounding the program performs:
    /// `(float)(_Float16)0.3f16` is the nearest `float` to the nearest
    /// `_Float16` to `0.3`, which is not the nearest `float` to `0.3`.
    ///
    /// A unit test rather than an e2e one on purpose: x86-64 re-rounds
    /// through the half-precision bits when it emits the constant, so the
    /// host masks this entirely and only an aarch64 run showed it.
    #[test]
    fn a_conversion_rounds_at_its_source_format_first() {
        let types = TypeTable::new(&Target::host());
        let h = (types.float16_id, 16);
        let f = (types.float_id, 32);
        // 0.3 rounded to binary16 is 0x34CD, which is exactly
        // 1229 * 2^-12 -- written out rather than computed, so the
        // expectation does not come from the code under test.
        let want = 1229.0 / 4096.0;
        assert_eq!(fold_fcvtf(0.3, h, f), Some(want));
        assert_ne!(fold_fcvtf(0.3, h, f), Some(f64::from(0.3f32)));
    }

    /// A pseudo that means something beyond its value must not be converted.
    #[test]
    fn only_a_plain_temporary_becomes_a_constant() {
        let types = TypeTable::new(&Target::host());
        let mut func = make_test_func_with_insns(
            vec![Instruction::new(Opcode::FNeg)
                .with_target(PseudoId(1))
                .with_src(PseudoId(0))
                .with_type_and_size(types.double_id, 64)],
            vec![fval(0, 1.5), Pseudo::arg(PseudoId(1), 0)],
        );
        run(&mut func, &host_types());
        assert_eq!(insn_at(&func, 0).op, Opcode::FNeg, "an Arg is not a temp");
        assert!(!func.is_plain_temp(PseudoId(1)));
        // An id nothing has registered is a plain register, which is how
        // most temporaries arrive: the linearizer records only the pseudos
        // that are something else.
        assert!(func.is_plain_temp(PseudoId(97)));
    }

    // Integer width conversions
    //
    // Unary in the IR, carrying the *source* width in `src_size`. Reading the
    // operand at `size` instead is the identity for an extension, which is
    // how a negative `char` comes back positive.

    /// One `Sext`/`Zext`/`Trunc` of a constant, folded.
    fn fold_convert(op: Opcode, v: i128, src_size: u32, dst_size: u32) -> Option<i128> {
        let types = TypeTable::new(&Target::host());
        let mut insn = Instruction::new(op)
            .with_target(PseudoId(1))
            .with_src(PseudoId(0))
            .with_type_and_size(types.int_id, dst_size);
        insn.src_size = src_size;
        let mut func = make_test_func_with_insns(
            vec![insn],
            vec![Pseudo::val(PseudoId(0), v), Pseudo::reg(PseudoId(1), 1)],
        );
        run(&mut func, &host_types());
        let got = insn_at(&func, 0);
        if got.op != Opcode::Copy {
            return None;
        }
        func.const_val(got.src[0])
    }

    #[test]
    fn integer_conversions_fold_at_the_source_width() {
        // Sign extension reads the operand signed at its own width.
        assert_eq!(fold_convert(Opcode::Sext, 200, 8, 32), Some(-56));
        assert_eq!(fold_convert(Opcode::Sext, -56, 8, 32), Some(-56));
        assert_eq!(fold_convert(Opcode::Sext, 70000, 16, 32), Some(4464));
        // Zero extension reads it unsigned.
        assert_eq!(fold_convert(Opcode::Zext, 200, 8, 32), Some(200));
        assert_eq!(fold_convert(Opcode::Zext, -56, 8, 32), Some(200));
        assert_eq!(fold_convert(Opcode::Zext, -1, 32, 64), Some(0xFFFF_FFFF));
        // Truncation keeps the low bits, but only where they mean one
        // thing: see below.
        assert_eq!(fold_convert(Opcode::Trunc, 0x1234, 32, 8), Some(0x34));
    }

    /// A truncation's result is read at a *wider* width by whatever consumes
    /// it, and nothing in the IR says how it is widened: `-(unsigned char)200`
    /// is `trunc.32to8` feeding `neg.32` with no extension between them.
    /// Leaving the `Trunc` in place is what lets the backend's move decide,
    /// so a value that reads two ways must not be folded away.
    #[test]
    fn an_ambiguous_truncation_is_left_alone() {
        // 200 at eight bits is 200 unsigned and -56 signed.
        assert_eq!(fold_convert(Opcode::Trunc, 200, 32, 8), None);
        assert_eq!(fold_convert(Opcode::Trunc, -1, 32, 8), None);
        assert_eq!(fold_convert(Opcode::Trunc, 0xFF, 32, 8), None);
        // These read the same either way, so they fold.
        assert_eq!(fold_convert(Opcode::Trunc, 0x1200, 32, 8), Some(0));
        assert_eq!(fold_convert(Opcode::Trunc, 100, 32, 8), Some(100));
    }
}
