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
    at_width, cmp_mask, cmp_operand_width, eval_binop, eval_fbinop, eval_fcmp, eval_fcvt,
    eval_fcvtf, eval_funop, eval_unop, get_cmp_info, mirror_mask, result_type_of, unambiguous_at,
    CMP_ALL,
};
use super::{ConstValue, Function, Instruction, Opcode, PseudoId, PseudoKind};
use crate::float::FloatVal;
use crate::types::{TypeId, TypeTable};
use std::collections::{HashMap, HashSet};

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
    /// Every `PseudoKind::FVal` pseudo's value, kept apart from `vals`
    /// because the two never mix: no rule reads a float as an integer
    /// without going through a conversion opcode that says so.
    fvals: HashMap<PseudoId, FloatVal>,
    /// Target -> (source, the width at which the two are the same value).
    ///
    /// A `Copy` records its own operand width. A `Trunc` belongs here too
    /// and records the width it truncates *to*: a truncation is its operand
    /// read at that width, which is precisely what this field means, and
    /// recording it is what lets `(int)(signed char)200` fold. The value in
    /// the middle means two things, and only the extension that consumes it
    /// says which -- so it is reachable through [`Self::get_at`], which is
    /// told the signedness, and not through [`Self::get`], which is not.
    copies: HashMap<PseudoId, (PseudoId, u32)>,
}

impl ConstMap {
    fn new(func: &Function) -> Self {
        let mut vals = HashMap::new();
        let mut fvals = HashMap::new();
        for p in &func.pseudos {
            match &p.kind {
                PseudoKind::Val(v) => {
                    // I1 should make this unique. If it is not, a release
                    // build has no validator to say so, and folding the wrong
                    // one silently is worse than not folding: poison the
                    // entry.
                    if let Some(prev) = vals.insert(p.id, *v) {
                        if prev != *v {
                            vals.remove(&p.id);
                        }
                    }
                }
                PseudoKind::FVal(v) => {
                    // Poisoned the same way, and on the *encoding*: two
                    // constants that compare equal but are not the same
                    // value (the signed zeros) must not silently merge.
                    if let Some(prev) = fvals.insert(p.id, *v) {
                        if prev.key() != v.key() {
                            fvals.remove(&p.id);
                        }
                    }
                }
                _ => {}
            }
        }

        let mut copies: HashMap<PseudoId, (PseudoId, u32)> = HashMap::new();
        let mut poisoned: Vec<PseudoId> = Vec::new();
        for bb in &func.blocks {
            for insn in &bb.insns {
                if !matches!(insn.op, Opcode::Copy | Opcode::Trunc) || insn.src.len() != 1 {
                    continue;
                }
                if let Some(target) = insn.target {
                    let entry = (insn.src[0], insn.size.max(1));
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

        Self {
            vals,
            fvals,
            copies,
        }
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
            Simplification::FoldToFloat(v) => {
                self.fvals.insert(target, *v);
            }
            _ => {}
        }
    }

    /// The pseudo `id` ultimately copies from, reading it at `width` bits.
    ///
    /// Two values are the same value when they share a root, which is what
    /// the identity rules (`x - x`, `x & x`, `x == x`) actually need: after
    /// promotion out of memory every use of a local is its own `Copy`, so
    /// `x >> 0 != x` reaches the comparison as two distinct pseudos that are
    /// the same value. Comparing raw ids missed all of them.
    ///
    /// A copy *narrower* than `width` is not followed: it only carries its own
    /// width of the value, so treating it as value-preserving would equate two
    /// pseudos that differ above it.
    fn root(&self, id: PseudoId, width: u32) -> PseudoId {
        let mut cur = id;
        for _ in 0..=self.copies.len() {
            match self.copies.get(&cur) {
                Some(&(src, w)) if w >= width => cur = src,
                _ => return cur,
            }
        }
        cur
    }

    /// The constant at `id`, read at `size` bits in the given signedness.
    ///
    /// For a consumer that *knows* how to read its operands -- a comparison
    /// and a shift take their width and signedness from the opcode -- which
    /// is exactly the information [`Self::get`] refuses to guess. `get` is
    /// still right for everything else, and the two must not be merged: its
    /// guard is what stops an ambiguous value chaining into a second fold.
    ///
    /// Still `None` when the chain narrows *below* `size`, since the value
    /// was truncated before it got here.
    fn get_at(&self, id: PseudoId, size: u32, signed: bool) -> Option<i128> {
        let mut cur = id;
        let mut narrowest: Option<u32> = None;
        for _ in 0..=self.copies.len() {
            if let Some(&v) = self.vals.get(&cur) {
                return match narrowest {
                    Some(w) if w < size => None,
                    _ => Some(at_width(v, size, signed)),
                };
            }
            let &(src, width) = self.copies.get(&cur)?;
            narrowest = Some(narrowest.map_or(width, |w: u32| w.min(width)));
            cur = src;
        }
        None
    }

    /// The float constant at `id`, following `Copy` chains to their source.
    ///
    /// No width guard beyond the one [`Self::root`] applies: unlike an
    /// integer, a float is never narrowed by a `Copy` -- a change of format
    /// is an `FCvtF`, a separate instruction -- so the value at the root is
    /// the value here. What the caller still owes is rounding it to the
    /// format of the instruction consuming it, which a `FloatVal` has not
    /// been subjected to.
    fn fget(&self, id: PseudoId, width: u32) -> Option<FloatVal> {
        self.fvals.get(&self.root(id, width)).copied()
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

/// What is known about a pseudo that holds a comparison's result.
///
/// Built once per run, like `ConstMap`, and sound for the same reason: SSA
/// single-def makes "the comparison that defines %n" a fact about the whole
/// function rather than about a program point.
#[derive(Clone, Copy)]
struct CmpFact {
    /// Which of less/equal/greater make it true.
    mask: u8,
    lhs: PseudoId,
    rhs: PseudoId,
    /// A signed and an unsigned comparison over one pair are *not*
    /// comparable: `x < y` and `x > y` read signed are not complementary with
    /// the unsigned forms.
    signed: bool,
    width: u32,
}

/// Every pseudo defined by an integer comparison.
struct CmpFacts {
    facts: HashMap<PseudoId, CmpFact>,
}

impl CmpFacts {
    fn new(func: &Function, consts: &ConstMap) -> Self {
        let mut facts = HashMap::new();
        for bb in &func.blocks {
            for insn in &bb.insns {
                let (Some(target), Some(mask)) = (insn.target, cmp_mask(insn.op)) else {
                    continue;
                };
                if insn.src.len() != 2 {
                    continue;
                }
                let width = cmp_operand_width(insn);
                let signed = get_cmp_info(insn.op).map(|i| i.signed).unwrap_or(true);
                facts.insert(
                    target,
                    CmpFact {
                        mask,
                        lhs: consts.root(insn.src[0], width),
                        rhs: consts.root(insn.src[1], width),
                        signed,
                        width,
                    },
                );
            }
        }
        Self { facts }
    }

    fn get(&self, id: PseudoId) -> Option<CmpFact> {
        self.facts.get(&id).copied()
    }

    /// The fact for whatever `id` ultimately copies from.
    ///
    /// Needed because stripping the boolification leaves a `Copy` of the
    /// comparison in its place, and a later rule asking about that copy would
    /// otherwise learn nothing.
    fn get_through(&self, consts: &ConstMap, id: PseudoId, width: u32) -> Option<CmpFact> {
        self.get(consts.root(id, width))
    }

    /// `other`'s mask expressed over `base`'s operand order, or `None` when
    /// the two are not comparisons of the same pair in the same signedness.
    fn aligned_mask(&self, base: CmpFact, other: CmpFact) -> Option<u8> {
        if base.signed != other.signed || base.width != other.width {
            return None;
        }
        if base.lhs == other.lhs && base.rhs == other.rhs {
            return Some(other.mask);
        }
        if base.lhs == other.rhs && base.rhs == other.lhs {
            return Some(mirror_mask(other.mask));
        }
        None
    }
}

/// Everything one run of this pass knows that is fixed for the whole
/// function, gathered so the dispatch keeps a stable arity as rules are
/// added.
struct Facts<'a> {
    cmps: CmpFacts,
    /// Pseudos whose value is never *less than* zero -- which is not the
    /// same as non-negative, and is deliberately the weaker fact: `fabs` of
    /// a NaN is a NaN, and a NaN is not less than zero either, because an
    /// unordered comparison is false. Stating it this way is what makes the
    /// rule below correct without a NaN test.
    never_lt_zero: HashSet<PseudoId>,
    types: &'a TypeTable,
}

impl<'a> Facts<'a> {
    fn new(func: &Function, consts: &ConstMap, types: &'a TypeTable) -> Self {
        let mut never_lt_zero = HashSet::new();
        for bb in &func.blocks {
            for insn in &bb.insns {
                if matches!(insn.op, Opcode::Fabs32 | Opcode::Fabs64) {
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
    fn fp_format(&self, typ: Option<TypeId>) -> Option<crate::float::FpFormat> {
        typ.and_then(|t| self.types.fp_format(t))
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
    /// Become this float constant.
    ///
    /// Not "copy from one": a float constant is a pseudo kind, so the fold
    /// converts the target itself and rewrites the instruction to the
    /// `SetVal` that gives it a width. See `Function::make_float_const`.
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
    let facts = Facts::new(func, &consts, types);

    for (bb_idx, bb) in func.blocks.iter().enumerate() {
        for (insn_idx, insn) in bb.insns.iter().enumerate() {
            let mut result = try_simplify(insn, &consts, &facts);
            // A float fold converts the target pseudo itself, and only a
            // `Reg` may be converted. Deciding that here rather than when
            // the fold is applied is what keeps `record` honest: a recorded
            // constant the apply loop then declines would be read by every
            // later instruction in this same pass.
            if matches!(result, Simplification::FoldToFloat(_))
                && !insn.target.is_some_and(|t| func.is_plain_temp(t))
            {
                result = Simplification::None;
            }
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
            let (typ, size) = result_type_of(insn, types);
            (insn.target, typ, size)
        };

        let new_insn = match simplification {
            Simplification::None => continue,
            Simplification::CopyFrom(src) => make_copy_from_parts(target, typ, size, src),
            Simplification::FoldToConst(value) => {
                // Create a new constant pseudo
                let const_id = func.create_const_pseudo(value);
                make_copy_from_parts(target, typ, size, const_id)
            }
            Simplification::FoldToFloat(value) => {
                // The target becomes the constant, and the instruction
                // becomes the `SetVal` that carries its width -- which is
                // the shape a float literal is linearized into, and the
                // only one both allocators resolve.
                let Some(target_id) = target else { continue };
                if !func.make_const(target_id, ConstValue::Float(value)) {
                    continue;
                }
                Instruction {
                    op: Opcode::SetVal,
                    target,
                    src: Vec::new(),
                    typ,
                    size,
                    ..Default::default()
                }
            }
        };
        func.blocks[bb_idx].insns[insn_idx] = new_insn;
        changed = true;
    }

    changed
}

// Simplification Dispatch

/// Try to simplify an instruction. Returns the simplification to apply.
fn try_simplify(insn: &Instruction, consts: &ConstMap, facts: &Facts) -> Simplification {
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
        | Opcode::SetAe => simplify_comparison(insn, consts, facts),

        // A short-circuit `&&`/`||` after if-conversion.
        Opcode::Select => simplify_select(insn, consts, facts),

        // Floating point
        Opcode::FAdd | Opcode::FSub | Opcode::FMul | Opcode::FDiv => {
            simplify_fbinop(insn, consts, facts)
        }
        Opcode::FNeg => simplify_funop(insn, consts, facts),
        Opcode::FCvtF => simplify_fcvtf(insn, consts, facts),
        Opcode::FCmpOEq
        | Opcode::FCmpONe
        | Opcode::FCmpOLt
        | Opcode::FCmpOLe
        | Opcode::FCmpOGt
        | Opcode::FCmpOGe => simplify_fcmp(insn, consts, facts),
        Opcode::FCvtS | Opcode::FCvtU => simplify_fcvt(insn, consts, facts),

        // Unary
        Opcode::Neg => simplify_neg(insn, consts),
        Opcode::Not => simplify_not(insn, consts),
        Opcode::Sext | Opcode::Zext | Opcode::Trunc => simplify_convert(insn, consts),

        _ => Simplification::None,
    }
}

/// Return FoldToConst(0).
fn fold_to_zero() -> Simplification {
    Simplification::FoldToConst(0)
}

/// Fold `insn` over two known constants, deferring to `constfold` for the
/// width and signedness rules. `None` there means the operation is undefined
/// for these operands -- a zero divisor, an out-of-range shift count -- and
/// the instruction is left alone.
fn fold_with(insn: &Instruction, a: i128, b: i128) -> Simplification {
    match eval_binop(insn, a, b) {
        Some(v) => Simplification::FoldToConst(v),
        None => Simplification::None,
    }
}

/// The unary counterpart of [`fold_with`].
fn fold_unary_with(insn: &Instruction, a: i128) -> Simplification {
    match eval_unop(insn, a) {
        Some(v) => Simplification::FoldToConst(v),
        None => Simplification::None,
    }
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
        (Some(a), Some(b)) => fold_with(insn, a, b),

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

    // Identity: x - x -> 0, by root: the two sides are usually distinct
    // copies of one value.
    if consts.root(src1, insn.size.max(1)) == consts.root(src2, insn.size.max(1)) {
        return fold_to_zero();
    }

    let val1 = consts.get(src1);
    let val2 = consts.get(src2);

    match (val1, val2) {
        // Constant folding: a - b -> (a - b)
        (Some(a), Some(b)) => fold_with(insn, a, b),

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
        (Some(a), Some(b)) => fold_with(insn, a, b),

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
        (Some(a), Some(b)) => fold_with(insn, a, b),

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
        (Some(a), Some(b)) => fold_with(insn, a, b),

        // Algebraic: 0 % x -> 0
        (Some(0), None) => fold_to_zero(),

        // Algebraic: x % 1 -> 0
        (None, Some(1)) => fold_to_zero(),

        _ => Simplification::None,
    }
}

// Shift Simplifications

fn simplify_shift(insn: &Instruction, consts: &ConstMap) -> Simplification {
    if insn.src.len() != 2 {
        return Simplification::None;
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
        return Simplification::FoldToConst(at_width(-1, size, true));
    }

    match (val1, val2) {
        // Constant folding (shift amount must be in [0, type_width))
        (Some(a), Some(b)) => fold_with(insn, a, b),

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

    // x op x -> self_result, by root as above.
    if consts.root(src1, insn.size.max(1)) == consts.root(src2, insn.size.max(1)) {
        return match info.self_result {
            SelfOpResult::CopySrc => Simplification::CopyFrom(src1),
            SelfOpResult::Const(c) => fold_to_const(c),
        };
    }

    let val1 = consts.get(src1);
    let val2 = consts.get(src2);

    match (val1, val2) {
        // Constant folding: a op b
        (Some(a), Some(b)) => fold_with(insn, a, b),

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

/// Unified comparison simplification for all SetXX opcodes
fn simplify_comparison(insn: &Instruction, consts: &ConstMap, facts: &Facts) -> Simplification {
    let info = match get_cmp_info(insn.op) {
        Some(i) => i,
        None => return Simplification::None,
    };

    if insn.src.len() != 2 {
        return Simplification::None;
    }

    let src1 = insn.src[0];
    let src2 = insn.src[1];
    let width = cmp_operand_width(insn);

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
                return Simplification::CopyFrom(bool_side);
            }
        }
    }

    // Identity: x op x -> identity_result (comparison result is always i32/i64, never i128)
    //
    // By root rather than by pseudo id: promotion out of memory gives every
    // use of a local its own `Copy`, so `x >> 0 != x` arrives as two distinct
    // pseudos naming one value.
    if consts.root(src1, width) == consts.root(src2, width) {
        return Simplification::FoldToConst(info.identity_result);
    }

    // Constant folding. A comparison knows its operand width and signedness
    // from the opcode, so it can read a value `get` would refuse to guess at.
    let val1 = consts.get_at(src1, width, info.signed);
    let val2 = consts.get_at(src2, width, info.signed);

    if let (Some(a), Some(b)) = (val1, val2) {
        return fold_with(insn, a, b);
    }

    Simplification::None
}

/// `select(c, t, f)` where the arms make it a short-circuit `&&` or `||`.
///
/// If-conversion turns `a && b` into `select(a, b, 0)` and `a || b` into
/// `select(a, 1, b)`. When both `a` and `b` compare the *same* operand pair,
/// the answer needs nothing about the operands: `&&` is never true when their
/// orderings are disjoint, and `||` is always true when together they cover
/// less, equal and greater.
///
/// `(x == y) && (x != y)` is the first; `(x >= y) || (x < y)` the second. The
/// operands may be written either way round -- `(x < y) && (y < x)` is also
/// never true -- which `aligned_mask` handles by mirroring.
fn simplify_select(insn: &Instruction, consts: &ConstMap, facts: &Facts) -> Simplification {
    if insn.src.len() != 3 {
        return Simplification::None;
    }
    let (cond, t, f) = (insn.src[0], insn.src[1], insn.src[2]);

    // A constant condition needs no facts at all.
    if let Some(c) = consts.get(cond) {
        return Simplification::CopyFrom(if c != 0 { t } else { f });
    }
    // Both arms the same value, whatever the condition.
    let width = insn.size.max(1);
    if consts.root(t, width) == consts.root(f, width) {
        return Simplification::CopyFrom(t);
    }

    let Some(c_fact) = facts.cmps.get_through(consts, cond, width) else {
        return Simplification::None;
    };

    // `a && b`: false on the `a`-false edge.
    if consts.get(f) == Some(0) {
        if let Some(other) = facts.cmps.get_through(consts, t, width) {
            if let Some(mask) = facts.cmps.aligned_mask(c_fact, other) {
                if c_fact.mask & mask == 0 {
                    return fold_to_zero();
                }
            }
        }
    }
    // `a || b`: true on the `a`-true edge.
    if consts.get(t) == Some(1) {
        if let Some(other) = facts.cmps.get_through(consts, f, width) {
            if let Some(mask) = facts.cmps.aligned_mask(c_fact, other) {
                if c_fact.mask | mask == CMP_ALL {
                    return fold_to_const(1);
                }
            }
        }
    }
    Simplification::None
}

// Unary Simplifications

/// Fold an integer width conversion of a constant.
///
/// The operand is read at the width the conversion says it was stored in,
/// which is not `insn.size` -- that is the destination. `get_at` is the
/// accessor that takes both, and it is right here for the same reason it is
/// right for a comparison: the opcode carries the signedness.
fn simplify_convert(insn: &Instruction, consts: &ConstMap) -> Simplification {
    if insn.src.len() != 1 {
        return Simplification::None;
    }
    let (width, signed) = match insn.op {
        Opcode::Trunc => (insn.size.max(1), true),
        _ if insn.src_size != 0 => (insn.src_size, insn.op == Opcode::Sext),
        // Without a source width an extension cannot be read at all.
        _ => return Simplification::None,
    };
    match consts.get_at(insn.src[0], width, signed) {
        Some(a) => fold_unary_with(insn, a),
        None => Simplification::None,
    }
}

// Floating-Point Simplification

/// Fold a float comparison, either over two constants or over `fabs`.
///
/// The `fabs` half is gcc's `fabs(x) < 0.0` rule: no absolute value is ever
/// less than zero, and a NaN argument does not spoil it, since an unordered
/// `<` is false as well. Only the strict form folds -- `fabs(x) <= 0.0` is
/// true for a zero argument, and `fabs(x) >= 0.0` is false for a NaN one.
fn simplify_fcmp(insn: &Instruction, consts: &ConstMap, facts: &Facts) -> Simplification {
    if insn.src.len() != 2 {
        return Simplification::None;
    }
    let (lhs, rhs) = (insn.src[0], insn.src[1]);
    let width = insn.size.max(1);

    // `x < 0.0` and its mirror `0.0 > x`, for an `x` that is never below zero.
    let zero_at = |id: PseudoId| consts.fget(id, width).is_some_and(|v| v.is_zero());
    let never_below = |id: PseudoId| facts.never_lt_zero.contains(&consts.root(id, width));
    match insn.op {
        Opcode::FCmpOLt if never_below(lhs) && zero_at(rhs) => return fold_to_zero(),
        Opcode::FCmpOGt if zero_at(lhs) && never_below(rhs) => return fold_to_zero(),
        _ => {}
    }

    let (Some(a), Some(b)) = (consts.fget(lhs, width), consts.fget(rhs, width)) else {
        return Simplification::None;
    };
    let Some(fmt) = facts.fp_format(insn.typ) else {
        return Simplification::None;
    };
    match eval_fcmp(insn.op, fmt, a, b) {
        Some(v) => Simplification::FoldToConst(v),
        None => Simplification::None,
    }
}

/// Fold float arithmetic over two constants.
fn simplify_fbinop(insn: &Instruction, consts: &ConstMap, facts: &Facts) -> Simplification {
    if insn.src.len() != 2 {
        return Simplification::None;
    }
    let width = insn.size.max(1);
    let (Some(a), Some(b)) = (
        consts.fget(insn.src[0], width),
        consts.fget(insn.src[1], width),
    ) else {
        return Simplification::None;
    };
    let Some(fmt) = facts.fp_format(insn.typ) else {
        return Simplification::None;
    };
    match eval_fbinop(insn.op, fmt, a, b) {
        Some(v) => Simplification::FoldToFloat(v),
        None => Simplification::None,
    }
}

/// Fold `FNeg` of a constant.
///
/// This is what makes a negative float literal a constant at all: `-1.5` is
/// parsed as a negation of `1.5`, exactly as `-1` is of `1`, and the integer
/// half has always folded here.
fn simplify_funop(insn: &Instruction, consts: &ConstMap, facts: &Facts) -> Simplification {
    if insn.src.len() != 1 {
        return Simplification::None;
    }
    let Some(a) = consts.fget(insn.src[0], insn.size.max(1)) else {
        return Simplification::None;
    };
    let Some(fmt) = facts.fp_format(insn.typ) else {
        return Simplification::None;
    };
    match eval_funop(insn.op, fmt, a) {
        Some(v) => Simplification::FoldToFloat(v),
        None => Simplification::None,
    }
}

/// Fold a float-to-float conversion of a constant, to `typ`'s format.
fn simplify_fcvtf(insn: &Instruction, consts: &ConstMap, facts: &Facts) -> Simplification {
    if insn.src.len() != 1 {
        return Simplification::None;
    }
    let src_width = if insn.src_size != 0 {
        insn.src_size
    } else {
        insn.size.max(1)
    };
    let Some(a) = consts.fget(insn.src[0], src_width) else {
        return Simplification::None;
    };
    let (Some(src_fmt), Some(dst_fmt)) = (facts.fp_format(insn.src_typ), facts.fp_format(insn.typ))
    else {
        return Simplification::None;
    };
    match eval_fcvtf(insn.op, src_fmt, dst_fmt, a) {
        Some(v) => Simplification::FoldToFloat(v),
        None => Simplification::None,
    }
}

/// Fold a float-to-integer conversion of a constant.
///
/// The source format comes from `src_typ`, not `typ`: a conversion's `typ` is
/// the integer it produces, and the width it reads is the separate `src_size`.
fn simplify_fcvt(insn: &Instruction, consts: &ConstMap, facts: &Facts) -> Simplification {
    if insn.src.len() != 1 {
        return Simplification::None;
    }
    let src_width = if insn.src_size != 0 {
        insn.src_size
    } else {
        insn.size.max(1)
    };
    let Some(a) = consts.fget(insn.src[0], src_width) else {
        return Simplification::None;
    };
    let Some(fmt) = facts.fp_format(insn.src_typ) else {
        return Simplification::None;
    };
    match eval_fcvt(insn.op, insn.size.max(1), fmt, a) {
        Some(v) => Simplification::FoldToConst(v),
        None => Simplification::None,
    }
}

fn simplify_neg(insn: &Instruction, consts: &ConstMap) -> Simplification {
    if insn.src.len() != 1 {
        return Simplification::None;
    }

    let src = insn.src[0];
    match consts.get(src) {
        Some(val) => fold_unary_with(insn, val),
        None => Simplification::None,
    }
}

fn simplify_not(insn: &Instruction, consts: &ConstMap) -> Simplification {
    if insn.src.len() != 1 {
        return Simplification::None;
    }

    let src = insn.src[0];
    match consts.get(src) {
        Some(val) => fold_unary_with(insn, val),
        None => Simplification::None,
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
        assert!(run(&mut func, &host_types()));
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
                Instruction::binop(
                    Opcode::SetNe,
                    PseudoId(3),
                    PseudoId(1),
                    PseudoId(2),
                    types.int_id,
                    32,
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
        assert_ne!(
            consts.root(PseudoId(1), 32),
            consts.root(PseudoId(2), 32),
            "an 8-bit copy is not the 32-bit value"
        );
        assert_eq!(
            consts.root(PseudoId(1), 8),
            consts.root(PseudoId(2), 8),
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
                Instruction::binop(
                    Opcode::SetLt,
                    PseudoId(2),
                    PseudoId(0),
                    PseudoId(1),
                    types.int_id,
                    32,
                ),
                // operands swapped
                Instruction::binop(
                    Opcode::SetLt,
                    PseudoId(3),
                    PseudoId(1),
                    PseudoId(0),
                    types.int_id,
                    32,
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
        let types = TypeTable::new(&Target::host());
        // `other == 0` is `a && b`, so the comparison is the *true* arm;
        // `other == 1` is `a || b`, so it is the false arm.
        let (t, f) = if other == 0 {
            (PseudoId(3), PseudoId(4))
        } else {
            (PseudoId(4), PseudoId(3))
        };
        let mut func = make_test_func_with_insns(
            vec![
                Instruction::binop(
                    cond_op,
                    PseudoId(2),
                    PseudoId(0),
                    PseudoId(1),
                    types.int_id,
                    32,
                ),
                Instruction::binop(
                    arm_op,
                    PseudoId(3),
                    PseudoId(0),
                    PseudoId(1),
                    types.int_id,
                    32,
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
            vec![Instruction::binop(
                op,
                PseudoId(2),
                PseudoId(0),
                PseudoId(1),
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
            vec![Instruction::binop(
                Opcode::FCmpOLt,
                PseudoId(2),
                PseudoId(0),
                PseudoId(1),
                types.double_id,
                64,
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
    /// unordered `<` is false as well. The value itself is unknown.
    #[test]
    fn fabs_is_never_below_zero() {
        for (op, fabs_first) in [(Opcode::FCmpOLt, true), (Opcode::FCmpOGt, false)] {
            let types = TypeTable::new(&Target::host());
            let (l, r) = if fabs_first {
                (PseudoId(1), PseudoId(2))
            } else {
                (PseudoId(2), PseudoId(1))
            };
            let mut func = make_test_func_with_insns(
                vec![
                    Instruction::new(Opcode::Fabs64)
                        .with_target(PseudoId(1))
                        .with_src(PseudoId(0))
                        .with_type_and_size(types.double_id, 64),
                    Instruction::binop(op, PseudoId(3), l, r, types.double_id, 64),
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
                    Instruction::new(Opcode::Fabs64)
                        .with_target(PseudoId(1))
                        .with_src(PseudoId(0))
                        .with_type_and_size(types.double_id, 64),
                    Instruction::binop(
                        op,
                        PseudoId(3),
                        PseudoId(1),
                        PseudoId(2),
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

    /// Out of the destination's range the conversion is undefined, and the
    /// hardware's answer is the target's business, not this pass's.
    #[test]
    fn an_out_of_range_conversion_is_left_alone() {
        assert_eq!(fold_fcvt(Opcode::FCvtS, 3e9, 32), None);
        assert_eq!(fold_fcvt(Opcode::FCvtS, -3e9, 32), None);
        assert_eq!(fold_fcvt(Opcode::FCvtU, -1.0, 32), None);
        assert_eq!(fold_fcvt(Opcode::FCvtS, f64::NAN, 32), None);
        assert_eq!(fold_fcvt(Opcode::FCvtS, f64::INFINITY, 64), None);
        // The same value the 32-bit case refused does fit 64 bits.
        assert_eq!(fold_fcvt(Opcode::FCvtS, 3e9, 64), Some(3_000_000_000));
    }

    /// One float binary/unary op over constants; returns the folded value
    /// and the `SetVal` the instruction became.
    fn fold_float(op: Opcode, typ: TypeId, size: u32, args: &[f64]) -> Option<(FloatVal, u32)> {
        let target = PseudoId(args.len() as u32);
        let mut pseudos: Vec<Pseudo> = args
            .iter()
            .enumerate()
            .map(|(i, v)| fval(i as u32, *v))
            .collect();
        pseudos.push(Pseudo::reg(target, target.0));
        let insn = match args.len() {
            1 => Instruction::new(op)
                .with_target(target)
                .with_src(PseudoId(0))
                .with_type_and_size(typ, size),
            _ => Instruction::binop(op, target, PseudoId(0), PseudoId(1), typ, size),
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
        let want = f64::from(half_of(0.3));
        assert_eq!(fold_fcvtf(0.3, h, f), Some(want));
        assert_ne!(fold_fcvtf(0.3, h, f), Some(f64::from(0.3f32)));
    }

    /// `v` rounded to IEEE binary16 and back, computed independently of the
    /// code under test.
    fn half_of(v: f64) -> f32 {
        let bits = crate::float::f64_to_f16_bits(v);
        let sign = u32::from(bits >> 15) << 31;
        let exp = i32::from((bits >> 10) & 0x1F);
        let frac = u32::from(bits & 0x3FF);
        if exp == 0 {
            // Subnormal or zero; 0.3 is neither, so this is for completeness.
            return f32::from_bits(sign) + (frac as f32) * 2f32.powi(-24);
        }
        f32::from_bits(sign | (((exp - 15 + 127) as u32) << 23) | (frac << 13))
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

    /// Without a source width an extension cannot be read at all, and
    /// falling back on `size` would make it the identity.
    #[test]
    fn an_extension_with_no_source_width_is_left_alone() {
        assert_eq!(fold_convert(Opcode::Sext, 200, 0, 32), None);
        assert_eq!(fold_convert(Opcode::Zext, 200, 0, 32), None);
    }
}
