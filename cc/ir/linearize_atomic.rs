//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Lowering for `_Atomic` objects accessed through *ordinary operators*.
//
// C17 6.5.16.2p3 makes `x += 1` on an atomic object a single atomic
// read-modify-write, and 6.7.3 makes every access to one atomic. The
// `<stdatomic.h>` builtins already lowered correctly; ordinary operators did
// not, so `_Atomic int g; g += 1;` emitted a plain load and a plain store with
// no lock prefix and no barrier -- code that reads as synchronized and races
// silently (audit #X1).
//
// Everything here funnels through `atomic_lvalue`, which is also where a type
// too large to operate on lock-free is rejected rather than silently degraded.
//

use super::linearize::Linearizer;
use super::linearize_emit::{compound_assign_arith_type, compound_assign_opcode, CompoundAssign};
use super::{Instruction, MemoryOrder, Opcode, PseudoId};
use crate::diag;
use crate::float::FloatVal;
use crate::parse::ast::{AssignOp, Expr, ExprKind};
use crate::types::{TypeId, TypeKind};

/// An `_Atomic` lvalue that can be operated on with a single hardware atomic:
/// its address, its element type, and the width in bits.
pub(crate) struct AtomicLvalue {
    pub addr: PseudoId,
    pub elem_typ: TypeId,
    pub size_bits: u32,
}

/// C17 6.5.16.2p3 / 7.17.3p12: an operator on an atomic object is
/// sequentially consistent. Only the `_explicit` builtins choose otherwise.
const ORDER: MemoryOrder = MemoryOrder::SeqCst;

impl Linearizer<'_> {
    /// True when `typ` is `_Atomic`-qualified.
    pub(crate) fn is_atomic_type(&self, typ: TypeId) -> bool {
        self.types.is_atomic(typ)
    }

    /// True when an atomic object of this type can be operated on with a
    /// single hardware instruction.
    ///
    /// Anything else -- a complex value, `long double`, `__int128`, or any
    /// width that is not a machine integer size -- would need the lock-based
    /// fallbacks in libatomic, which this compiler does not link.
    ///
    /// An aggregate of a machine width qualifies. Its members are
    /// irrelevant: what the hardware needs is a width and an address, and gcc
    /// lowers `_Atomic struct S { int a; }` to a plain 4-byte access for the
    /// same reason. The instruction operates on the bits through the integer
    /// surrogate `atomic_access_type` picks.
    fn is_lock_free(&self, typ: TypeId) -> bool {
        let kind = self.types.kind(typ);
        let is_scalar = matches!(
            kind,
            TypeKind::Bool
                | TypeKind::Char
                | TypeKind::Short
                | TypeKind::Int
                | TypeKind::Long
                | TypeKind::LongLong
                | TypeKind::Enum
                | TypeKind::Pointer
                | TypeKind::Float
                | TypeKind::Double
        ) && !self.types.is_complex(typ);

        let is_aggregate = matches!(kind, TypeKind::Struct | TypeKind::Union);

        let bits = self.types.size_bits(typ);
        (is_scalar || is_aggregate)
            && bits % 8 == 0
            && crate::target::atomic_is_lock_free(u64::from(bits / 8))
    }

    /// The type the atomic instruction actually operates through.
    ///
    /// A scalar is its own surrogate. An aggregate has no arithmetic and no
    /// register class of its own, so it travels as an unsigned integer of the
    /// same width -- the same mapping bit-fields already use for their storage
    /// unit. `elem_typ` is what every backend reads to size the access, so
    /// substituting here is enough; nothing downstream has to know.
    fn atomic_access_type(&self, typ: TypeId) -> TypeId {
        if matches!(self.types.kind(typ), TypeKind::Struct | TypeKind::Union) {
            self.bitfield_storage_type((self.types.size_bytes(typ)).max(1))
        } else {
            typ
        }
    }

    /// Recognize an `_Atomic` lvalue and produce its address.
    ///
    /// Returns `None` for a non-atomic expression, so callers fall through to
    /// their ordinary path unchanged. For an atomic type that cannot be
    /// operated on lock-free this emits a diagnostic and returns `None`:
    /// gcc's equivalent emits calls to `__atomic_load`/`__atomic_store`, which
    /// then fail to link without `-latomic`; a compile-time error naming the
    /// type is a better outcome than a link failure.
    pub(crate) fn atomic_lvalue(&mut self, expr: &Expr) -> Option<AtomicLvalue> {
        let typ = expr.typ?;
        if !self.is_atomic_type(typ) {
            return None;
        }

        // Only these forms designate an object whose address we can take.
        if !matches!(
            expr.kind,
            ExprKind::Ident(_)
                | ExprKind::Member { .. }
                | ExprKind::Arrow { .. }
                | ExprKind::Index { .. }
                | ExprKind::Unary {
                    op: crate::parse::ast::UnaryOp::Deref,
                    ..
                }
        ) {
            return None;
        }

        if !self.is_lock_free(typ) {
            // Warn and fall through to the ordinary (non-atomic) path.
            //
            // An error here would be a source-compatibility break: gcc lowers
            // an 8-byte `_Atomic struct` to a single lock-free cmpxchg with no
            // libatomic reference, so code that builds with gcc must build
            // here too.
            //
            // What is left here is what libatomic exists for: `long double`,
            // `__int128`, complex, and any width that is not a machine integer
            // size. gcc calls `__atomic_*` for those and c17 links through the
            // host `cc` without `-latomic` (#X1), so the warning stands.
            diag::warning_args(
                expr.pos,
                "access to '{0}' ({1} bytes) is not atomic: c17 provides \
                 lock-free atomics only at 1, 2, 4 and 8 bytes",
                &[
                    &self.types.get(typ).to_string(),
                    &self.types.size_bytes(typ).to_string(),
                ],
            );
            return None;
        }

        let size_bits = self.types.size_bits(typ);
        let addr = self.linearize_lvalue(expr);
        Some(AtomicLvalue {
            addr,
            elem_typ: self.atomic_access_type(typ),
            size_bits,
        })
    }

    /// Emit one atomic instruction: `op addr, [value]` with sequential
    /// consistency, returning the target pseudo.
    ///
    /// This is the single place that builds an atomic instruction, so the
    /// operand order the backends expect is written down once.
    pub(crate) fn emit_atomic_op(
        &mut self,
        op: Opcode,
        lv: &AtomicLvalue,
        value: Option<PseudoId>,
    ) -> PseudoId {
        let target = self.alloc_reg_pseudo();
        let order = self.emit_const(ORDER as i128, self.types.int_id);

        let mut srcs = vec![lv.addr];
        if let Some(v) = value {
            srcs.push(v);
        }
        srcs.push(order);

        let mut insn = Instruction::new(op).with_target(target);
        insn.src = srcs;
        insn.typ = Some(lv.elem_typ);
        insn.size = lv.size_bits;
        insn.extra_mut().memory_order = ORDER;
        self.emit(insn);

        target
    }

    /// An atomic read of `lv`, as every ordinary rvalue use of an `_Atomic`
    /// object must be.
    ///
    /// On x86 a plain aligned `mov` already is a sequentially consistent load,
    /// so this changes no instructions there -- but on aarch64 it is the
    /// difference between `ldr` and `ldar`, and on both it keeps the load out
    /// of reach of optimizations that may not duplicate or elide an atomic
    /// access.
    pub(crate) fn emit_atomic_load(&mut self, lv: &AtomicLvalue) -> PseudoId {
        self.emit_atomic_op(Opcode::AtomicLoad, lv, None)
    }

    /// An atomic store, returning the stored value so that `x = v` still has
    /// the value of `v` as an expression.
    pub(crate) fn emit_atomic_store(&mut self, lv: &AtomicLvalue, value: PseudoId) -> PseudoId {
        self.emit_atomic_op(Opcode::AtomicStore, lv, Some(value));
        value
    }

    /// The atomic opcode implementing `op` directly, if the hardware has one.
    ///
    /// Multiplication, division, remainder, the shifts and everything
    /// floating-point have no native atomic form and go through a
    /// compare-and-swap retry loop instead. Neither does `nand`, on any
    /// target, which is why it is a flag on [`CompoundAssign`] rather than an
    /// opcode here.
    ///
    /// Having one does not by itself make it usable: see `native_rmw_opcode`.
    fn atomic_opcode_for(op: Opcode) -> Option<Opcode> {
        Some(match op {
            Opcode::Add => Opcode::AtomicFetchAdd,
            Opcode::Sub => Opcode::AtomicFetchSub,
            Opcode::And => Opcode::AtomicFetchAnd,
            Opcode::Or => Opcode::AtomicFetchOr,
            Opcode::Xor => Opcode::AtomicFetchXor,
            _ => return None,
        })
    }

    /// Perform `old <op> value` atomically and return the **old** value.
    ///
    /// Uses the native fetch-and-op where one exists, and otherwise builds a
    /// CAS retry loop:
    ///
    /// ```text
    ///   entry:  cur = AtomicLoad addr;  store cur -> exp;  br loop
    ///   loop:   old = load exp;  new = old <op> value
    ///           ok  = AtomicCas addr, &exp, new
    ///           cbr ok -> done, loop
    ///   done:   old' = load exp   (re-materialised; see below)
    /// ```
    ///
    /// The loop is built in the IR rather than in each backend for one
    /// concrete reason: an aarch64 LL/SC loop must not contain a call, and a
    /// floating-point `/=` lowers to a libgcc call. Keeping the arithmetic
    /// outside the `AtomicCas` -- which is itself a self-contained LL/SC loop
    /// on that target -- is the only correct arrangement.
    ///
    /// The result is re-loaded in the exit block rather than carried in a
    /// pseudo across the `AtomicCas`. A value live across an atomic operation
    /// *and* named as one of its sources is exempt from that instruction's
    /// clobber set, so the allocator would be free to place it in a register
    /// the compare-exchange destroys.
    pub(crate) fn emit_atomic_rmw(
        &mut self,
        lv: &AtomicLvalue,
        ca: &CompoundAssign,
        value: PseudoId,
    ) -> PseudoId {
        if let Some(atomic_op) = self.native_rmw_opcode(ca) {
            // The instruction computes at the object's own width, so the
            // operand arrives at the object's own type -- the truncation the
            // congruence below permits. Pointer arithmetic has scaled it to a
            // byte count already, at pointer width.
            let value = if ca.is_ptr_arith {
                value
            } else {
                self.emit_convert(value, ca.value_typ, ca.target_typ)
            };
            return self.emit_atomic_op(atomic_op, lv, Some(value));
        }
        self.emit_atomic_cas_loop(lv, ca, value)
    }

    /// The native atomic instruction that computes `ca` *exactly*, if one
    /// does.
    ///
    /// `AtomicFetchAdd` and its siblings operate at the object's width and
    /// store the raw result, where C17 6.5.16.2p3 computes at the operands'
    /// common type and converts the result back
    /// ([`Linearizer::compound_assign_value`]). The two agree when the
    /// operator is congruent modulo 2^n -- add, subtract and the three bitwise
    /// ops -- *and* the conversion back is the truncation congruence permits.
    ///
    /// `_Bool` is where that second condition fails: converting to it is a
    /// test against zero, not a truncation, so `b -= 1` must store 1 and only
    /// the CAS loop can convert before the store. Divide, remainder, the
    /// shifts and everything floating-point fail the first condition, and
    /// `nand` complements a value the hardware would store as it stands.
    fn native_rmw_opcode(&self, ca: &CompoundAssign) -> Option<Opcode> {
        if ca.invert || self.types.kind(ca.target_typ) == TypeKind::Bool {
            return None;
        }
        let arith_type = compound_assign_arith_type(self.types, ca);
        Self::atomic_opcode_for(compound_assign_opcode(self.types, ca.op, arith_type))
    }

    /// The CAS retry loop described on `emit_atomic_rmw`.
    fn emit_atomic_cas_loop(
        &mut self,
        lv: &AtomicLvalue,
        ca: &CompoundAssign,
        value: PseudoId,
    ) -> PseudoId {
        let elem_typ = lv.elem_typ;
        let bits = lv.size_bits;

        // A stack slot holding the expected value. AtomicCas takes its address
        // because both backends write the observed value back through it on
        // failure -- which is exactly the value the next iteration needs.
        let exp_addr = self.frame_temp_addr("__casexp", elem_typ);

        // Seed it with an atomic read of the object.
        let cur = self.emit_atomic_load(lv);
        self.emit(Instruction::store(cur, exp_addr, 0, elem_typ, bits));

        let loop_bb = self.alloc_bb();
        let done_bb = self.alloc_bb();

        // Through the accessor rather than `expect`: `current_bb` is `None`
        // wherever control cannot arrive -- a statement before a `switch`'s
        // first `case`, or after a `goto` -- and this loop has to hang its
        // blocks off something. `dce::remove_unreachable_blocks` takes the
        // lot away again.
        let entry_bb = self.current_or_unreachable_bb();
        self.emit(Instruction::br(loop_bb));
        self.link_bb(entry_bb, loop_bb);
        self.switch_bb(loop_bb);

        // old = *exp; new = the value `old <op> value` assigns.
        //
        // Through the shared helper, so the loop computes at the same type the
        // ordinary lowering does and converts the result back the same way --
        // which is also what keeps an `_Atomic _Bool` holding 0 or 1 rather
        // than the raw 2 or 255 the arithmetic produced (C17 6.3.1.2).
        let old = self.alloc_reg_pseudo();
        self.emit(Instruction::load(old, exp_addr, 0, elem_typ, bits));
        let new = self.compound_assign_value(ca, old, value);

        let ok = self.alloc_reg_pseudo();
        let order = self.emit_const(ORDER as i128, self.types.int_id);
        let mut cas = Instruction::new(Opcode::AtomicCas).with_target(ok);
        cas.src = vec![lv.addr, exp_addr, new, order];
        cas.typ = Some(self.types.bool_id);
        cas.size = bits;
        cas.extra_mut().memory_order = ORDER;
        self.emit(cas);

        let cas_bb = self.current_or_unreachable_bb();
        self.emit(Instruction::cbr(ok, done_bb, loop_bb));
        self.link_bb(cas_bb, done_bb);
        self.link_bb(cas_bb, loop_bb);
        self.switch_bb(done_bb);

        // Re-load rather than reuse `old`, so nothing is live across the CAS.
        let result = self.alloc_reg_pseudo();
        self.emit(Instruction::load(result, exp_addr, 0, elem_typ, bits));
        result
    }
}

impl Linearizer<'_> {
    /// Lower `target op= value` when `target` is an `_Atomic` lvalue.
    ///
    /// Returns `None` for anything non-atomic so the caller keeps its ordinary
    /// path. The value of the expression is the value stored, per C17
    /// 6.5.16p3, which for a compound assignment is the *new* value -- the
    /// read-modify-write returns the old one, so the operation is re-applied
    /// locally. That is not a second access to the object.
    pub(crate) fn try_emit_atomic_assign(
        &mut self,
        op: AssignOp,
        target: &Expr,
        value: &Expr,
    ) -> Option<PseudoId> {
        let target_typ = target.typ?;
        if !self.is_atomic_type(target_typ) {
            return None;
        }

        let lv = self.atomic_lvalue(target)?;
        let value_typ = self.expr_type(value);
        let rhs = self.linearize_expr(value);

        if op == AssignOp::Assign {
            let converted = self.emit_convert(rhs, value_typ, target_typ);
            return Some(self.emit_atomic_store(&lv, converted));
        }

        // Pointer arithmetic scales by the element size. The ordinary path does
        // this *after* the point we branched from, so it has to be repeated.
        let is_ptr_arith = self.types.kind(target_typ) == TypeKind::Pointer
            && self.types.is_integer(value_typ)
            && matches!(op, AssignOp::AddAssign | AssignOp::SubAssign);
        let (operand, value_typ) = if is_ptr_arith {
            (
                self.scale_pointer_addend(target_typ, value_typ, rhs),
                self.types.long_id,
            )
        } else {
            (rhs, value_typ)
        };

        // The right operand goes on at its own type. Converting it down to the
        // target here -- which this did -- computes `50 / (unsigned char)-5`
        // where C17 6.5.16.2p3 computes `50 / -5` at `int` and converts only
        // the result; `compound_assign_value` is the ordinary path's rule, now
        // shared rather than copied.
        let ca = CompoundAssign {
            is_ptr_arith,
            ..CompoundAssign::new(op, lv.elem_typ, value_typ)
        };
        let old = self.emit_atomic_rmw(&lv, &ca, operand);

        // Recompute the stored value from the old one, by the same rule that
        // stored it: C17 6.5.16p3 gives the expression the left operand's
        // value *after* the assignment, which for `_Atomic _Bool b = 0` makes
        // `(b -= 1)` the 1 that reached memory and not the 255 the subtraction
        // produced.
        Some(self.compound_assign_value(&ca, old, operand))
    }

    /// Scale an integer addend by the pointee size, for `p += n`.
    pub(crate) fn scale_pointer_addend(
        &mut self,
        ptr_typ: TypeId,
        value_typ: TypeId,
        rhs: PseudoId,
    ) -> PseudoId {
        let elem_type = self.types.base_type(ptr_typ).unwrap_or(self.types.char_id);
        let elem_size = self.types.size_bytes(elem_type);
        let scale = self.emit_const(elem_size as i128, self.types.long_id);
        let extended = self.emit_convert(rhs, value_typ, self.types.long_id);
        let scaled = self.alloc_reg_pseudo();
        self.emit(Instruction::binop(
            Opcode::Mul,
            scaled,
            extended,
            scale,
            self.types.long_id,
            64,
        ));
        scaled
    }
}

impl Linearizer<'_> {
    /// Lower `++x` / `x++` / `--x` / `x--` when `x` is an `_Atomic` lvalue.
    ///
    /// `prefix` selects which value the expression has: the new one for a
    /// prefix operator, the old one for a postfix operator. Only the
    /// read-modify-write itself has to be atomic -- recomputing the new value
    /// from the old one afterwards operates on a register copy and is not a
    /// second access to the object.
    pub(crate) fn try_emit_atomic_incdec(
        &mut self,
        operand: &Expr,
        is_inc: bool,
        prefix: bool,
    ) -> Option<PseudoId> {
        let typ = operand.typ?;
        if !self.is_atomic_type(typ) {
            return None;
        }

        let lv = self.atomic_lvalue(operand)?;

        // A pointer steps by one element; everything else by one. C17
        // 6.5.3.1p2 defines `++E` as `E += 1`, so the step is the right
        // operand of a compound assignment and the same helper applies --
        // including its conversion of the result, which is what makes `++b` on
        // an `_Atomic _Bool` still yield 0 or 1.
        let is_ptr_arith = self.types.kind(typ) == TypeKind::Pointer;
        let delta = self.incdec_delta(typ);
        let delta_typ = if is_ptr_arith {
            self.types.long_id
        } else {
            typ
        };
        let op = if is_inc {
            AssignOp::AddAssign
        } else {
            AssignOp::SubAssign
        };
        let ca = CompoundAssign {
            is_ptr_arith,
            ..CompoundAssign::new(op, lv.elem_typ, delta_typ)
        };

        let old = self.emit_atomic_rmw(&lv, &ca, delta);
        if !prefix {
            return Some(old);
        }
        Some(self.compound_assign_value(&ca, old, delta))
    }

    /// The amount `++`/`--` steps by: the pointee size for a pointer, else 1.
    pub(crate) fn incdec_delta(&mut self, typ: TypeId) -> PseudoId {
        if self.types.kind(typ) == TypeKind::Pointer {
            let elem = self.types.base_type(typ).unwrap_or(self.types.char_id);
            let bytes = (self.types.size_bytes(elem)).max(1);
            return self.emit_const(bytes as i128, self.types.long_id);
        }
        if self.types.is_float(typ) {
            return self.emit_fconst(FloatVal::from_f64(1.0), typ);
        }
        self.emit_const(1, typ)
    }
}
