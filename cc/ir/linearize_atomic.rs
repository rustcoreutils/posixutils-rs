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
use super::{Instruction, MemoryOrder, Opcode, PseudoId};
use crate::diag;
use crate::parse::ast::{AssignOp, Expr, ExprKind};
use crate::types::{TypeId, TypeKind, TypeModifiers};

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
        self.types.modifiers(typ).contains(TypeModifiers::ATOMIC)
    }

    /// True when an atomic object of this type can be operated on with a
    /// single hardware instruction.
    ///
    /// Anything else -- a struct, a complex value, `long double`, `__int128`,
    /// or any width that is not a machine integer size -- would need the
    /// lock-based fallbacks in libatomic, which this compiler does not link.
    fn is_lock_free(&self, typ: TypeId) -> bool {
        let kind = self.types.kind(typ);
        let scalar = matches!(
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
        );
        scalar && matches!(self.types.size_bits(typ), 8 | 16 | 32 | 64)
    }

    /// Recognize an `_Atomic` lvalue and produce its address.
    ///
    /// Returns `None` for a non-atomic expression, so callers fall through to
    /// their ordinary path unchanged. For an atomic type that cannot be
    /// operated on lock-free this emits a diagnostic and returns `None`:
    /// gcc's equivalent emits calls to `__atomic_load`/`__atomic_store`, which
    /// then fail to link without `-latomic`, and a compile-time error naming
    /// the type is a better outcome than either a link failure or the silent
    /// non-atomic copy this used to produce.
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
            diag::error(
                expr.pos,
                &format!(
                    "atomic operation on '{}' ({} bytes) is not lock-free; \
                     c17 does not link libatomic",
                    self.types.get(typ),
                    self.types.size_bytes(typ)
                ),
            );
            return None;
        }

        let size_bits = self.types.size_bits(typ);
        let addr = self.linearize_lvalue(expr);
        Some(AtomicLvalue {
            addr,
            elem_typ: typ,
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
        insn.memory_order = ORDER;
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
    /// compare-and-swap retry loop instead.
    pub(crate) fn atomic_opcode_for(op: Opcode) -> Option<Opcode> {
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
        op: Opcode,
        value: PseudoId,
    ) -> PseudoId {
        if let Some(atomic_op) = Self::atomic_opcode_for(op) {
            return self.emit_atomic_op(atomic_op, lv, Some(value));
        }
        self.emit_atomic_cas_loop(lv, op, value)
    }

    /// The CAS retry loop described on `emit_atomic_rmw`.
    fn emit_atomic_cas_loop(&mut self, lv: &AtomicLvalue, op: Opcode, value: PseudoId) -> PseudoId {
        let elem_typ = lv.elem_typ;
        let bits = lv.size_bits;

        // A stack slot holding the expected value. AtomicCas takes its address
        // because both backends write the observed value back through it on
        // failure -- which is exactly the value the next iteration needs.
        let exp_addr = self.alloc_local_temp(elem_typ);

        // Seed it with an atomic read of the object.
        let cur = self.emit_atomic_load(lv);
        self.emit(Instruction::store(cur, exp_addr, 0, elem_typ, bits));

        let loop_bb = self.alloc_bb();
        let done_bb = self.alloc_bb();

        let entry_bb = self.current_bb.expect("atomic RMW outside a block");
        self.emit(Instruction::br(loop_bb));
        self.link_bb(entry_bb, loop_bb);
        self.switch_bb(loop_bb);

        // old = *exp; new = old <op> value
        let old = self.alloc_reg_pseudo();
        self.emit(Instruction::load(old, exp_addr, 0, elem_typ, bits));
        let new = self.alloc_reg_pseudo();
        self.emit(Instruction::binop(op, new, old, value, elem_typ, bits));

        let ok = self.alloc_reg_pseudo();
        let order = self.emit_const(ORDER as i128, self.types.int_id);
        let mut cas = Instruction::new(Opcode::AtomicCas).with_target(ok);
        cas.src = vec![lv.addr, exp_addr, new, order];
        cas.typ = Some(self.types.bool_id);
        cas.size = bits;
        cas.memory_order = ORDER;
        self.emit(cas);

        let cas_bb = self.current_bb.expect("atomic CAS outside a block");
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

        let (operand, arith_typ) = if is_ptr_arith {
            (
                self.scale_pointer_addend(target_typ, value_typ, rhs),
                target_typ,
            )
        } else {
            (self.emit_convert(rhs, value_typ, target_typ), target_typ)
        };

        let opcode = self.compound_assign_opcode(op, arith_typ);
        let old = self.emit_atomic_rmw(&lv, opcode, operand);

        // Recompute the stored value from the old one.
        let new = self.alloc_reg_pseudo();
        let bits = self.types.size_bits(arith_typ);
        self.emit(Instruction::binop(
            opcode, new, old, operand, arith_typ, bits,
        ));
        Some(new)
    }

    /// Scale an integer addend by the pointee size, for `p += n`.
    pub(crate) fn scale_pointer_addend(
        &mut self,
        ptr_typ: TypeId,
        value_typ: TypeId,
        rhs: PseudoId,
    ) -> PseudoId {
        let elem_type = self.types.base_type(ptr_typ).unwrap_or(self.types.char_id);
        let elem_size = self.types.size_bits(elem_type) / 8;
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

    /// The arithmetic opcode a compound assignment operator applies.
    pub(crate) fn compound_assign_opcode(&self, op: AssignOp, typ: TypeId) -> Opcode {
        let is_float = self.types.is_float(typ);
        let is_unsigned = self.types.is_unsigned(typ);
        match op {
            AssignOp::Assign => unreachable!("plain assignment has no arithmetic opcode"),
            AssignOp::AddAssign => {
                if is_float {
                    Opcode::FAdd
                } else {
                    Opcode::Add
                }
            }
            AssignOp::SubAssign => {
                if is_float {
                    Opcode::FSub
                } else {
                    Opcode::Sub
                }
            }
            AssignOp::MulAssign => {
                if is_float {
                    Opcode::FMul
                } else {
                    Opcode::Mul
                }
            }
            AssignOp::DivAssign => {
                if is_float {
                    Opcode::FDiv
                } else if is_unsigned {
                    Opcode::DivU
                } else {
                    Opcode::DivS
                }
            }
            AssignOp::ModAssign => {
                if is_unsigned {
                    Opcode::ModU
                } else {
                    Opcode::ModS
                }
            }
            AssignOp::AndAssign => Opcode::And,
            AssignOp::OrAssign => Opcode::Or,
            AssignOp::XorAssign => Opcode::Xor,
            AssignOp::ShlAssign => Opcode::Shl,
            AssignOp::ShrAssign => {
                if is_unsigned {
                    Opcode::Lsr
                } else {
                    Opcode::Asr
                }
            }
        }
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

        // A pointer steps by one element; everything else by one.
        let delta = self.incdec_delta(typ);
        let is_float = self.types.is_float(typ);
        let opcode = match (is_inc, is_float) {
            (true, false) => Opcode::Add,
            (false, false) => Opcode::Sub,
            (true, true) => Opcode::FAdd,
            (false, true) => Opcode::FSub,
        };

        let old = self.emit_atomic_rmw(&lv, opcode, delta);
        if !prefix {
            return Some(old);
        }

        let bits = self.types.size_bits(typ);
        let new = self.alloc_reg_pseudo();
        self.emit(Instruction::binop(opcode, new, old, delta, typ, bits));

        // `++b` on an _Atomic _Bool must still yield 0 or 1.
        if self.types.kind(typ) == TypeKind::Bool {
            return Some(self.emit_convert(new, self.types.int_id, typ));
        }
        Some(new)
    }

    /// The amount `++`/`--` steps by: the pointee size for a pointer, else 1.
    pub(crate) fn incdec_delta(&mut self, typ: TypeId) -> PseudoId {
        if self.types.kind(typ) == TypeKind::Pointer {
            let elem = self.types.base_type(typ).unwrap_or(self.types.char_id);
            let bytes = (self.types.size_bits(elem) / 8).max(1);
            return self.emit_const(bytes as i128, self.types.long_id);
        }
        if self.types.is_float(typ) {
            return self.emit_fconst(1.0, typ);
        }
        self.emit_const(1, typ)
    }
}
