//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Complex multiplication and division of constants
//
// A floating complex `*` or `/` is a call to libgcc's `__mul?c3` or
// `__div?c3` (`emit_complex_float_muldiv`), whose four arguments are the
// operands' halves in the routine's format. Once the optimizer has made all
// four constant, the call is computed here by `FloatVal::complex_mul` and
// `FloatVal::complex_div` -- libgcc's own algorithms, which a static
// initializer is folded by too -- and its result written where the call
// would have written it.
//
// Only what the program would compute is folded, bit for bit:
// - Only libgcc's routines are modelled, so only on Linux: Apple and FreeBSD
//   link compiler-rt's, whose division scales by `logb` instead, and whose
//   build's contraction c17 has no way to know.
// - aarch64's libgcc fuses Smith's method's products into its sums, and a
//   division there is folded with the same fused steps.
// - As `constfold` folds no scalar operation on an infinity or a NaN, or
//   whose result is not finite, neither is a complex one: the run-time call
//   raises the exceptions such operations raise (C17 7.6), and the fold
//   would drop them. That leaves Annex G's recovery, which only a NaN + NaNi
//   intermediate reaches, to run time.
//

use super::{Facts, Folded};
use crate::float::{ComplexRoutineFormat, Contraction, FloatVal};
use crate::ir::build::Builder;
use crate::ir::Instruction;
use crate::parse::ast::LibFn;
use crate::target::{Arch, Os, Target};

/// What the call `insn` to `__mul?c3` (`MulComplex`) or `__div?c3`
/// (`DivComplex`) computes, when its four halves are finite constants and so
/// is its result.
pub(super) fn fold(known: LibFn, insn: &Instruction, facts: &Facts) -> Option<Folded> {
    // The halves are the last four arguments: a hidden pointer to the
    // result, where the ABI returns it through one, comes first.
    let first = insn.src.len().checked_sub(4)?;
    let typ = *insn.arg_types.last()?;
    let fmt = facts.types.fp_format(typ)?;
    let routine = fmt.complex_routine_format();
    if routine.format() != fmt {
        // Not the routine's own format: not a call the linearizer made.
        return None;
    }
    let mut halves = [FloatVal::ZERO; 4];
    for (half, &p) in halves.iter_mut().zip(&insn.src[first..]) {
        *half = facts.float(p, typ).filter(|v| v.is_finite())?;
    }
    let contraction = libgcc_contraction(facts.target, routine)?;
    let [a, b, c, d] = halves;
    let (re, im) = match known {
        LibFn::MulComplex => FloatVal::complex_mul((a, b), (c, d), fmt),
        LibFn::DivComplex => FloatVal::complex_div_by((a, b), (c, d), fmt, contraction)?,
        _ => return None,
    };
    (re.is_finite() && im.is_finite()).then_some(Folded::Complex(re, im))
}

/// How `target`'s libgcc divides in `routine`'s format; `None` where the
/// target's routines are not libgcc's.
///
/// Multiplication asks only whether they are: `__mul?c3` fuses only inside
/// Annex G's recovery, where fusing changes nothing but a NaN's sign (see
/// the comment over `FloatVal::complex_mul`).
fn libgcc_contraction(target: &Target, routine: ComplexRoutineFormat) -> Option<Contraction> {
    if target.os != Os::Linux {
        return None;
    }
    Some(match (target.arch, routine) {
        (Arch::Aarch64, ComplexRoutineFormat::Binary32 | ComplexRoutineFormat::Binary64) => {
            Contraction::Fused
        }
        _ => Contraction::Separate,
    })
}

/// Write `(re, im)` where `call` returns its result, in place of the call:
/// the storage its target names, or, where it returns through a hidden
/// pointer, the storage that pointer points at -- and the call's own value,
/// that pointer, stays defined.
pub(super) fn materialize(b: &mut Builder, call: &Instruction, (re, im): (FloatVal, FloatVal)) {
    let typ = *call
        .arg_types
        .last()
        .expect("a complex routine takes halves");
    let size = b.types.size_bits(typ);
    let dest = if call.returns_via_sret() {
        let slot = call.src[0];
        if let (Some(t), Some(ptr)) = (call.target, call.typ) {
            b.copy_into(t, slot, ptr, call.size);
        }
        slot
    } else {
        call.target.expect("a complex routine's result has storage")
    };
    for (half, at) in [(re, 0), (im, i64::from(size / 8))] {
        let v = b.float_constant(half, typ, size);
        b.store(v, dest, at, typ, size);
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::run_for;
    use super::*;
    use crate::abi::ArgClass;
    use crate::ir::strdata::fixture::Fixture;
    use crate::ir::{CallAbiInfo, Opcode, Pseudo, PseudoId};
    use crate::types::TypeId;

    fn linux(arch: Arch) -> Target {
        Target::new(arch, Os::Linux)
    }

    /// A float constant of `typ`.
    fn fconst(fx: &mut Fixture, v: f64, typ: TypeId) -> PseudoId {
        let size = fx.types.size_bits(typ);
        let f = fx.func();
        let p = f.alloc_pseudo();
        f.add_pseudo(Pseudo::fval(p, FloatVal::from_f64(v)));
        fx.push(
            Instruction::new(Opcode::SetVal)
                .with_target(p)
                .with_type_and_size(typ, size),
        );
        p
    }

    /// `known`'s call as the linearizer makes it, on halves of `typ`: its
    /// result in a `__cret` temporary it names.
    fn call(fx: &mut Fixture, known: LibFn, halves: [PseudoId; 4], typ: TypeId) -> PseudoId {
        let complex = fx.types.make_complex(typ);
        let f = fx.func();
        let ret = f.alloc_pseudo();
        f.add_pseudo(Pseudo::sym(ret, "__cret".to_string()));
        f.add_local("__cret", ret, complex, false, false, None, None);
        let size = fx.types.size_bits(complex);
        let mut insn = Instruction::call(
            Some(ret),
            "__muldc3",
            halves.to_vec(),
            vec![typ; 4],
            complex,
            size,
        );
        insn.known = Some(known);
        fx.push(insn);
        ret
    }

    /// The constants stored to `dest`, by offset, and whether a call is left.
    fn stored(fx: &Fixture, insns: &[Instruction], dest: PseudoId) -> (Vec<(i64, f64)>, bool) {
        let f = &fx.module.functions[0];
        let value = |p| match f.get_pseudo(p).map(|p| &p.kind) {
            Some(crate::ir::PseudoKind::FVal(v)) => v.to_f64(),
            other => panic!("not a float constant: {other:?}"),
        };
        let stores = insns
            .iter()
            .filter(|i| i.op == Opcode::Store && i.src[0] == dest)
            .map(|i| (i.offset, value(i.src[1])))
            .collect();
        (stores, insns.iter().any(|i| i.op == Opcode::Call))
    }

    /// `(1 + 2i) * (3 - i)` is `5 + 5i`, stored to the call's storage in
    /// place of the call.
    #[test]
    fn a_constant_product_is_stored_where_the_call_returns_it() {
        let mut fx = Fixture::new();
        let dbl = fx.types.double_id;
        let h = [1.0, 2.0, 3.0, -1.0].map(|v| fconst(&mut fx, v, dbl));
        let ret = call(&mut fx, LibFn::MulComplex, h, dbl);
        let insns = run_for(&mut fx, &linux(Arch::X86_64));
        assert_eq!(stored(&fx, &insns, ret), (vec![(0, 5.0), (8, 5.0)], false));
    }

    /// `(1 + 2i) / (3 + 4i)` is `0.44 + 0.08i`; `float` halves are four
    /// bytes apart.
    #[test]
    fn a_constant_quotient_folds_in_its_format() {
        let mut fx = Fixture::new();
        let dbl = fx.types.double_id;
        let h = [1.0, 2.0, 3.0, 4.0].map(|v| fconst(&mut fx, v, dbl));
        let ret = call(&mut fx, LibFn::DivComplex, h, dbl);
        let insns = run_for(&mut fx, &linux(Arch::X86_64));
        assert_eq!(
            stored(&fx, &insns, ret),
            (vec![(0, 0.44), (8, 0.08)], false)
        );

        let mut fx = Fixture::new();
        let flt = fx.types.float_id;
        let h = [1.0, 1.0, 1.0, -1.0].map(|v| fconst(&mut fx, v, flt));
        let ret = call(&mut fx, LibFn::DivComplex, h, flt);
        let insns = run_for(&mut fx, &linux(Arch::Aarch64));
        assert_eq!(stored(&fx, &insns, ret), (vec![(0, 0.0), (4, 1.0)], false));
    }

    /// Where the result comes back through a hidden pointer, it is stored
    /// through that pointer, and the call's value is still that pointer.
    #[test]
    fn a_hidden_return_pointer_receives_the_result() {
        let mut fx = Fixture::new();
        let dbl = fx.types.double_id;
        let slot = fx.unknown();
        let h = [1.0, 2.0, 3.0, -1.0].map(|v| fconst(&mut fx, v, dbl));
        let ptr = fx.types.void_ptr_id;
        let t = fx.fresh();
        let mut insn = Instruction::call(
            Some(t),
            "__muldc3",
            [slot].into_iter().chain(h).collect(),
            [ptr].into_iter().chain([dbl; 4]).collect(),
            ptr,
            64,
        );
        let indirect = ArgClass::Indirect {
            align: 16,
            size_bytes: 16,
        };
        insn.abi_info = Some(Box::new(CallAbiInfo::new(vec![], indirect)));
        insn.known = Some(LibFn::MulComplex);
        fx.push(insn);
        let insns = run_for(&mut fx, &linux(Arch::X86_64));
        assert_eq!(stored(&fx, &insns, slot), (vec![(0, 5.0), (8, 5.0)], false));
        let copy = insns.iter().find(|i| i.target == Some(t)).unwrap();
        assert_eq!((copy.op, copy.src[0]), (Opcode::Copy, slot));
    }

    /// An operand that is not constant, or not finite, leaves the call; so
    /// does a result that is not finite, and any target whose routines are
    /// not libgcc's.
    #[test]
    fn what_the_program_would_compute_differently_is_left_to_it() {
        let cases: [(LibFn, [Option<f64>; 4], Target); 7] = [
            (
                LibFn::MulComplex,
                [Some(1.0), None, Some(1.0), Some(1.0)],
                linux(Arch::X86_64),
            ),
            (
                LibFn::MulComplex,
                [Some(f64::INFINITY), Some(0.0), Some(1.0), Some(1.0)],
                linux(Arch::X86_64),
            ),
            (
                LibFn::MulComplex,
                [Some(f64::NAN), Some(0.0), Some(1.0), Some(1.0)],
                linux(Arch::Aarch64),
            ),
            (
                LibFn::MulComplex,
                [Some(1e300), Some(0.0), Some(1e300), Some(0.0)],
                linux(Arch::X86_64),
            ),
            (
                LibFn::DivComplex,
                [Some(1.0), Some(1.0), Some(0.0), Some(0.0)],
                linux(Arch::X86_64),
            ),
            (
                LibFn::DivComplex,
                [Some(1.0), Some(2.0), Some(3.0), Some(4.0)],
                Target::new(Arch::Aarch64, Os::MacOS),
            ),
            (
                LibFn::MulComplex,
                [Some(1.0), Some(2.0), Some(3.0), Some(4.0)],
                Target::new(Arch::X86_64, Os::FreeBSD),
            ),
        ];
        for (known, halves, target) in cases {
            let mut fx = Fixture::new();
            let dbl = fx.types.double_id;
            let h = halves.map(|v| match v {
                Some(v) => fconst(&mut fx, v, dbl),
                None => fx.unknown(),
            });
            let ret = call(&mut fx, known, h, dbl);
            let insns = run_for(&mut fx, &target);
            assert_eq!(
                stored(&fx, &insns, ret),
                (vec![], true),
                "{known:?} {halves:?} {target:?}"
            );
        }
    }

    /// aarch64's `__divdc3` fuses `x * r + y`, and the fold follows it:
    /// `(1 + i) / (3 + 7i)` differs in the last place of both halves between
    /// the two, and each target gets the one its libgcc computes (x86-64's
    /// and aarch64's run-time results).
    #[test]
    fn a_division_rounds_as_the_targets_libgcc_does() {
        let quotient = |target: &Target| {
            let mut fx = Fixture::new();
            let dbl = fx.types.double_id;
            let h = [1.0, 1.0, 3.0, 7.0].map(|v| fconst(&mut fx, v, dbl));
            let ret = call(&mut fx, LibFn::DivComplex, h, dbl);
            let insns = run_for(&mut fx, target);
            stored(&fx, &insns, ret).0
        };
        let halves = |re: u64, im: u64| vec![(0, f64::from_bits(re)), (8, f64::from_bits(im))];
        assert_eq!(
            quotient(&linux(Arch::X86_64)),
            halves(0x3fc6_11a7_b961_1a7c, 0xbfb1_a7b9_611a_7b96)
        );
        assert_eq!(
            quotient(&linux(Arch::Aarch64)),
            halves(0x3fc6_11a7_b961_1a7b, 0xbfb1_a7b9_611a_7b95)
        );
    }

    /// Which targets run libgcc's routines, and which of those fuse.
    #[test]
    fn only_linux_divides_by_libgcc() {
        use ComplexRoutineFormat as R;
        let aarch64 = linux(Arch::Aarch64);
        let x86 = linux(Arch::X86_64);
        assert_eq!(
            libgcc_contraction(&aarch64, R::Binary64),
            Some(Contraction::Fused)
        );
        assert_eq!(
            libgcc_contraction(&aarch64, R::Binary32),
            Some(Contraction::Fused)
        );
        assert_eq!(
            libgcc_contraction(&aarch64, R::Binary128),
            Some(Contraction::Separate)
        );
        assert_eq!(
            libgcc_contraction(&x86, R::Binary64),
            Some(Contraction::Separate)
        );
        assert_eq!(
            libgcc_contraction(&x86, R::X87Extended),
            Some(Contraction::Separate)
        );
        for os in [Os::MacOS, Os::FreeBSD] {
            for arch in [Arch::X86_64, Arch::Aarch64] {
                let t = Target::new(arch, os);
                assert_eq!(libgcc_contraction(&t, R::Binary64), None, "{t:?}");
            }
        }
    }
}
