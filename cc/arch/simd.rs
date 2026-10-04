//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The lane-wise GNU vector operations each target computes with its packed
// instructions. The linearizer builds an `Opcode::Simd` for exactly what this
// table lists and computes everything else lane by lane; each backend emits
// exactly what it lists. It is the one statement of what is native.
//

use crate::float::FpFormat;
use crate::ir::{ShuffleIndices, SimdOp};
use crate::target::{Arch, Target, X86Simd};
use crate::types::{TypeId, TypeTable};

/// Whether `target` computes `op` on vectors of type `vec` with packed
/// instructions.
///
/// Only whole-register vectors qualify: sixteen bytes (an XMM or Q
/// register) or eight (the low half, or a D register). Integer lanes of one
/// to eight bytes and floating lanes of `float` or `double` format.
///
/// A shuffle's answer depends on its indices: it is asked with
/// [`native_shuffle`], and this answers `false` for one.
pub fn native(target: &Target, op: SimdOp, vec: TypeId, types: &TypeTable) -> bool {
    let Some(Whole {
        bytes,
        lane_bytes,
        float,
    }) = whole_register(vec, types)
    else {
        return false;
    };
    if op == SimdOp::Shuffle || op.float_lanes().is_some_and(|f| f != float) {
        return false;
    }
    match target.arch {
        Arch::X86_64 => x86_64(op, bytes, lane_bytes, float, target.x86_isa.simd),
        Arch::Aarch64 => aarch64(op, lane_bytes),
    }
}

/// Whether `target` computes the constant shuffle `idx` of vectors of type
/// `vec` with packed instructions: any on NEON (`tbl`), what
/// [`x86_64_shuffle`] finds on SSE2.
pub fn native_shuffle(
    target: &Target,
    idx: &ShuffleIndices,
    vec: TypeId,
    types: &TypeTable,
) -> bool {
    let Some(Whole {
        bytes, lane_bytes, ..
    }) = whole_register(vec, types)
    else {
        return false;
    };
    match target.arch {
        Arch::X86_64 => {
            bytes == 16
                && (x86_64_shuffle(idx, lane_bytes).is_some()
                    || (target.x86_isa.simd >= X86Simd::Ssse3
                        && x86_64_byte_shuffle(idx, lane_bytes).is_some()))
        }
        Arch::Aarch64 => true,
    }
}

/// The shape of a vector that fills a register, or its low half.
struct Whole {
    /// Sixteen or eight.
    bytes: usize,
    lane_bytes: usize,
    /// Whether the lanes are `float` or `double`; else integers.
    float: bool,
}

/// The shape of vectors of type `vec` when a packed instruction can take
/// them: sixteen or eight bytes, of integer lanes of one to eight bytes or
/// floating lanes of `float` or `double` format.
fn whole_register(vec: TypeId, types: &TypeTable) -> Option<Whole> {
    let (lane, _) = types.vector_lanes(vec)?;
    let bytes = types.size_bytes(vec);
    if bytes != 16 && bytes != 8 {
        return None;
    }
    let lane_bytes = types.size_bytes(lane);
    let float = match types.fp_format(lane) {
        Some(FpFormat::Binary32 | FpFormat::Binary64) => true,
        Some(_) => return None,
        None if types.is_integer(lane) && lane_bytes <= 8 => false,
        None => return None,
    };
    Some(Whole {
        bytes,
        lane_bytes,
        float,
    })
}

/// SSE2: integer lanes in either width -- an eight-byte vector rides in the
/// low half of an XMM register, and a lane-wise integer operation never
/// reads across lanes, so the upper half is harmless. Floating lanes only
/// at sixteen bytes: whatever the upper half holds, a packed operation on
/// it could raise a floating-point exception flag the program never asked
/// for.
///
/// SSE2 multiplies 16-bit lanes only (`pmullw`), and shifts every lane by
/// one count -- no bytes, and no 64-bit arithmetic right shift. `simd` adds
/// what the flags allow: SSE4.1's `pmulld`, `pcmpeqq` and `pmaxuw`/`pmaxud`
/// (unsigned order beyond bytes), SSE4.2's `pcmpgtq`.
fn x86_64(op: SimdOp, bytes: usize, lane_bytes: usize, float: bool, simd: X86Simd) -> bool {
    if float && bytes != 16 {
        return false;
    }
    let sse41 = simd >= X86Simd::Sse41;
    match op {
        SimdOp::Mul => lane_bytes == 2 || (sse41 && lane_bytes == 4),
        SimdOp::Shl | SimdOp::Lsr | SimdOp::Asr => false,
        SimdOp::ShlScalar | SimdOp::LsrScalar => lane_bytes >= 2,
        SimdOp::AsrScalar => matches!(lane_bytes, 2 | 4),
        // pcmpeq and pcmpgt reach dwords; pcmpeqq is SSE4.1, pcmpgtq 4.2.
        SimdOp::CmpEq | SimdOp::CmpNe => lane_bytes <= 4 || sse41,
        SimdOp::CmpGt | SimdOp::CmpGe => lane_bytes <= 4 || simd >= X86Simd::Sse42,
        // An unsigned order is a maximum compared: pmaxub, pmaxuw/ud.
        SimdOp::CmpGtU | SimdOp::CmpGeU => lane_bytes == 1 || (sse41 && lane_bytes <= 4),
        // cvtdq2ps and cvttps2dq: signed 32-bit lanes only.
        SimdOp::CvtSF | SimdOp::CvtFS => bytes == 16 && lane_bytes == 4,
        SimdOp::CvtUF | SimdOp::CvtFU => false,
        _ => true,
    }
}

/// The SSSE3 `pshufb` control of a one-source shuffle of byte or word
/// lanes: each result byte the source byte it names, or (`0x80`) zero for an
/// unspecified lane. `None` for a shuffle of two sources, or of wider lanes.
pub fn x86_64_byte_shuffle(idx: &ShuffleIndices, lane_bytes: usize) -> Option<(usize, [u8; 16])> {
    let n = 16 / lane_bytes;
    if idx.len() != n || !matches!(lane_bytes, 1 | 2) {
        return None;
    }
    let mut source = None;
    let mut control = [0x80u8; 16];
    for k in 0..n {
        let Some(i) = idx.lane(k) else { continue };
        if *source.get_or_insert(i / n) != i / n {
            return None;
        }
        for b in 0..lane_bytes {
            control[k * lane_bytes + b] = ((i % n) * lane_bytes + b) as u8;
        }
    }
    Some((source.unwrap_or(0), control))
}

/// How SSE2 computes a sixteen-byte shuffle: one source's dwords
/// rearranged (`pshufd`), or two dwords from one source then two from one
/// source (`shufps`), or a qword from each (`shufpd`). `first` and
/// `second` are operand numbers: 0, or 1 for the second vector.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum X86Shuffle {
    Pshufd {
        src: usize,
        imm: u8,
    },
    Shufps {
        first: usize,
        second: usize,
        imm: u8,
    },
    Shufpd {
        first: usize,
        second: usize,
        imm: u8,
    },
}

/// The SSE2 form of the shuffle `idx` of lanes `lane_bytes` wide, if it
/// has one. An unspecified lane fits any form. Bytes and words need SSSE3's
/// `pshufb`.
pub fn x86_64_shuffle(idx: &ShuffleIndices, lane_bytes: usize) -> Option<X86Shuffle> {
    let n = 16 / lane_bytes;
    if idx.len() != n || !matches!(lane_bytes, 4 | 8) {
        return None;
    }
    // Which operand a group of result lanes comes from: the one source all
    // its specified lanes share, if they share one.
    let source = |lanes: std::ops::Range<usize>| -> Option<Option<usize>> {
        let mut src = None;
        for k in lanes {
            if let Some(i) = idx.lane(k) {
                match src {
                    None => src = Some(i / n),
                    Some(s) if s == i / n => {}
                    Some(_) => return None,
                }
            }
        }
        Some(src)
    };
    let within = |k: usize| idx.lane(k).map_or(0, |i| i % n) as u8;
    if lane_bytes == 4 {
        let imm = (0..4).fold(0, |imm, k| imm | within(k) << (2 * k));
        if let Some(src) = source(0..4) {
            return Some(X86Shuffle::Pshufd {
                src: src.unwrap_or(0),
                imm,
            });
        }
        let first = source(0..2)?.unwrap_or(0);
        let second = source(2..4)?.unwrap_or(0);
        return Some(X86Shuffle::Shufps { first, second, imm });
    }
    let imm = within(0) | within(1) << 1;
    if let Some(src) = source(0..2) {
        // A qword is a pair of dwords.
        let pick = |k| 2 * within(k);
        let imm = pick(0) | (pick(0) + 1) << 2 | pick(1) << 4 | (pick(1) + 1) << 6;
        return Some(X86Shuffle::Pshufd {
            src: src.unwrap_or(0),
            imm,
        });
    }
    let first = source(0..1)?.unwrap_or(0);
    let second = source(1..2)?.unwrap_or(0);
    Some(X86Shuffle::Shufpd { first, second, imm })
}

/// NEON has the operations at both widths, in the Q register or its D
/// half. It multiplies lanes up to 32 bits, and shifts each lane by its own
/// count (`ushl`/`sshl`); a single count is spread to every lane first.
///
/// Any constant shuffle is a `tbl`, and every same-width conversion
/// between integer and floating lanes is one instruction.
fn aarch64(op: SimdOp, lane_bytes: usize) -> bool {
    match op {
        SimdOp::Mul => lane_bytes <= 4,
        SimdOp::ShlScalar | SimdOp::LsrScalar | SimdOp::AsrScalar => false,
        SimdOp::CvtSF | SimdOp::CvtUF | SimdOp::CvtFS | SimdOp::CvtFU => lane_bytes >= 4,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::target::Os;

    #[test]
    fn test_native_vector_operations() {
        let target = Target::new(Arch::X86_64, Os::Linux);
        let mut types = TypeTable::new(&target);
        let v4si = types.vector_of(types.int_id, 4, None);
        let v2si = types.vector_of(types.int_id, 2, None);
        let v16qi = types.vector_of(types.char_id, 16, None);
        let v4sf = types.vector_of(types.float_id, 4, None);
        let v2sf = types.vector_of(types.float_id, 2, None);
        let v2df = types.vector_of(types.double_id, 2, None);
        let v2hi = types.vector_of(types.short_id, 2, None);
        let v1ti = types.vector_of(types.int128_id, 1, None);
        let n = |op, v| native(&target, op, v, &types);
        for v in [v4si, v2si, v16qi] {
            for op in [
                SimdOp::Add,
                SimdOp::Sub,
                SimdOp::Xor,
                SimdOp::Not,
                SimdOp::Neg,
            ] {
                assert!(n(op, v), "{op:?}");
            }
            assert!(!n(SimdOp::FAdd, v));
        }
        for v in [v4sf, v2df] {
            assert!(n(SimdOp::FDiv, v) && n(SimdOp::FNeg, v));
            assert!(!n(SimdOp::Add, v));
        }
        // Eight bytes of floats, four bytes of anything, and a lane wider
        // than a register's lane stay lane by lane.
        assert!(!n(SimdOp::FAdd, v2sf));
        assert!(!n(SimdOp::Add, v2hi));
        assert!(!n(SimdOp::Add, v1ti));
        // NEON has them all, eight-byte floats included.
        let a64 = Target::new(Arch::Aarch64, Os::Linux);
        for (op, v) in [
            (SimdOp::Add, v4si),
            (SimdOp::FAdd, v2sf),
            (SimdOp::Neg, v2si),
        ] {
            assert!(native(&a64, op, v, &types), "{op:?}");
        }
        assert!(!native(&a64, SimdOp::Add, v2hi, &types));
    }

    #[test]
    fn test_native_splat_multiply_and_shift() {
        let x86 = Target::new(Arch::X86_64, Os::Linux);
        let a64 = Target::new(Arch::Aarch64, Os::Linux);
        let mut types = TypeTable::new(&x86);
        let v16qi = types.vector_of(types.char_id, 16, None);
        let v8hi = types.vector_of(types.short_id, 8, None);
        let v4si = types.vector_of(types.int_id, 4, None);
        let v2di = types.vector_of(types.long_id, 2, None);
        let v4sf = types.vector_of(types.float_id, 4, None);
        let n = |t: &Target, op, v| native(t, op, v, &types);
        // A splat of either lane kind.
        for v in [v16qi, v4si, v2di, v4sf] {
            assert!(n(&x86, SimdOp::Splat, v) && n(&a64, SimdOp::Splat, v));
        }
        // SSE2 multiplies words; NEON up to 32-bit lanes.
        assert!(n(&x86, SimdOp::Mul, v8hi) && !n(&x86, SimdOp::Mul, v4si));
        assert!(n(&a64, SimdOp::Mul, v4si) && !n(&a64, SimdOp::Mul, v2di));
        assert!(!n(&x86, SimdOp::Mul, v4sf));
        // SSE2 shifts by one count, not bytes, no 64-bit arithmetic shift;
        // NEON by per-lane counts.
        assert!(n(&x86, SimdOp::ShlScalar, v2di) && n(&x86, SimdOp::AsrScalar, v4si));
        assert!(!n(&x86, SimdOp::ShlScalar, v16qi) && !n(&x86, SimdOp::AsrScalar, v2di));
        assert!(!n(&x86, SimdOp::Shl, v4si));
        assert!(n(&a64, SimdOp::Asr, v16qi) && n(&a64, SimdOp::Lsr, v2di));
        assert!(!n(&a64, SimdOp::ShlScalar, v4si));
    }

    /// What the `-m` flags add: 32-bit multiply, 64-bit compares, unsigned
    /// orders of words and dwords, and byte and word shuffles.
    #[test]
    fn test_native_with_sse4() {
        let mut x86 = Target::new(Arch::X86_64, Os::Linux);
        let mut types = TypeTable::new(&x86);
        let v4si = types.vector_of(types.int_id, 4, None);
        let v2di = types.vector_of(types.long_id, 2, None);
        let v8hu = types.vector_of(types.ushort_id, 8, None);
        let v16qu = types.vector_of(types.uchar_id, 16, None);
        let words = ShuffleIndices::new(&[
            Some(7),
            Some(6),
            Some(5),
            Some(4),
            None,
            Some(2),
            Some(1),
            Some(0),
        ]);
        // The baseline: unsigned order of bytes only.
        assert!(!native(&x86, SimdOp::Mul, v4si, &types));
        assert!(native(&x86, SimdOp::CmpGeU, v16qu, &types));
        assert!(!native(&x86, SimdOp::CmpGeU, v8hu, &types));
        assert!(!native_shuffle(&x86, &words, v8hu, &types));
        x86.x86_isa.simd = X86Simd::Ssse3;
        assert!(native_shuffle(&x86, &words, v8hu, &types));
        x86.x86_isa.simd = X86Simd::Sse41;
        assert!(native(&x86, SimdOp::Mul, v4si, &types));
        assert!(native(&x86, SimdOp::CmpEq, v2di, &types));
        assert!(!native(&x86, SimdOp::CmpGt, v2di, &types));
        assert!(native(&x86, SimdOp::CmpGtU, v8hu, &types));
        assert!(!native(&x86, SimdOp::CmpGtU, v2di, &types));
        x86.x86_isa.simd = X86Simd::Sse42;
        assert!(native(&x86, SimdOp::CmpGe, v2di, &types));
        // The control of a word reversal, its unspecified lane zero.
        let (src, ctl) = x86_64_byte_shuffle(&words, 2).unwrap();
        assert_eq!(src, 0);
        assert_eq!(&ctl[..10], &[14, 15, 12, 13, 10, 11, 8, 9, 0x80, 0x80]);
    }

    #[test]
    fn test_native_comparisons() {
        let x86 = Target::new(Arch::X86_64, Os::Linux);
        let a64 = Target::new(Arch::Aarch64, Os::Linux);
        let mut types = TypeTable::new(&x86);
        let v16qi = types.vector_of(types.char_id, 16, None);
        let v4si = types.vector_of(types.int_id, 4, None);
        let v2di = types.vector_of(types.long_id, 2, None);
        let v4sf = types.vector_of(types.float_id, 4, None);
        let v2sf = types.vector_of(types.float_id, 2, None);
        let n = |t: &Target, op, v| native(t, op, v, &types);
        for v in [v16qi, v4si] {
            assert!(n(&x86, SimdOp::CmpEq, v) && n(&x86, SimdOp::CmpGe, v));
        }
        // SSE2's unsigned order is pmaxub's, bytes only.
        assert!(n(&x86, SimdOp::CmpGtU, v16qi) && !n(&x86, SimdOp::CmpGtU, v4si));
        assert!(!n(&x86, SimdOp::CmpEq, v2di) && !n(&x86, SimdOp::CmpGt, v2di));
        assert!(n(&x86, SimdOp::FCmpNe, v4sf) && !n(&x86, SimdOp::FCmpNe, v2sf));
        assert!(!n(&x86, SimdOp::CmpEq, v4sf) && !n(&x86, SimdOp::FCmpEq, v4si));
        for v in [v16qi, v4si, v2di] {
            assert!(n(&a64, SimdOp::CmpGtU, v) && n(&a64, SimdOp::CmpNe, v));
        }
        assert!(n(&a64, SimdOp::FCmpGe, v2sf));
    }

    #[test]
    fn test_x86_64_shuffle_forms() {
        let idx = |l: &[Option<u32>]| ShuffleIndices::new(l);
        let s = |l: &[Option<u32>], w| x86_64_shuffle(&idx(l), w);
        // One source: pshufd, the second operand's too.
        assert_eq!(
            s(&[Some(3), Some(2), Some(1), Some(0)], 4),
            Some(X86Shuffle::Pshufd { src: 0, imm: 0x1b })
        );
        assert_eq!(
            s(&[Some(4), None, Some(4), Some(5)], 4),
            Some(X86Shuffle::Pshufd { src: 1, imm: 0x40 })
        );
        // Two from each: shufps.
        assert_eq!(
            s(&[Some(1), Some(0), Some(6), Some(7)], 4),
            Some(X86Shuffle::Shufps {
                first: 0,
                second: 1,
                imm: 0xe1
            })
        );
        // Mixed within a pair: no SSE2 form.
        assert_eq!(s(&[Some(0), Some(4), Some(1), Some(5)], 4), None);
        // Qwords: a swap is pshufd, one from each shufpd.
        assert_eq!(
            s(&[Some(1), Some(0)], 8),
            Some(X86Shuffle::Pshufd { src: 0, imm: 0x4e })
        );
        assert_eq!(
            s(&[Some(3), Some(0)], 8),
            Some(X86Shuffle::Shufpd {
                first: 1,
                second: 0,
                imm: 1
            })
        );
        // Words and bytes wait for pshufb.
        assert_eq!(s(&[Some(0); 8], 2), None);
    }

    /// A shuffle is asked about with its indices; `native` answers no.
    #[test]
    fn test_native_shuffle() {
        let x86 = Target::new(Arch::X86_64, Os::Linux);
        let a64 = Target::new(Arch::Aarch64, Os::Linux);
        let mut types = TypeTable::new(&x86);
        let v4si = types.vector_of(types.int_id, 4, None);
        let v8hi = types.vector_of(types.short_id, 8, None);
        let rev = ShuffleIndices::new(&[Some(3), Some(2), Some(1), Some(0)]);
        let words = ShuffleIndices::new(&[Some(0); 8]);
        assert!(native_shuffle(&x86, &rev, v4si, &types));
        assert!(!native_shuffle(&x86, &words, v8hi, &types));
        assert!(native_shuffle(&a64, &words, v8hi, &types));
        assert!(!native(&a64, SimdOp::Shuffle, v4si, &types));
    }
}
