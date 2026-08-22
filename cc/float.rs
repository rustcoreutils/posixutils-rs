//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Exact floating-point literal values.
//
// A C floating literal cannot be carried as an `f64`. The widest target type
// is `long double`, which on x86-64 is the x87 80-bit format with a 64-bit
// significand and a 15-bit exponent -- strictly wider than double in both
// directions. Rounding a literal to `f64` at parse time is lossy before the
// type of the literal is even known: `LDBL_MAX` becomes `inf` and `LDBL_MIN`
// becomes zero.
//
// [`FloatVal`] carries a 15-bit exponent and a 128-bit significand -- wider
// than every target format rather than equal to one. x87's 64 significand bits
// and binary128's 113 both fit, so a literal reaches either format having been
// rounded exactly once, at emission, by the routine that knows the width it is
// rounding to. `float` and `double` are strict subsets and round out of it on
// demand.
//

use std::fmt;

/// The exponent bias of the x87 80-bit extended format.
const BIAS: i32 = 16383;
/// Biased exponent denoting infinity or NaN.
const EXP_SPECIAL: u16 = 0x7FFF;
/// The largest finite biased exponent.
const EXP_MAX_FINITE: u16 = 0x7FFE;
/// The top bit of the significand, which is always set for a normal value:
/// the integer bit, held explicitly as x87 holds it.
const INTEGER_BIT: u128 = 1 << 127;
/// Significand width. Wider than any target format, so that rounding happens
/// once, on the way out, at the width being emitted.
const SIG_BITS: u32 = 128;

/// A floating-point literal, held wider than any target format.
///
/// The value is `(-1)^neg * sig * 2^(exp - BIAS - 127)` for a normal number.
/// `exp == 0` is zero (`sig == 0`) or subnormal; `exp == 0x7FFF` is infinity
/// (`sig == INTEGER_BIT`) or NaN.
#[derive(Clone, Copy, Debug)]
pub struct FloatVal {
    neg: bool,
    exp: u16,
    sig: u128,
}

/// Convert an `f64` to IEEE 754 half-precision (binary16) bits.
///
/// Rounds to nearest, ties to even, which is the default rounding mode every
/// target uses and what the standard requires of a conversion. Truncating
/// instead put every inexact `_Float16` constant one ulp below the value the
/// source named -- `0.3f16` came out 0.299805 where gcc gives 0.300049 -- so
/// a program's constants disagreed with the same constants computed at run
/// time.
pub(crate) fn f64_to_f16_bits(val: f64) -> u16 {
    let bits = val.to_bits();
    let sign = ((bits >> 63) & 1) as u16;
    let exp = ((bits >> 52) & 0x7FF) as i32;
    let frac = bits & 0xF_FFFF_FFFF_FFFF;

    if exp == 0x7FF {
        // NaN keeps its quiet bit and as much payload as fits; infinity is
        // exact either way.
        if frac != 0 {
            return (sign << 15) | 0x7E00 | ((frac >> 42) as u16 & 0x1FF);
        }
        return (sign << 15) | 0x7C00;
    }

    // Rebias: f64 is excess-1023, f16 excess-15.
    let new_exp = exp - 1023 + 15;

    if new_exp >= 31 {
        return (sign << 15) | 0x7C00;
    }

    if new_exp <= 0 {
        // Subnormal, or small enough to vanish. The significand carries its
        // hidden bit and is shifted a further `1 - new_exp` places.
        let shift = (1 - new_exp) as u32;
        let Some(drop) = 42u32.checked_add(shift).filter(|d| *d < 64) else {
            // Below half the smallest subnormal: rounds to zero, and the
            // shift would be undefined.
            return sign << 15;
        };
        let sig = frac | 0x10_0000_0000_0000;
        // A carry out of the ten fraction bits lands in the exponent field as
        // exponent 1, fraction 0 -- the smallest normal, which is exactly the
        // value being rounded to.
        return (sign << 15) | (round_to_nearest_even(sig, drop) as u16 & 0x7FFF);
    }

    let rounded = round_to_nearest_even(frac, 42);
    // A carry out of the fraction is one more power of two.
    let (new_exp, new_frac) = if rounded > 0x3FF {
        (new_exp + 1, 0)
    } else {
        (new_exp, rounded)
    };
    if new_exp >= 31 {
        return (sign << 15) | 0x7C00;
    }
    (sign << 15) | ((new_exp as u16) << 10) | (new_frac as u16 & 0x3FF)
}

/// Shift `value` right by `drop` bits, rounding to nearest with ties to even.
///
/// The result can be one greater than `value >> drop` fits, which the callers
/// read as a carry into the exponent.
fn round_to_nearest_even(value: u64, drop: u32) -> u64 {
    debug_assert!(drop > 0 && drop < 64);
    let kept = value >> drop;
    let half = 1u64 << (drop - 1);
    let rest = value & ((1u64 << drop) - 1);
    if rest > half || (rest == half && kept & 1 == 1) {
        kept + 1
    } else {
        kept
    }
}

impl FloatVal {
    /// Positive zero.
    pub const ZERO: FloatVal = FloatVal {
        neg: false,
        exp: 0,
        sig: 0,
    };

    /// An infinity with the given sign.
    pub fn infinity(neg: bool) -> Self {
        FloatVal {
            neg,
            exp: EXP_SPECIAL,
            sig: INTEGER_BIT,
        }
    }

    /// A quiet NaN.
    pub fn nan() -> Self {
        FloatVal {
            neg: false,
            exp: EXP_SPECIAL,
            // Integer bit plus the quiet bit, matching what x87 produces.
            sig: INTEGER_BIT | (1 << 126),
        }
    }

    /// Widen an `f64`. Always exact -- every double is representable.
    pub fn from_f64(v: f64) -> Self {
        let bits = v.to_bits();
        let neg = bits >> 63 != 0;
        let exp11 = ((bits >> 52) & 0x7FF) as i32;
        let frac = bits & ((1u64 << 52) - 1);

        if exp11 == 0x7FF {
            return if frac == 0 {
                Self::infinity(neg)
            } else {
                // Preserve the payload; the integer bit is always set in x87.
                FloatVal {
                    neg,
                    exp: EXP_SPECIAL,
                    sig: INTEGER_BIT | ((frac as u128) << 75),
                }
            };
        }

        if exp11 == 0 {
            if frac == 0 {
                return FloatVal {
                    neg,
                    exp: 0,
                    sig: 0,
                };
            }
            // A subnormal double is a *normal* 80-bit value: the wider
            // exponent range has room for it. Normalizing here is what the
            // old f64-to-x87 conversion skipped, and it produced a wrong
            // value rather than an imprecise one.
            let frac = frac as u128;
            let shift = frac.leading_zeros();
            let sig = frac << shift;
            // frac has value frac * 2^-1074; after shifting left by `shift`
            // the integer bit sits at bit 127, worth 2^(127-1074-shift).
            let unbiased = 127 - 1074 - shift as i32;
            return FloatVal {
                neg,
                exp: (unbiased + BIAS) as u16,
                sig,
            };
        }

        FloatVal {
            neg,
            exp: (exp11 - 1023 + BIAS) as u16,
            sig: INTEGER_BIT | ((frac as u128) << 75),
        }
    }

    /// Widen an integer. Always exact -- a `u128` magnitude has no more bits
    /// than the significand.
    pub fn from_i128(v: i128) -> Self {
        Self::from_parts(v < 0, v.unsigned_abs(), 0)
    }

    /// Build from an exact `mantissa * 2^exp2`.
    ///
    /// Exact: a `u128` mantissa has no more bits than the significand, so
    /// nothing is rounded here. Rounding happens once, at the width being
    /// emitted -- which is the point of carrying more bits than any target.
    ///
    /// This is the shape `parse_hex_float_parts` produces, so a hex literal
    /// reaches the target format without ever passing through `f64`.
    pub fn from_parts(neg: bool, mantissa: u128, exp2: i32) -> Self {
        if mantissa == 0 {
            return FloatVal {
                neg,
                exp: 0,
                sig: 0,
            };
        }

        // Normalize so the leading one sits at the top of the significand.
        let width = SIG_BITS - mantissa.leading_zeros();
        // Value is mantissa * 2^exp2; the top bit is worth 2^(exp2 + width - 1).
        let unbiased = exp2 + width as i32 - 1;
        let sig = mantissa << (SIG_BITS - width);

        Self::from_normalized(neg, sig, unbiased)
    }

    /// Assemble from a significand already normalized to bit 63, handling
    /// overflow to infinity and underflow through the subnormal range.
    fn from_normalized(neg: bool, sig: u128, unbiased: i32) -> Self {
        let biased = unbiased + BIAS;
        if biased > EXP_MAX_FINITE as i32 {
            return Self::infinity(neg);
        }
        if biased <= 0 {
            // Subnormal: shift the significand down until the exponent is 1,
            // which is the smallest the format encodes.
            //
            // The bits shifted out are rounded away, not dropped. Truncating
            // costs up to a full ulp on every subnormal, and it is directed
            // rounding at that -- always toward zero -- where the rest of this
            // conversion rounds to nearest, ties to even.
            let kept = Self::round_to(sig, (1 - biased) as u32);
            // Rounding up can carry into the integer bit, and a significand
            // with its integer bit set is the smallest normal value rather
            // than the largest subnormal one.
            let exp = u16::from(kept & INTEGER_BIT != 0);
            return FloatVal {
                neg,
                exp,
                sig: kept,
            };
        }
        FloatVal {
            neg,
            exp: biased as u16,
            sig,
        }
    }

    /// Round to `f64`, saturating to infinity on overflow.
    pub fn to_f64(self) -> f64 {
        if self.is_nan() {
            return f64::NAN;
        }
        if self.exp == EXP_SPECIAL {
            return if self.neg {
                f64::NEG_INFINITY
            } else {
                f64::INFINITY
            };
        }
        if self.sig == 0 {
            return if self.neg { -0.0 } else { 0.0 };
        }

        // Re-normalize before converting. A subnormal 80-bit value cannot
        // become a normal double -- the range only shrinks -- but a
        // subnormal's significand is not left-aligned, and the exponent
        // arithmetic below assumes it is, so align it first.
        let (sig, unbiased) = self.aligned();

        let exp11 = unbiased + 1023;
        if exp11 >= 0x7FF {
            return if self.neg {
                f64::NEG_INFINITY
            } else {
                f64::INFINITY
            };
        }

        // 53 significand bits, so 75 of the 128 are rounded away.
        const DROP: u32 = SIG_BITS - 53;
        let (frac, exp11) = if exp11 <= 0 {
            // Subnormal double, or zero.
            let shift = 1 - exp11;
            if shift >= 64 {
                return if self.neg { -0.0 } else { 0.0 };
            }
            (Self::round_to(sig >> shift, DROP) as u64, 0)
        } else {
            let rounded = Self::round_to(sig, DROP);
            // Rounding can carry out of the 53 bits, which means the
            // significand became 2.0 and the exponent steps.
            if rounded >> 52 > 1 {
                (0, exp11 + 1)
            } else {
                ((rounded & ((1u64 << 52) - 1) as u128) as u64, exp11)
            }
        };

        if exp11 >= 0x7FF {
            return if self.neg {
                f64::NEG_INFINITY
            } else {
                f64::INFINITY
            };
        }

        let bits = ((self.neg as u64) << 63) | ((exp11 as u64) << 52) | frac;
        f64::from_bits(bits)
    }

    /// Round `sig` right by `drop` bits, to nearest with ties to even.
    ///
    /// `drop` may be the full width, which is what the smallest subnormal of
    /// a format needs: everything is shifted out and only the rounding
    /// decision is left, and more than half an ulp still rounds up to one.
    /// Returning zero without asking is how `0x1p-16494L` -- binary128's
    /// smallest subnormal -- became zero.
    fn round_to(sig: u128, drop: u32) -> u128 {
        if drop > SIG_BITS {
            return 0;
        }
        let (kept, rest, half) = if drop == SIG_BITS {
            (0, sig, INTEGER_BIT)
        } else {
            (
                sig >> drop,
                sig & ((1u128 << drop) - 1),
                1u128 << (drop - 1),
            )
        };
        if rest > half || (rest == half && kept & 1 != 0) {
            kept + 1
        } else {
            kept
        }
    }

    /// The significand left-aligned with its integer bit at the top, and the
    /// unbiased exponent that goes with it.
    ///
    /// A subnormal's significand is not left-aligned and every conversion out
    /// of this format assumes it is.
    fn aligned(self) -> (u128, i32) {
        let shift = self.sig.leading_zeros();
        let unbiased = self.exp as i32 - BIAS - shift as i32 + i32::from(self.exp == 0);
        (self.sig << shift, unbiased)
    }

    /// The x87 80-bit encoding, as `(significand, sign-and-exponent)`.
    ///
    /// Rounds the significand to x87's 64 bits, which is where a value wider
    /// than the target format gives up its extra precision -- once, here,
    /// rather than at parse time when the type is not yet known.
    fn to_x87_parts(self) -> (u64, u16) {
        let se = |exp: u16| ((self.neg as u16) << 15) | (exp & 0x7FFF);

        if self.exp == EXP_SPECIAL {
            // Infinity keeps just the integer bit; a NaN keeps its payload,
            // which lives immediately below it.
            let sig = if self.is_nan() {
                (self.sig >> (SIG_BITS - 64)) as u64 | (1 << 63) | 1
            } else {
                1 << 63
            };
            return (sig, se(EXP_SPECIAL));
        }
        if self.sig == 0 {
            return (0, se(0));
        }

        const DROP: u32 = SIG_BITS - 64;
        let (aligned, unbiased) = self.aligned();
        let biased = unbiased + BIAS;

        if biased <= 0 {
            // Subnormal in x87 as well: shift down to exponent 1 and round
            // whatever falls off, exactly as `from_normalized` does.
            let shift = DROP as i32 + 1 - biased;
            if shift > SIG_BITS as i32 {
                return (0, se(0));
            }
            let rounded = Self::round_to(aligned, shift as u32) as u64;
            // A carry into the integer bit makes it the smallest normal.
            let exp = u16::from(rounded & (1 << 63) != 0);
            return (rounded, se(exp));
        }

        let rounded = Self::round_to(aligned, DROP);
        // Rounding can carry out of the 64 bits, giving 2.0 and one more
        // exponent -- which can in turn overflow to infinity.
        let (sig, biased) = if rounded > u64::MAX as u128 {
            (1u64 << 63, biased + 1)
        } else {
            (rounded as u64, biased)
        };
        if biased > EXP_MAX_FINITE as i32 {
            return (1 << 63, se(EXP_SPECIAL));
        }
        (sig, se(biased as u16))
    }

    /// The 16-byte x87 80-bit image, little-endian, as stored in memory.
    pub fn to_x87_bytes(self) -> [u8; 16] {
        let (sig, se) = self.to_x87_parts();
        let mut out = [0u8; 16];
        out[..8].copy_from_slice(&sig.to_le_bytes());
        out[8..10].copy_from_slice(&se.to_le_bytes());
        out
    }

    /// The value's bit pattern at `fp_size` bits, as an integer.
    ///
    /// This is the only way to name a floating constant in an assembler
    /// operand: an inline-asm constraint asking for an immediate gets these
    /// bits, and one asking for a general register gets them loaded into it.
    ///
    /// Widths above 64 bits are not representable in one integer; callers with
    /// a `long double` want [`to_x87_bytes`](Self::to_x87_bytes) or
    /// [`to_f128_bits`](Self::to_f128_bits) instead, and this answers with the
    /// `double` rounding rather than a wrong wide value.
    pub fn to_bits_at_width(self, fp_size: u32) -> i64 {
        match fp_size {
            16 => f64_to_f16_bits(self.to_f64()) as i64,
            32 => (self.to_f64() as f32).to_bits() as i64,
            _ => self.to_f64().to_bits() as i64,
        }
    }

    /// The IEEE binary128 encoding, as `(low, high)` 64-bit halves.
    ///
    /// Rounds the significand to binary128's 113 bits, which is the other
    /// width a `long double` can have.
    pub fn to_f128_bits(self) -> (u64, u64) {
        let sign = (self.neg as u64) << 63;
        let halves = |sig: u128, biased: i32| {
            // The integer bit is implicit in binary128, so only the 112 bits
            // below it are stored.
            let frac = sig & ((1u128 << 112) - 1);
            let hi = sign | ((biased as u64) << 48) | ((frac >> 64) as u64);
            (frac as u64, hi)
        };

        if self.exp == EXP_SPECIAL {
            let hi = sign | (0x7FFF << 48);
            return if self.is_nan() {
                // Move the payload below the explicit integer bit.
                (0, hi | (1 << 47))
            } else {
                (0, hi)
            };
        }
        if self.sig == 0 {
            return (0, sign);
        }

        // 113 significand bits, so 15 of the 128 are rounded away.
        const DROP: u32 = SIG_BITS - 113;
        let (aligned, unbiased) = self.aligned();
        let biased = unbiased + BIAS;

        if biased <= 0 {
            // Subnormal in binary128: shift down to exponent 1, rounding.
            let shift = DROP as i32 + 1 - biased;
            if shift > SIG_BITS as i32 {
                return (0, sign);
            }
            let rounded = Self::round_to(aligned, shift as u32);
            // A carry into the integer bit makes it the smallest normal, and
            // the encoding of that is a biased exponent of 1 with a zero
            // fraction -- which is what dropping the implicit bit leaves.
            let biased = i32::from(rounded >> 112 != 0);
            return halves(rounded, biased);
        }

        let rounded = Self::round_to(aligned, DROP);
        // Rounding can carry out of the 113 bits, giving 2.0 and one more
        // exponent.
        let (rounded, biased) = if rounded >> 113 != 0 {
            (rounded >> 1, biased + 1)
        } else {
            (rounded, biased)
        };
        if biased >= 0x7FFF {
            return (0, sign | (0x7FFF << 48));
        }
        halves(rounded, biased)
    }

    /// True if this is any NaN.
    pub fn is_nan(self) -> bool {
        self.exp == EXP_SPECIAL && self.sig & !INTEGER_BIT != 0
    }

    /// True for either signed zero.
    pub fn is_zero(self) -> bool {
        self.exp == 0 && self.sig == 0
    }

    /// True for positive zero specifically -- the test that decides whether a
    /// static initializer can live in `.bss`.
    pub fn is_positive_zero(self) -> bool {
        self.is_zero() && !self.neg
    }

    /// The same magnitude with the opposite sign.
    pub fn negated(self) -> Self {
        FloatVal {
            neg: !self.neg,
            ..self
        }
    }

    /// The encoding as an opaque key, for constant pooling.
    ///
    /// Distinct values must give distinct keys: pooling on the rounded `f64`
    /// merged 80-bit constants that differ only below the 53rd bit.
    pub fn key(self) -> (bool, u16, u128) {
        (self.neg, self.exp, self.sig)
    }

    /// A key for the x87 constant pool, and the name of the pooled label.
    ///
    /// Keyed on the *x87 encoding* rather than on this value: the pool holds
    /// what is emitted, and two literals that differ only below x87's 64th
    /// significand bit emit the same constant and should share one slot.
    pub fn pool_key(self) -> u128 {
        let (sig, se) = self.to_x87_parts();
        ((se as u128) << 64) | sig as u128
    }
}

impl PartialEq for FloatVal {
    /// Bitwise equality, deliberately: this compares *encodings*, not
    /// arithmetic values, so it is reflexive on NaN and distinguishes the
    /// signed zeros. Callers wanting C's `==` should compare `to_f64`.
    fn eq(&self, other: &Self) -> bool {
        self.key() == other.key()
    }
}

impl Eq for FloatVal {}

impl fmt::Display for FloatVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_f64())
    }
}

impl From<f64> for FloatVal {
    fn from(v: f64) -> Self {
        Self::from_f64(v)
    }
}

// Decimal to binary conversion

/// A minimal unsigned big integer, little-endian limbs.
///
/// Exists because converting a decimal literal exactly needs numbers far wider
/// than any primitive: `10^4932` is about 16,400 bits. Only the four
/// operations that conversion uses are implemented -- there is no general
/// bignum here, and none is wanted.
#[derive(Clone, Debug)]
struct Big {
    /// Little-endian 32-bit limbs, no trailing zeros.
    limbs: Vec<u32>,
}

impl Big {
    fn zero() -> Self {
        Big { limbs: Vec::new() }
    }

    fn from_u32(v: u32) -> Self {
        Big {
            limbs: if v == 0 { Vec::new() } else { vec![v] },
        }
    }

    fn is_zero(&self) -> bool {
        self.limbs.is_empty()
    }

    fn trim(&mut self) {
        while self.limbs.last() == Some(&0) {
            self.limbs.pop();
        }
    }

    /// Number of significant bits.
    fn bit_len(&self) -> usize {
        match self.limbs.last() {
            None => 0,
            Some(top) => self.limbs.len() * 32 - top.leading_zeros() as usize,
        }
    }

    fn bit(&self, i: usize) -> bool {
        let limb = i / 32;
        limb < self.limbs.len() && (self.limbs[limb] >> (i % 32)) & 1 == 1
    }

    /// `self = self * m + a`, the digit-accumulation step.
    fn mul_add_small(&mut self, m: u32, a: u32) {
        let mut carry = a as u64;
        for limb in self.limbs.iter_mut() {
            let v = *limb as u64 * m as u64 + carry;
            *limb = v as u32;
            carry = v >> 32;
        }
        while carry != 0 {
            self.limbs.push(carry as u32);
            carry >>= 32;
        }
        self.trim();
    }

    fn shl(&mut self, bits: usize) {
        if self.is_zero() || bits == 0 {
            return;
        }
        let (whole, part) = (bits / 32, bits % 32);
        if part != 0 {
            let mut carry = 0u32;
            for limb in self.limbs.iter_mut() {
                let v = ((*limb as u64) << part) | carry as u64;
                *limb = v as u32;
                carry = (v >> 32) as u32;
            }
            if carry != 0 {
                self.limbs.push(carry);
            }
        }
        if whole != 0 {
            let mut out = vec![0u32; whole];
            out.extend_from_slice(&self.limbs);
            self.limbs = out;
        }
    }

    /// Compare against `other`, both trimmed.
    fn cmp(&self, other: &Big) -> std::cmp::Ordering {
        use std::cmp::Ordering;
        if self.limbs.len() != other.limbs.len() {
            return self.limbs.len().cmp(&other.limbs.len());
        }
        for i in (0..self.limbs.len()).rev() {
            match self.limbs[i].cmp(&other.limbs[i]) {
                Ordering::Equal => continue,
                ord => return ord,
            }
        }
        Ordering::Equal
    }

    /// `self -= other`, which the caller has checked is no larger.
    fn sub(&mut self, other: &Big) {
        let mut borrow = 0i64;
        for i in 0..self.limbs.len() {
            let rhs = *other.limbs.get(i).unwrap_or(&0) as i64;
            let v = self.limbs[i] as i64 - rhs - borrow;
            if v < 0 {
                self.limbs[i] = (v + (1i64 << 32)) as u32;
                borrow = 1;
            } else {
                self.limbs[i] = v as u32;
                borrow = 0;
            }
        }
        self.trim();
    }

    /// Multiply by `10^n`, in chunks that fit a limb.
    fn mul_pow10(&mut self, mut n: u32) {
        const CHUNK: u32 = 9;
        const P10: u32 = 1_000_000_000;
        while n >= CHUNK {
            self.mul_add_small(P10, 0);
            n -= CHUNK;
        }
        if n != 0 {
            self.mul_add_small(10u32.pow(n), 0);
        }
    }
}

/// The number of significand bits produced before rounding.
///
/// Comfortably more than the 113 the widest target format needs -- binary128,
/// not x87's 64, which is what this was sized for and why a decimal
/// `long double` literal on aarch64 came out with its low bits clear. The
/// spare bits, and the sticky bit folded into bit 0, are what let the emitter
/// round to any narrower width without double-rounding error.
const DEC_PRECISION: usize = 127;

/// Convert `digits * 10^exp10` into an exact-enough `(mantissa, exp2)` pair
/// for [`FloatVal::from_parts`].
///
/// The result is `mantissa * 2^exp2`, correctly rounded to `DEC_PRECISION`
/// bits with a sticky bit in bit 0, which is what lets the caller round to any
/// narrower width without double-rounding error.
fn decimal_to_binary(digits: &Big, exp10: i32) -> (u128, i32) {
    if digits.is_zero() {
        return (0, 0);
    }

    // The value is num/den. Only one of them ever needs the power of ten.
    let mut num = digits.clone();
    let mut den = Big::from_u32(1);
    if exp10 >= 0 {
        num.mul_pow10(exp10 as u32);
    } else {
        den.mul_pow10((-exp10) as u32);
    }

    // Shift the numerator until the quotient has at least DEC_PRECISION bits,
    // so the division below produces every bit that can affect rounding.
    let want = DEC_PRECISION as i64 + 1;
    let have = num.bit_len() as i64 - den.bit_len() as i64;
    let shift = (want - have).max(0) as usize;
    num.shl(shift);

    // Schoolbook bit-at-a-time division. Slower than a limb-wise algorithm,
    // and much easier to be sure of; it runs only for literals a target format
    // cannot hold directly.
    let mut quotient = Big::zero();
    let mut rem = Big::zero();
    let top = num.bit_len();
    quotient.limbs = vec![0u32; top.div_ceil(32)];
    for i in (0..top).rev() {
        rem.shl(1);
        if num.bit(i) {
            if rem.limbs.is_empty() {
                rem.limbs.push(1);
            } else {
                rem.limbs[0] |= 1;
            }
        }
        if rem.cmp(&den) != std::cmp::Ordering::Less {
            rem.sub(&den);
            quotient.limbs[i / 32] |= 1 << (i % 32);
        }
    }
    quotient.trim();

    // Keep the top DEC_PRECISION bits; everything dropped, plus any remainder,
    // becomes the sticky bit.
    let qbits = quotient.bit_len();
    let mut exp2 = -(shift as i32);
    let mut sticky = !rem.is_zero();
    let mut mantissa: u128 = 0;
    if qbits > DEC_PRECISION {
        let drop = qbits - DEC_PRECISION;
        for i in 0..drop {
            if quotient.bit(i) {
                sticky = true;
            }
        }
        for i in 0..DEC_PRECISION {
            if quotient.bit(drop + i) {
                mantissa |= 1u128 << i;
            }
        }
        exp2 += drop as i32;
    } else {
        for i in 0..qbits {
            if quotient.bit(i) {
                mantissa |= 1u128 << i;
            }
        }
    }
    if sticky {
        mantissa |= 1;
    }
    (mantissa, exp2)
}

/// Parse a decimal floating literal into an exact `(mantissa, exp2)` pair.
///
/// The literal's digits and its decimal exponent are gathered exactly, then
/// scaled by a power of ten in full precision. Going through `f64` instead --
/// which is what this replaces -- costs a `long double` eleven of its
/// significand bits, and collapses anything outside double's range to `inf` or
/// zero before the literal's type is even known.
///
/// Accepts the C grammar for a decimal floating constant, without sign or
/// suffix: the caller has already stripped both.
pub(crate) fn parse_decimal_float_parts(s: &str) -> Result<(u128, i32), ()> {
    let bytes = s.as_bytes();
    let mut i = 0;
    let mut digits = Big::zero();
    let mut any = false;
    // Digits are accumulated nine at a time; one `mul_add_small` per chunk
    // rather than per digit.
    let mut chunk: u32 = 0;
    let mut chunk_len: u32 = 0;
    let push = |digits: &mut Big, chunk: &mut u32, chunk_len: &mut u32| {
        if *chunk_len != 0 {
            digits.mul_add_small(10u32.pow(*chunk_len), *chunk);
            *chunk = 0;
            *chunk_len = 0;
        }
    };

    while i < bytes.len() && bytes[i].is_ascii_digit() {
        chunk = chunk * 10 + (bytes[i] - b'0') as u32;
        chunk_len += 1;
        if chunk_len == 9 {
            push(&mut digits, &mut chunk, &mut chunk_len);
        }
        any = true;
        i += 1;
    }

    // Digits after the point shift the decimal exponent down by one each.
    let mut exp10: i32 = 0;
    if i < bytes.len() && bytes[i] == b'.' {
        i += 1;
        while i < bytes.len() && bytes[i].is_ascii_digit() {
            chunk = chunk * 10 + (bytes[i] - b'0') as u32;
            chunk_len += 1;
            if chunk_len == 9 {
                push(&mut digits, &mut chunk, &mut chunk_len);
            }
            any = true;
            exp10 -= 1;
            i += 1;
        }
    }
    push(&mut digits, &mut chunk, &mut chunk_len);
    if !any {
        return Err(());
    }

    if i < bytes.len() && (bytes[i] | 0x20) == b'e' {
        i += 1;
        let neg = match bytes.get(i) {
            Some(b'+') => {
                i += 1;
                false
            }
            Some(b'-') => {
                i += 1;
                true
            }
            _ => false,
        };
        let start = i;
        let mut value: i64 = 0;
        while i < bytes.len() && bytes[i].is_ascii_digit() {
            // Saturate rather than overflow: an exponent this large is already
            // far outside every target format, and the scaling below turns it
            // into an infinity or a zero regardless.
            value = (value * 10 + (bytes[i] - b'0') as i64).min(1 << 30);
            i += 1;
        }
        if i == start {
            return Err(());
        }
        exp10 += if neg { -value as i32 } else { value as i32 };
    }

    if i != bytes.len() {
        return Err(());
    }

    // Zero is zero at every exponent. The saturating paths below reason about
    // the exponent alone, which is only sound for a non-zero significand:
    // `0e6000` would otherwise come out as an infinity.
    if digits.is_zero() {
        return Ok((0, 0));
    }

    // Well outside any target's range; let the caller's rounding produce the
    // infinity or zero rather than building a 16,000-bit number to find out.
    if exp10 > 5000 {
        return Ok((1, i32::MAX / 2));
    }
    if exp10 < -5000 {
        return Ok((0, 0));
    }

    Ok(decimal_to_binary(&digits, exp10))
}

// Exact arithmetic

/// A target binary floating-point format.
///
/// Named by what it is rather than by how wide it is: `long double` is three
/// different formats across the targets c17 supports, two of which are 128
/// bits, and folding at the wrong one gives a wrong answer rather than an
/// imprecise one.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FpFormat {
    /// IEEE binary16 -- `_Float16`.
    Binary16,
    /// IEEE binary32 -- `float`.
    Binary32,
    /// IEEE binary64 -- `double`, and `long double` on Apple arm64.
    Binary64,
    /// The x87 80-bit format -- `long double` on x86-64.
    X87Extended,
    /// IEEE binary128 -- `__float128`, and `long double` on Linux aarch64.
    Binary128,
}

impl FpFormat {
    /// Significand bits, counting the integer bit whether or not it is stored.
    pub fn precision(self) -> u32 {
        match self {
            FpFormat::Binary16 => 11,
            FpFormat::Binary32 => 24,
            FpFormat::Binary64 => 53,
            FpFormat::X87Extended => 64,
            FpFormat::Binary128 => 113,
        }
    }

    /// The unbiased exponent of the smallest normal value.
    fn emin(self) -> i32 {
        match self {
            FpFormat::Binary16 => -14,
            FpFormat::Binary32 => -126,
            FpFormat::Binary64 => -1022,
            FpFormat::X87Extended | FpFormat::Binary128 => -16382,
        }
    }

    /// The unbiased exponent of the largest finite value.
    fn emax(self) -> i32 {
        match self {
            FpFormat::Binary16 => 15,
            FpFormat::Binary32 => 127,
            FpFormat::Binary64 => 1023,
            FpFormat::X87Extended | FpFormat::Binary128 => 16383,
        }
    }
}

/// A 256-bit unsigned integer.
///
/// Two 128-bit significands multiplied, or one aligned against another before
/// adding, do not fit in a `u128`. Only what the four operations below need is
/// implemented.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct U256 {
    hi: u128,
    lo: u128,
}

impl U256 {
    const ZERO: U256 = U256 { hi: 0, lo: 0 };
    const ONE: U256 = U256 { hi: 0, lo: 1 };

    /// `sig * 2^127`.
    ///
    /// One bit short of the top, so that two of these can be added without
    /// leaving 256 bits: a significand is normalized to bit 127, so `sig <<
    /// 128` would put the sum of two of them past 2^256.
    fn scaled127(sig: u128) -> Self {
        U256 {
            hi: sig >> 1,
            lo: sig << 127,
        }
    }

    fn is_zero(self) -> bool {
        self.hi == 0 && self.lo == 0
    }

    fn leading_zeros(self) -> u32 {
        if self.hi == 0 {
            128 + self.lo.leading_zeros()
        } else {
            self.hi.leading_zeros()
        }
    }

    fn add(self, o: Self) -> Self {
        let (lo, carry) = self.lo.overflowing_add(o.lo);
        U256 {
            hi: self.hi.wrapping_add(o.hi).wrapping_add(carry as u128),
            lo,
        }
    }

    fn sub(self, o: Self) -> Self {
        let (lo, borrow) = self.lo.overflowing_sub(o.lo);
        U256 {
            hi: self.hi.wrapping_sub(o.hi).wrapping_sub(borrow as u128),
            lo,
        }
    }

    fn shl(self, n: u32) -> Self {
        match n {
            0 => self,
            256.. => Self::ZERO,
            128.. => U256 {
                hi: self.lo << (n - 128),
                lo: 0,
            },
            _ => U256 {
                hi: (self.hi << n) | (self.lo >> (128 - n)),
                lo: self.lo << n,
            },
        }
    }

    fn shr(self, n: u32) -> Self {
        match n {
            0 => self,
            256.. => Self::ZERO,
            128.. => U256 {
                hi: 0,
                lo: self.hi >> (n - 128),
            },
            _ => U256 {
                hi: self.hi >> n,
                lo: (self.lo >> n) | (self.hi << (128 - n)),
            },
        }
    }

    /// Shift right by `n`, reporting whether any set bit fell off the bottom.
    fn shr_lossy(self, n: u32) -> (Self, bool) {
        let out = self.shr(n);
        (out, out.shl(n) != self)
    }

    /// The exact 256-bit product of two 128-bit values.
    fn mul(a: u128, b: u128) -> Self {
        const HALF: u128 = u64::MAX as u128;
        let (a1, a0) = (a >> 64, a & HALF);
        let (b1, b0) = (b >> 64, b & HALF);

        // a*b = a1b1*2^128 + (a1b0 + a0b1)*2^64 + a0b0, and the middle sum can
        // carry out of a u128, which is worth 2^192.
        let (mid, carry) = (a1 * b0).overflowing_add(a0 * b1);
        let (lo, lo_carry) = (a0 * b0).overflowing_add(mid << 64);
        let hi = (a1 * b1) + (mid >> 64) + ((carry as u128) << 64) + lo_carry as u128;
        U256 { hi, lo }
    }

    /// `a * 2^128 / b`, exactly, as a quotient and a remainder-is-nonzero flag.
    ///
    /// Both significands are normalized to bit 127, so the quotient lies in
    /// `(2^127, 2^129)` -- more than any target format keeps, which is what
    /// leaves room to fold the remainder into the low bit as a sticky.
    fn div(a: u128, b: u128) -> (Self, bool) {
        let n = U256 { hi: a, lo: 0 };
        let d = U256 { hi: 0, lo: b };
        let mut q = U256::ZERO;
        let mut r = U256::ZERO;
        for i in (0..256).rev() {
            // r stays below d < 2^128, so doubling it cannot overflow.
            r = r.shl(1);
            if n.shr(i).lo & 1 != 0 {
                r.lo |= 1;
            }
            if r >= d {
                r = r.sub(d);
                q = q.add(U256::ONE.shl(i));
            }
        }
        (q, !r.is_zero())
    }
}

impl FloatVal {
    /// The left-aligned significand and the power of two its low bit is worth.
    ///
    /// Aligning first is what makes the quotient bound in [`U256::div`] hold:
    /// a subnormal's significand has leading zeros, and dividing by one that
    /// still had them would leave the sticky bit inside the kept digits.
    fn scaled(self) -> (u128, i32) {
        let (sig, unbiased) = self.aligned();
        (sig, unbiased - (SIG_BITS as i32 - 1))
    }

    /// A zero with the given sign.
    fn signed_zero(neg: bool) -> Self {
        FloatVal {
            neg,
            exp: 0,
            sig: 0,
        }
    }

    /// True for either infinity.
    fn is_infinite(self) -> bool {
        self.exp == EXP_SPECIAL && !self.is_nan()
    }

    /// Round to `fmt`, saturating to infinity on overflow.
    ///
    /// This is what a cast does, and what each operand of an arithmetic
    /// operation has already had done to it: an operand's type says how many
    /// bits it has, whatever the literal it came from was written with.
    pub fn round_to_format(self, fmt: FpFormat) -> Self {
        if self.exp == EXP_SPECIAL || self.is_zero() {
            return self;
        }
        let (sig, exp2) = self.scaled();
        Self::round_wide(self.neg, U256::scaled127(sig), exp2 - 127, false, fmt)
    }

    /// `self + other`, rounded once to `fmt`.
    pub fn add(self, other: Self, fmt: FpFormat) -> Self {
        self.add_rounded(other, fmt)
    }

    /// `self - other`, rounded once to `fmt`.
    pub fn sub(self, other: Self, fmt: FpFormat) -> Self {
        self.add_rounded(other.negated(), fmt)
    }

    /// The common core of `add` and `sub`: `sub` negates first, since
    /// `a - b` and `a + (-b)` are the same in every rounding mode.
    fn add_rounded(self, other: Self, fmt: FpFormat) -> Self {
        let a = self.round_to_format(fmt);
        let b = other.round_to_format(fmt);

        if a.is_nan() || b.is_nan() {
            return Self::nan();
        }
        if a.is_infinite() || b.is_infinite() {
            // Infinities of opposite sign have no defined difference.
            if a.is_infinite() && b.is_infinite() && a.neg != b.neg {
                return Self::nan();
            }
            return if a.is_infinite() { a } else { b };
        }
        if a.is_zero() && b.is_zero() {
            // Round to nearest gives +0 unless both addends are -0.
            return Self::signed_zero(a.neg && b.neg);
        }
        if a.is_zero() {
            return b;
        }
        if b.is_zero() {
            return a;
        }

        let (sig_a, exp_a) = a.scaled();
        let (sig_b, exp_b) = b.scaled();
        let exp = exp_a.max(exp_b);
        // Each is `sig * 2^127` shifted down to the common exponent, so the
        // pair is exact to 127 bits below the smaller operand's last bit.
        let (wa, lost_a) = U256::scaled127(sig_a).shr_lossy((exp - exp_a) as u32);
        let (wb, lost_b) = U256::scaled127(sig_b).shr_lossy((exp - exp_b) as u32);
        let scale = exp - 127;

        if a.neg == b.neg {
            return Self::round_wide(a.neg, wa.add(wb), scale, lost_a || lost_b, fmt);
        }

        // Opposite signs: the magnitudes subtract, and the result takes the
        // sign of the larger. Only the operand that was shifted can have lost
        // bits, and a shift large enough to lose any is larger than 128, which
        // leaves it below 2^127 while the unshifted one is at least 2^254 --
        // so a tie here is always an exact tie.
        let (big, small, big_lost, small_lost, neg) = match wa.cmp(&wb) {
            std::cmp::Ordering::Greater => (wa, wb, lost_a, lost_b, a.neg),
            std::cmp::Ordering::Less => (wb, wa, lost_b, lost_a, b.neg),
            std::cmp::Ordering::Equal => return Self::ZERO,
        };
        let diff = big.sub(small);
        let (w, sticky) = if big_lost {
            // The larger is truly a little above `big`, so the difference is a
            // little above `diff`.
            (diff, true)
        } else if small_lost {
            // The smaller is truly a little above `small`, so the difference
            // is a little *below* `diff` -- which is `diff - 1` plus a sticky.
            (diff.sub(U256::ONE), true)
        } else {
            (diff, false)
        };
        Self::round_wide(neg, w, scale, sticky, fmt)
    }

    /// `self * other`, rounded once to `fmt`.
    pub fn mul(self, other: Self, fmt: FpFormat) -> Self {
        let a = self.round_to_format(fmt);
        let b = other.round_to_format(fmt);

        if a.is_nan() || b.is_nan() {
            return Self::nan();
        }
        let neg = a.neg != b.neg;
        if a.is_infinite() || b.is_infinite() {
            if a.is_zero() || b.is_zero() {
                return Self::nan();
            }
            return Self::infinity(neg);
        }
        if a.is_zero() || b.is_zero() {
            return Self::signed_zero(neg);
        }

        let (sig_a, exp_a) = a.scaled();
        let (sig_b, exp_b) = b.scaled();
        // The full product is exact in 256 bits; nothing is lost before the
        // single rounding below.
        Self::round_wide(neg, U256::mul(sig_a, sig_b), exp_a + exp_b, false, fmt)
    }

    /// `self / other`, rounded once to `fmt`.
    pub fn div(self, other: Self, fmt: FpFormat) -> Self {
        let a = self.round_to_format(fmt);
        let b = other.round_to_format(fmt);

        if a.is_nan() || b.is_nan() {
            return Self::nan();
        }
        let neg = a.neg != b.neg;
        if a.is_infinite() {
            return if b.is_infinite() {
                Self::nan()
            } else {
                Self::infinity(neg)
            };
        }
        if b.is_infinite() {
            return Self::signed_zero(neg);
        }
        if b.is_zero() {
            return if a.is_zero() {
                Self::nan()
            } else {
                Self::infinity(neg)
            };
        }
        if a.is_zero() {
            return Self::signed_zero(neg);
        }

        let (sig_a, exp_a) = a.scaled();
        let (sig_b, exp_b) = b.scaled();
        let (q, inexact) = U256::div(sig_a, sig_b);
        // The quotient is at least 2^127 and the widest format keeps 113 bits,
        // so bit 0 is far below the rounding position and can carry the
        // remainder as a sticky.
        let q = if inexact { q.add(U256::ONE) } else { q };
        Self::round_wide(neg, q, exp_a - exp_b - 128, inexact, fmt)
    }

    /// Round `w * 2^scale` to `fmt`, once, to nearest with ties to even.
    ///
    /// `sticky` says that a non-zero value smaller than `w`'s low bit was
    /// discarded on the way here, which decides a tie and nothing else.
    fn round_wide(neg: bool, w: U256, scale: i32, sticky: bool, fmt: FpFormat) -> Self {
        if w.is_zero() {
            return Self::signed_zero(neg);
        }

        let msb = 255 - w.leading_zeros() as i32;
        let precision = fmt.precision() as i32;
        // The bit that will hold the result's last significand bit: `precision`
        // below the top for a normal, or the format's smallest ulp for a
        // subnormal, which is fixed rather than relative to the value.
        let last = if msb + scale >= fmt.emin() {
            msb - (precision - 1)
        } else {
            fmt.emin() - (precision - 1) - scale
        };

        let drop = last.max(0) as u32;
        let kept = w.shr(drop);
        let round_up = if drop == 0 {
            // Nothing is being discarded here; a sticky from further back is
            // below half an ulp on its own.
            false
        } else if drop > 256 {
            // Half an ulp is wider than the whole intermediate, so the value
            // is below it and rounds away to zero. Forming `half` here would
            // shift the one straight off the top and leave zero, which every
            // value compares greater than -- the far-underflow product
            // `0x9482dbp-94f * 0xb85f3dp-118f` came out as the smallest
            // subnormal where it should have been none at all.
            false
        } else {
            let half = U256::ONE.shl(drop - 1);
            match w.sub(kept.shl(drop)).cmp(&half) {
                std::cmp::Ordering::Greater => true,
                std::cmp::Ordering::Equal => sticky || kept.lo & 1 != 0,
                std::cmp::Ordering::Less => false,
            }
        };
        let kept = if round_up { kept.add(U256::ONE) } else { kept };

        // `kept` holds at most `precision` bits, plus one if rounding carried,
        // so it always fits the significand this type is built from.
        debug_assert!(kept.hi == 0, "rounded significand wider than 128 bits");
        if kept.lo == 0 {
            return Self::signed_zero(neg);
        }

        let exp2 = scale + drop as i32;
        if 127 - kept.lo.leading_zeros() as i32 + exp2 > fmt.emax() {
            return Self::infinity(neg);
        }
        Self::from_parts(neg, kept.lo, exp2)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn f64_round_trips_exactly() {
        for v in [
            0.0,
            -0.0,
            1.0,
            -1.0,
            0.5,
            1.5,
            std::f64::consts::PI,
            f64::MAX,
            f64::MIN_POSITIVE,
            f64::EPSILON,
            1e308,
            -1e-308,
        ] {
            let round = FloatVal::from_f64(v).to_f64();
            assert_eq!(round.to_bits(), v.to_bits(), "{v} round-tripped to {round}");
        }
    }

    #[test]
    fn decimal_literals_match_a_hex_literal_of_the_same_value() {
        // Each pair is a decimal spelling and the hex spelling of the value
        // gcc produces for it; the hex path was already exact.
        // Each row is a decimal spelling, then the same value written as the
        // `(mantissa, exp2)` pair a hex literal would produce -- the hex path
        // was already exact, so it is the reference. `0xc.90fdaa22168c235p-2`
        // is the 64-bit significand 0xC90FDAA22168C235 scaled by 2^-62, the
        // point having moved fifteen hex digits right.
        let cases: &[(&str, u128, i32)] = &[
            ("3.14159265358979323846", 0xC90F_DAA2_2168_C235, -62),
            ("0.1", 0xCCCC_CCCC_CCCC_CCCD, -67),
            ("1.18973149535723176502e+4932", 0xFFFF_FFFF_FFFF_FFFF, 16320),
            ("1e-4900", 0xBBB4_DF56_BAF6_2972, -16341),
            ("1.0", 1, 0),
            ("1e10", 0x2540_BE400, 0),
            ("123456789.0", 0x075B_CD15, 0),
            ("1500.0", 1500, 0),
        ];
        for (dec, mantissa, exp2) in cases {
            let (dm, de) = parse_decimal_float_parts(dec).expect(dec);
            let got = FloatVal::from_parts(false, dm, de);
            let want = FloatVal::from_parts(false, *mantissa, *exp2);
            // Compared as x87 emits them. The references are the 64
            // significand bits gcc prints, and the decimal path now carries
            // more than that -- agreeing to 64 bits is the claim being made.
            assert_eq!(got.to_x87_bytes(), want.to_x87_bytes(), "{dec}");
        }
    }

    /// Digit accumulation happens nine at a time, so the boundaries around a
    /// chunk are where an off-by-one would hide.
    #[test]
    fn decimal_digit_chunking_is_exact_across_its_boundaries() {
        for n in 1..=25usize {
            let dec: String = std::iter::repeat_n('9', n).collect();
            let (m, e) = parse_decimal_float_parts(&dec).expect(&dec);
            let got = FloatVal::from_parts(false, m, e);
            // Up to 2^53 the value is exactly representable in f64, so f64
            // parsing is a trustworthy reference for the shorter cases.
            if n <= 15 {
                let want = FloatVal::from_f64(dec.parse::<f64>().unwrap());
                assert_eq!(got.key(), want.key(), "{dec}");
            }
        }
    }

    #[test]
    fn decimal_exponent_forms_agree() {
        let forms = ["1500.0", "1.5e3", "15e2", "150000e-2", "0.15e4"];
        let first = parse_decimal_float_parts(forms[0]).unwrap();
        let first = FloatVal::from_parts(false, first.0, first.1);
        for f in &forms[1..] {
            let (m, e) = parse_decimal_float_parts(f).expect(f);
            assert_eq!(FloatVal::from_parts(false, m, e).key(), first.key(), "{f}");
        }
    }

    #[test]
    fn decimal_rejects_what_is_not_a_number() {
        for bad in ["", ".", "e5", "1e", "1e+", "1.0x", "abc"] {
            assert!(parse_decimal_float_parts(bad).is_err(), "{bad:?}");
        }
    }

    #[test]
    fn subnormal_doubles_survive() {
        for v in [f64::from_bits(1), f64::from_bits(0x000F_FFFF_FFFF_FFFF)] {
            let round = FloatVal::from_f64(v).to_f64();
            assert_eq!(round.to_bits(), v.to_bits(), "subnormal {v:e} lost");
        }
    }

    #[test]
    fn specials_survive() {
        assert!(FloatVal::from_f64(f64::NAN).is_nan());
        assert!(FloatVal::from_f64(f64::INFINITY).to_f64() == f64::INFINITY);
        assert!(FloatVal::from_f64(f64::NEG_INFINITY).to_f64() == f64::NEG_INFINITY);
        assert!(FloatVal::infinity(false).to_f64() == f64::INFINITY);
        assert!(FloatVal::nan().to_f64().is_nan());
        assert!(FloatVal::ZERO.is_positive_zero());
        assert!(!FloatVal::ZERO.negated().is_positive_zero());
    }

    #[test]
    fn values_outside_double_range_are_kept() {
        // LDBL_MAX: 2^16384 - 2^16320, i.e. all 64 significand bits set.
        let max = FloatVal::from_parts(false, u64::MAX as u128, 16384 - 64);
        assert_eq!(max.to_f64(), f64::INFINITY, "but it does exceed a double");
        let bytes = max.to_x87_bytes();
        assert_eq!(&bytes[..8], &u64::MAX.to_le_bytes());
        assert_eq!(u16::from_le_bytes([bytes[8], bytes[9]]), 0x7FFE);

        // LDBL_MIN: 2^-16382, the smallest normal.
        let min = FloatVal::from_parts(false, 1, -16382);
        assert!(!min.is_zero(), "LDBL_MIN must not flush to zero");
        assert_eq!(min.to_f64(), 0.0, "but it is below a double's range");
        let bytes = min.to_x87_bytes();
        assert_eq!(u16::from_le_bytes([bytes[8], bytes[9]]), 1);
        assert_eq!(&bytes[..8], &(1u64 << 63).to_le_bytes());
    }

    /// `from_parts` keeps every bit; x87 emission is what rounds to 64.
    #[test]
    fn x87_emission_rounds_to_nearest_even() {
        let x87_sig = |v: FloatVal| u64::from_le_bytes(v.to_x87_bytes()[..8].try_into().unwrap());
        const X87_INTEGER_BIT: u64 = 1 << 63;

        // 65 significant bits: the low one must round away, ties to even.
        // 0x1_0000_0000_0000_0001 is a tie with an even kept value, so it
        // rounds down to 0x8000_0000_0000_0000.
        let tie = FloatVal::from_parts(false, (1u128 << 64) | 1, 0);
        assert_eq!(x87_sig(tie), X87_INTEGER_BIT);
        // Nothing was lost on the way in, though: the bit is still there.
        assert_ne!(tie.key().2 & !INTEGER_BIT, 0);

        // Carry out of the top: all ones plus a rounding bit becomes 1.0.
        let carry = FloatVal::from_parts(false, (u64::MAX as u128) << 1 | 1, 0);
        assert_eq!(x87_sig(carry), X87_INTEGER_BIT);

        // Above the tie it rounds up rather than to even: 66 bits whose low
        // two are 0b11, so what is dropped is three quarters of an ulp.
        let up = FloatVal::from_parts(false, (1u128 << 65) | 0b11, 0);
        assert_eq!(x87_sig(up), X87_INTEGER_BIT + 1);
    }

    #[test]
    fn binary128_widening_is_exact() {
        // A value needing all 64 significand bits must survive into
        // binary128, whose 113 bits have room for it.
        let v = FloatVal::from_parts(false, u64::MAX as u128, -63);
        let (lo, hi) = v.to_f128_bits();
        // 1.111...1 x 2^0: biased exponent 16383, top fraction bits all ones.
        assert_eq!((hi >> 48) & 0x7FFF, 16383);
        assert_eq!(hi & 0xFFFF_FFFF_FFFF, 0xFFFF_FFFF_FFFF);
        // The 63 fraction bits sit at the top of the 112, so the low half is
        // 2^112 - 2^49 truncated: everything above bit 49 set.
        assert_eq!(lo, 0xFFFE_0000_0000_0000);

        // Doubles must agree with the straightforward widening.
        let one = FloatVal::from_f64(1.0).to_f128_bits();
        assert_eq!(one, (0, 0x3FFF_0000_0000_0000));
        let neg = FloatVal::from_f64(-2.0).to_f128_bits();
        assert_eq!(neg, (0, 0xC000_0000_0000_0000));
    }

    #[test]
    fn a_zero_significand_stays_zero_at_any_exponent() {
        // The exponent alone says "far outside every format", but a zero
        // significand is zero regardless -- `0e6000` is not an infinity.
        for lit in ["0e6000", "0.0e9999", "0.000e6000", "00e10000"] {
            let (mantissa, exp2) = parse_decimal_float_parts(lit).unwrap();
            assert_eq!(mantissa, 0, "{lit} has a non-zero significand");
            let v = FloatVal::from_parts(false, mantissa, exp2);
            assert!(v.is_zero(), "{lit} converted to {}", v.to_f64());
        }

        // The underflow side of the same saturation, which was already right.
        let (mantissa, exp2) = parse_decimal_float_parts("1e-9999").unwrap();
        assert!(FloatVal::from_parts(false, mantissa, exp2).is_zero());
    }

    /// A subnormal is rounded to nearest, not truncated.
    ///
    /// Asserted on the emitted x87 image rather than on the internal
    /// significand, so it says something about the value a target receives
    /// rather than about how this type happens to hold it. x87 encodes a
    /// subnormal as `sig * 2^-16445`, so a literal written as a multiple of
    /// `2^-16447` puts the rounding decision in the low two bits.
    #[test]
    fn subnormals_round_to_nearest_rather_than_truncate() {
        // (quarters of an ulp, expected significand, expected exponent)
        let x87 = |m: u128| {
            let b = FloatVal::from_parts(false, m, -16447).to_x87_bytes();
            (
                u64::from_le_bytes(b[..8].try_into().unwrap()),
                u16::from_le_bytes([b[8], b[9]]),
            )
        };

        // 4k+3 quarters: three quarters of an ulp above k, so it rounds up.
        // Truncation would leave k = 2^62 - 1.
        assert_eq!(x87(u64::MAX as u128), (1 << 62, 0), "must round up");

        // 4k+2 with k even is an exact tie, and ties go to even: it stays.
        assert_eq!(
            x87((u64::MAX - 5) as u128),
            ((1 << 62) - 2, 0),
            "tie to even"
        );

        // Rounding up out of the subnormal range gives the smallest normal,
        // which is an exponent of 1 with the integer bit set.
        assert_eq!(x87((1u128 << 65) - 1), (1 << 63, 1), "carry to normal");

        // At the bottom: three quarters of the smallest subnormal rounds up
        // to it, one quarter rounds away to zero.
        assert_eq!(x87(3), (1, 0), "must not flush to zero");
        assert_eq!(x87(1), (0, 0), "below half an ulp flushes");
    }

    /// The four operations round once, at the format's own precision.
    ///
    /// Every expectation here was checked against gcc compiling the same
    /// constant expression; the encodings are what it emits.
    #[test]
    fn arithmetic_rounds_once_at_the_target_precision() {
        let q = |v: FloatVal| {
            let (lo, hi) = v.to_f128_bits();
            (hi, lo)
        };
        let one = FloatVal::from_f64(1.0);
        let three = FloatVal::from_f64(3.0);
        let seven = FloatVal::from_f64(7.0);

        // A repeating quotient keeps all 113 bits. Through `f64` this was
        // 0x3ffd5555555555555000000000000000 -- sixty bits short.
        assert_eq!(
            q(one.div(three, FpFormat::Binary128)),
            (0x3ffd_5555_5555_5555, 0x5555_5555_5555_5555)
        );
        assert_eq!(
            q(FloatVal::from_f64(2.0).div(seven, FpFormat::Binary128)),
            (0x3ffd_2492_4924_9249, 0x2492_4924_9249_2492)
        );

        // The same division at x87's 64 bits, and at double's 53.
        let (sig, se) = {
            let b = one.div(three, FpFormat::X87Extended).to_x87_bytes();
            (
                u64::from_le_bytes(b[..8].try_into().unwrap()),
                u16::from_le_bytes([b[8], b[9]]),
            )
        };
        assert_eq!((se, sig), (0x3ffd, 0xaaaa_aaaa_aaaa_aaab));
        assert_eq!(one.div(three, FpFormat::Binary64).to_f64(), 1.0 / 3.0);
        assert_eq!(
            one.div(three, FpFormat::Binary32).to_f64(),
            (1.0f32 / 3.0f32) as f64
        );

        // An addend sixty bits down survives at binary128 and does not at
        // double, which is the whole difference the format makes.
        let tiny = FloatVal::from_f64(1e-30);
        assert_eq!(
            q(one.add(tiny, FpFormat::Binary128)),
            (0x3fff_0000_0000_0000, 0x0000_0000_0000_1448)
        );
        assert_eq!(one.add(tiny, FpFormat::Binary64).to_f64(), 1.0);
    }

    /// Ties go to even, and only exact ties are ties.
    #[test]
    fn ties_at_the_last_significand_bit_go_to_even() {
        let q = |v: FloatVal| {
            let (lo, hi) = v.to_f128_bits();
            (hi, lo)
        };
        let one = FloatVal::from_f64(1.0);
        let ulp = |e: i32| FloatVal::from_parts(false, 1, e);

        // Exactly half an ulp above 1.0, whose last bit is even: it stays.
        assert_eq!(
            q(one.add(ulp(-113), FpFormat::Binary128)),
            (0x3fff_0000_0000_0000, 0)
        );
        // Three quarters of an ulp: above half, so it rounds up.
        assert_eq!(
            q(one.add(FloatVal::from_parts(false, 3, -114), FpFormat::Binary128)),
            (0x3fff_0000_0000_0000, 1)
        );
        // Half an ulp above a value whose last bit is odd: rounds up.
        let odd = one.add(ulp(-112), FpFormat::Binary128);
        assert_eq!(
            q(odd.add(ulp(-113), FpFormat::Binary128)),
            (0x3fff_0000_0000_0000, 2)
        );
    }

    /// Underflow rounds through the subnormal range and off the bottom.
    #[test]
    fn underflow_rounds_rather_than_flushing() {
        let q = |v: FloatVal| {
            let (lo, hi) = v.to_f128_bits();
            (hi, lo)
        };
        let two = FloatVal::from_f64(2.0);
        // binary128's smallest subnormal is 2^-16494.
        let denorm_min = FloatVal::from_parts(false, 1, -16494);

        // Exactly half of it is a tie, and zero is the even side.
        assert_eq!(q(denorm_min.div(two, FpFormat::Binary128)), (0, 0));
        // Three quarters of it is above half, so it rounds back up to one.
        assert_eq!(
            q(FloatVal::from_parts(false, 3, -16495).div(two, FpFormat::Binary128)),
            (0, 1)
        );
        // Far below half an ulp -- the case where forming half an ulp
        // overflows the intermediate -- must round away to nothing.
        assert_eq!(q(denorm_min.mul(denorm_min, FpFormat::Binary128)), (0, 0));
        assert!(FloatVal::from_f64(1e-300)
            .mul(FloatVal::from_f64(1e-300), FpFormat::Binary64)
            .is_zero());
        // The step across the subnormal boundary stays exact: half the
        // smallest normal is the largest power of two below it, encoded with
        // a zero exponent and the top fraction bit set.
        let min_normal = FloatVal::from_parts(false, 1, -16382);
        assert_eq!(
            q(min_normal.div(two, FpFormat::Binary128)),
            (0x0000_8000_0000_0000, 0)
        );
    }

    /// Overflow saturates to infinity at the format's own ceiling.
    #[test]
    fn overflow_saturates_at_each_formats_ceiling() {
        let ten = FloatVal::from_f64(10.0);
        let huge128 = FloatVal::from_parts(false, 1, 16383);
        assert_eq!(
            huge128.mul(ten, FpFormat::Binary128).to_f128_bits().1 >> 48,
            0x7fff
        );
        // The same value is finite in binary128 and infinite in double.
        assert!(!huge128.round_to_format(FpFormat::Binary128).is_nan());
        assert_eq!(
            huge128.round_to_format(FpFormat::Binary64).to_f64(),
            f64::INFINITY
        );
        assert_eq!(
            FloatVal::from_f64(1e308)
                .mul(ten, FpFormat::Binary64)
                .to_f64(),
            f64::INFINITY
        );
    }

    /// Zeros, infinities and NaNs follow IEEE 754.
    #[test]
    fn special_values_follow_ieee() {
        let f = FpFormat::Binary128;
        let zero = FloatVal::ZERO;
        let neg_zero = zero.negated();
        let one = FloatVal::from_f64(1.0);
        let inf = FloatVal::infinity(false);
        let neg_inf = FloatVal::infinity(true);

        // Only -0 + -0 is -0; every other sum of zeros is +0.
        assert!(
            neg_zero.add(neg_zero, f).is_zero()
                && neg_zero.add(neg_zero, f).negated().is_positive_zero()
        );
        assert!(zero.add(neg_zero, f).is_positive_zero());
        assert!(one.sub(one, f).is_positive_zero());

        // Signs multiply and divide.
        assert!(neg_zero
            .mul(FloatVal::from_f64(3.0), f)
            .negated()
            .is_positive_zero());
        assert!(one.div(neg_inf, f).negated().is_positive_zero());

        // Infinities.
        assert_eq!(one.div(zero, f).to_f64(), f64::INFINITY);
        assert_eq!(one.div(neg_zero, f).to_f64(), f64::NEG_INFINITY);
        assert_eq!(inf.add(one, f).to_f64(), f64::INFINITY);
        assert_eq!(inf.mul(neg_inf, f).to_f64(), f64::NEG_INFINITY);

        // The indeterminate forms.
        assert!(inf.sub(inf, f).is_nan());
        assert!(inf.mul(zero, f).is_nan());
        assert!(zero.div(zero, f).is_nan());
        assert!(inf.div(inf, f).is_nan());
        // A NaN operand poisons everything.
        assert!(FloatVal::nan().add(one, f).is_nan());
        assert!(one.mul(FloatVal::nan(), f).is_nan());
    }

    /// Rounding to a format is idempotent, and matches what emission does.
    ///
    /// An operand is rounded to its own type before the operation, so a
    /// literal wider than the type it is written for -- which `FloatVal`
    /// holds exactly -- contributes only the bits that type has.
    #[test]
    fn rounding_to_a_format_agrees_with_emission() {
        for v in [
            FloatVal::from_f64(0.1),
            FloatVal::from_f64(-1e300),
            FloatVal::from_parts(false, u128::MAX, -200),
            FloatVal::from_parts(true, 12345678901234567890, -60),
        ] {
            assert_eq!(v.round_to_format(FpFormat::Binary64).to_f64(), v.to_f64());
            assert_eq!(
                v.round_to_format(FpFormat::Binary128).to_f128_bits(),
                v.to_f128_bits()
            );
            assert_eq!(
                v.round_to_format(FpFormat::X87Extended).to_x87_bytes(),
                v.to_x87_bytes()
            );
            // Idempotent: rounding an already-rounded value changes nothing.
            let once = v.round_to_format(FpFormat::X87Extended);
            assert_eq!(once.round_to_format(FpFormat::X87Extended), once);
        }
    }

    /// Every `double` result agrees with the hardware, over a wide random
    /// sample: for the `double` format this arithmetic must match `f64`.
    #[test]
    fn double_results_agree_with_hardware() {
        // A xorshift, so the sample is fixed without pulling in a dependency.
        let mut state = 0x2545F4914F6CDD1Du64;
        let mut next = || {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            state
        };
        for _ in 0..20000 {
            let a = f64::from_bits(next());
            let b = f64::from_bits(next());
            if !a.is_finite() || !b.is_finite() {
                continue;
            }
            let (x, y) = (FloatVal::from_f64(a), FloatVal::from_f64(b));
            let f = FpFormat::Binary64;
            assert_eq!(
                x.add(y, f).to_f64().to_bits(),
                (a + b).to_bits(),
                "{a} + {b}"
            );
            assert_eq!(
                x.sub(y, f).to_f64().to_bits(),
                (a - b).to_bits(),
                "{a} - {b}"
            );
            assert_eq!(
                x.mul(y, f).to_f64().to_bits(),
                (a * b).to_bits(),
                "{a} * {b}"
            );
            if b != 0.0 {
                assert_eq!(
                    x.div(y, f).to_f64().to_bits(),
                    (a / b).to_bits(),
                    "{a} / {b}"
                );
            }
        }
    }

    /// The same, for `float`.
    #[test]
    fn float_results_agree_with_hardware() {
        let mut state = 0x9E3779B97F4A7C15u64;
        let mut next = || {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            state as u32
        };
        for _ in 0..20000 {
            let a = f32::from_bits(next());
            let b = f32::from_bits(next());
            if !a.is_finite() || !b.is_finite() {
                continue;
            }
            let (x, y) = (FloatVal::from_f64(a as f64), FloatVal::from_f64(b as f64));
            let f = FpFormat::Binary32;
            let got = |v: FloatVal| v.to_f64() as f32;
            assert_eq!(got(x.add(y, f)).to_bits(), (a + b).to_bits(), "{a} + {b}");
            assert_eq!(got(x.sub(y, f)).to_bits(), (a - b).to_bits(), "{a} - {b}");
            assert_eq!(got(x.mul(y, f)).to_bits(), (a * b).to_bits(), "{a} * {b}");
            if b != 0.0 {
                assert_eq!(got(x.div(y, f)).to_bits(), (a / b).to_bits(), "{a} / {b}");
            }
        }
    }

    /// An integer operand converts without passing through `f64`.
    #[test]
    fn integers_convert_exactly() {
        // Needs 54 bits, so `as f64` would round it.
        let v = FloatVal::from_i128(9007199254740993);
        assert_eq!(
            v.to_f128_bits(),
            (0x0800_0000_0000_0000, 0x4034_0000_0000_0000)
        );
        // Rounding it to double loses the low bit.
        assert_ne!(
            v.key(),
            FloatVal::from_f64(9007199254740993i64 as f64).key()
        );
        assert_eq!(v.to_f64(), 9007199254740992.0);
        // The full width of the significand, and the sign.
        assert_eq!(FloatVal::from_i128(i128::MIN).to_f64(), i128::MIN as f64);
        assert_eq!(FloatVal::from_i128(-1).to_f64(), -1.0);
        assert!(FloatVal::from_i128(0).is_positive_zero());
    }

    #[test]
    fn distinct_wide_values_get_distinct_keys() {
        // These differ only below the 53rd significand bit, so pooling them
        // on the rounded f64 would merge two different constants.
        let a = FloatVal::from_parts(false, (1u128 << 63) | 1, 0);
        let b = FloatVal::from_parts(false, (1u128 << 63) | 3, 0);
        assert_eq!(a.to_f64().to_bits(), b.to_f64().to_bits());
        assert_ne!(a.key(), b.key());
        assert_ne!(a, b);
    }
}
