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
// [`FloatVal`] carries the x87 80-bit encoding itself. That choice covers both
// supported targets exactly: it is the native `long double` on x86-64, and
// widening its 64-bit significand to the 113 bits of aarch64's binary128 is
// also exact, so no target loses precision by going through it. `float` and
// `double` are strict subsets and round out of it on demand.
//

use std::fmt;

/// The exponent bias of the x87 80-bit extended format.
const BIAS: i32 = 16383;
/// Biased exponent denoting infinity or NaN.
const EXP_SPECIAL: u16 = 0x7FFF;
/// The largest finite biased exponent.
const EXP_MAX_FINITE: u16 = 0x7FFE;
/// Bit 63 of the significand: x87 stores the integer bit explicitly.
const INTEGER_BIT: u64 = 1 << 63;

/// A floating-point literal held at x87 80-bit precision.
///
/// The value is `(-1)^neg * sig * 2^(exp - BIAS - 63)` for a normal number.
/// `exp == 0` is zero (`sig == 0`) or subnormal; `exp == 0x7FFF` is infinity
/// (`sig == INTEGER_BIT`) or NaN.
#[derive(Clone, Copy, Debug)]
pub struct FloatVal {
    neg: bool,
    exp: u16,
    sig: u64,
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
            sig: INTEGER_BIT | (1 << 62),
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
                    sig: INTEGER_BIT | (frac << 11),
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
            let shift = frac.leading_zeros();
            let sig = frac << shift;
            // frac has value frac * 2^-1074; after shifting left by `shift`
            // the integer bit sits at bit 63, worth 2^(63-1074-shift).
            let unbiased = 63 - 1074 - shift as i32;
            return FloatVal {
                neg,
                exp: (unbiased + BIAS) as u16,
                sig,
            };
        }

        FloatVal {
            neg,
            exp: (exp11 - 1023 + BIAS) as u16,
            sig: INTEGER_BIT | (frac << 11),
        }
    }

    /// Build from an exact `mantissa * 2^exp2`, rounding to 64 significand
    /// bits (round-to-nearest, ties to even).
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

        // Normalize so the leading one sits at bit 63 of a u64.
        let width = 128 - mantissa.leading_zeros() as i32;
        // Value is mantissa * 2^exp2; the top bit is worth 2^(exp2 + width - 1).
        let mut unbiased = exp2 + width - 1;

        let mut sig;
        if width <= 64 {
            sig = (mantissa as u64) << (64 - width);
        } else {
            let drop = width - 64;
            let kept = mantissa >> drop;
            let rest = mantissa & ((1u128 << drop) - 1);
            let half = 1u128 << (drop - 1);
            sig = kept as u64;
            // Round to nearest, ties to even.
            if rest > half || (rest == half && sig & 1 != 0) {
                sig = match sig.checked_add(1) {
                    Some(s) => s,
                    None => {
                        // Carried out of the top: 0xFFFF... becomes 1.0 with
                        // one more exponent.
                        unbiased += 1;
                        INTEGER_BIT
                    }
                };
                if sig == 0 {
                    unbiased += 1;
                    sig = INTEGER_BIT;
                }
            }
        }

        Self::from_normalized(neg, sig, unbiased)
    }

    /// Assemble from a significand already normalized to bit 63, handling
    /// overflow to infinity and underflow through the subnormal range.
    fn from_normalized(neg: bool, sig: u64, unbiased: i32) -> Self {
        let biased = unbiased + BIAS;
        if biased > EXP_MAX_FINITE as i32 {
            return Self::infinity(neg);
        }
        if biased <= 0 {
            // Subnormal: shift the significand down until the exponent is 1,
            // which is the smallest the format encodes.
            let shift = 1 - biased;
            if shift >= 64 {
                return FloatVal {
                    neg,
                    exp: 0,
                    sig: 0,
                };
            }
            return FloatVal {
                neg,
                exp: 0,
                sig: sig >> shift,
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

        // Re-normalize: a subnormal 80-bit value may still be a normal double
        // is impossible (the range only shrinks), but the significand of a
        // subnormal is not left-aligned, so align it first.
        let shift = self.sig.leading_zeros();
        let sig = self.sig << shift;
        let unbiased = self.exp as i32 - BIAS - shift as i32 + if self.exp == 0 { 1 } else { 0 };

        let exp11 = unbiased + 1023;
        if exp11 >= 0x7FF {
            return if self.neg {
                f64::NEG_INFINITY
            } else {
                f64::INFINITY
            };
        }

        let (frac, exp11) = if exp11 <= 0 {
            // Subnormal double, or zero.
            let shift = 1 - exp11;
            if shift >= 64 {
                return if self.neg { -0.0 } else { 0.0 };
            }
            (Self::round_to(sig >> shift, 11), 0)
        } else {
            let rounded = Self::round_to(sig, 11);
            // Rounding can carry into bit 64, which means the significand
            // became 2.0 and the exponent steps.
            if rounded >> 52 > 1 {
                (0, exp11 + 1)
            } else {
                (rounded & ((1u64 << 52) - 1), exp11)
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
    fn round_to(sig: u64, drop: u32) -> u64 {
        let kept = sig >> drop;
        let rest = sig & ((1u64 << drop) - 1);
        let half = 1u64 << (drop - 1);
        if rest > half || (rest == half && kept & 1 != 0) {
            kept + 1
        } else {
            kept
        }
    }

    /// The 16-byte x87 80-bit image, little-endian, as stored in memory.
    pub fn to_x87_bytes(self) -> [u8; 16] {
        let mut out = [0u8; 16];
        out[..8].copy_from_slice(&self.sig.to_le_bytes());
        let se = ((self.neg as u16) << 15) | (self.exp & 0x7FFF);
        out[8..10].copy_from_slice(&se.to_le_bytes());
        out
    }

    /// The IEEE binary128 encoding, as `(low, high)` 64-bit halves.
    ///
    /// Exact: binary128's 113-bit significand has room for all 64 bits.
    pub fn to_f128_bits(self) -> (u64, u64) {
        let sign = (self.neg as u64) << 63;

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

        // binary128 has an implicit leading bit, so drop x87's explicit one.
        let shift = self.sig.leading_zeros();
        let sig = self.sig << shift;
        let unbiased = self.exp as i32 - BIAS - shift as i32 + if self.exp == 0 { 1 } else { 0 };
        let biased = unbiased + 16383;
        if biased >= 0x7FFF {
            return (0, sign | (0x7FFF << 48));
        }
        if biased <= 0 {
            let shift = 1 - biased;
            if shift >= 113 {
                return (0, sign);
            }
            let frac = (sig as u128) << 49 >> shift;
            return (frac as u64, sign | ((frac >> 64) as u64));
        }

        // 112 fraction bits: the 63 below the integer bit, shifted up by 49.
        let frac = ((sig & !INTEGER_BIT) as u128) << 49;
        let hi = sign | ((biased as u64) << 48) | ((frac >> 64) as u64);
        (frac as u64, hi)
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
    pub fn key(self) -> (bool, u16, u64) {
        (self.neg, self.exp, self.sig)
    }

    /// [`FloatVal::key`] packed into one integer, for use as a map key and in
    /// the generated label name of a pooled constant.
    pub fn pool_key(self) -> u128 {
        let se = ((self.neg as u16) << 15) | (self.exp & 0x7FFF);
        ((se as u128) << 64) | self.sig as u128
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

// ============================================================================
// Decimal to binary conversion
// ============================================================================

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
/// One more than the 64 the widest target needs, plus room for a round bit;
/// [`FloatVal::from_parts`] does the final rounding, and the sticky bit folded
/// into bit 0 keeps a true tie distinguishable from "just above".
const DEC_PRECISION: usize = 96;

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
            assert_eq!(got.key(), want.key(), "{dec}");
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
        // The old f64-to-x87 conversion gave these a zero exponent and no
        // integer bit, which is a different value, not a rounded one.
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
        assert_eq!(&bytes[..8], &INTEGER_BIT.to_le_bytes());
    }

    #[test]
    fn from_parts_rounds_to_nearest_even() {
        // 65 significant bits: the low one must round away, ties to even.
        // 0b11 followed by 63 zeros, plus a half -- ties to even keeps it even.
        let exact = FloatVal::from_parts(false, (1u128 << 64) | 1, 0);
        // Dropping one bit from a 65-bit value: 0x1_0000_0000_0000_0001
        // rounds down to 0x8000_0000_0000_0000 with exponent stepped.
        assert_eq!(exact.key().2, INTEGER_BIT);

        // Carry out of the top: all ones plus a rounding bit becomes 1.0.
        let carry = FloatVal::from_parts(false, (u64::MAX as u128) << 1 | 1, 0);
        assert_eq!(carry.key().2, INTEGER_BIT);
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
