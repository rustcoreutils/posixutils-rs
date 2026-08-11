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
