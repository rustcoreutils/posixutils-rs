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
/// The top fraction bit, which in a NaN is the quiet bit: set for a quiet
/// NaN, clear for a signalling one. Every target format c17 supports puts it
/// there (IEEE 754-2008 6.2.1's recommendation), so it is one position here
/// whatever the format.
const QUIET_BIT: u128 = 1 << 126;
/// Significand width. Wider than any target format, so that rounding happens
/// once, on the way out, at the width being emitted.
const SIG_BITS: u32 = 128;

/// Which integer a value rounds to: the choice between `floor`, `ceil`,
/// `trunc`, `round`, `rint` and `nearbyint`, named after them.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IntegralRounding {
    /// Toward negative infinity.
    Floor,
    /// Toward positive infinity.
    Ceil,
    /// Toward zero.
    Trunc,
    /// To nearest, a tie away from zero.
    Round,
    /// In the current rounding direction, raising *inexact* when the value
    /// changes.
    Rint,
    /// In the current rounding direction, raising nothing.
    NearbyInt,
}

/// Which of the two kinds of NaN a value is.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NanKind {
    /// Propagates through arithmetic without raising anything.
    Quiet,
    /// Raises *invalid* when an operation consumes it, and is quieted by that
    /// operation -- or by a conversion -- rather than passed through.
    Signalling,
}

/// A floating-point literal, held wider than any target format.
///
/// The value is `(-1)^neg * sig * 2^(exp - BIAS - 127)` for a normal number.
/// `exp == 0` is zero (`sig == 0`) or subnormal; `exp == 0x7FFF` is infinity
/// (`sig == INTEGER_BIT`) or NaN.
///
/// A NaN is a value like any other: its sign, its quiet bit and its payload
/// are all part of it. The significand is held left-aligned as for a number
/// -- the integer bit at the top, the quiet bit below it, the rest of the
/// payload below that -- so a NaN of a narrower format has its payload in the
/// high bits, and converting it between formats is a shift, as it is in the
/// hardware.
#[derive(Clone, Copy, Debug)]
pub struct FloatVal {
    neg: bool,
    exp: u16,
    sig: u128,
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

    /// The default quiet NaN: positive, with an empty payload.
    pub fn nan() -> Self {
        FloatVal {
            neg: false,
            exp: EXP_SPECIAL,
            // Integer bit plus the quiet bit, matching what x87 produces.
            sig: INTEGER_BIT | QUIET_BIT,
        }
    }

    /// The positive NaN of format `fmt` whose payload is `payload`: what
    /// `__builtin_nan` and `__builtin_nans` build from their string.
    ///
    /// The payload is the integer value of the trailing significand field
    /// below the quiet bit, so it keeps as many low bits of `payload` as the
    /// format has room for -- 22 in `float`, 51 in `double` -- and the quiet
    /// bit is then set or cleared by `kind`, whatever `payload` said there.
    /// A signalling NaN cannot have an all-zero significand, which would
    /// encode an infinity, so an empty signalling payload becomes the bit
    /// just below the quiet bit. Both rules are gcc's, and its emitted bits
    /// are what they were checked against.
    pub fn nan_with_payload(fmt: FpFormat, payload: u128, kind: NanKind) -> Self {
        let p = fmt.precision();
        // The fraction below the integer bit, as the format stores it.
        let mut frac = payload & ((1u128 << (p - 1)) - 1);
        let quiet = 1u128 << (p - 2);
        match kind {
            NanKind::Quiet => frac |= quiet,
            NanKind::Signalling => {
                frac &= !quiet;
                if frac == 0 {
                    frac = quiet >> 1;
                }
            }
        }
        FloatVal {
            neg: false,
            exp: EXP_SPECIAL,
            sig: INTEGER_BIT | frac << (SIG_BITS - p),
        }
    }

    /// The same value with a NaN made quiet: what an operation or a
    /// conversion does to a signalling NaN it consumes. The sign and the rest
    /// of the payload are kept. Anything else is returned unchanged.
    fn quieted(self) -> Self {
        if self.is_nan() {
            FloatVal {
                sig: self.sig | QUIET_BIT,
                ..self
            }
        } else {
            self
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
    ///
    /// The `double` encoding read back as a host `f64`, so a NaN comes out
    /// with its own sign and payload rather than as `f64::NAN`.
    pub fn to_f64(self) -> f64 {
        f64::from_bits(self.to_bits(FpFormat::Binary64) as u64)
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

    /// The value's encoding in `fmt`, right-aligned in a `u128`: sign, then
    /// biased exponent, then the stored significand.
    ///
    /// The one place a value becomes bits. It is rounded to `fmt` first (see
    /// [`round_to_format`](Self::round_to_format)), which leaves it exactly
    /// representable there, so what follows only moves fields -- including a
    /// NaN's sign, quiet bit and payload, which never pass through a host
    /// float that could change them. For the x87 format the integer bit is
    /// stored, so the 80 bits are `sign:exp:64-bit significand`.
    pub fn to_bits(self, fmt: FpFormat) -> u128 {
        let v = self.round_to_format(fmt);
        let p = fmt.precision();
        let stored_bits = fmt.stored_significand_bits();
        let exp_bits = fmt.exponent_bits();
        // The top `p` bits of a left-aligned significand, less the integer
        // bit where the format leaves it implicit.
        let stored = |sig: u128| (sig >> (SIG_BITS - p)) & ((1u128 << stored_bits) - 1);

        let (biased, sig) = if v.exp == EXP_SPECIAL {
            ((1u128 << exp_bits) - 1, stored(v.sig))
        } else if v.sig == 0 {
            (0, 0)
        } else {
            let (sig, unbiased) = v.aligned();
            if unbiased >= fmt.emin() {
                ((unbiased + fmt.emax()) as u128, stored(sig))
            } else {
                // Subnormal in `fmt`: the exponent field is zero and the
                // significand is shifted down to the smallest normal's scale.
                // `v` is representable, so nothing is shifted out.
                (0, stored(sig >> (fmt.emin() - unbiased)))
            }
        };
        ((v.neg as u128) << (exp_bits + stored_bits)) | (biased << stored_bits) | sig
    }

    /// The 16-byte x87 80-bit image, little-endian, as stored in memory.
    pub fn to_x87_bytes(self) -> [u8; 16] {
        // x86 is little-endian whatever the host is, and `to_le_bytes` says
        // so rather than assuming the host agrees.
        self.to_bits(FpFormat::X87Extended).to_le_bytes()
    }

    /// The value's bit pattern at `fp_size` bits, as an integer.
    ///
    /// This is the only way to name a floating constant in an assembler
    /// operand or an immediate: an inline-asm constraint asking for an
    /// immediate gets these bits, and one asking for a general register gets
    /// them loaded into it.
    ///
    /// Widths above 64 bits are not representable in one integer; callers with
    /// a `long double` want [`to_bits`](Self::to_bits) at its format instead,
    /// and this answers with the `double` encoding rather than a wrong wide
    /// value.
    pub fn to_bits_at_width(self, fp_size: u32) -> i64 {
        let fmt = match fp_size {
            16 => FpFormat::Binary16,
            32 => FpFormat::Binary32,
            _ => FpFormat::Binary64,
        };
        self.to_bits(fmt) as i64
    }

    /// The IEEE binary128 encoding, as `(low, high)` 64-bit halves.
    pub fn to_f128_bits(self) -> (u64, u64) {
        let bits = self.to_bits(FpFormat::Binary128);
        (bits as u64, (bits >> 64) as u64)
    }

    /// True if this is any NaN.
    pub fn is_nan(self) -> bool {
        self.exp == EXP_SPECIAL && self.sig & !INTEGER_BIT != 0
    }

    /// True for a signalling NaN: one whose quiet bit is clear, so that an
    /// operation consuming it raises *invalid*.
    pub fn is_signalling_nan(self) -> bool {
        self.is_nan() && self.sig & QUIET_BIT == 0
    }

    /// True for a finite value: neither an infinity nor a NaN.
    pub fn is_finite(self) -> bool {
        self.exp != EXP_SPECIAL
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

    /// The same magnitude with the sign cleared: `fabs`, which changes
    /// nothing else, a NaN's payload included.
    pub fn magnitude(self) -> Self {
        FloatVal { neg: false, ..self }
    }

    /// Whether the sign bit is set: `signbit`, true for `-0.0` and for a
    /// NaN whose sign is set, which no comparison can see.
    pub fn sign_bit(self) -> bool {
        self.neg
    }

    /// This magnitude with the sign of `sign`: `copysign(self, sign)`. Only
    /// the sign bit is taken, of a zero or a NaN as of anything else, and
    /// nothing of this value but its sign changes, a NaN's payload included.
    pub fn with_sign_of(self, sign: Self) -> Self {
        FloatVal {
            neg: sign.neg,
            ..self
        }
    }

    /// This value's ordering against `other`, or `None` when the two are
    /// unordered because either is a NaN.
    ///
    /// C's ordering, which neither of the two comparisons already here gives:
    /// [`PartialEq`] compares *encodings*, so it separates the signed zeros
    /// that C's `==` equates, and `to_f64` rounds, so two `long double`
    /// values differing only below the 53rd significand bit come back equal.
    ///
    /// Exact because the encoding is canonical sign-magnitude: within one
    /// sign, `(exp, sig)` orders magnitude outright, infinity being the one
    /// value at the largest exponent once NaN has been taken out.
    pub fn cmp_value(self, other: Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering;
        if self.is_nan() || other.is_nan() {
            return None;
        }
        // C has one zero.
        if self.is_zero() && other.is_zero() {
            return Some(Ordering::Equal);
        }
        if self.neg != other.neg {
            return Some(if self.neg {
                Ordering::Less
            } else {
                Ordering::Greater
            });
        }
        let ord = (self.exp, self.sig).cmp(&(other.exp, other.sig));
        Some(if self.neg { ord.reverse() } else { ord })
    }

    /// The integer part, truncated toward zero, as a sign and a magnitude.
    ///
    /// `None` when there is no integer part to take: a NaN, an infinity, or
    /// a magnitude of 2^128 or more.
    ///
    /// Not `to_f64() as i128`: rounding to `f64` first moves the value before
    /// the fractional part is discarded, so `0x1p62L + 1` came out 2^62 and
    /// a `long double` just below an integer truncated one too high.
    fn trunc_magnitude(self) -> Option<(bool, u128)> {
        if self.exp == EXP_SPECIAL {
            return None;
        }
        if self.sig == 0 {
            return Some((self.neg, 0));
        }
        // The significand's top bit is worth 2^(exp - BIAS).
        let top = self.exp as i32 - BIAS;
        if top < 0 {
            // Magnitude below 1, which includes every subnormal.
            return Some((self.neg, 0));
        }
        if top >= SIG_BITS as i32 {
            return None;
        }
        Some((self.neg, self.sig >> (127 - top as u32)))
    }

    /// C's conversion of this value to an integer type `bits` wide (C17
    /// 6.3.1.4p1): the fractional part is discarded, truncating toward zero.
    ///
    /// The one place a floating value becomes an integer, for every fold that
    /// converts one: a cast in a constant expression, a static initializer,
    /// an `fcvt` instruction with a constant operand.
    ///
    /// `None` when the truncated value is outside the type's range -- a NaN
    /// and an infinity included -- which is where C leaves the conversion
    /// undefined. The value is whatever it already is: a caller holding a
    /// wider value than its type rounds it to that type's format first.
    ///
    /// The answer is the integer's two's-complement bit pattern read as an
    /// `i128`, so an unsigned 128-bit result of 2^127 or more is negative
    /// here, as it is everywhere else c17 carries one.
    pub fn to_integer(self, bits: u32, signed: bool) -> Option<i128> {
        debug_assert!((1..=128).contains(&bits), "integer width {bits}");
        let (neg, mag) = self.trunc_magnitude()?;
        let limit = if signed { bits - 1 } else { bits };
        // The largest magnitude on the side the value is on: 2^(bits-1) - 1
        // or 2^(bits-1) for a signed type, 2^bits - 1 or 0 for an unsigned
        // one. A negative value truncating to zero is always in range.
        let largest = match (signed, neg) {
            (false, true) => 0,
            (true, true) => 1u128 << limit,
            _ if limit == 128 => u128::MAX,
            _ => (1u128 << limit) - 1,
        };
        if mag > largest {
            return None;
        }
        let v = mag as i128;
        Some(if neg { v.wrapping_neg() } else { v })
    }

    /// [`to_integer`](Self::to_integer), with gcc's answer where C gives
    /// none: a value beyond the range saturates to the nearer end of it, and
    /// a NaN becomes 0.
    ///
    /// Only for a context that must have *some* value and cannot leave the
    /// conversion to run time -- a static initializer. gcc folds the same
    /// constant to the same answer there, so the two compilers agree on a
    /// program whose behavior C does not define.
    pub fn to_integer_saturating(self, bits: u32, signed: bool) -> i128 {
        if let Some(v) = self.to_integer(bits, signed) {
            return v;
        }
        if self.is_nan() {
            return 0;
        }
        match (signed, self.neg) {
            (false, true) => 0,
            (true, true) => (-1i128) << (bits - 1),
            (true, false) if bits == 128 => i128::MAX,
            (true, false) => (1i128 << (bits - 1)) - 1,
            (false, false) if bits == 128 => -1,
            (false, false) => ((1u128 << bits) - 1) as i128,
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
        self.to_bits(FpFormat::X87Extended)
    }
}

impl PartialEq for FloatVal {
    /// Bitwise equality, deliberately: this compares *encodings*, not
    /// arithmetic values, so it is reflexive on NaN and distinguishes the
    /// signed zeros. Callers wanting C's `==` want [`FloatVal::cmp_value`].
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

    /// Width of the stored significand field: the fraction, plus the integer
    /// bit in the one format that stores it explicitly.
    fn stored_significand_bits(self) -> u32 {
        match self {
            FpFormat::X87Extended => 64,
            _ => self.precision() - 1,
        }
    }

    /// Width of the biased exponent field.
    fn exponent_bits(self) -> u32 {
        match self {
            FpFormat::Binary16 => 5,
            FpFormat::Binary32 => 8,
            FpFormat::Binary64 => 11,
            FpFormat::X87Extended | FpFormat::Binary128 => 15,
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

    fn trailing_zeros(self) -> u32 {
        if self.lo == 0 {
            128 + self.hi.trailing_zeros()
        } else {
            self.lo.trailing_zeros()
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

    /// The integer square root, `floor(sqrt(self))`, and whether it was
    /// inexact -- whether a remainder was left.
    ///
    /// Digit by digit, two bits of the radicand for each bit of the root:
    /// the root of a 256-bit value has at most 128 bits, and the remainder
    /// stays below `2 * root + 1`, so neither leaves its type.
    fn isqrt(self) -> (u128, bool) {
        let mut root: u128 = 0;
        let mut rem = U256::ZERO;
        for i in (0..128).rev() {
            rem = rem.shl(2);
            rem.lo |= self.shr(2 * i).lo & 3;
            // Whether the next root bit is 1: (2 * root + 1)^2 - (2 * root)^2
            // is 4 * root + 1, in the scale of the two bits just brought down.
            let trial = U256 { hi: 0, lo: root }.shl(2).add(U256::ONE);
            root <<= 1;
            if rem >= trial {
                rem = rem.sub(trial);
                root |= 1;
            }
        }
        (root, !rem.is_zero())
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
    ///
    /// A NaN keeps its sign, its kind and as much of its payload as `fmt` has
    /// room for -- the high-order bits, since the payload is held
    /// left-aligned. The one NaN `fmt` cannot hold as it is, a signalling one
    /// whose payload lies wholly below `fmt`'s significand, is quieted rather
    /// than turned into an infinity; no NaN c17 builds has that shape, since
    /// each is built in the format of its own type.
    pub fn round_to_format(self, fmt: FpFormat) -> Self {
        if self.is_nan() {
            let kept = self.sig & !((1u128 << (SIG_BITS - fmt.precision())) - 1);
            let nan = FloatVal { sig: kept, ..self };
            return if nan.is_nan() { nan } else { self.quieted() };
        }
        if self.exp == EXP_SPECIAL || self.is_zero() {
            return self;
        }
        let (sig, exp2) = self.scaled();
        Self::round_wide(self.neg, U256::scaled127(sig), exp2 - 127, false, fmt)
    }

    /// C's conversion of this value from format `src` to format `dst`
    /// (6.3.1.5): as `src` holds it, then rounded to `dst`.
    ///
    /// **Rounded twice, and both roundings are load-bearing.** A literal is
    /// held at 128 significand bits, not yet the value its own type holds, so
    /// rounding straight to `dst` skips a step the program does not:
    /// `(float)(_Float16)0.3f16` is `0.30004883`, the nearest `float` to the
    /// nearest `_Float16` to `0.3`, and converting in one go gives `0.3f`
    /// instead.
    ///
    /// A conversion between two formats quiets a signalling NaN, as every
    /// target's conversion instruction does, and keeps its sign and the
    /// high-order bits of its payload: narrowing drops the low ones, widening
    /// appends zeros. Converting to the same format is no conversion at all
    /// and leaves a signalling NaN signalling.
    pub fn convert(self, src: FpFormat, dst: FpFormat) -> Self {
        let v = self.round_to_format(src);
        if src == dst {
            return v;
        }
        v.quieted().round_to_format(dst)
    }

    /// The NaN an arithmetic operation on `a` and `b` gives when either of
    /// them is one: the first NaN operand, quieted, with its own sign and
    /// payload -- what x86-64's SSE and x87 instructions deliver, and what
    /// aarch64 delivers unless only the second operand is signalling. `None`
    /// when neither operand is a NaN.
    fn propagated_nan(a: Self, b: Self) -> Option<Self> {
        [a, b].into_iter().find(|v| v.is_nan()).map(Self::quieted)
    }

    /// `self + other`, rounded once to `fmt`.
    pub fn add(self, other: Self, fmt: FpFormat) -> Self {
        self.add_rounded(other, fmt)
    }

    /// `self - other`, rounded once to `fmt`.
    pub fn sub(self, other: Self, fmt: FpFormat) -> Self {
        // A NaN subtrahend comes out with its own sign: the subtraction does
        // not negate it, so it is taken out before the negation below.
        let (a, b) = (self.round_to_format(fmt), other.round_to_format(fmt));
        if let Some(nan) = Self::propagated_nan(a, b) {
            return nan;
        }
        a.add_rounded(b.negated(), fmt)
    }

    /// The common core of `add` and `sub`: `sub` negates first, since
    /// `a - b` and `a + (-b)` are the same in every rounding mode.
    fn add_rounded(self, other: Self, fmt: FpFormat) -> Self {
        let a = self.round_to_format(fmt);
        let b = other.round_to_format(fmt);

        if let Some(nan) = Self::propagated_nan(a, b) {
            return nan;
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

        if let Some(nan) = Self::propagated_nan(a, b) {
            return nan;
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

        if let Some(nan) = Self::propagated_nan(a, b) {
            return nan;
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

    /// The square root, rounded once to `fmt` (to nearest, ties to even):
    /// what `sqrt`, `sqrtf` and `sqrtl` compute, and what every target's
    /// square-root instruction delivers.
    ///
    /// `None` where the answer is not the value's alone. A number below zero
    /// is a domain error: the library sets `errno`, and the NaN it returns is
    /// the target's default NaN, which is negative on x86-64 and positive on
    /// aarch64. A signalling NaN raises *invalid* as it is quieted. Everything
    /// else is exact: `-0` and `+inf` are their own roots, and a quiet NaN
    /// comes back as it went in, sign and payload included.
    ///
    /// The root is taken of the significand as an integer: scaled by an even
    /// power of two to 255 or 256 bits, its integer square root has 128,
    /// which is more than any format keeps, and a remainder is the sticky bit
    /// that settles a tie -- though a square root is never exactly half way.
    pub fn sqrt(self, fmt: FpFormat) -> Option<Self> {
        let a = self.round_to_format(fmt);
        if a.is_nan() {
            return (!a.is_signalling_nan()).then_some(a);
        }
        if a.is_zero() {
            return Some(a);
        }
        if a.neg {
            return None;
        }
        if a.is_infinite() {
            return Some(a);
        }
        // The value is `sig * 2^exp2`. Shifting `sig` up by 127 or 128 bits,
        // whichever leaves an even exponent, halves that exponent exactly.
        let (sig, exp2) = a.scaled();
        let shift = if exp2.rem_euclid(2) == 0 { 128 } else { 127 };
        let (root, inexact) = U256::scaled127(sig).shl(shift - 127).isqrt();
        Some(Self::round_wide(
            false,
            U256 { hi: 0, lo: root },
            (exp2 - shift as i32) / 2,
            inexact,
            fmt,
        ))
    }

    /// The smaller of two values, as `fmin` computes it: a NaN operand is
    /// ignored for the other, and of two zeros `-0` is the smaller.
    ///
    /// C leaves the zeros to the implementation (F.10.9.2); `-0` is what
    /// aarch64's `fminnm` answers and what gcc folds. glibc's x86-64 `fmin`
    /// answers its first operand instead, so there the fold and the call can
    /// disagree, as they do under gcc. Two NaNs give the first, quiet. `None`
    /// for a signalling NaN, which raises *invalid*.
    pub fn fmin(self, other: Self, fmt: FpFormat) -> Option<Self> {
        self.min_max(other, fmt, std::cmp::Ordering::Less)
    }

    /// The larger of two values, as `fmax` computes it: [`Self::fmin`]'s
    /// rules, with `+0` the larger of two zeros.
    pub fn fmax(self, other: Self, fmt: FpFormat) -> Option<Self> {
        self.min_max(other, fmt, std::cmp::Ordering::Greater)
    }

    /// `fmin` (`want` Less) or `fmax` (`want` Greater).
    fn min_max(self, other: Self, fmt: FpFormat, want: std::cmp::Ordering) -> Option<Self> {
        let (a, b) = (self.round_to_format(fmt), other.round_to_format(fmt));
        if a.is_signalling_nan() || b.is_signalling_nan() {
            return None;
        }
        if a.is_nan() {
            return Some(if b.is_nan() { a } else { b });
        }
        if b.is_nan() {
            return Some(a);
        }
        if a.is_zero() && b.is_zero() {
            // -0 below +0, for this purpose only.
            let neg = if want == std::cmp::Ordering::Less {
                a.neg || b.neg
            } else {
                a.neg && b.neg
            };
            return Some(Self::signed_zero(neg));
        }
        Some(if a.cmp_value(b) == Some(want.reverse()) {
            b
        } else {
            a
        })
    }

    /// `self * y + z`, rounded once to `fmt`: what `fma` computes.
    ///
    /// The product is exact in 256 bits, and the sum is formed exactly or with
    /// a sticky bit below the smaller addend, as in `add_rounded`, so there is
    /// one rounding at the end. `None` for an infinite or NaN operand, and for
    /// a result that overflows -- the cases that raise, which arithmetic
    /// leaves to run time too.
    pub fn fma(self, y: Self, z: Self, fmt: FpFormat) -> Option<Self> {
        let (a, b, c) = (
            self.round_to_format(fmt),
            y.round_to_format(fmt),
            z.round_to_format(fmt),
        );
        if !a.is_finite() || !b.is_finite() || !c.is_finite() {
            return None;
        }
        let neg_p = a.neg != b.neg;
        let r = if a.is_zero() || b.is_zero() {
            if c.is_zero() {
                // Round to nearest gives +0 unless both addends are -0.
                Self::signed_zero(neg_p && c.neg)
            } else {
                c
            }
        } else {
            let (sa, ea) = a.scaled();
            let (sb, eb) = b.scaled();
            let product = U256::mul(sa, sb);
            if c.is_zero() {
                Self::round_wide(neg_p, product, ea + eb, false, fmt)
            } else {
                let (sc, ec) = c.scaled();
                Self::sum_exact(
                    (neg_p, product, ea + eb),
                    (c.neg, U256 { hi: 0, lo: sc }, ec),
                    fmt,
                )
            }
        };
        r.is_finite().then_some(r)
    }

    /// `p + c` for two nonzero values each `(sign, magnitude, exponent of its
    /// low bit)`, rounded once to `fmt`.
    ///
    /// Both are aligned in one 256-bit frame whose top is two bits above the
    /// larger's leading bit, so the sum cannot carry out of it. The larger
    /// always fits exactly; the smaller may fall off the bottom, and then it
    /// is at least 28 bits below the larger's leading bit, far enough that a
    /// sticky bit in its place rounds exactly as the lost bits would (see
    /// `add_rounded`).
    fn sum_exact(p: (bool, U256, i32), c: (bool, U256, i32), fmt: FpFormat) -> Self {
        // Trailing zeros cost nothing to drop, and dropping them leaves each
        // magnitude at most 226 bits: no format has more than 113.
        let trim = |(neg, m, e): (bool, U256, i32)| {
            let tz = m.trailing_zeros();
            (neg, m.shr(tz), e + tz as i32)
        };
        let (p, c) = (trim(p), trim(c));
        let top = |(_, m, e): (bool, U256, i32)| 255 - m.leading_zeros() as i32 + e;
        let frame = top(p).max(top(c)) - 253;
        let place = |(neg, m, e): (bool, U256, i32)| {
            if e >= frame {
                (neg, m.shl((e - frame) as u32), false)
            } else {
                let (m, lost) = m.shr_lossy((frame - e) as u32);
                (neg, m, lost)
            }
        };
        let ((pn, pm, pl), (cn, cm, cl)) = (place(p), place(c));
        if pn == cn {
            return Self::round_wide(pn, pm.add(cm), frame, pl || cl, fmt);
        }
        let (big, small, small_lost, neg) = match pm.cmp(&cm) {
            std::cmp::Ordering::Greater => (pm, cm, cl, pn),
            std::cmp::Ordering::Less => (cm, pm, pl, cn),
            // Only an exact tie cancels to zero: a lossy operand is far below
            // the other.
            std::cmp::Ordering::Equal => return Self::ZERO,
        };
        let diff = big.sub(small);
        // A lossy smaller operand is truly a little above `small`, so the
        // difference is a little below `diff`.
        let (w, sticky) = if small_lost {
            (diff.sub(U256::ONE), true)
        } else {
            (diff, false)
        };
        Self::round_wide(neg, w, frame, sticky, fmt)
    }

    /// The integer `how` rounds this value to, in `fmt`: what `floor`,
    /// `ceil`, `trunc`, `round`, `rint` and `nearbyint` compute.
    ///
    /// Exact, since the integer is always representable where the value is,
    /// and the sign is always the value's: `ceil(-0.5)` is `-0`. An infinity,
    /// a zero and a quiet NaN (sign and payload kept) are their own answer.
    ///
    /// `None` where the answer is not the value's alone: a signalling NaN,
    /// which raises *invalid* as it is quieted, and a `rint` or `nearbyint`
    /// of a value that is not already an integer, whose answer is the current
    /// rounding direction's -- a program can change it with `fesetround`, so
    /// gcc does not fold one either.
    pub fn round_to_integral(self, how: IntegralRounding, fmt: FpFormat) -> Option<Self> {
        let a = self.round_to_format(fmt);
        if a.is_nan() {
            return (!a.is_signalling_nan()).then_some(a);
        }
        if a.exp == EXP_SPECIAL || a.is_zero() {
            return Some(a);
        }
        // The value is `sig * 2^exp2`; nothing below 2^0 means an integer.
        let (sig, exp2) = a.scaled();
        if exp2 >= 0 {
            return Some(a);
        }
        let frac_bits = exp2.unsigned_abs();
        let (int, rem, half) = match frac_bits {
            ..=127 => (
                sig >> frac_bits,
                sig & ((1u128 << frac_bits) - 1),
                1u128 << (frac_bits - 1),
            ),
            // The whole significand is fraction; at 128 bits it is at least
            // half, beyond that below it.
            128 => (0, sig, 1u128 << 127),
            _ => (0, sig, u128::MAX),
        };
        if rem == 0 {
            return Some(a);
        }
        let away = match how {
            IntegralRounding::Floor => a.neg,
            IntegralRounding::Ceil => !a.neg,
            IntegralRounding::Trunc => false,
            IntegralRounding::Round => rem >= half,
            IntegralRounding::Rint | IntegralRounding::NearbyInt => return None,
        };
        Some(Self::from_parts(a.neg, int + u128::from(away), 0))
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

/// A complex value: its real half, then its imaginary half.
pub type Complex = (FloatVal, FloatVal);

/// The four halves of a complex operation's operands, `(a, b, c, d)` of
/// `(a + bi) op (c + di)`.
type Operands = (FloatVal, FloatVal, FloatVal, FloatVal);

/// A format whose floating complex `*` and `/` c17 computes by calling
/// libgcc's `__mul?c3` and `__div?c3` for that format.
///
/// Every floating format but binary16 has its own routine. gcc does not call
/// `__mulhc3`/`__divhc3` for `_Float16 _Complex` on x86-64 or aarch64: it
/// widens the operands to `float`, calls `__mulsc3`/`__divsc3`, and narrows
/// the result -- which rounds differently from computing each step in half
/// precision. Leaving binary16 out of this type is what makes a caller ask
/// [`FpFormat::complex_routine_format`] rather than pick a routine for it.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ComplexRoutineFormat {
    Binary32,
    Binary64,
    X87Extended,
    Binary128,
}

impl ComplexRoutineFormat {
    /// The format the routine takes and returns its halves in.
    pub fn format(self) -> FpFormat {
        match self {
            ComplexRoutineFormat::Binary32 => FpFormat::Binary32,
            ComplexRoutineFormat::Binary64 => FpFormat::Binary64,
            ComplexRoutineFormat::X87Extended => FpFormat::X87Extended,
            ComplexRoutineFormat::Binary128 => FpFormat::Binary128,
        }
    }

    /// The wider format libgcc divides this one's complex values in, where
    /// it has one: `__divsc3` works in `double`, and the extra precision lets
    /// it use the textbook formula. The wider formats have nothing wider to
    /// work in and use Smith's method instead.
    fn div_working_format(self) -> Option<FpFormat> {
        match self {
            ComplexRoutineFormat::Binary32 => Some(FpFormat::Binary64),
            _ => None,
        }
    }
}

impl FpFormat {
    /// The format a complex `*` or `/` with halves in this format is
    /// computed in, as gcc computes it: the format itself, except that
    /// binary16 is widened to binary32 (see [`ComplexRoutineFormat`]).
    pub fn complex_routine_format(self) -> ComplexRoutineFormat {
        match self {
            FpFormat::Binary16 | FpFormat::Binary32 => ComplexRoutineFormat::Binary32,
            FpFormat::Binary64 => ComplexRoutineFormat::Binary64,
            FpFormat::X87Extended => ComplexRoutineFormat::X87Extended,
            FpFormat::Binary128 => ComplexRoutineFormat::Binary128,
        }
    }
}

// Complex multiplication and division.
//
// A complex constant is folded by the algorithm the program runs when the
// same operation is not constant: c17 lowers a floating complex `*` and `/`
// to libgcc's `__mul?c3` and `__div?c3`, and these are those routines
// (libgcc2.c, GCC 13), each operation rounded to the base format as they
// round it. Folding by any other rule -- the textbook formula through `f64`
// was the old one -- makes `static` and automatic copies of one expression
// differ.
//
// Measured against libgcc 13 on 20000 random operand sets per format,
// specials and cancelling products included, this agrees bit for bit --
// every finite result, and Annex G's infinity recovery -- with
// `__mul{s,d,x}c3`/`__div{s,d,x}c3` on x86-64 and with `__mul{s,d,t}c3`,
// `__div{s,t}c3` on aarch64. A NaN result is a NaN, but its sign and payload
// are this type's default rather than the hardware's. What does not agree:
// - aarch64 libgcc is built with floating contraction, and `__divdc3` fuses
//   each `x * ratio + y` into one `fmadd`, so a `double _Complex` quotient
//   there can differ in its last place (about a third of random ones do).
//   [`Contraction::Fused`] computes those steps as aarch64 does, for a
//   caller that must match its run-time result exactly. The rest fuse to no
//   effect: the multiplications fuse only inside the infinity recovery,
//   where each fused product is exact or is added to a zero, so only a
//   NaN's sign can come out different; `__divsc3`'s fused `double` steps
//   multiply `float`s, which `double` holds exactly; binary128 is software
//   and fuses nothing; nothing on x86-64 fuses.
// - gcc folds a complex constant with MPC, correctly rounded, and can differ
//   from its own run-time result -- and so from this -- where `ac - bd`
//   cancels.
// - Apple's `__divdc3` is compiler-rt's, which scales by `logb` instead.
/// How a target's `__div?c3` computes a product that feeds a sum, `x * y +
/// z`, in Smith's method.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Contraction {
    /// The product is rounded, then the sum: x86-64's libgcc, and every
    /// binary128 routine, which is software.
    Separate,
    /// One `fmadd`, rounded once: aarch64's libgcc.
    Fused,
}

impl FloatVal {
    /// `1` or `0`, as `isinf(v) ? 1 : 0`, carrying `v`'s sign: how libgcc
    /// "boxes" an infinite operand before recomputing.
    fn boxed(self) -> Self {
        let unit = if self.is_infinite() {
            FloatVal::from_i128(1)
        } else {
            FloatVal::ZERO
        };
        unit.with_sign_of(self)
    }

    /// A NaN operand replaced by a zero of its own sign; anything else kept.
    fn nan_to_zero(self) -> Self {
        if self.is_nan() {
            FloatVal::ZERO.with_sign_of(self)
        } else {
            self
        }
    }

    /// `|self| < |other|`, false when either is a NaN -- C's `<` on `fabs`.
    fn magnitude_below(self, other: Self) -> bool {
        self.magnitude().cmp_value(other.magnitude()) == Some(std::cmp::Ordering::Less)
    }

    /// `(a + bi) * (c + di)` with halves in `fmt`, as the program computes
    /// it: by the `__mul?c3` of `fmt`'s routine format, and rounded back.
    pub fn complex_mul(x: Complex, y: Complex, fmt: FpFormat) -> Complex {
        Self::through_routine(x, y, fmt, |x, y, r| Some(Self::routine_mul(x, y, r)))
            .expect("a multiplication always computes")
    }

    /// `(a + bi) / (c + di)` with halves in `fmt`, as the program computes
    /// it: by the `__div?c3` of `fmt`'s routine format, and rounded back.
    pub fn complex_div(x: Complex, y: Complex, fmt: FpFormat) -> Complex {
        Self::complex_div_by(x, y, fmt, Contraction::Separate)
            .expect("separate steps always compute")
    }

    /// [`complex_div`](Self::complex_div) by a `__div?c3` that contracts
    /// as `contraction` says; `None` where a fused step is not computed
    /// here (see `mul_add`).
    pub fn complex_div_by(
        x: Complex,
        y: Complex,
        fmt: FpFormat,
        contraction: Contraction,
    ) -> Option<Complex> {
        Self::through_routine(x, y, fmt, |x, y, r| Self::routine_div(x, y, r, contraction))
    }

    /// `op` applied in `fmt`'s routine format, as c17's lowering applies it:
    /// the operands, held in `fmt`, converted to the routine's format, and
    /// the result converted back.
    fn through_routine(
        x: Complex,
        y: Complex,
        fmt: FpFormat,
        op: impl FnOnce(Complex, Complex, ComplexRoutineFormat) -> Option<Complex>,
    ) -> Option<Complex> {
        let routine = fmt.complex_routine_format();
        let wide = routine.format();
        let w = |v: Self| v.convert(fmt, wide);
        let (re, im) = op((w(x.0), w(x.1)), (w(y.0), w(y.1)), routine)?;
        Some((re.convert(wide, fmt), im.convert(wide, fmt)))
    }

    /// `x * y + z` in `fmt`, as a routine built with `contraction` computes
    /// it.
    ///
    /// Fused, it is rounded once, which [`Self::fma`] computes for finite
    /// operands. With an infinite or NaN factor the product is exact, so
    /// fusing changes nothing; with an infinite or NaN addend it changes
    /// nothing either while the rounded product is finite. What is left --
    /// a result that overflows, or a product that overflows only once
    /// rounded -- is `None`.
    fn mul_add(x: Self, y: Self, z: Self, fmt: FpFormat, contraction: Contraction) -> Option<Self> {
        let product = x.mul(y, fmt);
        let separate = product.add(z, fmt);
        if contraction == Contraction::Separate {
            return Some(separate);
        }
        if x.is_finite() && y.is_finite() && z.is_finite() {
            x.fma(y, z, fmt)
        } else if !x.is_finite() || !y.is_finite() || product.is_finite() {
            Some(separate)
        } else {
            None
        }
    }

    /// `(a + bi) * (c + di)` as `__mul?c3` computes it: `(ac - bd) +
    /// (ad + bc)i`, each product and each sum rounded to the routine's
    /// format, and C17 Annex G's recovery of an infinity that came out
    /// NaN + NaNi.
    fn routine_mul(x: Complex, y: Complex, routine: ComplexRoutineFormat) -> Complex {
        let fmt = routine.format();
        let (mut a, mut b, mut c, mut d) = (x.0, x.1, y.0, y.1);
        let (ac, bd) = (a.mul(c, fmt), b.mul(d, fmt));
        let (ad, bc) = (a.mul(d, fmt), b.mul(c, fmt));
        let re = ac.sub(bd, fmt);
        let im = ad.add(bc, fmt);
        if !(re.is_nan() && im.is_nan()) {
            return (re, im);
        }

        let mut recalc = false;
        if a.is_infinite() || b.is_infinite() {
            // The left factor is infinite: box it, and zero the NaNs in the
            // right one.
            (a, b) = (a.boxed(), b.boxed());
            (c, d) = (c.nan_to_zero(), d.nan_to_zero());
            recalc = true;
        }
        if c.is_infinite() || d.is_infinite() {
            (c, d) = (c.boxed(), d.boxed());
            (a, b) = (a.nan_to_zero(), b.nan_to_zero());
            recalc = true;
        }
        if !recalc && [ac, bd, ad, bc].iter().any(|p| p.is_infinite()) {
            // An infinity from overflow, not from an operand.
            (a, b, c, d) = (
                a.nan_to_zero(),
                b.nan_to_zero(),
                c.nan_to_zero(),
                d.nan_to_zero(),
            );
            recalc = true;
        }
        if !recalc {
            return (re, im);
        }
        let inf = Self::infinity(false);
        let re = a.mul(c, fmt).sub(b.mul(d, fmt), fmt);
        let im = a.mul(d, fmt).add(b.mul(c, fmt), fmt);
        (inf.mul(re, fmt), inf.mul(im, fmt))
    }

    /// `(a + bi) / (c + di)` as `__div?c3` computes it.
    ///
    /// `float` divides by the textbook formula in `double` and rounds the
    /// result once more; the wider formats use Smith's method, dividing
    /// through by the larger half of the divisor, with libgcc's scaling
    /// against overflow and underflow. Annex G's recovery of an infinity or a
    /// zero that came out NaN + NaNi follows.
    fn routine_div(
        x: Complex,
        y: Complex,
        routine: ComplexRoutineFormat,
        contraction: Contraction,
    ) -> Option<Complex> {
        let fmt = routine.format();
        let (a, b, c, d) = (x.0, x.1, y.0, y.1);
        let ((re, im), (a, b, c, d)) = match routine.div_working_format() {
            Some(wide) => {
                // Widening is exact, so the operands need no conversion.
                let denom = c.mul(c, wide).add(d.mul(d, wide), wide);
                let re = a.mul(c, wide).add(b.mul(d, wide), wide).div(denom, wide);
                let im = b.mul(c, wide).sub(a.mul(d, wide), wide).div(denom, wide);
                ((re.convert(wide, fmt), im.convert(wide, fmt)), (a, b, c, d))
            }
            None => Self::smith_div((a, b), (c, d), fmt, contraction)?,
        };
        if !(re.is_nan() && im.is_nan()) {
            return Some((re, im));
        }
        Some(Self::recover_quotient((re, im), (a, b, c, d), fmt))
    }

    /// Annex G's recovery of a quotient `(re, im)` that came out NaN + NaNi,
    /// from the operands as `__div?c3` left them.
    fn recover_quotient((re, im): Complex, (a, b, c, d): Operands, fmt: FpFormat) -> Complex {
        let inf = Self::infinity(false);
        let zero = FloatVal::ZERO;
        if c.is_zero() && d.is_zero() && (!a.is_nan() || !b.is_nan()) {
            // Non-zero over zero.
            let inf = inf.with_sign_of(c);
            (inf.mul(a, fmt), inf.mul(b, fmt))
        } else if (a.is_infinite() || b.is_infinite()) && c.is_finite() && d.is_finite() {
            // Infinite over finite.
            let (a, b) = (a.boxed(), b.boxed());
            let re = a.mul(c, fmt).add(b.mul(d, fmt), fmt);
            let im = b.mul(c, fmt).sub(a.mul(d, fmt), fmt);
            (inf.mul(re, fmt), inf.mul(im, fmt))
        } else if (c.is_infinite() || d.is_infinite()) && a.is_finite() && b.is_finite() {
            // Finite over infinite.
            let (c, d) = (c.boxed(), d.boxed());
            let re = a.mul(c, fmt).add(b.mul(d, fmt), fmt);
            let im = b.mul(c, fmt).sub(a.mul(d, fmt), fmt);
            (zero.mul(re, fmt), zero.mul(im, fmt))
        } else {
            (re, im)
        }
    }

    /// The Smith's-method half of [`complex_div`](Self::complex_div): the
    /// quotient before Annex G's recovery, and the four operands as scaled
    /// here, since libgcc scales them in place and recovers from those.
    ///
    /// Every product here feeds a sum, and each is where aarch64 fuses: the
    /// denominator `small * ratio + big`, and each numerator's product with
    /// the half it is added to or subtracted from.
    fn smith_div(
        x: Complex,
        y: Complex,
        fmt: FpFormat,
        contraction: Contraction,
    ) -> Option<(Complex, Operands)> {
        let (mut a, mut b) = x;
        let (c, d) = y;
        let p = fmt.precision() as i32;
        // libgcc's thresholds, from the format's own <float.h> values.
        let rbig = Self::from_parts(false, (1u128 << p) - 1, fmt.emax() - p);
        let rmin = Self::from_parts(false, 1, fmt.emin());
        let rmin2 = Self::from_parts(false, 1, 1 - p);
        let rminscal = Self::from_parts(false, 1, p - 1);
        let rmax2 = rbig.mul(rmin2, fmt);
        let two = Self::from_i128(2);

        // Smith's two arms are one computation with the divisor's halves
        // exchanged: `big` is the half divided through by, `small` the other.
        let swapped = c.magnitude_below(d);
        let (mut big, mut small) = if swapped { (d, c) } else { (c, d) };
        if !big.magnitude_below(rbig) && !big.is_nan() {
            // Prevent underflow when the denominator is near the largest
            // finite value.
            (a, b) = (a.div(two, fmt), b.div(two, fmt));
            (big, small) = (big.div(two, fmt), small.div(two, fmt));
        }
        // Scaling up avoids some underflows; none can overflow, since the
        // halves are below `rmin2` or `rmax2`.
        let scale_up = big.magnitude_below(rmin2)
            || (a.magnitude_below(rmin) && b.magnitude_below(rmax2) && big.magnitude_below(rmax2))
            || (b.magnitude_below(rmin) && a.magnitude_below(rmax2) && big.magnitude_below(rmax2));
        if scale_up {
            (a, b) = (a.mul(rminscal, fmt), b.mul(rminscal, fmt));
            (big, small) = (big.mul(rminscal, fmt), small.mul(rminscal, fmt));
        }

        let mul_add = |x, y, z| Self::mul_add(x, y, z, fmt, contraction);
        let ratio = small.div(big, fmt);
        let denom = mul_add(small, ratio, big)?;
        // The numerators in the order libgcc writes them. `ratio` below the
        // smallest normal is computed the other way round, dividing first, so
        // its subnormal precision is not what the products are built from.
        let ratio_is_normal = rmin.magnitude_below(ratio);
        // `v * r + z`, where `v * r` is `v` scaled.
        let scaled_plus = |v: Self, z| {
            if ratio_is_normal {
                mul_add(v, ratio, z)
            } else {
                mul_add(v.div(big, fmt), small, z)
            }
        };
        let (re, im) = if swapped {
            // |c| < |d|: ((a*r + b) + (b*r - a)i) / (c*r + d), r = c/d.
            (scaled_plus(a, b)?, scaled_plus(b, a.negated())?)
        } else {
            // |c| >= |d|: ((b*r + a) + (b - a*r)i) / (d*r + c), r = d/c.
            (scaled_plus(b, a)?, scaled_plus(a.negated(), b)?)
        };
        let (c, d) = if swapped { (small, big) } else { (big, small) };
        Some(((re.div(denom, fmt), im.div(denom, fmt)), (a, b, c, d)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Value comparison and truncation
    //
    // Both exist because the two comparisons already on this type answer a
    // different question: `PartialEq` compares encodings and `to_f64` rounds.

    #[test]
    fn cmp_value_is_c_equality_not_bitwise() {
        use std::cmp::Ordering;
        let pos = FloatVal::from_f64(0.0);
        let neg = FloatVal::from_f64(-0.0);
        assert_ne!(pos.key(), neg.key(), "the encodings differ");
        assert_eq!(pos.cmp_value(neg), Some(Ordering::Equal), "C has one zero");
    }

    #[test]
    fn cmp_value_is_unordered_on_nan() {
        let nan = FloatVal::nan();
        for other in [
            FloatVal::from_f64(1.0),
            FloatVal::from_f64(-1.0),
            FloatVal::ZERO,
            FloatVal::infinity(false),
            nan,
        ] {
            assert_eq!(nan.cmp_value(other), None, "NaN against {other}");
            assert_eq!(other.cmp_value(nan), None, "{other} against NaN");
        }
    }

    #[test]
    fn cmp_value_orders_across_signs_and_infinities() {
        use std::cmp::Ordering;
        let cases = [
            (-1.0, 1.0, Ordering::Less),
            (-0.0, 1.0, Ordering::Less),
            (-1.0, -0.0, Ordering::Less),
            (-5.0, -1.0, Ordering::Less),
            (1.0, 2.0, Ordering::Less),
            (2.0, 2.0, Ordering::Equal),
            (f64::NEG_INFINITY, -1e308, Ordering::Less),
            (1e308, f64::INFINITY, Ordering::Less),
            (f64::INFINITY, f64::INFINITY, Ordering::Equal),
        ];
        for (a, b, want) in cases {
            let (fa, fb) = (FloatVal::from_f64(a), FloatVal::from_f64(b));
            assert_eq!(fa.cmp_value(fb), Some(want), "{a} vs {b}");
            assert_eq!(fb.cmp_value(fa), Some(want.reverse()), "{b} vs {a}");
        }
    }

    /// The reason this is not `to_f64().partial_cmp(..)`: two values that
    /// differ only below the 53rd significand bit are one `f64`.
    #[test]
    fn cmp_value_separates_values_f64_cannot() {
        use std::cmp::Ordering;
        let one = FloatVal::from_parts(false, 1u128 << 64, 0);
        let barely_more = FloatVal::from_parts(false, (1u128 << 64) | 1, 0);
        assert_eq!(one.to_f64().to_bits(), barely_more.to_f64().to_bits());
        assert_eq!(one.cmp_value(barely_more), Some(Ordering::Less));
    }

    #[test]
    fn to_integer_discards_the_fraction_toward_zero() {
        for (v, want) in [
            (0.0, 0),
            (-0.0, 0),
            (0.9, 0),
            (-0.9, 0),
            (1.0, 1),
            (1.9, 1),
            (-1.9, -1),
            (-2.0, -2),
            (1e18, 1_000_000_000_000_000_000i128),
            (-1e18, -1_000_000_000_000_000_000i128),
        ] {
            assert_eq!(
                FloatVal::from_f64(v).to_integer(64, true),
                Some(want),
                "{v}"
            );
        }
        // A negative value truncating to zero is in range even unsigned.
        assert_eq!(FloatVal::from_f64(-0.9).to_integer(32, false), Some(0));
    }

    /// Just past where `double` stops holding every integer, and where a
    /// `long double` still does: `to_f64() as i128` rounded each of these
    /// to its neighbour first.
    #[test]
    fn to_integer_is_exact_past_double_precision() {
        // 2^53 + 1, 2^62 + 1 and -(2^62 + 5) as signed 64-bit values.
        for (mag, neg) in [
            ((1u128 << 53) + 1, false),
            ((1u128 << 62) + 1, false),
            ((1u128 << 62) + 5, true),
        ] {
            let v = FloatVal::from_parts(neg, mag, 0);
            let want = if neg { -(mag as i128) } else { mag as i128 };
            assert_eq!(v.to_integer(64, true), Some(want), "{want}");
            // The fraction below the last integer bit is discarded, not
            // rounded into it.
            let plus_half = v.add(FloatVal::from_parts(neg, 1, -1), FpFormat::Binary128);
            assert_eq!(plus_half.to_integer(64, true), Some(want), "{want} + 0.5");
        }
        // 2^63 + 3 fits `unsigned long long` and not `long long`.
        let big = FloatVal::from_parts(false, (1u128 << 63) + 3, 0);
        assert_eq!(big.to_integer(64, false), Some((1i128 << 63) + 3));
        assert_eq!(big.to_integer(64, true), None);
        // The least `long long` fits; one below it does not.
        let min = FloatVal::from_parts(true, 1u128 << 63, 0);
        assert_eq!(min.to_integer(64, true), Some(i64::MIN as i128));
        let below = FloatVal::from_parts(true, (1u128 << 63) + 1, 0);
        assert_eq!(below.to_integer(64, true), None);
    }

    /// The range is the destination type's, at every width.
    #[test]
    fn to_integer_checks_the_types_range() {
        let v = |x: f64| FloatVal::from_f64(x);
        assert_eq!(v(127.9).to_integer(8, true), Some(127));
        assert_eq!(v(128.0).to_integer(8, true), None);
        assert_eq!(v(-128.5).to_integer(8, true), Some(-128));
        assert_eq!(v(-129.0).to_integer(8, true), None);
        assert_eq!(v(255.5).to_integer(8, false), Some(255));
        assert_eq!(v(256.0).to_integer(8, false), None);
        assert_eq!(v(-1.0).to_integer(8, false), None);
        assert_eq!(v(3e9).to_integer(32, true), None);
        assert_eq!(v(3e9).to_integer(32, false), Some(3_000_000_000));
        // An unsigned 128-bit result at or above 2^127 is carried as its
        // bit pattern.
        let top = FloatVal::from_parts(false, 1, 127);
        assert_eq!(top.to_integer(128, false), Some(i128::MIN));
        assert_eq!(top.to_integer(128, true), None);
        assert_eq!(
            FloatVal::from_parts(false, 1, 128).to_integer(128, false),
            None
        );
    }

    /// Where C leaves the conversion undefined, this must refuse rather than
    /// invent an answer: a fold that guesses disagrees with the hardware.
    #[test]
    fn to_integer_refuses_what_it_cannot_represent() {
        assert_eq!(FloatVal::nan().to_integer(64, true), None);
        assert_eq!(FloatVal::infinity(false).to_integer(64, true), None);
        assert_eq!(FloatVal::infinity(true).to_integer(64, true), None);
    }

    /// gcc's answer for a static initializer C gives none for.
    #[test]
    fn to_integer_saturating_follows_gcc() {
        let v = |x: f64| FloatVal::from_f64(x);
        assert_eq!(v(1e300).to_integer_saturating(32, true), i32::MAX as i128);
        assert_eq!(v(-1e300).to_integer_saturating(32, true), i32::MIN as i128);
        assert_eq!(v(5e9).to_integer_saturating(32, false), u32::MAX as i128);
        assert_eq!(v(-1.5).to_integer_saturating(32, false), 0);
        assert_eq!(FloatVal::nan().to_integer_saturating(32, true), 0);
        assert_eq!(v(1e40).to_integer_saturating(64, true), i64::MAX as i128);
        assert_eq!(v(1e300).to_integer_saturating(128, true), i128::MAX);
        assert_eq!(v(1e300).to_integer_saturating(128, false), -1);
        // In range, it is the exact conversion.
        assert_eq!(v(-2.5).to_integer_saturating(32, true), -2);
    }

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

    // NaN payloads
    //
    // Every expected encoding below is what gcc emits for the same constant,
    // on x86-64 for x87 and on aarch64 for binary128.

    /// `nan_with_payload` puts the payload in the low bits of the trailing
    /// significand field, and the kind decides the quiet bit.
    #[test]
    fn a_nan_is_built_with_its_payload_in_each_format() {
        use NanKind::{Quiet, Signalling};
        let nan = FloatVal::nan_with_payload;
        let cases: &[(FpFormat, u128, NanKind, u128)] = &[
            (FpFormat::Binary16, 0x5, Quiet, 0x7e05),
            (FpFormat::Binary32, 0x123, Quiet, 0x7fc0_0123),
            (FpFormat::Binary32, 0x123, Signalling, 0x7f80_0123),
            (FpFormat::Binary64, 0x1234, Quiet, 0x7ff8_0000_0000_1234),
            (
                FpFormat::Binary64,
                0x1234,
                Signalling,
                0x7ff0_0000_0000_1234,
            ),
            (FpFormat::Binary64, 0, Quiet, 0x7ff8_0000_0000_0000),
            (
                FpFormat::X87Extended,
                0x1234,
                Quiet,
                0x7fff_c000_0000_0000_1234,
            ),
            (
                FpFormat::X87Extended,
                0x1234,
                Signalling,
                0x7fff_8000_0000_0000_1234,
            ),
            (
                FpFormat::Binary128,
                0x1234,
                Quiet,
                0x7fff_8000_0000_0000_0000_0000_0000_1234,
            ),
            (
                FpFormat::Binary128,
                0x1234,
                Signalling,
                0x7fff_0000_0000_0000_0000_0000_0000_1234,
            ),
        ];
        for &(fmt, payload, kind, want) in cases {
            let v = nan(fmt, payload, kind);
            assert!(v.is_nan());
            assert_eq!(v.to_bits(fmt), want, "{kind:?} {payload:#x} at {fmt:?}");
        }
    }

    /// A signalling NaN cannot have an empty significand -- that would be an
    /// infinity -- so gcc gives it the bit below the quiet bit; and a payload
    /// wider than the format keeps its low bits, the quiet bit overridden.
    #[test]
    fn nan_payload_edges_follow_gcc() {
        use NanKind::{Quiet, Signalling};
        let bits = |fmt, payload, kind| FloatVal::nan_with_payload(fmt, payload, kind).to_bits(fmt);
        assert_eq!(
            bits(FpFormat::Binary64, 0, Signalling),
            0x7ff4_0000_0000_0000
        );
        assert_eq!(bits(FpFormat::Binary32, 0, Signalling), 0x7fa0_0000);
        assert_eq!(
            bits(FpFormat::X87Extended, 0, Signalling),
            0x7fff_a000_0000_0000_0000
        );
        // The payload is exactly the quiet bit: cleared, so empty again.
        assert_eq!(
            bits(FpFormat::Binary64, 1 << 51, Signalling),
            0x7ff4_0000_0000_0000
        );
        assert_eq!(
            bits(FpFormat::Binary64, u64::MAX as u128, Quiet),
            0x7fff_ffff_ffff_ffff
        );
        assert_eq!(
            bits(FpFormat::Binary64, u64::MAX as u128, Signalling),
            0x7ff7_ffff_ffff_ffff
        );
        assert_eq!(bits(FpFormat::Binary32, u128::MAX, Signalling), 0x7fbf_ffff);
    }

    /// The sign operations change the sign and nothing else.
    #[test]
    fn negating_a_nan_flips_only_its_sign() {
        for kind in [NanKind::Quiet, NanKind::Signalling] {
            let v = FloatVal::nan_with_payload(FpFormat::Binary64, 0x1234, kind);
            let neg = v.negated();
            assert_eq!(
                neg.to_bits(FpFormat::Binary64),
                v.to_bits(FpFormat::Binary64) | 1 << 63
            );
            assert_eq!(neg.magnitude(), v);
            assert_eq!(neg.negated(), v);
            assert!(neg.sign_bit() && !v.sign_bit());
            // `copysign` takes the sign alone, from a NaN or a zero too.
            assert_eq!(v.with_sign_of(FloatVal::ZERO.negated()), neg);
            assert_eq!(neg.with_sign_of(FloatVal::from_f64(2.0)), v);
            assert_eq!(FloatVal::from_f64(1.5).with_sign_of(neg).to_f64(), -1.5);
        }
        let x87 = FloatVal::nan_with_payload(FpFormat::X87Extended, 0x1234, NanKind::Quiet);
        assert_eq!(
            x87.negated().to_bits(FpFormat::X87Extended),
            0xffff_c000_0000_0000_1234
        );
    }

    /// A conversion keeps the payload's high-order bits, as the hardware
    /// does: narrowing drops the low ones, widening appends zeros. It quiets
    /// a signalling NaN and keeps the sign.
    #[test]
    fn converting_a_nan_keeps_its_high_payload_bits() {
        use FpFormat::{Binary128, Binary32, Binary64, X87Extended};
        let d = |payload, kind| FloatVal::nan_with_payload(Binary64, payload, kind);
        let f = |payload, kind| FloatVal::nan_with_payload(Binary32, payload, kind);

        // Narrowing: the payload moves down 29 bits, what falls off is lost.
        let v = d(0x4000_0000, NanKind::Quiet).convert(Binary64, Binary32);
        assert_eq!(v.to_bits(Binary32), 0x7fc0_0002);
        let v = d(0x1234, NanKind::Quiet).convert(Binary64, Binary32);
        assert_eq!(v.to_bits(Binary32), 0x7fc0_0000);
        // Widening: up 29 bits.
        let v = f(0x123, NanKind::Quiet).convert(Binary32, Binary64);
        assert_eq!(v.to_bits(Binary64), 0x7ff8_0024_6000_0000);
        // And from binary64 into both long doubles.
        let v = d(0x1234, NanKind::Quiet);
        assert_eq!(
            v.convert(Binary64, X87Extended).to_bits(X87Extended),
            0x7fff_c000_0000_0091_a000
        );
        assert_eq!(
            v.convert(Binary64, Binary128).to_bits(Binary128),
            0x7fff_8000_0000_0123_4000_0000_0000_0000
        );

        // A conversion quiets, and keeps the sign.
        let s = d(0x4000_0000, NanKind::Signalling).negated();
        assert_eq!(s.convert(Binary64, Binary32).to_bits(Binary32), 0xffc0_0002);
        let s = f(0x123, NanKind::Signalling);
        assert_eq!(
            s.convert(Binary32, Binary64).to_bits(Binary64),
            0x7ff8_0024_6000_0000
        );
        // Converting to the same format is no conversion: still signalling.
        let s = d(0x1234, NanKind::Signalling);
        assert_eq!(
            s.convert(Binary64, Binary64).to_bits(Binary64),
            0x7ff0_0000_0000_1234
        );
        // Rounding to a format a signalling NaN's payload lies wholly below
        // cannot keep it signalling; it stays a NaN rather than an infinity.
        assert!(s.round_to_format(Binary32).is_nan());
    }

    /// Arithmetic on a NaN gives the first NaN operand, quieted, with its own
    /// sign and payload -- which a subtraction does not negate.
    #[test]
    fn arithmetic_propagates_a_nan_operand() {
        let fmt = FpFormat::Binary64;
        let one = FloatVal::from_f64(1.0);
        let q5 = FloatVal::nan_with_payload(fmt, 5, NanKind::Quiet);
        let s7 = FloatVal::nan_with_payload(fmt, 7, NanKind::Signalling).negated();
        let bits = |v: FloatVal| v.to_bits(fmt);
        assert_eq!(bits(q5.add(one, fmt)), 0x7ff8_0000_0000_0005);
        assert_eq!(bits(one.mul(q5, fmt)), 0x7ff8_0000_0000_0005);
        assert_eq!(bits(one.sub(s7, fmt)), 0xfff8_0000_0000_0007);
        assert_eq!(bits(s7.div(q5, fmt)), 0xfff8_0000_0000_0007);
        assert_eq!(bits(q5.sub(s7, fmt)), 0x7ff8_0000_0000_0005);
    }

    /// `to_f64` is the binary64 encoding exactly, NaNs included: it used to
    /// answer `f64::NAN` for every NaN, dropping sign, payload and kind.
    #[test]
    fn to_f64_of_a_nan_is_exact() {
        let s = FloatVal::nan_with_payload(FpFormat::Binary64, 0x1234, NanKind::Signalling);
        assert_eq!(s.negated().to_f64().to_bits(), 0xfff0_0000_0000_1234);
        for bits in [0x7ff8_0000_0000_1234u64, 0xfff4_0000_0000_0001] {
            let v = FloatVal::from_f64(f64::from_bits(bits));
            assert_eq!(v.to_f64().to_bits(), bits);
            assert_eq!(v.to_bits(FpFormat::Binary64), u128::from(bits));
        }
    }

    /// `to_bits` at the narrow formats rounds once, from the exact value, and
    /// saturates and underflows at each format's own limits.
    #[test]
    fn narrow_encodings_round_once_from_the_exact_value() {
        let bits16 = |v: f64| FloatVal::from_f64(v).to_bits(FpFormat::Binary16);
        assert_eq!(bits16(0.3), 0x34cd);
        assert_eq!(bits16(-2.0), 0xc000);
        assert_eq!(bits16(65504.0), 0x7bff);
        assert_eq!(bits16(65520.0), 0x7c00, "rounds up to infinity");
        assert_eq!(bits16(2f64.powi(-24)), 0x0001, "smallest subnormal");
        assert_eq!(bits16(2f64.powi(-26)), 0x0000, "below half of it");
        assert_eq!(bits16(f64::NEG_INFINITY), 0xfc00);

        // A `float` literal is rounded to `float` directly, not through
        // `double`: 1 + 2^-24 + 2^-54 is just above a `float` tie, and
        // rounding it to `double` first drops the 2^-54 and makes it a tie,
        // which goes to even -- down.
        let v = FloatVal::from_parts(false, (1u128 << 54) | (1 << 30) | 1, -54);
        assert_eq!(v.to_bits(FpFormat::Binary32), 0x3f80_0001);
        assert_eq!((v.to_f64() as f32).to_bits(), 0x3f80_0000);
    }

    // Complex multiplication and division

    /// A finite value from its encoding in `fmt`: the inverse of `to_bits`
    /// for the operands the libgcc cases below were recorded with.
    fn finite_from_bits(fmt: FpFormat, bits: u128) -> FloatVal {
        let stored = fmt.stored_significand_bits();
        let exp_bits = fmt.exponent_bits();
        let neg = (bits >> (exp_bits + stored)) & 1 != 0;
        let biased = ((bits >> stored) & ((1 << exp_bits) - 1)) as i32;
        assert!(biased != (1 << exp_bits) - 1, "not finite: {bits:x}");
        let mut sig = bits & ((1u128 << stored) - 1);
        if fmt != FpFormat::X87Extended && biased != 0 {
            sig |= 1u128 << stored;
        }
        let exp = biased.max(1) - fmt.emax() - (fmt.precision() as i32 - 1);
        FloatVal::from_parts(neg, sig, exp)
    }

    /// Each case is an operand set and the four halves libgcc 13 returned
    /// for it: `__mulxc3`/`__divxc3` on x86-64, `__multc3`/`__divtc3` on
    /// aarch64.
    #[test]
    fn complex_mul_and_div_agree_with_libgcc() {
        let cases: [(FpFormat, [u128; 8]); 5] = [
            (
                FpFormat::X87Extended,
                [
                    0xbffaeedd22562c4c680f,
                    0xc008fdc2ae94e4dbf800,
                    0xc00aed159abfb303f800,
                    0xbff5a3ef7347ffdd7fd7,
                    0x4006dbf1e0b11d859b87,
                    0x4014eb02a5fd5fee13b1,
                    0x3fef81b36710062b6755,
                    0x3ffd8900d764111b2b41,
                ],
            ),
            (
                FpFormat::X87Extended,
                [
                    0x3fe8a5474d793f2c780a,
                    0x3ff6ff2e6910bdea3800,
                    0xbffc8cb68db0491e1000,
                    0x4024bb4de2fabe6d5fd1,
                    0xc01cbab489f5bea81208,
                    0x400df1dabce3457e95d8,
                    0x3fd1ae62c56ac44b93b5,
                    0xbfc2e1e5688ceda9d084,
                ],
            ),
            (
                FpFormat::X87Extended,
                [
                    0x4011dd56f9d1347a280e,
                    0x3fd9f4c5fd46fe517800,
                    0x400da2d0e03f67599800,
                    0xbfff8a3fadc2684877dd,
                    0x40208cc5a2a44989ee2c,
                    0xc011ef0fe29c3b090dfa,
                    0x4003ae0262cdac84457d,
                    0x3ff593c0d0627c6c6af6,
                ],
            ),
            (
                FpFormat::Binary128,
                [
                    0xbffaddba44ac5898d01ddba44ac5898d,
                    0xc008fb855d29c9b7f000000000000000,
                    0xc00ada2b357f6607f000000000000000,
                    0xbff547dee68fffbaffae08465c001140,
                    0x4006b7e3c1623b0b370e4641afcc431b,
                    0x4014d6054bfabfdc276274af111384db,
                    0x3fef0366ce200c56cea8cd95d626a0c9,
                    0x3ffd1201aec822365682a60b022b31ce,
                ],
            ),
            (
                FpFormat::Binary128,
                [
                    0x3fe84a8e9af27e58f014a8e9af27e58f,
                    0x3ff6fe5cd2217bd47000000000000000,
                    0xbffc196d1b60923c2000000000000000,
                    0x4024769bc5f57cdabfa2590e82a0c950,
                    0xc01c756913eb7d502410b5daa3e92e1a,
                    0x400de3b579c68afd2bb1e9ecb1d9d620,
                    0x3fd15cc58ad58897276a77f9462a0f6e,
                    0xbfc2c3cad119db53a108953a18f51c2e,
                ],
            ),
        ];
        for (fmt, bits) in cases {
            let v = |i: usize| finite_from_bits(fmt, bits[i]);
            let (x, y) = ((v(0), v(1)), (v(2), v(3)));
            let (mre, mim) = FloatVal::complex_mul(x, y, fmt);
            let (qre, qim) = FloatVal::complex_div(x, y, fmt);
            let got = [mre, mim, qre, qim].map(|h| h.to_bits(fmt));
            assert_eq!(
                got,
                [bits[4], bits[5], bits[6], bits[7]],
                "{fmt:?} {:x?}",
                &bits[..4]
            );
        }
    }

    /// At `long double` precision, in both of its formats: the halves keep
    /// the 2^-60 a `double` has no room for.
    #[test]
    fn complex_arithmetic_keeps_long_double_precision() {
        let one = FloatVal::from_i128(1);
        let tiny = FloatVal::from_parts(false, 1, -60);
        let one_plus = FloatVal::from_parts(false, (1u128 << 60) + 1, -60);
        for fmt in [FpFormat::X87Extended, FpFormat::Binary128] {
            let zero = FloatVal::ZERO;
            let (re, im) = FloatVal::complex_mul((one_plus, zero), (one, one), fmt);
            assert_eq!((re, im), (one_plus, one_plus), "{fmt:?} mul");
            let two_plus = FloatVal::from_parts(false, (1u128 << 60) + 1, -59);
            let two = FloatVal::from_i128(2);
            let (re, im) = FloatVal::complex_div((two_plus, zero), (two, zero), fmt);
            assert_eq!((re, im), (one_plus, zero), "{fmt:?} div");
            // Where the textbook formula through `f64` loses the 2^-60 twice:
            // (1 + 2^-60 + i) / (1 + i) = 1 + 2^-61 - 2^-61 i.
            let (re, im) = FloatVal::complex_div((one_plus, one), (one, one), fmt);
            let half_tiny = FloatVal::from_parts(false, 1, -61);
            assert_eq!(re, one.add(half_tiny, fmt), "{fmt:?} re");
            assert_eq!(im, half_tiny.negated(), "{fmt:?} im");
            // `(1 + 2^-60) + 2i` and `(1 + i) - 2^-60`, componentwise.
            assert_eq!(one.add(tiny, fmt), one_plus);
            assert_eq!(
                one.sub(tiny, fmt).cmp_value(one),
                Some(std::cmp::Ordering::Less)
            );
        }
    }

    /// Each product and each sum is rounded to the format, as `__mul?c3`
    /// rounds it -- not the exact `ac - bd` rounded once.
    #[test]
    fn complex_mul_rounds_each_step() {
        let fmt = FpFormat::Binary64;
        // a = c = 1 + 2^-30, b = d = 1: ac rounds away its 2^-60 term, so
        // ac - bd is 2^-29 exactly, where the exact value is 2^-29 + 2^-60.
        let a = FloatVal::from_parts(false, (1u128 << 30) + 1, -30);
        let one = FloatVal::from_i128(1);
        let (re, _) = FloatVal::complex_mul((a, one), (a, one), fmt);
        assert_eq!(re, FloatVal::from_parts(false, 1, -29));
    }

    /// Annex G's recovery, as libgcc does it: an infinite operand gives an
    /// infinite result, not NaN + NaNi, and so does a zero divisor.
    #[test]
    fn complex_recovers_infinities_as_libgcc_does() {
        let fmt = FpFormat::Binary64;
        let v = FloatVal::from_f64;
        let inf = FloatVal::infinity(false);
        let one = v(1.0);

        // (inf + NaN i) * (1 + i): the infinity is boxed to 1 and the NaN
        // zeroed, giving inf + inf i.
        let (re, im) = FloatVal::complex_mul((inf, FloatVal::nan()), (one, one), fmt);
        assert_eq!((re, im), (inf, inf));
        // Overflow of the products recovers too: (big + big i)^2.
        let big = v(1e300);
        let (re, im) = FloatVal::complex_mul((big, big), (big, big), fmt);
        assert!(re.is_nan() && im.is_infinite(), "{re} {im}");

        // Non-zero over zero is an infinity carrying the operand's sign.
        let (re, im) = FloatVal::complex_div((one, v(-1.0)), (FloatVal::ZERO, FloatVal::ZERO), fmt);
        assert_eq!((re, im), (inf, inf.negated()));
        // Finite over infinite is a zero.
        let (re, im) = FloatVal::complex_div((one, one), (inf, FloatVal::ZERO), fmt);
        assert!(re.is_zero() && im.is_zero(), "{re} {im}");
        // Infinite over finite is infinite.
        let (re, im) = FloatVal::complex_div((inf, one), (one, one), fmt);
        assert_eq!((re, im), (inf, inf.negated()));
    }

    /// Smith's method, and libgcc's scaling, keep a quotient whose operands
    /// are near the ends of the range from overflowing or flushing to zero.
    #[test]
    fn complex_div_scales_extreme_operands() {
        let fmt = FpFormat::Binary64;
        let v = FloatVal::from_f64;
        let max = v(f64::MAX);
        let (re, im) = FloatVal::complex_div((max, max), (max, max), fmt);
        assert_eq!((re, im), (v(1.0), FloatVal::ZERO));
        let tiny = v(f64::MIN_POSITIVE * 4.0);
        let (re, im) = FloatVal::complex_div((tiny, tiny), (tiny, tiny), fmt);
        assert_eq!((re, im), (v(1.0), FloatVal::ZERO));
        // (1 + 2i) / (3 + 4i) = 0.44 + 0.08i, as libgcc computes it.
        let (re, im) = FloatVal::complex_div((v(1.0), v(2.0)), (v(3.0), v(4.0)), fmt);
        assert_eq!((re.to_f64(), im.to_f64()), (0.44, 0.08));
    }

    /// `_Float16 _Complex` `*` and `/` fold as gcc computes them at run time
    /// on x86-64 and aarch64: widened to `float`, through `__mulsc3` and
    /// `__divsc3`, and narrowed. Each row is four operand halves and the four
    /// result halves gcc 13's program printed, identical on both targets.
    ///
    /// The fourth row tells the two rules apart: rounding each step to half
    /// precision, as `__mulhc3` would, gives an imaginary part of `0x59c0`.
    #[test]
    fn complex_binary16_goes_through_the_float_routines() {
        let fmt = FpFormat::Binary16;
        let rows: [[u128; 8]; 6] = [
            [
                0xbd80, 0x392f, 0xd42f, 0x1c78, 0x55c1, 0xd16c, 0x2542, 0xa0f5,
            ],
            [
                0x6760, 0xcca1, 0xe42d, 0x35cb, 0xfc00, 0x7500, 0xbf11, 0x2448,
            ],
            [
                0x3628, 0x2581, 0x0ab3, 0x9e0d, 0x0abe, 0x98a6, 0xbd89, 0x5413,
            ],
            [
                0xa6f1, 0x0869, 0x5d2b, 0xeea1, 0xc807, 0x59c1, 0x8004, 0x8043,
            ],
            [
                0xf5b3, 0x043a, 0xb333, 0x5591, 0x6d21, 0xfc00, 0x394c, 0x5c18,
            ],
            [
                0x0cea, 0x1f1e, 0x05f6, 0x862c, 0x000b, 0x000a, 0xd093, 0x50d1,
            ],
        ];
        for bits in rows {
            let v = |i: usize| finite_from_bits(fmt, bits[i]);
            let (x, y) = ((v(0), v(1)), (v(2), v(3)));
            let (mre, mim) = FloatVal::complex_mul(x, y, fmt);
            let (qre, qim) = FloatVal::complex_div(x, y, fmt);
            let got = [mre, mim, qre, qim].map(|h| h.to_bits(fmt));
            assert_eq!(
                got,
                [bits[4], bits[5], bits[6], bits[7]],
                "{:x?}",
                &bits[..4]
            );
        }
        assert_eq!(
            FpFormat::Binary16.complex_routine_format(),
            ComplexRoutineFormat::Binary32
        );
    }

    /// `float` divides in `double` and rounds once more, as `__divsc3` does.
    #[test]
    fn complex_div_of_float_works_in_double() {
        let fmt = FpFormat::Binary32;
        let v = |x: f32| FloatVal::from_f64(f64::from(x));
        let (re, im) = FloatVal::complex_div((v(1.0), v(2.0)), (v(3.0), v(4.0)), fmt);
        assert_eq!((re.to_f64() as f32, im.to_f64() as f32), (0.44f32, 0.08f32));
        let (re, im) = FloatVal::complex_div((v(1.0), v(1.0)), (v(1.0), v(-1.0)), fmt);
        assert_eq!((re, im), (FloatVal::ZERO, v(1.0)));
    }

    /// `__divdc3` as x86-64's libgcc (separate) and aarch64's (fused)
    /// compute it: each row is four operand halves and the quotient each
    /// target's routine returned, in both of Smith's arms and with a ratio
    /// below the smallest normal.
    #[test]
    fn complex_div_contracts_as_each_libgcc_does() {
        let fmt = FpFormat::Binary64;
        let v = FloatVal::from_f64;
        let rows: [([f64; 4], [u64; 2], [u64; 2]); 4] = [
            (
                [1.0, 1.0, 3.0, 7.0],
                [0x3fc6_11a7_b961_1a7c, 0xbfb1_a7b9_611a_7b96],
                [0x3fc6_11a7_b961_1a7b, 0xbfb1_a7b9_611a_7b95],
            ),
            (
                [1.0, 1.0, 7.0, 3.0],
                [0x3fc6_11a7_b961_1a7c, 0x3fb1_a7b9_611a_7b96],
                [0x3fc6_11a7_b961_1a7b, 0x3fb1_a7b9_611a_7b95],
            ),
            (
                [0.1, 0.7, 3.0, f64::from_bits(1 << 14)],
                [0x3fa1_1111_1111_1111, 0x3fcd_dddd_dddd_dddd],
                [0x3fa1_1111_1111_1111, 0x3fcd_dddd_dddd_dddd],
            ),
            (
                [
                    f64::from_bits(24),
                    0.3,
                    f64::powi(2.0, -1000),
                    f64::from_bits(1 << 14),
                ],
                [0x7a93_3333_3333_3333, 0x7e53_3333_3333_3333],
                [0x7a93_3333_3333_3333, 0x7e53_3333_3333_3333],
            ),
        ];
        for ([a, b, c, d], separate, fused) in rows {
            let (x, y) = ((v(a), v(b)), (v(c), v(d)));
            for (contraction, want) in [
                (Contraction::Separate, separate),
                (Contraction::Fused, fused),
            ] {
                let (re, im) = FloatVal::complex_div_by(x, y, fmt, contraction).unwrap();
                let got = [re, im].map(|h| h.to_bits(fmt) as u64);
                assert_eq!(got, want, "{contraction:?} ({a} + {b}i) / ({c} + {d}i)");
            }
        }
    }

    /// Fused, a step whose operand is infinite or NaN rounds as the separate
    /// one does, since its product is exact; one whose fused result
    /// overflows is not computed.
    #[test]
    fn complex_div_fused_steps_outside_the_finite_range() {
        let fmt = FpFormat::Binary64;
        let v = FloatVal::from_f64;
        let inf = FloatVal::infinity(false);
        for (x, y) in [
            ((inf, v(1.0)), (v(1.0), v(1.0))),
            ((FloatVal::nan(), v(1.0)), (v(3.0), v(7.0))),
            ((v(1.0), v(1.0)), (v(3.0), inf)),
        ] {
            let fused = FloatVal::complex_div_by(x, y, fmt, Contraction::Fused).unwrap();
            let separate = FloatVal::complex_div(x, y, fmt);
            assert_eq!(
                [fused.0.key(), fused.1.key()],
                [separate.0.key(), separate.1.key()]
            );
        }
        let max = v(f64::MAX);
        let y = (v(1.0), v(0.5));
        assert_eq!(
            FloatVal::complex_div_by((max, max), y, fmt, Contraction::Fused),
            None
        );
    }

    // Square root

    /// A xorshift, so a sample is fixed without pulling in a dependency.
    fn xorshift(seed: u64) -> impl FnMut() -> u64 {
        let mut state = seed;
        move || {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            state
        }
    }

    /// Every `double` and `float` root agrees with the hardware's, which
    /// IEEE 754 requires to be correctly rounded, over a wide random sample
    /// that includes the subnormals.
    #[test]
    fn sqrt_agrees_with_hardware() {
        let mut next = xorshift(0x5DEECE66D);
        for _ in 0..50000 {
            let a = f64::from_bits(next() >> 1);
            if a.is_finite() {
                let got = FloatVal::from_f64(a).sqrt(FpFormat::Binary64).unwrap();
                assert_eq!(got.to_f64().to_bits(), a.sqrt().to_bits(), "sqrt({a:e})");
            }
            let f = f32::from_bits((next() >> 33) as u32);
            if f.is_finite() {
                let got = FloatVal::from_f64(f as f64)
                    .sqrt(FpFormat::Binary32)
                    .unwrap();
                assert_eq!(
                    got.to_bits(FpFormat::Binary32) as u32,
                    f.sqrt().to_bits(),
                    "sqrtf({f:e})"
                );
            }
        }
    }

    /// The 80-bit root agrees with the x87 unit's `fsqrt`, at the extended
    /// precision a Linux process runs it in.
    #[cfg(all(target_arch = "x86_64", target_os = "linux"))]
    #[test]
    fn sqrt_agrees_with_x87() {
        fn fsqrt(x: [u8; 16]) -> [u8; 16] {
            let mut out = [0u8; 16];
            // SAFETY: loads ten bytes from `x` and stores ten to `out`, both
            // sixteen bytes long; the x87 stack is left as it was found.
            unsafe {
                std::arch::asm!(
                    "fld tbyte ptr [{x}]",
                    "fsqrt",
                    "fstp tbyte ptr [{out}]",
                    x = in(reg) x.as_ptr(),
                    out = in(reg) out.as_mut_ptr(),
                    out("st(0)") _,
                    options(nostack),
                );
            }
            out
        }
        let mut next = xorshift(0x1234_5678_9ABC_DEF1);
        for _ in 0..20000 {
            // A normal value: the integer bit set, and an exponent kept
            // clear of the extremes so the sample is spread across scales.
            let sig = next() | 1 << 63;
            let exp = 16383 - 2000 + (next() % 4000) as u128;
            let bits = exp << 64 | sig as u128;
            let v = finite_from_bits(FpFormat::X87Extended, bits);
            let got = v.sqrt(FpFormat::X87Extended).unwrap().to_x87_bytes();
            let want = fsqrt(v.to_x87_bytes());
            assert_eq!(got[..10], want[..10], "sqrtl of {bits:#x}");
        }
    }

    /// A binary128 root: exact for a perfect square, and otherwise the
    /// nearest -- `(2m - 1)^2 < 4x < (2m + 1)^2` for the root's significand
    /// `m`, which says no other representable value is nearer. There is no
    /// hardware to ask.
    #[test]
    fn sqrt_rounds_binary128_to_nearest() {
        let fmt = FpFormat::Binary128;
        let mut next = xorshift(0xC0FF_EE00_DEAD_BEEF);
        for _ in 0..2000 {
            let a = next() >> 8 | 1;
            let square = FloatVal::from_parts(false, a as u128 * a as u128, 0);
            assert_eq!(
                square.sqrt(fmt),
                Some(FloatVal::from_parts(false, a as u128, 0))
            );

            // x in [1, 4) is mx * 2^-112, with mx below 2^114 and at most 113
            // significant bits; its root in [1, 2) is m * 2^-112, so 4x
            // against (2m +- 1)^2 is mx * 2^114 against (2m +- 1)^2, all in
            // integers.
            let frac = ((next() as u128) << 64 | next() as u128) >> 16;
            let mx = (frac | 1 << 112) << (next() & 1);
            let x = FloatVal::from_parts(false, mx, -112);
            let r = x.sqrt(fmt).unwrap();
            let bits = r.to_bits(fmt);
            assert_eq!(bits >> 112, 0x3fff, "the root of [1, 4) is in [1, 2)");
            let m = bits & ((1 << 112) - 1) | 1 << 112;
            let four_x = U256 {
                hi: mx >> 14,
                lo: mx << 114,
            };
            assert!(
                U256::mul(2 * m - 1, 2 * m - 1) < four_x,
                "{mx:#x}: root too big"
            );
            assert!(
                four_x < U256::mul(2 * m + 1, 2 * m + 1),
                "{mx:#x}: root too small"
            );
        }
    }

    /// `floor`, `ceil`, `trunc` and `round` agree with the host's, for
    /// `double` and `float`, over a random sample weighted to the exponents
    /// where there is a fraction to round away.
    #[test]
    fn round_to_integral_agrees_with_the_host() {
        use IntegralRounding::*;
        let mut next = xorshift(0x0123_4567_89AB_CDEF);
        let d = |f: f64| FloatVal::from_f64(f);
        for _ in 0..50000 {
            let bits = next();
            // An exponent within 60 of 2^0 most of the time.
            let exp = if bits & 3 == 0 {
                bits >> 52 & 0x7ff
            } else {
                1023 - 60 + (bits >> 52) % 120
            };
            let a = f64::from_bits(bits & 0x800f_ffff_ffff_ffff | exp << 52);
            if !a.is_finite() {
                continue;
            }
            let fmt = FpFormat::Binary64;
            for (how, want) in [
                (Floor, a.floor()),
                (Ceil, a.ceil()),
                (Trunc, a.trunc()),
                (Round, a.round()),
            ] {
                let got = d(a).round_to_integral(how, fmt).unwrap();
                assert_eq!(got.to_f64().to_bits(), want.to_bits(), "{how:?}({a:e})");
            }
            let f = a as f32;
            if f.is_finite() {
                let fmt = FpFormat::Binary32;
                for (how, want) in [
                    (Floor, f.floor()),
                    (Ceil, f.ceil()),
                    (Trunc, f.trunc()),
                    (Round, f.round()),
                ] {
                    let got = d(f as f64).round_to_integral(how, fmt).unwrap();
                    assert_eq!(got.to_bits(fmt) as u32, want.to_bits(), "{how:?}f({f:e})");
                }
            }
        }
    }

    /// The edges: signed zeros out of a fraction of either sign, halves,
    /// the values at and past the last fraction bit, and the operands that
    /// have no fold -- a signalling NaN, and a `rint` or `nearbyint` that
    /// would depend on the rounding direction.
    #[test]
    fn round_to_integral_edges() {
        use IntegralRounding::*;
        let v = FloatVal::from_f64;
        let bits = |r: Option<FloatVal>| r.map(|r| r.to_f64().to_bits());
        let fmt = FpFormat::Binary64;
        assert_eq!(
            bits(v(-0.5).round_to_integral(Ceil, fmt)),
            Some(0x8000_0000_0000_0000)
        );
        assert_eq!(
            bits(v(-0.5).round_to_integral(Trunc, fmt)),
            Some(0x8000_0000_0000_0000)
        );
        assert_eq!(
            bits(v(-0.4).round_to_integral(Round, fmt)),
            Some(0x8000_0000_0000_0000)
        );
        assert_eq!(bits(v(0.5).round_to_integral(Floor, fmt)), Some(0));
        assert_eq!(v(-2.5).round_to_integral(Round, fmt), Some(v(-3.0)));
        assert_eq!(v(0.5).round_to_integral(Round, fmt), Some(v(1.0)));
        let tiny = v(f64::from_bits(1));
        assert_eq!(tiny.round_to_integral(Ceil, fmt), Some(v(1.0)));
        assert_eq!(tiny.negated().round_to_integral(Floor, fmt), Some(v(-1.0)));
        assert_eq!(bits(tiny.round_to_integral(Round, fmt)), Some(0));
        let below = v(4503599627370495.5); // 2^52 - 0.5
        assert_eq!(
            below.round_to_integral(Ceil, fmt),
            Some(v(4503599627370496.0))
        );
        assert_eq!(
            below.round_to_integral(Round, fmt),
            Some(v(4503599627370496.0))
        );
        for how in [Floor, Ceil, Trunc, Round, Rint, NearbyInt] {
            assert_eq!(v(3.0).round_to_integral(how, fmt), Some(v(3.0)), "{how:?}");
            let inf = v(f64::NEG_INFINITY);
            assert_eq!(inf.round_to_integral(how, fmt), Some(inf), "{how:?}");
            let qnan = FloatVal::nan_with_payload(fmt, 0x12, NanKind::Quiet).negated();
            assert_eq!(qnan.round_to_integral(how, fmt), Some(qnan), "{how:?}");
            let snan = FloatVal::nan_with_payload(fmt, 0x12, NanKind::Signalling);
            assert_eq!(snan.round_to_integral(how, fmt), None, "{how:?}");
        }
        assert_eq!(v(2.5).round_to_integral(Rint, fmt), None);
        assert_eq!(v(-0.25).round_to_integral(NearbyInt, fmt), None);
        // Rounded to the format first: 2.5 + 2^-30 is 2.5 as a float.
        let f = v(2.5 + 2f64.powi(-30));
        assert_eq!(f.round_to_integral(Round, FpFormat::Binary32), Some(v(3.0)));
        assert_eq!(
            f.round_to_integral(Trunc, FpFormat::X87Extended),
            Some(v(2.0))
        );
    }

    /// Every `double` and `float` fused multiply-add agrees with the host's
    /// `mul_add`, which is correctly rounded, over a random sample -- a third
    /// of it with the addend built to cancel most of the product, which is
    /// where a multiply and an add each rounded come out different.
    #[test]
    fn fma_agrees_with_the_host() {
        let mut next = xorshift(0xF00D_FACE_1234_5678);
        let d = FloatVal::from_f64;
        // An exponent within 30 of 2^0, so products neither overflow nor
        // vanish.
        let mut near_one = |sign: u64| {
            let bits = next();
            let exp = 1023 - 30 + (bits >> 52) % 60;
            f64::from_bits(sign << 63 | exp << 52 | bits & ((1 << 52) - 1))
        };
        for i in 0..30000 {
            let (a, b) = (near_one(i & 1), near_one(i >> 1 & 1));
            let c = if i % 3 == 0 {
                // -(a * b), rounded, nudged by a few ulps.
                -(a * b) * (1.0 + (i % 7) as f64 * f64::EPSILON)
            } else {
                near_one(i >> 2 & 1)
            };
            let got = d(a).fma(d(b), d(c), FpFormat::Binary64);
            assert_eq!(
                got.map(|v| v.to_f64().to_bits()),
                Some(a.mul_add(b, c).to_bits()),
                "fma({a:e}, {b:e}, {c:e})"
            );
            let (fa, fb, fc) = (a as f32, b as f32, c as f32);
            let got = d(fa as f64).fma(d(fb as f64), d(fc as f64), FpFormat::Binary32);
            assert_eq!(
                got.map(|v| v.to_bits(FpFormat::Binary32) as u32),
                Some(fa.mul_add(fb, fc).to_bits()),
                "fmaf({fa:e}, {fb:e}, {fc:e})"
            );
        }
    }

    /// The `fma` edges: signed zeros out of a zero product, an exact
    /// cancellation, a subnormal result, and the operands and results that
    /// are not folded.
    #[test]
    fn fma_edges() {
        let v = FloatVal::from_f64;
        let bits = |r: Option<FloatVal>| r.map(|r| r.to_f64().to_bits());
        let f = FpFormat::Binary64;
        assert_eq!(bits(v(-0.0).fma(v(1.0), v(0.0), f)), Some(0));
        assert_eq!(bits(v(-0.0).fma(v(1.0), v(-0.0), f)), Some(1 << 63));
        assert_eq!(bits(v(2.0).fma(v(3.0), v(-6.0), f)), Some(0), "+0, not -0");
        assert_eq!(v(0.0).fma(v(5.0), v(-2.5), f), Some(v(-2.5)));
        // 2^-1022 * 0.5 + 2^-1074: a subnormal, exactly.
        let min_normal = v(f64::MIN_POSITIVE);
        let got = min_normal.fma(v(0.5), v(f64::from_bits(1)), f);
        assert_eq!(bits(got), Some(0x0008_0000_0000_0001));
        assert_eq!(v(f64::MAX).fma(v(2.0), v(0.0), f), None, "overflow");
        assert_eq!(v(f64::INFINITY).fma(v(1.0), v(1.0), f), None);
        assert_eq!(v(1.0).fma(v(1.0), v(f64::NAN), f), None);
        // binary128 and x87: the product's low half survives the addend.
        for fmt in [FpFormat::X87Extended, FpFormat::Binary128] {
            let ulp = v(2f64.powi(1 - fmt.precision() as i32));
            let one = v(1.0);
            let got = one.add(ulp, fmt).fma(one.sub(ulp, fmt), v(-1.0), fmt);
            assert_eq!(got, Some(ulp.mul(ulp, fmt).negated()), "{fmt:?}");
        }
    }

    /// `fmin` and `fmax`: a quiet NaN gives the other operand, two NaNs the
    /// first, the zeros gcc's and `fminnm`'s -0 and +0, and a signalling NaN
    /// no fold.
    #[test]
    fn fmin_and_fmax() {
        let v = FloatVal::from_f64;
        let f = FpFormat::Binary64;
        let bits = |r: Option<FloatVal>| r.map(|r| r.to_f64().to_bits());
        assert_eq!(v(1.0).fmin(v(2.0), f), Some(v(1.0)));
        assert_eq!(v(2.0).fmin(v(1.0), f), Some(v(1.0)));
        assert_eq!(v(1.0).fmax(v(-2.0), f), Some(v(1.0)));
        assert_eq!(v(f64::NEG_INFINITY).fmax(v(-2.0), f), Some(v(-2.0)));
        let qnan = FloatVal::nan_with_payload(f, 0x12, NanKind::Quiet);
        let other = FloatVal::nan_with_payload(f, 0x34, NanKind::Quiet);
        assert_eq!(qnan.fmin(v(3.0), f), Some(v(3.0)));
        assert_eq!(v(3.0).fmax(qnan, f), Some(v(3.0)));
        assert_eq!(qnan.fmin(other, f), Some(qnan));
        for (a, b) in [(0.0, -0.0), (-0.0, 0.0)] {
            assert_eq!(bits(v(a).fmin(v(b), f)), Some(1 << 63), "fmin({a}, {b})");
            assert_eq!(bits(v(a).fmax(v(b), f)), Some(0), "fmax({a}, {b})");
        }
        let snan = FloatVal::nan_with_payload(f, 0x12, NanKind::Signalling);
        assert_eq!(snan.fmin(v(1.0), f), None);
        assert_eq!(v(1.0).fmax(snan, f), None);
    }

    /// The operands with no root to compute: zeros and infinity are their
    /// own, a quiet NaN passes through unchanged, and a negative number or a
    /// signalling NaN has no answer that is the value's alone.
    #[test]
    fn sqrt_of_special_operands() {
        let v = FloatVal::from_f64;
        for fmt in [
            FpFormat::Binary32,
            FpFormat::Binary64,
            FpFormat::X87Extended,
            FpFormat::Binary128,
        ] {
            assert_eq!(v(0.0).sqrt(fmt), Some(v(0.0)), "{fmt:?}");
            assert_eq!(v(-0.0).sqrt(fmt), Some(v(-0.0)), "{fmt:?}");
            assert_eq!(v(f64::INFINITY).sqrt(fmt), Some(v(f64::INFINITY)));
            assert_eq!(v(4.0).sqrt(fmt), Some(v(2.0)));
            assert_eq!(v(0.25).sqrt(fmt), Some(v(0.5)));
            assert_eq!(v(-4.0).sqrt(fmt), None, "a domain error");
            assert_eq!(v(f64::NEG_INFINITY).sqrt(fmt), None, "a domain error");
            let qnan = FloatVal::nan_with_payload(fmt, 0x15, NanKind::Quiet).negated();
            assert_eq!(qnan.sqrt(fmt), Some(qnan), "payload and sign kept");
            let snan = FloatVal::nan_with_payload(fmt, 0x15, NanKind::Signalling);
            assert_eq!(snan.sqrt(fmt), None, "raises invalid");
        }
    }
}
