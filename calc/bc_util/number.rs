//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use bigdecimal::{num_bigint::BigInt, BigDecimal, Num, One, Pow, Signed, ToPrimitive, Zero};

/// Upper bound on the number of decimal digits a single operation may build.
///
/// `scale` may legally be as large as `BC_SCALE_MAX`, and an operation at that
/// scale would need a multi-gigabyte integer. Fail fast instead, well above any
/// scale a real program uses.
const MAX_WORKING_DIGITS: u64 = 1_000_000;

/// `10^n`, or `None` if that many digits exceeds what we are willing to build.
fn ten_pow(n: u64) -> Option<BigInt> {
    if n > MAX_WORKING_DIGITS {
        return None;
    }
    Some(BigInt::from(10u8).pow(n as u32))
}

/// Floor of the square root of a non-negative integer, by Newton's method.
///
/// POSIX requires `sqrt` to be truncated, and the floor of the integer square
/// root of the scaled radicand is exactly the truncated result.
fn integer_sqrt(n: &BigInt) -> BigInt {
    if n.is_zero() {
        return BigInt::zero();
    }
    // Any starting point at or above the true root converges down to the floor.
    let mut x = BigInt::one() << (n.bits() as usize).div_ceil(2);
    loop {
        let next = (&x + n / &x) >> 1;
        if next >= x {
            return x;
        }
        x = next;
    }
}

/// `a / b`, truncated toward zero, as a value with exactly `scale` fractional
/// digits.
///
/// POSIX (XCU bc, "when an exact result is not achieved ... the result shall be
/// truncated") requires the quotient's digits to be real digits of the true
/// quotient. Computing it through `BigDecimal`'s `Div` would instead apply that
/// crate's 100-significant-digit default context and round, so every digit past
/// the hundredth would be invented. Shifting the numerator and dividing the
/// underlying integers is exact at any scale.
fn divide_exact(a: &BigDecimal, b: &BigDecimal, scale: u64) -> Result<BigDecimal, &'static str> {
    // The quotient has `scale` digits after the point, whatever the
    // operands' own scales.
    if scale > MAX_WORKING_DIGITS {
        return Err("number too large");
    }
    let (a_int, a_scale) = a.as_bigint_and_exponent();
    let (b_int, b_scale) = b.as_bigint_and_exponent();
    // a/b truncated to `scale` digits is
    //     trunc(a_int * 10^(b_scale - a_scale + scale) / b_int) * 10^-scale
    let shift = b_scale - a_scale + scale as i64;
    let (numerator, denominator) = if shift >= 0 {
        (
            a_int * ten_pow(shift as u64).ok_or("number too large")?,
            b_int,
        )
    } else {
        (
            a_int,
            b_int * ten_pow(shift.unsigned_abs()).ok_or("number too large")?,
        )
    };
    // Integer division truncates toward zero, which is the rule bc wants.
    Ok(BigDecimal::new(numerator / denominator, scale as i64))
}

/// Digit strings at most this long are read directly.
const DIRECT_READ_DIGITS: usize = 1000;

/// The value of a string of digits valid in `base`.
///
/// num-bigint reads a decimal string in time quadratic in its length, which
/// made a literal of a million digits take seconds. Reading the two halves
/// and joining them as `high * base^len(low) + low` costs a few large
/// multiplications per level instead.
fn read_digits(digits: &str, base: u32) -> BigInt {
    if digits.len() <= DIRECT_READ_DIGITS {
        return BigInt::from_str_radix(digits, base).expect("digits checked against the base");
    }
    // Digits are ASCII, so any byte index is a character boundary.
    let (high, low) = digits.split_at(digits.len() / 2);
    read_digits(high, base) * BigInt::from(base).pow(low.len() as u32) + read_digits(low, base)
}

/// Converts a character to a number
/// # Panics
/// panics if the character is not a valid hexadecimal digit
fn to_digit(c: u8) -> u8 {
    match c {
        b'0'..=b'9' => c - b'0',
        b'A'..=b'F' => c - b'A' + 10,
        _ => panic!("number has invalid digit {}", c as char),
    }
}

/// The number of decimal columns one digit of `base` occupies, for the
/// space-separated form POSIX defines for bases above 16.
///
/// The width comes from the largest digit, `base - 1`, not from the base:
/// POSIX says "for bases from 17 to 100, bc shall write two-digit decimal
/// numbers; for bases from 101 to 1 000, three-digit", and base 100's largest
/// digit is 99. Taking the base's own width makes every exact power of ten one
/// column too wide.
fn digit_width(base: u64) -> usize {
    (base - 1).ilog10() as usize + 1
}

/// Renders one digit zero-padded to `width` columns.
fn pad_digit(d: u64, width: usize) -> String {
    format!("{:0width$}", d, width = width)
}

/// How many digits of `base` to print for a value carrying `scale` decimal
/// fractional digits: the smallest k with `base^k >= 10^scale`.
///
/// POSIX leaves the count unspecified beyond "the number of digits output
/// shall be s if obase is 10, less than or equal to s if obase is greater than
/// 10, or greater than or equal to s if obase is less than 10"; this is the
/// rule GNU bc uses.
fn fractional_digits_for(base: u64, scale: u64) -> u64 {
    if scale == 0 {
        return 0;
    }
    if base == 10 {
        return scale;
    }
    let target = match ten_pow(scale) {
        Some(target) => target,
        None => return scale,
    };
    let base_big = BigInt::from(base);
    // Start from a floating-point estimate, then correct it exactly.
    let mut k = ((scale as f64) / (base as f64).log10()).ceil().max(1.0) as u64;
    while Pow::pow(&base_big, k as u32) < target {
        k += 1;
    }
    while k > 1 && Pow::pow(&base_big, k as u32 - 1) >= target {
        k -= 1;
    }
    k
}

/// Appends `value` (non-negative) in `base`, most significant digit first.
///
/// Above the cutoff the loop below divides once per digit, which is quadratic
/// in the number of digits. `push_fractional_digits` has the same cutoff and
/// the same loop. Measured on this tree, against GNU bc:
///
/// | `2^n`, n =         | 50 000 | 100 000 | 200 000 |
/// |--------------------|--------|---------|---------|
/// | `obase=16` (below) | 0.00s  | 0.00s   | 0.00s   |
/// | `obase=17` (loop)  | 0.03s  | 0.13s   | 0.55s   |
/// | GNU bc, `obase=17` | 2.98s  | 12.02s  | 48.56s  |
///
/// So the quadratic term is real -- time quadruples per doubling -- and we are
/// still 20-90x faster than the reference implementation at every size it can
/// be measured at. Left alone deliberately.
///
/// If it is ever revisited: `num_bigint`'s `to_str_radix` accepts bases 2..=36
/// and we stop at 16, so raising the cutoff to 36 is nearly free (its output is
/// `0-9a-z`, which has to be mapped back to the space-separated decimal groups
/// POSIX wants above base 16). Beyond 36 there is no radix conversion to
/// borrow, and a genuine fix needs divide-and-conquer by `base^(2^i)`.
fn push_integer_digits(result: &mut String, value: &BigInt, base: u64) {
    if base <= 16 {
        // to_str_radix is far cheaper than extracting one digit at a time.
        result.push_str(&value.to_str_radix(base as u32).to_uppercase());
        return;
    }
    let width = digit_width(base);
    let base_big = BigInt::from(base);
    let mut remaining = value.clone();
    let mut stack = Vec::new();
    while !remaining.is_zero() {
        let digit = (&remaining % &base_big).to_u64().unwrap_or(0);
        remaining /= &base_big;
        stack.push(digit);
    }
    for digit in stack.iter().rev() {
        result.push(' ');
        result.push_str(&pad_digit(*digit, width));
    }
}

/// Appends exactly `count` fractional digits of `value` in `base`.
fn push_fractional_digits(result: &mut String, value: &BigInt, base: u64, count: u64) {
    let count = count as usize;
    if base <= 16 {
        let digits = value.to_str_radix(base as u32).to_uppercase();
        for _ in digits.len()..count {
            result.push('0');
        }
        result.push_str(&digits);
        return;
    }
    let width = digit_width(base);
    let base_big = BigInt::from(base);
    let mut remaining = value.clone();
    let mut stack = Vec::with_capacity(count);
    for _ in 0..count {
        let digit = (&remaining % &base_big).to_u64().unwrap_or(0);
        remaining /= &base_big;
        stack.push(digit);
    }
    for (i, digit) in stack.iter().rev().enumerate() {
        if i > 0 {
            result.push(' ');
        }
        result.push_str(&pad_digit(*digit, width));
    }
}

pub type NumericResult = Result<Number, &'static str>;

#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Number(BigDecimal);

impl Number {
    fn rescale(self, new_scale: u64) -> Self {
        Self(self.0.with_scale(new_scale as i64))
    }

    pub fn zero() -> Self {
        Self(BigDecimal::zero())
    }

    pub fn as_u64(&self) -> Option<u64> {
        self.0.to_u64()
    }

    /// Parse a number from a string in the given base.
    /// # Errors
    /// A digit not valid in `base`, or more integer or fractional digits
    /// than the working limit.
    /// # Panics
    /// panics if:
    /// - the string is empty
    /// - `base` is not in the range 2..=16.
    /// - `s` does not contain a valid number
    ///
    /// all the above should have been already checked by the parser
    pub fn parse(s: &str, base: u64) -> NumericResult {
        assert!(!s.is_empty(), "parsed number has no digits");
        assert!((2..=16).contains(&base), "base must be in the range 2..=16");

        for c in s.bytes() {
            if c != b'.' && to_digit(c) >= base as u8 {
                return Err("invalid digit for the current ibase");
            }
        }

        let (int, fraction) = s.split_once('.').unwrap_or((s, ""));
        // Both parts are held to the working limit, like every value built.
        if int.len() as u64 > MAX_WORKING_DIGITS || fraction.len() as u64 > MAX_WORKING_DIGITS {
            return Err("number too large");
        }
        let base = base as u32;
        let integer_part = if int.is_empty() {
            BigInt::zero()
        } else {
            read_digits(int, base)
        };
        // In regards to the scale of parsed values, the standard doesn't specify.
        // The following matches the GNU implementation, which sets the scale
        // to the number of fractional digits in the string, regardless of the base
        let scale = fraction.len() as u32;
        let ten_to_scale = BigInt::from(10u8).pow(scale);
        let fractional_part = if fraction.is_empty() {
            BigInt::zero()
        } else if base == 10 {
            read_digits(fraction, base)
        } else {
            // The fraction is digits / base^scale, truncated to `scale`
            // decimal places: floor(digits * 10^scale / base^scale).
            read_digits(fraction, base) * &ten_to_scale / BigInt::from(base).pow(scale)
        };
        Ok(Self(BigDecimal::new(
            integer_part * ten_to_scale + fractional_part,
            i64::from(scale),
        )))
    }

    /// Convert the number to a string in the given base.
    ///
    /// Digits are produced in bulk rather than one at a time: extracting them
    /// by repeatedly multiplying the whole value costs a full-precision
    /// operation per digit, which is quadratic in the digit count and made
    /// printing a high-scale result far slower than computing it.
    pub fn to_string(&self, base: u64) -> String {
        if self.0.is_zero() {
            return "0".to_string();
        }

        let scale = self.scale();
        let magnitude = self.0.abs().with_scale(scale as i64);
        let (unscaled, _) = magnitude.into_bigint_and_exponent();
        // Every operation that sets a scale holds it to MAX_WORKING_DIGITS,
        // so this is a bounded cost, and is computed even past it rather than
        // printing a value that is wrong.
        let ten_to_scale = BigInt::from(10u8).pow(scale as u32);
        let integer_part = &unscaled / &ten_to_scale;
        let fraction_numerator = &unscaled - &integer_part * &ten_to_scale;

        let mut result = String::new();
        if self.0.is_negative() {
            result.push('-');
        }
        if integer_part.is_zero() {
            result.push('0');
        } else {
            push_integer_digits(&mut result, &integer_part, base);
        }

        if scale == 0 {
            return result;
        }
        result.push('.');
        let count = fractional_digits_for(base, scale);
        // floor(fraction * base^count), the first `count` digits of the
        // fraction in `base`.
        let digits = fraction_numerator * BigInt::from(base).pow(count as u32) / &ten_to_scale;
        push_fractional_digits(&mut result, &digits, base, count);
        result
    }

    /// The number of decimal digits in the number.
    pub fn scale(&self) -> u64 {
        self.0.fractional_digit_count().max(0) as u64
    }

    /// POSIX: "the total number of significant decimal digits". A value below
    /// one still has the digits its scale gives it, so `length(.001)` is 3;
    /// counting the digits of the unscaled mantissa alone reported 1.
    pub fn length(&self) -> u64 {
        self.0.digits().max(self.scale()).max(1)
    }

    pub fn negate(self) -> Self {
        Self(-self.0)
    }

    // POSIX: the scale of a sum or difference is max(scale(a), scale(b)).
    // bigdecimal's `Sub` short-circuits when either operand is zero and returns
    // the other one unchanged, dropping the zero's scale, so the scale is set
    // explicitly here rather than inherited. `Add` does not currently
    // short-circuit, but it is pinned the same way so it cannot start to.

    pub fn add(self, other: &Number) -> Number {
        let scale = self.scale().max(other.scale());
        Self((self.0 + &other.0).with_scale(scale as i64))
    }

    pub fn sub(self, other: &Number) -> Number {
        let scale = self.scale().max(other.scale());
        Self((self.0 - &other.0).with_scale(scale as i64))
    }

    pub fn mul(self, other: &Number, scale: u64) -> NumericResult {
        let a = self.scale();
        let b = other.scale();
        let required_scale = u64::min(a + b, scale.max(a).max(b));
        // Each multiplication can add the operands' scales, so without this
        // a few of them build a value no other operation could.
        if required_scale > MAX_WORKING_DIGITS {
            return Err("number too large");
        }
        let result = self.0 * &other.0;
        Ok(Self(result).rescale(required_scale))
    }

    pub fn div(self, other: &Number, scale: u64) -> NumericResult {
        if other.is_zero() {
            return Err("division by zero");
        }
        Ok(Self(divide_exact(&self.0, &other.0, scale)?))
    }

    pub fn pow(self, other: &Number, scale: u64) -> NumericResult {
        if !other.0.is_integer() {
            return Err("exponent has to be an integer");
        }

        let a = self.scale();
        let exponent = other.0.to_i64().ok_or("exponent is too large")?;
        let b = exponent.unsigned_abs();
        // Per POSIX: if b >= 0 the scale is min(a*b, max(scale, a)); if b < 0
        // it is the scale register. b == 0 takes the first branch, giving a
        // scale of 0 (so e.g. `scale=5; 2.5^0` is "1", not "1.00000").
        let result_scale = if exponent >= 0 {
            u64::min(a.saturating_mul(b), u64::max(scale, a))
        } else {
            scale
        };
        if exponent < 0 && self.is_zero() {
            return Err("division by zero");
        }
        // The result has roughly `b * digits(self)` digits. Refuse the ones
        // that could not be held in memory rather than looping toward them.
        if b.saturating_mul(self.0.digits()) > MAX_WORKING_DIGITS {
            return Err("exponent is too large");
        }

        // Exponentiation by squaring: the operand count is logarithmic in b,
        // where repeated multiplication was linear.
        let mut result = BigDecimal::one();
        let mut base = self.0.clone();
        let mut remaining = b;
        while remaining > 0 {
            if remaining & 1 == 1 {
                result *= &base;
            }
            remaining >>= 1;
            if remaining > 0 {
                base = &base * &base;
            }
        }
        if exponent < 0 {
            result = divide_exact(&BigDecimal::one(), &result, result_scale)?;
        }
        Ok(Self(result).rescale(result_scale))
    }

    pub fn modulus(self, other: &Number, scale: u64) -> NumericResult {
        // POSIX defines a%b as a - (a/b)*b, with a/b evaluated at `scale`.
        let a_over_b = self.clone().div(other, scale)?;
        let scale = u64::max(scale.saturating_add(other.scale()), self.scale());
        Ok(self.sub(&a_over_b.mul(other, scale)?))
    }

    pub fn sqrt(self, min_scale: u64) -> NumericResult {
        if self.0.is_negative() {
            return Err("square root of negative number");
        }
        // POSIX: the scale of the result is the larger of the expression's
        // scale and the scale register.
        let scale = self.scale().max(min_scale);
        let (radicand, radicand_scale) = self.0.into_bigint_and_exponent();
        // floor(sqrt(x) * 10^scale) == floor(sqrt(x_int * 10^(2*scale - x_scale)))
        // and floor(sqrt(floor(n))) == floor(sqrt(n)), so truncating the shift
        // when it is negative is exact too.
        let shift = 2 * scale as i64 - radicand_scale;
        let n = if shift >= 0 {
            radicand * ten_pow(shift as u64).ok_or("number too large")?
        } else {
            radicand / ten_pow(shift.unsigned_abs()).ok_or("number too large")?
        };
        Ok(Self(BigDecimal::new(integer_sqrt(&n), scale as i64)))
    }

    pub fn inc(&mut self) {
        self.0 += 1;
    }

    pub fn dec(&mut self) {
        self.0 -= 1;
    }

    pub fn is_zero(&self) -> bool {
        self.0.is_zero()
    }

    pub fn is_negative(&self) -> bool {
        self.0.is_negative()
    }
}

impl From<u64> for Number {
    fn from(n: u64) -> Self {
        Self(BigDecimal::from(n))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_base_10() {
        assert_eq!(&Number::from(1).to_string(10), "1");
        assert_eq!(&Number::parse("123", 10).unwrap().to_string(10), "123");
        assert_eq!(
            &Number::parse("123.456", 10).unwrap().to_string(10),
            "123.456"
        );
        assert_eq!(&Number::parse(".1234", 10).unwrap().to_string(10), "0.1234");
    }

    #[test]
    fn test_parse_base_2() {
        assert_eq!(&Number::parse("1", 2).unwrap().to_string(10), "1");
        assert_eq!(&Number::parse("1101", 2).unwrap().to_string(10), "13");
        assert_eq!(
            &Number::parse("1101.101", 2).unwrap().to_string(10),
            "13.625"
        );
        assert_eq!(&Number::parse(".1101", 2).unwrap().to_string(10), "0.8125");
    }

    #[test]
    fn test_parse_base_12() {
        assert_eq!(&Number::parse("1", 12).unwrap().to_string(10), "1");
        assert_eq!(&Number::parse("123", 12).unwrap().to_string(10), "171");
        assert_eq!(
            &Number::parse("1B3.BA6", 12).unwrap().to_string(10),
            "279.989"
        );
        assert_eq!(&Number::parse(".1B3A", 12).unwrap().to_string(10), "0.1619");
    }

    #[test]
    fn test_output_base_10() {
        assert_eq!(Number::from(1).to_string(10), "1");
        assert_eq!(Number::from(123).to_string(10), "123");
        assert_eq!(Number::from(123).negate().to_string(10), "-123");
        assert_eq!(
            Number::parse("123.456", 10).unwrap().to_string(10),
            "123.456"
        );
        assert_eq!(
            Number::parse("123.456", 10).unwrap().negate().to_string(10),
            "-123.456"
        );
        assert_eq!(Number::parse(".1234", 10).unwrap().to_string(10), "0.1234");
        assert_eq!(
            Number::parse(".1234", 10).unwrap().negate().to_string(10),
            "-0.1234"
        );
        assert_eq!(
            Number::parse("2.000000", 10).unwrap().to_string(10),
            "2.000000"
        );
        assert_eq!(
            Number::parse("0.000000000000000000000000001", 10)
                .unwrap()
                .to_string(10),
            "0.000000000000000000000000001"
        );
        assert_eq!(
            Number::parse("100000000000000000000000000000000000000000000000", 10)
                .unwrap()
                .to_string(10),
            "100000000000000000000000000000000000000000000000",
        );
    }

    #[test]
    fn test_output_base_2() {
        assert_eq!(Number::from(1).to_string(2), "1");
        assert_eq!(Number::from(13).to_string(2), "1101");
        assert_eq!(Number::from(13).negate().to_string(2), "-1101");
        assert_eq!(
            Number::parse("13.625", 10).unwrap().to_string(2),
            "1101.1010000000"
        );
        assert_eq!(
            Number::parse("13.625", 10).unwrap().negate().to_string(2),
            "-1101.1010000000"
        );
        assert_eq!(
            Number::parse("0.8125", 10).unwrap().to_string(2),
            "0.11010000000000"
        );
    }

    #[test]
    fn test_output_base_12() {
        assert_eq!(Number::from(1).to_string(12), "1");
        assert_eq!(Number::from(123).negate().to_string(12), "-A3");
        assert_eq!(
            Number::parse("123.321", 10).unwrap().to_string(12),
            "A3.3A2"
        );
        assert_eq!(Number::parse("0.0891", 10).unwrap().to_string(12), "0.109B");
    }

    #[test]
    fn test_output_base_150() {
        assert_eq!(Number::from(1).to_string(150), " 001");
        assert_eq!(Number::from(1040).negate().to_string(150), "- 006 140");
        assert_eq!(
            Number::parse("230.461", 10).unwrap().to_string(150),
            " 001 080.069 022"
        );
        assert_eq!(
            Number::parse("0.673", 10).unwrap().to_string(150),
            "0.100 142"
        );
    }

    #[test]
    fn test_output_base_1029() {
        assert_eq!(Number::from(1).to_string(1029), " 0001");
        assert_eq!(Number::from(1040).negate().to_string(1029), "- 0001 0011");
        assert_eq!(
            Number::parse("193.286", 10).unwrap().to_string(1029),
            " 0193.0294"
        );
        assert_eq!(
            Number::parse("0.2964", 10).unwrap().to_string(1029),
            "0.0304 1024"
        );
    }

    #[test]
    fn test_integer_has_zero_scale() {
        assert_eq!(Number::from(10).scale(), 0);
    }

    #[test]
    fn test_trailing_zeros_increase_scale() {
        assert_eq!(Number::parse("10.000", 10).unwrap().scale(), 3);
    }

    #[test]
    fn test_pow_to_zero_has_zero_scale() {
        // x^0 is 1 with scale 0, regardless of the scale register (audit #B8).
        let r = Number::from(2).pow(&Number::from(0), 5).unwrap();
        assert_eq!(r.scale(), 0);
        assert_eq!(r.to_string(10), "1");
    }

    #[test]
    fn test_add() {
        let n = Number::parse("10.25", 10)
            .unwrap()
            .add(&Number::parse("20.750", 10).unwrap());
        assert_eq!(n.scale(), 3);
        assert_eq!(n.to_string(10), "31.000");
    }

    #[test]
    fn test_sub() {
        let n = Number::parse("10.25", 10)
            .unwrap()
            .sub(&Number::parse("20.750", 10).unwrap());
        assert_eq!(n.scale(), 3);
        assert_eq!(n.to_string(10), "-10.500");
    }

    #[test]
    fn test_mul_integers() {
        let n = Number::parse("2", 10)
            .unwrap()
            .mul(&Number::parse("5", 10).unwrap(), 10)
            .unwrap();
        assert_eq!(n.scale(), 0);
        assert_eq!(n.to_string(10), "10");
    }

    #[test]
    fn test_mul() {
        let n = Number::parse("2.25", 10)
            .unwrap()
            .mul(&Number::from(4), 10)
            .unwrap();
        assert_eq!(n.scale(), 2);
        assert_eq!(n.to_string(10), "9.00");
    }

    #[test]
    fn test_div_integers_no_remainder() {
        let n = Number::from(4)
            .div(&Number::from(2), 0)
            .expect("error dividing two positive integers");

        assert_eq!(n.scale(), 0);
        assert_eq!(n.to_string(10), "2");
    }

    #[test]
    fn test_div_with_remainder_but_zero_scale_returns_an_integer() {
        let n = Number::from(4)
            .div(&Number::from(3), 0)
            .expect("error dividing two positive integers");

        assert_eq!(n.scale(), 0);
        assert_eq!(n.to_string(10), "1");
    }

    #[test]
    fn test_div_exact() {
        let n = Number::parse("4.5", 10)
            .unwrap()
            .div(&Number::from(2), 2)
            .expect("error dividing two positive integers");

        assert_eq!(n.scale(), 2);
        assert_eq!(n.to_string(10), "2.25");
    }

    #[test]
    fn test_div_by_zero_is_error() {
        let n = Number::parse("4.5", 10).unwrap();
        let result = n.div(&Number::zero(), 2);
        assert_eq!(result, Err("division by zero"));
    }

    #[test]
    fn test_raise_integer_to_positive_integer() {
        let n = Number::parse("2", 10)
            .unwrap()
            .pow(&Number::from(3), 10)
            .expect("error raising 2 to the power of 3");
        assert_eq!(n.scale(), 0);
        assert_eq!(n.to_string(10), "8");
    }

    #[test]
    fn test_raise_integer_to_negative_integer() {
        let n = Number::parse("2", 10)
            .unwrap()
            .pow(&Number::from(3).negate(), 2)
            .expect("error raising 2 to the power of -3");

        assert_eq!(n.scale(), 2);
        assert_eq!(n.to_string(10), "0.12");
    }

    #[test]
    fn test_raise_to_zero() {
        let n = Number::from(2)
            .pow(&Number::from(0), 0)
            .expect("error raising 2 to the power of 0");

        assert_eq!(n.scale(), 0);
        assert_eq!(n.to_string(10), "1");
    }

    #[test]
    fn test_raise_negative_number() {
        let n = Number::from(2)
            .negate()
            .pow(&Number::from(2), 0)
            .expect("error raising -2 to the power of 2");
        assert_eq!(n, Number::from(4));
        let n = Number::from(2)
            .negate()
            .pow(&Number::from(3), 0)
            .expect("error raising -2 to the power of 3");
        assert_eq!(n, Number::from(8).negate());
    }

    #[test]
    fn test_raise_to_non_integer_is_error() {
        let n = Number::from(2);
        let result = n.pow(&Number::parse("3.5", 10).unwrap(), 2);
        assert_eq!(result, Err("exponent has to be an integer"));
    }

    #[test]
    fn test_raise_too_large_integer_is_error() {
        let n = Number::from(2);
        let result = n.pow(
            &Number::parse("10000000000000000000000000000", 10).unwrap(),
            2,
        );
        assert_eq!(result, Err("exponent is too large"));
    }

    #[test]
    fn test_mod_zero_is_error() {
        let n = Number::parse("4.5", 10).unwrap();
        let result = n.modulus(&Number::zero(), 2);
        assert_eq!(result, Err("division by zero"));
    }

    #[test]
    fn test_mod() {
        let n = Number::from(11)
            .modulus(&Number::parse("2.5", 10).unwrap(), 0)
            .unwrap();

        assert_eq!(n.scale(), 1);
        assert_eq!(n.to_string(10), "1.0");
    }

    #[test]
    fn test_to_string() {
        let n = Number::parse("4.5", 10).unwrap().negate();
        assert_eq!(n.to_string(10), "-4.5");
        assert_eq!(n.to_string(10), "-4.5");
    }

    fn ten_to_the(n: u64) -> Number {
        Number::from(10).pow(&Number::from(n), 0).unwrap()
    }

    #[test]
    fn test_division_is_exact_past_the_hundredth_digit() {
        // bigdecimal's default context stops at 100 significant digits, so
        // every digit past that used to be a fabricated zero.
        let n = Number::from(1).div(&Number::from(3), 105).unwrap();
        assert_eq!(n.to_string(10), format!("0.{}", "3".repeat(105)));

        let n = Number::from(1).div(&Number::from(7), 105).unwrap();
        let repeating: String = "142857".repeat(18).chars().take(105).collect();
        assert_eq!(n.to_string(10), format!("0.{}", repeating));
    }

    #[test]
    fn test_division_truncates_rather_than_rounds() {
        // 2/3 to 100 places is a hundred sixes; rounding would end in a seven.
        let n = Number::from(2).div(&Number::from(3), 100).unwrap();
        assert_eq!(n.to_string(10), format!("0.{}", "6".repeat(100)));
    }

    #[test]
    fn test_integer_division_with_a_hundred_digit_quotient() {
        // No large scale involved: the quotient alone exceeded the context.
        let q = ten_to_the(100).div(&Number::from(7), 0).unwrap();
        let expected: String = "142857".repeat(17).chars().take(100).collect();
        assert_eq!(q.to_string(10), expected);
    }

    #[test]
    fn test_modulus_with_large_operands() {
        // a - (a/b)*b over a rounded quotient produced -3 here.
        let a = Number::from(2).pow(&Number::from(500), 0).unwrap();
        assert_eq!(a.modulus(&Number::from(7), 0).unwrap().to_string(10), "4");

        let a = ten_to_the(200);
        assert_eq!(a.modulus(&Number::from(97), 0).unwrap().to_string(10), "81");
    }

    #[test]
    fn test_sqrt_of_a_large_perfect_square_is_exact() {
        let root = ten_to_the(200).sqrt(0).unwrap();
        assert_eq!(root.to_string(10), format!("1{}", "0".repeat(100)));
    }

    #[test]
    fn test_sqrt_is_the_largest_truncated_root() {
        // r must satisfy r^2 <= n < (r + one ulp)^2 at the requested scale.
        let two = Number::from(2);
        let root = two.clone().sqrt(150).unwrap();
        assert_eq!(root.scale(), 150);
        assert!(
            root.clone().mul(&root, 300).unwrap() <= two,
            "the truncated root must not exceed the true root"
        );
        let ulp = Number::from(1).div(&ten_to_the(150), 150).unwrap();
        let above = root.add(&ulp);
        assert!(
            above.clone().mul(&above, 300).unwrap() > two,
            "the truncated root must be the largest one that fits"
        );
    }

    #[test]
    fn test_subtraction_keeps_the_scale_of_a_zero_operand() {
        // bigdecimal's Sub returns the other operand untouched when one side
        // is zero, which dropped that side's scale.
        let a = Number::parse("1.5", 10).unwrap();
        let zero = Number::parse("0.0000", 10).unwrap();
        assert_eq!(a.clone().sub(&zero).scale(), 4);
        assert_eq!(zero.clone().sub(&a).scale(), 4);
        assert_eq!(a.add(&zero).scale(), 4);
    }

    #[test]
    fn test_zero_to_a_negative_power_is_an_error() {
        // This reached bigdecimal's own division-by-zero panic.
        assert_eq!(
            Number::zero().pow(&Number::from(2).negate(), 10),
            Err("division by zero")
        );
    }

    #[test]
    fn test_absurd_exponent_is_rejected_rather_than_looped() {
        assert_eq!(
            Number::from(2).pow(&Number::from(10_000_000_000u64), 0),
            Err("exponent is too large")
        );
        assert_eq!(
            Number::from(1).pow(&Number::from(10_000_000_000u64), 0),
            Err("exponent is too large")
        );
    }

    #[test]
    fn test_pow_by_squaring_stays_exact() {
        assert_eq!(
            Number::from(2)
                .pow(&Number::from(10), 0)
                .unwrap()
                .to_string(10),
            "1024"
        );
        assert_eq!(
            Number::from(3)
                .pow(&Number::from(5), 0)
                .unwrap()
                .to_string(10),
            "243"
        );
        assert_eq!(
            Number::from(2).pow(&Number::from(400), 0).unwrap().length(),
            121
        );
    }

    #[test]
    fn test_digit_width_comes_from_the_largest_digit() {
        // POSIX: bases 17-100 write two-digit groups, 101-1000 three, and so
        // on. Taking the width from the base made every power of ten one
        // column too wide.
        assert_eq!(Number::from(1024).to_string(100), " 10 24");
        assert_eq!(Number::from(1024).to_string(1000), " 001 024");
        assert_eq!(Number::from(1024).to_string(10000), " 1024");
        assert_eq!(Number::from(1024).to_string(99), " 10 34");
        assert_eq!(Number::from(1024).to_string(101), " 010 014");
        // The two worked examples from the standard.
        assert_eq!(Number::from(1024).to_string(25), " 01 15 24");
        assert_eq!(Number::from(1024).to_string(125), " 008 024");
    }

    #[test]
    fn test_length_counts_leading_fractional_zeros() {
        // POSIX: "the total number of significant decimal digits".
        assert_eq!(Number::parse(".001", 10).unwrap().length(), 3);
        assert_eq!(Number::parse(".0001234", 10).unwrap().length(), 7);
        assert_eq!(Number::parse("0.000", 10).unwrap().length(), 3);
        assert_eq!(Number::parse("0.10", 10).unwrap().length(), 2);
        assert_eq!(Number::parse("1.100", 10).unwrap().length(), 4);
        assert_eq!(Number::from(0).length(), 1);
        assert_eq!(Number::from(1000).length(), 4);
    }

    #[test]
    fn test_high_scale_rendering_is_not_quadratic() {
        // Printing used to cost a full-precision operation per digit, which
        // made a high-scale result far slower to print than to compute.
        let n = Number::from(1).div(&Number::from(3), 20_000).unwrap();
        let text = n.to_string(10);
        assert_eq!(text.len(), 20_002); // "0." and 20000 threes
        assert!(text.ends_with("333"));
    }

    #[test]
    fn test_scale_beyond_the_working_limit_is_rejected() {
        assert_eq!(
            Number::from(1).div(&Number::from(3), 2_000_000),
            Err("number too large")
        );
    }

    /// A product's scale is the sum of its operands' scales, so repeated
    /// multiplication could build a value past the working limit, which then
    /// printed as 0.
    #[test]
    fn test_product_beyond_the_working_limit_is_rejected() {
        let third = Number::from(1).div(&Number::from(3), 600_000).unwrap();
        assert_eq!(
            third.clone().mul(&third, 2_000_000),
            Err("number too large")
        );
        // At the limit it is still computed.
        let third = Number::from(1).div(&Number::from(3), 500_000).unwrap();
        assert_eq!(
            third.clone().mul(&third, 2_000_000).unwrap().scale(),
            1_000_000
        );
    }

    /// A quotient has the scale register's scale, which division held to the
    /// limit only through the size of its working numerator: a dividend with
    /// a large scale of its own let the quotient past it.
    #[test]
    fn test_quotient_beyond_the_working_limit_is_rejected() {
        let third = Number::from(1).div(&Number::from(3), 1_000_000).unwrap();
        assert_eq!(
            third.div(&Number::from(1), 2_000_000),
            Err("number too large")
        );
    }

    /// The integer digits of a literal are held to the limit as well.
    #[test]
    fn test_long_integer_literal_is_too_large() {
        assert_eq!(
            Number::parse(&"7".repeat(1_000_001), 10),
            Err("number too large")
        );
        let n = Number::parse(&"7".repeat(1_000_000), 10).unwrap();
        assert_eq!(n.length(), 1_000_000);
    }

    /// Reading digits splits the string in halves; the pieces must join with
    /// the right power, including leading zeros in the low half.
    #[test]
    fn test_digit_strings_read_exactly() {
        for base in [2u32, 3, 10, 16] {
            let digits: String = (0..5000)
                .map(|i| {
                    char::from_digit((i * 7 + i / 3) % base, base)
                        .unwrap()
                        .to_ascii_uppercase()
                })
                .collect();
            assert_eq!(
                read_digits(&digits, base),
                BigInt::from_str_radix(&digits, base).unwrap(),
                "base {base}"
            );
        }
        assert_eq!(
            read_digits(&format!("1{}", "0".repeat(3000)), 10),
            BigInt::from(10u8).pow(3000u32)
        );
    }

    /// A literal with more fractional digits than the working limit is too
    /// large, not an invalid digit.
    #[test]
    fn test_literal_beyond_the_working_limit_is_too_large() {
        let text = format!(".{}", "7".repeat(1_000_001));
        assert_eq!(Number::parse(&text, 10), Err("number too large"));
        assert_eq!(
            Number::parse("19", 9),
            Err("invalid digit for the current ibase")
        );
    }

    #[test]
    fn test_long_fraction_parses_exactly() {
        let digits = "7".repeat(5000);
        let n = Number::parse(&format!("1.{digits}"), 10).unwrap();
        assert_eq!(n.to_string(10), format!("1.{digits}"));
        // In base 16, 0.8 is a half and keeps one fractional digit.
        assert_eq!(Number::parse(".8", 16).unwrap().to_string(10), "0.5");
        assert_eq!(Number::parse("A.C", 16).unwrap().to_string(10), "10.7");
        // Truncated, not rounded: 0.2 in base 3 is 0.666...
        assert_eq!(Number::parse(".2", 3).unwrap().to_string(10), "0.6");
    }
}
