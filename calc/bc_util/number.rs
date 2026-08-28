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

/// Split very long numeric output across lines, matching bc's convention (and
/// GNU bc): continued lines hold up to 68 characters followed by a `<backslash>`
/// continuation, and the final line holds the remainder. Output is ASCII, so
/// character and column counts coincide.
fn wrap_long_output(s: String) -> String {
    const CHUNK: usize = 68;
    if s.len() <= CHUNK {
        return s;
    }
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len() + s.len() / CHUNK * 2);
    let mut i = 0;
    while i < bytes.len() {
        let end = usize::min(i + CHUNK, bytes.len());
        out.push_str(&s[i..end]);
        if end < bytes.len() {
            out.push_str("\\\n");
        }
        i = end;
    }
    out
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
    /// # Returns
    /// `None` if the string contains invalid characters for the given base.
    /// # Panics
    /// panics if:
    /// - the string is empty
    /// - `base` is not in the range 2..=16.
    /// - `s` does not contain a valid number
    ///
    /// all the above should have been already checked by the parser
    pub fn parse(s: &str, base: u64) -> Option<Number> {
        assert!(!s.is_empty(), "parsed number has no digits");
        assert!((2..=16).contains(&base), "base must be in the range 2..=16");

        for c in s.bytes() {
            if c != b'.' && to_digit(c) >= base as u8 {
                return None;
            }
        }

        let mut integer_part = BigDecimal::zero();
        let mut fractional_part = BigDecimal::zero();
        let mut max_scale = 0;

        if let Some((int, decimal)) = s.split_once('.') {
            if !int.is_empty() {
                integer_part = BigInt::from_str_radix(int, base as u32).unwrap().into();
            }
            max_scale = decimal.len() as u32;
            let mut nominator = BigInt::zero();
            let mut denominator = BigInt::one();

            for c in decimal.bytes() {
                nominator *= base;
                denominator *= base;
                let digit = to_digit(c);
                if digit != 0 {
                    nominator += digit;
                }
            }
            // Truncate to the number of fractional digits written. Dividing
            // through BigDecimal would cap the quotient at its default
            // 100-digit context and invent every digit past that.
            let scaled = nominator * ten_pow(max_scale as u64)?;
            fractional_part = BigDecimal::new(scaled / denominator, max_scale as i64);
        } else {
            integer_part = BigInt::from_str_radix(s, base as u32).unwrap().into();
        }

        // In regards to the scale of parsed values, the standard doesn't specify.
        // The following matches the GNU implementation, which sets the scale
        // to the number of fractional digits in the string, regardless of the base
        Some(Self(
            (integer_part + fractional_part).with_scale(max_scale as i64),
        ))
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
        let ten_to_scale = match ten_pow(scale) {
            Some(value) => value,
            // A scale this large cannot be rendered; the arithmetic that
            // produced it would already have been refused.
            None => return "0".to_string(),
        };
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
            return wrap_long_output(result);
        }
        result.push('.');
        let count = fractional_digits_for(base, scale);
        // floor(fraction * base^count), the first `count` digits of the
        // fraction in `base`.
        let digits = fraction_numerator * BigInt::from(base).pow(count as u32) / &ten_to_scale;
        push_fractional_digits(&mut result, &digits, base, count);
        wrap_long_output(result)
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

    pub fn mul(self, other: &Number, scale: u64) -> Number {
        let a = self.scale();
        let b = other.scale();
        let required_scale = u64::min(a + b, scale.max(a).max(b));
        let result = self.0 * &other.0;
        Self(result).rescale(required_scale)
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
        Ok(self.sub(&a_over_b.mul(other, scale)))
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
    fn test_long_output_is_wrapped() {
        // 2^240 has 73 decimal digits: a 68-char line ending in a backslash
        // continuation, then the 5-character remainder (audit #B6).
        let n = Number::from(2).pow(&Number::from(240), 0).unwrap();
        let s = n.to_string(10);
        let first_line = s.split('\n').next().unwrap();
        assert_eq!(first_line.len(), 69);
        assert!(first_line.ends_with('\\'));
        assert_eq!(s.replace("\\\n", "").len(), 73);
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
            .mul(&Number::parse("5", 10).unwrap(), 10);
        assert_eq!(n.scale(), 0);
        assert_eq!(n.to_string(10), "10");
    }

    #[test]
    fn test_mul() {
        let n = Number::parse("2.25", 10).unwrap().mul(&Number::from(4), 10);
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

    /// Long output is split across lines; these tests care about the digits.
    fn digits(n: &Number, base: u64) -> String {
        n.to_string(base).replace("\\\n", "")
    }

    fn ten_to_the(n: u64) -> Number {
        Number::from(10).pow(&Number::from(n), 0).unwrap()
    }

    #[test]
    fn test_division_is_exact_past_the_hundredth_digit() {
        // bigdecimal's default context stops at 100 significant digits, so
        // every digit past that used to be a fabricated zero.
        let n = Number::from(1).div(&Number::from(3), 105).unwrap();
        assert_eq!(digits(&n, 10), format!("0.{}", "3".repeat(105)));

        let n = Number::from(1).div(&Number::from(7), 105).unwrap();
        let repeating: String = "142857".repeat(18).chars().take(105).collect();
        assert_eq!(digits(&n, 10), format!("0.{}", repeating));
    }

    #[test]
    fn test_division_truncates_rather_than_rounds() {
        // 2/3 to 100 places is a hundred sixes; rounding would end in a seven.
        let n = Number::from(2).div(&Number::from(3), 100).unwrap();
        assert_eq!(digits(&n, 10), format!("0.{}", "6".repeat(100)));
    }

    #[test]
    fn test_integer_division_with_a_hundred_digit_quotient() {
        // No large scale involved: the quotient alone exceeded the context.
        let q = ten_to_the(100).div(&Number::from(7), 0).unwrap();
        let expected: String = "142857".repeat(17).chars().take(100).collect();
        assert_eq!(digits(&q, 10), expected);
    }

    #[test]
    fn test_modulus_with_large_operands() {
        // a - (a/b)*b over a rounded quotient produced -3 here.
        let a = Number::from(2).pow(&Number::from(500), 0).unwrap();
        assert_eq!(digits(&a.modulus(&Number::from(7), 0).unwrap(), 10), "4");

        let a = ten_to_the(200);
        assert_eq!(digits(&a.modulus(&Number::from(97), 0).unwrap(), 10), "81");
    }

    #[test]
    fn test_sqrt_of_a_large_perfect_square_is_exact() {
        let root = ten_to_the(200).sqrt(0).unwrap();
        assert_eq!(digits(&root, 10), format!("1{}", "0".repeat(100)));
    }

    #[test]
    fn test_sqrt_is_the_largest_truncated_root() {
        // r must satisfy r^2 <= n < (r + one ulp)^2 at the requested scale.
        let two = Number::from(2);
        let root = two.clone().sqrt(150).unwrap();
        assert_eq!(root.scale(), 150);
        assert!(
            root.clone().mul(&root, 300) <= two,
            "the truncated root must not exceed the true root"
        );
        let ulp = Number::from(1).div(&ten_to_the(150), 150).unwrap();
        let above = root.add(&ulp);
        assert!(
            above.clone().mul(&above, 300) > two,
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
            digits(&Number::from(2).pow(&Number::from(10), 0).unwrap(), 10),
            "1024"
        );
        assert_eq!(
            digits(&Number::from(3).pow(&Number::from(5), 0).unwrap(), 10),
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
        let text = digits(&n, 10);
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
}
