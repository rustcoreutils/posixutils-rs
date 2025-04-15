use bigdecimal::{BigDecimal, Num, One, Signed, ToPrimitive, Zero, num_bigint::BigInt};

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

/// Convert a number to a character
/// # Panics
/// panics if the number is bigger than 15
fn to_char(val: u8) -> char {
    match val {
        0..=9 => (val + b'0') as char,
        10..=15 => (val - 10 + b'A') as char,
        _ => panic!("number bigger than biggest representable base {}", val),
    }
}

/// converts a number to a string of len `base_ilog10`, padding with zeros
fn pad_digit(d: u64, base_ilog10: u32) -> String {
    let width = base_ilog10 + 1;
    format!("{:0width$}", d, width = width as usize)
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
            fractional_part = BigDecimal::from(nominator) / BigDecimal::from(denominator);
        } else {
            integer_part = BigInt::from_str_radix(s, base as u32).unwrap().into();
        }

        // In regards to the scale of parsed values, the standard doesn't specify.
        // The following matches the GNU implementation, which sets the scale
        // to the number of fractional digits in the string, regardless of the base
        Some(Self(
            integer_part + fractional_part.with_scale(max_scale as i64),
        ))
    }

    /// Convert the number to a string in the given base.
    pub fn to_string(&self, base: u64) -> String {
        let mut number = self.0.clone();
        if number.is_zero() {
            return "0".to_string();
        }

        let mut result = String::new();
        if self.0.is_negative() {
            result.push('-');
            number = -number;
        }

        let base_ilog10 = base.ilog10();
        let integer_part = number.with_scale(0);
        let mut fractional_part = number - &integer_part;

        if integer_part.is_zero() {
            result.push('0');
        } else {
            let (mut integer_part, _) = integer_part.into_bigint_and_exponent();

            let mut stack = Vec::new();
            while !integer_part.is_zero() {
                let remainder = &integer_part % base;
                integer_part /= base;
                stack.push(remainder.to_u64().unwrap());
            }

            for digit in stack.iter().rev() {
                if base <= 16 {
                    result.push(to_char(*digit as u8));
                } else {
                    result.push(' ');
                    result.push_str(&pad_digit(*digit, base_ilog10))
                }
            }
        }

        if fractional_part.fractional_digit_count() == 0 {
            return result;
        }

        result.push('.');
        let mut temp = BigDecimal::one();
        let scale = self.scale();
        // The standard doesn't specify how many fractional digits to print.
        // Here, we set the scale of the number to the value smallest value of
        // i such that: (base ^ i).digits() > scale.
        // This method is also used in other implementations, including GNU bc.
        while temp.digits() <= scale {
            fractional_part *= base;
            let integer_part = fractional_part.with_scale(0);
            let digit = integer_part.to_u64().unwrap();
            if base <= 16 {
                result.push(to_char(digit as u8));
            } else {
                result.push_str(&pad_digit(digit, base_ilog10));
                result.push(' ');
            }
            fractional_part -= &integer_part;
            temp *= base
        }
        // remove trailing space
        if base > 16 {
            result.pop();
        }
        result
    }

    /// The number of decimal digits in the number.
    pub fn scale(&self) -> u64 {
        self.0.fractional_digit_count().max(0) as u64
    }

    pub fn length(&self) -> u64 {
        self.0.digits()
    }

    pub fn negate(self) -> Self {
        Self(-self.0)
    }

    pub fn add(self, other: &Number) -> Number {
        Self(self.0 + &other.0)
    }

    pub fn sub(self, other: &Number) -> Number {
        Self(self.0 - &other.0)
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
        let result = self.0 / &other.0;
        Ok(Self(result).rescale(scale))
    }

    pub fn pow(self, other: &Number, scale: u64) -> NumericResult {
        if !other.0.is_integer() {
            return Err("exponent has to be an integer");
        }

        let a = self.scale();
        let b = other
            .0
            .to_i64()
            .ok_or("exponent is too large")?
            .unsigned_abs();
        let scale = if other.0.is_positive() {
            u64::min(a * b, u64::max(scale, a))
        } else {
            scale
        };
        let mut result = BigDecimal::one();
        for _ in 0..b {
            result *= &self.0;
        }
        if other.0.is_negative() {
            result = BigDecimal::one() / result;
        }
        Ok(Self(result).rescale(scale))
    }

    pub fn modulus(self, other: &Number, scale: u64) -> NumericResult {
        let a_over_b = self.clone().div(other, scale)?;
        let scale = u64::max(scale + other.scale(), self.scale());
        let result = self.sub(&a_over_b.mul(other, scale));
        Ok(result)
    }

    pub fn sqrt(self, min_scale: u64) -> NumericResult {
        let scale = self.scale().max(min_scale);
        if let Some(result) = self.0.sqrt() {
            return Ok(Self(result).rescale(scale));
        }
        Err("square root of negative number")
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
}
