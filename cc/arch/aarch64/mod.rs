//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64 architecture support
//

mod call;
pub mod codegen;
mod expression;
mod features;
mod float;
pub mod lir;
pub mod macros;
pub(crate) mod mapping;
pub mod regalloc;

pub use macros::get_macros;

/// Convert f64 to IEEE 754 half-precision (binary16) bits.
/// This handles the conversion from 64-bit double to 16-bit half precision.
pub(super) fn f64_to_f16_bits(val: f64) -> u16 {
    let bits = val.to_bits();
    let sign = ((bits >> 63) & 1) as u16;
    let exp = ((bits >> 52) & 0x7FF) as i32;
    let frac = bits & 0xFFFFFFFFFFFFF;

    // Handle special cases
    if exp == 0x7FF {
        // NaN or Infinity
        if frac != 0 {
            // NaN: preserve some mantissa bits
            return (sign << 15) | 0x7E00 | ((frac >> 42) as u16 & 0x1FF);
        } else {
            // Infinity
            return (sign << 15) | 0x7C00;
        }
    }

    // Rebias exponent: f64 bias is 1023, f16 bias is 15
    let new_exp = exp - 1023 + 15;

    if new_exp >= 31 {
        // Overflow to infinity
        return (sign << 15) | 0x7C00;
    }

    if new_exp <= 0 {
        // Denormal or zero
        if new_exp < -10 {
            // Too small, flush to zero
            return sign << 15;
        }
        // Denormal: shift mantissa right
        let shift = 1 - new_exp;
        let frac_with_hidden = frac | 0x10000000000000; // Add hidden bit
        let shifted = frac_with_hidden >> (42 + shift);
        return (sign << 15) | (shifted as u16 & 0x3FF);
    }

    // Normal number: truncate mantissa from 52 bits to 10 bits
    let new_frac = (frac >> 42) as u16;
    (sign << 15) | ((new_exp as u16) << 10) | (new_frac & 0x3FF)
}

/// Widen an `f64` to IEEE binary128, returning `(low 64 bits, high 64 bits)`.
///
/// The parser stores every floating literal as an `f64` (`ExprKind::FloatLit`),
/// so a `long double` constant carries at most 53 bits of mantissa regardless.
/// This produces the exact binary128 encoding *of that value*, which is what
/// the ABI requires even though the value itself was already rounded.
pub(crate) fn f64_to_f128_bits(v: f64) -> (u64, u64) {
    let bits = v.to_bits();
    let sign = bits >> 63;
    let exp = ((bits >> 52) & 0x7FF) as i64;
    let mantissa = bits & 0x000F_FFFF_FFFF_FFFF;

    let (exp128, mant_hi, mant_lo) = if exp == 0 && mantissa == 0 {
        // Signed zero.
        (0u64, 0u64, 0u64)
    } else if exp == 0x7FF {
        // Infinity or NaN: the quiet bit is the mantissa's top bit either way.
        (0x7FFF, mantissa >> 4, mantissa << 60)
    } else if exp == 0 {
        // Subnormal double. Normalize into binary128's much wider range: the
        // leading 1 is found by shifting, and binary128's exponent span makes
        // the result an ordinary normal number.
        let shift = mantissa.leading_zeros() - 11;
        let norm = (mantissa << (shift + 1)) & 0x000F_FFFF_FFFF_FFFF;
        let e = 1 - 1023 - (shift as i64) + 16383;
        (e as u64, norm >> 4, norm << 60)
    } else {
        // Normal: rebias the exponent and left-align the mantissa, which grows
        // from 52 bits to 112.
        let e = (exp - 1023 + 16383) as u64;
        (e, mantissa >> 4, mantissa << 60)
    };

    let hi = (sign << 63) | (exp128 << 48) | mant_hi;
    (mant_lo, hi)
}
