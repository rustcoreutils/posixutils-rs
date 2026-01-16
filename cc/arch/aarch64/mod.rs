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
