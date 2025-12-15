//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Safe parsing utilities for integers with radix validation.
//!
//! This module provides helper functions to safely parse integers from strings
//! with specific radixes, addressing the issue that Rust's `from_str_radix`
//! accepts leading `+` or `-` signs even for non-decimal radixes.
//!
//! For example, `i64::from_str_radix("+55", 16)` succeeds, which may be
//! undesirable when parsing user input that should strictly conform to
//! hexadecimal or octal format without signs.

use std::num::ParseIntError;

/// Safely parse an unsigned integer from a string with the given radix.
///
/// This function validates that the input string contains only valid digits
/// for the specified radix, rejecting any leading signs (`+` or `-`).
///
/// # Examples
///
/// ```
/// use plib::safe_parse::parse_u32_radix;
///
/// assert!(parse_u32_radix("ff", 16).is_ok());
/// assert!(parse_u32_radix("+ff", 16).is_err()); // Reject leading +
/// assert!(parse_u32_radix("377", 8).is_ok());
/// assert!(parse_u32_radix("+377", 8).is_err()); // Reject leading +
/// ```
pub fn parse_u32_radix(s: &str, radix: u32) -> Result<u32, ParseIntError> {
    if s.is_empty() {
        return Err("empty string".parse::<u32>().unwrap_err());
    }
    
    // Check for leading signs
    if s.starts_with('+') || s.starts_with('-') {
        return Err("leading sign not allowed".parse::<u32>().unwrap_err());
    }
    
    u32::from_str_radix(s, radix)
}

/// Safely parse a signed integer from a string with the given radix.
///
/// This function is similar to `parse_u32_radix`, but allows a leading `-`
/// sign for negative numbers. It still rejects a leading `+` sign for
/// non-decimal radixes to ensure strict format validation.
///
/// # Examples
///
/// ```
/// use plib::safe_parse::parse_i64_radix;
///
/// assert!(parse_i64_radix("ff", 16).is_ok());
/// assert!(parse_i64_radix("-ff", 16).is_ok()); // Allow leading -
/// assert!(parse_i64_radix("+ff", 16).is_err()); // Reject leading +
/// ```
pub fn parse_i64_radix(s: &str, radix: u32) -> Result<i64, ParseIntError> {
    if s.is_empty() {
        return Err("empty string".parse::<i64>().unwrap_err());
    }
    
    // For non-decimal radixes, reject leading + sign
    if radix != 10 && s.starts_with('+') {
        return Err("leading + sign not allowed".parse::<i64>().unwrap_err());
    }
    
    i64::from_str_radix(s, radix)
}

/// Safely parse a u64 from a string with the given radix.
pub fn parse_u64_radix(s: &str, radix: u32) -> Result<u64, ParseIntError> {
    if s.is_empty() {
        return Err("empty string".parse::<u64>().unwrap_err());
    }
    
    if s.starts_with('+') || s.starts_with('-') {
        return Err("leading sign not allowed".parse::<u64>().unwrap_err());
    }
    
    u64::from_str_radix(s, radix)
}

/// Safely parse a usize from a string with the given radix.
pub fn parse_usize_radix(s: &str, radix: u32) -> Result<usize, ParseIntError> {
    if s.is_empty() {
        return Err("empty string".parse::<usize>().unwrap_err());
    }
    
    if s.starts_with('+') || s.starts_with('-') {
        return Err("leading sign not allowed".parse::<usize>().unwrap_err());
    }
    
    usize::from_str_radix(s, radix)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_u32_radix_hex() {
        // Valid hex
        assert_eq!(parse_u32_radix("ff", 16).unwrap(), 255);
        assert_eq!(parse_u32_radix("FF", 16).unwrap(), 255);
        assert_eq!(parse_u32_radix("0", 16).unwrap(), 0);
        
        // Invalid - leading signs
        assert!(parse_u32_radix("+ff", 16).is_err());
        assert!(parse_u32_radix("-ff", 16).is_err());
        
        // Invalid - non-hex digits
        assert!(parse_u32_radix("gg", 16).is_err());
        
        // Invalid - empty
        assert!(parse_u32_radix("", 16).is_err());
    }

    #[test]
    fn test_parse_u32_radix_octal() {
        // Valid octal
        assert_eq!(parse_u32_radix("377", 8).unwrap(), 255);
        assert_eq!(parse_u32_radix("0", 8).unwrap(), 0);
        
        // Invalid - leading signs
        assert!(parse_u32_radix("+377", 8).is_err());
        assert!(parse_u32_radix("-377", 8).is_err());
        
        // Invalid - non-octal digits
        assert!(parse_u32_radix("389", 8).is_err());
    }

    #[test]
    fn test_parse_i64_radix_hex() {
        // Valid hex
        assert_eq!(parse_i64_radix("ff", 16).unwrap(), 255);
        assert_eq!(parse_i64_radix("-ff", 16).unwrap(), -255);
        
        // Invalid - leading + sign
        assert!(parse_i64_radix("+ff", 16).is_err());
    }

    #[test]
    fn test_parse_i64_radix_octal() {
        // Valid octal
        assert_eq!(parse_i64_radix("377", 8).unwrap(), 255);
        assert_eq!(parse_i64_radix("-377", 8).unwrap(), -255);
        
        // Invalid - leading + sign
        assert!(parse_i64_radix("+377", 8).is_err());
    }

    #[test]
    fn test_parse_i64_radix_decimal() {
        // Decimal allows + sign
        assert_eq!(parse_i64_radix("123", 10).unwrap(), 123);
        assert_eq!(parse_i64_radix("+123", 10).unwrap(), 123);
        assert_eq!(parse_i64_radix("-123", 10).unwrap(), -123);
    }
}
