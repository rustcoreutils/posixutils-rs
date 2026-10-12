//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Units of size: a value width in bits, an object size in bytes, and an
// alignment.
//

//! The three quantities a size can be, as three types.
//!
//! - [`Bits`] is a **value width**: what a register or an instruction operand
//!   holds. It is small -- at most 128 for a scalar, and never more than the
//!   `u32` it is kept in -- and it is not an object size.
//! - [`ByteSize`] is an **object size** or a byte offset into an object: what
//!   `sizeof` answers and what a copy or a layout counts. It is bounded only by
//!   the target's `PTRDIFF_MAX`, so it is kept in a `u64`.
//! - [`Align`] is an **alignment**: a power of two, kept as its logarithm so
//!   that no other value can be one.
//!
//! None of them implements an arithmetic operator. A conversion from one to
//! another is a named method, so that it can be found by name, and none of them
//! saturates: an operation that can overflow answers an `Option`, and one whose
//! overflow would mean a bug elsewhere in the compiler is an internal compiler
//! error that names what overflowed.

use std::fmt;

/// A value width in bits: what an instruction operand or a register holds.
///
/// Not an object size, and so not divisible into bytes by arbitrary code: a
/// width becomes a byte count only through [`Bits::whole_bytes`] or
/// [`Bits::bytes_exact`], which refuse a width that is not a whole number of
/// bytes rather than rounding it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Bits(u32);

impl Bits {
    /// No width: what a function type or `void` in a value position has.
    pub const ZERO: Bits = Bits(0);

    /// A width of `bits` bits.
    pub const fn new(bits: u32) -> Self {
        Bits(bits)
    }

    /// The width as a raw bit count.
    pub const fn get(self) -> u32 {
        self.0
    }

    /// The width in bytes, when it is a whole number of them.
    pub const fn whole_bytes(self) -> Option<u32> {
        if self.0.is_multiple_of(8) {
            Some(self.0 / 8)
        } else {
            None
        }
    }

    /// The width in bytes, for a width that is known to be a whole number of
    /// them -- every type's is. A width that is not is an internal compiler
    /// error, since it means a bit-field width or a bit offset was taken for a
    /// type's width.
    pub fn bytes_exact(self) -> u32 {
        match self.whole_bytes() {
            Some(bytes) => bytes,
            None => panic!(
                "internal compiler error: a width of {} bits is not a whole number of bytes",
                self.0
            ),
        }
    }

    /// The bytes a value of this width touches: a partial byte counts as a
    /// whole one, since an access to it still reaches the byte it ends in.
    pub const fn bytes_covering(self) -> u32 {
        self.0.div_ceil(8)
    }

    /// The width as an object size: the bytes a value of this width occupies
    /// in memory, when it is a whole number of them.
    pub fn byte_size(self) -> Option<ByteSize> {
        self.whole_bytes().map(|b| ByteSize::new(u64::from(b)))
    }
}

impl fmt::Display for Bits {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// An object size, or a byte offset into an object.
///
/// Kept in a `u64` on every host, because the target's objects can be as large
/// as its `PTRDIFF_MAX` whatever the host's `usize` is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ByteSize(u64);

impl ByteSize {
    /// No bytes: an empty struct, an incomplete or variable-length array.
    pub const ZERO: ByteSize = ByteSize(0);

    /// A size of `bytes` bytes.
    pub const fn new(bytes: u64) -> Self {
        ByteSize(bytes)
    }

    /// A size counted in the host's `usize`.
    pub fn from_usize(bytes: usize) -> Self {
        match u64::try_from(bytes) {
            Ok(b) => ByteSize(b),
            Err(_) => panic!("internal compiler error: a size of {bytes} bytes does not fit a u64"),
        }
    }

    /// The size counted in whole bytes, from a count of bits that is a
    /// multiple of 8. `None` when it is not, or when the byte count does not
    /// fit: the caller decides what an object too large to describe means.
    pub fn from_bit_count(bits: u128) -> Option<Self> {
        if !bits.is_multiple_of(8) {
            return None;
        }
        u64::try_from(bits / 8).ok().map(ByteSize)
    }

    /// The size as a raw byte count.
    pub const fn get(self) -> u64 {
        self.0
    }

    /// The size as a host `usize`, for indexing host memory or for an
    /// interface still counted in `usize`. A size past the host's `usize` is an
    /// internal compiler error: on a 64-bit host every size fits, and on a
    /// narrower one an object that large cannot be built in memory anyway.
    pub fn to_usize(self) -> usize {
        match usize::try_from(self.0) {
            Ok(b) => b,
            Err(_) => panic!(
                "internal compiler error: a size of {} bytes does not fit the host's usize",
                self.0
            ),
        }
    }

    /// The size or offset as a signed byte count: what an instruction's
    /// displacement and pointer arithmetic count in. No object is larger than
    /// `PTRDIFF_MAX`, which an `i64` holds on every target, so a size that
    /// does not fit is an internal compiler error.
    pub fn to_i64(self) -> i64 {
        match i64::try_from(self.0) {
            Ok(b) => b,
            Err(_) => panic!(
                "internal compiler error: a size of {} bytes does not fit an i64; no object is that large",
                self.0
            ),
        }
    }

    /// Whether the size is zero.
    pub const fn is_zero(self) -> bool {
        self.0 == 0
    }

    /// The sum of two sizes, or `None` when it does not fit.
    pub fn checked_add(self, other: ByteSize) -> Option<ByteSize> {
        self.0.checked_add(other.0).map(ByteSize)
    }

    /// The size less `other`, or `None` when `other` is the larger: how far
    /// into an object one offset is past another.
    pub fn checked_sub(self, other: ByteSize) -> Option<ByteSize> {
        self.0.checked_sub(other.0).map(ByteSize)
    }

    /// The offset `delta` bytes on from this one, for two offsets inside one
    /// object -- a member's within a member's, say. No object is larger than
    /// `PTRDIFF_MAX`, so a sum that overflows is an internal compiler error.
    pub fn offset_by(self, delta: ByteSize) -> ByteSize {
        match self.checked_add(delta) {
            Some(sum) => sum,
            None => panic!(
                "internal compiler error: offset {} plus {} overflows; no object is that large",
                self.0, delta.0
            ),
        }
    }

    /// The size of `count` objects of this size, or `None` when it does not
    /// fit.
    pub fn checked_mul(self, count: u64) -> Option<ByteSize> {
        self.0.checked_mul(count).map(ByteSize)
    }

    /// The size rounded up to a multiple of `align`, or `None` when the
    /// rounding does not fit.
    pub fn align_up(self, align: Align) -> Option<ByteSize> {
        self.0.checked_next_multiple_of(align.bytes()).map(ByteSize)
    }

    /// Whether the size, or the offset, is a multiple of `align`.
    pub fn is_aligned(self, align: Align) -> bool {
        self.0.is_multiple_of(align.bytes())
    }

    /// The size as a value width: `None` when the object is too large for a
    /// value to have it -- past `u32::MAX` bits, which is 512 MiB.
    pub fn bits(self) -> Option<Bits> {
        self.0
            .checked_mul(8)
            .and_then(|b| u32::try_from(b).ok())
            .map(Bits)
    }

    /// The size counted in bits, exactly: what a layout that places bit-fields
    /// measures in. Every `u64` byte count has one in a `u128`.
    pub fn bit_count(self) -> u128 {
        u128::from(self.0) * 8
    }
}

impl fmt::Display for ByteSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// An alignment in bytes: always a power of two, kept as its logarithm.
///
/// Ordered by the alignment it stands for, so `max` is the stricter of two.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Align(u8);

impl Align {
    /// Byte alignment: no requirement at all.
    pub const ONE: Align = Align(0);

    /// Eightbyte alignment: a pointer's, on every target.
    pub const EIGHT: Align = Align(3);

    /// The alignment of `bytes` bytes, or `None` when `bytes` is not a power
    /// of two (zero included).
    pub const fn from_bytes(bytes: u64) -> Option<Align> {
        if bytes.is_power_of_two() {
            Some(Align(bytes.trailing_zeros() as u8))
        } else {
            None
        }
    }

    /// The alignment of `bytes` bytes, for a count known to be a power of two.
    /// Any other is an internal compiler error: an alignment the source wrote
    /// is checked when it is parsed, and every other is derived from one.
    pub fn of_bytes(bytes: u64) -> Align {
        match Align::from_bytes(bytes) {
            Some(a) => a,
            None => {
                panic!("internal compiler error: an alignment of {bytes} is not a power of two")
            }
        }
    }

    /// The alignment in bytes.
    pub const fn bytes(self) -> u64 {
        1u64 << self.0
    }

    /// The alignment in bytes, as a host `usize`.
    pub fn to_usize(self) -> usize {
        ByteSize::new(self.bytes()).to_usize()
    }

    /// The alignment's base-2 logarithm: what `.p2align` takes.
    pub const fn log2(self) -> u32 {
        self.0 as u32
    }
}

impl fmt::Display for Align {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.bytes().fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bits_convert_to_bytes_only_when_whole() {
        assert_eq!(Bits::new(64).whole_bytes(), Some(8));
        assert_eq!(Bits::new(0).whole_bytes(), Some(0));
        assert_eq!(Bits::new(12).whole_bytes(), None);
        assert_eq!(Bits::new(128).bytes_exact(), 16);
        assert_eq!(Bits::new(32).byte_size(), Some(ByteSize::new(4)));
        assert_eq!(Bits::new(3).byte_size(), None);
        assert_eq!(Bits::new(3).bytes_covering(), 1);
        assert_eq!(Bits::new(64).bytes_covering(), 8);
        assert_eq!(Bits::new(65).bytes_covering(), 9);
        assert_eq!(Bits::new(7).get(), 7);
        assert!(Bits::new(32) < Bits::new(64));
        assert_eq!(Bits::ZERO, Bits::new(0));
    }

    #[test]
    #[should_panic(expected = "internal compiler error: a width of 12 bits")]
    fn bits_bytes_exact_refuses_a_partial_byte() {
        Bits::new(12).bytes_exact();
    }

    #[test]
    fn byte_size_arithmetic_is_checked() {
        let max = ByteSize::new(u64::MAX);
        assert_eq!(
            ByteSize::new(3).checked_add(ByteSize::new(4)),
            Some(ByteSize::new(7))
        );
        assert_eq!(max.checked_add(ByteSize::new(1)), None);
        assert_eq!(
            ByteSize::new(7).checked_sub(ByteSize::new(3)),
            Some(ByteSize::new(4))
        );
        assert_eq!(ByteSize::new(3).checked_sub(ByteSize::new(7)), None);
        assert_eq!(
            ByteSize::new(8).offset_by(ByteSize::new(4)),
            ByteSize::new(12)
        );
        assert_eq!(ByteSize::new(6).checked_mul(7), Some(ByteSize::new(42)));
        assert_eq!(max.checked_mul(2), None);
        assert_eq!(ByteSize::ZERO.checked_mul(u64::MAX), Some(ByteSize::ZERO));
        assert!(ByteSize::ZERO.is_zero());
        assert_eq!(ByteSize::from_usize(12).get(), 12);
        assert_eq!(ByteSize::new(12).to_usize(), 12);
        assert_eq!(ByteSize::new(12).to_i64(), 12);
        assert_eq!(ByteSize::new(i64::MAX as u64).to_i64(), i64::MAX);
    }

    #[test]
    #[should_panic(expected = "internal compiler error: a size of 9223372036854775808 bytes")]
    fn byte_size_to_i64_refuses_past_ptrdiff_max() {
        ByteSize::new(1 << 63).to_i64();
    }

    #[test]
    #[should_panic(expected = "internal compiler error: offset")]
    fn byte_size_offset_by_refuses_to_overflow() {
        ByteSize::new(u64::MAX).offset_by(ByteSize::new(1));
    }

    #[test]
    fn byte_size_rounds_up_to_an_alignment() {
        let eight = Align::of_bytes(8);
        assert_eq!(ByteSize::new(0).align_up(eight), Some(ByteSize::new(0)));
        assert_eq!(ByteSize::new(1).align_up(eight), Some(ByteSize::new(8)));
        assert_eq!(ByteSize::new(16).align_up(eight), Some(ByteSize::new(16)));
        assert_eq!(ByteSize::new(u64::MAX).align_up(eight), None);
        assert_eq!(
            ByteSize::new(u64::MAX).align_up(Align::ONE),
            Some(ByteSize::new(u64::MAX))
        );
        assert!(ByteSize::new(24).is_aligned(eight));
        assert!(!ByteSize::new(20).is_aligned(eight));
    }

    #[test]
    fn byte_size_has_a_width_only_when_small() {
        assert_eq!(ByteSize::new(16).bits(), Some(Bits::new(128)));
        let largest = u64::from(u32::MAX / 8);
        assert_eq!(ByteSize::new(largest).bits(), Some(Bits::new(u32::MAX - 7)));
        assert_eq!(ByteSize::new(largest + 1).bits(), None);
        assert_eq!(ByteSize::new(u64::MAX).bits(), None);
        assert_eq!(
            ByteSize::new(u64::MAX).bit_count(),
            u128::from(u64::MAX) * 8
        );
    }

    #[test]
    fn byte_size_from_a_bit_count() {
        assert_eq!(ByteSize::from_bit_count(64), Some(ByteSize::new(8)));
        assert_eq!(ByteSize::from_bit_count(65), None);
        let past = (u128::from(u64::MAX) + 1) * 8;
        assert_eq!(ByteSize::from_bit_count(past), None);
        assert_eq!(
            ByteSize::from_bit_count(u128::from(u64::MAX) * 8),
            Some(ByteSize::new(u64::MAX))
        );
    }

    #[test]
    fn align_is_a_power_of_two() {
        assert_eq!(Align::from_bytes(0), None);
        assert_eq!(Align::from_bytes(12), None);
        assert_eq!(Align::from_bytes(1), Some(Align::ONE));
        assert_eq!(Align::from_bytes(8), Some(Align::EIGHT));
        let sixteen = Align::of_bytes(16);
        assert_eq!(sixteen.bytes(), 16);
        assert_eq!(sixteen.log2(), 4);
        assert_eq!(sixteen.to_usize(), 16);
        assert_eq!(Align::of_bytes(1 << 63).bytes(), 1 << 63);
        assert!(Align::of_bytes(4) < Align::of_bytes(8));
        assert_eq!(
            Align::of_bytes(4).max(Align::of_bytes(2)),
            Align::of_bytes(4)
        );
        assert_eq!(sixteen.to_string(), "16");
    }

    #[test]
    #[should_panic(expected = "internal compiler error: an alignment of 24")]
    fn align_of_bytes_refuses_a_non_power() {
        Align::of_bytes(24);
    }
}
