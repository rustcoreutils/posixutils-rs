//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Microsoft x64 calling convention, as `__attribute__((ms_abi))` selects it
// on an x86-64 target.
//
// Reference: Microsoft, "x64 calling convention"; gcc 13 on x86-64 Linux is
// the oracle for every case the reference leaves to the compiler.
//
// Key rules:
// - Every argument takes exactly one eight-byte *position*. The first four
//   positions are RCX, RDX, R8, R9 -- or XMM0-XMM3 for a floating-point
//   value -- and the rest are stacked after a 32-byte shadow area the caller
//   always reserves.
// - An aggregate of 1, 2, 4 or 8 bytes travels in its position as an
//   integer, whatever its members. Any other size -- and every value wider
//   than eight bytes: `long double`, `__int128`, `__float128` -- travels by
//   reference: the caller copies it and passes the copy's address.
// - The same sizes come back in RAX; anything else through a hidden pointer
//   in the first position, which the callee hands back in RAX.
//

use super::{is_float, is_integer, Abi, ArgClass, RegClass};
use crate::types::{TypeId, TypeKind, TypeTable};

/// Register positions: RCX, RDX, R8, R9 or XMM0-XMM3.
pub const WIN64_REG_POSITIONS: usize = 4;

/// The bytes one argument position occupies in memory: a stacked argument's
/// slot, a shadow slot, and the stride a `__builtin_ms_va_list` walks.
pub const WIN64_POSITION_BYTES: usize = 8;

/// The bytes a caller reserves below its stacked arguments for the callee to
/// spill the four register positions into, whether or not it has any.
pub const WIN64_SHADOW_BYTES: usize = WIN64_POSITION_BYTES * WIN64_REG_POSITIONS;

/// The Microsoft x64 convention.
#[derive(Debug, Clone, Default)]
pub struct Win64Abi;

impl Win64Abi {
    pub fn new() -> Self {
        Self
    }

    /// One integer position carrying the value's own bits.
    fn integer(size_bits: u32) -> ArgClass {
        ArgClass::Direct {
            classes: vec![RegClass::Integer],
            size_bits,
        }
    }

    /// By reference, or through the hidden return pointer.
    fn indirect(ty: TypeId, types: &TypeTable) -> ArgClass {
        ArgClass::Indirect {
            align: types.alignment(ty) as u32,
            size_bytes: types.size_bytes(ty),
        }
    }

    /// The rule both directions share for everything but a scalar float and
    /// a narrow integer: 1, 2, 4 or 8 bytes in an integer register, anything
    /// else in memory. An aggregate, a complex value, `__int128`, `long
    /// double` and `__float128` all meet it here, and so does a zero-sized
    /// struct, which gcc passes by reference and charges a position.
    fn by_size(ty: TypeId, types: &TypeTable) -> ArgClass {
        match types.size_bytes(ty) {
            1 | 2 | 4 | 8 => Self::integer(types.size_bits(ty)),
            _ => Self::indirect(ty, types),
        }
    }
}

/// A scalar `float` or `double`: the one value an XMM position carries.
///
/// `_Float16` is not one of them. gcc passes and returns it in the integer
/// position under `ms_abi`, as two bytes, and it is the peer this convention
/// has to meet.
fn is_sse_scalar(kind: TypeKind, ty: TypeId, types: &TypeTable) -> bool {
    matches!(kind, TypeKind::Float | TypeKind::Double) && !types.is_complex(ty)
}

impl Abi for Win64Abi {
    /// Win64 passes a vector by its size alone: eight bytes or fewer in a
    /// general register, sixteen by reference and returned in XMM0 -- as a
    /// plain `__int128` is -- and anything larger as an aggregate.
    fn vector_carrier(&self, vec: TypeId, types: &TypeTable) -> Option<TypeId> {
        match types.size_bytes(vec) {
            17.. => types.vector_memory_carrier(vec),
            16 => Some(types.int128_id),
            bytes => types.unsigned_of_size(bytes),
        }
    }

    fn classify_param(&self, ty: TypeId, types: &TypeTable) -> ArgClass {
        if types.is_vector(ty) {
            return match self.vector_carrier(ty, types) {
                Some(carrier) => self.classify_param(carrier, types),
                None => super::uncarried_vector_class(ty, types),
            };
        }
        if let Some(first) = types.transparent_union_first_member(ty) {
            return self.classify_param(first, types);
        }
        let kind = types.kind(ty);
        if kind == TypeKind::Void {
            return ArgClass::Ignore;
        }
        if is_sse_scalar(kind, ty, types) {
            return ArgClass::Direct {
                classes: vec![RegClass::Sse],
                size_bits: types.size_bits(ty),
            };
        }
        let size_bits = types.size_bits(ty);
        if is_integer(kind) && !types.is_complex(ty) && size_bits < 32 {
            return ArgClass::Extend {
                signed: !types.is_unsigned(ty),
                size_bits,
            };
        }
        if matches!(
            kind,
            TypeKind::Pointer | TypeKind::Array | TypeKind::Function
        ) {
            return Self::integer(64);
        }
        Self::by_size(ty, types)
    }

    fn classify_return(&self, ty: TypeId, types: &TypeTable) -> ArgClass {
        if types.is_vector(ty) {
            return match self.vector_return_carrier(ty, types) {
                Some(carrier) => self.classify_return(carrier, types),
                None => super::uncarried_vector_class(ty, types),
            };
        }
        let kind = types.kind(ty);
        // gcc returns a bare `__int128` whole in XMM0, the way it returns a
        // 128-bit vector, rather than through the hidden pointer its size
        // would otherwise call for.
        if types.is_plain_int128(ty) {
            return ArgClass::Direct {
                classes: vec![RegClass::Sse],
                size_bits: 128,
            };
        }
        // `long double` and `__float128` are floats too wide for any
        // register position: the size rule sends them through the hidden
        // pointer, which is what gcc does.
        if is_float(kind) && !is_sse_scalar(kind, ty, types) && !types.is_complex(ty) {
            return Self::by_size(ty, types);
        }
        self.classify_param(ty, types)
    }

    /// The callee owns the copy a by-reference argument points at.
    fn indirect_param_is_reference(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::target::{Arch, Os, Target};
    use crate::types::{CompositeType, MemberAlign, StructMember, Type};

    fn types() -> TypeTable {
        TypeTable::new(&Target::new(Arch::X86_64, Os::Linux))
    }

    /// A complete struct of `members`, each `(type, offset)`.
    fn record(
        types: &mut TypeTable,
        members: &[(TypeId, usize)],
        size: usize,
        align: usize,
    ) -> TypeId {
        let members = members
            .iter()
            .map(|&(typ, offset)| StructMember {
                name: crate::strings::StringId::default(),
                typ,
                offset,
                bit_width: None,
                bit_offset: None,
                access_bytes: None,
                align: MemberAlign::NATURAL,
            })
            .collect();
        types.intern(Type::struct_type(CompositeType {
            tag: None,
            members,
            enum_constants: vec![],
            size,
            align,
            member_align: align,
            is_complete: true,
            transparent: false,
            reverse_order: false,
            anon_id: None,
            tag_type: None,
        }))
    }

    /// A struct of `n` `char` members: exactly `n` bytes, alignment 1.
    fn chars(types: &mut TypeTable, n: usize) -> TypeId {
        let members: Vec<_> = (0..n).map(|i| (types.char_id, i)).collect();
        record(types, &members, n, 1)
    }

    fn integer(bits: u32) -> ArgClass {
        ArgClass::Direct {
            classes: vec![RegClass::Integer],
            size_bits: bits,
        }
    }

    fn sse(bits: u32) -> ArgClass {
        ArgClass::Direct {
            classes: vec![RegClass::Sse],
            size_bits: bits,
        }
    }

    #[test]
    fn scalars_take_their_register_file() {
        let t = types();
        let abi = Win64Abi::new();
        assert_eq!(abi.classify_param(t.int_id, &t), integer(32));
        assert_eq!(abi.classify_param(t.long_id, &t), integer(64));
        assert_eq!(abi.classify_param(t.pointer_to(t.int_id), &t), integer(64));
        assert_eq!(abi.classify_param(t.float_id, &t), sse(32));
        assert_eq!(abi.classify_param(t.double_id, &t), sse(64));
        assert_eq!(
            abi.classify_param(t.char_id, &t),
            ArgClass::Extend {
                signed: true,
                size_bits: 8
            }
        );
        assert_eq!(
            abi.classify_param(t.ushort_id, &t),
            ArgClass::Extend {
                signed: false,
                size_bits: 16
            }
        );
        // gcc's choice: the integer position, two bytes of it.
        assert_eq!(abi.classify_param(t.float16_id, &t), integer(16));
        assert_eq!(abi.classify_param(t.void_id, &t), ArgClass::Ignore);
    }

    #[test]
    fn aggregates_of_one_two_four_or_eight_bytes_travel_in_a_register() {
        let mut t = types();
        let abi = Win64Abi::new();
        for n in [1, 2, 4, 8] {
            let s = chars(&mut t, n);
            assert_eq!(abi.classify_param(s, &t), integer(8 * n as u32), "{n}");
            assert_eq!(abi.classify_return(s, &t), integer(8 * n as u32), "{n}");
        }
        // Whatever the members are: two floats are one integer eightbyte.
        let float = t.float_id;
        let two_floats = record(&mut t, &[(float, 0), (float, 4)], 8, 4);
        assert_eq!(abi.classify_param(two_floats, &t), integer(64));
        assert_eq!(abi.classify_return(two_floats, &t), integer(64));
    }

    #[test]
    fn every_other_size_travels_by_reference() {
        let mut t = types();
        let abi = Win64Abi::new();
        for n in [0, 3, 5, 6, 7, 9, 12, 16, 24, 100] {
            let s = chars(&mut t, n);
            assert!(
                matches!(abi.classify_param(s, &t), ArgClass::Indirect { size_bytes, .. } if size_bytes == n),
                "{n}"
            );
            assert!(
                matches!(abi.classify_return(s, &t), ArgClass::Indirect { .. }),
                "{n}"
            );
        }
        assert!(abi.indirect_param_is_reference());
    }

    #[test]
    fn wide_scalars_and_complex_values_follow_the_size_rule() {
        let t = types();
        let abi = Win64Abi::new();
        // Parameters: by reference, every one of them past eight bytes.
        for ty in [
            t.longdouble_id,
            t.float128_id,
            t.int128_id,
            t.make_complex(t.double_id),
            t.make_complex(t.longdouble_id),
        ] {
            assert!(
                matches!(abi.classify_param(ty, &t), ArgClass::Indirect { .. }),
                "{ty:?}"
            );
        }
        // `float _Complex` is eight bytes: one integer register.
        let fc = t.make_complex(t.float_id);
        assert_eq!(abi.classify_param(fc, &t), integer(64));
        assert_eq!(abi.classify_return(fc, &t), integer(64));

        // Returns: the hidden pointer, except `__int128`, which gcc hands
        // back whole in XMM0.
        for ty in [t.longdouble_id, t.float128_id, t.make_complex(t.double_id)] {
            assert!(
                matches!(abi.classify_return(ty, &t), ArgClass::Indirect { .. }),
                "{ty:?}"
            );
        }
        assert_eq!(abi.classify_return(t.int128_id, &t), sse(128));
        assert_eq!(abi.classify_return(t.double_id, &t), sse(64));
        assert_eq!(abi.classify_return(t.float16_id, &t), integer(16));
    }

    #[test]
    fn a_complex_integer_is_sized_like_an_aggregate() {
        let t = types();
        let abi = Win64Abi::new();
        let ci = t.make_complex(t.int_id);
        assert_eq!(abi.classify_param(ci, &t), integer(64));
        let cc = t.make_complex(t.char_id);
        assert_eq!(abi.classify_param(cc, &t), integer(16));
        let cl = t.make_complex(t.long_id);
        assert!(matches!(
            abi.classify_param(cl, &t),
            ArgClass::Indirect { size_bytes: 16, .. }
        ));
    }
}
