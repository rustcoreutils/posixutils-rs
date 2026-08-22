//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AAPCS64 (Procedure Call Standard for the ARM 64-bit Architecture) implementation
//
// Reference: ARM IHI 0055 - Procedure Call Standard for the Arm 64-bit Architecture
// (https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst)
//
// Key rules:
// - Arguments passed in X0-X7 (INTEGER) and V0-V7 (FP/SIMD)
// - Return values in X0, X1 or V0-V3
// - Structs > 16 bytes use sret (hidden pointer in X8, NOT X0!)
// - HFA (Homogeneous Floating-Point Aggregate) up to 4 elements in V0-V3
// - Structs 9-16 bytes use X0+X1
//

use super::{is_aggregate, is_float, is_integer, is_pointer, Abi, ArgClass, HfaBase, RegClass};
use crate::types::{TypeId, TypeKind, TypeTable};

/// Maximum aggregate size (in bits) that can be passed in registers.
/// Structs larger than 128 bits (16 bytes) must use sret (unless HFA).
const MAX_AGGREGATE_BITS: u32 = 128;

/// Maximum number of HFA/HVA elements.
const MAX_HFA_ELEMENTS: u8 = 4;

/// The HFA element type for a complex value, or `None` if it cannot be passed
/// in V registers.
///
/// A `_Complex` is a two-member homogeneous aggregate, so the only question is
/// how wide a member is — and that is a question about *width*, not about how
/// the type is spelled. Apple makes `long double` a 64-bit double, so
/// `long double _Complex` is an ordinary pair of doubles there; matching on
/// `TypeKind::LongDouble` instead sent it by reference, disagreeing with clang
/// about every such argument and return.
///
/// On aarch64 Linux `long double` is IEEE binary128, which occupies a whole Q
/// register -- so `long double _Complex` is a two-element HVA in q0/q1, the
/// same shape as the narrower complex types, not an indirect return.
fn complex_hfa_base(ty: TypeId, types: &TypeTable) -> Option<HfaBase> {
    match types.size_bits(types.complex_base(ty)) {
        16 => Some(HfaBase::Float16),
        32 => Some(HfaBase::Float32),
        64 => Some(HfaBase::Float64),
        128 => Some(HfaBase::Float128),
        _ => None,
    }
}

#[derive(Debug, Clone, Default)]
pub struct Aapcs64Abi;

impl Aapcs64Abi {
    /// Create a new AAPCS64 ABI classifier.
    pub fn new() -> Self {
        Self
    }

    /// Check if a type is a potential HFA base type (float or double).
    /// Keyed on the member's *width* rather than its kind, so Darwin's 64-bit
    /// `long double` still answers Float64 while the base standard's 128-bit
    /// one answers Float128.
    fn is_hfa_base_type(&self, kind: TypeKind, typ: TypeId, types: &TypeTable) -> Option<HfaBase> {
        match kind {
            // AAPCS64 admits half precision as a base type.
            TypeKind::Float16 => Some(HfaBase::Float16),
            TypeKind::Float => Some(HfaBase::Float32),
            TypeKind::Double => Some(HfaBase::Float64),
            TypeKind::Float128 => Some(HfaBase::Float128),
            TypeKind::LongDouble => match types.size_bits(typ) {
                64 => Some(HfaBase::Float64),
                128 => Some(HfaBase::Float128),
                _ => None,
            },
            _ => None,
        }
    }

    /// Try to classify an aggregate as an HFA (Homogeneous Floating-Point Aggregate).
    ///
    /// Returns Some(base, count) if the type is an HFA with up to 4 identical
    /// float or double members, None otherwise.
    fn try_classify_hfa(&self, ty: TypeId, types: &TypeTable) -> Option<(HfaBase, u8)> {
        let kind = types.kind(ty);
        let typ = types.get(ty);

        // Only structs and arrays can be HFAs
        if !is_aggregate(kind) && kind != TypeKind::Array {
            return None;
        }

        // For arrays, check if element type is float/double
        if kind == TypeKind::Array {
            if let Some(elem_ty) = typ.base {
                let elem_kind = types.kind(elem_ty);
                if let Some(base) = self.is_hfa_base_type(elem_kind, elem_ty, types) {
                    if let Some(len) = typ.array_size {
                        if len >= 1 && len <= MAX_HFA_ELEMENTS as usize {
                            return Some((base, len as u8));
                        }
                    }
                }
            }
            return None;
        }

        // For structs, check all fields
        let composite = typ.composite.as_ref()?;
        // A zero-width bit-field allocates nothing -- 6.7.2.1p12 gives it only
        // an effect on layout -- so it is not a member for the purpose of
        // AAPCS64 5.9.5 and must not stop the aggregate being homogeneous.
        // Counting it did: `struct { float f; int :0; }` was passed in a
        // general register where gcc passes it in `s0`, so a gcc caller's 1.5f
        // was read as a bit pattern. A bit-field of non-zero width is a real
        // integer member and still disqualifies the struct, as in gcc.
        let members = || composite.members.iter().filter(|m| m.bit_width != Some(0));
        let member_count = members().count();
        if member_count == 0 || member_count > MAX_HFA_ELEMENTS as usize {
            return None;
        }

        let mut base_type: Option<HfaBase> = None;
        let mut count: u8 = 0;
        // A union's members overlap, so it holds as many elements as its
        // largest member does -- not as many as all of them put together.
        // Summing them made `union { double v; double d; }` a two-element HFA:
        // the callee read sixteen bytes out of an eight-byte object and the
        // caller wrote sixteen back into an eight-byte slot, over whatever
        // followed it. AAPCS64 5.9.5 takes the maximum.
        let overlaps = kind == TypeKind::Union;
        let add = |count: &mut u8, n: u8| {
            *count = if overlaps {
                (*count).max(n)
            } else {
                count.saturating_add(n)
            };
        };

        for member in members() {
            let field_ty = member.typ;
            let field_kind = types.kind(field_ty);

            // Check if field is a valid HFA base type
            if let Some(field_base) = self.is_hfa_base_type(field_kind, field_ty, types) {
                if let Some(existing_base) = base_type {
                    if existing_base != field_base {
                        return None; // Mixed types, not an HFA
                    }
                } else {
                    base_type = Some(field_base);
                }
                add(&mut count, 1);
            } else if is_aggregate(field_kind) || field_kind == TypeKind::Array {
                // Nested struct or array - recursively check if it's an HFA.
                // The array arm of `try_classify_hfa` was only ever reachable
                // for a top-level array type, which C does not form, so a
                // member like `float v[1]` was rejected as a non-FP field.
                if let Some((nested_base, nested_count)) = self.try_classify_hfa(field_ty, types) {
                    if let Some(existing_base) = base_type {
                        if existing_base != nested_base {
                            return None;
                        }
                    } else {
                        base_type = Some(nested_base);
                    }
                    add(&mut count, nested_count);
                    if count > MAX_HFA_ELEMENTS {
                        return None;
                    }
                } else {
                    return None; // Nested struct is not an HFA
                }
            } else {
                return None; // Non-FP field
            }
        }

        if (1..=MAX_HFA_ELEMENTS).contains(&count) {
            base_type.map(|base| (base, count))
        } else {
            None
        }
    }

    /// Classify an aggregate type.
    fn classify_aggregate(&self, ty: TypeId, types: &TypeTable) -> ArgClass {
        let size_bits = types.size_bits(ty);

        // Empty struct
        if size_bits == 0 {
            return ArgClass::Ignore;
        }

        // Try HFA classification first
        if let Some((base, count)) = self.try_classify_hfa(ty, types) {
            return ArgClass::Hfa { base, count };
        }

        // Non-HFA aggregates: check size
        if size_bits > MAX_AGGREGATE_BITS {
            // Large aggregate - pass by reference
            return ArgClass::Indirect {
                align: types.alignment(ty) as u32,
                size_bits,
            };
        }

        // Small aggregate (≤16 bytes) - pass in X registers
        if size_bits <= 64 {
            ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits,
            }
        } else {
            // 9-16 bytes: use two registers
            ArgClass::Direct {
                classes: vec![RegClass::Integer, RegClass::Integer],
                size_bits,
            }
        }
    }
}

impl Abi for Aapcs64Abi {
    fn classify_param(&self, ty: TypeId, types: &TypeTable) -> ArgClass {
        let kind = types.kind(ty);
        let size_bits = types.size_bits(ty);

        // `__attribute__((transparent_union))` passes the union exactly as its
        // first member would be passed. Substituted here rather than on the
        // declared type, so the front end still sees a union and can check an
        // argument against every member. Without this, `RegClass::merge` folds
        // the members together and rule (d) makes `union { float f; int i; }`
        // INTEGER where gcc hands it over in SSE.
        if let Some(first) = types.transparent_union_first_member(ty) {
            return self.classify_param(first, types);
        }

        // Void type - ignore
        if kind == TypeKind::Void {
            return ArgClass::Ignore;
        }

        // Integer types smaller than 32 bits need extension
        // AAPCS64: "the size of the argument is rounded up to 4 bytes"
        if is_integer(kind) && size_bits < 32 {
            let signed = !types.is_unsigned(ty);
            return ArgClass::Extend { signed, size_bits };
        }

        // 128-bit integer types: two consecutive GP registers, 16-byte aligned
        if kind == TypeKind::Int128 {
            return ArgClass::Direct {
                classes: vec![RegClass::Integer, RegClass::Integer],
                size_bits,
            };
        }

        // Integer and pointer types - pass in X registers
        if is_integer(kind) || is_pointer(kind) {
            return ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits,
            };
        }

        // Complex types: a two-member HFA, in V registers.
        //
        // Must be tested BEFORE `is_float`, because a complex type carries its
        // *base's* kind — `float _Complex` answers `TypeKind::Float`. Testing
        // the other way round classified every complex parameter as a single
        // scalar V register, so the imaginary half was never passed and every
        // later floating-point argument sat one register too high.
        // `classify_return` already had the order right.
        if types.is_complex(ty) {
            if let Some(base) = complex_hfa_base(ty, types) {
                return ArgClass::Hfa { base, count: 2 };
            }
            return ArgClass::Indirect {
                align: 16,
                size_bits,
            };
        }

        // Floating-point types - pass in V registers
        if is_float(kind) {
            return ArgClass::Direct {
                classes: vec![RegClass::Sse], // Using Sse for FP registers
                size_bits,
            };
        }

        // Aggregate types (struct, union)
        if is_aggregate(kind) {
            return self.classify_aggregate(ty, types);
        }

        // Arrays - in parameter context, usually decay to pointers
        // but if passed by value, classify as aggregate
        if kind == TypeKind::Array {
            return self.classify_aggregate(ty, types);
        }

        // Function types (function pointers)
        if kind == TypeKind::Function {
            return ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits: 64,
            };
        }

        // Default: small values in registers, large by reference
        if size_bits <= 64 {
            ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits,
            }
        } else if size_bits <= MAX_AGGREGATE_BITS {
            ArgClass::Direct {
                classes: vec![RegClass::Integer, RegClass::Integer],
                size_bits,
            }
        } else {
            ArgClass::Indirect {
                align: types.alignment(ty) as u32,
                size_bits,
            }
        }
    }

    fn classify_return(&self, ty: TypeId, types: &TypeTable) -> ArgClass {
        let kind = types.kind(ty);
        let size_bits = types.size_bits(ty);

        // Void return
        if kind == TypeKind::Void {
            return ArgClass::Ignore;
        }

        // 128-bit integer types: return in X0+X1
        if kind == TypeKind::Int128 {
            return ArgClass::Direct {
                classes: vec![RegClass::Integer, RegClass::Integer],
                size_bits,
            };
        }

        // Integer and pointer types - return in X0
        if is_integer(kind) || is_pointer(kind) {
            return ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits,
            };
        }

        // Complex types - return as HFA (must check BEFORE is_float since complex
        // types have TypeKind::Float/Double/LongDouble)
        if types.is_complex(ty) {
            if let Some(base) = complex_hfa_base(ty, types) {
                return ArgClass::Hfa { base, count: 2 };
            }
            return ArgClass::Indirect {
                align: 16,
                size_bits,
            };
        }

        // Floating-point types (non-complex) - return in V0
        if is_float(kind) {
            return ArgClass::Direct {
                classes: vec![RegClass::Sse],
                size_bits,
            };
        }

        // Aggregate types
        if is_aggregate(kind) {
            // Try HFA first
            if let Some((base, count)) = self.try_classify_hfa(ty, types) {
                return ArgClass::Hfa { base, count };
            }

            // Non-HFA: check size
            if size_bits > MAX_AGGREGATE_BITS {
                // Large aggregate - return via X8 (sret)
                return ArgClass::Indirect {
                    align: types.alignment(ty) as u32,
                    size_bits,
                };
            }

            // Small aggregate
            if size_bits <= 64 {
                return ArgClass::Direct {
                    classes: vec![RegClass::Integer],
                    size_bits,
                };
            } else {
                // 9-16 bytes: X0+X1
                return ArgClass::Direct {
                    classes: vec![RegClass::Integer, RegClass::Integer],
                    size_bits,
                };
            }
        }

        // Default
        if size_bits <= 64 {
            ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits,
            }
        } else {
            ArgClass::Indirect {
                align: types.alignment(ty) as u32,
                size_bits,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::target::{Arch, Os, Target};
    use crate::types::{CompositeType, StructMember, Type};

    /// A union's members overlap, so it is an HFA of its *largest* member, not
    /// of all of them put together.
    ///
    /// Summing them made `union { double v; double d; }` -- eight bytes -- a
    /// two-element HFA: the callee read sixteen bytes out of it and the caller
    /// wrote sixteen back into an eight-byte slot, over whatever followed. On
    /// Apple arm64, where `long double` is `double`, that is exactly what
    /// `union { long double v; double d; }` is, and it corrupted the frame.
    #[test]
    fn a_union_is_an_hfa_of_its_largest_member() {
        let abi = Aapcs64Abi::new();
        let mut types = TypeTable::new(&Target::new(Arch::Aarch64, Os::Linux));
        let d = types.double_id;
        let member = |t| StructMember {
            name: crate::strings::StringId::default(),
            typ: t,
            offset: 0,
            bit_width: None,
            bit_offset: None,
            access_bytes: None,
            explicit_align: None,
        };
        let m0 = member(d);
        let m1 = member(d);
        let u = types.intern(Type::union_type(CompositeType {
            tag: None,
            members: vec![m0, m1],
            enum_constants: vec![],
            size: 8,
            align: 8,
            is_complete: true,
            transparent: false,
        }));
        assert!(
            matches!(
                abi.classify_return(u, &types),
                ArgClass::Hfa { count: 1, .. }
            ),
            "a union of two doubles is one element, got {:?}",
            abi.classify_return(u, &types)
        );

        // A *struct* of two doubles really is two elements.
        let s0 = member(d);
        let mut s1 = member(d);
        s1.offset = 8;
        let st = types.intern(Type::struct_type(CompositeType {
            tag: None,
            members: vec![s0, s1],
            enum_constants: vec![],
            size: 16,
            align: 8,
            is_complete: true,
            transparent: false,
        }));
        assert!(
            matches!(
                abi.classify_return(st, &types),
                ArgClass::Hfa { count: 2, .. }
            ),
            "a struct of two doubles is two elements, got {:?}",
            abi.classify_return(st, &types)
        );
    }

    #[test]
    fn test_abi_creation() {
        let abi = Aapcs64Abi::new();
        // Just ensure it constructs
        assert_eq!(format!("{:?}", abi), "Aapcs64Abi");
    }

    /// A complex value is a two-member HFA whose element width — not whose
    /// type *name* — decides the register class. Apple's `long double` is a
    /// 64-bit double, so `long double _Complex` belongs in two D registers
    /// like any other pair of doubles; matching on `TypeKind::LongDouble`
    /// sent it by reference and disagreed with clang.
    #[test]
    fn complex_is_classified_by_element_width() {
        use crate::target::{Arch, Os, Target};

        let abi = Aapcs64Abi::new();
        let two_f32 = ArgClass::Hfa {
            base: HfaBase::Float32,
            count: 2,
        };
        let two_f64 = ArgClass::Hfa {
            base: HfaBase::Float64,
            count: 2,
        };

        // A two-element HVA either way -- only the element width differs.
        // Apple makes `long double` a 64-bit double; the base standard makes it
        // IEEE binary128, which occupies a whole Q register.
        //
        // This asserted `Indirect` on Linux while `HfaBase` had no 128-bit
        // form. gcc disagrees: for `long double _Complex g(void)` it emits no
        // x8 indirect-result pointer and reads the real part straight out of
        // q0, which is only possible if the value came back in q0/q1.
        let two_f128 = ArgClass::Hfa {
            base: HfaBase::Float128,
            count: 2,
        };

        for (os, long_double) in [(Os::MacOS, two_f64.clone()), (Os::Linux, two_f128)] {
            let target = Target::new(Arch::Aarch64, os);
            let types = TypeTable::new(&target);

            for classify in [
                Aapcs64Abi::classify_param as fn(&Aapcs64Abi, TypeId, &TypeTable) -> ArgClass,
                Aapcs64Abi::classify_return,
            ] {
                assert_eq!(
                    classify(&abi, types.complex_float_id, &types),
                    two_f32,
                    "float _Complex on {:?}",
                    os
                );
                assert_eq!(
                    classify(&abi, types.complex_double_id, &types),
                    two_f64,
                    "double _Complex on {:?}",
                    os
                );
                assert_eq!(
                    classify(&abi, types.complex_longdouble_id, &types),
                    long_double,
                    "long double _Complex on {:?}",
                    os
                );
            }
        }
    }
}
