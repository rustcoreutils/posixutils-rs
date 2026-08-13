//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// System V AMD64 ABI implementation
//
// Reference: System V Application Binary Interface AMD64 Architecture
// Processor Supplement (https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf)
//
// Key rules:
// - Arguments passed in RDI, RSI, RDX, RCX, R8, R9 (INTEGER) and XMM0-XMM7 (SSE)
// - Return values in RAX, RDX (INTEGER) or XMM0, XMM1 (SSE)
// - Structs > 16 bytes: returned via sret (hidden pointer in RDI), passed by value on stack
// - Structs 9-16 bytes may use two registers
// - Each eightbyte is classified independently, then merged
//

use super::{is_aggregate, is_float, is_integer, is_pointer, Abi, ArgClass, RegClass};
use crate::types::{TypeId, TypeKind, TypeTable};

/// Maximum aggregate size (in bits) that can be passed in registers.
/// Structs larger than 128 bits (16 bytes) must use sret.
const MAX_AGGREGATE_BITS: u32 = 128;

/// Size of an eightbyte in bits.
const EIGHTBYTE_BITS: u32 = 64;

/// System V AMD64 ABI implementation.
#[derive(Debug, Clone, Default)]
pub struct SysVAmd64Abi;

/// The single scalar an aggregate consists of, seen through one-member structs
/// and unions and one-element arrays, or `None` if it holds anything else.
///
/// System V classifies `long double` as X87 plus X87UP and binary128 as SSE
/// plus SSEUP, and in both the *pair* is the unit: an aggregate holding nothing
/// besides one of them is passed and returned exactly as the bare scalar is.
/// The moment anything shares an eightbyte the merge rules apply instead, and
/// gcc confirms both edges -- `union { long double v; double d; }` is MEMORY,
/// and `union { __float128 v; double d[2]; }` really is two SSE registers.
fn sole_scalar_content(ty: TypeId, types: &TypeTable) -> Option<TypeId> {
    let inner = match types.kind(ty) {
        TypeKind::Struct | TypeKind::Union => {
            let typ = types.get(ty);
            let composite = typ.composite.as_ref()?;
            match composite.members.as_slice() {
                [only] => sole_scalar_content(only.typ, types)?,
                _ => return None,
            }
        }
        TypeKind::Array => {
            let typ = types.get(ty);
            if typ.array_size != Some(1) {
                return None;
            }
            sole_scalar_content(typ.base?, types)?
        }
        _ => return Some(ty),
    };
    // An over-aligned or padded wrapper is not the scalar; it is bigger.
    (types.size_bits(ty) == types.size_bits(inner)).then_some(inner)
}

impl SysVAmd64Abi {
    /// Create a new System V AMD64 ABI classifier.
    pub fn new() -> Self {
        Self
    }

    /// Classify a single eightbyte of a type at the given bit offset.
    fn classify_eightbyte(
        &self,
        ty: TypeId,
        _offset_bits: u32,
        _size_bits: u32,
        types: &TypeTable,
    ) -> RegClass {
        let kind = types.kind(ty);

        // Basic scalar types
        if is_integer(kind) || is_pointer(kind) {
            return RegClass::Integer;
        }

        if is_float(kind) {
            return if kind == TypeKind::LongDouble {
                // Long double (80-bit x87) gets MEMORY classification
                RegClass::Memory
            } else {
                RegClass::Sse
            };
        }

        // Complex types: real part in one eightbyte, imaginary in another
        if types.is_complex(ty) {
            return RegClass::Sse;
        }

        // Arrays - classify element type
        if kind == TypeKind::Array {
            let typ = types.get(ty);
            if let Some(elem_ty) = typ.base {
                return self.classify_eightbyte(elem_ty, 0, 0, types);
            }
        }

        // Structs and unions - classify by field overlap
        if is_aggregate(kind) {
            return self.classify_aggregate_eightbyte(ty, _offset_bits, types);
        }

        // Function pointers are integers
        if kind == TypeKind::Function {
            return RegClass::Integer;
        }

        // Default to memory for unknown types
        RegClass::Memory
    }

    /// Classify an eightbyte of an aggregate type by examining overlapping fields.
    fn classify_aggregate_eightbyte(
        &self,
        ty: TypeId,
        eightbyte_start: u32,
        types: &TypeTable,
    ) -> RegClass {
        let eightbyte_end = eightbyte_start + EIGHTBYTE_BITS;
        let mut class = RegClass::NoClass;

        let typ = types.get(ty);

        // Iterate over struct/union fields
        if let Some(ref composite) = typ.composite {
            for member in &composite.members {
                let field_ty = member.typ;
                let field_size = types.size_bits(field_ty);
                let field_start = member.offset as u32 * 8; // Convert byte offset to bits
                let field_end = field_start + field_size;

                // Check if field overlaps this eightbyte
                if field_start < eightbyte_end && field_end > eightbyte_start {
                    // Calculate the portion of the field in this eightbyte
                    let overlap_start = field_start.max(eightbyte_start);
                    let overlap_size = field_end.min(eightbyte_end) - overlap_start;

                    // Classify the overlapping portion
                    let field_class = self.classify_eightbyte(
                        field_ty,
                        overlap_start - field_start,
                        overlap_size,
                        types,
                    );
                    class = class.merge(field_class);
                }
            }
        }

        class
    }

    /// Classify a complete aggregate type into ArgClass.
    fn classify_aggregate(&self, ty: TypeId, types: &TypeTable) -> ArgClass {
        let size_bits = types.size_bits(ty);

        // Rule: Aggregates larger than 2 eightbytes (16 bytes) go to MEMORY
        if size_bits > MAX_AGGREGATE_BITS {
            return ArgClass::Indirect {
                align: types.alignment(ty) as u32,
                size_bits,
            };
        }

        // Empty struct
        if size_bits == 0 {
            return ArgClass::Ignore;
        }

        // Classify each eightbyte
        let mut classes = Vec::new();
        let num_eightbytes = size_bits.div_ceil(EIGHTBYTE_BITS);

        for i in 0..num_eightbytes {
            let offset = i * EIGHTBYTE_BITS;
            let class = self.classify_eightbyte(ty, offset, size_bits - offset, types);
            classes.push(class);
        }

        // Post-merge cleanup: if any eightbyte is MEMORY, the whole thing is MEMORY
        if classes.contains(&RegClass::Memory) {
            return ArgClass::Indirect {
                align: types.alignment(ty) as u32,
                size_bits,
            };
        }

        // Remove trailing NO_CLASS
        while classes.last() == Some(&RegClass::NoClass) {
            classes.pop();
        }

        if classes.is_empty() {
            ArgClass::Ignore
        } else {
            ArgClass::Direct { classes, size_bits }
        }
    }
}

impl Abi for SysVAmd64Abi {
    fn classify_param(&self, ty: TypeId, types: &TypeTable) -> ArgClass {
        let kind = types.kind(ty);
        let size_bits = types.size_bits(ty);

        // Void type - ignore
        if kind == TypeKind::Void {
            return ArgClass::Ignore;
        }

        // Integer types smaller than 32 bits need extension
        if is_integer(kind) && size_bits < 32 {
            // Check if type is unsigned
            let signed = !types.is_unsigned(ty);
            return ArgClass::Extend { signed, size_bits };
        }

        // 128-bit integer types: two GP registers
        if kind == TypeKind::Int128 {
            return ArgClass::Direct {
                classes: vec![RegClass::Integer, RegClass::Integer],
                size_bits,
            };
        }

        // Integer and pointer types
        if is_integer(kind) || is_pointer(kind) {
            return ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits,
            };
        }

        // Complex types - check BEFORE is_float, since a complex type carries
        // the Float/Double/LongDouble kind of its base.
        //
        // §3.2.3 classifies the three differently, and it is not "two SSE
        // eightbytes" for any of them but `double _Complex`:
        //
        //   float _Complex        8 bytes  -> ONE eightbyte, class SSE. Both
        //                                     floats are packed into the low
        //                                     64 bits of a single XMM.
        //   double _Complex      16 bytes  -> two eightbytes, both SSE.
        //   long double _Complex 32 bytes  -> COMPLEX_X87, which is passed in
        //                                     MEMORY. There is no XMM form; a
        //                                     value in an x87 slot cannot be
        //                                     moved through one.
        if types.is_complex(ty) {
            let base_ty = types.complex_base(ty);
            if types.kind(base_ty) == TypeKind::LongDouble {
                return ArgClass::Indirect {
                    align: 16,
                    size_bits,
                };
            }
            let base_bits = types.size_bits(base_ty);
            let total_bits = base_bits * 2;
            let classes = if total_bits <= 64 {
                vec![RegClass::Sse]
            } else {
                vec![RegClass::Sse, RegClass::Sse]
            };
            return ArgClass::Direct {
                classes,
                size_bits: total_bits,
            };
        }

        // Floating-point types (non-complex)
        if is_float(kind) {
            if kind == TypeKind::LongDouble {
                // Long double uses x87, passed on stack
                return ArgClass::Indirect {
                    align: 16,
                    size_bits,
                };
            }
            return ArgClass::Direct {
                classes: vec![RegClass::Sse],
                size_bits,
            };
        }

        // Aggregate types (struct, union, array)
        if is_aggregate(kind) {
            return self.classify_aggregate(ty, types);
        }

        // Arrays decay to pointers in parameter context
        if kind == TypeKind::Array {
            return ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits: 64, // Pointer size
            };
        }

        // Function types (function pointers)
        if kind == TypeKind::Function {
            return ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits: 64,
            };
        }

        // Default: pass by value if small enough
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

    fn classify_return(&self, ty: TypeId, types: &TypeTable) -> ArgClass {
        let kind = types.kind(ty);
        let size_bits = types.size_bits(ty);

        // Void return
        if kind == TypeKind::Void {
            return ArgClass::Ignore;
        }

        // 128-bit integer types: return in RAX+RDX
        if kind == TypeKind::Int128 {
            return ArgClass::Direct {
                classes: vec![RegClass::Integer, RegClass::Integer],
                size_bits,
            };
        }

        // Integer and pointer types - return in RAX
        if is_integer(kind) || is_pointer(kind) {
            return ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits,
            };
        }

        // Complex types - check BEFORE is_float, as in `classify_param`.
        //
        //   float _Complex        -> XMM0, both halves packed in one eightbyte
        //   double _Complex       -> XMM0 (real) + XMM1 (imag)
        //   long double _Complex  -> COMPLEX_X87: st(0) and st(1). We return
        //                            it indirectly instead, which keeps the
        //                            value correct within a translation unit
        //                            without inventing an x87 register pair
        //                            the rest of the backend cannot model. See
        //                            #C2 in cc/audit.md.
        if types.is_complex(ty) {
            let base_ty = types.complex_base(ty);
            if types.kind(base_ty) == TypeKind::LongDouble {
                // COMPLEX_X87: real in st(0), imaginary in st(1).
                return ArgClass::X87 { size_bits };
            }
            let base_bits = types.size_bits(base_ty);
            let total_bits = base_bits * 2;
            let classes = if total_bits <= 64 {
                vec![RegClass::Sse]
            } else {
                vec![RegClass::Sse, RegClass::Sse]
            };
            return ArgClass::Direct {
                classes,
                size_bits: total_bits,
            };
        }

        // Floating-point types - return in XMM0 (non-complex)
        if is_float(kind) {
            if kind == TypeKind::LongDouble {
                // Long double returned in ST(0) per System V AMD64 ABI
                return ArgClass::X87 { size_bits };
            }
            return ArgClass::Direct {
                classes: vec![RegClass::Sse],
                size_bits,
            };
        }

        // Aggregate types
        if is_aggregate(kind) {
            // X87 + X87UP comes back in st(0), exactly as the bare scalar
            // does: gcc emits `fld1; ret` for
            // `struct R { long double v; } f(void)`. Only the *return*
            // differs -- as an argument the class is MEMORY, which
            // `classify_aggregate` already answers with `Indirect`.
            if sole_scalar_content(ty, types)
                .is_some_and(|inner| types.kind(inner) == TypeKind::LongDouble)
            {
                return ArgClass::X87 { size_bits };
            }
            return self.classify_aggregate(ty, types);
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

    #[test]
    fn test_abi_creation() {
        let abi = SysVAmd64Abi::new();
        // Basic sanity - just ensure it constructs
        assert_eq!(format!("{:?}", abi), "SysVAmd64Abi");
    }

    /// A `TypeTable` for the x86-64 System V target these rules describe.
    fn x86_types() -> TypeTable {
        TypeTable::new(&Target::new(Arch::X86_64, Os::Linux))
    }

    /// A one-member struct wrapping `member`.
    fn wrap(types: &mut TypeTable, member: TypeId) -> TypeId {
        let size = (types.size_bits(member) / 8) as usize;
        let align = types.alignment(member);
        types.intern(Type::struct_type(CompositeType {
            tag: None,
            members: vec![StructMember {
                name: crate::strings::StringId::default(),
                typ: member,
                offset: 0,
                bit_width: None,
                bit_offset: None,
                storage_unit_size: None,
                explicit_align: None,
            }],
            enum_constants: vec![],
            size,
            align,
            is_complete: true,
        }))
    }

    /// `_Float16` is a floating type, scalar or wrapped.
    ///
    /// It was missing from the classifier's `is_float`, so an eightbyte
    /// holding one reached the default arm and answered MEMORY: a
    /// `struct { _Float16 h; }` was returned through a hidden pointer that
    /// nothing ever wrote, and read back as zero. A scalar one fell to the
    /// size-based default and was classified INTEGER, so the argument
    /// accounting charged it a general register while the backend used an SSE
    /// one.
    #[test]
    fn float16_is_classified_as_a_floating_type() {
        let abi = SysVAmd64Abi::new();
        let mut types = x86_types();
        let half = types.float16_id;

        assert!(
            matches!(abi.classify_return(half, &types),
                     ArgClass::Direct { ref classes, .. } if classes == &[RegClass::Sse]),
            "scalar _Float16 returns in an SSE register, got {:?}",
            abi.classify_return(half, &types)
        );
        assert!(
            matches!(abi.classify_param(half, &types),
                     ArgClass::Direct { ref classes, .. } if classes == &[RegClass::Sse]),
            "scalar _Float16 is passed in an SSE register"
        );

        let wrapped = wrap(&mut types, half);
        assert!(
            matches!(abi.classify_return(wrapped, &types),
                     ArgClass::Direct { ref classes, size_bits }
                         if classes == &[RegClass::Sse] && size_bits == 16),
            "struct {{ _Float16 }} returns in one SSE register, got {:?}",
            abi.classify_return(wrapped, &types)
        );
        assert!(
            !matches!(
                abi.classify_return(wrapped, &types),
                ArgClass::Indirect { .. }
            ),
            "struct {{ _Float16 }} must not be returned through a hidden pointer"
        );
    }

    /// An aggregate that is nothing but a `long double` is returned in st(0),
    /// and one that merges it with anything else is MEMORY.
    ///
    /// System V classifies the two eightbytes X87 and X87UP, and X87UP is
    /// preceded by X87, so the merge-to-MEMORY rule does not fire -- gcc emits
    /// `fld1; ret` for `struct R { long double v; } f(void)`. As an *argument*
    /// the class really is MEMORY, which is why only the return arm asks.
    #[test]
    fn a_lone_long_double_aggregate_returns_in_st0() {
        let abi = SysVAmd64Abi::new();
        let mut types = x86_types();
        let ld = types.longdouble_id;

        // Directly, and through the wrappers `sole_scalar_content` sees past.
        let plain = wrap(&mut types, ld);
        let nested = wrap(&mut types, plain);
        let arr1 = types.intern(Type::array(ld, 1));
        let wrapped_arr = wrap(&mut types, arr1);
        for (name, ty) in [
            ("struct { long double }", plain),
            ("struct { struct { long double } }", nested),
            ("struct { long double[1] }", wrapped_arr),
        ] {
            assert!(
                matches!(abi.classify_return(ty, &types), ArgClass::X87 { .. }),
                "{name} returns in st(0), got {:?}",
                abi.classify_return(ty, &types)
            );
            assert!(
                matches!(abi.classify_param(ty, &types), ArgClass::Indirect { .. }),
                "{name} is MEMORY class as an argument"
            );
        }

        // Two elements is not a lone scalar, and neither is a longer array.
        let arr2 = types.intern(Type::array(ld, 2));
        let wrapped_arr2 = wrap(&mut types, arr2);
        assert!(
            matches!(
                abi.classify_return(wrapped_arr2, &types),
                ArgClass::Indirect { .. }
            ),
            "two long doubles are over two eightbytes and go in memory"
        );
    }

    /// The other floating types keep the classification they had.
    #[test]
    fn the_other_floating_types_are_unchanged() {
        let abi = SysVAmd64Abi::new();
        let mut types = x86_types();

        for (name, id) in [("float", types.float_id), ("double", types.double_id)] {
            assert!(
                matches!(abi.classify_return(id, &types),
                         ArgClass::Direct { ref classes, .. } if classes == &[RegClass::Sse]),
                "{name} returns in an SSE register"
            );
        }
        // `long double` is x87 and its wrapper is MEMORY as an argument.
        assert!(matches!(
            abi.classify_return(types.longdouble_id, &types),
            ArgClass::X87 { .. }
        ));
        let ld = types.longdouble_id;
        let wrapped_ld = wrap(&mut types, ld);
        assert!(
            matches!(
                abi.classify_param(wrapped_ld, &types),
                ArgClass::Indirect { .. }
            ),
            "a struct holding a long double is MEMORY class as an argument"
        );
    }
}
