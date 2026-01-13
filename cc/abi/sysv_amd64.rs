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
// - Structs > 16 bytes use sret (hidden pointer in RDI)
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

        // Integer and pointer types
        if is_integer(kind) || is_pointer(kind) {
            return ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits,
            };
        }

        // Complex types - check BEFORE is_float since complex has Float/Double kind
        // Complex types are passed as two separate FP values in SSE registers
        if types.is_complex(ty) {
            let base_ty = types.complex_base(ty);
            let base_bits = types.size_bits(base_ty);
            return ArgClass::Direct {
                classes: vec![RegClass::Sse, RegClass::Sse],
                size_bits: base_bits * 2,
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

        // Integer and pointer types - return in RAX
        if is_integer(kind) || is_pointer(kind) {
            return ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits,
            };
        }

        // Complex types - check BEFORE is_float since complex has Float/Double kind
        // Complex types returned in XMM0 and XMM1
        if types.is_complex(ty) {
            let base_ty = types.complex_base(ty);
            let base_bits = types.size_bits(base_ty);
            return ArgClass::Direct {
                classes: vec![RegClass::Sse, RegClass::Sse],
                size_bits: base_bits * 2,
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

    // Note: More comprehensive tests require a TypeTable, which would be
    // created in integration tests. These tests verify basic behavior.

    #[test]
    fn test_abi_creation() {
        let abi = SysVAmd64Abi::new();
        // Basic sanity - just ensure it constructs
        assert_eq!(format!("{:?}", abi), "SysVAmd64Abi");
    }
}
