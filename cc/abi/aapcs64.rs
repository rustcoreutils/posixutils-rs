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

/// AAPCS64 ABI implementation.
#[derive(Debug, Clone, Default)]
pub struct Aapcs64Abi;

impl Aapcs64Abi {
    /// Create a new AAPCS64 ABI classifier.
    pub fn new() -> Self {
        Self
    }

    /// Check if a type is a potential HFA base type (float or double).
    fn is_hfa_base_type(&self, kind: TypeKind) -> Option<HfaBase> {
        match kind {
            TypeKind::Float => Some(HfaBase::Float32),
            TypeKind::Double => Some(HfaBase::Float64),
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
                if let Some(base) = self.is_hfa_base_type(elem_kind) {
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
        if composite.members.is_empty() || composite.members.len() > MAX_HFA_ELEMENTS as usize {
            return None;
        }

        let mut base_type: Option<HfaBase> = None;
        let mut count: u8 = 0;

        for member in &composite.members {
            let field_ty = member.typ;
            let field_kind = types.kind(field_ty);

            // Check if field is a valid HFA base type
            if let Some(field_base) = self.is_hfa_base_type(field_kind) {
                if let Some(existing_base) = base_type {
                    if existing_base != field_base {
                        return None; // Mixed types, not an HFA
                    }
                } else {
                    base_type = Some(field_base);
                }
                count += 1;
            } else if is_aggregate(field_kind) {
                // Nested struct - recursively check if it's an HFA
                if let Some((nested_base, nested_count)) = self.try_classify_hfa(field_ty, types) {
                    if let Some(existing_base) = base_type {
                        if existing_base != nested_base {
                            return None;
                        }
                    } else {
                        base_type = Some(nested_base);
                    }
                    count += nested_count;
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

        // Integer and pointer types - pass in X registers
        if is_integer(kind) || is_pointer(kind) {
            return ArgClass::Direct {
                classes: vec![RegClass::Integer],
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

        // Complex types
        if types.is_complex(ty) {
            let base_ty = types.complex_base(ty);
            let base_kind = types.kind(base_ty);

            if base_kind == TypeKind::Float {
                // Complex float: HFA-like with 2 floats
                return ArgClass::Hfa {
                    base: HfaBase::Float32,
                    count: 2,
                };
            } else if base_kind == TypeKind::Double {
                // Complex double: HFA-like with 2 doubles
                return ArgClass::Hfa {
                    base: HfaBase::Float64,
                    count: 2,
                };
            } else {
                // Complex long double: pass by reference
                return ArgClass::Indirect {
                    align: 16,
                    size_bits,
                };
            }
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

        // Integer and pointer types - return in X0
        if is_integer(kind) || is_pointer(kind) {
            return ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits,
            };
        }

        // Floating-point types - return in V0
        if is_float(kind) {
            return ArgClass::Direct {
                classes: vec![RegClass::Sse],
                size_bits,
            };
        }

        // Complex types - return as HFA
        if types.is_complex(ty) {
            let base_ty = types.complex_base(ty);
            let base_kind = types.kind(base_ty);

            if base_kind == TypeKind::Float {
                return ArgClass::Hfa {
                    base: HfaBase::Float32,
                    count: 2,
                };
            } else if base_kind == TypeKind::Double {
                return ArgClass::Hfa {
                    base: HfaBase::Float64,
                    count: 2,
                };
            } else {
                // Complex long double via sret
                return ArgClass::Indirect {
                    align: 16,
                    size_bits,
                };
            }
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

    #[test]
    fn test_abi_creation() {
        let abi = Aapcs64Abi::new();
        // Just ensure it constructs
        assert_eq!(format!("{:?}", abi), "Aapcs64Abi");
    }
}
