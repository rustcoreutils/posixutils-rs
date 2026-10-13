//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The type names gcc builds into its aarch64 compiler: the AdvSIMD vectors
// (`__Float32x4_t`) and the SVE vectors (`__SVFloat32_t`). gcc declares them
// as typedefs at file scope before the first token, which is what this does.
// glibc's <bits/math-vector.h> -- included by every <math.h> on aarch64 --
// names `__Float32x4_t`, `__Float64x2_t` and three SVE types for gcc 9 and
// 10 onwards. On any other target the names are ordinary identifiers, as in
// gcc.
//

use super::parser::Parser;
use crate::diag;
use crate::strings::StringId;
use crate::symbol::Symbol;
use crate::target::Arch;
use crate::token::lexer::Position;
use crate::types::{TypeId, TypeTable};

/// A use of an SVE type gcc refuses with a message of its own: anything
/// that needs the type's size, which only an SVE machine knows, or SVE code.
#[derive(Clone, Copy)]
pub(super) enum SveUse {
    /// An object of static storage duration, or an `extern` one: its size
    /// is fixed when the program is linked.
    StaticObject,
    /// `sizeof`.
    Size,
    /// `_Alignof`.
    Alignment,
    /// A structure or union member.
    Field,
    /// An array element.
    Element,
    /// Pointer arithmetic, which scales by the size.
    PointerArithmetic,
    /// A value of the type -- an automatic object, an argument, a load or
    /// a store -- which c17, generating no SVE code, cannot make.
    Value,
}

/// What a builtin type name names.
#[derive(Clone, Copy)]
enum Shape {
    /// An AdvSIMD vector: `count` lanes of the integer type of `bytes`
    /// bytes, unsigned or not, or of the floating type of `bytes` bytes.
    AdvSimd { lane: Lane, count: usize },
    /// An SVE vector, whose size only the machine knows; see
    /// [`TypeTable::sve_type`].
    Sve,
}

/// The lane type of an AdvSIMD vector, as gcc has it: `signed char`,
/// `short`, `int`, `long` and their unsigned counterparts, `float` and
/// `double` -- standard types, so `((__Int64x2_t){0})[0]` is a `long`.
#[derive(Clone, Copy)]
enum Lane {
    Int(u8),
    Uint(u8),
    Float(u8),
}

impl Lane {
    fn type_id(self, types: &TypeTable) -> TypeId {
        match self {
            Lane::Int(1) => types.schar_id,
            Lane::Int(2) => types.short_id,
            Lane::Int(4) => types.int_id,
            Lane::Int(_) => types.long_id,
            Lane::Uint(1) => types.uchar_id,
            Lane::Uint(2) => types.ushort_id,
            Lane::Uint(4) => types.uint_id,
            Lane::Uint(_) => types.ulong_id,
            Lane::Float(4) => types.float_id,
            Lane::Float(_) => types.double_id,
        }
    }
}

const fn adv(lane: Lane, count: usize) -> Shape {
    Shape::AdvSimd { lane, count }
}

/// Every aarch64 builtin type name c17 declares, with what it names. gcc 13
/// has the AdvSIMD vectors of `__Poly8_t`, `__fp16` and `__bf16` lanes too
/// (`__Poly8x8_t`, `__Float16x4_t`, `__Bfloat16x4_t`), whose lane types c17
/// does not have; they are left out rather than given the wrong lanes. The
/// SVE list is gcc 13's whole one. `kw.rs` interns each name.
const AARCH64_BUILTIN_TYPES: [(&str, Shape); 33] = [
    ("__Int8x8_t", adv(Lane::Int(1), 8)),
    ("__Int8x16_t", adv(Lane::Int(1), 16)),
    ("__Int16x4_t", adv(Lane::Int(2), 4)),
    ("__Int16x8_t", adv(Lane::Int(2), 8)),
    ("__Int32x2_t", adv(Lane::Int(4), 2)),
    ("__Int32x4_t", adv(Lane::Int(4), 4)),
    ("__Int64x1_t", adv(Lane::Int(8), 1)),
    ("__Int64x2_t", adv(Lane::Int(8), 2)),
    ("__Uint8x8_t", adv(Lane::Uint(1), 8)),
    ("__Uint8x16_t", adv(Lane::Uint(1), 16)),
    ("__Uint16x4_t", adv(Lane::Uint(2), 4)),
    ("__Uint16x8_t", adv(Lane::Uint(2), 8)),
    ("__Uint32x2_t", adv(Lane::Uint(4), 2)),
    ("__Uint32x4_t", adv(Lane::Uint(4), 4)),
    ("__Uint64x1_t", adv(Lane::Uint(8), 1)),
    ("__Uint64x2_t", adv(Lane::Uint(8), 2)),
    ("__Float32x2_t", adv(Lane::Float(4), 2)),
    ("__Float32x4_t", adv(Lane::Float(4), 4)),
    ("__Float64x1_t", adv(Lane::Float(8), 1)),
    ("__Float64x2_t", adv(Lane::Float(8), 2)),
    ("__SVInt8_t", Shape::Sve),
    ("__SVInt16_t", Shape::Sve),
    ("__SVInt32_t", Shape::Sve),
    ("__SVInt64_t", Shape::Sve),
    ("__SVUint8_t", Shape::Sve),
    ("__SVUint16_t", Shape::Sve),
    ("__SVUint32_t", Shape::Sve),
    ("__SVUint64_t", Shape::Sve),
    ("__SVFloat16_t", Shape::Sve),
    ("__SVFloat32_t", Shape::Sve),
    ("__SVFloat64_t", Shape::Sve),
    ("__SVBfloat16_t", Shape::Sve),
    ("__SVBool_t", Shape::Sve),
];

impl Parser<'_> {
    /// If `typ` is one of the SVE builtin types, report its use as `what`
    /// at `pos` in gcc's words and answer true. The caller then skips the
    /// incomplete-type diagnostic it would otherwise give, which would
    /// refuse the same use less clearly.
    pub(super) fn report_sve_use(&self, typ: TypeId, what: SveUse, pos: Position) -> bool {
        if self.types.sve_name(typ).is_none() {
            return false;
        }
        let named = self.types.format_type(typ, Some(self.idents));
        let template = match what {
            SveUse::StaticObject | SveUse::Size => "SVE type '{0}' does not have a fixed size",
            SveUse::Alignment => "SVE type '{0}' does not have a defined alignment",
            SveUse::Field => "fields cannot have SVE type '{0}'",
            SveUse::Element => "array elements cannot have SVE type '{0}'",
            SveUse::PointerArithmetic => "arithmetic on pointer to SVE type '{0}'",
            SveUse::Value => "this operation requires the SVE ISA extension",
        };
        diag::error_args(pos, template, &[&named]);
        true
    }

    /// If the function `name` of type `typ`, being defined, takes or
    /// returns an SVE type, report it in gcc's words and answer true: its
    /// parameters and result would live in SVE registers.
    pub(super) fn report_sve_definition(&self, name: StringId, typ: TypeId, pos: Position) -> bool {
        let func = self.types.get(typ);
        let sve = func
            .base
            .iter()
            .chain(func.params.iter().flatten())
            .any(|&t| self.types.sve_name(t).is_some());
        if sve {
            diag::error_args(
                pos,
                "'{0}' requires the SVE ISA extension",
                &[self.idents.get_opt(name).unwrap_or("")],
            );
        }
        sve
    }

    /// Declare the target's builtin type names at file scope, as gcc does
    /// before the first token: on aarch64, [`AARCH64_BUILTIN_TYPES`]. A
    /// declaration of the program's own may shadow one in an inner scope,
    /// as any typedef name may be.
    pub(super) fn declare_target_types(&mut self) {
        if self.types.target().arch != Arch::Aarch64 {
            return;
        }
        for (name, shape) in AARCH64_BUILTIN_TYPES {
            let Some(id) = self.idents.lookup(name) else {
                debug_assert!(false, "{name} is not interned");
                continue;
            };
            if self.symbols.lookup_typedef(id).is_some() {
                continue;
            }
            let typ = match shape {
                Shape::AdvSimd { lane, count } => {
                    let lane = lane.type_id(self.types);
                    self.types.advsimd_vector(lane, count)
                }
                Shape::Sve => self.types.sve_type(name),
            };
            let declared = self
                .symbols
                .declare(Symbol::typedef(id, typ, self.symbols.depth()));
            debug_assert!(declared.is_ok(), "{name} declared twice");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::strings::StringTable;
    use crate::target::Target;

    /// The parser's identifier table is read-only, so every name must be
    /// one `kw.rs` interns.
    #[test]
    fn every_builtin_type_name_is_interned() {
        let table = StringTable::new();
        for (name, _) in AARCH64_BUILTIN_TYPES {
            assert!(table.lookup(name).is_some(), "{name}");
        }
    }

    /// Each AdvSIMD name is the one `TypeTable::advsimd_name` spells its
    /// type with, so a diagnostic names the type as the program did.
    #[test]
    fn advsimd_names_round_trip() {
        let target = Target::from_triple("aarch64-unknown-linux-gnu").unwrap();
        let mut types = TypeTable::new(&target);
        for (name, shape) in AARCH64_BUILTIN_TYPES {
            if let Shape::AdvSimd { lane, count } = shape {
                let lane = lane.type_id(&types);
                let typ = types.advsimd_vector(lane, count);
                assert_eq!(types.advsimd_name(typ).as_deref(), Some(name));
            }
        }
    }
}
