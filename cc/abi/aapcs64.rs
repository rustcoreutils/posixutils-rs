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

/// The alignment AAPCS64 gives an argument of this type.
///
/// **Not `TypeTable::alignment`.** AAPCS64 has no notion of over-alignment: a
/// type's own `__attribute__((aligned(N)))` does not change how it is passed,
/// while alignment contributed by a *field* does. Measured against gcc, which
/// implements the same rule in `aarch64_function_arg_alignment`:
///
/// | type | `_Alignof` | stacked at |
/// |---|---|---|
/// | `struct { double a,b,c,d; }` | 8 | 8 |
/// | the same, `aligned(32)` | 32 | **8** -- own attribute ignored |
/// | `aligned(16) struct { long long a,b; }` | 16 | **8** -- ignored |
/// | `struct { long long a __attribute__((aligned(16))); long long b; }` | 16 | **16** -- member honoured |
/// | `struct { G16 g; }`, `G16` the `aligned(16)` struct above | 16 | **16** -- honoured as a field |
/// | `struct __attribute__((packed)) { __int128 x; }` | 1 | **8** -- packing honoured, floored at 8 |
/// | `typedef long long L32 __attribute__((aligned(32)))` | 32 | **8** -- attributed typedef ignored |
/// | `struct { __int128 x; }`, scalar `__int128` | 16 | 16 |
///
/// This governs both the stacked-argument offset (stage C.12-C.14) and the
/// even-register pairing (stage C.10), so every aarch64 site that lays out an
/// argument asks *this* and not `alignment()`.
///
/// The clamp to 16 is unobservable through natural alignment alone -- gcc
/// reports `_Alignof <= 16` for every non-attributed type on this target,
/// including a 32-byte vector -- but it is what gcc's own code does, so it is
/// written as a clamp rather than left implicit.
pub(crate) fn argument_alignment(types: &TypeTable, ty: TypeId) -> usize {
    let raw = match types.kind(ty) {
        // `composite.align` would carry the struct's own attribute; the
        // member-derived value is recorded separately for exactly this.
        TypeKind::Struct | TypeKind::Union => {
            types.composite(ty).map(|c| c.member_align).unwrap_or(1)
        }
        // An array is passed as its element type repeated, so it aligns as
        // one element does.
        TypeKind::Array => types
            .base_type(ty)
            .map(|elem| argument_alignment(types, elem))
            .unwrap_or(1),
        // `natural_alignment` already ignores `explicit_align`, which is where
        // an attributed *typedef* records itself.
        _ => types.natural_alignment(ty),
    };
    raw.clamp(8, 16)
}

/// The alignment of the stack slot an argument of type `ty` occupies.
///
/// [`argument_alignment`] for an argument passed by value; eight for a
/// composite over sixteen bytes, which stage C.4 replaces by a pointer to a
/// copy -- the slot holds that pointer, however aligned the composite's
/// members are. Both sides asked `argument_alignment` of the composite
/// instead, so a `struct { long a; long double b; }` argument's pointer was
/// placed on a sixteen-byte boundary where gcc puts it on eight: a gcc caller
/// crashed a c17 callee, and a gcc callee read a c17 caller's wrong slot.
pub(crate) fn stacked_argument_alignment(types: &TypeTable, ty: TypeId) -> usize {
    match Aapcs64Abi::new().classify_param(ty, types) {
        ArgClass::Indirect { .. } => 8,
        _ => argument_alignment(types, ty),
    }
}

/// Stage C.10: where an argument's run of `n` general registers starts.
///
/// An argument whose AAPCS64 alignment is 16 -- see [`argument_alignment`] --
/// begins at an *even* NGRN, so an odd one skips a register and leaves it
/// unused. That is the whole of the rule: it is the alignment that decides,
/// not the type, so a scalar `__int128`, a `struct { __int128 x; }` and a
/// struct whose first member carries `aligned(16)` all round, while the same
/// struct carrying `aligned(16)` on *itself* does not.
///
/// `None` means the run does not fit. Stage C.11 then sets NGRN to
/// `num_regs`, so every later argument is on the stack as well -- unlike
/// System V, which leaves the registers it did not fit in available.
pub(crate) fn gr_run_start(
    types: &TypeTable,
    ty: TypeId,
    ngrn: usize,
    n: usize,
    num_regs: usize,
) -> Option<usize> {
    let start = if argument_alignment(types, ty) == 16 {
        (ngrn + 1) & !1
    } else {
        ngrn
    };
    (start + n <= num_regs).then_some(start)
}

/// The stack slot a variadic argument of type `ty` occupies on Apple targets.
///
/// Returns `(bytes, align)`. Apple's arm64 convention puts *every* variadic
/// argument on the stack, one eight-byte granule at a time, and a type whose
/// alignment is 16 starts on a sixteen-byte boundary. The size is rounded up
/// to the granule, so a `__int128` occupies sixteen bytes rather than the one
/// slot every scalar used to get -- the caller wrote half of it and the
/// reader took half of it, which agreed with itself and with nothing else.
///
/// An aggregate too large to pass directly travels as a pointer to the
/// caller's copy, so that slot is eight bytes however aligned the object is.
///
/// **The type's full alignment, uncapped.** Above sixteen this is where clang
/// contradicts itself: its *caller* lowers an over-aligned aggregate through
/// LLVM's Darwin vararg convention, which stacks it a legalized element at a
/// time in eight-byte granules and never looks at the attribute, while its
/// `va_arg` rounds the cursor up to the type's own alignment. c17 follows
/// `va_arg` -- which means the caller has to realign its outgoing area to
/// match, since `%sp` is only guaranteed to sixteen.
pub(crate) fn darwin_va_slot(
    pos: crate::diag::Position,
    types: &TypeTable,
    ty: TypeId,
    target: &crate::target::Target,
) -> (i32, i32) {
    let abi = crate::abi::get_abi_for_conv(crate::abi::CallingConv::C, target);
    if matches!(abi.classify_param(ty, types), ArgClass::Indirect { .. }) {
        return (8, 8);
    }
    let bytes =
        (crate::abi::slot_bytes(types.size_bytes(ty).max(1), pos, "a variadic argument") + 7) & !7;
    (bytes, (types.alignment(ty) as i32).max(8))
}

/// Where the first variadic argument of a Darwin call sits, given where the
/// named arguments that overflowed their registers end.
///
/// Named arguments pack at their natural size ([`StackedArgs::Natural`]), so
/// they can end on any byte; every variadic slot starts on at least an
/// eight-byte boundary ([`darwin_va_slot`]). The caller lays the first one
/// out from here, and the callee's `va_start` points here.
pub(crate) fn darwin_va_area_start(named_end: i32) -> i32 {
    (named_end + 7) & !7
}

/// How a platform lays out the *named* arguments that did not fit in
/// registers.
///
/// This is the one rule both sides of a call ask: the caller's outgoing
/// argument area and the callee's incoming parameter offsets are laid out by
/// [`StackedArgs::slot`] and [`StackSlot::place`] and nothing else, so the two
/// cannot drift apart the way they could when each counted slots for itself.
/// Variadic arguments on Apple targets follow [`darwin_va_slot`] instead.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackedArgs {
    /// AAPCS64 §6.4.2 stages C.12-C.16: the next stacked-argument address is
    /// rounded up to `max(8, alignment)` and the argument's size to a
    /// multiple of eight, so every argument takes whole eight-byte granules.
    /// A `char` occupies eight bytes.
    Granules,
    /// Apple arm64 ("Writing ARM64 code for Apple platforms"): a scalar takes
    /// its natural size at its natural alignment -- a `char` one byte, a
    /// `short` two at an even offset -- and an HFA packs its elements at the
    /// element's alignment. A composite that is not an HFA is still rounded
    /// to eight-byte granules, because clang coerces it to an array of
    /// `i64` (or an `i128`) before it is placed, exactly as AAPCS64 does.
    Natural,
}

impl StackedArgs {
    /// The rule `target` lays its stacked named arguments out by.
    pub fn of(target: &crate::target::Target) -> Self {
        if target.os == crate::target::Os::MacOS {
            StackedArgs::Natural
        } else {
            StackedArgs::Granules
        }
    }

    /// The slot a named argument of type `ty` occupies once it is on the
    /// stack.
    ///
    /// Only asked of an argument that is passed at all: a zero-sized type
    /// (`ArgClass::Ignore`) never reaches the stack.
    pub fn slot(self, types: &TypeTable, ty: TypeId) -> StackSlot {
        let class = Aapcs64Abi::new().classify_param(ty, types);
        // An argument replaced by a pointer to a copy (stage C.4) is that
        // pointer on the stack, whatever the object is.
        if matches!(class, ArgClass::Indirect { .. }) {
            return StackSlot { bytes: 8, align: 8 };
        }
        let bytes = match class {
            ArgClass::Hfa { base, count } => base.bytes() * count as usize,
            _ => types.size_bytes(ty),
        };
        match self {
            StackedArgs::Natural => match class {
                ArgClass::Hfa { base, .. } => StackSlot {
                    bytes,
                    align: base.bytes(),
                },
                _ if types.is_aggregate_or_complex(ty) => StackedArgs::Granules.slot(types, ty),
                _ => StackSlot {
                    bytes,
                    align: types.natural_alignment(ty).max(1),
                },
            },
            StackedArgs::Granules => StackSlot {
                bytes: (bytes + 7) & !7,
                align: stacked_argument_alignment(types, ty),
            },
        }
    }
}

/// The bytes a stacked argument occupies and the alignment its start needs,
/// both relative to the base of the argument area.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StackSlot {
    pub bytes: usize,
    pub align: usize,
}

impl StackSlot {
    /// Place this slot at the first suitably aligned offset at or after `at`,
    /// returning where it starts and where the next argument may begin.
    ///
    /// The rounding applies to the offset within the argument area -- the
    /// NSAA of stage C -- not to any frame displacement the area happens to
    /// sit at.
    pub fn place(self, at: i64) -> (i64, i64) {
        let align = self.align as i64;
        let start = (at + align - 1) & !(align - 1);
        (start, start + self.bytes as i64)
    }
}

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
/// `long double _Complex` is an ordinary pair of doubles there, as clang
/// passes it.
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
pub struct Aapcs64Abi {
    /// Apple's variant: clang, Darwin's compiler, returns small integer
    /// vectors in V0 where gcc returns them in a general register.
    darwin: bool,
}

/// A GNU complex integer's argument or return class under AAPCS64.
///
/// It is a composite type with no floating members, so it is never an HFA:
/// §5.4.2 stage C.10 puts a composite of sixteen bytes or fewer in one X
/// register per eightbyte, and anything larger travels by reference. That
/// makes `_Complex int` X(n), `_Complex long` X(n)+X(n+1), and
/// `_Complex __int128` -- thirty-two bytes -- indirect.
///
/// Asked before every integer path, because a complex type carries its base's
/// kind: `_Complex long` satisfied `is_integer` and was handed a single X
/// register for a sixteen-byte value.
fn classify_complex_integer(types: &TypeTable, ty: TypeId, size_bits: u32) -> ArgClass {
    let size_bytes = types.size_bytes(ty);
    if size_bits > MAX_AGGREGATE_BITS {
        return ArgClass::Indirect {
            align: types.alignment(ty) as u32,
            size_bytes,
        };
    }
    ArgClass::Direct {
        classes: vec![RegClass::Integer; size_bits.div_ceil(64) as usize],
        size_bits,
    }
}

impl Aapcs64Abi {
    /// Create a new AAPCS64 ABI classifier.
    pub fn new() -> Self {
        Self { darwin: false }
    }

    /// The classifier for `os`'s variant of AAPCS64.
    pub fn for_os(os: crate::target::Os) -> Self {
        Self {
            darwin: os == crate::target::Os::MacOS,
        }
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

        // A vector member is one short vector (AAPCS64 4.1.2), whatever its
        // lanes: an aggregate of up to four of one size is a Homogeneous
        // Short-Vector Aggregate, passed as an HFA is.
        if types.is_vector(ty) {
            return match types.size_bytes(ty) {
                8 => Some((HfaBase::ShortVector64, 1)),
                16 => Some((HfaBase::ShortVector128, 1)),
                _ => None,
            };
        }

        // Only structs and arrays can be HFAs
        if !is_aggregate(kind) && kind != TypeKind::Array {
            return None;
        }

        // For arrays, the element contributes however many members it has.
        if kind == TypeKind::Array {
            let elem_ty = typ.base?;
            let len = typ.array_size?;
            let elem_kind = types.kind(elem_ty);
            // A scalar element is one member; an aggregate element is as
            // many as it flattens to. AAPCS64 5.9.5 counts a composite's
            // floating-point members through every level of nesting, and the
            // struct arm below already recurses -- only this one asked
            // whether the element was itself a floating type, so
            // `struct { struct { float x, y; } p[2]; }` answered "not an
            // HFA" and four floats went in general registers where gcc and
            // clang use s0-s3.
            let (base, per_element) = match self.is_hfa_base_type(elem_kind, elem_ty, types) {
                Some(base) => (base, 1u8),
                None => self.try_classify_hfa(elem_ty, types)?,
            };
            let total = len.checked_mul(per_element as usize)?;
            if (1..=MAX_HFA_ELEMENTS as usize).contains(&total) {
                return Some((base, total as u8));
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
        let size_bytes = types.size_bytes(ty);

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
                size_bytes,
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
    /// Stage B.4: a composite over sixteen bytes is replaced by a pointer to
    /// a caller-made copy.
    fn indirect_param_is_reference(&self) -> bool {
        true
    }

    /// gcc's convention ([`super::native_vector_carrier`]), and for a
    /// floating vector of four bytes or fewer -- `v1sf`, `v2hf`, `v1hf` --
    /// the compiler's own:
    ///
    /// - gcc gives one neither register class: it lays it on the stack in an
    ///   eight-byte slot and sends every later general-register argument
    ///   there too, leaving the V registers alone. Its carrier is a type of
    ///   its own, classed [`ArgClass::Stacked`].
    /// - clang on Darwin coerces one to `i32` -- a general register, or four
    ///   bytes of the stack once those run out -- whatever its size, so the
    ///   two-byte `v1hf` travels as an `unsigned int` too.
    fn vector_carrier(&self, vec: TypeId, types: &TypeTable) -> Option<TypeId> {
        if !types.is_small_float_vector(vec) {
            return super::native_vector_carrier(vec, types);
        }
        if self.darwin {
            Some(types.uint_id)
        } else {
            types.vector_stack_carrier(vec)
        }
    }

    /// A vector of four bytes or fewer is returned other than it is passed.
    ///
    /// gcc returns a floating one in a general register, as the unsigned
    /// integer of its size is -- W0, zero-extended at two bytes.
    ///
    /// clang on Darwin returns any one in V0: a single lane in its low bits
    /// -- as a `float` carrying those bits travels, or a `_Float16` for the
    /// two-byte `v1hf`, which LLVM returns in H0 -- and integer lanes, when
    /// there are several, widened to fill D0
    /// ([`Self::vector_return_widened`]). Floating lanes are not widened:
    /// LLVM legalizes `<2 x half>` by adding lanes, so `v2hf` is the low
    /// four bytes of D0, as a `float` carrying them is.
    fn vector_return_carrier(&self, vec: TypeId, types: &TypeTable) -> Option<TypeId> {
        if let Some(widened) = self.vector_return_widened(vec, types) {
            return self.vector_carrier(widened, types);
        }
        let bytes = types.size_bytes(vec);
        let small = types.vector_lanes(vec).is_some() && bytes <= 4;
        if self.darwin && small {
            return Some(if types.is_small_float_vector(vec) && bytes == 2 {
                types.float16_id
            } else {
                types.float_id
            });
        }
        if types.is_small_float_vector(vec) {
            return types.unsigned_of_size(bytes);
        }
        self.vector_carrier(vec, types)
    }

    /// On Darwin, an integer vector of several lanes and four bytes or fewer
    /// is returned with its lanes widened to fill D0: `v2hi` as two 32-bit
    /// lanes, `v4qi` as four 16-bit ones, as LLVM legalizes the type.
    fn vector_return_widened(&self, vec: TypeId, types: &TypeTable) -> Option<TypeId> {
        self.darwin.then(|| types.vector_widened(vec)).flatten()
    }

    fn classify_param(&self, ty: TypeId, types: &TypeTable) -> ArgClass {
        if types.is_vector(ty) {
            return match self.vector_carrier(ty, types) {
                Some(carrier) => self.classify_param(carrier, types),
                None => super::uncarried_vector_class(ty, types),
            };
        }
        if types.is_vector_stack_carrier(ty) {
            return ArgClass::Stacked {
                size_bytes: types.size_bytes(ty),
            };
        }
        let kind = types.kind(ty);
        let size_bits = types.size_bits(ty);
        let size_bytes = types.size_bytes(ty);

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

        // Complex integers, before any integer path: see
        // `classify_complex_integer`. The sub-32-bit path below would have
        // sign-extended `_Complex short` as a scalar, overwriting the
        // imaginary half.
        if types.is_complex_integer(ty) {
            return classify_complex_integer(types, ty, size_bits);
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
        if types.is_complex_float(ty) {
            if let Some(base) = complex_hfa_base(ty, types) {
                return ArgClass::Hfa { base, count: 2 };
            }
            return ArgClass::Indirect {
                align: 16,
                size_bytes,
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
                size_bytes,
            }
        }
    }

    fn classify_return(&self, ty: TypeId, types: &TypeTable) -> ArgClass {
        if types.is_vector(ty) {
            return match self.vector_return_carrier(ty, types) {
                Some(carrier) => self.classify_return(carrier, types),
                None => super::uncarried_vector_class(ty, types),
            };
        }
        let kind = types.kind(ty);
        let size_bits = types.size_bits(ty);
        let size_bytes = types.size_bytes(ty);

        // Void return
        if kind == TypeKind::Void {
            return ArgClass::Ignore;
        }

        // Complex integers, before any integer path: see
        // `classify_complex_integer`.
        if types.is_complex_integer(ty) {
            return classify_complex_integer(types, ty, size_bits);
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
        if types.is_complex_float(ty) {
            if let Some(base) = complex_hfa_base(ty, types) {
                return ArgClass::Hfa { base, count: 2 };
            }
            return ArgClass::Indirect {
                align: 16,
                size_bytes,
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
                    size_bytes,
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
                size_bytes,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::target::{Arch, Os, Target};
    use crate::types::{CompositeType, MemberAlign, StructMember, Type};

    /// `argument_alignment` follows the members, not the type's own attribute.
    ///
    /// Every row here was read off gcc's own aarch64 output; the doc comment on
    /// the function records the offsets. The two that a naive "walk the
    /// members" implementation gets wrong are the packed struct (whose `#pragma pack`
    /// cap is recorded nowhere but `member_align`) and the attributed typedef
    /// (whose attribute lives in `explicit_align`, not in a composite).
    #[test]
    fn argument_alignment_follows_the_members() {
        fn member(typ: TypeId, written: Option<u32>) -> StructMember {
            StructMember {
                name: crate::strings::StringId::default(),
                typ,
                offset: 0,
                bit_width: None,
                bit_offset: None,
                access_bytes: None,
                align: MemberAlign {
                    written,
                    packed: false,
                },
            }
        }
        // `align` is what the type reports to the language; `member_align` is
        // what the members require. An `aligned(N)` attribute raises only the
        // first, which is the whole distinction being tested.
        fn composite(
            types: &mut TypeTable,
            members: Vec<StructMember>,
            size: usize,
            align: usize,
            member_align: usize,
        ) -> TypeId {
            types.intern(Type::struct_type(CompositeType {
                tag: None,
                members,
                enum_constants: vec![],
                size,
                align,
                member_align,
                is_complete: true,
                transparent: false,
                anon_id: None,
                tag_type: None,
            }))
        }

        let mut types = TypeTable::new(&Target::new(Arch::Aarch64, Os::Linux));
        let d = types.double_id;
        let ll = types.longlong_id;
        let i128 = types.int128_id;

        // Four doubles: the members want 8, and an `aligned(32)` attribute on
        // the struct does not change what the ABI passes.
        let plain = composite(&mut types, vec![member(d, None); 4], 32, 8, 8);
        let over = composite(&mut types, vec![member(d, None); 4], 32, 32, 8);
        // A member's own alignment does count.
        let member_aligned = composite(
            &mut types,
            vec![member(ll, Some(16)), member(ll, None)],
            16,
            16,
            16,
        );
        // Packed: `member_align` is the only record that the pack cap ever
        // applied, and the result floors at 8 rather than at 1.
        let packed = composite(&mut types, vec![member(ll, None)], 8, 1, 1);
        // Naturally 16-aligned composite.
        let nat16 = composite(&mut types, vec![member(i128, None)], 16, 16, 16);
        // An attributed typedef records itself in `explicit_align`, which
        // `natural_alignment` already ignores.
        let mut attributed = types.get(ll).clone();
        attributed.explicit_align = Some(32);
        let attributed = types.intern(attributed);

        assert_eq!(argument_alignment(&types, plain), 8);
        assert_eq!(
            argument_alignment(&types, over),
            8,
            "a struct's own aligned(32) must not reach the argument area"
        );
        assert_eq!(argument_alignment(&types, member_aligned), 16);
        assert_eq!(argument_alignment(&types, packed), 8);
        assert_eq!(argument_alignment(&types, nat16), 16);
        assert_eq!(argument_alignment(&types, i128), 16);
        assert_eq!(
            argument_alignment(&types, attributed),
            8,
            "an attributed typedef must not reach the argument area either"
        );
    }

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
            align: MemberAlign::NATURAL,
        };
        let m0 = member(d);
        let m1 = member(d);
        let u = types.intern(Type::union_type(CompositeType {
            tag: None,
            members: vec![m0, m1],
            enum_constants: vec![],
            size: 8,
            align: 8,
            member_align: 8,
            is_complete: true,
            transparent: false,
            anon_id: None,
            tag_type: None,
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
            member_align: 8,
            is_complete: true,
            transparent: false,
            anon_id: None,
            tag_type: None,
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

    /// A struct of `members` at the given offsets, sized and aligned as C
    /// lays it out.
    fn record(types: &mut TypeTable, members: &[(TypeId, usize)], size: usize) -> TypeId {
        let align = members
            .iter()
            .map(|(t, _)| types.alignment(*t))
            .max()
            .unwrap_or(1);
        types.intern(Type::struct_type(CompositeType {
            tag: None,
            members: members
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
                .collect(),
            enum_constants: vec![],
            size,
            align,
            member_align: align,
            is_complete: true,
            transparent: false,
            anon_id: None,
            tag_type: None,
        }))
    }

    /// The slot each kind of named argument takes once it is on the stack,
    /// under AAPCS64's granules and under Apple's natural packing.
    ///
    /// Apple's rule ("Writing ARM64 code for Apple platforms"): a scalar at
    /// its own size and alignment, an HFA at its element's, and a composite
    /// that is not an HFA in eight-byte granules -- clang coerces it to
    /// `i64`s before it is placed. Anything over sixteen bytes is a pointer.
    #[test]
    fn stacked_argument_slots_per_platform() {
        for os in [Os::Linux, Os::MacOS] {
            let target = Target::new(Arch::Aarch64, os);
            let mut types = TypeTable::new(&target);
            let (c, sh, f, d, h, l) = (
                types.char_id,
                types.short_id,
                types.float_id,
                types.double_id,
                types.float16_id,
                types.long_id,
            );
            let s3 = record(&mut types, &[(c, 0), (c, 1), (c, 2)], 3);
            let s5 = record(&mut types, &[(c, 0), (c, 1), (c, 2), (c, 3), (c, 4)], 5);
            let s6 = record(&mut types, &[(sh, 0), (sh, 2), (sh, 4)], 6);
            let s16 = record(&mut types, &[(l, 0), (l, 8)], 16);
            let big = record(&mut types, &[(l, 0), (l, 8), (l, 16)], 24);
            let f1 = record(&mut types, &[(f, 0)], 4);
            let f3 = record(&mut types, &[(f, 0), (f, 4), (f, 8)], 12);
            let d2 = record(&mut types, &[(d, 0), (d, 8)], 16);
            let h3 = record(&mut types, &[(h, 0), (h, 2), (h, 4)], 6);
            let ptr = types.void_ptr_id;

            let slot = |bytes, align| StackSlot { bytes, align };
            let darwin = os == Os::MacOS;
            // (type, what, Apple's slot); AAPCS64 rounds each up to granules.
            let rows = [
                (types.bool_id, "_Bool", slot(1, 1)),
                (c, "char", slot(1, 1)),
                (sh, "short", slot(2, 2)),
                (types.int_id, "int", slot(4, 4)),
                (l, "long", slot(8, 8)),
                (ptr, "void *", slot(8, 8)),
                (h, "_Float16", slot(2, 2)),
                (f, "float", slot(4, 4)),
                (d, "double", slot(8, 8)),
                (types.int128_id, "__int128", slot(16, 16)),
                (s3, "struct of 3 chars", slot(8, 8)),
                (s5, "struct of 5 chars", slot(8, 8)),
                (s6, "struct of 3 shorts", slot(8, 8)),
                (s16, "struct of 2 longs", slot(16, 8)),
                (big, "24-byte struct (by reference)", slot(8, 8)),
                (f1, "struct { float; }", slot(4, 4)),
                (f3, "HFA of 3 floats", slot(12, 4)),
                (d2, "HFA of 2 doubles", slot(16, 8)),
                (h3, "HFA of 3 _Float16", slot(6, 2)),
                (types.complex_float_id, "float _Complex", slot(8, 4)),
                (types.complex_double_id, "double _Complex", slot(16, 8)),
            ];
            let stacked = StackedArgs::of(&target);
            for (ty, what, apple) in rows {
                let want = if darwin {
                    apple
                } else {
                    slot((apple.bytes + 7) & !7, apple.align.clamp(8, 16))
                };
                assert_eq!(stacked.slot(&types, ty), want, "{what} on {os:?}");
            }

            // `long double` is binary128 on Linux and a double on Apple.
            let ld = stacked.slot(&types, types.longdouble_id);
            assert_eq!(ld, if darwin { slot(8, 8) } else { slot(16, 16) });

            // Placed one after another from the area's base.
            let mut at = 0;
            let offsets: Vec<i64> = [c, f3, c, s3, sh, d]
                .into_iter()
                .map(|t| {
                    let (start, end) = stacked.slot(&types, t).place(at);
                    at = end;
                    start
                })
                .collect();
            if darwin {
                assert_eq!(offsets, [0, 4, 16, 24, 32, 40]);
                assert_eq!(at, 48);
            } else {
                assert_eq!(offsets, [0, 8, 24, 32, 40, 48]);
                assert_eq!(at, 56);
            }
        }
    }

    /// A Darwin callee's `va_list` starts where its caller began laying the
    /// variadic arguments out: past the packed named ones, on a granule.
    #[test]
    fn darwin_variadics_start_on_a_granule() {
        assert_eq!(darwin_va_area_start(0), 0);
        assert_eq!(darwin_va_area_start(1), 8);
        assert_eq!(darwin_va_area_start(8), 8);
        assert_eq!(darwin_va_area_start(25), 32);
    }

    #[test]
    fn test_abi_creation() {
        assert!(!Aapcs64Abi::new().darwin);
        assert!(!Aapcs64Abi::for_os(Os::Linux).darwin);
        assert!(Aapcs64Abi::for_os(Os::MacOS).darwin);
    }

    /// A complex value is a two-member HFA whose element width — not whose
    /// type *name* — decides the register class. Apple's `long double` is a
    /// 64-bit double, so `long double _Complex` belongs in two D registers
    /// like any other pair of doubles, which is what clang does.
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
