//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Platform-specific calling-convention classification for function parameters
// and return values: the contract between the frontend (linearizer) and the
// backend (code generator).
//
// - System V AMD64 (x86-64 Linux/BSD/macOS)
// - Microsoft x64 (x86-64, `__attribute__((ms_abi))`)
// - AAPCS64 (AArch64 Linux/macOS)
//

// Crate-visible rather than `pub`: the aarch64 backend reaches this module's
// layout helpers by path, so it cannot be private like `sysv_amd64` beside it,
// but nothing outside the crate has any business with them. What leaves the
// crate is the `pub use` below.
pub(crate) mod aapcs64;
mod sysv_amd64;
mod win64;

pub use aapcs64::Aapcs64Abi;
pub use sysv_amd64::{
    param_is_ignored, param_is_memory_class, sse_struct_regs, struct_param_classes, SysVAmd64Abi,
};
pub use win64::{Win64Abi, WIN64_POSITION_BYTES, WIN64_REG_POSITIONS, WIN64_SHADOW_BYTES};

use crate::target::{Arch, Target};
use crate::types::{TypeId, TypeKind, TypeTable};

/// An object's byte count as the backends' frame arithmetic needs it.
///
/// Every stack slot and every stacked-argument slot in both backends is
/// addressed by a signed 32-bit displacement, so a size has to become an `i32`
/// before that arithmetic runs. Fifteen sites used to spell that `as i32`,
/// which wraps: 3000000000 came out as -1294967296, the `size.max(8)` that
/// follows gave the object an eight-byte slot, and the function's whole frame
/// was `subq $32, %rsp` with the array laid over it.
///
/// [`TypeTable::MAX_STACK_OBJECT_BYTES`] is the bound, and
/// `Parser::check_stack_object_size` refuses a *declaration* that passes it.
/// This is the backstop for the objects that check cannot see, and each one is
/// reachable from C source:
///
/// - the `__sret` local a call to a function returning a large aggregate
///   allocates -- a by-value *return* type is not a declared object;
/// - a compound literal's anonymous local.
///
/// Reachable is why the message is the user's and carries no `internal error:`
/// prefix: a programmer can write either, and a bug report is not what either
/// one wants. A diagnostic and not `.expect()`, because an ICE is not a
/// diagnostic. No function in `arch/` or `abi/` returns `Result`, so
/// [`crate::diag::error_args`] is the channel -- the same shape `FrameBase::of`
/// uses in both backends.
///
/// The placeholder is one eightbyte: non-zero, so the `& !(align - 1)`
/// roundings and the `while done < bytes` copy loops that consume it still
/// terminate, and small enough that nothing it feeds overflows in turn.
pub fn slot_bytes(bytes: usize, pos: crate::diag::Position, what: &str) -> i32 {
    if bytes > TypeTable::MAX_STACK_OBJECT_BYTES {
        crate::diag::error_args(
            pos,
            "size of {0} is {1} bytes, past the {2} bytes a stack frame slot can address",
            &[
                what,
                &bytes.to_string(),
                &TypeTable::MAX_STACK_OBJECT_BYTES.to_string(),
            ],
        );
        return 8;
    }
    bytes as i32
}

// Register Classification

/// Classification of a single eightbyte (x86-64) or register slot.
///
/// Based on AMD64 ABI Section 3.2.3 classification classes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum RegClass {
    /// No data or padding (zero-sized type)
    #[default]
    NoClass,
    /// Integer or pointer type - uses GP register
    Integer,
    /// Floating-point type - uses SSE/FP register
    Sse,
    /// Must be passed on stack (too large, unaligned, or register exhausted)
    Memory,
}

impl RegClass {
    /// Merge two register classes according to AMD64-ABI 3.2.3p2 rules.
    ///
    /// The merging is used when determining the classification of struct
    /// fields that overlap the same eightbyte.
    pub fn merge(self, other: RegClass) -> RegClass {
        use RegClass::*;
        match (self, other) {
            // (a) If both classes are equal, this is the resulting class
            (a, b) if a == b => a,
            // (b) If one is NO_CLASS, the other is the result
            (NoClass, b) => b,
            (a, NoClass) => a,
            // (c) If one is MEMORY, the result is MEMORY
            (Memory, _) | (_, Memory) => Memory,
            // (d) If one is INTEGER, the result is INTEGER
            (Integer, _) | (_, Integer) => Integer,
            // (e) X87/X87UP/COMPLEX_X87 -> MEMORY (we don't support X87)
            // (f) Otherwise, SSE
            _ => Sse,
        }
    }
}

// Argument Classification

/// Base type for Homogeneous Floating-Point Aggregate (HFA) on AAPCS64.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HfaBase {
    /// 16-bit half precision (`_Float16`).
    Float16,
    /// 32-bit float
    Float32,
    /// 64-bit double
    Float64,
    /// 128-bit IEEE binary128 (`long double` on aarch64/Linux).
    ///
    /// AAPCS64 treats a homogeneous aggregate of these like any other, one
    /// element per Q register -- so `long double _Complex` goes in q0/q1
    /// rather than indirectly, which is what gcc does.
    Float128,
}

impl HfaBase {
    /// The width of one element, in bytes.
    pub fn bytes(self) -> usize {
        match self {
            HfaBase::Float16 => 2,
            HfaBase::Float32 => 4,
            HfaBase::Float64 => 8,
            HfaBase::Float128 => 16,
        }
    }
}

/// How an argument or return value should be passed.
///
/// This is the primary classification result used by the backend to
/// determine register/stack allocation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArgClass {
    /// Pass directly in register(s).
    /// For x86-64: classification per eightbyte determines GP vs SSE.
    /// For AArch64: simple scalars and small composites.
    Direct {
        /// Classification per eightbyte/slot (usually 1-2 elements)
        classes: Vec<RegClass>,
        /// Size in bits of the value
        size_bits: u32,
    },
    /// The argument travels in memory rather than in registers.
    ///
    /// Used for large structs that don't fit in registers. On SysV amd64 the
    /// MEMORY class means the caller pushes the object *by value*, so the
    /// payload below is the object's own size.
    Indirect {
        /// Alignment requirement in bytes
        align: u32,
        /// Size in **bytes**: an object size, not a value width. Deriving it
        /// from [`crate::types::TypeTable::size_bits`] answered 536870911 for
        /// any aggregate past `u32::MAX` bits, which sized the stacked
        /// argument short.
        size_bytes: usize,
    },
    /// Homogeneous Floating-Point Aggregate (AAPCS64 specific).
    /// Struct with 1-4 identical float/double members passed in FP registers.
    Hfa {
        /// Base type (float or double)
        base: HfaBase,
        /// Number of elements (1-4)
        count: u8,
    },
    /// Extend small integer to register width.
    /// Used for char, short, etc.
    Extend {
        /// True for signed extension, false for zero extension
        signed: bool,
        /// Original size in bits
        size_bits: u32,
    },
    /// x87 FPU return (long double returned in ST(0)).
    /// Only used for return values on x86-64.
    X87 {
        /// Size in bits (80 for long double, stored in 128-bit slot)
        size_bits: u32,
    },
    /// Zero-sized type, ignored in parameter passing.
    Ignore,
}

impl ArgClass {
    /// Whether an aggregate of this class travels in registers that the
    /// backend fills from, or spills to, the aggregate's memory: any all-SSE
    /// classification (two eightbytes, or one of sixteen), an HFA, or two
    /// eightbytes of any classes -- AAPCS64 C.10 and System V AMD64 3.2.3
    /// agree on the pair.
    ///
    /// The one statement both ends of a call read: the caller keeps such an
    /// argument's aggregate type, and the callee stores the registers into
    /// its parameter's local.
    pub fn is_register_aggregate(&self) -> bool {
        match self {
            ArgClass::Direct { classes, .. } => {
                classes.len() == 2
                    || (!classes.is_empty() && classes.iter().all(|c| *c == RegClass::Sse))
            }
            ArgClass::Hfa { .. } => true,
            _ => false,
        }
    }
}

// Calling Convention

/// The calling convention of a function type.
///
/// A property of the *type*, as gcc has it: a call through a pointer, a call
/// to a prototype and a definition are each classified by the convention of
/// the function type they name, and two function types that differ only in
/// convention are incompatible. `__attribute__((ms_abi))` sets it on an x86-64
/// target; `sysv_abi` names the x86-64 default, which is [`CallingConv::C`].
/// On aarch64 both attributes are ignored, as gcc ignores them there, so a
/// function type is never anything but `C`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum CallingConv {
    /// The target's own convention: System V AMD64 or AAPCS64.
    #[default]
    C,
    /// The Microsoft x64 convention (`__attribute__((ms_abi))`).
    Win64,
}

impl CallingConv {
    /// The convention a call through `callee` uses: the function type's, or
    /// the pointed-to function type's for a call through a pointer.
    ///
    /// The one place a call site decides its convention. A callee of any other
    /// type -- an implicit declaration, a diagnosed expression -- is called
    /// with the target's own.
    pub fn of_callee(callee: TypeId, types: &TypeTable) -> CallingConv {
        types
            .callee_function_type(callee)
            .map_or(CallingConv::C, |func| types.get(func).conv)
    }
}

// ABI Trait

/// Trait for platform-specific ABI classification.
///
/// Implementations provide the rules for classifying C types according
/// to the platform's calling convention.
pub trait Abi {
    /// Classify a parameter type.
    fn classify_param(&self, ty: TypeId, types: &TypeTable) -> ArgClass;

    /// Classify a return type.
    fn classify_return(&self, ty: TypeId, types: &TypeTable) -> ArgClass;

    /// What an `ArgClass::Indirect` *parameter* travels as.
    ///
    /// The class means "not in registers", and the two ABIs mean different
    /// things by it. System V AMD64 MEMORY class puts the argument's bytes in
    /// the outgoing argument area, so the callee's parameter is the caller's
    /// copy by construction. AAPCS64 stage B.4 instead passes a *pointer*,
    /// and the memory it points to must be a copy the caller made: the callee
    /// owns it and may write to it.
    fn indirect_param_is_reference(&self) -> bool;
}

// ABI Factory

/// Get the appropriate ABI implementation for a target.
pub fn get_abi(target: &Target) -> Box<dyn Abi> {
    match target.arch {
        Arch::X86_64 => Box::new(SysVAmd64Abi::new()),
        Arch::Aarch64 => Box::new(Aapcs64Abi::new()),
    }
}

/// The classifier for a function type's calling convention.
///
/// `Win64` exists only on x86-64: the parser records it nowhere else, and
/// the AAPCS64 classifier answers for any convention on aarch64.
pub fn get_abi_for_conv(conv: CallingConv, target: &Target) -> Box<dyn Abi> {
    match (conv, target.arch) {
        (CallingConv::Win64, Arch::X86_64) => Box::new(Win64Abi::new()),
        _ => get_abi(target),
    }
}

// Helper Functions

/// Check if a type is a struct or union.
fn is_aggregate(kind: TypeKind) -> bool {
    matches!(kind, TypeKind::Struct | TypeKind::Union)
}

/// Check if a type is a floating-point type.
///
/// `_Float16` belongs here for the same reason the others do: System V puts it
/// in an SSE register. Leaving it out sent every eightbyte holding one to the
/// default arm, which answers MEMORY, so `struct { _Float16 h; }` was returned
/// through a hidden pointer that nobody ever wrote -- it read back as zero.
/// A scalar `_Float16` fell to the size-based default and was classified
/// INTEGER, so the argument accounting charged it a general register while the
/// backend put it in an SSE one.
fn is_float(kind: TypeKind) -> bool {
    matches!(
        kind,
        TypeKind::Float
            | TypeKind::Double
            | TypeKind::LongDouble
            | TypeKind::Float16
            | TypeKind::Float128
    )
}

/// Check if a type is an integer type (including char, bool, enums).
/// Note: signedness is tracked via TypeModifiers::UNSIGNED, not separate TypeKind variants.
fn is_integer(kind: TypeKind) -> bool {
    matches!(
        kind,
        TypeKind::Char
            | TypeKind::Short
            | TypeKind::Int
            | TypeKind::Long
            | TypeKind::LongLong
            | TypeKind::Int128
            | TypeKind::Bool
            | TypeKind::Enum
    )
}

/// Check if a type is a pointer type.
fn is_pointer(kind: TypeKind) -> bool {
    kind == TypeKind::Pointer
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regclass_merge() {
        use RegClass::*;
        // Same class returns that class
        assert_eq!(Integer.merge(Integer), Integer);
        assert_eq!(Sse.merge(Sse), Sse);

        // NoClass with anything returns the other
        assert_eq!(NoClass.merge(Integer), Integer);
        assert_eq!(Sse.merge(NoClass), Sse);

        // Memory dominates
        assert_eq!(Memory.merge(Integer), Memory);
        assert_eq!(Sse.merge(Memory), Memory);

        // Integer dominates over SSE
        assert_eq!(Integer.merge(Sse), Integer);
        assert_eq!(Sse.merge(Integer), Integer);
    }

    #[test]
    fn test_sysv_amd64_basic_types() {
        use crate::types::TypeTable;

        let types = TypeTable::new(&Target::host());
        let abi = SysVAmd64Abi::new();

        // Integer types -> INTEGER class
        let int_class = abi.classify_param(types.int_id, &types);
        assert!(
            matches!(int_class, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Integer])
        );

        let long_class = abi.classify_param(types.long_id, &types);
        assert!(
            matches!(long_class, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Integer])
        );

        // Pointer -> INTEGER class
        let ptr_id = types.pointer_to(types.int_id);
        let ptr_class = abi.classify_param(ptr_id, &types);
        assert!(
            matches!(ptr_class, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Integer])
        );

        // Float/double -> SSE class
        let float_class = abi.classify_param(types.float_id, &types);
        assert!(
            matches!(float_class, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Sse])
        );

        let double_class = abi.classify_param(types.double_id, &types);
        assert!(
            matches!(double_class, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Sse])
        );

        // Void -> Ignore
        let void_class = abi.classify_param(types.void_id, &types);
        assert!(matches!(void_class, ArgClass::Ignore));
    }

    /// Each complex type as psABI 3.2.3 classifies it for a parameter, which
    /// is what `va_arg` now reads it by. `_Float128 _Complex` is four
    /// eightbytes (SSE, SSEUP, SSE, SSEUP) that the post-merger cleanup makes
    /// MEMORY, as gcc passes it and as c17's own call lowering
    /// (`complex_sse_regs`) already did.
    #[test]
    fn test_sysv_amd64_complex_params() {
        use crate::types::TypeTable;

        let types = TypeTable::new(&Target::new(
            crate::target::Arch::X86_64,
            crate::target::Os::Linux,
        ));
        let abi = SysVAmd64Abi::new();
        let sse = |n| vec![RegClass::Sse; n];
        for (base, want) in [
            (types.float16_id, Some(sse(1))),
            (types.float_id, Some(sse(1))),
            (types.double_id, Some(sse(2))),
            (types.longdouble_id, None),
            (types.float128_id, None),
        ] {
            let got = abi.classify_param(types.make_complex(base), &types);
            match want {
                Some(classes) => assert!(
                    matches!(&got, ArgClass::Direct { classes: c, .. } if *c == classes),
                    "{base:?}: {got:?}"
                ),
                None => assert!(
                    matches!(
                        got,
                        ArgClass::Indirect {
                            align: 16,
                            size_bytes: 32
                        }
                    ),
                    "{base:?}: {got:?}"
                ),
            }
        }
    }

    #[test]
    fn test_sysv_amd64_small_integers() {
        use crate::types::TypeTable;

        let types = TypeTable::new(&Target::host());
        let abi = SysVAmd64Abi::new();

        // char (8 bits) needs extension
        let char_class = abi.classify_param(types.char_id, &types);
        assert!(matches!(char_class, ArgClass::Extend { size_bits: 8, .. }));

        // short (16 bits) needs extension
        let short_class = abi.classify_param(types.short_id, &types);
        assert!(matches!(
            short_class,
            ArgClass::Extend { size_bits: 16, .. }
        ));
    }

    #[test]
    fn test_sysv_amd64_return_values() {
        use crate::types::TypeTable;

        let types = TypeTable::new(&Target::host());
        let abi = SysVAmd64Abi::new();

        // Integer return -> INTEGER
        let int_ret = abi.classify_return(types.int_id, &types);
        assert!(
            matches!(int_ret, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Integer])
        );

        // Float return -> SSE
        let float_ret = abi.classify_return(types.float_id, &types);
        assert!(
            matches!(float_ret, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Sse])
        );

        // Void return -> Ignore
        let void_ret = abi.classify_return(types.void_id, &types);
        assert!(matches!(void_ret, ArgClass::Ignore));
    }

    #[test]
    fn test_aapcs64_basic_types() {
        use crate::types::TypeTable;

        let types = TypeTable::new(&Target::host());
        let abi = Aapcs64Abi::new();

        // Integer types -> INTEGER class (in X registers)
        let int_class = abi.classify_param(types.int_id, &types);
        assert!(
            matches!(int_class, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Integer])
        );

        // Float/double -> SSE class (in V registers)
        let float_class = abi.classify_param(types.float_id, &types);
        assert!(
            matches!(float_class, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Sse])
        );

        let double_class = abi.classify_param(types.double_id, &types);
        assert!(
            matches!(double_class, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Sse])
        );
    }

    #[test]
    fn test_get_abi_factory() {
        use crate::target::{Arch, Os, Target};

        // x86-64 should get SysV ABI
        let x86_target = Target::new(Arch::X86_64, Os::Linux);
        let _x86_abi = get_abi(&x86_target);

        // AArch64 Linux should get AAPCS64
        let aarch64_target = Target::new(Arch::Aarch64, Os::Linux);
        let _aarch64_abi = get_abi(&aarch64_target);

        // AArch64 macOS also gets AAPCS64
        let darwin_target = Target::new(Arch::Aarch64, Os::MacOS);
        let _darwin_abi = get_abi(&darwin_target);
    }

    /// The register aggregates: all-SSE, any two eightbytes, an HFA.
    #[test]
    fn test_is_register_aggregate() {
        use RegClass::*;
        let direct = |classes: Vec<RegClass>| ArgClass::Direct {
            classes,
            size_bits: 128,
        };
        assert!(direct(vec![Sse]).is_register_aggregate());
        assert!(direct(vec![Sse, Sse]).is_register_aggregate());
        assert!(direct(vec![Integer, Integer]).is_register_aggregate());
        assert!(direct(vec![Integer, Sse]).is_register_aggregate());
        assert!(ArgClass::Hfa {
            base: HfaBase::Float32,
            count: 1
        }
        .is_register_aggregate());
        assert!(!direct(vec![]).is_register_aggregate());
        assert!(!direct(vec![Integer]).is_register_aggregate());
        assert!(!ArgClass::Indirect {
            align: 8,
            size_bytes: 16
        }
        .is_register_aggregate());
        assert!(!ArgClass::Ignore.is_register_aggregate());
    }
}
