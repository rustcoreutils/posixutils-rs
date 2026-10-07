//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Architecture-specific predefined macros and code generators
//

pub const DEFAULT_LIR_BUFFER_CAPACITY: usize = 5000;

pub mod aarch64;
pub mod asm_constraints;
pub mod codegen;
pub mod dwarf;
pub mod lir;
pub mod mapping;
pub mod regalloc;
pub mod simd;
pub mod stack_protect;
pub mod x86_64;

// Re-export inline asm support traits and functions
pub use codegen::{substitute_asm_operands, AsmOperandFormatter, AsmOperandSlot};

use crate::target::{Arch, CharSignedness, IntType, Os, Target};

/// The first source position this function's instructions carry, for a backend
/// diagnostic that has no better one.
///
/// `ir::Function` records no position of its own. This is the recovery
/// `CodeGenBase::emit_function_entry_loc` already performs for the entry `.loc`,
/// and a widening of the `Opcode::Asm`-only scan both `FrameBase::of`s do: any
/// instruction carrying a position is nearer the object than
/// `Position::default()`, which names no file at all.
pub(crate) fn func_pos(func: &crate::ir::Function) -> crate::diag::Position {
    func.blocks
        .iter()
        .flat_map(|b| b.insns.iter())
        .find_map(|i| i.pos)
        .unwrap_or_default()
}

/// Get architecture-specific predefined macros as (name, value) pairs
pub fn get_arch_macros(target: &Target) -> Vec<(&'static str, Option<&'static str>)> {
    let mut macros = vec![
        // Common architecture macros based on type sizes
        ("__CHAR_BIT__", Some("8")),
        ("__SIZEOF_POINTER__", Some("8")),
        // The integer sizes are with the rest of the integer facts, in
        // `get_integer_macros`.
        ("__SIZEOF_FLOAT__", Some("4")),
        ("__SIZEOF_DOUBLE__", Some("8")),
    ];

    // __CHAR_UNSIGNED__ is defined exactly when plain `char` is unsigned, and
    // is the only place that fact reaches <limits.h>'s CHAR_MIN/CHAR_MAX. An
    // empty-body #define still satisfies #ifdef, so a signed target must not
    // get the macro at all.
    if target.plain_char == CharSignedness::Unsigned {
        macros.push(("__CHAR_UNSIGNED__", Some("1")));
    }

    // LP64 macros only on LP64 targets (Unix), not on LLP64 (Windows)
    if target.long_width == 64 {
        macros.push(("__LP64__", Some("1")));
        macros.push(("_LP64", Some("1")));
    }

    // Architecture-specific
    match target.arch {
        Arch::X86_64 => {
            macros.extend(x86_64::get_macros());
        }
        Arch::Aarch64 => {
            macros.extend(aarch64::get_macros());
            if target.os == Os::MacOS {
                macros.extend(aarch64::get_darwin_macros());
            }
        }
    }

    macros
}

/// Which facts about an integer typedef its predefines state.
///
/// The set per typedef is gcc's, plus the `_WIDTH__`, `_C_SUFFIX__` and
/// `_FMT*__` macros clang adds, which c17 has always provided.
#[derive(Clone, Copy, Default)]
struct Describe {
    max: bool,
    /// Only the three typedefs whose minimum `<stdint.h>` cannot write as
    /// `-MAX - 1` or `0` without knowing the signedness get one.
    min: bool,
    width: bool,
    suffix: bool,
    fmt: bool,
}

/// One of the library's integer typedefs: the `NAME` in `__NAME_TYPE__`, and
/// the type the target gives it.
struct Typedef {
    name: String,
    ty: IntType,
    describe: Describe,
}

/// Every integer typedef the predefines describe, with the target's type for
/// each. The types come from [`Target`] alone; nothing here restates one.
fn integer_typedefs(target: &Target) -> Vec<Typedef> {
    let signed = Describe {
        max: true,
        width: true,
        fmt: true,
        ..Describe::default()
    };
    let unsigned = Describe {
        width: false,
        ..signed
    };
    let with_suffix = |d: Describe| Describe { suffix: true, ..d };
    let bounded = Describe {
        max: true,
        min: true,
        width: true,
        ..Describe::default()
    };

    let mut out = Vec::new();
    let mut add =
        |name: String, ty: IntType, describe: Describe| out.push(Typedef { name, ty, describe });
    for bits in [8, 16, 32, 64] {
        let exact = target.exact_int_type(bits);
        add(format!("INT{bits}"), exact, with_suffix(signed));
        add(
            format!("UINT{bits}"),
            exact.to_unsigned(),
            with_suffix(unsigned),
        );
    }
    for bits in [8, 16, 32, 64] {
        let least = target.least_int_type(bits);
        add(format!("INT_LEAST{bits}"), least, signed);
        add(format!("UINT_LEAST{bits}"), least.to_unsigned(), unsigned);
    }
    for bits in [8, 16, 32, 64] {
        let fast = target.fast_int_type(bits);
        add(format!("INT_FAST{bits}"), fast, signed);
        add(format!("UINT_FAST{bits}"), fast.to_unsigned(), unsigned);
    }
    let intptr = target.intptr_type();
    add("INTPTR".into(), intptr, signed);
    add("UINTPTR".into(), intptr.to_unsigned(), unsigned);
    let intmax = target.intmax_type();
    add("INTMAX".into(), intmax, with_suffix(signed));
    add(
        "UINTMAX".into(),
        intmax.to_unsigned(),
        with_suffix(Describe {
            width: true,
            ..unsigned
        }),
    );
    add("PTRDIFF".into(), target.ptrdiff_type(), signed);
    add(
        "SIZE".into(),
        target.size_type(),
        Describe {
            width: true,
            ..unsigned
        },
    );
    add("WCHAR".into(), target.wchar_type(), bounded);
    add("WINT".into(), target.wint_type(), bounded);
    add("SIG_ATOMIC".into(), target.sig_atomic_type(), bounded);
    add("CHAR16".into(), target.char16_type(), Describe::default());
    add("CHAR32".into(), target.char32_type(), Describe::default());
    out
}

/// The `__*_TYPE__` macros: each typedef's type, spelled as gcc spells it.
pub fn get_type_macros(target: &Target) -> Vec<(String, &'static str)> {
    integer_typedefs(target)
        .into_iter()
        .map(|t| (format!("__{}_TYPE__", t.name), t.ty.spelling()))
        .collect()
}

/// gcc's `__INTN_C(c)` macros, as (name, suffix): each gives an integer
/// constant the promoted type of `int_leastN_t` (C17 7.20.4.1), and `<stdint.h>`
/// defines `INTN_C` as it.
pub fn get_constant_fn_macros(target: &Target) -> Vec<(String, &'static str)> {
    integer_typedefs(target)
        .into_iter()
        .filter(|t| t.describe.suffix)
        .map(|t| (format!("__{}_C", t.name), t.ty.constant_suffix()))
        .collect()
}

/// `t`'s largest value as a constant of type `t` (or of its promoted type,
/// for one narrower than `int`): `0x7fffffffffffffffL`.
fn max_literal(target: &Target, t: IntType) -> String {
    format!("{:#x}{}", target.int_max(t), t.constant_suffix())
}

/// Every integer limit, width, size, constant-suffix and format predefine,
/// derived from the target's types -- for the basic types (`<limits.h>`) and
/// for each typedef (`<stdint.h>`, `<stddef.h>`, `<inttypes.h>`).
pub fn get_integer_macros(target: &Target) -> Vec<(String, String)> {
    let mut out: Vec<(String, String)> = Vec::new();

    // The basic types. `__LLONG_WIDTH__` is clang's spelling of gcc's
    // `__LONG_LONG_WIDTH__`; both are in use.
    for (name, ty) in [
        ("SCHAR", IntType::SChar),
        ("SHRT", IntType::Short),
        ("INT", IntType::Int),
        ("LONG", IntType::Long),
        ("LONG_LONG", IntType::LongLong),
    ] {
        out.push((format!("__{name}_MAX__"), max_literal(target, ty)));
        out.push((
            format!("__{name}_WIDTH__"),
            target.int_width(ty).to_string(),
        ));
    }
    out.push((
        "__LLONG_WIDTH__".into(),
        target.int_width(IntType::LongLong).to_string(),
    ));
    let sizeof = |ty: IntType| (target.int_width(ty) / 8).to_string();
    for (name, ty) in [
        ("SHORT", IntType::Short),
        ("INT", IntType::Int),
        ("LONG", IntType::Long),
        ("LONG_LONG", IntType::LongLong),
        ("SIZE_T", target.size_type()),
        ("PTRDIFF_T", target.ptrdiff_type()),
        ("WCHAR_T", target.wchar_type()),
        ("WINT_T", target.wint_type()),
    ] {
        out.push((format!("__SIZEOF_{name}__"), sizeof(ty)));
    }

    for t in integer_typedefs(target) {
        let (name, ty, d) = (&t.name, t.ty, t.describe);
        if d.max {
            out.push((format!("__{name}_MAX__"), max_literal(target, ty)));
        }
        if d.min {
            let min = if ty.is_signed() {
                format!("(-__{name}_MAX__ - 1)")
            } else {
                format!("0{}", ty.constant_suffix())
            };
            out.push((format!("__{name}_MIN__"), min));
        }
        if d.width {
            out.push((
                format!("__{name}_WIDTH__"),
                target.int_width(ty).to_string(),
            ));
        }
        if d.suffix {
            out.push((format!("__{name}_C_SUFFIX__"), ty.constant_suffix().into()));
        }
        if d.fmt {
            let convs: &[char] = if ty.is_signed() {
                &['d', 'i']
            } else {
                &['o', 'u', 'x', 'X']
            };
            for c in convs {
                out.push((
                    format!("__{name}_FMT{c}__"),
                    format!("\"{}{c}\"", ty.printf_length()),
                ));
            }
        }
    }
    out
}

/// `__GCC_ATOMIC_*_LOCK_FREE` for each type `<stdatomic.h>` asks about -- 2,
/// "always", or 0, "never", from the type's size -- and
/// `__GCC_ATOMIC_TEST_AND_SET_TRUEVAL`.
///
/// Typed out per architecture, the list had no `CHAR16_T`, `CHAR32_T` or
/// `WCHAR_T` entries, so `ATOMIC_WCHAR_T_LOCK_FREE` expanded to an undeclared
/// identifier.
pub fn get_atomic_macros(target: &Target) -> Vec<(String, String)> {
    let int_bytes = |t: IntType| u64::from(target.int_width(t) / 8);
    let mut out: Vec<(String, String)> = [
        ("BOOL", 1),
        ("CHAR", 1),
        ("CHAR16_T", int_bytes(target.char16_type())),
        ("CHAR32_T", int_bytes(target.char32_type())),
        ("WCHAR_T", int_bytes(target.wchar_type())),
        ("SHORT", int_bytes(IntType::Short)),
        ("INT", int_bytes(IntType::Int)),
        ("LONG", int_bytes(IntType::Long)),
        ("LLONG", int_bytes(IntType::LongLong)),
        ("POINTER", u64::from(target.pointer_width / 8)),
    ]
    .into_iter()
    .map(|(name, bytes)| {
        let always = crate::target::atomic_is_lock_free(bytes);
        (
            format!("__GCC_ATOMIC_{name}_LOCK_FREE"),
            if always { "2" } else { "0" }.to_string(),
        )
    })
    .collect();
    out.push((
        "__GCC_ATOMIC_TEST_AND_SET_TRUEVAL".into(),
        crate::target::ATOMIC_TEST_AND_SET_TRUEVAL.to_string(),
    ));
    out
}

pub fn get_additional_sizeof_macros(target: &Target) -> Vec<(&'static str, &'static str)> {
    // 16 bytes for the x87 80-bit format and for IEEE binary128 alike; Apple's
    // aarch64 `long double` is a `double` and occupies 8.
    let sizeof_long_double = if target.arch == Arch::Aarch64 && target.os == Os::MacOS {
        "8"
    } else {
        "16"
    };
    vec![("__SIZEOF_LONG_DOUBLE__", sizeof_long_double)]
}

/// Get miscellaneous macros
pub fn get_misc_macros(_target: &Target) -> Vec<(&'static str, &'static str)> {
    vec![
        // Pointer width in bits (always 64 on our targets)
        ("__POINTER_WIDTH__", "64"),
        // Alignment
        ("__BIGGEST_ALIGNMENT__", "16"),
        ("__BOOL_WIDTH__", "8"),
        // Byte order (all our supported architectures are little-endian),
        // and the order of the words of a multi-word floating type, which
        // follows it on both.
        ("__ORDER_LITTLE_ENDIAN__", "1234"),
        ("__ORDER_BIG_ENDIAN__", "4321"),
        ("__ORDER_PDP_ENDIAN__", "3412"),
        ("__BYTE_ORDER__", "__ORDER_LITTLE_ENDIAN__"),
        ("__FLOAT_WORD_ORDER__", "__ORDER_LITTLE_ENDIAN__"),
        ("__LITTLE_ENDIAN__", "1"),
        // Floating point base
        ("__FLT_RADIX__", "2"),
        // C17 5.2.4.2.2p9: every floating operation is evaluated in its own
        // type -- SSE on x86-64, the FP registers on aarch64, never x87 --
        // so 0. <float.h>'s FLT_EVAL_METHOD and glibc's float_t/double_t
        // (<bits/flt-eval-method.h>) both read this.
        ("__FLT_EVAL_METHOD__", "0"),
        ("__FINITE_MATH_ONLY__", "0"),
    ]
}

/// Get floating-point limit macros (IEEE 754)
pub fn get_float_limit_macros(target: &Target) -> Vec<(String, String)> {
    // `long double` is three different types across our targets, and these
    // macros are the only description of it a program gets. Hardcoding the
    // x86_64 values everywhere told macOS/aarch64 code that LDBL_EPSILON was
    // 1.08e-19 when the type is really a 64-bit double, so `1.0L + LDBL_EPSILON`
    // compared equal to 1.0L and <float.h> was simply lying.
    let ldbl = FormatLimits::long_double(target);
    let fixed: Vec<(&'static str, &'static str)> = vec![
        // Float16 (16-bit IEEE 754 binary16, half precision)
        (
            "__FLT16_MIN__",
            "6.10351562500000000000000000000000000e-5F16",
        ),
        (
            "__FLT16_MAX__",
            "6.55040000000000000000000000000000000e+4F16",
        ),
        (
            "__FLT16_EPSILON__",
            "9.76562500000000000000000000000000000e-4F16",
        ),
        (
            "__FLT16_DENORM_MIN__",
            "5.96046447753906250000000000000000000e-8F16",
        ),
        ("__FLT16_MANT_DIG__", "11"),
        ("__FLT16_DIG__", "3"),
        ("__FLT16_MIN_EXP__", "(-13)"),
        ("__FLT16_MAX_EXP__", "16"),
        ("__FLT16_MIN_10_EXP__", "(-4)"),
        ("__FLT16_MAX_10_EXP__", "4"),
        ("__FLT16_HAS_DENORM__", "1"),
        ("__FLT16_HAS_INFINITY__", "1"),
        ("__FLT16_HAS_QUIET_NAN__", "1"),
        ("__FLT16_DECIMAL_DIG__", "5"),
        ("__SIZEOF_FLOAT16__", "2"),
        // Float (32-bit IEEE 754)
        ("__FLT_MIN__", "1.17549435082228750796873653722224568e-38F"),
        ("__FLT_MAX__", "3.40282346638528859811704183484516925e+38F"),
        (
            "__FLT_EPSILON__",
            "1.19209289550781250000000000000000000e-7F",
        ),
        (
            "__FLT_DENORM_MIN__",
            "1.40129846432481707092372958328991613e-45F",
        ),
        ("__FLT_MANT_DIG__", "24"),
        ("__FLT_DIG__", "6"),
        ("__FLT_MIN_EXP__", "(-125)"),
        ("__FLT_MAX_EXP__", "128"),
        ("__FLT_MIN_10_EXP__", "(-37)"),
        ("__FLT_MAX_10_EXP__", "38"),
        ("__FLT_HAS_DENORM__", "1"),
        ("__FLT_HAS_INFINITY__", "1"),
        ("__FLT_HAS_QUIET_NAN__", "1"),
        // Double (64-bit IEEE 754)
        ("__DBL_MIN__", "2.22507385850720138309023271733240406e-308"),
        ("__DBL_MAX__", "1.79769313486231570814527423731704357e+308"),
        (
            "__DBL_EPSILON__",
            "2.22044604925031308084726333618164062e-16",
        ),
        (
            "__DBL_DENORM_MIN__",
            "4.94065645841246544176568792868221372e-324",
        ),
        ("__DBL_MANT_DIG__", "53"),
        ("__DBL_DIG__", "15"),
        ("__DBL_MIN_EXP__", "(-1021)"),
        ("__DBL_MAX_EXP__", "1024"),
        ("__DBL_MIN_10_EXP__", "(-307)"),
        ("__DBL_MAX_10_EXP__", "308"),
        ("__DBL_HAS_DENORM__", "1"),
        ("__DBL_HAS_INFINITY__", "1"),
        ("__DBL_HAS_QUIET_NAN__", "1"),
        // Decimal digits for exact conversion
        ("__FLT_DECIMAL_DIG__", "9"),
        ("__DBL_DECIMAL_DIG__", "17"),
        ("__DECIMAL_DIG__", ldbl.decimal_dig),
    ];
    let mut macros: Vec<(String, String)> = fixed
        .into_iter()
        .map(|(name, value)| (name.to_string(), value.to_string()))
        .collect();

    // Long double -- see `FormatLimits::long_double`.
    macros.extend(ldbl.macros("LDBL", "L"));

    // C23's `_Float32`, `_Float64` and `_Float32x` (TS 18661-3), in the
    // formats of `float` and `double`; and `_Float64x` in `long double`'s,
    // where that is wider than `double`. <float.h> and glibc's
    // <bits/floatn.h> read these.
    macros.extend(FormatLimits::BINARY32.macros("FLT32", "F32"));
    macros.extend(FormatLimits::BINARY64.macros("FLT64", "F64"));
    macros.extend(FormatLimits::BINARY64.macros("FLT32X", "F32x"));
    if has_float64x(target) {
        macros.extend(ldbl.macros("FLT64X", "F64x"));
    }

    // __float128 / _Float128 (IEEE 754 binary128, quad precision).
    //
    // Spelled in hex so the values reach binary128 exactly, and suffixed `q`
    // so they carry that type rather than being rounded to whatever
    // `long double` happens to be. Same on every target that has the type --
    // unlike `long double`, binary128 does not vary -- but only on the targets
    // that have it at all: describing a type the runtime cannot support would
    // have <float.h> advertise an FLT128_* family that fails at link time.
    if has_float128(target) {
        macros.extend(FormatLimits::BINARY128.macros("FLT128", "q"));
        macros.push(("__SIZEOF_FLOAT128__".to_string(), "16".to_string()));
    }

    macros
}

/// Whether `_Float64x` exists on this target: it needs a format wider than
/// `double`, which `long double` is everywhere but Apple's aarch64.
pub fn has_float64x(target: &Target) -> bool {
    !(target.arch == Arch::Aarch64 && target.os == Os::MacOS)
}

/// Whether `__float128` exists on this target; see `TypeTable::has_float128`,
/// which must agree with this.
pub fn has_float128(target: &Target) -> bool {
    target.os != Os::MacOS
}

/// The <float.h> description of one binary floating format, from which
/// every `__<prefix>_*__` family of a type in that format is spelled.
struct FormatLimits {
    min: &'static str,
    max: &'static str,
    epsilon: &'static str,
    denorm_min: &'static str,
    mant_dig: &'static str,
    dig: &'static str,
    min_exp: &'static str,
    max_exp: &'static str,
    min_10_exp: &'static str,
    max_10_exp: &'static str,
    decimal_dig: &'static str,
}

impl FormatLimits {
    // The float-valued limits are spelled as hex literals, without a suffix,
    // which reach the format exactly once a suffix names a type of it. As
    // decimal they were rounded through `f64` when parsed back, so
    // `__LDBL_MAX__` expanded to a literal that had already become infinity
    // and `__LDBL_MIN__` to one that had become zero.

    const BINARY32: Self = Self {
        min: "0x1p-126",
        max: "0x1.fffffep+127",
        epsilon: "0x1p-23",
        denorm_min: "0x1p-149",
        mant_dig: "24",
        dig: "6",
        min_exp: "(-125)",
        max_exp: "128",
        min_10_exp: "(-37)",
        max_10_exp: "38",
        decimal_dig: "9",
    };

    const BINARY64: Self = Self {
        min: "0x1p-1022",
        max: "0x1.fffffffffffffp+1023",
        epsilon: "0x1p-52",
        denorm_min: "0x1p-1074",
        mant_dig: "53",
        dig: "15",
        min_exp: "(-1021)",
        max_exp: "1024",
        min_10_exp: "(-307)",
        max_10_exp: "308",
        decimal_dig: "17",
    };

    const BINARY128: Self = Self {
        min: "0x1p-16382",
        max: "0x1.ffffffffffffffffffffffffffffp+16383",
        epsilon: "0x1p-112",
        denorm_min: "0x1p-16494",
        mant_dig: "113",
        dig: "33",
        min_exp: "(-16381)",
        max_exp: "16384",
        min_10_exp: "(-4931)",
        max_10_exp: "4932",
        decimal_dig: "36",
    };

    const X87_EXTENDED: Self = Self {
        min: "0x1p-16382",
        max: "0x1.fffffffffffffffep+16383",
        epsilon: "0x1p-63",
        denorm_min: "0x1p-16445",
        mant_dig: "64",
        dig: "18",
        min_exp: "(-16381)",
        max_exp: "16384",
        min_10_exp: "(-4931)",
        max_10_exp: "4932",
        decimal_dig: "21",
    };

    /// The format of `long double`, which is a different one on each of
    /// our targets:
    ///
    /// - **x86_64**: the x87 80-bit extended format, padded to 16 bytes.
    /// - **aarch64 Linux/FreeBSD**: IEEE 754 binary128 (true quad precision).
    /// - **aarch64 macOS**: Apple makes `long double` an alias for `double`.
    fn long_double(target: &Target) -> Self {
        match (target.arch, target.os) {
            // Apple aarch64: long double *is* double.
            (Arch::Aarch64, Os::MacOS) => Self::BINARY64,
            // aarch64 elsewhere: IEEE binary128.
            (Arch::Aarch64, _) => Self::BINARY128,
            // x86_64: x87 80-bit extended.
            _ => Self::X87_EXTENDED,
        }
    }

    /// The `__<prefix>_*__` family describing this format, its float-valued
    /// limits carrying `suffix` so they have the family's type.
    fn macros(&self, prefix: &str, suffix: &str) -> Vec<(String, String)> {
        let valued = [
            ("MIN", self.min),
            ("MAX", self.max),
            ("EPSILON", self.epsilon),
            ("DENORM_MIN", self.denorm_min),
        ];
        let counted = [
            ("MANT_DIG", self.mant_dig),
            ("DIG", self.dig),
            ("MIN_EXP", self.min_exp),
            ("MAX_EXP", self.max_exp),
            ("MIN_10_EXP", self.min_10_exp),
            ("MAX_10_EXP", self.max_10_exp),
            ("DECIMAL_DIG", self.decimal_dig),
            ("HAS_DENORM", "1"),
            ("HAS_INFINITY", "1"),
            ("HAS_QUIET_NAN", "1"),
        ];
        let valued = valued
            .into_iter()
            .map(|(field, value)| (field, format!("{value}{suffix}")));
        let counted = counted
            .into_iter()
            .map(|(field, value)| (field, value.to_string()));
        valued
            .chain(counted)
            .map(|(field, value)| (format!("__{prefix}_{field}__"), value))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::target::Os;

    fn macro_value<N: AsRef<str>, V: AsRef<str>>(macros: &[(N, V)], name: &str) -> String {
        macros
            .iter()
            .find(|(n, _)| n.as_ref() == name)
            .unwrap_or_else(|| panic!("{} is not defined", name))
            .1
            .as_ref()
            .to_string()
    }

    fn all_targets() -> Vec<Target> {
        let mut out = Vec::new();
        for arch in [Arch::X86_64, Arch::Aarch64] {
            for os in [Os::Linux, Os::MacOS, Os::FreeBSD] {
                out.push(Target::new(arch, os));
            }
        }
        out
    }

    /// gcc's `__INTN_C(c)` family exists for exactly the typedefs with a
    /// `_C_SUFFIX__`, and pastes that suffix.
    #[test]
    fn constant_fn_macros_paste_the_typedef_suffix() {
        for target in all_targets() {
            let fns = get_constant_fn_macros(&target);
            let ints = get_integer_macros(&target);
            let names: Vec<&str> = fns.iter().map(|(n, _)| n.as_str()).collect();
            assert_eq!(
                names,
                [
                    "__INT8_C",
                    "__UINT8_C",
                    "__INT16_C",
                    "__UINT16_C",
                    "__INT32_C",
                    "__UINT32_C",
                    "__INT64_C",
                    "__UINT64_C",
                    "__INTMAX_C",
                    "__UINTMAX_C"
                ]
            );
            for (name, suffix) in &fns {
                assert_eq!(
                    macro_value(&ints, &format!("{name}_SUFFIX__")),
                    *suffix,
                    "{name} on {}-{}",
                    target.arch,
                    target.os
                );
            }
        }
    }

    /// Every type `<stdatomic.h>` names in an `ATOMIC_*_LOCK_FREE` has its
    /// predefine, and on every target here all ten are always lock-free, as
    /// gcc says for both Linux targets.
    #[test]
    fn atomic_lock_free_macros_cover_stdatomic() {
        for target in all_targets() {
            let macros = get_atomic_macros(&target);
            for name in [
                "BOOL", "CHAR", "CHAR16_T", "CHAR32_T", "WCHAR_T", "SHORT", "INT", "LONG", "LLONG",
                "POINTER",
            ] {
                assert_eq!(
                    macro_value(&macros, &format!("__GCC_ATOMIC_{name}_LOCK_FREE")),
                    "2",
                    "{name} on {}-{}",
                    target.arch,
                    target.os
                );
            }
            assert_eq!(
                macro_value(&macros, "__GCC_ATOMIC_TEST_AND_SET_TRUEVAL"),
                "1"
            );
        }
        assert!(!crate::target::atomic_is_lock_free(16));
        assert!(!crate::target::atomic_is_lock_free(3));
    }

    /// `long double` is a different type on each target, and these macros are
    /// the only description of it a program gets. They were hardcoded to the
    /// x86_64 values everywhere, so `<float.h>` told macOS/aarch64 code that
    /// LDBL_EPSILON was 1.08e-19 for a type that is really a 64-bit double.
    #[test]
    fn long_double_limits_describe_the_target() {
        for (arch, os, mant_dig, sizeof) in [
            (Arch::X86_64, Os::Linux, "64", "16"),   // x87 80-bit extended
            (Arch::X86_64, Os::MacOS, "64", "16"),   // likewise
            (Arch::Aarch64, Os::Linux, "113", "16"), // IEEE binary128
            (Arch::Aarch64, Os::MacOS, "53", "8"),   // Apple: long double == double
        ] {
            let target = Target::new(arch, os);
            let floats = get_float_limit_macros(&target);
            assert_eq!(
                macro_value(&floats, "__LDBL_MANT_DIG__"),
                mant_dig,
                "__LDBL_MANT_DIG__ on {:?}/{:?}",
                arch,
                os
            );
            assert_eq!(
                macro_value(
                    &get_additional_sizeof_macros(&target),
                    "__SIZEOF_LONG_DOUBLE__"
                ),
                sizeof,
                "__SIZEOF_LONG_DOUBLE__ on {:?}/{:?}",
                arch,
                os
            );
            // The epsilon has to belong to the same format as the mantissa.
            // Spelled in hex it is exactly 2^(1 - MANT_DIG), so this pins the
            // value rather than approximating it by a decimal exponent.
            let eps = macro_value(&floats, "__LDBL_EPSILON__");
            let expected = format!("0x1p-{}L", mant_dig.parse::<i32>().unwrap() - 1);
            assert_eq!(
                eps, expected,
                "__LDBL_EPSILON__ does not match a {}-bit mantissa on {:?}/{:?}",
                mant_dig, arch, os
            );
        }
    }

    /// `__CHAR_UNSIGNED__` is defined exactly when plain `char` is unsigned,
    /// once, and per OS as well as per architecture: Apple arm64 makes `char`
    /// signed where AAPCS64 makes it unsigned, and `<limits.h>` derives
    /// `CHAR_MIN`/`CHAR_MAX` from this macro alone.
    #[test]
    fn char_unsigned_macro_follows_the_target() {
        for (arch, os, unsigned) in [
            (Arch::X86_64, Os::Linux, false),
            (Arch::X86_64, Os::MacOS, false),
            (Arch::Aarch64, Os::Linux, true),
            (Arch::Aarch64, Os::MacOS, false),
        ] {
            let target = Target::new(arch, os);
            let macros = get_arch_macros(&target);
            let defs: Vec<_> = macros
                .iter()
                .filter(|(n, _)| *n == "__CHAR_UNSIGNED__")
                .collect();
            assert_eq!(
                defs.len(),
                usize::from(unsigned),
                "__CHAR_UNSIGNED__ on {arch}-{os}"
            );
            // The macro and the type system read the same fact.
            assert_eq!(
                unsigned,
                target.plain_char == CharSignedness::Unsigned,
                "{arch}-{os}"
            );
            let bits = macros.iter().find(|(n, _)| *n == "__CHAR_BIT__");
            assert_eq!(bits, Some(&("__CHAR_BIT__", Some("8"))));
            assert_eq!(
                macro_value(&get_integer_macros(&target), "__SCHAR_MAX__"),
                "0x7f"
            );
        }
    }

    /// Our `<stdint.h>` has to name the same type the host's headers do.
    /// Every target is LP64, so the width does not settle it: Linux says
    /// `long`, Darwin says `long long`, and they are distinct types.
    #[test]
    fn int64_spelling_follows_the_platform() {
        for (os, ty, suffix) in [
            (Os::Linux, "long int", "L"),
            (Os::FreeBSD, "long int", "L"),
            (Os::MacOS, "long long int", "LL"),
        ] {
            let target = Target::new(Arch::X86_64, os);
            assert_eq!(
                macro_value(&get_type_macros(&target), "__INT64_TYPE__"),
                ty,
                "__INT64_TYPE__ on {:?}",
                os
            );
            // INT64_C() must produce that same type, and so must INT64_MAX,
            // and PRId64 must print it.
            let ints = get_integer_macros(&target);
            assert_eq!(
                macro_value(&ints, "__INT64_C_SUFFIX__"),
                suffix,
                "__INT64_C_SUFFIX__ on {os}"
            );
            assert_eq!(
                macro_value(&ints, "__INT64_MAX__"),
                format!("0x7fffffffffffffff{suffix}"),
                "__INT64_MAX__ on {os}"
            );
            assert_eq!(
                macro_value(&ints, "__INT64_FMTd__"),
                format!("\"{}d\"", suffix.to_lowercase()),
                "__INT64_FMTd__ on {os}"
            );
        }
    }

    /// Every limit, constant suffix and format a typedef's predefines state
    /// has to describe the type its `__*_TYPE__` names. They were typed out
    /// one macro at a time, and `__INT64_MAX__` said `LL` and
    /// `__INT64_FMTd__` said `"lld"` for an `int64_t` that is `long` on
    /// Linux. This reads the facts back out of the macro text alone.
    #[test]
    fn integer_macros_agree_with_their_type() {
        // (spelling, width, signed, promoted constant suffix, length modifier)
        let table: &[(&str, u32, bool, &str, &str)] = &[
            ("signed char", 8, true, "", "hh"),
            ("unsigned char", 8, false, "", "hh"),
            ("short int", 16, true, "", "h"),
            ("short unsigned int", 16, false, "", "h"),
            ("int", 32, true, "", ""),
            ("unsigned int", 32, false, "U", ""),
            ("long int", 64, true, "L", "l"),
            ("long unsigned int", 64, false, "UL", "l"),
            ("long long int", 64, true, "LL", "ll"),
            ("long long unsigned int", 64, false, "ULL", "ll"),
        ];
        for target in all_targets() {
            let types = get_type_macros(&target);
            let ints = get_integer_macros(&target);
            let lookup = |n: &str| ints.iter().find(|(k, _)| k == n).map(|(_, v)| v.clone());
            for (tname, spelling) in &types {
                let name = &tname[2..tname.len() - "_TYPE__".len()];
                let &(_, width, signed, suffix, len) = table
                    .iter()
                    .find(|row| row.0 == *spelling)
                    .unwrap_or_else(|| panic!("{tname} is {spelling}"));
                let what = format!("{name} ({spelling}) on {}-{}", target.arch, target.os);
                if let Some(max) = lookup(&format!("__{name}_MAX__")) {
                    let bits = width - u32::from(signed);
                    let want = format!("{:#x}{suffix}", u64::MAX >> (64 - bits));
                    assert_eq!(max, want, "max of {what}");
                }
                if let Some(min) = lookup(&format!("__{name}_MIN__")) {
                    let want = if signed {
                        format!("(-__{name}_MAX__ - 1)")
                    } else {
                        format!("0{suffix}")
                    };
                    assert_eq!(min, want, "min of {what}");
                }
                if let Some(w) = lookup(&format!("__{name}_WIDTH__")) {
                    assert_eq!(w, width.to_string(), "width of {what}");
                }
                if let Some(sfx) = lookup(&format!("__{name}_C_SUFFIX__")) {
                    assert_eq!(sfx, suffix, "constant suffix of {what}");
                }
                for c in ['d', 'i', 'o', 'u', 'x', 'X'] {
                    if let Some(fmt) = lookup(&format!("__{name}_FMT{c}__")) {
                        assert_eq!(fmt, format!("\"{len}{c}\""), "format of {what}");
                        assert_eq!(signed, matches!(c, 'd' | 'i'), "{c} for {what}");
                    }
                }
            }
            // The three minimums <stdint.h> cannot compute without knowing
            // the signedness.
            for name in ["WCHAR", "WINT", "SIG_ATOMIC"] {
                assert!(
                    lookup(&format!("__{name}_MIN__")).is_some(),
                    "__{name}_MIN__"
                );
            }
        }
    }

    /// Every integer predefine is in the implementation's namespace (C17
    /// 7.1.3): `SSIZE_MAX` was predefined, where POSIX puts it in
    /// `<limits.h>` and a program without that header may use the name.
    #[test]
    fn integer_macros_are_reserved_names() {
        let reserved = |n: &str| {
            n.starts_with("__")
                || (n.starts_with('_') && n[1..].starts_with(|c: char| c.is_ascii_uppercase()))
        };
        for target in all_targets() {
            for (name, _) in get_integer_macros(&target)
                .into_iter()
                .chain(get_atomic_macros(&target))
                .chain(
                    get_type_macros(&target)
                        .into_iter()
                        .map(|(n, v)| (n, v.to_string())),
                )
                .chain(
                    get_constant_fn_macros(&target)
                        .into_iter()
                        .map(|(n, v)| (n, v.to_string())),
                )
            {
                assert!(reserved(&name), "{name} on {}-{}", target.arch, target.os);
            }
            for (name, _) in get_arch_macros(&target) {
                assert!(reserved(name), "{name} on {}-{}", target.arch, target.os);
            }
        }
    }

    /// `__arm64__` and `__arm64` are Apple's spellings: clang defines them
    /// for Darwin arm64 only, and gcc for aarch64 Linux defines neither. Code
    /// reads `__arm64__` as "Apple", so on Linux it must be absent.
    #[test]
    fn arm64_spelling_is_apple_only() {
        for target in all_targets() {
            let macros = get_arch_macros(&target);
            let apple_arm64 = target.arch == Arch::Aarch64 && target.os == Os::MacOS;
            for name in ["__arm64__", "__arm64"] {
                let defs = macros.iter().filter(|(n, _)| *n == name).count();
                assert_eq!(
                    defs,
                    usize::from(apple_arm64),
                    "{name} on {}-{}",
                    target.arch,
                    target.os
                );
            }
            let aarch64 = macros.iter().any(|(n, _)| *n == "__aarch64__");
            assert_eq!(aarch64, target.arch == Arch::Aarch64);
        }
    }
}
