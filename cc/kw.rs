//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Pre-interned keyword system for c17 C17 compiler
//
// All well-known strings (C keywords, builtins, attribute names, preprocessor
// directives) are pre-interned at StringTable creation time. Each gets a
// deterministic StringId and a u32 tag bitmask for O(1) set-membership queries.
//
// This eliminates string comparisons in hot paths (is_declaration_start,
// parse_declaration_specifiers, parse_statement, is_builtin, etc.) by replacing them
// with integer comparisons.
//

use crate::strings::StringId;

// Tag bit constants (u32, 15 of 32 used)

pub const TYPE_SPEC: u32 = 1 << 0;
pub const STORAGE: u32 = 1 << 1;
pub const QUALIFIER: u32 = 1 << 2;
pub const INLINE_KW: u32 = 1 << 3;
pub const NORETURN_KW: u32 = 1 << 4;
pub const ATTR_KW: u32 = 1 << 5;
pub const ASM_KW: u32 = 1 << 6;
pub const ASSERT_KW: u32 = 1 << 7;
pub const NULLABILITY: u32 = 1 << 8;
pub const STMT_KW: u32 = 1 << 9;
pub const BUILTIN: u32 = 1 << 10;
pub const SUPPORTED_ATTR: u32 = 1 << 11;
pub const ALIGNAS_KW: u32 = 1 << 12;
pub const TYPE_KEYWORD: u32 = 1 << 13;
/// Never valid as a declarator name.
///
/// A separate bit rather than a derived predicate, because the boundary is
/// empirical rather than structural: it is exactly the set gcc rejects in C17
/// mode. `alignof`, `typeof_unqual` and `_BitInt` are C23 spellings and stay
/// usable as identifiers; `typeof` is a GNU extension and gcc accepts
/// `int typeof;` too. Nor can it be derived from the naming convention that
/// separates standard C from GNU extensions (`FOO` vs `GNU_FOO`), because that
/// lives in the Rust const identifier and is not queryable at runtime.
pub const RESERVED_NAME: u32 = 1 << 14;
/// An attribute, builtin or qualifier gcc knows only on x86-64: the
/// calling-convention attributes, the Microsoft `va_list` builtins and the
/// `__seg_fs`/`__seg_gs` address spaces. Anywhere else gcc has never heard
/// of the name, so neither has c17 -- see [`exists_on`].
pub const X86_64_ONLY: u32 = 1 << 15;

/// Composite: all tags that start a declaration
pub const DECL_START: u32 =
    TYPE_SPEC | STORAGE | QUALIFIER | INLINE_KW | NORETURN_KW | ATTR_KW | ASSERT_KW | ALIGNAS_KW;

// Keyword definition macros

/// Are these two strings equal? A `const fn`, so [`id_of`] can run at compile
/// time; `str` has no `const` comparison of its own.
const fn str_eq(a: &str, b: &str) -> bool {
    let (a, b) = (a.as_bytes(), b.as_bytes());
    if a.len() != b.len() {
        return false;
    }
    let mut i = 0;
    while i < a.len() {
        if a[i] != b[i] {
            return false;
        }
        i += 1;
    }
    true
}

/// The `StringId` of a keyword, found by its spelling at compile time.
///
/// Ids are one-based: zero is reserved, so a `StringId` is never the default.
const fn id_of(s: &str) -> StringId {
    let mut i = 0;
    while i < KEYWORD_STRINGS.len() {
        if str_eq(KEYWORD_STRINGS[i], s) {
            return StringId(i as u32 + 1);
        }
        i += 1;
    }
    panic!("keyword has no entry in KEYWORD_STRINGS")
}

/// Helper macro: one `pub const` per named entry. `_` is anonymous — it is
/// interned and tagged, but names nothing.
macro_rules! define_id {
    (_, $str:literal) => {};
    ($name:ident, $str:literal) => {
        pub const $name: StringId = id_of($str);
    };
}

/// Main keyword definition macro. Generates:
/// - KEYWORD_COUNT: total number of keywords
/// - One `pub const NAME: StringId` per named keyword (entries with `_` are anonymous)
/// - KEYWORD_STRINGS: array of string literals (all entries)
/// - KEYWORD_TAGS: array of tag bitmasks (all entries)
///
/// Each id is looked up by spelling rather than counted by a macro that
/// recurses once per entry. The counting version built the *n*th id as the
/// expression `((((1u32 + 1) + 1) + 1) ...)`, nested once per preceding
/// keyword, because a substituted `$counter:expr` is one opaque node rather
/// than a flat token run. At around seven hundred keywords that made **rustc
/// itself** overflow its stack parsing this file -- a SIGSEGV in
/// `parse_expr_assoc_with` on aarch64, where the frames are wider, while
/// x86-64 still fit. The table has to be free to grow.
macro_rules! define_keywords {
    ( $( ($name:tt, $str:literal, $tags:expr) ),* $(,)? ) => {
        pub const KEYWORD_COUNT: usize = [ $( $str ),* ].len();
        pub(crate) const KEYWORD_STRINGS: [&str; KEYWORD_COUNT] = [ $( $str ),* ];
        pub(crate) const KEYWORD_TAGS: [u32; KEYWORD_COUNT] = [ $( $tags ),* ];
        $( define_id!($name, $str); )*
    };
}

// ============================================================================
// Keyword table — single source of truth
// ============================================================================
//
// Naming convention:
//   FOO          — standard C keyword: const, inline, _Noreturn
//   GNU_FOO      — __foo__ (double-underscore-wrapped GNU spelling)
//   GNU_FOO2     — __foo (leading-underscore-only GNU spelling)
//   FOO_C23      — C23 spelling: static_assert
//   BUILTIN_*    — __builtin_* compiler builtins
//   C11_ATOMIC_* — __c11_atomic_* builtins
//   ATTR_*       — attribute names (plain)
//   GNU_ATTR_*   — attribute names (__foo__ form)
//   PP_*         — preprocessor directives that conflict with Rust keywords

define_keywords! {
    // ---- Type specifiers (TYPE_SPEC) ----
    (VOID,              "void",              TYPE_SPEC | TYPE_KEYWORD),
    (CHAR,              "char",              TYPE_SPEC | TYPE_KEYWORD),
    (SHORT,             "short",             TYPE_SPEC | TYPE_KEYWORD),
    (INT,               "int",               TYPE_SPEC | TYPE_KEYWORD),
    (LONG,              "long",              TYPE_SPEC | TYPE_KEYWORD),
    (FLOAT,             "float",             TYPE_SPEC | TYPE_KEYWORD),
    (DOUBLE,            "double",            TYPE_SPEC | TYPE_KEYWORD),
    (SIGNED,            "signed",            TYPE_SPEC | TYPE_KEYWORD),
    (UNSIGNED,          "unsigned",          TYPE_SPEC | TYPE_KEYWORD),
    (BOOL,              "_Bool",             TYPE_SPEC | TYPE_KEYWORD),
    (COMPLEX,           "_Complex",          TYPE_SPEC | TYPE_KEYWORD),
    // gcc's spellings of the same specifier, reserved for the same reason the
    // `__typeof__` forms are: a leading double underscore belongs to the
    // implementation in every scope (C17 7.1.3). Without them a declaration
    // like `__complex__ float f(void)` parses as a missing type specifier and
    // draws the implicit-int diagnostic, which points at the wrong thing.
    (GNU_COMPLEX,       "__complex__",       TYPE_SPEC | TYPE_KEYWORD | RESERVED_NAME),
    (GNU_COMPLEX2,      "__complex",         TYPE_SPEC | TYPE_KEYWORD | RESERVED_NAME),
    // C99 6.4.1 reserves `_Imaginary` whether or not imaginary types are
    // provided (Annex G makes the types optional, not the keyword). It is
    // tagged as the type specifier it is, so that it starts a declaration or
    // a type name and reaches the one diagnostic for it in the specifier
    // loop; c17 provides no type behind it.
    (IMAGINARY,         "_Imaginary",        TYPE_SPEC | TYPE_KEYWORD | RESERVED_NAME),
    (FLOAT16,           "_Float16",          TYPE_SPEC | TYPE_KEYWORD),
    (FLOAT32,           "_Float32",          TYPE_SPEC | TYPE_KEYWORD),
    (FLOAT64,           "_Float64",          TYPE_SPEC | TYPE_KEYWORD),
    (FLOAT32X,          "_Float32x",         TYPE_SPEC | TYPE_KEYWORD),
    (FLOAT64X,          "_Float64x",         TYPE_SPEC | TYPE_KEYWORD),
    (FLOAT128,          "__float128",        TYPE_SPEC | TYPE_KEYWORD),
    (FLOAT128_ALIAS,    "_Float128",         TYPE_SPEC | TYPE_KEYWORD),
    (INT128,            "__int128",          TYPE_SPEC | TYPE_KEYWORD),
    (INT128_T,          "__int128_t",        TYPE_SPEC | TYPE_KEYWORD),
    (UINT128_T,         "__uint128_t",       TYPE_SPEC | TYPE_KEYWORD),
    (BUILTIN_VA_LIST,   "__builtin_va_list", TYPE_SPEC | TYPE_KEYWORD | BUILTIN),
    (BUILTIN_MS_VA_LIST, "__builtin_ms_va_list", TYPE_SPEC | TYPE_KEYWORD | BUILTIN | X86_64_ONLY),
    (STRUCT,            "struct",            TYPE_SPEC | TYPE_KEYWORD),
    (UNION,             "union",             TYPE_SPEC | TYPE_KEYWORD),
    (ENUM,              "enum",              TYPE_SPEC | TYPE_KEYWORD),
    (TYPEOF,            "typeof",            TYPE_SPEC | TYPE_KEYWORD),
    // The `__`-spelled forms stay reserved even though plain `typeof` does
    // not: a leading double underscore is reserved to the implementation in
    // every scope (C17 7.1.3), and gcc rejects `int __typeof__;` accordingly.
    (GNU_TYPEOF,        "__typeof__",        TYPE_SPEC | TYPE_KEYWORD | RESERVED_NAME),
    (GNU_TYPEOF2,       "__typeof",          TYPE_SPEC | TYPE_KEYWORD | RESERVED_NAME),
    (ATOMIC,            "_Atomic",           TYPE_SPEC | QUALIFIER | TYPE_KEYWORD),

    // ---- Storage class (STORAGE) ----
    (STATIC,            "static",            STORAGE),
    (EXTERN,            "extern",            STORAGE),
    (AUTO,              "auto",              STORAGE),
    (REGISTER,          "register",          STORAGE),
    (TYPEDEF,           "typedef",           STORAGE),
    (THREAD_LOCAL,      "_Thread_local",     STORAGE),
    (GNU_THREAD,        "__thread",          STORAGE),

    // ---- Type qualifiers (QUALIFIER) ----
    (CONST,             "const",             QUALIFIER | TYPE_KEYWORD | SUPPORTED_ATTR),
    (VOLATILE,          "volatile",          QUALIFIER | TYPE_KEYWORD),
    (RESTRICT,          "restrict",          QUALIFIER),
    (GNU_CONST,         "__const__",         QUALIFIER | SUPPORTED_ATTR),
    (GNU_CONST2,        "__const",           QUALIFIER | SUPPORTED_ATTR),
    (GNU_VOLATILE,      "__volatile__",      QUALIFIER),
    (GNU_VOLATILE2,     "__volatile",        QUALIFIER),
    (GNU_RESTRICT,      "__restrict__",      QUALIFIER),
    (GNU_RESTRICT2,     "__restrict",        QUALIFIER),
    // gcc's x86-64 named address spaces.
    (SEG_FS,            "__seg_fs",          QUALIFIER | X86_64_ONLY),
    (SEG_GS,            "__seg_gs",          QUALIFIER | X86_64_ONLY),

    // ---- Inline (INLINE_KW) ----
    (INLINE,            "inline",            INLINE_KW),
    (GNU_INLINE,        "__inline__",        INLINE_KW),
    (GNU_INLINE2,       "__inline",          INLINE_KW),

    // ---- Noreturn (NORETURN_KW) ----
    (NORETURN,          "_Noreturn",         NORETURN_KW),
    (GNU_NORETURN,      "__noreturn__",      NORETURN_KW | SUPPORTED_ATTR),

    // ---- Attribute keyword (ATTR_KW) ----
    (GNU_ATTRIBUTE,     "__attribute__",     ATTR_KW),
    (GNU_ATTRIBUTE2,    "__attribute",       ATTR_KW),

    // ---- Asm keyword (ASM_KW) ----
    (ASM,               "asm",               ASM_KW),
    (GNU_ASM,           "__asm__",           ASM_KW),
    (GNU_ASM2,          "__asm",             ASM_KW),

    // ---- Static assert (ASSERT_KW) ----
    (_,                 "_Static_assert",    ASSERT_KW | RESERVED_NAME),
    (_,                 "static_assert",     ASSERT_KW),

    // ---- Alignas (ALIGNAS_KW) ----
    (ALIGNAS,           "_Alignas",          ALIGNAS_KW),

    // ---- Nullability qualifiers (NULLABILITY) ----
    (_,                 "_Nonnull",          NULLABILITY),
    (_,                 "__nonnull",         NULLABILITY),
    (_,                 "_Nullable",         NULLABILITY),
    (_,                 "__nullable",        NULLABILITY),
    (_,                 "_Null_unspecified",  NULLABILITY),
    (_,                 "__null_unspecified", NULLABILITY),

    // ---- Statement keywords (STMT_KW) ----
    (IF,                "if",                STMT_KW | RESERVED_NAME),
    (ELSE,              "else",              STMT_KW | RESERVED_NAME),
    (WHILE,             "while",             STMT_KW | RESERVED_NAME),
    (DO,                "do",                STMT_KW | RESERVED_NAME),
    (FOR,               "for",               STMT_KW | RESERVED_NAME),
    (RETURN,            "return",            STMT_KW | RESERVED_NAME),
    (BREAK,             "break",             STMT_KW | RESERVED_NAME),
    (CONTINUE,          "continue",          STMT_KW | RESERVED_NAME),
    (GOTO,              "goto",              STMT_KW | RESERVED_NAME),
    (SWITCH,            "switch",            STMT_KW | RESERVED_NAME),
    (CASE,              "case",              STMT_KW | RESERVED_NAME),
    (DEFAULT,           "default",           STMT_KW | RESERVED_NAME),

    // ---- Sizeof / Alignof / Generic ----
    (SIZEOF,            "sizeof",            RESERVED_NAME),
    // _Generic introduces a primary expression, so it carries no tag. A
    // TYPE_SPEC or DECL_START tag would make is_declaration_start and
    // try_parse_type_name's entry gate mistake it for the start of a type.
    (GENERIC,           "_Generic",          RESERVED_NAME),
    (ALIGNOF,           "_Alignof",          RESERVED_NAME),
    (GNU_ALIGNOF,       "__alignof__",       RESERVED_NAME),
    (GNU_ALIGNOF2,      "__alignof",         RESERVED_NAME),
    (ALIGNOF_C23,       "alignof",           0),

    // ---- Wide char prefix ----
    (_,                 "L",                 0),

    // ---- Preprocessor directives ----
    (DEFINE,            "define",            0),
    (UNDEF,             "undef",             0),
    (IFDEF,             "ifdef",             0),
    (IFNDEF,            "ifndef",            0),
    (ELIF,              "elif",              0),
    (ENDIF,             "endif",             0),
    (INCLUDE,           "include",           0),
    (INCLUDE_NEXT,      "include_next",      0),
    (PP_ERROR,          "error",             0),
    (WARNING,           "warning",           0),
    (PRAGMA,            "pragma",            0),
    (LINE,              "line",              0),
    (PP_IDENT,          "ident",             0),
    (SCCS,              "sccs",              0),

    // ---- Preprocessor special names ----
    (_,                 "defined",           0),
    (_,                 "__VA_ARGS__",       0),
    (_,                 "once",              0),

    // ---- Predefined identifiers ----
    (FUNC,              "__func__",          0),
    (FUNCTION,          "__FUNCTION__",      0),
    (PRETTY_FUNCTION,   "__PRETTY_FUNCTION__", 0),

    // ---- Builtins (BUILTIN) ----
    (BUILTIN_VA_START,  "__builtin_va_start", BUILTIN),
    (BUILTIN_VA_END,    "__builtin_va_end",   BUILTIN),
    (BUILTIN_VA_ARG,    "__builtin_va_arg",   BUILTIN),
    (BUILTIN_VA_COPY,   "__builtin_va_copy",  BUILTIN),
    // The Microsoft x64 `va_list`, for an `ms_abi` function; `va_arg` is
    // the shared `__builtin_va_arg`, which the list's type steers.
    (BUILTIN_MS_VA_START, "__builtin_ms_va_start", BUILTIN | X86_64_ONLY),
    (BUILTIN_MS_VA_END,   "__builtin_ms_va_end",   BUILTIN | X86_64_ONLY),
    (BUILTIN_MS_VA_COPY,  "__builtin_ms_va_copy",  BUILTIN | X86_64_ONLY),
    // Forwarding builtins: inside an `always_inline` variadic function these
    // name the *caller's* variadic arguments, and are resolved when it is
    // inlined. glibc forwards sprintf/printf into the `__*_chk` family with
    // them.
    (BUILTIN_VA_ARG_PACK,     "__builtin_va_arg_pack",     BUILTIN),
    (BUILTIN_VA_ARG_PACK_LEN, "__builtin_va_arg_pack_len", BUILTIN),
    (BUILTIN_BSWAP16,   "__builtin_bswap16",  BUILTIN),
    (BUILTIN_BSWAP32,   "__builtin_bswap32",  BUILTIN),
    (BUILTIN_BSWAP64,   "__builtin_bswap64",  BUILTIN),
    (BUILTIN_CTZ,       "__builtin_ctz",      BUILTIN),
    (BUILTIN_CTZL,      "__builtin_ctzl",     BUILTIN),
    (BUILTIN_CTZLL,     "__builtin_ctzll",    BUILTIN),
    (BUILTIN_CLZ,       "__builtin_clz",      BUILTIN),
    (BUILTIN_CLZL,      "__builtin_clzl",     BUILTIN),
    (BUILTIN_CLZLL,     "__builtin_clzll",    BUILTIN),
    (BUILTIN_CLRSB,     "__builtin_clrsb",    BUILTIN),
    (BUILTIN_CLRSBL,    "__builtin_clrsbl",   BUILTIN),
    (BUILTIN_CLRSBLL,   "__builtin_clrsbll",  BUILTIN),
    (BUILTIN_POPCOUNT,  "__builtin_popcount", BUILTIN),
    (BUILTIN_POPCOUNTL, "__builtin_popcountl", BUILTIN),
    (BUILTIN_POPCOUNTLL, "__builtin_popcountll", BUILTIN),
    (BUILTIN_PARITY,    "__builtin_parity",   BUILTIN),
    (BUILTIN_PARITYL,   "__builtin_parityl",  BUILTIN),
    (BUILTIN_PARITYLL,  "__builtin_parityll", BUILTIN),
    (BUILTIN_FFS,       "__builtin_ffs",      BUILTIN),
    (BUILTIN_FFSL,      "__builtin_ffsl",     BUILTIN),
    (BUILTIN_FFSLL,     "__builtin_ffsll",    BUILTIN),
    (BUILTIN_CHOOSE_EXPR, "__builtin_choose_expr", BUILTIN),
    // Builtins that are the library function of the same name; see
    // `is_library_builtin`. Listed so `__has_builtin` answers for them.
    (BUILTIN_STRLEN,    "__builtin_strlen",   BUILTIN),
    (BUILTIN_STRCMP,    "__builtin_strcmp",   BUILTIN),
    (BUILTIN_ABS,       "__builtin_abs",      BUILTIN),
    (BUILTIN_LABS,      "__builtin_labs",     BUILTIN),
    (BUILTIN_LLABS,     "__builtin_llabs",    BUILTIN),
    (BUILTIN_SQRT,      "__builtin_sqrt",     BUILTIN),
    (BUILTIN_COPYSIGN,  "__builtin_copysign", BUILTIN),
    // `bits/floatn.h` reaches for `__builtin_copysignf`, so the suffixed
    // spellings are as load-bearing as the plain one.
    (BUILTIN_COPYSIGNF, "__builtin_copysignf", BUILTIN),
    (BUILTIN_COPYSIGNL, "__builtin_copysignl", BUILTIN),
    (BUILTIN_SQRTF,     "__builtin_sqrtf",    BUILTIN),
    (BUILTIN_SQRTL,     "__builtin_sqrtl",    BUILTIN),
    (BUILTIN_FMAX,      "__builtin_fmax",     BUILTIN),
    (BUILTIN_FMAXF,     "__builtin_fmaxf",    BUILTIN),
    (BUILTIN_FMAXL,     "__builtin_fmaxl",    BUILTIN),
    (BUILTIN_FMIN,      "__builtin_fmin",     BUILTIN),
    (BUILTIN_FMINF,     "__builtin_fminf",    BUILTIN),
    (BUILTIN_FMINL,     "__builtin_fminl",    BUILTIN),
    (BUILTIN_POW,       "__builtin_pow",      BUILTIN),
    (BUILTIN_POWF,      "__builtin_powf",     BUILTIN),
    (BUILTIN_POWL,      "__builtin_powl",     BUILTIN),
    (BUILTIN_FMA,       "__builtin_fma",      BUILTIN),
    (BUILTIN_FMAF,      "__builtin_fmaf",     BUILTIN),
    (BUILTIN_FMAL,      "__builtin_fmal",     BUILTIN),
    (BUILTIN_BCMP,      "__builtin_bcmp",     BUILTIN),
    (BUILTIN_BZERO,     "__builtin_bzero",    BUILTIN),
    (BUILTIN_STPNCPY,   "__builtin_stpncpy",  BUILTIN),
    // The libm entry points, at each of their three real widths, and the
    // two POSIX case-insensitive comparisons. Signatures come from one
    // table in `builtin_expr.rs`, not from a suffix test at each site.
    (BUILTIN_CBRT,        "__builtin_cbrt", BUILTIN),
    (BUILTIN_CBRTF,       "__builtin_cbrtf", BUILTIN),
    (BUILTIN_CBRTL,       "__builtin_cbrtl", BUILTIN),
    (BUILTIN_CEIL,        "__builtin_ceil", BUILTIN),
    (BUILTIN_CEILF,       "__builtin_ceilf", BUILTIN),
    (BUILTIN_CEILL,       "__builtin_ceill", BUILTIN),
    (BUILTIN_FLOOR,       "__builtin_floor", BUILTIN),
    (BUILTIN_FLOORF,      "__builtin_floorf", BUILTIN),
    (BUILTIN_FLOORL,      "__builtin_floorl", BUILTIN),
    (BUILTIN_TRUNC,       "__builtin_trunc", BUILTIN),
    (BUILTIN_TRUNCF,      "__builtin_truncf", BUILTIN),
    (BUILTIN_TRUNCL,      "__builtin_truncl", BUILTIN),
    (BUILTIN_ROUND,       "__builtin_round", BUILTIN),
    (BUILTIN_ROUNDF,      "__builtin_roundf", BUILTIN),
    (BUILTIN_ROUNDL,      "__builtin_roundl", BUILTIN),
    (BUILTIN_RINT,        "__builtin_rint", BUILTIN),
    (BUILTIN_RINTF,       "__builtin_rintf", BUILTIN),
    (BUILTIN_RINTL,       "__builtin_rintl", BUILTIN),
    (BUILTIN_NEARBYINT,   "__builtin_nearbyint", BUILTIN),
    (BUILTIN_NEARBYINTF,  "__builtin_nearbyintf", BUILTIN),
    (BUILTIN_NEARBYINTL,  "__builtin_nearbyintl", BUILTIN),
    (BUILTIN_SIN,         "__builtin_sin", BUILTIN),
    (BUILTIN_SINF,        "__builtin_sinf", BUILTIN),
    (BUILTIN_SINL,        "__builtin_sinl", BUILTIN),
    (BUILTIN_COS,         "__builtin_cos", BUILTIN),
    (BUILTIN_COSF,        "__builtin_cosf", BUILTIN),
    (BUILTIN_COSL,        "__builtin_cosl", BUILTIN),
    (BUILTIN_TAN,         "__builtin_tan", BUILTIN),
    (BUILTIN_TANF,        "__builtin_tanf", BUILTIN),
    (BUILTIN_TANL,        "__builtin_tanl", BUILTIN),
    (BUILTIN_ASIN,        "__builtin_asin", BUILTIN),
    (BUILTIN_ASINF,       "__builtin_asinf", BUILTIN),
    (BUILTIN_ASINL,       "__builtin_asinl", BUILTIN),
    (BUILTIN_ACOS,        "__builtin_acos", BUILTIN),
    (BUILTIN_ACOSF,       "__builtin_acosf", BUILTIN),
    (BUILTIN_ACOSL,       "__builtin_acosl", BUILTIN),
    (BUILTIN_ATAN,        "__builtin_atan", BUILTIN),
    (BUILTIN_ATANF,       "__builtin_atanf", BUILTIN),
    (BUILTIN_ATANL,       "__builtin_atanl", BUILTIN),
    (BUILTIN_SINH,        "__builtin_sinh", BUILTIN),
    (BUILTIN_SINHF,       "__builtin_sinhf", BUILTIN),
    (BUILTIN_SINHL,       "__builtin_sinhl", BUILTIN),
    (BUILTIN_COSH,        "__builtin_cosh", BUILTIN),
    (BUILTIN_COSHF,       "__builtin_coshf", BUILTIN),
    (BUILTIN_COSHL,       "__builtin_coshl", BUILTIN),
    (BUILTIN_TANH,        "__builtin_tanh", BUILTIN),
    (BUILTIN_TANHF,       "__builtin_tanhf", BUILTIN),
    (BUILTIN_TANHL,       "__builtin_tanhl", BUILTIN),
    (BUILTIN_ASINH,       "__builtin_asinh", BUILTIN),
    (BUILTIN_ASINHF,      "__builtin_asinhf", BUILTIN),
    (BUILTIN_ASINHL,      "__builtin_asinhl", BUILTIN),
    (BUILTIN_ACOSH,       "__builtin_acosh", BUILTIN),
    (BUILTIN_ACOSHF,      "__builtin_acoshf", BUILTIN),
    (BUILTIN_ACOSHL,      "__builtin_acoshl", BUILTIN),
    (BUILTIN_ATANH,       "__builtin_atanh", BUILTIN),
    (BUILTIN_ATANHF,      "__builtin_atanhf", BUILTIN),
    (BUILTIN_ATANHL,      "__builtin_atanhl", BUILTIN),
    (BUILTIN_EXP,         "__builtin_exp", BUILTIN),
    (BUILTIN_EXPF,        "__builtin_expf", BUILTIN),
    (BUILTIN_EXPL,        "__builtin_expl", BUILTIN),
    (BUILTIN_EXP2,        "__builtin_exp2", BUILTIN),
    (BUILTIN_EXP2F,       "__builtin_exp2f", BUILTIN),
    (BUILTIN_EXP2L,       "__builtin_exp2l", BUILTIN),
    (BUILTIN_EXPM1,       "__builtin_expm1", BUILTIN),
    (BUILTIN_EXPM1F,      "__builtin_expm1f", BUILTIN),
    (BUILTIN_EXPM1L,      "__builtin_expm1l", BUILTIN),
    (BUILTIN_LOG,         "__builtin_log", BUILTIN),
    (BUILTIN_LOGF,        "__builtin_logf", BUILTIN),
    (BUILTIN_LOGL,        "__builtin_logl", BUILTIN),
    (BUILTIN_LOG2,        "__builtin_log2", BUILTIN),
    (BUILTIN_LOG2F,       "__builtin_log2f", BUILTIN),
    (BUILTIN_LOG2L,       "__builtin_log2l", BUILTIN),
    (BUILTIN_LOG10,       "__builtin_log10", BUILTIN),
    (BUILTIN_LOG10F,      "__builtin_log10f", BUILTIN),
    (BUILTIN_LOG10L,      "__builtin_log10l", BUILTIN),
    (BUILTIN_LOG1P,       "__builtin_log1p", BUILTIN),
    (BUILTIN_LOG1PF,      "__builtin_log1pf", BUILTIN),
    (BUILTIN_LOG1PL,      "__builtin_log1pl", BUILTIN),
    (BUILTIN_LOGB,        "__builtin_logb", BUILTIN),
    (BUILTIN_ILOGB,       "__builtin_ilogb", BUILTIN),
    (BUILTIN_ILOGBF,      "__builtin_ilogbf", BUILTIN),
    (BUILTIN_ILOGBL,      "__builtin_ilogbl", BUILTIN),
    (BUILTIN_LOGBF,       "__builtin_logbf", BUILTIN),
    (BUILTIN_LOGBL,       "__builtin_logbl", BUILTIN),
    (BUILTIN_TGAMMA,      "__builtin_tgamma", BUILTIN),
    (BUILTIN_TGAMMAF,     "__builtin_tgammaf", BUILTIN),
    (BUILTIN_TGAMMAL,     "__builtin_tgammal", BUILTIN),
    (BUILTIN_LGAMMA,      "__builtin_lgamma", BUILTIN),
    (BUILTIN_LGAMMAF,     "__builtin_lgammaf", BUILTIN),
    (BUILTIN_LGAMMAL,     "__builtin_lgammal", BUILTIN),
    (BUILTIN_ERF,         "__builtin_erf", BUILTIN),
    (BUILTIN_ERFF,        "__builtin_erff", BUILTIN),
    (BUILTIN_ERFL,        "__builtin_erfl", BUILTIN),
    (BUILTIN_ERFC,        "__builtin_erfc", BUILTIN),
    (BUILTIN_ERFCF,       "__builtin_erfcf", BUILTIN),
    (BUILTIN_ERFCL,       "__builtin_erfcl", BUILTIN),
    (BUILTIN_FMOD,        "__builtin_fmod", BUILTIN),
    (BUILTIN_FMODF,       "__builtin_fmodf", BUILTIN),
    (BUILTIN_FMODL,       "__builtin_fmodl", BUILTIN),
    (BUILTIN_ATAN2,       "__builtin_atan2", BUILTIN),
    (BUILTIN_ATAN2F,      "__builtin_atan2f", BUILTIN),
    (BUILTIN_ATAN2L,      "__builtin_atan2l", BUILTIN),
    (BUILTIN_HYPOT,       "__builtin_hypot", BUILTIN),
    (BUILTIN_HYPOTF,      "__builtin_hypotf", BUILTIN),
    (BUILTIN_HYPOTL,      "__builtin_hypotl", BUILTIN),
    (BUILTIN_FDIM,        "__builtin_fdim", BUILTIN),
    (BUILTIN_FDIMF,       "__builtin_fdimf", BUILTIN),
    (BUILTIN_FDIML,       "__builtin_fdiml", BUILTIN),
    (BUILTIN_REMAINDER,   "__builtin_remainder", BUILTIN),
    (BUILTIN_REMAINDERF,  "__builtin_remainderf", BUILTIN),
    (BUILTIN_REMAINDERL,  "__builtin_remainderl", BUILTIN),
    (BUILTIN_NEXTAFTER,   "__builtin_nextafter", BUILTIN),
    (BUILTIN_NEXTAFTERF,  "__builtin_nextafterf", BUILTIN),
    (BUILTIN_NEXTAFTERL,  "__builtin_nextafterl", BUILTIN),
    (BUILTIN_MODF,        "__builtin_modf", BUILTIN),
    (BUILTIN_MODFF,       "__builtin_modff", BUILTIN),
    (BUILTIN_MODFL,       "__builtin_modfl", BUILTIN),
    (BUILTIN_FREXP,       "__builtin_frexp", BUILTIN),
    (BUILTIN_FREXPF,      "__builtin_frexpf", BUILTIN),
    (BUILTIN_FREXPL,      "__builtin_frexpl", BUILTIN),
    (BUILTIN_LDEXP,       "__builtin_ldexp", BUILTIN),
    (BUILTIN_LDEXPF,      "__builtin_ldexpf", BUILTIN),
    (BUILTIN_LDEXPL,      "__builtin_ldexpl", BUILTIN),
    (BUILTIN_STRCASECMP,  "__builtin_strcasecmp", BUILTIN),
    (BUILTIN_STRNCASECMP, "__builtin_strncasecmp", BUILTIN),
    (BUILTIN_STRDUP,    "__builtin_strdup",   BUILTIN),
    (BUILTIN_STRNDUP,   "__builtin_strndup",  BUILTIN),
    // gcc's equality-only `memcmp`: it answers zero or non-zero rather than
    // an ordering, which lets it use a wider compare. Answering the ordering
    // too is a correct implementation of it.
    (BUILTIN_MEMCMP_EQ, "__builtin_memcmp_eq", BUILTIN),
    // Identity on both targets c17 has. It exists for architectures that
    // encode a flag in the return address -- ARM Thumb sets bit 0 -- and glibc
    // and libgcc unwinders call it unconditionally.
    (BUILTIN_EXTRACT_RETURN_ADDR, "__builtin_extract_return_addr", BUILTIN),
    (BUILTIN_CLEAR_CACHE, "__builtin___clear_cache", BUILTIN),
    (BUILTIN_TRAP,      "__builtin_trap",     BUILTIN),
    (BUILTIN_ABORT,     "__builtin_abort",    BUILTIN),
    (BUILTIN_EXIT,      "__builtin_exit",     BUILTIN),
    (BUILTIN_PRINTF,    "__builtin_printf",   BUILTIN),
    (BUILTIN_SPRINTF,   "__builtin_sprintf",  BUILTIN),
    (BUILTIN_SNPRINTF,  "__builtin_snprintf", BUILTIN),
    (BUILTIN_PUTS,      "__builtin_puts",     BUILTIN),
    (BUILTIN_MALLOC,    "__builtin_malloc",   BUILTIN),
    (BUILTIN_CALLOC,    "__builtin_calloc",   BUILTIN),
    (BUILTIN_REALLOC,   "__builtin_realloc",  BUILTIN),
    (BUILTIN_FREE,      "__builtin_free",     BUILTIN),
    (BUILTIN_MEMCMP,    "__builtin_memcmp",   BUILTIN),
    (BUILTIN_MEMPCPY,   "__builtin_mempcpy",  BUILTIN),
    (BUILTIN_STRCPY,    "__builtin_strcpy",   BUILTIN),
    (BUILTIN_STRNCPY,   "__builtin_strncpy",  BUILTIN),
    (BUILTIN_STPCPY,    "__builtin_stpcpy",   BUILTIN),
    (BUILTIN_STRCAT,    "__builtin_strcat",   BUILTIN),
    (BUILTIN_STRNCAT,   "__builtin_strncat",  BUILTIN),
    (BUILTIN_STRNCMP,   "__builtin_strncmp",  BUILTIN),
    (BUILTIN_STRCHR,    "__builtin_strchr",   BUILTIN),
    (BUILTIN_STRRCHR,   "__builtin_strrchr",  BUILTIN),
    (BUILTIN_STRSTR,    "__builtin_strstr",   BUILTIN),
    (BUILTIN_IMAXABS,   "__builtin_imaxabs",   BUILTIN),
    (BUILTIN_MEMCHR,    "__builtin_memchr",    BUILTIN),
    (BUILTIN_BCOPY,     "__builtin_bcopy",     BUILTIN),
    (BUILTIN_INDEX,     "__builtin_index",     BUILTIN),
    (BUILTIN_RINDEX,    "__builtin_rindex",    BUILTIN),
    (BUILTIN_PUTCHAR,   "__builtin_putchar",   BUILTIN),
    (BUILTIN_STRCSPN,   "__builtin_strcspn",   BUILTIN),
    (BUILTIN_STRSPN,    "__builtin_strspn",    BUILTIN),
    (BUILTIN_STRPBRK,   "__builtin_strpbrk",   BUILTIN),
    (BUILTIN_PRINTF_UNLOCKED, "__builtin_printf_unlocked", BUILTIN),
    (BUILTIN_FPRINTF_UNLOCKED, "__builtin_fprintf_unlocked", BUILTIN),
    (BUILTIN_FPUTS_UNLOCKED, "__builtin_fputs_unlocked", BUILTIN),
    (BUILTIN_FPRINTF,   "__builtin_fprintf",   BUILTIN),
    (BUILTIN_FPUTS,     "__builtin_fputs",     BUILTIN),
    (BUILTIN_FPUTC,     "__builtin_fputc",     BUILTIN),
    (BUILTIN_FWRITE,    "__builtin_fwrite",    BUILTIN),
    // ---- Checked arithmetic (C23 spells these ckd_add and friends) ----
    (BUILTIN_ADD_OVERFLOW, "__builtin_add_overflow", BUILTIN),
    (BUILTIN_ADD_OVERFLOW_P, "__builtin_add_overflow_p", BUILTIN),
    (BUILTIN_SUB_OVERFLOW_P, "__builtin_sub_overflow_p", BUILTIN),
    (BUILTIN_MUL_OVERFLOW_P, "__builtin_mul_overflow_p", BUILTIN),
    (BUILTIN_SUB_OVERFLOW, "__builtin_sub_overflow", BUILTIN),
    (BUILTIN_MUL_OVERFLOW, "__builtin_mul_overflow", BUILTIN),
    (BUILTIN_SADD_OVERFLOW, "__builtin_sadd_overflow", BUILTIN),
    (BUILTIN_SADDL_OVERFLOW, "__builtin_saddl_overflow", BUILTIN),
    (BUILTIN_SADDLL_OVERFLOW, "__builtin_saddll_overflow", BUILTIN),
    (BUILTIN_SSUB_OVERFLOW, "__builtin_ssub_overflow", BUILTIN),
    (BUILTIN_SSUBL_OVERFLOW, "__builtin_ssubl_overflow", BUILTIN),
    (BUILTIN_SSUBLL_OVERFLOW, "__builtin_ssubll_overflow", BUILTIN),
    (BUILTIN_SMUL_OVERFLOW, "__builtin_smul_overflow", BUILTIN),
    (BUILTIN_SMULL_OVERFLOW, "__builtin_smull_overflow", BUILTIN),
    (BUILTIN_SMULLL_OVERFLOW, "__builtin_smulll_overflow", BUILTIN),
    (BUILTIN_UADD_OVERFLOW, "__builtin_uadd_overflow", BUILTIN),
    (BUILTIN_UADDL_OVERFLOW, "__builtin_uaddl_overflow", BUILTIN),
    (BUILTIN_UADDLL_OVERFLOW, "__builtin_uaddll_overflow", BUILTIN),
    (BUILTIN_USUB_OVERFLOW, "__builtin_usub_overflow", BUILTIN),
    (BUILTIN_USUBL_OVERFLOW, "__builtin_usubl_overflow", BUILTIN),
    (BUILTIN_USUBLL_OVERFLOW, "__builtin_usubll_overflow", BUILTIN),
    (BUILTIN_UMUL_OVERFLOW, "__builtin_umul_overflow", BUILTIN),
    (BUILTIN_UMULL_OVERFLOW, "__builtin_umull_overflow", BUILTIN),
    (BUILTIN_UMULLL_OVERFLOW, "__builtin_umulll_overflow", BUILTIN),
    (BUILTIN_ALLOCA,    "__builtin_alloca",   BUILTIN),
    (BUILTIN_MEMSET,    "__builtin_memset",   BUILTIN),
    (BUILTIN_MEMCPY,    "__builtin_memcpy",   BUILTIN),
    (BUILTIN_MEMMOVE,   "__builtin_memmove",  BUILTIN),
    (BUILTIN_CONSTANT_P, "__builtin_constant_p", BUILTIN),
    (BUILTIN_TYPES_COMPATIBLE_P, "__builtin_types_compatible_p", BUILTIN),
    (BUILTIN_CLASSIFY_TYPE, "__builtin_classify_type", BUILTIN),
    (BUILTIN_UNREACHABLE, "__builtin_unreachable", BUILTIN),
    (BUILTIN_UNWIND_INIT, "__builtin_unwind_init", BUILTIN),
    (BUILTIN_SETJMP,    "__builtin_setjmp",   BUILTIN),
    (BUILTIN_LONGJMP,   "__builtin_longjmp",  BUILTIN),
    (BUILTIN_OFFSETOF,  "__builtin_offsetof", BUILTIN),
    (OFFSETOF,          "offsetof",           BUILTIN),
    (BUILTIN_INF,       "__builtin_inf",      BUILTIN),
    (BUILTIN_INFF,      "__builtin_inff",     BUILTIN),
    (BUILTIN_INFL,      "__builtin_infl",     BUILTIN),
    (BUILTIN_HUGE_VAL,  "__builtin_huge_val", BUILTIN),
    (BUILTIN_HUGE_VALF, "__builtin_huge_valf", BUILTIN),
    (BUILTIN_HUGE_VALL, "__builtin_huge_vall", BUILTIN),
    (BUILTIN_INFF16,    "__builtin_inff16",      BUILTIN),
    (BUILTIN_INFF32,    "__builtin_inff32",      BUILTIN),
    (BUILTIN_INFF64,    "__builtin_inff64",      BUILTIN),
    (BUILTIN_INFF128,   "__builtin_inff128",     BUILTIN),
    (BUILTIN_HUGE_VALF16, "__builtin_huge_valf16", BUILTIN),
    (BUILTIN_HUGE_VALF32, "__builtin_huge_valf32", BUILTIN),
    (BUILTIN_HUGE_VALF64, "__builtin_huge_valf64", BUILTIN),
    (BUILTIN_HUGE_VALF128, "__builtin_huge_valf128", BUILTIN),
    (BUILTIN_INFF32X,   "__builtin_inff32x",     BUILTIN),
    (BUILTIN_INFF64X,   "__builtin_inff64x",     BUILTIN),
    (BUILTIN_HUGE_VALF32X, "__builtin_huge_valf32x", BUILTIN),
    (BUILTIN_HUGE_VALF64X, "__builtin_huge_valf64x", BUILTIN),
    // gcc's `q` spellings name `__float128`, which is `_Float128` here.
    (BUILTIN_INFQ,      "__builtin_infq",        BUILTIN),
    (BUILTIN_HUGE_VALQ, "__builtin_huge_valq",   BUILTIN),
    (BUILTIN_NANQ,      "__builtin_nanq",        BUILTIN),
    (BUILTIN_NANSQ,     "__builtin_nansq",       BUILTIN),
    // The _FloatN spellings of fabs and copysign, which glibc's
    // <bits/floatn.h> and <math.h> reach for.
    (BUILTIN_FABSF32,   "__builtin_fabsf32",     BUILTIN),
    (BUILTIN_FABSF64,   "__builtin_fabsf64",     BUILTIN),
    (BUILTIN_FABSF128,  "__builtin_fabsf128",    BUILTIN),
    (BUILTIN_FABSF32X,  "__builtin_fabsf32x",    BUILTIN),
    (BUILTIN_FABSF64X,  "__builtin_fabsf64x",    BUILTIN),
    (BUILTIN_FABSQ,     "__builtin_fabsq",       BUILTIN),
    (BUILTIN_COPYSIGNF32,  "__builtin_copysignf32",  BUILTIN),
    (BUILTIN_COPYSIGNF64,  "__builtin_copysignf64",  BUILTIN),
    (BUILTIN_COPYSIGNF128, "__builtin_copysignf128", BUILTIN),
    (BUILTIN_COPYSIGNF32X, "__builtin_copysignf32x", BUILTIN),
    (BUILTIN_COPYSIGNF64X, "__builtin_copysignf64x", BUILTIN),
    (BUILTIN_COPYSIGNQ,    "__builtin_copysignq",    BUILTIN),
    (BUILTIN_SQRTF128,  "__builtin_sqrtf128",    BUILTIN),
    (BUILTIN_FMAF128,   "__builtin_fmaf128",     BUILTIN),
    // roundeven, lrint, lround, scalbn in every type, and the _FloatN
    // forms of the libm functions glibc's sources name.
    (BUILTIN_ROUNDEVEN, "__builtin_roundeven", BUILTIN),
    (BUILTIN_ROUNDEVENF, "__builtin_roundevenf", BUILTIN),
    (BUILTIN_ROUNDEVENL, "__builtin_roundevenl", BUILTIN),
    (BUILTIN_ROUNDEVENF32, "__builtin_roundevenf32", BUILTIN),
    (BUILTIN_ROUNDEVENF64, "__builtin_roundevenf64", BUILTIN),
    (BUILTIN_ROUNDEVENF32X, "__builtin_roundevenf32x", BUILTIN),
    (BUILTIN_ROUNDEVENF64X, "__builtin_roundevenf64x", BUILTIN),
    (BUILTIN_ROUNDEVENF128, "__builtin_roundevenf128", BUILTIN),
    (BUILTIN_LRINT, "__builtin_lrint", BUILTIN),
    (BUILTIN_LRINTF, "__builtin_lrintf", BUILTIN),
    (BUILTIN_LRINTL, "__builtin_lrintl", BUILTIN),
    (BUILTIN_LRINTF32, "__builtin_lrintf32", BUILTIN),
    (BUILTIN_LRINTF64, "__builtin_lrintf64", BUILTIN),
    (BUILTIN_LRINTF32X, "__builtin_lrintf32x", BUILTIN),
    (BUILTIN_LRINTF64X, "__builtin_lrintf64x", BUILTIN),
    (BUILTIN_LRINTF128, "__builtin_lrintf128", BUILTIN),
    (BUILTIN_LLRINT, "__builtin_llrint", BUILTIN),
    (BUILTIN_LLRINTF, "__builtin_llrintf", BUILTIN),
    (BUILTIN_LLRINTL, "__builtin_llrintl", BUILTIN),
    (BUILTIN_LLRINTF32, "__builtin_llrintf32", BUILTIN),
    (BUILTIN_LLRINTF64, "__builtin_llrintf64", BUILTIN),
    (BUILTIN_LLRINTF32X, "__builtin_llrintf32x", BUILTIN),
    (BUILTIN_LLRINTF64X, "__builtin_llrintf64x", BUILTIN),
    (BUILTIN_LLRINTF128, "__builtin_llrintf128", BUILTIN),
    (BUILTIN_LROUND, "__builtin_lround", BUILTIN),
    (BUILTIN_LROUNDF, "__builtin_lroundf", BUILTIN),
    (BUILTIN_LROUNDL, "__builtin_lroundl", BUILTIN),
    (BUILTIN_LROUNDF32, "__builtin_lroundf32", BUILTIN),
    (BUILTIN_LROUNDF64, "__builtin_lroundf64", BUILTIN),
    (BUILTIN_LROUNDF32X, "__builtin_lroundf32x", BUILTIN),
    (BUILTIN_LROUNDF64X, "__builtin_lroundf64x", BUILTIN),
    (BUILTIN_LROUNDF128, "__builtin_lroundf128", BUILTIN),
    (BUILTIN_LLROUND, "__builtin_llround", BUILTIN),
    (BUILTIN_LLROUNDF, "__builtin_llroundf", BUILTIN),
    (BUILTIN_LLROUNDL, "__builtin_llroundl", BUILTIN),
    (BUILTIN_LLROUNDF32, "__builtin_llroundf32", BUILTIN),
    (BUILTIN_LLROUNDF64, "__builtin_llroundf64", BUILTIN),
    (BUILTIN_LLROUNDF32X, "__builtin_llroundf32x", BUILTIN),
    (BUILTIN_LLROUNDF64X, "__builtin_llroundf64x", BUILTIN),
    (BUILTIN_LLROUNDF128, "__builtin_llroundf128", BUILTIN),
    (BUILTIN_SCALBN, "__builtin_scalbn", BUILTIN),
    (BUILTIN_SCALBNF, "__builtin_scalbnf", BUILTIN),
    (BUILTIN_SCALBNL, "__builtin_scalbnl", BUILTIN),
    (BUILTIN_SCALBNF32, "__builtin_scalbnf32", BUILTIN),
    (BUILTIN_SCALBNF64, "__builtin_scalbnf64", BUILTIN),
    (BUILTIN_SCALBNF32X, "__builtin_scalbnf32x", BUILTIN),
    (BUILTIN_SCALBNF64X, "__builtin_scalbnf64x", BUILTIN),
    (BUILTIN_SCALBNF128, "__builtin_scalbnf128", BUILTIN),
    (BUILTIN_SCALBLN, "__builtin_scalbln", BUILTIN),
    (BUILTIN_SCALBLNF, "__builtin_scalblnf", BUILTIN),
    (BUILTIN_SCALBLNL, "__builtin_scalblnl", BUILTIN),
    (BUILTIN_SCALBLNF32, "__builtin_scalblnf32", BUILTIN),
    (BUILTIN_SCALBLNF64, "__builtin_scalblnf64", BUILTIN),
    (BUILTIN_SCALBLNF32X, "__builtin_scalblnf32x", BUILTIN),
    (BUILTIN_SCALBLNF64X, "__builtin_scalblnf64x", BUILTIN),
    (BUILTIN_SCALBLNF128, "__builtin_scalblnf128", BUILTIN),
    (BUILTIN_ROUNDF32, "__builtin_roundf32", BUILTIN),
    (BUILTIN_ROUNDF64, "__builtin_roundf64", BUILTIN),
    (BUILTIN_ROUNDF32X, "__builtin_roundf32x", BUILTIN),
    (BUILTIN_ROUNDF64X, "__builtin_roundf64x", BUILTIN),
    (BUILTIN_ROUNDF128, "__builtin_roundf128", BUILTIN),
    (BUILTIN_RINTF32, "__builtin_rintf32", BUILTIN),
    (BUILTIN_RINTF64, "__builtin_rintf64", BUILTIN),
    (BUILTIN_RINTF32X, "__builtin_rintf32x", BUILTIN),
    (BUILTIN_RINTF64X, "__builtin_rintf64x", BUILTIN),
    (BUILTIN_RINTF128, "__builtin_rintf128", BUILTIN),
    (BUILTIN_NEARBYINTF32, "__builtin_nearbyintf32", BUILTIN),
    (BUILTIN_NEARBYINTF64, "__builtin_nearbyintf64", BUILTIN),
    (BUILTIN_NEARBYINTF32X, "__builtin_nearbyintf32x", BUILTIN),
    (BUILTIN_NEARBYINTF64X, "__builtin_nearbyintf64x", BUILTIN),
    (BUILTIN_NEARBYINTF128, "__builtin_nearbyintf128", BUILTIN),
    (BUILTIN_TRUNCF32, "__builtin_truncf32", BUILTIN),
    (BUILTIN_TRUNCF64, "__builtin_truncf64", BUILTIN),
    (BUILTIN_TRUNCF32X, "__builtin_truncf32x", BUILTIN),
    (BUILTIN_TRUNCF64X, "__builtin_truncf64x", BUILTIN),
    (BUILTIN_TRUNCF128, "__builtin_truncf128", BUILTIN),
    (BUILTIN_FLOORF32, "__builtin_floorf32", BUILTIN),
    (BUILTIN_FLOORF64, "__builtin_floorf64", BUILTIN),
    (BUILTIN_FLOORF32X, "__builtin_floorf32x", BUILTIN),
    (BUILTIN_FLOORF64X, "__builtin_floorf64x", BUILTIN),
    (BUILTIN_FLOORF128, "__builtin_floorf128", BUILTIN),
    (BUILTIN_CEILF32, "__builtin_ceilf32", BUILTIN),
    (BUILTIN_CEILF64, "__builtin_ceilf64", BUILTIN),
    (BUILTIN_CEILF32X, "__builtin_ceilf32x", BUILTIN),
    (BUILTIN_CEILF64X, "__builtin_ceilf64x", BUILTIN),
    (BUILTIN_CEILF128, "__builtin_ceilf128", BUILTIN),
    (BUILTIN_LOGBF32, "__builtin_logbf32", BUILTIN),
    (BUILTIN_LOGBF64, "__builtin_logbf64", BUILTIN),
    (BUILTIN_LOGBF32X, "__builtin_logbf32x", BUILTIN),
    (BUILTIN_LOGBF64X, "__builtin_logbf64x", BUILTIN),
    (BUILTIN_LOGBF128, "__builtin_logbf128", BUILTIN),
    (BUILTIN_SQRTF32, "__builtin_sqrtf32", BUILTIN),
    (BUILTIN_SQRTF64, "__builtin_sqrtf64", BUILTIN),
    (BUILTIN_SQRTF32X, "__builtin_sqrtf32x", BUILTIN),
    (BUILTIN_SQRTF64X, "__builtin_sqrtf64x", BUILTIN),
    (BUILTIN_CBRTF32, "__builtin_cbrtf32", BUILTIN),
    (BUILTIN_CBRTF64, "__builtin_cbrtf64", BUILTIN),
    (BUILTIN_CBRTF32X, "__builtin_cbrtf32x", BUILTIN),
    (BUILTIN_CBRTF64X, "__builtin_cbrtf64x", BUILTIN),
    (BUILTIN_CBRTF128, "__builtin_cbrtf128", BUILTIN),
    (BUILTIN_FMINF32, "__builtin_fminf32", BUILTIN),
    (BUILTIN_FMINF64, "__builtin_fminf64", BUILTIN),
    (BUILTIN_FMINF32X, "__builtin_fminf32x", BUILTIN),
    (BUILTIN_FMINF64X, "__builtin_fminf64x", BUILTIN),
    (BUILTIN_FMINF128, "__builtin_fminf128", BUILTIN),
    (BUILTIN_FMAXF32, "__builtin_fmaxf32", BUILTIN),
    (BUILTIN_FMAXF64, "__builtin_fmaxf64", BUILTIN),
    (BUILTIN_FMAXF32X, "__builtin_fmaxf32x", BUILTIN),
    (BUILTIN_FMAXF64X, "__builtin_fmaxf64x", BUILTIN),
    (BUILTIN_FMAXF128, "__builtin_fmaxf128", BUILTIN),
    (BUILTIN_FMAF32, "__builtin_fmaf32", BUILTIN),
    (BUILTIN_FMAF64, "__builtin_fmaf64", BUILTIN),
    (BUILTIN_FMAF32X, "__builtin_fmaf32x", BUILTIN),
    (BUILTIN_FMAF64X, "__builtin_fmaf64x", BUILTIN),
    (BUILTIN_ILOGBF32, "__builtin_ilogbf32", BUILTIN),
    (BUILTIN_ILOGBF64, "__builtin_ilogbf64", BUILTIN),
    (BUILTIN_ILOGBF32X, "__builtin_ilogbf32x", BUILTIN),
    (BUILTIN_ILOGBF64X, "__builtin_ilogbf64x", BUILTIN),
    (BUILTIN_ILOGBF128, "__builtin_ilogbf128", BUILTIN),
    (BUILTIN_LDEXPF32, "__builtin_ldexpf32", BUILTIN),
    (BUILTIN_LDEXPF64, "__builtin_ldexpf64", BUILTIN),
    (BUILTIN_LDEXPF32X, "__builtin_ldexpf32x", BUILTIN),
    (BUILTIN_LDEXPF64X, "__builtin_ldexpf64x", BUILTIN),
    (BUILTIN_LDEXPF128, "__builtin_ldexpf128", BUILTIN),
    (BUILTIN_FREXPF32, "__builtin_frexpf32", BUILTIN),
    (BUILTIN_FREXPF64, "__builtin_frexpf64", BUILTIN),
    (BUILTIN_FREXPF32X, "__builtin_frexpf32x", BUILTIN),
    (BUILTIN_FREXPF64X, "__builtin_frexpf64x", BUILTIN),
    (BUILTIN_FREXPF128, "__builtin_frexpf128", BUILTIN),
    (BUILTIN_MODFF32, "__builtin_modff32", BUILTIN),
    (BUILTIN_MODFF64, "__builtin_modff64", BUILTIN),
    (BUILTIN_MODFF32X, "__builtin_modff32x", BUILTIN),
    (BUILTIN_MODFF64X, "__builtin_modff64x", BUILTIN),
    (BUILTIN_MODFF128, "__builtin_modff128", BUILTIN),
    (BUILTIN_NEXTAFTERF32, "__builtin_nextafterf32", BUILTIN),
    (BUILTIN_NEXTAFTERF64, "__builtin_nextafterf64", BUILTIN),
    (BUILTIN_NEXTAFTERF32X, "__builtin_nextafterf32x", BUILTIN),
    (BUILTIN_NEXTAFTERF64X, "__builtin_nextafterf64x", BUILTIN),
    (BUILTIN_NEXTAFTERF128, "__builtin_nextafterf128", BUILTIN),
    (BUILTIN_FDIMF32, "__builtin_fdimf32", BUILTIN),
    (BUILTIN_FDIMF64, "__builtin_fdimf64", BUILTIN),
    (BUILTIN_FDIMF32X, "__builtin_fdimf32x", BUILTIN),
    (BUILTIN_FDIMF64X, "__builtin_fdimf64x", BUILTIN),
    (BUILTIN_FDIMF128, "__builtin_fdimf128", BUILTIN),
    (BUILTIN_HYPOTF32, "__builtin_hypotf32", BUILTIN),
    (BUILTIN_HYPOTF64, "__builtin_hypotf64", BUILTIN),
    (BUILTIN_HYPOTF32X, "__builtin_hypotf32x", BUILTIN),
    (BUILTIN_HYPOTF64X, "__builtin_hypotf64x", BUILTIN),
    (BUILTIN_HYPOTF128, "__builtin_hypotf128", BUILTIN),
    (BUILTIN_FABS,      "__builtin_fabs",     BUILTIN),
    (BUILTIN_FABSF,     "__builtin_fabsf",    BUILTIN),
    (BUILTIN_FABSL,     "__builtin_fabsl",    BUILTIN),
    (BUILTIN_ISNAN,     "__builtin_isnan",    BUILTIN),
    (BUILTIN_ISINF,     "__builtin_isinf",    BUILTIN),
    (BUILTIN_ISNANF,    "__builtin_isnanf",   BUILTIN),
    (BUILTIN_ISNANL,    "__builtin_isnanl",   BUILTIN),
    (BUILTIN_ISINFF,    "__builtin_isinff",   BUILTIN),
    (BUILTIN_ISINFL,    "__builtin_isinfl",   BUILTIN),
    (BUILTIN_CONJ,      "__builtin_conj",     BUILTIN),
    (BUILTIN_CONJF,     "__builtin_conjf",    BUILTIN),
    (BUILTIN_CONJL,     "__builtin_conjl",    BUILTIN),
    (BUILTIN_CREAL,     "__builtin_creal",    BUILTIN),
    (BUILTIN_CREALF,    "__builtin_crealf",   BUILTIN),
    (BUILTIN_CREALL,    "__builtin_creall",   BUILTIN),
    (BUILTIN_CIMAG,     "__builtin_cimag",    BUILTIN),
    (BUILTIN_CIMAGF,    "__builtin_cimagf",   BUILTIN),
    (BUILTIN_CIMAGL,    "__builtin_cimagl",   BUILTIN),
    (BUILTIN_ISINF_SIGN, "__builtin_isinf_sign", BUILTIN),
    (BUILTIN_ISFINITE,  "__builtin_isfinite", BUILTIN),
    (BUILTIN_ISNORMAL,  "__builtin_isnormal", BUILTIN),
    (BUILTIN_FPCLASSIFY,"__builtin_fpclassify", BUILTIN),
    (BUILTIN_ISSIGNALING, "__builtin_issignaling", BUILTIN),
    // gcc's atomic builtins. These are not `__builtin_`-prefixed, and they
    // are reserved spellings all the same: `__sync_` and `__atomic_` both
    // start with two underscores, so no conforming program defines one.
    (SYNC_FETCH_AND_ADD,  "__sync_fetch_and_add", BUILTIN),
    (SYNC_ADD_AND_FETCH,  "__sync_add_and_fetch", BUILTIN),
    (ATOMIC_FETCH_ADD,    "__atomic_fetch_add", BUILTIN),
    (ATOMIC_ADD_FETCH,    "__atomic_add_fetch", BUILTIN),
    (SYNC_FETCH_AND_SUB,  "__sync_fetch_and_sub", BUILTIN),
    (SYNC_SUB_AND_FETCH,  "__sync_sub_and_fetch", BUILTIN),
    (ATOMIC_FETCH_SUB,    "__atomic_fetch_sub", BUILTIN),
    (ATOMIC_SUB_FETCH,    "__atomic_sub_fetch", BUILTIN),
    (SYNC_FETCH_AND_AND,  "__sync_fetch_and_and", BUILTIN),
    (SYNC_AND_AND_FETCH,  "__sync_and_and_fetch", BUILTIN),
    (ATOMIC_FETCH_AND,    "__atomic_fetch_and", BUILTIN),
    (ATOMIC_AND_FETCH,    "__atomic_and_fetch", BUILTIN),
    (SYNC_FETCH_AND_OR,   "__sync_fetch_and_or", BUILTIN),
    (SYNC_OR_AND_FETCH,   "__sync_or_and_fetch", BUILTIN),
    (ATOMIC_FETCH_OR,     "__atomic_fetch_or", BUILTIN),
    (ATOMIC_OR_FETCH,     "__atomic_or_fetch", BUILTIN),
    (SYNC_FETCH_AND_XOR,  "__sync_fetch_and_xor", BUILTIN),
    (SYNC_XOR_AND_FETCH,  "__sync_xor_and_fetch", BUILTIN),
    (ATOMIC_FETCH_XOR,    "__atomic_fetch_xor", BUILTIN),
    (ATOMIC_XOR_FETCH,    "__atomic_xor_fetch", BUILTIN),
    (SYNC_FETCH_AND_NAND, "__sync_fetch_and_nand", BUILTIN),
    (SYNC_NAND_AND_FETCH, "__sync_nand_and_fetch", BUILTIN),
    (ATOMIC_FETCH_NAND,   "__atomic_fetch_nand", BUILTIN),
    (ATOMIC_NAND_FETCH,   "__atomic_nand_fetch", BUILTIN),
    (SYNC_BOOL_COMPARE_AND_SWAP, "__sync_bool_compare_and_swap", BUILTIN),
    (SYNC_VAL_COMPARE_AND_SWAP, "__sync_val_compare_and_swap", BUILTIN),
    (SYNC_LOCK_TEST_AND_SET, "__sync_lock_test_and_set", BUILTIN),
    (SYNC_LOCK_RELEASE,   "__sync_lock_release", BUILTIN),
    (SYNC_SYNCHRONIZE,    "__sync_synchronize", BUILTIN),
    (ATOMIC_LOAD_N,       "__atomic_load_n", BUILTIN),
    (ATOMIC_STORE_N,      "__atomic_store_n", BUILTIN),
    (ATOMIC_EXCHANGE_N,   "__atomic_exchange_n", BUILTIN),
    (ATOMIC_COMPARE_EXCHANGE_N, "__atomic_compare_exchange_n", BUILTIN),
    (ATOMIC_TEST_AND_SET, "__atomic_test_and_set", BUILTIN),
    (ATOMIC_CLEAR,        "__atomic_clear", BUILTIN),
    (ATOMIC_THREAD_FENCE, "__atomic_thread_fence", BUILTIN),
    (ATOMIC_SIGNAL_FENCE, "__atomic_signal_fence", BUILTIN),
    (ATOMIC_ALWAYS_LOCK_FREE, "__atomic_always_lock_free", BUILTIN),
    (ATOMIC_IS_LOCK_FREE, "__atomic_is_lock_free", BUILTIN),

    // C99 7.12.14, the unordered-safe relations. glibc's <math.h> *defines*
    // `isgreater` and its siblings as these, so a translation unit that
    // includes <math.h> and uses one does not compile without them.
    (BUILTIN_ISGREATER, "__builtin_isgreater", BUILTIN),
    (BUILTIN_ISGREATEREQUAL, "__builtin_isgreaterequal", BUILTIN),
    (BUILTIN_ISLESS,    "__builtin_isless",   BUILTIN),
    (BUILTIN_ISLESSEQUAL, "__builtin_islessequal", BUILTIN),
    (BUILTIN_ISLESSGREATER, "__builtin_islessgreater", BUILTIN),
    (BUILTIN_ISUNORDERED, "__builtin_isunordered", BUILTIN),
    (BUILTIN_ISEQSIG,   "__builtin_iseqsig",  BUILTIN),
    (BUILTIN_SIGNBIT,   "__builtin_signbit",  BUILTIN),
    (BUILTIN_SIGNBITF,  "__builtin_signbitf", BUILTIN),
    (BUILTIN_SIGNBITL,  "__builtin_signbitl", BUILTIN),
    (BUILTIN_NAN,       "__builtin_nan",      BUILTIN),
    (BUILTIN_NANF,      "__builtin_nanf",     BUILTIN),
    (BUILTIN_NANL,      "__builtin_nanl",     BUILTIN),
    (BUILTIN_NANS,      "__builtin_nans",     BUILTIN),
    (BUILTIN_NANSF,     "__builtin_nansf",    BUILTIN),
    (BUILTIN_NANSL,     "__builtin_nansl",    BUILTIN),
    (BUILTIN_NANF16,    "__builtin_nanf16",      BUILTIN),
    (BUILTIN_NANF32,    "__builtin_nanf32",      BUILTIN),
    (BUILTIN_NANF64,    "__builtin_nanf64",      BUILTIN),
    (BUILTIN_NANF128,   "__builtin_nanf128",     BUILTIN),
    (BUILTIN_NANF32X,   "__builtin_nanf32x",     BUILTIN),
    (BUILTIN_NANF64X,   "__builtin_nanf64x",     BUILTIN),
    (BUILTIN_NANSF16,   "__builtin_nansf16",     BUILTIN),
    (BUILTIN_NANSF32,   "__builtin_nansf32",     BUILTIN),
    (BUILTIN_NANSF64,   "__builtin_nansf64",     BUILTIN),
    (BUILTIN_NANSF128,  "__builtin_nansf128",    BUILTIN),
    (BUILTIN_NANSF32X,  "__builtin_nansf32x",    BUILTIN),
    (BUILTIN_NANSF64X,  "__builtin_nansf64x",    BUILTIN),
    (BUILTIN_COMPLEX,   "__builtin_complex",  BUILTIN),
    (BUILTIN_SHUFFLE,   "__builtin_shuffle",  BUILTIN),
    (BUILTIN_SHUFFLEVECTOR, "__builtin_shufflevector", BUILTIN),
    (BUILTIN_CONVERTVECTOR, "__builtin_convertvector", BUILTIN),
    (BUILTIN_EXPECT,    "__builtin_expect",   BUILTIN),
    (BUILTIN_ASSUME_ALIGNED, "__builtin_assume_aligned", BUILTIN),
    (BUILTIN_PREFETCH,  "__builtin_prefetch", BUILTIN),
    (BUILTIN_FLT_ROUNDS, "__builtin_flt_rounds", BUILTIN),
    (BUILTIN_FRAME_ADDRESS, "__builtin_frame_address", BUILTIN),
    (BUILTIN_RETURN_ADDRESS, "__builtin_return_address", BUILTIN),
    (BUILTIN_OBJECT_SIZE, "__builtin_object_size", BUILTIN),
    (BUILTIN_DYNAMIC_OBJECT_SIZE, "__builtin_dynamic_object_size", BUILTIN),
    (BUILTIN_EXPECT_WITH_PROBABILITY, "__builtin_expect_with_probability", BUILTIN),
    (BUILTIN_FILE,      "__builtin_FILE",     BUILTIN),
    // x86's CPU detection, read from libgcc's (and compiler-rt's)
    // `__cpu_model`, filled in by `__cpu_indicator_init`.
    (BUILTIN_CPU_INIT,     "__builtin_cpu_init",     BUILTIN | X86_64_ONLY),
    (BUILTIN_CPU_SUPPORTS, "__builtin_cpu_supports", BUILTIN | X86_64_ONLY),
    (BUILTIN_CPU_IS,       "__builtin_cpu_is",       BUILTIN | X86_64_ONLY),
    // One machine instruction each: `parse::ast::MachineOp`.
    (BUILTIN_IA32_RDTSC,    "__builtin_ia32_rdtsc",    BUILTIN | X86_64_ONLY),
    (BUILTIN_IA32_RDTSCP,   "__builtin_ia32_rdtscp",   BUILTIN | X86_64_ONLY),
    (BUILTIN_IA32_CVTPD2PS, "__builtin_ia32_cvtpd2ps", BUILTIN | X86_64_ONLY),
    (BUILTIN_THREAD_POINTER, "__builtin_thread_pointer", BUILTIN),
    // <tgmath.h>'s: the call its arguments pick of its functions.
    (BUILTIN_TGMATH,        "__builtin_tgmath",        BUILTIN),
    (CPU_MODEL,            "__cpu_model",            0),
    (CPU_FEATURES2,        "__cpu_features2",        0),
    (CPU_INDICATOR_INIT,   "__cpu_indicator_init",   0),
    (BUILTIN_LINE,      "__builtin_LINE",     BUILTIN),
    (BUILTIN_FUNCTION,  "__builtin_FUNCTION", BUILTIN),
    (_, "__builtin___snprintf_chk", BUILTIN),
    (_, "__builtin___vsnprintf_chk", BUILTIN),
    (_, "__builtin___sprintf_chk", BUILTIN),
    (_, "__builtin___fprintf_chk", BUILTIN),
    (_, "__builtin___printf_chk", BUILTIN),
    (_, "__builtin___memcpy_chk", BUILTIN),
    (_, "__builtin___memmove_chk", BUILTIN),
    (_, "__builtin___memset_chk", BUILTIN),
    (_, "__builtin___stpcpy_chk", BUILTIN),
    (_, "__builtin___strcat_chk", BUILTIN),
    (_, "__builtin___strcpy_chk", BUILTIN),
    (_, "__builtin___strncat_chk", BUILTIN),
    (_, "__builtin___strncpy_chk", BUILTIN),
    (_, "__builtin___stpncpy_chk", BUILTIN),
    (_, "__builtin___vsprintf_chk", BUILTIN),
    (_, "__builtin___memccpy_chk", BUILTIN),
    (_, "__builtin___strlcpy_chk", BUILTIN),
    (_, "__builtin___strlcat_chk", BUILTIN),

    // ---- C11 atomic builtins (BUILTIN) ----
    (C11_ATOMIC_INIT,    "__c11_atomic_init",    BUILTIN),
    (C11_ATOMIC_LOAD,    "__c11_atomic_load",    BUILTIN),
    (C11_ATOMIC_STORE,   "__c11_atomic_store",   BUILTIN),
    (C11_ATOMIC_EXCHANGE, "__c11_atomic_exchange", BUILTIN),
    (C11_ATOMIC_COMPARE_EXCHANGE_STRONG, "__c11_atomic_compare_exchange_strong", BUILTIN),
    (C11_ATOMIC_COMPARE_EXCHANGE_WEAK, "__c11_atomic_compare_exchange_weak", BUILTIN),
    (C11_ATOMIC_FETCH_ADD, "__c11_atomic_fetch_add", BUILTIN),
    (C11_ATOMIC_FETCH_SUB, "__c11_atomic_fetch_sub", BUILTIN),
    (C11_ATOMIC_FETCH_AND, "__c11_atomic_fetch_and", BUILTIN),
    (C11_ATOMIC_FETCH_OR,  "__c11_atomic_fetch_or",  BUILTIN),
    (C11_ATOMIC_FETCH_XOR, "__c11_atomic_fetch_xor", BUILTIN),
    (C11_ATOMIC_THREAD_FENCE, "__c11_atomic_thread_fence", BUILTIN),
    (C11_ATOMIC_SIGNAL_FENCE, "__c11_atomic_signal_fence", BUILTIN),

    // ---- Library functions the parser special-cases ----
    // gcc predefines bare `alloca` as well as `__builtin_alloca`, and code in
    // the wild calls it without including <alloca.h>. Tagged 0, not BUILTIN:
    // `__has_builtin` asks about the reserved spelling, and a user declaration
    // may still displace this one (see `builtin_is_shadowed`).
    (ALLOCA,            "alloca",            0),

    // ---- Plain spellings of libm entry points ----
    // gcc recognizes the standard names whether or not <math.h> was included,
    // which is what lets `fabs(x) < 0.0` fold to 0 in a program that only
    // declares `extern double fabs(double);`. Tagged 0 for the same reason
    // `alloca` is, and displaceable the same way (see `builtin_is_shadowed`).
    // The magnitude and sign functions are each one bit operation.
    (FABS,              "fabs",              0),
    (FABSF,             "fabsf",             0),
    (FABSL,             "fabsl",             0),
    (FABSF32,           "fabsf32",           0),
    (FABSF64,           "fabsf64",           0),
    (FABSF128,          "fabsf128",          0),
    (FABSF32X,          "fabsf32x",          0),
    (FABSF64X,          "fabsf64x",          0),
    (FABSQ,             "fabsq",             0),
    (COPYSIGNF32,       "copysignf32",       0),
    (COPYSIGNF64,       "copysignf64",       0),
    (COPYSIGNF128,      "copysignf128",      0),
    (COPYSIGNF32X,      "copysignf32x",      0),
    (COPYSIGNF64X,      "copysignf64x",      0),
    (COPYSIGNQ,         "copysignq",         0),
    (SQRTF128,          "sqrtf128",          0),
    (FMAF128,           "fmaf128",           0),
    (ROUNDEVEN, "roundeven", 0),
    (ROUNDEVENF, "roundevenf", 0),
    (ROUNDEVENL, "roundevenl", 0),
    (ROUNDEVENF32, "roundevenf32", 0),
    (ROUNDEVENF64, "roundevenf64", 0),
    (ROUNDEVENF32X, "roundevenf32x", 0),
    (ROUNDEVENF64X, "roundevenf64x", 0),
    (ROUNDEVENF128, "roundevenf128", 0),
    (LRINT, "lrint", 0),
    (LRINTF, "lrintf", 0),
    (LRINTL, "lrintl", 0),
    (LRINTF32, "lrintf32", 0),
    (LRINTF64, "lrintf64", 0),
    (LRINTF32X, "lrintf32x", 0),
    (LRINTF64X, "lrintf64x", 0),
    (LRINTF128, "lrintf128", 0),
    (LLRINT, "llrint", 0),
    (LLRINTF, "llrintf", 0),
    (LLRINTL, "llrintl", 0),
    (LLRINTF32, "llrintf32", 0),
    (LLRINTF64, "llrintf64", 0),
    (LLRINTF32X, "llrintf32x", 0),
    (LLRINTF64X, "llrintf64x", 0),
    (LLRINTF128, "llrintf128", 0),
    (LROUND, "lround", 0),
    (LROUNDF, "lroundf", 0),
    (LROUNDL, "lroundl", 0),
    (LROUNDF32, "lroundf32", 0),
    (LROUNDF64, "lroundf64", 0),
    (LROUNDF32X, "lroundf32x", 0),
    (LROUNDF64X, "lroundf64x", 0),
    (LROUNDF128, "lroundf128", 0),
    (LLROUND, "llround", 0),
    (LLROUNDF, "llroundf", 0),
    (LLROUNDL, "llroundl", 0),
    (LLROUNDF32, "llroundf32", 0),
    (LLROUNDF64, "llroundf64", 0),
    (LLROUNDF32X, "llroundf32x", 0),
    (LLROUNDF64X, "llroundf64x", 0),
    (LLROUNDF128, "llroundf128", 0),
    (SCALBN, "scalbn", 0),
    (SCALBNF, "scalbnf", 0),
    (SCALBNL, "scalbnl", 0),
    (SCALBNF32, "scalbnf32", 0),
    (SCALBNF64, "scalbnf64", 0),
    (SCALBNF32X, "scalbnf32x", 0),
    (SCALBNF64X, "scalbnf64x", 0),
    (SCALBNF128, "scalbnf128", 0),
    (SCALBLN, "scalbln", 0),
    (SCALBLNF, "scalblnf", 0),
    (SCALBLNL, "scalblnl", 0),
    (SCALBLNF32, "scalblnf32", 0),
    (SCALBLNF64, "scalblnf64", 0),
    (SCALBLNF32X, "scalblnf32x", 0),
    (SCALBLNF64X, "scalblnf64x", 0),
    (SCALBLNF128, "scalblnf128", 0),
    (ROUNDF32, "roundf32", 0),
    (ROUNDF64, "roundf64", 0),
    (ROUNDF32X, "roundf32x", 0),
    (ROUNDF64X, "roundf64x", 0),
    (ROUNDF128, "roundf128", 0),
    (RINTF32, "rintf32", 0),
    (RINTF64, "rintf64", 0),
    (RINTF32X, "rintf32x", 0),
    (RINTF64X, "rintf64x", 0),
    (RINTF128, "rintf128", 0),
    (NEARBYINTF32, "nearbyintf32", 0),
    (NEARBYINTF64, "nearbyintf64", 0),
    (NEARBYINTF32X, "nearbyintf32x", 0),
    (NEARBYINTF64X, "nearbyintf64x", 0),
    (NEARBYINTF128, "nearbyintf128", 0),
    (TRUNCF32, "truncf32", 0),
    (TRUNCF64, "truncf64", 0),
    (TRUNCF32X, "truncf32x", 0),
    (TRUNCF64X, "truncf64x", 0),
    (TRUNCF128, "truncf128", 0),
    (FLOORF32, "floorf32", 0),
    (FLOORF64, "floorf64", 0),
    (FLOORF32X, "floorf32x", 0),
    (FLOORF64X, "floorf64x", 0),
    (FLOORF128, "floorf128", 0),
    (CEILF32, "ceilf32", 0),
    (CEILF64, "ceilf64", 0),
    (CEILF32X, "ceilf32x", 0),
    (CEILF64X, "ceilf64x", 0),
    (CEILF128, "ceilf128", 0),
    (LOGBF32, "logbf32", 0),
    (LOGBF64, "logbf64", 0),
    (LOGBF32X, "logbf32x", 0),
    (LOGBF64X, "logbf64x", 0),
    (LOGBF128, "logbf128", 0),
    (SQRTF32, "sqrtf32", 0),
    (SQRTF64, "sqrtf64", 0),
    (SQRTF32X, "sqrtf32x", 0),
    (SQRTF64X, "sqrtf64x", 0),
    (CBRTF32, "cbrtf32", 0),
    (CBRTF64, "cbrtf64", 0),
    (CBRTF32X, "cbrtf32x", 0),
    (CBRTF64X, "cbrtf64x", 0),
    (CBRTF128, "cbrtf128", 0),
    (FMINF32, "fminf32", 0),
    (FMINF64, "fminf64", 0),
    (FMINF32X, "fminf32x", 0),
    (FMINF64X, "fminf64x", 0),
    (FMINF128, "fminf128", 0),
    (FMAXF32, "fmaxf32", 0),
    (FMAXF64, "fmaxf64", 0),
    (FMAXF32X, "fmaxf32x", 0),
    (FMAXF64X, "fmaxf64x", 0),
    (FMAXF128, "fmaxf128", 0),
    (FMAF32, "fmaf32", 0),
    (FMAF64, "fmaf64", 0),
    (FMAF32X, "fmaf32x", 0),
    (FMAF64X, "fmaf64x", 0),
    (ILOGBF32, "ilogbf32", 0),
    (ILOGBF64, "ilogbf64", 0),
    (ILOGBF32X, "ilogbf32x", 0),
    (ILOGBF64X, "ilogbf64x", 0),
    (ILOGBF128, "ilogbf128", 0),
    (LDEXPF32, "ldexpf32", 0),
    (LDEXPF64, "ldexpf64", 0),
    (LDEXPF32X, "ldexpf32x", 0),
    (LDEXPF64X, "ldexpf64x", 0),
    (LDEXPF128, "ldexpf128", 0),
    (FREXPF32, "frexpf32", 0),
    (FREXPF64, "frexpf64", 0),
    (FREXPF32X, "frexpf32x", 0),
    (FREXPF64X, "frexpf64x", 0),
    (FREXPF128, "frexpf128", 0),
    (MODFF32, "modff32", 0),
    (MODFF64, "modff64", 0),
    (MODFF32X, "modff32x", 0),
    (MODFF64X, "modff64x", 0),
    (MODFF128, "modff128", 0),
    (NEXTAFTERF32, "nextafterf32", 0),
    (NEXTAFTERF64, "nextafterf64", 0),
    (NEXTAFTERF32X, "nextafterf32x", 0),
    (NEXTAFTERF64X, "nextafterf64x", 0),
    (NEXTAFTERF128, "nextafterf128", 0),
    (FDIMF32, "fdimf32", 0),
    (FDIMF64, "fdimf64", 0),
    (FDIMF32X, "fdimf32x", 0),
    (FDIMF64X, "fdimf64x", 0),
    (FDIMF128, "fdimf128", 0),
    (HYPOTF32, "hypotf32", 0),
    (HYPOTF64, "hypotf64", 0),
    (HYPOTF32X, "hypotf32x", 0),
    (HYPOTF64X, "hypotf64x", 0),
    (HYPOTF128, "hypotf128", 0),
    (COPYSIGN,          "copysign",          0),
    (COPYSIGNF,         "copysignf",         0),
    (COPYSIGNL,         "copysignl",         0),
    // The square roots, one instruction on both targets, with a call kept
    // for a negative argument so that `errno` is set.
    (SQRT,              "sqrt",              0),
    (SQRTF,             "sqrtf",             0),
    (SQRTL,             "sqrtl",             0),
    // Minimum, maximum and fused multiply-add: instructions on aarch64,
    // calls on the x86-64 baseline, and folded for constants on both.
    (FMIN,              "fmin",              0),
    (FMINF,             "fminf",             0),
    (FMAX,              "fmax",              0),
    (FMAXF,             "fmaxf",             0),
    (FMA,               "fma",               0),
    (FMAF,              "fmaf",              0),

    // The integer magnitudes, recognized by their plain names for the same
    // reason and displaceable the same way. Each becomes a branch-free
    // expression rather than a call, as it does under gcc at every level.
    (ABS,               "abs",               0),
    (LABS,              "labs",              0),
    (LLABS,             "llabs",             0),
    (IMAXABS,           "imaxabs",           0),

    // The complex accessors, from <complex.h>: parts and conjugate, each
    // exact at every precision, so recognized and computed in place.
    (CREAL,             "creal",             0),
    (CREALF,            "crealf",            0),
    (CREALL,            "creall",            0),
    (CIMAG,             "cimag",             0),
    (CIMAGF,            "cimagf",            0),
    (CIMAGL,            "cimagl",            0),
    (CONJ,              "conj",              0),
    (CONJF,             "conjf",             0),
    (CONJL,             "conjl",             0),

    // The block memory functions of <string.h>, recognized by their plain
    // names for the same reason and displaceable the same way. A constant
    // length small enough is expanded into loads and stores, as under gcc.
    (MEMCPY,            "memcpy",            0),
    (MEMSET,            "memset",            0),
    (MEMMOVE,           "memmove",           0),

    // The roundings, computed in place, and exactly rounding: the `float`
    // form gives the same answer as the `double` one applied to a `float`
    // argument, which is what lets `(float)floor((double)x)` become
    // `floorf(x)`; `sin` and `log` are deliberately absent, since theirs
    // differ in the last bit.
    (FLOOR,             "floor",             0),
    (CEIL,              "ceil",              0),
    (TRUNC,             "trunc",             0),
    (ROUND,             "round",             0),
    (RINT,              "rint",              0),
    (NEARBYINT,         "nearbyint",         0),
    (FLOORF,            "floorf",            0),
    (CEILF,             "ceilf",             0),
    (TRUNCF,            "truncf",            0),
    (ROUNDF,            "roundf",            0),
    (RINTF,             "rintf",             0),
    (NEARBYINTF,        "nearbyintf",        0),

    // ---- Fortified libc entry points ----
    // Interned but untagged: these are ordinary identifiers, listed only so
    // the parser can name one when it synthesizes the declaration glibc
    // never writes. `bits/string_fortified.h` calls
    // `__builtin___memcpy_chk` and expects the compiler to know
    // `__memcpy_chk` intrinsically.
    (MEMCPY_CHK,        "__memcpy_chk",         0),
    (MEMMOVE_CHK,       "__memmove_chk",        0),
    (MEMPCPY_CHK,       "__mempcpy_chk",        0),
    (MEMSET_CHK,        "__memset_chk",         0),
    (STRCPY_CHK,        "__strcpy_chk",         0),
    (STPCPY_CHK,        "__stpcpy_chk",         0),
    (STRNCPY_CHK,       "__strncpy_chk",        0),
    (STPNCPY_CHK,       "__stpncpy_chk",        0),
    (STRCAT_CHK,        "__strcat_chk",         0),
    (STRNCAT_CHK,       "__strncat_chk",        0),
    (SPRINTF_CHK,       "__sprintf_chk",        0),
    (SNPRINTF_CHK,      "__snprintf_chk",       0),
    (PRINTF_CHK,        "__printf_chk",         0),
    (FPRINTF_CHK,       "__fprintf_chk",        0),
    (VSPRINTF_CHK,      "__vsprintf_chk",       0),
    (VSNPRINTF_CHK,     "__vsnprintf_chk",      0),
    (VPRINTF_CHK,       "__vprintf_chk",        0),
    (VFPRINTF_CHK,      "__vfprintf_chk",       0),
    // Darwin's own: its `<secure/_string.h>` calls them through the
    // `__builtin___` spelling, and its C library has them.
    (MEMCCPY_CHK,       "__memccpy_chk",        0),
    (STRLCPY_CHK,       "__strlcpy_chk",        0),
    (STRLCAT_CHK,       "__strlcat_chk",        0),
    // Same reason, for the builtins that are just the library function: a
    // translation unit may call `__builtin_strlen` without having included
    // the header that declares `strlen`, exactly as gcc allows.
    (STRLEN,            "strlen",               0),
    (STRCMP,            "strcmp",               0),
    (_,                 "ffs",                  0),
    (_,                 "ffsl",                 0),
    (_,                 "ffsll",                0),
    (FMAXL,             "fmaxl",                0),
    (FMINL,             "fminl",                0),
    (POW,               "pow",                  0),
    (POWF,              "powf",                 0),
    (POWL,              "powl",                 0),
    (FMAL,              "fmal",                 0),
    (BCMP,              "bcmp",                 0),
    (BZERO,             "bzero",                0),
    (STPNCPY,           "stpncpy",              0),
    (STRDUP,            "strdup",               0),
    (NAN,               "nan",                  0),
    (NANF,              "nanf",                 0),
    (NANL,              "nanl",                 0),
    (NANF16,            "nanf16",                0),
    (NANF32,            "nanf32",                0),
    (NANF64,            "nanf64",                0),
    (NANF128,           "nanf128",               0),
    (NANF32X,           "nanf32x",               0),
    (NANF64X,           "nanf64x",               0),
    (STRNDUP,           "strndup",              0),
    (CBRT,              "cbrt",                   0),
    (CBRTF,             "cbrtf",                  0),
    (CBRTL,             "cbrtl",                  0),
    (CEILL,             "ceill",                  0),
    (FLOORL,            "floorl",                 0),
    (TRUNCL,            "truncl",                 0),
    (ROUNDL,            "roundl",                 0),
    (RINTL,             "rintl",                  0),
    (NEARBYINTL,        "nearbyintl",             0),
    (SIN,               "sin",                    0),
    (SINF,              "sinf",                   0),
    (SINL,              "sinl",                   0),
    (COS,               "cos",                    0),
    (COSF,              "cosf",                   0),
    (COSL,              "cosl",                   0),
    (TAN,               "tan",                    0),
    (TANF,              "tanf",                   0),
    (TANL,              "tanl",                   0),
    (ASIN,              "asin",                   0),
    (ASINF,             "asinf",                  0),
    (ASINL,             "asinl",                  0),
    (ACOS,              "acos",                   0),
    (ACOSF,             "acosf",                  0),
    (ACOSL,             "acosl",                  0),
    (ATAN,              "atan",                   0),
    (ATANF,             "atanf",                  0),
    (ATANL,             "atanl",                  0),
    (SINH,              "sinh",                   0),
    (SINHF,             "sinhf",                  0),
    (SINHL,             "sinhl",                  0),
    (COSH,              "cosh",                   0),
    (COSHF,             "coshf",                  0),
    (COSHL,             "coshl",                  0),
    (TANH,              "tanh",                   0),
    (TANHF,             "tanhf",                  0),
    (TANHL,             "tanhl",                  0),
    (ASINH,             "asinh",                  0),
    (ASINHF,            "asinhf",                 0),
    (ASINHL,            "asinhl",                 0),
    (ACOSH,             "acosh",                  0),
    (ACOSHF,            "acoshf",                 0),
    (ACOSHL,            "acoshl",                 0),
    (ATANH,             "atanh",                  0),
    (ATANHF,            "atanhf",                 0),
    (ATANHL,            "atanhl",                 0),
    (EXP,               "exp",                    0),
    (EXPF,              "expf",                   0),
    (EXPL,              "expl",                   0),
    (EXP2,              "exp2",                   0),
    (EXP2F,             "exp2f",                  0),
    (EXP2L,             "exp2l",                  0),
    (EXPM1,             "expm1",                  0),
    (EXPM1F,            "expm1f",                 0),
    (EXPM1L,            "expm1l",                 0),
    (LOG,               "log",                    0),
    (LOGF,              "logf",                   0),
    (LOGL,              "logl",                   0),
    (LOG2,              "log2",                   0),
    (LOG2F,             "log2f",                  0),
    (LOG2L,             "log2l",                  0),
    (LOG10,             "log10",                  0),
    (LOG10F,            "log10f",                 0),
    (LOG10L,            "log10l",                 0),
    (LOG1P,             "log1p",                  0),
    (LOG1PF,            "log1pf",                 0),
    (LOG1PL,            "log1pl",                 0),
    (LOGB,              "logb",                   0),
    (ILOGB,             "ilogb",                  0),
    (ILOGBF,            "ilogbf",                 0),
    (ILOGBL,            "ilogbl",                 0),
    (LOGBF,             "logbf",                  0),
    (LOGBL,             "logbl",                  0),
    (TGAMMA,            "tgamma",                 0),
    (TGAMMAF,           "tgammaf",                0),
    (TGAMMAL,           "tgammal",                0),
    (LGAMMA,            "lgamma",                 0),
    (LGAMMAF,           "lgammaf",                0),
    (LGAMMAL,           "lgammal",                0),
    (ERF,               "erf",                    0),
    (ERFF,              "erff",                   0),
    (ERFL,              "erfl",                   0),
    (ERFC,              "erfc",                   0),
    (ERFCF,             "erfcf",                  0),
    (ERFCL,             "erfcl",                  0),
    (FMOD,              "fmod",                   0),
    (FMODF,             "fmodf",                  0),
    (FMODL,             "fmodl",                  0),
    (ATAN2,             "atan2",                  0),
    (ATAN2F,            "atan2f",                 0),
    (ATAN2L,            "atan2l",                 0),
    (HYPOT,             "hypot",                  0),
    (HYPOTF,            "hypotf",                 0),
    (HYPOTL,            "hypotl",                 0),
    (FDIM,              "fdim",                   0),
    (FDIMF,             "fdimf",                  0),
    (FDIML,             "fdiml",                  0),
    (REMAINDER,         "remainder",              0),
    (REMAINDERF,        "remainderf",             0),
    (REMAINDERL,        "remainderl",             0),
    (NEXTAFTER,         "nextafter",              0),
    (NEXTAFTERF,        "nextafterf",             0),
    (NEXTAFTERL,        "nextafterl",             0),
    (MODF,              "modf",                   0),
    (MODFF,             "modff",                  0),
    (MODFL,             "modfl",                  0),
    (FREXP,             "frexp",                  0),
    (FREXPF,            "frexpf",                 0),
    (FREXPL,            "frexpl",                 0),
    (LDEXP,             "ldexp",                  0),
    (LDEXPF,            "ldexpf",                 0),
    (LDEXPL,            "ldexpl",                 0),
    (STRCASECMP,        "strcasecmp",             0),
    (STRNCASECMP,       "strncasecmp",            0),
    (_,                 "__clear_cache",        0),
    (ABORT,             "abort",                0),
    (EXIT,              "exit",                 0),
    (PRINTF,            "printf",               0),
    (SPRINTF,           "sprintf",              0),
    (SNPRINTF,          "snprintf",             0),
    (PUTS,              "puts",                 0),
    // `malloc` is not listed here: it is already interned below as the
    // `__attribute__((malloc))` name, and one spelling is one entry.
    (CALLOC,            "calloc",               0),
    (REALLOC,           "realloc",              0),
    (FREE,              "free",                 0),
    (MEMCMP,            "memcmp",               0),
    (MEMPCPY,           "mempcpy",              0),
    (STRCPY,            "strcpy",               0),
    (STRNCPY,           "strncpy",              0),
    (STPCPY,            "stpcpy",               0),
    (STRCAT,            "strcat",               0),
    (STRNCAT,           "strncat",              0),
    (STRNCMP,           "strncmp",              0),
    (STRCHR,            "strchr",               0),
    (STRRCHR,           "strrchr",              0),
    (STRSTR,            "strstr",               0),
    (MEMCHR,            "memchr",               0),
    (BCOPY,             "bcopy",                 0),
    (INDEX,             "index",                0),
    (RINDEX,            "rindex",               0),
    (PUTCHAR,           "putchar",              0),
    (STRCSPN,           "strcspn",              0),
    (STRSPN,            "strspn",                0),
    (STRPBRK,           "strpbrk",              0),
    (PRINTF_UNLOCKED,   "printf_unlocked",      0),
    (FPRINTF_UNLOCKED,  "fprintf_unlocked",     0),
    (FPUTS_UNLOCKED,    "fputs_unlocked",       0),
    (FPRINTF,           "fprintf",              0),
    (FPUTS,             "fputs",                0),
    (FPUTC,             "fputc",                0),
    // Known to the optimizer by prototype only, with no `__builtin_` form
    // of their own that c17 parses.
    (STRNLEN,           "strnlen",              0),
    (VPRINTF,           "vprintf",              0),
    (VFPRINTF,          "vfprintf",             0),
    (FWRITE,            "fwrite",               0),

    // ---- Supported attribute names (SUPPORTED_ATTR) ----
    // Plain forms
    (_, "noreturn",             SUPPORTED_ATTR),
    (_, "unused",               SUPPORTED_ATTR),
    (_, "aligned",              SUPPORTED_ATTR),
    (_, "packed",               SUPPORTED_ATTR),
    (_, "deprecated",           SUPPORTED_ATTR),
    (_, "weak",                 SUPPORTED_ATTR),
    (_, "section",              SUPPORTED_ATTR),
    (_, "visibility",           SUPPORTED_ATTR),
    (_, "alias",                SUPPORTED_ATTR),
    (_, "constructor",          SUPPORTED_ATTR),
    (_, "destructor",           SUPPORTED_ATTR),
    (_, "used",                 SUPPORTED_ATTR),
    (_, "noinline",             SUPPORTED_ATTR),
    (_, "always_inline",        SUPPORTED_ATTR),
    (_, "hot",                  SUPPORTED_ATTR),
    (_, "cold",                 SUPPORTED_ATTR),
    (_, "warn_unused_result",   SUPPORTED_ATTR),
    (_, "format",               SUPPORTED_ATTR),
    (_, "fallthrough",          SUPPORTED_ATTR),
    (_, "nonstring",            SUPPORTED_ATTR),
    (MALLOC, "malloc",               SUPPORTED_ATTR),
    (_, "pure",                 SUPPORTED_ATTR),
    (_, "sentinel",             SUPPORTED_ATTR),
    (_, "no_sanitize_memory",   SUPPORTED_ATTR),
    (_, "no_sanitize_address",  SUPPORTED_ATTR),
    (_, "no_sanitize_thread",   SUPPORTED_ATTR),
    (_, "gnu_inline",              SUPPORTED_ATTR),
    (_, "artificial",              SUPPORTED_ATTR),
    (_, "sysv_abi",                SUPPORTED_ATTR | X86_64_ONLY),
    (_, "ms_abi",                  SUPPORTED_ATTR | X86_64_ONLY),
    (_, "nocf_check",              SUPPORTED_ATTR),
    (_, "mode",                    SUPPORTED_ATTR),
    (_, "vector_size",             SUPPORTED_ATTR),
    (_, "scalar_storage_order",    SUPPORTED_ATTR),
    // Parsed and ignored. Recognised so that a build does not drown in
    // warnings for the attributes glibc's headers put on everything; each
    // is semantically free, or free enough that ignoring it cannot change
    // what a correct program computes.
    (_, "nonnull",                 SUPPORTED_ATTR),
    (_, "returns_nonnull",         SUPPORTED_ATTR),
    (_, "nothrow",                 SUPPORTED_ATTR),
    (_, "access",                  SUPPORTED_ATTR),
    (_, "returns_twice",           SUPPORTED_ATTR),
    (_, "externally_visible",      SUPPORTED_ATTR),
    (_, "abi_tag",                 SUPPORTED_ATTR),
    (_, "weakref",                 SUPPORTED_ATTR),
    (_, "transparent_union",       SUPPORTED_ATTR),
    (_, "simd",                    SUPPORTED_ATTR),
    (_, "regparm",                 SUPPORTED_ATTR),
    (_, "leaf",                    SUPPORTED_ATTR),
    (_, "alloc_size",              SUPPORTED_ATTR),
    (_, "alloc_align",             SUPPORTED_ATTR),
    (_, "assume_aligned",          SUPPORTED_ATTR),
    (_, "noclone",                 SUPPORTED_ATTR),
    (_, "no_instrument_function",  SUPPORTED_ATTR),
    (_, "nonnull_if_nonzero",      SUPPORTED_ATTR),
    (_, "copy",                    SUPPORTED_ATTR),
    (_, "designated_init",         SUPPORTED_ATTR),
    (_, "may_alias",               SUPPORTED_ATTR),
    // How gcc optimizes one function, which ignoring cannot make wrong --
    // but for a stack-protector option, which sets the function's level
    // (`AttributeList::optimize_stack_protector`).
    (_, "optimize",                SUPPORTED_ATTR),
    // GNU forms (__foo__)
    // Note: __noreturn__ is already defined above with NORETURN_KW | SUPPORTED_ATTR
    (_, "__unused__",           SUPPORTED_ATTR),
    (_, "__aligned__",          SUPPORTED_ATTR),
    (_, "__packed__",           SUPPORTED_ATTR),
    (_, "__deprecated__",       SUPPORTED_ATTR),
    (_, "__weak__",             SUPPORTED_ATTR),
    (_, "__section__",          SUPPORTED_ATTR),
    (_, "__visibility__",       SUPPORTED_ATTR),
    (_, "__alias__",            SUPPORTED_ATTR),
    (_, "__constructor__",      SUPPORTED_ATTR),
    (_, "__destructor__",       SUPPORTED_ATTR),
    (_, "__used__",             SUPPORTED_ATTR),
    (_, "__noinline__",         SUPPORTED_ATTR),
    (_, "__always_inline__",    SUPPORTED_ATTR),
    (_, "__hot__",              SUPPORTED_ATTR),
    (_, "__cold__",             SUPPORTED_ATTR),
    (_, "__warn_unused_result__", SUPPORTED_ATTR),
    (_, "__format__",           SUPPORTED_ATTR),
    (_, "__fallthrough__",      SUPPORTED_ATTR),
    (_, "__nonstring__",        SUPPORTED_ATTR),
    (_, "__malloc__",           SUPPORTED_ATTR),
    (_, "__pure__",             SUPPORTED_ATTR),
    (_, "__sentinel__",         SUPPORTED_ATTR),
    (_, "__no_sanitize_memory__",    SUPPORTED_ATTR),
    (_, "__no_sanitize_address__",   SUPPORTED_ATTR),
    (_, "__no_sanitize_thread__",    SUPPORTED_ATTR),
    (_, "__gnu_inline__",            SUPPORTED_ATTR),
    (_, "__artificial__",            SUPPORTED_ATTR),
    (_, "__sysv_abi__",              SUPPORTED_ATTR | X86_64_ONLY),
    (_, "__ms_abi__",                SUPPORTED_ATTR | X86_64_ONLY),
    (_, "__nocf_check__",            SUPPORTED_ATTR),
    (_, "__mode__",                  SUPPORTED_ATTR),
    (_, "__vector_size__",           SUPPORTED_ATTR),
    (_, "__scalar_storage_order__",  SUPPORTED_ATTR),
    (_, "__nonnull__",               SUPPORTED_ATTR),
    (_, "__returns_nonnull__",       SUPPORTED_ATTR),
    (_, "__nothrow__",               SUPPORTED_ATTR),
    (_, "__access__",                SUPPORTED_ATTR),
    (_, "__returns_twice__",         SUPPORTED_ATTR),
    (_, "__externally_visible__",    SUPPORTED_ATTR),
    (_, "__abi_tag__",               SUPPORTED_ATTR),
    (_, "__weakref__",               SUPPORTED_ATTR),
    (_, "__transparent_union__",     SUPPORTED_ATTR),
    (_, "__simd__",                  SUPPORTED_ATTR),
    (_, "__regparm__",               SUPPORTED_ATTR),
    (_, "__leaf__",                  SUPPORTED_ATTR),
    (_, "__alloc_size__",            SUPPORTED_ATTR),
    (_, "__alloc_align__",           SUPPORTED_ATTR),
    (_, "__assume_aligned__",        SUPPORTED_ATTR),
    (_, "__noclone__",               SUPPORTED_ATTR),
    (_, "__no_instrument_function__", SUPPORTED_ATTR),
    (_, "__nonnull_if_nonzero__",    SUPPORTED_ATTR),
    (_, "__copy__",                  SUPPORTED_ATTR),
    (_, "__designated_init__",       SUPPORTED_ATTR),
    (_, "__may_alias__",             SUPPORTED_ATTR),
    (_, "__optimize__",              SUPPORTED_ATTR),
    // GCC's complex-part operators. Appended, because `define_ids!` numbers
    // entries by table position. Both spellings of each, as gcc accepts.
    (REAL_KW,           "__real__",          RESERVED_NAME),
    (REAL_KW_SHORT,     "__real",            RESERVED_NAME),
    (IMAG_KW,           "__imag__",          RESERVED_NAME),
    (IMAG_KW_SHORT,     "__imag",            RESERVED_NAME),
    // `cleanup(fn)`: runs `fn(&var)` when an automatic variable leaves its
    // scope. Appended, as above.
    (_, "cleanup",                   SUPPORTED_ATTR),
    (_, "__cleanup__",               SUPPORTED_ATTR),
    // `ifunc("resolver")`: a GNU indirect function. Appended, as above.
    (_, "ifunc",                     SUPPORTED_ATTR),
    (_, "__ifunc__",                 SUPPORTED_ATTR),
    // `target("...")` and `target_clones(...)`: one function compiled for
    // another ISA, or for several with a resolver. Appended, as above.
    (_, "target",                    SUPPORTED_ATTR),
    (_, "__target__",                SUPPORTED_ATTR),
    (_, "target_clones",             SUPPORTED_ATTR),
    (_, "__target_clones__",         SUPPORTED_ATTR),
    // `__auto_type`: a declaration whose type is its initializer's. A type
    // specifier, and reserved like every `__` spelling. Appended, as above.
    (GNU_AUTO_TYPE,     "__auto_type",       TYPE_SPEC | TYPE_KEYWORD | RESERVED_NAME),
    // `__label__`: declares labels local to a block, at its head. A
    // statement keyword, so it is never itself a label. Appended, as above.
    (GNU_LABEL,         "__label__",         STMT_KW | RESERVED_NAME),
    // Builtins gcc makes for its own lowering, which code may also call.
    // Appended, as above. `cexp` and `cpow` are what the complex ones call.
    (BUILTIN_STACK_SAVE,    "__builtin_stack_save",    BUILTIN),
    (BUILTIN_STACK_RESTORE, "__builtin_stack_restore", BUILTIN),
    (BUILTIN_CLEAR_PADDING, "__builtin_clear_padding", BUILTIN),
    (BUILTIN_CEXPI,     "__builtin_cexpi",   BUILTIN),
    (BUILTIN_CEXPIF,    "__builtin_cexpif",  BUILTIN),
    (BUILTIN_CEXPIL,    "__builtin_cexpil",  BUILTIN),
    (BUILTIN_CPOW,      "__builtin_cpow",    BUILTIN),
    (BUILTIN_CPOWF,     "__builtin_cpowf",   BUILTIN),
    (BUILTIN_CPOWL,     "__builtin_cpowl",   BUILTIN),
    (CEXP,              "cexp",              0),
    (CEXPF,             "cexpf",             0),
    (CEXPL,             "cexpl",             0),
    (CPOW,              "cpow",              0),
    (CPOWF,             "cpowf",             0),
    (CPOWL,             "cpowl",             0),
    // `stack_protect` and `no_stack_protector`: a function's own say over
    // `-fstack-protector`. Appended, as above.
    (_, "stack_protect",             SUPPORTED_ATTR),
    (_, "__stack_protect__",         SUPPORTED_ATTR),
    (_, "no_stack_protector",        SUPPORTED_ATTR),
    (_, "__no_stack_protector__",    SUPPORTED_ATTR),
    // gcc's spellings of `signed`, keywords as in gcc rather than macros, so
    // `#ifdef __signed__` is false and `-E` keeps them. Appended, as above.
    (GNU_SIGNED,        "__signed__",        TYPE_SPEC | TYPE_KEYWORD),
    (GNU_SIGNED2,       "__signed",          TYPE_SPEC | TYPE_KEYWORD),
    // `__extension__`: marks a GNU construct. Taken before a declaration,
    // a block item or a member declaration, and as a unary prefix; it means
    // nothing else. Appended, as above.
    (GNU_EXTENSION,     "__extension__",     RESERVED_NAME),
    // `uninitialized`: an automatic variable `-ftrivial-auto-var-init`
    // leaves alone. Appended, as above.
    (_, "uninitialized",             SUPPORTED_ATTR),
    (_, "__uninitialized__",         SUPPORTED_ATTR),
    // `indirect_branch` and `function_return`: one function's own
    // `-mindirect-branch=` and `-mfunction-return=`. x86-64's alone.
    // Appended, as above.
    (_, "indirect_branch",           SUPPORTED_ATTR | X86_64_ONLY),
    (_, "__indirect_branch__",       SUPPORTED_ATTR | X86_64_ONLY),
    (_, "function_return",           SUPPORTED_ATTR | X86_64_ONLY),
    (_, "__function_return__",       SUPPORTED_ATTR | X86_64_ONLY),
}

// Tag query API

/// Check if a StringId has any of the given tag bits set.
/// Returns false for non-keyword IDs (dynamic strings interned after keywords).
pub fn has_tag(id: StringId, mask: u32) -> bool {
    let idx = id.0 as usize;
    idx > 0 && idx <= KEYWORD_COUNT && KEYWORD_TAGS[idx - 1] & mask != 0
}

/// Does keyword `id` exist on a target of architecture `arch`?
///
/// Every keyword does, except an [`X86_64_ONLY`] one off x86-64: there it is
/// an ordinary identifier, so an attribute of that name warns as unknown and
/// `__has_attribute` answers 0, exactly as gcc for aarch64 does.
pub fn exists_on(id: StringId, arch: crate::target::Arch) -> bool {
    arch == crate::target::Arch::X86_64 || !has_tag(id, X86_64_ONLY)
}

/// [`exists_on`] for a spelling rather than an interned id: `__has_builtin`
/// is asked about a name.
pub fn spelling_exists_on(name: &str, arch: crate::target::Arch) -> bool {
    arch == crate::target::Arch::X86_64
        || !KEYWORD_STRINGS
            .iter()
            .zip(KEYWORD_TAGS.iter())
            .any(|(s, &t)| *s == name && t & X86_64_ONLY != 0)
}

/// Is `id` an attribute c17 supports on a target of architecture `arch`?
/// The one answer the parser's "directive ignored" warning and both
/// `__has_attribute` evaluators give.
pub fn attribute_supported(id: StringId, arch: crate::target::Arch) -> bool {
    has_tag(id, SUPPORTED_ATTR) && exists_on(id, arch)
}

/// Every spelling in the table carrying any of `mask`.
///
/// The inverse of `has_tag`, for the checks that have to walk the table rather
/// than ask about one name -- proving a roster elsewhere in the crate lists
/// exactly what the table tags, in both directions. Only the registry checks
/// need it, so it is not compiled into the compiler.
#[cfg(test)]
pub fn tagged_spellings(mask: u32) -> Vec<&'static str> {
    (0..KEYWORD_COUNT)
        .filter(|&i| KEYWORD_TAGS[i] & mask != 0)
        .map(|i| KEYWORD_STRINGS[i])
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::strings::StringTable;
    use std::collections::HashSet;

    /// Look up a pre-interned keyword by string, panicking if not found.
    fn id(table: &StringTable, s: &str) -> StringId {
        table
            .lookup(s)
            .unwrap_or_else(|| panic!("keyword '{}' not interned", s))
    }

    #[test]
    fn test_keyword_ids_deterministic() {
        let table = StringTable::new();
        assert_eq!(table.get(VOID), "void");
        assert_eq!(table.get(CHAR), "char");
        assert_eq!(table.get(INT), "int");
        assert_eq!(table.get(STATIC), "static");
        assert_eq!(table.get(CONST), "const");
        assert_eq!(table.get(INLINE), "inline");
        assert_eq!(table.get(IF), "if");
        assert_eq!(table.get(RETURN), "return");
        assert_eq!(table.get(BUILTIN_VA_START), "__builtin_va_start");
        assert_eq!(table.get(C11_ATOMIC_LOAD), "__c11_atomic_load");
        // Anonymous entries verified via lookup
        assert!(table.lookup("noreturn").is_some());
        assert!(table.lookup("__packed__").is_some());
    }

    /// Every id names the string it was defined from, for the whole table.
    ///
    /// The ids used to be counted by a macro that recursed once per entry;
    /// they are looked up by spelling now, and this is the invariant that
    /// swap has to preserve. `id_of` panics at compile time for a spelling
    /// that is absent, and `test_no_duplicate_strings` rules out a spelling
    /// that appears twice -- so the remaining way to get this wrong is for an
    /// id to be off by one against the interned table, which this catches for
    /// every entry rather than the dozen `test_keyword_ids_deterministic`
    /// samples.
    #[test]
    fn test_every_id_matches_its_string() {
        let table = StringTable::new();
        for (i, &s) in KEYWORD_STRINGS.iter().enumerate() {
            let id = id_of(s);
            assert_eq!(
                id,
                StringId(i as u32 + 1),
                "'{s}' is at index {i} but id_of answered {id:?}"
            );
            assert_eq!(table.get(id), s, "interned table disagrees for '{s}'");
        }
        assert_eq!(KEYWORD_STRINGS.len(), KEYWORD_COUNT);
        assert_eq!(KEYWORD_TAGS.len(), KEYWORD_COUNT);
    }

    #[test]
    fn test_no_duplicate_strings() {
        let mut seen = HashSet::new();
        for (i, &s) in KEYWORD_STRINGS.iter().enumerate() {
            assert!(
                seen.insert(s),
                "duplicate keyword string '{}' at index {}",
                s,
                i
            );
        }
    }

    #[test]
    fn test_tags_type_spec() {
        for &s in &[
            "void",
            "char",
            "short",
            "int",
            "long",
            "float",
            "double",
            "signed",
            "unsigned",
            "_Bool",
            "_Complex",
            "_Float16",
            "_Float32",
            "_Float64",
            "_Float32x",
            "_Float64x",
            "__float128",
            "_Float128",
            "__int128",
            "__int128_t",
            "__uint128_t",
            "__builtin_va_list",
            "struct",
            "union",
            "enum",
            "typeof",
            "__typeof__",
            "__typeof",
            "__auto_type",
            "_Atomic",
        ] {
            let table = StringTable::new();
            let sid = id(&table, s);
            assert!(has_tag(sid, TYPE_SPEC), "'{}' should have TYPE_SPEC", s);
        }
    }

    #[test]
    fn test_tags_qualifier() {
        for &s in &[
            "const",
            "volatile",
            "restrict",
            "_Atomic",
            "__const__",
            "__const",
            "__volatile__",
            "__volatile",
            "__restrict__",
            "__restrict",
        ] {
            let table = StringTable::new();
            let sid = id(&table, s);
            assert!(has_tag(sid, QUALIFIER), "'{}' should have QUALIFIER", s);
        }
    }

    #[test]
    fn test_tags_type_keyword() {
        for &s in &[
            "void",
            "_Bool",
            "_Complex",
            "_Atomic",
            "char",
            "short",
            "int",
            "long",
            "float",
            "double",
            "_Float16",
            "_Float32",
            "_Float64",
            "_Float32x",
            "_Float64x",
            "__float128",
            "_Float128",
            "signed",
            "unsigned",
            "const",
            "volatile",
            "struct",
            "union",
            "enum",
            "__int128",
            "__int128_t",
            "__uint128_t",
            "__builtin_va_list",
            "typeof",
            "__typeof__",
            "__typeof",
            "__auto_type",
        ] {
            let table = StringTable::new();
            let sid = id(&table, s);
            assert!(
                has_tag(sid, TYPE_KEYWORD),
                "'{}' should have TYPE_KEYWORD",
                s
            );
        }
    }

    #[test]
    fn test_tags_decl_start() {
        for &s in &[
            "void",
            "char",
            "short",
            "int",
            "long",
            "float",
            "double",
            "_Float16",
            "_Float32",
            "_Float64",
            "_Float32x",
            "_Float64x",
            "__float128",
            "_Float128",
            "_Complex",
            "_Atomic",
            "_Alignas",
            "signed",
            "unsigned",
            "const",
            "volatile",
            "static",
            "extern",
            "auto",
            "register",
            "typedef",
            "inline",
            "__inline",
            "__inline__",
            "_Noreturn",
            "__noreturn__",
            "struct",
            "union",
            "enum",
            "_Bool",
            "__attribute__",
            "__attribute",
            "__int128",
            "__int128_t",
            "__uint128_t",
            "__builtin_va_list",
            "typeof",
            "__typeof__",
            "__typeof",
            "__auto_type",
            "_Thread_local",
            "__thread",
            "_Static_assert",
            "static_assert",
            "_Imaginary",
        ] {
            let table = StringTable::new();
            let sid = id(&table, s);
            assert!(has_tag(sid, DECL_START), "'{}' should have DECL_START", s);
        }
    }

    #[test]
    fn test_tags_nullability() {
        for &s in &[
            "_Nonnull",
            "__nonnull",
            "_Nullable",
            "__nullable",
            "_Null_unspecified",
            "__null_unspecified",
        ] {
            let table = StringTable::new();
            let sid = id(&table, s);
            assert!(has_tag(sid, NULLABILITY), "'{}' should have NULLABILITY", s);
        }
    }

    #[test]
    fn test_tags_builtin() {
        // Spot-check some builtins (named constants)
        let builtins = [
            BUILTIN_VA_START,
            BUILTIN_VA_END,
            BUILTIN_VA_ARG,
            BUILTIN_VA_COPY,
            BUILTIN_BSWAP16,
            BUILTIN_MEMCPY,
            BUILTIN_UNREACHABLE,
            BUILTIN_EXPECT,
            BUILTIN_VA_LIST,
            OFFSETOF,
            BUILTIN_OBJECT_SIZE,
            C11_ATOMIC_LOAD,
            C11_ATOMIC_STORE,
            C11_ATOMIC_EXCHANGE,
        ];
        for &bid in &builtins {
            assert!(
                has_tag(bid, BUILTIN),
                "'{}' should have BUILTIN",
                KEYWORD_STRINGS[bid.0 as usize - 1]
            );
        }
        // Count total builtins
        let builtin_count = KEYWORD_TAGS.iter().filter(|&&t| t & BUILTIN != 0).count();
        assert!(
            builtin_count >= 68,
            "expected at least 68 builtins, got {}",
            builtin_count
        );
    }

    /// The calling-convention attributes and the Microsoft `va_list`
    /// builtins are gcc's on x86-64 only; elsewhere they are plain names.
    #[test]
    fn test_x86_64_only_names() {
        use crate::target::Arch;
        let table = StringTable::new();
        for attr in ["ms_abi", "__ms_abi__", "sysv_abi", "__sysv_abi__"] {
            let sid = id(&table, attr);
            assert!(attribute_supported(sid, Arch::X86_64), "{attr}");
            assert!(!attribute_supported(sid, Arch::Aarch64), "{attr}");
        }
        assert!(attribute_supported(id(&table, "noreturn"), Arch::Aarch64));
        for name in [
            "__builtin_ms_va_list",
            "__builtin_ms_va_start",
            "__builtin_ms_va_end",
            "__builtin_ms_va_copy",
        ] {
            assert!(exists_on(id(&table, name), Arch::X86_64), "{name}");
            assert!(!exists_on(id(&table, name), Arch::Aarch64), "{name}");
            assert!(!spelling_exists_on(name, Arch::Aarch64), "{name}");
        }
        assert!(spelling_exists_on("__builtin_va_start", Arch::Aarch64));
    }

    #[test]
    fn test_tags_supported_attr() {
        for &s in &[
            "noreturn",
            "__noreturn__",
            "unused",
            "__unused__",
            "aligned",
            "__aligned__",
            "packed",
            "__packed__",
            "always_inline",
            "__always_inline__",
            "mode",
            "__mode__",
            "vector_size",
            "__vector_size__",
            "cleanup",
            "__cleanup__",
            "ifunc",
            "__ifunc__",
            "target",
            "__target__",
            "target_clones",
            "__target_clones__",
            "optimize",
            "__optimize__",
            "uninitialized",
            "__uninitialized__",
        ] {
            let table = StringTable::new();
            let sid = id(&table, s);
            assert!(
                has_tag(sid, SUPPORTED_ATTR),
                "'{}' should have SUPPORTED_ATTR",
                s
            );
        }
    }

    #[test]
    fn test_has_tag_returns_false_for_dynamic() {
        assert!(!has_tag(StringId(9999), TYPE_SPEC));
        assert!(!has_tag(StringId(9999), BUILTIN));
        assert!(!has_tag(StringId(9999), DECL_START));
    }

    #[test]
    fn test_has_tag_returns_false_for_empty() {
        assert!(!has_tag(StringId::EMPTY, TYPE_SPEC));
        assert!(!has_tag(StringId::EMPTY, BUILTIN));
        assert!(!has_tag(StringId::EMPTY, DECL_START));
    }
}
