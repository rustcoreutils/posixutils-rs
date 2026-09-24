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
// parse_type_specifiers, parse_statement, is_builtin, etc.) by replacing them
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
    // provided (Annex G makes the types optional, not the keyword), so the
    // name is reserved here without a type behind it.
    (_,                 "_Imaginary",        RESERVED_NAME),
    (FLOAT16,           "_Float16",          TYPE_SPEC | TYPE_KEYWORD),
    (FLOAT32,           "_Float32",          TYPE_SPEC | TYPE_KEYWORD),
    (FLOAT64,           "_Float64",          TYPE_SPEC | TYPE_KEYWORD),
    (FLOAT128,          "__float128",        TYPE_SPEC | TYPE_KEYWORD),
    (FLOAT128_ALIAS,    "_Float128",         TYPE_SPEC | TYPE_KEYWORD),
    (INT128,            "__int128",          TYPE_SPEC | TYPE_KEYWORD),
    (INT128_T,          "__int128_t",        TYPE_SPEC | TYPE_KEYWORD),
    (UINT128_T,         "__uint128_t",       TYPE_SPEC | TYPE_KEYWORD),
    (BUILTIN_VA_LIST,   "__builtin_va_list", TYPE_SPEC | TYPE_KEYWORD | BUILTIN),
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
    (BUILTIN_CHOOSE_EXPR, "__builtin_choose_expr", BUILTIN),
    // Builtins that are the library function of the same name; see
    // `is_library_builtin`. Listed so `__has_builtin` answers for them.
    (BUILTIN_STRLEN,    "__builtin_strlen",   BUILTIN),
    (BUILTIN_STRCMP,    "__builtin_strcmp",   BUILTIN),
    (BUILTIN_ABS,       "__builtin_abs",      BUILTIN),
    (BUILTIN_LABS,      "__builtin_labs",     BUILTIN),
    (BUILTIN_LLABS,     "__builtin_llabs",    BUILTIN),
    (BUILTIN_FFS,       "__builtin_ffs",      BUILTIN),
    (BUILTIN_FFSL,      "__builtin_ffsl",     BUILTIN),
    (BUILTIN_FFSLL,     "__builtin_ffsll",    BUILTIN),
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
    (BUILTIN_OFFSETOF,  "__builtin_offsetof", BUILTIN),
    (OFFSETOF,          "offsetof",           BUILTIN),
    (BUILTIN_INF,       "__builtin_inf",      BUILTIN),
    (BUILTIN_INFF,      "__builtin_inff",     BUILTIN),
    (BUILTIN_INFL,      "__builtin_infl",     BUILTIN),
    (BUILTIN_HUGE_VAL,  "__builtin_huge_val", BUILTIN),
    (BUILTIN_HUGE_VALF, "__builtin_huge_valf", BUILTIN),
    (BUILTIN_HUGE_VALL, "__builtin_huge_vall", BUILTIN),
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
    (BUILTIN_SIGNBIT,   "__builtin_signbit",  BUILTIN),
    (BUILTIN_SIGNBITF,  "__builtin_signbitf", BUILTIN),
    (BUILTIN_SIGNBITL,  "__builtin_signbitl", BUILTIN),
    (BUILTIN_NAN,       "__builtin_nan",      BUILTIN),
    (BUILTIN_NANF,      "__builtin_nanf",     BUILTIN),
    (BUILTIN_NANL,      "__builtin_nanl",     BUILTIN),
    (BUILTIN_NANS,      "__builtin_nans",     BUILTIN),
    (BUILTIN_NANSF,     "__builtin_nansf",    BUILTIN),
    (BUILTIN_NANSL,     "__builtin_nansl",    BUILTIN),
    (BUILTIN_COMPLEX,   "__builtin_complex",  BUILTIN),
    (BUILTIN_EXPECT,    "__builtin_expect",   BUILTIN),
    (BUILTIN_ASSUME_ALIGNED, "__builtin_assume_aligned", BUILTIN),
    (BUILTIN_PREFETCH,  "__builtin_prefetch", BUILTIN),
    (BUILTIN_FLT_ROUNDS, "__builtin_flt_rounds", BUILTIN),
    (BUILTIN_FRAME_ADDRESS, "__builtin_frame_address", BUILTIN),
    (BUILTIN_RETURN_ADDRESS, "__builtin_return_address", BUILTIN),
    (BUILTIN_OBJECT_SIZE, "__builtin_object_size", BUILTIN),
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

    // ---- setjmp/longjmp (special-cased in parser, not true builtins) ----
    // gcc predefines bare `alloca` as well as `__builtin_alloca`, and code in
    // the wild calls it without including <alloca.h>. Tagged 0, not BUILTIN:
    // `__has_builtin` asks about the reserved spelling, and a user declaration
    // may still displace this one (see `builtin_is_shadowed`).
    (ALLOCA,            "alloca",            0),
    (SETJMP,            "setjmp",            0),
    (SETJMP2,           "_setjmp",           0),
    (LONGJMP,           "longjmp",           0),
    (LONGJMP2,          "_longjmp",          0),

    // ---- Plain spellings of libm entry points ----
    // gcc recognizes the standard names whether or not <math.h> was included,
    // which is what lets `fabs(x) < 0.0` fold to 0 in a program that only
    // declares `extern double fabs(double);`. Tagged 0 for the same reason
    // `alloca` is, and displaceable the same way (see `builtin_is_shadowed`).
    (FABS,              "fabs",              0),
    (FABSF,             "fabsf",             0),
    (FABSL,             "fabsl",             0),

    // The exactly-rounding functions, whose `float` form gives the same
    // answer as the `double` one applied to a `float` argument. Narrowing
    // them is what lets `(float)floor((double)x)` become `floorf(x)`;
    // `sin` and `log` are deliberately absent, since theirs differ in the
    // last bit. The `f` spellings are interned so the prototype can be
    // synthesized for a program that never declared one.
    (FLOOR,             "floor",             0),
    (CEIL,              "ceil",              0),
    (TRUNC,             "trunc",             0),
    (ROUND,             "round",             0),
    (RINT,              "rint",              0),
    (NEARBYINT,         "nearbyint",         0),
    (_,                 "floorf",            0),
    (_,                 "ceilf",             0),
    (_,                 "truncf",            0),
    (_,                 "roundf",            0),
    (_,                 "rintf",             0),
    (_,                 "nearbyintf",        0),

    // ---- Fortified libc entry points ----
    // Interned but untagged: these are ordinary identifiers, listed only so
    // the parser can name one when it synthesizes the declaration glibc
    // never writes. `bits/string_fortified.h` calls
    // `__builtin___memcpy_chk` and expects the compiler to know
    // `__memcpy_chk` intrinsically.
    (_,                 "__memcpy_chk",         0),
    (_,                 "__memmove_chk",        0),
    (_,                 "__mempcpy_chk",        0),
    (_,                 "__memset_chk",         0),
    (_,                 "__strcpy_chk",         0),
    (_,                 "__stpcpy_chk",         0),
    (_,                 "__strncpy_chk",        0),
    (_,                 "__stpncpy_chk",        0),
    (_,                 "__strcat_chk",         0),
    (_,                 "__strncat_chk",        0),
    (_,                 "__sprintf_chk",        0),
    (_,                 "__snprintf_chk",       0),
    (_,                 "__printf_chk",         0),
    (_,                 "__fprintf_chk",        0),
    (_,                 "__vsprintf_chk",       0),
    (_,                 "__vsnprintf_chk",      0),
    (_,                 "__vprintf_chk",        0),
    (_,                 "__vfprintf_chk",       0),
    // Same reason, for the builtins that are just the library function: a
    // translation unit may call `__builtin_strlen` without having included
    // the header that declares `strlen`, exactly as gcc allows.
    (_,                 "strlen",               0),
    (_,                 "strcmp",               0),
    (_,                 "abs",                  0),
    (_,                 "labs",                 0),
    (_,                 "llabs",                0),
    (_,                 "ffs",                  0),
    (_,                 "ffsl",                 0),
    (_,                 "ffsll",                0),
    (_,                 "sqrt",                 0),
    (_,                 "sqrtf",                0),
    (_,                 "sqrtl",                0),
    (_,                 "copysign",             0),
    (_,                 "copysignf",            0),
    (_,                 "copysignl",            0),
    (_,                 "fmax",                 0),
    (_,                 "fmaxf",                0),
    (_,                 "fmaxl",                0),
    (_,                 "fmin",                 0),
    (_,                 "fminf",                0),
    (_,                 "fminl",                0),
    (_,                 "pow",                  0),
    (_,                 "powf",                 0),
    (_,                 "powl",                 0),
    (_,                 "fma",                  0),
    (_,                 "fmaf",                 0),
    (_,                 "fmal",                 0),
    (_,                 "bcmp",                 0),
    (_,                 "bzero",                0),
    (_,                 "stpncpy",              0),
    (_,                 "strdup",               0),
    (_,                 "strndup",              0),
    (_,                 "cbrt",                   0),
    (_,                 "cbrtf",                  0),
    (_,                 "cbrtl",                  0),
    (_,                 "ceill",                  0),
    (_,                 "floorl",                 0),
    (_,                 "truncl",                 0),
    (_,                 "roundl",                 0),
    (_,                 "rintl",                  0),
    (_,                 "nearbyintl",             0),
    (_,                 "sin",                    0),
    (_,                 "sinf",                   0),
    (_,                 "sinl",                   0),
    (_,                 "cos",                    0),
    (_,                 "cosf",                   0),
    (_,                 "cosl",                   0),
    (_,                 "tan",                    0),
    (_,                 "tanf",                   0),
    (_,                 "tanl",                   0),
    (_,                 "asin",                   0),
    (_,                 "asinf",                  0),
    (_,                 "asinl",                  0),
    (_,                 "acos",                   0),
    (_,                 "acosf",                  0),
    (_,                 "acosl",                  0),
    (_,                 "atan",                   0),
    (_,                 "atanf",                  0),
    (_,                 "atanl",                  0),
    (_,                 "sinh",                   0),
    (_,                 "sinhf",                  0),
    (_,                 "sinhl",                  0),
    (_,                 "cosh",                   0),
    (_,                 "coshf",                  0),
    (_,                 "coshl",                  0),
    (_,                 "tanh",                   0),
    (_,                 "tanhf",                  0),
    (_,                 "tanhl",                  0),
    (_,                 "asinh",                  0),
    (_,                 "asinhf",                 0),
    (_,                 "asinhl",                 0),
    (_,                 "acosh",                  0),
    (_,                 "acoshf",                 0),
    (_,                 "acoshl",                 0),
    (_,                 "atanh",                  0),
    (_,                 "atanhf",                 0),
    (_,                 "atanhl",                 0),
    (_,                 "exp",                    0),
    (_,                 "expf",                   0),
    (_,                 "expl",                   0),
    (_,                 "exp2",                   0),
    (_,                 "exp2f",                  0),
    (_,                 "exp2l",                  0),
    (_,                 "expm1",                  0),
    (_,                 "expm1f",                 0),
    (_,                 "expm1l",                 0),
    (_,                 "log",                    0),
    (_,                 "logf",                   0),
    (_,                 "logl",                   0),
    (_,                 "log2",                   0),
    (_,                 "log2f",                  0),
    (_,                 "log2l",                  0),
    (_,                 "log10",                  0),
    (_,                 "log10f",                 0),
    (_,                 "log10l",                 0),
    (_,                 "log1p",                  0),
    (_,                 "log1pf",                 0),
    (_,                 "log1pl",                 0),
    (_,                 "logb",                   0),
    (_,                 "logbf",                  0),
    (_,                 "logbl",                  0),
    (_,                 "tgamma",                 0),
    (_,                 "tgammaf",                0),
    (_,                 "tgammal",                0),
    (_,                 "lgamma",                 0),
    (_,                 "lgammaf",                0),
    (_,                 "lgammal",                0),
    (_,                 "erf",                    0),
    (_,                 "erff",                   0),
    (_,                 "erfl",                   0),
    (_,                 "erfc",                   0),
    (_,                 "erfcf",                  0),
    (_,                 "erfcl",                  0),
    (_,                 "fmod",                   0),
    (_,                 "fmodf",                  0),
    (_,                 "fmodl",                  0),
    (_,                 "atan2",                  0),
    (_,                 "atan2f",                 0),
    (_,                 "atan2l",                 0),
    (_,                 "hypot",                  0),
    (_,                 "hypotf",                 0),
    (_,                 "hypotl",                 0),
    (_,                 "fdim",                   0),
    (_,                 "fdimf",                  0),
    (_,                 "fdiml",                  0),
    (_,                 "remainder",              0),
    (_,                 "remainderf",             0),
    (_,                 "remainderl",             0),
    (_,                 "nextafter",              0),
    (_,                 "nextafterf",             0),
    (_,                 "nextafterl",             0),
    (_,                 "modf",                   0),
    (_,                 "modff",                  0),
    (_,                 "modfl",                  0),
    (_,                 "frexp",                  0),
    (_,                 "frexpf",                 0),
    (_,                 "frexpl",                 0),
    (_,                 "ldexp",                  0),
    (_,                 "ldexpf",                 0),
    (_,                 "ldexpl",                 0),
    (_,                 "strcasecmp",             0),
    (_,                 "strncasecmp",            0),
    (_,                 "__clear_cache",        0),
    (_,                 "abort",                0),
    (_,                 "exit",                 0),
    (_,                 "printf",               0),
    (_,                 "sprintf",              0),
    (_,                 "snprintf",             0),
    (_,                 "puts",                 0),
    // `malloc` is not listed here: it is already interned below as the
    // `__attribute__((malloc))` name, and one spelling is one entry.
    (_,                 "calloc",               0),
    (_,                 "realloc",              0),
    (_,                 "free",                 0),
    (_,                 "memcmp",               0),
    (_,                 "mempcpy",              0),
    (_,                 "strcpy",               0),
    (_,                 "strncpy",              0),
    (_,                 "stpcpy",               0),
    (_,                 "strcat",               0),
    (_,                 "strncat",              0),
    (_,                 "strncmp",              0),
    (_,                 "strchr",               0),
    (_,                 "strrchr",              0),
    (_,                 "strstr",               0),
    (_,                 "imaxabs",               0),
    (_,                 "memchr",                0),
    (_,                 "bcopy",                 0),
    (_,                 "index",                 0),
    (_,                 "rindex",                0),
    (_,                 "putchar",               0),
    (_,                 "strcspn",               0),
    (_,                 "strspn",                0),
    (_,                 "strpbrk",               0),
    (_,                 "printf_unlocked",  0),
    (_,                 "fprintf_unlocked",  0),
    (_,                 "fputs_unlocked",  0),
    // The long-double magnitude and sign builtins lower to these rather than
    // to `fabs`/`__signbit`, which take a `double` and so read only the low
    // eight bytes of an x87 value. `fabsl` is named above, as a plain
    // spelling the parser recognizes; it is still looked up here by string,
    // to synthesize the prototype the call needs.
    (_,                 "__signbitl",           0),

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
    (_, "malloc",               SUPPORTED_ATTR),
    (_, "pure",                 SUPPORTED_ATTR),
    (_, "sentinel",             SUPPORTED_ATTR),
    (_, "no_sanitize_memory",   SUPPORTED_ATTR),
    (_, "no_sanitize_address",  SUPPORTED_ATTR),
    (_, "no_sanitize_thread",   SUPPORTED_ATTR),
    (_, "gnu_inline",              SUPPORTED_ATTR),
    (_, "artificial",              SUPPORTED_ATTR),
    (_, "sysv_abi",                SUPPORTED_ATTR),
    (_, "ms_abi",                  SUPPORTED_ATTR),
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
    (_, "noclone",                 SUPPORTED_ATTR),
    (_, "no_instrument_function",  SUPPORTED_ATTR),
    (_, "nonnull_if_nonzero",      SUPPORTED_ATTR),
    (_, "copy",                    SUPPORTED_ATTR),
    (_, "designated_init",         SUPPORTED_ATTR),
    (_, "may_alias",               SUPPORTED_ATTR),
    // GNU forms (__foo__)
    // Note: __noreturn__ is already defined above with NORETURN_KW | SUPPORTED_ATTR
    (_, "__unused__",           SUPPORTED_ATTR),
    (_, "__aligned__",          SUPPORTED_ATTR),
    (_, "__packed__",           SUPPORTED_ATTR),
    (_, "__deprecated__",       SUPPORTED_ATTR),
    (_, "__weak__",             SUPPORTED_ATTR),
    (_, "__section__",          SUPPORTED_ATTR),
    (_, "__visibility__",       SUPPORTED_ATTR),
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
    (_, "__sysv_abi__",              SUPPORTED_ATTR),
    (_, "__ms_abi__",                SUPPORTED_ATTR),
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
    (_, "__noclone__",               SUPPORTED_ATTR),
    (_, "__no_instrument_function__", SUPPORTED_ATTR),
    (_, "__nonnull_if_nonzero__",    SUPPORTED_ATTR),
    (_, "__copy__",                  SUPPORTED_ATTR),
    (_, "__designated_init__",       SUPPORTED_ATTR),
    (_, "__may_alias__",             SUPPORTED_ATTR),
    // GCC's complex-part operators. Appended, because `define_ids!` numbers
    // entries by table position. Both spellings of each, as gcc accepts.
    (REAL_KW,           "__real__",          RESERVED_NAME),
    (REAL_KW_SHORT,     "__real",            RESERVED_NAME),
    (IMAG_KW,           "__imag__",          RESERVED_NAME),
    (IMAG_KW_SHORT,     "__imag",            RESERVED_NAME),
}

// Tag query API

/// Check if a StringId has any of the given tag bits set.
/// Returns false for non-keyword IDs (dynamic strings interned after keywords).
pub fn has_tag(id: StringId, mask: u32) -> bool {
    let idx = id.0 as usize;
    idx > 0 && idx <= KEYWORD_COUNT && KEYWORD_TAGS[idx - 1] & mask != 0
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
            "_Thread_local",
            "__thread",
            "_Static_assert",
            "static_assert",
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
