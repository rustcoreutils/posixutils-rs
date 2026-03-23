//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Pre-interned keyword system for pcc C99 compiler
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

// ============================================================================
// Tag bit constants (u32, 14 of 32 used)
// ============================================================================

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

/// Composite: all tags that start a declaration
pub const DECL_START: u32 =
    TYPE_SPEC | STORAGE | QUALIFIER | INLINE_KW | NORETURN_KW | ATTR_KW | ASSERT_KW | ALIGNAS_KW;

// ============================================================================
// Keyword definition macros
// ============================================================================

/// Helper macro: recursive counter that assigns sequential StringId values starting from 1.
macro_rules! define_ids {
    // Base case: no more entries
    ($counter:expr; ) => {};
    // Recursive case: emit one const, recurse with counter+1
    ($counter:expr; ($name:ident, $str:literal, $tags:expr) $(, ($name_rest:ident, $str_rest:literal, $tags_rest:expr))* $(,)? ) => {
        pub const $name: StringId = StringId($counter);
        define_ids!($counter + 1; $(($name_rest, $str_rest, $tags_rest)),*);
    };
}

/// Main keyword definition macro. Generates:
/// - KEYWORD_COUNT: total number of keywords
/// - One `pub const NAME: StringId` per keyword
/// - KEYWORD_STRINGS: array of string literals
/// - KEYWORD_TAGS: array of tag bitmasks
macro_rules! define_keywords {
    ( $( ($name:ident, $str:literal, $tags:expr) ),* $(,)? ) => {
        pub const KEYWORD_COUNT: usize = [ $( $str ),* ].len();
        define_ids!(1u32; $( ($name, $str, $tags) ),* );
        pub(crate) const KEYWORD_STRINGS: [&str; KEYWORD_COUNT] = [ $( $str ),* ];
        pub(crate) const KEYWORD_TAGS: [u32; KEYWORD_COUNT] = [ $( $tags ),* ];
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
    (FLOAT16,           "_Float16",          TYPE_SPEC | TYPE_KEYWORD),
    (FLOAT32,           "_Float32",          TYPE_SPEC | TYPE_KEYWORD),
    (FLOAT64,           "_Float64",          TYPE_SPEC | TYPE_KEYWORD),
    (INT128,            "__int128",          TYPE_SPEC | TYPE_KEYWORD),
    (INT128_T,          "__int128_t",        TYPE_SPEC | TYPE_KEYWORD),
    (UINT128_T,         "__uint128_t",       TYPE_SPEC | TYPE_KEYWORD),
    (BUILTIN_VA_LIST,   "__builtin_va_list", TYPE_SPEC | TYPE_KEYWORD | BUILTIN),
    (STRUCT,            "struct",            TYPE_SPEC | TYPE_KEYWORD),
    (UNION,             "union",             TYPE_SPEC | TYPE_KEYWORD),
    (ENUM,              "enum",              TYPE_SPEC | TYPE_KEYWORD),
    (TYPEOF,            "typeof",            TYPE_SPEC | TYPE_KEYWORD),
    (GNU_TYPEOF,        "__typeof__",        TYPE_SPEC | TYPE_KEYWORD),
    (GNU_TYPEOF2,       "__typeof",          TYPE_SPEC | TYPE_KEYWORD),
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
    (CONST,             "const",             QUALIFIER | TYPE_KEYWORD),
    (VOLATILE,          "volatile",          QUALIFIER | TYPE_KEYWORD),
    (RESTRICT,          "restrict",          QUALIFIER),
    (GNU_CONST,         "__const__",         QUALIFIER),
    (GNU_CONST2,        "__const",           QUALIFIER),
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
    (STATIC_ASSERT,     "_Static_assert",    ASSERT_KW),
    (STATIC_ASSERT_C23, "static_assert",     ASSERT_KW),

    // ---- Alignas (ALIGNAS_KW) ----
    (ALIGNAS,           "_Alignas",          ALIGNAS_KW),

    // ---- Nullability qualifiers (NULLABILITY) ----
    (NONNULL,           "_Nonnull",          NULLABILITY),
    (GNU_NONNULL,       "__nonnull",         NULLABILITY),
    (NULLABLE,          "_Nullable",         NULLABILITY),
    (GNU_NULLABLE,      "__nullable",        NULLABILITY),
    (NULL_UNSPECIFIED,  "_Null_unspecified",  NULLABILITY),
    (GNU_NULL_UNSPECIFIED, "__null_unspecified", NULLABILITY),

    // ---- Statement keywords (STMT_KW) ----
    (IF,                "if",                STMT_KW),
    (ELSE,              "else",              STMT_KW),
    (WHILE,             "while",             STMT_KW),
    (DO,                "do",                STMT_KW),
    (FOR,               "for",               STMT_KW),
    (RETURN,            "return",            STMT_KW),
    (BREAK,             "break",             STMT_KW),
    (CONTINUE,          "continue",          STMT_KW),
    (GOTO,              "goto",              STMT_KW),
    (SWITCH,            "switch",            STMT_KW),
    (CASE,              "case",              STMT_KW),
    (DEFAULT,           "default",           STMT_KW),

    // ---- Sizeof / Alignof ----
    (SIZEOF,            "sizeof",            0),
    (ALIGNOF,           "_Alignof",          0),
    (GNU_ALIGNOF,       "__alignof__",       0),
    (GNU_ALIGNOF2,      "__alignof",         0),
    (ALIGNOF_C23,       "alignof",           0),

    // ---- Wide char prefix ----
    (WIDE_PREFIX,       "L",                 0),

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

    // ---- Preprocessor special names ----
    (DEFINED,           "defined",           0),
    (VA_ARGS,           "__VA_ARGS__",       0),
    (ONCE,              "once",              0),

    // ---- Predefined identifiers ----
    (FUNC,              "__func__",          0),
    (FUNCTION,          "__FUNCTION__",      0),
    (PRETTY_FUNCTION,   "__PRETTY_FUNCTION__", 0),

    // ---- Builtins (BUILTIN) ----
    (BUILTIN_VA_START,  "__builtin_va_start", BUILTIN),
    (BUILTIN_VA_END,    "__builtin_va_end",   BUILTIN),
    (BUILTIN_VA_ARG,    "__builtin_va_arg",   BUILTIN),
    (BUILTIN_VA_COPY,   "__builtin_va_copy",  BUILTIN),
    (BUILTIN_BSWAP16,   "__builtin_bswap16",  BUILTIN),
    (BUILTIN_BSWAP32,   "__builtin_bswap32",  BUILTIN),
    (BUILTIN_BSWAP64,   "__builtin_bswap64",  BUILTIN),
    (BUILTIN_CTZ,       "__builtin_ctz",      BUILTIN),
    (BUILTIN_CTZL,      "__builtin_ctzl",     BUILTIN),
    (BUILTIN_CTZLL,     "__builtin_ctzll",    BUILTIN),
    (BUILTIN_CLZ,       "__builtin_clz",      BUILTIN),
    (BUILTIN_CLZL,      "__builtin_clzl",     BUILTIN),
    (BUILTIN_CLZLL,     "__builtin_clzll",    BUILTIN),
    (BUILTIN_POPCOUNT,  "__builtin_popcount", BUILTIN),
    (BUILTIN_POPCOUNTL, "__builtin_popcountl", BUILTIN),
    (BUILTIN_POPCOUNTLL, "__builtin_popcountll", BUILTIN),
    (BUILTIN_ALLOCA,    "__builtin_alloca",   BUILTIN),
    (BUILTIN_MEMSET,    "__builtin_memset",   BUILTIN),
    (BUILTIN_MEMCPY,    "__builtin_memcpy",   BUILTIN),
    (BUILTIN_MEMMOVE,   "__builtin_memmove",  BUILTIN),
    (BUILTIN_CONSTANT_P, "__builtin_constant_p", BUILTIN),
    (BUILTIN_TYPES_COMPATIBLE_P, "__builtin_types_compatible_p", BUILTIN),
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
    (BUILTIN_SIGNBIT,   "__builtin_signbit",  BUILTIN),
    (BUILTIN_SIGNBITF,  "__builtin_signbitf", BUILTIN),
    (BUILTIN_SIGNBITL,  "__builtin_signbitl", BUILTIN),
    (BUILTIN_NAN,       "__builtin_nan",      BUILTIN),
    (BUILTIN_NANF,      "__builtin_nanf",     BUILTIN),
    (BUILTIN_NANL,      "__builtin_nanl",     BUILTIN),
    (BUILTIN_NANS,      "__builtin_nans",     BUILTIN),
    (BUILTIN_NANSF,     "__builtin_nansf",    BUILTIN),
    (BUILTIN_NANSL,     "__builtin_nansl",    BUILTIN),
    (BUILTIN_EXPECT,    "__builtin_expect",   BUILTIN),
    (BUILTIN_ASSUME_ALIGNED, "__builtin_assume_aligned", BUILTIN),
    (BUILTIN_PREFETCH,  "__builtin_prefetch", BUILTIN),
    (BUILTIN_FLT_ROUNDS, "__builtin_flt_rounds", BUILTIN),
    (BUILTIN_FRAME_ADDRESS, "__builtin_frame_address", BUILTIN),
    (BUILTIN_RETURN_ADDRESS, "__builtin_return_address", BUILTIN),
    (BUILTIN_OBJECT_SIZE, "__builtin_object_size", BUILTIN),
    (BUILTIN_SNPRINTF_CHK, "__builtin___snprintf_chk", BUILTIN),
    (BUILTIN_VSNPRINTF_CHK, "__builtin___vsnprintf_chk", BUILTIN),
    (BUILTIN_SPRINTF_CHK, "__builtin___sprintf_chk", BUILTIN),
    (BUILTIN_FPRINTF_CHK, "__builtin___fprintf_chk", BUILTIN),
    (BUILTIN_PRINTF_CHK, "__builtin___printf_chk", BUILTIN),
    (BUILTIN_MEMCPY_CHK, "__builtin___memcpy_chk", BUILTIN),
    (BUILTIN_MEMMOVE_CHK, "__builtin___memmove_chk", BUILTIN),
    (BUILTIN_MEMSET_CHK, "__builtin___memset_chk", BUILTIN),
    (BUILTIN_STPCPY_CHK, "__builtin___stpcpy_chk", BUILTIN),
    (BUILTIN_STRCAT_CHK, "__builtin___strcat_chk", BUILTIN),
    (BUILTIN_STRCPY_CHK, "__builtin___strcpy_chk", BUILTIN),
    (BUILTIN_STRNCAT_CHK, "__builtin___strncat_chk", BUILTIN),
    (BUILTIN_STRNCPY_CHK, "__builtin___strncpy_chk", BUILTIN),

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
    (SETJMP,            "setjmp",            0),
    (SETJMP2,           "_setjmp",           0),
    (LONGJMP,           "longjmp",           0),
    (LONGJMP2,          "_longjmp",          0),

    // ---- Supported attribute names (SUPPORTED_ATTR) ----
    // Plain forms
    (ATTR_NORETURN,             "noreturn",             SUPPORTED_ATTR),
    (ATTR_UNUSED,               "unused",               SUPPORTED_ATTR),
    (ATTR_ALIGNED,              "aligned",              SUPPORTED_ATTR),
    (ATTR_PACKED,               "packed",               SUPPORTED_ATTR),
    (ATTR_DEPRECATED,           "deprecated",           SUPPORTED_ATTR),
    (ATTR_WEAK,                 "weak",                 SUPPORTED_ATTR),
    (ATTR_SECTION,              "section",              SUPPORTED_ATTR),
    (ATTR_VISIBILITY,           "visibility",           SUPPORTED_ATTR),
    (ATTR_CONSTRUCTOR,          "constructor",          SUPPORTED_ATTR),
    (ATTR_DESTRUCTOR,           "destructor",           SUPPORTED_ATTR),
    (ATTR_USED,                 "used",                 SUPPORTED_ATTR),
    (ATTR_NOINLINE,             "noinline",             SUPPORTED_ATTR),
    (ATTR_ALWAYS_INLINE,        "always_inline",        SUPPORTED_ATTR),
    (ATTR_HOT,                  "hot",                  SUPPORTED_ATTR),
    (ATTR_COLD,                 "cold",                 SUPPORTED_ATTR),
    (ATTR_WARN_UNUSED_RESULT,   "warn_unused_result",   SUPPORTED_ATTR),
    (ATTR_FORMAT,               "format",               SUPPORTED_ATTR),
    (ATTR_FALLTHROUGH,          "fallthrough",          SUPPORTED_ATTR),
    (ATTR_NONSTRING,            "nonstring",            SUPPORTED_ATTR),
    (ATTR_MALLOC,               "malloc",               SUPPORTED_ATTR),
    (ATTR_PURE,                 "pure",                 SUPPORTED_ATTR),
    (ATTR_SENTINEL,             "sentinel",             SUPPORTED_ATTR),
    (ATTR_NO_SANITIZE_MEMORY,   "no_sanitize_memory",   SUPPORTED_ATTR),
    (ATTR_NO_SANITIZE_ADDRESS,  "no_sanitize_address",  SUPPORTED_ATTR),
    (ATTR_NO_SANITIZE_THREAD,   "no_sanitize_thread",   SUPPORTED_ATTR),
    // GNU forms (__foo__)
    // Note: __noreturn__ is already defined above with NORETURN_KW | SUPPORTED_ATTR
    (GNU_ATTR_UNUSED,           "__unused__",           SUPPORTED_ATTR),
    (GNU_ATTR_ALIGNED,          "__aligned__",          SUPPORTED_ATTR),
    (GNU_ATTR_PACKED,           "__packed__",           SUPPORTED_ATTR),
    (GNU_ATTR_DEPRECATED,       "__deprecated__",       SUPPORTED_ATTR),
    (GNU_ATTR_WEAK,             "__weak__",             SUPPORTED_ATTR),
    (GNU_ATTR_SECTION,          "__section__",          SUPPORTED_ATTR),
    (GNU_ATTR_VISIBILITY,       "__visibility__",       SUPPORTED_ATTR),
    (GNU_ATTR_CONSTRUCTOR,      "__constructor__",      SUPPORTED_ATTR),
    (GNU_ATTR_DESTRUCTOR,       "__destructor__",       SUPPORTED_ATTR),
    (GNU_ATTR_USED,             "__used__",             SUPPORTED_ATTR),
    (GNU_ATTR_NOINLINE,         "__noinline__",         SUPPORTED_ATTR),
    (GNU_ATTR_ALWAYS_INLINE,    "__always_inline__",    SUPPORTED_ATTR),
    (GNU_ATTR_HOT,              "__hot__",              SUPPORTED_ATTR),
    (GNU_ATTR_COLD,             "__cold__",             SUPPORTED_ATTR),
    (GNU_ATTR_WARN_UNUSED_RESULT, "__warn_unused_result__", SUPPORTED_ATTR),
    (GNU_ATTR_FORMAT,           "__format__",           SUPPORTED_ATTR),
    (GNU_ATTR_FALLTHROUGH,      "__fallthrough__",      SUPPORTED_ATTR),
    (GNU_ATTR_NONSTRING,        "__nonstring__",        SUPPORTED_ATTR),
    (GNU_ATTR_MALLOC,           "__malloc__",           SUPPORTED_ATTR),
    (GNU_ATTR_PURE,             "__pure__",             SUPPORTED_ATTR),
    (GNU_ATTR_SENTINEL,         "__sentinel__",         SUPPORTED_ATTR),
}

// ============================================================================
// Tag query API
// ============================================================================

/// Check if a StringId has any of the given tag bits set.
/// Returns false for non-keyword IDs (dynamic strings interned after keywords).
#[inline]
pub fn has_tag(id: StringId, mask: u32) -> bool {
    let idx = id.0 as usize;
    idx > 0 && idx <= KEYWORD_COUNT && KEYWORD_TAGS[idx - 1] & mask != 0
}

/// Get the full tag bitmask for a StringId.
/// Returns 0 for non-keyword IDs.
#[inline]
pub fn tags(id: StringId) -> u32 {
    let idx = id.0 as usize;
    if idx > 0 && idx <= KEYWORD_COUNT {
        KEYWORD_TAGS[idx - 1]
    } else {
        0
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::strings::StringTable;
    use std::collections::HashSet;

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
        assert_eq!(table.get(ATTR_NORETURN), "noreturn");
        assert_eq!(table.get(GNU_ATTR_PACKED), "__packed__");
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
        let type_specs = [
            VOID,
            CHAR,
            SHORT,
            INT,
            LONG,
            FLOAT,
            DOUBLE,
            SIGNED,
            UNSIGNED,
            BOOL,
            COMPLEX,
            FLOAT16,
            FLOAT32,
            FLOAT64,
            INT128,
            INT128_T,
            UINT128_T,
            BUILTIN_VA_LIST,
            STRUCT,
            UNION,
            ENUM,
            TYPEOF,
            GNU_TYPEOF,
            GNU_TYPEOF2,
            ATOMIC,
        ];
        for &id in &type_specs {
            assert!(
                has_tag(id, TYPE_SPEC),
                "'{}' (id={}) should have TYPE_SPEC",
                KEYWORD_STRINGS[id.0 as usize - 1],
                id.0
            );
        }
    }

    #[test]
    fn test_tags_qualifier() {
        let qualifiers = [
            CONST,
            VOLATILE,
            RESTRICT,
            ATOMIC,
            GNU_CONST,
            GNU_CONST2,
            GNU_VOLATILE,
            GNU_VOLATILE2,
            GNU_RESTRICT,
            GNU_RESTRICT2,
        ];
        for &id in &qualifiers {
            assert!(
                has_tag(id, QUALIFIER),
                "'{}' should have QUALIFIER",
                KEYWORD_STRINGS[id.0 as usize - 1]
            );
        }
    }

    #[test]
    fn test_tags_type_keyword() {
        // Exact 25 members that match is_type_keyword()
        let type_keywords = [
            VOID,
            BOOL,
            COMPLEX,
            ATOMIC,
            CHAR,
            SHORT,
            INT,
            LONG,
            FLOAT,
            DOUBLE,
            FLOAT16,
            FLOAT32,
            FLOAT64,
            SIGNED,
            UNSIGNED,
            CONST,
            VOLATILE,
            STRUCT,
            UNION,
            ENUM,
            INT128,
            INT128_T,
            UINT128_T,
            BUILTIN_VA_LIST,
            TYPEOF,
            GNU_TYPEOF,
            GNU_TYPEOF2,
        ];
        for &id in &type_keywords {
            assert!(
                has_tag(id, TYPE_KEYWORD),
                "'{}' should have TYPE_KEYWORD",
                KEYWORD_STRINGS[id.0 as usize - 1]
            );
        }
    }

    #[test]
    fn test_tags_decl_start() {
        // All 43+ entries from current is_declaration_start()
        let decl_start = [
            VOID,
            CHAR,
            SHORT,
            INT,
            LONG,
            FLOAT,
            DOUBLE,
            FLOAT16,
            FLOAT32,
            FLOAT64,
            COMPLEX,
            ATOMIC,
            ALIGNAS,
            SIGNED,
            UNSIGNED,
            CONST,
            VOLATILE,
            STATIC,
            EXTERN,
            AUTO,
            REGISTER,
            TYPEDEF,
            INLINE,
            GNU_INLINE2,
            GNU_INLINE,
            NORETURN,
            GNU_NORETURN,
            STRUCT,
            UNION,
            ENUM,
            BOOL,
            GNU_ATTRIBUTE,
            GNU_ATTRIBUTE2,
            INT128,
            INT128_T,
            UINT128_T,
            BUILTIN_VA_LIST,
            TYPEOF,
            GNU_TYPEOF,
            GNU_TYPEOF2,
            THREAD_LOCAL,
            GNU_THREAD,
            STATIC_ASSERT,
            STATIC_ASSERT_C23,
        ];
        for &id in &decl_start {
            assert!(
                has_tag(id, DECL_START),
                "'{}' should have DECL_START",
                KEYWORD_STRINGS[id.0 as usize - 1]
            );
        }
    }

    #[test]
    fn test_tags_nullability() {
        let nullability = [
            NONNULL,
            GNU_NONNULL,
            NULLABLE,
            GNU_NULLABLE,
            NULL_UNSPECIFIED,
            GNU_NULL_UNSPECIFIED,
        ];
        for &id in &nullability {
            assert!(
                has_tag(id, NULLABILITY),
                "'{}' should have NULLABILITY",
                KEYWORD_STRINGS[id.0 as usize - 1]
            );
        }
    }

    #[test]
    fn test_tags_builtin() {
        // Spot-check some builtins
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
        for &id in &builtins {
            assert!(
                has_tag(id, BUILTIN),
                "'{}' should have BUILTIN",
                KEYWORD_STRINGS[id.0 as usize - 1]
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
        let attrs = [
            ATTR_NORETURN,
            GNU_NORETURN,
            ATTR_UNUSED,
            GNU_ATTR_UNUSED,
            ATTR_ALIGNED,
            GNU_ATTR_ALIGNED,
            ATTR_PACKED,
            GNU_ATTR_PACKED,
            ATTR_ALWAYS_INLINE,
            GNU_ATTR_ALWAYS_INLINE,
        ];
        for &id in &attrs {
            assert!(
                has_tag(id, SUPPORTED_ATTR),
                "'{}' should have SUPPORTED_ATTR",
                KEYWORD_STRINGS[id.0 as usize - 1]
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
