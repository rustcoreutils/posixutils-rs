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
/// Entries named `_` are anonymous — they get interned and tagged but no `pub const` is emitted.
macro_rules! define_ids {
    // Base case: no more entries
    ($counter:expr; ) => {};
    // Anonymous entry (name is `_`): skip const, just recurse
    ($counter:expr; (_, $str:literal, $tags:expr) $(, ($name_rest:tt, $str_rest:literal, $tags_rest:expr))* $(,)? ) => {
        define_ids!($counter + 1; $(($name_rest, $str_rest, $tags_rest)),*);
    };
    // Named entry: emit pub const, then recurse
    ($counter:expr; ($name:ident, $str:literal, $tags:expr) $(, ($name_rest:tt, $str_rest:literal, $tags_rest:expr))* $(,)? ) => {
        pub const $name: StringId = StringId($counter);
        define_ids!($counter + 1; $(($name_rest, $str_rest, $tags_rest)),*);
    };
}

/// Main keyword definition macro. Generates:
/// - KEYWORD_COUNT: total number of keywords
/// - One `pub const NAME: StringId` per named keyword (entries with `_` are anonymous)
/// - KEYWORD_STRINGS: array of string literals (all entries)
/// - KEYWORD_TAGS: array of tag bitmasks (all entries)
macro_rules! define_keywords {
    ( $( ($name:tt, $str:literal, $tags:expr) ),* $(,)? ) => {
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
    (_,                 "_Static_assert",    ASSERT_KW),
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
    (SETJMP,            "setjmp",            0),
    (SETJMP2,           "_setjmp",           0),
    (LONGJMP,           "longjmp",           0),
    (LONGJMP2,          "_longjmp",          0),

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

// ============================================================================
// Tests
// ============================================================================

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
