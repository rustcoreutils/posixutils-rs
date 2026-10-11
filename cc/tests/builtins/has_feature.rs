//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Has Feature/Builtin Mega-Test
//
// Consolidates: __has_builtin, __has_feature tests
//

use crate::common::{compile_and_run, compile_and_run_everywhere};

// ============================================================================
// Mega-test: __has_builtin, __has_feature, __has_attribute
// ============================================================================

#[test]
fn builtins_has_feature_mega() {
    let code = r#"
int main(void) {
    // ========== __HAS_BUILTIN (returns 1-29) ==========
    {
        // Common builtins that should exist
#if !__has_builtin(__builtin_expect)
        return 1;
#endif

#if !__has_builtin(__builtin_clz)
        return 2;
#endif

#if !__has_builtin(__builtin_ctz)
        return 3;
#endif

#if !__has_builtin(__builtin_popcount)
        return 4;
#endif

#if !__has_builtin(__builtin_bswap32)
        return 5;
#endif

#if !__has_builtin(__builtin_bswap64)
        return 6;
#endif

#if !__has_builtin(__builtin_alloca)
        return 7;
#endif

#if !__has_builtin(__builtin_types_compatible_p)
        return 8;
#endif

#if !__has_builtin(__builtin_constant_p)
        return 9;
#endif

        // C11 atomic builtins
#if !__has_builtin(__c11_atomic_load)
        return 11;
#endif

#if !__has_builtin(__c11_atomic_store)
        return 12;
#endif

#if !__has_builtin(__c11_atomic_fetch_add)
        return 13;
#endif

#if !__has_builtin(__c11_atomic_compare_exchange_strong)
        return 14;
#endif

        // Builtins gcc makes for its own lowering
#if !__has_builtin(__builtin_stack_save) || !__has_builtin(__builtin_stack_restore)
        return 16;
#endif

#if !__has_builtin(__builtin_clear_padding)
        return 17;
#endif

#if !__has_builtin(__builtin_cexpi) || !__has_builtin(__builtin_cexpif) || !__has_builtin(__builtin_cexpil)
        return 18;
#endif

#if !__has_builtin(__builtin_cpow) || !__has_builtin(__builtin_cpowf) || !__has_builtin(__builtin_cpowl)
        return 19;
#endif

        // Non-existent builtin should return 0
#if __has_builtin(__nonexistent_builtin_xyz)
        return 15;
#endif
    }

    // ========== __HAS_FEATURE (returns 30-49) ==========
    {
        // C11 features
#if !__has_feature(c_atomic)
        return 30;
#endif

#if !__has_feature(c_static_assert)
        return 31;
#endif

#if !__has_feature(c_alignof)
        return 32;
#endif

#if !__has_feature(c_alignas)
        return 33;
#endif

#if !__has_feature(c_thread_local)
        return 34;
#endif

        // Non-existent feature should return 0
#if __has_feature(nonexistent_feature_xyz)
        return 35;
#endif
    }

    // ========== __HAS_EXTENSION (returns 50-69) ==========
    {
        // GNU extensions
#if !__has_extension(gnu_asm)
        return 50;
#endif

#if !__has_extension(statement_expressions_in_macros)
        return 51;
#endif

        // C features as extensions
#if !__has_extension(c_atomic)
        return 52;
#endif
    }

    // ========== __HAS_ATTRIBUTE (returns 70-89) ==========
    {
#if !__has_attribute(unused)
        return 70;
#endif

#if !__has_attribute(aligned)
        return 71;
#endif

#if !__has_attribute(packed)
        return 72;
#endif

#if !__has_attribute(noreturn)
        return 73;
#endif

#if !__has_attribute(deprecated)
        return 74;
#endif

        // Non-existent attribute should return 0
#if __has_attribute(nonexistent_attribute_xyz)
        return 75;
#endif

        // CPython-required attributes
#if !__has_attribute(noinline)
        return 76;
#endif

#if !__has_attribute(always_inline)
        return 77;
#endif

#if !__has_attribute(hot)
        return 78;
#endif

#if !__has_attribute(cold)
        return 79;
#endif

#if !__has_attribute(warn_unused_result)
        return 80;
#endif

#if !__has_attribute(format)
        return 81;
#endif

#if !__has_attribute(fallthrough)
        return 82;
#endif

#if !__has_attribute(malloc)
        return 83;
#endif

#if !__has_attribute(pure)
        return 84;
#endif

#if !__has_attribute(sentinel)
        return 85;
#endif

#if !__has_attribute(no_sanitize_address)
        return 86;
#endif

        // Double-underscore variants
#if !__has_attribute(__noinline__)
        return 87;
#endif

#if !__has_attribute(__always_inline__)
        return 88;
#endif

#if !__has_attribute(cleanup) || !__has_attribute(__cleanup__)
        return 89;
#endif
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_has_feature_mega", code, &[]), 0);
}

/// `__builtin_has_attribute`, gcc 13's answers for declarations and types,
/// usable where an integer constant is required; and `__has_builtin`
/// admits it. The operand is not evaluated.
#[test]
fn builtins_has_attribute() {
    let code = r#"
/* Mach-O names a segment with the section. */
#ifdef __APPLE__
#define XX "__DATA,xx"
#define YY "__DATA,yy"
#else
#define XX "xx"
#define YY "yy"
#endif
extern void f1(void) __attribute__((noreturn, cold));
int v1 __attribute__((aligned(16), used));
static int v2 __attribute__((section(XX), unused)) = 3;
typedef int T1 __attribute__((aligned(32)));
typedef int V4 __attribute__((vector_size(16)));
struct __attribute__((packed)) P { char c; int i; };
extern int pf(const char *, ...) __attribute__((format(printf, 1, 2), nonnull(1)));
int vis __attribute__((visibility("hidden")));
#if !__has_builtin(__builtin_has_attribute)
#error "__has_builtin(__builtin_has_attribute)"
#endif
_Static_assert(__builtin_has_attribute(f1, __noreturn__), "noreturn");
char a[__builtin_has_attribute(v1, aligned(16)) ? 1 : -1];
int main(void) {
    int n = 0;
    int r[] = {
        __builtin_has_attribute(f1, cold), !__builtin_has_attribute(f1, hot),
        __builtin_has_attribute(v1, used), !__builtin_has_attribute(v1, aligned(8)),
        __builtin_has_attribute(v2, section(XX)), !__builtin_has_attribute(v2, section(YY)),
        __builtin_has_attribute(v2, unused), __builtin_has_attribute(T1, aligned),
        !__builtin_has_attribute(int, aligned), __builtin_has_attribute(struct P, packed),
        __builtin_has_attribute(V4, vector_size(16)), __builtin_has_attribute(pf, format),
        __builtin_has_attribute(vis, visibility("hidden")),
        !__builtin_has_attribute(vis, visibility("default")),
        !__builtin_has_attribute(int[n++], aligned),
    };
    for (unsigned i = 0; i < sizeof r / sizeof r[0]; i++)
        if (!r[i])
            return 1 + (int)i;
    return n;
}
"#;
    compile_and_run_everywhere("builtins_has_attribute", code);
}
