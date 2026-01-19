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

use crate::common::compile_and_run;

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
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_has_feature_mega", code, &[]), 0);
}
