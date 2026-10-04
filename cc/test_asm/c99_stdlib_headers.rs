//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 standard headers: the compile-only halves of the `<limits.h>` cases in
// `tests/c99/stdlib_headers.rs`, whose run-time halves run there as one
// program. A redefinition warning is a property of each original program's
// own include sequence, so each is compiled as it was written.
//

use crate::test_compile::compile_expect_no_diagnostic;

/// `SSIZE_MAX` belongs to `<limits.h>` (POSIX), not to the compiler. It was
/// predefined, so a program that defines its own, or tests `#ifndef
/// SSIZE_MAX` to learn whether it has included `<limits.h>`, was wrong before
/// it included anything. It comes from the system's `<limits.h>`, which the
/// bundled one forwards to.
const SSIZE_LIMIT: &str = r#"
#ifdef SSIZE_MAX
#error "SSIZE_MAX is defined before <limits.h>"
#endif
#include <limits.h>
#include <limits.h>
#include <stddef.h>
#include <stdint.h>

#if !(SSIZE_MAX > 0 && SSIZE_MAX == SIZE_MAX / 2)
#error "SSIZE_MAX is not usable in #if, or not size_t's signed half"
#endif

int main(void) {
    /* ssize_t is the signed type of size_t's width. */
    if (sizeof(SSIZE_MAX) != sizeof(size_t)) return 1;
    if ((__typeof__(SSIZE_MAX))-1 >= 0) return 2;
    if (SSIZE_MAX != PTRDIFF_MAX) return 3;
    if (_POSIX_SSIZE_MAX != 32767 || SSIZE_MAX < _POSIX_SSIZE_MAX) return 4;
    return 0;
}
"#;

#[test]
fn c99_ssize_max_comes_from_limits_h() {
    compile_expect_no_diagnostic("ssize_limit_clean", SSIZE_LIMIT, "redefin");
}

/// POSIX adds its limits to `<limits.h>`, which the system's header defines:
/// the bundled one owns only the integer sizes and forwards to the system's
/// for the rest. It used to stop at the sizes, so `LINE_MAX`, `NGROUPS_MAX`,
/// `IOV_MAX` and the like were undeclared. The POSIX base names are checked
/// everywhere, the XSI ones against glibc; the sizes must still be the
/// compiler's.
const POSIX_LIMITS: &str = r#"
#include <limits.h>
#include <stdio.h>
#include <limits.h>

#if LINE_MAX < _POSIX2_LINE_MAX || NGROUPS_MAX < _POSIX_NGROUPS_MAX
#error "LINE_MAX or NGROUPS_MAX missing or below its POSIX minimum"
#endif
#if RE_DUP_MAX < _POSIX2_RE_DUP_MAX
#error "RE_DUP_MAX missing or below its POSIX minimum"
#endif
#ifdef __linux__
#if IOV_MAX < _XOPEN_IOV_MAX || !defined HOST_NAME_MAX
#error "IOV_MAX missing or below its POSIX minimum, or HOST_NAME_MAX missing"
#endif
#if LONG_BIT != __SIZEOF_LONG__ * CHAR_BIT || WORD_BIT != __SIZEOF_INT__ * CHAR_BIT
#error "LONG_BIT or WORD_BIT wrong"
#endif
#endif

int main(void) {
    if (_POSIX_ARG_MAX != 4096 || _POSIX2_LINE_MAX != 2048) return 1;
    if (CHAR_BIT != __CHAR_BIT__ || INT_MAX != __INT_MAX__) return 3;
    if (LLONG_MIN != -__LONG_LONG_MAX__ - 1 || ULLONG_MAX != (unsigned long long)-1) return 4;
    if (SCHAR_MIN != -128 || UCHAR_MAX != 255 || USHRT_MAX != 65535) return 5;
    if (MB_LEN_MAX < 1) return 6;
    return 0;
}
"#;

#[test]
fn c99_limits_h_has_the_posix_limits() {
    compile_expect_no_diagnostic("posix_limits_clean", POSIX_LIMITS, "redefin");
}
