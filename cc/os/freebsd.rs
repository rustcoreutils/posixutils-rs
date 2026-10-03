//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// FreeBSD-specific predefined macros
//

/// Get FreeBSD-specific predefined macros
pub fn get_macros() -> Vec<(&'static str, Option<String>)> {
    vec![
        // FreeBSD identification
        // ELF adds no prefix to a C identifier.
        ("__USER_LABEL_PREFIX__", Some("".into())),
        ("__FreeBSD__", Some("13".into())), // Conservative version
        // ELF binary format
        ("__ELF__", Some("1".into())),
        // Not `__FreeBSD_kernel__`, which names a FreeBSD kernel under a GNU
        // userland (GNU/kFreeBSD), nor `__BSD_VISIBLE`, which <sys/cdefs.h>
        // computes from the feature-test macros a program defines -- as the
        // system compiler leaves it to.
        // POSIX threads
        ("_REENTRANT", Some("1".into())),
    ]
}

/// Get standard include paths for FreeBSD
pub fn get_include_paths() -> Vec<&'static str> {
    vec!["/usr/local/include", "/usr/include"]
}
