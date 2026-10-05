//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly cases of tests/codegen/memopt_blockops.rs, in process: a block op
// (a `memset`, `memcpy` or `memmove` too long for `memexpand`) dereferences its
// pointers and keeps none of them, so a local it touches has not escaped. These
// prove those answers are used, by a call to an undefined `link_error` that only
// a forwarded load deletes.
//

use crate::test_asm::asm_probe::{asm_for_with, assert_body_lacks, AARCH64_LINUX, X86_64_LINUX};

/// Assert that `main` of `src`, compiled at -O2 without inlining for both
/// targets, no longer calls `link_error`.
fn assert_link_error_folded(name: &str, src: &str, why: &str) {
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with(name, triple, src, &["-O2", "-fno-inline"]);
        assert_body_lacks(&asm, "main", "link_error", why);
    }
}

/// A `static` function whose only write is a `memset` of its own buffer
/// writes nothing its caller can observe, so a global the caller stored
/// survives the call to it -- exactly as it would if the buffer were filled
/// by stores.
#[test]
fn memopt_blockops_a_memset_of_a_private_buffer_is_no_write() {
    let src = r#"
extern void *memset(void *, int, unsigned long);
extern void *memcpy(void *, const void *, unsigned long);
extern void *memmove(void *, const void *, unsigned long);
extern void abort(void);
extern void link_error(void);
int g;

static int fill(int n) {
    char buf[256];
    memset(buf, n, sizeof buf);
    return buf[n & 255];
}

int main(void) {
    g = 5;
    int r = fill(3);
    if (g != 5) link_error();
    return r - 3;
}
"#;
    assert_link_error_folded(
        "blockops_private_memset",
        src,
        "fill writes only its own buffer, so g is still 5",
    );
}

/// A local whose address went only to a `memcpy` has not escaped, so a call
/// that was never given it cannot change it.
#[test]
fn memopt_blockops_a_memcpy_source_has_not_escaped() {
    let src = r#"
extern void *memset(void *, int, unsigned long);
extern void *memcpy(void *, const void *, unsigned long);
extern void *memmove(void *, const void *, unsigned long);
extern void abort(void);
extern void link_error(void);
extern void opaque(char *);

int main(void) {
    char buf[200];
    char out[200];
    buf[0] = 7;
    memcpy(out, buf, sizeof out);
    opaque(out);
    if (buf[0] != 7) link_error();
    return 0;
}
"#;
    assert_link_error_folded(
        "blockops_memcpy_source",
        src,
        "opaque was given out, never buf",
    );
}
