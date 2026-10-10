//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The folds Linux's `BUILD_BUG_ON` relies on gcc to make when optimizing:
// a condition it cannot fold leaves a call to an undefined
// `__compiletime_assert_N`, and the program does not link. Each program
// here names one such function per condition, so it links only when every
// condition folded, and then checks the values at run time too.
//

use crate::common::{compile_and_run, compile_and_run_aarch64};

/// The kernel's `BUILD_BUG_ON(cond)`: a call to a function nothing defines,
/// reached only when `cond` holds.
const BUILD_BUG_ON: &str = r#"
#define BUILD_BUG_ON(cond, name) do { \
    extern void name(void) __attribute__((__noreturn__)); \
    if (cond) name(); \
} while (0)
"#;

/// Run `body` (with `BUILD_BUG_ON` defined) at -O1 and -O2, on the host and
/// on aarch64 under qemu when it is there; every run must link and exit 0.
fn folds_when_optimizing(name: &str, body: &str) {
    let src = format!("{BUILD_BUG_ON}{body}");
    for opt in ["-O1", "-O2"] {
        assert_eq!(compile_and_run(name, &src, &[opt.to_string()]), 0, "{opt}");
        if let Some(code) = compile_and_run_aarch64(name, &src, opt) {
            assert_eq!(code, 0, "aarch64 {opt}");
        }
    }
}

/// Two addresses into one object compare as their offsets into it, whatever
/// its address: mm/ksm.c's `STABLE_NODE_DUP_HEAD <= &migrate_nodes`, an
/// element against an element, a member against a member, and a byte
/// offset against a member's address -- of a global and of a local.
#[test]
fn codegen_address_comparisons_within_one_object_fold() {
    folds_when_optimizing(
        "fold_addr_compare",
        r#"
struct list_head { struct list_head *next, *prev; };
static struct list_head migrate_nodes = { &migrate_nodes, &migrate_nodes };
#define STABLE_NODE_DUP_HEAD ((struct list_head *)&migrate_nodes.prev)
struct pair { int a; int b; };
struct pair gpair;
int arr[4];
static int check(struct pair *l) { return l->b - l->a; }
int main(void)
{
    struct pair local = { 1, 3 };
    BUILD_BUG_ON(STABLE_NODE_DUP_HEAD <= &migrate_nodes, __compiletime_assert_1);
    BUILD_BUG_ON(!(&arr[1] > &arr[0]), __compiletime_assert_2);
    BUILD_BUG_ON(&gpair.b == &gpair.a, __compiletime_assert_3);
    BUILD_BUG_ON((char *)&gpair + 4 != (char *)&gpair.b, __compiletime_assert_4);
    BUILD_BUG_ON(&local.b <= &local.a, __compiletime_assert_5);
    BUILD_BUG_ON(&arr[3] < &arr[3], __compiletime_assert_6);
    return migrate_nodes.next != &migrate_nodes || check(&local) != 2;
}
"#,
    );
}

/// An integer element or member of a `static const` aggregate, read at a
/// constant index, is its initializer -- after inlining too, which is the
/// shape arch/x86/kvm/reverse_cpuid.h has: `reverse_cpuid[leaf].function`
/// in an `always_inline` function called with a constant. Elements the
/// initializer leaves out are zero, a nested aggregate is looked into, and
/// a member narrower than an `int` reads as its own type.
#[test]
fn codegen_const_aggregate_element_loads_fold() {
    folds_when_optimizing(
        "fold_const_aggregate",
        r#"
struct cpuid_reg { unsigned function, index; signed char reg; };
static const struct cpuid_reg reverse_cpuid[] = {
    [0] = { 1, 0, 3 }, [1] = { 0x80000001, 0, -2 }, [2] = { 7, 0, 1 }, [4] = { 9, 2, 0 },
};
static const struct { int a; struct { char c; long l; } in[2]; } nested = {
    1, { { 'x', 5 }, { 'y', -6 } },
};
static const unsigned short wide[3] = { 1, 0xfffe, 3 };
static inline __attribute__((__always_inline__)) unsigned get(unsigned leaf)
{
    BUILD_BUG_ON(reverse_cpuid[leaf].function == 0, __compiletime_assert_680);
    return reverse_cpuid[leaf].function;
}
int main(void)
{
    BUILD_BUG_ON(reverse_cpuid[3].function != 0, __compiletime_assert_1);
    BUILD_BUG_ON(reverse_cpuid[1].reg != -2, __compiletime_assert_2);
    BUILD_BUG_ON(reverse_cpuid[4].index != 2, __compiletime_assert_3);
    BUILD_BUG_ON(nested.in[1].l != -6, __compiletime_assert_4);
    BUILD_BUG_ON(nested.in[0].c != 'x', __compiletime_assert_5);
    BUILD_BUG_ON(wide[1] != 0xfffe, __compiletime_assert_6);
    return get(2) == 7 && get(1) == 0x80000001 ? 0 : 1;
}
"#,
    );
}

/// `__builtin_object_size` of a member reached through a pointer is the
/// member's declared size for types 1 and 3, as gcc gives it once
/// optimizing, and constant (fs/ext4/super.c's `strtomem_pad(es->
/// s_volume_name, ...)` asserts both); `&p->arr[2]` is what is left of the
/// member. A trailing array may run past its declared size, so it, like
/// the whole object behind an unknown pointer, is unknown.
#[test]
fn codegen_object_size_of_a_member_through_a_pointer() {
    folds_when_optimizing(
        "objsize_member_ptr",
        r#"
#include <stddef.h>
struct in { char x[6]; int y; };
struct sb { int a; char s_volume_name[16]; struct in in; int b; char tail[8]; };
#define OS(e, t) __builtin_object_size(e, t)
__attribute__((noinline)) size_t f(struct sb *es)
{
    const size_t len = OS(es->s_volume_name, 1);
    BUILD_BUG_ON(!__builtin_constant_p(len) || len == (size_t)-1, __compiletime_assert_1411);
    BUILD_BUG_ON(OS(es->s_volume_name, 3) != 16, __compiletime_assert_1);
    BUILD_BUG_ON(OS(&es->s_volume_name[2], 1) != 14, __compiletime_assert_2);
    BUILD_BUG_ON(OS(es->in.x + 1, 1) != 5, __compiletime_assert_3);
    BUILD_BUG_ON(OS(&es->in, 1) != 12, __compiletime_assert_4);
    BUILD_BUG_ON(OS(es[1].s_volume_name, 1) != 16, __compiletime_assert_5);
    BUILD_BUG_ON(OS((*es).s_volume_name, 3) != 16, __compiletime_assert_6);
    BUILD_BUG_ON(OS(es->tail, 1) != (size_t)-1, __compiletime_assert_7);
    BUILD_BUG_ON(OS(es->tail, 3) != 0, __compiletime_assert_8);
    return len;
}
int main(void) { struct sb v; return f(&v) == 16 ? 0 : 1; }
"#,
    );
}
