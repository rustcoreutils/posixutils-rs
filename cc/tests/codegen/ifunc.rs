//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__attribute__((ifunc("resolver")))` (GNU, ELF only): the symbol is a
// GNU indirect function, bound at load time to whatever its resolver
// returns -- glibc's string functions and runtime CPU dispatch use it.
// Mach-O has no indirect functions, so the runtime test is Linux-only.
//

#[cfg(target_os = "linux")]
use crate::common::{compile_and_run, compile_and_run_everywhere};

#[cfg(target_os = "linux")]
const IFUNC: &str = r#"
/* `ifunc("resolver")`: the dynamic linker (or the static startup code)
   calls the resolver once and binds the symbol to the function it returns,
   as glibc's string functions and zlib-ng's dispatch do. */
static int impl_b(int x) { return x + 2; }

typedef int (*fn_t)(int);

/* A resolver runs before main and before relocation of everything else is
   complete, so it only reads constants here. */
static fn_t pick(void) { return impl_b; }

int add(int x) __attribute__((ifunc("pick")));

/* A static ifunc, declared before its resolver is defined. */
static int twice(int x) __attribute__((ifunc("pick_twice")));
static int twice_impl(int x) { return 2 * x; }
static void *pick_twice(void) { return (void *)twice_impl; }

/* Referenced through a pointer as well as called directly. */
static int (*volatile padd)(int) = add;

int main(void)
{
    if (add(1) != 3) return 1;
    if (padd(5) != 7) return 2;
    if (twice(21) != 42) return 3;
    return 0;
}
"#;

/// Called directly and through a pointer, global and static, dynamic on the
/// host and static (IRELATIVE) on aarch64.
#[cfg(target_os = "linux")]
#[test]
fn codegen_ifunc_binds_to_the_resolvers_choice() {
    compile_and_run_everywhere("ifunc", IFUNC);
}

/// An executable that is not PIE binds an indirect function through an IPLT
/// entry and an IRELATIVE relocation the startup code applies; PIC code and
/// a PIE through the PLT and GOT the dynamic linker fills. Each must bind.
#[cfg(target_os = "linux")]
#[test]
fn codegen_ifunc_binds_without_and_with_pic() {
    for flags in [&["-no-pie"][..], &["-fPIC"][..], &["-O2", "-no-pie"][..]] {
        let flags: Vec<String> = flags.iter().map(|s| s.to_string()).collect();
        assert_eq!(compile_and_run("ifunc_pic", IFUNC, &flags), 0, "{flags:?}");
    }
}
