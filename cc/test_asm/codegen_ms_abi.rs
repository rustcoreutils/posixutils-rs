//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__attribute__((ms_abi))`: the diagnostic half, compiled in process. The
// interop half, against gcc in every pairing, is `tests/codegen/ms_abi.rs`.
//

use crate::test_compile::compile_expect_warning_with;

/// gcc for aarch64 has no `ms_abi` or `sysv_abi`: it warns that each is
/// ignored, and the function keeps the native convention. So does c17 --
/// the warning is in the attributes group. The run against gcc is
/// `codegen_ms_abi_is_ignored_on_aarch64` in `tests/codegen/ms_abi.rs`.
#[test]
fn codegen_ms_abi_is_ignored_on_aarch64_with_a_warning() {
    let src = "__attribute__((ms_abi)) long f(long a);\n\
               __attribute__((sysv_abi)) long g(long a);\n";
    let mut args: Vec<String> = vec!["--target=aarch64-unknown-linux-gnu".to_string()];
    let stderr = compile_expect_warning_with("ms_abi_a64", src, &args);
    assert!(
        stderr.contains("'ms_abi' attribute directive ignored")
            && stderr.contains("'sysv_abi' attribute directive ignored"),
        "{stderr}"
    );
    args.push("-Wno-attributes".to_string());
    let quiet = compile_expect_warning_with("ms_abi_a64_quiet", src, &args);
    assert!(!quiet.contains("ms_abi"), "{quiet}");
}

/// An `ms_abi` function saves `%rsi` and `%rdi`, callee-saved under Win64 --
/// but not one that is a global register variable, which is the variable's:
/// restoring it on the way out undid every store the function made to it.
#[test]
fn codegen_ms_abi_does_not_save_a_global_register() {
    use super::asm_probe::{asm_for_with, body_of, X86_64_LINUX};
    let src = "register long g asm(\"rdi\");\n\
               __attribute__((ms_abi)) long w(long a) { g = a; return a + 1; }\n";
    for opt in ["-O0", "-O2"] {
        let asm = asm_for_with("ms_abi_global_reg", X86_64_LINUX, src, &[opt]);
        let body = body_of(&asm, "w");
        assert!(body.contains("pushq %rsi"), "{opt}:\n{body}");
        assert!(!body.contains("pushq %rdi"), "{opt}:\n{body}");
        assert!(!body.contains("popq %rdi"), "{opt}:\n{body}");
    }
}

/// gcc's placement of a function-type attribute written inside a grouped
/// declarator: on the function type the group's pointer points at, not on
/// the declared function -- `getfp` returns a pointer to an `ms_abi`
/// function, `take` one to a vector-PCS function, and neither is one itself.
/// An attribute in a group with nothing outside it to give it to -- `(attr
/// plain)` -- is the declared function's, and one among the specifiers goes
/// where it always did, to `outer` itself. Each pointer below is the type
/// gcc gives its initializer, so neither compiler has anything to say; c17
/// warned that `getfp` returned the wrong type, and that `arr`, an array of
/// such pointers, took an attribute only a function type takes. The calls
/// are run in `tests/codegen/ms_abi.rs`.
#[test]
fn codegen_fn_type_attribute_in_a_group_is_the_pointed_functions() {
    use super::asm_probe::{asm_for_with, AARCH64_LINUX};
    use crate::test_compile::compile_warnings;
    let x86 = r#"
long __attribute__((ms_abi)) target(long a, long b) { return a - b; }
long (__attribute__((ms_abi)) *getfp(long x))(long, long) { return x ? target : 0; }
long (*__attribute__((ms_abi)) getfp2(long x))(long, long) { return x ? target : 0; }
long (__attribute__((ms_abi)) plain)(long a) { return a; }
__attribute__((ms_abi)) long (*outer(long x))(long, long) { return 0; }
long (__attribute__((ms_abi)) *fp)(long, long) = target;
long (__attribute__((ms_abi)) *arr[2])(long, long) = { target, target };
long (__attribute__((ms_abi)) *(*g1)(long))(long, long) = getfp;
long (__attribute__((ms_abi)) *(*g2)(long))(long, long) = getfp2;
long (__attribute__((ms_abi)) *pp)(long) = plain;
__attribute__((ms_abi)) long (*(*po)(long))(long, long) = outer;
"#;
    let w = compile_warnings(
        "grouped_ms_abi",
        x86,
        &["--target=x86_64-unknown-linux-gnu"],
    );
    assert!(w.is_empty(), "{w:?}");
    let a64 = r#"
typedef float v4sf __attribute__((vector_size(16)));
static __attribute__((aarch64_vector_pcs)) v4sf local_vpcs(v4sf a) { return a + a; }
v4sf (__attribute__((aarch64_vector_pcs)) *take(void))(v4sf) { return local_vpcs; }
v4sf (*__attribute__((aarch64_vector_pcs)) take2(void))(v4sf) { return local_vpcs; }
"#;
    let w = compile_warnings("grouped_vpcs", a64, &["--target=aarch64-unknown-linux-gnu"]);
    assert!(w.is_empty(), "{w:?}");
    let asm = asm_for_with("grouped_vpcs", AARCH64_LINUX, a64, &["-O2"]);
    assert!(asm.contains(".variant_pcs local_vpcs"), "{asm}");
    assert!(
        !asm.contains(".variant_pcs take"),
        "a function returning a vector-PCS pointer is marked:\n{asm}"
    );
}
