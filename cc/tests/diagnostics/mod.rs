//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Negative-path tests: programs that must be REJECTED.
//
// Every other suite proves that accepted programs run correctly. None proved
// that invalid programs are diagnosed — `compile_and_run` collapses a compile
// failure into the sentinel `-1` and discards stderr — which is how a dozen
// missing C constraint checks went unnoticed.
//
// Each constraint gets both directions: a program that must be rejected, and
// one that must still be accepted, so a check cannot pass by rejecting
// everything.
//

use crate::common::{compile_expect_error, compile_expect_ok};

// ============================================================================
// #L1 — implicit int (C99 6.7.2p2)
// ============================================================================

#[test]
fn diagnostics_implicit_int_is_rejected() {
    compile_expect_error(
        "implicit_int_func",
        "f(void){return 1;}\n",
        "type specifier missing",
    );
    compile_expect_error(
        "implicit_int_global",
        "static x;\n",
        "type specifier missing",
    );
    compile_expect_error(
        "implicit_int_local",
        "int main(void){ const y = 3; return y-3; }\n",
        "type specifier missing",
    );
}

/// The predicate is subtler than "no base kind was set": `signed`/`unsigned`
/// name a type while only setting a modifier, and `short`/`long` set the kind.
/// Everything here must still compile.
#[test]
fn diagnostics_explicit_types_are_accepted() {
    for (name, src) in [
        ("ok_unsigned", "unsigned u; int main(void){u=1;return 0;}\n"),
        ("ok_signed", "signed s; int main(void){s=1;return 0;}\n"),
        (
            "ok_short_long",
            "short sh; long lo; int main(void){return 0;}\n",
        ),
        (
            "ok_unsigned_long",
            "unsigned long ul; int main(void){return 0;}\n",
        ),
        (
            "ok_struct",
            "struct S{int a;}; struct S v; int main(void){return 0;}\n",
        ),
        (
            "ok_typedef",
            "typedef int T; T t; int main(void){return 0;}\n",
        ),
        (
            "ok_enum",
            "enum E{A}; enum E e; int main(void){return 0;}\n",
        ),
        (
            "ok_const_int",
            "const int ci=1; int main(void){return 0;}\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

// ============================================================================
// #L2 — duplicate case and default labels (C99 6.8.4.2p3)
// ============================================================================

#[test]
fn diagnostics_duplicate_switch_labels_are_rejected() {
    compile_expect_error(
        "dup_case",
        "int f(int x){switch(x){case 1: return 1; case 1: return 2;} return 0;}\n",
        "duplicate case value",
    );
    compile_expect_error(
        "dup_default",
        "int f(int x){switch(x){default: return 1; default: return 2;} return 0;}\n",
        "multiple default labels",
    );
}

/// A non-constant case label can never match, so it was silently dropped.
#[test]
fn diagnostics_non_constant_case_label_is_rejected() {
    compile_expect_error(
        "nonconst_case",
        "int f(int x, int y){switch(x){case y: return 1;} return 0;}\n",
        "not an integer constant expression",
    );
}

#[test]
fn diagnostics_distinct_switch_labels_are_accepted() {
    compile_expect_ok(
        "ok_switch",
        "int f(int x){switch(x){case 1: return 1; case 2: return 2; default: return 0;}}\n",
    );
    // Duff's device: case labels nested inside a loop within the switch.
    compile_expect_ok(
        "ok_duff",
        "void f(int n, char *d, char *s){ int i=(n+7)/8; switch(n%8){ case 0: do{ *d++=*s++;\n\
         case 7: *d++=*s++; case 6: *d++=*s++; case 5: *d++=*s++; case 4: *d++=*s++;\n\
         case 3: *d++=*s++; case 2: *d++=*s++; case 1: *d++=*s++; }while(--i>0); } }\n",
    );
}

// ============================================================================
// #L3 — `return` versus the function's return type (C99 6.8.6.4p1)
// ============================================================================

#[test]
fn diagnostics_return_type_mismatch_is_rejected() {
    compile_expect_error(
        "return_value_from_void",
        "void f(void){ return 1; }\n",
        "'return' with a value in a function returning void",
    );
    compile_expect_error(
        "return_nothing_from_int",
        "int g(void){ return; }\n",
        "'return' with no value in a function returning non-void",
    );
}

#[test]
fn diagnostics_matching_returns_are_accepted() {
    compile_expect_ok("ok_void_return", "void f(void){ return; }\n");
    compile_expect_ok("ok_int_return", "int g(void){ return 1; }\n");
    // Falling off the end of a non-void function is not this constraint.
    compile_expect_ok(
        "ok_implicit_fallthrough",
        "int h(int x){ if(x) return 1; return 0; }\n",
    );
}

// ============================================================================
// #L5 — typedef of a variably modified type
// ============================================================================

#[test]
fn diagnostics_typedef_of_vla_is_rejected() {
    compile_expect_error(
        "typedef_vla",
        "int main(void){int n=4; typedef int arr_t[n]; arr_t x; x[0]=1; return x[0]-1;}\n",
        "variable-length array",
    );
}

/// An ordinary VLA is still fine — only the typedef form was mishandled.
#[test]
fn diagnostics_plain_vla_is_accepted() {
    compile_expect_ok(
        "ok_vla",
        "int main(void){int n=4; int vla[n]; vla[0]=1; return vla[0]-1;}\n",
    );
}

// ============================================================================
// #L6 — call argument count (C99 6.5.2.2p2)
// ============================================================================

#[test]
fn diagnostics_call_arity_mismatch_is_rejected() {
    compile_expect_error(
        "call_too_few",
        "int g(int,int);\nint main(void){return g(1);}\n",
        "call expects 2 arguments",
    );
    compile_expect_error(
        "call_too_many",
        "int g(int);\nint main(void){return g(1,2,3);}\n",
        "call expects 1 argument",
    );
    compile_expect_error(
        "call_variadic_short",
        "int g(int,int,...);\nint main(void){return g(1);}\n",
        "call expects at least 2 arguments",
    );
}

#[test]
fn diagnostics_correct_calls_are_accepted() {
    for (name, src) in [
        (
            "ok_call_exact",
            "int g(int,int);\nint main(void){return g(1,2);}\n",
        ),
        (
            "ok_call_void",
            "int g(void);\nint main(void){return g();}\n",
        ),
        // An unprototyped declaration leaves the parameters unspecified, so
        // any number of arguments is legal.
        (
            "ok_call_noproto",
            "int g();\nint main(void){return g(1,2,3);}\n",
        ),
        (
            "ok_call_variadic",
            "int g(int,...);\nint main(void){return g(1)+g(1,2,3);}\n",
        ),
        (
            "ok_call_fnptr",
            "int (*fp)(int,int);\nint main(void){return fp(1,2);}\n",
        ),
        (
            "ok_call_stdlib",
            "#include <stdio.h>\nint main(void){printf(\"%d %d\\n\",1,2);return 0;}\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

// ============================================================================
// #X4 — incompatible typedef redefinition (C11/C17 6.7p3)
// ============================================================================

#[test]
fn diagnostics_incompatible_typedef_redefinition_is_rejected() {
    compile_expect_error(
        "typedef_conflict",
        "typedef int foo; typedef char foo; foo x;\n",
        "incompatible type",
    );
}

/// C11 legalized redefining a typedef to a *compatible* type, which two
/// headers declaring the same alias rely on.
#[test]
fn diagnostics_compatible_typedef_redefinition_is_accepted() {
    compile_expect_ok(
        "typedef_same",
        "typedef int foo; typedef int foo; foo x; int main(void){x=1;return 0;}\n",
    );
}

// ============================================================================
// #X9 — `_Atomic` on an array or function type (C17 6.7.2.4p3)
// ============================================================================

#[test]
fn diagnostics_atomic_on_array_is_rejected() {
    compile_expect_error(
        "atomic_array",
        "#include <stdatomic.h>\n_Atomic(int[3]) a;\n",
        "'_Atomic' cannot be applied to an array type",
    );
}

/// `_Atomic int a[3]` is an array *of* atomic ints, which is legal — the
/// qualifier lands on the element type, not the array.
#[test]
fn diagnostics_atomic_qualified_forms_are_accepted() {
    compile_expect_ok(
        "ok_atomic_scalar",
        "#include <stdatomic.h>\n_Atomic int ai;\nint main(void){return 0;}\n",
    );
    compile_expect_ok(
        "ok_atomic_array_of",
        "#include <stdatomic.h>\n_Atomic int arr[3];\nint main(void){return 0;}\n",
    );
}

// ============================================================================
// Checks that already existed — pinned so the new suite covers them too
// ============================================================================

#[test]
fn diagnostics_preexisting_constraints_still_fire() {
    compile_expect_error(
        "undeclared_ident",
        "int main(void){ return undefined_thing; }\n",
        "undeclared identifier",
    );
    compile_expect_error(
        "assign_to_const",
        "int main(void){ const int c = 1; c = 2; return c; }\n",
        "read-only",
    );
}
